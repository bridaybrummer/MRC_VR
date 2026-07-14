# =============================================================================
# Signal Detection in Vital Registration Data — Wrangling & Analysis Script
# =============================================================================
# Runs all four methods (MA, CUSUM, Farrington, NB) for:
#   Case Study 1: Influenza deaths (J09–J18)
#   Case Study 2: Intentional self-harm / Suicide (X60–X84)
#
# Saves precomputed outputs to projects/signal_detection/sd_results.rda
# =============================================================================

library(arrow)
library(dplyr)
library(data.table)
library(lubridate)
library(zoo)
library(surveillance)
library(MASS)
library(ggplot2)
library(haven)
library(scales)

# ─── 1. LOAD DATA ─────────────────────────────────────────────────────────────
cat("Loading data...\n")
df <- read_feather("Deaths2022_MRCversionFINAL.feather") %>% as.data.table()

# Normalise haven_labelled columns
# Numeric columns: cast straight to integer/numeric (as_factor would re-level)
df[, DeathYear       := as.integer(as.numeric(DeathYear))]
df[, epi_week        := as.integer(as.numeric(epi_week))]
df[, epi_year        := as.integer(as.numeric(epi_year))]
df[, weekstart       := as.Date(weekstart)]
# Character column: use as_factor then coerce to character
df[, UnderlyingCause := as.character(as_factor(UnderlyingCause))]

# Restrict to reasonable analysis window (2010–2022)
df <- df[DeathYear >= 2010 & DeathYear <= 2022]

cat("Rows after year filter:", nrow(df), "\n")

# ─── 2. HELPER: BUILD COMPLETE WEEKLY GRID ────────────────────────────────────
# Ensures every epi_week 1–52 exists in every epi_year, even if count = 0.
complete_weekly <- function(ts_dt) {
  all_weeks <- CJ(
    epi_year = min(ts_dt$epi_year):max(ts_dt$epi_year),
    epi_week = 1:52
  )
  ts_dt[all_weeks, on = .(epi_year, epi_week)][is.na(deaths), deaths := 0L] %>%
    setorder(epi_year, epi_week) %>%
    .[, week_index := .I] %>%
    # Attach a representative date (start of that epi week)
    .[, week_date := as.Date(paste(epi_year, epi_week, 1, sep = "-"),
                             format = "%Y-%U-%u")]
}

# ─── 3. AGGREGATE WEEKLY COUNTS ───────────────────────────────────────────────

## Influenza: J09–J18 (influenza-specific J09–J11, plus pneumonia J12–J18)
flu_raw <- df[grepl("^J09|^J10|^J11|^J12|^J13|^J14|^J15|^J16|^J17|^J18",
                    UnderlyingCause),
              .(deaths = .N), by = .(epi_year, epi_week)]
flu_ts  <- complete_weekly(flu_raw)

## Suicide: X60–X84 (intentional self-harm)
# Regex: X6[0-9], X7[0-9], X8[0-4]
sui_raw <- df[grepl("^X[67][0-9]|^X8[0-4]", UnderlyingCause),
              .(deaths = .N), by = .(epi_year, epi_week)]
sui_ts  <- complete_weekly(sui_raw)

cat("Influenza weekly obs:", nrow(flu_ts), " | Suicide weekly obs:", nrow(sui_ts), "\n")
cat("Influenza total deaths:", sum(flu_ts$deaths),
    " | Suicide total deaths:", sum(sui_ts$deaths), "\n")

# ─── 4. HELPER FUNCTIONS FOR EACH METHOD ──────────────────────────────────────

## ── 4a. Moving Average ────────────────────────────────────────────────────────
run_ma <- function(ts_dt, baseline_years, k = 52) {
  dt <- copy(ts_dt)
  dt[, `:=`(
    ma       = rollapply(deaths, width = k, FUN = mean, fill = NA,
                         align = "right", partial = FALSE),
    roll_sd  = rollapply(deaths, width = k, FUN = sd,   fill = NA,
                         align = "right", partial = FALSE)
  )]
  dt[, threshold_ma := ma + 2 * roll_sd]
  dt[, alert_ma     := deaths > threshold_ma & !epi_year %in% baseline_years]
  dt
}

## ── 4b. CUSUM via surveillance pkg ────────────────────────────────────────────
# range = NULL monitors all time points. m = mu0 sets the in-control mean from
# the baseline. k and h are the reference (slack) value and decision boundary.
run_cusum <- function(ts_dt, baseline_years, k = 1.04, h = 2.26) {
  sts_obj <- sts(
    observed  = matrix(ts_dt$deaths, ncol = 1),
    start     = c(min(ts_dt$epi_year), 1L),
    frequency = 52L
  )
  baseline_idx <- which(ts_dt$epi_year %in% baseline_years)
  mu0 <- mean(ts_dt$deaths[baseline_idx])
  res <- cusum(sts_obj,
               control = list(
                 range  = seq_len(nrow(ts_dt)),  # cusum() requires explicit numeric vector
                 k      = k,
                 h      = h,
                 m      = mu0,
                 reset  = TRUE
               ))
  list(sts = res, ts = ts_dt)
}

## ── 4c. Farrington Flexible ───────────────────────────────────────────────────
# range = NULL lets farringtonFlexible auto-select computable epochs (i.e. those
# that have >= b years of prior data for the reference window). Passing a manual
# row-index range causes a subscript error because the function internally maps
# epochs to a different index space.
run_farrington <- function(ts_dt, b = 5, w = 3, trend = TRUE,
                           noPeriods = 1, alpha = 0.05) {
  sts_obj <- sts(
    observed  = matrix(ts_dt$deaths, ncol = 1),
    start     = c(min(ts_dt$epi_year), 1L),
    frequency = 52L
  )
  control <- list(
    range     = NULL,       # auto: monitor all epochs for which b years of history exist
    b         = b,
    w         = w,
    reweight  = TRUE,
    trend     = trend,
    noPeriods = noPeriods,
    alpha     = alpha
  )
  tryCatch(
    farringtonFlexible(sts_obj, control = control),
    error = function(e) { warning("Farrington failed: ", e$message); NULL }
  )
}

## ── 4d. Negative Binomial GLM ─────────────────────────────────────────────────
run_nb <- function(ts_dt, baseline_years, p_threshold = 0.95) {
  train <- ts_dt[epi_year %in% baseline_years]
  test  <- copy(ts_dt)

  mod <- glm.nb(deaths ~ week_index + sin(2 * pi * epi_week / 52) +
                  cos(2 * pi * epi_week / 52),
                data = train)

  test[, predicted  := predict(mod, newdata = test, type = "response")]
  test[, threshold_nb := qnbinom(p     = p_threshold,
                                 mu    = predicted,
                                 size  = mod$theta)]
  test[, alert_nb := deaths > threshold_nb]
  list(model = mod, predictions = test)
}

# ─── 5. SET BASELINE AND RUN ANALYSES ─────────────────────────────────────────
BASELINE_YEARS <- 2010:2019   # exclude 2020+ pandemic period

cat("Running Moving Average...\n")
flu_ma  <- run_ma(flu_ts,  BASELINE_YEARS)
sui_ma  <- run_ma(sui_ts,  BASELINE_YEARS)

cat("Running CUSUM...\n")
flu_cusum <- run_cusum(flu_ts, BASELINE_YEARS)
sui_cusum <- run_cusum(sui_ts, BASELINE_YEARS)

cat("Running Farrington...\n")
flu_farr <- run_farrington(flu_ts, b = 5, w = 3)
sui_farr <- run_farrington(sui_ts, b = 5, w = 4)   # wider window for lower counts

cat("Running Negative Binomial...\n")
flu_nb <- run_nb(flu_ts, BASELINE_YEARS)
sui_nb <- run_nb(sui_ts, BASELINE_YEARS)

# ─── 6. PLOTS ─────────────────────────────────────────────────────────────────
PANDEMIC_SHADE <- annotate("rect", xmin = as.Date("2020-03-01"),
                           xmax = as.Date("2022-12-31"),
                           ymin = -Inf, ymax = Inf,
                           fill = "grey85", alpha = 0.5)

# ── Fig 1: Influenza — Moving Average
fig_flu_ma <- ggplot(flu_ma, aes(x = week_date)) +
  PANDEMIC_SHADE +
  geom_ribbon(aes(ymin = ma, ymax = pmax(ma, threshold_ma)),
              fill = "steelblue", alpha = 0.25) +
  geom_line(aes(y = deaths), linewidth = 0.4, colour = "grey30") +
  geom_line(aes(y = ma), colour = "steelblue", linewidth = 0.8) +
  geom_line(aes(y = threshold_ma), colour = "steelblue", linetype = "dashed") +
  geom_point(data = flu_ma[alert_ma == TRUE],
             aes(y = deaths), colour = "red", size = 1.8) +
  labs(title    = "Influenza deaths — 52-week trailing moving average",
       subtitle = "Red points exceed MA + 2 SD threshold | Shaded = pandemic period",
       x = NULL, y = "Weekly deaths") +
  theme_minimal()

# ── Fig 2: Suicide — Moving Average
fig_sui_ma <- ggplot(sui_ma, aes(x = week_date)) +
  PANDEMIC_SHADE +
  geom_ribbon(aes(ymin = ma, ymax = pmax(ma, threshold_ma)),
              fill = "darkorange", alpha = 0.25) +
  geom_line(aes(y = deaths), linewidth = 0.4, colour = "grey30") +
  geom_line(aes(y = ma), colour = "darkorange", linewidth = 0.8) +
  geom_line(aes(y = threshold_ma), colour = "darkorange", linetype = "dashed") +
  geom_point(data = sui_ma[alert_ma == TRUE],
             aes(y = deaths), colour = "red", size = 1.8) +
  labs(title    = "Suicide deaths — 52-week trailing moving average",
       subtitle = "Red points exceed MA + 2 SD threshold | Shaded = pandemic period",
       x = NULL, y = "Weekly deaths") +
  theme_minimal()

# ── Fig 3: Farrington plots (base R surveillance plots → captured as grob)
fig_flu_farr_fn <- function() {
  if (!is.null(flu_farr)) plot(flu_farr, main = "Farrington — Influenza (J09–J18)")
  else plot.new(); text(0.5, 0.5, "Farrington model failed")
}
fig_sui_farr_fn <- function() {
  if (!is.null(sui_farr)) plot(sui_farr, main = "Farrington — Suicide (X60–X84)")
  else plot.new(); text(0.5, 0.5, "Farrington model failed")
}

# ── Fig 4: NB model
fig_flu_nb <- ggplot(flu_nb$predictions, aes(x = week_date)) +
  PANDEMIC_SHADE +
  geom_ribbon(aes(ymin = predicted, ymax = threshold_nb),
              fill = "forestgreen", alpha = 0.2) +
  geom_line(aes(y = deaths), linewidth = 0.4, colour = "grey30") +
  geom_line(aes(y = predicted), colour = "forestgreen", linewidth = 0.8) +
  geom_line(aes(y = threshold_nb), colour = "forestgreen", linetype = "dashed") +
  geom_point(data = flu_nb$predictions[alert_nb == TRUE],
             aes(y = deaths), colour = "red", size = 1.8) +
  labs(title    = "Influenza deaths — Negative Binomial model",
       subtitle = "Green = fitted mean | Dashed = 95th NB percentile | Red = alarm",
       x = NULL, y = "Weekly deaths") +
  theme_minimal()

fig_sui_nb <- ggplot(sui_nb$predictions, aes(x = week_date)) +
  PANDEMIC_SHADE +
  geom_ribbon(aes(ymin = predicted, ymax = threshold_nb),
              fill = "purple", alpha = 0.2) +
  geom_line(aes(y = deaths), linewidth = 0.4, colour = "grey30") +
  geom_line(aes(y = predicted), colour = "purple", linewidth = 0.8) +
  geom_line(aes(y = threshold_nb), colour = "purple", linetype = "dashed") +
  geom_point(data = sui_nb$predictions[alert_nb == TRUE],
             aes(y = deaths), colour = "red", size = 1.8) +
  labs(title    = "Suicide deaths — Negative Binomial model",
       subtitle = "Purple = fitted mean | Dashed = 95th NB percentile | Red = alarm",
       x = NULL, y = "Weekly deaths") +
  theme_minimal()

# ── Fig 5: Seasonality heatmaps (week × year)
make_heatmap <- function(ts_dt, title) {
  ggplot(ts_dt[epi_year >= 2012], aes(x = epi_week, y = factor(epi_year), fill = deaths)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_viridis_c(option = "magma", name = "Deaths") +
    labs(title = title, x = "Epidemiological week", y = NULL) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 8))
}
fig_flu_heatmap <- make_heatmap(flu_ts, "Influenza deaths by week and year (J09–J18)")
fig_sui_heatmap <- make_heatmap(sui_ts, "Suicide deaths by week and year (X60–X84)")

# ── Fig 6: Method comparison — alarm rate by week-of-year (flu only)
alarm_comparison <- data.table(
  week    = flu_ma$epi_week,
  year    = flu_ma$epi_year,
  MA      = flu_ma$alert_ma,
  NB      = flu_nb$predictions$alert_nb
)
alarm_summary <- alarm_comparison[, .(
  MA = sum(MA, na.rm = TRUE),
  NB = sum(NB, na.rm = TRUE)
), by = week]

fig_alarm_compare <- ggplot(
  melt(alarm_summary, id.vars = "week", variable.name = "Method",
       value.name = "Alarms"),
  aes(x = week, y = Alarms, colour = Method)) +
  geom_line() +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(1, 52, by = 4)) +
  labs(title    = "Influenza: cumulative alarms per calendar week (2020–2022)",
       subtitle = "MA vs Negative Binomial",
       x = "Epidemiological week", y = "Number of alarm years") +
  theme_minimal()

# ─── 7. SUMMARY STATISTICS ────────────────────────────────────────────────────
# Helper: extract alarm vector from a farrington result aligned to a ts data.table
# farringtonFlexible only covers epochs with >= b years of history, so its @alarm
# slot is shorter than flu_ts. We map by epoch index (1-based row in sts).
farr_alarms_for_years <- function(farr_res, ts_dt, years) {
  if (is.null(farr_res)) return(NA_integer_)
  # epoch() returns the index of each monitored time point within the sts object
  ep  <- as.integer(epoch(farr_res))
  alm <- as.integer(farr_res@alarm[, 1])
  # match to ts_dt rows in the requested years
  monitor_rows <- which(ts_dt$epi_year %in% years)
  sum(alm[ep %in% monitor_rows], na.rm = TRUE)
}

alert_summary_table <- data.frame(
  Method = c("Moving Average (MA)",
             "CUSUM",
             "Farrington",
             "Negative Binomial (NB)"),
  `Influenza alarms (2020-2022)` = c(
    sum(flu_ma$alert_ma[flu_ma$epi_year >= 2020], na.rm = TRUE),
    sum(flu_cusum$sts@alarm[flu_ts$epi_year >= 2020, ], na.rm = TRUE),
    farr_alarms_for_years(flu_farr, flu_ts, 2020:2022),
    sum(flu_nb$predictions$alert_nb[flu_nb$predictions$epi_year >= 2020], na.rm = TRUE)
  ),
  `Suicide alarms (2020-2022)` = c(
    sum(sui_ma$alert_ma[sui_ma$epi_year >= 2020], na.rm = TRUE),
    sum(sui_cusum$sts@alarm[sui_ts$epi_year >= 2020, ], na.rm = TRUE),
    farr_alarms_for_years(sui_farr, sui_ts, 2020:2022),
    sum(sui_nb$predictions$alert_nb[sui_nb$predictions$epi_year >= 2020], na.rm = TRUE)
  ),
  check.names = FALSE
)

# ─── 8. ONSET DETECTION ───────────────────────────────────────────────────────
# Primary question: *when does the season start?*
# Onset = first epi-week where >= min_consec CONSECUTIVE alarm weeks occur
# within the defined seasonal window. This week is the proposed trigger for
# public health response escalation based on vital registration data alone.
#
# Southern Hemisphere influenza season window: epi weeks 15–40 (~Apr–Oct).
# Suicide: broad window (all 52 weeks) — seasonal signal is modest.

detect_onset <- function(alert_vec, week_vec, year_vec,
                         season_weeks = 15:40, min_consec = 2) {
  dt <- data.table(
    alert = as.logical(alert_vec),
    week  = as.integer(week_vec),
    year  = as.integer(year_vec)
  )
  dt[order(year, week)][week %in% season_weeks, {
    r          <- rle(alert)
    cum_ends   <- cumsum(r$lengths)
    cum_starts <- c(1L, head(cum_ends, -1L) + 1L)
    alarm_runs <- which(r$values & r$lengths >= min_consec)
    if (length(alarm_runs) > 0L) {
      onset_week <- week[cum_starts[alarm_runs[1]]]
    } else {
      onset_week <- NA_integer_
    }
    .(onset_week = onset_week)
  }, by = year]
}

# Align all alarm vectors to the full ts row space
align_farr <- function(farr_res, ts_dt) {
  v <- rep(FALSE, nrow(ts_dt))
  if (!is.null(farr_res)) {
    ep <- as.integer(epoch(farr_res))
    v[ep] <- as.logical(farr_res@alarm[, 1])
  }
  v
}

flu_farr_alarm  <- align_farr(flu_farr, flu_ts)
sui_farr_alarm  <- align_farr(sui_farr, sui_ts)
flu_cusum_alarm <- as.logical(flu_cusum$sts@alarm[, 1])
sui_cusum_alarm <- as.logical(sui_cusum$sts@alarm[, 1])

# ── Influenza onset (season window: epi weeks 15–40)
onset_flu <- rbindlist(list(
  detect_onset(flu_ma$alert_ma,
               flu_ma$epi_week, flu_ma$epi_year)[, method := "MA"],
  detect_onset(flu_cusum_alarm,
               flu_ts$epi_week, flu_ts$epi_year)[, method := "CUSUM"],
  detect_onset(flu_farr_alarm,
               flu_ts$epi_week, flu_ts$epi_year)[, method := "Farrington"],
  detect_onset(flu_nb$predictions$alert_nb,
               flu_nb$predictions$epi_week,
               flu_nb$predictions$epi_year)[,    method := "NB"]
))
onset_flu[, method := factor(method, levels = c("MA", "CUSUM", "Farrington", "NB"))]
onset_flu_wide <- dcast(onset_flu, year ~ method, value.var = "onset_week")

# ── Suicide onset (all weeks — weaker seasonality)
onset_sui <- rbindlist(list(
  detect_onset(sui_ma$alert_ma,
               sui_ma$epi_week, sui_ma$epi_year,   season_weeks = 1:52)[, method := "MA"],
  detect_onset(sui_cusum_alarm,
               sui_ts$epi_week, sui_ts$epi_year,   season_weeks = 1:52)[, method := "CUSUM"],
  detect_onset(sui_farr_alarm,
               sui_ts$epi_week, sui_ts$epi_year,   season_weeks = 1:52)[, method := "Farrington"],
  detect_onset(sui_nb$predictions$alert_nb,
               sui_nb$predictions$epi_week,
               sui_nb$predictions$epi_year,         season_weeks = 1:52)[, method := "NB"]
))
onset_sui[, method := factor(method, levels = c("MA", "CUSUM", "Farrington", "NB"))]
onset_sui_wide <- dcast(onset_sui, year ~ method, value.var = "onset_week")

# ── Inter-method consistency (SD of onset week across 4 methods per year)
onset_consistency <- onset_flu[, .(
  mean_onset         = round(mean(onset_week,  na.rm = TRUE), 1),
  sd_onset           = round(sd(onset_week,    na.rm = TRUE), 1),
  n_methods_detected = sum(!is.na(onset_week))
), by = year][order(year)]

# ── False alarm rate: % of influenza alarms firing OUTSIDE the season window
fa_counts <- data.table(
  Method = c("MA", "CUSUM", "Farrington", "NB"),
  total_alarms = c(
    sum(flu_ma$alert_ma,                      na.rm = TRUE),
    sum(flu_cusum_alarm),
    sum(flu_farr_alarm),
    sum(flu_nb$predictions$alert_nb,          na.rm = TRUE)
  ),
  outside_window = c(
    sum(flu_ma$alert_ma[!flu_ma$epi_week %in% 15:40],                    na.rm = TRUE),
    sum(flu_cusum_alarm[!flu_ts$epi_week %in% 15:40]),
    sum(flu_farr_alarm[ !flu_ts$epi_week %in% 15:40]),
    sum(flu_nb$predictions$alert_nb[!flu_nb$predictions$epi_week %in% 15:40], na.rm = TRUE)
  )
)
fa_counts[, pct_outside := round(100 * outside_window / pmax(total_alarms, 1), 1)]

# ── Fig A: Onset week by year and method (influenza)
fig_onset_flu <- ggplot(onset_flu[!is.na(onset_week)],
                        aes(x = year, y = onset_week,
                            colour = method, shape = method)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 15, ymax = 40,
           fill = "lightblue", alpha = 0.15) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2010:2022) +
  scale_y_continuous(breaks = seq(15, 40, by = 5),
                     labels = function(w) paste0("Wk ", w)) +
  scale_colour_brewer(palette = "Set1") +
  labs(
    title    = "Influenza season onset week — South Africa, 2010–2022",
    subtitle = "Season window: epi weeks 15–40 | Onset = first of ≥2 consecutive alarm weeks",
    x = "Year", y = "Onset epi-week",
    colour = "Method", shape = "Method"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# ── Fig B: Distribution of onset weeks by method (boxplot)
fig_onset_boxplot <- ggplot(onset_flu[!is.na(onset_week)],
                             aes(x = method, y = onset_week, fill = method)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.85, aes(colour = method)) +
  scale_fill_brewer(palette = "Set1") +
  scale_colour_brewer(palette = "Set1") +
  labs(
    title    = "Distribution of influenza onset weeks by detection method",
    subtitle = "Each point = one year (2010–2022); box = median + IQR",
    x = "Detection method", y = "Onset epi-week"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

# ── Fig C: Inter-method agreement per year (SD of onset across 4 methods)
fig_onset_agreement <- ggplot(onset_consistency[!is.na(sd_onset)],
                              aes(x = year, y = sd_onset)) +
  geom_col(fill = "steelblue", alpha = 0.75) +
  geom_hline(yintercept = 2, linetype = "dashed", colour = "firebrick") +
  annotate("text", x = 2010.3, y = 2.4,
           label = "2-week tolerance", colour = "firebrick", hjust = 0, size = 3.2) +
  scale_x_continuous(breaks = 2010:2022) +
  labs(
    title    = "Inter-method agreement: SD of onset week across 4 methods",
    subtitle = "Lower = higher agreement; dashed = 2-week tolerance",
    x = "Year", y = "SD of onset week (weeks)"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cat("Onset detection complete.\n")
print(onset_flu_wide)
print(onset_consistency)

# ─── 9. SAVE ──────────────────────────────────────────────────────────────────
save(
  flu_ts, sui_ts,
  flu_ma, sui_ma,
  flu_cusum, sui_cusum,
  flu_farr, sui_farr,
  flu_nb, sui_nb,
  flu_farr_alarm, sui_farr_alarm,
  flu_cusum_alarm, sui_cusum_alarm,
  fig_flu_ma, fig_sui_ma,
  fig_flu_nb, fig_sui_nb,
  fig_flu_heatmap, fig_sui_heatmap,
  fig_alarm_compare,
  fig_flu_farr_fn, fig_sui_farr_fn,
  alert_summary_table,
  onset_flu, onset_sui,
  onset_flu_wide, onset_sui_wide,
  onset_consistency,
  fa_counts,
  fig_onset_flu,
  fig_onset_boxplot,
  fig_onset_agreement,
  file = "projects/signal_detection/sd_results.rda"
)
cat("Done. Results saved to projects/signal_detection/sd_results.rda\n")
