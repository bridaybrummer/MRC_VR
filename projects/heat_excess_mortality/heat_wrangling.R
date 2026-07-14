# =============================================================================
# Heat-Excess Mortality Analysis — Wrangling & Modelling Script
# =============================================================================
# Links ERA5-Land heat stress metrics to South African vital registration data
# to estimate excess deaths attributable to high heat stress during the
# extended austral summer (September–April).
#
# Analysis chain:
#   1. Load VR death data  (Deaths2022_MRCversionFINAL.feather)
#   2. Load ERA5 heat stress weekly aggregates (data/era5/heat_stress_weekly_prov.csv)
#      OR simulate a climatologically plausible heat-stress series if ERA5 data
#      are not yet available.
#   3. Merge deaths + heat stress on epi_year × epi_week (× province).
#   4. Descriptive summaries and plots.
#   5. Negative Binomial DLNM (Distributed Lag Non-linear Model) for
#      exposure–response estimation.
#   6. Attributable fraction / excess death estimates.
#
# Output: projects/heat_excess_mortality/heat_results.rda
# =============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(data.table)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(haven)
  library(flextable)
})

# Optional packages — loaded with a soft check
has_dlnm  <- requireNamespace("dlnm",  quietly = TRUE)
has_MASS  <- requireNamespace("MASS",  quietly = TRUE)
has_splines <- requireNamespace("splines", quietly = TRUE)

if (has_dlnm)  library(dlnm)
if (has_MASS)  library(MASS)
if (has_splines) library(splines)

cat("══════════════════════════════════════════════════════════╗\n")
cat("  Heat–Excess Mortality Analysis                           \n")
cat("══════════════════════════════════════════════════════════╝\n\n")


# ─── 1. LOAD VR DEATH DATA ────────────────────────────────────────────────────
cat("1. Loading VR death data …\n")

vr_path <- "Deaths2022_MRCversionFINAL.feather"
if (!file.exists(vr_path)) stop("VR data not found: ", vr_path)

df <- read_feather(vr_path) |> as.data.table()

# Normalise haven_labelled columns
df[, DeathYear  := as.integer(as.numeric(DeathYear))]
df[, epi_week   := as.integer(as.numeric(epi_week))]
df[, epi_year   := as.integer(as.numeric(epi_year))]
df[, weekstart  := as.Date(weekstart)]

# Province column (try several candidate names)
prov_col <- intersect(names(df),
                      c("Province", "province", "prov", "ProvinceCode",
                        "ProvCode", "GeoCodeProv", "ProvinceOfDeath"))[1]
has_province <- !is.na(prov_col)
if (has_province) {
  df[[prov_col]] <- as.character(haven::as_factor(df[[prov_col]]))
  setnames(df, prov_col, "province")
  cat("   Province column found:", prov_col, "\n")
} else {
  cat("   No province column found — national-level analysis only.\n")
}

df <- df[DeathYear >= 2010 & DeathYear <= 2022]
cat("   Rows after year filter:", format(nrow(df), big.mark = ","), "\n\n")


# ─── 2. AGGREGATE WEEKLY DEATHS ──────────────────────────────────────────────
cat("2. Aggregating weekly all-cause deaths …\n")

if (has_province) {
  deaths_weekly <- df[, .(deaths = .N), by = .(epi_year, epi_week, province)]
} else {
  deaths_weekly <- df[, .(deaths = .N), by = .(epi_year, epi_week)]
  deaths_weekly[, province := "South Africa"]
}

# Complete grid: ensure every week appears even if deaths = 0
provinces_all <- unique(deaths_weekly$province)

complete_grid <- CJ(
  epi_year = 2010:2022,
  epi_week = 1:52,
  province = provinces_all
)
deaths_weekly <- deaths_weekly[complete_grid, on = .(epi_year, epi_week, province)]
deaths_weekly[is.na(deaths), deaths := 0L]
setorder(deaths_weekly, province, epi_year, epi_week)

# Attach week start date (Monday of ISO week) — base R calculation
# The Monday of ISO week W of year Y is computed via the Thursday rule:
# Jan 4 of year Y is always in week 1, so week 1 Monday = Jan 4 - weekday(Jan4) + 1
deaths_weekly[, week_start := {
  # First Thursday of the year → defines week 1
  jan4        <- as.Date(paste0(epi_year, "-01-04"))
  wday_jan4   <- as.integer(format(jan4, "%u"))  # Mon=1 ... Sun=7
  week1_mon   <- jan4 - wday_jan4 + 1L
  week1_mon + (epi_week - 1L) * 7L
}]

# Austral summer flag: Sep (month 9) – Apr (month 4 next year)
deaths_weekly[, month := month(week_start)]
deaths_weekly[, is_summer := month %in% c(9, 10, 11, 12, 1, 2, 3, 4)]

cat("   Weekly death records:", format(nrow(deaths_weekly), big.mark = ","), "\n\n")


# ─── 3. LOAD / SIMULATE HEAT STRESS DATA ──────────────────────────────────────
cat("3. Loading heat stress data …\n")

era5_path <- "data/era5/heat_stress_weekly_prov.csv"

if (file.exists(era5_path)) {
  cat("   ERA5 heat stress CSV found — loading real data.\n")
  heat_dt <- fread(era5_path)
  heat_dt[, week_start := as.Date(week_start)]
  data_source_label <- "ERA5-Land (real)"

} else {
  cat("   ERA5 data not available — generating climatological simulation.\n")
  cat("   (Run _download_era5.py and _calc_heat_stress.py to use real data)\n\n")

  # Simulate weekly national-mean apparent temperature for South Africa.
  # Climatological characteristics:
  #   Summer (Jan): mean AT ~28°C (Gauteng/Limpopo), coastal moderated by humidity
  #   Winter (Jul): mean AT ~12°C
  #   Harmonic 1 dominates (annual cycle); harmonic 2 adds minor shoulder asymmetry.

  set.seed(20260309)

  sim_weeks <- CJ(epi_year = 2010:2022, epi_week = 1:52)
  sim_weeks[, week_start := {
    jan4      <- as.Date(paste0(epi_year, "-01-04"))
    wday_jan4 <- as.integer(format(jan4, "%u"))
    week1_mon <- jan4 - wday_jan4 + 1L
    week1_mon + (epi_week - 1L) * 7L
  }]
  sim_weeks[, t_index := .I]

  # Day-of-year for Southern Hemisphere: peak heat around epi week 3 (~Jan)
  sim_weeks[, doy_frac := 2 * pi * (epi_week - 3) / 52]

  simulate_province_heat <- function(prov_name, baseline_at, amplitude_at,
                                     baseline_wbgt, amplitude_wbgt,
                                     phase_shift = 0, noise_sd = 1.5) {
    dt <- copy(sim_weeks)
    dt[, province := prov_name]

    doy_frac_v <- dt$doy_frac   # extract as plain vector inside function

    # Seasonal signal (harmonic 1 + small harmonic 2 for asymmetry)
    seasonal <- amplitude_at * cos(doy_frac_v + phase_shift) +
                0.15 * amplitude_at * cos(2 * (doy_frac_v + phase_shift))

    # About 4% of weeks have "heatwave" spikes of +2–5°C
    n <- nrow(dt)
    hw_flag <- rbinom(n, 1, 0.04)
    hw_spike <- hw_flag * runif(n, 2, 5)

    # Inter-annual trend (+0.03°C/yr, consistent with warming signal)
    trend <- 0.03 * (dt$epi_year - 2010)

    # Noise
    noise <- rnorm(n, 0, noise_sd)

    dt[, mean_at   := baseline_at + seasonal + trend + noise]
    dt[, max_at    := mean_at + abs(rnorm(n, 3, 1)) + hw_spike]
    dt[, mean_wbgt := baseline_wbgt + amplitude_wbgt * cos(doy_frac_v + phase_shift) + trend + rnorm(n, 0, 1.2)]
    dt[, max_wbgt  := mean_wbgt + abs(rnorm(n, 2, 0.8)) + hw_spike * 0.6]
    dt[, n_days    := 7L]

    dt
  }

  # Province-specific climatological parameters (AT mean, amplitude, WBGT mean, amplitude)
  prov_params <- list(
    list("Gauteng",           22, 10, 18, 8,  0.0),
    list("Limpopo",           24, 11, 19, 9,  0.1),
    list("Mpumalanga",        22,  9, 18, 8, -0.1),
    list("North West",        22, 10, 17, 8,  0.0),
    list("Free State",        20,  9, 15, 7,  0.0),
    list("KwaZulu-Natal",     23,  7, 21, 6, -0.2),
    list("Eastern Cape",      19,  7, 17, 6,  0.1),
    list("Western Cape",      18,  7, 15, 6,  0.2),
    list("Northern Cape",     22, 12, 16, 9,  0.0)
  )

  heat_dt <- rbindlist(lapply(prov_params, function(p) {
    simulate_province_heat(p[[1]], p[[2]], p[[3]], p[[4]], p[[5]], p[[6]])
  }))

  # Keep only provinces present in death data
  if (length(provinces_all) > 1) {
    heat_dt <- heat_dt[province %in% provinces_all]
    if (nrow(heat_dt) == 0) {
      # Province names don't match — use national aggregate
      heat_dt <- heat_dt[, .(
        mean_at   = mean(mean_at),
        max_at    = max(max_at),
        mean_wbgt = mean(mean_wbgt),
        max_wbgt  = max(max_wbgt),
        n_days    = 7L
      ), by = .(epi_year, epi_week, week_start)][, province := "South Africa"]
    }
  } else {
    heat_dt <- heat_dt[, .(
      mean_at   = mean(mean_at),
      max_at    = max(max_at),
      mean_wbgt = mean(mean_wbgt),
      max_wbgt  = max(max_wbgt),
      n_days    = 7L
    ), by = .(epi_year, epi_week, week_start)][, province := "South Africa"]
  }

  data_source_label <- "Simulated (ERA5 not yet downloaded)"
}

cat("   Heat stress records:", format(nrow(heat_dt), big.mark = ","), "\n")
cat("   Provinces:", paste(sort(unique(heat_dt$province)), collapse = ", "), "\n\n")


# ─── 4. MERGE DEATHS + HEAT STRESS ──────────────────────────────────────────────
cat("4. Merging deaths with heat stress …\n")

analysis_dt <- merge(
  deaths_weekly,
  heat_dt[, .(epi_year, epi_week, province, mean_at, max_at, mean_wbgt, max_wbgt)],
  by    = c("epi_year", "epi_week", "province"),
  all.x = TRUE
)

setorder(analysis_dt, province, epi_year, epi_week)

# Restrict to summer for main heat analysis
summer_dt <- analysis_dt[is_summer == TRUE]

cat("   Merged records (all years, all weeks):",
    format(nrow(analysis_dt), big.mark = ","), "\n")
cat("   Austral summer weeks only:",
    format(nrow(summer_dt),   big.mark = ","), "\n\n")


# ─── 5. HEAT STRESS DESCRIPTIVE STATISTICS ─────────────────────────────────────
cat("5. Computing descriptive heat stress statistics …\n")

# National (or top-level provincial) weekly average AT
national_heat <- analysis_dt[
  province == "South Africa" | length(unique(province)) == 1
]
if (nrow(national_heat) == 0) {
  national_heat <- analysis_dt[, .(
    mean_at   = mean(mean_at,   na.rm = TRUE),
    max_at    = mean(max_at,    na.rm = TRUE),
    mean_wbgt = mean(mean_wbgt, na.rm = TRUE),
    max_wbgt  = mean(max_wbgt,  na.rm = TRUE),
    deaths    = sum(deaths,     na.rm = TRUE)
  ), by = .(epi_year, epi_week, week_start, is_summer, month)]
  setorder(national_heat, epi_year, epi_week)
}

# Percentile thresholds for heat stress (over summer weeks only)
at_p90 <- quantile(national_heat[is_summer == TRUE, mean_at], 0.90, na.rm = TRUE)
at_p95 <- quantile(national_heat[is_summer == TRUE, mean_at], 0.95, na.rm = TRUE)

cat(sprintf("   Austral summer mean AT — p90: %.1f°C  p95: %.1f°C\n",
            at_p90, at_p95))


# ─── 6. FIGURES ──────────────────────────────────────────────────────────────
cat("6. Creating figures …\n")

## ── Fig A: Weekly heat stress heatmap (AT), 2010-2022 ──────────────────────
plot_data_nh <- national_heat[!is.na(mean_at) & epi_week <= 52]

fig_heat_heatmap <- ggplot(
  plot_data_nh,
  aes(x = epi_week, y = factor(epi_year), fill = mean_at)
) +
  geom_tile() +
  scale_fill_gradient2(
    low      = "#2166ac",
    mid      = "#ffffbf",
    high     = "#d73027",
    midpoint = 20,
    name     = "Apparent\nTemp (°C)"
  ) +
  annotate("rect", xmin = 1, xmax = 17, ymin = -Inf, ymax = Inf,
           alpha = 0.05, fill = "red") +
  annotate("rect", xmin = 36, xmax = 52, ymin = -Inf, ymax = Inf,
           alpha = 0.05, fill = "red") +
  labs(
    title    = "Weekly Mean Apparent Temperature — South Africa, 2010–2022",
    subtitle = "Shaded areas = extended austral summer (Sep–Apr); Red = hot, Blue = cool",
    x        = "Epidemiological week",
    y        = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank())

## ── Fig B: Deaths vs heat stress scatter (summer only) ─────────────────────
nh_summer <- national_heat[is_summer == TRUE & !is.na(mean_at) & !is.na(deaths)]

fig_heat_scatter <- ggplot(nh_summer, aes(x = mean_at, y = deaths)) +
  geom_point(aes(colour = factor(epi_year)), alpha = 0.55, size = 1.8) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5),
              colour = "#d73027", se = TRUE, fill = "#d7302740") +
  geom_vline(xintercept = at_p90, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = at_p95, linetype = "dotted", colour = "grey20") +
  annotate("text", x = at_p90 + 0.3, y = min(nh_summer$deaths, na.rm = TRUE),
           label = "p90", hjust = 0, size = 3, colour = "grey40") +
  annotate("text", x = at_p95 + 0.3, y = min(nh_summer$deaths, na.rm = TRUE),
           label = "p95", hjust = 0, size = 3, colour = "grey20") +
  labs(
    title    = "Excess deaths vs heat stress — Austral summer weeks (Sep–Apr)",
    subtitle = paste0("Weekly all-cause deaths vs mean apparent temperature. ",
                      "Data source: ", data_source_label),
    x        = "Mean apparent temperature (°C)",
    y        = "Weekly all-cause deaths",
    colour   = "Year"
  ) +
  theme_minimal(base_size = 12)

## ── Fig C: Annual summer deaths + heat stress (two-panel, no dual axis) ─────
annual_summer <- national_heat[is_summer == TRUE, .(
  deaths_total   = sum(deaths,    na.rm = TRUE),
  mean_at_summer = mean(mean_at,  na.rm = TRUE),
  max_at_summer  = max(max_at,    na.rm = TRUE),
  n_weeks        = .N
), by = epi_year]

# Build long form for two-panel facet (avoids dual-axis closure issues)
annual_long <- rbind(
  annual_summer[, .(epi_year, value = deaths_total,
                    panel = "Total all-cause deaths (Sep–Apr)")],
  annual_summer[, .(epi_year, value = mean_at_summer,
                    panel = "Mean apparent temperature (°C)")]
)

fig_annual_summer <- ggplot(annual_long, aes(x = epi_year, y = value)) +
  geom_col(data = annual_long[panel == "Total all-cause deaths (Sep–Apr)"],
           fill = "#d6604d", alpha = 0.75) +
  geom_line(data = annual_long[panel == "Mean apparent temperature (°C)"],
            colour = "#4393c3", linewidth = 1.3) +
  geom_point(data = annual_long[panel == "Mean apparent temperature (°C)"],
             colour = "#2166ac", size = 2) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  labs(
    title    = "Annual austral-summer all-cause deaths and mean heat stress, 2010–2022",
    subtitle = "Top: total deaths Sep–Apr; Bottom: mean apparent temperature",
    x        = "Year",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))


# ─── 7. DLNM EXCESS MORTALITY MODEL ──────────────────────────────────────────
cat("7. Fitting Distributed Lag Non-linear Model (DLNM) …\n")

dlnm_result     <- NULL
fig_dlnm_contour <- NULL
fig_dlnm_lag0   <- NULL
dlnm_available  <- FALSE
attrib_dt       <- NULL

if (has_dlnm && has_MASS && has_splines) {
  tryCatch({
    mod_data <- copy(national_heat)
    mod_data <- mod_data[!is.na(mean_at) & !is.na(deaths) & epi_week <= 52]
    setorder(mod_data, epi_year, epi_week)

    # Cross-basis: natural spline in temperature dimension (4 df)
    #              natural spline in lag dimension (3 df), max lag = 3 weeks
    cb_at <- crossbasis(
      mod_data$mean_at,
      lag   = 3,
      argvar = list(fun = "ns", df = 4, Boundary.knots = range(mod_data$mean_at, na.rm = TRUE)),
      arglag = list(fun = "ns", df = 3)
    )

    # Time variables for confounding control
    mod_data[, week_index := .I]
    mod_data[, sin52 := sin(2 * pi * epi_week / 52)]
    mod_data[, cos52 := cos(2 * pi * epi_week / 52)]

    # Quasi-Poisson GLM with cross-basis + seasonality + long-term trend
    dlnm_model <- glm(
      deaths ~ cb_at + ns(week_index, df = 7) + sin52 + cos52,
      data   = mod_data,
      family = quasipoisson(link = "log")
    )

    cat(sprintf("   DLNM quasi-Poisson: deviance = %.1f  df.resid = %d\n",
                deviance(dlnm_model), df.residual(dlnm_model)))

    # Prediction over exposure range at lag 0–3
    cp_at <- crosspred(
      cb_at,
      dlnm_model,
      at   = seq(min(mod_data$mean_at, na.rm = TRUE),
                 max(mod_data$mean_at, na.rm = TRUE),
                 length.out = 80),
      cen  = quantile(mod_data$mean_at, 0.10, na.rm = TRUE),  # Ref = 10th percentile
      cumul = TRUE
    )

    dlnm_result    <- cp_at
    dlnm_available <- TRUE

    ## ── Cumulative exposure-response curve at lag 0 ──────────────────────────
    cen_val     <- quantile(mod_data$mean_at, 0.10, na.rm = TRUE)
    pred_df <- data.frame(
      at_val = cp_at$predvar,
      rr     = exp(cp_at$matfit[, "lag0"]),
      rr_lo  = exp(cp_at$matfit[, "lag0"] - 1.96 * cp_at$matse[, "lag0"]),
      rr_hi  = exp(cp_at$matfit[, "lag0"] + 1.96 * cp_at$matse[, "lag0"])
    )

    fig_dlnm_lag0 <- ggplot(pred_df, aes(x = at_val)) +
      geom_ribbon(aes(ymin = rr_lo, ymax = rr_hi), fill = "#d7302720") +
      geom_line(aes(y = rr), colour = "#d73027", linewidth = 1.2) +
      geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
      geom_vline(xintercept = cen_val, linetype = "dotted", colour = "#2166ac") +
      annotate("text", x = cen_val + 0.3, y = max(pred_df$rr_hi, na.rm = TRUE),
               label = sprintf("Ref = %.1f°C (p10)", cen_val),
               hjust = 0, size = 3.2, colour = "#2166ac") +
      geom_vline(xintercept = at_p90, linetype = "dashed", colour = "grey50", linewidth = 0.7) +
      annotate("text", x = at_p90 + 0.3, y = 1.02,
               label = "p90", size = 3, colour = "grey40") +
      labs(
        title    = "Exposure–response: weekly mean apparent temperature vs relative risk of death",
        subtitle = sprintf("DLNM quasi-Poisson — lag 0 slice; reference = %.1f°C (10th percentile)",
                           cen_val),
        x        = "Mean apparent temperature (°C)",
        y        = "Relative Risk (RR)"
      ) +
      theme_minimal(base_size = 12)

    ## ── Attributable fraction (AF) for exposure above p75 ────────────────────
    at_p75 <- quantile(mod_data$mean_at, 0.75, na.rm = TRUE)

    af_dt <- mod_data[mean_at >= at_p75, .(
      epi_year, epi_week, week_start, mean_at, deaths
    )]

    if (nrow(af_dt) > 0) {
      # For each hot week: AF = 1 - 1/RR (cumulative over 4-week lag)
      rr_vec <- exp(cp_at$allfit[
        findInterval(af_dt$mean_at, cp_at$predvar)
      ])
      af_dt[, rr     := rr_vec]
      af_dt[, af     := 1 - 1 / rr]
      af_dt[, attr_deaths := round(deaths * af)]

      attrib_dt <- af_dt

      total_attr <- sum(af_dt$attr_deaths, na.rm = TRUE)
      cat(sprintf("   Attributable deaths (AT ≥ p75, 2010–2022): %s\n",
                  format(total_attr, big.mark = ",")))
    }

    cat("   DLNM model fitted successfully.\n")
  },
  error = function(e) {
    cat("   DLNM error:", conditionMessage(e), "\n")
    cat("   Install dlnm package: install.packages('dlnm')\n")
  })
} else {
  cat("   dlnm/MASS/splines not available. Install: install.packages('dlnm')\n")
}


# ─── 8. SIMPLE EXCESS DEATH CALCULATION (p90 threshold) ─────────────────────
cat("8. Simple threshold-based excess death estimates …\n")

# Compare observed deaths in hot weeks (mean AT > p90) vs expected baseline
# Expected = median of same epi_week across cool years (baseline: 2010–2019)
baseline_bywk <- national_heat[
  epi_year %in% 2010:2019 & mean_at <= at_p90,
  .(expected_median = median(deaths, na.rm = TRUE),
    expected_mean   = mean(deaths,   na.rm = TRUE)),
  by = epi_week
]

# Hot weeks in 2010-2022
hot_weeks_dt <- national_heat[
  is_summer == TRUE & mean_at >= at_p90 & !is.na(deaths)
]

hot_weeks_dt <- merge(hot_weeks_dt, baseline_bywk, by = "epi_week", all.x = TRUE)
hot_weeks_dt[, excess_vs_median := deaths - expected_median]
hot_weeks_dt[, excess_vs_mean   := deaths - expected_mean]

excess_summary <- hot_weeks_dt[, .(
  n_hot_weeks   = .N,
  total_deaths  = sum(deaths,           na.rm = TRUE),
  total_excess  = sum(pmax(excess_vs_median, 0), na.rm = TRUE),
  mean_excess_pw = mean(pmax(excess_vs_median, 0), na.rm = TRUE)
), by = epi_year]

cat("   Threshold-based excess summary (hot weeks = AT ≥ p90):\n")
print(as.data.frame(excess_summary))
cat("\n")


# ─── 9. SAVE RESULTS ──────────────────────────────────────────────────────────
cat("9. Saving results to heat_results.rda …\n")

save(
  analysis_dt,
  national_heat,
  summer_dt,
  annual_summer,
  hot_weeks_dt,
  excess_summary,
  attrib_dt,
  at_p90,
  at_p95,
  data_source_label,
  dlnm_result,
  dlnm_available,
  fig_heat_heatmap,
  fig_heat_scatter,
  fig_annual_summer,
  fig_dlnm_lag0,
  file = "projects/heat_excess_mortality/heat_results.rda"
)

cat("\n┌────────────────────────────────────────────────────────────┐\n")
cat("│  heat_wrangling.R complete — results saved to               │\n")
cat("│  projects/heat_excess_mortality/heat_results.rda            │\n")
cat("└────────────────────────────────────────────────────────────┘\n")
