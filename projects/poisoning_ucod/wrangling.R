suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(ggplot2)
  library(scales)
})

project_dir <- "projects/poisoning_ucod"
output_rda  <- file.path(project_dir, "ucod_results.rda")

# ── Load and decode ──────────────────────────────────────────────────────────
input_file <- "LGH_MasterFile_preCollapsedAll.feather"
if (!file.exists(input_file)) stop("Input not found: ", input_file)
message("Reading: ", input_file)
dt <- as.data.table(read_feather(input_file))
for (v in names(dt)) {
  if (inherits(dt[[v]], "haven_labelled")) {
    raw <- unclass(dt[[v]])
    dt[, (v) := if (is.numeric(raw)) as.integer(raw) else as.character(raw)]
  }
}

# ── Restrict to Gauteng (7) and KwaZulu-Natal (5) ───────────────────────────
PROV_LU <- c("5" = "KwaZulu-Natal", "7" = "Gauteng")
gk <- dt[as.character(ResProvince) %in% names(PROV_LU) & !is.na(DeathYear)]
gk[, province := PROV_LU[as.character(ResProvince)]]
gk[, year     := as.integer(DeathYear)]
message("GP + KZN records: ", format(nrow(gk), big.mark = ","))

# ── Standardise cause fields: sentinel values → NA ──────────────────────────
# 888 = blank/not filled on the certificate
# 111 in an Injury field = no injury-nature code associated
CAUSE_COLS <- c("CauseA", "CauseB", "CauseC", "CauseD", "UnderlyingCause")
for (v in CAUSE_COLS) {
  gk[, (v) := {
    x <- trimws(toupper(as.character(get(v))))
    x[x %in% c("888", "111", "999", "")] <- NA_character_
    x
  }]
}

# ── Poisoning flag: X40–X49, X60–X69, X85, Y10–Y19 ──────────────────────────
POIS_RE <- "^(X4[0-9]|X6[0-9]|X85|Y1[0-9])"
is_pois <- function(x) !is.na(x) & grepl(POIS_RE, x, ignore.case = FALSE)

# ── Derived fields ───────────────────────────────────────────────────────────
# Last non-NA cause field in the Part I chain (A → B → C → D)
gk[, last_cause := fcase(
  !is.na(CauseD), CauseD,
  !is.na(CauseC), CauseC,
  !is.na(CauseB), CauseB,
  !is.na(CauseA), CauseA,
  default = NA_character_
)]
# Number of Part I lines filled (0–4)
gk[, n_lines_filled := as.integer(!is.na(CauseA)) + as.integer(!is.na(CauseB)) +
                       as.integer(!is.na(CauseC)) + as.integer(!is.na(CauseD))]

# ── Four identification approaches ──────────────────────────────────────────
gk[, line1_pois    := is_pois(CauseA)]       # First line (immediate cause)
gk[, line4_pois    := is_pois(CauseD)]       # Line 4 specifically (often blank)
gk[, lastline_pois := is_pois(last_cause)]   # Last reported (non-empty) line
gk[, ucod_pois     := is_pois(UnderlyingCause)]  # Reference: UCOD
gk[, anyline_pois  := is_pois(CauseA) | is_pois(CauseB) |
                       is_pois(CauseC) | is_pois(CauseD)]

APPROACHES <- c("Line 1 (CauseA)", "Line 4 (CauseD)", "Last reported line", "UCOD")
FLAGS      <- c("line1_pois",       "line4_pois",       "lastline_pois",      "ucod_pois")

APPROACH_COLS <- c(
  "Line 1 (CauseA)"    = "#2166AC",
  "Line 4 (CauseD)"    = "#74ADD1",
  "Last reported line" = "#F46D43",
  "UCOD"               = "#1A9641"
)

# ── Position of poisoning code in the Part I chain (UCOD-pois records only) ──
# Identifies which line the coder placed the X/Y code on
gk[ucod_pois == TRUE,
   pois_line_pos := fcase(
     is_pois(CauseA), "Line 1",
     is_pois(CauseB), "Line 2",
     is_pois(CauseC), "Line 3",
     is_pois(CauseD), "Line 4",
     default = "Not in Part I"
   )]

# ── TABLE 1: Poisoning deaths captured, by approach and province ─────────────
ucod_by_prov <- gk[ucod_pois == TRUE, .N, by = province]
ucod_total   <- sum(ucod_by_prov$N)

tbl_capture <- rbindlist(lapply(seq_along(APPROACHES), function(i) {
  n_gp  <- gk[province == "Gauteng",       sum(get(FLAGS[i]), na.rm = TRUE)]
  n_kzn <- gk[province == "KwaZulu-Natal", sum(get(FLAGS[i]), na.rm = TRUE)]
  n_tot <- n_gp + n_kzn
  u_gp  <- ucod_by_prov[province == "Gauteng",       N]
  u_kzn <- ucod_by_prov[province == "KwaZulu-Natal", N]
  data.table(
    Approach               = APPROACHES[i],
    `Gauteng (n)`          = format(n_gp,  big.mark = ","),
    `Gauteng % of UCOD`    = paste0(round(100 * n_gp  / u_gp,  1), "%"),
    `KZN (n)`              = format(n_kzn, big.mark = ","),
    `KZN % of UCOD`        = paste0(round(100 * n_kzn / u_kzn, 1), "%"),
    `Total (n)`            = format(n_tot, big.mark = ","),
    `Total % of UCOD`      = paste0(round(100 * n_tot / ucod_total, 1), "%")
  )
}))

# ── TABLE 2: Discordance (sensitivity / specificity vs UCOD) ────────────────
tbl_discordance <- rbindlist(lapply(seq_along(APPROACHES), function(i) {
  tp <- gk[get(FLAGS[i]) == TRUE  & ucod_pois == TRUE,  .N]
  fp <- gk[get(FLAGS[i]) == TRUE  & ucod_pois == FALSE, .N]
  fn <- gk[get(FLAGS[i]) == FALSE & ucod_pois == TRUE,  .N]
  tn <- gk[get(FLAGS[i]) == FALSE & ucod_pois == FALSE, .N]
  data.table(
    Approach          = APPROACHES[i],
    `True positive`   = format(tp, big.mark = ","),
    `False positive`  = format(fp, big.mark = ","),
    `False negative`  = format(fn, big.mark = ","),
    `Sensitivity (%)`  = round(100 * tp / (tp + fn), 1),
    `Specificity (%)`  = round(100 * tn / (tn + fp), 1)
  )
}))

# ── TABLE 3: Position of poisoning code in the Part I chain ─────────────────
POS_LEVELS <- c("Line 1", "Line 2", "Line 3", "Line 4", "Not in Part I")

tbl_chain_pos <- gk[ucod_pois == TRUE & !is.na(pois_line_pos),
                    .N, by = .(province, pois_line_pos)]
tbl_chain_pos[, pois_line_pos := factor(pois_line_pos, levels = POS_LEVELS)]
tbl_chain_pos[, pct := round(100 * N / sum(N), 1), by = province]
setorder(tbl_chain_pos, province, pois_line_pos)

tbl_chain_wide <- dcast(tbl_chain_pos[, .(pois_line_pos, province, N)],
                        pois_line_pos ~ province, value.var = "N", fill = 0L)
for (pv in c("Gauteng", "KwaZulu-Natal")) {
  tot <- sum(tbl_chain_wide[[pv]])
  tbl_chain_wide[, (paste0("% ", pv)) := round(100 * get(pv) / tot, 1)]
}
tbl_chain_wide[, Total := Gauteng + `KwaZulu-Natal`]
setnames(tbl_chain_wide, "pois_line_pos", "Code position (Part I)")

# ── Lines-filled distribution ────────────────────────────────────────────────
tbl_lines_filled <- gk[, .N, by = .(province, n_lines_filled)]
tbl_lines_filled[, pct := round(100 * N / sum(N), 1), by = province]
setorder(tbl_lines_filled, province, n_lines_filled)

# ── Illustrative discordant records ─────────────────────────────────────────
SHOW_COLS <- c("CauseA", "CauseB", "CauseC", "CauseD", "UnderlyingCause", "province", "year")

dash_na <- function(dt_in) {
  dt_out <- copy(dt_in)
  for (v in c("CauseA", "CauseB", "CauseC", "CauseD")) {
    dt_out[is.na(get(v)), (v) := "\u2014"]
  }
  dt_out
}

# Misses: UCOD = poisoning, Line 1 ≠ poisoning
examples_miss <- dash_na(
  gk[ucod_pois == TRUE & line1_pois == FALSE & !is.na(CauseA),
     .SD, .SDcols = SHOW_COLS][1:min(12, .N)]
)

# False positives: Line 1 = poisoning, UCOD ≠ poisoning
examples_fp <- dash_na(
  gk[line1_pois == TRUE & ucod_pois == FALSE & !is.na(CauseA),
     .SD, .SDcols = SHOW_COLS][1:min(10, .N)]
)

# Buried cases: poisoning on Line 3 (visible to "any line" but not Line 1 or last-line)
examples_buried <- dash_na(
  gk[ucod_pois == TRUE & !is_pois(CauseA) & !is_pois(CauseB) & is_pois(CauseC),
     .SD, .SDcols = SHOW_COLS][1:min(10, .N)]
)

# ── Annual counts by approach ────────────────────────────────────────────────
annual_cap <- gk[, .(
  `Line 1 (CauseA)`    = sum(line1_pois,    na.rm = TRUE),
  `Line 4 (CauseD)`    = sum(line4_pois,    na.rm = TRUE),
  `Last reported line` = sum(lastline_pois, na.rm = TRUE),
  `UCOD`               = sum(ucod_pois,     na.rm = TRUE)
), by = year]
setorder(annual_cap, year)

annual_long <- melt(annual_cap, id.vars = "year",
                    variable.name = "Approach", value.name = "Deaths")
annual_long[, Approach := factor(Approach, levels = APPROACHES)]

# ── Summary statistics for inline text ──────────────────────────────────────
n_gk              <- nrow(gk)
n_ucod_pois       <- gk[ucod_pois == TRUE,  .N]
n_line1_pois      <- gk[line1_pois == TRUE, .N]
n_ucod_miss_line1 <- gk[ucod_pois == TRUE & line1_pois == FALSE, .N]
n_ucod_miss_last  <- gk[ucod_pois == TRUE & lastline_pois == FALSE, .N]
n_fp_line1        <- gk[line1_pois == TRUE & ucod_pois == FALSE, .N]
n_ucod_no_part1   <- gk[ucod_pois == TRUE & anyline_pois == FALSE, .N]
n_gp_ucod         <- gk[ucod_pois == TRUE & province == "Gauteng", .N]
n_kzn_ucod        <- gk[ucod_pois == TRUE & province == "KwaZulu-Natal", .N]
yr_range          <- range(gk$year, na.rm = TRUE)

summary_stats <- list(
  n_gk              = n_gk,
  n_ucod_pois       = n_ucod_pois,
  n_gp_ucod         = n_gp_ucod,
  n_kzn_ucod        = n_kzn_ucod,
  n_line1_pois      = n_line1_pois,
  n_ucod_miss_line1 = n_ucod_miss_line1,
  pct_miss_line1    = round(100 * n_ucod_miss_line1 / n_ucod_pois, 1),
  n_ucod_miss_last  = n_ucod_miss_last,
  pct_miss_last     = round(100 * n_ucod_miss_last  / n_ucod_pois, 1),
  n_fp_line1        = n_fp_line1,
  pct_fp_line1      = round(100 * n_fp_line1 / n_line1_pois, 1),
  n_ucod_no_part1   = n_ucod_no_part1,
  pct_ucod_no_part1 = round(100 * n_ucod_no_part1 / n_ucod_pois, 1),
  yr_range          = yr_range
)

# ── FIGURES ──────────────────────────────────────────────────────────────────

# Fig 1: Capture comparison — bar chart by province
fig_capture <- {
  plot_dt <- rbindlist(lapply(seq_along(APPROACHES), function(i) {
    gk[, .(N = sum(get(FLAGS[i]), na.rm = TRUE)), by = province][
       , Approach := APPROACHES[i]]
  }))
  plot_dt[, Approach := factor(Approach, levels = APPROACHES)]

  ggplot(plot_dt, aes(x = Approach, y = N, fill = Approach)) +
    geom_col(alpha = 0.85, width = 0.65) +
    geom_text(aes(label = format(N, big.mark = ",")),
              vjust = -0.35, size = 3) +
    facet_wrap(~ province, scales = "free_y") +
    scale_fill_manual(values = APPROACH_COLS, guide = "none") +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.16))) +
    scale_x_discrete(labels = function(x) gsub(" \\(", "\n(", x)) +
    labs(x = NULL, y = "Poisoning deaths identified") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text         = element_text(face = "bold"),
          axis.text.x        = element_text(size = 9))
}

# Fig 2: Where in the Part I chain is the poisoning code? (100% stacked bar)
fig_chain_pos <- {
  LINE_COLS <- c(
    "Line 1"        = "#2166AC",
    "Line 2"        = "#74ADD1",
    "Line 3"        = "#FDAE61",
    "Line 4"        = "#F46D43",
    "Not in Part I" = "#AAAAAA"
  )
  ggplot(tbl_chain_pos, aes(x = province, y = N, fill = pois_line_pos)) +
    geom_col(position = "fill", alpha = 0.85) +
    geom_text(aes(label = ifelse(pct >= 2, paste0(pct, "%"), "")),
              position = position_fill(vjust = 0.5),
              colour = "white", fontface = "bold", size = 3.8) +
    scale_fill_manual(values = LINE_COLS, name = "Position of\npoisoning code") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(x = NULL, y = "Proportion of UCOD-poisoning records") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank())
}

# Fig 3: Annual counts by approach over time
fig_annual <- {
  ggplot(annual_long, aes(x = year, y = Deaths, colour = Approach,
                          linetype = Approach)) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = APPROACH_COLS, name = NULL) +
    scale_linetype_manual(
      values = c("Line 1 (CauseA)"    = "dashed",
                 "Line 4 (CauseD)"    = "dotted",
                 "Last reported line" = "dotdash",
                 "UCOD"               = "solid"),
      name = NULL) +
    scale_x_continuous(breaks = seq(yr_range[1], yr_range[2], 3)) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.07))) +
    labs(x = NULL, y = "Poisoning deaths identified per year") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          legend.position  = "bottom",
          legend.key.width = unit(1.5, "cm"))
}

# Fig 4: Sensitivity and specificity (vs UCOD as reference) — dot plot
disc_sens <- data.table(
  Approach    = APPROACHES,
  Sensitivity = tbl_discordance$`Sensitivity (%)`,
  Specificity = tbl_discordance$`Specificity (%)`
)
disc_long <- melt(disc_sens, id.vars = "Approach",
                  variable.name = "Metric", value.name = "Value")
disc_long[, Approach := factor(Approach, levels = rev(APPROACHES))]

fig_disc <- {
  ggplot(disc_long, aes(y = Approach, x = Value, colour = Metric)) +
    geom_segment(aes(xend = Value, yend = Approach, x = 0),
                 colour = "grey82", linewidth = 0.8) +
    geom_point(size = 4.5, alpha = 0.9) +
    facet_wrap(~ Metric) +
    scale_colour_manual(values = c(Sensitivity = "firebrick",
                                   Specificity = "steelblue"),
                        guide = "none") +
    scale_x_continuous(limits = c(0, 100),
                       labels = function(x) paste0(x, "%")) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          strip.text       = element_text(face = "bold"))
}

# Fig 5: Number of Part I lines filled (completeness of certificate)
fig_lines_filled <- {
  tbl_lines_filled[, n_lines_label := paste0(n_lines_filled, " line",
                                             ifelse(n_lines_filled == 1, "", "s"))]
  tbl_lines_filled[, n_lines_label := factor(n_lines_label,
    levels = paste0(0:4, c(" lines", " line", " lines", " lines", " lines")))]

  ggplot(tbl_lines_filled, aes(x = n_lines_filled, y = pct, fill = province)) +
    geom_col(position = position_dodge(width = 0.7), alpha = 0.85, width = 0.65) +
    geom_text(aes(label = paste0(pct, "%")),
              position = position_dodge(width = 0.7),
              vjust = -0.35, size = 3) +
    scale_fill_manual(values = c(Gauteng = "steelblue",
                                 `KwaZulu-Natal` = "firebrick"),
                      name = NULL) +
    scale_x_continuous(breaks = 0:4,
                       labels = c("0\n(blank)", "1", "2", "3", "4")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.14)),
                       labels = function(x) paste0(x, "%")) +
    labs(x = "Number of Part I lines filled",
         y = "% of all deaths") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position    = "bottom")
}

# ── DECISION SUMMARY TABLE (combines counts + sensitivity + recommendation) ──
VERDICTS <- c(
  "Line 1 (CauseA)"    = "Undercounts: misses all deaths where the poisoning code is not the immediate cause",
  "Line 4 (CauseD)"    = "Largely uninformative: most certifiers do not fill Line 4",
  "Last reported line" = "Better than Line 1 but still misses intermediate-chain poisoning codes",
  "UCOD"               = "Reference standard \u2014 captures all certificate information via ICD-10 selection rules"
)

tbl_decision <- rbindlist(lapply(seq_along(APPROACHES), function(i) {
  ap  <- APPROACHES[i]
  fl  <- FLAGS[i]
  n_gp  <- gk[province == "Gauteng",       sum(get(fl), na.rm = TRUE)]
  n_kzn <- gk[province == "KwaZulu-Natal", sum(get(fl), na.rm = TRUE)]
  n_tot <- n_gp + n_kzn
  u_gp  <- ucod_by_prov[province == "Gauteng",       N]
  u_kzn <- ucod_by_prov[province == "KwaZulu-Natal", N]
  tp <- gk[get(fl) == TRUE  & ucod_pois == TRUE,  .N]
  fp <- gk[get(fl) == TRUE  & ucod_pois == FALSE, .N]
  fn <- gk[get(fl) == FALSE & ucod_pois == TRUE,  .N]
  tn <- gk[get(fl) == FALSE & ucod_pois == FALSE, .N]
  data.table(
    Option              = paste0("Option ", i),
    `Approach / field`  = ap,
    `GP (n)`            = n_gp,
    `KZN (n)`           = n_kzn,
    `GP + KZN total`    = n_tot,
    `% of UCOD total`   = round(100 * n_tot / ucod_total, 1),
    `Sensitivity (%)`   = round(100 * tp / (tp + fn), 1),
    `False positives`   = fp,
    `Specificity (%)`   = round(100 * tn / (tn + fp), 1),
    Consequence         = VERDICTS[ap]
  )
}))

# ── Save ─────────────────────────────────────────────────────────────────────
save(
  summary_stats,
  tbl_capture, tbl_discordance, tbl_chain_wide, tbl_chain_pos,
  tbl_lines_filled, tbl_decision,
  examples_miss, examples_fp, examples_buried,
  annual_cap, annual_long,
  fig_capture, fig_chain_pos, fig_annual, fig_disc, fig_lines_filled,
  file = output_rda
)
message("Saved: ", output_rda)
