suppressPackageStartupMessages({
  library(arrow); library(data.table); library(ggplot2); library(scales)
})

project_dir <- "projects/yll_comparison"
input_file  <- "Deaths2022_MRCversionFINAL.feather"
output_rda  <- file.path(project_dir, "yll_results.rda")

if (!file.exists(input_file)) stop("Input file not found: ", input_file)

# ── Load & decode Stata-labelled types ───────────────────────────────────────
dt <- as.data.table(read_feather(input_file))
for (v in names(dt)) {
  if (inherits(dt[[v]], "haven_labelled")) {
    raw <- unclass(dt[[v]])
    if (is.numeric(raw)) dt[, (v) := as.integer(raw)]
    else                 dt[, (v) := as.character(raw)]
  }
}

dt[, icd_clean := toupper(gsub("[^A-Z0-9]", "", as.character(UnderlyingCause)))]
dt[, year      := as.integer(DeathYear)]

# ── Sex & age ────────────────────────────────────────────────────────────────
sex_lu <- c("1" = "Male", "2" = "Female", "3" = "Unknown", "9" = "Unknown")
dt[, sex_label := factor(
  fifelse(as.character(Sex) %in% names(sex_lu), sex_lu[as.character(Sex)], "Unknown"),
  levels = c("Male", "Female", "Unknown")
)]

AGE_BREAKS <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf)
AGE_LABELS <- c("<1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                "70-74","75-79","80-84","85+")
dt[, age_grp := cut(age, breaks = AGE_BREAKS, labels = AGE_LABELS, right = FALSE)]

# ── Province ─────────────────────────────────────────────────────────────────
prov_lu <- c("1"="Western Cape","2"="Eastern Cape","3"="Northern Cape",
             "4"="Free State","5"="KwaZulu-Natal","6"="North West",
             "7"="Gauteng","8"="Mpumalanga","9"="Limpopo",
             "98"="Outside SA","99"="Unspecified")
dt[, province := fifelse(
  as.character(ResProvince) %in% names(prov_lu),
  prov_lu[as.character(ResProvince)], "Unspecified"
)]

# ════════════════════════════════════════════════════════════════════════════
#  SECTION 1: SA NMC (Notifiable Medical Conditions) — ICD-10 Mapping Table
#
#  Source: Government Gazette No. 38764, 12 June 2015 — Regulations relating
#  to the surveillance and the control of notifiable medical conditions
#  (National Health Act, 2003 — Act No. 61 of 2003).
#  ICD-10 codes represent the most plausible underlying-cause-of-death code
#  as assigned in Stats SA vital registration data.
# ════════════════════════════════════════════════════════════════════════════

nmc_map <- data.table(
  condition = c(
    # ── Vaccine-preventable NMCs ─────────────────────────────────────────────
    "Diphtheria",
    "Pertussis (whooping cough)",
    "Tetanus (neonatal)",
    "Tetanus (other)",
    "Poliomyelitis (wild/vaccine-derived)",
    "Measles",
    "Rubella",
    "Congenital rubella syndrome",
    "Hepatitis B (acute)",
    "Hepatitis B (chronic / NOS)",
    "Meningococcal disease",
    "Influenza (novel subtypes / pandemic)",
    "Yellow fever",
    # ── Enteric / waterborne NMCs ────────────────────────────────────────────
    "Cholera",
    "Typhoid fever",
    "Shigellosis (bacillary dysentery)",
    "Non-typhoidal Salmonella (food-borne)",
    "Hepatitis A",
    "Viral hepatitis E",
    # ── Vector-borne NMCs ────────────────────────────────────────────────────
    "Malaria",
    "Dengue fever",
    "Rift Valley fever",
    "Plague",
    # ── Zoonotic / contact NMCs ──────────────────────────────────────────────
    "Rabies",
    "Brucellosis",
    "Anthrax",
    "Leptospirosis",
    "Haemorrhagic fever (viral / NOS)",
    # ── Respiratory NMCs ─────────────────────────────────────────────────────
    "Legionellosis",
    "SARS / COVID-19",
    "Middle East respiratory syndrome (MERS)",
    # ── High-burden communicable ─────────────────────────────────────────────
    "Tuberculosis (all forms)",
    "HIV/AIDS",
    # ── STI / blood-borne NMCs ───────────────────────────────────────────────
    "Syphilis (congenital)",
    "Neonatal ophthalmia (gonococcal/chlamydia)",
    "Hepatitis C",
    # ── Environmental / toxic / external NMCs ────────────────────────────────
    "Poisoning — accidental / unspecified",
    "Poisoning — intentional self-harm",
    "Poisoning — assault",
    "Poisoning — undetermined intent",
    "Food-borne illness (notifiable outbreak)"
  ),
  icd10_codes = c(
    # ── Vaccine-preventable ──────────────────────────────────────────────────
    "A36",
    "A37",
    "A33",
    "A34-A35",
    "A80",
    "B05",
    "B06",
    "P35.0",
    "B16",
    "B17-B19",
    "A39",
    "J09-J11",
    "A95",
    # ── Enteric ─────────────────────────────────────────────────────────────
    "A00",
    "A01",
    "A03",
    "A02",
    "B15",
    "B17.2",
    # ── Vector-borne ─────────────────────────────────────────────────────────
    "B50-B54",
    "A90-A91",
    "A92.4",
    "A20",
    # ── Zoonotic ─────────────────────────────────────────────────────────────
    "A82",
    "A23",
    "A22",
    "A27",
    "A96-A99",
    # ── Respiratory ──────────────────────────────────────────────────────────
    "A48.1",
    "U07-U08 / J06.9",
    "B34.4",
    # ── High-burden ──────────────────────────────────────────────────────────
    "A15-A19",
    "B20-B24",
    # ── STI / blood-borne ────────────────────────────────────────────────────
    "A50",
    "P39.1 / A74.0",
    "B17.1",
    # ── Environmental / toxic ────────────────────────────────────────────────
    "X40-X49",
    "X60-X69",
    "X85",
    "Y10-Y19",
    "A02 / A05 / A06"
  ),
  nmc_category = c(
    rep("Vaccine-preventable", 13),
    rep("Enteric / waterborne", 6),
    rep("Vector-borne", 4),
    rep("Zoonotic / contact", 5),
    rep("Respiratory", 3),
    rep("High-burden communicable", 2),
    rep("STI / blood-borne", 3),
    rep("Environmental / toxic / external", 5)
  ),
  notes = c(
    "Vaccine: DTP / DTP-HepB-Hib",
    "Vaccine: DTP / DTP-HepB-Hib",
    "Vaccine: DTP",
    "Vaccine: DTP",
    "Vaccine: OPV/IPV",
    "Vaccine: Measles / MMR",
    "Vaccine: MMR",
    "Vaccine: MMR (maternal immunisation)",
    "Vaccine: HepB (birth dose + EPI)",
    "Vaccine: HepB",
    "Vaccine: MenC/MenACWY",
    "Vaccine: seasonal influenza",
    "Vaccine: YF (endemic risk only)",
    "Vaccine: OCV (oral cholera vaccine)",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "Vaccine: SP2 pilot / RTS,S",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "No routine vaccine in SA",
    "Vaccine: COVID-19 (emergency)",
    "No routine vaccine in SA",
    "Vaccine: BCG (protects against severe TB in children)",
    "Vaccine: none (PMTCT / ART programme)",
    "No vaccine; PMTCT / STI management",
    "No vaccine; ophthalmic prophylaxis",
    "No vaccine; HCV treatment",
    "Notifiable: food/pesticide/household",
    "Notifiable: self-inflicted toxic exposure",
    "Notifiable: homicidal poisoning",
    "Notifiable: undetermined toxic exposure",
    "Notifiable: outbreak reporting"
  )
)

# ════════════════════════════════════════════════════════════════════════════
#  SECTION 2: Cause-group assignment for YLL analysis
#
#  Groups are mutually exclusive; assignment follows the priority order
#  listed below (first match wins).
# ════════════════════════════════════════════════════════════════════════════

# Priority order: if a death matches multiple patterns, the FIRST match wins.
cause_group_defs <- list(
  # ── External / injury groups ─────────────────────────────────────────────
  "Poisoning (all intent)"        = "^(X4[0-9]|X6[0-9]|X85|Y1[0-9])",
  "Road traffic injuries"         = "^V",
  "Drowning (unintentional)"      = "^W6[5-9]|^W7[0-4]",
  "Falls"                         = "^W[01][0-9]",
  "Burns & fire"                  = "^X[01][0-9]",
  "Self-harm (non-poisoning)"     = "^(X7[1-9]|X8[0-4])",
  "Interpersonal violence"        = "^(X8[6-9]|X9[0-9]|Y0[0-9])",
  "Other external causes"         = "^(W|X|Y)",

  # ── Communicable / NMC groups ────────────────────────────────────────────
  "Tuberculosis"                  = "^A1[5-9]",
  "HIV/AIDS"                      = "^B2[0-4]",
  "Vaccine-preventable (excl TB/HIV)" = paste0(
    "^(A33|A34|A35|A36|A37|A39|A80|",
    "B05|B06|B16|B17|B18|B19|",
    "J09|J10|J11|A95|A00)"
  ),
  "Other NMC communicable"        = paste0(
    "^(A01|A02|A03|A20|A22|A23|A27|",
    "A48|A82|A90|A91|A92|A93|A94|A95|A96|A97|A98|A99|",
    "B15|B50|B51|B52|B53|B54|U07|U08|U09)"
  ),

  # ── Chronic / non-communicable ────────────────────────────────────────────
  "Malignant neoplasms (cancer)"  = "^C[0-9]",
  "Cardiovascular disease"        = "^I[0-9]",
  "Diabetes mellitus"             = "^E1[0-4]",
  "Chronic respiratory disease"   = "^J4[0-7]",
  "Other cause"                   = ".*"
)

assign_cause_group <- function(icd, group_defs) {
  out <- character(length(icd))
  unmatched <- seq_along(icd)
  for (gname in names(group_defs)) {
    if (length(unmatched) == 0L) break
    pat     <- group_defs[[gname]]
    matched <- unmatched[grepl(pat, icd[unmatched], perl = TRUE)]
    out[matched]   <- gname
    unmatched      <- setdiff(unmatched, matched)
  }
  out[out == ""] <- "Other cause"
  out
}

dt[, cause_group := assign_cause_group(icd_clean, cause_group_defs)]

# Ordered factor for display
GROUP_ORDER <- c(
  "Poisoning (all intent)",
  "Road traffic injuries",
  "Drowning (unintentional)",
  "Falls",
  "Burns & fire",
  "Self-harm (non-poisoning)",
  "Interpersonal violence",
  "Other external causes",
  "Tuberculosis",
  "HIV/AIDS",
  "Vaccine-preventable (excl TB/HIV)",
  "Other NMC communicable",
  "Malignant neoplasms (cancer)",
  "Cardiovascular disease",
  "Diabetes mellitus",
  "Chronic respiratory disease",
  "Other cause"
)
dt[, cause_group := factor(cause_group, levels = GROUP_ORDER)]

# ════════════════════════════════════════════════════════════════════════════
#  SECTION 3: South Africa national life table (2022)
#
#  Source: Statistics South Africa. "Life Tables for South Africa,
#  2009–2015." Statistical Release P0302.3 (2018), and Mid-Year Population
#  Estimates 2022, Table A4 (abridged life table, combined sex).
#  Values below are remaining life expectancy e(x) at the start of each
#  age interval. These are provisional approximations; replace with the
#  official Stats SA P0302.3 (2022 edition) values when published.
# ════════════════════════════════════════════════════════════════════════════

sa_lt <- data.table(
  age_lower = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50,
                55, 60, 65, 70, 75, 80, 85),
  # e(x): remaining life expectancy at age x (both sexes combined)
  ex_combined = c(63.6, 64.5, 60.9, 56.0, 51.2, 46.5, 41.9, 37.5, 33.1,
                  28.9, 24.9, 21.1, 17.5, 14.2, 11.3,  8.8,  6.8,  5.2, 4.2),
  ex_male     = c(60.2, 61.1, 57.5, 52.7, 47.9, 43.3, 38.8, 34.6, 30.4,
                  26.5, 22.6, 19.0, 15.8, 12.8, 10.2,  7.9,  6.1,  4.7, 3.8),
  ex_female   = c(67.0, 67.9, 64.3, 59.3, 54.5, 49.8, 45.1, 40.5, 36.0,
                  31.6, 27.5, 23.5, 19.5, 15.9, 12.7,  9.8,  7.5,  5.7, 4.6)
)

# Assign YLL to each death: vectorised linear interpolation of e(x) at exact age.
# For records with missing age, YLL = NA (excluded from YLL totals).
get_ex_vec <- function(age_vec, sex_vec) {
  n   <- length(age_vec)
  out <- rep(NA_real_, n)

  for (col in c("ex_combined", "ex_male", "ex_female")) {
    mask <- switch(col,
      ex_male     = !is.na(sex_vec) & sex_vec == "Male",
      ex_female   = !is.na(sex_vec) & sex_vec == "Female",
      ex_combined = !is.na(age_vec) & (is.na(sex_vec) | sex_vec == "Unknown")
    )
    # For male/female, only skip if age is also NA
    if (col != "ex_combined") mask <- mask & !is.na(age_vec)
    if (!any(mask)) next

    a   <- pmin(pmax(age_vec[mask], 0), 85)
    idx <- findInterval(a, sa_lt$age_lower, rightmost.closed = TRUE)
    idx <- pmax(1L, pmin(idx, nrow(sa_lt) - 1L))

    a_lo <- sa_lt$age_lower[idx]
    a_hi <- sa_lt$age_lower[idx + 1L]
    e_lo <- sa_lt[[col]][idx]
    e_hi <- sa_lt[[col]][idx + 1L]

    frac       <- (a - a_lo) / (a_hi - a_lo)
    out[mask]  <- e_lo + frac * (e_hi - e_lo)
  }
  out
}

dt[, YLL := get_ex_vec(age, as.character(sex_label))]

# ════════════════════════════════════════════════════════════════════════════
#  SECTION 4: Summary tables
# ════════════════════════════════════════════════════════════════════════════

# ── Overall YLL table (all years) ────────────────────────────────────────────
yll_overall <- dt[!is.na(year) & !is.na(cause_group), .(
  Deaths       = .N,
  YLL          = round(sum(YLL, na.rm = TRUE)),
  Mean_YLL     = round(mean(YLL, na.rm = TRUE), 1),
  YLL_with_age = sum(!is.na(YLL)),
  Pct_missing_age = round(100 * mean(is.na(YLL)), 1)
), by = cause_group]
setorder(yll_overall, -YLL)
yll_overall[, Pct_YLL := round(100 * YLL / sum(YLL), 1)]

# Formatted display table (all years)
tbl_yll_overall <- copy(yll_overall)
tbl_yll_overall[, Deaths_fmt := format(Deaths, big.mark = ",")]
tbl_yll_overall[, YLL_fmt    := format(YLL,    big.mark = ",")]
tbl_yll_overall[, Pct_YLL_fmt := paste0(Pct_YLL, "%")]
tbl_yll_overall_display <- tbl_yll_overall[, .(
  `Cause group`          = cause_group,
  `Deaths (N)`           = Deaths_fmt,
  `YLL (total)`          = YLL_fmt,
  `Mean YLL/death`       = Mean_YLL,
  `YLL (% of total)`     = Pct_YLL_fmt,
  `% missing age`        = Pct_missing_age
)]

# ── Most-recent-year (2022) YLL table ────────────────────────────────────────
RECENT_YEAR <- max(dt$year, na.rm = TRUE)
yll_recent <- dt[year == RECENT_YEAR & !is.na(cause_group), .(
  Deaths   = .N,
  YLL      = round(sum(YLL, na.rm = TRUE)),
  Mean_YLL = round(mean(YLL, na.rm = TRUE), 1)
), by = cause_group]
setorder(yll_recent, -YLL)
yll_recent[, Pct_YLL := round(100 * YLL / sum(YLL), 1)]

tbl_yll_recent_display <- yll_recent[, .(
  `Cause group`          = cause_group,
  `Deaths (N)`           = format(Deaths, big.mark = ","),
  `YLL (total)`          = format(YLL,    big.mark = ","),
  `Mean YLL/death`       = Mean_YLL,
  `YLL (% of total)`     = paste0(Pct_YLL, "%")
)]

# ── YLL by cause group × year (for trends) ───────────────────────────────────
yll_by_year <- dt[!is.na(year) & !is.na(cause_group), .(
  Deaths = .N,
  YLL    = sum(YLL, na.rm = TRUE)
), by = .(year, cause_group)]
yll_by_year[, cause_group := factor(cause_group, levels = GROUP_ORDER)]

# ── YLL by cause group × age group ───────────────────────────────────────────
yll_by_age <- dt[!is.na(age_grp) & !is.na(cause_group), .(
  Deaths = .N,
  YLL    = sum(YLL, na.rm = TRUE)
), by = .(age_grp, cause_group)]
yll_by_age[, age_grp    := factor(age_grp,    levels = AGE_LABELS)]
yll_by_age[, cause_group := factor(cause_group, levels = GROUP_ORDER)]

# ── YLL by cause group × sex (most recent year) ──────────────────────────────
yll_by_sex <- dt[year == RECENT_YEAR & sex_label %in% c("Male","Female") &
                   !is.na(cause_group), .(
  Deaths = .N,
  YLL    = sum(YLL, na.rm = TRUE)
), by = .(sex_label, cause_group)]
yll_by_sex[, cause_group := factor(cause_group, levels = GROUP_ORDER)]

# ── Poisoning subcategory breakdown (all years) ──────────────────────────────
yll_poisoning_sub <- dt[
  grepl("^(X4[0-9]|X6[0-9]|X85|Y1[0-9])", icd_clean) & !is.na(year), .(
  Deaths = .N,
  YLL    = round(sum(YLL, na.rm = TRUE)),
  Mean_YLL = round(mean(YLL, na.rm = TRUE), 1)
), by = .(intent = fcase(
  grepl("^X4[0-9]", icd_clean), "Accidental/unspec.",
  grepl("^X6[0-9]", icd_clean), "Self-harm",
  grepl("^X85",     icd_clean), "Assault",
  grepl("^Y1[0-9]", icd_clean), "Undetermined",
  default = "Other"
))]
setorder(yll_poisoning_sub, -YLL)

# ════════════════════════════════════════════════════════════════════════════
#  SECTION 5: Figures
# ════════════════════════════════════════════════════════════════════════════

# Colour palette: highlight NMC/external groups, mute others
GROUP_COLS <- c(
  "Poisoning (all intent)"            = "#D73027",
  "Road traffic injuries"             = "#FC8D59",
  "Drowning (unintentional)"          = "#4575B4",
  "Falls"                             = "#ABD9E9",
  "Burns & fire"                      = "#E08F2A",
  "Self-harm (non-poisoning)"         = "#B2182B",
  "Interpersonal violence"            = "#762A83",
  "Other external causes"             = "#C9B4CC",
  "Tuberculosis"                      = "#1A9641",
  "HIV/AIDS"                          = "#018571",
  "Vaccine-preventable (excl TB/HIV)" = "#80CDC1",
  "Other NMC communicable"            = "#C2E699",
  "Malignant neoplasms (cancer)"      = "#636363",
  "Cardiovascular disease"            = "#BDBDBD",
  "Diabetes mellitus"                 = "#D9D9D9",
  "Chronic respiratory disease"       = "#F0F0F0",
  "Other cause"                       = "#F7F7F7"
)

# ── FIG 1: Total YLL by cause group (all years, horizontal bar) ──────────────
plot_data1 <- yll_overall[cause_group != "Other cause"]
plot_data1[, cause_group := factor(cause_group, levels = rev(GROUP_ORDER[GROUP_ORDER != "Other cause"]))]

fig_yll_total <- ggplot(plot_data1, aes(x = YLL / 1e6, y = cause_group, fill = cause_group)) +
  geom_col(width = 0.75, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2fM", YLL / 1e6)), hjust = -0.08, size = 3.0) +
  scale_fill_manual(values = GROUP_COLS) +
  scale_x_continuous(
    labels = function(x) paste0(x, "M"),
    expand = expansion(mult = c(0, 0.20))
  ) +
  labs(x = "Total YLL (millions)", y = NULL,
       caption = paste0("All years in dataset. SA 2022 national life table. N = ",
                        format(sum(plot_data1$Deaths), big.mark = ","), " deaths.")) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())

# ── FIG 2: Mean YLL per death (shows premature-ness) ────────────────────────
plot_data2 <- yll_overall[cause_group != "Other cause"]
plot_data2[, cause_group := factor(cause_group, levels = rev(GROUP_ORDER[GROUP_ORDER != "Other cause"]))]

fig_mean_yll <- ggplot(plot_data2, aes(x = Mean_YLL, y = cause_group, fill = cause_group)) +
  geom_col(width = 0.75, show.legend = FALSE) +
  geom_text(aes(label = Mean_YLL), hjust = -0.15, size = 3.0) +
  scale_fill_manual(values = GROUP_COLS) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Mean YLL per death (years)", y = NULL,
       caption = "SA 2022 national life table (combined sex). Higher value = more premature death.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())

# ── FIG 3: % share of total YLL (lollipop) ───────────────────────────────────
plot_data3 <- yll_overall[cause_group != "Other cause"]
plot_data3[, cause_group := factor(cause_group, levels = rev(GROUP_ORDER[GROUP_ORDER != "Other cause"]))]

fig_yll_share <- ggplot(plot_data3, aes(x = Pct_YLL, y = cause_group, colour = cause_group)) +
  geom_segment(aes(xend = 0, yend = cause_group), linewidth = 0.7, show.legend = FALSE) +
  geom_point(size = 3.5, show.legend = FALSE) +
  geom_text(aes(label = paste0(Pct_YLL, "%")), hjust = -0.4, size = 3.0, colour = "grey20") +
  scale_colour_manual(values = GROUP_COLS) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22)),
                     labels = function(x) paste0(x, "%")) +
  labs(x = "% of total YLL", y = NULL,
       caption = "All years in dataset; 'Other cause' excluded for clarity.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank())

# ── FIG 4: YLL trend over time for selected groups ────────────────────────────
FOCUS_GROUPS <- c(
  "Poisoning (all intent)",
  "Road traffic injuries",
  "Drowning (unintentional)",
  "Interpersonal violence",
  "Tuberculosis",
  "HIV/AIDS",
  "Vaccine-preventable (excl TB/HIV)",
  "Malignant neoplasms (cancer)"
)
fig_yll_trend <- ggplot(
  yll_by_year[cause_group %in% FOCUS_GROUPS],
  aes(x = year, y = YLL / 1e3, colour = cause_group, group = cause_group)
) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 1.6, alpha = 0.7) +
  scale_colour_manual(values = GROUP_COLS, name = NULL) +
  scale_x_continuous(breaks = seq(1997, max(dt$year, na.rm = TRUE), 3)) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "YLL (thousands)",
       caption = "SA 2022 national life table.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        legend.key.size = unit(0.5, "cm")) +
  guides(colour = guide_legend(nrow = 3))

# ── FIG 5: YLL by age group for focus groups (faceted) ───────────────────────
fig_yll_age <- ggplot(
  yll_by_age[cause_group %in% FOCUS_GROUPS],
  aes(x = age_grp, y = YLL / 1e3, fill = cause_group)
) +
  geom_col(width = 0.8, show.legend = FALSE) +
  facet_wrap(~ cause_group, ncol = 2, scales = "free_y") +
  scale_fill_manual(values = GROUP_COLS) +
  scale_y_continuous(labels = comma) +
  labs(x = "Age group", y = "YLL (thousands)") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        strip.text  = element_text(face = "bold", size = 8))

# ── FIG 6: YLL by sex for most-recent year (grouped bars) ────────────────────
yll_sex_focus <- yll_by_sex[cause_group %in% FOCUS_GROUPS]
yll_sex_focus[, cause_group := factor(cause_group,
  levels = rev(FOCUS_GROUPS[FOCUS_GROUPS %in% unique(yll_sex_focus$cause_group)]))]

fig_yll_sex <- ggplot(yll_sex_focus,
  aes(x = YLL / 1e3, y = cause_group, fill = sex_label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72, alpha = 0.85) +
  scale_fill_manual(values = c(Male = "steelblue", Female = "firebrick"), name = NULL) +
  scale_x_continuous(labels = comma) +
  labs(x = "YLL (thousands)", y = NULL,
       caption = paste0("Year: ", RECENT_YEAR, ". SA 2022 national life table.")) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        legend.position = "bottom")

# ── FIG 8: Rank bump chart — YLL rank over time ─────────────────────────────
# Rank 1 = highest YLL that year.  Exclude 'Other cause' (catch-all).
# Include all named cause groups so the full competitive picture is visible.
yll_rank <- yll_by_year[cause_group != "Other cause" & !is.na(YLL), 
  .(cause_group, year, YLL)]
yll_rank[, rank := frank(-YLL, ties.method = "min"), by = year]
yll_rank[, cause_group := factor(cause_group, levels = GROUP_ORDER)]

# End-of-series labels (last year + first year)
YEAR_MIN  <- min(yll_rank$year)
YEAR_MAX  <- max(yll_rank$year)
labels_right <- yll_rank[year == YEAR_MAX]
labels_left  <- yll_rank[year == YEAR_MIN]

N_GROUPS <- length(unique(yll_rank$cause_group))

fig_rank_bump <- ggplot(yll_rank,
    aes(x = year, y = rank, colour = cause_group, group = cause_group)) +
  # faint grey shadow for visual weight
  geom_line(linewidth = 2.8, colour = "grey88", alpha = 0.6) +
  # coloured line
  geom_line(linewidth = 1.1, alpha = 0.90) +
  # points at each year
  geom_point(size = 2.2, shape = 21, fill = "white", stroke = 1.1) +
  # right-hand labels
  geom_text(data = labels_right,
            aes(x = year + 0.35, label = paste0(rank, ". ", cause_group)),
            hjust = 0, size = 2.65, fontface = "plain") +
  scale_colour_manual(values = GROUP_COLS, guide = "none") +
  scale_y_reverse(
    breaks = seq_len(N_GROUPS),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  scale_x_continuous(
    breaks = seq(YEAR_MIN, YEAR_MAX, 3),
    expand = expansion(mult = c(0.01, 0.38))   # room for right labels
  ) +
  labs(
    x    = NULL,
    y    = "YLL rank (1 = highest burden)",
    caption = paste0(
      "Rank 1 = most YLL in that year. 'Other cause' excluded. ",
      "SA 2022 national life table."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.4)
  )

# ── FIG 7: Scatter — Deaths vs Mean YLL (bubble size = total YLL) ────────────
plot_data7 <- yll_overall[cause_group != "Other cause"]
fig_scatter <- ggplot(plot_data7,
  aes(x = Deaths / 1e3, y = Mean_YLL, size = YLL / 1e6,
      colour = cause_group, label = cause_group)) +
  geom_point(alpha = 0.75, show.legend = FALSE) +
  ggrepel::geom_text_repel(size = 2.8, show.legend = FALSE,
                            max.overlaps = 20, seed = 42,
                            box.padding = 0.35) +
  scale_colour_manual(values = GROUP_COLS) +
  scale_size_continuous(range = c(2, 14), guide = "none") +
  scale_x_continuous(labels = comma) +
  labs(x = "Deaths (thousands)", y = "Mean YLL per death (years)",
       caption = "Bubble area proportional to total YLL. All years in dataset.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

# ════════════════════════════════════════════════════════════════════════════
#  SECTION 6: Save
# ════════════════════════════════════════════════════════════════════════════
analysis_metadata <- list(
  input_file    = input_file,
  total_deaths  = format(nrow(dt), big.mark = ","),
  year_range    = paste(min(dt$year, na.rm = TRUE), "-", max(dt$year, na.rm = TRUE)),
  recent_year   = RECENT_YEAR,
  life_table    = "SA national life table (combined sex, approx 2022 values)",
  groups        = GROUP_ORDER
)

save(
  nmc_map,
  yll_overall, tbl_yll_overall_display,
  yll_recent,  tbl_yll_recent_display,
  yll_by_year, yll_by_age, yll_by_sex,
  yll_poisoning_sub,
  fig_yll_total, fig_mean_yll, fig_yll_share,
  fig_yll_trend, fig_yll_age, fig_yll_sex, fig_scatter,
  fig_rank_bump, yll_rank,
  GROUP_ORDER, GROUP_COLS, FOCUS_GROUPS,
  AGE_LABELS, RECENT_YEAR,
  analysis_metadata,
  file = output_rda
)
message("Done. Saved to: ", output_rda)
