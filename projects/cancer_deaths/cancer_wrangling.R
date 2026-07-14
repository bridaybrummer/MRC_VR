# =============================================================================
# Cancer Deaths Analysis — Wrangling & Summarisation Script
# =============================================================================
# Monitors trends in cancer mortality in South Africa using the MRC-processed
# vital registration data (1997–2022), with particular focus on:
#   • HIV-associated / AIDS-defining cancers (cervical, KS, NHL)
#   • Top cancers by burden (breast, prostate, oesophageal, colorectal, lung)
#
# Sections:
#   1. Load & prepare VR data
#   2. Define cancer groups (ICD-10 prefix mapping)
#   3. Overall cancer time trends
#   4. Top-cancer time trends
#   5. HIV-associated cancer focus
#   6. Cervical cancer deep-dive (women, by province, age)
#   7. Age-at-death distributions
#   8. Province distribution
#   9. Save output
#
# Output: projects/cancer_deaths/cancer_results.rda
# =============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(ggplot2)
  library(scales)
  library(flextable)
})

cat("══════════════════════════════════════════════════════════╗\n")
cat("  Cancer Deaths Analysis — SA VR 1997–2022                 \n")
cat("══════════════════════════════════════════════════════════╝\n\n")


# ─── 1. LOAD VR DEATH DATA ────────────────────────────────────────────────────
cat("1. Loading VR death data …\n")

vr_path <- "Deaths2022_MRCversionFINAL.feather"
if (!file.exists(vr_path)) stop("VR data not found: ", vr_path)

df <- read_feather(vr_path) |> as.data.table()

# Normalise key columns
df[, epi_year := as.integer(as.numeric(epi_year))]
df[, epi_week := as.integer(as.numeric(epi_week))]

# Sex: convert to readable label
if ("SexName" %in% names(df)) {
  df[, sex_label := as.character(SexName)]
} else if ("Sex" %in% names(df)) {
  df[, sex_label := fcase(
    as.integer(as.numeric(Sex)) == 1L, "Male",
    as.integer(as.numeric(Sex)) == 2L, "Female",
    default = "Unknown"
  )]
} else {
  df[, sex_label := "Unknown"]
}

# Age at death
age_col <- intersect(c("AgeYear", "age", "AgeinYears"), names(df))[1]
if (!is.na(age_col)) {
  df[, age_death := as.integer(as.numeric(get(age_col)))]
  df[age_death < 0L | age_death > 120L, age_death := NA_integer_]
} else {
  df[, age_death := NA_integer_]
}

# Age groups (10-year)
df[, agegroup10 := cut(age_death,
  breaks = c(0, 15, 25, 35, 45, 55, 65, 75, Inf),
  labels = c("<15", "15–24", "25–34", "35–44", "45–54", "55–64", "65–74", "75+"),
  right  = FALSE, include.lowest = TRUE
)]

# Province
prov_col <- intersect(c("DeathProvinceName", "Province", "province",
                        "ProvinceCode", "ProvCode", "GeoCodeProv"), names(df))[1]
if (!is.na(prov_col)) {
  setnames(df, prov_col, "province", skip_absent = TRUE)
  df[, province := as.character(province)]
} else {
  df[, province := "South Africa"]
}

# Keep only natural individual deaths and valid cancer codes
df_c <- df[
  NaturalUnnatural == 1L | is.na(NaturalUnnatural),
][
  DeathType == 1L | is.na(DeathType),
][
  !is.na(UnderlyingCause) & nchar(UnderlyingCause) >= 3L &
    substr(UnderlyingCause, 1L, 1L) == "C"
]

cat("   Total cancer deaths (all years):", format(nrow(df_c), big.mark = ","), "\n\n")


# ─── 2. DEFINE CANCER GROUPS ─────────────────────────────────────────────────
cat("2. Defining cancer groups …\n")

# ICD-10 3-character prefix → readable label
# HIV-associated and AIDS-defining cancers are flagged separately
cancer_map <- list(
  # ── HIV-associated / AIDS-defining ──────────────────────────────────────
  "Cervical cancer"              = c("C53"),
  "Kaposi's sarcoma"             = c("C46"),
  "Non-Hodgkin lymphoma"         = c("C82", "C83", "C84", "C85", "C86", "C96"),

  # ── Top SA cancers by burden ─────────────────────────────────────────────
  "Breast cancer"                = c("C50"),
  "Prostate cancer"              = c("C61"),
  "Oesophageal cancer"           = c("C15"),
  "Colorectal cancer"            = c("C18", "C19", "C20", "C21"),
  "Lung / trachea / bronchus"    = c("C33", "C34"),
  "Liver cancer"                 = c("C22"),
  "Stomach cancer"               = c("C16"),
  "Leukaemia"                    = c("C91", "C92", "C93", "C94", "C95")
)

hiv_cancers <- c("Cervical cancer", "Kaposi's sarcoma", "Non-Hodgkin lymphoma")

# Helper: assign group from 3-char prefix
assign_cancer_group <- function(code, map) {
  pfx <- substr(code, 1L, 3L)
  for (grp in names(map)) {
    if (pfx %in% map[[grp]]) return(grp)
  }
  return(NA_character_)
}

df_c[, cancer_group := vapply(UnderlyingCause, assign_cancer_group,
                              FUN.VALUE = character(1L), map = cancer_map)]
df_c[, hiv_associated := cancer_group %in% hiv_cancers]

monitored <- df_c[!is.na(cancer_group)]
cat("   Deaths in monitored cancer groups:", format(nrow(monitored), big.mark = ","), "\n")
cat("   Groups:", paste(names(cancer_map), collapse = ", "), "\n\n")


# ─── 3. OVERALL CANCER TRENDS ────────────────────────────────────────────────
cat("3. Overall cancer time trends …\n")

# Annual totals: all cancers vs population context
annual_all <- df_c[, .(cancer_deaths = .N), by = .(epi_year)]
setkey(annual_all, epi_year)

# Proportion of all deaths that are cancer
annual_total <- df[
  NaturalUnnatural == 1L | is.na(NaturalUnnatural),
  .(total_deaths = .N), by = .(epi_year)
]
annual_all <- merge(annual_all, annual_total, by = "epi_year", all.x = TRUE)
annual_all[, pct_cancer := round(100 * cancer_deaths / total_deaths, 2)]

fig_cancer_trend <- ggplot(annual_all[epi_year >= 2000L], aes(x = epi_year)) +
  geom_col(aes(y = cancer_deaths), fill = "#6f42c1", alpha = 0.8) +
  geom_line(aes(y = pct_cancer * max(cancer_deaths, na.rm = TRUE) / 10),
            colour = "#dc3545", linewidth = 1) +
  scale_y_continuous(
    name   = "Cancer deaths (count)",
    labels = comma,
    sec.axis = sec_axis(
      ~ . * 10 / max(annual_all$cancer_deaths, na.rm = TRUE),
      name = "% of all natural deaths"
    )
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  labs(
    title    = "Cancer mortality in South Africa, 2000–2022",
    subtitle = "Bars = annual cancer deaths; red line = % of all natural deaths",
    x        = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.title.y.right = element_text(colour = "#dc3545"))

cat("   fig_cancer_trend done\n")


# ─── 4. TOP-CANCER TIME TRENDS ───────────────────────────────────────────────
cat("4. Top-cancer trends by group …\n")

annual_group <- monitored[, .(deaths = .N), by = .(epi_year, cancer_group)]
setkey(annual_group, epi_year, cancer_group)

# Total rank order for ordering factor
group_rank <- monitored[epi_year >= 2015L,
  .(total = .N), by = cancer_group][order(-total), cancer_group]
annual_group[, cancer_group := factor(cancer_group, levels = group_rank)]

# Colour palette: HIV-associated in warm reds, others in blues/greens
grp_colours <- c(
  "Cervical cancer"           = "#e31a1c",
  "Kaposi's sarcoma"          = "#ff7f00",
  "Non-Hodgkin lymphoma"      = "#fb9a99",
  "Breast cancer"             = "#1f78b4",
  "Prostate cancer"           = "#a6cee3",
  "Oesophageal cancer"        = "#33a02c",
  "Colorectal cancer"         = "#b2df8a",
  "Lung / trachea / bronchus" = "#6a3d9a",
  "Liver cancer"              = "#cab2d6",
  "Stomach cancer"            = "#b15928",
  "Leukaemia"                 = "#999999"
)

fig_group_trend <- ggplot(
    annual_group[epi_year >= 2000L],
    aes(x = epi_year, y = deaths, colour = cancer_group)
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_colour_manual(values = grp_colours, name = NULL) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  labs(
    title    = "Annual cancer deaths by type — South Africa, 2000–2022",
    subtitle = "HIV-associated cancers in red/orange; other top cancers in blue/green",
    x        = "Year",
    y        = "Deaths"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right", legend.text = element_text(size = 9))

cat("   fig_group_trend done\n")

# Faceted version (free-y) for peer comparison
fig_group_facet <- ggplot(
    annual_group[epi_year >= 2000L],
    aes(x = epi_year, y = deaths, fill = cancer_group)
  ) +
  geom_col(width = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = grp_colours) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(2005, 2015, 2022)) +
  facet_wrap(~ cancer_group, scales = "free_y", ncol = 3L) +
  labs(
    title = "Cancer deaths by type — annual counts, 2000–2022",
    x = "Year", y = "Deaths"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 8))

cat("   fig_group_facet done\n")


# ─── 5. HIV-ASSOCIATED CANCERS ───────────────────────────────────────────────
cat("5. HIV-associated cancer focus …\n")

hiv_annual <- monitored[hiv_associated == TRUE,
  .(deaths = .N), by = .(epi_year, cancer_group)]
setkey(hiv_annual, epi_year, cancer_group)
hiv_annual[, cancer_group := factor(cancer_group, levels = hiv_cancers)]

fig_hiv_trend <- ggplot(hiv_annual[epi_year >= 2000L],
    aes(x = epi_year, y = deaths, colour = cancer_group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_colour_manual(
    values = c("Cervical cancer"     = "#e31a1c",
               "Kaposi's sarcoma"    = "#ff7f00",
               "Non-Hodgkin lymphoma"= "#984ea3"),
    name = NULL
  ) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  labs(
    title    = "HIV-associated cancer deaths — South Africa, 2000–2022",
    subtitle = "Cervical cancer, Kaposi's sarcoma, Non-Hodgkin lymphoma",
    x        = "Year",
    y        = "Deaths"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

cat("   fig_hiv_trend done\n")

# Summary table: HIV cancer totals by decade
hiv_summary <- monitored[hiv_associated == TRUE & epi_year >= 2000L, .(
  Period = cut(epi_year,
    breaks = c(2000, 2010, 2020, 2023),
    labels = c("2000–2009", "2010–2019", "2020–2022"),
    right  = FALSE),
  cancer_group
)][, .(Deaths = .N), by = .(Period, cancer_group)]
setkey(hiv_summary, Period, cancer_group)


# ─── 6. CERVICAL CANCER DEEP-DIVE ────────────────────────────────────────────
cat("6. Cervical cancer deep-dive …\n")

cervical <- df_c[substr(UnderlyingCause, 1L, 3L) == "C53"]

# Annual trend
cervical_annual <- cervical[, .(deaths = .N), by = .(epi_year)]

# By province
cervical_prov <- cervical[epi_year >= 2015L & !is.na(province),
  .(deaths = .N), by = .(epi_year, province)]

# Age distribution
cervical_age <- cervical[!is.na(agegroup10),
  .(deaths = .N), by = .(agegroup10)]

fig_cervical_trend <- ggplot(cervical_annual[epi_year >= 2000L],
    aes(x = epi_year, y = deaths)) +
  geom_col(fill = "#e31a1c", alpha = 0.85, width = 0.8) +
  geom_smooth(method = "loess", se = FALSE, colour = "black",
              linetype = "dashed", linewidth = 0.7) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  labs(
    title    = "Cervical cancer deaths — South Africa, 2000–2022",
    subtitle = "ICD-10 C53. Red bars = annual deaths; dashed line = LOESS trend",
    x        = "Year",
    y        = "Deaths"
  ) +
  theme_minimal(base_size = 12)

fig_cervical_age <- ggplot(cervical_age, aes(x = agegroup10, y = deaths)) +
  geom_col(fill = "#e31a1c", alpha = 0.85) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Cervical cancer deaths by age group (all years)",
    x     = "Age group",
    y     = "Deaths (total 1997–2022)"
  ) +
  theme_minimal(base_size = 12)

fig_cervical_province <- ggplot(
    cervical_prov[, .(deaths = sum(deaths)), by = province][order(-deaths)],
    aes(x = reorder(province, deaths), y = deaths)
  ) +
  geom_col(fill = "#e31a1c", alpha = 0.85) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Cervical cancer deaths by province, 2015–2022",
    x        = NULL,
    y        = "Deaths"
  ) +
  theme_minimal(base_size = 12)

cat("   Cervical cancer figures done\n")


# ─── 7. AGE-AT-DEATH DISTRIBUTIONS ───────────────────────────────────────────
cat("7. Age-at-death distributions …\n")

age_dist <- monitored[!is.na(agegroup10) & epi_year >= 2010L,
  .(deaths = .N), by = .(agegroup10, cancer_group)]
age_dist[, cancer_group := factor(cancer_group, levels = group_rank)]
age_dist[, pct := deaths / sum(deaths) * 100, by = cancer_group]

fig_age_dist <- ggplot(age_dist,
    aes(x = agegroup10, y = pct, fill = cancer_group)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = grp_colours) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  facet_wrap(~ cancer_group, scales = "free_y", ncol = 3L) +
  labs(
    title    = "Age distribution of cancer deaths, 2010–2022",
    subtitle = "% within each cancer group",
    x        = "Age group",
    y        = "% of group deaths"
  ) +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(face = "bold", size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1))

cat("   fig_age_dist done\n")


# ─── 8. PROVINCE DISTRIBUTION ────────────────────────────────────────────────
cat("8. Province distribution …\n")

prov_dist <- monitored[!is.na(province) & epi_year >= 2015L,
  .(deaths = .N), by = .(province, cancer_group)]
prov_dist[, cancer_group := factor(cancer_group, levels = group_rank)]

# Restrict to top-5 groups for readability
top5_groups <- group_rank[1:5]
prov_top5 <- prov_dist[cancer_group %in% top5_groups]

fig_province <- ggplot(prov_top5,
    aes(x = reorder(province, deaths), y = deaths, fill = cancer_group)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = grp_colours, name = NULL) +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  labs(
    title    = "Cancer deaths by province (top 5 groups), 2015–2022",
    x        = NULL,
    y        = "Deaths"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))

cat("   fig_province done\n")


# ─── 9. SUMMARY TABLES ───────────────────────────────────────────────────────
cat("9. Building summary tables …\n")

# Overall totals by group (recent decade)
cancer_summary <- monitored[epi_year >= 2013L, .(
  Deaths       = .N,
  Pct_of_total = NA_real_   # filled below
), by = cancer_group][order(-Deaths)]
cancer_summary[, Pct_of_total := round(100 * Deaths / sum(Deaths), 1)]
cancer_summary[, HIV_associated := cancer_group %in% hiv_cancers]
cancer_summary[, Period := "2013–2022"]

# Sex split for monitored groups
sex_split <- monitored[epi_year >= 2013L & sex_label %in% c("Male", "Female"),
  .(deaths = .N), by = .(cancer_group, sex_label)]
sex_split[, pct := round(100 * deaths / sum(deaths), 1), by = cancer_group]

# Trend 2010 vs 2022 (% change)
yr_compare <- monitored[epi_year %in% c(2010L, 2022L),
  .(deaths = .N), by = .(epi_year, cancer_group)] |>
  dcast(cancer_group ~ epi_year, value.var = "deaths") |>
  setnames(c("2010", "2022"), c("deaths_2010", "deaths_2022"))
yr_compare[, pct_change := round(100 * (deaths_2022 - deaths_2010) / deaths_2010, 1)]
yr_compare <- yr_compare[order(-deaths_2022)]

cat("   Tables done\n\n")


# ─── 10. SAVE ──────────────────────────────────────────────────────────────────
cat("10. Saving results …\n")

save(
  # Data
  annual_all, annual_group, hiv_annual, hiv_summary,
  cervical_annual, cervical_prov, cervical_age,
  cancer_summary, sex_split, yr_compare,
  # Figures
  fig_cancer_trend, fig_group_trend, fig_group_facet,
  fig_hiv_trend,
  fig_cervical_trend, fig_cervical_age, fig_cervical_province,
  fig_age_dist, fig_province,
  # Helpers
  cancer_map, hiv_cancers, grp_colours,
  file = "projects/cancer_deaths/cancer_results.rda"
)

cat("\n✓ cancer_results.rda written to projects/cancer_deaths/\n")
cat("  Run: quarto render projects/cancer_deaths/index.qmd\n\n")
