# =============================================================================
# DHIS2 Hospital Indicators — Wrangling Script
# =============================================================================
# Source data  (NDoH DHIS2 platform, supplied by Pam Groenewald):
#   • data/DHIS_from_pam/DHIS DATA 2015_2026 (2).xls
#       Sheets: 2015_2018 | 2019_2020 | 2021 | 2022_2026 1-3
#       Format: wide — cols = Province, District, Sub-district, Facility, Data,
#               then one column per year (annual aggregates)
#   • data/DHIS_from_pam/DHIS DATA APR 2024_MAR 2026_test.xls
#       Sheets: APR_DEC 24 | JAN_JUNE 25 | JULY_DEC 25 | JAN-MAR 26
#       Format: wide — same geo columns, then one column per month
#
# What the data contains (27 indicators):
#   BIRTHS / DELIVERIES: Born alive before arrival, Live birth in facility,
#     LBW (<2500g), Delivery by age group (10-14, 15-19, 20+),
#     Caesarean section, Still birth, BCG dose
#   DEATHS: Death in facility by age group (0-6d, 7-28d, 29d-11m, 12-59m),
#     Maternal death, Dead on arrival, Inpatient deaths - total,
#     Inpatient death - Maternity
#   CONTRACEPTION/REPRODUCTIVE: Medroxyprogesterone, Norethisterone,
#     Oral pill, IUCD, Sub-dermal implant, Sterilisation (M/F),
#     Termination of pregnancy (0-12w, 13-20w, by age 10-19 / 20+)
#
# Output: projects/dhis_hospital_mortality/dhis_results.rda
#   Objects saved:
#     dhis_births    — births, deliveries and BCG (annual + monthly where available)
#     dhis_deaths    — all death indicators
#     dhis_contra    — contraception and reproductive indicators
#     dhis_annual    — all indicators combined, annual (for trend plots)
#     dhis_monthly   — all indicators combined, monthly (file 2 only)
#     indicator_meta — lookup table: indicator → domain → short_label
#     audit          — provenance / run metadata
#
# Run from the workspace root:
#   Rscript projects/dhis_hospital_mortality/wrangling.R
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(data.table)
  library(lubridate)
  library(stringr)
})

# ─── 0. PATHS ─────────────────────────────────────────────────────────────────
# All paths relative to workspace root.
# Actual filenames supplied by Pam (case-sensitive on macOS):
PROJECT_DIR    <- "projects/dhis_hospital_mortality"
DATA_DIR       <- "data/DHIS_from_pam"
OUTPUT_RDA     <- file.path(PROJECT_DIR, "dhis_results.rda")

FILE_ANNUAL    <- file.path(DATA_DIR, "DHIS DATA 2015_2026 (2).xls")
FILE_MONTHLY   <- file.path(DATA_DIR, "DHIS DATA APR 2024_MAR 2026_test.xls")
# Condom distribution data — separate source file, separate quality profile
# (see section 4b): facility-level reporting 2015-2017, then a single
# synthetic "<Municipality> Primary Condom Distribution Site" row per district
# from 2018 onwards, so condoms are only usable at district+year granularity.
FILE_CONDOM    <- file.path(DATA_DIR, "ANC_DHIS DATA 2015_2026 2.xls")

stopifnot(
  "Annual file not found"  = file.exists(FILE_ANNUAL),
  "Monthly file not found" = file.exists(FILE_MONTHLY),
  "Condom file not found"  = file.exists(FILE_CONDOM)
)
cat("Source files confirmed:\n  ", FILE_ANNUAL, "\n  ", FILE_MONTHLY, "\n  ", FILE_CONDOM, "\n\n")


# ─── 1. INDICATOR DOMAIN MAP ──────────────────────────────────────────────────
# Single source of truth mapping every DHIS2 indicator name → domain + short label.
# Three domains:
#   "births"  — live/still births, deliveries, LBW, BCG
#   "deaths"  — all in-hospital death indicators
#   "contra"  — contraception, sterilisation, TOP
#
# This table drives the split into three output datasets AND is saved in the RDA
# for use in the QMD (hover-text, axis labels, filter options, etc.).

indicator_meta <- data.table(
  indicator = c(
    # ── births domain ──────────────────────────────────────────────────────
    "Born alive before arrival at facility",
    "Live birth in facility",
    "Live birth under 2500g in facility",
    "Delivery 10-14 years in facility",
    "Delivery 15-19 years in facility",
    "Delivery 20 years and older in facility",
    "Delivery by caesarean section",
    "Still birth in facility",
    "BCG dose",
    # ── deaths domain ──────────────────────────────────────────────────────
    "Death in facility 0-6 days",
    "Death in facility 7-28 days",
    "Death in facility 29 days - 11 months",
    "Death in facility 12-59 months",
    "Maternal death in facility",
    "Dead on arrival",
    "Inpatient deaths - total",
    "Inpatient death - Maternity",
    # ── contra domain ──────────────────────────────────────────────────────
    "Medroxyprogesterone injection",
    "Norethisterone enanthate injection",
    "Oral pill cycle",
    "IUCD inserted",
    "Sub-dermal implant inserted",
    "Sterilisation - female",
    "Sterilisation - male",
    "Termination of pregnancy 0-12 weeks",
    "Termination of pregnancy 13-20 weeks",
    "Termination of pregnancy 10-19 years",
    "Termination of pregnancy 20 years and older",
    # ── condom domain (separate source file; district+year granularity only,
    #    see section 4b) ────────────────────────────────────────────────────
    "Male condoms distributed",
    "Female condoms distributed"
  ),
  domain = c(
    rep("births", 9),
    rep("deaths", 8),   # 8 death indicators (was mistakenly 9, which pushed
                         # "Medroxyprogesterone injection" into "deaths")
    rep("contra", 11),
    rep("condom", 2)
  ),
  short_label = c(
    # births
    "Born alive (BBA)", "Live birth", "LBW (<2500g)",
    "Delivery 10-14y", "Delivery 15-19y", "Delivery 20+y",
    "Caesarean section", "Still birth", "BCG dose",
    # deaths
    "Death 0-6d", "Death 7-28d", "Death 29d-11m", "Death 12-59m",
    "Maternal death", "Dead on arrival", "Inpatient deaths (total)",
    "Inpatient death (Maternity)",
    # contra
    "Medroxyprogesterone", "Norethisterone", "Oral pill",
    "IUCD", "Sub-dermal implant", "Sterilisation (F)", "Sterilisation (M)",
    "TOP 0-12wk", "TOP 13-20wk", "TOP 10-19y", "TOP 20+y",
    # condom
    "Male condom", "Female condom"
  )
)
setkey(indicator_meta, indicator)


# ─── 2. HELPER: STRIP PROVINCE PREFIX FROM GEO COLUMNS ───────────────────────
# The exported strings look like "ec Eastern Cape Province" or
# "ec Sarah Baartman District Municipality".
# We strip the two-letter lowercase prefix and trailing administrative suffixes.

clean_geo <- function(x) {
  x <- str_trim(x)
  # Remove leading two-letter province code + space (e.g. "ec ", "wc ", "gp ")
  x <- sub("^[a-z]{2,3}\\s+", "", x)
  # Tidy up common trailing noise
  x <- sub("\\s+(District Municipality|Local Municipality|Metropolitan Municipality|Province)\\s*$",
           "", x, ignore.case = TRUE)
  str_trim(x)
}


# ─── 3. READ ANNUAL FILE (2015–2026) ──────────────────────────────────────────
# Layout per sheet:
#   Row 1  : header — Province | District | Sub-district | Facility | Data | <year1> | ...
#   Rows 2+: data
#   Geo cols are ALWAYS cols 1-4; indicator name is col 5; value cols 6-end.
#
# The 2022+ data is split across three sheets (one per indicator domain).
# All sheets have the same geo+indicator layout; only the year columns differ.

read_annual_sheet <- function(path, sheet) {
  raw <- as.data.table(
    read_excel(path, sheet = sheet, col_names = TRUE,
               col_types = "text",   # read everything as text first
               .name_repair = "minimal")
  )
  # Row 1 is the header (already consumed by col_names = TRUE).
  # Col 1 = Province, Col 2 = District, Col 3 = Sub-district,
  # Col 4 = Facility, Col 5 = Data (indicator), Col 6+ = year values.

  # Rename fixed columns defensively (they may carry auto-names if blank)
  setnames(raw, 1:5,
           c("province_raw", "district_raw", "subdistrict_raw",
             "facility_raw", "indicator"))

  # Year columns: names are "2015", "2016", ... (character strings of the year)
  year_cols <- names(raw)[6:ncol(raw)]

  # Melt to long
  long <- melt(raw,
               id.vars      = c("province_raw", "district_raw", "subdistrict_raw",
                                "facility_raw", "indicator"),
               measure.vars = year_cols,
               variable.name = "year_str",
               value.name    = "value",
               variable.factor = FALSE)

  # Drop rows where indicator or value is missing / blank
  long <- long[!is.na(indicator) & indicator != "" & indicator != "Data"]
  long <- long[!is.na(value) & value != ""]

  # Parse value as numeric (thousands separators, spaces, etc.)
  long[, value := as.numeric(gsub("[^0-9.-]", "", value))]
  long <- long[!is.na(value)]

  # Parse year
  long[, year := as.integer(year_str)]
  long[, year_str := NULL]

  # Clean geo strings
  long[, province    := clean_geo(province_raw)]
  long[, district    := clean_geo(district_raw)]
  long[, subdistrict := clean_geo(subdistrict_raw)]
  long[, facility    := clean_geo(facility_raw)]

  # Drop raw geo columns
  long[, c("province_raw", "district_raw", "subdistrict_raw", "facility_raw") := NULL]

  long[]
}

cat("Reading annual file...\n")
annual_sheets <- excel_sheets(FILE_ANNUAL)
cat("  Sheets found:", paste(annual_sheets, collapse = " | "), "\n")

annual_parts <- lapply(annual_sheets, function(sh) {
  cat("  Reading sheet:", sh, "\n")
  read_annual_sheet(FILE_ANNUAL, sh)
})
dhis_annual_raw <- rbindlist(annual_parts, fill = TRUE)

# Drop 2026 from the ANNUAL series — the calendar year is incomplete and would
# otherwise show a spurious partial-year drop in every trend. (Monthly data,
# which is complete per-month through Mar 2026, is handled separately below.)
ANNUAL_MAX_YEAR <- 2025L
n_drop_2026 <- nrow(dhis_annual_raw[year > ANNUAL_MAX_YEAR])
dhis_annual_raw <- dhis_annual_raw[year <= ANNUAL_MAX_YEAR]
cat("  Dropped", n_drop_2026, "rows for incomplete year(s) >", ANNUAL_MAX_YEAR, "\n")

cat("  Annual raw rows:", nrow(dhis_annual_raw), "\n")
cat("  Year range:", min(dhis_annual_raw$year, na.rm = TRUE),
    "–", max(dhis_annual_raw$year, na.rm = TRUE), "\n")


# ─── 4. READ MONTHLY FILE (Apr 2024 – Mar 2026) ───────────────────────────────
# Layout per sheet:
#   Row 1 : Province | District | Sub-district | Facility | Data | <Month YYYY> | ...
#   Col headers for period look like "April 2024", "May 2024", etc.

read_monthly_sheet <- function(path, sheet) {
  raw <- as.data.table(
    read_excel(path, sheet = sheet, col_names = TRUE,
               col_types = "text",
               .name_repair = "minimal")
  )
  setnames(raw, 1:5,
           c("province_raw", "district_raw", "subdistrict_raw",
             "facility_raw", "indicator"))

  month_cols <- names(raw)[6:ncol(raw)]

  long <- melt(raw,
               id.vars      = c("province_raw", "district_raw", "subdistrict_raw",
                                "facility_raw", "indicator"),
               measure.vars = month_cols,
               variable.name = "period_str",
               value.name    = "value",
               variable.factor = FALSE)

  long <- long[!is.na(indicator) & indicator != "" & indicator != "Data"]
  long <- long[!is.na(value) & value != ""]

  long[, value := as.numeric(gsub("[^0-9.-]", "", value))]
  long <- long[!is.na(value)]

  # Parse "April 2024" → Date (first of month)
  long[, period_date := suppressWarnings(
    as.Date(parse_date_time(period_str, orders = c("B Y", "b Y"), locale = "en"))
  )]
  long[, year  := year(period_date)]
  long[, month := month(period_date)]

  long[, province    := clean_geo(province_raw)]
  long[, district    := clean_geo(district_raw)]
  long[, subdistrict := clean_geo(subdistrict_raw)]
  long[, facility    := clean_geo(facility_raw)]
  long[, c("province_raw", "district_raw", "subdistrict_raw", "facility_raw") := NULL]

  long[]
}


cat("\nReading monthly file...\n")
monthly_sheets <- excel_sheets(FILE_MONTHLY)
cat("  Sheets found:", paste(monthly_sheets, collapse = " | "), "\n")

monthly_parts <- lapply(monthly_sheets, function(sh) {
  cat("  Reading sheet:", sh, "\n")
  read_monthly_sheet(FILE_MONTHLY, sh)
})

dhis_monthly_raw <- rbindlist(monthly_parts, fill = TRUE)

# De-duplicate: some months overlap across sheets (e.g. APR_DEC 24 / JAN_JUNE 25)
dhis_monthly_raw <- unique(dhis_monthly_raw,
  by = c("province", "district", "subdistrict", "facility", "indicator", "period_date"))

cat("  Monthly raw rows:", nrow(dhis_monthly_raw), "\n")
cat("  Period range:", format(min(dhis_monthly_raw$period_date, na.rm = TRUE)),
    "–", format(max(dhis_monthly_raw$period_date, na.rm = TRUE)), "\n")


# ─── 4b. READ CONDOM FILE (annual only) ──────────────────────────────────────
# Same wide layout as FILE_ANNUAL (Province | District | Sub-district |
# Facility | Data | <year1> ... on sheet "2015_2026"), so the same reader can
# be reused. BUT the reporting unit changes partway through the series:
#   • 2015-2017 : one row per real facility (thousands of rows/year)
#   • 2018+     : one row per district/municipality, disguised as a single
#                 facility named "<Municipality> Primary Condom Distribution
#                 Site" (row counts collapse from ~7,000-9,500/year pre-2018
#                 to ~600-700/year from 2018 onwards, with distinct facility
#                 names ≈ distinct sub-districts, confirmed by inspection).
# Facility-level totals are therefore NOT comparable across the 2017/2018
# boundary. Condoms are summed up to province + district + year for the WHOLE
# series (safe either way — collapsing real facilities to their district gives
# the same district total as the later synthetic single row), and kept OUT of
# dhis_annual / dhis_contra so they never enter the facility-count
# completeness, outlier or heatmap logic built for the other contra indicators
# (which assumes a stable facility identity over time).
cat("\nReading condom file...\n")
condom_annual_raw <- read_annual_sheet(FILE_CONDOM, "2015_2026")
condom_annual_raw <- condom_annual_raw[
  indicator %in% c("Male condoms distributed", "Female condoms distributed")
]
condom_annual_raw <- condom_annual_raw[year <= ANNUAL_MAX_YEAR]  # drop incomplete 2026
cat("  Condom rows (facility/district mixed grain):", nrow(condom_annual_raw), "\n")

dhis_condom_annual <- condom_annual_raw[
  !is.na(district) & district != "",
  .(value = sum(value, na.rm = TRUE)),
  by = .(province, district, indicator, year)
][order(province, district, indicator, year)]
dhis_condom_annual <- indicator_meta[dhis_condom_annual, on = "indicator"]

cat("  Condom district-year rows:", nrow(dhis_condom_annual), "\n")
cat("  Condom year range:", min(dhis_condom_annual$year), "–", max(dhis_condom_annual$year), "\n")


# ─── 5. ATTACH DOMAIN METADATA ───────────────────────────────────────────────
# Left join indicator_meta onto both datasets.
# Indicators not in the map are tagged domain = "other".

attach_meta <- function(dt) {
  dt <- indicator_meta[dt, on = "indicator"]
  dt[is.na(domain), domain       := "other"]
  dt[is.na(short_label), short_label := indicator]
  dt[]
}

dhis_annual_raw  <- attach_meta(dhis_annual_raw)
dhis_monthly_raw <- attach_meta(dhis_monthly_raw)

# Confirm all indicators mapped
unmapped <- unique(dhis_annual_raw[domain == "other", indicator])
if (length(unmapped) > 0) {
  cat("\nNOTE: The following indicators were not in the domain map (tagged 'other'):\n")
  cat(paste0("  • ", unmapped, "\n"))
}


# ─── 6. SPLIT INTO THREE THEMED DATASETS ─────────────────────────────────────
# Each dataset keeps: province | district | subdistrict | facility |
#                     indicator | short_label | domain | year | value
# Monthly datasets additionally have: period_date | month

# ── 6a: Births / deliveries / BCG (annual) ────────────────────────────────────
dhis_births <- dhis_annual_raw[domain == "births"][
  order(province, facility, indicator, year)
]

# ── 6b: Deaths (annual) ────────────────────────────────────────────────────────
dhis_deaths <- dhis_annual_raw[domain == "deaths"][
  order(province, facility, indicator, year)
]

# ── 6c: Contraception / reproductive (annual) ────────────────────────────────
dhis_contra <- dhis_annual_raw[domain == "contra"][
  order(province, facility, indicator, year)
]

# ── 6d: Full annual (all domains) — for cross-domain explorers ────────────────
dhis_annual <- dhis_annual_raw[order(domain, province, facility, indicator, year)]

# ── 6e: Full monthly (all domains) ───────────────────────────────────────────
dhis_monthly <- dhis_monthly_raw[order(domain, province, facility, indicator, period_date)]

cat("\nDataset sizes:\n")
cat("  dhis_births  (annual)  :", nrow(dhis_births),  "rows\n")
cat("  dhis_deaths  (annual)  :", nrow(dhis_deaths),  "rows\n")
cat("  dhis_contra  (annual)  :", nrow(dhis_contra),  "rows\n")
cat("  dhis_annual  (all)     :", nrow(dhis_annual),  "rows\n")
cat("  dhis_monthly (all)     :", nrow(dhis_monthly), "rows\n")


# ─── 7. CONVENIENCE AGGREGATIONS (pre-baked for the QMD) ─────────────────────
# These avoid repeating aggregation logic inside every QMD code chunk.
# Naming convention: agg_<domain>_<geography>_<period>

# ── 7a: National annual by indicator (all three domains) ──────────────────────
agg_national_annual <- dhis_annual[,
  .(value = sum(value, na.rm = TRUE)),
  by = .(domain, indicator, short_label, year)
][order(domain, indicator, year)]

# ── 7b: Provincial annual by indicator ────────────────────────────────────────
agg_prov_annual <- dhis_annual[!is.na(province) & province != "",
  .(value = sum(value, na.rm = TRUE)),
  by = .(domain, indicator, short_label, province, year)
][order(domain, indicator, province, year)]

# ── 7c: National monthly (monthly file only) by indicator ────────────────────
agg_national_monthly <- dhis_monthly[,
  .(value = sum(value, na.rm = TRUE)),
  by = .(domain, indicator, short_label, year, month, period_date)
][order(domain, indicator, period_date)]

# ── 7d: Reporting completeness — facilities per period ───────────────────────
agg_completeness_annual <- dhis_annual[,
  .(n_facilities = uniqueN(paste(province, district, facility))),
  by = .(domain, year)
][order(domain, year)]

agg_completeness_monthly <- dhis_monthly[!is.na(period_date),
  .(n_facilities = uniqueN(paste(province, district, facility))),
  by = .(domain, year, month, period_date)
][order(domain, period_date)]

# ── 7e: Facility-level totals (for searchable table) ─────────────────────────
agg_facility_totals <- dhis_annual[,
  .(total_value = sum(value, na.rm = TRUE),
    years_reported = uniqueN(year)),
  by = .(domain, indicator, short_label, province, district, facility)
][order(domain, indicator, -total_value)]


# ─── 7f. FACILITY COUNT METHOD (contraception domain) ────────────────────────
# "Number of facilities" for the contra domain is defined the same way as the
# reporting-completeness metric used elsewhere in this script: a facility is
# counted for a given period if it has at least one non-missing, non-zero
# service record for ANY contraceptive/reproductive indicator in that period
# (province + district + facility name form the unique facility key — there is
# no separate facility ID/code in the source files, so name clashes across
# districts are NOT de-duplicated further than this key).
CONTRA_FACILITY_KEY <- c("province", "district", "facility")

agg_contra_completeness_annual <- dhis_annual[domain == "contra",
  .(n_facilities = uniqueN(paste(province, district, facility))),
  by = .(year)
][order(year)]

agg_contra_completeness_monthly <- dhis_monthly[domain == "contra" & !is.na(period_date),
  .(n_facilities = uniqueN(paste(province, district, facility))),
  by = .(year, month, period_date)
][order(period_date)]

# District-level monthly facility counts, used to flag poorly-reporting districts
agg_contra_completeness_district_monthly <- dhis_monthly[
  domain == "contra" & !is.na(period_date) & !is.na(district) & district != "",
  .(n_facilities = uniqueN(paste(province, district, facility))),
  by = .(province, district, year, month, period_date)
][order(province, district, period_date)]

# Each district's OWN maximum facility count in the monthly window is used as
# its reporting denominator (rather than a fixed national count), because
# district size varies enormously. reporting_rate = facilities this month /
# facility's own max facilities ever seen reporting in the window.
agg_contra_completeness_district_monthly[
  , facilities_max := max(n_facilities), by = .(province, district)
][, reporting_rate := n_facilities / facilities_max]

# Districts with chronically poor reporting: mean reporting rate < 70% across
# the monthly window (Apr 2024 – Mar 2026).
agg_contra_district_poor_reporting <- agg_contra_completeness_district_monthly[,
  .(mean_reporting_rate = mean(reporting_rate, na.rm = TRUE),
    min_reporting_rate  = min(reporting_rate, na.rm = TRUE),
    n_months            = .N),
  by = .(province, district)
][order(mean_reporting_rate)]


# ─── 7g. COUPLE-YEARS OF PROTECTION (CYP) ─────────────────────────────────────
# Standard USAID/FP2030 conversion factors — service-to-CYP multipliers used to
# convert raw commodity/procedure counts into a common "couple-years of
# protection" unit. Termination of pregnancy is NOT a contraceptive method and
# is excluded from CYP.
cyp_factors <- data.table(
  short_label = c("Medroxyprogesterone", "Norethisterone", "Oral pill",
                  "IUCD", "Sub-dermal implant",
                  "Sterilisation (F)", "Sterilisation (M)"),
  cyp_factor  = c(1/4,     # DMPA: 4 injections (3-monthly) = 1 CYP
                  1/6,     # NET-EN: 6 injections (2-monthly) = 1 CYP
                  1/15,    # oral pill: 15 cycles = 1 CYP
                  4.6,     # IUCD insertion: 4.6 CYP per device (avg use life)
                  3.5,     # implant insertion: 3.5 CYP per device
                  10,      # female sterilisation: 10 CYP per procedure
                  10)      # male sterilisation: 10 CYP per procedure
)
setkey(cyp_factors, short_label)

dhis_contra_cyp <- cyp_factors[dhis_contra, on = "short_label", nomatch = 0]
dhis_contra_cyp[, cyp := value * cyp_factor]

agg_cyp_national_annual <- dhis_contra_cyp[,
  .(cyp = sum(cyp, na.rm = TRUE)),
  by = .(short_label, year)
][order(short_label, year)]

agg_cyp_national_annual_total <- dhis_contra_cyp[,
  .(cyp = sum(cyp, na.rm = TRUE)),
  by = .(year)
][order(year)]

agg_cyp_prov_annual <- dhis_contra_cyp[!is.na(province) & province != "",
  .(cyp = sum(cyp, na.rm = TRUE)),
  by = .(province, year)
][order(province, year)]

# ── Condoms: fold into the CYP aggregates above ──────────────────────────────
# Standard USAID/FP2030 CYP factor for condoms is 120 units = 1 CYP; applied to
# male and female condoms separately (no separate published factor exists for
# female condoms — this is a working assumption, flagged in the report methods
# note, not a verified national standard).
cyp_factors <- rbindlist(list(
  cyp_factors,
  data.table(short_label = c("Male condom", "Female condom"), cyp_factor = c(1/120, 1/120))
))
setkey(cyp_factors, short_label)

dhis_condom_cyp <- cyp_factors[dhis_condom_annual, on = "short_label", nomatch = 0]
dhis_condom_cyp[, cyp := value * cyp_factor]

agg_cyp_national_annual <- rbindlist(list(
  agg_cyp_national_annual,
  dhis_condom_cyp[, .(cyp = sum(cyp, na.rm = TRUE)), by = .(short_label, year)]
))[order(short_label, year)]

agg_cyp_national_annual_total <- agg_cyp_national_annual[
  , .(cyp = sum(cyp, na.rm = TRUE)), by = .(year)
][order(year)]

agg_cyp_prov_annual <- rbindlist(list(
  agg_cyp_prov_annual,
  dhis_condom_cyp[!is.na(province) & province != "",
    .(cyp = sum(cyp, na.rm = TRUE)), by = .(province, year)]
))[, .(cyp = sum(cyp, na.rm = TRUE)), by = .(province, year)][order(province, year)]

# National/provincial condom volume trend (own aggregates — kept separate from
# the facility-grain contra aggregates, for a dedicated condoms sub-section).
agg_condom_national_annual <- dhis_condom_annual[,
  .(value = sum(value, na.rm = TRUE)), by = .(short_label, year)
][order(short_label, year)]

agg_condom_prov_annual <- dhis_condom_annual[!is.na(province) & province != "",
  .(value = sum(value, na.rm = TRUE)), by = .(province, short_label, year)
][order(province, short_label, year)]


# ─── 7g-ii. CONDOM CYP SENSITIVITY (national) ────────────────────────────────
# Condom counts measure distribution, not confirmed use — these scenarios vary
# assumed real-world utilization of distributed condoms while holding the
# 120-units-per-CYP factor fixed. Purely additive: does not alter
# agg_cyp_national_annual/_total/_prov, which stay at the 100%-used default.
noncondom_cyp_annual <- agg_cyp_national_annual[
  !short_label %in% c("Male condom", "Female condom"),
  .(noncondom_cyp = sum(cyp, na.rm = TRUE)), by = year
]

condom_units_annual <- dhis_condom_annual[, .(units = sum(value, na.rm = TRUE)), by = year]

CONDOM_CYP_SCENARIOS <- data.table(
  scenario    = c("Excluded", "Conservative (50% used)", "Standard (100% used, current default)"),
  utilization = c(0, 0.5, 1.0)
)

agg_condom_sensitivity_annual <- CJ(
  scenario = CONDOM_CYP_SCENARIOS$scenario,
  year     = noncondom_cyp_annual$year
)
agg_condom_sensitivity_annual <- CONDOM_CYP_SCENARIOS[agg_condom_sensitivity_annual, on = "scenario"]
agg_condom_sensitivity_annual <- condom_units_annual[agg_condom_sensitivity_annual, on = "year"]
agg_condom_sensitivity_annual <- noncondom_cyp_annual[agg_condom_sensitivity_annual, on = "year"]
agg_condom_sensitivity_annual[, condom_cyp := units * utilization / 120]
agg_condom_sensitivity_annual[, total_cyp := noncondom_cyp + condom_cyp]
agg_condom_sensitivity_annual[, scenario := factor(scenario, levels = CONDOM_CYP_SCENARIOS$scenario)]
setorder(agg_condom_sensitivity_annual, scenario, year)


# ─── 7h. MONTHLY OUTLIER FLAGGING (contraception domain) ─────────────────────
# Robust (median/MAD-based) z-scores computed WITHIN each facility × indicator
# monthly series, so a facility is only ever compared against its own history
# — this normalises for facility size and flags sudden reporting spikes/drops
# rather than simply large facilities. Facilities need >= 6 months of data to
# be scored (MAD is unstable on very short series).
dhis_contra_monthly <- dhis_monthly[domain == "contra" & !is.na(period_date)]

robust_z <- function(x) {
  m   <- stats::median(x, na.rm = TRUE)
  mad <- stats::mad(x, na.rm = TRUE)
  if (is.na(mad) || mad == 0) return(rep(NA_real_, length(x)))
  (x - m) / mad
}

dhis_contra_monthly[
  , n_months := .N, by = .(province, district, facility, short_label)
][
  , z_robust := if (n_months[1] >= 6) robust_z(value) else NA_real_,
  by = .(province, district, facility, short_label)
]

# Flagged as an outlier if |robust z| > 3.5 (Iglewicz & Hoaglin threshold)
agg_contra_outliers_monthly <- dhis_contra_monthly[
  !is.na(z_robust) & abs(z_robust) > 3.5,
  .(province, district, facility, short_label, period_date, value, z_robust)
][order(-abs(z_robust))]


# ─── 7i. REPORTING HEATMAP GRIDS (contraception domain) ──────────────────────
# Two heatmap-ready grids so gaps/outliers are visible at a glance:
#   (i)  district x month reporting rate — one row per district, whole country
#   (ii) facility x month grid WITHIN a district — total contra volume per
#        facility per month, with a robust z-score for colour and TRUE blanks
#        (no row at all) for months the facility didn't report anything.

# (i) District x month reporting-rate matrix, ordered worst-reporting first.
agg_contra_heatmap_district <- copy(agg_contra_completeness_district_monthly)
agg_contra_heatmap_district[
  , district_label := paste0(district, " (", province, ")")
]
district_order <- agg_contra_district_poor_reporting[
  , district_label := paste0(district, " (", province, ")")
][order(mean_reporting_rate), district_label]
agg_contra_heatmap_district[
  , district_label := factor(district_label, levels = rev(district_order))
]

# (ii) Facility x month grid. Build the FULL cross of every facility that ever
# reported any contra indicator against every month in the monthly window, so
# months with a genuine gap (no row in the source data at all) are explicit
# NAs rather than silently absent.
fac_keys    <- unique(dhis_contra_monthly[, .(province, district, facility)])
all_months  <- sort(unique(dhis_contra_monthly$period_date))

fac_month_totals <- dhis_contra_monthly[,
  .(total_value = sum(value, na.rm = TRUE)),
  by = .(province, district, facility, period_date)
]
fac_grid <- fac_keys[, .(period_date = all_months), by = .(province, district, facility)]
fac_grid <- fac_month_totals[fac_grid, on = c("province", "district", "facility", "period_date")]

fac_grid[
  , n_reported_months := sum(!is.na(total_value)), by = .(province, district, facility)
][
  , z_robust := if (n_reported_months[1] >= 6) robust_z(total_value) else NA_real_,
  by = .(province, district, facility)
]

agg_contra_facility_monthly_grid <- fac_grid[order(province, district, facility, period_date)]


# ─── 7j. CYP DATA-QUALITY DEEP DIVE (facility x month x method) ──────────────
# The heatmaps/outliers above pool ALL contra indicators (including TOP,
# which is not a contraceptive method) into one facility-level total. This
# section restricts to the facility-based methods that actually feed CYP
# (excludes TOP; excludes condoms, which have no monthly facility data — see
# section 4b) and keeps the METHOD dimension separate, since a spike in one
# method at a facility can be masked by summing across methods.
CYP_ELIGIBLE_METHODS <- setdiff(cyp_factors$short_label, c("Male condom", "Female condom"))

dhis_cyp_monthly <- dhis_contra_monthly[short_label %in% CYP_ELIGIBLE_METHODS]

# Outlier rate by method — which CYP-feeding methods are noisiest to report?
agg_cyp_outlier_rate_method <- dhis_cyp_monthly[
  , .(n_facility_months = .N,
      n_outliers = sum(!is.na(z_robust) & abs(z_robust) > 3.5)),
  by = short_label
][, outlier_rate := n_outliers / n_facility_months][order(-outlier_rate)]

# Facility x month x method outlier table, CYP-eligible methods only (subset
# of the all-indicator table already computed in section 7h).
agg_cyp_outliers_monthly <- agg_contra_outliers_monthly[short_label %in% CYP_ELIGIBLE_METHODS]

# The single district with the most flagged CYP outliers, used to scope a
# facility x month x method heatmap to a legible size (a full national
# district x method combination heatmap would need 50+ districts x 7 methods
# of traces, too large to render usefully in one figure).
cyp_worst_district <- agg_cyp_outliers_monthly[, .N, by = .(province, district)][order(-N)][1]

cyp_deepdive_raw <- dhis_cyp_monthly[
  province == cyp_worst_district$province & district == cyp_worst_district$district
]
all_months_cyp <- sort(unique(dhis_cyp_monthly$period_date))
grid_cyp <- CJ(
  facility    = unique(cyp_deepdive_raw$facility),
  short_label = CYP_ELIGIBLE_METHODS,
  period_date = all_months_cyp,
  sorted      = FALSE
)
agg_cyp_facility_method_monthly_grid <- cyp_deepdive_raw[
  grid_cyp, on = c("facility", "short_label", "period_date")
][, .(facility, short_label, period_date, value, z_robust,
      province = cyp_worst_district$province, district = cyp_worst_district$district)]

cat("\nCYP deep-dive district:", cyp_worst_district$district,
    "(", cyp_worst_district$province, ") —", cyp_worst_district$N, "flagged outlier facility-months\n")


# ─── 8. AUDIT RECORD ──────────────────────────────────────────────────────────
audit <- list(
  run_timestamp      = Sys.time(),
  r_version          = paste(R.version$major, R.version$minor, sep = "."),
  source_annual      = normalizePath(FILE_ANNUAL),
  source_monthly     = normalizePath(FILE_MONTHLY),
  annual_sheets      = annual_sheets,
  monthly_sheets     = monthly_sheets,
  n_rows_annual_raw  = nrow(dhis_annual_raw),
  n_rows_monthly_raw = nrow(dhis_monthly_raw),
  n_facilities       = uniqueN(dhis_annual$paste <- paste(
                          dhis_annual$province, dhis_annual$district, dhis_annual$facility)),
  year_range_annual  = range(dhis_annual$year, na.rm = TRUE),
  period_range_monthly = range(dhis_monthly$period_date, na.rm = TRUE),
  n_indicators       = uniqueN(dhis_annual$indicator),
  domains            = c("births", "deaths", "contra", "condom"),
  unmapped_indicators = unmapped,
  dedup_monthly_removed = nrow(rbindlist(monthly_parts)) - nrow(dhis_monthly_raw),
  source_condom      = normalizePath(FILE_CONDOM),
  condom_year_range  = range(dhis_condom_annual$year, na.rm = TRUE),
  cyp_deepdive_district = paste0(cyp_worst_district$district, " (", cyp_worst_district$province, ")")
)
# clean up the temp paste column used for facility count
dhis_annual[, paste := NULL]

cat("\n─── Audit record ─────────────────────────────────────────────────────────\n")
cat("Run timestamp    :", format(audit$run_timestamp), "\n")
cat("Annual year range:", audit$year_range_annual[1], "–", audit$year_range_annual[2], "\n")
cat("Monthly period   :", format(audit$period_range_monthly[1]),
    "–", format(audit$period_range_monthly[2]), "\n")
cat("Indicators       :", audit$n_indicators, "\n")
cat("Domains          :", paste(audit$domains, collapse = ", "), "\n")


# ─── 9. SAVE ──────────────────────────────────────────────────────────────────

save(
  # Primary split datasets
  dhis_births,
  dhis_deaths,
  dhis_contra,
  # Full combined
  dhis_annual,
  dhis_monthly,
  # Metadata
  indicator_meta,
  # Pre-baked aggregations
  agg_national_annual,
  agg_prov_annual,
  agg_national_monthly,
  agg_completeness_annual,
  agg_completeness_monthly,
  agg_facility_totals,
  # Contraception domain: facility-count method, CYP, monthly QA
  CONTRA_FACILITY_KEY,
  agg_contra_completeness_annual,
  agg_contra_completeness_monthly,
  agg_contra_completeness_district_monthly,
  agg_contra_district_poor_reporting,
  cyp_factors,
  agg_cyp_national_annual,
  agg_cyp_national_annual_total,
  agg_cyp_prov_annual,
  agg_contra_outliers_monthly,
  agg_contra_heatmap_district,
  agg_contra_facility_monthly_grid,
  # CYP data-quality deep dive (facility x month x method, CYP-eligible only)
  CYP_ELIGIBLE_METHODS,
  agg_cyp_outlier_rate_method,
  agg_cyp_outliers_monthly,
  agg_cyp_facility_method_monthly_grid,
  cyp_worst_district,
  # Condoms (district+year grain, separate source file — see section 4b)
  dhis_condom_annual,
  agg_condom_national_annual,
  agg_condom_prov_annual,
  # Condom CYP sensitivity (utilization-discount scenarios, national only)
  CONDOM_CYP_SCENARIOS,
  agg_condom_sensitivity_annual,
  # Provenance
  audit,
  file = OUTPUT_RDA
)

cat("\n✓ Saved to:", OUTPUT_RDA, "\n")
cat("  Objects: dhis_births, dhis_deaths, dhis_contra, dhis_annual, dhis_monthly,\n")
cat("           dhis_condom_annual, indicator_meta, agg_*, cyp_factors, audit\n")
