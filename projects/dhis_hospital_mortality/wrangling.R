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

stopifnot(
  "Annual file not found"  = file.exists(FILE_ANNUAL),
  "Monthly file not found" = file.exists(FILE_MONTHLY)
)
cat("Source files confirmed:\n  ", FILE_ANNUAL, "\n  ", FILE_MONTHLY, "\n\n")


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
    "Termination of pregnancy 20 years and older"
  ),
  domain = c(
    rep("births", 9),
    rep("deaths", 9),
    rep("contra", 10)
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
    "TOP 0-12wk", "TOP 13-20wk", "TOP 10-19y", "TOP 20+y"
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
  domains            = c("births", "deaths", "contra"),
  unmapped_indicators = unmapped,
  dedup_monthly_removed = nrow(rbindlist(monthly_parts)) - nrow(dhis_monthly_raw)
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
  # Provenance
  audit,
  file = OUTPUT_RDA
)

cat("\n✓ Saved to:", OUTPUT_RDA, "\n")
cat("  Objects: dhis_births, dhis_deaths, dhis_contra, dhis_annual, dhis_monthly,\n")
cat("           indicator_meta, agg_*, audit\n")
