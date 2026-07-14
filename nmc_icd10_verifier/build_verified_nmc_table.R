#!/usr/bin/env Rscript
# nmc_icd10_verifier/build_verified_nmc_table.R
#
# Purpose: Join the SA NMC ICD-10 reference map (Excel) to the NMC surveillance
#   master file and produce:
#   1. outputs/verified_nmc_condition_icd10.csv  — verified two-column NMC ↔ ICD-10 map
#   2. outputs/nmc_icd_annual_counts.csv         — cases per condition per year

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
  library(arrow)
})

# ── Paths ─────────────────────────────────────────────────────────────────────
args        <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
script_dir  <- if (length(script_path) == 1) dirname(normalizePath(script_path)) else getwd()
project_root <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)

icd_map_path  <- file.path(project_root, "data", "NMC_ICD_link", "SA_NMC_ICD_Codes_v3.xlsx")
nmc_data_path <- path.expand("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.feather")
output_dir    <- file.path(project_root, "outputs")

# ── Helpers ───────────────────────────────────────────────────────────────────

normalise_name <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

# ── Manual overrides for condition name mismatches ────────────────────────────
# Maps normalised NMC feather condition → normalised Excel NMC condition.
# Add entries here whenever names diverge between the two sources.
condition_overrides <- c(
  # NMC feather condition (normalised)                              = Excel condition (normalised)
  "covid 19"                                                        = "coronavirus disease covid 19",
  "crimean congo viral haemorrhagic fever human"                    = "viral haemorrhagic fever diseases",
  "ebola virus vhf"                                                 = "viral haemorrhagic fever diseases",
  "marburg virus vhf"                                               = "viral haemorrhagic fever diseases",
  "endemic arboviral diseases chikungunya virus"                    = "endemic arboviral diseases west nile sindbis chikungunya",
  "endemic arboviral diseases sindbis virus"                        = "endemic arboviral diseases west nile sindbis chikungunya",
  "endemic arboviral diseases west nile virus"                      = "endemic arboviral diseases west nile sindbis chikungunya",
  "food borne illness outbreak"                                     = "food borne disease outbreak",
  "agricultural or stock remedy poisoning"                          = "agricultural stock remedy poisoning",
  "enteric fever typhoid or paratyphoid fever"                      = "enteric fever typhoid paratyphoid",
  "maternal death pregnancy childbirth and puerperium"              = "maternal death",
  "non typhoidal salmonellosis"                                     = "non typhoidal salmonellosis nts",
  "shiga toxin producing escherichia coli"                          = "shiga toxin producing e coli stec",
  "tuberculosis extensively drug resistant xdr tb"                  = "tuberculosis xdr tb",
  "tuberculosis multidrug resistant mdr tb"                         = "tuberculosis mdr tb",
  "respiratory disease caused by a novel respiratory pathogen"      = "respiratory disease novel pathogen",
  "rabies"                                                          = "rabies human"
  # "waterborne illness outbreak undefined" has no entry in the Excel map
)

# Extract the first clean ICD-10 code token from a string that may contain
# compound codes like "A15.0 + U51" — returns only the primary code.
extract_primary_icd10 <- function(x) {
  x   <- toupper(as.character(x))
  pat <- "[A-Z][0-9][0-9A-Z](\\.[0-9A-Z]{1,4})?"
  m   <- regexpr(pat, x, perl = TRUE)
  out <- rep(NA_character_, length(x))
  # regmatches(x, m) returns only the matched substrings (length == sum(m > 0))
  out[m > 0] <- regmatches(x, m)
  out
}

# ── Load Excel ICD-10 reference map ───────────────────────────────────────────

load_icd_map <- function() {
  if (!file.exists(icd_map_path)) stop("Excel map not found: ", icd_map_path)

  raw <- read_excel(icd_map_path, sheet = "NMC ICD Codes v3", skip = 2)

  # Standardise column names
  names(raw) <- c(
    "nmc_category", "nmc_condition", "icd10_code", "icd11_code",
    "pathogen_notes", "surveillance_artefacts", "macod_note", "reporting_timeframe"
  )

  icd_map <- raw |>
    filter(!is.na(nmc_condition), !is.na(icd10_code)) |>
    mutate(
      nmc_category  = trimws(nmc_category),
      nmc_condition = trimws(nmc_condition),
      icd10_code    = extract_primary_icd10(icd10_code),
      # normalised key for joining
      join_key      = normalise_name(nmc_condition)
    ) |>
    filter(!is.na(icd10_code)) |>
    select(nmc_category, nmc_condition, icd10_code, join_key)

  # The current SA map workbook has no Mpox row; add the verified ICD-10 code.
  bind_rows(
    icd_map,
    tibble::tibble(
      nmc_category = "Cat 1",
      nmc_condition = "Mpox",
      icd10_code = "B04",
      join_key = normalise_name("Mpox")
    )
  ) |>
    distinct(join_key, .keep_all = TRUE)
}

# ── Load NMC surveillance master ──────────────────────────────────────────────

load_nmc_data <- function() {
  if (!file.exists(nmc_data_path)) stop("NMC feather not found: ", nmc_data_path)

  message("Reading NMC master file...")
  nmc <- read_feather(nmc_data_path,
                      col_select = c("condition", "year", "duplicate"))

  nmc |>
    # collect to R before custom mutate logic (Arrow compute doesn't support it)
    collect() |>
    # "duplicate" column is a character: keep "unique" and NAs
    filter(is.na(duplicate) | tolower(as.character(duplicate)) != "duplicate") |>
    filter(!is.na(condition), !is.na(year)) |>
    mutate(
      year     = as.integer(year),
      # apply manual overrides then fall back to simple normalisation
      join_key = {
        nk             <- normalise_name(condition)
        override_target <- condition_overrides[nk]
        # normalise the override target so spacing/punct differences don't matter
        ifelse(!is.na(override_target),
               normalise_name(override_target),
               nk)
      }
    ) |>
    select(condition, year, join_key)
}

# ── Main ──────────────────────────────────────────────────────────────────────

main <- function() {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # 1. Load the verified ICD-10 map from Excel
  icd_map <- load_icd_map()
  message("ICD map rows: ", nrow(icd_map))

  # 2. Output 1: verified two-column NMC ↔ ICD-10 table
  verified_map <- icd_map |>
    select(nmc_condition, icd10_code) |>
    arrange(nmc_condition)

  verified_out <- file.path(output_dir, "verified_nmc_condition_icd10.csv")
  write_csv(verified_map, verified_out)
  message("Verified map written (", nrow(verified_map), " rows): ", verified_out)

  # 3. Load NMC surveillance data and join
  nmc <- load_nmc_data()
  message("NMC records loaded: ", nrow(nmc))

  joined <- nmc |>
    inner_join(icd_map, by = "join_key")

  unmatched <- nmc |>
    anti_join(icd_map, by = "join_key") |>
    distinct(condition) |>
    pull(condition)

  if (length(unmatched) > 0) {
    message("Conditions in NMC data with NO ICD-10 match (", length(unmatched), "):")
    message(paste0("  - ", unmatched, collapse = "\n"))
  }

  # 4. Annual counts per condition + ICD-10
  annual_counts <- joined |>
    count(nmc_category, nmc_condition, icd10_code, year, name = "n_cases") |>
    arrange(nmc_category, nmc_condition, year)

  counts_out <- file.path(output_dir, "nmc_icd_annual_counts.csv")
  write_csv(annual_counts, counts_out)
  message("Annual counts written (", nrow(annual_counts), " rows): ", counts_out)

  # 5. Wide summary: conditions as rows, years as columns
  year_range <- sort(unique(annual_counts$year))
  wide <- annual_counts |>
    pivot_wider(
      id_cols     = c(nmc_category, nmc_condition, icd10_code),
      names_from  = year,
      values_from = n_cases,
      values_fill = 0L
    ) |>
    arrange(nmc_category, nmc_condition)

  wide_out <- file.path(output_dir, "nmc_icd_annual_counts_wide.csv")
  write_csv(wide, wide_out)
  message("Wide table written (", nrow(wide), " conditions × ", ncol(wide) - 3,
          " years): ", wide_out)
}

main()
