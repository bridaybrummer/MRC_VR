suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(dplyr)
  library(readr)
  library(haven)
})

# ── Paths ─────────────────────────────────────────────────────────────────────
project_dir  <- "projects/nmc_macod_comparison"
root         <- here::here()   # falls back to getwd() if {here} not installed

nmc_feather  <- path.expand("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.feather")
lgh_feather  <- "LGH_MasterFile_preCollapsedAll.feather"
icd_map_csv  <- "outputs/verified_nmc_condition_icd10.csv"

out_nmc_vs   <- file.path(project_dir, "nmc_vital_status_counts.rds")
out_macod    <- file.path(project_dir, "macod_nmc_counts.rds")

# ── Helpers ───────────────────────────────────────────────────────────────────

normalise_name <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

# Condition name mismatches between NMC feather and the verified ICD map.
condition_overrides <- c(
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
)

make_join_key <- function(condition) {
  nk <- normalise_name(condition)
  ov <- condition_overrides[nk]
  ifelse(!is.na(ov), normalise_name(ov), nk)
}

decode_labelled <- function(dt) {
  for (v in names(dt)) {
    if (inherits(dt[[v]], "haven_labelled")) {
      raw <- unclass(dt[[v]])
      if (is.numeric(raw)) set(dt, j = v, value = as.integer(raw))
      else                  set(dt, j = v, value = as.character(raw))
    }
  }
  dt
}

# ── 1. Verified NMC ICD-10 map ────────────────────────────────────────────────

icd_map <- read_csv(icd_map_csv, show_col_types = FALSE) |>
  mutate(
    join_key   = normalise_name(nmc_condition),
    icd3       = substr(icd10_code, 1, 3)   # 3-char prefix for MACOD matching
  )

# For MACOD: one label per 3-char code (collapse multi-condition codes)
icd3_labels <- icd_map |>
  group_by(icd3) |>
  summarise(
    nmc_conditions = paste(sort(unique(nmc_condition)), collapse = " / "),
    .groups = "drop"
  )

message("Verified ICD map loaded: ", nrow(icd_map), " conditions, ",
        n_distinct(icd_map$icd3), " distinct 3-char codes")

# ── 2. NMC vital-status counts per condition per year ─────────────────────────

message("Loading NMC master file…")
nmc_raw <- read_feather(
  nmc_feather,
  col_select = c("condition", "year", "patient_vital_status", "patientoutcome", "duplicate")
)

nmc <- nmc_raw |>
  collect() |>
  filter(is.na(duplicate) | tolower(as.character(duplicate)) != "duplicate") |>
  filter(!is.na(condition), !is.na(year)) |>
  mutate(
    year     = as.integer(year),
    join_key = make_join_key(condition),
    # Harmonise vital-status into three levels
    vital_status = case_when(
      tolower(as.character(patient_vital_status)) == "deceased" ~ "Deceased",
      tolower(as.character(patientoutcome)) %in% c("died", "died (non-covid)") ~ "Deceased",
      tolower(as.character(patient_vital_status)) == "alive"   ~ "Alive",
      TRUE ~ "Unknown / not recorded"
    )
  ) |>
  inner_join(icd_map |> select(join_key, nmc_condition, icd10_code),
             by = "join_key")

nmc_vs_counts <- nmc |>
  count(nmc_condition, icd10_code, year, vital_status, name = "n") |>
  arrange(nmc_condition, year, vital_status)

saveRDS(nmc_vs_counts, out_nmc_vs)
message("NMC vital-status counts saved: ", nrow(nmc_vs_counts), " rows → ", out_nmc_vs)

# Quick summary
nmc_vs_counts |>
  group_by(vital_status) |>
  summarise(total = sum(n), .groups = "drop") |>
  print()

# ── 3. MACOD (LGH) deaths matched to NMC ICD codes ───────────────────────────

message("Loading LGH feather…")
lgh_raw <- as.data.table(read_feather(lgh_feather,
  col_select = c("DeathYear", "UnderlyingCause", "ResProvince", "sex")))
lgh_raw <- decode_labelled(lgh_raw)

# Clean and 3-char-match UnderlyingCause
lgh_raw[, icd3 := toupper(trimws(as.character(UnderlyingCause)))]
lgh_raw[, icd3 := substr(icd3, 1, 3)]
lgh_raw[, DeathYear := as.integer(DeathYear)]

# Filter to NMC-relevant ICD-10 codes and overlap years (NMC started 2016)
nmc_icd3 <- unique(icd3_labels$icd3)
macod <- lgh_raw[
  icd3 %in% nmc_icd3 & !is.na(DeathYear) & DeathYear >= 2016,
  .(icd3, DeathYear)
]

macod_counts <- macod |>
  as_tibble() |>
  count(icd3, DeathYear, name = "n_deaths") |>
  left_join(icd3_labels, by = "icd3") |>
  rename(year = DeathYear) |>
  arrange(nmc_conditions, year)

saveRDS(macod_counts, out_macod)
message("MACOD NMC-matched counts saved: ", nrow(macod_counts), " rows → ", out_macod)

macod_counts |>
  group_by(year) |>
  summarise(total_deaths = sum(n_deaths), .groups = "drop") |>
  print()
