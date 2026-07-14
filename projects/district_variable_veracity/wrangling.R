# =============================================================================
# District QA: Death Counts + Crude Rates + Map
# =============================================================================
# Purpose:
#   Simple descriptive checks for district-level data quality:
#   1) verify each district has deaths recorded,
#   2) match deaths to MYPES population,
#   3) map crude death rates to flag potential over/under-reporting areas.
#
# Inputs:
#   Deaths2022_MRCversionFINAL.feather
#   projects/data_explorer/population_data.rda   (MYPES-derived district pop)
#   projects/data_explorer/shape_files.rda       (district sf geometry)
#
# Output:
#   projects/district_variable_veracity/district_veracity_results.rda
#
# Run:
#   Rscript projects/district_variable_veracity/wrangling.R
#
# Optional:
#   ANALYSIS_YEAR=2022 Rscript projects/district_variable_veracity/wrangling.R
# =============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(sf)
  library(ggplot2)
  library(scales)
  library(stringi)
})

PROJECT_DIR <- "projects/district_variable_veracity"
INPUT_DEATHS <- "Deaths2022_MRCversionFINAL.feather"
INPUT_POP <- "projects/data_explorer/population_data.rda"
INPUT_SHAPE <- "projects/data_explorer/shape_files.rda"
OUTPUT_RDA <- file.path(PROJECT_DIR, "district_veracity_results.rda")

stopifnot(
  "Deaths file not found" = file.exists(INPUT_DEATHS),
  "Population file not found" = file.exists(INPUT_POP),
  "Shape file not found" = file.exists(INPUT_SHAPE)
)

norm_key <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]", "", x)
  x
}

alias_key <- function(k) {
  k <- as.character(k)
  alias_map <- c(
    bojanala = "bojanalaplatinum",
    thabomofutsanyane = "thabomofutsanyana"
  )
  idx <- k %in% names(alias_map)
  k[idx] <- alias_map[k[idx]]
  k
}

cat("Reading deaths (district + year only)...\n")
deaths_raw <- tryCatch(
  read_feather(INPUT_DEATHS, col_select = c("DeathYear", "deathdistrictname")),
  error = function(e) read_feather(INPUT_DEATHS)
)
deaths_dt <- as.data.table(deaths_raw)[, .(DeathYear, deathdistrictname)]

# Decode haven-labelled vectors before coercion.
for (v in names(deaths_dt)) {
  if (inherits(deaths_dt[[v]], "haven_labelled")) {
    deaths_dt[, (v) := unclass(get(v))]
  }
}

deaths_dt[, year := as.integer(as.character(DeathYear))]
deaths_dt[, district := trimws(as.character(deathdistrictname))]
deaths_dt <- deaths_dt[!is.na(year) & !is.na(district) & district != ""]
deaths_dt <- deaths_dt[!grepl("unknown|unspecified|outside", district, ignore.case = TRUE)]
deaths_dt[, district_key := alias_key(norm_key(district))]

district_counts_year <- deaths_dt[, .(deaths = .N), by = .(year, district_key)][order(year, district_key)]

analysis_year_env <- suppressWarnings(as.integer(Sys.getenv("ANALYSIS_YEAR", unset = NA_character_)))
analysis_year <- if (!is.na(analysis_year_env)) analysis_year_env else max(district_counts_year$year, na.rm = TRUE)
district_counts_selected <- district_counts_year[year == analysis_year]

cat("Reading population (MYPES) and shape data...\n")
load(INPUT_POP)   # object: pop
load(INPUT_SHAPE) # object: shape_files

pop_dt <- as.data.table(pop)
pop_dt[, year := as.integer(as.character(pop_dt[["Year"]]))]
pop_dt[, district_name := as.character(pop_dt[["district_standard"]])]
pop_dt[, population := as.numeric(pop_dt[["Population"]])]
pop_dt <- pop_dt[!is.na(year) & !is.na(district_name) & district_name != ""]

# Sum over age/sex strata to district total population for each year.
pop_total <- pop_dt[
  , .(population = sum(population, na.rm = TRUE)),
  by = .(year, district_name)
]
pop_total[, district_key := alias_key(norm_key(district_name))]

districts_sf <- shape_files$districts
districts_sf$district_name <- as.character(districts_sf$district_standard)
districts_sf$district_key <- alias_key(norm_key(districts_sf$district_name))

map_ref <- as.data.table(st_drop_geometry(districts_sf))[
  , .(district_key, district_name, province)
]

district_coverage <- merge(
  map_ref,
  district_counts_selected[, .(district_key, deaths)],
  by = "district_key",
  all.x = TRUE
)
district_coverage[is.na(deaths), deaths := 0]
district_coverage[, has_deaths := deaths > 0]
setorder(district_coverage, has_deaths, district_name)

district_rates <- merge(
  district_coverage,
  pop_total[year == analysis_year, .(district_key, population)],
  by = "district_key",
  all.x = TRUE
)
district_rates[, crude_rate_per_100k := fifelse(
  !is.na(population) & population > 0,
  (deaths / population) * 1e5,
  NA_real_
)]

q10 <- suppressWarnings(quantile(district_rates$crude_rate_per_100k, 0.10, na.rm = TRUE))
q90 <- suppressWarnings(quantile(district_rates$crude_rate_per_100k, 0.90, na.rm = TRUE))

district_rates[, rate_flag := fifelse(
  is.na(crude_rate_per_100k), "no_population",
  fifelse(crude_rate_per_100k <= q10, "low", fifelse(crude_rate_per_100k >= q90, "high", "mid"))
)]

district_rate_table <- district_rates[order(-crude_rate_per_100k)]

district_map_sf <- merge(districts_sf, district_rates, by = "district_key", all.x = TRUE)

fig_counts_bar <- ggplot(district_rates[order(-deaths)],
                         aes(x = reorder(district_name, deaths), y = deaths)) +
  geom_col(fill = "#1f78b4") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Deaths", title = paste("Deaths by district (", analysis_year, ")", sep = "")) +
  theme_minimal(base_size = 11)

fig_rates_bar <- ggplot(district_rates[!is.na(crude_rate_per_100k)][order(-crude_rate_per_100k)],
                        aes(x = reorder(district_name, crude_rate_per_100k), y = crude_rate_per_100k,
                            fill = rate_flag)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(low = "#a6cee3", mid = "#b2df8a", high = "#fb9a99", no_population = "#cccccc")) +
  scale_y_continuous(labels = comma) +
  labs(x = NULL, y = "Crude rate per 100,000", fill = "Rate band",
       title = paste("Crude death rate by district (", analysis_year, ")", sep = "")) +
  theme_minimal(base_size = 11)

fig_map_counts <- ggplot(district_map_sf) +
  geom_sf(aes(fill = deaths), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "C", na.value = "grey90", labels = comma) +
  labs(fill = "Deaths", title = paste("District deaths map (", analysis_year, ")", sep = "")) +
  theme_minimal(base_size = 11)

fig_map_rates <- ggplot(district_map_sf) +
  geom_sf(aes(fill = crude_rate_per_100k), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "D", na.value = "grey90", labels = comma) +
  labs(fill = "Rate / 100k", title = paste("District crude death rate map (", analysis_year, ")", sep = "")) +
  theme_minimal(base_size = 11)

death_only_keys <- setdiff(unique(deaths_dt$district_key), unique(map_ref$district_key))
deaths_not_in_map <- unique(deaths_dt[district_key %in% death_only_keys, .(district, district_key)])[order(district)]

qa_summary <- list(
  analysis_year = analysis_year,
  n_districts_map = nrow(map_ref),
  n_districts_with_deaths = district_coverage[has_deaths == TRUE, .N],
  n_districts_no_deaths = district_coverage[has_deaths == FALSE, .N],
  n_death_district_names_not_in_map = nrow(deaths_not_in_map),
  source_deaths = normalizePath(INPUT_DEATHS),
  source_population = normalizePath(INPUT_POP),
  source_shapes = normalizePath(INPUT_SHAPE),
  generated_at = Sys.time()
)

save(
  district_counts_year,
  district_counts_selected,
  district_coverage,
  district_rates,
  district_rate_table,
  district_map_sf,
  deaths_not_in_map,
  fig_counts_bar,
  fig_rates_bar,
  fig_map_counts,
  fig_map_rates,
  qa_summary,
  file = OUTPUT_RDA
)

cat("Saved district QA results to:\n  ", OUTPUT_RDA, "\n")
cat("Analysis year:", analysis_year, "\n")
cat("Districts with deaths:", qa_summary$n_districts_with_deaths,
    "out of", qa_summary$n_districts_map, "\n")
