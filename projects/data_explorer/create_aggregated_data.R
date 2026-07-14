################################################################################
# Create Pre-Aggregated Data for Safe Deployment
#
# This script creates summarized data files that can be safely:
# 1. Deployed to shinyapps.io (small file size)
# 2. Optionally committed to GitHub (no individual records)
#
# The aggregated data contains only COUNT statistics - no individual-level
# mortality records that could identify persons.
################################################################################

library(data.table)
library(arrow)
library(haven)

cat("Loading full dataset...\n")
dt <- read_feather("Deaths2022_MRCversionFINAL.feather") |> as.data.table()
cat("  Loaded", format(nrow(dt), big.mark = ","), "rows\n")

# Convert labelled columns to standard types to avoid vctrs errors
labelled_cols <- names(dt)[sapply(dt, function(x) inherits(x, "haven_labelled"))]
for (col in labelled_cols) {
  dt[[col]] <- as.character(haven::as_factor(dt[[col]]))
}
cat("  Converted", length(labelled_cols), "labelled columns\n")

# Create output directory
out_dir <- "projects/data_explorer/aggregated_data"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# 1. Temporal aggregations
# ============================================================================
cat("\nCreating temporal aggregations...\n")

# Weekly by province
agg_weekly_province <- dt[, .(
  deaths = .N
), by = .(epi_year, epi_week, DeathProvince)]
fwrite(agg_weekly_province, file.path(out_dir, "weekly_province.csv"))
cat("  ✓ weekly_province.csv:", format(nrow(agg_weekly_province), big.mark = ","), "rows\n")

# Weekly by cause
agg_weekly_cause <- dt[, .(
  deaths = .N
), by = .(epi_year, epi_week, UnderlyingCause)]
fwrite(agg_weekly_cause, file.path(out_dir, "weekly_cause.csv"))
cat("  ✓ weekly_cause.csv:", format(nrow(agg_weekly_cause), big.mark = ","), "rows\n")

# Monthly overall
agg_monthly <- dt[, .(
  deaths = .N
), by = .(DeathYear, DeathMonth)]
fwrite(agg_monthly, file.path(out_dir, "monthly.csv"))
cat("  ✓ monthly.csv:", format(nrow(agg_monthly), big.mark = ","), "rows\n")

# ============================================================================
# 2. Demographic aggregations
# ============================================================================
cat("\nCreating demographic aggregations...\n")

# Age-sex-year
agg_age_sex <- dt[, .(
  deaths = .N
), by = .(epi_year, age, Sex)]
fwrite(agg_age_sex, file.path(out_dir, "age_sex_year.csv"))
cat("  ✓ age_sex_year.csv:", format(nrow(agg_age_sex), big.mark = ","), "rows\n")

# Age group-sex-province-year (for pyramids)
dt[, agegroup5 := cut(age, breaks = seq(0, 100, 5), right = FALSE, 
                       labels = paste0(seq(0, 95, 5), "-", seq(4, 99, 5)))]
agg_pyramid <- dt[, .(
  deaths = .N
), by = .(epi_year, agegroup5, Sex, DeathProvince)]
fwrite(agg_pyramid, file.path(out_dir, "pyramid_data.csv"))
cat("  ✓ pyramid_data.csv:", format(nrow(agg_pyramid), big.mark = ","), "rows\n")

# ============================================================================
# 3. Geographic aggregations
# ============================================================================
cat("\nCreating geographic aggregations...\n")

# Province-year summary
agg_province <- dt[, .(
  deaths = .N,
  natural = sum(NaturalUnnatural == "Natural" | NaturalUnnatural == "1", na.rm = TRUE),
  unnatural = sum(NaturalUnnatural == "Unnatural" | NaturalUnnatural == "2", na.rm = TRUE)
), by = .(epi_year, DeathProvince)]
fwrite(agg_province, file.path(out_dir, "province_year.csv"))
cat("  ✓ province_year.csv:", format(nrow(agg_province), big.mark = ","), "rows\n")

# District-year summary
agg_district <- dt[, .(
  deaths = .N
), by = .(epi_year, deathdistrictname, DeathProvince)]
fwrite(agg_district, file.path(out_dir, "district_year.csv"))
cat("  ✓ district_year.csv:", format(nrow(agg_district), big.mark = ","), "rows\n")

# ============================================================================
# 4. Cause code aggregations
# ============================================================================
cat("\nCreating cause code aggregations...\n")

# Cause by year
agg_cause_year <- dt[, .(
  deaths = .N
), by = .(epi_year, UnderlyingCause)]
fwrite(agg_cause_year, file.path(out_dir, "cause_year.csv"))
cat("  ✓ cause_year.csv:", format(nrow(agg_cause_year), big.mark = ","), "rows\n")

# Cause by province-year
agg_cause_province <- dt[, .(
  deaths = .N
), by = .(epi_year, UnderlyingCause, DeathProvince)]
fwrite(agg_cause_province, file.path(out_dir, "cause_province_year.csv"))
cat("  ✓ cause_province_year.csv:", format(nrow(agg_cause_province), big.mark = ","), "rows\n")

# LGH cause groups (if available)
if ("Code_Main" %in% names(dt)) {
  agg_lgh <- dt[, .(
    deaths = .N
  ), by = .(epi_year, Code_Main, DeathProvince)]
  fwrite(agg_lgh, file.path(out_dir, "lgh_cause_year.csv"))
  cat("  ✓ lgh_cause_year.csv:", format(nrow(agg_lgh), big.mark = ","), "rows\n")
}

# ============================================================================
# 5. Full cross-tabulation for flexible filtering
# ============================================================================
cat("\nCreating full cross-tabulation (this may take a moment)...\n")

# Create 5-year age groups for manageable size
dt[, agegroup10 := cut(age, breaks = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, 150),
                        right = FALSE,
                        labels = c("<1", "1-4", "5-14", "15-24", "25-34", "35-44", 
                                   "45-54", "55-64", "65-74", "75-84", "85+"))]

agg_full <- dt[, .(
  deaths = .N
), by = .(epi_year, epi_week, DeathProvince, agegroup10, Sex, 
          NaturalUnnatural, UnderlyingCause)]
fwrite(agg_full, file.path(out_dir, "full_aggregation.csv"))
cat("  ✓ full_aggregation.csv:", format(nrow(agg_full), big.mark = ","), "rows\n")

# ============================================================================
# Summary
# ============================================================================
cat("\n", strrep("=", 60), "\n")
cat("AGGREGATION COMPLETE\n")
cat(strrep("=", 60), "\n\n")

files <- list.files(out_dir, pattern = "\\.csv$", full.names = TRUE)
sizes <- file.info(files)$size / 1e6  # MB

cat("Output files in:", normalizePath(out_dir), "\n\n")
for (i in seq_along(files)) {
  cat(sprintf("  %-30s %8.2f MB\n", basename(files[i]), sizes[i]))
}
cat(sprintf("\n  TOTAL: %8.2f MB\n", sum(sizes)))

cat("\n📋 Next steps:\n")
cat("  1. These aggregated files can be safely committed to GitHub\n")
cat("  2. Modify app.R to use these files instead of the raw feather\n")
cat("  3. Or deploy to shinyapps.io with much smaller file sizes\n")
