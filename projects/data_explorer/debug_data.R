# Check data distribution for debugging the overview plot
library(arrow)
library(data.table)

cat("Loading data...\n")
dt <- as.data.table(read_feather("Deaths2022_MRCversionFINAL.feather"))

cat("\n=== Year distribution ===\n")
year_counts <- dt[, .N, by = epi_year][order(epi_year)]
print(year_counts)

cat("\n=== Deaths per year summary ===\n")
cat("Min year:", min(dt$epi_year, na.rm = TRUE), "\n")
cat("Max year:", max(dt$epi_year, na.rm = TRUE), "\n")
cat("Total records:", nrow(dt), "\n")

cat("\n=== Weekly counts for a sample year (2020) ===\n")
weekly_2020 <- dt[epi_year == 2020, .N, by = .(epi_year, epi_week)][order(epi_week)]
print(head(weekly_2020, 10))

cat("\n=== Check for years with very few records ===\n")
low_years <- year_counts[N < 10000]
if (nrow(low_years) > 0) {
  print(low_years)
} else {
  cat("All years have >10,000 records\n")
}
