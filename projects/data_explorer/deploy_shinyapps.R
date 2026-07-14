################################################################################
# Deploy Data Explorer LITE to ShinyApps.io
# 
# This script deploys the lightweight Shiny app.
# Uses pre-aggregated summary data (~14MB) instead of raw data (~3GB).
#
# FIRST TIME SETUP:
# 1. Create account at https://www.shinyapps.io/
# 2. Get your tokens from: Account > Tokens
# 3. Run the rsconnect::setAccountInfo() command below with your credentials
################################################################################

# Install rsconnect if needed
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}
library(rsconnect)

# ============================================================================
# FIRST TIME ONLY: Configure your shinyapps.io account
# Uncomment and fill in your credentials from shinyapps.io > Account > Tokens
# ============================================================================
# rsconnect::setAccountInfo(
#   name   = "YOUR_ACCOUNT_NAME",
#   token  = "YOUR_TOKEN",
#   secret = "YOUR_SECRET"
# )

# ============================================================================
# Verify required data files exist
# ============================================================================
app_dir <- "projects/data_explorer"
data_dir <- file.path(app_dir, "aggregated_data")

required_files <- c(
  "weekly_province.csv", "weekly_cause.csv", "monthly.csv",
  "age_sex_year.csv", "pyramid_data.csv",
  "province_year.csv", "district_year.csv",
  "cause_year.csv", "cause_province_year.csv"
)

cat("Checking required files...\n")
all_exist <- TRUE
for (f in required_files) {
  path <- file.path(data_dir, f)
  if (file.exists(path)) {
    cat("  ✓", f, "\n")
  } else {
    cat("  ✗", f, "- MISSING\n")
    all_exist <- FALSE
  }
}

if (!all_exist) {
  stop("Missing required files! Run create_aggregated_data.R first.")
}

# Calculate total size
total_size <- sum(file.info(file.path(data_dir, required_files))$size) / 1e6
cat("\nTotal data size:", round(total_size, 1), "MB\n")

# ============================================================================
# Deploy the AGGREGATED app
# ============================================================================
cat("\n📦 Deploying AGGREGATED version to ShinyApps.io...\n")

# Use curl backend to avoid SSL errors on macOS
# Note: If this fails with SSL errors, try running in RStudio directly
if (Sys.info()[["sysname"]] == "Darwin") {
  options(rsconnect.http = "curl")
}

# Files to include in deployment
app_files <- c(
  "app_aggregated.R",
  "aggregated_data",
  "shape_files.rda",
  "population_data.rda"
)

rsconnect::deployApp(
  appDir = app_dir,
  appFiles = app_files,
  appName = "mrc-vr-explorer",
  appTitle = "South Africa Mortality Explorer",
  appPrimaryDoc = "app_aggregated.R",
  account = "briday", 
  server = "shinyapps.io",
  forceUpdate = TRUE,
  launch.browser = TRUE
)

cat("\n✅ Deployment complete!\n")
cat("URL: https://briday.shinyapps.io/mrc-vr-explorer/\n")
