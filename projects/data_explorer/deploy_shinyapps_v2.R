################################################################################
# Deploy Data Explorer v2 to ShinyApps.io
# 
# Cleaner deployment script with retry logic for stuck tasks.
#
# SETUP (one-time):
#   rsconnect::setAccountInfo(
#     name   = Sys.getenv("SHINYAPPS_NAME"),
#     token  = Sys.getenv("SHINYAPPS_TOKEN"),
#     secret = Sys.getenv("SHINYAPPS_SECRET")
#   )
#
# Or set environment variables in ~/.Renviron:
#   SHINYAPPS_NAME=briday
#   SHINYAPPS_TOKEN=your_token_here
#   SHINYAPPS_SECRET=your_secret_here
################################################################################

# Load rsconnect
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}
library(rsconnect)

# Set working directory to project root (relative, portable)
if (!file.exists("_quarto.yml") && file.exists("../../_quarto.yml")) {
  setwd("../..")
} else if (!file.exists("_quarto.yml")) {
  stop("Run this script from the MRC_VR project root or from projects/data_explorer/")
}
cat("Working directory:", getwd(), "\n\n")

# ============================================================================
# Check account configuration
# ============================================================================
cat("Current accounts:\n")
accounts <- rsconnect::accounts()
print(accounts)

if (nrow(accounts) == 0) {
  stop(
    "No accounts configured!\n",
    "Run once: rsconnect::setAccountInfo(name='briday', token='...', secret='...')\n",
    "Get tokens from: https://www.shinyapps.io/admin/#/tokens"
  )
}

# ============================================================================
# Verify required files exist
# ============================================================================
app_dir <- "projects/data_explorer"

required_files <- c(
  "app_aggregated.R",
  "shape_files.rda",
  "population_data.rda",
  file.path("aggregated_data", "weekly_province.csv"),
  file.path("aggregated_data", "weekly_cause.csv"),
  file.path("aggregated_data", "monthly.csv"),
  file.path("aggregated_data", "age_sex_year.csv"),
  file.path("aggregated_data", "pyramid_data.csv"),
  file.path("aggregated_data", "province_year.csv"),
  file.path("aggregated_data", "district_year.csv"),
  file.path("aggregated_data", "cause_year.csv"),
  file.path("aggregated_data", "cause_province_year.csv")
)

cat("\n📁 Checking required files in", app_dir, "...\n")
all_exist <- TRUE
for (f in required_files) {
  path <- file.path(app_dir, f)
  if (file.exists(path)) {
    size_mb <- round(file.info(path)$size / 1e6, 2)
    cat("  ✓", f, "(", size_mb, "MB)\n")
  } else {
    cat("  ✗", f, "- MISSING!\n")
    all_exist <- FALSE
  }
}

if (!all_exist) {
  stop("Missing required files! Run create_aggregated_data.R first.")
}

# ============================================================================
# Show current deployed apps
# ============================================================================
cat("\n📱 Currently deployed apps:\n")
tryCatch({
  apps <- rsconnect::applications(account = "briday", server = "shinyapps.io")
  if (nrow(apps) > 0) {
    print(apps[, c("name", "status", "url")])
  } else {
    cat("  No apps deployed yet.\n")
  }
}, error = function(e) {
  cat("  Could not fetch apps:", e$message, "\n")
})

# ============================================================================
# Deploy with retry for HTTP 409 (stuck task)
# ============================================================================
cat("\nDeploying to ShinyApps.io...\n")
cat("   App directory:", app_dir, "\n")
cat("   App name: mrc-vr-explorer\n\n")

deploy_with_retry <- function(max_attempts = 3, wait_seconds = 30) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch({
      rsconnect::deployApp(
        appDir = app_dir,
        appFiles = c(
          "app_aggregated.R",
          "aggregated_data",
          "shape_files.rda",
          "population_data.rda"
        ),
        appName = "mrc-vr-explorer",
        appTitle = "SA Mortality Explorer",
        appPrimaryDoc = "app_aggregated.R",
        account = "briday",
        server = "shinyapps.io",
        forceUpdate = TRUE,
        launch.browser = (attempt == 1)
      )
      return(TRUE)
    }, error = function(e) {
      if (grepl("409|tasks already in progress", e$message, ignore.case = TRUE)) {
        if (attempt < max_attempts) {
          cat("\n  Previous deployment still in progress (HTTP 409).",
              "\n  Waiting", wait_seconds, "seconds before retry",
              attempt, "of", max_attempts, "...\n\n")
          Sys.sleep(wait_seconds)
          return(NULL)
        } else {
          stop(
            "Deployment blocked after ", max_attempts, " attempts.\n",
            "A previous task is still running on shinyapps.io.\n",
            "Cancel it at: https://www.shinyapps.io/admin/#/dashboard\n",
            "Then re-run this script.",
            call. = FALSE
          )
        }
      } else {
        stop(e)
      }
    })
    if (isTRUE(result)) break
  }
}

deploy_with_retry()

cat("\nDeployment complete!\n")
cat("URL: https://briday.shinyapps.io/mrc-vr-explorer/\n")
