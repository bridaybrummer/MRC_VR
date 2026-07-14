# MRC VR Data Explorer - Deployment Guide

## Overview

This folder contains two versions of the Shiny app:

| File | Data Source | Size | Use Case |
|------|-------------|------|----------|
| `app.R` | Raw feather (1.8GB) | Full features | Local use only |
| `app_aggregated.R` | Aggregated CSVs (~375MB) | Most features | **Deploy to shinyapps.io** |

### Features Removed in Aggregated Version
- Raw Data tab (no individual records)
- Registration delay chart
- Mean age display in Cause Codes table

---

## Pre-Deployment Setup

### 1. Fix SSL Certificate Error (macOS)

If you get this error:
```
SSL connect error: TLS connect error: error:0BFFF065:x509 certificate routines
```

Add this to your `~/.Rprofile`:
```r
options(rsconnect.http = "libcurl")
```

Or run in terminal:
```bash
echo 'options(rsconnect.http = "libcurl")' >> ~/.Rprofile
```

### 2. Verify rsconnect Account

```r
rsconnect::accounts()
```

If no account, set one up:
```r
rsconnect::setAccountInfo(
  name = "YOUR_USERNAME",
  token = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)
```

Get your token from: https://www.shinyapps.io/admin/#/tokens

---

## Deployment Steps

### Quick Deploy (Recommended)

```r
# Set working directory
setwd("/Users/briday/Desktop/study_stats/MRC_VR/projects/data_explorer")

# Fix SSL if needed
options(rsconnect.http = "libcurl")

# Deploy
rsconnect::deployApp(
  appDir = ".",
  appPrimaryDoc = "app_aggregated.R",
  appFiles = c(
    "app_aggregated.R", 
    "aggregated_data",
    "shape_files.rda",
    "population_data.rda"
  ),
  appName = "mrc-vr-explorer",
  account = "briday",
  server = "shinyapps.io",
  forceUpdate = TRUE
)

# or

setwd("/Users/briday/Desktop/study_stats/MRC_VR/projects/data_explorer")
options(rsconnect.http = "libcurl")

rsconnect::deployApp(
  appDir = ".",
  appPrimaryDoc = "app_aggregated.R",
  appFiles = c("app_aggregated.R", "aggregated_data", "shape_files.rda", "population_data.rda"),
  appName = "mrc-vr-explorer",
  account = "briday",
  server = "shinyapps.io",
  forceUpdate = TRUE
)

```

### What Gets Uploaded

```
app_aggregated.R           # Main app file
aggregated_data/
├── full_aggregation.csv   # 361 MB - main data
├── district_year.csv      # 61 KB
├── pyramid_data.csv       # 532 KB
└── (other files)
shape_files.rda            # 25 MB - choropleth maps
population_data.rda        # 396 KB - rate calculations
```

**Total upload: ~400 MB**

---

## Regenerating Aggregated Data

If the source data changes, regenerate the aggregated files:

```r
setwd("/Users/briday/Desktop/study_stats/MRC_VR")
source("projects/data_explorer/create_aggregated_data.R")
```

This creates/updates all CSV files in `aggregated_data/`.

---

## Shinyapps.io Plan Requirements

| Plan | RAM | Active Hours | Suitable? |
|------|-----|--------------|-----------|
| Free | 1 GB | 25/month | ❌ Too small |
| **Basic ($39/mo)** | 8 GB | 500/month | ✅ Recommended |
| Standard ($99/mo) | 8 GB | 2000/month | ✅ Heavy usage |

The app needs ~2GB RAM to load the 361MB CSV into memory.

---

## Testing Locally Before Deploy

```r
setwd("/Users/briday/Desktop/study_stats/MRC_VR/projects/data_explorer")
shiny::runApp("app_aggregated.R", port = 3839)
```

---

## Troubleshooting

### "Shape files not available" on deployed app

The choropleth maps require shape files that aren't included in deployment.
To fix, copy shape files locally:

```r
# Copy shape files to app directory
file.copy(
  "/Users/briday/Desktop/SAFETP/CLA/NMCleaner/data/shape_files.rda",
  "/Users/briday/Desktop/study_stats/MRC_VR/projects/data_explorer/shape_files.rda"
)

# Then add to appFiles in deployApp():
appFiles = c("app_aggregated.R", "aggregated_data", "shape_files.rda")
```

### "Population data not found" warning

Same issue - copy population data if you need rate calculations:

```r
file.copy(
  "/Users/briday/Desktop/SAFETP/CLA/NMCleaner/data/population_data.rda",
  "/Users/briday/Desktop/study_stats/MRC_VR/projects/data_explorer/population_data.rda"
)
```

### App crashes on shinyapps.io

- Check logs: https://www.shinyapps.io/admin/#/applications
- Usually means insufficient RAM → upgrade plan
- Or reduce data size by excluding `full_aggregation.csv` (loses some features)

---

## File Structure

```
projects/data_explorer/
├── app.R                    # Full version (local only)
├── app_aggregated.R         # Deployable version ⭐
├── app_lite.R               # Minimal version (~14MB)
├── create_aggregated_data.R # Script to regenerate CSVs
├── aggregated_data/
│   ├── full_aggregation.csv # 361 MB
│   ├── district_year.csv
│   ├── pyramid_data.csv
│   └── ...
├── README_DEPLOYMENT.md     # This file
└── deploy.R                 # (optional) deployment script
```

---

## Quick Reference Commands

```r
# Check accounts
rsconnect::accounts()

# See deployed apps
rsconnect::applications()

# Terminate an app
rsconnect::terminateApplication("mrc-vr-explorer", account = "briday")

# View logs
rsconnect::showLogs(appName = "mrc-vr-explorer", account = "briday")
```

---

*Last updated: January 2026*
