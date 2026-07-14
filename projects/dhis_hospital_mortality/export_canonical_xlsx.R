# =============================================================================
# DHIS2 Hospital Indicators — Canonical XLSX Export (Reviewer Package)
# =============================================================================
# Purpose:
#   Build a reviewer-ready Excel workbook from dhis_results.rda with clear,
#   self-documented tabs and stable column ordering.
#
# Run from workspace root:
#   Rscript projects/dhis_hospital_mortality/export_canonical_xlsx.R
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

PROJECT_DIR <- "projects/dhis_hospital_mortality"
INPUT_RDA <- file.path(PROJECT_DIR, "dhis_results.rda")
OUTPUT_XLSX <- file.path(PROJECT_DIR, "dhis_canonical_for_review.xlsx")

EXCEL_MAX_ROWS <- 1048576L
# Reserve row 1 for headers
SHEET_DATA_MAX_ROWS <- EXCEL_MAX_ROWS - 1L

stopifnot("Input RDA not found" = file.exists(INPUT_RDA))

load(INPUT_RDA)

# Normalize object classes for safer Excel writing.
to_dt <- function(x) {
  if (is.data.table(x)) return(copy(x))
  as.data.table(x)
}

# Write a table across one or more sheets if it exceeds Excel's row limit.
write_dt_chunked <- function(wb, dt, base_sheet_name) {
  dt <- to_dt(dt)
  n <- nrow(dt)

  if (n <= SHEET_DATA_MAX_ROWS) {
    addWorksheet(wb, base_sheet_name)
    writeDataTable(wb, base_sheet_name, dt, withFilter = TRUE, tableStyle = "TableStyleMedium2")
    freezePane(wb, base_sheet_name, firstRow = TRUE)
    setColWidths(wb, base_sheet_name, cols = 1:ncol(dt), widths = "auto")
    return(invisible(list(sheet_names = base_sheet_name, n_chunks = 1L)))
  }

  n_chunks <- as.integer(ceiling(n / SHEET_DATA_MAX_ROWS))
  sheet_names <- character(n_chunks)

  for (i in seq_len(n_chunks)) {
    start_i <- (i - 1L) * SHEET_DATA_MAX_ROWS + 1L
    end_i <- min(i * SHEET_DATA_MAX_ROWS, n)
    chunk <- dt[start_i:end_i]
    sheet_name <- sprintf("%s_%02d", base_sheet_name, i)
    sheet_names[i] <- sheet_name

    addWorksheet(wb, sheet_name)
    writeDataTable(wb, sheet_name, chunk, withFilter = TRUE, tableStyle = "TableStyleMedium2")
    freezePane(wb, sheet_name, firstRow = TRUE)
    setColWidths(wb, sheet_name, cols = 1:ncol(chunk), widths = "auto")
  }

  invisible(list(sheet_names = sheet_names, n_chunks = n_chunks))
}

# Build a compact audit table from the audit list.
audit_dt <- rbindlist(lapply(names(audit), function(nm) {
  val <- audit[[nm]]
  if (length(val) == 0L) val <- NA_character_
  data.table(
    field = nm,
    value = paste(as.character(val), collapse = " | ")
  )
}), fill = TRUE)

# Canonical sheet order and standardized datasets.
indicator_map <- to_dt(indicator_meta)[order(domain, short_label)]

annual_canonical <- to_dt(dhis_annual)[
  , .(domain, indicator, short_label, year,
      province, district, subdistrict, facility, value)
][order(domain, indicator, year, province, district, subdistrict, facility)]

monthly_canonical <- to_dt(dhis_monthly)[
  , .(domain, indicator, short_label, period_date, year, month,
      province, district, subdistrict, facility, value)
][order(domain, indicator, period_date, province, district, subdistrict, facility)]

agg_national_annual_dt <- to_dt(agg_national_annual)[order(domain, indicator, year)]
agg_prov_annual_dt <- to_dt(agg_prov_annual)[order(domain, indicator, province, year)]
agg_national_monthly_dt <- to_dt(agg_national_monthly)[order(domain, indicator, period_date)]
agg_completeness_annual_dt <- to_dt(agg_completeness_annual)[order(domain, year)]
agg_completeness_monthly_dt <- to_dt(agg_completeness_monthly)[order(domain, period_date)]
agg_facility_totals_dt <- to_dt(agg_facility_totals)[order(domain, indicator, -total_value)]

readme_dt <- data.table(
  section = c(
    "Package",
    "Purpose",
    "Generated at",
    "Source RDA",
    "Sheet guide",
    "Sheet guide",
    "Sheet guide",
    "Sheet guide",
    "Sheet guide",
    "Sheet guide",
    "Sheet guide",
    "Sheet guide",
    "Notes"
  ),
  detail = c(
    "DHIS2 Hospital Indicators canonical reviewer dataset",
    "Stable, analysis-ready export for external review",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    normalizePath(INPUT_RDA),
    "README: this overview",
    "indicator_map: indicator-domain-label lookup",
    "annual_canonical: annual long-format records (2015-2025)",
    "monthly_canonical: monthly long-format records (Apr 2024-Mar 2026)",
    "agg_national_annual: national annual totals by indicator",
    "agg_prov_annual: province annual totals by indicator",
    "agg_national_monthly: national monthly totals by indicator",
    "audit: provenance and wrangling metadata",
    "If a dataset exceeds Excel row limits, it is split across _01, _02, ... tabs"
  )
)

wb <- createWorkbook(creator = "MRC_VR pipeline")

addWorksheet(wb, "README")
writeData(wb, "README", readme_dt)
setColWidths(wb, "README", cols = 1:2, widths = c(24, 120))

addWorksheet(wb, "indicator_map")
writeDataTable(wb, "indicator_map", indicator_map, withFilter = TRUE, tableStyle = "TableStyleMedium2")
freezePane(wb, "indicator_map", firstRow = TRUE)
setColWidths(wb, "indicator_map", cols = 1:ncol(indicator_map), widths = "auto")

write_dt_chunked(wb, annual_canonical, "annual_canonical")
write_dt_chunked(wb, monthly_canonical, "monthly_canonical")
write_dt_chunked(wb, agg_national_annual_dt, "agg_national_annual")
write_dt_chunked(wb, agg_prov_annual_dt, "agg_prov_annual")
write_dt_chunked(wb, agg_national_monthly_dt, "agg_national_monthly")
write_dt_chunked(wb, agg_completeness_annual_dt, "agg_completeness_ann")
write_dt_chunked(wb, agg_completeness_monthly_dt, "agg_completeness_mon")
write_dt_chunked(wb, agg_facility_totals_dt, "agg_facility_totals")

addWorksheet(wb, "audit")
writeDataTable(wb, "audit", audit_dt, withFilter = TRUE, tableStyle = "TableStyleMedium2")
freezePane(wb, "audit", firstRow = TRUE)
setColWidths(wb, "audit", cols = 1:ncol(audit_dt), widths = c(34, 120))

saveWorkbook(wb, OUTPUT_XLSX, overwrite = TRUE)

cat("Canonical reviewer workbook written to:\n")
cat("  ", OUTPUT_XLSX, "\n")
