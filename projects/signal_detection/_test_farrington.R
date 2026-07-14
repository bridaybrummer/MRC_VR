library(arrow); library(haven); library(data.table)
library(lubridate); library(surveillance)

df <- read_feather("Deaths2022_MRCversionFINAL.feather") |> as.data.table()
df[, epi_year := as.integer(as.numeric(epi_year))]
df[, epi_week := as.integer(as.numeric(epi_week))]
df[, UnderlyingCause := as.character(as_factor(UnderlyingCause))]
df <- df[epi_year >= 2010 & epi_year <= 2022]

flu_raw <- df[grepl("^J09|^J10|^J11|^J12|^J13|^J14|^J15|^J16|^J17|^J18", UnderlyingCause),
              .(deaths = .N), by = .(epi_year, epi_week)]
all_weeks <- CJ(epi_year = 2010:2022, epi_week = 1:52)
flu_ts <- flu_raw[all_weeks, on = .(epi_year, epi_week)][is.na(deaths), deaths := 0L]
setorder(flu_ts, epi_year, epi_week)[, week_index := .I]
sts_flu <- sts(observed = matrix(flu_ts$deaths, ncol = 1), start = c(2010, 1), frequency = 52L)

# Test range = NULL
ctrl <- list(range = NULL, b = 5, w = 3, reweight = TRUE, trend = TRUE, noPeriods = 1, alpha = 0.05)
res <- tryCatch(farringtonFlexible(sts_flu, control = ctrl), error = function(e) e)
if (inherits(res, "error")) {
  cat("ERR with NULL:", res$message, "\n")
} else {
  cat("Farrington OK (NULL range), alarms:", sum(res@alarm, na.rm = TRUE), "\n")
}
