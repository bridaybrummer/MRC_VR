# dashboard_helpers.r
# Clean helper functions for cause code analysis
# This file contains ONLY function definitions - no data loading
# Source this from any Quarto document, then load data separately

library(ggplot2)
library(data.table)
library(flextable)
library(magrittr)
library(glue)
library(tibble)

# Check if NMCleaner is available for flextable_to_rmd
if (!requireNamespace("NMCleaner", quietly = TRUE)) {
  # Define flextable_to_rmd if NMCleaner not available
  flextable_to_rmd <- function(ft) {
    knitr::knit_print(ft)
  }
} else {
  library(NMCleaner)
}

# =============================================================================
# ICD LOOKUP TABLE - Embedded directly to avoid path issues
# This is the same data as LGH_ICD10_Cause_Lookup.rda
# =============================================================================
get_icd_lookup <- function() {
  tibble::tribble(
    ~LGH_Cause,               ~description,
    NA_character_,            "Missing / Unknown",
    "A00-B99",                "Certain infectious and parasitic diseases",
    "B24",                    "Human immunodeficiency virus [HIV] disease",
    "B33",                    "Other viral diseases, not elsewhere classified",
    "C00-D48",                "Neoplasms (malignant, in situ, benign, and uncertain/unknown behaviour)",
    "D50-D99",                "Diseases of the blood and blood-forming organs and certain immune disorders (ICD-10 is D50–D89; D90–D99 not used in ICD-10)",
    "E00-E99*",               "Endocrine, nutritional and metabolic diseases (overall grouping)",
    "G00-G99",                "Diseases of the nervous system",
    "I00-I99*",               "Diseases of the circulatory system (overall grouping)",
    "I49-I51",                "Cardiac arrhythmias and other forms of heart disease",
    "I60-I69",                "Cerebrovascular diseases (stroke, etc.)",
    "J09-J18",                "Influenza and pneumonia",
    "J20-J22",                "Other acute lower respiratory infections (e.g., acute bronchitis/bronchiolitis)",
    "J96-J98",                "Respiratory failure and other specified respiratory disorders",
    "P00-P99",                "Certain conditions originating in the perinatal period",
    "R00-R99+I46",            "Symptoms, signs and abnormal clinical/lab findings (R00–R99) plus cardiac arrest (I46)",
    "ZZOthers (F/H/K-M/O/Q)", "Other chapters: mental/behavioural (F), eye/adnexa & ear/mastoid (H), digestive (K), musculoskeletal (M), pregnancy/childbirth (O), congenital malformations (Q)",
    "E10-E14",                "Diabetes mellitus",
    "I10-I15",                "Hypertensive diseases",
    "I20-I25",                "Ischaemic heart diseases",
    "I26-I28",                "Pulmonary heart disease and diseases of pulmonary circulation",
    "I42",                    "Cardiomyopathy",
    "J00-J99*",               "Diseases of the respiratory system (overall grouping)",
    "J45",                    "Asthma",
    "N00-N99*",               "Diseases of the genitourinary system (overall grouping)",
    "N17-N19",                "Acute and chronic renal failure (N17–N19)",
    "J80",                    "Acute respiratory distress syndrome (ARDS)",
    "U07",                    "COVID-19 (e.g., U07.1 virus identified; U07.2 virus not identified)"
  )
}

# Create icd_lookup in global environment when this file is sourced
if (!exists("icd_lookup")) {
  icd_lookup <- get_icd_lookup()
}

# =============================================================================
# FUNCTION: icd_code_by_agegroup
# Plot ICD code data faceted by age group
# =============================================================================
icd_code_by_agegroup <- function(dt, icd_code = NA, actual_vs_difference = c("actual", "difference")) {
    actual_vs_difference <- match.arg(actual_vs_difference)
    
    if (is.na(icd_code)) {
        stop("Please provide a valid icd_code, such as 'J09-J18', 'U07'")
    }

    if (actual_vs_difference == "actual") {
        return(
            dt[
                LGH_Cause %in% icd_code &
                epi_year %in% 2019:2022,
                .(
                    count = sum(count),
                    baseline = sum(baseline)
                ),
                by = .(agegroup, week_start, epi_week, epi_year)
            ][order(agegroup, -count)] %>%
            ggplot() +
            geom_line(aes(x = week_start, y = count, color = "Observed")) +
            geom_line(aes(x = week_start, y = baseline, color = "Expected")) +
            scale_color_manual(values = c("Expected" = "blue", "Observed" = "red")) +
            facet_wrap(~agegroup, scales = "free_y", nrow = 6) +
            theme_minimal()
        )
    } else if (actual_vs_difference == "difference") {
        return(
            dt[
                LGH_Cause %in% icd_code &
                epi_year %in% 2019:2022,
                {
                    csum <- sum(count, na.rm = TRUE)    
                    bsum <- sum(baseline, na.rm = TRUE)
                    .(
                        count = csum,
                        baseline = bsum,
                        difference = csum - bsum,
                        excess_deficit = ifelse(csum - bsum > 0, "Excess", "Deficit"),
                        covid_period = ifelse(epi_year < 2020, "Pre-COVID", "COVID")
                    )
                },
                by = .(week_start, agegroup)
            ] %>%
            ggplot() +
            geom_col(aes(x = week_start, y = difference, fill = excess_deficit),
                     stat = "identity", position = "dodge", width = 7) + 
            theme_minimal() +
            scale_fill_manual(values = c("Excess" = "orange", "Deficit" = "green")) +
            geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dashed", color = "red") +
            facet_wrap(~agegroup, nrow = 6, scales = "free_y") +
            labs(x = "Cause of Death", y = "Excess Deaths", fill = "Excess vs Deficit") 
        )
    } else {
        stop("Invalid value for actual_vs_difference. Choose 'actual' or 'difference'.")
    }
}

# =============================================================================
# FUNCTION: icd_code_by_province
# Plot ICD code data faceted by province
# =============================================================================
icd_code_by_province <- function(dt, icd_code = NA, actual_vs_difference = c("actual", "difference")) {
    actual_vs_difference <- match.arg(actual_vs_difference)
    
    if (is.na(icd_code)) {
        stop("Please provide a valid icd_code, such as 'J09-J18', 'U07'")
    }

    if (actual_vs_difference == "actual") {
        return(
            dt[
                LGH_Cause == icd_code & epi_year %in% 2019:2022, 
                .(count = sum(count), baseline = sum(pred_simple_interactions)), 
                by = .(week_start, DeathProvince)
            ] %>%
            ggplot() +
            geom_line(aes(x = week_start, y = count, color = "Observed")) +
            geom_line(aes(x = week_start, y = baseline, color = "Expected")) +
            scale_color_manual(values = c("Expected" = "blue", "Observed" = "red")) +
            facet_wrap(~DeathProvince, scales = "free_y", nrow = 3) +
            theme_minimal()
        )
    } else if (actual_vs_difference == "difference") {
        return(
            dt[
                LGH_Cause == icd_code & epi_year %in% 2019:2022,
                {
                    csum <- sum(count, na.rm = TRUE)
                    bsum <- sum(pred_simple_interactions, na.rm = TRUE)
                    .(
                        count = csum,
                        baseline = bsum,
                        difference = csum - bsum,
                        excess_deficit = ifelse(csum - bsum > 0, "Excess", "Deficit")
                    )
                },
                by = .(week_start, DeathProvince)
            ] %>%
            ggplot() +
            geom_col(aes(x = week_start, y = difference, fill = excess_deficit),
                     stat = "identity", position = "dodge", width = 7) +
            theme_minimal() +
            scale_fill_manual(values = c("Excess" = "orange", "Deficit" = "green")) +
            geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dashed", color = "red") +
            facet_wrap(~DeathProvince, nrow = 3, scales = "free_y") +
            labs(x = "Week", y = "Excess Deaths", fill = "Excess vs Deficit")
        )
    } else {
        stop("Invalid value for actual_vs_difference. Choose 'actual' or 'difference'.")
    }
}

# =============================================================================
# FUNCTION: tabulate_excess_factor
# Create summary table of excess mortality by ICD code
# =============================================================================
tabulate_excess_factor <- function(dt, icd_codes = NA) {
    if (all(is.na(icd_codes))) {
        stop("Please provide valid icd_codes")
    }
    
    result <- dt[
        LGH_Cause %in% icd_codes & epi_year %in% 2019:2022,
        .(
            observed = sum(count, na.rm = TRUE),
            expected = sum(baseline, na.rm = TRUE)
        ),
        by = .(epi_year)
    ][, `:=`(
        difference = observed - expected,
        excess_pct = round((observed - expected) / expected * 100, 1)
    )]
    
    ft <- flextable(result) %>%
        set_header_labels(
            epi_year = "Year",
            observed = "Observed",
            expected = "Expected", 
            difference = "Difference",
            excess_pct = "Excess %"
        ) %>%
        colformat_num(j = c("observed", "expected", "difference"), big.mark = ",") %>%
        autofit()
    
    return(ft)
}

# =============================================================================
# FUNCTION: fmt_table
# Format a data frame as a flextable with optional title and subtitle headers
# =============================================================================
fmt_table <- function(x, title = NULL, subtitle = NULL) {
    ft <- flextable(x) %>%
        theme_zebra() %>%
        bold(part = "header")
    
    # Align first column left, rest right
    ft <- align(ft, align = "left", j = 1)
    if (ncol(x) > 1) {
        ft <- align(ft, align = "right", j = 2:ncol(x))
    }
    
    # Format Deaths column if present
    if ("Deaths" %in% names(x)) {
        ft <- colformat_int(ft, j = "Deaths", big.mark = ",")
    }
    
    # Format percentage columns
    pct_cols <- grep("Share|%", names(x), value = TRUE)
    if (length(pct_cols) > 0) {
        ft <- colformat_num(ft, j = pct_cols, digits = 1, suffix = "%")
    }
    
    ft <- autofit(ft)
    
    # Add title and subtitle as header lines
    if (!is.null(title)) {
        ft <- add_header_lines(ft, values = title)
    }
    if (!is.null(subtitle)) {
        ft <- add_header_lines(ft, values = subtitle)
    }
    
    return(ft)
}

# =============================================================================
# FUNCTION: make_sequence_tables
# Create tables for multiple cause code sequences
# 
# NOTE: The actual data uses columns CauseA, CauseB, CauseC, CauseD (not LGH_Cause_1, etc.)
#       Filter by LGH_CauseGroup (not LGH_Cause_1)
#       Uses sum(count) for weighted deaths (not .N for row count)
#       Missing values are coded as "888" (not NA or "")
# =============================================================================
make_sequence_tables <- function(
    dt,
    icd_code = NA,
    years = 2019:2022,
    top_n = 30
) {
    if (is.na(icd_code)) {
        stop("Please provide a valid icd_code")
    }
    
    # Ensure data.table
    if (!is.data.table(dt)) dt <- as.data.table(dt)
    
    # Filter data for the specified ICD code group and years
    dt_filtered <- dt[LGH_CauseGroup %in% icd_code & epi_year %in% years]
    
    if (nrow(dt_filtered) == 0) {
        stop(paste0("No data found for icd_code '", icd_code, "' in years ", 
                    paste(years, collapse = ", ")))
    }
    
    # Store total records for reporting
    nrow_total <- nrow(dt_filtered)
    years_label <- paste(unique(dt_filtered$epi_year), collapse = ", ")
    
    # Convert 888 to NA for cause columns (standard missing value code)
    cause_cols <- intersect(c("CauseA", "CauseB", "CauseC", "CauseD"), names(dt_filtered))
    dt_filtered[, (cause_cols) := lapply(.SD, function(x) fifelse(x == "888", NA_character_, x)), 
                .SDcols = cause_cols]
    
    # Determine cause level for each record
    dt_filtered[, cause_level := fifelse(!is.na(CauseD), "4",
                                  fifelse(!is.na(CauseC), "3",
                                    fifelse(!is.na(CauseB), "2",
                                      fifelse(!is.na(CauseA), "1", "0"))))]
    
    total_deaths <- dt_filtered[, sum(count, na.rm = TRUE)]
    
    # ---------- Level 1: Singlets (only Cause A) ----------
    singlets <- dt_filtered[cause_level == "1",
                            .(Deaths = sum(count, na.rm = TRUE)), 
                            by = .(CauseA)][order(-Deaths)]
    singlets[, `Share (%)` := round(100 * Deaths / sum(Deaths), 1)]
    
    n_unique_singlets <- nrow(singlets)
    total_singlet_deaths <- sum(singlets$Deaths, na.rm = TRUE)
    
    if (nrow(singlets) > top_n) singlets <- singlets[1:top_n]
    setnames(singlets, "CauseA", "Cause A")
    
    ft1 <- fmt_table(
        singlets, 
        title = "Single cause deaths (Level 1)",
        subtitle = sprintf(
            "In year(s) %s, of the %s LGH %s cause records, there were %d unique singlet Cause A codes, accounting for %s deaths (%.1f%% of all LGH deaths).",
            years_label,
            formatC(nrow_total, format = "d", big.mark = ","),
            icd_code,
            n_unique_singlets,
            formatC(total_singlet_deaths, format = "d", big.mark = ","),
            100 * total_singlet_deaths / total_deaths
        )
    )
    
    # ---------- Level 2: Doubles A → B (global) ----------
    doubles_global <- dt_filtered[cause_level == "2",
                                  .(Deaths = sum(count, na.rm = TRUE)), 
                                  by = .(CauseA, CauseB)][order(-Deaths)]
    doubles_global[, `Share (%)` := round(100 * Deaths / sum(Deaths), 1)]
    
    n_unique_doubles <- nrow(doubles_global)
    total_double_deaths <- sum(doubles_global$Deaths, na.rm = TRUE)
    
    if (nrow(doubles_global) > top_n) doubles_global <- doubles_global[1:top_n]
    setnames(doubles_global, c("CauseA", "CauseB"), c("Cause A", "Cause B"))
    
    ft2_global <- fmt_table(
        doubles_global, 
        title = "Two-cause sequences (A → B)",
        subtitle = sprintf(
            "In year(s) %s, of the %s LGH %s cause records, there were %d unique doublet Cause A → B sequences, accounting for %s deaths (%.1f%% of all LGH deaths).",
            years_label,
            formatC(nrow_total, format = "d", big.mark = ","),
            icd_code,
            n_unique_doubles,
            formatC(total_double_deaths, format = "d", big.mark = ","),
            100 * total_double_deaths / total_deaths
        )
    )
    
    # ---------- Level 2: Within A view (B causes within each A) ----------
    doubles_within <- dt_filtered[cause_level == "2",
                                  .(Deaths = sum(count, na.rm = TRUE)), 
                                  by = .(CauseA, CauseB)]
    doubles_within[, totalA := sum(Deaths), by = CauseA]
    doubles_within[, `Share within A (%)` := round(100 * Deaths / totalA, 1)]
    doubles_within <- doubles_within[order(CauseA, -Deaths)]
    
    # Keep top entries per A
    doubles_within[, rank_in_A := frank(-Deaths, ties.method = "first"), by = CauseA]
    doubles_within <- doubles_within[rank_in_A <= 5]  # Top 5 per A
    doubles_within[, c("totalA", "rank_in_A") := NULL]
    
    if (nrow(doubles_within) > top_n) doubles_within <- doubles_within[1:top_n]
    setnames(doubles_within, c("CauseA", "CauseB"), c("Cause A", "Cause B"))
    
    ft2_withinA <- fmt_table(
        doubles_within, 
        title = "Common sequences within each A (A → B)",
        subtitle = "Shows the most common secondary causes (B) for each primary cause (A), with share calculated within each A group."
    )
    
    # ---------- Level 3: Triplets A → B → C ----------
    triplets <- dt_filtered[cause_level == "3",
                            .(Deaths = sum(count, na.rm = TRUE)), 
                            by = .(CauseA, CauseB, CauseC)][order(-Deaths)]
    
    n_unique_triplets <- nrow(triplets)
    total_triplet_deaths <- sum(triplets$Deaths, na.rm = TRUE)
    
    ft3_global <- if (nrow(triplets) > 0) {
        triplets[, `Share (%)` := round(100 * Deaths / sum(Deaths), 1)]
        if (nrow(triplets) > top_n) triplets <- triplets[1:top_n]
        setnames(triplets, c("CauseA", "CauseB", "CauseC"), c("Cause A", "Cause B", "Cause C"))
        fmt_table(
            triplets, 
            title = "Three-cause sequences (A → B → C)",
            subtitle = sprintf(
                "In year(s) %s, of the %s LGH %s cause records, there were %d unique triplet Cause A → B → C sequences, accounting for %s deaths (%.1f%% of all LGH deaths).",
                years_label,
                formatC(nrow_total, format = "d", big.mark = ","),
                icd_code,
                n_unique_triplets,
                formatC(total_triplet_deaths, format = "d", big.mark = ","),
                100 * total_triplet_deaths / total_deaths
            )
        )
    } else {
        fmt_table(data.table(Message = "No triplet sequences found"), title = "Three-cause sequences")
    }
    
    # ---------- Level 4: Quadruplets A → B → C → D ----------
    if ("CauseD" %in% names(dt_filtered)) {
        quads <- dt_filtered[cause_level == "4",
                             .(Deaths = sum(count, na.rm = TRUE)), 
                             by = .(CauseA, CauseB, CauseC, CauseD)][order(-Deaths)]
        
        n_unique_quads <- nrow(quads)
        total_quad_deaths <- sum(quads$Deaths, na.rm = TRUE)
        
        ft4_global <- if (nrow(quads) > 0) {
            quads[, `Share (%)` := round(100 * Deaths / sum(Deaths), 1)]
            if (nrow(quads) > top_n) quads <- quads[1:top_n]
            setnames(quads, c("CauseA", "CauseB", "CauseC", "CauseD"), 
                     c("Cause A", "Cause B", "Cause C", "Cause D"))
            fmt_table(
                quads, 
                title = "Four-cause sequences (A → B → C → D)",
                subtitle = sprintf(
                    "In year(s) %s, of the %s LGH %s cause records, there were %d unique quadruplet Cause A → B → C → D sequences, accounting for %s deaths (%.1f%% of all LGH deaths).",
                    years_label,
                    formatC(nrow_total, format = "d", big.mark = ","),
                    icd_code,
                    n_unique_quads,
                    formatC(total_quad_deaths, format = "d", big.mark = ","),
                    100 * total_quad_deaths / total_deaths
                )
            )
        } else {
            fmt_table(data.table(Message = "No quadruplet sequences found"), title = "Four-cause sequences")
        }
    } else {
        ft4_global <- fmt_table(data.table(Message = "CauseD not available"), title = "Four-cause sequences")
    }
    
    return(list(
        ft1 = ft1,
        ft2_global = ft2_global,
        ft2_withinA = ft2_withinA,
        ft3_global = ft3_global,
        ft4_global = ft4_global
    ))
}
