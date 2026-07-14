# dashboard_helpers.r
# Clean helper functions for cause code analysis
# This file contains ONLY function definitions - no data loading
# Source this from any Quarto document, then load data separately

library(ggplot2)
library(data.table)
library(flextable)
library(officer)
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
# EXTERNAL CAUSES LOOKUP TABLE - For unnatural deaths groupings
# =============================================================================
get_external_causes_lookup <- function() {
  tibble::tribble(
    ~external_code,               ~description,
    "V01-V99",                   "Transport accidents (including road traffic accidents)",
    "W00-W19",                   "Falls",
    "W20-W49",                   "Exposure to inanimate mechanical forces",
    "W50-W64",                   "Exposure to animate mechanical forces", 
    "W65-W74",                   "Accidental drowning and submersion",
    "W75-W84",                   "Other accidental threats to breathing",
    "W85-W99",                   "Exposure to electric current, radiation, extreme temperatures",
    "X00-X09",                   "Exposure to smoke, fire and flames",
    "X10-X19",                   "Contact with heat and hot substances",
    "X20-X29",                   "Contact with venomous animals and plants",
    "X30-X39",                   "Exposure to forces of nature",
    "X40-X49",                   "Accidental poisoning by drugs, medicaments and biological substances",
    "X50-X59",                   "Overexertion, travel and privation",
    "X60-X84",                   "Intentional self-harm (suicide)",
    "X85-Y09",                   "Assault (including gunshots and homicide)",
    "Y10-Y34",                   "Event of undetermined intent",
    "Y35-Y36",                   "Legal intervention and operations of war",
    "Y40-Y84",                   "Complications of medical and surgical care",
    "Y85-Y89",                   "Sequelae of external causes",
    "Y90-Y98",                   "Supplementary factors related to causes of morbidity and mortality"
  )
}

# Create external_causes_lookup in global environment
if (!exists("external_causes_lookup")) {
  external_causes_lookup <- get_external_causes_lookup()
}

# Helper function to categorize ICD-10 external cause codes
categorize_external_cause <- function(icd_code) {
  # Handle vector inputs by processing each element
  if (length(icd_code) > 1) {
    return(sapply(icd_code, categorize_external_cause, USE.NAMES = FALSE))
  }
  
  # Handle single value
  if (is.na(icd_code) || icd_code == "") return(NA_character_)
  
  # Extract first character and numeric part
  first_char <- substr(icd_code, 1, 1)
  
  if (!first_char %in% c("V", "W", "X", "Y")) return(NA_character_)
  
  # Extract numeric part (handle cases like "V01", "X40.1", etc.)
  numeric_part <- as.numeric(gsub("([VWXY])([0-9]+).*", "\\2", icd_code))
  
  if (is.na(numeric_part)) return(NA_character_)
  
  # Categorize based on ranges
  if (first_char == "V") {
    if (numeric_part >= 1 & numeric_part <= 99) return("V01-V99")
  } else if (first_char == "W") {
    if (numeric_part >= 0 & numeric_part <= 19) return("W00-W19")
    if (numeric_part >= 20 & numeric_part <= 49) return("W20-W49")
    if (numeric_part >= 50 & numeric_part <= 64) return("W50-W64")
    if (numeric_part >= 65 & numeric_part <= 74) return("W65-W74")
    if (numeric_part >= 75 & numeric_part <= 84) return("W75-W84")
    if (numeric_part >= 85 & numeric_part <= 99) return("W85-W99")
  } else if (first_char == "X") {
    if (numeric_part >= 0 & numeric_part <= 9) return("X00-X09")
    if (numeric_part >= 10 & numeric_part <= 19) return("X10-X19")
    if (numeric_part >= 20 & numeric_part <= 29) return("X20-X29")
    if (numeric_part >= 30 & numeric_part <= 39) return("X30-X39")
    if (numeric_part >= 40 & numeric_part <= 49) return("X40-X49")
    if (numeric_part >= 50 & numeric_part <= 59) return("X50-X59")
    if (numeric_part >= 60 & numeric_part <= 84) return("X60-X84")
    if (numeric_part >= 85 & numeric_part <= 99) return("X85-Y09") # X85-X99 part of assault range
  } else if (first_char == "Y") {
    if (numeric_part >= 0 & numeric_part <= 9) return("X85-Y09") # Y00-Y09 part of assault range
    if (numeric_part >= 10 & numeric_part <= 34) return("Y10-Y34")
    if (numeric_part >= 35 & numeric_part <= 36) return("Y35-Y36")
    if (numeric_part >= 40 & numeric_part <= 84) return("Y40-Y84")
    if (numeric_part >= 85 & numeric_part <= 89) return("Y85-Y89")
    if (numeric_part >= 90 & numeric_part <= 98) return("Y90-Y98")
  }
  
  return(NA_character_)
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

# =============================================================================
# FUNCTION: tabulate_excess_grand_total_with_external
# Create summary table of excess mortality including both natural (LGH) and external causes
# Shows the "grand total" of excess deaths per year from each condition
# =============================================================================
tabulate_excess_grand_total_with_external <- function(dt_natural, dt_external = NULL, years = 2019:2022, include_external = TRUE) {
    
    # Ensure data.table
    if (!is.data.table(dt_natural)) dt_natural <- as.data.table(dt_natural)
    
    result_list <- list()
    
    # Process natural causes (LGH codes)
    icd_codes <- dt_natural[epi_year %in% years & !is.na(LGH_Cause), unique(LGH_Cause)]
    icd_codes <- icd_codes[icd_codes != ""]
    
    for (code in icd_codes) {
        excess_data <- dt_natural[
            LGH_Cause == code & epi_year %in% years,
            .(
                observed = round(sum(count, na.rm = TRUE)),
                expected = round(sum(baseline, na.rm = TRUE)),
                cause_code = code,
                cause_type = "Natural"
            ),
            by = .(epi_year)
        ][, difference := round(observed - expected)]
        
        result_list[[paste0("LGH_", code)]] <- excess_data
    }
    
    # Process external causes if available and requested
    if (!is.null(dt_external) && include_external) {
        if (!is.data.table(dt_external)) dt_external <- as.data.table(dt_external)
        
        # Add external cause categorization if UnderlyingCause column exists
        if ("UnderlyingCause" %in% names(dt_external)) {
            dt_external[, external_category := categorize_external_cause(UnderlyingCause)]
            
            # Filter for valid external causes
            external_codes <- dt_external[
                epi_year %in% years & !is.na(external_category), 
                unique(external_category)
            ]
            external_codes <- external_codes[!is.na(external_codes)]
            
            for (ext_code in external_codes) {
                # For external causes, we'll assume baseline = 0 (no model predictions for external causes)
                # This shows actual counts rather than excess
                excess_data <- dt_external[
                    external_category == ext_code & epi_year %in% years,
                    .(
                        observed = .N, # Count number of rows (deaths)
                        expected = 0, # No baseline model for external causes
                        cause_code = ext_code,
                        cause_type = "External"
                    ),
                    by = .(epi_year)
                ][, difference := observed] # difference = observed since baseline = 0
                
                result_list[[paste0("EXT_", ext_code)]] <- excess_data
            }
        }
    }
    
    # Combine all results
    result <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
    
    # Get descriptions for causes
    result[cause_type == "Natural", description := icd_lookup$description[match(cause_code, icd_lookup$LGH_Cause)]]
    result[cause_type == "External", description := external_causes_lookup$description[match(cause_code, external_causes_lookup$external_code)]]
    result[is.na(description), description := "Unknown"]
    
    # Create display labels
    result[, display_code := paste0(cause_type, ": ", cause_code)]
    
    # Pivot wider to show years as columns
    result_wide <- dcast(result, display_code + description ~ epi_year, 
                        value.var = "difference", fill = 0)
    
    # Calculate total excess across all years (rounded)
    year_cols <- as.character(years)
    available_years <- intersect(year_cols, names(result_wide))
    
    if (length(available_years) > 0) {
        result_wide[, Total := round(rowSums(.SD, na.rm = TRUE)), .SDcols = available_years]
    } else {
        result_wide[, Total := 0]
    }
    
    # Order by total excess deaths (descending)
    result_wide <- result_wide[order(-abs(Total))]
    
    # Add grand total row across all conditions
    if (nrow(result_wide) > 0) {
        grand_totals <- result_wide[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)), 
                                  .SDcols = c(available_years, "Total")]
        grand_totals[, display_code := "** GRAND TOTAL (All Causes) **"]
        grand_totals[, description := "Sum of excess/deficit across all natural causes"]
        
        # Reorder columns to match
        setcolorder(grand_totals, names(result_wide))
        
        # Bind the grand total row
        result_wide <- rbind(result_wide, grand_totals)
    }
    
    # Format as flextable
    ft <- flextable(result_wide) %>%
        set_header_labels(
            display_code = "Cause Category",
            description = "Description"
        ) %>%
        colformat_num(j = available_years, big.mark = ",", digits = 0, na_str = "0") %>%
        colformat_num(j = "Total", big.mark = ",", digits = 0) %>%
        autofit() %>%
        add_header_lines(values = paste0("Excess Deaths by Natural and External Causes (", 
                                        min(years), "-", max(years), ")")) %>%
        theme_zebra() %>%
        bold(part = "header")
    
    # Color scheme
    if (length(available_years) > 0) {
        for (year_col in available_years) {
            # Green for negative (deficit)
            negative_rows <- which(result_wide[[year_col]] < 0)
            if (length(negative_rows) > 0) {
                ft <- color(ft, i = negative_rows, j = year_col, color = "#228B22")
                ft <- bg(ft, i = negative_rows, j = year_col, bg = "#F0FFF0")
            }
            
            # Red for high positive
            high_positive_rows <- which(result_wide[[year_col]] >= 10000)
            if (length(high_positive_rows) > 0) {
                ft <- color(ft, i = high_positive_rows, j = year_col, color = "#8B0000")
                ft <- bg(ft, i = high_positive_rows, j = year_col, bg = "#FFE4E1")
            }
        }
    }
    
    # Color total column
    negative_total_rows <- which(result_wide[["Total"]] < 0)
    if (length(negative_total_rows) > 0) {
        ft <- color(ft, i = negative_total_rows, j = "Total", color = "#228B22")
        ft <- bg(ft, i = negative_total_rows, j = "Total", bg = "#F0FFF0")
    }
    
    high_total_rows <- which(result_wide[["Total"]] >= 50000)
    if (length(high_total_rows) > 0) {
        ft <- color(ft, i = high_total_rows, j = "Total", color = "#8B0000")
        ft <- bg(ft, i = high_total_rows, j = "Total", bg = "#FFE4E1")
    }
    
    # Format grand total row differently (bold and with border)
    grand_total_row <- which(grepl("GRAND TOTAL", result_wide$display_code))
    if (length(grand_total_row) > 0) {
        ft <- bold(ft, i = grand_total_row)
        ft <- hline(ft, i = grand_total_row - 1, border = fp_border(color = "black", width = 2))
        # Color grand total based on value
        grand_total_value <- result_wide[grand_total_row, "Total"][[1]]
        if (grand_total_value < 0) {
            ft <- color(ft, i = grand_total_row, color = "#228B22")
            ft <- bg(ft, i = grand_total_row, bg = "#F0FFF0")
        } else if (grand_total_value >= 50000) {
            ft <- color(ft, i = grand_total_row, color = "#8B0000") 
            ft <- bg(ft, i = grand_total_row, bg = "#FFE4E1")
        }
    }
    
    return(ft)
}

# =============================================================================
# FUNCTION: tabulate_external_causes_only
# Create summary table of external causes (unnatural deaths) only
# =============================================================================
tabulate_external_causes_only <- function(dt_external, years = 2019:2022) {
    if (is.null(dt_external)) {
        return(data.table(Message = "No external causes data available"))
    }
    
    if (!is.data.table(dt_external)) dt_external <- as.data.table(dt_external)
    
    result_list <- list()
    
    # Add external cause categorization if UnderlyingCause column exists
    if ("UnderlyingCause" %in% names(dt_external)) {
        dt_external[, external_category := categorize_external_cause(UnderlyingCause)]
        
        # Filter for valid external causes
        external_codes <- dt_external[
            epi_year %in% years & !is.na(external_category), 
            unique(external_category)
        ]
        external_codes <- external_codes[!is.na(external_codes)]
        
        for (ext_code in external_codes) {
            # Count actual deaths for external causes (no baseline model)
            excess_data <- dt_external[
                external_category == ext_code & epi_year %in% years,
                .(
                    observed = .N, # Count number of rows (deaths)
                    expected = 0, # No baseline model for external causes
                    cause_code = ext_code,
                    cause_type = "External"
                ),
                by = .(epi_year)
            ][, difference := observed] # difference = observed since baseline = 0
            
            result_list[[paste0("EXT_", ext_code)]] <- excess_data
        }
    }
    
    if (length(result_list) == 0) {
        return(data.table(Message = "No valid external causes found"))
    }
    
    # Combine all results
    result <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
    
    # Reshape to wide format for better display
    result_wide <- dcast(result, cause_code + cause_type ~ epi_year, 
                         value.var = "observed", fill = 0)
    
    # Add total column
    year_cols <- as.character(years)
    available_years <- intersect(year_cols, names(result_wide))
    if (length(available_years) > 0) {
        result_wide[, Total := rowSums(.SD), .SDcols = available_years]
    } else {
        result_wide[, Total := 0]
    }
    
    # Sort by total deaths (descending)
    setorder(result_wide, -Total)
    
    # Add descriptions and format column names
    if (exists("external_causes_lookup")) {
        result_wide[, Description := external_causes_lookup$description[match(cause_code, external_causes_lookup$external_code)]]
        result_wide[, display_code := paste0(cause_code, " - ", Description)]
    } else {
        result_wide[, display_code := cause_code]
        result_wide[, Description := cause_code]
    }
    
    # Reorder columns
    setcolorder(result_wide, c("display_code", "Description", "cause_type", available_years, "Total"))
    
    # Create flextable
    ft <- flextable(result_wide) %>%
        set_header_labels(
            display_code = "External Cause Category",
            Description = "Description",
            cause_type = "Type"
        ) %>%
        colformat_num(j = available_years, big.mark = ",", digits = 0, na_str = "0") %>%
        colformat_num(j = "Total", big.mark = ",", digits = 0) %>%
        autofit() %>%
        add_header_lines(values = paste0("Unnatural Deaths by External Cause Category (", 
                                        min(years), "-", max(years), ")")) %>%
        theme_zebra() %>%
        bold(part = "header")
    
    # Color coding for high death counts
    if ("Total" %in% colnames(result_wide)) {
        high_total_rows <- which(result_wide[["Total"]] >= 5000)
        if (length(high_total_rows) > 0) {
            ft <- color(ft, i = high_total_rows, j = "Total", color = "#8B0000")
            ft <- bg(ft, i = high_total_rows, j = "Total", bg = "#FFE4E1")
        }
        
        # Color individual year columns for high counts
        for (year_col in available_years) {
            high_year_rows <- which(result_wide[[year_col]] >= 1000)
            if (length(high_year_rows) > 0) {
                ft <- color(ft, i = high_year_rows, j = year_col, color = "#8B0000")
                ft <- bg(ft, i = high_year_rows, j = year_col, bg = "#FFE4E1")
            }
        }
    }
    
    return(ft)
}

# =============================================================================
# FUNCTION: tabulate_excess_grand_total
# Create summary table of excess mortality aggregated across all causes by year
# Shows the "grand total" of excess deaths per year from each condition
# =============================================================================
tabulate_excess_grand_total <- function(dt, years = 2019:2022) {
    
    # Ensure data.table
    if (!is.data.table(dt)) dt <- as.data.table(dt)
    
    # Get all available ICD codes
    icd_codes <- dt[epi_year %in% years & !is.na(LGH_Cause), unique(LGH_Cause)]
    icd_codes <- icd_codes[icd_codes != ""]
    
    # Calculate excess deaths for each cause and year
    result_list <- list()
    
    for (code in icd_codes) {
        excess_data <- dt[
            LGH_Cause == code & epi_year %in% years,
            .(
                observed = round(sum(count, na.rm = TRUE)),
                expected = round(sum(baseline, na.rm = TRUE)),
                LGH_Cause = code
            ),
            by = .(epi_year)
        ][, difference := round(observed - expected)]
        
        result_list[[code]] <- excess_data
    }
    
    # Combine all results
    result <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
    
    # Get descriptions for causes
    if (exists("icd_lookup")) {
        result[, description := icd_lookup$description[match(LGH_Cause, icd_lookup$LGH_Cause)]]
        result[, description := fifelse(is.na(description), "Unknown", description)]
    } else {
        result[, description := "Description unavailable"]
    }
    
    # Pivot wider to show years as columns
    result_wide <- dcast(result, LGH_Cause + description ~ epi_year, 
                        value.var = "difference", fill = 0)
    
    # Calculate total excess across all years (rounded)
    year_cols <- as.character(years)
    available_years <- intersect(year_cols, names(result_wide))
    
    if (length(available_years) > 0) {
        result_wide[, Total := round(rowSums(.SD, na.rm = TRUE)), .SDcols = available_years]
    } else {
        result_wide[, Total := 0]
    }
    
    # Order by total excess deaths (descending)
    result_wide <- result_wide[order(-Total)]
    
    # Format as flextable
    ft <- flextable(result_wide) %>%
        set_header_labels(
            LGH_Cause = "ICD Code",
            description = "Description"
        ) %>%
        colformat_num(j = available_years, big.mark = ",", digits = 0, na_str = "0") %>%
        colformat_num(j = "Total", big.mark = ",", digits = 0) %>%
        autofit() %>%
        add_header_lines(values = paste0("Excess Deaths by Cause and Year (", 
                                        min(years), "-", max(years), ")")) %>%
        theme_zebra() %>%
        bold(part = "header")
    
    # Improve color scheme for negative values (deficit) and positive values (excess)
    if (length(available_years) > 0) {
        for (year_col in available_years) {
            # Color negative values (deficit) in forest green
            negative_rows <- which(result_wide[[year_col]] < 0)
            if (length(negative_rows) > 0) {
                ft <- color(ft, 
                           i = negative_rows, 
                           j = year_col, 
                           color = "#228B22")
                ft <- bg(ft,
                        i = negative_rows,
                        j = year_col,
                        bg = "#F0FFF0")
            }
            
            # Color high positive values (large excess) in darker red
            high_positive_rows <- which(result_wide[[year_col]] >= 10000)
            if (length(high_positive_rows) > 0) {
                ft <- color(ft, 
                           i = high_positive_rows, 
                           j = year_col, 
                           color = "#8B0000")
                ft <- bg(ft,
                        i = high_positive_rows,
                        j = year_col,
                        bg = "#FFE4E1")
            }
        }
    }
    
    # Color total column with improved scheme
    negative_total_rows <- which(result_wide[["Total"]] < 0)
    if (length(negative_total_rows) > 0) {
        ft <- color(ft, 
                   i = negative_total_rows, 
                   j = "Total", 
                   color = "#228B22")
        ft <- bg(ft,
                i = negative_total_rows,
                j = "Total",
                bg = "#F0FFF0")
    }
    
    # Color high total excess in darker red
    high_total_rows <- which(result_wide[["Total"]] >= 50000)
    if (length(high_total_rows) > 0) {
        ft <- color(ft, 
                   i = high_total_rows, 
                   j = "Total", 
                   color = "#8B0000")
        ft <- bg(ft,
                i = high_total_rows,
                j = "Total",
                bg = "#FFE4E1")
    }
    
    return(ft)
}

# =============================================================================
# FUNCTION: plot_excess_grand_total
# Create a stacked bar plot showing excess deaths by cause and year
# =============================================================================
plot_excess_grand_total <- function(dt, years = 2019:2022, top_n = 15) {
    
    # Ensure data.table
    if (!is.data.table(dt)) dt <- as.data.table(dt)
    
    # Get all available ICD codes
    icd_codes <- dt[epi_year %in% years & !is.na(LGH_Cause), unique(LGH_Cause)]
    icd_codes <- icd_codes[icd_codes != ""]
    
    # Calculate excess deaths for each cause and year
    result_list <- list()
    
    for (code in icd_codes) {
        excess_data <- dt[
            LGH_Cause == code & epi_year %in% years,
            .(
                observed = sum(count, na.rm = TRUE),
                expected = sum(baseline, na.rm = TRUE),
                LGH_Cause = code
            ),
            by = .(epi_year)
        ][, difference := observed - expected]
        
        result_list[[code]] <- excess_data
    }
    
    # Combine all results
    result <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
    
    # Get descriptions for causes
    if (exists("icd_lookup")) {
        result[, description := icd_lookup$description[match(LGH_Cause, icd_lookup$LGH_Cause)]]
        result[, description := fifelse(is.na(description), "Unknown", description)]
        result[, cause_label := paste0(LGH_Cause, " (", substr(description, 1, 40), 
                                      ifelse(nchar(description) > 40, "...)", ")"))]
    } else {
        result[, cause_label := LGH_Cause]
    }
    
    # Get top causes by total excess
    top_causes <- result[, .(total_excess = sum(difference, na.rm = TRUE)), 
                        by = .(LGH_Cause, cause_label)
                        ][order(-abs(total_excess))][1:top_n]
    
    # Filter to top causes
    plot_data <- result[LGH_Cause %in% top_causes$LGH_Cause]
    plot_data[, cause_label := factor(cause_label, 
                                     levels = top_causes[order(total_excess)]$cause_label)]
    
    # Create stacked bar plot
    p <- ggplot(plot_data, aes(x = epi_year, y = difference, fill = cause_label)) +
        geom_col(position = "stack") +
        scale_fill_viridis_d(option = "plasma", name = "Cause of Death") +
        scale_y_continuous(labels = scales::comma, name = "Excess Deaths") +
        scale_x_continuous(breaks = years, name = "Year") +
        labs(title = "Excess Deaths by Cause and Year",
             subtitle = paste0("Top ", top_n, " causes by total excess mortality, ", 
                              min(years), "-", max(years))) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "right",
            legend.key.size = unit(0.3, "cm"),
            legend.text = element_text(size = 8),
            plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 10, color = "grey40"),
            panel.grid.minor = element_blank()
        ) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.3)
    
    return(p)
}
