# Practice 

library(NMCleaner)
library(arrow)
library(here)
library(data.table)
# load the dt_analysis data 

dt_analysis <- read_feather("../LGH_AnalysisData_2022.feather") %>% as.data.table()

dt_analysis$agegroup %>% unique()
# sett age group factors 
dt_analysis[, 
agegroup := factor( 
    agegroup , 
    levels = c(
        "0-4", 
        "5-39",
        "40-59",
        "60-69",
        "70-79",
        "80+" 
    )
)]-> dt_analysis

load("../LGH_ICD10_Cause_Lookup.rda") 
# show an ICD code by age group 
icd_lookup%>%print( n = 30)
names(dt_analysis)

icd_code <- "J09-J18"

# obsevred vs expected actual 

dt_analysis[
    LGH_Cause %in% icd_code, 
    .(
    count = sum(count), 
      baseline = sum(baseline)
    ), 
    by = .(agegroup, description, week_start, epi_week, epi_year)
][order(agegroup, -count)]%>%
    ggplot() + 
    geom_line(
        aes(
            x = week_start, 
            y = baseline, 
            color = "Expected"
        )
    ) + 
    geom_line( 
        aes(
            x = week_start, 
            y = count, 
            color = "Observed"
        )
    ) +
    scale_fill_manual(
        values = c("Expected" = "blue", "Observed" = "red")
    ) +
    facet_wrap(
        ~agegroup, 
        scales = "free_y", 
        nrow = 6
    ) + 
    theme_minimal()


# observed vs expected difference 
dt_analysis[
    LGH_Cause %in% icd_code & 
    epi_year %in% 2019:2022, # this will ensure it is only for modelled causes
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
    geom_col(
        aes(
            x = week_start,
            y = difference,
            fill = excess_deficit
        ),
        stat = "identity",
        position = "dodge", 
        width = 7
    ) +
    scale_fill_manual(
        values = c("Excess" = "orange", "Deficit" = "green")
    ) +
    geom_vline(
        xintercept = as.Date("2020-04-01"),
        linetype = "dashed",
        color = "red"
    ) +
    facet_wrap(
        ~agegroup,
        nrow = 6,
        scales = "free_y"
    ) +
    labs(x = "Cause of Death", y = "Excess Deaths", fill = "Excess vs Deficit") +
    theme_minimal()

# show observed only by province






# will have to show actual count data by province 



# Functions 
## General rules 
## Needs to take from an aggregated dataset
    ### Overall
    ### National by Code 
    ### Agegroup by Code 
    ### Province by Code (work in progress)
## Needs to take on an argument of a code
    ## May need to handle NA and U07 specially 
## Want to flip between showing:
### Actual and observed
### Difference with excess and deficit coloring

# plot code overview 




# plot age groups 


icd_code_by_agegroup <- function(dt, icd_code = NA , actual_vs_difference = c("actual", "difference")) {
    actual_vs_difference # will show the actual basline and observed or the difference with excess deficit coloring

    if (is.na(icd_code)) {
        stop("Please provide a valid icd_code, such as 'J09-J18', `U07")
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
        geom_line(
            aes(
                x = week_start,
                y = count,
                color = "Observed"
            )
        ) +
        geom_line(
            aes(
                x = week_start,
                y = baseline,
                color = "Expected"
            )
        ) +
        scale_color_manual(
            values = c("Expected" = "blue", "Observed" = "red")
        ) +
        facet_wrap(
            ~agegroup,
            scales = "free_y",
            nrow = 6
        ) +
        theme_minimal()
        )
    } else if (actual_vs_difference == "difference") {
        return(
    dt[
        LGH_Cause %in% icd_code &
        epi_year %in% 2019:2022, # this will ensure it is only for modelled causes
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
        geom_col(
            aes(
                x = week_start,
                y = difference,
                fill = excess_deficit
            ),
            stat = "identity",
            position = "dodge", 
            width = 7
        ) + 
        theme_minimal() +
        scale_fill_manual(
            values = c("Excess" = "orange", "Deficit" = "green")
        ) +
        geom_vline(
            xintercept = as.Date("2020-04-01"),
            linetype = "dashed",
            color = "red"
        ) +
        facet_wrap(
            ~agegroup,
            nrow = 6,
            scales = "free_y"
        ) +
        labs(x = "Cause of Death", y = "Excess Deaths", fill = "Excess vs Deficit") 
        )
    } else {
        stop("Invalid value for actual_vs_difference. Choose 'actual' or 'difference'.")
}
}

icd_code_by_agegroup(dt_analysis, icd_code = "J09-J18", actual_vs_difference = "difference")

# Create a function to plot an ICd code faceted by province with the dt_collapse_prov data

read_feather("../LGH_BaselinePredictions_ProvinceLevel_TempPop.feather")%>%as.data.table()  -> dt_collapse_prov

# raw example first 
dt_collapse_prov%>%names()
dt_collapse_prov[
    LGH_Cause == "J09-J18" & epi_year %in% 2019:2022, 
    .(count = sum(count), baseline = sum(pred_simple_interactions)
    ), 
    by = .(DeathProvince, week_start, epi_week, epi_year)
] %>%
ggplot() + 
    geom_line(
        aes(
            x = week_start, 
            y = count, 
            color = "Observed"
        )
    ) + 
    geom_line( 
        aes(
            x = week_start, 
            y = baseline, 
            color = "Expected"
        )
    ) +
    scale_color_manual(
        values = c("Expected" = "blue", "Observed" = "red")
    ) +
    facet_wrap(
        ~DeathProvince, 
        scales = "free_y", 
        nrow = 9
    ) + 
    theme_minimal()

icd_code_by_province <- function(dt, icd_code = NA , actual_vs_difference = c("actual", "difference")) {
    #actual_vs_difference # will show the actual basline and observed or the difference with excess deficit coloring

    if (is.na(icd_code)) {
        stop("Please provide a valid icd_code, such as 'J09-J18', `U07")
    }

    if (actual_vs_difference == "actual") {
        return(

    dt[LGH_Cause == icd_code &      epi_year %in% 2019:2022, 
    .(
        count = sum(count), 
        baseline = sum(pred_simple_interactions)
    ), by = .(week_start, DeathProvince)
    ] %>%
        ggplot() +
        geom_line(
            aes(
                x = week_start,
                y = count,
                color = "Observed"
            )
        ) +
        geom_line(
            aes(
                x = week_start,
                y = baseline,
                color = "Expected"
            )
        ) +
        scale_color_manual(
            values = c("Expected" = "blue", "Observed" = "red")
        ) +
        facet_wrap(
            ~DeathProvince,
            scales = "free_y",
            nrow = 10
        ) +
        theme_minimal()
        )
    } else if (actual_vs_difference == "difference") {
        return(
    dt[LGH_Cause == icd_code & epi_year %in% 2019:2022, 
    {
        csum <- sum(count, na.rm = TRUE)    
        bsum <- sum(pred_simple_interactions, na.rm = TRUE)
        .(
            count = csum,
            baseline = bsum,
            difference = csum - bsum,
            excess_deficit = ifelse(csum - bsum > 0, "Excess", "Deficit"),
            covid_period = ifelse(epi_year < 2020, "Pre-COVID", "COVID")
        )
    }, by = .(week_start, DeathProvince)
    ] %>%
        ggplot() +
        geom_col(
            aes(
                x = week_start,
                y = difference,
                fill = excess_deficit
            ),
            stat = "identity",
            position = "dodge", 
            width = 7
        ) + 
        theme_minimal() +
        scale_fill_manual(
            values = c("Excess" = "orange", "Deficit" = "green")
        ) +
        geom_vline(
            xintercept = as.Date("2020-04-01"),
            linetype = "dashed",
            color = "red"
        ) +
        facet_wrap(
            ~DeathProvince,
            nrow = 10,
            scales = "free_y"
        ) +
        labs(x = "Cause of Death", y = "Excess Deaths", fill = "Excess vs Deficit") 
        )
    } else {
        stop("Invalid value for actual_vs_difference. Choose 'actual' or 'difference'.")
    }

}

icd_code_by_province(dt_collapse_prov, icd_code = "J09-J18", actual_vs_difference = "difference")





# Create a flextable of the factor of excess

# Order conditions by high proportion of excess deaths to high proportion of deficit deaths
dt_analysis[
    !is.na(LGH_Cause) & epi_year %in% 2020:2022, # this will ensure it is only for modelled causes
    {
        csum <- sum(count, na.rm = TRUE)
        bsum <- sum(baseline, na.rm = TRUE)
        .(
            count = csum,
            baseline = bsum,
            difference = csum - bsum,
            excess_deficit = ifelse(csum - bsum > 0, "Excess", "Deficit")
            # covid_period = ifelse(epi_year < 2020, "Pre-COVID", "COVID")
        )
    },
    by = .(LGH_Cause, epi_year)
][
    ,
    `:=`(
        prop_diff = (count - baseline) / baseline * 100,
        factor_diff = count / baseline
    )
] -> dt_analysis_ordered_by_year

dt_analysis_ordered_by_year%>%print() 

 color_green_to_red <- function(n) {
    colors <- colorRampPalette(c("green", "white", "red"))(n)
    return(colors)
}



dt_analysis[
    !is.na(LGH_Cause) & epi_year %in% 2020:2022, # this will ensure it is only for modelled causes
    {
        csum <- sum(count, na.rm = TRUE)
        bsum <- sum(baseline, na.rm = TRUE)
        .(
            count = csum,
            baseline = bsum,
            difference = csum - bsum,
            excess_deficit = ifelse(csum - bsum > 0, "Excess", "Deficit")
            # covid_period = ifelse(epi_year < 2020, "Pre-COVID", "COVID")
        )
    },
    by = .(LGH_Cause)
][
    ,
    `:=`(
        prop_diff = (count - baseline) / baseline * 100,
        factor_diff = count / baseline,
        epi_year = "Overall (2020-2022)"
    )
] -> dt_analysis_ordered_overall



rbind(
    dt_analysis_ordered_by_year[, period := as.character(epi_year)],
    dt_analysis_ordered_overall[, period := "Overall (2020-2022)"]
) -> dt_analysis_ordered_final

dt_analysis_ordered_overall[order(-prop_diff)]$LGH_Cause -> LGH_descending_label

dt_analysis_ordered_final$LGH_Cause <- factor(
    dt_analysis_ordered_final$LGH_Cause,
    levels = LGH_descending_label
)

dt_analysis_ordered_final[order(prop_diff),"color_severity"] <- color_green_to_red( nrow( dt_analysis_ordered_final ) )


dt_analysis_ordered_final %>%
    ggplot(aes(x = period, y = LGH_Cause, fill = color_severity)) +
    geom_tile() +
    scale_fill_identity() +
    theme_minimal()

# Create a plotly version of this that is colored with a hover text showing the factor difference
#library(plotly)

# create a flextable version that is coloured, with similar ordering and only shows the ICD code 
library(flextable)
# ensure a single color per LGH_Cause, pivot factor_diff to wide, then re-attach color_severity
dt_colors <- unique(dt_analysis_ordered_final[, .(LGH_Cause, color_severity)])

dt_colors
dt_analysis_ordered_final

library(data.table)
library(flextable)
library(officer)
library(gtsummary)

# reshape so that epi_years are coumns and the rest are rows
dcast(dt_analysis_ordered_final[LGH_Cause %in% icd_code, ], LGH_Cause + count + baseline + difference ~ period, value.var = "factor_diff")

## 1) Make separate wide tables (values vs colours)
dt_wide_vals_factor_diff <- dcast(
    dt_analysis_ordered_final[
        LGH_Cause %in% icd_code,
        .(LGH_Cause, period, factor_diff, count, baseline, difference)
    ],
    LGH_Cause ~ period,
    value.var = c("factor_diff")
)[, stat := "Factor Difference"]

dt_wide_vals_factor_diff

dt_analysis_ordered_final$epi_year %>% unique()-> years_in_analysis

dt_wide_vals_baseline <- dcast(
    dt_analysis_ordered_final[
        LGH_Cause %in% icd_code,
        .(LGH_Cause, period, baseline)
    ],
    LGH_Cause ~ period,
    value.var = "baseline"
)[, stat := "Baseline"]

dt_wide_vals_count <- dcast(
    dt_analysis_ordered_final[
        LGH_Cause %in% icd_code,
        .(LGH_Cause, period, count)
    ],
    LGH_Cause ~ period,
    value.var = "count"
)[, stat := "Count"]

rbind(
    dt_wide_vals_factor_diff,
    dt_wide_vals_baseline,
    dt_wide_vals_count
)[, 
c("LGH_Cause", "stat") := .(
    factor(LGH_Cause, levels = LGH_descending_label), 
    factor(stat, levels = c("Count", "Baseline", "Factor Difference")))
    ] -> dt_wide_vals

    dt_wide_vals%>%print() 

    # round year columns for rows where stat is Baseline or Count
    year_cols <- intersect(as.character(years_in_analysis), names(dt_wide_vals))
    year_cols
    if (length(year_cols) > 0) {
        dt_wide_vals[stat %in% c("Baseline", "Count"), (year_cols) := lapply(.SD, function(x) as.numeric(x)), .SDcols = year_cols]
    }


dt_wide_vals


# ensure desired column order exists, robust to missing period columns
desired_periods <- c(as.character(years_in_analysis), "Overall (2020-2022)")
cols_order <- c("LGH_Cause", "stat", intersect(desired_periods, names(dt_wide_vals)))

# set column order in-place
setcolorder(dt_wide_vals, cols_order)

dt_wide_vals%>%print() 

dt_wide_cols <- dcast(
    dt_analysis_ordered_final[
        LGH_Cause %in% icd_code,
        .(LGH_Cause, period, color_severity)
    ],
    LGH_Cause ~ period,
    value.var = "color_severity"
)[,
stat := "Factor Difference"]

setcolorder(dt_wide_cols, cols_order)


## (Optional) If color_severity stores labels (e.g., "low","med","high"),
## map them to actual colours here:
# map_vec <- c(low = "#E8F5E9", med = "#FFF8E1", high = "#FFEBEE")
# for (cl in setdiff(names(dt_wide_cols), "LGH_Cause")) {
#   dt_wide_cols[[cl]] <- unname(map_vec[as.character(dt_wide_cols[[cl]])])
# }

## 2) Ensure identical column order (periods) between the two tables
stopifnot(identical(names(dt_wide_vals), names(dt_wide_cols))) # same names?
# If not, align:
# setcolorder(dt_wide_cols, names(dt_wide_vals))

## 3) Build flextable from numeric values
ft <- regulartable(as.data.frame(dt_wide_vals))
ft
## 4) Apply background colours cell-by-cell from dt_wide_cols
value_cols <- setdiff(names(dt_wide_vals), c("LGH_Cause", "stat"))
rows <- seq_len(nrow(dt_wide_vals))

for (col in value_cols) {
    # NA bg values are ignored by flextable (cells keep current bg)
    ft <- bg(ft, i = rows, j = col, bg = dt_wide_cols[[col]])
}


## 5) Formatting niceties
# Numeric formatting for the factor_diff columns (tweak digits as needed)
# Format each cell: whole numbers without decimals, others with 2 decimals
for (col in value_cols) {
    for (r in rows) {
        val <- dt_wide_vals[[col]][r]
        if (is.na(val)) next
        if (is.numeric(val) && !is.na(val) && val == floor(val)) {
            txt <- formatC(val, format = "f", digits = 0, big.mark = ",")
        } else {
            txt <- formatC(as.numeric(val), format = "f", digits = 2, big.mark = ",")
        }
        ft <- compose(ft, i = r, j = col, value = as_paragraph(as_chunk(txt)), part = "body")
    }
}

# Align numbers center/right as you prefer
ft <- align(ft, j = value_cols, align = "center", part = "body")
ft <- align(ft, j = "LGH_Cause", align = "left", part = "body")

# Header styling
ft <- bold(ft, part = "header")
ft <- align(ft, part = "header", align = "center")

# Borders and width
ft <- autofit(ft)
ft <- theme_box(ft)

ft

tabulate_excess_factor <- function(dt, icd_codes = NA) {
    if (is.na(icd_codes) || length(icd_codes) == 0) {
        stop("Please provide a valid vector of icd_codes, such as c('J09-J18', 'U07')")
    }

    # helper colour function (same as in script)
    color_green_to_red <- function(n) {
        colorRampPalette(c("green", "white", "red"))(n)
    }

    # 1) Build ordered summary by year and overall (2020-2022) from the provided dt
    dt_analysis_ordered_by_year <- dt[
        !is.na(LGH_Cause) & epi_year %in% 2020:2022,
        {
            csum <- sum(count, na.rm = TRUE)
            bsum <- sum(baseline, na.rm = TRUE)
            .(
                count = csum,
                baseline = bsum,
                difference = csum - bsum,
                excess_deficit = ifelse(csum - bsum > 0, "Excess", "Deficit")
            )
        },
        by = .(LGH_Cause, epi_year)
    ][
        ,
        `:=`(
            prop_diff = (count - baseline) / baseline * 100,
            factor_diff = count / baseline
        )
    ]

    dt_analysis_ordered_overall <- dt[
        !is.na(LGH_Cause) & epi_year %in% 2020:2022,
        {
            csum <- sum(count, na.rm = TRUE)
            bsum <- sum(baseline, na.rm = TRUE)
            .(
                count = csum,
                baseline = bsum,
                difference = csum - bsum,
                excess_deficit = ifelse(csum - bsum > 0, "Excess", "Deficit")
            )
        },
        by = .(LGH_Cause)
    ][
        ,
        `:=`(
            prop_diff = (count - baseline) / baseline * 100,
            factor_diff = count / baseline,
            epi_year = "Overall (2020-2022)"
        )
    ]

    # combine and set ordering
    dt_analysis_ordered_final <- rbind(
        dt_analysis_ordered_by_year[, period := as.character(epi_year)],
        dt_analysis_ordered_overall[, period := "Overall (2020-2022)"]
    )

    # determine ordering of LGH_Cause by overall prop_diff
    LGH_descending_label <- dt_analysis_ordered_overall[order(-prop_diff)]$LGH_Cause
    dt_analysis_ordered_final$LGH_Cause <- factor(
        dt_analysis_ordered_final$LGH_Cause,
        levels = LGH_descending_label
    )

    # assign color severity (one color per row in dt_analysis_ordered_final)
    dt_analysis_ordered_final[order(prop_diff), color_severity := color_green_to_red(nrow(dt_analysis_ordered_final))]

    # restrict to requested icd_codes (keep only those present)
    selected_codes <- intersect(as.character(icd_codes), unique(as.character(dt_analysis_ordered_final$LGH_Cause)))
    if (length(selected_codes) == 0) {
        stop("No requested icd_codes found in data.")
    }

    # prepare wide tables for values (Factor Difference, Baseline, Count)
    years_in_analysis <- sort(unique(dt_analysis_ordered_by_year$epi_year))
    desired_periods <- c(as.character(years_in_analysis), "Overall (2020-2022)")

    dt_wide_vals_factor_diff <- dcast(
        dt_analysis_ordered_final[LGH_Cause %in% selected_codes, .(LGH_Cause, period, factor_diff)],
        LGH_Cause ~ period,
        value.var = "factor_diff"
    )[, stat := "Factor Difference"]

    dt_wide_vals_baseline <- dcast(
        dt_analysis_ordered_final[LGH_Cause %in% selected_codes, .(LGH_Cause, period, baseline)],
        LGH_Cause ~ period,
        value.var = "baseline"
    )[, stat := "Baseline"]

    dt_wide_vals_count <- dcast(
        dt_analysis_ordered_final[LGH_Cause %in% selected_codes, .(LGH_Cause, period, count)],
        LGH_Cause ~ period,
        value.var = "count"
    )[, stat := "Count"]

    dt_wide_vals <- rbind(
        dt_wide_vals_factor_diff,
        dt_wide_vals_baseline,
        dt_wide_vals_count
    )[, `:=`(
        LGH_Cause = factor(LGH_Cause, levels = LGH_descending_label),
        stat = factor(stat, levels = c("Count", "Baseline", "Factor Difference"))
    )]

    # ensure desired column order exists, robust to missing period columns
    cols_order <- c("LGH_Cause", "stat", intersect(desired_periods, names(dt_wide_vals)))
    setcolorder(dt_wide_vals, cols_order)

    # prepare wide colour table (only for Factor Difference rows)
    dt_wide_cols_fd <- dcast(
        dt_analysis_ordered_final[LGH_Cause %in% selected_codes, .(LGH_Cause, period, color_severity)],
        LGH_Cause ~ period,
        value.var = "color_severity"
    )[, stat := "Factor Difference"]

    # Create NA color rows for Count and Baseline stats
    dt_wide_cols_count <- copy(dt_wide_cols_fd)
    dt_wide_cols_count[, stat := "Count"]
    for (pc in setdiff(names(dt_wide_cols_count), c("LGH_Cause", "stat"))) {
        dt_wide_cols_count[, (pc) := NA_character_]
    }
    
    dt_wide_cols_baseline <- copy(dt_wide_cols_fd)
    dt_wide_cols_baseline[, stat := "Baseline"]
    for (pc in setdiff(names(dt_wide_cols_baseline), c("LGH_Cause", "stat"))) {
        dt_wide_cols_baseline[, (pc) := NA_character_]
    }

    # Combine to match the structure of dt_wide_vals
    dt_wide_cols <- rbind(
        dt_wide_cols_fd,
        dt_wide_cols_baseline,
        dt_wide_cols_count
    )[, `:=`(
        LGH_Cause = factor(LGH_Cause, levels = LGH_descending_label),
        stat = factor(stat, levels = c("Count", "Baseline", "Factor Difference"))
    )]

    # align column order of colours to the values table (if needed)
    # add missing columns with NA if necessary to ensure identical names
    missing_cols <- setdiff(names(dt_wide_vals), names(dt_wide_cols))
    for (mc in missing_cols) {
        if (mc %in% c("LGH_Cause", "stat")) next
        dt_wide_cols[, (mc) := NA_character_]
    }
    # order columns
    cols_order_colors <- c("LGH_Cause", "stat", intersect(desired_periods, names(dt_wide_cols)))
    setcolorder(dt_wide_cols, cols_order_colors)
    
    # Sort both tables to ensure row alignment
    setorder(dt_wide_vals, LGH_Cause, stat)
    setorder(dt_wide_cols, LGH_Cause, stat)

    stopifnot(identical(names(dt_wide_vals), names(dt_wide_cols)))
    stopifnot(nrow(dt_wide_vals) == nrow(dt_wide_cols))

    # Build flextable from values
    ft <- regulartable(as.data.frame(dt_wide_vals))

    # Apply background colours cell-by-cell from dt_wide_cols (only Factor Difference rows get colors)
    value_cols <- setdiff(names(dt_wide_vals), c("LGH_Cause", "stat"))
    rows <- seq_len(nrow(dt_wide_vals))

    for (col in value_cols) {
        # NA bg values are ignored by flextable (so Count/Baseline rows stay uncolored)
        ft <- bg(ft, i = rows, j = col, bg = dt_wide_cols[[col]])
    }

    # Formatting niceties: format numeric display per stat
    for (col in value_cols) {
        for (r in rows) {
            val <- dt_wide_vals[[col]][r]
            if (is.na(val)) next
            # determine stat for this row
            row_stat <- as.character(dt_wide_vals$stat[r])
            if (row_stat %in% c("Baseline", "Count")) {
                # integer formatting
                if (is.numeric(val)) {
                    txt <- formatC(as.numeric(val), format = "f", digits = 0, big.mark = ",")
                } else {
                    txt <- as.character(val)
                }
            } else {
                # Factor Difference: two decimals
                txt <- formatC(as.numeric(val), format = "f", digits = 2, big.mark = ",")
            }
            ft <- compose(ft, i = r, j = col, value = as_paragraph(as_chunk(txt)), part = "body")
        }
    }

    # Align and style
    ft <- align(ft, j = value_cols, align = "center", part = "body")
    ft <- align(ft, j = "LGH_Cause", align = "left", part = "body")
    ft <- bold(ft, part = "header")
    ft <- align(ft, part = "header", align = "center")
    ft <- autofit(ft)
    ft <- theme_box(ft)

    return(ft)
}

tabulate_excess_factor(dt_analysis, icd_codes = c("J09-J18"))


# Tabulate cause codes 
#write_feather(dt, "LGH_MasterFile_preCollapsed2022.feather")

library(arrow)
library(magrittr)
library(data.table)
library(flextable)
read_feather("../LGH_MasterFile_preCollapsed2022.feather") %>% as.data.table() -> dt

# assign ICD codes to the dataset. 
txt_file <-     "../icd_code_map/icd10cm-Code Descriptions-2026/icd10cm-codes-2026.txt"
txt_file_2 <- "../icd_code_map/icd10cm-Code Descriptions-2026/icd10cm-order-2026.txt"

icd_codes <- fread(
    txt_file,
    header = FALSE,
    sep = "",
    #delim = "\t",
    #col.names = c("ICD_Code", "Description"),
    #colClasses = c("character", "character"),
    #na.strings = c("", "NA")
)

icd_codes[
    , `:=`(
        icd_code = trimws(stringr::str_split(V1, " ", n = 2, simplify = TRUE)[, 1]),
        icd_description = trimws(stringr::str_split(V1, " ", n = 2, simplify = TRUE)[, 2]),
        icd_label = paste0(
            trimws(stringr::str_split(V1, " ", n = 2, simplify = TRUE)[, 1]),
            " - ",
            trimws(stringr::str_split(V1, " ", n = 2, simplify = TRUE)[, 2])
        )
    )
][
    ,
    c("icd_code", "icd_description", "icd_label")
] -> icd_codes_map_1


icd_codes <- fread(
    txt_file_2,
    header = FALSE,
    fill = TRUE,                 # handle missing trailing columns
   # col.names = c("line_id", "code", "level", "short_desc", "long_desc"),
    colClasses = "character",    # prevent auto numeric conversion
    sep = NULL                   # tells fread to auto-detect fixed widths
    )


icd_codes

# remove the rownames() from the V1 column

# remove the first numeric value from the V1 column
# remove leading numeric value (and any surrounding whitespace) from V1
icd_codes[, V1 := trimws(sub("^\\s*\\d+\\s*", "", V1))]
icd_codes

icd_codes$V1%>%head() 


lines <- icd_codes$V1

# Regex: capture CODE, LEVEL, SHORT, LONG (2+ spaces between short & long)
rx <- "^([A-Z][0-9A-Z]{2,4})\\s+(\\d)\\s+(.*?)\\s{2,}(.*)$"

# Convert the boundary to tabs so fread can split easily
tabbed <- sub(rx, "\\1\t\\2\t\\3\t\\4", lines, perl = TRUE)

tabbed

# Read into a data.table
dt_icd_codes <- fread(
    text = tabbed,
    sep = "\t",
    header = FALSE,
    col.names = c("icd_code", "level", "icd_description", "long_desc"),
    colClasses = "character",
    fill = TRUE
)
dt_icd_codes

dt_icd_codes[
    ,   `:=`(
    icd_code = trimws(icd_code),
    icd_description = trimws(icd_description),
    icd_label = paste0(icd_code, " - ", icd_description)
    )
][, c( "icd_code", "icd_description", "icd_label" ) ]-> icd_codes_map_2
    
# exceptions are based on the missing codes in missing_description coded a bit later. 


library(tibble)
library(dplyr)

tibble::tribble(
    ~icd_code, ~icd_description,
    # MIS-C 
    "RA03" , "Multisystem inflammatory syndrome associated with COVID-19", # RA03- :contentReference[oaicite:30]{index=30}


    # circulatory
    "I50.0", "Congestive heart failure", # I500-  :contentReference[oaicite:0]{index=0}
    "I64", "Stroke, not specified as haemorrhage or infarction", # I64-   :contentReference[oaicite:1]{index=1}
    "I69.4", "Sequelae of stroke, not specified as haemorrhage or infarction", # I694- :contentReference[oaicite:2]{index=2}
    "I51.6", "Cardiomyopathy in diseases classified elsewhere", # I516-  :contentReference[oaicite:3]{index=3}
    "I46.1", "Sudden cardiac death, so described", # I461-  (WHO rubric text) :contentReference[oaicite:4]{index=4}

    # infectious
    "A09.9", "Gastroenteritis and colitis of unspecified origin", # A099-  :contentReference[oaicite:5]{index=5}
    "A16.2", "Respiratory tuberculosis, not confirmed bacteriologically or histologically: Tuberculosis of lung", # A162- :contentReference[oaicite:6]{index=6}
    "A16.9", "Respiratory tuberculosis, not confirmed bacteriologically or histologically, unspecified", # A169- :contentReference[oaicite:7]{index=7}
    "A16", "Respiratory tuberculosis, not confirmed bacteriologically or histologically", #  A16- :contentReference[oaicite:8]{index=8}
    "B24", "Unspecified human immunodeficiency virus [HIV] disease", # B24-   :contentReference[oaicite:9]{index=9}

    # endocrine / diabetes (unspecified type)
    "E14.2", "Unspecified diabetes mellitus with renal complications", # E142-  :contentReference[oaicite:10]{index=10}
    "E14.9", "Unspecified diabetes mellitus without complications", # E149-  :contentReference[oaicite:11]{index=11}
    "E14.1", "Unspecified diabetes mellitus with ketoacidosis", # E141-  :contentReference[oaicite:12]{index=12}
    "E14.5", "Unspecified diabetes mellitus with peripheral circulatory complications", # E145- :contentReference[oaicite:13]{index=13}
    "E14.0", "Unspecified diabetes mellitus with coma", # E140-  :contentReference[oaicite:14]{index=14}
    "E14", "Unspecified diabetes mellitus", #  E14-  :contentReference[oaicite:15]{index=15}

    # respiratory / influenza
    "J11.0", "Influenza, virus not identified, with pneumonia", # J110-  :contentReference[oaicite:16]{index=16}
    "J11.8", "Influenza, virus not identified, with other manifestations", # J118-  :contentReference[oaicite:17]{index=17}
    "J46", "Status asthmaticus", #  J46-  :contentReference[oaicite:18]{index=18}
    "J17.3", "Pneumonia in diseases classified elsewhere: in other viral diseases", # J173- (ICE) :contentReference[oaicite:19]{index=19}
    "J30.3", "Other allergic rhinitis", # J303-  (WHO tabular wording) :contentReference[oaicite:20]{index=20}

    # perinatal
    "P07", "Disorders related to short gestation and low birth weight, not elsewhere classified", # P07-  :contentReference[oaicite:21]{index=21}
    "P21.9", "Birth asphyxia, unspecified", # P219- :contentReference[oaicite:22]{index=22}

    # mental & behavioural
    "F17.9", "Mental and behavioural disorders due to use of tobacco, unspecified", # F179- :contentReference[oaicite:23]{index=23}
    "F05.9", "Delirium, unspecified", # F059- :contentReference[oaicite:24]{index=24}
    "F01.9", "Vascular dementia, unspecified", # F019- :contentReference[oaicite:25]{index=25}
    "F79.9", "Intellectual disability, unspecified", # F799- :contentReference[oaicite:26]{index=26}
    "F01.1", "Multi-infarct dementia", # F011- :contentReference[oaicite:27]{index=27}
    "F10.0", "Acute intoxication due to use of alcohol", # F100- :contentReference[oaicite:28]{index=28}
    "F10.4", "Withdrawal state due to use of alcohol", # F104- :contentReference[oaicite:29]{index=29}
    "F45.3", "Somatoform autonomic dysfunction", # F453- :contentReference[oaicite:30]{index=30}
    "F22.0", "Delusional disorder", # F220- :contentReference[oaicite:31]{index=31}
    "F10.7", "Residual and late-onset psychotic disorder due to alcohol", # F107- :contentReference[oaicite:32]{index=32}
    "F78.8", "Other intellectual disabilities", # F788- :contentReference[oaicite:33]{index=33}
    "F19.0", "Acute intoxication due to multiple drug use and use of other psychoactive substances", # F190- :contentReference[oaicite:34]{index=34}
    "F171", "Mental and behavioural disorders due to use of tobacco: harmful use", # F171- :contentReference[oaicite:35]{index=35}
    # neurology
    "G41.9", "Status epilepticus, unspecified", # G419- :contentReference[oaicite:35]{index=35}
    "G82.0", "Flaccid paraplegia", # G820- :contentReference[oaicite:36]{index=36}

    # symptoms / signs
    "R56.8", "Other and unspecified convulsions", # R568- :contentReference[oaicite:37]{index=37}
    "R07.4", "Chest pain, unspecified", # R074- :contentReference[oaicite:38]{index=38}
    "R02", "Gangrene, not elsewhere classified", # R02-  :contentReference[oaicite:39]{index=39}
    "R52.9", "Pain, unspecified", # R529- :contentReference[oaicite:40]{index=40}
    "R96.0", "Instantaneous death", # R960- :contentReference[oaicite:41]{index=41}
    "R95", "Sudden infant death syndrome", # R95-  :contentReference[oaicite:42]{index=42}
    "R95.9", "Sudden infant death syndrome (unspecified)", # R959- (used by some browsers) :contentReference[oaicite:43]{index=43}

    # external causes – obstruction/threats to breathing
    "W78", "Inhalation of gastric contents", # W78-  :contentReference[oaicite:44]{index=44}
    "W79", "Inhalation and ingestion of food causing obstruction of respiratory tract", # W79- :contentReference[oaicite:45]{index=45}
    "W80", "Inhalation and ingestion of other objects causing obstruction of respiratory tract", # W80- :contentReference[oaicite:46]{index=46}
    "W76", "Other accidental hanging and strangulation", # W76-  :contentReference[oaicite:47]{index=47}
    "W84", "Unspecified threat to breathing", # W84-  :contentReference[oaicite:48]{index=48}

    # exposure / environment / injury
    "X33", "Victim of lightning", # X33-  :contentReference[oaicite:49]{index=49}
    "X09", "Exposure to unspecified smoke, fire and flames", # X09-  :contentReference[oaicite:50]{index=50}
    "X59.9", "Exposure to unspecified factor, unspecified", # X599- :contentReference[oaicite:51]{index=51}
    "X59", "Exposure to unspecified factor", # X59-  :contentReference[oaicite:52]{index=52}
    "X49", "Accidental poisoning by and exposure to other and unspecified chemicals and noxious substances", # X49- :contentReference[oaicite:53]{index=53}

    # undetermined intent & sequelae (Y10–Y34, Y85–Y89)
    "Y19", "Poisoning by and exposure to other and unspecified chemicals and noxious substances, undetermined intent", # Y19- :contentReference[oaicite:54]{index=54}
    "Y87.2", "Sequelae of events of undetermined intent", # Y872- :contentReference[oaicite:55]{index=55}
    "Y86", "Sequelae of unspecified accident", # Y86-  :contentReference[oaicite:56]{index=56}

    # drug adverse effects (complications in Y40–Y59)
    "Y57.9", "Other and unspecified drugs and medicaments causing adverse effects in therapeutic use", # Y579- :contentReference[oaicite:57]{index=57}
    "Y44", "Anticoagulants causing adverse effects in therapeutic use", # Y44-  (category header) :contentReference[oaicite:58]{index=58}
    "Y44.2", "Other and unspecified anticoagulants causing adverse effects in therapeutic use", # Y442- :contentReference[oaicite:59]{index=59}
    "Y41.5", "Other and unspecified systemic anti-infectives and antiparasitics causing adverse effects", # Y415- :contentReference[oaicite:60]{index=60}
    "Y52.9", "Other and unspecified agents primarily affecting the cardiovascular system causing adverse effects in therapeutic use", # Y529- :contentReference[oaicite:61]{index=61}
    "Y53.8", "Other agents primarily affecting the gastrointestinal system and metabolism causing adverse effects in therapeutic use", # Y538- :contentReference[oaicite:62]{index=62}
    "Y53", "Agents primarily affecting the gastrointestinal system and metabolism causing adverse effects in therapeutic use", # Y53-  (category) :contentReference[oaicite:63]{index=63}
    "Y49.6", "Psychostimulants with abuse potential causing adverse effects in therapeutic use", # Y496- :contentReference[oaicite:64]{index=64}
    "Y48.4", "Other and unspecified general anaesthetics causing adverse effects in therapeutic use", # Y484- :contentReference[oaicite:65]{index=65}
    "Y43.3", "Immunosuppressants causing adverse effects in therapeutic use", # Y433- :contentReference[oaicite:66]{index=66}
    "Y55.6", "Other and unspecified agents primarily acting on smooth and skeletal muscles and the respiratory system causing adverse effects in therapeutic use", # Y556- :contentReference[oaicite:67]{index=67}
    "Y59.9", "Other and unspecified vaccines and biological substances causing adverse effects in therapeutic use", # Y599- :contentReference[oaicite:68]{index=68}

    # COVID / special U-codes (WHO emergency use)
    "U07.2", "COVID-19, virus not identified", # U072-  :contentReference[oaicite:69]{index=69}
    "U04.9", "Severe acute respiratory syndrome [SARS], unspecified", # U049-  :contentReference[oaicite:70]{index=70}
    "U02", "Emergency use of U codes (provisional assignment) – country-specific use", # U02-  (header use; varies) :contentReference[oaicite:71]{index=71}
    "U51", "Cognitive impairment",

    # digestive
    "K29.1", "Other acute gastritis", # K291-  :contentReference[oaicite:72]{index=72}
    "K57.9", "Diverticular disease of intestine, part unspecified, without perforation or abscess", # K579- :contentReference[oaicite:73]{index=73}
    "K91.9", "Postprocedural disorder of digestive system, unspecified", # K919-  :contentReference[oaicite:74]{index=74}
    "K91.4", "Postprocedural intestinal obstruction", # K914-  :contentReference[oaicite:75]{index=75}

    # kidney / musculoskeletal / skin / eye
    "N28", "Other and unspecified disorders of kidney and ureter", # N28-   :contentReference[oaicite:76]{index=76}
    "M13.9", "Arthritis, unspecified", # M139-  :contentReference[oaicite:77]{index=77}
    "M81.9", "Osteoporosis without current pathological fracture, unspecified", # M819- :contentReference[oaicite:78]{index=78}
    "M80.9", "Osteoporosis with current pathological fracture, unspecified", # M809-  :contentReference[oaicite:79]{index=79}
    "L98.4", "Chronic ulcer of skin, not elsewhere classified", # L984-  :contentReference[oaicite:80]{index=80}
    "H54.9", "Visual impairment, unspecified", # H549-  :contentReference[oaicite:81]{index=81}

    # neoplasms / haematology
    "C80.9", "Malignant neoplasm, unspecified", # C809-  :contentReference[oaicite:82]{index=82}
    "D61", "Aplastic anaemia", # D61-   :contentReference[oaicite:83]{index=83}
    "D47", "Other neoplasms of uncertain or unknown behaviour of lymphoid, haematopoietic and related tissue", # D47-  :contentReference[oaicite:84]{index=84}
    "D47.7", "Other specified neoplasms of uncertain behaviour of lymphoid, haematopoietic and related tissue",  # D477- :contentReference[oaicite:85]{index=85}

    "X590", "Exposure to unspecified factor causing injury", # X590- :contentReference[oaicite:86]{index=86}
    "A090", "Gastroenteritis and colitis of unspecified origin", # A090- :contentReference[oaicite:87]{index=87}
    "Y532", "?", 
    "R628", "Other lack of expected normal physiological development", 
     "Y450", "Analgesics, antipyretics and anti-inflammatory drugs (opioids and other)", 
     "W87","Exposure to electic current", 
     "Y11", "Poisoning by and exposure to antiepileptic, sedative-hypnotic, antiparkinsonism and psychotropic drugs, not elsewhere classified, undetermined intent",
         "Y14", "Poisoning by and exposure to other and unspecified drugs, medicaments and biological substances, undetermined intent.", 
    "Y16", "Poisoning by and exposure to organic solvents and halogenated hydrocarbons and their vapours, undetermined intent", 
     "Y17", "Poisoning by and exposure to unspecified gases and vapours, undetermined intent", 
     "Y18", "Poisoning by and exposure to pesticides, undetermined intent", 
     "A160", "Tuberclosis of lunhg, not confirmed", 
     "Y539", "Pertains to an unspecified agent primarily affecting the gastrointestinal system"



) %>%
    mutate(
        icd_code = gsub("\\.", "", icd_code), # remove any trailing periods
        icd_label = paste0(icd_code, " - Handled by manual exception ", icd_description))%>%
    as.data.table() -> icd_codes_map_exceptions
    icd_codes_map_exceptions



rbind( 
    icd_codes_map_1,
    icd_codes_map_2, 
    icd_codes_map_exceptions
)-> icd_codes_map


    icd_codes_map%>%print()

icd_codes_map


#icd_code <- "E10-E14" Also set in line 50ish 
dt%>%names()
dt_code <- dt[LGH_CauseGroup %in% icd_code, ]

nrow_LGH_code <- nrow( dt_code )

dt_code$UnderlyingCause

dt_code[, 
    .(count = sum(count)), 
    by = .(LGH_Cause, LGH_CauseGroup, CauseA,CauseB, CauseC, CauseD, weekstart, epi_week, epi_year)
]

c(dt$CauseA, dt$CauseB, dt$CauseC, dt$CauseD)%>%unique()%>%length()

# tag the completeness of causes 
# across CauseA: CauseD, make NA if == 888
dt_code[, c("CauseA", "CauseB", "CauseC", "CauseD") := lapply(.SD, function(x) fifelse(x == 888, NA_character_, x)), .SDcols = c("CauseA", "CauseB", "CauseC", "CauseD")]

dt_code$CauseA %>%
    is.na() %>%
    table()
dt_code$CauseB %>%
is.na() %>%
    table()

# assign the icd_label to each CauseA to CauseD in dt, keeping the same var names
dt_code[, c("CauseA", "CauseB", "CauseC", "CauseD") := .(
    icd_codes_map[match(CauseA, icd_code), icd_label]%>%if_else( is.na(.), paste0(CauseA, "- Unmapped description"), .),
    icd_codes_map[match(CauseB, icd_code), icd_label]%>%if_else( 
                                                                                                                    is.na(.), 
                                                                                                                        if_else( !is.na(CauseB), 
                                                                                                                            paste0(CauseB, "- Unmapped description"), NA_character_), .),
    icd_codes_map[match(CauseC, icd_code), icd_label]%>%if_else( 
        is.na(.), 
        if_else( !is.na(CauseC), paste0(CauseC, "- Unmapped description"), NA_character_), .),
    icd_codes_map[match(CauseD, icd_code), icd_label]%>%if_else( 
        is.na(.), 
        if_else( !is.na(CauseD), paste0(CauseD, "- Unmapped description"), NA_character_), .)
)]


missing_description <- 
data.table( 
    icd_code = c( dt_code$CauseA, dt_code$CauseB, dt_code$CauseC, dt_code$CauseD )
)[
    , .N, by = icd_code
][
    grepl("Unmapped description", icd_code)
]

missing_description[order(-N)]%>%as.data.frame() %>% print()


icd_labelling <- 
    function( 
        dt = dt, 
        icd_code_map = icd_codes_map # should have icd_code and icd_label columns
        ){

            dt[, c("CauseA", "CauseB", "CauseC", "CauseD") := .(
                icd_code_map[match(CauseA, icd_code), icd_label]%>%if_else( is.na(.), paste0(CauseA, "- Unmapped description"), .),
                icd_code_map[match(CauseB, icd_code), icd_label]%>%if_else( 
                    is.na(.), 
                    if_else( !is.na(CauseB), 
                             paste0(CauseB, "- Unmapped description"), NA_character_), .),
                icd_code_map[match(CauseC, icd_code), icd_label]%>%if_else( 
                    is.na(.), 
                    if_else( !is.na(CauseC), paste0(CauseC, "- Unmapped description"), NA_character_), .),
                icd_code_map[match(CauseD, icd_code), icd_label]%>%if_else( 
                    is.na(.), 
                    if_else( !is.na(CauseD), paste0(CauseD, "- Unmapped description"), NA_character_), .)
            )]

    return(dt)
    }



dt_code$CauseA%>%is.na()%>%table()
dt_code$CauseB %>%   is.na() %>%table()

dt_code[, cause_level := fifelse(!is.na(CauseD), "4",
                          fifelse(!is.na(CauseC), "3",
                            fifelse(!is.na(CauseB), "2",
                              fifelse(!is.na(CauseA), "1", "0")
                            )
                          )
                        )]

dt_code %>% print()

# find most common CauseA code for those tagged cause_level 1
dt_code[cause_level == "1", ]
dt_code[cause_level == "1", 
    .(count = sum(count)), 
    by = .(CauseA)][order(-count)] 

# show common sequences for the top CauseA code with cause_level 2
dt_code[ cause_level %in% c("2") , 
    .(count = sum(count)), 
    by = .(CauseA, CauseB)][order(-count)]

# show common sequences for the top CauseA code with cause_level 3
dt_code[ cause_level %in% c("3") , 
    .(count = sum(count)), 
    by = .(CauseA, CauseB, CauseC)][order(-count)]

dt_code[cause_level %in% c("4"),
    .(count = sum(count)),
    by = .(CauseA, CauseB, CauseC, CauseD)
][order(-count)]


#
# ---------- libs ----------
library(data.table)
library(flextable)

# ---------- helpers ----------
fmt_table <- function(x, title = NULL, subtitle = NULL) {
    ft <- flextable(x) |>
        theme_zebra() |>
        bold(part = "header")
    ft <- align(ft, align = "left", j = 1)
    ft <- align(ft, align = "right", j = setdiff(colnames(x), colnames(x)[1]))
    if ("Deaths" %in% names(x)) ft <- colformat_int(ft, j = "Deaths", big.mark = " ")
    if ("Share" %in% names(x)) ft <- colformat_num(ft, j = "Share", digits = 1, suffix = "%")
    if ("Share within A" %in% names(x)) ft <- colformat_num(ft, j = "Share within A", digits = 1, suffix = "%")
    ft <- autofit(ft)
    if (!is.null(title)) ft <- add_header_lines(ft, values = title)
    if (!is.null(subtitle)) ft <- add_header_lines(ft, values = subtitle)
    ft
}

# ---------- params ----------
top_pairs_n <- 30 # global top A→B pairs
top_triplets_n <- 30 # global top A→B→C triplets
top_quads_n <- 30 # global top A→B→C→D quads

topB_per_A <- 50 # OPTIONAL: within each A, show top B's (kept for comparison)

total_LGH_deaths <- dt_code[, sum(count)]

# ---------- level 1: top A (updated naming & labeling for ft) ----------
level1 <- dt_code[cause_level == "1",
    .(Deaths = sum(count)),
    by = .(CauseA)
][order(-Deaths)]

# compute shares
level1[, Share_within_level1 := round(100 * Deaths / sum(Deaths),2)]
level1[, Share_of_Total := round(100 * Deaths / total_LGH_deaths, 2)]

# create table and set clearer column names
tbl1 <- copy(level1)
setnames(tbl1,
    old = c("CauseA", "Deaths", "Share_within_level1", "Share_of_Total"),
    new = c("Cause A", "Deaths", "Share of single cause\n response (%)", "Share of Total (%)")
)

# improved title/subtitle with counts
# assemble summary numbers (use saved objects)
n_unique_singlets <- nrow(tbl1)                 # distinct Cause A codes in level1 table
nrow_LGH_code <- nrow_LGH_code                  # total rows in dt_code (as defined earlier)
nrow_singlets <- sum(level1$Deaths)             # sum of deaths from level1 (same as total_level1_deaths)
total_level1_deaths <- sum(tbl1$Deaths, na.rm = TRUE)
total_LGH_deaths <- total_LGH_deaths           # overall LGH deaths (from above)

# create ft1 with a subtitle that references the saved objects
ft1 <- fmt_table(tbl1,
    #title = sprintf("Top underlying Cause A (Level 1) among %s", icd_code),
    subtitle = sprintf(
        "Of the %s LGH %s cause records selected, there were %d unique singlet Cause A codes, accounting for %s deaths (%0.1f%% of all LGH deaths).",
        formatC(nrow_LGH_code, format = "d", big.mark = ","),
        icd_code,
        n_unique_singlets,
        formatC(total_level1_deaths, format = "d", big.mark = ","),
        100 * total_level1_deaths / total_LGH_deaths
    )
)

# format numeric columns to 2 decimal places

ft1

# Of the x number of LGH cause ... records, there were x unique singlet Cause A codes, accounting for y deaths (z% of all LGH deaths).


# ---------- level 2: GLOBAL top A→B ----------


level2_all <- dt_code[cause_level == "2",
    .(Deaths = sum(count)),
    by = .(CauseA, CauseB)
][order(-Deaths)]
level2_all
level2_all[, 
    Share_of_sequence_within_two_cause := round(Deaths / sum(Deaths) * 100, 2)][, 
    Share_sequence_AB := round(Deaths / total_LGH_deaths * 100, 2)]
    


setnames(level2_all, "Deaths", "Deaths") # in case of slip

level2_all$Deaths %>%sum() 

tbl2_global <- level2_all[
    1:min(top_pairs_n, .N)
    ][]

tbl2_global

setnames(
    tbl2_global, c("CauseA", "CauseB", "Deaths", "Share_of_sequence_within_two_cause", "Share_sequence_AB"),
    c("Cause A", "Cause B", "Deaths", "Share of specific sequence", "Share of Total LGH codes(%)")
)


n_unique_doubles <- nrow(level2_all) # distinct Cause A codes in level1 table
nrow_LGH_code <- nrow_LGH_code # total rows in dt_code (as defined earlier)
nrow_singlets <- sum(level2_all$Deaths) # sum of deaths from level1 (same as total_level1_deaths)
total_level2_deaths <- sum(level2_all$Deaths, na.rm = TRUE)
total_LGH_deaths <- total_LGH_deaths # overall LGH deaths (from above)
ft2_global <- fmt_table(
    tbl2_global,
    subtitle = sprintf(
        "In year(s) %s, of the %s LGH %s cause records selected, there were %d unique doublet Cause A \u2192 B sequences, accounting for %s deaths (%0.1f%% of all LGH deaths).",
        
        paste(unique(dt_code$epi_year), collapse = ", "),
        formatC(nrow_LGH_code, format = "d", big.mark = ","),
        icd_code,
        n_unique_doubles,
        formatC(total_level2_deaths, format = "d", big.mark = ","),
        100 * total_level2_deaths / total_LGH_deaths
    )
)
ft2_global
# ---------- OPTIONAL: within-A view for A→B (kept for analytical contrast) ----------
level2_withinA <- copy(level2_all)
level2_withinA[, totalA := sum(Deaths), by = CauseA]
level2_withinA[, `Share within A` := 100 * Deaths / totalA]
level2_withinA[, rB := frank(-Deaths, ties.method = "first"), by = CauseA]
tbl2_withinA <- level2_withinA[
   , # rB <= topB_per_A was previoulsy filtered, but this doesnt make the denominator consistenct across tables. 
    .(CauseA, CauseB, Deaths, `Share within A`)
][order(CauseA, -Deaths)]

tbl2_withinA

# Show number of deaths within each CauseA and order it by the total deaths for CauseA
tbl2_withinA[order(-Deaths)]


tbl2_withinA[, totalA := sum(Deaths), by = CauseA]
tbl2_withinA[, prop_totalA := Deaths / sum(Deaths)*100]
tbl2_withinA[, prop_totalLGH := Deaths / total_LGH_deaths * 100]
tbl2_withinA
tbl2_withinA[order(-totalA)]$CauseA %>% unique() -> ordered_CauseA


tbl2_withinA$CauseA <- factor(
    tbl2_withinA$CauseA,
    levels = ordered_CauseA
)

tbl2_withinA[order(CauseA, -Deaths)] -> tbl2_withinA
tbl2_withinA

# tag the rows in each cause by rank and size, keep by proportions so this is easy to turn into a function
# parameters (tweak as needed or expose as function args)
prop_keep <- 0.5         # keep top X proportion of B per A (e.g. 0.5 = top 50%)
min_share_within_A <- 20 # keep any B that contributes >= this % within its A
min_Causes_contribution <- 0.25 # keep observations where the two cases contribute more than x % of total in all causes. 
max_tbl_length <- 50 

tbl2_withinA[, rB := frank(-Deaths, ties.method = "first"), by = CauseA]
tbl2_withinA[, total_rows := .N, by = CauseA]

# ensure 'Share within A' exists and is numeric; if not, compute it
if (!("Share within A" %in% names(tbl2_withinA))) {
    tbl2_withinA[, totalA := sum(Deaths), by = CauseA]
    tbl2_withinA[, `Share within A` := 100 * Deaths / totalA]
    tbl2_withinA[, totalA := NULL]
}
tbl2_withinA
# keep rows that are within the top proportion per CauseA OR exceed the minimum share threshold
tbl2_withinA <- tbl2_withinA[
    rB <= ceiling(total_rows * prop_keep) | (`Share within A` >= min_share_within_A) | (prop_totalA >= min_Causes_contribution)
]

tbl2_withinA[prop_totalA >= min_Causes_contribution, ]-> tbl2_withinA

tbl2_withinA[1:max_tbl_length, ] -> tbl2_withinA

tbl2_withinA[, -c( "total_rows")]-> tbl2_withinA

setnames(
    tbl2_withinA, 
    c("CauseA", "CauseB", "Deaths", "Share within A", "prop_totalA", "prop_totalLGH"),
    c("Cause A", "Cause B", "Deaths", "Share within of Cause B within Cause A (%)", "Proportion of sequence among two-cause entries (%)", "Proportion of sequence among all LGH entries (%)")
)


ft2_withinA <- fmt_table(tbl2_withinA,
    title    = "Common sequences within each A (A \u2192 B)"
) |>
    merge_v(j = "Cause A") |>
    valign(j = "Cause A", valign = "top")

# format numeric columns: Deaths as integer, percentage/proportion columns to 1 decimal with % suffix
pct_cols <- grep("%", names(tbl2_withinA), value = TRUE)
if ("Deaths" %in% names(tbl2_withinA)) {
    ft2_withinA <- colformat_int(ft2_withinA, j = "Deaths", big.mark = ",")
}
if (length(pct_cols) > 0) {
    ft2_withinA <- colformat_num(ft2_withinA, j = pct_cols, digits = 1, suffix = "%")
}
ft2_withinA

# ---------- level 3: GLOBAL top A→B→C ----------
level3_all <- dt_code[cause_level == "3",
    .(Deaths = sum(count)),
    by = .(CauseA, CauseB, CauseC)
][order(-Deaths)]
level3_all[,
     Share_of_Total := round(100 * Deaths / sum(Deaths),2)][, 
     Share_within_AB := round(100 * Deaths / sum(Deaths),2), by = .(CauseA, CauseB)
    ]


level3_all$Deaths %>% sum()

tbl3_global <- level3_all[1:min(top_triplets_n, .N)][]



tbl3_global

setnames(
    tbl3_global, c("CauseA", "CauseB", "CauseC", "Deaths", "Share_of_Total", "Share_within_AB"),
    c("Cause A", "Cause B", "Cause C", "Deaths", "Share of Total LGH code (%)", "Share of Cause C within AB (%)")
)
total_triplets <- nrow(level3_all)
total_three_cause_deaths <- sum(level3_all$Deaths, na.rm = TRUE)

ft3_global <- fmt_table(tbl3_global,
    subtitle = sprintf(
        "Of the %s LGH %s cause records selected, there were %d unique triplet Cause A \u2192 B \u2192 C sequences, accounting for %s deaths (%0.1f%% of all LGH deaths).",
        formatC(nrow_LGH_code, format = "d", big.mark = ","),
        icd_code,
        total_triplets,
        formatC(total_three_cause_deaths, format = "d", big.mark = ","),
        100 * total_three_cause_deaths / total_LGH_deaths
    )
)

ft3_global

# ---------- level 4: GLOBAL top A→B→C→D ----------
level4_all <- dt_code[cause_level == "4",
    .(Deaths = sum(count)),
    by = .(CauseA, CauseB, CauseC, CauseD)
][order(-Deaths)]
level4_all[, Share := 100 * Deaths / sum(Deaths)]
tbl4_global <- level4_all[1:min(top_quads_n, .N)][]
setnames(
    tbl4_global, c("CauseA", "CauseB", "CauseC", "CauseD", "Deaths", "Share"),
    c("Cause A", "Cause B", "Cause C", "Cause D", "Deaths", "Share")
)
ft4_global <- fmt_table(tbl4_global,
    subtitle  = sprintf(
        "In year(s) %s, of the %s LGH %s cause records selected, there were %d unique quadruplet Cause A \u2192 B \u2192 C \u2192 D sequences, accounting for %s deaths (%0.1f%% of all LGH deaths).",
        paste(unique(dt_code$epi_year), collapse = ", "),
        formatC(nrow_LGH_code, format = "d", big.mark = ","),
        icd_code,
        nrow(level4_all),
        formatC(sum(level4_all$Deaths, na.rm = TRUE), format = "d", big.mark = ","),
        100 * sum(level4_all$Deaths, na.rm = TRUE) / total_LGH_deaths
    )
)
ft4_global
# ---------- render ----------
ft1
ft2_global
ft2_withinA # optional comparison: delete if you only want global
ft3_global
ft4_global

# ---------- simple row-highlighting helper (top-k per A in the within-A table) ----------
# Works directly on tbl2_withinA (the data behind ft2_withinA)
tbl2_withinA[, rB := frank(-Deaths, ties.method = "first"), by = `Cause A`]
hi_rows <- which(tbl2_withinA$rB <= 3)
ft2_withinA <- bg(ft2_withinA, i = hi_rows, bg = "#FFF3CD")
ft2_withinA


# Create a function to return each table for a specific ICD code


make_sequence_tables <- function(
  dt,
  year = NULL,
  icd_code,
  top_pairs_n    = 30,
  top_triplets_n = 30,
  top_quads_n    = 30,
  topB_per_A     = 50,
  prop_keep = 0.5,
  min_share_within_A = 20,
  min_Causes_contribution = 0.25,
  max_tbl_length = 50
) {

  # ---------- libs ----------
  require(data.table)
  require(flextable)

  # ---------- helpers ----------
  fmt_table <- function(x, title = NULL, subtitle = NULL) {
    ft <- flextable(x) |>
      theme_zebra() |>
      bold(part = "header")
    ft <- align(ft, align = "left",  j = 1)
    ft <- align(ft, align = "right", j = setdiff(colnames(x), colnames(x)[1]))
    if ("Deaths" %in% names(x))            ft <- colformat_int(ft, j = "Deaths", big.mark = " ")
    if ("Share" %in% names(x))             ft <- colformat_num(ft, j = "Share", digits = 1, suffix = "%")
    if ("Share within A" %in% names(x))    ft <- colformat_num(ft, j = "Share within A", digits = 1, suffix = "%")
    ft <- autofit(ft)
    if (!is.null(title))    ft <- add_header_lines(ft, values = title)
    if (!is.null(subtitle)) ft <- add_header_lines(ft, values = subtitle)
    ft
  }



  # ---------- data subset ----------
  if (!is.data.table(dt)) dt <- as.data.table(dt)
    if(!is.null(year)){

        dt <- dt[epi_year %in% year, ]
    }else( 
        dt <- dt
    )


  dt_code <- dt[LGH_CauseGroup %in% icd_code, ]
  nrow_LGH_code <- nrow(dt_code)

  # tag the completeness of causes; 888 -> NA
  dt_code[, c("CauseA","CauseB","CauseC","CauseD") :=
            lapply(.SD, function(x) fifelse(x == 888, NA_character_, x)),
            .SDcols = c("CauseA","CauseB","CauseC","CauseD")]

  # cause level tag
  dt_code[, cause_level := fifelse(!is.na(CauseD), "4",
                            fifelse(!is.na(CauseC), "3",
                              fifelse(!is.na(CauseB), "2",
                                fifelse(!is.na(CauseA), "1", "0"))))]

    # icd_label 
    icd_labelling(
        dt = dt_code,
        icd_code_map = icd_codes_map
    ) -> dt_code


  total_LGH_deaths <- dt_code[, sum(count, na.rm = TRUE)]

  # ---------- level 1: top A ----------
  level1 <- dt_code[cause_level == "1",
                    .(Deaths = sum(count)), by = .(CauseA)][order(-Deaths)]
  level1[, Share_within_level1 := round(100 * Deaths / sum(Deaths), 2)]
  level1[, Share_of_Total      := round(100 * Deaths / total_LGH_deaths, 2)]

  tbl1 <- copy(level1)
  setnames(tbl1,
           old = c("CauseA","Deaths","Share_within_level1","Share_of_Total"),
           new = c("Cause A","Deaths","Share of single cause\n response (%)","Share of Total (%)"))

  total_level1_deaths <- sum(tbl1$Deaths, na.rm = TRUE)
  n_unique_singlets   <- nrow(tbl1)

  ft1 <- fmt_table(
    tbl1,
    subtitle = sprintf(
      "In year(s) %s, of the %s LGH %s cause records selected, there were %d unique singlet Cause A codes, accounting for %s deaths (%0.1f%% of all LGH deaths).",
        paste(unique(dt_code$epi_year), collapse = ", "),
      formatC(nrow_LGH_code, format = "d", big.mark = ","),
      icd_code,
      n_unique_singlets,
      formatC(total_level1_deaths, format = "d", big.mark = ","),
      100 * total_level1_deaths / total_LGH_deaths
    )
  )

  # ---------- level 2: GLOBAL top A → B ----------
  level2_all <- dt_code[cause_level == "2",
                        .(Deaths = sum(count)), by = .(CauseA, CauseB)][order(-Deaths)]
  level2_all[, Share_of_double := round(Deaths / sum(Deaths) * 100, 2)]
  level2_all[, Share_of_Total := round(Deaths / total_LGH_deaths * 100, 2)]

  tbl2_global <- level2_all[1:min(top_pairs_n, .N)][]
  setnames(tbl2_global,
           c("CauseA","CauseB","Deaths","Share_of_double","Share_of_Total"),
           c("Cause A","Cause B","Deaths","Share of specific sequence","Share of Total LGH codes(%)"))

  n_unique_doubles     <- nrow(level2_all)
  total_level2_deaths  <- sum(level2_all$Deaths, na.rm = TRUE)

  ft2_global <- fmt_table(
    tbl2_global,
    subtitle = sprintf(
      "In year(s) %s, of the %s LGH %s cause records selected, there were %d unique doublet Cause A \u2192 B sequences, accounting for %s deaths (%0.1f%% of all LGH deaths).",
      paste(unique(dt_code$epi_year), collapse = ", "),
      formatC(nrow_LGH_code, format = "d", big.mark = ","),
      icd_code,
      n_unique_doubles,
      formatC(total_level2_deaths, format = "d", big.mark = ","),
      100 * total_level2_deaths / total_LGH_deaths
    )
  )

  # ---------- OPTIONAL within-A view: A → B ----------
  level2_withinA <- copy(level2_all)
  level2_withinA[, totalA := sum(Deaths), by = CauseA]
  level2_withinA[, `Share within A` := 100 * Deaths / totalA]
  level2_withinA[, rB := frank(-Deaths, ties.method = "first"), by = CauseA]

  tbl2_withinA <- level2_withinA[, .(CauseA, CauseB, Deaths, `Share within A`)][order(CauseA, -Deaths)]
  # order A by total deaths
  tbl2_withinA[, totalA := sum(Deaths), by = CauseA]
  tbl2_withinA[, prop_totalA  := Deaths / sum(Deaths) * 100]            # share among level-2 total
  tbl2_withinA[, prop_totalLGH := Deaths / total_LGH_deaths * 100]

  ordered_CauseA <- tbl2_withinA[order(-totalA)]$CauseA |> unique()
  tbl2_withinA[, CauseA := factor(CauseA, levels = ordered_CauseA)]
  setorder(tbl2_withinA, CauseA, -Deaths)

  # keep rows by rank/share rules
  tbl2_withinA[, rB := frank(-Deaths, ties.method = "first"), by = CauseA]
  tbl2_withinA[, total_rows := .N, by = CauseA]

  if (!("Share within A" %in% names(tbl2_withinA))) {
    tbl2_withinA[, totalA := sum(Deaths), by = CauseA]
    tbl2_withinA[, `Share within A` := 100 * Deaths / totalA]
    tbl2_withinA[, totalA := NULL]
  }

  tbl2_withinA <- tbl2_withinA[
    rB <= ceiling(total_rows * prop_keep) | (`Share within A` >= min_share_within_A) | (prop_totalA >= min_Causes_contribution)
  ]
  tbl2_withinA <- tbl2_withinA[prop_totalA >= min_Causes_contribution]
  if (nrow(tbl2_withinA) > max_tbl_length) tbl2_withinA <- tbl2_withinA[1:max_tbl_length]
  tbl2_withinA[, c("total_rows") := NULL]

  setnames(
    tbl2_withinA,
    c("CauseA","CauseB","Deaths","Share within A","prop_totalA","prop_totalLGH"),
    c("Cause A","Cause B","Deaths","Share within of Cause B within Cause A (%)",
      "Proportion of sequence among two-cause entries (%)",
      "Proportion of sequence among all LGH entries (%)")
  )

  ft2_withinA <- fmt_table(tbl2_withinA, title = "Common sequences within each A (A \u2192 B)") |>
    merge_v(j = "Cause A") |>
    valign(j = "Cause A", valign = "top")

  # format numeric cols with % in name
  pct_cols <- grep("%", names(tbl2_withinA), value = TRUE)
  if ("Deaths" %in% names(tbl2_withinA)) {
    ft2_withinA <- colformat_int(ft2_withinA, j = "Deaths", big.mark = ",")
  }
  if (length(pct_cols) > 0) {
    ft2_withinA <- colformat_num(ft2_withinA, j = pct_cols, digits = 1, suffix = "%")
  }
  # highlight top-3 per A
  tmp_rank <- copy(tbl2_withinA)
  tmp_rank[, `Cause A` := as.character(`Cause A`)]
  tmp_rank[, rB := frank(-Deaths, ties.method = "first"), by = `Cause A`]
  hi_rows <- which(tmp_rank$rB <= 3)
  if (length(hi_rows)) ft2_withinA <- bg(ft2_withinA, i = hi_rows, bg = "#FFF3CD")

  # ---------- level 3: GLOBAL top A → B → C ----------
  level3_all <- dt_code[cause_level == "3",
                        .(Deaths = sum(count)), by = .(CauseA, CauseB, CauseC)][order(-Deaths)]

  level3_all[,  Share_of_Total := round(100 * Deaths / sum(Deaths), 2)]
  level3_all[,  Share_of_triplet := round(100 * Deaths / sum(Deaths), 2), by = .(CauseA, CauseB)]

  tbl3_global <- level3_all[1:min(top_triplets_n, .N)][]
  setnames(tbl3_global,
           c("CauseA","CauseB","CauseC","Deaths","Share_of_triplet", "Share_of_Total"),
           c("Cause A","Cause B","Cause C","Deaths","Share of Triplet (%)", "Share of Total LGH code (%)"))

  total_triplets          <- nrow(level3_all)
  total_three_cause_deaths <- sum(level3_all$Deaths, na.rm = TRUE)

  ft3_global <- fmt_table(
    tbl3_global,
    subtitle = sprintf(
      "In year(s) %s, of the %s LGH %s cause records selected, there were %d unique triplet Cause A \u2192 B \u2192 C sequences, accounting for %s deaths (%0.1f%% of all LGH deaths).",
              paste(unique(dt_code$epi_year), collapse = ", "),
      formatC(nrow_LGH_code, format = "d", big.mark = ","),
      icd_code,
      total_triplets,
      formatC(total_three_cause_deaths, format = "d", big.mark = ","),
      100 * total_three_cause_deaths / total_LGH_deaths
    )
  )

  # ---------- level 4: GLOBAL top A → B → C → D ----------
  level4_all <- dt_code[cause_level == "4",
                        .(Deaths = sum(count)), by = .(CauseA, CauseB, CauseC, CauseD)][order(-Deaths)]
  level4_all[, 
    Share_of_quadruplets := round(100 * Deaths / sum(Deaths), 2)][, 
    Share_of_Total := round(100 * Deaths / sum(Deaths), 2)
    ]

  tbl4_global <- level4_all[1:min(top_quads_n, .N)][]
  setnames(tbl4_global,
           c("CauseA","CauseB","CauseC","CauseD","Deaths","Share_of_quadruplets", "Share_of_Total"),
           c("Cause A","Cause B","Cause C","Cause D","Deaths","Share of Quadruplets (%)", "Share of Total LGH Code(%)"))

  ft4_global <- fmt_table(
    tbl4_global,
    subtitle = sprintf(
      "In year(s) %s, of the %s LGH %s cause records selected, there were %d unique quadruplet Cause A \u2192 B \u2192 C \u2192 D sequences, accounting for %s deaths (%0.1f%% of all LGH deaths).",
      paste(unique(dt_code$epi_year), collapse = ", "),
      formatC(nrow_LGH_code, format = "d", big.mark = ","),
      icd_code,
      nrow(level4_all),
      formatC(sum(level4_all$Deaths, na.rm = TRUE), format = "d", big.mark = ","),
      100 * sum(level4_all$Deaths, na.rm = TRUE) / total_LGH_deaths
    )
  )

  # ---------- return the five flextables ----------
  list(
    ft1          = ft1,
    ft2_global   = ft2_global,
    ft2_withinA  = ft2_withinA,
    ft3_global   = ft3_global,
    ft4_global   = ft4_global
  )
}

tabs <- make_sequence_tables(dt, icd_code = "U07")
tabs$ft1
tabs$ft2_global
tabs$ft2_withinA
tabs$ft3_global
tabs$ft4_global


# To compare years 
make_sequence_tables_compare <- function(
  dt,
  icd_code,
  year_left,
  year_right,
  top_pairs_n    = 30,
  top_triplets_n = 30,
  top_quads_n    = 30,
  # within-A view tuning
  prop_keep = 0.5,
  min_share_within_A = 20,
  min_Causes_contribution = 0.25,
  max_tbl_length = 50
) {
  require(data.table)
  require(flextable)

  if (!is.data.table(dt)) dt <- as.data.table(dt)

  # ---------- helpers ----------
  fmt_table <- function(x, title = NULL, subtitle = NULL) {
    ft <- flextable(x) |> theme_zebra() |> bold(part = "header")
    ft <- align(ft, align = "left",  j = 1)
    ft <- align(ft, align = "right", j = setdiff(colnames(x), colnames(x)[1]))
    num_cols <- names(x)[vapply(x, is.numeric, TRUE)]
    if (any(grepl("^Deaths", names(x)))) {
      ft <- colformat_int(ft, j = grep("^Deaths", names(x), value = TRUE), big.mark = " ")
    }
    pct_cols <- grep("Share|Proportion|%|within A", names(x), value = TRUE)
    if (length(pct_cols)) ft <- colformat_num(ft, j = pct_cols, digits = 1, suffix = "%")
    ft <- autofit(ft)
    if (!is.null(title))    ft <- add_header_lines(ft, values = title)
    if (!is.null(subtitle)) ft <- add_header_lines(ft, values = subtitle)
    ft
  }

  tag_levels <- function(dx) {
    dx[, c("CauseA","CauseB","CauseC","CauseD") :=
         lapply(.SD, function(x) fifelse(x == 888, NA_character_, x)),
         .SDcols = c("CauseA","CauseB","CauseC","CauseD")]
    dx[, cause_level := fifelse(!is.na(CauseD), "4",
                         fifelse(!is.na(CauseC), "3",
                           fifelse(!is.na(CauseB), "2",
                             fifelse(!is.na(CauseA), "1", "0"))))]
    dx
  }

  total_deaths_by_year <- function(dx) dx[, sum(count, na.rm = TRUE)]

  # ---------- slice data ----------
  base <- dt[LGH_CauseGroup %in% icd_code]
  yL   <- tag_levels(base[epi_year == year_left])
  yR   <- tag_levels(base[epi_year == year_right])

  nrow_L <- nrow(yL); nrow_R <- nrow(yR)
  tot_L  <- total_deaths_by_year(yL)
  tot_R  <- total_deaths_by_year(yR)



  # ========== LEVEL 1 ==========
  lev1 <- function(dx, tot) {
    out <- dx[cause_level == "1", .(Deaths = sum(count)), by = .(CauseA)][order(-Deaths)]
    out[, `Share of single cause\n response (%)` := 100 * Deaths / sum(Deaths)]
    out[, `Share of Total (%)` := 100 * Deaths / tot]
    out
  }
  l1 <- lev1(yL, tot_L); r1 <- lev1(yR, tot_R)

  tbl1 <- merge(l1, r1, by = "CauseA", all = TRUE,
                suffixes = c(paste0("_", year_left), paste0("_", year_right)))
  for (j in names(tbl1)) if (is.numeric(tbl1[[j]]) && anyNA(tbl1[[j]])) set(tbl1, which(is.na(tbl1[[j]])), j, 0)
  setnames(tbl1, "CauseA", "Cause A")
  tbl1[, `Δ Deaths` := get(paste0("Deaths_", year_right)) - get(paste0("Deaths_", year_left))]
  tbl1[, `Δ Share of Total (pp)` := get(paste0("Share of Total (%)_", year_right)) - get(paste0("Share of Total (%)_", year_left))]
  left_deaths_col <- paste0("Deaths_", year_left)
  if (!(left_deaths_col %in% names(tbl1))) left_deaths_col <- paste0("Deaths_", year_left) # safeguard
  setorderv(tbl1, cols = left_deaths_col, order = -1L, na.last = TRUE)

  ft1_comp <- fmt_table(
    tbl1,
    subtitle = sprintf(
      "%s: %s rows, %s deaths | %s: %s rows, %s deaths",
      year_left,  formatC(nrow_L, format = "d", big.mark = ","),
      formatC(tot_L,  format = "d", big.mark = ","),
      year_right, formatC(nrow_R, format = "d", big.mark = ","),
      formatC(tot_R,  format = "d", big.mark = ",")
    )
  )

  # ========== LEVEL 2 GLOBAL (A→B) ==========
  lev2_global <- function(dx, tot) {
    out <- dx[cause_level == "2", .(Deaths = sum(count)), by = .(CauseA, CauseB)][order(-Deaths)]
    out[, `Share of specific sequence` := 100 * Deaths / sum(Deaths)]
    out[, `Share of Total LGH codes(%)` := 100 * Deaths / tot]
    out
  }
  l2 <- lev2_global(yL, tot_L); r2 <- lev2_global(yR, tot_R)

  l2_top <- l2[1:min(top_pairs_n, .N)]
  r2_top <- r2[1:min(top_pairs_n, .N)]
  tbl2 <- merge(l2_top, r2_top, by = c("CauseA","CauseB"), all = TRUE,
                suffixes = c(paste0("_", year_left), paste0("_", year_right)))
  for (j in names(tbl2)) if (is.numeric(tbl2[[j]]) && anyNA(tbl2[[j]])) set(tbl2, which(is.na(tbl2[[j]])), j, 0)
  setnames(tbl2, c("CauseA","CauseB"), c("Cause A","Cause B"))
  tbl2[, `Δ Deaths` := get(paste0("Deaths_", year_right)) - get(paste0("Deaths_", year_left))]
  tbl2[, `Δ Share (pp)` := get(paste0("Share of Total LGH codes(%)_", year_right)) - get(paste0("Share of Total LGH codes(%)_", year_left))]
  left_deaths_col <- paste0("Deaths_", year_left)
  if (!(left_deaths_col %in% names(tbl2))) left_deaths_col <- paste0("Deaths_", year_left)
  setorderv(tbl2, cols = left_deaths_col, order = -1L, na.last = TRUE)

  ft2_global_comp <- fmt_table(tbl2)

  # ========== LEVEL 2 WITHIN-A ==========
  lev2_withinA <- function(dx) {
    out <- dx[cause_level == "2", .(Deaths = sum(count)), by = .(CauseA, CauseB)]
    out[, totalA := sum(Deaths), by = CauseA]
    out[, `Share within A` := 100 * Deaths / totalA]
    out[, rB := frank(-Deaths, ties.method = "first"), by = CauseA]
    out[, total_rows := .N, by = CauseA]
    out <- out[rB <= ceiling(total_rows * prop_keep) | (`Share within A` >= min_share_within_A)]
    out[, c("total_rows") := NULL]
    setorder(out, CauseA, -Deaths)
    out
  }
  l2w <- lev2_withinA(yL); r2w <- lev2_withinA(yR)

  tbl2w <- merge(l2w, r2w, by = c("CauseA","CauseB"), all = TRUE,
                 suffixes = c(paste0("_", year_left), paste0("_", year_right)))
  for (j in names(tbl2w)) if (is.numeric(tbl2w[[j]]) && anyNA(tbl2w[[j]])) set(tbl2w, which(is.na(tbl2w[[j]])), j, 0)
  setnames(tbl2w, c("CauseA","CauseB"), c("Cause A","Cause B"))

  # Limit if huge by left-year Deaths
  left_deaths_col <- paste0("Deaths_", year_left)
  if (!(left_deaths_col %in% names(tbl2w))) left_deaths_col <- paste0("Deaths_", year_left)
  if (nrow(tbl2w) > max_tbl_length) {
    setorderv(tbl2w, cols = left_deaths_col, order = -1L, na.last = TRUE)
    tbl2w <- tbl2w[1:max_tbl_length]
  }
  ft2_withinA_comp <- fmt_table(tbl2w,
                                title = sprintf("A \u2192 B within-A shares (%s vs %s)", year_left, year_right)) |>
    merge_v(j = "Cause A") |>
    valign(j = "Cause A", valign = "top")

  # ========== LEVEL 3 GLOBAL (A→B→C) ==========
  lev3_global <- function(dx) {
    out <- dx[cause_level == "3",
              .(Deaths = sum(count)), by = .(CauseA, CauseB, CauseC)][order(-Deaths)]
    out[, `Share of Total LGH code (%)` := 100 * Deaths / sum(Deaths)]
    out[, `Share of Cause C within AB (%)` := 100 * Deaths / sum(Deaths), by = .(CauseA, CauseB)]
    out
  }
  l3 <- lev3_global(yL); r3 <- lev3_global(yR)

  l3_top <- l3[1:min(top_triplets_n, .N)]
  r3_top <- r3[1:min(top_triplets_n, .N)]
  tbl3 <- merge(l3_top, r3_top, by = c("CauseA","CauseB","CauseC"), all = TRUE,
                suffixes = c(paste0("_", year_left), paste0("_", year_right)))
  for (j in names(tbl3)) if (is.numeric(tbl3[[j]]) && anyNA(tbl3[[j]])) set(tbl3, which(is.na(tbl3[[j]])), j, 0)
  setnames(tbl3, c("CauseA","CauseB","CauseC"), c("Cause A","Cause B","Cause C"))
  tbl3[, `Δ Deaths` := get(paste0("Deaths_", year_right)) - get(paste0("Deaths_", year_left))]
  left_deaths_col <- paste0("Deaths_", year_left)
  if (!(left_deaths_col %in% names(tbl3))) left_deaths_col <- paste0("Deaths_", year_left)
  setorderv(tbl3, cols = left_deaths_col, order = -1L, na.last = TRUE)

  ft3_global_comp <- fmt_table(tbl3)

  # ========== LEVEL 4 GLOBAL (A→B→C→D) ==========
  lev4_global <- function(dx) {
    out <- dx[cause_level == "4",
              .(Deaths = sum(count)), by = .(CauseA, CauseB, CauseC, CauseD)][order(-Deaths)]
    out[, Share := 100 * Deaths / sum(Deaths)]
    out
  }
  l4 <- lev4_global(yL); r4 <- lev4_global(yR)

  l4_top <- l4[1:min(top_quads_n, .N)]
  r4_top <- r4[1:min(top_quads_n, .N)]
  tbl4 <- merge(l4_top, r4_top,
                by = c("CauseA","CauseB","CauseC","CauseD"), all = TRUE,
                suffixes = c(paste0("_", year_left), paste0("_", year_right)))
  for (j in names(tbl4)) if (is.numeric(tbl4[[j]]) && anyNA(tbl4[[j]])) set(tbl4, which(is.na(tbl4[[j]])), j, 0)
  setnames(tbl4, c("CauseA","CauseB","CauseC","CauseD"), c("Cause A","Cause B","Cause C","Cause D"))
  tbl4[, `Δ Deaths` := get(paste0("Deaths_", year_right)) - get(paste0("Deaths_", year_left))]
  left_deaths_col <- paste0("Deaths_", year_left)
  if (!(left_deaths_col %in% names(tbl4))) left_deaths_col <- paste0("Deaths_", year_left)
  setorderv(tbl4, cols = left_deaths_col, order = -1L, na.last = TRUE)

  ft4_global_comp <- fmt_table(tbl4)

  # ---------- return ----------
  list(
    ft1_comp          = ft1_comp,
    ft2_global_comp   = ft2_global_comp,
    ft2_withinA_comp  = ft2_withinA_comp,
    ft3_global_comp   = ft3_global_comp,
    ft4_global_comp   = ft4_global_comp
  )
}


tabs_y <- make_sequence_tables_compare(
    dt,
    icd_code = "E10-E14",
    year_left = 2021,
    year_right = 2022,
    top_pairs_n = 30, top_triplets_n = 30, top_quads_n = 30
)

# Then render:
tabs_y$ft1_comp
tabs_y$ft2_global_comp
tabs_y$ft2_withinA_comp
tabs_y$ft3_global_comp
tabs_y$ft4_global_comp

make_multi_cause_change_heatmap <- function(
  dt,
  icd_code,
  year_left,
  year_right,
  level = 2,      # 2=A→B, 3=A→B→C, 4=A→B→C→D
  top_n = 40,
  min_total = 10,
  alpha = 0.05,
  p_adjust = "BH"
) {
  require(data.table)
  require(ggplot2)
  require(scales)
  conflicts_prefer(stats::fisher.test)

  stopifnot(level %in% c(2L,3L,4L))
  if (!is.data.table(dt)) dt <- as.data.table(dt)

  # --- subset & tag ---
  dx <- dt[LGH_CauseGroup %in% icd_code]
  dx[, c("CauseA","CauseB","CauseC","CauseD") :=
        lapply(.SD, function(x) fifelse(x == 888, NA_character_, x)),
        .SDcols = c("CauseA","CauseB","CauseC","CauseD")]
  dx[, cause_level := fifelse(!is.na(CauseD), "4",
                       fifelse(!is.na(CauseC), "3",
                         fifelse(!is.na(CauseB), "2",
                           fifelse(!is.na(CauseA), "1", "0"))))]

  key_cols <- switch(
    as.character(level),
    `2` = c("CauseA","CauseB"),
    `3` = c("CauseA","CauseB","CauseC"),
    `4` = c("CauseA","CauseB","CauseC","CauseD")
  )

  dx <- dx[cause_level == as.character(level) & epi_year %in% c(year_left, year_right)]

  totals_year <- dx[, .(total_level = sum(count, na.rm = TRUE)), by = epi_year]
  seq_year <- dx[, .(Deaths = sum(count, na.rm = TRUE)), by = c(key_cols, "epi_year")]
  seq_year <- dcast(seq_year, as.formula(paste(paste(key_cols, collapse = " + "), "~ epi_year")),
                    value.var = "Deaths", fill = 0)

  total_L <- totals_year[epi_year == year_left,  total_level];  if (length(total_L)==0) total_L <- 0
  total_R <- totals_year[epi_year == year_right, total_level];  if (length(total_R)==0) total_R <- 0

  seq_year_long <- melt(seq_year, id.vars = key_cols, variable.name = "epi_year", value.name = "Deaths")
  seq_year_long[, epi_year := as.integer(as.character(epi_year))]
  seq_year_long[, share := fifelse(
    epi_year == year_left, 100 * Deaths / pmax(1, total_L),
    100 * Deaths / pmax(1, total_R)
  )]

  seq_sum <- seq_year_long[, .(
    Deaths_L = Deaths[epi_year == year_left],
    Deaths_R = Deaths[epi_year == year_right]
  ), by = key_cols]
  seq_sum[is.na(Deaths_L), Deaths_L := 0]
  seq_sum[is.na(Deaths_R), Deaths_R := 0]

  seq_sum[, `:=`(
    total_L = total_L,
    total_R = total_R,
    other_L = pmax(total_L - Deaths_L, 0),
    other_R = pmax(total_R - Deaths_R, 0)
  )]

  seq_sum <- seq_sum[(Deaths_L + Deaths_R) >= min_total]

  seq_sum[, p := {
    mat <- matrix(c(Deaths_L, other_L, Deaths_R, other_R), nrow = 2, byrow = TRUE)
    suppressWarnings(fisher.test(mat)$p.value)
  }, by = key_cols]
  seq_sum[, q := p.adjust(p, method = p_adjust)]

  eps <- 0.5
  seq_sum[, `:=`(
    share_L = 100 * Deaths_L / pmax(1, total_L),
    share_R = 100 * Deaths_R / pmax(1, total_R),
    d_pp    = (100 * Deaths_R / pmax(1, total_R)) - (100 * Deaths_L / pmax(1, total_L)),
    log2FC  = log2(((Deaths_R + eps)/pmax(1, total_R)) / ((Deaths_L + eps)/pmax(1, total_L)))
  )]

  seq_sum[, seq_lab := do.call(paste, c(.SD, list(sep = " \u2192 "))), .SDcols = key_cols]
  seq_sum[, combined := Deaths_L + Deaths_R]
  seq_sum[, abs_dpp := abs(d_pp)]                     # <-- add helper col
  setorder(seq_sum, -combined, -abs_dpp)              # <-- order by names only
  if (nrow(seq_sum) > top_n) seq_sum <- seq_sum[1:top_n]

  plot_df <- merge(
    seq_sum[, .(seq_lab, share_L)],
    seq_sum[, .(seq_lab, share_R, sig = q < alpha, d_pp, log2FC)],
    by = "seq_lab", all = TRUE
  )

  plot_long <- melt(plot_df, id.vars = c("seq_lab","sig","d_pp","log2FC"),
                    variable.name = "side", value.name = "share")
  plot_long[, epi_year := ifelse(side == "share_L", year_left, year_right)]
  plot_long[, side := NULL]

  ord <- seq_sum[order(-abs_dpp)]$seq_lab           # <-- base order() on helper col
  plot_long[, seq_lab := factor(seq_lab, levels = rev(ord))]
  plot_long[, star := ifelse(epi_year == year_right & sig, "\u2605", ""), by = seq_lab]

  p <- ggplot(plot_long, aes(x = factor(epi_year), y = seq_lab, fill = share)) +
    geom_tile() +
    geom_text(aes(label = star), vjust = 0.5, hjust = -0.2, size = 4) +
    scale_fill_continuous(name = "Share within level (%)", labels = label_number(accuracy = 0.1)) +
    scale_x_discrete(name = "Year") +
    labs(
      y = paste0("Sequence (level ", level, ")"),
      title = paste0("Multiple-cause change heatmap: ", icd_code, " — Level ", level),
      subtitle = paste0("Tile = share of level-", level, " sequences per year; ",
                        "\u2605 = FDR<", alpha, " (", p_adjust, "); ordered by |Δpp|"),
      caption = "Returned table includes Δpp, log2FC, p, q"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.text.y  = element_text(family = "mono"),
          legend.position = "right",
          plot.title.position = "plot") +
    coord_cartesian(clip = "off")

  list(
    plot    = p,
    results = seq_sum[, .(sequence = seq_lab, Deaths_L, Deaths_R, total_L, total_R,
                          share_L, share_R, d_pp, abs_dpp, log2FC, p, q, combined)][order(-abs_dpp)]
  )
}


out <- make_multi_cause_change_heatmap(
    dt,
    icd_code   = "I49-I51",
    year_left  = 2021,
    year_right = 2022,
    level      = 2, # 2=A→B, 3=A→B→C, 4=A→B→C→D
    top_n      = 30
)
graphics.off()
out$plot # the heatmap
out$results
