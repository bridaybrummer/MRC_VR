# Compare Tom's model with no provincial effects to simple glm.nb with provincial effects 
library(data.table)
library(arrow)
library(magrittr)
library(ggplot2)
conflicts_prefer(lubridate::year)
# modelled data at province level
read_feather("LGH_BaselinePredictions_ProvinceLevel_TempPop.feather") -> dt_collapse_prov

# modelled data at national level 
read_feather("LGH_AnalysisData_2022.feather") -> dt_analysis







names(dt_collapse_prov)

dt_collapse_prov[, 
    .(
        count = sum(count), # the observed 
        baseline = sum(pred_simple, na.rm = TRUE) # the expected
    ),
    by = .(week_start)] %>%
        ggplot(
            aes(
                x = week_start, 
                y = count - baseline
            )
        )+ 
        geom_line(color = "blue")

dt_collapse_prov

dt_collapse_prov[,
    .(
        count = sum(count), # the observed
        pred_simple = sum(pred_simple, na.rm = TRUE),  # the expected
        pred_simple_fourier = sum(pred_simple_fourier, na.rm = TRUE), 
        pred_simple_interactions = sum(pred_simple_interactions, na.rm = TRUE), 
        pred_simple_pop = sum(pred_simple_pop, na.rm = TRUE), 
        pred_fourier_interactions_pop_offset = sum(pred_fourier_interactions_pop_offset, na.rm = TRUE)
    ),
    by = .(week_start)
] -> dt_prov_model

dt_prov_model%>%
    ggplot(
        aes(
            x = week_start,
        )
    ) +
    geom_line(aes(
                    y = count, 
                    color ="observed")) +
    geom_line(aes(
                    y = pred_simple, 
                    color ="expected")) +
    scale_color_manual(values = c("#4e4ee3", "#f65b5b"))+
    theme_minimal()-> 
    provincial_model_plot_SA

    provincial_model_plot_SA


# Show overall observed vs expected  by province
dt_collapse_prov[, 
    .(
        count = sum(count), # the observed
        baseline = sum(pred_simple, na.rm = TRUE) # the expected
    ),
    by = .(DeathProvince, week_start)
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
    geom_vline(
        xintercept = as.Date("2020-04-01"),
        linetype = "dashed",
        color = "red"
    ) +
    scale_color_manual(
        values = c("Expected" = "blue", "Observed" = "red")
    ) +
    facet_wrap(
        ~DeathProvince,
        scales = "free_y",
        nrow = 4
    ) +
    theme_minimal()-> 
    provincial_model_prov_O_vs_E_plot_SA


# by cause codes 
dt_collapse_prov[, 
.(
    count = sum(count), # the observed
    baseline = sum(pred_simple, na.rm = TRUE) # the expected
), by = .(LGH_Cause, week_start)
] -> dt_cause_model


# national level comparison

dt_analysis[epi_week %in% 1:52,
    .(
        count = sum(count), # the observed
        baseline = sum(baseline, na.rm = TRUE) # the expected
    ),
    by = .(week_start)
] -> dt_national_model 

dt_national_model %>%
    ggplot(
        aes(
            x = week_start, 
        )
    ) +
    geom_line(aes(
                    y = count, 
                    color ="observed")) +
    geom_line(aes(
                    y = baseline, 
                    color ="expected")) +
    scale_color_manual(values = c("#4e4ee3", "#f65b5b"))+
    theme_minimal()-> 
    national_model_plot_SA

    national_model_plot_SA


# Natioanl model by ICD codes 2019-2022
dt_analysis[epi_week %in% 1:52 & epi_year %in% 2019:2022,
    .(
        count = sum(count), # the observed
        baseline = sum(baseline, na.rm = TRUE) # the expected
    ),
    by = .(LGH_Cause, week_start)
] -> dt_national_cause_model


dt_analysis[
     epi_year %in% 2019:2022, # this will ensure it is only for modelled causes
    .(
        count = sum(count, na.rm = TRUE),
        baseline = sum(baseline, na.rm = TRUE),
        difference = sum(count, na.rm = TRUE) - sum(baseline, na.rm = TRUE)
    ),
    by = .(week_start, LGH_Cause)
] %>%
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
    scale_color_manual(
        values = c("Expected" = "#f65b5b", "Observed" = "#4e4ee3")
    ) +
    geom_vline(
        xintercept = as.Date("2020-04-01"),
        linetype = "dashed",
        color = "red"
    ) +
    facet_wrap(
        ~LGH_Cause,
        ncol = 3,
        scales = "free_y"
    ) +
    labs(x = "Cause of Death", y = "Excess Deaths", fill = "Year") +
    theme_minimal() -> national_model_icd_O_vs_E_plot_SA

    national_model_icd_O_vs_E_plot_SA

dt_analysis[
    !is.na(LGH_Cause) & epi_year %in% 2019:2022, # this will ensure it is only for modelled causes
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
    by = .(week_start, LGH_Cause)
] %>%
    ggplot() +
    geom_col(
        aes(
            x = week_start,
            y = difference,
            fill = excess_deficit
        ),
        stat = "identity",
        position = "dodge"
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
        ~LGH_Cause,
        ncol = 3,
        scales = "free_y"
    ) +
    labs(x = "Cause of Death", y = "Excess Deaths", fill = "Year") +
    theme_minimal()-> national_model_icd_difference_plot_SA

national_model_icd_difference_plot_SA

dt_national_cause_model%>%
    ggplot(
        aes(
            x = week_start, 
        )
    ) +
    geom_line(aes(
                    y = count, 
                    color ="observed")) +
    geom_line(aes(
                    y = baseline, 
                    color ="expected")) +
    scale_color_manual(values = c("blue", "red"))+
    facet_wrap(~LGH_Cause, scales = "free_y")+
    theme_minimal() -> 
    national_model_cause_plot_SA


    national_model_cause_plot_SA

# ==========================
# GEOM_TILE 
# ==========================

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

dt_analysis_ordered_final %>%
    ggplot() +
    geom_tile(
        aes(
            x = period,
            y = LGH_Cause,
            fill = factor_diff
        )
    ) +
    geom_text(
        aes(
            x = period,
            y = LGH_Cause,
            label = round(factor_diff, 1)
        ),
        color = "black",
        size = 3
    ) +
    scale_fill_gradient2(
        low = "green",
        mid = "white",
        high = "red",
        midpoint = 1,
        name = "Proportional Difference (%)"
    )+ 
    theme_minimal()-> 
    national_model_cause_heatmap_SA

national_model_cause_heatmap_SA






# now compare how different the national model vs the provincial model is 
dt_prov_model[, 
    .(
        week_start,
        count = count,
        prov_simple = pred_simple,
        prov_simple_fourier = pred_simple_fourier,
        prov_simple_interactions = pred_simple_interactions, 
        prov_simple_pop = pred_simple_pop,
        prov_fourier_interactions_pop_offset = pred_fourier_interactions_pop_offset
    )
] -> dt_prov_model2

dt_national_model[, 
    .(
        week_start,
        nat_model = baseline
    )
] -> dt_national_model2

dt_prov_model2[dt_national_model2, on = .(week_start), 
    nat_model := i.nat_model
] -> dt_compare_models





# this shows the difference to to the national model 
#week_start %in% seq(as.Date("2015-01-05"), as.Date("2019-12-30"), by = "day"),
dt_compare_models[
    , 
    .(
        week_start,
        diff_prov_simple = prov_simple - nat_model
        #diff_prov_simple_fourier = prov_simple_fourier - nat_model,
        #diff_prov_simple_interactions = prov_simple_interactions - nat_model, 
        #diff_prov_simple_pop = prov_simple_pop - nat_model
        #diff_fourier_interactions_pop_offset = prov_fourier_interactions_pop_offset - nat_model
    )
] %>%
    melt( 
        id.vars = "week_start",
        variable.name = "model",
        value.name = "diff"
    )%>%
    ggplot(
        aes(
            x = week_start,
            y = diff, 
            color = model
        )
    ) +
    geom_line() +
    geom_hline(yintercept = 0, linetype = "dashed")+
    theme_minimal()-> 
    national_vs_provincial_model_diff_plot_SA


save(
    provincial_model_plot_SA,
    provincial_model_prov_O_vs_E_plot_SA,
    national_model_plot_SA,
    national_model_icd_O_vs_E_plot_SA,
    national_model_icd_difference_plot_SA,
    national_model_cause_plot_SA,
    national_model_cause_heatmap_SA,
    national_vs_provincial_model_diff_plot_SA, 
    file = "figures/model_comparison_prov_nat_plots.rda"
)



# look at the RMSE for each model 
dt_compare_models[week_start %in% seq(as.Date("2015-01-05"), as.Date("2019-12-30"), by = "day"),
    .(
        rmse_prov_simple = sqrt(mean((count - prov_simple)^2)),
        rmse_prov_simple_fourier = sqrt(mean((count - prov_simple_fourier)^2)),
        rmse_prov_simple_interactions = sqrt(mean((count - prov_simple_interactions)^2)),
        rmse_nat_model = sqrt(mean((count - nat_model)^2))
    )
]

# look at the difference to the actual observed deaths
dt_compare_models[week_start %in% seq(as.Date("2015-01-05"), as.Date("2019-12-30"), by = "day"),
    .(
        prov_simple = sum(count - prov_simple),
        prov_simple_fourier = sum(count - prov_simple_fourier),
        prov_simple_interactions = sum(count - prov_simple_interactions),
        nat_model = sum(count - nat_model)
    ), 
    by = .(week_start)
]%>%
    melt(
        id.vars = "week_start",
        variable.name = "model",
        value.name = "mae"
    ) %>%
    ggplot(
        aes(
            x = week_start,
            y = mae,
            color = model
        )
    ) +
    geom_line() +
    theme_minimal()

# the provincial model may show that some provinces may have interacted with the morbidity causing a higher number of predicted number of deaths than if just looking at the national average. 

# look at the difference by province
dt_collapse_prov[LGH_Cause == "J09-J18",
    .(count = sum(count), baseline = sum(pred_simple_interactions)),
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
        nrow = 4
    ) +
    theme_minimal()
