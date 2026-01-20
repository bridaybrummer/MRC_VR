# On the assumption of influenza during COVID-19 

# Influenza coded deaths during the COVID-19 pandemic are unlikely to represent influenza since there was minimal to no detection of influenza during the pandmic. 

# Influenza coded deaths are therefore likley to represent COVID-19 deaths. 

# The aim is to determine what the range of deaths coded as influenza are, and to reassign these deaths to COVID-19.


# J codes are almost all for flu


# --------- Libs ---------#
library(data.table)
library(dplyr)
library(arrow)
library(tidyr)
library(ggplot2)
library(mgcv)
library(lubridate)
library(purrr)

# --------- Load data ---------#

source( "lgh_cause_description_map.r")

icd_lookup

read_feather("LGH_MasterFile_preCollapsed2022.feather") %>% as.data.table() -> dt

# combine for LGH descriiption
dt[, grepl("LGH_Cause", names(dt))] %>%names(dt)[.]

dt$LGH_CauseGroup%>%unique()
dt$LGH_Cause%>%unique()
dt$LGH_Cause_encoded %>% unique()
dt$epi_year%>%unique() 

dt[icd_lookup, on = .(LGH_Cause)]-> dt

# --------- Data prep ---------#

# Filter J codes 

dt[grepl("J", LGH_Cause), ]-> dt_codes

dt_codes

# model these by counts with glm.nb

dt_codes%>%names() 

# comnvert from haven labelled to factor to numeric
#dt_codes$sex <- as.numeric(as.character(dt_codes$sex))
haven::as_factor(dt_codes$sex) %>% as.numeric() -> dt_codes$sex


dt_codes[, 
    .(count = .N),
    by = .(weekstart, epi_week, epi_year, LGH_Cause, sex, agegroup,LGH_Cause, DeathProvince, LGH_Cause_encoded)
][, 
week_start := weekstart]-> dt_collapsed

# DT collapsed must be factors for prediciton before filtering 
dt_collapsed



################################################################################
# SECTION 6: CREATE BASELINE (2015-2019)
################################################################################
dt_collapsed$count %>% sum()
nrow(dt_codes)
# Note: The original Stata code has some inconsistencies in variable names
# (uses 'age' vs 'agegroup', 'LGH' vs 'LGH_Cause_encoded')
# I'll follow the logic as closely as possible


# U07 is COVID, which shoudl not be in baseline

# Aggregate for baseline model
# Note: Original uses 'age' and 'LGH' but we have 'agegroup' and 'LGH_Cause_encoded'
    dt_for_model_unfiltered <- dt_collapsed[, .(count = sum(count)),
        by = .(week_start, epi_week, epi_year, sex, agegroup, LGH_Cause, DeathProvince, LGH_Cause_encoded)
    ]

dt_for_model_unfiltered
dt_for_model_unfiltered$LGH_Cause %>% table()


dt_for_model_unfiltered[agegroup %in% "40-59" & sex %in% 1,] %>%
    ggplot() +
    geom_line(
        aes(
            x = epi_week,
            y = count,
            color = as.factor(LGH_Cause),
        ),
        show.legend = FALSE
    ) +
    facet_wrap(
        DeathProvince~epi_year, 
        ncol = 9, 
       # nrow = 9
    ) +
    theme_minimal()

################################################################################
# SECTION 7: NEGATIVE BINOMIAL REGRESSION FOR BASELINE
################################################################################

cat("Fitting negative binomial regression model for baseline...\n")
cat("This may take several minutes...\n")

# Filter out week 53 for model fitting
dt_for_model_unfiltered_no_epi53 <- dt_for_model_unfiltered[epi_week != 53 & !is.na(DeathProvince), ]


# LGH_Cause_base_line_factor_levels <- dt_model_train$LGH_Cause %>% unique() %>% sort()

# Create interaction terms for the model
# In Stata: i.sex#i.agegroup#i.LGH i.epi_week#i.LGH
# In R: use factor() and * for interactions
dt_for_model_unfiltered_no_epi53[, sex_f := factor(sex)]
dt_for_model_unfiltered_no_epi53[, agegroup_f := factor(agegroup)]
dt_for_model_unfiltered_no_epi53[, LGH_f := factor(LGH_Cause)]
dt_for_model_unfiltered_no_epi53[, epi_week_f := factor(epi_week)]
dt_for_model_unfiltered_no_epi53[, province_f := factor(DeathProvince)]

# Add fourier terms

wk <- as.integer(dt_for_model_unfiltered_no_epi53$epi_week) # 1..52
dt_for_model_unfiltered_no_epi53$sin1 <- sin(2 * pi * wk / 52)
dt_for_model_unfiltered_no_epi53$cos1 <- cos(2 * pi * wk / 52)
dt_for_model_unfiltered_no_epi53$sin2 <- sin(4 * pi * wk / 52)
dt_for_model_unfiltered_no_epi53$cos2 <- cos(4 * pi * wk / 52) # optional 2nd harmonic

# add population 
# add in the population
load("pop_collapsed_prov.RData") # loads dt_pop
pop_collapsed_prov
#dt_for_model -> dt_collapse_prov
# validation check on the matching levels
# Province
setdiff(
    dt_for_model_unfiltered_no_epi53$province_f %>% unique(),
    pop_collapsed_prov$province_f %>% unique()
)
# agegroup
setdiff(
    dt_for_model_unfiltered_no_epi53$agegroup_f %>% unique(),
    pop_collapsed_prov$agegroup_f %>% unique()
)
# sex
setdiff(
    dt_for_model_unfiltered_no_epi53$sex_f %>% unique(),
    pop_collapsed_prov$sex_f %>% unique()
)

dt_for_model_unfiltered_no_epi53 %>% na.omit()
dt_for_model_unfiltered_no_epi53[
    pop_collapsed_prov, on = .(epi_year, sex_f, agegroup_f, province_f)
    ] -> dt_for_model_unfiltered_no_epi53


# Fit negative binomial regression
# This is computationally intensive!
# Formula: count ~ epi_year + sex:agegroup:LGH + epi_week:LGH

# ========================================
# MODELLING START
# ========================================
dt_for_model_unfiltered_no_epi53
dt_for_model_unfiltered_no_epi53[
        epi_year %in%2015:2019
        ]-> dt_baseline

dt_for_model <- copy(dt_baseline)

# you now have 
dt_for_model_unfiltered_no_epi53 # total dataset without epiweek 53, facotrs vars
dt_baseline # filtered for only training
dt_for_model # for modelling

# Create factors (this shouldnt be necessary anymore )


#dt_for_model %>% na.omit() -> dt_collapse_prov




# plot all data
dt_for_model[,
    .(count = sum(count)),
    by = .(province_f, week_start, epi_year, epi_week)
] %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = count,
            color = as.factor(province_f),
        ),
        show.legend = FALSE
    ) +
    facet_wrap(
        ~province_f
    ) +
    theme_minimal()

# plot training data only
dt_for_model[,
    .(count = sum(count)),
    by = .(province_f, week_start, epi_year, epi_week)
] %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = count,
            color = as.factor(province_f),
        ),
        show.legend = FALSE
    ) +
    facet_wrap(
        ~province_f
    ) +
    theme_minimal()

# plot the population in each province
dt_for_model[,
    .(pop = unique(pop)),
    by = .(province_f, week_start, epi_year, epi_week)
] %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = pop,
            color = as.factor(province_f),
        ),
        show.legend = FALSE
    ) +
    facet_wrap(
        ~province_f
    ) +
    theme_minimal()

# plot mortality rates per province
dt_for_model[,
    .(count = sum(count), pop = unique(pop)),
    by = .(province_f, week_start, epi_year, epi_week)
][
    ,
    mortality_rate := count / pop * 100000
] %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = mortality_rate,
            color = as.factor(province_f),
        ),
        show.legend = FALSE
    ) +
    facet_wrap(
        ~province_f
    ) +
    theme_minimal()


library(MASS)
if(FALSE) {
    print("Starting simple models fitting...")
    model_simple <- glm.nb(count ~ province_f + sex_f + agegroup_f + LGH_Cause_encoded + epi_week_f + epi_year,
        data = dt_model_train_prov
    )
    save(model_simple, file = "j_code/LGH_NegativeBinomialModel_2015_2019_ProvinceSimple_glmnb.rda")

    print("Starting fourier models fitting...")
    model_simple_fourier <- glm.nb(count ~ province_f + sex_f + agegroup_f + LGH_Cause_encoded + sin1 + sin2 + cos1 + cos2,
        data = dt_model_train_prov
    )
    save(model_simple_fourier, file = "j_code/LGH_NegativeBinomialModel_2015_2019_ProvinceSimpleFourier_glmnb.rda")

    print("Starting interaction models fitting...")
    model_simple_interactions <- glm.nb(count ~ province_f + sex_f:agegroup_f:LGH_Cause_encoded + epi_week_f:LGH_Cause_encoded + epi_year,
        data = dt_model_train_prov
    )
    save(model_simple_interactions, file = "j_code/LGH_NegativeBinomialModel_2015_2019_ProvinceSimpleInteractions_glmnb.rda")

    print("Starting population offset models fitting...")
    model_simple_pop <- glm.nb(count ~ offset(log(pop)) + province_f + sex_f + agegroup_f + LGH_Cause_encoded + epi_week_f + epi_year,
        data = dt_model_train_prov
    )
    save(model_simple_pop, file = "j_code/LGH_NegativeBinomialModel_2015_2019_ProvinceSimplePopOffset_glmnb.rda")

    print("Starting fourier interaction population offset models fitting...")
    model_fourier_interactions_pop_offset <- glm.nb(
        count ~
            offset(log(pop)) + sin1 + cos1 + sin2 + cos2 + province_f:(sin1 + cos1) + sex_f + agegroup_f + LGH_Cause_encoded:province_f,
        data = dt_model_train_prov
    )
    save(model_fourier_interactions_pop_offset, file = "j_code/LGH_NegativeBinomialModel_2015_2019_ProvinceFourierInteractionsPopOffset_glmnb.rda")
} else {
    load("j_code/LGH_NegativeBinomialModel_2015_2019_ProvinceSimple_glmnb.rda")
    load("j_code/LGH_NegativeBinomialModel_2015_2019_ProvinceSimpleFourier_glmnb.rda")
    load("j_code/LGH_NegativeBinomialModel_2015_2019_ProvinceSimpleInteractions_glmnb.rda")
    load("j_code/LGH_NegativeBinomialModel_2015_2019_ProvinceSimplePopOffset_glmnb.rda")
    load("j_code/LGH_NegativeBinomialModel_2015_2019_ProvinceFourierInteractionsPopOffset_glmnb.rda")
}


# GLMMTMB is good for modelling some hierarchical trends. 
# GAM is good if there are smooth trends over time, with less need to capture hierarchy 
if(FALSE){
# try glmmTMB again
library(glmmTMB)
fit_tmb <- glmmTMB(
    count ~ province_f + poly(epi_year, 2) +
        sin1 + cos1 + sin2 + cos2 +
        province_f:(sin1 + cos1) +
        sex_f + agegroup_f + LGH_Cause_encoded +
        
        offset(log(pop)),
    family = nbinom2(), # try nbinom1 too; sometimes better
    dispformula = ~province_f + LGH_Cause_encoded, # start modest; expand if needed
    data = dt_model_train_prov
)
fit_tmb

save(fit_tmb, file = "j_code/fit_tmb.rda")
# try glmmTMB again
library(glmmTMB)
fit_tmb_interactions <- glmmTMB(
    count ~ province_f + poly(epi_year, 2) +
        sin1 + cos1 + sin2 + cos2 +
        province_f:(sin1 + cos1) +
        sex_f:agegroup_f:LGH_Cause_encoded +

        offset(log(pop)),
    family = nbinom2(), # try nbinom1 too; sometimes better
    dispformula = ~ province_f + LGH_Cause_encoded, # start modest; expand if needed
    data = dt_model_train_prov
)
fit_tmb_interactions
save(fit_tmb_interactions, file = "j_code/fit_tmb_interactions.rda")

# fit a GAM
library(mgcv)
model_gam <- gam(
    count ~ s(epi_week, bs = "cc", k = 20) +
        s(epi_year, k = 5) +
        province_f +
        sex_f + agegroup_f + LGH_Cause_encoded +
        offset(log(pop)),
    family = nb(),
    data = dt_model_train_prov
)
save(model_gam, file = "j_code/model_gam.rda")

}else{
    load("j_code/fit_tmb_interactions.rda")
    load("j_code/fit_tmb.rda")
    load("j_code/model_gam.rda")
}



# models

model_simple
model_simple_fourier
model_simple_interactions
model_simple_pop
model_fourier_interactions_pop_offset

AIC(fit_tmb,fit_tmb_interactions, model_gam,  model_simple, model_simple_fourier, model_simple_interactions, model_simple_pop, model_fourier_interactions_pop_offset)
BIC(fit_tmb, fit_tmb_interactions,model_gam,  model_simple, model_simple_fourier, model_simple_interactions, model_simple_pop, model_fourier_interactions_pop_offset)



# Model simple pop diagnsotics
pacman::p_load(DHARMa)
#sim <- simulateResiduals(model_simple_pop) # from MASS::glm.nb
#plot(sim)
#testDispersion(sim)
#testZeroInflation(sim)
dt_for_model_unfiltered_no_epi53$epi_year %>% table()

dt_for_model_unfiltered_no_epi53[, pred_fit_tmb := predict(fit_tmb, newdata = dt_for_model_unfiltered_no_epi53, type = "response", allow.new.levels = TRUE)]
dt_for_model_unfiltered_no_epi53[, pred_fit_tmb_interactions := predict(fit_tmb_interactions, newdata = dt_for_model_unfiltered_no_epi53, type = "response", allow.new.levels = TRUE)]
dt_for_model_unfiltered_no_epi53[, pred_gam := predict(model_gam, newdata = dt_for_model_unfiltered_no_epi53, type = "response", allow.new.levels = TRUE)]
dt_for_model_unfiltered_no_epi53[, pred_simple := predict(model_simple, newdata = dt_for_model_unfiltered_no_epi53, type = "response", allow.new.levels = TRUE)]
dt_for_model_unfiltered_no_epi53[, pred_simple_fourier := predict(model_simple_fourier, newdata = dt_for_model_unfiltered_no_epi53, type = "response", allow.new.levels = TRUE)]
dt_for_model_unfiltered_no_epi53[, pred_simple_interactions := predict(model_simple_interactions, newdata = dt_for_model_unfiltered_no_epi53, type = "response", allow.new.levels = TRUE)]
dt_for_model_unfiltered_no_epi53[, pred_simple_pop := predict(model_simple_pop, newdata = dt_for_model_unfiltered_no_epi53, type = "response", allow.new.levels = TRUE)]
dt_for_model_unfiltered_no_epi53[, pred_fourier_interactions_pop_offset := predict(model_fourier_interactions_pop_offset, newdata = dt_for_model_unfiltered_no_epi53, type = "response", allow.new.levels = TRUE)]

dt_for_model_unfiltered_no_epi53[, pred_fit_tmb_diff := count - pred_fit_tmb]
dt_for_model_unfiltered_no_epi53[, pred_fit_tmb_interactions_diff := count - pred_fit_tmb_interactions]
dt_for_model_unfiltered_no_epi53[, pred_gam_diff := count - pred_gam]
dt_for_model_unfiltered_no_epi53[, pred_simple_diff := count - pred_simple]
dt_for_model_unfiltered_no_epi53[, pred_simple_fourier_diff := count - pred_simple_fourier]
dt_for_model_unfiltered_no_epi53[, pred_simple_interactions_diff := count - pred_simple_interactions]
dt_for_model_unfiltered_no_epi53[, pred_simple_pop_diff := count - pred_simple_pop]
dt_for_model_unfiltered_no_epi53[, pred_fourier_interactions_pop_offset_diff := count - pred_fourier_interactions_pop_offset]
#write_feather(dt_collapse_prov, "LGH_BaselinePredictions_ProvinceLevel_TempPop.feather")

# filder out experimnetal years for difference and RMSE AX
dt_for_model_unfiltered_no_epi53[epi_year %in% 2015:2019, ] -> dt_collapse_prov

# Aggregate to province-week level (sums predictions and observed counts)
dt_wide_preds <- dt_collapse_prov[,
    .(
        count = sum(count, na.rm = TRUE),
        pred_fit_tmb = sum(pred_fit_tmb, na.rm = TRUE),
        pred_fit_tmb_interactions = sum(pred_fit_tmb_interactions, na.rm = TRUE),
        pred_gam = sum(pred_gam, na.rm = TRUE),
        pred_simple = sum(pred_simple, na.rm = TRUE),
        pred_simple_fourier = sum(pred_simple_fourier, na.rm = TRUE),
        pred_simple_interactions = sum(pred_simple_interactions, na.rm = TRUE),
        pred_simple_pop = sum(pred_simple_pop, na.rm = TRUE),
        pred_fourier_interactions_pop_offset = sum(pred_fourier_interactions_pop_offset, na.rm = TRUE),

        pred_fit_tmb_diff = sum(pred_fit_tmb_diff, na.rm = TRUE),
        pred_fit_tmb_interactions_diff = sum(pred_fit_tmb_interactions_diff, na.rm = TRUE),
        pred_gam_diff = sum(pred_gam_diff, na.rm = TRUE),
        pred_simple_diff = sum(pred_simple_diff, na.rm = TRUE), 
        pred_simple_fourier_diff = sum(pred_simple_fourier_diff, na.rm = TRUE),
        pred_simple_interactions_diff = sum(pred_simple_interactions_diff, na.rm = TRUE),
        pred_simple_pop_diff = sum(pred_simple_pop_diff, na.rm = TRUE),
        pred_fourier_interactions_pop_offset_diff = sum(pred_fourier_interactions_pop_offset_diff, na.rm = TRUE)
    ),
    by = .(week_start, epi_year, epi_week, province_f, LGH_Cause)
]
dt_wide_preds

setorder(dt_wide_preds, province_f, week_start)
dt_wide_preds

melt(dt_wide_preds, id.vars = c("week_start", "epi_year", "epi_week", "province_f", "LGH_Cause"),
    measure.vars = c(
        "count",
        "pred_fit_tmb",
        "pred_fit_tmb_interactions",
        "pred_gam",
        "pred_simple",
        "pred_simple_fourier",
        "pred_simple_interactions",
        "pred_simple_pop",
        "pred_fourier_interactions_pop_offset"
    ),
    variable.name = "model",
    value.name = "deaths"
) -> dt_long_preds_actual

graphics.off()
dt_long_preds_actual%>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = deaths,
            color = model
        )
    )   +
    facet_wrap( 
        province_f ~LGH_Cause, 
        scales = "free_y", 
        strip.position = "right"
        )+ 
        theme_minimal() 



melt(dt_wide_preds, id.vars = c("week_start", "epi_year", "epi_week", "province_f", "LGH_Cause"),
    measure.vars = c(
       # "count",
        "pred_fit_tmb_diff",
        "pred_fit_tmb_interactions_diff",
        "pred_gam_diff",
        "pred_simple_diff",
        "pred_simple_fourier_diff",
        "pred_simple_interactions_diff",
        "pred_simple_pop_diff",
        "pred_fourier_interactions_pop_offset_diff"
    ),
    variable.name = "model",
    value.name = "difference"
) -> dt_long_preds_actual_long

dt_long_preds_actual_long[, 
.(difference = mean(difference)), 
by = .(LGH_Cause, province_f, model)
]%>%
    ggplot() +
    geom_col(
        aes(
            x = model,
            y = difference,
            fill = model
        )
    ) +
    facet_wrap(
         LGH_Cause~province_f,
        scales = "free_y",
       # strip.position = "right", 
       ncol = 9
    ) +
    theme_minimal()

# compute RMSE for each model

rmse<- function(observed, predicted) {
    sqrt(mean((observed - predicted)^2, na.rm = TRUE))
}

rmse_results<- 
dt_wide_preds[, .(
    rmse_fit_tlmb = rmse(count, pred_fit_tmb),
    rmse_fit_tlmb_interactions = rmse(count, pred_fit_tmb_interactions),
    rmse_gam = rmse(count, pred_gam),
    rmse_simple = rmse(count, pred_simple),
    rmse_simple_fourier = rmse(count, pred_simple_fourier),
    rmse_simple_interactions = rmse(count, pred_simple_interactions),
    rmse_simple_pop = rmse(count, pred_simple_pop),
    rmse_fourier_interactions_pop_offset = rmse(count, pred_fourier_interactions_pop_offset)
), by = province_f]

rmse_results


# plot the difference from count 
# use the modelled data for a natioanl picture with initial couterfacutal and then apply a decay to the expected number 
###


dt_for_model_unfiltered_no_epi53$LGH_Cause %>% table()

dt_for_model_unfiltered_no_epi53[, 
.(observed = sum(count), 
    expected = sum(pred_simple)), 
by = .(epi_year, epi_week, week_start)
][order(as.numeric(week_start)),]-> national_j_codes

national_j_codes
national_j_codes%>%na.omit() 



national_j_codes%>%
    ggplot() + 
    geom_line( 
        aes( 
            x = week_start, 
            y = observed, 
            color = "blue"
        )
    )+ 
    geom_line( 
        aes( 
            x = week_start, 
            y = expected, 
            color = "red"
        )
    )+ theme_minimal() 


# generate various credibility bands using a ladder apprach
# poisson
# rates
# Monte carlo 

dt_for_model_unfiltered_no_epi53[, pred_simple := predict(model_simple, newdata = dt_for_model_unfiltered_no_epi53, type = "response", allow.new.levels = TRUE)]


# Some options. 
    # 1. Assume that there are No J codes during COVID-19 period
    # 2. Decompose the trend of J codes over time and set teh trend to zero during COVID-19 period
    # 3. Assume that there are J codes during COVID-19 period, but reduce in reversed proprtion to COVID-19 cases
    # 4. Model J-codes to detection of influenza and other surveillance data. 

# Look at the difference between observed and predicted counts

# As modelled 
    # this assume influenza was causing deaths as per normal and any excess was due to COVID. 
national_j_codes_scenario_1 <- copy(national_j_codes)[
    ,
    `:=`(
        excess   = observed - expected,
        scenario = "Scenario 1"
    )
]
# - modelled on previous 5 years trend - This assumes Influenza deaths remained constant during COVID-19 period

national_j_codes_scenario_1 %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = excess
        )
    ) +
    theme_minimal()

# number of Exess deaths 
national_j_codes_scenario_1[, sum(excess[epi_year >= 2020 & excess > 0], na.rm = TRUE)]


# Assume worse case scenario where all J code deaths during COVID-19 period are re-assigned to COVID-19
national_j_codes_scenario_2 <- copy(national_j_codes)[
    ,
    `:=`(
        excess   = fifelse(week_start > as.Date("2020-04-01"), observed, observed - expected),
        scenario = "Scenario 2"
    )
]
# - Assumes no J coded deaths occurred during COVID-19 period
national_j_codes_scenario_2%>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = excess
        )
    ) +
    theme_minimal()

national_j_codes_scenario_2[, sum(excess[epi_year >= 2020 & excess > 0], na.rm = TRUE)]

# Assume that J code deaths during COVID-19 period are reduced in proportion to COVID-19 deaths
    # the expected J code deaths are modelled as per normal but wiht the trend of COVID-9 deaths inversely applied to its trend. 

# decompose trend from COVID-19 deaths
dt_collapsed$LGH_Cause %>% unique()
dt_collapsed$LGH_Cause %>% table()

dt[LGH_Cause %in% "U07", 
    .(count = .N),
    by = .(weekstart, epi_week, epi_year)
][,
week_start := weekstart]-> dt_covid_deaths

dt_covid_deaths%>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = count
        )
    ) +
    theme_minimal()

dt_covid_deaths$weekstart%>% min()
# decompose trend traditionally 
library(forecast)
dt_covid_deaths
ts_covid <- ts(dt_covid_deaths$count, frequency = 52, start = c(2020, 1))

plot(ts_covid)

ts_covid_decomp <- stl(ts_covid, s.window = "periodic")

ts_covid_decomp_v2 <- decompose(ts_covid, type = "multiplicative")

plot(ts_covid_decomp)
plot(ts_covid_decomp_v2)

# Create the seasonality and trend as a proportion of itself 
covid_trend <- ts_covid_decomp$time.series[, c("seasonal", "trend")] %>% rowSums()
covid_trend <- ts_covid_decomp$time.series[, c("seasonal")] %>% rowSums()

covid_trend
data.table( 
    week_start = dt_covid_deaths$week_start,
    year = dt_covid_deaths$epi_year,
    covid_trend = covid_trend
)[, 
    .(prop_trend = covid_trend / max(covid_trend, na.rm = TRUE)),
    by = .(year)
]-> covid_trend_prop_by_year


if(FALSE){ # if false, just takes the proportion of COVID waves by year
# identify and number consecutive waves if count exceeds 300 deaths per week 


dt_covid_deaths[, 
is_wave := fifelse(count >= 1000, 1, 0)
][, 
wave_n := rleid(is_wave)
][is_wave == 0, wave_n := NA
][,
wave_n := zoo::na.locf(wave_n, na.rm = FALSE)
][,
wave_n := fifelse(is.na(wave_n), 0, wave_n)
]



dt_covid_deaths[
    ,
    .(prop_trend = count / max(count, na.rm = TRUE)),
] -> covid_trend_prop_by_year

dt_covid_deaths[, 
.( prop_trend = count / max(count, na.rm = TRUE)),
by = .(epi_year)
]-> covid_trend_prop_by_year



}else{
    covid_trend
    data.table(
        week_start = dt_covid_deaths$week_start,
        year = dt_covid_deaths$epi_year,
        covid_trend = covid_trend
    )[,
        .(prop_trend = covid_trend / max(covid_trend, na.rm = TRUE)),

    ] -> covid_trend_prop_by_year

}


covid_trend_prop <- covid_trend / max(covid_trend, na.rm = TRUE)
# and invert it 
covid_trend_prop <- covid_trend_prop_by_year$prop_trend

ggplot() + 
    geom_line(
        aes(
            x = dt_covid_deaths$week_start,
            y = covid_trend_prop
        )
    ) +
    theme_minimal()

covid_trend_prop
inverse_covid_trend_prop <- 1 - covid_trend_prop

inverse_covid_trend_prop
graphics.off()
ggplot() +
    geom_line(
        aes(
            x = dt_covid_deaths$week_start,
            y = inverse_covid_trend_prop
        )
    ) +
    theme_minimal()

# only apply the inverse factor from the start of COVID-19 period

# Manually decompose the trend traditionally 




nrow(national_j_codes)

c( 
    rep( 1, nrow( national_j_codes[week_start < as.Date("2020-04-01"), ] ) ),
    inverse_covid_trend_prop
)-> observed_inverse_covid_trend

observed_inverse_covid_trend

c(observed_inverse_covid_trend, 
rep( 1, nrow(national_j_codes) - length(observed_inverse_covid_trend)
)
)-> observed_inverse_covid_trend_full 

national_j_codes$covid_trend <- observed_inverse_covid_trend_full

national_j_codes_scenario_3 <- copy(national_j_codes)[
    ,
    `:=`(
        inverse_covid_expected = expected * covid_trend
    )
][
    ,
    `:=`(
            excess = observed - inverse_covid_expected,
        scenario = "Scenario 3"
        )]
        # - Assumes J coded deaths were competitive with COVID-19 deaths"

# plot the observed and excess to see the difference
national_j_codes_scenario_3%>%
    ggplot() + 
    geom_line( 
        aes( 
            x = week_start, 
            y = observed, 
            color = "blue"
        )
    )+ 
    geom_line( 
        aes( 
            x = week_start, 
            y = inverse_covid_expected, 
            color = "red"
        )
    )+ theme_minimal()


national_j_codes_scenario_3%>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = excess
        )
    ) +
    theme_minimal()
    
national_j_codes_scenario_3[, sum(excess[epi_year >= 2020 & excess > 0], na.rm = TRUE)]


# plot these three scenarios together 
national_j_codes_scenario_1[, c("week_start", "excess", "scenario")]

national_j_codes_scenario_2[, c("week_start", "excess", "scenario")]

national_j_codes_scenario_3[, c("week_start", "excess", "scenario")]

data.table( 
    scenario = c("Scenario 1", "Scenario 2", "Scenario 3"),
    total_excess_deaths = round(c(
        national_j_codes_scenario_1[, sum(excess[epi_year >= 2020 & excess > 0], na.rm = TRUE)],
        national_j_codes_scenario_2[, sum(excess[epi_year >= 2020 & excess > 0], na.rm = TRUE)],
        national_j_codes_scenario_3[, sum(excess[epi_year >= 2020 & excess > 0], na.rm = TRUE)] 
        ), 0)
)-> total_excess_deaths_summary



total_excess_deaths_summary
rbind( 
    national_j_codes_scenario_1[, .(week_start, excess, scenario)],
    national_j_codes_scenario_2[, .(week_start, excess, scenario)],
    national_j_codes_scenario_3[, .(week_start, excess, scenario)]
) %>%
    ggplot(aes(x = week_start, y = excess, color = scenario)) +
    geom_line(size = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    geom_text(
        data = total_excess_deaths_summary,
        aes(
            x = as.Date("2023-01-01"),
            y = 500,
            label = paste0("\"Covid deaths\" in \nJ coded deaths: \n", total_excess_deaths)
        ),
        inherit.aes = FALSE,
        hjust = 0,
        size = 3.5,
        color = "gray20"
    ) +
    scale_color_brewer(palette = "Set2") +
    scale_x_date(

        limits = c(as.Date("2018-01-01"), as.Date("2023-01-01")),
        date_breaks = "1 year",
        date_labels = "%Y", 
        expand = c(0,0,0.25,0)
    ) +
    facet_wrap(~scenario, ncol = 1, scales = "free_y") +
    labs(
      #  title = "Excess deaths in J-coded causes under alternative scenarios",
        x = "Week start",
        y = "Excess deaths within J-coded causes",
        color = "Scenario"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5)
    )-> fig_1

save( fig_1,total_excess_deaths_summary,  file = "j_code/LGH_ExcessDeaths_JCodes_Scenarios_Fig.rda")
ggsave("figures/LGH_ExcessDeaths_JCodes_Scenarios.png", fig_1, width = 8, height = 6)

















predict_glm.nb_with_ci <- function(model, newdata, type = "link", level = 0.95) {
    # Get predictions on the link scale
    link_pred <- predict(model, newdata = newdata, type = type, se.fit = TRUE)

    # Calculate the critical value for the normal distribution
    crit_value <- qnorm((1 + level) / 2)

    # Calculate the confidence intervals on the link scale
    link_lower <- link_pred$fit - crit_value * link_pred$se.fit
    link_upper <- link_pred$fit + crit_value * link_pred$se.fit

    # Transform back to response scale
    response_pred <- exp(link_pred$fit)
    response_lower <- exp(link_lower)
    response_upper <- exp(link_upper)

    # Combine into a data.table
    result <- data.table(
        .row_id = 1:nrow(newdata),
        week_start = newdata$week_start,
        fit = response_pred,
        lower = response_lower,
        upper = response_upper
    )

    return(result)
}

# use function to predict on province level data
# model_simple
predict_glm.nb_with_ci(model_simple, newdata = dt_for_model_unfiltered_no_epi53) -> simple_model_preds

simple_model_preds
    
dt_for_model_unfiltered_no_epi53$.row_id <- 1:nrow(dt_for_model_unfiltered_no_epi53)

dt_for_model_unfiltered_no_epi53[simple_model_preds, on = .(.row_id)] -> dt_for_model_unfiltered_no_epi53

dt_for_model_unfiltered_no_epi53



