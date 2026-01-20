# Find the ICC by district or province 

# Purpose 
## The purpose of this is to infrom the ICC of by dsitrict or province in grabage coding (also by place of death) to assit in a 
## the sample sie calculation for the eMCCD pilot 

# province
# place of death
# district
# deathype 

library(arrow)
library(NMCleaner)
library(data.table)
library(janitor)
library(glmmTMB)
library(data.table)
library(glmmTMB)
library(purrr)
library(gtsummary)
library(lme4)
library(performance)
conflicts_prefer(lubridate::year)

dt <- read_feather("LGH_MasterFile_preCollapsed2022.feather") %>% as.data.table()

dt%>%names()


# tabulate the garbage labels among natural hospial deaths

dt$DeathInst%>%unique()


# keep only in hospial DOA and emergency causla death inst
dt[
    DeathInst %in% c(1,2,3), 
]-> dt_hospital


# Need to model the ICC on the proportion of garbage codes
dt_hospital$garbage_flag_label %>% tabyl()


# 0/1 binary outcome
dt_hospital[ , 
    garbage_bin := dplyr::if_else(garbage_flag_label == "Not garbage", FALSE, TRUE)
][
    epi_year %in% 2019:2020
, ]-> dt_hospital_19_20

# check vars and names 
dt_hospital_19_20$deathdistrictname
dt_hospital_19_20$epi_year%>%unique
dt_hospital_19_20$garbage_flag_label%>%unique
dt_hospital_19_20$garbage_bin%>%tabyl(., addNA = TRUE)
dt_hospital_19_20$agegroup%>%tabyl()
dt_hospital_19_20$DeathProvince%>%tabyl()

# confirm filtering ability 
dt_hospital_19_20[ DeathProvince == "Western Cape",]
dt_hospital_19_20[deathdistrictname %in% "Alfred Nzo", ]



# to save computation time, amke a df of the unique district names 
tibble::tibble( 
    deathdistrictname = dt_hospital_19_20$deathdistrictname%>%unique()
) %>%na.omit() %>%
    NMCleaner::mutate_district_name( ., district_variable = "deathdistrictname" ) -> 
dt_district_names_map

dt_district_names_map


dt_hospital_19_20[dt_district_names_map, on  = .( deathdistrictname ), 
    deathdistrictname := district_standard
][, 
    hiv_pos := fifelse( LGH_Cause %in% c("B33"), TRUE, FALSE )] -> dt_hospital_19_20

dt_hospital_19_20

dt_hospital_19_20$agegroup%>%levels()
dt_hospital_19_20$agegroup %>% unique()


# Look at proprtion of garbage codes across provinces by age groups dt_hospital


dt_hospital_19_20$deathdistrictname%>%na.omit() %>%unique()%>%sort()%>%
    map(
        ~
        dt_hospital_19_20[deathdistrictname %in% .x, ]%>%
            dplyr::select( 
                garbage_bin, 
                hiv_pos,
                agegroup
            )%>%
            tbl_summary( 
                by = agegroup, 
                percent = "column", 
                label = list( garbage_bin = "Garbage Code", hiv_pos = "B33 code(HIV)"), 
                statistic = all_categorical() ~ "{n} ({p}%)"
            )%>%
            modify_header( 
                all_stat_cols() ~ "**{level}**"
            )%>%
            bstfun::add_variable_grouping(
                !!sym(.x) := c("garbage_bin", "hiv_pos")
            )
    )%>%
tbl_stack() -> garbage_by_district_agegroup_table

garbage_by_district_agegroup_table


dt_hospital_19_20[agegroup %in% c("5-39", "40-59", "60-69"), ]-> dt_hospital_19_20
# refactor agegroup
dt_hospital_19_20$agegroup <- factor( 
    dt_hospital_19_20$agegroup,
    levels = c(  "5-39", "40-59",  "60-69")
)

# odds of being garbage code with clustering at the district and provinial level as a proxy for deisgn effect 
glmer( 
    garbage_bin ~ agegroup + epi_year + sex + (1 | deathdistrictname) + (1 | DeathProvince),
    data = dt_hospital_19_20,
    family = binomial(link = "logit")
)-> glmer_model

glmer_model%>%summary()
icc(glmer_model)

dt_hospital_19_20$agegroup%>%tabyl()

glmer_model_hierarchical <- glmer( 
    garbage_bin ~ agegroup+ epi_year + sex + (1 | DeathProvince/deathdistrictname),
    data = dt_hospital_19_20,
    family = binomial(link = "logit")
)

glmer_model_hierarchical%>%summary()
icc(glmer_model_hierarchical)-> icc_hierarchical

icc_hierarchical

tbl_regression( 
    glmer_model,
    exponentiate = TRUE
)%>%
add_glance_table()

tbl_regression( 
    glmer_model_hierarchical,
    exponentiate = TRUE
)%>%
add_glance_table()-> 
garbage_glmer_table

# We want to see the sample size we should get in order to measure an increase in HIV related deaths from ~ 5% to ~25%

# Take WC as an example, we want to findage groups with the highest HIV prevalence and ensure that the total sample we have will have enough of a sample of HIV to measure imrpovements of ~ 10%. 

# Report the prevalence of HIV in WC 
# look at the prevalence of HIV deaths 
dt_hospital_19_20[ 
    DeathProvince == "Western Cape", ]-> 
dt_hospital_19_20

dt_hospital_19_20$hiv_pos%>%tabyl(., addNA = TRUE)

dt_hospital_19_20%>%
    select( 
        hiv_pos, 
        garbage_bin,
        agegroup, 
        epi_year
    )%>%
tbl_strata(
    data = ., 
    strata = agegroup, 
    .tbl_fun = 
    ~ .x %>%
    tbl_summary( 
        by = epi_year, 
        #percent = "row", 
       # label = list( hiv_pos = "Western Cape")
    )%>%
    add_p()
)-> hiv_by_agegroup_table


# SO based on this, if we had a sample of 1000 people 

# We want to ensure that, we have power to estimate a change in garbage codes from 20% -> 10%
# An increase in HIV from 5% to 25%
# total deaths in WC 15-49 year olds 

dt_hospital[ DeathProvince == "Western Cape" & age %in% 15:49 & epi_year %in% 2022, ]%>%nrow()-> WC_deaths
# tembisa model AIDS deaths 2022
# Download ProvOutput4.6final2.xlsx
# Sheet WC filter to find "AIDS deaths in 15-49 year olds"
tembisa_aids_deaths <-  1697

modelled_prov_aids_deaths <- tembisa_aids_deaths/WC_deaths
modelled_prov_aids_deaths







## ---- setup ----
set.seed(123)   # for reproducibility

#libs
    library(dplyr)
library(gtsummary)
library(lme4)
library(stats)
library(performance)

# Generic function to simulate power for a two-proportion comparison
simulate_power_two_proportions <- function(p1, p2,
                                           n_per_group,
                                           nsim      = 10000,
                                           alpha     = 0.05,
                                           two_sided = FALSE) {
                                            
  # Basic sanity checks
  if (p1 <= 0 || p1 >= 1 || p2 <= 0 || p2 >= 1) {
    stop("p1 and p2 must both be strictly between 0 and 1.")
  }

  n_vec <- n_per_group

  out <- data.frame(
    n_per_group = n_vec,
    power       = NA_real_
  )

  for (i in seq_along(n_vec)) {
    n <- n_vec[i]

    # Precompute alternative hypothesis direction once per n
    alt <- if (two_sided) {
      "two.sided"
    } else if (p2 > p1) {
      "greater"  # H1: p2 > p1
    } else {
      "less"     # H1: p2 < p1
    }

    # Simulate counts under the assumed true proportions
    x1 <- rbinom(nsim, size = n, prob = p1)
    x2 <- rbinom(nsim, size = n, prob = p2)

    rbinom(100, 100, 0.2)
    rbinom(100, 100, 0.1)

    prop.test(
        x = c(rbinom(1, 100, 0.2),rbinom(1, 100, 0.1)),
        n = c(100, 100),
        #alternative = alt
    )-> test



    # Store p-values for each simulated dataset
    pvals <- numeric(nsim)

    for (s in seq_len(nsim)) {
      test <- prop.test(
        x = c(x1[s], x2[s]),
        n = c(n, n),
        alternative = alt
      )
      pvals[s] <- test$p.value
    }

    # Estimated power = proportion of p-values below alpha
    out$power[i] <- mean(pvals < alpha)
  }

  out
}



## ---- parameters ----

# Effect 1: Garbage codes 20% -> 10%
p1_garbage <- 0.15
p2_garbage <- 0.1

# Effect 2: HIV 5% -> 25%
p1_hiv <- 0.05
p2_hiv <- 0.25

# Range of sample sizes per time point / group to explore
n_grid <- seq(1, 1000, by = 25)

## ---- run simulations ----

# Power curve for garbage codes
power_garbage <- simulate_power_two_proportions(
  p1 = p1_garbage,
  p2 = p2_garbage,
  n_per_group = n_grid,
  nsim  = 1000,     # increase if you want a smoother estimate
  alpha = 0.05, 
  two_sided = TRUE
)

power_garbage
# Power curve for HIV proportion

power_hiv <- simulate_power_two_proportions(
  p1 = p1_hiv,
  p2 = p2_hiv,
  n_per_group = n_grid,
  nsim  = 100,
  alpha = 0.05
)

power_garbage
power_hiv

## ---- find minimum n for target power ----

target_power <- 0.80

min_n_garbage <- power_garbage$n_per_group[which(power_garbage$power >= target_power)[1]]
min_n_hiv     <- power_hiv$n_per_group[which(power_hiv$power     >= target_power)[1]]

min_n_garbage
min_n_hiv

# If you need a single n that gives ≥ target power for BOTH outcomes:
n_required_both <- max(min_n_garbage, min_n_hiv)
n_required_both

## ---- (optional) quick plot if you like ggplot2 ----
# install.packages("ggplot2") if needed

library(ggplot2)

power_garbage$endpoint <- "Garbage codes 15% → 10%"
power_hiv$endpoint     <- "HIV 5% → 25%"

power_df <- rbind(power_garbage, power_hiv)

ggplot(power_df, aes(x = n_per_group, y = power, linetype = endpoint)) +
  geom_line() +
  geom_hline(yintercept = target_power, linetype = 2) +
  labs(
    x = "Sample size per group/time",
    y = "Simulated power",
    linetype = "Endpoint",
    title = "Power curves for change in garbage codes and HIV"
  ) +
  theme_minimal()

## Include ICC 
# FOr ICC you need to account for joint probabilities 


# =========================
# Joint probability builders
# =========================
  
build_joint_probs_v1 <- function(p1, p2, rho, p01_floor = 0.01) {
  ###########
  # VERSION 1 # hardcodes a small value to p01 to make it feasible
  ###########
  #p1 <- 0.15
 # p2 <- 0.1
 # rho <- 0.9
 # p01 <- 0.01

  # Standard deviations of the Bernoulli variables
  sd1 <- sqrt(p1 * (1 - p1))
  sd2 <- sqrt(p2 * (1 - p2))

  # Desired covariance implied by the correlation rho
  cov12 <- rho * sd1 * sd2

  # Joint probability of both events being 1
  p11 <- p1 * p2 + cov12

  # Joint probabilities for the other cells
  p10 <- max(p1 - p11, 0 )            # Y1 = 1, Y2 = 0
  #p01 <- max(p2 - p11, 0 )           # Y1 = 0, Y2 = 1 # this is from not garbage to garbage, which is unliley 
  p01 <- max(p01_floor, 0) # set to a small value to make it feasible that if paper is well defined, so will electronic
  p00 <- max(1 - p11 - p10 - p01, 0)



  probs <- c(p00 = p00, p01 = p01, p10 = p10, p11 = p11)
  
  # Check feasibility: we cannot have negative probabilities
  if (any(probs < -1e-20)) {
    print( probs )
    print( paste("rho on failure is", rho) )
    stop("The requested rho_within is not feasible for the supplied p1 and p2.")
  }

  # Numerical protection: truncate any tiny negatives to zero,
  # and renormalise so probabilities sum exactly to 1.
  probs <- pmax(probs, 0)
  probs / sum(probs)
}

p1 <- 0.15 # proportion of garbage codes at T1
p2 <- 0.10 # proportion of garbage codes at T2

rhos <- c(seq(0, 0.9, by = 0.1))
rhos
build_joint_probs_v1(0.150, 0.10, 0.5, p01 = 0.1)

# based on the rho, how much can we expect the probabiloity of going from garbage to not garbage to change?
probabilities_list <- lapply(rhos, function(rho) build_joint_probs_v1(p1, p2, rho, p01_floor = 0.1)) 
probabilities_list

data.frame(
  rho = rhos,
  do.call(rbind, probabilities_list)
) %>%
  tidyr::pivot_longer(
    cols = c(p00, p01, p10, p11),
    names_to = "outcome",
    values_to = "probability"
  ) %>%
  ggplot() +
  geom_line(
    aes(x = rho, y = probability, color = outcome)
  ) +
  scale_color_manual(
    values = c("p00" = "blue", "p01" = "red", "p10" = "green", "p11" = "purple"),
    labels = c("p00" = "Neither garbage", "p01" = "Well defined to Garbage", "p10" = "Garbage to Well defined", "p11" = "Both garbage")
  ) +
  labs(
    x = "Within-certificate correlation (rho)",
    y = "Probability",
    color = "Outcome",
    title = "Probabilities of outcomes for varying within-certificate correlation (rho)"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1)
  ) +
  theme_minimal() 


# we look at proporiton of ill-defined codes at time 1 and time 2
# therefore p01 is moving from well-defined to ill-defined #unlikely 
# and p10 is moving from ill-defined to well-defined ( what we hope for. )


build_joint_probs_v2 <- function(
  p1, # proportion of garbage codes at T1 (paper)
  p2, # proportion of garbage codes at T2 (electronic)
  rho, 
    p01_floor = 0.01, # P that good paper becomes garbage electronic
    p10_floor = 0.1, # P that garbage paper becomes good electronic ( hoping for this)
  allow_worsening = FALSE) {

  stopifnot(p1 > 0, p1 < 1, p2 > 0, p2 < 1)
  if (!allow_worsening && p2 > p1 + 1e-9) {
    stop("allow_worsening is FALSE but p2 > p1; adjust inputs or set allow_worsening = TRUE")
  }

  sd1  <- sqrt(p1 * (1 - p1))
  sd2  <- sqrt(p2 * (1 - p2))
  cov12 <- rho * sd1 * sd2

  # raw solution implied by rho
  p11_raw <- p1 * p2 + cov12

  # Frechet bounds to ensure feasibility
  p11_min <- max(0, p1 + p2 - 1)
  p11_max <- min(p1, p2)
  p11 <- min(max(p11_raw, p11_min), p11_max)

  p10 <- p1 - p11
  p01 <- p2 - p11
  p00 <- 1 - p11 - p10 - p01

  probs <- c(p00 = p00, p01 = p01, p10 = p10, p11 = p11)

  if (any(probs < -1e-10)) {
    stop("rho_within not feasible for given p1, p2.")
  }

  probs <- pmax(probs, 0)
  probs <- probs / sum(probs)

  if (p01_floor > 0 && probs["p01"] < p01_floor - 1e-9) {
    print("p01_floor exceeds feasible probability for supplied p1/p2/rho.")
  } else if (p01_floor > 0 && probs["p01"] > p01_floor) {
    probs["p01"] <- p01_floor
    probs <- probs / sum(probs)
  }

  if (p10_floor > 0 && probs["p10"] < p10_floor - 1e-9) {
    print("p10_floor exceeds feasible probability for supplied p1/p2/rho.")
  }

  probs
}

# so we would assume that when rho is high, we get few transitions from well-defined to garbage coding (p01) and more transitions from garbage to well-defined coding (p10)
build_joint_probs_v2(0.20, 0.10, 0.5, p01_floor = 0.001, p10_floor = 0.1, allow_worsening = TRUE)

build_joint_probs_v2(0.20, 0.10, 0.5, p01_floor = 0.001, p10_floor = 0.001, allow_worsening = TRUE)

build_joint_probs_v2(0.20, 0.10, 0.5, p01_floor = 0.001, p10_floor = 0.001, allow_worsening = FALSE)

build_joint_probs_v2(0.20, 0.10, 0.9, p01_floor = 0.001, p10_floor = 0.001, allow_worsening = FALSE)


build_joint_probs_v2(0.20, 0.10, 0.5, p01_floor = 0.1, p10_floor = 0.01, allow_worsening = FALSE)



build_joint_probs_conditional <- function(p1, p2, q01 = 0.01) {
  # p1  = P(Y1 = 1) at baseline (paper)
  # p2  = P(Y2 = 1) at follow-up (electronic)
  # q01 = P(Y2 = 1 | Y1 = 0): worsening probability (good -> bad), small

  if (p1 <= 0 || p1 >= 1 || p2 <= 0 || p2 >= 1) {
    stop("p1 and p2 must both be strictly between 0 and 1.")
  }

  # Solve for q10 = P(Y2 = 0 | Y1 = 1): improvement probability
  q10 <- (p1 - p2 + (1 - p1) * q01) / p1

  if (q10 < 0 || q10 > 1) {
    stop("Requested p1, p2, q01 combination is not feasible (q10 not in [0,1]).")
  }

  p11 <- p1 * (1 - q10)
  p10 <- p1 * q10
  p01 <- max((1 - p1) * q01, 0)
  p00 <- 1 - p11 - p10 - p01


  
  probs <- c(p00 = p00, p01 = p01, p10 = p10, p11 = p11)

  if (any(probs < -1e-12)) {
    stop("Numerical issue: some joint probabilities are negative.")
  }

  probs <- pmax(probs, 0)
  probs / sum(probs)
}

build_joint_probs_conditional(0.20, 0.10, q01 = 0.01)



p1 <- 0.15 # proportion of garbage codes at T1 
p2 <- 0.10 # proportion of garbage codes at T2 

rhos <- c(seq(0, 0.9, by = 0.1))

#based on the rho, how much can we expect the probabiloity of going from garbage to not garbage to change? 
probabilities_list <- lapply(rhos, function(rho) build_joint_probs_v2(p1, p2, rho, p01_floor = 0.1, p10_floor = 0.1, allow_worsening = TRUE))
probabilities_list

#probabilities_list<- lapply(rhos, function(rho) build_joint_probs_conditional(p1, p2, q01 = 0.01))
#probabilities_list

data.frame( 
    rho = rhos,
    do.call(rbind, probabilities_list)
)%>%tidyr::pivot_longer( 
    cols = c( p00, p01, p10, p11),
    names_to = "outcome",
    values_to = "probability"
)%>%
ggplot() + 
geom_line( 
    aes( x = rho, y = probability, color = outcome)
)+ 
scale_color_manual( 
    values = c( "p00" = "blue", "p01" = "red", "p10" = "green", "p11" = "purple"),
    labels = c( "p00" = "Neither garbage", "p01" = "Well defined to Garbage", "p10" = "Garbage to Well defined", "p11" = "Both garbage")
)+
labs(
    x = "Within-certificate correlation (rho)",
    y = "Probability",
    color = "Outcome",
    title = "Probabilities of outcomes for varying within-certificate correlation (rho)"
)+ 
scale_y_continuous( 
    limits = c(0, 1), 
    breaks = seq(0, 1, by = 0.1)
)+
theme_minimal() -> joint_probability_plot

joint_probability_plot
# this plot shows that as rho increases, the probability of both outcomes being garbage (p11) increases, while the probabilities of discordant outcomes (p01 and p10) decrease. This reflects higher within-certificate correlation leading to more consistent classification between paper and electronic certificates.



# -------------------------------------------------------------------
# simulate_power_two_proportions()
#
# Monte Carlo power simulation for comparing two proportions, with
# optional paired/repeated-measures structure (e.g. paper vs electronic form
# on the same death certificate).
#
# Arguments:
#   p1          - Proportion in condition 1 (e.g. paper ill-defined)
#   p2          - Proportion in condition 2 (e.g. electronic ill-defined)
#   n_per_group - Sample size per group / per condition.
#                 Can be a single number (e.g. 300) or a numeric vector
#                 (e.g. seq(50, 500, by = 50)) to get a power curve.
#   nsim        - Number of Monte Carlo simulations per sample size.
#   alpha       - Significance level for the hypothesis test.
#   two_sided   - If TRUE, use a two-sided test.
#                 If FALSE, use a one-sided test that looks in the
#                 direction of p2 > p1 or p2 < p1.
#   paired      - If TRUE, assume paired data (same unit measured twice).
#                 If FALSE, assume independent groups.
#   rho_within  - ONLY used when paired = TRUE.
#                 Correlation between the two binary measures (paper vs electronic)
#                 on the same certificate: 0 <= rho_within < 1.
#                 This affects the joint distribution of the paired outcomes.
#                  A high rho means that the paired observations are more similar.
#
# Returns:
#   A data.frame with:
#     n_per_group - The sample size per condition used in the simulation.
#     power       - Estimated power (proportion of simulations with p-value < alpha).
#
# -------------------------------------------------------------------
simulate_power_two_proportions_icc <- function(p1, p2,
                                               n_per_group,
                                               nsim        = 1000,
                                               alpha       = 0.05,
                                               two_sided   = FALSE,
                                               paired      = TRUE,
                                               rho_within  = 0,
                                               joint_version = c("v1", "v2"),
                                               p01_floor   = 0.01, # recommended small value to prevent worsening
                                               p10_floor   = 0.1, # recommended value to encourage improvement
                                               allow_worsening = TRUE) {
  # ---------------------------
  # Basic input checks
  # ---------------------------
  if (p1 <= 0 || p1 >= 1 || p2 <= 0 || p2 >= 1) {
    stop("p1 and p2 must both be strictly between 0 and 1.")
  }

  if (paired && (rho_within < 0 || rho_within >= 1)) {
    stop("When paired = TRUE, please supply 0 <= rho_within < 1.")
  }

  joint_version <- match.arg(joint_version)

  # Ensure we always treat sample size as a vector
  n_vec <- n_per_group

  # Container for results across different n values
  out <- data.frame(
    n_per_group = n_vec,
    power       = NA_real_
  )

  # ---------------------------
  # Precompute joint probabilities if paired
  # ---------------------------
  if (paired) {
    if (joint_version == "v1") {
      paired_probs <- build_joint_probs_v1(p1, p2, rho_within)
    } else if (joint_version == "v2") {
      paired_probs <- build_joint_probs_v2(p1, p2, rho_within,
                                           p01_floor = p01_floor,
                                           p10_floor = p10_floor, 
                                           allow_worsening = allow_worsening)
    }
  } else {
    paired_probs <- NULL
  }

  # -------------------------------------------------------------------
  # Main simulation loop across all values of n
  # -------------------------------------------------------------------
  for (i in seq_along(n_vec)) {
    n  <- n_vec[i]
    pvals <- numeric(nsim)  # store p-values for this sample size

    # ---------------------------------------------------------------
    # Inner loop: simulate nsim datasets of size n
    # ---------------------------------------------------------------
    for (s in seq_len(nsim)) {

      if (!paired) {
        # -----------------------------------------------------------
        # INDEPENDENT-GROUPS CASE
        # -----------------------------------------------------------
        x1 <- rbinom(1, size = n, prob = p1)
        x2 <- rbinom(1, size = n, prob = p2)

        test <- prop.test(
          x = c(x1, x2),
          n = c(n, n),
          alternative = if (two_sided) {
            "two.sided"
          } else if (p1 > p2) {
            "greater"   # H1: p1 > p2
          } else {
            "less"      # H1: p1 < p2
          }
        )

        pvals[s] <- test$p.value

      } else {
        # -----------------------------------------------------------
        # PAIRED / REPEATED-MEASURES CASE (McNemar)
        # -----------------------------------------------------------

        # Simulate counts in the 2x2 table via a multinomial
        draws <- rmultinom(1, size = n, prob = paired_probs)

        n00 <- draws["p00", ]  # Y1=0, Y2=0
        n01 <- draws["p01", ]  # Y1=0, Y2=1
        n10 <- draws["p10", ]  # Y1=1, Y2=0
        n11 <- draws["p11", ]  # Y1=1, Y2=1

        discordant <- n01 + n10

        if (discordant == 0) {
          pvals[s] <- 1
        } else {
          if (two_sided) {
            tail_prob <- pbinom(min(n01, n10), discordant, 0.5)
            pvals[s] <- min(1, 2 * tail_prob)
          } else if (p2 > p1) {
            # H1: p2 > p1 → more 0->1 transitions (n01)
            pvals[s] <- pbinom(n01 - 1, discordant, 0.5, lower.tail = FALSE)
          } else {
            # H1: p2 < p1 → more 1->0 transitions (n10)
            pvals[s] <- pbinom(n10 - 1, discordant, 0.5, lower.tail = FALSE)
          }
        }
      } # end paired vs unpaired

    } # end simulation loop

    out$power[i] <- mean(pvals < alpha)
  } # end n loop

  out
}


# interpreting Rho 


# print the probabilitys of the various outcomes in a ggplot for varying rho 




set.seed(123)

p1_ill <- 0.2 # paper ill-defined
p2_ill <- 0.1  # electronic ill-defined

n_grid <- c(seq(10, 100, by = 10), seq(125, 200, by = 25), seq(250, 500, by = 50))

power_ill_defined <- simulate_power_two_proportions_icc(
  p1          = p1_ill,
  p2          = p2_ill,
  n_per_group = n_grid,
  nsim        = 5000,
  alpha       = 0.05,
  two_sided   = TRUE,    # directional: electronic < paper
  paired      = TRUE,
  p01_floor   = 0.1,
  p10_floor   = 0.1  ,
  rho_within  = 0.2 ,      # moderate within-certificate correlation, 
  joint_version = "v2"
)

power_ill_defined
# Power curve for HIV proportion with ICC

# For HIV, the logic is flipped for the joint probs builder so it is easier to change the coding here. 
#   we assume an imporvement is technically "a drop" in HIV positive classification from paper to electronic - 
#   We know in practice that it is the other way round. 
p1_hiv <- 0.2  # paper HIV positive
p2_hiv <- 0.05  # electronic HIV positive

power_hiv <- simulate_power_two_proportions_icc(
  p1          = p1_hiv,
  p2          = p2_hiv,
  n_per_group = n_grid,
  nsim        = 5000,
  alpha       = 0.05,
  two_sided   = TRUE,    # directional: electronic > paper
  paired      = TRUE,
  p01_floor   = 0.1,
  p10_floor   = 0.01,
  rho_within  = 0.2,      # moderate within-certificate correlation
  joint_version = "v2"
)

power_hiv



target_power <- 0.80

min_n_garbage <- power_ill_defined$n_per_group[which(power_ill_defined$power >= target_power)[1]]
min_n_hiv     <- power_hiv$n_per_group[which(power_hiv$power     >= target_power)[1]]

min_n_garbage
min_n_hiv

# If you need a single n that gives ≥ target power for BOTH outcomes:
n_required_both <- max(min_n_garbage, min_n_hiv)
n_required_both

## ---- (optional) quick plot if you like ggplot2 ----
# install.packages("ggplot2") if needed

library(ggplot2)


power_ill_defined$endpoint <- paste0("Garbage codes ", p1_ill*100, "% → ", p2_ill*100, "%")
power_hiv$endpoint     <- paste0("HIV ", p1_hiv*100, "% → ", p2_hiv*100, "%")

power_df <- rbind(power_ill_defined, power_hiv) 
ggplot(power_df, aes(x = n_per_group, y = power, linetype = endpoint)) +
  geom_line() +
  geom_hline(yintercept = target_power, linetype = 2) +
  labs(
    x = "Sample size per group/time",
    y = "Simulated power",
    linetype = "Endpoint",
    title = "Power curves for change in garbage codes and HIV"
  ) +
  scale_y_continuous( 
    limits = c(0,1),
    breaks = seq(0,1, by = 0.1)
  )+
  theme_minimal()-> 
joint_power_curve_plot

joint_power_curve_plot


# function to show diff effect sizes 

plot_power_sample_size <- function(
    p1_g1 = 0.2,
    p2_g1 = 0.1,
    p1_g2 = 0.05,
    p2_g2 = 0.25,
    rho_1 = 0.5,
    rho_2 = 0.5,
    n_grid = c(
      seq(10, 100, by = 10),
      seq(125, 200, by = 25),
      seq(250, 1000, by = 50)
    ),
    nsim = 5000,
    alpha = 0.05,
    target_power = 0.80,
    p01_floor = 0.01,
    p10_floor = 0.1,
    joint_version = "v2",
    two_sided = TRUE,
    paired = TRUE, 
    allow_worsening = TRUE
) {
  # --- Run simulations for endpoint 1 ---
  power_1 <- simulate_power_two_proportions_icc(
    p1 = p1_g1,
    p2 = p2_g1,
    n_per_group = n_grid,
    nsim = nsim,
    alpha = alpha,
    two_sided = two_sided,
    paired = paired,
    p01_floor = p01_floor,
    p10_floor = p10_floor,
    rho_within = rho_1,
    joint_version = joint_version, 
    allow_worsening = allow_worsening
  )

  # --- Run simulations for endpoint 2 ---
  power_2 <- simulate_power_two_proportions_icc(
    p1 = p1_g2,
    p2 = p2_g2,
    n_per_group = n_grid,
    nsim = nsim,
    alpha = alpha,
    two_sided = two_sided,
    paired = paired,
    p01_floor = p01_floor,
    p10_floor = p10_floor,
    rho_within = rho_2,
    joint_version = joint_version, 
    allow_worsening = allow_worsening
  )

  # Label endpoints with effect sizes
  power_1$endpoint <- paste0("Endpoint 1: ", p1_g1 * 100, "% → ", p2_g1 * 100, "%")
  power_2$endpoint <- paste0("Endpoint 2 (HIV proxy): ", p1_g2 * 100, "% → ", p2_g2 * 100, "%")

  # Combine into a single data frame
  power_df <- rbind(power_1, power_2)

  # --- Find minimum n for each endpoint ---
  min_n_1 <- power_1$n_per_group[which(power_1$power >= target_power)[1]]
  min_n_2 <- power_2$n_per_group[which(power_2$power >= target_power)[1]]

  # If target power not reached on the grid, these will be NA
  n_required_both <- max(min_n_1, min_n_2, na.rm = TRUE)
  if (is.infinite(n_required_both)) n_required_both <- NA

  # --- Plot ---
  library(ggplot2)

  p <- ggplot(power_df, aes(x = n_per_group, y = power, linetype = endpoint)) +
    geom_line() +
    geom_hline(yintercept = target_power, linetype = 2) +
    labs(
      x = "Sample size per group/time",
      y = "Simulated power",
      linetype = "Endpoint",
      title = "Power curves for two endpoints (paired with ICC)"
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.1)
    ) +
    theme_minimal()

  # Return everything in a list
  return(list(
    power_df        = power_df,
    plot            = p,
    min_n_endpoint1 = min_n_1,
    min_n_endpoint2 = min_n_2,
    n_required_both = n_required_both
  )
  )
}

# Example usage:
result <- plot_power_sample_size(
  p1_g1 = 0.15,
  p2_g1 = 0.1,
  p1_g2 = 0.25,
  p2_g2 = 0.05,
  rho_1 = 0.5,
  rho_2 = 0.5,
  n_grid = c(
    seq(10, 100, by = 10),
    seq(125, 200, by = 25),
    seq(250, 500, by = 50)
  ),
  nsim = 5000,
  alpha = 0.05,
  target_power = 0.80,
  p01_floor = 0.05,
  p10_floor = 0.1,
  joint_version = "v2",
  two_sided = TRUE,
  paired = TRUE, 
  allow_worsening = FALSE
)-> output_1

output_1$plot

# Map over multiple rho to create a full DF of results and colour code the RHO values with a light to dark colour scheme 

rho_values <- c(0.1, 0.3, 0.5, 0.7, 0.9)

power_plots_list <- lapply(rho_values, function(rho) {
  plot_power_sample_size(
    p1_g1 = 0.15,
    p2_g1 = 0.1,
    p1_g2 = 0.15,
    p2_g2 = 0.05,
    rho_1 = rho,
    rho_2 = rho,
    n_grid = c(
      seq(10, 100, by = 10),
      seq(125, 200, by = 25),
      seq(250, 750, by = 50)
    ),
    nsim = 1000,
    alpha = 0.05,
    target_power = 0.80,
    p01_floor = 0.1, # this is the probability of well defined paper becoming garbage 
    p10_floor = 0.1,
    joint_version = "v1",
    two_sided = TRUE,
    paired = TRUE, 
    allow_worsening = TRUE 
  )
}
)

power_plots_list

# extrat and rbdind the data frames and combine
power_dfs <- do.call(rbind, lapply(seq_along(power_plots_list), function(i) {
  df <- power_plots_list[[i]]$power_df
  df$rho <- rho_values[i]
  df
}))

power_dfs%>%
  ggplot(
    aes(x = n_per_group, y = power, linetype = endpoint, color = as.factor(rho))
  )+ 
  geom_line()+ 
  geom_hline( 
    yintercept = 0.80,
    linetype = 2
  )+
  labs(
    x = "Sample size per group/time",
    y = "Simulated power",
    linetype = "Endpoint",
    color = "Within-certificate correlation (rho)",
    title = "Power curves for two endpoints (paired with ICC)"
  )+
  scale_y_continuous( 
    limits = c(0,1),
    breaks = seq(0,1, by = 0.1)
  )+
  theme_minimal()-> joint_power_curve_rho_plot

  joint_power_curve_rho_plot




# saving tables and figures 
#system("mkdir -p icc_outputs")
save( 
  garbage_by_district_agegroup_table, 
  garbage_glmer_table, 
  hiv_by_agegroup_table,
  icc_hierarchical, 
  joint_probability_plot, 
  joint_power_curve_rho_plot,
  file = "icc_outputs/ICC_wrangling_outputs.RData"
)


# Look at a power calclation with simulated ICC cluster size and Design effects 

