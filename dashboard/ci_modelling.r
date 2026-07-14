# CI modelling 

# --- Packages ---
# install.packages(c("mgcv","dplyr","tidyr","purrr","lubridate","ggplot2"))
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(mgcv)

conflicts_prefer(lubridate::isoweek)

set.seed(123)

# --- Simulate South Africa-like weekly deaths 2015-2021 ---
# Calendar
weeks <- tibble(date = seq(as.Date("2015-01-05"), as.Date("2021-12-27"), by = "week")) |>
    mutate(
        year = isoyear(date),
        week = isoweek(date),
        t = row_number()
    )

# Simulate population (slow growth)
weeks <- weeks |> mutate(pop = 55e6 + (year - 2015) * 0.7e6)

# Seasonality (peaks in SA winter ~ weeks 25-35; adjust as needed)
season_fun <- function(wk) {
    # cyclic seasonality with peak mid-year
    0.12 * sin(2 * pi * (wk - 27) / 52) + 0.08 * cos(2 * pi * (wk - 27) / 52)
}

# Trend (gradual decline pre-2019 due to improvements)
trend_fun <- function(t) -0.00015 * (t - min(t))

# Base log-rate
weeks <- weeks |>
    mutate(
        seas = season_fun(week),
        trend = trend_fun(t),
        log_mu_base = -10.2 + seas + trend + log(pop) # scale to realistic counts
    )

# Create provinces (aggregate later), with province-specific random effects
provs <- tibble(
    province = c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC"),
    re = rnorm(9, 0, 0.10)
)

expand_grid(weeks, provs) |>
    mutate(
        covid = ifelse(date >= as.Date("2020-03-30"), 1, 0),
        # Cause-specific COVID "pressure" that raises all-cause or certain causes
        covid_bump = 0.10 * covid * (1 + 0.5 * sin(2 * pi * (week - 20) / 52)),
        # Province-level intensity variation
        log_mu = log_mu_base + re + covid_bump,
        # Over-dispersed counts via Negative Binomial
        mu = exp(log_mu),
        theta = 20, # dispersion (larger -> closer to Poisson)
        # rnbinom parametrized by size=theta, mu=mu
        y = rnbinom(n(), size = theta, mu = mu)
    ) |>
    group_by(date, year, week, t) |>
    summarise(pop = sum(pop), y = sum(y), .groups = "drop") -> df

# Partition: baseline <=2019; pandemic 2020-2021
train <- df |> filter(year <= 2019)
test <- df |> filter(year >= 2020)

# --- Fit baseline NB-GAM on 2015-2019 ---
# Cyclic cubic spline for week, smooth for time trend, offset log(pop)
m_nb <- gam(y ~ s(week, bs = "cc", k = 20) + s(t, k = 30) + offset(log(pop)),
    data = train,
    family = nb(link = "log")
) # negative binomial with log link

summary(m_nb)

# --- Counterfactual predictions for 2020-2021 with uncertainty ---
# We'll parametric-bootstrap from the NB-GAM:
# 1) Draw coefficient vectors from approx. multivariate normal around estimates
# 2) Predict mu_t for each week
# 3) Draw predictive counts ~ NegBin(mu_t, theta)
# mgcv stores theta in family$multiplicative parameter NB(), get via m_nb$family$getTheta(TRUE)
theta_hat <- m_nb$family$getTheta(TRUE)

# Prediction frame (counterfactual: same covariates, no COVID term in model)
newdat <- test |> mutate(week = week, t = t, pop = pop)

# Design matrices for parametric bootstrap
Xp <- predict(m_nb, newdat, type = "lpmatrix")
beta_hat <- coef(m_nb)
Vb <- vcov(m_nb) # Bayesian covariance of spline coefficients

# Number of draws
S <- 5000

# Simulate parameter draws and predictive counts
betas <- MASS::mvrnorm(S, mu = beta_hat, Sigma = Vb)

# helper: inverse link
inv_link <- m_nb$family$linkinv

mu_draws <- matrix(NA_real_, nrow = S, ncol = nrow(newdat))
for (s in 1:S) {
    eta <- as.numeric(Xp %*% betas[s, ]) + log(newdat$pop) # add offset manually
    mu_draws[s, ] <- inv_link(eta)
}

# Predictive counts under counterfactual per draw
y_cf_draws <- apply(mu_draws, 2, function(mus) rnbinom(S, size = theta_hat, mu = mus))

# Summaries for counterfactual
cf_q <- apply(y_cf_draws, 2, quantile, probs = c(0.025, 0.5, 0.975))
cf_mean <- colMeans(y_cf_draws)

cf_df <- newdat |>
    mutate(
        cf_lo = cf_q[1, ],
        cf_med = cf_q[2, ],
        cf_hi = cf_q[3, ],
        cf_mean = cf_mean
    )

# --- Excess deaths and intervals ---
# For each draw s, excess_s = observed - y_cf_draws[s,]
obs_vec <- newdat$y
excess_draws <- sweep(matrix(rep(obs_vec, each = S), nrow = S), 2, y_cf_draws, "-")

excess_q <- apply(excess_draws, 2, quantile, probs = c(0.025, 0.5, 0.975))
excess_df <- cf_df |>
    mutate(
        excess_lo = excess_q[1, ],
        excess_med = excess_q[2, ],
        excess_hi = excess_q[3, ]
    )

# Cumulative excess with uncertainty (sum across weeks for each draw)
cum_excess_draws <- rowSums(excess_draws)
cume_q <- quantile(cum_excess_draws, c(0.025, 0.5, 0.975))

cume_q
# -> report as: median [lo, hi]

# --- Intervals for observed counts (descriptive) ---
# 1) Exact Poisson (Garwood) 95% CI for observed counts (informative baseline display)
pois_ci <- function(y, conf.level = 0.95) {
    a <- (1 - conf.level) / 2
    lo <- ifelse(y == 0, 0, 0.5 * qchisq(a, 2 * y))
    hi <- 0.5 * qchisq(1 - a, 2 * (y + 1))
    c(lo, hi)
}
obs_ci <- t(vapply(obs_vec, pois_ci, numeric(2)))
colnames(obs_ci) <- c("obs_lo_pois", "obs_hi_pois")

obs_int_df <- newdat |> mutate(obs_lo_pois = obs_ci[, 1], obs_hi_pois = obs_ci[, 2])

# --- Bayesian "toy" credible interval for observed count via Poisson-Gamma ---
# (Illustration when treating a single-week Poisson with weak prior)
# Prior: lambda ~ Gamma(a,b), choose a=0.5, b=0.000001 to be weakly informative on SA scale
# Posterior predictive for Y_new is Negative Binomial with parameters derived below.
bayes_toy_pp <- function(y, a = 0.5, b = 1e-6) {
    a_post <- a + y
    b_post <- b + 1 # treat exposure=1 week (can scale if needed)
    # Predictive: Y_new ~ NegBin(size=a_post, prob=b_post/(b_post+1))
    size <- a_post
    prob <- b_post / (b_post + 1)
    q <- qnbinom(c(0.025, 0.5, 0.975), size = size, prob = prob)
    c(lo = q[1], med = q[2], hi = q[3])
}
toy_pp <- t(vapply(obs_vec, bayes_toy_pp, numeric(3)))
colnames(toy_pp) <- c("obs_lo_bayes_toy", "obs_med_bayes_toy", "obs_hi_bayes_toy")

obs_bayes_df <- newdat |>
    mutate(
        obs_lo_bayes_toy = toy_pp[, 1],
        obs_med_bayes_toy = toy_pp[, 2],
        obs_hi_bayes_toy = toy_pp[, 3]
    )

# --- Visualization examples ---
# Weekly counterfactual vs observed with 95% bands
plot_df <- excess_df |>
   dplyr::select(date, y_obs = y, cf_med, cf_lo, cf_hi)

ggplot(plot_df, aes(date)) +
    geom_ribbon(aes(ymin = cf_lo, ymax = cf_hi), alpha = 0.2) +
    geom_line(aes(y = cf_med), linetype = 2) +
    geom_line(aes(y = y_obs)) +
    labs(
        title = "Observed vs Counterfactual (NB-GAM predictive band)",
        y = "Weekly deaths", x = NULL
    )

# Weekly excess with 95% intervals
ex_plot <- excess_df |>
    transmute(date, excess_med, excess_lo, excess_hi)

ggplot(ex_plot, aes(date)) +
    geom_ribbon(aes(ymin = excess_lo, ymax = excess_hi), alpha = 0.2) +
    geom_line(aes(y = excess_med)) +
    geom_hline(yintercept = 0, linetype = 2) +
    labs(
        title = "Weekly excess deaths with uncertainty",
        y = "Excess deaths (obs - counterfactual)", x = NULL
    )


# install.packages("brms")
library(brms)

# Fit baseline only (<=2019) with weakly-informative priors
b_nb <- brm(
    bf(y ~ s(week, bs = "cc", k = 20) + s(t, k = 30) + offset(log(pop))),
    family = negbinomial(),
    data = train,
    prior = c(
        prior(normal(0, 5), class = "Intercept"),
        prior(exponential(1), class = "shape") # NB overdispersion prior
    ),
    chains = 4, cores = 4, iter = 4000, control = list(adapt_delta = 0.95)
)

# Posterior predictive counterfactual for 2020-2021
pp_cf <- posterior_predict(b_nb, newdata = test, ndraws = 4000) # draws x time

# Summaries
cf_lo <- apply(pp_cf, 2, quantile, 0.025)
cf_med <- apply(pp_cf, 2, median)
cf_hi <- apply(pp_cf, 2, quantile, 0.975)

# Excess draws and intervals
obs <- test$y
ex_draws <- sweep(pp_cf, 2, obs, FUN = function(pred, o) o - pred)
ex_lo <- apply(ex_draws, 2, quantile, 0.025)
ex_med <- apply(ex_draws, 2, median)
ex_hi <- apply(ex_draws, 2, quantile, 0.975)

# Cumulative excess credible interval
cum_q <- quantile(rowSums(ex_draws), c(0.025, 0.5, 0.975))
cum_q


# Explain this methodology 

methods <- paste(
    "We fitted a negative binomial generalized additive model (NB-GAM) to weekly all-cause mortality data from 2015 to 2019, 
    incorporating seasonal and temporal trends via smooth functions.
     Using this model, we generated counterfactual predictions for 2020-2021, 
     representing expected deaths in the absence of the COVID-19 pandemic. 
     We employed a parametric bootstrap approach to quantify uncertainty in these predictions, 
     drawing samples from the estimated coefficient distributions and simulating predictive counts. 
     Excess deaths were calculated as the difference between observed and counterfactual deaths, 
     with uncertainty intervals derived from the bootstrap samples. 
     Additionally, we explored a Bayesian framework using the 'brms' package to validate our 
     findings and provide alternative credible intervals for excess mortality estimates. The brms model 
     also utilized weakly informative priors to ensure robust estimation."
)