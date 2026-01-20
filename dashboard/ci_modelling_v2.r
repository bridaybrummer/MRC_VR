 # --- Packages ---
 library(dplyr)
 library(tidyr)
 library(purrr)
 library(lubridate)
 library(ggplot2)
 library(MASS) # glm.nb + mvrnorm

 conflicts_prefer(lubridate::isoweek)
 set.seed(123)

 # --- Simulate South Africa-like weekly deaths 2015-2021 ---
 weeks <- tibble(
     date = seq(as.Date("2015-01-05"), as.Date("2021-12-27"), by = "week")
 ) |>
     mutate(
         year = isoyear(date),
         week = isoweek(date),
         t    = row_number()
     )

 # Population with slow growth
 weeks <- weeks |>
     mutate(pop = 55e6 + (year - 2015) * 0.7e6)

 # Simple seasonal function (peak winter)
 season_fun <- function(wk) {
     0.12 * sin(2 * pi * (wk - 27) / 52) + 0.08 * cos(2 * pi * (wk - 27) / 52)
 }

 # Linear trend
 trend_fun <- function(t) -0.00015 * (t - min(t))

 weeks <- weeks |>
     mutate(
         seas        = season_fun(week),
         trend       = trend_fun(t),
         log_mu_base = -10.2 + seas + trend + log(pop)
     )

 # Provinces with random effects, then aggregate to national
 provs <- tibble(
     province = c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC"),
     re       = rnorm(9, 0, 0.10)
 )

 df <- expand_grid(weeks, provs) |>
     mutate(
         covid      = ifelse(date >= as.Date("2020-03-30"), 1, 0),
         covid_bump = 0.10 * covid * (1 + 0.5 * sin(2 * pi * (week - 20) / 52)),
         log_mu     = log_mu_base + re + covid_bump,
         mu         = exp(log_mu),
         theta      = 20,
         y          = rnbinom(n(), size = theta, mu = mu)
     ) |>
     group_by(date, year, week, t) |>
     summarise(pop = sum(pop), y = sum(y), .groups = "drop")

 train <- df |> filter(year <= 2019) # baseline
 test <- df |> filter(year >= 2020) # "pandemic" period

# Fit 
# Create a factor week with fixed levels
train <- train |>
    mutate(week_f = factor(week))

test <- test |>
    mutate(week_f = factor(week, levels = levels(train$week_f)))


train




# --- Baseline Negative Binomial GLM (no COVID term) ---
m_nb <- glm.nb(
    y ~ week_f + t + offset(log(pop)),
    data = train,
    link = log
)

summary(m_nb)
theta_hat <- m_nb$theta
theta_hat

newdat <- test # already has week_f, t, pop, y

pred_link <- predict(m_nb, newdata = newdat, type = "link", se.fit = TRUE)
eta_hat <- pred_link$fit
mu_hat <- exp(eta_hat) # predicted expected deaths

# Poisson variance approximation: Var(Y) = mu
sd_pois <- sqrt(mu_hat)

newdat <- newdat |>
    mutate(
        mu_hat          = mu_hat,
        ci_pois_lo      = pmax(mu_hat - 1.96 * sd_pois, 0),
        ci_pois_hi      = mu_hat + 1.96 * sd_pois
    )

    rate_hat <- mu_hat / newdat$pop # deaths per person per week
    se_log_rate <- sqrt(1 / mu_hat) # Poisson-based approximation

    log_rate_hat <- log(rate_hat)

    newdat <- newdat |>
        mutate(
            rate_hat       = rate_hat,
            log_rate_hat   = log_rate_hat,
            rate_ci_lo     = exp(log_rate_hat - 1.96 * se_log_rate),
            rate_ci_hi     = exp(log_rate_hat + 1.96 * se_log_rate),
            rate_per_100k  = rate_hat * 1e5,
            rate_lo_100k   = rate_ci_lo * 1e5,
            rate_hi_100k   = rate_ci_hi * 1e5
        )

# Design matrix for new data (matches fitted model)
X_new <- model.matrix(delete.response(terms(m_nb)), newdat)


beta_hat <- coef(m_nb)
Vb <- vcov(m_nb)

S <- 5000 # number of Monte Carlo draws

betas <- MASS::mvrnorm(S, mu = beta_hat, Sigma = Vb)

# Simulate mu draws and predictive counts
mu_draws <- matrix(NA_real_, nrow = S, ncol = nrow(newdat))
y_draws <- matrix(NA_real_, nrow = S, ncol = nrow(newdat))

for (s in 1:S) {
    eta_s <- X_new %*% betas[s, ] + log(newdat$pop) # add offset on link scale
    mu_s <- exp(eta_s)
    mu_draws[s, ] <- mu_s
    y_draws[s, ] <- rnbinom(n = ncol(mu_draws), size = theta_hat, mu = mu_s)
}

# Pointwise predictive intervals for counts
pred_q <- apply(y_draws, 2, quantile, probs = c(0.025, 0.5, 0.975))

newdat <- newdat |>
    mutate(
        mc_cf_lo   = pred_q[1, ],
        mc_cf_med  = pred_q[2, ],
        mc_cf_hi   = pred_q[3, ]
    )

rate_draws <- sweep(mu_draws, 2, newdat$pop, "/") # expected rates per person

rate_q <- apply(rate_draws, 2, quantile, probs = c(0.025, 0.5, 0.975))

newdat <- newdat |>
    mutate(
        mc_rate_lo = rate_q[1, ],
        mc_rate_med = rate_q[2, ],
        mc_rate_hi = rate_q[3, ],
        mc_rate_lo_100k = mc_rate_lo * 1e5,
        mc_rate_med_100k = mc_rate_med * 1e5,
        mc_rate_hi_100k = mc_rate_hi * 1e5
    )

names(newdat)
    
 newdat %>%
    mutate(y_obs = y) %>%
        dplyr::select(date,
            y_obs , mu_hat,
            ci_pois_lo, ci_pois_hi,
            mc_cf_lo, mc_cf_med, mc_cf_hi
        )-> plot_counts

    ggplot(plot_counts, aes(date)) +
        geom_ribbon(aes(ymin = mc_cf_lo, ymax = mc_cf_hi), alpha = 0.15) +
        geom_line(aes(y = mc_cf_med), linetype = 2) +
        geom_line(aes(y = y_obs)) +
        labs(
            title = "Observed vs glm.nb counterfactual (Monte Carlo NB interval)",
            y = "Weekly deaths", x = NULL
        )


        plot_rates <- newdat |>
            dplyr::select(date, rate_per_100k, mc_rate_lo_100k, mc_rate_med_100k, mc_rate_hi_100k)

        ggplot(plot_rates, aes(date)) +
            geom_ribbon(aes(ymin = mc_rate_lo_100k, ymax = mc_rate_hi_100k), alpha = 0.15) +
            geom_line(aes(y = mc_rate_med_100k), linetype = 2) +
            geom_line(aes(y = rate_per_100k)) +
            labs(
                title = "Weekly mortality rates per 100k (glm.nb Monte Carlo interval)",
                y = "Deaths per 100 000", x = NULL
            )
