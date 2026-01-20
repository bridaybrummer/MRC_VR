# tracys sample size 

library(dplyr)
library(tidyr)
library(purrr)

# Paired binary sample size (McNemar, normal approx)
mcnemar_n <- function(p1, p2, rho, power = 0.8,
                      alpha = 0.05, two_sided = TRUE) {
  # p1 = baseline prop unusable
  # p2 = follow-up prop unusable (after intervention)
  # rho = within-certificate correlation between time 1 and 2
  
  # joint probabilities implied by p1,p2,rho
  sd1   <- sqrt(p1 * (1 - p1))
  sd2   <- sqrt(p2 * (1 - p2))
  cov12 <- rho * sd1 * sd2
  
  p11 <- p1 * p2 + cov12
  p10 <- p1 - p11
  p01 <- p2 - p11
  p00 <- 1 - p11 - p10 - p01
  
  if (any(c(p00, p01, p10, p11) < -1e-6)) {
    warning("Some joint probabilities < 0; check p1, p2, rho.")
  }
  
  # truncate tiny negatives to 0 to keep formula stable
  p10 <- pmax(p10, 0)
  p01 <- pmax(p01, 0)
  
  z_alpha <- qnorm(1 - alpha / if (two_sided) 2 else 1)
  z_beta  <- qnorm(power)
  
  n <- ((z_alpha + z_beta)^2 * (p10 + p01)) / (p10 - p01)^2
  
  ceiling(n)   # number of *pairs* needed ignoring clustering
}


design_effect <- function(cluster_size, icc) {
    1 + (cluster_size - 1) * icc
}

power_deff_grid <- function(
    p1, p2,
    rho_vec,
    icc_vec,
    cluster_size,
    power = 0.8,
    alpha = 0.05,
    incompleteness = c(0.10, 0.20),
    n_periods = 2) {
    grid <- expand_grid(
        correlation = rho_vec,
        ICC         = icc_vec
    ) %>%
        mutate(
            Estimated_N = map_int(
                correlation,
                ~ mcnemar_n(
                    p1 = p1,
                    p2 = p2,
                    rho = .x,
                    power = power,
                    alpha = alpha
                )
            ),
            DEFF = design_effect(cluster_size, ICC),
            Adjusted_N = ceiling(Estimated_N * DEFF)
        )

    for (inc in incompleteness) {
        col_name <- paste0("N_", round(inc * 100), "pct_incomplete")
        grid[[col_name]] <- ceiling(grid$Adjusted_N * (1 + inc))
    }

    if (length(incompleteness) > 0) {
        first_col <- paste0("N_", round(incompleteness[1] * 100), "pct_incomplete")
        grid$Total_all_periods <- n_periods * grid[[first_col]]
    }

    grid
}


tab_10drop <- power_deff_grid(
    p1              = 0.20,
    p2              = 0.10, # 10% absolute drop
    rho_vec         = c(0.6, 0.5, 0.3), # your “Correlation” column
    icc_vec         = c(0.05, 0.03, 0.01), # your ICC column
    cluster_size    = 130,
    power           = 0.8,
    alpha           = 0.05,
    incompleteness  = c(0.10, 0.20),
    n_periods       = 2
)
tab_10drop


tab_5drop <- power_deff_grid(
    p1              = 0.15,
    p2              = 0.10, # 5% absolute drop
    rho_vec         = c(0.6, 0.5, 0.3),
    icc_vec         = c(0.05, 0.03, 0.01),
    cluster_size    = 30,
    power           = 0.8,
    alpha           = 0.05,
    incompleteness  = c(0.10, 0.20),
    n_periods       = 2
)

tab_5drop
