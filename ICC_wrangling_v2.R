# ICC wrangling utilities (v2)
# --------------------------------
# This script collects the probability builders and power-simulation helpers
# discussed in ICC_wrangling.r, but exposes cleaner hooks plus "real-life"
# defaults for common intervention strategies.

library(data.table)
library(stats)

# ---- Joint probability helpers ------------------------------------------------

#' Build a feasible joint distribution for paired Bernoulli outcomes
#'
#' @param p1 Baseline garbage proportion (paper form)
#' @param p2 Post-intervention garbage proportion (electronic/eMCCD)
#' @param rho Within-certificate correlation (ICC analogue)
#' @param p01_floor Minimum probability that a well-defined paper form becomes
#'   garbage electronically. Keeps realistic non-zero error rates.
#' @param p10_floor Minimum probability that a garbage paper form is corrected.
#' @return Named vector (p00, p01, p10, p11)
build_joint_probs_frechet <- function(p1, p2, rho,
                                      p01_floor = 0,
                                      p10_floor = 0) {
  stopifnot(p1 > 0, p1 < 1, p2 > 0, p2 < 1)
  if (abs(rho) > 0.999) stop("rho must be between -0.999 and 0.999")

  sd1 <- sqrt(p1 * (1 - p1))
  sd2 <- sqrt(p2 * (1 - p2))
  cov12 <- rho * sd1 * sd2

  p11_raw <- p1 * p2 + cov12
  p11_min <- max(0, p1 + p2 - 1)
  p11_max <- min(p1, p2)
  p11 <- min(max(p11_raw, p11_min), p11_max)

  p10 <- p1 - p11
  p01 <- p2 - p11
  p00 <- 1 - p11 - p10 - p01

  probs <- c(p00 = p00, p01 = p01, p10 = p10, p11 = p11)
  if (any(probs < -1e-10)) {
    stop("rho_within not feasible; adjust rho or effect size")
  }

  probs <- pmax(probs, 0)
  probs <- probs / sum(probs)

  if (p01_floor > 0 && probs["p01"] < p01_floor - 1e-9) {
    stop("Requested p01_floor exceeds feasible p01 under this rho.")
  }
  if (p10_floor > 0 && probs["p10"] < p10_floor - 1e-9) {
    stop("Requested p10_floor exceeds feasible p10 under this rho.")
  }

  probs
}

#' Conditional builder that fixes the well-defined -> garbage probability
#' and adjusts the remaining cells to preserve the marginals.
build_joint_probs_conditional <- function(p1, p2, q01 = 0.01) {
  stopifnot(q01 >= 0, q01 <= min(1 - p2, 1 - p1))
  p01 <- q01
  p00 <- 1 - p2 - p01
  p11 <- p2 - p01
  p10 <- p1 - p11
  probs <- c(p00 = p00, p01 = p01, p10 = p10, p11 = p11)
  if (any(probs < -1e-10)) stop("q01 incompatible with p1/p2")
  pmax(probs, 0) / sum(pmax(probs, 0))
}

# ---- Power simulation ---------------------------------------------------------

simulate_power_two_proportions_v2 <- function(p1,
                                              p2,
                                              n_per_group,
                                              nsim = 5000,
                                              alpha = 0.05,
                                              two_sided = FALSE,
                                              paired = FALSE,
                                              rho_within = 0,
                                              joint_version = c("frechet", "conditional"),
                                              p01_floor = 0.01,
                                              p10_floor = 0) {
  joint_version <- match.arg(joint_version)
  n_vec <- n_per_group
  out <- data.table(n_per_group = n_vec, power = NA_real_)

  for (i in seq_along(n_vec)) {
    n <- n_vec[i]
    pvals <- numeric(nsim)

    if (paired) {
      joint <- if (joint_version == "frechet") {
        build_joint_probs_frechet(p1, p2, rho_within,
                                  p01_floor = p01_floor,
                                  p10_floor = p10_floor)
      } else {
        build_joint_probs_conditional(p1, p2, q01 = p01_floor)
      }
    }

    for (s in seq_len(nsim)) {
      if (!paired) {
        x1 <- rbinom(1, size = n, prob = p1)
        x2 <- rbinom(1, size = n, prob = p2)
        alt <- if (two_sided) {
          "two.sided"
        } else if (p2 > p1) {
          "greater"
        } else {
          "less"
        }
        test <- prop.test(x = c(x1, x2), n = c(n, n), alternative = alt)
        pvals[s] <- test$p.value
      } else {
        draws <- rmultinom(1, size = n, prob = joint)
        n01 <- draws["p01", ]
        n10 <- draws["p10", ]
        discordant <- n01 + n10
        if (discordant == 0) {
          pvals[s] <- 1
        } else if (two_sided) {
          tail <- pbinom(min(n01, n10), discordant, 0.5)
          pvals[s] <- min(1, 2 * tail)
        } else if (p2 > p1) {
          pvals[s] <- pbinom(n01 - 1, discordant, 0.5, lower.tail = FALSE)
        } else {
          pvals[s] <- pbinom(n10 - 1, discordant, 0.5, lower.tail = FALSE)
        }
      }
    }

    out$power[i] <- mean(pvals < alpha)
  }

  out
}

# ---- Scenario presets ---------------------------------------------------------

intervention_scenarios <- data.table(
  scenario = c("Status quo", "Paper training", "Full eMCCD rollout"),
  p1 = c(0.22, 0.22, 0.22),
  p2 = c(0.22, 0.16, 0.10),
  rho = c(0.1, 0.25, 0.25),
  p01_floor = c(0.02, 0.01, 0.005),
  p10_floor = c(0.05, 0.10, 0.15)
)

#' Evaluate power curves for each scenario on a common grid.
run_intervention_scenarios <- function(n_grid = seq(50, 600, by = 25),
                                       target_power = 0.80) {
  results <- rbindlist(lapply(seq_len(nrow(intervention_scenarios)), function(i) {
    row <- intervention_scenarios[i]
    power_df <- simulate_power_two_proportions_v2(
      p1 = row$p1,
      p2 = row$p2,
      n_per_group = n_grid,
      nsim = 4000,
      alpha = 0.05,
      two_sided = TRUE,
      paired = TRUE,
      rho_within = row$rho,
      joint_version = "frechet",
      p01_floor = row$p01_floor,
      p10_floor = row$p10_floor
    )
    min_n <- power_df$n_per_group[which(power_df$power >= target_power)[1]]
    power_df[, `:=`(scenario = row$scenario, min_n = min_n, target = target_power)]
  }))
  results
}

run_intervention_scenarios()->
    intervention_power_results

    intervention_power_results%>%
        ggplot() +
        aes(x = n_per_group, y = power, color = scenario) +
        geom_line(size = 1) +
        geom_hline(aes(yintercept = target), linetype = 2) +
        geom_vline(aes(xintercept = min_n, color = scenario), linetype = 3) +
        labs(
            title = "Power curves for ICC study scenarios",
            x = "Sample size per group",
            y = "Power"
        ) 

