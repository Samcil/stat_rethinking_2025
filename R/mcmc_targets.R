#' MCMC target densities and gradients (tidyverse-friendly)
#'
#' Refactored negative log posterior ("energy") functions and their analytic
#' gradients extracted from scripts/08_MCMC.r. These are data-first,
#' pipe-friendly, and return tibbles.
#'
#' @family mcmc_targets
#' @keywords internal
NULL

#' Negative log posterior for Normal model with parameters (mu, log_sigma)
#'
#' Observations are modeled as y ~ Normal(mu, sigma) with sigma = exp(log_sigma).
#' Priors: mu ~ Normal(a, b); log_sigma ~ Normal(k, d). Returns the negative log
#' posterior (energy) U. Accepts either a single parameter vector or a tibble
#' of parameter rows.
#'
#' @param data A tibble/data.frame with a numeric column `y` of observations.
#' @param params Either a numeric length-2 vector c(mu, log_sigma) or a tibble
#'   with columns `mu` and `log_sigma` (vectorized evaluation across rows).
#' @param a Prior mean for mu (default 0).
#' @param b Prior sd for mu (default 1, > 0).
#' @param k Prior mean for log_sigma (default 0).
#' @param d Prior sd for log_sigma (default 0.5, > 0).
#' @return A tibble with columns: mu, log_sigma, neg_log_prob.
#' @examples
#' dat <- tibble::tibble(y = c(-1, 0, 1))
#' normal_mu_logsigma_target(dat, c(0, 0))
#' @export
#' @importFrom tibble tibble
#' @importFrom stats dnorm
normal_mu_logsigma_target <- function(data,
                                      params,
                                      a = 0, b = 1,
                                      k = 0, d = 0.5) {
  stopifnot(!is.null(data), all(c("y") %in% names(data)))
  stopifnot(is.numeric(a), is.numeric(b), b > 0,
            is.numeric(k), is.numeric(d), d > 0)

  eval_one <- function(mu, log_sigma) {
    s <- exp(log_sigma)
    U <- sum(stats::dnorm(data$y, mean = mu, sd = s, log = TRUE)) +
      stats::dnorm(mu, mean = a, sd = b, log = TRUE) +
      stats::dnorm(log_sigma, mean = k, sd = d, log = TRUE)
    -U
  }

  if (is.numeric(params) && length(params) == 2L) {
    mu <- params[1]; log_sigma <- params[2]
    tibble::tibble(mu = mu, log_sigma = log_sigma,
                   neg_log_prob = eval_one(mu, log_sigma))
  } else if (is.data.frame(params)) {
    stopifnot(all(c("mu", "log_sigma") %in% names(params)))
    neg_log_prob <- mapply(eval_one, params$mu, params$log_sigma)
    tibble::tibble(mu = params$mu, log_sigma = params$log_sigma,
                   neg_log_prob = as.numeric(neg_log_prob))
  } else {
    stop("params must be numeric length-2 or data frame with mu, log_sigma")
  }
}

#' Gradient of negative log posterior for Normal(mu, log_sigma)
#'
#' Analytic gradient of U with respect to (mu, log_sigma) for the model in
#' [normal_mu_logsigma_target()]. Returns a tibble with columns d_mu and
#' d_log_sigma. Accepts vector or tibble of parameter rows.
#'
#' @inheritParams normal_mu_logsigma_target
#' @return A tibble with columns: mu, log_sigma, d_mu, d_log_sigma.
#' @examples
#' dat <- tibble::tibble(y = c(-1, 0, 1))
#' normal_mu_logsigma_gradient(dat, c(0, 0))
#' @export
normal_mu_logsigma_gradient <- function(data, params,
                                        a = 0, b = 1,
                                        k = 0, d = 0.5) {
  stopifnot(!is.null(data), all(c("y") %in% names(data)))
  stopifnot(is.numeric(a), is.numeric(b), b > 0,
            is.numeric(k), is.numeric(d), d > 0)

  grad_one <- function(mu, log_sigma) {
    s <- exp(log_sigma)
    G1 <- sum(data$y - mu) * exp(-2 * log_sigma) + (a - mu) / (b^2)
    G2 <- sum((data$y - mu)^2) * exp(-2 * log_sigma) - length(data$y) + (k - log_sigma) / (d^2)
    # Return negative gradient because U is negative log-probability
    c(d_mu = -G1, d_log_sigma = -G2)
  }

  if (is.numeric(params) && length(params) == 2L) {
    mu <- params[1]; log_sigma <- params[2]
    g <- grad_one(mu, log_sigma)
    tibble::tibble(mu = mu, log_sigma = log_sigma,
                   d_mu = unname(g["d_mu"]), d_log_sigma = unname(g["d_log_sigma"]))
  } else if (is.data.frame(params)) {
    stopifnot(all(c("mu", "log_sigma") %in% names(params)))
    grads <- mapply(grad_one, params$mu, params$log_sigma)
    tibble::tibble(mu = params$mu, log_sigma = params$log_sigma,
                   d_mu = as.numeric(grads[1, ]), d_log_sigma = as.numeric(grads[2, ]))
  } else {
    stop("params must be numeric length-2 or data frame with mu, log_sigma")
  }
}

#' Negative log posterior for 2-parameter Normal with sum mu = a1 + a2
#'
#' Observations follow y ~ Normal(mu, 1) with mu = a1 + a2. Priors:
#' a1 ~ Normal(a, b), a2 ~ Normal(a, b). Returns U (negative log posterior).
#'
#' @param params Either a numeric length-2 vector c(a1, a2) or a tibble/data.frame
#'   with columns `a1` and `a2`.
#' @param a Prior mean for both parameters (default 0).
#' @param b Prior sd for both parameters (default 1, > 0).
#' @inheritParams normal_mu_logsigma_target
#' @return A tibble with columns: a1, a2, neg_log_prob.
#' @examples
#' dat <- tibble::tibble(y = c(-1, 0, 1))
#' normal_sum2d_target(dat, c(0, 0))
#' @export
#' @importFrom stats dnorm
normal_sum2d_target <- function(data, params, a = 0, b = 1) {
  stopifnot(!is.null(data), all(c("y") %in% names(data)))
  stopifnot(is.numeric(a), is.numeric(b), b > 0)

  eval_one <- function(a1, a2) {
    mu <- a1 + a2
    U <- sum(stats::dnorm(data$y, mean = mu, sd = 1, log = TRUE)) +
      stats::dnorm(a1, mean = a, sd = b, log = TRUE) +
      stats::dnorm(a2, mean = a, sd = b, log = TRUE)
    -U
  }

  if (is.numeric(params) && length(params) == 2L) {
    a1 <- params[1]; a2 <- params[2]
    tibble::tibble(a1 = a1, a2 = a2, neg_log_prob = eval_one(a1, a2))
  } else if (is.data.frame(params)) {
    stopifnot(all(c("a1", "a2") %in% names(params)))
    neg_log_prob <- mapply(eval_one, params$a1, params$a2)
    tibble::tibble(a1 = params$a1, a2 = params$a2,
                   neg_log_prob = as.numeric(neg_log_prob))
  } else {
    stop("params must be numeric length-2 or data frame with a1, a2")
  }
}

#' Gradient of negative log posterior for Normal with mu = a1 + a2
#'
#' Analytic gradient of U with respect to (a1, a2) for the model in
#' [normal_sum2d_target()]. Returns a tibble with columns d_a1 and d_a2.
#'
#' @inheritParams normal_sum2d_target
#' @return A tibble with columns: a1, a2, d_a1, d_a2.
#' @examples
#' dat <- tibble::tibble(y = c(-1, 0, 1))
#' normal_sum2d_gradient(dat, c(0, 0))
#' @export
normal_sum2d_gradient <- function(data, params, a = 0, b = 1) {
  stopifnot(!is.null(data), all(c("y") %in% names(data)))
  stopifnot(is.numeric(a), is.numeric(b), b > 0)

  grad_one <- function(a1, a2) {
    mu <- a1 + a2
    G1 <- sum(data$y - mu) / 1 + (a - a1) / (b^2)
    G2 <- sum(data$y - mu) / 1 + (a - a2) / (b^2)
    c(d_a1 = -G1, d_a2 = -G2)
  }

  if (is.numeric(params) && length(params) == 2L) {
    a1 <- params[1]; a2 <- params[2]
    g <- grad_one(a1, a2)
    tibble::tibble(a1 = a1, a2 = a2,
                   d_a1 = unname(g["d_a1"]), d_a2 = unname(g["d_a2"]))
  } else if (is.data.frame(params)) {
    stopifnot(all(c("a1", "a2") %in% names(params)))
    grads <- mapply(grad_one, params$a1, params$a2)
    tibble::tibble(a1 = params$a1, a2 = params$a2,
                   d_a1 = as.numeric(grads[1, ]), d_a2 = as.numeric(grads[2, ]))
  } else {
    stop("params must be numeric length-2 or data frame with a1, a2")
  }
}

