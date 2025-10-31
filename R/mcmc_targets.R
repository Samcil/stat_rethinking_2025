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
#' @seealso [normal_mu_logsigma_gradient()]
#' @family mcmc_targets
#' @export
#' @importFrom tibble tibble
#' @importFrom stats dnorm
normal_mu_logsigma_target <- function(data,
                                      params,
                                      a = 0, b = 1,
                                      k = 0, d = 0.5) {
  if (is.null(data) || !all(c("y") %in% names(data))) cli::cli_abort("`data` must be a data frame with column `y`.")
  if (!is.numeric(a) || !is.numeric(b) || b <= 0 || !is.numeric(k) || !is.numeric(d) || d <= 0) cli::cli_abort("Priors must satisfy: b > 0 and d > 0.")

  eval_one <- function(mu, log_sigma) {
    s <- exp(log_sigma)
    U <- sum(stats::dnorm(data$y, mean = mu, sd = s, log = TRUE)) +
      stats::dnorm(mu, mean = a, sd = b, log = TRUE) +
      stats::dnorm(log_sigma, mean = k, sd = d, log = TRUE)
    -U
  }

  if (is.numeric(params) && length(params) == 2L) {
    mu <- params[1]
    log_sigma <- params[2]
    tibble::tibble(
      mu = mu, log_sigma = log_sigma,
      neg_log_prob = eval_one(mu, log_sigma)
    )
  } else if (is.data.frame(params)) {
    if (!all(c("mu", "log_sigma") %in% names(params))) cli::cli_abort("`params` must have columns `mu` and `log_sigma`.")
    neg_log_prob <- purrr::pmap_dbl(list(params$mu, params$log_sigma), ~ eval_one(..1, ..2))
    tibble::tibble(
      mu = params$mu, log_sigma = params$log_sigma,
      neg_log_prob = neg_log_prob
    )
  } else {
    cli::cli_abort("`params` must be numeric length-2 or a data frame with columns `mu`, `log_sigma`.")
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
#' @seealso [normal_mu_logsigma_target()]
#' @family mcmc_targets
#' @export
normal_mu_logsigma_gradient <- function(data, params,
                                        a = 0, b = 1,
                                        k = 0, d = 0.5) {
  if (is.null(data) || !all(c("y") %in% names(data))) cli::cli_abort("`data` must be a data frame with column `y`.")
  if (!is.numeric(a) || !is.numeric(b) || b <= 0 || !is.numeric(k) || !is.numeric(d) || d <= 0) cli::cli_abort("Priors must satisfy: b > 0 and d > 0.")

  grad_one <- function(mu, log_sigma) {
    s <- exp(log_sigma)
    G1 <- sum(data$y - mu) * exp(-2 * log_sigma) + (a - mu) / (b^2)
    G2 <- sum((data$y - mu)^2) * exp(-2 * log_sigma) - length(data$y) + (k - log_sigma) / (d^2)
    # Return negative gradient because U is negative log-probability
    c(d_mu = -G1, d_log_sigma = -G2)
  }

  if (is.numeric(params) && length(params) == 2L) {
    mu <- params[1]
    log_sigma <- params[2]
    g <- grad_one(mu, log_sigma)
    tibble::tibble(
      mu = mu, log_sigma = log_sigma,
      d_mu = unname(g["d_mu"]), d_log_sigma = unname(g["d_log_sigma"])
    )
  } else if (is.data.frame(params)) {
    if (!all(c("mu", "log_sigma") %in% names(params))) cli::cli_abort("`params` must have columns `mu` and `log_sigma`.")
    d_mu <- purrr::pmap_dbl(list(params$mu, params$log_sigma), ~ grad_one(..1, ..2)["d_mu"])
    d_log_sigma <- purrr::pmap_dbl(list(params$mu, params$log_sigma), ~ grad_one(..1, ..2)["d_log_sigma"])
    tibble::tibble(
      mu = params$mu, log_sigma = params$log_sigma,
      d_mu = d_mu, d_log_sigma = d_log_sigma
    )
  } else {
    cli::cli_abort("`params` must be numeric length-2 or a data frame with columns `mu`, `log_sigma`.")
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
#' @seealso [normal_sum2d_gradient()]
#' @family mcmc_targets
#' @export
#' @importFrom stats dnorm
normal_sum2d_target <- function(data, params, a = 0, b = 1) {
  if (is.null(data) || !all(c("y") %in% names(data))) cli::cli_abort("`data` must be a data frame with column `y`.")
  if (!is.numeric(a) || !is.numeric(b) || b <= 0) cli::cli_abort("Prior sd `b` must be > 0.")

  eval_one <- function(a1, a2) {
    mu <- a1 + a2
    U <- sum(stats::dnorm(data$y, mean = mu, sd = 1, log = TRUE)) +
      stats::dnorm(a1, mean = a, sd = b, log = TRUE) +
      stats::dnorm(a2, mean = a, sd = b, log = TRUE)
    -U
  }

  if (is.numeric(params) && length(params) == 2L) {
    a1 <- params[1]
    a2 <- params[2]
    tibble::tibble(a1 = a1, a2 = a2, neg_log_prob = eval_one(a1, a2))
  } else if (is.data.frame(params)) {
    if (!all(c("a1", "a2") %in% names(params))) cli::cli_abort("`params` must have columns `a1` and `a2`.")
    neg_log_prob <- purrr::pmap_dbl(list(params$a1, params$a2), ~ eval_one(..1, ..2))
    tibble::tibble(
      a1 = params$a1, a2 = params$a2,
      neg_log_prob = neg_log_prob
    )
  } else {
    cli::cli_abort("`params` must be numeric length-2 or a data frame with columns `a1`, `a2`.")
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
#' @seealso [normal_sum2d_target()]
#' @family mcmc_targets
#' @export
normal_sum2d_gradient <- function(data, params, a = 0, b = 1) {
  if (is.null(data) || !all(c("y") %in% names(data))) cli::cli_abort("`data` must be a data frame with column `y`.")
  if (!is.numeric(a) || !is.numeric(b) || b <= 0) cli::cli_abort("Prior sd `b` must be > 0.")

  grad_one <- function(a1, a2) {
    mu <- a1 + a2
    G1 <- sum(data$y - mu) / 1 + (a - a1) / (b^2)
    G2 <- sum(data$y - mu) / 1 + (a - a2) / (b^2)
    c(d_a1 = -G1, d_a2 = -G2)
  }

  if (is.numeric(params) && length(params) == 2L) {
    a1 <- params[1]
    a2 <- params[2]
    g <- grad_one(a1, a2)
    tibble::tibble(
      a1 = a1, a2 = a2,
      d_a1 = unname(g["d_a1"]), d_a2 = unname(g["d_a2"])
    )
  } else if (is.data.frame(params)) {
    if (!all(c("a1", "a2") %in% names(params))) cli::cli_abort("`params` must have columns `a1` and `a2`.")
    d_a1 <- purrr::pmap_dbl(list(params$a1, params$a2), ~ grad_one(..1, ..2)["d_a1"])
    d_a2 <- purrr::pmap_dbl(list(params$a1, params$a2), ~ grad_one(..1, ..2)["d_a2"])
    tibble::tibble(
      a1 = params$a1, a2 = params$a2,
      d_a1 = d_a1, d_a2 = d_a2
    )
  } else {
    cli::cli_abort("`params` must be numeric length-2 or a data frame with columns `a1`, `a2`.")
  }
}



#' Wrapper: negative log posterior for Gaussian(mu, log_sigma)
#'
#' Convenience alias matching the planned API name. Delegates to
#' [normal_mu_logsigma_target()].
#'
#' @inheritParams normal_mu_logsigma_target
#' @return Tibble with columns: mu, log_sigma, neg_log_prob.
#' @examples
#' dat <- tibble::tibble(y = c(-1, 0, 1))
#' nlp_gaussian_mu_log_sigma(dat, c(0, 0))
#' @seealso [grad_nlp_gaussian_mu_log_sigma()], [normal_mu_logsigma_target()]
#' @family mcmc_targets
#' @export
nlp_gaussian_mu_log_sigma <- function(data, params, a = 0, b = 1, k = 0, d = 0.5) {
  normal_mu_logsigma_target(data = data, params = params, a = a, b = b, k = k, d = d)
}

#' Wrapper: gradient for Gaussian(mu, log_sigma) negative log posterior
#'
#' Convenience alias matching the planned API name. Delegates to
#' [normal_mu_logsigma_gradient()].
#'
#' @inheritParams normal_mu_logsigma_gradient
#' @return Tibble with columns: mu, log_sigma, d_mu, d_log_sigma.
#' @examples
#' dat <- tibble::tibble(y = c(-1, 0, 1))
#' grad_nlp_gaussian_mu_log_sigma(dat, c(0, 0))
#' @seealso [nlp_gaussian_mu_log_sigma()], [normal_mu_logsigma_gradient()]
#' @family mcmc_targets
#' @export
grad_nlp_gaussian_mu_log_sigma <- function(data, params, a = 0, b = 1, k = 0, d = 0.5) {
  normal_mu_logsigma_gradient(data = data, params = params, a = a, b = b, k = k, d = d)
}

#' Wrapper: negative log posterior for two-parameter additive Normal
#'
#' Convenience alias for [normal_sum2d_target()].
#'
#' @inheritParams normal_sum2d_target
#' @return Tibble with columns: a1, a2, neg_log_prob.
#' @examples
#' dat <- tibble::tibble(y = c(-1, 0, 1))
#' nlp_two_param_additive(dat, c(0, 0))
#' @seealso [grad_nlp_two_param_additive()], [normal_sum2d_target()]
#' @family mcmc_targets
#' @export
nlp_two_param_additive <- function(data, params, a = 0, b = 1) {
  normal_sum2d_target(data = data, params = params, a = a, b = b)
}

#' Wrapper: gradient for two-parameter additive Normal negative log posterior
#'
#' Convenience alias for [normal_sum2d_gradient()].
#'
#' @inheritParams normal_sum2d_gradient
#' @return Tibble with columns: a1, a2, d_a1, d_a2.
#' @examples
#' dat <- tibble::tibble(y = c(-1, 0, 1))
#' grad_nlp_two_param_additive(dat, c(0, 0))
#' @seealso [nlp_two_param_additive()], [normal_sum2d_gradient()]
#' @family mcmc_targets
#' @export
grad_nlp_two_param_additive <- function(data, params, a = 0, b = 1) {
  normal_sum2d_gradient(data = data, params = params, a = a, b = b)
}

#' Neal's funnel: negative log posterior (energy)
#'
#' Defines the "funnel" density with scalar x and v: x | v ~ Normal(0, exp(v)),
#' v ~ Normal(0, s). Returns U (negative log-probability). Data is unused and
#' kept for a consistent API.
#'
#' @param data Unused; kept for consistency with other targets.
#' @param params Either numeric length-2 c(x, v) or a data.frame with columns `x`, `v`.
#' @param s Prior sd for v (default 3, > 0).
#' @return Tibble with columns: x, v, neg_log_prob.
#' @examples
#' funnel_nlp(NULL, c(0, 0))
#' @seealso [funnel_grad()]
#' @family mcmc_targets
#' @export
#' @importFrom stats dnorm
funnel_nlp <- function(data, params, s = 3) {
  if (!is.numeric(s) || length(s) != 1L || s <= 0) cli::cli_abort("`s` must be a single positive number.")
  eval_one <- function(x, v) {
    U <- stats::dnorm(x, 0, exp(v), log = TRUE) + stats::dnorm(v, 0, s, log = TRUE)
    -U
  }
  if (is.numeric(params) && length(params) == 2L) {
    x <- params[1]
    v <- params[2]
    tibble::tibble(x = x, v = v, neg_log_prob = eval_one(x, v))
  } else if (is.data.frame(params)) {
    if (!all(c("x", "v") %in% names(params))) cli::cli_abort("`params` must have columns `x` and `v`.")
    neg_log_prob <- purrr::pmap_dbl(list(params$x, params$v), ~ eval_one(..1, ..2))
    tibble::tibble(x = params$x, v = params$v, neg_log_prob = neg_log_prob)
  } else {
    cli::cli_abort("`params` must be numeric length-2 or a data frame with columns `x`, `v`.")
  }
}

#' Neal's funnel: gradient of negative log posterior
#'
#' Analytic gradient of U for [funnel_nlp()]. Returns d_x and d_v (gradient of U).
#'
#' @inheritParams funnel_nlp
#' @return Tibble with columns: x, v, d_x, d_v.
#' @examples
#' funnel_grad(NULL, c(0.5, -0.2))
#' @seealso [funnel_nlp()]
#' @family mcmc_targets
#' @export
funnel_grad <- function(data, params, s = 3) {
  if (!is.numeric(s) || length(s) != 1L || s <= 0) cli::cli_abort("`s` must be a single positive number.")
  grad_one <- function(x, v) {
    # dU/dx and dU/dv for scalar x
    dUx <- -exp(-2 * v) * x
    dUv <- (-v) / s^2 - 1 + exp(-2 * v) * (x^2)
    c(d_x = dUx, d_v = dUv)
  }
  if (is.numeric(params) && length(params) == 2L) {
    x <- params[1]
    v <- params[2]
    g <- grad_one(x, v)
    tibble::tibble(x = x, v = v, d_x = unname(g["d_x"]), d_v = unname(g["d_v"]))
  } else if (is.data.frame(params)) {
    if (!all(c("x", "v") %in% names(params))) cli::cli_abort("`params` must have columns `x` and `v`.")
    d_x <- purrr::pmap_dbl(list(params$x, params$v), ~ grad_one(..1, ..2)["d_x"])
    d_v <- purrr::pmap_dbl(list(params$x, params$v), ~ grad_one(..1, ..2)["d_v"])
    tibble::tibble(x = params$x, v = params$v, d_x = d_x, d_v = d_v)
  } else {
    cli::cli_abort("`params` must be numeric length-2 or a data frame with columns `x`, `v`.")
  }
}
