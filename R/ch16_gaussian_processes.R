#' Gaussian Processes - Chapter 16
#'
#' Functions for working with Gaussian processes including various
#' covariance functions and GP regression.
#'
#' @name ch16_gaussian_processes
#' @keywords internal
"_PACKAGE"

#' Squared Exponential Covariance Function
#'
#' Compute covariance matrix using squared exponential (RBF) kernel.
#'
#' @param x Numeric vector of input points
#' @param eta_sq Marginal variance parameter
#' @param rho_sq Squared length-scale parameter
#' @param sigma_sq Nugget variance (observation noise)
#'
#' @return A covariance matrix
#'
#' @export
#' @examples
#' x <- seq(0, 10, length.out = 50)
#' K <- compute_squared_exponential(x, eta_sq = 1, rho_sq = 1)
compute_squared_exponential <- function(x,
                                       eta_sq = 1,
                                       rho_sq = 1,
                                       sigma_sq = 0) {
  validate_positive(eta_sq, "eta_sq")
  validate_positive(rho_sq, "rho_sq")

  if (sigma_sq < 0) {
    rlang::abort("`sigma_sq` must be non-negative")
  }

  n <- length(x)
  K <- matrix(0, n, n)

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      d_sq <- (x[i] - x[j])^2
      K[i, j] <- eta_sq * exp(-d_sq / (2 * rho_sq))
    }
  }

  # Add nugget for numerical stability and observation noise
  diag(K) <- diag(K) + sigma_sq

  K
}

#' Periodic Covariance Function
#'
#' Compute covariance matrix using periodic kernel for cyclic patterns.
#'
#' @param x Numeric vector of input points
#' @param eta_sq Marginal variance parameter
#' @param rho_sq Length-scale parameter
#' @param period Period of the function
#' @param sigma_sq Nugget variance (observation noise)
#'
#' @return A covariance matrix
#'
#' @export
#' @examples
#' x <- seq(0, 20, length.out = 100)
#' K <- compute_periodic_covariance(x, period = 5, eta_sq = 1, rho_sq = 1)
compute_periodic_covariance <- function(x,
                                       eta_sq = 1,
                                       rho_sq = 1,
                                       period = 1,
                                       sigma_sq = 0) {
  validate_positive(eta_sq, "eta_sq")
  validate_positive(rho_sq, "rho_sq")
  validate_positive(period, "period")

  if (sigma_sq < 0) {
    rlang::abort("`sigma_sq` must be non-negative")
  }

  n <- length(x)
  K <- matrix(0, n, n)

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      d <- abs(x[i] - x[j])
      sin_term <- sin(pi * d / period)
      K[i, j] <- eta_sq * exp(-2 * sin_term^2 / rho_sq)
    }
  }

  diag(K) <- diag(K) + sigma_sq

  K
}

#' Sample from Gaussian Process Prior
#'
#' Generate samples from a GP prior with specified covariance function.
#'
#' @param x Numeric vector of input points
#' @param covariance_fn Covariance function ("squared_exp" or "periodic")
#' @param n_samples Number of function samples to draw
#' @param eta_sq Marginal variance parameter
#' @param rho_sq Length-scale parameter (squared for squared_exp)
#' @param period Period (only for periodic kernel)
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with columns: x, sample_id, y
#'
#' @export
#' @examples
#' x <- seq(0, 10, length.out = 100)
#' samples <- sample_gp_prior(
#'   x,
#'   covariance_fn = "squared_exp",
#'   n_samples = 5,
#'   eta_sq = 1,
#'   rho_sq = 1
#' )
sample_gp_prior <- function(x,
                           covariance_fn = "squared_exp",
                           n_samples = 3,
                           eta_sq = 1,
                           rho_sq = 1,
                           period = 1,
                           seed = NULL) {
  validate_positive(n_samples, "n_samples")

  if (!covariance_fn %in% c("squared_exp", "periodic")) {
    rlang::abort('`covariance_fn` must be "squared_exp" or "periodic"')
  }

  if (!is.null(seed)) set.seed(seed)

  # Compute covariance matrix
  if (covariance_fn == "squared_exp") {
    K <- compute_squared_exponential(x, eta_sq, rho_sq, sigma_sq = 1e-10)
  } else {
    K <- compute_periodic_covariance(x, eta_sq, rho_sq, period, sigma_sq = 1e-10)
  }

  # Generate samples using Cholesky decomposition
  L <- chol(K)
  n <- length(x)

  samples <- purrr::map_dfr(seq_len(n_samples), function(i) {
    y <- as.vector(stats::rnorm(n) %*% L)

    tibble::tibble(
      x = x,
      sample_id = i,
      y = y
    )
  })

  samples
}

#' Compute Gaussian Process Posterior
#'
#' Calculate posterior mean and variance for GP regression.
#'
#' @param x_obs Observed input points
#' @param y_obs Observed output values
#' @param x_pred Points at which to make predictions
#' @param eta_sq Marginal variance parameter
#' @param rho_sq Length-scale parameter
#' @param sigma_sq Observation noise variance
#'
#' @return A tibble with columns: x, post_mean, post_var, post_sd
#'
#' @export
#' @examples
#' # Observed data
#' x_obs <- c(1, 3, 5, 7, 9)
#' y_obs <- sin(x_obs) + rnorm(5, 0, 0.1)
#'
#' # Prediction points
#' x_pred <- seq(0, 10, length.out = 100)
#'
#' # Compute posterior
#' posterior <- compute_gp_posterior(
#'   x_obs, y_obs, x_pred,
#'   eta_sq = 1, rho_sq = 1, sigma_sq = 0.01
#' )
compute_gp_posterior <- function(x_obs,
                                y_obs,
                                x_pred,
                                eta_sq = 1,
                                rho_sq = 1,
                                sigma_sq = 0.1) {
  if (length(x_obs) != length(y_obs)) {
    rlang::abort("`x_obs` and `y_obs` must have same length")
  }

  validate_positive(eta_sq, "eta_sq")
  validate_positive(rho_sq, "rho_sq")
  validate_positive(sigma_sq, "sigma_sq")

  # Covariance matrices
  K_obs <- compute_squared_exponential(x_obs, eta_sq, rho_sq, sigma_sq)
  K_pred <- compute_squared_exponential(x_pred, eta_sq, rho_sq, 0)

  # Cross-covariance
  n_obs <- length(x_obs)
  n_pred <- length(x_pred)
  K_cross <- matrix(0, n_obs, n_pred)

  for (i in seq_len(n_obs)) {
    for (j in seq_len(n_pred)) {
      d_sq <- (x_obs[i] - x_pred[j])^2
      K_cross[i, j] <- eta_sq * exp(-d_sq / (2 * rho_sq))
    }
  }

  # Posterior calculations
  K_obs_inv <- solve(K_obs)
  post_mean <- t(K_cross) %*% K_obs_inv %*% y_obs
  post_cov <- K_pred - t(K_cross) %*% K_obs_inv %*% K_cross

  tibble::tibble(
    x = x_pred,
    post_mean = as.vector(post_mean),
    post_var = diag(post_cov),
    post_sd = sqrt(diag(post_cov))
  )
}

#' Sample from Gaussian Process Posterior
#'
#' Generate samples from the GP posterior predictive distribution.
#'
#' @param posterior_data Tibble from compute_gp_posterior with post_mean, post_var
#' @param n_samples Number of samples to draw
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with columns: x, sample_id, y
#'
#' @export
#' @examples
#' x_obs <- c(1, 3, 5, 7, 9)
#' y_obs <- sin(x_obs) + rnorm(5, 0, 0.1)
#' x_pred <- seq(0, 10, length.out = 50)
#'
#' posterior <- compute_gp_posterior(x_obs, y_obs, x_pred)
#' samples <- sample_gp_posterior(posterior, n_samples = 10)
sample_gp_posterior <- function(posterior_data,
                               n_samples = 3,
                               seed = NULL) {
  validate_tibble(posterior_data, "posterior_data")
  validate_positive(n_samples, "n_samples")

  if (!all(c("x", "post_mean", "post_var") %in% names(posterior_data))) {
    rlang::abort("`posterior_data` must have columns: x, post_mean, post_var")
  }

  if (!is.null(seed)) set.seed(seed)

  n_points <- nrow(posterior_data)

  samples <- purrr::map_dfr(seq_len(n_samples), function(i) {
    y <- posterior_data$post_mean +
      sqrt(posterior_data$post_var) * stats::rnorm(n_points)

    tibble::tibble(
      x = posterior_data$x,
      sample_id = i,
      y = y
    )
  })

  samples
}
