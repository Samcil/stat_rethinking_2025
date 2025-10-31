#' Generate Prior Predictive Samples for Linear Models
#'
#' Sample from the prior distribution of a linear model to visualize
#' implications of prior choices before seeing data.
#'
#' @param n_samples Integer. Number of prior samples to generate. Default is 100.
#' @param prior_intercept Numeric vector of length 2. Mean and SD for intercept prior.
#'   Default is c(0, 1).
#' @param prior_slope Numeric vector of length 2. Mean and SD for slope prior.
#'   Default is c(0, 1).
#' @param x_range Numeric vector of length 2. Range of x values. Default is c(-2, 2).
#' @param n_points Integer. Number of x points to evaluate. Default is 50.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - sample: Sample identifier
#'   - x: Predictor values
#'   - y: Predicted values (a + b * x)
#'   - intercept: Sampled intercept value
#'   - slope: Sampled slope value
#'
#' @family chapter03
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Generate prior predictive samples
#' prior_lines <- generate_prior_predictive_linear(
#'   n_samples = 50,
#'   prior_intercept = c(0, 1),
#'   prior_slope = c(0, 1),
#'   seed = 42
#' )
#'
#' # Visualize prior predictions
#' ggplot(prior_lines, aes(x = x, y = y, group = sample)) +
#'   geom_line(alpha = 0.3) +
#'   labs(title = "Prior Predictive Distribution",
#'        subtitle = "a ~ N(0,1), b ~ N(0,1)") +
#'   theme_rethinking()
generate_prior_predictive_linear <- function(n_samples = 100,
                                            prior_intercept = c(0, 1),
                                            prior_slope = c(0, 1),
                                            x_range = c(-2, 2),
                                            n_points = 50,
                                            seed = NULL) {
  checkmate::assert_int(n_samples, lower = 1)
  checkmate::assert_numeric(prior_intercept, len = 2, finite = TRUE)
  checkmate::assert_numeric(prior_slope, len = 2, finite = TRUE)
  checkmate::assert_numeric(x_range, len = 2, finite = TRUE)
  checkmate::assert_int(n_points, lower = 2)
  
  validate_positive(c(prior_intercept[2], prior_slope[2]), 
                   arg_name = "prior standard deviations", 
                   allow_zero = FALSE)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Sample from priors
  intercepts <- rnorm(n_samples, prior_intercept[1], prior_intercept[2])
  slopes <- rnorm(n_samples, prior_slope[1], prior_slope[2])
  
  # Create x grid
  x_grid <- seq(x_range[1], x_range[2], length.out = n_points)
  
  # Generate predictions for each sample
  purrr::map2_dfr(intercepts, slopes, function(a, b, idx) {
    tibble::tibble(
      sample = idx,
      x = x_grid,
      y = a + b * x_grid,
      intercept = a,
      slope = b
    )
  }, idx = seq_len(n_samples))
}


#' Compute Posterior from Linear Model
#'
#' Fit a simple linear model using normal approximation (Laplace approximation)
#' to obtain posterior distribution of parameters.
#'
#' @param x Numeric vector. Predictor variable.
#' @param y Numeric vector. Outcome variable.
#' @param prior_intercept Numeric vector of length 2. Mean and SD for intercept prior.
#' @param prior_slope Numeric vector of length 2. Mean and SD for slope prior.
#' @param sigma_fixed Numeric. Fixed value for residual standard deviation.
#'   Default is 1.
#'
#' @return List with components:
#'   - coefficients: Named vector of posterior means
#'   - vcov: Variance-covariance matrix
#'   - sigma: Residual standard deviation
#'
#' @family chapter03
#' @export
#'
#' @examples
#' # Simulate data
#' x <- rnorm(20)
#' y <- 0.5 + 0.7 * x + rnorm(20, 0, 0.5)
#'
#' # Fit model
#' posterior <- compute_linear_posterior(
#'   x, y,
#'   prior_intercept = c(0, 1),
#'   prior_slope = c(0, 1)
#' )
#'
#' posterior$coefficients
compute_linear_posterior <- function(x, y,
                                    prior_intercept = c(0, 1),
                                    prior_slope = c(0, 1),
                                    sigma_fixed = 1) {
  validate_matching_lengths(x = x, y = y)
  checkmate::assert_numeric(prior_intercept, len = 2, finite = TRUE)
  checkmate::assert_numeric(prior_slope, len = 2, finite = TRUE)
  validate_positive(sigma_fixed, arg_name = "sigma_fixed", allow_zero = FALSE)
  
  n <- length(x)
  
  # Design matrix
  X <- cbind(1, x)
  
  # Prior precision matrix
  prior_precision <- diag(c(1 / prior_intercept[2]^2, 1 / prior_slope[2]^2))
  prior_mean <- c(prior_intercept[1], prior_slope[1])
  
  # Posterior precision and covariance
  likelihood_precision <- (t(X) %*% X) / sigma_fixed^2
  posterior_precision <- prior_precision + likelihood_precision
  posterior_vcov <- solve(posterior_precision)
  
  # Posterior mean
  prior_contribution <- prior_precision %*% prior_mean
  likelihood_contribution <- (t(X) %*% y) / sigma_fixed^2
  posterior_mean <- posterior_vcov %*% (prior_contribution + likelihood_contribution)
  
  list(
    coefficients = setNames(as.vector(posterior_mean), c("intercept", "slope")),
    vcov = posterior_vcov,
    sigma = sigma_fixed
  )
}


#' Sample from Linear Model Posterior
#'
#' Generate samples from the posterior distribution of a linear model.
#'
#' @param posterior_fit List from \code{compute_linear_posterior()}.
#' @param n_samples Integer. Number of samples to draw. Default is 1000.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - sample: Sample identifier
#'   - intercept: Sampled intercept value
#'   - slope: Sampled slope value
#'
#' @family chapter03
#' @export
#'
#' @examples
#' # Simulate data and fit
#' x <- rnorm(20)
#' y <- 0.5 + 0.7 * x + rnorm(20, 0, 0.5)
#' posterior <- compute_linear_posterior(x, y)
#'
#' # Sample from posterior
#' samples <- sample_linear_posterior(posterior, n_samples = 100, seed = 42)
#' 
#' # Visualize posterior samples
#' library(ggplot2)
#' ggplot(samples, aes(x = intercept, y = slope)) +
#'   geom_point(alpha = 0.3) +
#'   theme_rethinking()
sample_linear_posterior <- function(posterior_fit, n_samples = 1000, seed = NULL) {
  checkmate::assert_list(posterior_fit, names = "named")
  checkmate::assert_int(n_samples, lower = 1)
  
  required_names <- c("coefficients", "vcov")
  if (!all(required_names %in% names(posterior_fit))) {
    rlang::abort("posterior_fit must have 'coefficients' and 'vcov' components")
  }
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Sample from multivariate normal
  samples_matrix <- MASS::mvrnorm(
    n = n_samples,
    mu = posterior_fit$coefficients,
    Sigma = posterior_fit$vcov
  )
  
  tibble::tibble(
    sample = seq_len(n_samples),
    intercept = samples_matrix[, 1],
    slope = samples_matrix[, 2]
  )
}


#' Generate Posterior Predictive Lines
#'
#' Create data for visualizing posterior predictive distribution as lines.
#'
#' @param posterior_samples Tibble from \code{sample_linear_posterior()}.
#' @param x_range Numeric vector of length 2. Range of x values.
#' @param n_points Integer. Number of x points to evaluate. Default is 50.
#'
#' @return Tibble with columns:
#'   - sample: Sample identifier
#'   - x: Predictor values
#'   - y: Predicted values
#'
#' @family chapter03
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Fit model and generate samples
#' x <- rnorm(20)
#' y <- 0.5 + 0.7 * x + rnorm(20, 0, 0.5)
#' posterior <- compute_linear_posterior(x, y)
#' samples <- sample_linear_posterior(posterior, n_samples = 50)
#'
#' # Generate posterior predictive lines
#' pred_lines <- generate_posterior_predictive_lines(samples, c(-2, 2))
#'
#' # Visualize
#' ggplot(pred_lines, aes(x = x, y = y, group = sample)) +
#'   geom_line(alpha = 0.2, color = "steelblue") +
#'   theme_rethinking()
generate_posterior_predictive_lines <- function(posterior_samples,
                                               x_range,
                                               n_points = 50) {
  validate_tibble(posterior_samples, 
                 required_cols = c("sample", "intercept", "slope"))
  checkmate::assert_numeric(x_range, len = 2, finite = TRUE)
  checkmate::assert_int(n_points, lower = 2)
  
  x_grid <- seq(x_range[1], x_range[2], length.out = n_points)
  
  posterior_samples %>%
    dplyr::rowwise() %>%
    dplyr::reframe(
      sample = sample,
      x = x_grid,
      y = intercept + slope * x_grid
    )
}


#' Compute Credible Intervals for Predictions
#'
#' Calculate credible intervals for predictions across a range of x values.
#'
#' @param posterior_samples Tibble from \code{sample_linear_posterior()}.
#' @param x_values Numeric vector. X values at which to compute intervals.
#' @param prob Numeric. Probability mass for interval (0 to 1). Default is 0.89.
#'
#' @return Tibble with columns:
#'   - x: Predictor values
#'   - mean: Mean prediction
#'   - lower: Lower bound of credible interval
#'   - upper: Upper bound of credible interval
#'
#' @family chapter03
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Fit model and generate samples
#' x <- rnorm(20)
#' y <- 0.5 + 0.7 * x + rnorm(20, 0, 0.5)
#' posterior <- compute_linear_posterior(x, y)
#' samples <- sample_linear_posterior(posterior, n_samples = 1000)
#'
#' # Compute intervals
#' x_seq <- seq(-2, 2, length.out = 50)
#' intervals <- compute_prediction_intervals(samples, x_seq)
#'
#' # Visualize
#' ggplot(intervals, aes(x = x)) +
#'   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
#'   geom_line(aes(y = mean)) +
#'   theme_rethinking()
compute_prediction_intervals <- function(posterior_samples,
                                        x_values,
                                        prob = 0.89) {
  validate_tibble(posterior_samples, 
                 required_cols = c("intercept", "slope"))
  checkmate::assert_numeric(x_values, min.len = 1, finite = TRUE)
  validate_probability(prob, arg_name = "prob")
  
  # Compute predictions for each sample and x value
  predictions <- purrr::map_dfr(x_values, function(x_val) {
    y_pred <- posterior_samples$intercept + posterior_samples$slope * x_val
    
    tibble::tibble(
      x = x_val,
      mean = mean(y_pred),
      lower = quantile(y_pred, (1 - prob) / 2),
      upper = quantile(y_pred, 1 - (1 - prob) / 2)
    )
  })
  
  predictions
}
