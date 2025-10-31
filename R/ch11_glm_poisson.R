#' Simulate Poisson Count Data
#'
#' Generate count data from a Poisson process with optional predictors
#' using the log link function.
#'
#' @param n Integer. Number of observations. Default is 100.
#' @param intercept Numeric. Intercept on log scale. Default is 0.
#' @param slopes Numeric vector. Slopes for predictors on log scale.
#'   Default is NULL (no predictors).
#' @param predictors Matrix. Predictor values. If NULL, generates standard normal.
#'   Default is NULL.
#' @param exposure Numeric or vector. Exposure time/area. Default is 1.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - id: Observation identifier
#'   - count: Observed count
#'   - lambda: True rate parameter
#'   - exposure: Exposure value
#'   - Any predictor columns (x1, x2, etc.)
#'
#' @family chapter11
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Simple Poisson with no predictors
#' data1 <- simulate_poisson_data(n = 100, intercept = 1, seed = 42)
#'
#' # Poisson with predictor
#' data2 <- simulate_poisson_data(
#'   n = 100,
#'   intercept = 1,
#'   slopes = 0.5,
#'   seed = 42
#' )
#'
#' ggplot(data2, aes(x = x1, y = count)) +
#'   geom_point() +
#'   geom_smooth(method = "glm", method.args = list(family = "poisson")) +
#'   theme_rethinking()
simulate_poisson_data <- function(n = 100,
                                 intercept = 0,
                                 slopes = NULL,
                                 predictors = NULL,
                                 exposure = 1,
                                 seed = NULL) {
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_number(intercept, finite = TRUE)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Handle exposure
  if (length(exposure) == 1) {
    exposure <- rep(exposure, n)
  }
  checkmate::assert_numeric(exposure, len = n, lower = 0, finite = TRUE)
  
  # Generate or validate predictors
  n_predictors <- length(slopes)
  if (n_predictors > 0) {
    checkmate::assert_numeric(slopes, finite = TRUE)
    
    if (is.null(predictors)) {
      predictors <- matrix(rnorm(n * n_predictors), nrow = n, ncol = n_predictors)
    } else {
      checkmate::assert_matrix(predictors, nrows = n, ncols = n_predictors)
    }
  }
  
  # Calculate log rate
  log_lambda <- rep(intercept, n) + log(exposure)
  if (n_predictors > 0) {
    log_lambda <- log_lambda + as.vector(predictors %*% slopes)
  }
  
  # Convert to rate
  lambda <- exp(log_lambda)
  
  # Generate Poisson counts
  counts <- rpois(n, lambda = lambda)
  
  # Create result tibble
  result <- tibble::tibble(
    id = seq_len(n),
    count = counts,
    lambda = lambda,
    exposure = exposure
  )
  
  # Add predictors if present
  if (n_predictors > 0) {
    for (i in seq_len(n_predictors)) {
      result[[paste0("x", i)]] <- predictors[, i]
    }
  }
  
  result
}


#' Simulate Zero-Inflated Poisson Data
#'
#' Generate count data with excess zeros from a zero-inflated Poisson model.
#'
#' @param n Integer. Number of observations. Default is 100.
#' @param prob_zero Numeric. Probability of structural zero. Default is 0.3.
#' @param lambda Numeric. Poisson rate parameter for non-zero process. Default is 5.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - id: Observation identifier
#'   - count: Observed count
#'   - structural_zero: Whether this is a structural zero
#'   - lambda: Rate parameter for Poisson process
#'
#' @family chapter11
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Simulate zero-inflated data
#' zip_data <- simulate_zero_inflated_poisson(
#'   n = 200,
#'   prob_zero = 0.4,
#'   lambda = 3,
#'   seed = 42
#' )
#'
#' # Plot distribution
#' ggplot(zip_data, aes(x = count)) +
#'   geom_histogram(binwidth = 1, fill = "steelblue") +
#'   labs(title = "Zero-Inflated Poisson Distribution") +
#'   theme_rethinking()
#'
#' # Compare structural vs. sampling zeros
#' table(count = zip_data$count, structural = zip_data$structural_zero)
simulate_zero_inflated_poisson <- function(n = 100,
                                          prob_zero = 0.3,
                                          lambda = 5,
                                          seed = NULL) {
  checkmate::assert_int(n, lower = 1)
  validate_probability(prob_zero)
  validate_positive(lambda, allow_zero = FALSE)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Generate structural zeros
  structural_zero <- rbinom(n, size = 1, prob = prob_zero) == 1
  
  # Generate Poisson counts for non-structural zeros
  counts <- rpois(n, lambda = lambda)
  
  # Set structural zeros to 0
  counts[structural_zero] <- 0
  
  tibble::tibble(
    id = seq_len(n),
    count = counts,
    structural_zero = structural_zero,
    lambda = lambda
  )
}


#' Compute Poisson Rate Ratios
#'
#' Calculate rate ratios (exponentiated coefficients) for Poisson regression.
#'
#' @param log_rate1 Numeric. Log rate for group 1 or condition 1.
#' @param log_rate2 Numeric. Log rate for group 2 or condition 2.
#'
#' @return Numeric. Rate ratio (rate2 / rate1).
#'
#' @family chapter11
#' @export
#'
#' @examples
#' # Log rates differ by 0.5
#' compute_rate_ratio(log_rate1 = 1, log_rate2 = 1.5)
#'
#' # Rate ratio of 2
#' log_rate1 <- log(5)
#' log_rate2 <- log(10)
#' compute_rate_ratio(log_rate1, log_rate2)  # Should be 2
compute_rate_ratio <- function(log_rate1, log_rate2) {
  checkmate::assert_number(log_rate1, finite = TRUE)
  checkmate::assert_number(log_rate2, finite = TRUE)
  
  exp(log_rate2 - log_rate1)
}


#' Sample from Poisson Posterior (Grid Approximation)
#'
#' Use grid approximation to sample from the posterior distribution
#' of a Poisson rate parameter.
#'
#' @param counts Integer vector. Observed counts.
#' @param exposure Numeric vector. Exposure times/areas. Default is 1 for all.
#' @param prior_fn Function. Prior density function for log(lambda). 
#'   Default is normal(0, 1).
#' @param n_grid Integer. Number of grid points. Default is 100.
#' @param n_samples Integer. Number of samples to draw. Default is 1000.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return List with components:
#'   - grid: Tibble with lambda, log_lambda, prior, likelihood, posterior
#'   - samples: Tibble with posterior samples of lambda
#'
#' @family chapter11
#' @export
#'
#' @examples
#' # Simulate data and fit
#' counts <- rpois(20, lambda = 5)
#' result <- sample_poisson_posterior(counts, n_samples = 1000, seed = 42)
#'
#' # Plot posterior
#' library(ggplot2)
#' ggplot(result$grid, aes(x = lambda, y = posterior_norm)) +
#'   geom_line() +
#'   geom_vline(xintercept = 5, linetype = "dashed", color = "red") +
#'   labs(title = "Posterior Distribution of Lambda") +
#'   theme_rethinking()
sample_poisson_posterior <- function(counts,
                                    exposure = NULL,
                                    prior_fn = function(log_lambda) dnorm(log_lambda, 0, 1),
                                    n_grid = 100,
                                    n_samples = 1000,
                                    seed = NULL) {
  checkmate::assert_integer(counts, lower = 0, min.len = 1)
  checkmate::assert_function(prior_fn, args = "log_lambda")
  checkmate::assert_int(n_grid, lower = 10)
  checkmate::assert_int(n_samples, lower = 1)
  
  n <- length(counts)
  
  if (is.null(exposure)) {
    exposure <- rep(1, n)
  }
  checkmate::assert_numeric(exposure, len = n, lower = 0)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Create grid on log scale
  total_count <- sum(counts)
  total_exposure <- sum(exposure)
  observed_rate <- total_count / total_exposure
  
  # Grid around observed rate (wider range on log scale)
  log_lambda_grid <- seq(
    log(max(0.1, observed_rate / 5)),
    log(observed_rate * 5),
    length.out = n_grid
  )
  lambda_grid <- exp(log_lambda_grid)
  
  # Compute prior
  prior <- prior_fn(log_lambda_grid)
  
  # Compute likelihood
  likelihood <- sapply(lambda_grid, function(lam) {
    prod(dpois(counts, lambda = lam * exposure))
  })
  
  # Compute posterior
  posterior <- likelihood * prior
  posterior_norm <- posterior / sum(posterior)
  
  # Sample from posterior
  samples <- sample(lambda_grid, size = n_samples, replace = TRUE, 
                   prob = posterior_norm)
  
  list(
    grid = tibble::tibble(
      lambda = lambda_grid,
      log_lambda = log_lambda_grid,
      prior = prior,
      likelihood = likelihood,
      posterior = posterior,
      posterior_norm = posterior_norm
    ),
    samples = tibble::tibble(
      sample = seq_len(n_samples),
      lambda = samples
    )
  )
}


#' Compute Poisson Credible Intervals
#'
#' Calculate credible intervals from Poisson posterior samples.
#'
#' @param posterior_samples Numeric vector. Posterior samples of rate parameter.
#' @param prob Numeric. Probability mass for interval. Default is 0.89.
#'
#' @return Named numeric vector with lower, median, upper bounds.
#'
#' @family chapter11
#' @export
#'
#' @examples
#' # Sample from posterior
#' counts <- rpois(50, lambda = 8)
#' result <- sample_poisson_posterior(counts, n_samples = 5000, seed = 42)
#'
#' # Compute intervals
#' compute_poisson_intervals(result$samples$lambda, prob = 0.89)
#' compute_poisson_intervals(result$samples$lambda, prob = 0.95)
compute_poisson_intervals <- function(posterior_samples, prob = 0.89) {
  checkmate::assert_numeric(posterior_samples, min.len = 1, finite = TRUE)
  validate_probability(prob, arg_name = "prob")
  
  lower_tail <- (1 - prob) / 2
  upper_tail <- 1 - lower_tail
  
  c(
    lower = quantile(posterior_samples, lower_tail, names = FALSE),
    median = median(posterior_samples),
    upper = quantile(posterior_samples, upper_tail, names = FALSE)
  )
}
