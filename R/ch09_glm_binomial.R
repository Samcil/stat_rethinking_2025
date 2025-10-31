#' Simulate Binomial Data
#'
#' Generate data from a binomial process with optional predictors
#' using the logit link function.
#'
#' @param n Integer. Number of observations. Default is 100.
#' @param trials Integer or vector. Number of trials per observation. 
#'   Default is 10.
#' @param intercept Numeric. Intercept on log-odds scale. Default is 0.
#' @param slopes Numeric vector. Slopes for predictors on log-odds scale.
#'   Default is NULL (no predictors).
#' @param predictors Matrix. Predictor values. If NULL, generates standard normal.
#'   Default is NULL.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - id: Observation identifier
#'   - trials: Number of trials
#'   - successes: Number of successes
#'   - prob: True probability of success
#'   - Any predictor columns (x1, x2, etc.)
#'
#' @family chapter09
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Simple binomial with no predictors
#' data1 <- simulate_binomial_data(n = 50, trials = 10, intercept = 0, seed = 42)
#'
#' # Binomial with one predictor
#' data2 <- simulate_binomial_data(
#'   n = 100,
#'   trials = 20,
#'   intercept = -1,
#'   slopes = 0.5,
#'   seed = 42
#' )
#'
#' ggplot(data2, aes(x = x1, y = successes / trials)) +
#'   geom_point() +
#'   geom_smooth(method = "glm", method.args = list(family = "binomial"),
#'               formula = cbind(successes, trials - successes) ~ x) +
#'   theme_rethinking()
simulate_binomial_data <- function(n = 100,
                                  trials = 10,
                                  intercept = 0,
                                  slopes = NULL,
                                  predictors = NULL,
                                  seed = NULL) {
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_numeric(intercept, len = 1, finite = TRUE)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Handle trials
  if (length(trials) == 1) {
    trials <- rep(trials, n)
  }
  checkmate::assert_integer(trials, len = n, lower = 1)
  
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
  
  # Calculate log-odds
  log_odds <- rep(intercept, n)
  if (n_predictors > 0) {
    log_odds <- log_odds + as.vector(predictors %*% slopes)
  }
  
  # Convert to probabilities via logistic function
  probs <- plogis(log_odds)
  
  # Generate binomial outcomes
  successes <- rbinom(n, size = trials, prob = probs)
  
  # Create result tibble
  result <- tibble::tibble(
    id = seq_len(n),
    trials = trials,
    successes = successes,
    prob = probs
  )
  
  # Add predictors if present
  if (n_predictors > 0) {
    for (i in seq_len(n_predictors)) {
      result[[paste0("x", i)]] <- predictors[, i]
    }
  }
  
  result
}


#' Compute Logit and Inverse Logit
#'
#' Transform between probability and log-odds scales.
#'
#' @param p Numeric vector. Probabilities (for logit) or log-odds (for inv_logit).
#'
#' @return Numeric vector of transformed values.
#'
#' @family chapter09
#' @export
#'
#' @examples
#' # Probability to log-odds
#' logit(0.5)  # Should be 0
#' logit(c(0.1, 0.5, 0.9))
#'
#' # Log-odds to probability
#' inv_logit(0)  # Should be 0.5
#' inv_logit(c(-2, 0, 2))
#'
#' # Round trip
#' p <- c(0.1, 0.5, 0.9)
#' inv_logit(logit(p))  # Should equal p
logit <- function(p) {
  validate_probability(p, allow_na = FALSE)
  log(p / (1 - p))
}

#' @rdname logit
#' @export
inv_logit <- function(p) {
  checkmate::assert_numeric(p, finite = TRUE)
  plogis(p)
}


#' Sample from Binomial Posterior (Grid Approximation)
#'
#' Use grid approximation to sample from the posterior distribution
#' of a binomial model parameter.
#'
#' @param successes Integer. Number of successes.
#' @param trials Integer. Number of trials.
#' @param prior_fn Function. Prior density function. Default is uniform.
#' @param n_grid Integer. Number of grid points. Default is 100.
#' @param n_samples Integer. Number of samples to draw. Default is 1000.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - p: Probability parameter values (grid)
#'   - prior: Prior density
#'   - likelihood: Likelihood
#'   - posterior: Unnormalized posterior
#'   - posterior_norm: Normalized posterior
#'   And a nested tibble `samples` with posterior samples
#'
#' @family chapter09
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Beta-Binomial with uniform prior
#' result <- sample_binomial_posterior(
#'   successes = 6,
#'   trials = 9,
#'   n_samples = 1000,
#'   seed = 42
#' )
#'
#' # Plot posterior
#' ggplot(result, aes(x = p, y = posterior_norm)) +
#'   geom_line() +
#'   geom_area(alpha = 0.3) +
#'   labs(title = "Posterior Distribution") +
#'   theme_rethinking()
sample_binomial_posterior <- function(successes,
                                     trials,
                                     prior_fn = function(p) rep(1, length(p)),
                                     n_grid = 100,
                                     n_samples = 1000,
                                     seed = NULL) {
  checkmate::assert_int(successes, lower = 0, upper = trials)
  checkmate::assert_int(trials, lower = 1)
  checkmate::assert_function(prior_fn, args = "p")
  checkmate::assert_int(n_grid, lower = 10)
  checkmate::assert_int(n_samples, lower = 1)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Create grid
  p_grid <- seq(0, 1, length.out = n_grid)
  
  # Compute prior
  prior <- prior_fn(p_grid)
  
  # Compute likelihood
  likelihood <- dbinom(successes, size = trials, prob = p_grid)
  
  # Compute posterior
  posterior <- likelihood * prior
  posterior_norm <- posterior / sum(posterior)
  
  # Sample from posterior
  samples <- sample(p_grid, size = n_samples, replace = TRUE, prob = posterior_norm)
  
  list(
    grid = tibble::tibble(
      p = p_grid,
      prior = prior,
      likelihood = likelihood,
      posterior = posterior,
      posterior_norm = posterior_norm
    ),
    samples = tibble::tibble(
      sample = seq_len(n_samples),
      p = samples
    )
  )
}


#' Compute Binomial Credible Intervals
#'
#' Calculate credible intervals from binomial posterior samples.
#'
#' @param posterior_samples Numeric vector. Posterior samples of probability parameter.
#' @param prob Numeric. Probability mass for interval. Default is 0.89.
#'
#' @return Named numeric vector with lower, median, upper bounds.
#'
#' @family chapter09
#' @export
#'
#' @examples
#' # Sample from posterior
#' result <- sample_binomial_posterior(6, 9, n_samples = 5000, seed = 42)
#'
#' # Compute intervals
#' compute_binomial_intervals(result$samples$p, prob = 0.89)
#' compute_binomial_intervals(result$samples$p, prob = 0.95)
compute_binomial_intervals <- function(posterior_samples, prob = 0.89) {
  checkmate::assert_numeric(posterior_samples, min.len = 1)
  validate_probability(posterior_samples, allow_na = FALSE)
  validate_probability(prob, arg_name = "prob")
  
  lower_tail <- (1 - prob) / 2
  upper_tail <- 1 - lower_tail
  
  c(
    lower = quantile(posterior_samples, lower_tail, names = FALSE),
    median = median(posterior_samples),
    upper = quantile(posterior_samples, upper_tail, names = FALSE)
  )
}


#' Compute Log-Odds Ratios
#'
#' Calculate log-odds ratios (effect sizes) from probabilities or counts.
#'
#' @param p1 Numeric. Probability for group 1, or count of successes if n1 provided.
#' @param p2 Numeric. Probability for group 2, or count of successes if n2 provided.
#' @param n1 Integer. Optional. Total trials for group 1 (converts counts to proportions).
#' @param n2 Integer. Optional. Total trials for group 2 (converts counts to proportions).
#'
#' @return Numeric. Log-odds ratio.
#'
#' @family chapter09
#' @export
#'
#' @examples
#' # From probabilities
#' compute_log_odds_ratio(0.3, 0.7)
#'
#' # From counts
#' compute_log_odds_ratio(3, 7, n1 = 10, n2 = 10)
compute_log_odds_ratio <- function(p1, p2, n1 = NULL, n2 = NULL) {
  # Convert counts to proportions if n provided
  if (!is.null(n1)) {
    checkmate::assert_number(p1, lower = 0, upper = n1)
    p1 <- p1 / n1
  }
  if (!is.null(n2)) {
    checkmate::assert_number(p2, lower = 0, upper = n2)
    p2 <- p2 / n2
  }
  
  validate_probability(c(p1, p2))
  
  # Compute log-odds for each group
  log_odds_1 <- logit(p1)
  log_odds_2 <- logit(p2)
  
  # Return difference (log-odds ratio)
  log_odds_2 - log_odds_1
}
