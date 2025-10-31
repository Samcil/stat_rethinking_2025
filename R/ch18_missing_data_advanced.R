#' Advanced Missing Data and Imputation - Chapter 18
#'
#' Functions for sophisticated handling of missing data including
#' non-ignorable missingness and instrumental variables.
#'
#' @name ch18_missing_data_advanced
#' @keywords internal
"_PACKAGE"

#' Simulate Missing Not at Random (MNAR)
#'
#' Generate data where missingness depends on the unobserved value itself.
#'
#' @param n Number of observations
#' @param intercept Intercept for outcome
#' @param slope Slope for predictor effect
#' @param missing_intercept Baseline log-odds of missingness
#' @param missing_slope Effect of outcome value on missingness
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with columns: obs_id, x, y_complete, y_observed, is_missing
#'
#' @export
#' @examples
#' # Higher values more likely to be missing
#' data <- simulate_mnar(
#'   n = 200,
#'   missing_slope = 0.5  # Positive = high values missing
#' )
simulate_mnar <- function(n = 100,
                         intercept = 0,
                         slope = 1,
                         missing_intercept = -1,
                         missing_slope = 1,
                         seed = NULL) {
  validate_positive(n, "n")

  if (!is.null(seed)) set.seed(seed)

  # Generate complete data
  x <- stats::rnorm(n)
  y_complete <- intercept + slope * x + stats::rnorm(n)

  # Missingness depends on y itself (MNAR)
  logit_missing <- missing_intercept + missing_slope * y_complete
  prob_missing <- stats::plogis(logit_missing)
  is_missing <- stats::runif(n) < prob_missing

  # Create observed version
  y_observed <- y_complete
  y_observed[is_missing] <- NA

  tibble::tibble(
    obs_id = seq_len(n),
    x = x,
    y_complete = y_complete,
    y_observed = y_observed,
    is_missing = is_missing
  )
}

#' Compute Selection Bias from MNAR
#'
#' Calculate expected bias in complete-case analysis under MNAR.
#'
#' @param mnar_data Tibble from simulate_mnar
#'
#' @return A list with complete-case and true regression coefficients
#'
#' @export
#' @examples
#' data <- simulate_mnar(n = 500, missing_slope = 1, seed = 123)
#' bias <- compute_selection_bias(data)
compute_selection_bias <- function(mnar_data) {
  validate_tibble(mnar_data, "mnar_data")

  required_cols <- c("x", "y_complete", "y_observed")
  if (!all(required_cols %in% names(mnar_data))) {
    rlang::abort("`mnar_data` must have columns: x, y_complete, y_observed")
  }

  # True regression (on complete data)
  true_model <- stats::lm(y_complete ~ x, data = mnar_data)
  true_coef <- stats::coef(true_model)

  # Complete-case regression (observed only)
  complete_case_model <- stats::lm(y_observed ~ x, data = mnar_data)
  cc_coef <- stats::coef(complete_case_model)

  list(
    true_intercept = true_coef[1],
    true_slope = true_coef[2],
    cc_intercept = cc_coef[1],
    cc_slope = cc_coef[2],
    bias_intercept = cc_coef[1] - true_coef[1],
    bias_slope = cc_coef[2] - true_coef[2],
    prop_missing = mean(is.na(mnar_data$y_observed))
  )
}

#' Simulate Instrumental Variable for Missing Data
#'
#' Generate data with an instrument that predicts missingness but not outcome.
#'
#' @param n Number of observations
#' @param effect_x Effect of X on Y
#' @param effect_instrument Effect of instrument on X
#' @param sigma_x Standard deviation of X
#' @param sigma_y Residual standard deviation for Y
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with columns: obs_id, instrument, x, y
#'
#' @export
#' @examples
#' # Instrument predicts X but not Y directly
#' data <- simulate_instrument(
#'   n = 200,
#'   effect_x = 2,
#'   effect_instrument = 1.5
#' )
simulate_instrument <- function(n = 100,
                               effect_x = 1,
                               effect_instrument = 1,
                               sigma_x = 1,
                               sigma_y = 1,
                               seed = NULL) {
  validate_positive(n, "n")
  validate_positive(sigma_x, "sigma_x")
  validate_positive(sigma_y, "sigma_y")

  if (!is.null(seed)) set.seed(seed)

  # Generate instrument
  instrument <- stats::rnorm(n)

  # X depends on instrument
  x <- effect_instrument * instrument + stats::rnorm(n, 0, sigma_x)

  # Y depends on X but not directly on instrument (exclusion restriction)
  y <- effect_x * x + stats::rnorm(n, 0, sigma_y)

  tibble::tibble(
    obs_id = seq_len(n),
    instrument = instrument,
    x = x,
    y = y
  )
}

#' Compute Two-Stage Least Squares Estimate
#'
#' Implement instrumental variables regression (simplified).
#'
#' @param data Tibble with instrument, x, and y columns
#'
#' @return A list with first-stage and second-stage results
#'
#' @export
#' @examples
#' data <- simulate_instrument(n = 500, effect_x = 2, seed = 123)
#' tsls <- compute_tsls(data)
compute_tsls <- function(data) {
  validate_tibble(data, "data")

  required_cols <- c("instrument", "x", "y")
  if (!all(required_cols %in% names(data))) {
    rlang::abort("`data` must have columns: instrument, x, y")
  }

  # First stage: regress X on instrument
  first_stage <- stats::lm(x ~ instrument, data = data)
  x_predicted <- stats::predict(first_stage)

  # Second stage: regress Y on predicted X
  second_stage_data <- data |>
    dplyr::mutate(x_predicted = x_predicted)

  second_stage <- stats::lm(y ~ x_predicted, data = second_stage_data)

  # Naive OLS for comparison
  naive_ols <- stats::lm(y ~ x, data = data)

  list(
    first_stage_coef = stats::coef(first_stage),
    first_stage_r_squared = summary(first_stage)$r.squared,
    tsls_estimate = stats::coef(second_stage)[2],
    naive_ols_estimate = stats::coef(naive_ols)[2]
  )
}

#' Simulate Censored Data
#'
#' Generate data with censoring (e.g., detection limits, top-coding).
#'
#' @param n Number of observations
#' @param intercept Intercept for outcome
#' @param slope Slope for predictor effect
#' @param lower_threshold Lower censoring threshold (NA for no lower censoring)
#' @param upper_threshold Upper censoring threshold (NA for no upper censoring)
#' @param sigma Residual standard deviation
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with columns: obs_id, x, y_true, y_observed, censored_lower, censored_upper
#'
#' @export
#' @examples
#' # Data censored at detection limit
#' data <- simulate_censored(
#'   n = 200,
#'   lower_threshold = -2,
#'   upper_threshold = 2
#' )
simulate_censored <- function(n = 100,
                             intercept = 0,
                             slope = 1,
                             lower_threshold = NA,
                             upper_threshold = NA,
                             sigma = 1,
                             seed = NULL) {
  validate_positive(n, "n")
  validate_positive(sigma, "sigma")

  if (!is.null(seed)) set.seed(seed)

  # Generate complete data
  x <- stats::rnorm(n)
  y_true <- intercept + slope * x + stats::rnorm(n, 0, sigma)

  # Apply censoring
  y_observed <- y_true
  censored_lower <- rep(FALSE, n)
  censored_upper <- rep(FALSE, n)

  if (!is.na(lower_threshold)) {
    censored_lower <- y_true < lower_threshold
    y_observed[censored_lower] <- lower_threshold
  }

  if (!is.na(upper_threshold)) {
    censored_upper <- y_true > upper_threshold
    y_observed[censored_upper] <- upper_threshold
  }

  tibble::tibble(
    obs_id = seq_len(n),
    x = x,
    y_true = y_true,
    y_observed = y_observed,
    censored_lower = censored_lower,
    censored_upper = censored_upper
  )
}

#' Compute Censoring Summary
#'
#' Summarize patterns of censoring in data.
#'
#' @param censored_data Tibble from simulate_censored
#'
#' @return A list with censoring statistics
#'
#' @export
#' @examples
#' data <- simulate_censored(n = 500, lower_threshold = -2, upper_threshold = 2)
#' summary <- compute_censoring_summary(data)
compute_censoring_summary <- function(censored_data) {
  validate_tibble(censored_data, "censored_data")

  required_cols <- c("y_true", "y_observed", "censored_lower", "censored_upper")
  if (!all(required_cols %in% names(censored_data))) {
    rlang::abort("`censored_data` must have appropriate columns from simulate_censored")
  }

  n <- nrow(censored_data)
  n_lower <- sum(censored_data$censored_lower)
  n_upper <- sum(censored_data$censored_upper)
  n_uncensored <- n - n_lower - n_upper

  list(
    n_total = n,
    n_lower_censored = n_lower,
    n_upper_censored = n_upper,
    n_uncensored = n_uncensored,
    prop_lower_censored = n_lower / n,
    prop_upper_censored = n_upper / n,
    prop_uncensored = n_uncensored / n,
    range_observed = range(censored_data$y_observed),
    range_true = range(censored_data$y_true)
  )
}
