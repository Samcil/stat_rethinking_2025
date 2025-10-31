#' Measurement Error and Missing Data - Chapter 15
#'
#' Functions for handling measurement error in predictors and outcomes,
#' and working with missing data patterns.
#'
#' @name ch15_measurement_error
#' @keywords internal
"_PACKAGE"

#' Simulate Data with Measurement Error
#'
#' Generate data where predictors are measured with error.
#'
#' @param n Number of observations
#' @param intercept True intercept
#' @param slope True effect of predictor on outcome
#' @param true_x_mean Mean of true (unobserved) predictor
#' @param true_x_sd Standard deviation of true predictor
#' @param measurement_error_sd Standard deviation of measurement error
#' @param sigma_resid Residual standard deviation
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with columns: obs_id, true_x, observed_x, y
#'
#' @export
#' @examples
#' # Predictor measured with error
#' data <- simulate_measurement_error(
#'   n = 200,
#'   slope = 2,
#'   measurement_error_sd = 1.5
#' )
simulate_measurement_error <- function(n = 100,
                                      intercept = 0,
                                      slope = 1,
                                      true_x_mean = 0,
                                      true_x_sd = 1,
                                      measurement_error_sd = 0.5,
                                      sigma_resid = 1,
                                      seed = NULL) {
  validate_positive(n, "n")
  validate_positive(true_x_sd, "true_x_sd")
  validate_positive(measurement_error_sd, "measurement_error_sd")
  validate_positive(sigma_resid, "sigma_resid")

  if (!is.null(seed)) set.seed(seed)

  # Generate true predictor
  true_x <- stats::rnorm(n, true_x_mean, true_x_sd)

  # Add measurement error
  observed_x <- true_x + stats::rnorm(n, 0, measurement_error_sd)

  # Generate outcome based on true predictor
  y <- intercept + slope * true_x + stats::rnorm(n, 0, sigma_resid)

  tibble::tibble(
    obs_id = seq_len(n),
    true_x = true_x,
    observed_x = observed_x,
    y = y
  )
}

#' Compute Attenuation Bias
#'
#' Calculate the expected bias in regression coefficient due to measurement error.
#'
#' @param true_slope True effect size
#' @param true_x_var Variance of true predictor
#' @param measurement_error_var Variance of measurement error
#'
#' @return A list with observed_slope and bias_ratio
#'
#' @export
#' @examples
#' # Expected attenuation
#' compute_attenuation_bias(
#'   true_slope = 2,
#'   true_x_var = 1,
#'   measurement_error_var = 0.5
#' )
compute_attenuation_bias <- function(true_slope,
                                    true_x_var,
                                    measurement_error_var) {
  validate_positive(true_x_var, "true_x_var")
  validate_positive(measurement_error_var, "measurement_error_var")

  # Reliability ratio
  reliability <- true_x_var / (true_x_var + measurement_error_var)

  # Observed slope is attenuated by reliability
  observed_slope <- true_slope * reliability

  bias_ratio <- reliability

  list(
    observed_slope = observed_slope,
    bias_ratio = bias_ratio,
    reliability = reliability
  )
}

#' Simulate Missing Completely at Random (MCAR)
#'
#' Introduce MCAR missingness into a dataset.
#'
#' @param data Tibble with complete data
#' @param variables Character vector of variables to make missing
#' @param missing_prob Probability of missingness (0 to 1)
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with NA values introduced
#'
#' @export
#' @examples
#' complete_data <- tibble::tibble(x = rnorm(100), y = rnorm(100))
#' mcar_data <- simulate_mcar(complete_data, c("x", "y"), 0.2)
simulate_mcar <- function(data, variables, missing_prob, seed = NULL) {
  validate_tibble(data, "data")
  validate_probability(missing_prob, "missing_prob")

  if (!all(variables %in% names(data))) {
    rlang::abort("All `variables` must be columns in `data`")
  }

  if (!is.null(seed)) set.seed(seed)

  result <- data

  for (var in variables) {
    n <- nrow(result)
    missing_indices <- sample(seq_len(n), size = floor(n * missing_prob))
    result[[var]][missing_indices] <- NA
  }

  result
}

#' Simulate Missing at Random (MAR)
#'
#' Introduce MAR missingness where probability depends on observed variables.
#'
#' @param data Tibble with complete data
#' @param target_var Variable to make missing
#' @param predictor_var Variable that predicts missingness
#' @param base_prob Baseline probability of missingness
#' @param slope_logit Effect of predictor on log-odds of missingness
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with NA values introduced in target_var
#'
#' @export
#' @examples
#' complete_data <- tibble::tibble(
#'   age = rnorm(100, 50, 10),
#'   income = rnorm(100, 50000, 10000)
#' )
#' # Income more likely missing for older individuals
#' mar_data <- simulate_mar(
#'   complete_data,
#'   target_var = "income",
#'   predictor_var = "age",
#'   slope_logit = 0.05
#' )
simulate_mar <- function(data,
                        target_var,
                        predictor_var,
                        base_prob = 0.2,
                        slope_logit = 1,
                        seed = NULL) {
  validate_tibble(data, "data")
  validate_probability(base_prob, "base_prob")

  if (!target_var %in% names(data)) {
    rlang::abort("`target_var` must be a column in `data`")
  }

  if (!predictor_var %in% names(data)) {
    rlang::abort("`predictor_var` must be a column in `data`")
  }

  if (!is.null(seed)) set.seed(seed)

  # Standardize predictor
  predictor_standardized <- scale(data[[predictor_var]])[, 1]

  # Compute probability of missingness
  logit_prob <- stats::qlogis(base_prob) + slope_logit * predictor_standardized
  prob_missing <- stats::plogis(logit_prob)

  # Generate missingness indicators
  is_missing <- stats::runif(nrow(data)) < prob_missing

  # Apply missingness
  result <- data
  result[[target_var]][is_missing] <- NA

  result
}

#' Compute Multiple Imputation with Chained Equations (MICE) Summary
#'
#' Prepare summary statistics for imputed datasets (simplified version).
#'
#' @param complete_data Tibble with complete (true) data
#' @param incomplete_data Tibble with missing data
#' @param n_imputations Number of imputations to suggest
#'
#' @return A tibble summarizing missingness patterns
#'
#' @export
#' @examples
#' complete <- tibble::tibble(x = rnorm(100), y = rnorm(100))
#' incomplete <- simulate_mcar(complete, c("x", "y"), 0.3)
#' summary <- compute_mice_summary(complete, incomplete)
compute_mice_summary <- function(complete_data,
                                incomplete_data,
                                n_imputations = 5) {
  validate_tibble(complete_data, "complete_data")
  validate_tibble(incomplete_data, "incomplete_data")
  validate_positive(n_imputations, "n_imputations")

  if (nrow(complete_data) != nrow(incomplete_data)) {
    rlang::abort("`complete_data` and `incomplete_data` must have same number of rows")
  }

  # Calculate missingness for each variable
  vars <- names(incomplete_data)

  results <- purrr::map_dfr(vars, function(var) {
    n_missing <- sum(is.na(incomplete_data[[var]]))
    prop_missing <- n_missing / nrow(incomplete_data)

    tibble::tibble(
      variable = var,
      n_missing = n_missing,
      prop_missing = prop_missing,
      n_complete = nrow(incomplete_data) - n_missing
    )
  })

  results |>
    dplyr::filter(.data$n_missing > 0) |>
    dplyr::mutate(recommended_imputations = n_imputations)
}
