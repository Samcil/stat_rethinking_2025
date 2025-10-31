#' Advanced Multilevel Models - Chapter 14
#'
#' Functions for advanced multilevel modeling including cross-classified models,
#' spatial/temporal autocorrelation, and post-stratification.
#'
#' @name ch14_multilevel_advanced
#' @keywords internal
"_PACKAGE"

#' Simulate Cross-Classified Data
#'
#' Generate data with multiple non-nested grouping factors (e.g., students
#' within schools and neighborhoods).
#'
#' @param n_obs Number of observations
#' @param n_groups1 Number of groups in first classification
#' @param n_groups2 Number of groups in second classification
#' @param intercept Overall intercept
#' @param sigma_group1 Standard deviation for group 1 random effects
#' @param sigma_group2 Standard deviation for group 2 random effects
#' @param sigma_resid Residual standard deviation
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with columns: obs_id, group1_id, group2_id, y,
#'   group1_effect, group2_effect
#'
#' @export
#' @examples
#' # Students in schools and neighborhoods
#' data <- simulate_cross_classified(
#'   n_obs = 200,
#'   n_groups1 = 10,  # schools
#'   n_groups2 = 8,   # neighborhoods
#'   sigma_group1 = 1.5,
#'   sigma_group2 = 1.0
#' )
simulate_cross_classified <- function(n_obs = 100,
                                     n_groups1 = 10,
                                     n_groups2 = 10,
                                     intercept = 0,
                                     sigma_group1 = 1,
                                     sigma_group2 = 1,
                                     sigma_resid = 1,
                                     seed = NULL) {
  validate_positive(n_obs, "n_obs")
  validate_positive(n_groups1, "n_groups1")
  validate_positive(n_groups2, "n_groups2")
  validate_positive(sigma_group1, "sigma_group1")
  validate_positive(sigma_group2, "sigma_group2")
  validate_positive(sigma_resid, "sigma_resid")

  if (!is.null(seed)) set.seed(seed)

  # Generate random effects
  group1_effects <- stats::rnorm(n_groups1, 0, sigma_group1)
  group2_effects <- stats::rnorm(n_groups2, 0, sigma_group2)

  # Assign observations to groups
  group1_ids <- sample(seq_len(n_groups1), n_obs, replace = TRUE)
  group2_ids <- sample(seq_len(n_groups2), n_obs, replace = TRUE)

  # Generate outcomes
  y <- intercept +
    group1_effects[group1_ids] +
    group2_effects[group2_ids] +
    stats::rnorm(n_obs, 0, sigma_resid)

  tibble::tibble(
    obs_id = seq_len(n_obs),
    group1_id = group1_ids,
    group2_id = group2_ids,
    y = y,
    group1_effect = group1_effects[group1_ids],
    group2_effect = group2_effects[group2_ids]
  )
}

#' Simulate Temporal Autocorrelation
#'
#' Generate time series data with AR(1) autocorrelation structure.
#'
#' @param n_time Number of time points
#' @param n_groups Number of groups (e.g., individuals)
#' @param intercept Overall intercept
#' @param phi Autocorrelation parameter (between -1 and 1)
#' @param sigma_group Standard deviation for group random effects
#' @param sigma_resid Residual standard deviation
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with columns: time, group_id, y, group_effect, ar_component
#'
#' @export
#' @examples
#' # Time series with AR(1) structure
#' data <- simulate_temporal_autocorrelation(
#'   n_time = 50,
#'   n_groups = 5,
#'   phi = 0.7,  # Strong positive autocorrelation
#'   sigma_group = 1.5
#' )
simulate_temporal_autocorrelation <- function(n_time = 50,
                                             n_groups = 10,
                                             intercept = 0,
                                             phi = 0.5,
                                             sigma_group = 1,
                                             sigma_resid = 1,
                                             seed = NULL) {
  validate_positive(n_time, "n_time")
  validate_positive(n_groups, "n_groups")

  if (abs(phi) >= 1) {
    rlang::abort("`phi` must be between -1 and 1 for stationarity")
  }

  validate_positive(sigma_group, "sigma_group")
  validate_positive(sigma_resid, "sigma_resid")

  if (!is.null(seed)) set.seed(seed)

  # Generate group effects
  group_effects <- stats::rnorm(n_groups, 0, sigma_group)

  # Generate AR(1) process for each group
  results <- purrr::map_dfr(seq_len(n_groups), function(g) {
    ar_component <- numeric(n_time)
    ar_component[1] <- stats::rnorm(1, 0, sigma_resid / sqrt(1 - phi^2))

    for (t in 2:n_time) {
      ar_component[t] <- phi * ar_component[t - 1] +
        stats::rnorm(1, 0, sigma_resid)
    }

    y <- intercept + group_effects[g] + ar_component

    tibble::tibble(
      time = seq_len(n_time),
      group_id = g,
      y = y,
      group_effect = group_effects[g],
      ar_component = ar_component
    )
  })

  results
}

#' Compute Post-Stratification Weights
#'
#' Calculate population weights for post-stratification adjustment.
#'
#' @param sample_data Tibble with sample data containing strata variables
#' @param strata_vars Character vector of column names defining strata
#' @param population_data Tibble with population counts by strata
#'
#' @return A tibble with strata and their population weights
#'
#' @export
#' @examples
#' sample <- tibble::tibble(
#'   age_group = sample(c("young", "old"), 100, replace = TRUE),
#'   region = sample(c("north", "south"), 100, replace = TRUE),
#'   response = rnorm(100)
#' )
#'
#' population <- tibble::tibble(
#'   age_group = rep(c("young", "old"), each = 2),
#'   region = rep(c("north", "south"), 2),
#'   pop_count = c(5000, 3000, 2000, 4000)
#' )
#'
#' weights <- compute_poststrat_weights(
#'   sample, c("age_group", "region"), population
#' )
compute_poststrat_weights <- function(sample_data,
                                     strata_vars,
                                     population_data) {
  validate_tibble(sample_data, "sample_data")
  validate_tibble(population_data, "population_data")

  if (!all(strata_vars %in% names(sample_data))) {
    rlang::abort("All `strata_vars` must be columns in `sample_data`")
  }

  if (!all(strata_vars %in% names(population_data))) {
    rlang::abort("All `strata_vars` must be columns in `population_data`")
  }

  if (!"pop_count" %in% names(population_data)) {
    rlang::abort("`population_data` must contain a `pop_count` column")
  }

  # Calculate total population
  total_pop <- sum(population_data$pop_count)

  # Add population weights
  population_data |>
    dplyr::mutate(weight = .data$pop_count / total_pop) |>
    dplyr::select(dplyr::all_of(c(strata_vars, "pop_count", "weight")))
}

#' Simulate Spatial Autocorrelation
#'
#' Generate spatially correlated data using Gaussian process-like structure.
#'
#' @param n_locations Number of spatial locations
#' @param intercept Overall intercept
#' @param spatial_range Effective range of spatial correlation
#' @param spatial_sigma Marginal standard deviation of spatial process
#' @param sigma_resid Residual standard deviation (nugget effect)
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with columns: location_id, x_coord, y_coord, y, spatial_effect
#'
#' @export
#' @examples
#' # Spatially correlated observations
#' data <- simulate_spatial_autocorrelation(
#'   n_locations = 50,
#'   spatial_range = 0.3,
#'   spatial_sigma = 2,
#'   sigma_resid = 0.5
#' )
simulate_spatial_autocorrelation <- function(n_locations = 50,
                                            intercept = 0,
                                            spatial_range = 0.2,
                                            spatial_sigma = 1,
                                            sigma_resid = 0.5,
                                            seed = NULL) {
  validate_positive(n_locations, "n_locations")
  validate_positive(spatial_range, "spatial_range")
  validate_positive(spatial_sigma, "spatial_sigma")
  validate_positive(sigma_resid, "sigma_resid")

  if (!is.null(seed)) set.seed(seed)

  # Generate random locations
  x_coords <- stats::runif(n_locations, 0, 1)
  y_coords <- stats::runif(n_locations, 0, 1)

  # Compute distance matrix
  dist_matrix <- as.matrix(stats::dist(cbind(x_coords, y_coords)))

  # Exponential correlation function
  cor_matrix <- spatial_sigma^2 * exp(-dist_matrix / spatial_range)
  diag(cor_matrix) <- diag(cor_matrix) + sigma_resid^2

  # Generate spatially correlated values using Cholesky decomposition
  L <- chol(cor_matrix)
  spatial_effects <- as.vector(stats::rnorm(n_locations) %*% L)

  y <- intercept + spatial_effects

  tibble::tibble(
    location_id = seq_len(n_locations),
    x_coord = x_coords,
    y_coord = y_coords,
    y = y,
    spatial_effect = spatial_effects
  )
}
