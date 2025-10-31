#' Simulate Multilevel Data with Varying Intercepts
#'
#' Generate hierarchical data with group-level varying intercepts.
#'
#' @param n_groups Integer. Number of groups. Default is 20.
#' @param n_per_group Integer or vector. Observations per group. Default is 10.
#' @param grand_mean Numeric. Population mean. Default is 0.
#' @param group_sd Numeric. Standard deviation of group intercepts. Default is 1.
#' @param residual_sd Numeric. Within-group standard deviation. Default is 1.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - group: Group identifier
#'   - obs_id: Observation identifier within group
#'   - group_intercept: True group intercept
#'   - y: Observed outcome
#'
#' @family chapter12
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Simulate varying intercepts
#' data <- simulate_varying_intercepts(
#'   n_groups = 15,
#'   n_per_group = 20,
#'   group_sd = 0.5,
#'   seed = 42
#' )
#'
#' # Plot by group
#' ggplot(data, aes(x = factor(group), y = y)) +
#'   geom_jitter(width = 0.2, alpha = 0.5) +
#'   stat_summary(fun = mean, geom = "point", color = "red", size = 3) +
#'   labs(x = "Group", y = "Outcome") +
#'   theme_rethinking()
simulate_varying_intercepts <- function(n_groups = 20,
                                       n_per_group = 10,
                                       grand_mean = 0,
                                       group_sd = 1,
                                       residual_sd = 1,
                                       seed = NULL) {
  checkmate::assert_int(n_groups, lower = 2)
  checkmate::assert_number(grand_mean, finite = TRUE)
  validate_positive(c(group_sd, residual_sd), allow_zero = FALSE)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Handle n_per_group
  if (length(n_per_group) == 1) {
    n_per_group <- rep(n_per_group, n_groups)
  }
  checkmate::assert_integer(n_per_group, len = n_groups, lower = 1)
  
  # Generate group intercepts
  group_intercepts <- rnorm(n_groups, grand_mean, group_sd)
  
  # Generate observations for each group
  purrr::map2_dfr(seq_len(n_groups), group_intercepts, function(grp, intercept) {
    n <- n_per_group[grp]
    y <- rnorm(n, mean = intercept, sd = residual_sd)
    
    tibble::tibble(
      group = grp,
      obs_id = seq_len(n),
      group_intercept = intercept,
      y = y
    )
  })
}


#' Simulate Multilevel Data with Varying Slopes
#'
#' Generate hierarchical data with group-level varying slopes.
#'
#' @param n_groups Integer. Number of groups. Default is 20.
#' @param n_per_group Integer or vector. Observations per group. Default is 10.
#' @param grand_intercept Numeric. Population intercept. Default is 0.
#' @param grand_slope Numeric. Population slope. Default is 1.
#' @param slope_sd Numeric. Standard deviation of group slopes. Default is 0.5.
#' @param residual_sd Numeric. Within-group standard deviation. Default is 1.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - group: Group identifier
#'   - obs_id: Observation identifier within group
#'   - x: Predictor variable
#'   - group_slope: True group slope
#'   - y: Observed outcome
#'
#' @family chapter12
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Simulate varying slopes
#' data <- simulate_varying_slopes(
#'   n_groups = 10,
#'   n_per_group = 30,
#'   grand_slope = 0.5,
#'   slope_sd = 0.3,
#'   seed = 42
#' )
#'
#' # Plot by group
#' ggplot(data, aes(x = x, y = y, color = factor(group))) +
#'   geom_point(alpha = 0.5) +
#'   geom_smooth(method = "lm", se = FALSE) +
#'   labs(color = "Group") +
#'   theme_rethinking()
simulate_varying_slopes <- function(n_groups = 20,
                                   n_per_group = 10,
                                   grand_intercept = 0,
                                   grand_slope = 1,
                                   slope_sd = 0.5,
                                   residual_sd = 1,
                                   seed = NULL) {
  checkmate::assert_int(n_groups, lower = 2)
  checkmate::assert_number(grand_intercept, finite = TRUE)
  checkmate::assert_number(grand_slope, finite = TRUE)
  validate_positive(c(slope_sd, residual_sd), allow_zero = FALSE)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Handle n_per_group
  if (length(n_per_group) == 1) {
    n_per_group <- rep(n_per_group, n_groups)
  }
  checkmate::assert_integer(n_per_group, len = n_groups, lower = 1)
  
  # Generate group slopes
  group_slopes <- rnorm(n_groups, grand_slope, slope_sd)
  
  # Generate observations for each group
  purrr::map2_dfr(seq_len(n_groups), group_slopes, function(grp, slope) {
    n <- n_per_group[grp]
    x <- rnorm(n, 0, 1)
    y <- grand_intercept + slope * x + rnorm(n, 0, residual_sd)
    
    tibble::tibble(
      group = grp,
      obs_id = seq_len(n),
      x = x,
      group_slope = slope,
      y = y
    )
  })
}


#' Simulate Multilevel Data with Correlated Random Effects
#'
#' Generate hierarchical data with correlated varying intercepts and slopes.
#'
#' @param n_groups Integer. Number of groups. Default is 20.
#' @param n_per_group Integer. Observations per group. Default is 10.
#' @param grand_intercept Numeric. Population intercept. Default is 0.
#' @param grand_slope Numeric. Population slope. Default is 1.
#' @param intercept_sd Numeric. SD of group intercepts. Default is 1.
#' @param slope_sd Numeric. SD of group slopes. Default is 0.5.
#' @param correlation Numeric. Correlation between intercepts and slopes.
#'   Default is 0.
#' @param residual_sd Numeric. Within-group SD. Default is 1.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - group: Group identifier
#'   - obs_id: Observation identifier
#'   - x: Predictor variable
#'   - group_intercept: True group intercept
#'   - group_slope: True group slope
#'   - y: Observed outcome
#'
#' @family chapter12
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Simulate with negative correlation
#' data <- simulate_correlated_effects(
#'   n_groups = 15,
#'   correlation = -0.7,
#'   seed = 42
#' )
#'
#' # Check correlation
#' group_effects <- data %>%
#'   dplyr::group_by(group) %>%
#'   dplyr::slice(1) %>%
#'   dplyr::select(group_intercept, group_slope)
#'
#' cor(group_effects$group_intercept, group_effects$group_slope)
simulate_correlated_effects <- function(n_groups = 20,
                                       n_per_group = 10,
                                       grand_intercept = 0,
                                       grand_slope = 1,
                                       intercept_sd = 1,
                                       slope_sd = 0.5,
                                       correlation = 0,
                                       residual_sd = 1,
                                       seed = NULL) {
  checkmate::assert_int(n_groups, lower = 2)
  checkmate::assert_number(grand_intercept, finite = TRUE)
  checkmate::assert_number(grand_slope, finite = TRUE)
  validate_positive(c(intercept_sd, slope_sd, residual_sd), allow_zero = FALSE)
  checkmate::assert_number(correlation, lower = -1, upper = 1)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Create covariance matrix
  cov_matrix <- matrix(
    c(intercept_sd^2, 
      correlation * intercept_sd * slope_sd,
      correlation * intercept_sd * slope_sd, 
      slope_sd^2),
    nrow = 2
  )
  
  # Generate correlated random effects
  effects <- MASS::mvrnorm(n = n_groups, 
                          mu = c(grand_intercept, grand_slope),
                          Sigma = cov_matrix)
  
  # Generate observations for each group
  purrr::map_dfr(seq_len(n_groups), function(grp) {
    intercept <- effects[grp, 1]
    slope <- effects[grp, 2]
    
    x <- rnorm(n_per_group, 0, 1)
    y <- intercept + slope * x + rnorm(n_per_group, 0, residual_sd)
    
    tibble::tibble(
      group = grp,
      obs_id = seq_len(n_per_group),
      x = x,
      group_intercept = intercept,
      group_slope = slope,
      y = y
    )
  })
}


#' Compute Shrinkage for Multilevel Estimates
#'
#' Calculate the degree of shrinkage (pooling) for multilevel model estimates.
#'
#' @param group_means Numeric vector. Observed group means.
#' @param group_ns Integer vector. Sample sizes per group.
#' @param within_group_sd Numeric. Within-group standard deviation.
#' @param between_group_sd Numeric. Between-group standard deviation.
#'
#' @return Tibble with columns:
#'   - group: Group identifier
#'   - observed_mean: Observed group mean
#'   - grand_mean: Grand mean across groups
#'   - shrinkage_factor: Degree of shrinkage (0 = no pooling, 1 = complete pooling)
#'   - shrunken_estimate: Partially pooled estimate
#'
#' @family chapter12
#' @export
#'
#' @examples
#' # Group means with different sample sizes
#' group_means <- c(2, 3, 5, 4, 1, 6)
#' group_ns <- c(5, 10, 20, 15, 8, 12)
#'
#' shrinkage <- compute_shrinkage(
#'   group_means = group_means,
#'   group_ns = group_ns,
#'   within_group_sd = 2,
#'   between_group_sd = 1
#' )
#'
#' # Small groups shrink more
#' library(ggplot2)
#' ggplot(shrinkage, aes(x = group)) +
#'   geom_point(aes(y = observed_mean, color = "Observed")) +
#'   geom_point(aes(y = shrunken_estimate, color = "Shrunken")) +
#'   geom_hline(aes(yintercept = grand_mean, color = "Grand Mean"),
#'              linetype = "dashed") +
#'   labs(y = "Estimate", color = "Type")
compute_shrinkage <- function(group_means,
                             group_ns,
                             within_group_sd,
                             between_group_sd) {
  validate_matching_lengths(group_means = group_means, group_ns = group_ns)
  checkmate::assert_numeric(group_means, finite = TRUE)
  checkmate::assert_integer(group_ns, lower = 1)
  validate_positive(c(within_group_sd, between_group_sd), allow_zero = FALSE)
  
  n_groups <- length(group_means)
  grand_mean <- mean(group_means)
  
  # Calculate shrinkage factor for each group
  # Based on reliability: tau^2 / (tau^2 + sigma^2/n)
  shrinkage_factors <- vapply(group_ns, function(n) {
    between_group_sd^2 / (between_group_sd^2 + within_group_sd^2 / n)
  }, numeric(1))
  
  # Compute partially pooled estimates
  shrunken_estimates <- shrinkage_factors * group_means + 
                       (1 - shrinkage_factors) * grand_mean
  
  tibble::tibble(
    group = seq_len(n_groups),
    observed_mean = group_means,
    grand_mean = grand_mean,
    shrinkage_factor = shrinkage_factors,
    shrunken_estimate = shrunken_estimates
  )
}


#' Compute Intraclass Correlation (ICC)
#'
#' Calculate the proportion of variance explained by group membership.
#'
#' @param between_group_sd Numeric. Between-group standard deviation.
#' @param within_group_sd Numeric. Within-group standard deviation.
#'
#' @return Numeric. ICC value between 0 and 1.
#'
#' @family chapter12
#' @export
#'
#' @examples
#' # Low ICC (groups are similar)
#' compute_icc(between_group_sd = 0.5, within_group_sd = 2)
#'
#' # High ICC (groups are very different)
#' compute_icc(between_group_sd = 2, within_group_sd = 0.5)
compute_icc <- function(between_group_sd, within_group_sd) {
  validate_positive(c(between_group_sd, within_group_sd), allow_zero = FALSE)
  
  between_var <- between_group_sd^2
  within_var <- within_group_sd^2
  total_var <- between_var + within_var
  
  between_var / total_var
}
