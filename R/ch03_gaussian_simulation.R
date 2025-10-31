#' Simulate Random Walk to Demonstrate Central Limit Theorem
#'
#' Generate random walk data to illustrate how the sum of random variables
#' converges to a Gaussian distribution (Central Limit Theorem).
#'
#' @param n_individuals Integer. Number of individuals/coins. Default is 1000.
#' @param n_steps Integer. Number of steps/tosses. Default is 16.
#' @param step_values Numeric vector. Possible step values. Default is c(-1, 1).
#' @param step_probs Numeric vector. Probabilities for each step value. 
#'   Default is c(0.5, 0.5).
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - individual: Individual identifier
#'   - step: Step number
#'   - value: Step value taken
#'   - position: Cumulative position
#'
#' @family chapter03
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Simulate random walk
#' walk_data <- simulate_random_walk(n_individuals = 100, n_steps = 16, seed = 42)
#'
#' # Plot final distribution
#' final_positions <- walk_data %>%
#'   dplyr::filter(step == max(step))
#'
#' ggplot(final_positions, aes(x = position)) +
#'   geom_histogram(binwidth = 1, fill = "steelblue") +
#'   theme_rethinking()
simulate_random_walk <- function(n_individuals = 1000, n_steps = 16,
                                step_values = c(-1, 1),
                                step_probs = c(0.5, 0.5),
                                seed = NULL) {
  checkmate::assert_int(n_individuals, lower = 1)
  checkmate::assert_int(n_steps, lower = 1)
  checkmate::assert_numeric(step_values, min.len = 1)
  checkmate::assert_numeric(step_probs, len = length(step_values))
  validate_probability(step_probs)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Check probabilities sum to 1
  if (abs(sum(step_probs) - 1) > 1e-10) {
    rlang::abort("step_probs must sum to 1")
  }
  
  # Generate all steps at once
  steps_matrix <- matrix(
    sample(step_values, size = n_individuals * n_steps, 
           replace = TRUE, prob = step_probs),
    nrow = n_individuals,
    ncol = n_steps
  )
  
  # Calculate cumulative positions
  positions_matrix <- t(apply(steps_matrix, 1, cumsum))
  
  # Convert to long format
  purrr::map_dfr(seq_len(n_individuals), function(i) {
    tibble::tibble(
      individual = i,
      step = seq_len(n_steps),
      value = steps_matrix[i, ],
      position = positions_matrix[i, ]
    )
  })
}


#' Compute Statistics from Random Walk
#'
#' Calculate summary statistics for each step of the random walk to
#' demonstrate convergence to Gaussian distribution.
#'
#' @param walk_data Tibble from \code{simulate_random_walk()}.
#'
#' @return Tibble with columns:
#'   - step: Step number
#'   - mean: Mean position
#'   - sd: Standard deviation of positions
#'   - min: Minimum position
#'   - max: Maximum position
#'   - n: Number of individuals
#'
#' @family chapter03
#' @export
#'
#' @examples
#' walk_data <- simulate_random_walk(n_individuals = 1000, n_steps = 16, seed = 42)
#' stats <- compute_walk_statistics(walk_data)
#'
#' # Plot growth of variance
#' library(ggplot2)
#' ggplot(stats, aes(x = step, y = sd^2)) +
#'   geom_line() +
#'   geom_point() +
#'   labs(y = "Variance", x = "Step")
compute_walk_statistics <- function(walk_data) {
  validate_tibble(walk_data, required_cols = c("step", "position"))
  
  walk_data %>%
    dplyr::group_by(step) %>%
    dplyr::summarise(
      mean = mean(position, na.rm = TRUE),
      sd = sd(position, na.rm = TRUE),
      min = min(position, na.rm = TRUE),
      max = max(position, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
}


#' Test Normality of Random Walk Distribution
#'
#' Perform statistical tests to assess how well the final distribution
#' of random walk positions approximates a normal distribution.
#'
#' @param walk_data Tibble from \code{simulate_random_walk()}.
#' @param final_step_only Logical. Test only the final step? Default is TRUE.
#'
#' @return Tibble with columns:
#'   - step: Step number
#'   - shapiro_statistic: Shapiro-Wilk test statistic
#'   - shapiro_p_value: Shapiro-Wilk p-value
#'   - skewness: Distribution skewness
#'   - kurtosis: Distribution kurtosis
#'
#' @family chapter03
#' @export
#'
#' @examples
#' walk_data <- simulate_random_walk(n_individuals = 1000, n_steps = 16, seed = 42)
#' normality <- test_walk_normality(walk_data, final_step_only = TRUE)
test_walk_normality <- function(walk_data, final_step_only = TRUE) {
  validate_tibble(walk_data, required_cols = c("step", "position"))
  checkmate::assert_logical(final_step_only, len = 1)
  
  if (final_step_only) {
    walk_data <- walk_data %>%
      dplyr::filter(step == max(step))
  }
  
  steps_to_test <- unique(walk_data$step)
  
  purrr::map_dfr(steps_to_test, function(s) {
    positions <- walk_data$position[walk_data$step == s]
    
    # Shapiro-Wilk test (sample if too many observations)
    if (length(positions) > 5000) {
      positions_sample <- sample(positions, 5000)
    } else {
      positions_sample <- positions
    }
    
    shapiro_result <- stats::shapiro.test(positions_sample)
    
    # Calculate moments
    skew <- (mean((positions - mean(positions))^3) / sd(positions)^3)
    kurt <- (mean((positions - mean(positions))^4) / sd(positions)^4) - 3
    
    tibble::tibble(
      step = s,
      shapiro_statistic = shapiro_result$statistic,
      shapiro_p_value = shapiro_result$p.value,
      skewness = skew,
      kurtosis = kurt
    )
  })
}


#' Compare Random Walk to Theoretical Normal
#'
#' Generate comparison data between observed random walk distribution
#' and theoretical normal distribution with matching parameters.
#'
#' @param walk_data Tibble from \code{simulate_random_walk()}.
#' @param step_to_compare Integer. Which step to compare. Default is NULL (final step).
#' @param n_points Integer. Number of points for theoretical curve. Default is 100.
#'
#' @return Tibble with columns:
#'   - x: Position values
#'   - empirical_density: Observed density from random walk
#'   - theoretical_density: Density from matching normal distribution
#'   - source: "empirical" or "theoretical"
#'
#' @family chapter03
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' walk_data <- simulate_random_walk(n_individuals = 1000, n_steps = 16, seed = 42)
#' comparison <- compare_to_normal(walk_data)
#'
#' # Overlay distributions
#' ggplot(comparison, aes(x = x)) +
#'   geom_line(aes(y = empirical_density, color = "Observed")) +
#'   geom_line(aes(y = theoretical_density, color = "Normal"), linetype = "dashed") +
#'   labs(y = "Density", color = "Distribution")
compare_to_normal <- function(walk_data, step_to_compare = NULL, n_points = 100) {
  validate_tibble(walk_data, required_cols = c("step", "position"))
  checkmate::assert_int(n_points, lower = 10)
  
  if (is.null(step_to_compare)) {
    step_to_compare <- max(walk_data$step)
  } else {
    checkmate::assert_int(step_to_compare, lower = 1)
  }
  
  # Get positions for specified step
  positions <- walk_data$position[walk_data$step == step_to_compare]
  
  # Calculate empirical parameters
  emp_mean <- mean(positions)
  emp_sd <- sd(positions)
  
  # Create x grid
  x_range <- range(positions)
  x_extend <- diff(x_range) * 0.1
  x_grid <- seq(x_range[1] - x_extend, x_range[2] + x_extend, length.out = n_points)
  
  # Calculate empirical density
  empirical_dens <- density(positions, n = n_points, from = min(x_grid), to = max(x_grid))
  
  # Calculate theoretical density
  theoretical_dens <- dnorm(x_grid, mean = emp_mean, sd = emp_sd)
  
  tibble::tibble(
    x = x_grid,
    empirical_density = approx(empirical_dens$x, empirical_dens$y, xout = x_grid)$y,
    theoretical_density = theoretical_dens
  )
}
