test_that("simulate_random_walk generates correct structure", {
  walk_data <- simulate_random_walk(n_individuals = 100, n_steps = 16, seed = 42)
  
  expect_s3_class(walk_data, "tbl_df")
  expect_true(all(c("individual", "step", "value", "position") %in% names(walk_data)))
  
  # Check dimensions
  expect_equal(length(unique(walk_data$individual)), 100)
  expect_equal(max(walk_data$step), 16)
  expect_equal(nrow(walk_data), 100 * 16)
  
  # Check that position is cumulative sum of value
  individual_1 <- walk_data[walk_data$individual == 1, ]
  expect_equal(individual_1$position, cumsum(individual_1$value))
})


test_that("simulate_random_walk uses custom step values", {
  walk_data <- simulate_random_walk(
    n_individuals = 50,
    n_steps = 10,
    step_values = c(-2, 2),
    seed = 42
  )
  
  # All steps should be -2 or 2
  expect_true(all(walk_data$value %in% c(-2, 2)))
})


test_that("simulate_random_walk seed works", {
  walk1 <- simulate_random_walk(n_individuals = 10, n_steps = 5, seed = 123)
  walk2 <- simulate_random_walk(n_individuals = 10, n_steps = 5, seed = 123)
  walk3 <- simulate_random_walk(n_individuals = 10, n_steps = 5, seed = 456)
  
  expect_equal(walk1$value, walk2$value)
  expect_false(identical(walk1$value, walk3$value))
})


test_that("simulate_random_walk validates inputs", {
  expect_error(simulate_random_walk(n_individuals = 0))
  expect_error(simulate_random_walk(n_steps = 0))
  expect_error(simulate_random_walk(step_probs = c(0.5)))  # wrong length
})


test_that("compute_walk_statistics works correctly", {
  walk_data <- simulate_random_walk(n_individuals = 100, n_steps = 10, seed = 42)
  stats <- compute_walk_statistics(walk_data)
  
  expect_s3_class(stats, "tbl_df")
  expect_equal(nrow(stats), 10)
  expect_true(all(c("step", "mean", "sd", "min", "max", "n") %in% names(stats)))
  
  # Check that n is correct
  expect_true(all(stats$n == 100))
  
  # Mean should be close to 0
  expect_true(abs(stats$mean[10]) < 1)
})


test_that("compute_walk_statistics validates inputs", {
  expect_error(compute_walk_statistics(data.frame()))
})


test_that("test_walk_normality works correctly", {
  walk_data <- simulate_random_walk(n_individuals = 1000, n_steps = 16, seed = 42)
  normality <- test_walk_normality(walk_data, final_step_only = TRUE)
  
  expect_s3_class(normality, "tbl_df")
  expect_equal(nrow(normality), 1)
  expect_true(all(c("step", "shapiro_statistic", "shapiro_p_value", 
                    "skewness", "kurtosis") %in% names(normality)))
  
  # For large n with many steps, should be approximately normal
  expect_true(normality$shapiro_p_value > 0.05)
  expect_true(abs(normality$skewness) < 0.5)
  expect_true(abs(normality$kurtosis) < 1)
})


test_that("test_walk_normality handles multiple steps", {
  walk_data <- simulate_random_walk(n_individuals = 100, n_steps = 5, seed = 42)
  normality <- test_walk_normality(walk_data, final_step_only = FALSE)
  
  expect_equal(nrow(normality), 5)
})


test_that("test_walk_normality validates inputs", {
  expect_error(test_walk_normality(data.frame()))
})


test_that("compare_to_normal works correctly", {
  walk_data <- simulate_random_walk(n_individuals = 500, n_steps = 16, seed = 42)
  comparison <- compare_to_normal(walk_data, n_points = 50)
  
  expect_s3_class(comparison, "tbl_df")
  expect_equal(nrow(comparison), 50)
  expect_true(all(c("x", "empirical_density", "theoretical_density") %in% names(comparison)))
  
  # Densities should be non-negative
  expect_true(all(comparison$empirical_density >= 0, na.rm = TRUE))
  expect_true(all(comparison$theoretical_density >= 0))
})


test_that("compare_to_normal handles step selection", {
  walk_data <- simulate_random_walk(n_individuals = 100, n_steps = 10, seed = 42)
  
  comparison_final <- compare_to_normal(walk_data, step_to_compare = NULL)
  comparison_5 <- compare_to_normal(walk_data, step_to_compare = 5)
  
  expect_s3_class(comparison_final, "tbl_df")
  expect_s3_class(comparison_5, "tbl_df")
})


test_that("compare_to_normal validates inputs", {
  walk_data <- simulate_random_walk(n_individuals = 10, n_steps = 5, seed = 42)
  
  expect_error(compare_to_normal(data.frame()))
  expect_error(compare_to_normal(walk_data, n_points = 5))
})
