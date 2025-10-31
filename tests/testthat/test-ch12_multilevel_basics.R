test_that("simulate_varying_intercepts generates correct structure", {
  data <- simulate_varying_intercepts(n_groups = 10, n_per_group = 20, seed = 42)
  
  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 10 * 20)
  expect_true(all(c("group", "obs_id", "group_intercept", "y") %in% names(data)))
  
  # Check groups
  expect_equal(length(unique(data$group)), 10)
  
  # Check observations per group
  group_counts <- table(data$group)
  expect_true(all(group_counts == 20))
})


test_that("simulate_varying_intercepts creates between-group variation", {
  data <- simulate_varying_intercepts(
    n_groups = 20,
    n_per_group = 50,
    group_sd = 2,
    seed = 42
  )
  
  # Group means should vary
  group_means <- data %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(mean_y = mean(y), .groups = "drop")
  
  sd_of_means <- sd(group_means$mean_y)
  expect_true(sd_of_means > 0.5)  # Should have substantial variation
})


test_that("simulate_varying_intercepts handles variable group sizes", {
  group_sizes <- c(5, 10, 15, 20, 25)
  data <- simulate_varying_intercepts(
    n_groups = 5,
    n_per_group = group_sizes,
    seed = 42
  )
  
  expect_equal(nrow(data), sum(group_sizes))
  
  actual_sizes <- data %>%
    dplyr::count(group) %>%
    dplyr::pull(n)
  
  expect_equal(actual_sizes, group_sizes)
})


test_that("simulate_varying_intercepts validates inputs", {
  expect_error(simulate_varying_intercepts(n_groups = 1))
  expect_error(simulate_varying_intercepts(group_sd = -1))
  expect_error(simulate_varying_intercepts(residual_sd = 0))
})


test_that("simulate_varying_slopes generates correct structure", {
  data <- simulate_varying_slopes(n_groups = 10, n_per_group = 30, seed = 42)
  
  expect_s3_class(data, "tbl_df")
  expect_true(all(c("group", "obs_id", "x", "group_slope", "y") %in% names(data)))
  
  # Check that slopes vary
  unique_slopes <- unique(data$group_slope)
  expect_equal(length(unique_slopes), 10)
})


test_that("simulate_varying_slopes creates different slopes", {
  data <- simulate_varying_slopes(
    n_groups = 15,
    grand_slope = 1,
    slope_sd = 0.5,
    n_per_group = 50,
    seed = 42
  )
  
  # Fit simple models per group to check slopes
  group_slopes <- data %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
      slope = stats::coef(lm(y ~ x))[2],
      .groups = "drop"
    )
  
  # Should have variation in slopes
  expect_true(sd(group_slopes$slope) > 0.2)
})


test_that("simulate_varying_slopes validates inputs", {
  expect_error(simulate_varying_slopes(n_groups = 1))
  expect_error(simulate_varying_slopes(slope_sd = -1))
})


test_that("simulate_correlated_effects generates correlated effects", {
  data <- simulate_correlated_effects(
    n_groups = 50,
    n_per_group = 30,
    correlation = 0.7,
    seed = 42
  )
  
  expect_s3_class(data, "tbl_df")
  expect_true(all(c("group_intercept", "group_slope") %in% names(data)))
  
  # Extract group-level effects
  group_effects <- data %>%
    dplyr::group_by(group) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  # Check correlation
  observed_cor <- cor(group_effects$group_intercept, group_effects$group_slope)
  expect_true(abs(observed_cor - 0.7) < 0.3)  # Should be roughly 0.7
})


test_that("simulate_correlated_effects handles zero correlation", {
  data <- simulate_correlated_effects(
    n_groups = 100,
    correlation = 0,
    seed = 42
  )
  
  group_effects <- data %>%
    dplyr::group_by(group) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  observed_cor <- cor(group_effects$group_intercept, group_effects$group_slope)
  expect_true(abs(observed_cor) < 0.3)  # Should be close to 0
})


test_that("simulate_correlated_effects validates inputs", {
  expect_error(simulate_correlated_effects(correlation = 1.5))
  expect_error(simulate_correlated_effects(correlation = -1.5))
  expect_error(simulate_correlated_effects(intercept_sd = -1))
})


test_that("compute_shrinkage works correctly", {
  group_means <- c(2, 3, 5, 4, 1, 6)
  group_ns <- c(5, 10, 20, 15, 8, 12)
  
  shrinkage <- compute_shrinkage(
    group_means = group_means,
    group_ns = group_ns,
    within_group_sd = 2,
    between_group_sd = 1
  )
  
  expect_s3_class(shrinkage, "tbl_df")
  expect_equal(nrow(shrinkage), 6)
  expect_true(all(c("observed_mean", "shrunken_estimate", "shrinkage_factor") %in% 
                  names(shrinkage)))
  
  # Shrinkage factors should be between 0 and 1
  expect_true(all(shrinkage$shrinkage_factor >= 0))
  expect_true(all(shrinkage$shrinkage_factor <= 1))
  
  # Shrunken estimates should be between observed and grand mean
  for (i in seq_len(nrow(shrinkage))) {
    obs <- shrinkage$observed_mean[i]
    shrunken <- shrinkage$shrunken_estimate[i]
    grand <- shrinkage$grand_mean[i]
    
    if (obs > grand) {
      expect_true(shrunken <= obs)
      expect_true(shrunken >= grand)
    } else {
      expect_true(shrunken >= obs)
      expect_true(shrunken <= grand)
    }
  }
})


test_that("compute_shrinkage shows more shrinkage for small groups", {
  group_means <- rep(5, 3)
  group_ns <- c(5, 50, 500)
  
  shrinkage <- compute_shrinkage(
    group_means = group_means,
    group_ns = group_ns,
    within_group_sd = 2,
    between_group_sd = 1
  )
  
  # Smaller groups should have less shrinkage factor (more pooling)
  expect_true(shrinkage$shrinkage_factor[1] < shrinkage$shrinkage_factor[2])
  expect_true(shrinkage$shrinkage_factor[2] < shrinkage$shrinkage_factor[3])
})


test_that("compute_shrinkage validates inputs", {
  expect_error(compute_shrinkage(c(1, 2), c(5), 1, 1))  # mismatched lengths
  expect_error(compute_shrinkage(c(1, 2), c(5, 10), -1, 1))  # negative SD
})


test_that("compute_icc works correctly", {
  # Equal variances
  icc1 <- compute_icc(between_group_sd = 1, within_group_sd = 1)
  expect_equal(icc1, 0.5, tolerance = 1e-10)
  
  # All variance between groups
  icc2 <- compute_icc(between_group_sd = 2, within_group_sd = 0.0001)
  expect_true(icc2 > 0.99)
  
  # All variance within groups
  icc3 <- compute_icc(between_group_sd = 0.0001, within_group_sd = 2)
  expect_true(icc3 < 0.01)
})


test_that("compute_icc returns value between 0 and 1", {
  icc <- compute_icc(between_group_sd = 1.5, within_group_sd = 2.3)
  expect_true(icc >= 0)
  expect_true(icc <= 1)
})


test_that("compute_icc validates inputs", {
  expect_error(compute_icc(-1, 1))
  expect_error(compute_icc(1, -1))
  expect_error(compute_icc(0, 1))
})
