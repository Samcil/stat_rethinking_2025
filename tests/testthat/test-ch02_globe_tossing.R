test_that("simulate_globe_tosses generates correct structure", {
  tosses <- simulate_globe_tosses(10, prob_water = 0.7, seed = 42)
  
  expect_s3_class(tosses, "tbl_df")
  expect_equal(nrow(tosses), 10)
  expect_true(all(c("toss", "result", "result_numeric", "cumulative_water") %in% names(tosses)))
  
  # Check that results are W or L
  expect_true(all(tosses$result %in% c("W", "L")))
  
  # Check numeric encoding
  expect_true(all(tosses$result_numeric %in% c(0, 1)))
  expect_equal(tosses$result_numeric, ifelse(tosses$result == "W", 1, 0))
  
  # Check cumulative counts
  expect_equal(tosses$cumulative_water[10], sum(tosses$result == "W"))
  expect_equal(tosses$cumulative_land[10], sum(tosses$result == "L"))
})


test_that("simulate_globe_tosses seed works", {
  tosses1 <- simulate_globe_tosses(10, prob_water = 0.5, seed = 123)
  tosses2 <- simulate_globe_tosses(10, prob_water = 0.5, seed = 123)
  tosses3 <- simulate_globe_tosses(10, prob_water = 0.5, seed = 456)
  
  expect_equal(tosses1$result, tosses2$result)
  expect_false(identical(tosses1$result, tosses3$result))
})


test_that("compute_beta_updates works correctly", {
  observations <- c("W", "L", "W", "W")
  posteriors <- compute_beta_updates(observations, prior_alpha = 1, prior_beta = 1)
  
  expect_s3_class(posteriors, "tbl_df")
  expect_equal(nrow(posteriors), 4)
  expect_true(all(c("toss", "alpha", "beta", "posterior_mean") %in% names(posteriors)))
  
  # Check parameter updates
  expect_equal(posteriors$alpha[1], 2)  # 1 + 1 water
  expect_equal(posteriors$beta[1], 1)   # 1 + 0 land
  expect_equal(posteriors$alpha[2], 2)  # 2 + 0 water
  expect_equal(posteriors$beta[2], 2)   # 1 + 1 land
  
  # Check posterior mean
  final_mean <- posteriors$posterior_mean[4]
  expect_equal(final_mean, posteriors$alpha[4] / (posteriors$alpha[4] + posteriors$beta[4]))
})


test_that("compute_beta_updates handles numeric input", {
  observations_num <- c(1, 0, 1, 1)
  observations_char <- c("W", "L", "W", "W")
  
  posteriors_num <- compute_beta_updates(observations_num)
  posteriors_char <- compute_beta_updates(observations_char)
  
  expect_equal(posteriors_num$alpha, posteriors_char$alpha)
  expect_equal(posteriors_num$beta, posteriors_char$beta)
})


test_that("generate_posterior_density creates density data", {
  observations <- c("W", "L", "W")
  posteriors <- compute_beta_updates(observations)
  density_data <- generate_posterior_density(posteriors, n_points = 50)
  
  expect_s3_class(density_data, "tbl_df")
  expect_true(all(c("toss", "p", "density") %in% names(density_data)))
  expect_equal(nrow(density_data), 3 * 50)  # 3 tosses, 50 points each
  
  # Check that p values are between 0 and 1
  expect_true(all(density_data$p >= 0 & density_data$p <= 1))
  
  # Check that densities are non-negative
  expect_true(all(density_data$density >= 0))
})


test_that("sample_posterior_predictive generates samples", {
  samples <- sample_posterior_predictive(
    posterior_alpha = 7,
    posterior_beta = 4,
    n_tosses = 9,
    n_samples = 100,
    seed = 42
  )
  
  expect_s3_class(samples, "tbl_df")
  expect_equal(nrow(samples), 100)
  expect_true(all(c("sample", "p_water", "n_water") %in% names(samples)))
  
  # Check that p_water is between 0 and 1
  expect_true(all(samples$p_water >= 0 & samples$p_water <= 1))
  
  # Check that n_water is between 0 and n_tosses
  expect_true(all(samples$n_water >= 0 & samples$n_water <= 9))
})


test_that("sample_posterior_predictive seed works", {
  samples1 <- sample_posterior_predictive(7, 4, 9, 50, seed = 123)
  samples2 <- sample_posterior_predictive(7, 4, 9, 50, seed = 123)
  samples3 <- sample_posterior_predictive(7, 4, 9, 50, seed = 456)
  
  expect_equal(samples1$n_water, samples2$n_water)
  expect_false(identical(samples1$n_water, samples3$n_water))
})


test_that("compute_posterior_interval works correctly", {
  interval <- compute_posterior_interval(alpha = 7, beta = 4, prob = 0.89)
  
  expect_type(interval, "double")
  expect_length(interval, 2)
  expect_true(all(names(interval) %in% c("lower", "upper")))
  expect_true(interval["lower"] < interval["upper"])
  expect_true(interval["lower"] >= 0 && interval["upper"] <= 1)
  
  # Check that interval contains specified probability mass
  # Approximate check using samples
  samples <- rbeta(10000, 7, 4)
  empirical_prob <- mean(samples >= interval["lower"] & samples <= interval["upper"])
  expect_equal(empirical_prob, 0.89, tolerance = 0.02)
})


test_that("globe tossing functions validate inputs", {
  expect_error(simulate_globe_tosses(-1))
  expect_error(simulate_globe_tosses(10, prob_water = 1.5))
  expect_error(compute_beta_updates(c()))
  expect_error(compute_beta_updates("W", prior_alpha = -1))
  expect_error(sample_posterior_predictive(-1, 4, 9))
  expect_error(compute_posterior_interval(7, 4, prob = 1.5))
})
