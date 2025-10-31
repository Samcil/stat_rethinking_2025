test_that("simulate_poisson_data generates correct structure", {
  data <- simulate_poisson_data(n = 50, intercept = 1, seed = 42)
  
  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 50)
  expect_true(all(c("id", "count", "lambda", "exposure") %in% names(data)))
  
  # Counts should be non-negative integers
  expect_true(all(data$count >= 0))
  expect_true(all(data$count == floor(data$count)))
  
  # Lambda should be positive
  expect_true(all(data$lambda > 0))
})


test_that("simulate_poisson_data handles predictors", {
  data <- simulate_poisson_data(
    n = 100,
    intercept = 0,
    slopes = c(0.5, -0.3),
    seed = 42
  )
  
  expect_true("x1" %in% names(data))
  expect_true("x2" %in% names(data))
  
  # Check that x1 positively affects counts
  cor1 <- cor(data$x1, data$count)
  expect_true(cor1 > 0.1)
})


test_that("simulate_poisson_data handles exposure", {
  data <- simulate_poisson_data(
    n = 50,
    intercept = 1,
    exposure = 2,
    seed = 42
  )
  
  expect_true(all(data$exposure == 2))
  
  # Variable exposure
  exposures <- runif(50, 0.5, 2)
  data2 <- simulate_poisson_data(
    n = 50,
    intercept = 1,
    exposure = exposures,
    seed = 42
  )
  
  expect_equal(data2$exposure, exposures)
})


test_that("simulate_poisson_data validates inputs", {
  expect_error(simulate_poisson_data(n = 0))
  expect_error(simulate_poisson_data(slopes = 0.5))  # slopes without predictors
  expect_error(simulate_poisson_data(exposure = -1))
})


test_that("simulate_zero_inflated_poisson generates correct structure", {
  data <- simulate_zero_inflated_poisson(n = 200, prob_zero = 0.3, lambda = 5, seed = 42)
  
  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 200)
  expect_true(all(c("id", "count", "structural_zero", "lambda") %in% names(data)))
  
  # structural_zero should be logical
  expect_type(data$structural_zero, "logical")
  
  # All structural zeros should have count 0
  expect_true(all(data$count[data$structural_zero] == 0))
})


test_that("simulate_zero_inflated_poisson has excess zeros", {
  data <- simulate_zero_inflated_poisson(
    n = 1000,
    prob_zero = 0.3,
    lambda = 5,
    seed = 42
  )
  
  # Proportion of zeros should be higher than regular Poisson
  prop_zeros <- mean(data$count == 0)
  expected_poisson_zeros <- dpois(0, lambda = 5)
  
  expect_true(prop_zeros > expected_poisson_zeros + 0.1)
})


test_that("simulate_zero_inflated_poisson validates inputs", {
  expect_error(simulate_zero_inflated_poisson(n = 0))
  expect_error(simulate_zero_inflated_poisson(prob_zero = 1.5))
  expect_error(simulate_zero_inflated_poisson(lambda = -1))
})


test_that("compute_rate_ratio works correctly", {
  # Rate ratio of 2
  rr <- compute_rate_ratio(log(5), log(10))
  expect_equal(rr, 2, tolerance = 1e-10)
  
  # Equal rates
  rr_equal <- compute_rate_ratio(log(3), log(3))
  expect_equal(rr_equal, 1, tolerance = 1e-10)
  
  # Rate ratio less than 1
  rr_less <- compute_rate_ratio(log(10), log(5))
  expect_equal(rr_less, 0.5, tolerance = 1e-10)
})


test_that("compute_rate_ratio validates inputs", {
  expect_error(compute_rate_ratio(Inf, 1))
  expect_error(compute_rate_ratio(1, Inf))
})


test_that("sample_poisson_posterior works correctly", {
  set.seed(42)
  counts <- rpois(30, lambda = 5)
  
  result <- sample_poisson_posterior(counts, n_grid = 50, n_samples = 500, seed = 42)
  
  expect_type(result, "list")
  expect_true(all(c("grid", "samples") %in% names(result)))
  
  # Check grid
  expect_s3_class(result$grid, "tbl_df")
  expect_equal(nrow(result$grid), 50)
  expect_true(all(c("lambda", "log_lambda", "prior", "likelihood", "posterior") %in% 
                  names(result$grid)))
  
  # Check samples
  expect_s3_class(result$samples, "tbl_df")
  expect_equal(nrow(result$samples), 500)
  
  # Posterior should sum to 1
  expect_equal(sum(result$grid$posterior_norm), 1, tolerance = 1e-10)
  
  # Samples should be positive
  expect_true(all(result$samples$lambda > 0))
  
  # Mean of samples should be close to true lambda
  expect_true(mean(result$samples$lambda) > 3 && mean(result$samples$lambda) < 7)
})


test_that("sample_poisson_posterior handles exposure", {
  set.seed(42)
  counts <- rpois(20, lambda = 10)
  exposures <- rep(2, 20)
  
  result <- sample_poisson_posterior(counts, exposure = exposures, 
                                    n_samples = 500, seed = 42)
  
  # With exposure = 2, lambda should be around 5
  expect_true(mean(result$samples$lambda) > 3 && mean(result$samples$lambda) < 7)
})


test_that("sample_poisson_posterior validates inputs", {
  expect_error(sample_poisson_posterior(c(-1, 5, 10)))
  expect_error(sample_poisson_posterior(c(1, 2, 3), n_grid = 5))
  expect_error(sample_poisson_posterior(c(1, 2, 3), exposure = c(1, 2)))
})


test_that("compute_poisson_intervals works correctly", {
  set.seed(42)
  samples <- rpois(1000, lambda = 8) + rnorm(1000, 0, 0.5)
  samples <- pmax(samples, 0)  # Ensure positive
  
  intervals <- compute_poisson_intervals(samples, prob = 0.89)
  
  expect_type(intervals, "double")
  expect_length(intervals, 3)
  expect_true(all(names(intervals) %in% c("lower", "median", "upper")))
  
  # Check ordering
  expect_true(intervals["lower"] < intervals["median"])
  expect_true(intervals["median"] < intervals["upper"])
  
  # Check that interval contains roughly 89% of samples
  in_interval <- sum(samples >= intervals["lower"] & 
                     samples <= intervals["upper"])
  expect_true(in_interval / length(samples) > 0.85)
  expect_true(in_interval / length(samples) < 0.93)
})


test_that("compute_poisson_intervals validates inputs", {
  expect_error(compute_poisson_intervals(c()))
  expect_error(compute_poisson_intervals(c(1, 2, 3), prob = 1.5))
})
