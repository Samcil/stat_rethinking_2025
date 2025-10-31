test_that("generate_prior_predictive_linear works correctly", {
  prior_lines <- generate_prior_predictive_linear(
    n_samples = 10,
    prior_intercept = c(0, 1),
    prior_slope = c(0, 1),
    seed = 42
  )
  
  expect_s3_class(prior_lines, "tbl_df")
  expect_true(all(c("sample", "x", "y", "intercept", "slope") %in% names(prior_lines)))
  
  # Check number of samples
  expect_equal(length(unique(prior_lines$sample)), 10)
  
  # Check that lines follow y = a + b*x
  sample1 <- prior_lines[prior_lines$sample == 1, ]
  expect_equal(
    sample1$y,
    sample1$intercept[1] + sample1$slope[1] * sample1$x,
    tolerance = 1e-10
  )
})


test_that("generate_prior_predictive_linear validates inputs", {
  expect_error(generate_prior_predictive_linear(n_samples = 0))
  expect_error(generate_prior_predictive_linear(prior_intercept = c(0)))
  expect_error(generate_prior_predictive_linear(prior_slope = c(0, -1)))
})


test_that("compute_linear_posterior works correctly", {
  set.seed(42)
  x <- rnorm(20)
  y <- 0.5 + 0.7 * x + rnorm(20, 0, 0.5)
  
  posterior <- compute_linear_posterior(
    x, y,
    prior_intercept = c(0, 10),
    prior_slope = c(0, 10)
  )
  
  expect_type(posterior, "list")
  expect_true(all(c("coefficients", "vcov", "sigma") %in% names(posterior)))
  expect_length(posterior$coefficients, 2)
  expect_equal(names(posterior$coefficients), c("intercept", "slope"))
  
  # Check that posterior mean is reasonable
  expect_true(posterior$coefficients["intercept"] > -1 && 
              posterior$coefficients["intercept"] < 2)
  expect_true(posterior$coefficients["slope"] > 0 && 
              posterior$coefficients["slope"] < 1.5)
})


test_that("compute_linear_posterior validates inputs", {
  expect_error(compute_linear_posterior(1:5, 1:4))
  expect_error(compute_linear_posterior(1:5, 1:5, prior_intercept = c(0)))
  expect_error(compute_linear_posterior(1:5, 1:5, sigma_fixed = -1))
})


test_that("sample_linear_posterior works correctly", {
  set.seed(42)
  x <- rnorm(20)
  y <- 0.5 + 0.7 * x + rnorm(20, 0, 0.5)
  
  posterior <- compute_linear_posterior(x, y)
  samples <- sample_linear_posterior(posterior, n_samples = 100, seed = 42)
  
  expect_s3_class(samples, "tbl_df")
  expect_equal(nrow(samples), 100)
  expect_true(all(c("sample", "intercept", "slope") %in% names(samples)))
  
  # Check that means are close to posterior means
  expect_equal(mean(samples$intercept), 
               posterior$coefficients["intercept"], 
               tolerance = 0.1)
  expect_equal(mean(samples$slope), 
               posterior$coefficients["slope"], 
               tolerance = 0.1)
})


test_that("sample_linear_posterior validates inputs", {
  expect_error(sample_linear_posterior(list(a = 1)))
  expect_error(sample_linear_posterior(list(coefficients = 1:2), n_samples = 0))
})


test_that("generate_posterior_predictive_lines works correctly", {
  samples <- tibble::tibble(
    sample = 1:5,
    intercept = c(0, 0.5, 1, 1.5, 2),
    slope = c(0.5, 0.6, 0.7, 0.8, 0.9)
  )
  
  pred_lines <- generate_posterior_predictive_lines(samples, c(-1, 1), n_points = 10)
  
  expect_s3_class(pred_lines, "tbl_df")
  expect_true(all(c("sample", "x", "y") %in% names(pred_lines)))
  expect_equal(nrow(pred_lines), 5 * 10)  # 5 samples, 10 points each
  
  # Check that predictions follow y = a + b*x
  sample1 <- pred_lines[pred_lines$sample == 1, ]
  expect_equal(
    sample1$y,
    samples$intercept[1] + samples$slope[1] * sample1$x,
    tolerance = 1e-10
  )
})


test_that("generate_posterior_predictive_lines validates inputs", {
  samples <- tibble::tibble(sample = 1, intercept = 0, slope = 1)
  expect_error(generate_posterior_predictive_lines(samples, c(-1)))
  expect_error(generate_posterior_predictive_lines(data.frame(), c(-1, 1)))
})


test_that("compute_prediction_intervals works correctly", {
  set.seed(42)
  samples <- tibble::tibble(
    sample = 1:100,
    intercept = rnorm(100, 0, 0.1),
    slope = rnorm(100, 1, 0.1)
  )
  
  x_values <- c(-1, 0, 1)
  intervals <- compute_prediction_intervals(samples, x_values, prob = 0.89)
  
  expect_s3_class(intervals, "tbl_df")
  expect_equal(nrow(intervals), 3)
  expect_true(all(c("x", "mean", "lower", "upper") %in% names(intervals)))
  
  # Check that intervals contain the mean
  expect_true(all(intervals$lower < intervals$mean))
  expect_true(all(intervals$upper > intervals$mean))
  
  # Check that mean is approximately correct for x=0
  expect_equal(intervals$mean[intervals$x == 0], 0, tolerance = 0.05)
})


test_that("compute_prediction_intervals validates inputs", {
  samples <- tibble::tibble(intercept = 1, slope = 1)
  expect_error(compute_prediction_intervals(data.frame(), 1:5))
  expect_error(compute_prediction_intervals(samples, 1:5, prob = 1.5))
})
