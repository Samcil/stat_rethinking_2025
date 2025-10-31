test_that("simulate_binomial_data generates correct structure", {
  data <- simulate_binomial_data(n = 50, trials = 10, intercept = 0, seed = 42)
  
  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 50)
  expect_true(all(c("id", "trials", "successes", "prob") %in% names(data)))
  
  # Check constraints
  expect_true(all(data$successes >= 0))
  expect_true(all(data$successes <= data$trials))
  expect_true(all(data$prob >= 0 & data$prob <= 1))
})


test_that("simulate_binomial_data handles predictors", {
  data <- simulate_binomial_data(
    n = 100,
    trials = 20,
    intercept = -1,
    slopes = c(0.5, -0.3),
    seed = 42
  )
  
  expect_true("x1" %in% names(data))
  expect_true("x2" %in% names(data))
  
  # Check that x1 correlates positively with successes/trials
  cor1 <- cor(data$x1, data$successes / data$trials)
  expect_true(cor1 > 0.2)
})


test_that("simulate_binomial_data validates inputs", {
  expect_error(simulate_binomial_data(n = 0))
  expect_error(simulate_binomial_data(slopes = 0.5))  # slopes without predictors
  expect_error(simulate_binomial_data(trials = -1))
})


test_that("logit and inv_logit work correctly", {
  # Test specific values
  expect_equal(logit(0.5), 0, tolerance = 1e-10)
  expect_equal(inv_logit(0), 0.5, tolerance = 1e-10)
  
  # Test vectorization
  p_vals <- c(0.1, 0.5, 0.9)
  logit_vals <- logit(p_vals)
  expect_length(logit_vals, 3)
  
  # Test round trip
  expect_equal(inv_logit(logit(p_vals)), p_vals, tolerance = 1e-10)
  
  # Test boundaries
  expect_true(logit(0.01) < logit(0.99))
  expect_true(inv_logit(-10) < 0.01)
  expect_true(inv_logit(10) > 0.99)
})


test_that("logit validates inputs", {
  expect_error(logit(-0.1))
  expect_error(logit(1.5))
  expect_error(logit("a"))
})


test_that("sample_binomial_posterior works correctly", {
  result <- sample_binomial_posterior(
    successes = 6,
    trials = 9,
    n_grid = 100,
    n_samples = 1000,
    seed = 42
  )
  
  expect_type(result, "list")
  expect_true(all(c("grid", "samples") %in% names(result)))
  
  # Check grid
  expect_s3_class(result$grid, "tbl_df")
  expect_equal(nrow(result$grid), 100)
  expect_true(all(c("p", "prior", "likelihood", "posterior") %in% names(result$grid)))
  
  # Check samples
  expect_s3_class(result$samples, "tbl_df")
  expect_equal(nrow(result$samples), 1000)
  
  # Check that posterior sums to 1 (approximately)
  expect_equal(sum(result$grid$posterior_norm), 1, tolerance = 1e-10)
  
  # Check that samples are valid probabilities
  expect_true(all(result$samples$p >= 0 & result$samples$p <= 1))
})


test_that("sample_binomial_posterior uses custom prior", {
  # Beta(2, 2) prior (peaked at 0.5)
  beta_prior <- function(p) dbeta(p, 2, 2)
  
  result <- sample_binomial_posterior(
    successes = 5,
    trials = 10,
    prior_fn = beta_prior,
    n_samples = 1000,
    seed = 42
  )
  
  # Prior should be peaked at 0.5
  max_prior_idx <- which.max(result$grid$prior)
  expect_true(result$grid$p[max_prior_idx] > 0.4 && 
              result$grid$p[max_prior_idx] < 0.6)
})


test_that("sample_binomial_posterior validates inputs", {
  expect_error(sample_binomial_posterior(11, 10))  # successes > trials
  expect_error(sample_binomial_posterior(-1, 10))
  expect_error(sample_binomial_posterior(5, 10, n_grid = 5))
})


test_that("compute_binomial_intervals works correctly", {
  set.seed(42)
  samples <- rbeta(1000, 7, 4)  # Beta(7, 4) samples
  
  intervals <- compute_binomial_intervals(samples, prob = 0.89)
  
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


test_that("compute_binomial_intervals validates inputs", {
  expect_error(compute_binomial_intervals(c()))
  expect_error(compute_binomial_intervals(c(-0.1, 0.5, 1.5)))
  expect_error(compute_binomial_intervals(c(0.5), prob = 1.5))
})


test_that("compute_log_odds_ratio works with probabilities", {
  lor <- compute_log_odds_ratio(0.3, 0.7)
  
  expect_type(lor, "double")
  expect_length(lor, 1)
  
  # Higher p2 should give positive log-odds ratio
  expect_true(lor > 0)
  
  # Equal probabilities should give 0
  lor_equal <- compute_log_odds_ratio(0.5, 0.5)
  expect_equal(lor_equal, 0, tolerance = 1e-10)
})


test_that("compute_log_odds_ratio works with counts", {
  lor <- compute_log_odds_ratio(3, 7, n1 = 10, n2 = 10)
  
  expect_type(lor, "double")
  expect_length(lor, 1)
  expect_true(lor > 0)
  
  # Should match probability version
  lor_prob <- compute_log_odds_ratio(0.3, 0.7)
  expect_equal(lor, lor_prob, tolerance = 1e-10)
})


test_that("compute_log_odds_ratio validates inputs", {
  expect_error(compute_log_odds_ratio(-0.1, 0.5))
  expect_error(compute_log_odds_ratio(1.5, 0.5))
  expect_error(compute_log_odds_ratio(11, 5, n1 = 10))
})
