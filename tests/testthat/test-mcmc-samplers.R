test_that("metropolis_sampler returns tidy tibble with expected columns", {
  std_norm_target <- function(data, params) {
    x <- params[1]
    tibble::tibble(param1 = x, neg_log_prob = 0.5 * x^2)
  }
  draws <- metropolis_sampler(std_norm_target, data = NULL, init = 0, n_samples = 100, step = 1, chains = 2, seed = 1)
  expect_s3_class(draws, "tbl_df")
  expect_true(all(c("chain", "iter", "accept", "param1", "log_prob") %in% names(draws)))
  expect_equal(length(unique(draws$chain)), 2)
  expect_equal(max(draws$iter), 100)
})

test_that("R-hat near 1 and mean ~0 for standard normal", {
  std_norm_target <- function(data, params) {
    x <- params[1]
    tibble::tibble(param1 = x, neg_log_prob = 0.5 * x^2)
  }
  draws <- metropolis_sampler(std_norm_target, data = NULL, init = 0, n_samples = 2000, step = 1, chains = 2, seed = 2)
  # Discard first 200 as burn-in
  draws <- dplyr::filter(draws, iter > 200)
  mu <- mean(draws$param1)
  expect_lt(abs(mu), 0.2)
  rhat <- mcmc_rhat(draws, "param1")
  expect_lt(rhat, 1.1)
  ess <- mcmc_ess(draws, "param1")
  expect_gt(ess, 200)
})

