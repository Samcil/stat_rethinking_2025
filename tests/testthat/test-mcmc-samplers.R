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




test_that("numeric_grad matches analytic gradient for normal_mu_logsigma", {
  set.seed(3)
  y <- rnorm(10)
  dat <- tibble::tibble(y = y)
  q0 <- c(0.2, -0.1)
  g_num <- numeric_grad(normal_mu_logsigma_target, data = dat, params = q0)
  g_ana <- normal_mu_logsigma_gradient(dat, q0)
  expect_equal(g_num[1], as.numeric(g_ana$d_mu), tolerance = 1e-5)
  expect_equal(g_num[2], as.numeric(g_ana$d_log_sigma), tolerance = 1e-5)
})


test_that("metropolis_step returns expected structure", {
  std_norm_target <- function(data, params) {
    x <- params[1]
    tibble::tibble(param1 = x, neg_log_prob = 0.5 * x^2)
  }
  set.seed(1)
  st <- metropolis_step(std_norm_target, data = NULL, current = 0, step = 1)
  expect_true(is.list(st))
  expect_true(is.numeric(st$state))
  expect_true(is.numeric(st$log_prob))
  expect_true(st$accept %in% c(0L, 1L))
  expect_equal(length(st$state), 1)
})
