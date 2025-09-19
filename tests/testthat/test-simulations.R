test_that("simulate_lynx_hare basic dynamics produce expected first steps", {
  sim <- simulate_lynx_hare(n_steps = 3, init = c(10, 20), theta = c(0.5, 0.05, 0.025, 0.5), dt = 0.01)
  expect_s3_class(sim, "tbl_df")
  expect_equal(nrow(sim), 3)
  # manual step 2 values
  L1 <- 10; H1 <- 20; dt <- 0.01; th <- c(0.5, 0.05, 0.025, 0.5)
  H2 <- H1 + dt * H1 * (th[1] - th[2] * L1)
  L2 <- L1 + dt * L1 * (th[3] * H1 - th[4])
  expect_equal(sim$H[2], H2)
  expect_equal(sim$L[2], L2)
})

test_that("simulate_beta_binomial returns valid counts and probabilities", {
  set.seed(123)
  pp <- simulate_beta_binomial(n_draws = 2000, size = 9, alpha = 7, beta = 4)
  expect_true(all(pp$w >= 0 & pp$w <= 9))
  expect_true(all(pp$p >= 0 & pp$p <= 1))
  # mean(p) approx alpha/(alpha+beta)
  expect_equal(mean(pp$p), 7/(7+4), tolerance = 0.05)
})

test_that("simulate_bad_controls_post_treatment correlations have expected signs", {
  set.seed(42)
  d <- simulate_bad_controls_post_treatment(n = 5000, bXZ = 1, bZY = 1)
  # X and Z positively associated
  expect_gt(cor(d$X, d$Z), 0.4)
  # Z and Y positively associated
  expect_gt(cor(d$Z, d$Y), 0.4)
})

