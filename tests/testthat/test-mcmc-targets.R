finite_diff_grad <- function(f, x, eps = 1e-6) {
  # Central difference gradient
  p <- length(x)
  g <- numeric(p)
  for (i in seq_len(p)) {
    e <- rep(0, p)
    e[i] <- eps
    g[i] <- (f(x + e) - f(x - e)) / (2 * eps)
  }
  g
}

test_that("normal_mu_logsigma_target gradients match finite differences", {
  set.seed(7)
  y <- abs(rnorm(30))
  y <- c(y, -y)
  dat <- tibble::tibble(y = y)

  fU <- function(q) normal_mu_logsigma_target(dat, q)$neg_log_prob
  grad <- normal_mu_logsigma_gradient(dat, c(0.1, -0.2))
  g_fd <- finite_diff_grad(fU, c(0.1, -0.2))

  expect_equal(as.numeric(grad$d_mu), g_fd[1], tolerance = 1e-5)
  expect_equal(as.numeric(grad$d_log_sigma), g_fd[2], tolerance = 1e-5)
})

test_that("normal_sum2d_target gradients match finite differences", {
  set.seed(7)
  y <- abs(rnorm(20))
  y <- c(y, -y)
  dat <- tibble::tibble(y = y)

  fU <- function(q) normal_sum2d_target(dat, q)$neg_log_prob
  grad <- normal_sum2d_gradient(dat, c(0.3, -0.4))
  g_fd <- finite_diff_grad(fU, c(0.3, -0.4))

  expect_equal(as.numeric(grad$d_a1), g_fd[1], tolerance = 1e-5)
  expect_equal(as.numeric(grad$d_a2), g_fd[2], tolerance = 1e-5)
})

test_that("targets return tibbles and vectorize over parameter rows", {
  dat <- tibble::tibble(y = rnorm(5))
  params_df <- tibble::tibble(mu = c(0, 1), log_sigma = c(0, 0.5))
  out <- normal_mu_logsigma_target(dat, params_df)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 2)
})

test_that("input validation errors for malformed params", {
  dat <- tibble::tibble(y = rnorm(3))
  expect_error(normal_mu_logsigma_target(dat, params = list(1, 2)))
  expect_error(normal_sum2d_target(dat, params = tibble::tibble(a1 = 1)))
})




test_that("nlp_* wrappers match underlying functions", {
  dat <- tibble::tibble(y = rnorm(5))
  expect_equal(
    nlp_gaussian_mu_log_sigma(dat, c(0, 0))$neg_log_prob,
    normal_mu_logsigma_target(dat, c(0, 0))$neg_log_prob
  )
  expect_equal(
    grad_nlp_gaussian_mu_log_sigma(dat, c(0.1, -0.2)) |> dplyr::select(d_mu, d_log_sigma),
    normal_mu_logsigma_gradient(dat, c(0.1, -0.2)) |> dplyr::select(d_mu, d_log_sigma)
  )
  expect_equal(
    nlp_two_param_additive(dat, c(0, 0))$neg_log_prob,
    normal_sum2d_target(dat, c(0, 0))$neg_log_prob
  )
  expect_equal(
    grad_nlp_two_param_additive(dat, c(0.3, -0.4)) |> dplyr::select(d_a1, d_a2),
    normal_sum2d_gradient(dat, c(0.3, -0.4)) |> dplyr::select(d_a1, d_a2)
  )
})


test_that("funnel gradients match finite differences", {
  fU <- function(q) funnel_nlp(NULL, q)$neg_log_prob
  q0 <- c(0.25, -0.1) # (x, v)
  g_fd <- finite_diff_grad(fU, q0)
  g_an <- funnel_grad(NULL, q0)
  expect_equal(as.numeric(g_an$d_x), g_fd[1], tolerance = 1e-5)
  expect_equal(as.numeric(g_an$d_v), g_fd[2], tolerance = 1e-5)
})
