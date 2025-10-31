test_that("compute_squared_exponential generates valid covariance", {
  x <- seq(0, 5, length.out = 10)
  K <- compute_squared_exponential(x, eta_sq = 1, rho_sq = 1)

  expect_type(K, "double")
  expect_equal(dim(K), c(10, 10))

  # Should be symmetric
  expect_equal(K, t(K))

  # Should be positive definite (all eigenvalues positive)
  eigenvalues <- eigen(K, only.values = TRUE)$values
  expect_true(all(eigenvalues > 0))

  # Diagonal should be close to eta_sq (plus nugget)
  expect_true(all(abs(diag(K) - 1) < 1e-6))

  # Nearby points should be more correlated
  expect_true(K[1, 2] > K[1, 10])
})

test_that("compute_squared_exponential validation works", {
  x <- 1:10

  expect_error(
    compute_squared_exponential(x, eta_sq = -1),
    "positive"
  )

  expect_error(
    compute_squared_exponential(x, rho_sq = 0),
    "positive"
  )

  expect_error(
    compute_squared_exponential(x, sigma_sq = -0.1),
    "non-negative"
  )
})

test_that("compute_periodic_covariance generates valid covariance", {
  x <- seq(0, 10, length.out = 20)
  K <- compute_periodic_covariance(x, period = 2, eta_sq = 1, rho_sq = 1)

  expect_type(K, "double")
  expect_equal(dim(K), c(20, 20))

  # Should be symmetric
  expect_equal(K, t(K))

  # Should be positive definite
  eigenvalues <- eigen(K, only.values = TRUE)$values
  expect_true(all(eigenvalues > 0))
})

test_that("compute_periodic_covariance validation works", {
  x <- 1:10

  expect_error(
    compute_periodic_covariance(x, period = -1),
    "positive"
  )

  expect_error(
    compute_periodic_covariance(x, eta_sq = 0),
    "positive"
  )
})

test_that("sample_gp_prior generates valid samples", {
  x <- seq(0, 5, length.out = 50)
  samples <- sample_gp_prior(
    x,
    covariance_fn = "squared_exp",
    n_samples = 3,
    eta_sq = 1,
    rho_sq = 1,
    seed = 123
  )

  expect_s3_class(samples, "tbl_df")
  expect_named(samples, c("x", "sample_id", "y"))
  expect_equal(nrow(samples), 50 * 3)

  # Check sample IDs
  expect_equal(sort(unique(samples$sample_id)), 1:3)

  # Each sample should have all x values
  for (i in 1:3) {
    sample_x <- samples$x[samples$sample_id == i]
    expect_equal(sample_x, x)
  }
})

test_that("sample_gp_prior works with periodic kernel", {
  x <- seq(0, 10, length.out = 50)
  samples <- sample_gp_prior(
    x,
    covariance_fn = "periodic",
    n_samples = 2,
    period = 2,
    seed = 123
  )

  expect_s3_class(samples, "tbl_df")
  expect_equal(nrow(samples), 50 * 2)
})

test_that("sample_gp_prior validation works", {
  x <- 1:10

  expect_error(
    sample_gp_prior(x, n_samples = 0),
    "positive"
  )

  expect_error(
    sample_gp_prior(x, covariance_fn = "invalid"),
    'must be "squared_exp" or "periodic"'
  )
})

test_that("compute_gp_posterior generates valid predictions", {
  # Simple data
  x_obs <- c(1, 3, 5)
  y_obs <- c(1, 2, 1)
  x_pred <- seq(0, 6, length.out = 30)

  posterior <- compute_gp_posterior(
    x_obs, y_obs, x_pred,
    eta_sq = 1, rho_sq = 1, sigma_sq = 0.1
  )

  expect_s3_class(posterior, "tbl_df")
  expect_named(posterior, c("x", "post_mean", "post_var", "post_sd"))
  expect_equal(nrow(posterior), 30)

  # Posterior variance should be positive
  expect_true(all(posterior$post_var > 0))

  # Posterior SD should match sqrt of variance
  expect_equal(posterior$post_sd, sqrt(posterior$post_var))

  # At observed points, uncertainty should be lower
  # (though not zero due to observation noise)
  pred_at_obs <- posterior[posterior$x %in% x_obs, ]
  pred_between <- posterior[!posterior$x %in% x_obs, ]

  # This is a general tendency but not strict for all points
  expect_true(mean(pred_at_obs$post_var) < mean(pred_between$post_var))
})

test_that("compute_gp_posterior validation works", {
  x_obs <- c(1, 2, 3)
  y_obs <- c(1, 2, 3)
  x_pred <- 1:10

  expect_error(
    compute_gp_posterior(x_obs, c(1, 2), x_pred),
    "same length"
  )

  expect_error(
    compute_gp_posterior(x_obs, y_obs, x_pred, eta_sq = -1),
    "positive"
  )

  expect_error(
    compute_gp_posterior(x_obs, y_obs, x_pred, rho_sq = 0),
    "positive"
  )
})

test_that("sample_gp_posterior generates valid samples", {
  posterior_data <- tibble::tibble(
    x = 1:10,
    post_mean = rnorm(10),
    post_var = runif(10, 0.1, 1)
  )

  samples <- sample_gp_posterior(
    posterior_data,
    n_samples = 5,
    seed = 123
  )

  expect_s3_class(samples, "tbl_df")
  expect_named(samples, c("x", "sample_id", "y"))
  expect_equal(nrow(samples), 10 * 5)

  # Check sample IDs
  expect_equal(sort(unique(samples$sample_id)), 1:5)
})

test_that("sample_gp_posterior validation works", {
  posterior_data <- tibble::tibble(
    x = 1:10,
    post_mean = rnorm(10),
    post_var = runif(10, 0.1, 1)
  )

  expect_error(
    sample_gp_posterior(posterior_data, n_samples = 0),
    "positive"
  )

  invalid_data <- tibble::tibble(x = 1:10, y = rnorm(10))
  expect_error(
    sample_gp_posterior(invalid_data, n_samples = 3),
    "must have columns: x, post_mean, post_var"
  )
})

test_that("reproducibility with seed works", {
  x <- seq(0, 5, length.out = 20)

  samples1 <- sample_gp_prior(x, n_samples = 3, seed = 42)
  samples2 <- sample_gp_prior(x, n_samples = 3, seed = 42)

  expect_equal(samples1$y, samples2$y)
})

test_that("GP workflow integration test", {
  # Generate training data
  x_train <- c(0, 1, 2, 3, 4)
  y_train <- sin(x_train) + rnorm(5, 0, 0.1)

  # Make predictions
  x_test <- seq(-0.5, 4.5, length.out = 50)
  posterior <- compute_gp_posterior(
    x_train, y_train, x_test,
    eta_sq = 1, rho_sq = 0.5, sigma_sq = 0.01
  )

  # Sample from posterior
  samples <- sample_gp_posterior(posterior, n_samples = 10, seed = 123)

  # Check dimensions
  expect_equal(nrow(posterior), 50)
  expect_equal(nrow(samples), 50 * 10)

  # Posterior should have reasonable values
  expect_true(all(is.finite(posterior$post_mean)))
  expect_true(all(is.finite(posterior$post_var)))
})
