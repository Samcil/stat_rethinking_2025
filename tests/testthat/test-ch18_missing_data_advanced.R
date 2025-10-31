test_that("simulate_mnar generates valid data", {
  data <- simulate_mnar(
    n = 100,
    missing_slope = 1,
    seed = 123
  )

  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 100)
  expect_named(data, c("obs_id", "x", "y_complete", "y_observed", "is_missing"))

  # Should have some missing values
  expect_true(any(is.na(data$y_observed)))

  # Complete should never be NA
  expect_false(any(is.na(data$y_complete)))

  # is_missing should match NA in y_observed
  expect_equal(data$is_missing, is.na(data$y_observed))

  # With positive missing_slope, higher values more likely missing
  missing_mean <- mean(data$y_complete[data$is_missing])
  observed_mean <- mean(data$y_complete[!data$is_missing])
  expect_true(missing_mean > observed_mean)
})

test_that("simulate_mnar validation works", {
  expect_error(
    simulate_mnar(n = -1),
    "positive"
  )
})

test_that("compute_selection_bias calculates correctly", {
  data <- simulate_mnar(
    n = 500,
    slope = 2,
    missing_slope = 1,
    seed = 123
  )

  bias <- compute_selection_bias(data)

  expect_type(bias, "list")
  expect_named(bias, c(
    "true_intercept", "true_slope",
    "cc_intercept", "cc_slope",
    "bias_intercept", "bias_slope",
    "prop_missing"
  ))

  # True slope should be close to 2
  expect_equal(bias$true_slope, 2, tolerance = 0.2)

  # Complete-case slope should be biased
  expect_true(bias$bias_slope != 0)

  # Proportion missing should be reasonable
  expect_true(bias$prop_missing > 0 && bias$prop_missing < 1)
})

test_that("compute_selection_bias validation works", {
  invalid_data <- tibble::tibble(x = rnorm(100), y = rnorm(100))

  expect_error(
    compute_selection_bias(invalid_data),
    "must have columns: x, y_complete, y_observed"
  )
})

test_that("simulate_instrument generates valid data", {
  data <- simulate_instrument(
    n = 200,
    effect_x = 2,
    effect_instrument = 1.5,
    seed = 123
  )

  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 200)
  expect_named(data, c("obs_id", "instrument", "x", "y"))

  # Instrument should correlate with x
  cor_ix <- stats::cor(data$instrument, data$x)
  expect_true(abs(cor_ix) > 0.5)

  # Y should correlate with x
  cor_xy <- stats::cor(data$x, data$y)
  expect_true(abs(cor_xy) > 0.5)
})

test_that("simulate_instrument validation works", {
  expect_error(
    simulate_instrument(n = 0),
    "positive"
  )

  expect_error(
    simulate_instrument(sigma_x = -1),
    "positive"
  )
})

test_that("compute_tsls calculates correctly", {
  data <- simulate_instrument(
    n = 500,
    effect_x = 2,
    effect_instrument = 1.5,
    seed = 123
  )

  tsls <- compute_tsls(data)

  expect_type(tsls, "list")
  expect_named(tsls, c(
    "first_stage_coef", "first_stage_r_squared",
    "tsls_estimate", "naive_ols_estimate"
  ))

  # First stage should show effect of instrument
  expect_true(tsls$first_stage_r_squared > 0.1)

  # TSLS estimate should be close to true effect (2)
  expect_equal(tsls$tsls_estimate, 2, tolerance = 0.5)

  # Named coefficient
  expect_named(tsls$first_stage_coef, c("(Intercept)", "instrument"))
})

test_that("compute_tsls validation works", {
  invalid_data <- tibble::tibble(x = rnorm(100), y = rnorm(100))

  expect_error(
    compute_tsls(invalid_data),
    "must have columns: instrument, x, y"
  )
})

test_that("simulate_censored generates valid data", {
  data <- simulate_censored(
    n = 200,
    lower_threshold = -2,
    upper_threshold = 2,
    seed = 123
  )

  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 200)
  expect_named(data, c(
    "obs_id", "x", "y_true", "y_observed",
    "censored_lower", "censored_upper"
  ))

  # Should have some censored values
  expect_true(any(data$censored_lower) || any(data$censored_upper))

  # Observed should be within thresholds
  expect_true(all(data$y_observed >= -2))
  expect_true(all(data$y_observed <= 2))

  # True can exceed thresholds
  expect_true(any(data$y_true < -2) || any(data$y_true > 2))

  # Censoring indicators should match
  lower_censored <- data$y_true < -2
  expect_equal(data$censored_lower, lower_censored)
})

test_that("simulate_censored with no thresholds works", {
  data <- simulate_censored(
    n = 100,
    lower_threshold = NA,
    upper_threshold = NA,
    seed = 123
  )

  # No censoring should occur
  expect_false(any(data$censored_lower))
  expect_false(any(data$censored_upper))

  # y_observed should equal y_true
  expect_equal(data$y_observed, data$y_true)
})

test_that("simulate_censored validation works", {
  expect_error(
    simulate_censored(n = 0),
    "positive"
  )

  expect_error(
    simulate_censored(sigma = -1),
    "positive"
  )
})

test_that("compute_censoring_summary calculates correctly", {
  data <- simulate_censored(
    n = 500,
    lower_threshold = -1.5,
    upper_threshold = 1.5,
    seed = 123
  )

  summary <- compute_censoring_summary(data)

  expect_type(summary, "list")
  expect_named(summary, c(
    "n_total", "n_lower_censored", "n_upper_censored", "n_uncensored",
    "prop_lower_censored", "prop_upper_censored", "prop_uncensored",
    "range_observed", "range_true"
  ))

  expect_equal(summary$n_total, 500)

  # Proportions should sum to 1
  total_prop <- summary$prop_lower_censored +
    summary$prop_upper_censored +
    summary$prop_uncensored
  expect_equal(total_prop, 1)

  # Observed range should be within thresholds
  expect_true(summary$range_observed[1] >= -1.5)
  expect_true(summary$range_observed[2] <= 1.5)

  # True range can exceed
  expect_true(
    summary$range_true[1] < -1.5 ||
      summary$range_true[2] > 1.5
  )
})

test_that("compute_censoring_summary validation works", {
  invalid_data <- tibble::tibble(x = rnorm(100))

  expect_error(
    compute_censoring_summary(invalid_data),
    "must have appropriate columns"
  )
})

test_that("reproducibility with seed works", {
  data1 <- simulate_mnar(n = 100, seed = 42)
  data2 <- simulate_mnar(n = 100, seed = 42)

  expect_equal(data1$y_complete, data2$y_complete)
  expect_equal(data1$is_missing, data2$is_missing)
})
