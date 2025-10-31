test_that("simulate_measurement_error generates valid data", {
  data <- simulate_measurement_error(
    n = 100,
    slope = 2,
    measurement_error_sd = 0.5,
    seed = 123
  )

  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 100)
  expect_named(data, c("obs_id", "true_x", "observed_x", "y"))

  # Observed x should differ from true x
  expect_false(all(data$true_x == data$observed_x))

  # Check types
  expect_type(data$true_x, "double")
  expect_type(data$observed_x, "double")
  expect_type(data$y, "double")
})

test_that("simulate_measurement_error validation works", {
  expect_error(
    simulate_measurement_error(n = -1),
    "positive"
  )

  expect_error(
    simulate_measurement_error(measurement_error_sd = -0.5),
    "positive"
  )

  expect_error(
    simulate_measurement_error(sigma_resid = 0),
    "positive"
  )
})

test_that("compute_attenuation_bias calculates correctly", {
  result <- compute_attenuation_bias(
    true_slope = 2,
    true_x_var = 1,
    measurement_error_var = 0.5
  )

  expect_type(result, "list")
  expect_named(result, c("observed_slope", "bias_ratio", "reliability"))

  # Reliability should be between 0 and 1
  expect_true(result$reliability > 0 && result$reliability <= 1)

  # Observed slope should be attenuated
  expect_true(result$observed_slope < 2)

  # With no measurement error, observed = true
  result_no_error <- compute_attenuation_bias(2, 1, 1e-10)
  expect_equal(result_no_error$observed_slope, 2, tolerance = 1e-8)
  expect_equal(result_no_error$reliability, 1, tolerance = 1e-8)
})

test_that("compute_attenuation_bias validation works", {
  expect_error(
    compute_attenuation_bias(2, -1, 0.5),
    "positive"
  )

  expect_error(
    compute_attenuation_bias(2, 1, -0.5),
    "positive"
  )
})

test_that("simulate_mcar introduces missingness correctly", {
  complete_data <- tibble::tibble(
    x = rnorm(100),
    y = rnorm(100),
    z = rnorm(100)
  )

  mcar_data <- simulate_mcar(
    complete_data,
    c("x", "y"),
    missing_prob = 0.3,
    seed = 123
  )

  expect_s3_class(mcar_data, "tbl_df")
  expect_equal(nrow(mcar_data), 100)

  # Should have missing values in x and y
  expect_true(any(is.na(mcar_data$x)))
  expect_true(any(is.na(mcar_data$y)))

  # Should not have missing in z
  expect_false(any(is.na(mcar_data$z)))

  # Approximately 30% missing
  prop_missing_x <- sum(is.na(mcar_data$x)) / 100
  expect_true(prop_missing_x > 0.15 && prop_missing_x < 0.45)
})

test_that("simulate_mcar validation works", {
  data <- tibble::tibble(x = rnorm(100))

  expect_error(
    simulate_mcar(data, "y", 0.3),
    "must be columns in `data`"
  )

  expect_error(
    simulate_mcar(data, "x", 1.5),
    "between 0 and 1"
  )

  expect_error(
    simulate_mcar(data, "x", -0.1),
    "between 0 and 1"
  )
})

test_that("simulate_mar introduces systematic missingness", {
  complete_data <- tibble::tibble(
    age = seq(20, 80, length.out = 100),
    income = rnorm(100, 50000, 10000)
  )

  mar_data <- simulate_mar(
    complete_data,
    target_var = "income",
    predictor_var = "age",
    base_prob = 0.2,
    slope_logit = 2,
    seed = 123
  )

  expect_s3_class(mar_data, "tbl_df")
  expect_true(any(is.na(mar_data$income)))
  expect_false(any(is.na(mar_data$age)))

  # Higher age should have more missingness with positive slope
  missing_indicator <- is.na(mar_data$income)
  mean_age_missing <- mean(complete_data$age[missing_indicator])
  mean_age_observed <- mean(complete_data$age[!missing_indicator])

  expect_true(mean_age_missing > mean_age_observed)
})

test_that("simulate_mar validation works", {
  data <- tibble::tibble(x = rnorm(100), y = rnorm(100))

  expect_error(
    simulate_mar(data, "z", "x"),
    "must be a column in `data`"
  )

  expect_error(
    simulate_mar(data, "y", "z"),
    "must be a column in `data`"
  )

  expect_error(
    simulate_mar(data, "y", "x", base_prob = 1.5),
    "between 0 and 1"
  )
})

test_that("compute_mice_summary calculates correctly", {
  complete <- tibble::tibble(
    x = rnorm(100),
    y = rnorm(100),
    z = rnorm(100)
  )

  incomplete <- simulate_mcar(complete, c("x", "y"), 0.3, seed = 123)

  summary <- compute_mice_summary(complete, incomplete, n_imputations = 5)

  expect_s3_class(summary, "tbl_df")
  expect_named(summary, c("variable", "n_missing", "prop_missing", "n_complete", "recommended_imputations"))

  # Should only include variables with missingness
  expect_true(all(summary$variable %in% c("x", "y")))
  expect_false("z" %in% summary$variable)

  # Check values make sense
  expect_true(all(summary$n_missing > 0))
  expect_true(all(summary$prop_missing > 0 & summary$prop_missing <= 1))
  expect_equal(summary$recommended_imputations, rep(5, nrow(summary)))
})

test_that("compute_mice_summary validation works", {
  complete <- tibble::tibble(x = rnorm(100))
  incomplete <- tibble::tibble(x = rnorm(50))

  expect_error(
    compute_mice_summary(complete, incomplete),
    "same number of rows"
  )
})

test_that("reproducibility with seed works", {
  data1 <- simulate_measurement_error(n = 100, seed = 42)
  data2 <- simulate_measurement_error(n = 100, seed = 42)

  expect_equal(data1$true_x, data2$true_x)
  expect_equal(data1$observed_x, data2$observed_x)
  expect_equal(data1$y, data2$y)
})
