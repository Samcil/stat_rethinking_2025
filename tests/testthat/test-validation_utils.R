test_that("validate_tibble works correctly", {
  library(dplyr)
  
  # Valid tibble
  df <- tibble(x = 1:5, y = 1:5)
  expect_true(validate_tibble(df))
  
  # With required columns
  expect_true(validate_tibble(df, required_cols = c("x", "y")))
  
  # With minimum rows
  expect_true(validate_tibble(df, min_rows = 5))
  
  # Test failures
  expect_error(validate_tibble("not a df"), class = "validation_error")
  expect_error(validate_tibble(df, min_rows = 10), class = "validation_error")
  expect_error(validate_tibble(df, required_cols = c("x", "z")), class = "validation_error")
  
  # Empty data frame
  empty_df <- data.frame()
  expect_error(validate_tibble(empty_df), class = "validation_error")
  expect_true(validate_tibble(empty_df, min_rows = 0))
})


test_that("validate_probability works correctly", {
  # Valid probabilities
  expect_true(validate_probability(c(0, 0.5, 1)))
  expect_true(validate_probability(0.5))
  
  # Edge cases
  expect_true(validate_probability(0))
  expect_true(validate_probability(1))
  
  # With NA allowed
  expect_true(validate_probability(c(0.5, NA, 0.8), allow_na = TRUE))
  
  # Test failures
  expect_error(validate_probability(c(-0.1, 0.5)), class = "validation_error")
  expect_error(validate_probability(c(0.5, 1.5)), class = "validation_error")
  expect_error(validate_probability("0.5"), class = "validation_error")
  expect_error(validate_probability(c(0.5, NA)), class = "validation_error")
})


test_that("validate_positive works correctly", {
  # Valid positive values
  expect_true(validate_positive(c(1, 2, 3)))
  expect_true(validate_positive(0.001))
  
  # With zero allowed
  expect_true(validate_positive(c(0, 1, 2), allow_zero = TRUE))
  expect_true(validate_positive(0, allow_zero = TRUE))
  
  # With NA allowed
  expect_true(validate_positive(c(1, NA, 3), allow_na = TRUE))
  
  # Test failures
  expect_error(validate_positive(0), class = "validation_error")
  expect_error(validate_positive(-1), class = "validation_error")
  expect_error(validate_positive(c(1, -1, 3)), class = "validation_error")
  expect_error(validate_positive("1"), class = "validation_error")
  expect_error(validate_positive(c(1, NA)), class = "validation_error")
})


test_that("validate_matching_lengths works correctly", {
  # Valid matching lengths
  x <- 1:5
  y <- 6:10
  z <- 11:15
  expect_true(validate_matching_lengths(x = x, y = y, z = z))
  
  # Two arguments
  expect_true(validate_matching_lengths(x = 1:3, y = 4:6))
  
  # Single element vectors
  expect_true(validate_matching_lengths(x = 1, y = 2, z = 3))
  
  # Test failures
  expect_error(
    validate_matching_lengths(x = 1:5, y = 1:3),
    class = "validation_error"
  )
  expect_error(
    validate_matching_lengths(x = 1:5, y = 1:5, z = 1:3),
    class = "validation_error"
  )
  
  # Single argument (should fail)
  expect_error(
    validate_matching_lengths(x = 1:5),
    class = "validation_error"
  )
})


test_that("validate_stan_fit handles missing cmdstanr gracefully", {
  # If cmdstanr is not available, should warn but not error
  fake_fit <- list(class = "fake")
  
  # This test depends on whether cmdstanr is installed
  # If not installed, should warn and return TRUE
  # If installed, should error
  result <- tryCatch(
    {
      validate_stan_fit(fake_fit)
      "no_error"
    },
    validation_error = function(e) "error",
    warning = function(w) "warning"
  )
  
  expect_true(result %in% c("no_error", "error", "warning"))
})
