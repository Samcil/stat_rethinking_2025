test_that("simulate_cross_classified generates valid data", {
  data <- simulate_cross_classified(
    n_obs = 100,
    n_groups1 = 5,
    n_groups2 = 4,
    seed = 123
  )

  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 100)
  expect_named(data, c("obs_id", "group1_id", "group2_id", "y", "group1_effect", "group2_effect"))

  # Check group IDs are within range
  expect_true(all(data$group1_id %in% 1:5))
  expect_true(all(data$group2_id %in% 1:4))

  # Check effects are numeric
  expect_type(data$group1_effect, "double")
  expect_type(data$group2_effect, "double")
})

test_that("simulate_cross_classified validation works", {
  expect_error(
    simulate_cross_classified(n_obs = -1),
    "positive"
  )

  expect_error(
    simulate_cross_classified(n_groups1 = 0),
    "positive"
  )

  expect_error(
    simulate_cross_classified(sigma_group1 = -1),
    "positive"
  )
})

test_that("simulate_temporal_autocorrelation generates valid data", {
  data <- simulate_temporal_autocorrelation(
    n_time = 20,
    n_groups = 3,
    phi = 0.7,
    seed = 123
  )

  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 20 * 3)
  expect_named(data, c("time", "group_id", "y", "group_effect", "ar_component"))

  # Check time sequence
  expect_equal(unique(data$time), 1:20)

  # Check groups
  expect_equal(sort(unique(data$group_id)), 1:3)
})

test_that("simulate_temporal_autocorrelation validates phi", {
  expect_error(
    simulate_temporal_autocorrelation(phi = 1.5),
    "between -1 and 1"
  )

  expect_error(
    simulate_temporal_autocorrelation(phi = -1.1),
    "between -1 and 1"
  )
})

test_that("compute_poststrat_weights calculates correctly", {
  sample_data <- tibble::tibble(
    age = rep(c("young", "old"), each = 50),
    region = rep(c("north", "south"), 50),
    response = rnorm(100)
  )

  population <- tibble::tibble(
    age = rep(c("young", "old"), each = 2),
    region = rep(c("north", "south"), 2),
    pop_count = c(5000, 3000, 2000, 4000)
  )

  weights <- compute_poststrat_weights(
    sample_data,
    c("age", "region"),
    population
  )

  expect_s3_class(weights, "tbl_df")
  expect_named(weights, c("age", "region", "pop_count", "weight"))

  # Weights should sum to 1
  expect_equal(sum(weights$weight), 1)

  # Check specific weights
  total_pop <- sum(population$pop_count)
  expect_equal(weights$weight[1], 5000 / total_pop)
})

test_that("compute_poststrat_weights validation works", {
  sample_data <- tibble::tibble(age = c("young", "old"))
  population <- tibble::tibble(age = c("young", "old"), pop_count = c(100, 200))

  expect_error(
    compute_poststrat_weights(sample_data, c("region"), population),
    "must be columns in `sample_data`"
  )

  expect_error(
    compute_poststrat_weights(sample_data, c("age"), tibble::tibble(age = c("young", "old"))),
    "must contain a `pop_count` column"
  )
})

test_that("simulate_spatial_autocorrelation generates valid data", {
  data <- simulate_spatial_autocorrelation(
    n_locations = 30,
    spatial_range = 0.3,
    spatial_sigma = 1.5,
    seed = 123
  )

  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 30)
  expect_named(data, c("location_id", "x_coord", "y_coord", "y", "spatial_effect"))

  # Check coordinates are in [0, 1]
  expect_true(all(data$x_coord >= 0 & data$x_coord <= 1))
  expect_true(all(data$y_coord >= 0 & data$y_coord <= 1))

  # Check location IDs
  expect_equal(data$location_id, 1:30)
})

test_that("simulate_spatial_autocorrelation validation works", {
  expect_error(
    simulate_spatial_autocorrelation(n_locations = 0),
    "positive"
  )

  expect_error(
    simulate_spatial_autocorrelation(spatial_range = -0.1),
    "positive"
  )

  expect_error(
    simulate_spatial_autocorrelation(spatial_sigma = -1),
    "positive"
  )
})

test_that("reproducibility with seed works", {
  data1 <- simulate_cross_classified(n_obs = 50, seed = 42)
  data2 <- simulate_cross_classified(n_obs = 50, seed = 42)

  expect_equal(data1$y, data2$y)
  expect_equal(data1$group1_id, data2$group1_id)
})
