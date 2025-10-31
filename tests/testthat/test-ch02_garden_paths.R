test_that("simulate_garden_paths generates correct structure", {
  observations <- c(1, 0, 1)
  possibilities <- c(0, 1, 1, 1)
  
  paths <- simulate_garden_paths(observations, possibilities)
  
  expect_s3_class(paths, "tbl_df")
  expect_true(all(c("path_id", "depth", "value", "viable", "probability") %in% names(paths)))
  
  # Check that we have correct number of paths
  n_paths <- length(unique(paths$path_id))
  expected_paths <- length(possibilities)^length(observations)
  expect_equal(n_paths, expected_paths)
  
  # Check that probabilities sum to 1 for viable paths
  viable_paths <- paths %>%
    dplyr::filter(viable, depth == max(depth))
  expect_equal(sum(viable_paths$probability), 1, tolerance = 1e-10)
})


test_that("simulate_garden_paths handles different inputs", {
  # Single observation
  paths1 <- simulate_garden_paths(1, c(0, 1))
  expect_equal(length(unique(paths1$path_id)), 2)
  
  # All matches
  paths2 <- simulate_garden_paths(c(1, 1, 1), c(1, 1, 1, 1))
  expect_true(all(paths2$viable))
  
  # No matches
  paths3 <- simulate_garden_paths(c(1, 1), c(0, 0))
  expect_true(all(!paths3$viable))
})


test_that("create_garden_layout adds coordinates", {
  observations <- c(1, 0, 1)
  possibilities <- c(0, 1, 1, 1)
  
  paths <- simulate_garden_paths(observations, possibilities)
  layout <- create_garden_layout(paths)
  
  expect_s3_class(layout, "tbl_df")
  expect_true(all(c("x", "y", "angle", "radius") %in% names(layout)))
  
  # Check that origin (depth 0) starts at (0, 0)
  expect_true(all(layout$x_prev[layout$depth == 1] == 0))
  expect_true(all(layout$y_prev[layout$depth == 1] == 0))
})


test_that("count_path_outcomes counts correctly", {
  observations <- c(1, 0, 1)
  possibilities <- c(0, 1, 1, 1)
  
  counts <- count_path_outcomes(observations, possibilities)
  
  expect_s3_class(counts, "tbl_df")
  expect_true("n_ways" %in% names(counts))
  
  # Total ways should equal number of viable paths
  paths <- simulate_garden_paths(observations, possibilities)
  viable_count <- sum(paths$viable[paths$depth == max(paths$depth)])
  expect_equal(sum(counts$n_ways), viable_count)
})


test_that("garden path functions validate inputs", {
  expect_error(simulate_garden_paths("invalid", c(0, 1)))
  expect_error(simulate_garden_paths(c(1, 0), "invalid"))
  expect_error(create_garden_layout(data.frame()))
})
