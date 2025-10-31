test_that("polar_to_cartesian works correctly", {
  # Test basic conversion
  result <- polar_to_cartesian(1, 0)
  expect_equal(result[1], 1, tolerance = 1e-10)
  expect_equal(result[2], 0, tolerance = 1e-10)
  
  # Test at pi/2 (90 degrees)
  result <- polar_to_cartesian(1, pi/2)
  expect_equal(result[1], 0, tolerance = 1e-10)
  expect_equal(result[2], 1, tolerance = 1e-10)
  
  # Test with custom origin
  result <- polar_to_cartesian(2, pi/4, origin = c(1, 1))
  expect_true(result[1] > 1)
  expect_true(result[2] > 1)
  
  # Test input validation
  expect_error(polar_to_cartesian("a", pi/2))
  expect_error(polar_to_cartesian(1, "b"))
  expect_error(polar_to_cartesian(1, pi/2, origin = c(1)))
})


test_that("cartesian_to_polar works correctly", {
  # Test basic conversion
  result <- cartesian_to_polar(c(0, 0), c(1, 0))
  expect_equal(result["theta"], 0, tolerance = 1e-10)
  expect_equal(result["dist"], 1, tolerance = 1e-10)
  
  # Test at 90 degrees
  result <- cartesian_to_polar(c(0, 0), c(0, 1))
  expect_equal(result["theta"], pi/2, tolerance = 1e-10)
  expect_equal(result["dist"], 1, tolerance = 1e-10)
  
  # Test at 45 degrees
  result <- cartesian_to_polar(c(0, 0), c(1, 1))
  expect_equal(result["theta"], pi/4, tolerance = 1e-10)
  expect_equal(result["dist"], sqrt(2), tolerance = 1e-10)
  
  # Test zero distance
  result <- cartesian_to_polar(c(1, 1), c(1, 1))
  expect_equal(result["theta"], 0)
  expect_equal(result["dist"], 0)
  
  # Test input validation
  expect_error(cartesian_to_polar(c(1), c(1, 1)))
  expect_error(cartesian_to_polar(c(1, 1), c(1)))
})


test_that("polar_to_cartesian and cartesian_to_polar are inverses", {
  # Test round-trip conversion
  original <- c(1, 1)
  polar <- cartesian_to_polar(c(0, 0), original)
  cartesian <- polar_to_cartesian(polar["dist"], polar["theta"], origin = c(0, 0))
  
  expect_equal(cartesian[1], original[1], tolerance = 1e-10)
  expect_equal(cartesian[2], original[2], tolerance = 1e-10)
})


test_that("create_circle_polygon generates correct points", {
  circle <- create_circle_polygon(x = 0, y = 0, r = 1, n_points = 360)
  
  # Check output structure
  expect_s3_class(circle, "tbl_df")
  expect_true("x" %in% names(circle))
  expect_true("y" %in% names(circle))
  expect_equal(nrow(circle), 360)
  
  # Check that points are on the circle (distance from center = radius)
  distances <- sqrt(circle$x^2 + circle$y^2)
  expect_true(all(abs(distances - 1) < 1e-10))
  
  # Test with custom parameters
  circle2 <- create_circle_polygon(x = 5, y = 3, r = 2, n_points = 100)
  expect_equal(nrow(circle2), 100)
  distances2 <- sqrt((circle2$x - 5)^2 + (circle2$y - 3)^2)
  expect_true(all(abs(distances2 - 2) < 1e-10))
  
  # Test input validation
  expect_error(create_circle_polygon(r = -1))
  expect_error(create_circle_polygon(n_points = 2))
})


test_that("shorten_line_segment works correctly", {
  # Test basic shortening
  result <- shorten_line_segment(c(0, 10), c(0, 0), short = 0.1)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  
  # Check that line is shortened appropriately
  expect_equal(result$x[1], 1, tolerance = 1e-10)
  expect_equal(result$x[2], 9, tolerance = 1e-10)
  
  # Test diagonal line
  result2 <- shorten_line_segment(c(0, 10), c(0, 10), short = 0.2)
  expect_true(result2$x[1] > 0)
  expect_true(result2$x[2] < 10)
  expect_true(result2$y[1] > 0)
  expect_true(result2$y[2] < 10)
  
  # Test input validation
  expect_error(shorten_line_segment(c(0), c(0, 10)))
  expect_error(shorten_line_segment(c(0, 10), c(0, 10), short = -0.1))
  expect_error(shorten_line_segment(c(0, 10), c(0, 10), short = 0.6))
})
