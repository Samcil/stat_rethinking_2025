test_that("theme_rethinking returns a valid theme", {
  theme <- theme_rethinking()
  expect_s3_class(theme, "theme")
  expect_s3_class(theme, "gg")
  
  # Test with custom parameters
  theme2 <- theme_rethinking(base_size = 14, base_family = "sans")
  expect_s3_class(theme2, "theme")
})


test_that("rethinking_palette returns correct colors", {
  # Test full palette
  colors <- rethinking_palette()
  expect_type(colors, "character")
  expect_true(length(colors) >= 6)
  
  # Test subset
  colors_subset <- rethinking_palette(3)
  expect_length(colors_subset, 3)
  
  # Test with alpha
  colors_alpha <- rethinking_palette(2, alpha = 0.5)
  expect_length(colors_alpha, 2)
  
  # Test requesting more colors than available (should recycle)
  expect_warning(colors_many <- rethinking_palette(20))
  expect_length(colors_many, 20)
  
  # Test input validation
  expect_error(rethinking_palette(alpha = 1.5))
  expect_error(rethinking_palette(alpha = -0.1))
})


test_that("col_alpha creates transparent colors", {
  # Test single color
  result <- col_alpha("red", 0.5)
  expect_type(result, "character")
  expect_length(result, 1)
  
  # Test multiple colors
  result_multi <- col_alpha(c("red", "blue", "green"), 0.3)
  expect_length(result_multi, 3)
  
  # Test input validation
  expect_error(col_alpha("red", alpha = 1.5))
  expect_error(col_alpha("red", alpha = -0.1))
  expect_error(col_alpha(123))
})


test_that("plot_sequence generates correct sequences", {
  x <- c(1, 5, 3, 8, 2)
  
  # Test basic sequence
  seq <- plot_sequence(x)
  expect_type(seq, "double")
  expect_length(seq, 100)
  expect_true(min(seq) <= min(x))
  expect_true(max(seq) >= max(x))
  
  # Test with custom n
  seq2 <- plot_sequence(x, n = 50)
  expect_length(seq2, 50)
  
  # Test with no extension
  seq3 <- plot_sequence(x, n = 100, extend = 0)
  expect_equal(min(seq3), min(x), tolerance = 1e-10)
  expect_equal(max(seq3), max(x), tolerance = 1e-10)
  
  # Test that sequence is evenly spaced
  diffs <- diff(seq)
  expect_true(all(abs(diffs - diffs[1]) < 1e-10))
  
  # Test input validation
  expect_error(plot_sequence(c()))
  expect_error(plot_sequence(x, n = 1))
  expect_error(plot_sequence(x, extend = -0.1))
  expect_error(plot_sequence(x, extend = 0.6))
})


test_that("create_interval_ribbon creates correct structure", {
  x <- 1:10
  lower <- x - 1
  upper <- x + 1
  
  # Test basic creation
  ribbon <- create_interval_ribbon(x, lower, upper)
  expect_s3_class(ribbon, "tbl_df")
  expect_equal(nrow(ribbon), 10)
  expect_true(all(c("x", "lower", "upper") %in% names(ribbon)))
  
  # Test values
  expect_equal(ribbon$x, x)
  expect_equal(ribbon$lower, lower)
  expect_equal(ribbon$upper, upper)
  
  # Test warning for invalid intervals
  expect_warning(create_interval_ribbon(x, upper, lower))
  
  # Test input validation
  expect_error(create_interval_ribbon(c(), lower, upper))
  expect_error(create_interval_ribbon(x, lower[1:5], upper))
  expect_error(create_interval_ribbon(x, lower, upper[1:5]))
})
