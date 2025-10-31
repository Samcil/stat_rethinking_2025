test_that("lines_with_outline returns a ggplot object and builds", {
  df <- tibble::tibble(x = c(0, 1), y = c(0, 1))
  p <- lines_with_outline(df, color = "black", outline_color = "grey80")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("abline_with_outline returns layers that can be added to ggplot", {
  layers <- abline_with_outline(h = 0.5)
  expect_type(layers, "list")
  expect_true(all(purrr::map_lgl(layers, ~ inherits(.x, "LayerInstance") || inherits(.x, "Layer"))))

  # Test that layers can be added to a ggplot
  p <- ggplot2::ggplot() + layers
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))

  # Test composability with other plots
  df <- tibble::tibble(x = c(0, 1), y = c(0, 1))
  p2 <- lines_with_outline(df) + abline_with_outline(a = 0, b = 1, color = "red")
  expect_s3_class(p2, "ggplot")
  expect_silent(ggplot2::ggplot_build(p2))
})
