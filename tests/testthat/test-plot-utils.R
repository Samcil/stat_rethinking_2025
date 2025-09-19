test_that("lines_with_outline returns a ggplot object and builds", {
  df <- tibble::tibble(x = c(0, 1), y = c(0, 1))
  p <- lines_with_outline(df, color = "black", outline_color = "grey80")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("abline_with_outline returns a ggplot object and builds", {
  p <- abline_with_outline(h = 0.5)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

