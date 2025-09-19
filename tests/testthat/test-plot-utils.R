test_that("lines_with_outline and abline_with_outline draw without error", {
  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf)
  plot(0:1, 0:1, type = "n")
  expect_silent(lines_with_outline(c(0, 1), c(0, 1), col = "black", outline_col = "grey80"))
  expect_silent(abline_with_outline(h = 0.5, col = "red", outline_col = "grey80"))
  grDevices::dev.off()
  unlink(tf)
})

