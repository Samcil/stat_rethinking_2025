test_that("polar <-> screen round-trip works", {
  set.seed(123)
  r <- runif(10, 0, 10)
  th <- runif(10, -pi, pi)
  o <- c(1, -2)
  xy <- polar_to_screen(r, th, origin = o)
  rt <- screen_to_polar(xy$x, xy$y, origin = o)
  expect_equal(rt$dist, r, tolerance = 1e-8)
  # angle can wrap by 2*pi; compare cos/sin
  expect_equal(cos(rt$theta), cos(th), tolerance = 1e-8)
  expect_equal(sin(rt$theta), sin(th), tolerance = 1e-8)
})

test_that("point_on_line endpoints and midpoints", {
  p0 <- point_on_line(c(0, 1), c(0, 2), p = 0)
  p1 <- point_on_line(c(0, 1), c(0, 2), p = 1)
  pm <- point_on_line(c(0, 1), c(0, 2), p = 0.5)
  expect_equal(p0$x, 0); expect_equal(p0$y, 0)
  expect_equal(p1$x, 1); expect_equal(p1$y, 2)
  expect_equal(pm$x, 0.5); expect_equal(pm$y, 1)
})

test_that("shorten_segment reduces length by expected amount", {
  seg <- shorten_segment(c(0, 10), c(0, 0), frac = 0.1)
  new_len <- sqrt(diff(seg$x)^2 + diff(seg$y)^2)
  expect_equal(new_len, 10 * (1 - 2 * 0.1))
})

test_that("circle_points produce correct radius", {
  pts <- circle_points(3, origin = c(1, 1), n = 16)
  d <- sqrt((pts$x - 1)^2 + (pts$y - 1)^2)
  expect_true(all(abs(d - 3) < 1e-8))
})

test_that("line_polar end matches polar_to_screen", {
  lp <- line_polar(2, pi/3, origin = c(0, 0))
  end <- polar_to_screen(2, pi/3)
  expect_equal(lp$x[2], end$x)
  expect_equal(lp$y[2], end$y)
})

