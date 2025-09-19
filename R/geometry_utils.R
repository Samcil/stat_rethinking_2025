#' Geometry utilities for polar/screen coordinates and simple shapes
#'
#' Vectorized helpers for converting between polar and Cartesian (screen)
#' coordinates and constructing simple geometric primitives used across
#' the vignettes.
#'
#' Functions follow tidyverse conventions: return tibbles/data frames,
#' data-first where appropriate, and avoid side effects.
#'
#' @name geometry_utils
NULL

#' Convert polar coordinates to screen (Cartesian) coordinates
#'
#' @param dist Numeric vector of radial distances.
#' @param theta Numeric vector of angles in radians (same length as `dist`).
#' @param origin Numeric length-2 vector giving origin (x0, y0). Default c(0, 0).
#' @return A tibble with columns `x`, `y`.
#' @examples
#' polar_to_screen(1, pi/2)
#' polar_to_screen(c(1,2), c(0, pi/2), origin = c(1, 1))
#' @export
polar_to_screen <- function(dist, theta, origin = c(0, 0)) {
  if (length(origin) != 2) cli::cli_abort("`origin` must be length 2: c(x0, y0).")
  x <- origin[1] + dist * cos(theta)
  y <- origin[2] + dist * sin(theta)
  tibble::tibble(x = x, y = y)
}

#' Convert screen (Cartesian) coordinates to polar coordinates
#'
#' @param x Numeric vector of x coordinates.
#' @param y Numeric vector of y coordinates (same length as `x`).
#' @param origin Numeric length-2 vector giving origin (x0, y0). Default c(0, 0).
#' @return A tibble with columns `dist` and `theta` (radians).
#' @examples
#' screen_to_polar(0, 1)
#' @export
screen_to_polar <- function(x, y, origin = c(0, 0)) {
  if (length(origin) != 2) cli::cli_abort("`origin` must be length 2: c(x0, y0).")
  dx <- x - origin[1]
  dy <- y - origin[2]
  tibble::tibble(
    dist = sqrt(dx^2 + dy^2),
    theta = atan2(dy, dx)
  )
}

#' Compute a point on a line segment by interpolation
#'
#' @param x Numeric vector of length 2: x coordinates of the segment endpoints.
#' @param y Numeric vector of length 2: y coordinates of the segment endpoints.
#' @param p Numeric in [0,1] giving position along the segment (0=start, 1=end).
#' @return A tibble with columns `x`, `y` at the interpolated point.
#' @examples
#' point_on_line(c(0,1), c(0,1), p = 0.5)
#' @export
point_on_line <- function(x, y, p) {
  if (length(x) != 2 || length(y) != 2 || length(p) != 1) cli::cli_abort("`x` and `y` must be length 2; `p` must be length 1.")
  tibble::tibble(
    x = (1 - p) * x[1] + p * x[2],
    y = (1 - p) * y[1] + p * y[2]
  )
}

#' Shorten a line segment by trimming each end by a fraction
#'
#' @param x Numeric vector length 2 of x endpoints.
#' @param y Numeric vector length 2 of y endpoints.
#' @param frac Numeric in [0,0.5) fraction of original length trimmed from each end.
#' @return A tibble with columns `x`, `y` for the shortened segment endpoints (2 rows).
#' @examples
#' shorten_segment(c(0, 10), c(0, 0), frac = 0.1)
#' @export
shorten_segment <- function(x, y, frac = 0.1) {
  if (length(x) != 2 || length(y) != 2) cli::cli_abort("`x` and `y` must be length 2.")
  if (!is.numeric(frac) || frac < 0 || frac >= 0.5) cli::cli_abort("`frac` must be numeric in [0, 0.5).")
  dx <- x[2] - x[1]
  dy <- y[2] - y[1]
  L <- sqrt(dx^2 + dy^2)
  if (L == 0) return(tibble::tibble(x = x, y = y))
  ux <- dx / L
  uy <- dy / L
  trim <- frac * L
  tibble::tibble(
    x = c(x[1] + ux * trim, x[2] - ux * trim),
    y = c(y[1] + uy * trim, y[2] - uy * trim)
  )
}

#' Points along a circular arc (or full circle)
#'
#' @param radius Radius (single numeric) or vector matching `theta` length.
#' @param origin Numeric length-2 origin.
#' @param n Integer number of points along the arc.
#' @param theta_start Start angle (radians).
#' @param theta_end End angle (radians).
#' @return A tibble with columns `x`, `y`, `theta`.
#' @examples
#' circle_points(1, n = 4)
#' @export
circle_points <- function(radius, origin = c(0, 0), n = 100,
                          theta_start = 0, theta_end = 2 * pi) {
  if (length(origin) != 2) cli::cli_abort("`origin` must be length 2.")
  if (n < 2) cli::cli_abort("`n` must be >= 2.")
  theta <- seq(theta_start, theta_end, length.out = n)
  if (length(radius) == 1L) radius <- rep(radius, length(theta))
  pts <- polar_to_screen(radius, theta, origin = origin)
  pts$theta <- theta
  pts
}

#' Wedge polygon coordinates along an arc
#'
#' @param radius Radius of the arc.
#' @param theta_start Start angle in radians.
#' @param theta_end End angle in radians.
#' @param origin Origin (x0, y0).
#' @param n Points along arc.
#' @param include_origin Logical; if TRUE include the origin as first/last to form a wedge.
#' @return Tibble of x,y suitable for polygon drawing.
#' @examples
#' wedge_polygon(1, 0, pi/2)
#' @export
wedge_polygon <- function(radius, theta_start, theta_end,
                          origin = c(0, 0), n = 100, include_origin = FALSE) {
  arc <- circle_points(radius, origin = origin, n = n,
                       theta_start = theta_start, theta_end = theta_end)
  if (isTRUE(include_origin)) {
    arc <- rbind(
      tibble::tibble(x = origin[1], y = origin[2], theta = NA_real_),
      arc,
      tibble::tibble(x = origin[1], y = origin[2], theta = NA_real_)
    )
  }
  arc
}

#' Radial line segment from origin to a given polar coordinate
#'
#' @param radius Distance from origin.
#' @param theta Angle in radians.
#' @param origin Origin (x0, y0).
#' @return A tibble with two rows (start and end) and columns x,y.
#' @examples
#' line_polar(1, pi/4)
#' @export
line_polar <- function(radius, theta, origin = c(0, 0)) {
  end <- polar_to_screen(radius, theta, origin)
  tibble::tibble(x = c(origin[1], end$x), y = c(origin[2], end$y))
}

#' Point at a polar location
#'
#' @inheritParams line_polar
#' @return A tibble with columns x,y.
#' @examples
#' point_polar(2, pi)
#' @export
point_polar <- function(radius, theta, origin = c(0, 0)) {
  polar_to_screen(radius, theta, origin)
}

