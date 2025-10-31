#' Convert Polar Coordinates to Cartesian
#'
#' Transforms polar coordinates (distance and angle) to Cartesian coordinates
#' (x, y) relative to an origin point.
#'
#' @param dist Numeric. Distance from origin.
#' @param theta Numeric. Angle in radians.
#' @param origin Numeric vector of length 2. Origin point (x, y). Default is c(0, 0).
#'
#' @return Numeric vector of length 2 containing x and y coordinates.
#'
#' @family geometry
#' @export
#'
#' @examples
#' # Convert point at distance 1, angle pi/4 radians from origin
#' polar_to_cartesian(1, pi/4)
#'
#' # With custom origin
#' polar_to_cartesian(2, pi/2, origin = c(1, 1))
polar_to_cartesian <- function(dist, theta, origin = c(0, 0)) {
  checkmate::assert_number(dist, finite = TRUE)
  checkmate::assert_number(theta, finite = TRUE)
  checkmate::assert_numeric(origin, len = 2, finite = TRUE)
  
  vx <- cos(theta) * dist
  vy <- sin(theta) * dist
  c(origin[1] + vx, origin[2] + vy)
}


#' Convert Cartesian Coordinates to Polar
#'
#' Transforms two Cartesian points to polar coordinates, returning the
#' distance and angle from origin to destination.
#'
#' @param origin Numeric vector of length 2. Starting point (x, y).
#' @param dest Numeric vector of length 2. Destination point (x, y).
#'
#' @return Numeric vector of length 2 containing angle (theta in radians) and distance.
#'
#' @family geometry
#' @export
#'
#' @examples
#' # Convert between two points
#' cartesian_to_polar(c(0, 0), c(1, 1))
#'
#' # Distance and angle from custom origin
#' cartesian_to_polar(c(2, 2), c(3, 4))
cartesian_to_polar <- function(origin, dest) {
  checkmate::assert_numeric(origin, len = 2, finite = TRUE)
  checkmate::assert_numeric(dest, len = 2, finite = TRUE)
  
  vx <- dest[1] - origin[1]
  vy <- dest[2] - origin[2]
  dist <- sqrt(vx * vx + vy * vy)
  
  # Handle zero distance case
  if (dist == 0) {
    return(c(theta = 0, dist = 0))
  }
  
  theta <- asin(abs(vy) / dist)
  
  # Correct for quadrant
  if (vx < 0 && vy < 0) theta <- pi + theta        # lower-left
  if (vx < 0 && vy > 0) theta <- pi - theta        # upper-left
  if (vx > 0 && vy < 0) theta <- 2 * pi - theta    # lower-right
  if (vx < 0 && vy == 0) theta <- pi               # left
  if (vx == 0 && vy < 0) theta <- 3 * pi / 2       # down
  
  c(theta = theta, dist = dist)
}


#' Create Circle Polygon Points
#'
#' Generate x and y coordinates for drawing a circle or circular arc as a polygon.
#'
#' @param x Numeric. X coordinate of circle center. Default is 0.
#' @param y Numeric. Y coordinate of circle center. Default is 0.
#' @param r Numeric. Radius of circle. Default is 1.
#' @param angle Numeric. Starting angle in radians. Default is 0.
#' @param n_points Integer. Number of points to generate. Default is 360.
#'
#' @return Tibble with columns `x` and `y` containing circle coordinates.
#'
#' @family geometry
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#'
#' # Create a circle
#' circle_data <- create_circle_polygon(x = 0, y = 0, r = 1)
#' 
#' # Plot with ggplot2
#' ggplot(circle_data, aes(x = x, y = y)) +
#'   geom_polygon(fill = "lightblue", color = "black") +
#'   coord_fixed()
create_circle_polygon <- function(x = 0, y = 0, r = 1, angle = 0, n_points = 360) {
  checkmate::assert_number(x, finite = TRUE)
  checkmate::assert_number(y, finite = TRUE)
  checkmate::assert_number(r, lower = 0, finite = TRUE)
  checkmate::assert_number(angle, finite = TRUE)
  checkmate::assert_int(n_points, lower = 3)
  
  a <- seq(angle, angle + 2 * pi, length.out = n_points)
  
  tibble::tibble(
    x = r * cos(a) + x,
    y = r * sin(a) + y
  )
}


#' Shorten Line Segment
#'
#' Create a shortened version of a line segment while retaining its angle
#' and placement. Useful for creating arrow shafts or partial line segments.
#'
#' @param x Numeric vector of length 2. Start and end x coordinates.
#' @param y Numeric vector of length 2. Start and end y coordinates.
#' @param short Numeric. Proportion to shorten each end (0 to 0.5). Default is 0.1.
#'
#' @return Tibble with columns `x` and `y` containing shortened line segment coordinates.
#'
#' @family geometry
#' @export
#'
#' @examples
#' # Create a shortened line
#' shorten_line_segment(c(0, 10), c(0, 10), short = 0.1)
shorten_line_segment <- function(x, y, short = 0.1) {
  checkmate::assert_numeric(x, len = 2, finite = TRUE)
  checkmate::assert_numeric(y, len = 2, finite = TRUE)
  checkmate::assert_number(short, lower = 0, upper = 0.5)
  
  pt1 <- c(x[1], y[1])
  pt2 <- c(x[2], y[2])
  polar <- cartesian_to_polar(pt1, pt2)
  theta <- polar["theta"]
  dist <- polar["dist"]
  
  new_pt1 <- polar_to_cartesian(dist * short, theta, pt1)
  new_pt2 <- polar_to_cartesian(dist * (1 - short), theta, pt1)
  
  tibble::tibble(
    x = c(new_pt1[1], new_pt2[1]),
    y = c(new_pt1[2], new_pt2[2])
  )
}
