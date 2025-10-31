#' Statistical Rethinking ggplot2 Theme
#'
#' A clean, minimalist theme suitable for statistical visualizations
#' inspired by the Statistical Rethinking book aesthetic.
#'
#' @param base_size Numeric. Base font size. Default is 11.
#' @param base_family Character. Base font family. Default is "".
#'
#' @return A ggplot2 theme object.
#'
#' @family plotting
#' @export
#'
#' @examples
#' library(ggplot2)
#' 
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_rethinking()
theme_rethinking <- function(base_size = 11, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.line = ggplot2::element_line(color = "black"),
      axis.ticks = ggplot2::element_line(color = "black")
    )
}


#' Get Statistical Rethinking Color Palette
#'
#' Returns a color palette suitable for Statistical Rethinking visualizations.
#'
#' @param n Integer. Number of colors to return. If NULL, returns all colors.
#' @param alpha Numeric. Transparency level (0 to 1). Default is 1.
#'
#' @return Character vector of color codes.
#'
#' @family plotting
#' @export
#'
#' @examples
#' # Get all colors
#' rethinking_palette()
#'
#' # Get first 3 colors with transparency
#' rethinking_palette(3, alpha = 0.7)
rethinking_palette <- function(n = NULL, alpha = 1) {
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  
  # Color palette inspired by Statistical Rethinking visualizations
  colors <- c(
    "#E41A1C", # red
    "#377EB8", # blue
    "#4DAF4A", # green
    "#984EA3", # purple
    "#FF7F00", # orange
    "#FFFF33", # yellow
    "#A65628", # brown
    "#F781BF"  # pink
  )
  
  if (!is.null(n)) {
    checkmate::assert_int(n, lower = 1)
    if (n > length(colors)) {
      cli::cli_warn("Requested {n} colors but only {length(colors)} available. Recycling colors.")
    }
    colors <- rep_len(colors, n)
  }
  
  if (alpha < 1) {
    colors <- scales::alpha(colors, alpha)
  }
  
  colors
}


#' Create Alpha-Transparent Color
#'
#' Apply alpha transparency to a color or vector of colors.
#'
#' @param col Character. Color name or hex code.
#' @param alpha Numeric. Transparency level (0 to 1). Default is 0.5.
#'
#' @return Character vector of color codes with transparency.
#'
#' @family plotting
#' @export
#'
#' @examples
#' # Make red semi-transparent
#' col_alpha("red", 0.5)
#'
#' # Make multiple colors transparent
#' col_alpha(c("red", "blue", "green"), 0.3)
col_alpha <- function(col, alpha = 0.5) {
  checkmate::assert_character(col, min.len = 1)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  
  scales::alpha(col, alpha)
}


#' Create Sequence for Plotting
#'
#' Generate a sequence of values suitable for plotting smooth curves.
#' Useful for creating x-axis values for predicted lines.
#'
#' @param x Numeric vector. Data to create sequence from.
#' @param n Integer. Number of points in sequence. Default is 100.
#' @param extend Numeric. Proportion to extend beyond range (0 to 1). Default is 0.05.
#'
#' @return Numeric vector of evenly-spaced values.
#'
#' @family plotting
#' @export
#'
#' @examples
#' # Create sequence from data
#' x <- c(1, 5, 3, 8, 2)
#' plot_sequence(x)
#'
#' # More points, no extension
#' plot_sequence(x, n = 200, extend = 0)
plot_sequence <- function(x, n = 100, extend = 0.05) {
  checkmate::assert_numeric(x, min.len = 1, finite = TRUE)
  checkmate::assert_int(n, lower = 2)
  checkmate::assert_number(extend, lower = 0, upper = 0.5)
  
  x_range <- range(x, na.rm = TRUE)
  x_span <- diff(x_range)
  
  seq(
    from = x_range[1] - extend * x_span,
    to = x_range[2] + extend * x_span,
    length.out = n
  )
}


#' Add Shaded Interval to Plot
#'
#' Create a data frame suitable for adding shaded confidence/credible intervals
#' to ggplot2 plots using geom_ribbon.
#'
#' @param x Numeric vector. X-axis values.
#' @param lower Numeric vector. Lower bound of interval.
#' @param upper Numeric vector. Upper bound of interval.
#'
#' @return Tibble with columns x, lower, and upper.
#'
#' @family plotting
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' x <- 1:10
#' y <- x + rnorm(10)
#' lower <- y - 2
#' upper <- y + 2
#'
#' interval_data <- create_interval_ribbon(x, lower, upper)
#'
#' ggplot() +
#'   geom_ribbon(data = interval_data, 
#'               aes(x = x, ymin = lower, ymax = upper),
#'               alpha = 0.3) +
#'   geom_line(aes(x = x, y = y))
create_interval_ribbon <- function(x, lower, upper) {
  checkmate::assert_numeric(x, min.len = 1)
  checkmate::assert_numeric(lower, len = length(x))
  checkmate::assert_numeric(upper, len = length(x))
  
  if (any(upper < lower, na.rm = TRUE)) {
    cli::cli_warn("Some upper bounds are less than lower bounds.")
  }
  
  tibble::tibble(
    x = x,
    lower = lower,
    upper = upper
  )
}
