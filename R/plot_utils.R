#' Plotting helpers with outlines (ggplot2)
#'
#' ggplot2-based helpers that construct line paths and reference lines with an
#' "outline" stroke to improve readability in dense visuals. Functions return
#' ggplot objects that can be further customized using `+`.
#'
#' All functions are data-first and pipe-friendly. If `data` is not provided,
#' you can pass numeric vectors `x` and `y` and they will be combined into a
#' tibble with columns `x` and `y`.
#'
#' @family plotting
#' @name plot_utils
NULL

#' Draw a path with an outline using ggplot2
#'
#' Data-first wrapper that returns a ggplot object with two layered paths: an
#' outer outline and the inner colored line.
#'
#' @param data A data frame/tibble with columns `x` and `y`. If `NULL`, use `x` and `y`.
#' @param x Optional numeric vector of x coordinates (used when `data` is NULL).
#' @param y Optional numeric vector of y coordinates (used when `data` is NULL).
#' @param color Inner line color.
#' @param size Inner line width.
#' @param outline_color Outline color (default 'white').
#' @param outline_expand Multiplier for outline width relative to `size`.
#' @param ... Additional aesthetics passed to [ggplot2::geom_path()].
#' @return A ggplot object with two path layers (outline below, inner line above).
#' @examples
#' # From x/y vectors
#' lines_with_outline(x = c(0, 1), y = c(0, 1))
#' # From tibble with x,y columns
#' df <- tibble::tibble(x = c(0, 1), y = c(1, 0))
#' lines_with_outline(df, color = "steelblue")
#' @export
#' @importFrom ggplot2 ggplot aes geom_path
lines_with_outline <- function(data = NULL, x = NULL, y = NULL,
                               color = "black", size = 1,
                               outline_color = "white", outline_expand = 2, ...) {
  if (is.null(data)) {
    stopifnot(!is.null(x), !is.null(y), length(x) == length(y))
    data <- tibble::tibble(x = x, y = y)
  } else {
    stopifnot(all(c("x", "y") %in% names(data)))
  }
  ggplot2::ggplot(data, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_path(color = outline_color, linewidth = size * outline_expand, ...) +
    ggplot2::geom_path(color = color, linewidth = size, ...)
}

#' Draw an infinite line with an outline (abline) using ggplot2
#'
#' Returns a ggplot object containing a reference line layer (abline, hline or
#' vline) with an outline for improved contrast. If `data` is supplied and
#' contains `x` and `y` columns, the plot limits are derived from that data;
#' otherwise provide `xlim`/`ylim`.
#'
#' @param data Optional data frame/tibble with `x` and `y` columns to infer limits.
#' @param a Intercept for `geom_abline()` (used with `b`).
#' @param b Slope for `geom_abline()` (used with `a`).
#' @param h y-intercept(s) for horizontal lines.
#' @param v x-intercept(s) for vertical lines.
#' @param xlim Numeric length-2 giving x-axis limits (when `data` is NULL).
#' @param ylim Numeric length-2 giving y-axis limits (when `data` is NULL).
#' @inheritParams lines_with_outline
#' @return A ggplot object with outline and inner reference line.
#' @examples
#' abline_with_outline(h = 0.5)
#' @export
#' @importFrom ggplot2 ggplot geom_abline geom_hline geom_vline coord_cartesian
abline_with_outline <- function(data = NULL, a = NULL, b = NULL,
                                h = NULL, v = NULL,
                                xlim = c(0, 1), ylim = c(0, 1),
                                color = "black", size = 1,
                                outline_color = "white", outline_expand = 2, ...) {
  if (!is.null(data)) {
    if (all(c("x", "y") %in% names(data))) {
      xlim <- range(data$x, na.rm = TRUE)
      ylim <- range(data$y, na.rm = TRUE)
    }
  }
  p <- ggplot2::ggplot()
  # Outline first
  if (!is.null(a) && !is.null(b)) {
    p <- p + ggplot2::geom_abline(intercept = a, slope = b,
                                  color = outline_color, linewidth = size * outline_expand, ...)
    p <- p + ggplot2::geom_abline(intercept = a, slope = b,
                                  color = color, linewidth = size, ...)
  }
  if (!is.null(h)) {
    p <- p + ggplot2::geom_hline(yintercept = h,
                                 color = outline_color, linewidth = size * outline_expand, ...)
    p <- p + ggplot2::geom_hline(yintercept = h,
                                 color = color, linewidth = size, ...)
  }
  if (!is.null(v)) {
    p <- p + ggplot2::geom_vline(xintercept = v,
                                 color = outline_color, linewidth = size * outline_expand, ...)
    p <- p + ggplot2::geom_vline(xintercept = v,
                                 color = color, linewidth = size, ...)
  }
  p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
}

