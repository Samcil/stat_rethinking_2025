#' Plotting helpers with outlines
#'
#' Functions that draw lines with an outline/stroke to improve readability
#' in dense visuals. These are thin wrappers around base graphics.
#'
#' Side-effecting plotting is kept small and explicit; for vignettes only.
#'
#' @name plot_utils
NULL

#' Draw a line path with an outline
#'
#' @param x Numeric vector of x coordinates.
#' @param y Numeric vector of y coordinates.
#' @param col Inner line color.
#' @param lwd Inner line width.
#' @param outline_col Outline color (default 'white').
#' @param outline_expand Multiplier for outline width relative to `lwd`.
#' @param ... Passed to [graphics::lines()].
#' @return Invisibly, NULL.
#' @examples
#' plot(0:1, 0:1, type = "n"); lines_with_outline(c(0,1), c(0,1))
#' @export
#' @importFrom graphics lines
lines_with_outline <- function(x, y, col = "black", lwd = 2,
                               outline_col = "white", outline_expand = 2, ...) {
  stopifnot(length(x) == length(y))
  graphics::lines(x, y, col = outline_col, lwd = lwd * outline_expand, ...)
  graphics::lines(x, y, col = col, lwd = lwd, ...)
  invisible(NULL)
}

#' Draw an infinite line with an outline (abline)
#'
#' @param a,b,h,v Parameters as in [graphics::abline()]. Supply either `a` and `b`,
#'   or `h`, or `v`.
#' @inheritParams lines_with_outline
#' @return Invisibly, NULL.
#' @examples
#' plot(0:1, 0:1, type = "n"); abline_with_outline(h = 0.5)
#' @export
#' @importFrom graphics abline
abline_with_outline <- function(a = NULL, b = NULL, h = NULL, v = NULL,
                                col = "black", lwd = 2,
                                outline_col = "white", outline_expand = 2, ...) {
  graphics::abline(a = a, b = b, h = h, v = v, col = outline_col, lwd = lwd * outline_expand, ...)
  graphics::abline(a = a, b = b, h = h, v = v, col = col, lwd = lwd, ...)
  invisible(NULL)
}

