#' Validate Tibble Input
#'
#' Check that input is a tibble or data frame with required structure.
#' Provides clear error messages using rlang::abort.
#'
#' @param data Object to validate.
#' @param min_rows Integer. Minimum number of rows required. Default is 1.
#' @param required_cols Character vector. Names of required columns. Default is NULL.
#' @param arg_name Character. Name of argument for error messages. Default is "data".
#'
#' @return Invisibly returns TRUE if validation passes, otherwise throws error.
#'
#' @family validation
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Valid tibble
#' df <- tibble(x = 1:5, y = 1:5)
#' validate_tibble(df, required_cols = c("x", "y"))
#'
#' # This would fail:
#' # validate_tibble(df, required_cols = c("x", "z"))
validate_tibble <- function(data, min_rows = 1, required_cols = NULL, 
                           arg_name = "data") {
  # Check if it's a data frame
  if (!is.data.frame(data)) {
    rlang::abort(
      paste0("`", arg_name, "` must be a data frame or tibble."),
      class = "validation_error"
    )
  }
  
  # Check minimum rows
  if (nrow(data) < min_rows) {
    rlang::abort(
      paste0(
        "`", arg_name, "` must have at least ", min_rows, " row(s). ",
        "Found ", nrow(data), "."
      ),
      class = "validation_error"
    )
  }
  
  # Check required columns
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      rlang::abort(
        paste0(
          "`", arg_name, "` is missing required column(s): ",
          paste(missing_cols, collapse = ", ")
        ),
        class = "validation_error"
      )
    }
  }
  
  invisible(TRUE)
}


#' Validate Probability Values
#'
#' Check that values are valid probabilities (between 0 and 1).
#'
#' @param p Numeric vector to validate.
#' @param arg_name Character. Name of argument for error messages. Default is "p".
#' @param allow_na Logical. Whether to allow NA values. Default is FALSE.
#'
#' @return Invisibly returns TRUE if validation passes, otherwise throws error.
#'
#' @family validation
#' @export
#'
#' @examples
#' # Valid probabilities
#' validate_probability(c(0, 0.5, 1))
#'
#' # This would fail:
#' # validate_probability(c(-0.1, 0.5, 1.5))
validate_probability <- function(p, arg_name = "p", allow_na = FALSE) {
  if (!is.numeric(p)) {
    rlang::abort(
      paste0("`", arg_name, "` must be numeric."),
      class = "validation_error"
    )
  }
  
  if (!allow_na && any(is.na(p))) {
    rlang::abort(
      paste0("`", arg_name, "` contains NA values."),
      class = "validation_error"
    )
  }
  
  valid_values <- if (allow_na) !is.na(p) else rep(TRUE, length(p))
  
  if (any(p[valid_values] < 0 | p[valid_values] > 1)) {
    rlang::abort(
      paste0("`", arg_name, "` must contain values between 0 and 1."),
      class = "validation_error"
    )
  }
  
  invisible(TRUE)
}


#' Validate Positive Values
#'
#' Check that values are positive (greater than zero).
#'
#' @param x Numeric vector to validate.
#' @param arg_name Character. Name of argument for error messages. Default is "x".
#' @param allow_zero Logical. Whether to allow zero values. Default is FALSE.
#' @param allow_na Logical. Whether to allow NA values. Default is FALSE.
#'
#' @return Invisibly returns TRUE if validation passes, otherwise throws error.
#'
#' @family validation
#' @export
#'
#' @examples
#' # Valid positive values
#' validate_positive(c(1, 2, 3))
#'
#' # With zero allowed
#' validate_positive(c(0, 1, 2), allow_zero = TRUE)
validate_positive <- function(x, arg_name = "x", allow_zero = FALSE, 
                             allow_na = FALSE) {
  if (!is.numeric(x)) {
    rlang::abort(
      paste0("`", arg_name, "` must be numeric."),
      class = "validation_error"
    )
  }
  
  if (!allow_na && any(is.na(x))) {
    rlang::abort(
      paste0("`", arg_name, "` contains NA values."),
      class = "validation_error"
    )
  }
  
  valid_values <- if (allow_na) !is.na(x) else rep(TRUE, length(x))
  
  threshold <- if (allow_zero) 0 else .Machine$double.eps
  comparison <- if (allow_zero) ">=" else ">"
  
  if (any(x[valid_values] < threshold)) {
    rlang::abort(
      paste0(
        "`", arg_name, "` must contain values ", comparison, " ", 
        if (allow_zero) "0" else "0 (positive)"
      ),
      class = "validation_error"
    )
  }
  
  invisible(TRUE)
}


#' Validate Matching Lengths
#'
#' Check that multiple vectors have the same length.
#'
#' @param ... Named vectors to compare.
#'
#' @return Invisibly returns TRUE if validation passes, otherwise throws error.
#'
#' @family validation
#' @export
#'
#' @examples
#' x <- 1:5
#' y <- 6:10
#' z <- 11:15
#' validate_matching_lengths(x = x, y = y, z = z)
#'
#' # This would fail:
#' # validate_matching_lengths(x = 1:5, y = 1:3)
validate_matching_lengths <- function(...) {
  args <- list(...)
  arg_names <- names(args)
  
  if (length(args) < 2) {
    rlang::abort(
      "At least two arguments required for length comparison.",
      class = "validation_error"
    )
  }
  
  lengths <- vapply(args, length, integer(1))
  
  if (length(unique(lengths)) > 1) {
    length_info <- paste0(
      arg_names, " (n=", lengths, ")",
      collapse = ", "
    )
    
    rlang::abort(
      paste0("Arguments must have matching lengths. Found: ", length_info),
      class = "validation_error"
    )
  }
  
  invisible(TRUE)
}


#' Validate Stan Fit Object
#'
#' Check that an object appears to be a valid Stan fit object from cmdstanr.
#'
#' @param fit Object to validate.
#' @param arg_name Character. Name of argument for error messages. Default is "fit".
#'
#' @return Invisibly returns TRUE if validation passes, otherwise throws error.
#'
#' @family validation
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have a cmdstanr fit object:
#' validate_stan_fit(fit)
#' }
validate_stan_fit <- function(fit, arg_name = "fit") {
  # Check if cmdstanr is available
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    cli::cli_warn(
      "Package {.pkg cmdstanr} not available. Skipping Stan fit validation."
    )
    return(invisible(TRUE))
  }
  
  # Check if it's a CmdStanMCMC object
  if (!inherits(fit, "CmdStanMCMC")) {
    rlang::abort(
      paste0(
        "`", arg_name, "` must be a CmdStanMCMC object from cmdstanr. ",
        "Found class: ", paste(class(fit), collapse = ", ")
      ),
      class = "validation_error"
    )
  }
  
  invisible(TRUE)
}
