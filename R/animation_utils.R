#' Build Animation Frames Data
#'
#' Create a tibble structure for building animations with gganimate.
#' This function helps organize data for frame-by-frame animations.
#'
#' @param data List of data frames or tibbles, one per frame.
#' @param frame_var Character. Name of the frame variable to create. Default is "frame".
#'
#' @return Tibble with all frames combined and a frame identifier column.
#'
#' @family animation
#' @export
#'
#' @examples
#' # Create simple animation data
#' frame1 <- data.frame(x = 1:5, y = 1:5)
#' frame2 <- data.frame(x = 1:5, y = 2:6)
#' frame3 <- data.frame(x = 1:5, y = 3:7)
#'
#' animation_data <- build_animation_frames(
#'   list(frame1, frame2, frame3)
#' )
build_animation_frames <- function(data, frame_var = "frame") {
  checkmate::assert_list(data, min.len = 1, types = "data.frame")
  checkmate::assert_string(frame_var)
  
  # Add frame identifier to each data frame
  frames_with_id <- purrr::imap(data, function(df, idx) {
    df[[frame_var]] <- idx
    df
  })
  
  # Combine all frames
  dplyr::bind_rows(frames_with_id)
}


#' Create Animation Sequence
#'
#' Generate a sequence of values that changes over animation frames.
#' Useful for creating smooth transitions between values.
#'
#' @param from Numeric. Starting value.
#' @param to Numeric. Ending value.
#' @param n_frames Integer. Number of frames. Default is 50.
#' @param ease_function Function. Easing function for transitions. 
#'   Default is linear (\code{function(x) x}).
#'
#' @return Numeric vector of length n_frames with interpolated values.
#'
#' @family animation
#' @export
#'
#' @examples
#' # Linear animation from 0 to 10 over 50 frames
#' create_animation_sequence(0, 10, n_frames = 50)
#'
#' # With easing (quadratic ease-in)
#' create_animation_sequence(0, 10, n_frames = 50, 
#'                          ease_function = function(x) x^2)
create_animation_sequence <- function(from, to, n_frames = 50, 
                                     ease_function = function(x) x) {
  checkmate::assert_number(from, finite = TRUE)
  checkmate::assert_number(to, finite = TRUE)
  checkmate::assert_int(n_frames, lower = 2)
  checkmate::assert_function(ease_function, args = "x")
  
  # Create linear sequence
  t <- seq(0, 1, length.out = n_frames)
  
  # Apply easing function
  t_eased <- ease_function(t)
  
  # Interpolate between from and to
  from + (to - from) * t_eased
}


#' Prepare Data for Path Animation
#'
#' Transform a set of paths (e.g., MCMC trajectories) into a format
#' suitable for animating with gganimate, revealing paths progressively.
#'
#' @param paths Tibble or data frame with path data.
#' @param x_var Character. Name of x variable. Default is "x".
#' @param y_var Character. Name of y variable. Default is "y".
#' @param path_id_var Character. Name of path identifier variable. Default is "path_id".
#' @param n_frames Integer. Number of frames to create per path. Default is 50.
#'
#' @return Tibble with frame identifiers for animation.
#'
#' @family animation
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Create sample path data
#' paths <- tibble(
#'   x = c(1:10, 1:10),
#'   y = c(1:10, 10:1),
#'   path_id = rep(c(1, 2), each = 10)
#' )
#'
#' # Prepare for animation
#' animated_paths <- prepare_path_animation(paths)
prepare_path_animation <- function(paths, x_var = "x", y_var = "y", 
                                  path_id_var = "path_id", n_frames = 50) {
  checkmate::assert_data_frame(paths, min.rows = 1)
  checkmate::assert_string(x_var)
  checkmate::assert_string(y_var)
  checkmate::assert_string(path_id_var)
  checkmate::assert_int(n_frames, lower = 1)
  
  # Check that required columns exist
  required_cols <- c(x_var, y_var)
  if (!all(required_cols %in% names(paths))) {
    missing <- setdiff(required_cols, names(paths))
    rlang::abort(
      paste0("Missing required columns: ", paste(missing, collapse = ", "))
    )
  }
  
  # Add step identifier if not using path_id
  if (!path_id_var %in% names(paths)) {
    paths <- dplyr::mutate(paths, !!path_id_var := 1)
  }
  
  # Group by path and add step numbers
  paths <- paths %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(path_id_var))) %>%
    dplyr::mutate(
      step = dplyr::row_number(),
      max_step = dplyr::n()
    ) %>%
    dplyr::ungroup()
  
  # Create frames where each frame shows progressively more steps
  max_steps <- max(paths$max_step)
  frames <- purrr::map(seq_len(n_frames), function(frame) {
    # Calculate which step to show up to
    step_limit <- ceiling((frame / n_frames) * max_steps)
    
    paths %>%
      dplyr::filter(step <= step_limit) %>%
      dplyr::mutate(frame = frame)
  })
  
  dplyr::bind_rows(frames)
}


#' Create Discrete Animation Frames
#'
#' Helper to create discrete frames for categorical animations (e.g., showing
#' different scenarios or states one at a time).
#'
#' @param data Tibble or data frame with data to animate.
#' @param state_var Character. Name of variable defining states/categories.
#' @param repeat_each Integer. Number of frames to repeat each state. Default is 1.
#'
#' @return Tibble with frame identifiers.
#'
#' @family animation
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Create data with different scenarios
#' scenarios <- tibble(
#'   scenario = c("A", "A", "B", "B", "C", "C"),
#'   x = c(1, 2, 1, 2, 1, 2),
#'   y = c(1, 2, 2, 3, 3, 4)
#' )
#'
#' # Create animation frames (show each scenario for 5 frames)
#' animated <- create_discrete_frames(scenarios, "scenario", repeat_each = 5)
create_discrete_frames <- function(data, state_var, repeat_each = 1) {
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_string(state_var)
  checkmate::assert_int(repeat_each, lower = 1)
  
  if (!state_var %in% names(data)) {
    rlang::abort(paste0("Column '", state_var, "' not found in data."))
  }
  
  # Get unique states
  states <- unique(data[[state_var]])
  
  # Create frame assignments
  frame_list <- purrr::map(seq_along(states), function(i) {
    state_data <- data[data[[state_var]] == states[i], , drop = FALSE]
    
    # Repeat for multiple frames if requested
    if (repeat_each > 1) {
      purrr::map(seq_len(repeat_each), function(rep) {
        state_data$frame <- (i - 1) * repeat_each + rep
        state_data
      })
    } else {
      state_data$frame <- i
      list(state_data)
    }
  }) %>%
    purrr::flatten()
  
  dplyr::bind_rows(frame_list)
}
