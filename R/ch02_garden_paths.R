#' Simulate Garden of Forking Paths
#'
#' Generate data representing the garden of forking data paths for
#' Bayesian updating visualization. Each path represents a sequence of
#' observations and their associated posterior probabilities.
#'
#' @param observations Integer vector. Observed values (e.g., 0 for land, 1 for water).
#' @param possibilities Numeric vector. Possible values for each observation.
#' @param n_paths Integer. Number of paths to simulate. Default is NULL (all paths).
#'
#' @return Tibble with columns:
#'   - path_id: Path identifier
#'   - depth: Observation depth (1, 2, 3, ...)
#'   - value: Observed value
#'   - probability: Path probability
#'   - viable: Whether path is consistent with data
#'
#' @family chapter02
#' @export
#'
#' @examples
#' # Three observations: water, land, water
#' observations <- c(1, 0, 1)
#' possibilities <- c(0, 1, 1, 1)  # 1 land marble, 3 water marbles
#'
#' paths <- simulate_garden_paths(observations, possibilities)
simulate_garden_paths <- function(observations, possibilities, n_paths = NULL) {
  checkmate::assert_integer(observations, min.len = 1)
  checkmate::assert_numeric(possibilities, min.len = 1)
  
  n_obs <- length(observations)
  n_poss <- length(possibilities)
  
  # Generate all possible paths
  total_paths <- n_poss^n_obs
  
  if (!is.null(n_paths)) {
    checkmate::assert_int(n_paths, lower = 1)
    if (n_paths > total_paths) {
      cli::cli_warn(
        "Requested {n_paths} paths but only {total_paths} possible. Using all paths."
      )
      n_paths <- total_paths
    }
  } else {
    n_paths <- total_paths
  }
  
  # Create path data
  paths_list <- purrr::map(seq_len(n_paths), function(path_id) {
    # For each path, track the sequence of observations
    path_data <- purrr::map(seq_len(n_obs), function(depth) {
      # Determine which possibility this path follows at this depth
      # Use modular arithmetic to cycle through possibilities
      poss_idx <- ((path_id - 1) %/% (n_poss^(depth - 1))) %% n_poss + 1
      value <- possibilities[poss_idx]
      
      # Check if this step matches the observation
      matches <- value == observations[depth]
      
      tibble::tibble(
        path_id = path_id,
        depth = depth,
        value = value,
        observation = observations[depth],
        matches = matches
      )
    })
    
    dplyr::bind_rows(path_data) %>%
      dplyr::mutate(viable = all(matches))
  })
  
  result <- dplyr::bind_rows(paths_list)
  
  # Calculate path probabilities
  result <- result %>%
    dplyr::group_by(path_id) %>%
    dplyr::mutate(
      probability = ifelse(
        viable[1],
        1 / sum(result$viable[result$depth == n_obs]),
        0
      )
    ) %>%
    dplyr::ungroup()
  
  result
}


#' Create Garden Path Layout for Visualization
#'
#' Generate x, y coordinates for visualizing garden of forking paths
#' in a radial layout suitable for ggplot2.
#'
#' @param paths_data Tibble from \code{simulate_garden_paths()}.
#' @param arc_start Numeric. Starting angle in radians. Default is 0.
#' @param arc_end Numeric. Ending angle in radians. Default is pi.
#' @param growth_factor Numeric. Factor for exponential growth of ring distances.
#'   Default is 1.618 (golden ratio).
#'
#' @return Tibble with path data augmented with x, y coordinates.
#'
#' @family chapter02
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' observations <- c(1, 0, 1)
#' possibilities <- c(0, 1, 1, 1)
#'
#' paths <- simulate_garden_paths(observations, possibilities)
#' paths_layout <- create_garden_layout(paths)
#'
#' ggplot(paths_layout, aes(x = x, y = y, group = path_id)) +
#'   geom_path(aes(alpha = viable)) +
#'   geom_point(aes(color = factor(value), alpha = viable)) +
#'   coord_fixed() +
#'   theme_rethinking()
create_garden_layout <- function(paths_data, arc_start = 0, arc_end = pi,
                                growth_factor = 1.618) {
  validate_tibble(paths_data, required_cols = c("path_id", "depth", "value", "viable"))
  checkmate::assert_number(arc_start, finite = TRUE)
  checkmate::assert_number(arc_end, finite = TRUE)
  checkmate::assert_number(growth_factor, lower = 1)
  
  max_depth <- max(paths_data$depth)
  n_poss <- length(unique(paths_data$value))
  
  # Calculate ring distances with exponential growth
  ring_dist <- rep(1, max_depth)
  if (max_depth > 1) {
    for (i in 2:max_depth) {
      ring_dist[i] <- ring_dist[i - 1] * growth_factor
    }
  }
  ring_dist <- cumsum(ring_dist / sum(ring_dist))
  
  # Add coordinates to each point
  paths_data %>%
    dplyr::group_by(path_id) %>%
    dplyr::mutate(
      # Calculate angle for this path at this depth
      angle = {
        arc_span <- arc_end - arc_start
        # Subdivide arc based on depth
        n_divisions <- n_poss^depth
        path_position <- (path_id - 1) %% n_divisions
        arc_start + (path_position / n_divisions) * arc_span + 
          (arc_span / n_divisions / 2)
      },
      # Calculate radius for this depth
      radius = ring_dist[depth],
      # Convert to cartesian
      x = radius * cos(angle),
      y = radius * sin(angle)
    ) %>%
    dplyr::ungroup() %>%
    # Add origin point
    dplyr::group_by(path_id) %>%
    dplyr::mutate(
      x_prev = dplyr::lag(x, default = 0),
      y_prev = dplyr::lag(y, default = 0)
    ) %>%
    dplyr::ungroup()
}


#' Count Path Outcomes
#'
#' Count the number of ways to observe each possible outcome given
#' a set of possibilities. This is fundamental to the garden of forking
#' data metaphor.
#'
#' @param observations Integer vector. Observed sequence.
#' @param possibilities Numeric vector. Possible values at each step.
#'
#' @return Tibble with unique paths and their counts.
#'
#' @family chapter02
#' @export
#'
#' @examples
#' # Water, land, water observations
#' # With bag containing 1 land, 3 water marbles
#' observations <- c(1, 0, 1)
#' possibilities <- c(0, 1, 1, 1)
#'
#' counts <- count_path_outcomes(observations, possibilities)
count_path_outcomes <- function(observations, possibilities) {
  checkmate::assert_integer(observations, min.len = 1)
  checkmate::assert_numeric(possibilities, min.len = 1)
  
  paths <- simulate_garden_paths(observations, possibilities)
  
  paths %>%
    dplyr::filter(viable) %>%
    dplyr::group_by(path_id) %>%
    dplyr::summarise(
      path = paste(value, collapse = ","),
      matches_data = all(matches),
      .groups = "drop"
    ) %>%
    dplyr::count(path, name = "n_ways")
}
