#' Create DAG Node Positions
#'
#' Generate x, y coordinates for placing nodes in a directed acyclic graph (DAG).
#' Supports common layouts for causal diagrams.
#'
#' @param node_names Character vector. Names of nodes in the DAG.
#' @param layout Character. Layout type: "horizontal", "vertical", "circular", "custom".
#'   Default is "horizontal".
#' @param custom_positions Data frame with columns node, x, y. Required if layout = "custom".
#'
#' @return Tibble with columns:
#'   - node: Node name
#'   - x: X coordinate
#'   - y: Y coordinate
#'
#' @family chapter05
#' @export
#'
#' @examples
#' # Create horizontal layout
#' nodes <- c("X", "Z", "Y")
#' positions <- create_dag_positions(nodes, layout = "horizontal")
#'
#' # Custom layout
#' custom_pos <- data.frame(
#'   node = c("X", "Z", "Y"),
#'   x = c(0, 0.5, 1),
#'   y = c(0, 1, 0)
#' )
#' positions <- create_dag_positions(nodes, layout = "custom", 
#'                                   custom_positions = custom_pos)
create_dag_positions <- function(node_names,
                                layout = c("horizontal", "vertical", "circular", "custom"),
                                custom_positions = NULL) {
  checkmate::assert_character(node_names, min.len = 1, unique = TRUE)
  layout <- match.arg(layout)
  
  n_nodes <- length(node_names)
  
  if (layout == "custom") {
    if (is.null(custom_positions)) {
      rlang::abort("custom_positions required when layout = 'custom'")
    }
    validate_tibble(custom_positions, min_rows = n_nodes,
                   required_cols = c("node", "x", "y"))
    
    if (!all(node_names %in% custom_positions$node)) {
      missing <- setdiff(node_names, custom_positions$node)
      rlang::abort(paste0("Missing positions for nodes: ", paste(missing, collapse = ", ")))
    }
    
    return(custom_positions %>% dplyr::filter(node %in% node_names))
  }
  
  positions <- switch(
    layout,
    horizontal = {
      tibble::tibble(
        node = node_names,
        x = seq(0, 1, length.out = n_nodes),
        y = 0
      )
    },
    vertical = {
      tibble::tibble(
        node = node_names,
        x = 0,
        y = seq(0, 1, length.out = n_nodes)
      )
    },
    circular = {
      angles <- seq(0, 2 * pi * (n_nodes - 1) / n_nodes, length.out = n_nodes)
      tibble::tibble(
        node = node_names,
        x = cos(angles),
        y = sin(angles)
      )
    }
  )
  
  positions
}


#' Create DAG Edge Data
#'
#' Generate data for drawing edges (arrows) in a directed acyclic graph.
#'
#' @param edges Data frame with columns from, to specifying directed edges.
#' @param positions Tibble from \code{create_dag_positions()} with node positions.
#' @param curvature Numeric. Curvature of edges (0 = straight). Default is 0.
#'
#' @return Tibble with columns:
#'   - from: Source node
#'   - to: Target node
#'   - x: X coordinate of source
#'   - y: Y coordinate of source
#'   - xend: X coordinate of target
#'   - yend: Y coordinate of target
#'   - curvature: Edge curvature
#'
#' @family chapter05
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Define DAG structure
#' nodes <- c("X", "Z", "Y")
#' edges <- data.frame(from = c("X", "Z"), to = c("Z", "Y"))
#' positions <- create_dag_positions(nodes)
#' edge_data <- create_dag_edges(edges, positions)
#'
#' # Visualize
#' ggplot() +
#'   geom_segment(data = edge_data, 
#'                aes(x = x, y = y, xend = xend, yend = yend),
#'                arrow = arrow(length = unit(0.3, "cm"))) +
#'   geom_point(data = positions, aes(x = x, y = y), size = 10) +
#'   geom_text(data = positions, aes(x = x, y = y, label = node)) +
#'   coord_fixed() +
#'   theme_rethinking()
create_dag_edges <- function(edges, positions, curvature = 0) {
  checkmate::assert_data_frame(edges, min.rows = 1)
  validate_tibble(positions, required_cols = c("node", "x", "y"))
  checkmate::assert_number(curvature, finite = TRUE)
  
  if (!all(c("from", "to") %in% names(edges))) {
    rlang::abort("edges must have 'from' and 'to' columns")
  }
  
  # Check that all nodes in edges exist in positions
  all_edge_nodes <- unique(c(edges$from, edges$to))
  missing_nodes <- setdiff(all_edge_nodes, positions$node)
  if (length(missing_nodes) > 0) {
    rlang::abort(
      paste0("Edges reference undefined nodes: ", paste(missing_nodes, collapse = ", "))
    )
  }
  
  # Join positions for from and to nodes
  edges %>%
    tibble::as_tibble() %>%
    dplyr::left_join(positions %>% dplyr::rename(x = x, y = y), 
                     by = c("from" = "node")) %>%
    dplyr::left_join(positions %>% dplyr::rename(xend = x, yend = y), 
                     by = c("to" = "node")) %>%
    dplyr::mutate(curvature = curvature)
}


#' Simulate Confounding
#'
#' Generate data with a confounding variable to demonstrate causal inference concepts.
#'
#' @param n Integer. Number of observations. Default is 100.
#' @param effect_z_x Numeric. Effect of Z on X. Default is 0.5.
#' @param effect_z_y Numeric. Effect of Z on Y. Default is 0.5.
#' @param effect_x_y Numeric. Direct effect of X on Y. Default is 0.
#' @param noise_sd Numeric. Standard deviation of noise. Default is 1.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - z: Confounder variable
#'   - x: Treatment/exposure variable
#'   - y: Outcome variable
#'
#' @family chapter05
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Simulate confounded data (Z -> X, Z -> Y, no direct X -> Y)
#' confounded <- simulate_confounding(
#'   n = 100,
#'   effect_z_x = 1,
#'   effect_z_y = 1,
#'   effect_x_y = 0,
#'   seed = 42
#' )
#'
#' # X and Y appear correlated due to confounding
#' ggplot(confounded, aes(x = x, y = y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm") +
#'   labs(title = "Spurious correlation due to confounding")
simulate_confounding <- function(n = 100,
                                effect_z_x = 0.5,
                                effect_z_y = 0.5,
                                effect_x_y = 0,
                                noise_sd = 1,
                                seed = NULL) {
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_number(effect_z_x, finite = TRUE)
  checkmate::assert_number(effect_z_y, finite = TRUE)
  checkmate::assert_number(effect_x_y, finite = TRUE)
  validate_positive(noise_sd, allow_zero = FALSE)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Generate confounder
  z <- rnorm(n, 0, 1)
  
  # Generate X influenced by Z
  x <- effect_z_x * z + rnorm(n, 0, noise_sd)
  
  # Generate Y influenced by Z and X
  y <- effect_z_y * z + effect_x_y * x + rnorm(n, 0, noise_sd)
  
  tibble::tibble(z = z, x = x, y = y)
}


#' Simulate Collider Bias
#'
#' Generate data with a collider variable to demonstrate selection bias.
#' Pattern: X -> C <- Y, where C is a collider.
#'
#' @param n Integer. Number of observations. Default is 100.
#' @param effect_x_c Numeric. Effect of X on C. Default is 1.
#' @param effect_y_c Numeric. Effect of Y on C. Default is 1.
#' @param effect_x_y Numeric. Direct effect of X on Y. Default is 0.
#' @param noise_sd Numeric. Standard deviation of noise. Default is 1.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - x: First variable
#'   - y: Second variable (independent of X)
#'   - c: Collider variable (caused by both X and Y)
#'
#' @family chapter05
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Simulate collider structure
#' collider_data <- simulate_collider_bias(
#'   n = 200,
#'   effect_x_c = 1,
#'   effect_y_c = 1,
#'   effect_x_y = 0,
#'   seed = 42
#' )
#'
#' # X and Y are independent
#' ggplot(collider_data, aes(x = x, y = y)) +
#'   geom_point() +
#'   labs(title = "X and Y independent (full data)")
#'
#' # But conditioning on C creates spurious correlation
#' collider_conditioned <- collider_data %>% dplyr::filter(c > 0)
#' ggplot(collider_conditioned, aes(x = x, y = y)) +
#'   geom_point() +
#'   geom_smooth(method = "lm") +
#'   labs(title = "X and Y appear correlated when conditioning on C")
simulate_collider_bias <- function(n = 100,
                                   effect_x_c = 1,
                                   effect_y_c = 1,
                                   effect_x_y = 0,
                                   noise_sd = 1,
                                   seed = NULL) {
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_number(effect_x_c, finite = TRUE)
  checkmate::assert_number(effect_y_c, finite = TRUE)
  checkmate::assert_number(effect_x_y, finite = TRUE)
  validate_positive(noise_sd, allow_zero = FALSE)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Generate independent X and Y
  x <- rnorm(n, 0, 1)
  y <- effect_x_y * x + rnorm(n, 0, 1)
  
  # Generate collider C caused by both X and Y
  c_var <- effect_x_c * x + effect_y_c * y + rnorm(n, 0, noise_sd)
  
  tibble::tibble(x = x, y = y, c = c_var)
}


#' Simulate Post-Treatment Bias
#'
#' Generate data with a mediator variable to demonstrate post-treatment bias.
#' Pattern: X -> M -> Y, where M is a mediator.
#'
#' @param n Integer. Number of observations. Default is 100.
#' @param effect_x_m Numeric. Effect of X on M (mediator). Default is 0.5.
#' @param effect_m_y Numeric. Effect of M on Y. Default is 0.5.
#' @param effect_x_y Numeric. Direct effect of X on Y. Default is 0.5.
#' @param noise_sd Numeric. Standard deviation of noise. Default is 1.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - x: Treatment/exposure variable
#'   - m: Mediator variable
#'   - y: Outcome variable
#'
#' @family chapter05
#' @export
#'
#' @examples
#' # Simulate mediation
#' mediation_data <- simulate_post_treatment_bias(
#'   n = 100,
#'   effect_x_m = 1,
#'   effect_m_y = 1,
#'   effect_x_y = 0.5,
#'   seed = 42
#' )
#'
#' # Total effect of X on Y
#' lm(y ~ x, data = mediation_data)
#'
#' # Direct effect (controlling for mediator) - biased!
#' lm(y ~ x + m, data = mediation_data)
simulate_post_treatment_bias <- function(n = 100,
                                        effect_x_m = 0.5,
                                        effect_m_y = 0.5,
                                        effect_x_y = 0.5,
                                        noise_sd = 1,
                                        seed = NULL) {
  checkmate::assert_int(n, lower = 1)
  checkmate::assert_number(effect_x_m, finite = TRUE)
  checkmate::assert_number(effect_m_y, finite = TRUE)
  checkmate::assert_number(effect_x_y, finite = TRUE)
  validate_positive(noise_sd, allow_zero = FALSE)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Generate treatment X
  x <- rnorm(n, 0, 1)
  
  # Generate mediator M caused by X
  m <- effect_x_m * x + rnorm(n, 0, noise_sd)
  
  # Generate outcome Y caused by both X and M
  y <- effect_x_y * x + effect_m_y * m + rnorm(n, 0, noise_sd)
  
  tibble::tibble(x = x, m = m, y = y)
}
