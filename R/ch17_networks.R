#' Social Networks and Dyadic Data - Chapter 17
#'
#' Functions for analyzing network data, including dyadic regression,
#' reciprocity, and social relations models.
#'
#' @name ch17_networks
#' @keywords internal
"_PACKAGE"

#' Simulate Dyadic Network Data
#'
#' Generate network data with sender and receiver effects.
#'
#' @param n_nodes Number of nodes in the network
#' @param intercept Overall baseline tie probability (log-odds scale)
#' @param sigma_sender Standard deviation of sender effects
#' @param sigma_receiver Standard deviation of receiver effects
#' @param reciprocity Effect of reciprocal ties
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with columns: sender_id, receiver_id, tie, sender_effect,
#'   receiver_effect, prob
#'
#' @export
#' @examples
#' # Network with sender/receiver heterogeneity
#' network <- simulate_dyadic_network(
#'   n_nodes = 20,
#'   sigma_sender = 1,
#'   sigma_receiver = 1,
#'   reciprocity = 2
#' )
simulate_dyadic_network <- function(n_nodes = 20,
                                   intercept = -2,
                                   sigma_sender = 1,
                                   sigma_receiver = 1,
                                   reciprocity = 0,
                                   seed = NULL) {
  validate_positive(n_nodes, "n_nodes")
  validate_positive(sigma_sender, "sigma_sender")
  validate_positive(sigma_receiver, "sigma_receiver")

  if (!is.null(seed)) set.seed(seed)

  # Generate sender and receiver effects
  sender_effects <- stats::rnorm(n_nodes, 0, sigma_sender)
  receiver_effects <- stats::rnorm(n_nodes, 0, sigma_receiver)

  # Create all possible dyads (excluding self-ties)
  dyads <- expand.grid(
    sender_id = seq_len(n_nodes),
    receiver_id = seq_len(n_nodes)
  )
  dyads <- dyads[dyads$sender_id != dyads$receiver_id, ]

  # Compute tie probabilities
  dyads$sender_effect <- sender_effects[dyads$sender_id]
  dyads$receiver_effect <- receiver_effects[dyads$receiver_id]

  # Check for reciprocal ties (simplified: use random baseline)
  dyads$logit_prob <- intercept +
    dyads$sender_effect +
    dyads$receiver_effect

  # Add reciprocity effect (simplified)
  if (reciprocity != 0) {
    # For demonstration, add random reciprocity effect
    dyads$logit_prob <- dyads$logit_prob +
      reciprocity * stats::rbinom(nrow(dyads), 1, 0.3)
  }

  dyads$prob <- stats::plogis(dyads$logit_prob)
  dyads$tie <- stats::rbinom(nrow(dyads), 1, dyads$prob)

  tibble::as_tibble(dyads) |>
    dplyr::select(
      .data$sender_id, .data$receiver_id, .data$tie,
      .data$sender_effect, .data$receiver_effect, .data$prob
    )
}

#' Create Network Adjacency Matrix
#'
#' Convert dyadic data to adjacency matrix format.
#'
#' @param dyadic_data Tibble with sender_id, receiver_id, and tie columns
#'
#' @return A square adjacency matrix
#'
#' @export
#' @examples
#' network <- simulate_dyadic_network(n_nodes = 10)
#' adj_matrix <- create_adjacency_matrix(network)
create_adjacency_matrix <- function(dyadic_data) {
  validate_tibble(dyadic_data, "dyadic_data")

  required_cols <- c("sender_id", "receiver_id", "tie")
  if (!all(required_cols %in% names(dyadic_data))) {
    rlang::abort("`dyadic_data` must have columns: sender_id, receiver_id, tie")
  }

  n_nodes <- max(c(dyadic_data$sender_id, dyadic_data$receiver_id))
  adj_matrix <- matrix(0, n_nodes, n_nodes)

  for (i in seq_len(nrow(dyadic_data))) {
    sender <- dyadic_data$sender_id[i]
    receiver <- dyadic_data$receiver_id[i]
    adj_matrix[sender, receiver] <- dyadic_data$tie[i]
  }

  adj_matrix
}

#' Compute Network Statistics
#'
#' Calculate common network metrics from adjacency matrix.
#'
#' @param adj_matrix Square adjacency matrix
#'
#' @return A list with network statistics
#'
#' @export
#' @examples
#' network <- simulate_dyadic_network(n_nodes = 15)
#' adj <- create_adjacency_matrix(network)
#' stats <- compute_network_stats(adj)
compute_network_stats <- function(adj_matrix) {
  if (!is.matrix(adj_matrix)) {
    rlang::abort("`adj_matrix` must be a matrix")
  }

  if (nrow(adj_matrix) != ncol(adj_matrix)) {
    rlang::abort("`adj_matrix` must be square")
  }

  n <- nrow(adj_matrix)

  # Density: proportion of possible ties that exist
  n_possible <- n * (n - 1)
  n_ties <- sum(adj_matrix)
  density <- n_ties / n_possible

  # Reciprocity: proportion of reciprocated ties
  reciprocated <- sum(adj_matrix * t(adj_matrix)) / 2
  reciprocity <- if (n_ties > 0) reciprocated / n_ties else 0

  # Degree distributions
  out_degree <- rowSums(adj_matrix)
  in_degree <- colSums(adj_matrix)

  list(
    n_nodes = n,
    n_ties = n_ties,
    density = density,
    reciprocity = reciprocity,
    mean_out_degree = mean(out_degree),
    mean_in_degree = mean(in_degree),
    sd_out_degree = stats::sd(out_degree),
    sd_in_degree = stats::sd(in_degree)
  )
}

#' Simulate Transitive Closure Process
#'
#' Model transitive triads where "friend of friend becomes friend".
#'
#' @param n_nodes Number of nodes
#' @param baseline_prob Baseline probability of tie formation
#' @param transitivity_bonus Added probability for transitive closure
#' @param n_steps Number of time steps to simulate
#' @param seed Random seed for reproducibility
#'
#' @return A tibble with network at each time step
#'
#' @export
#' @examples
#' # Simulate network evolution with transitivity
#' evolution <- simulate_transitive_closure(
#'   n_nodes = 15,
#'   baseline_prob = 0.05,
#'   transitivity_bonus = 0.3,
#'   n_steps = 10
#' )
simulate_transitive_closure <- function(n_nodes = 20,
                                       baseline_prob = 0.1,
                                       transitivity_bonus = 0.2,
                                       n_steps = 5,
                                       seed = NULL) {
  validate_positive(n_nodes, "n_nodes")
  validate_probability(baseline_prob, "baseline_prob")
  validate_probability(transitivity_bonus, "transitivity_bonus")
  validate_positive(n_steps, "n_steps")

  if (!is.null(seed)) set.seed(seed)

  # Initialize empty adjacency matrix
  adj <- matrix(0, n_nodes, n_nodes)

  results <- purrr::map_dfr(seq_len(n_steps), function(step) {
    # For each potential tie
    for (i in seq_len(n_nodes)) {
      for (j in seq_len(n_nodes)) {
        if (i == j || adj[i, j] == 1) next

        # Check for transitive opportunities
        common_friends <- sum(adj[i, ] * adj[, j])
        prob <- baseline_prob + transitivity_bonus * (common_friends > 0)

        if (stats::runif(1) < prob) {
          adj[i, j] <- 1
        }
      }
    }

    # Convert to dyadic format
    dyads <- which(adj == 1, arr.ind = TRUE)

    if (nrow(dyads) > 0) {
      tibble::tibble(
        step = step,
        sender_id = dyads[, 1],
        receiver_id = dyads[, 2],
        tie = 1
      )
    } else {
      tibble::tibble(
        step = integer(),
        sender_id = integer(),
        receiver_id = integer(),
        tie = integer()
      )
    }
  })

  results
}

#' Compute Dyadic Independence Baseline
#'
#' Calculate expected tie probability under independence assumption.
#'
#' @param dyadic_data Tibble with sender_id, receiver_id, and tie columns
#'
#' @return A list with overall probability and by-node probabilities
#'
#' @export
#' @examples
#' network <- simulate_dyadic_network(n_nodes = 20)
#' baseline <- compute_dyadic_independence(network)
compute_dyadic_independence <- function(dyadic_data) {
  validate_tibble(dyadic_data, "dyadic_data")

  if (!"tie" %in% names(dyadic_data)) {
    rlang::abort("`dyadic_data` must have a `tie` column")
  }

  # Overall tie probability
  overall_prob <- mean(dyadic_data$tie)

  # Sender-specific probabilities
  sender_probs <- dyadic_data |>
    dplyr::group_by(.data$sender_id) |>
    dplyr::summarize(
      n_sent = dplyr::n(),
      n_ties = sum(.data$tie),
      prob = mean(.data$tie),
      .groups = "drop"
    )

  # Receiver-specific probabilities
  receiver_probs <- dyadic_data |>
    dplyr::group_by(.data$receiver_id) |>
    dplyr::summarize(
      n_received = dplyr::n(),
      n_ties = sum(.data$tie),
      prob = mean(.data$tie),
      .groups = "drop"
    )

  list(
    overall_prob = overall_prob,
    sender_probs = sender_probs,
    receiver_probs = receiver_probs
  )
}
