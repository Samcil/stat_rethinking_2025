test_that("simulate_dyadic_network generates valid data", {
  network <- simulate_dyadic_network(
    n_nodes = 10,
    sigma_sender = 1,
    sigma_receiver = 1,
    seed = 123
  )

  expect_s3_class(network, "tbl_df")
  expect_named(network, c("sender_id", "receiver_id", "tie", "sender_effect", "receiver_effect", "prob"))

  # Should have n*(n-1) dyads
  expect_equal(nrow(network), 10 * 9)

  # No self-ties
  expect_false(any(network$sender_id == network$receiver_id))

  # Ties should be 0 or 1
  expect_true(all(network$tie %in% c(0, 1)))

  # Probabilities between 0 and 1
  expect_true(all(network$prob >= 0 & network$prob <= 1))
})

test_that("simulate_dyadic_network validation works", {
  expect_error(
    simulate_dyadic_network(n_nodes = 0),
    "positive"
  )

  expect_error(
    simulate_dyadic_network(sigma_sender = -1),
    "positive"
  )
})

test_that("create_adjacency_matrix works correctly", {
  network <- simulate_dyadic_network(n_nodes = 5, seed = 123)
  adj <- create_adjacency_matrix(network)

  expect_type(adj, "double")
  expect_equal(dim(adj), c(5, 5))

  # Diagonal should be 0 (no self-ties)
  expect_equal(diag(adj), rep(0, 5))

  # Values should be 0 or 1
  expect_true(all(adj %in% c(0, 1)))

  # Number of 1s should match number of ties
  expect_equal(sum(adj), sum(network$tie))
})

test_that("create_adjacency_matrix validation works", {
  invalid_data <- tibble::tibble(x = 1:10, y = 1:10)

  expect_error(
    create_adjacency_matrix(invalid_data),
    "must have columns: sender_id, receiver_id, tie"
  )
})

test_that("compute_network_stats calculates correctly", {
  # Create simple network
  network <- tibble::tibble(
    sender_id = c(1, 1, 2, 2, 3),
    receiver_id = c(2, 3, 1, 3, 1),
    tie = c(1, 1, 1, 1, 1)
  )

  adj <- create_adjacency_matrix(network)
  stats <- compute_network_stats(adj)

  expect_type(stats, "list")
  expect_named(stats, c(
    "n_nodes", "n_ties", "density", "reciprocity",
    "mean_out_degree", "mean_in_degree",
    "sd_out_degree", "sd_in_degree"
  ))

  expect_equal(stats$n_nodes, 3)
  expect_equal(stats$n_ties, 5)

  # Density = ties / possible ties
  expect_equal(stats$density, 5 / (3 * 2))

  # Check reciprocity (1-2 and 2-1 are reciprocated, also 1-3 and 3-1)
  expect_true(stats$reciprocity > 0)
})

test_that("compute_network_stats validation works", {
  expect_error(
    compute_network_stats(c(1, 2, 3)),
    "must be a matrix"
  )

  non_square <- matrix(1:6, 2, 3)
  expect_error(
    compute_network_stats(non_square),
    "must be square"
  )
})

test_that("simulate_transitive_closure generates valid data", {
  evolution <- simulate_transitive_closure(
    n_nodes = 10,
    baseline_prob = 0.1,
    transitivity_bonus = 0.3,
    n_steps = 5,
    seed = 123
  )

  expect_s3_class(evolution, "tbl_df")
  expect_named(evolution, c("step", "sender_id", "receiver_id", "tie"))

  # Steps should be 1 to 5
  expect_true(all(evolution$step %in% 1:5))

  # All ties should be 1
  expect_true(all(evolution$tie == 1))

  # No self-ties
  if (nrow(evolution) > 0) {
    expect_false(any(evolution$sender_id == evolution$receiver_id))
  }
})

test_that("simulate_transitive_closure validation works", {
  expect_error(
    simulate_transitive_closure(n_nodes = 0),
    "positive"
  )

  expect_error(
    simulate_transitive_closure(baseline_prob = 1.5),
    "between 0 and 1"
  )

  expect_error(
    simulate_transitive_closure(transitivity_bonus = -0.1),
    "between 0 and 1"
  )
})

test_that("compute_dyadic_independence calculates correctly", {
  network <- simulate_dyadic_network(n_nodes = 15, seed = 123)
  baseline <- compute_dyadic_independence(network)

  expect_type(baseline, "list")
  expect_named(baseline, c("overall_prob", "sender_probs", "receiver_probs"))

  # Overall probability
  expect_equal(baseline$overall_prob, mean(network$tie))

  # Sender probs
  expect_s3_class(baseline$sender_probs, "tbl_df")
  expect_true("prob" %in% names(baseline$sender_probs))

  # Receiver probs
  expect_s3_class(baseline$receiver_probs, "tbl_df")
  expect_true("prob" %in% names(baseline$receiver_probs))
})

test_that("compute_dyadic_independence validation works", {
  invalid_data <- tibble::tibble(sender_id = 1:10, receiver_id = 1:10)

  expect_error(
    compute_dyadic_independence(invalid_data),
    "must have a `tie` column"
  )
})

test_that("reproducibility with seed works", {
  network1 <- simulate_dyadic_network(n_nodes = 10, seed = 42)
  network2 <- simulate_dyadic_network(n_nodes = 10, seed = 42)

  expect_equal(network1$tie, network2$tie)
  expect_equal(network1$sender_effect, network2$sender_effect)
})
