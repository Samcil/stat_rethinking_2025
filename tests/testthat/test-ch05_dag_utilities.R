test_that("create_dag_positions creates horizontal layout", {
  nodes <- c("X", "Z", "Y")
  positions <- create_dag_positions(nodes, layout = "horizontal")
  
  expect_s3_class(positions, "tbl_df")
  expect_equal(nrow(positions), 3)
  expect_true(all(c("node", "x", "y") %in% names(positions)))
  expect_equal(positions$node, nodes)
  expect_true(all(positions$y == 0))
  expect_true(all(diff(positions$x) > 0))  # x increases
})


test_that("create_dag_positions creates circular layout", {
  nodes <- c("A", "B", "C", "D")
  positions <- create_dag_positions(nodes, layout = "circular")
  
  expect_equal(nrow(positions), 4)
  
  # Check that nodes are on a circle
  distances <- sqrt(positions$x^2 + positions$y^2)
  expect_true(all(abs(distances - 1) < 1e-10))
})


test_that("create_dag_positions accepts custom positions", {
  nodes <- c("X", "Y", "Z")
  custom <- data.frame(
    node = c("X", "Y", "Z"),
    x = c(0, 0.5, 1),
    y = c(0, 1, 0)
  )
  
  positions <- create_dag_positions(nodes, layout = "custom", 
                                   custom_positions = custom)
  
  expect_equal(nrow(positions), 3)
  expect_equal(positions$x, c(0, 0.5, 1))
  expect_equal(positions$y, c(0, 1, 0))
})


test_that("create_dag_positions validates inputs", {
  expect_error(create_dag_positions(c()))
  expect_error(create_dag_positions(c("A", "A")))  # duplicates
  expect_error(create_dag_positions(c("A", "B"), layout = "custom"))  # missing custom_positions
})


test_that("create_dag_edges works correctly", {
  nodes <- c("X", "Z", "Y")
  positions <- create_dag_positions(nodes)
  edges <- data.frame(from = c("X", "Z"), to = c("Z", "Y"))
  
  edge_data <- create_dag_edges(edges, positions)
  
  expect_s3_class(edge_data, "tbl_df")
  expect_equal(nrow(edge_data), 2)
  expect_true(all(c("from", "to", "x", "y", "xend", "yend") %in% names(edge_data)))
  
  # Check that coordinates match
  expect_equal(edge_data$x[1], positions$x[positions$node == "X"])
  expect_equal(edge_data$xend[2], positions$x[positions$node == "Y"])
})


test_that("create_dag_edges validates inputs", {
  positions <- create_dag_positions(c("X", "Y"))
  
  # Missing columns
  expect_error(create_dag_edges(data.frame(a = 1, b = 2), positions))
  
  # Undefined nodes
  bad_edges <- data.frame(from = "X", to = "Z")
  expect_error(create_dag_edges(bad_edges, positions))
})


test_that("simulate_confounding generates correct structure", {
  data <- simulate_confounding(
    n = 100,
    effect_z_x = 1,
    effect_z_y = 1,
    effect_x_y = 0,
    seed = 42
  )
  
  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 100)
  expect_true(all(c("z", "x", "y") %in% names(data)))
  
  # Check that X is correlated with Z
  cor_z_x <- cor(data$z, data$x)
  expect_true(abs(cor_z_x) > 0.5)
  
  # Check that Y is correlated with Z
  cor_z_y <- cor(data$z, data$y)
  expect_true(abs(cor_z_y) > 0.5)
})


test_that("simulate_confounding seed works", {
  data1 <- simulate_confounding(n = 50, seed = 123)
  data2 <- simulate_confounding(n = 50, seed = 123)
  data3 <- simulate_confounding(n = 50, seed = 456)
  
  expect_equal(data1$x, data2$x)
  expect_false(identical(data1$x, data3$x))
})


test_that("simulate_confounding validates inputs", {
  expect_error(simulate_confounding(n = 0))
  expect_error(simulate_confounding(noise_sd = -1))
})


test_that("simulate_collider_bias generates correct structure", {
  data <- simulate_collider_bias(
    n = 200,
    effect_x_c = 1,
    effect_y_c = 1,
    effect_x_y = 0,
    seed = 42
  )
  
  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 200)
  expect_true(all(c("x", "y", "c") %in% names(data)))
  
  # X and Y should be independent (or weakly correlated)
  cor_x_y <- cor(data$x, data$y)
  expect_true(abs(cor_x_y) < 0.2)
  
  # C should be correlated with both X and Y
  cor_x_c <- cor(data$x, data$c)
  cor_y_c <- cor(data$y, data$c)
  expect_true(abs(cor_x_c) > 0.4)
  expect_true(abs(cor_y_c) > 0.4)
})


test_that("simulate_collider_bias shows selection bias", {
  data <- simulate_collider_bias(n = 1000, effect_x_y = 0, seed = 42)
  
  # Full data: X and Y independent
  cor_full <- cor(data$x, data$y)
  expect_true(abs(cor_full) < 0.1)
  
  # Conditioned data: X and Y appear correlated
  data_conditioned <- data[data$c > 0, ]
  cor_conditioned <- cor(data_conditioned$x, data_conditioned$y)
  expect_true(abs(cor_conditioned) > 0.3)
})


test_that("simulate_post_treatment_bias generates mediation", {
  data <- simulate_post_treatment_bias(
    n = 100,
    effect_x_m = 1,
    effect_m_y = 1,
    effect_x_y = 0.5,
    seed = 42
  )
  
  expect_s3_class(data, "tbl_df")
  expect_equal(nrow(data), 100)
  expect_true(all(c("x", "m", "y") %in% names(data)))
  
  # X should cause M
  cor_x_m <- cor(data$x, data$m)
  expect_true(abs(cor_x_m) > 0.5)
  
  # M should cause Y
  cor_m_y <- cor(data$m, data$y)
  expect_true(abs(cor_m_y) > 0.5)
})


test_that("simulate_post_treatment_bias validates inputs", {
  expect_error(simulate_post_treatment_bias(n = 0))
  expect_error(simulate_post_treatment_bias(noise_sd = -1))
})
