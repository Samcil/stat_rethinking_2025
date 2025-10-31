test_that("simulate_metropolis generates correct structure", {
  chain <- simulate_metropolis(n_steps = 100, seed = 42)
  
  expect_s3_class(chain, "tbl_df")
  expect_equal(nrow(chain), 100)
  expect_true(all(c("step", "island", "proposal", "accepted") %in% names(chain)))
  
  # Check islands are in valid range
  expect_true(all(chain$island >= 1 & chain$island <= 10))
  expect_true(all(chain$proposal >= 1 & chain$proposal <= 10))
})


test_that("simulate_metropolis samples target distribution", {
  # Run long chain
  chain <- simulate_metropolis(n_steps = 10000, seed = 42)
  
  # Check that distribution approximates target (proportional to island number)
  island_counts <- table(chain$island)
  proportions <- island_counts / sum(island_counts)
  
  # Higher islands should be visited more often
  expect_true(proportions[10] > proportions[5])
  expect_true(proportions[5] > proportions[1])
})


test_that("simulate_metropolis respects custom target", {
  # Uniform target
  uniform_target <- function(i) 1
  chain <- simulate_metropolis(
    n_steps = 5000,
    target_distribution = uniform_target,
    seed = 42
  )
  
  # Should visit all islands roughly equally
  island_counts <- table(chain$island)
  proportions <- island_counts / sum(island_counts)
  
  # All proportions should be close to 0.1
  expect_true(all(abs(proportions - 0.1) < 0.05))
})


test_that("simulate_metropolis validates inputs", {
  expect_error(simulate_metropolis(n_steps = 0))
  expect_error(simulate_metropolis(n_islands = 1))
  expect_error(simulate_metropolis(start_island = 0))
  expect_error(simulate_metropolis(start_island = 11, n_islands = 10))
})


test_that("compute_mcmc_diagnostics works with vector", {
  samples <- rnorm(1000)
  diag <- compute_mcmc_diagnostics(samples)
  
  expect_type(diag, "list")
  expect_true(all(c("mean", "sd", "n_unique", "autocorrelation") %in% names(diag)))
  
  expect_equal(diag$mean, mean(samples), tolerance = 1e-10)
  expect_equal(diag$sd, sd(samples), tolerance = 1e-10)
  
  expect_s3_class(diag$autocorrelation, "tbl_df")
  expect_true("lag" %in% names(diag$autocorrelation))
})


test_that("compute_mcmc_diagnostics works with tibble", {
  chain <- simulate_metropolis(n_steps = 500, seed = 42)
  diag <- compute_mcmc_diagnostics(chain, var = "island")
  
  expect_type(diag, "list")
  expect_true(!is.na(diag$acceptance_rate))
  expect_true(diag$acceptance_rate >= 0 && diag$acceptance_rate <= 1)
})


test_that("compute_mcmc_diagnostics validates inputs", {
  expect_error(compute_mcmc_diagnostics(data.frame(x = 1:10)))  # missing var
  expect_error(compute_mcmc_diagnostics(c(), max_lag = -1))
})


test_that("simulate_multiple_chains creates multiple chains", {
  chains <- simulate_multiple_chains(n_chains = 3, n_steps = 100, seed = 42)
  
  expect_s3_class(chains, "tbl_df")
  expect_true("chain" %in% names(chains))
  expect_equal(length(unique(chains$chain)), 3)
  expect_equal(nrow(chains), 3 * 100)
})


test_that("simulate_multiple_chains uses different starting points", {
  chains <- simulate_multiple_chains(
    n_chains = 4,
    n_steps = 10,
    start_islands = c(1, 3, 6, 10),
    seed = 42
  )
  
  # Check that chains start at specified islands
  starts <- chains %>%
    dplyr::filter(step == 1) %>%
    dplyr::pull(island)
  
  expect_equal(starts, c(1, 3, 6, 10))
})


test_that("simulate_multiple_chains validates inputs", {
  expect_error(simulate_multiple_chains(n_chains = 0))
  expect_error(simulate_multiple_chains(n_steps = 0))
  expect_error(simulate_multiple_chains(n_chains = 2, start_islands = c(1)))
})


test_that("compute_rhat calculates convergence diagnostic", {
  chains <- simulate_multiple_chains(n_chains = 4, n_steps = 1000, seed = 42)
  rhat <- compute_rhat(chains, var = "island", warmup = 100)
  
  expect_type(rhat, "double")
  expect_length(rhat, 1)
  expect_true(rhat > 0)
  
  # For a well-mixed chain, R-hat should be close to 1
  expect_true(rhat < 1.2)
})


test_that("compute_rhat handles warmup correctly", {
  chains <- simulate_multiple_chains(n_chains = 2, n_steps = 200, seed = 42)
  
  rhat_no_warmup <- compute_rhat(chains, var = "island", warmup = 0)
  rhat_with_warmup <- compute_rhat(chains, var = "island", warmup = 50)
  
  # Both should be valid
  expect_true(rhat_no_warmup > 0)
  expect_true(rhat_with_warmup > 0)
})


test_that("compute_rhat validates inputs", {
  chains <- simulate_multiple_chains(n_chains = 2, n_steps = 100, seed = 42)
  
  expect_error(compute_rhat(data.frame(), var = "island"))
  expect_error(compute_rhat(chains, var = "nonexistent"))
  expect_error(compute_rhat(chains, var = "island", warmup = -1))
})


test_that("create_trace_plot_data reshapes correctly", {
  chain <- simulate_metropolis(n_steps = 100, seed = 42)
  trace_data <- create_trace_plot_data(chain, vars = "island")
  
  expect_s3_class(trace_data, "tbl_df")
  expect_true(all(c("step", "variable", "value") %in% names(trace_data)))
  expect_equal(nrow(trace_data), 100)  # One variable, 100 steps
})


test_that("create_trace_plot_data handles thinning", {
  chain <- simulate_metropolis(n_steps = 100, seed = 42)
  trace_data_thin <- create_trace_plot_data(chain, vars = "island", thin = 10)
  
  expect_equal(nrow(trace_data_thin), 10)  # 100 / 10
})


test_that("create_trace_plot_data handles multiple variables", {
  chain <- tibble::tibble(
    step = 1:50,
    var1 = rnorm(50),
    var2 = rnorm(50)
  )
  
  trace_data <- create_trace_plot_data(chain, vars = c("var1", "var2"))
  
  expect_equal(nrow(trace_data), 100)  # 2 variables × 50 steps
  expect_equal(length(unique(trace_data$variable)), 2)
})


test_that("create_trace_plot_data validates inputs", {
  expect_error(create_trace_plot_data(data.frame()))
  expect_error(create_trace_plot_data(tibble::tibble(step = 1:10), thin = 0))
})
