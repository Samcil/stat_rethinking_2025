#' Simulate Metropolis Algorithm (King Markov Example)
#'
#' Implement the Metropolis algorithm using the "King Markov" island hopping
#' example to illustrate MCMC sampling.
#'
#' @param n_steps Integer. Number of MCMC steps. Default is 10000.
#' @param n_islands Integer. Number of islands. Default is 10.
#' @param start_island Integer. Starting island. Default is 10.
#' @param target_distribution Function. Function that takes island number and
#'   returns unnormalized target density. Default is proportional to island number.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - step: Step number
#'   - island: Current island
#'   - proposal: Proposed island
#'   - accepted: Whether proposal was accepted
#'
#' @family chapter08
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Simulate King Markov
#' chain <- simulate_metropolis(n_steps = 1000, seed = 42)
#'
#' # Plot trace
#' ggplot(chain, aes(x = step, y = island)) +
#'   geom_line() +
#'   labs(title = "Metropolis Algorithm: Island Hopping") +
#'   theme_rethinking()
#'
#' # Check distribution
#' ggplot(chain, aes(x = island)) +
#'   geom_histogram(aes(y = ..density..), binwidth = 1) +
#'   stat_function(fun = function(x) x / sum(1:10), color = "red") +
#'   labs(title = "Sampled vs. Target Distribution")
simulate_metropolis <- function(n_steps = 10000,
                               n_islands = 10,
                               start_island = 10,
                               target_distribution = function(i) i,
                               seed = NULL) {
  checkmate::assert_int(n_steps, lower = 1)
  checkmate::assert_int(n_islands, lower = 2)
  checkmate::assert_int(start_island, lower = 1, upper = n_islands)
  checkmate::assert_function(target_distribution, args = "i")
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Initialize
  current <- start_island
  islands <- integer(n_steps)
  proposals <- integer(n_steps)
  accepted <- logical(n_steps)
  
  for (i in seq_len(n_steps)) {
    # Record current position
    islands[i] <- current
    
    # Generate proposal (move left or right)
    proposal <- current + sample(c(-1, 1), size = 1)
    
    # Wrap around
    if (proposal < 1) proposal <- n_islands
    if (proposal > n_islands) proposal <- 1
    
    proposals[i] <- proposal
    
    # Calculate acceptance probability
    prob_current <- target_distribution(current)
    prob_proposal <- target_distribution(proposal)
    prob_move <- min(1, prob_proposal / prob_current)
    
    # Accept or reject
    if (runif(1) < prob_move) {
      current <- proposal
      accepted[i] <- TRUE
    } else {
      accepted[i] <- FALSE
    }
  }
  
  tibble::tibble(
    step = seq_len(n_steps),
    island = islands,
    proposal = proposals,
    accepted = accepted
  )
}


#' Compute MCMC Diagnostics
#'
#' Calculate diagnostic statistics for MCMC chains including
#' acceptance rate, effective sample size, and autocorrelation.
#'
#' @param chain Numeric vector or tibble. MCMC chain samples.
#' @param var Character. Variable name if chain is a tibble. Default is NULL.
#' @param max_lag Integer. Maximum lag for autocorrelation. Default is 100.
#'
#' @return List with components:
#'   - acceptance_rate: Proportion of accepted proposals (if available)
#'   - mean: Mean of samples
#'   - sd: Standard deviation of samples
#'   - n_unique: Number of unique values
#'   - autocorrelation: Tibble with lag and autocorrelation
#'
#' @family chapter08
#' @export
#'
#' @examples
#' # Simulate chain
#' chain <- simulate_metropolis(n_steps = 1000, seed = 42)
#'
#' # Compute diagnostics
#' diagnostics <- compute_mcmc_diagnostics(chain, var = "island")
#' diagnostics$acceptance_rate
#' diagnostics$mean
compute_mcmc_diagnostics <- function(chain, var = NULL, max_lag = 100) {
  checkmate::assert_int(max_lag, lower = 1)
  
  # Extract vector of samples
  if (is.data.frame(chain)) {
    if (is.null(var)) {
      rlang::abort("var must be specified when chain is a data frame")
    }
    samples <- chain[[var]]
    
    # Try to get acceptance rate if 'accepted' column exists
    acceptance_rate <- if ("accepted" %in% names(chain)) {
      mean(chain$accepted, na.rm = TRUE)
    } else {
      NA_real_
    }
  } else {
    samples <- as.numeric(chain)
    acceptance_rate <- NA_real_
  }
  
  checkmate::assert_numeric(samples, min.len = 1)
  
  # Compute autocorrelation
  acf_result <- stats::acf(samples, lag.max = max_lag, plot = FALSE)
  
  autocorr_data <- tibble::tibble(
    lag = as.integer(acf_result$lag),
    autocorrelation = as.numeric(acf_result$acf)
  )
  
  list(
    acceptance_rate = acceptance_rate,
    mean = mean(samples, na.rm = TRUE),
    sd = sd(samples, na.rm = TRUE),
    n_unique = length(unique(samples)),
    autocorrelation = autocorr_data
  )
}


#' Simulate Multiple MCMC Chains
#'
#' Run multiple independent MCMC chains for convergence diagnostics.
#'
#' @param n_chains Integer. Number of chains to run. Default is 4.
#' @param n_steps Integer. Number of steps per chain. Default is 1000.
#' @param start_islands Integer vector. Starting islands for each chain.
#'   If NULL, starts are spread across islands. Default is NULL.
#' @param ... Additional arguments passed to \code{simulate_metropolis()}.
#'
#' @return Tibble with columns:
#'   - chain: Chain identifier
#'   - step: Step number within chain
#'   - island: Current island
#'   - accepted: Whether proposal was accepted
#'
#' @family chapter08
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Run multiple chains
#' chains <- simulate_multiple_chains(n_chains = 4, n_steps = 500, seed = 42)
#'
#' # Plot all chains
#' ggplot(chains, aes(x = step, y = island, color = factor(chain))) +
#'   geom_line(alpha = 0.7) +
#'   labs(title = "Multiple MCMC Chains", color = "Chain") +
#'   theme_rethinking()
simulate_multiple_chains <- function(n_chains = 4,
                                    n_steps = 1000,
                                    start_islands = NULL,
                                    ...) {
  checkmate::assert_int(n_chains, lower = 1)
  checkmate::assert_int(n_steps, lower = 1)
  
  # Get n_islands from ... or use default
  dots <- list(...)
  n_islands <- if ("n_islands" %in% names(dots)) dots$n_islands else 10
  
  if (is.null(start_islands)) {
    # Spread starting points across islands
    start_islands <- round(seq(1, n_islands, length.out = n_chains))
  }
  
  checkmate::assert_integer(start_islands, len = n_chains, 
                           lower = 1, upper = n_islands)
  
  # Run chains
  purrr::map2_dfr(seq_len(n_chains), start_islands, function(chain_id, start) {
    result <- simulate_metropolis(
      n_steps = n_steps,
      start_island = start,
      ...
    )
    
    result %>%
      dplyr::mutate(chain = chain_id) %>%
      dplyr::select(chain, dplyr::everything())
  })
}


#' Compute Gelman-Rubin R-hat Statistic
#'
#' Calculate the Gelman-Rubin convergence diagnostic for multiple chains.
#'
#' @param chains Tibble from \code{simulate_multiple_chains()} or similar,
#'   with columns chain and the variable of interest.
#' @param var Character. Name of variable to compute R-hat for.
#' @param warmup Integer. Number of warmup steps to discard. Default is 0.
#'
#' @return Numeric. R-hat statistic. Values close to 1.0 indicate convergence.
#'
#' @family chapter08
#' @export
#'
#' @examples
#' # Run multiple chains
#' chains <- simulate_multiple_chains(n_chains = 4, n_steps = 1000, seed = 42)
#'
#' # Compute R-hat
#' rhat <- compute_rhat(chains, var = "island", warmup = 100)
#' rhat  # Should be close to 1.0 if converged
compute_rhat <- function(chains, var, warmup = 0) {
  validate_tibble(chains, required_cols = c("chain", var))
  checkmate::assert_int(warmup, lower = 0)
  
  # Remove warmup
  if (warmup > 0) {
    chains <- chains %>%
      dplyr::group_by(chain) %>%
      dplyr::slice(-(1:warmup)) %>%
      dplyr::ungroup()
  }
  
  # Split into list of chains
  chain_list <- chains %>%
    dplyr::group_by(chain) %>%
    dplyr::summarise(samples = list(.data[[var]]), .groups = "drop") %>%
    dplyr::pull(samples)
  
  n_chains <- length(chain_list)
  n_samples <- length(chain_list[[1]])
  
  # Calculate within-chain variance
  W <- mean(vapply(chain_list, var, numeric(1)))
  
  # Calculate between-chain variance
  chain_means <- vapply(chain_list, mean, numeric(1))
  B <- var(chain_means) * n_samples
  
  # Calculate R-hat
  var_plus <- ((n_samples - 1) / n_samples) * W + (1 / n_samples) * B
  rhat <- sqrt(var_plus / W)
  
  rhat
}


#' Create MCMC Trace Plot Data
#'
#' Prepare data for creating trace plots of MCMC samples.
#'
#' @param chain Tibble with MCMC samples.
#' @param vars Character vector. Variables to include. If NULL, uses all numeric columns.
#' @param thin Integer. Thinning interval (keep every nth sample). Default is 1.
#'
#' @return Tibble in long format suitable for ggplot2 faceting.
#'
#' @family chapter08
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # Simulate chain
#' chain <- simulate_metropolis(n_steps = 1000, seed = 42)
#'
#' # Prepare trace plot data
#' trace_data <- create_trace_plot_data(chain, vars = "island")
#'
#' # Plot
#' ggplot(trace_data, aes(x = step, y = value)) +
#'   geom_line() +
#'   facet_wrap(~ variable, scales = "free_y") +
#'   theme_rethinking()
create_trace_plot_data <- function(chain, vars = NULL, thin = 1) {
  validate_tibble(chain, min_rows = 1)
  checkmate::assert_int(thin, lower = 1)
  
  # Thin if requested
  if (thin > 1) {
    chain <- chain %>%
      dplyr::slice(seq(1, dplyr::n(), by = thin))
  }
  
  # Select variables
  if (is.null(vars)) {
    vars <- names(chain)[vapply(chain, is.numeric, logical(1))]
  }
  
  checkmate::assert_character(vars, min.len = 1)
  
  # Reshape to long format
  chain %>%
    dplyr::select(dplyr::all_of(c("step", vars))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(vars),
      names_to = "variable",
      values_to = "value"
    )
}
