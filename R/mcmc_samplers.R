#' Metropolis random-walk sampler and basic diagnostics
#'
#' Tidyverse-friendly Metropolis sampler that works with the target functions
#' in this package (functions that return a tibble with `neg_log_prob`). Returns
#' a tidy tibble of draws with per-iteration acceptance flags and log-prob.
#'
#' Uses purrr for mapping and can parallelize multi-chain sampling and bulk
#' target evaluations via [purrr::in_parallel()].
#'
#' @family mcmc
#' @keywords internal
NULL

#' Metropolis random-walk sampler (Gaussian proposals)
#'
#' @param target_fn A function with signature `function(data, params, ...)` that
#'   returns a tibble with column `neg_log_prob` for the given parameter vector.
#' @param data A tibble/data.frame of the data consumed by `target_fn`.
#' @param init Numeric vector of initial parameter values of length d, or a
#'   matrix/data.frame with d columns (one row per chain).
#' @param n_samples Integer number of samples per chain (>= 1).
#' @param step Proposal scale (scalar or length-d vector of standard deviations).
#' @param chains Number of chains (>= 1). If `init` has multiple rows, this must
#'   match `nrow(init)`.
#' @param seed Optional integer seed for reproducibility.
#' @param ... Additional arguments passed to `target_fn`.
#' @return A tibble with columns: `chain`, `iter`, `accept` (0/1), `log_prob`,
#'   and one column per parameter (named from `init` if available, otherwise
#'   `param1`, `param2`, ...).
#' @examples
#' # Sequential usage (no parallel daemons set):
#' std_norm_target <- function(data, params) {
#'   x <- params[1]
#'   tibble::tibble(param1 = x, neg_log_prob = 0.5 * x^2)
#' }
#' draws <- metropolis_sampler(std_norm_target,
#'   data = NULL, init = 0,
#'   n_samples = 200, step = 1, chains = 2, seed = 42
#' )
#'
#' # Parallel usage with purrr::in_parallel() and mirai daemons
#' # Parallelization is optional and controlled by the user.
#' if (FALSE) { # interactive() && rlang::is_installed("mirai") && rlang::is_installed("carrier")
#'   # Set up 4 background processes
#'   mirai::daemons(4)
#'
#'   # Run multiple sampler configurations in parallel (fresh, self-contained function)
#'   cfg <- tibble::tibble(step = c(0.5, 1, 1.5), seed = c(1L, 2L, 3L))
#'   runs <- purrr::pmap(
#'     cfg,
#'     purrr::in_parallel(
#'       \(step, seed) metropolis_sampler(
#'         std_norm_target,
#'         data = NULL, init = 0,
#'         n_samples = 1000, step = step, chains = 4, seed = seed
#'       ),
#'       metropolis_sampler = metropolis_sampler,
#'       std_norm_target = std_norm_target
#'     )
#'   )
#'
#'   # Combine draws if desired
#'   all_draws <- dplyr::bind_rows(runs, .id = "run_id")
#'
#'   # Clean up when done
#'   mirai::daemons(0)
#' }
#' @seealso [mcmc_rhat()], [mcmc_ess()] (basic diagnostics)
#' @family mcmc
#' @export
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate group_by ungroup select all_of arrange summarise n bind_rows pull filter across
#' @importFrom purrr map map_dfr pmap_dbl in_parallel
#' @importFrom cli cli_abort cli_warn
#' @importFrom stats rnorm runif var acf
#' @importFrom rlang sym list2
metropolis_sampler <- function(target_fn, data, init, n_samples, step, chains = 1, seed = NULL, ...) {
  if (!is.function(target_fn)) cli::cli_abort("`target_fn` must be a function like function(data, params, ...) returning a tibble with `neg_log_prob`.")
  if (!is.numeric(n_samples) || length(n_samples) != 1L || n_samples < 1) cli::cli_abort("`n_samples` must be a single integer >= 1.")
  if (!is.numeric(chains) || length(chains) != 1L || chains < 1) cli::cli_abort("`chains` must be a single integer >= 1.")
  dots <- rlang::list2(...)
  if (!is.null(seed)) set.seed(seed)

  # Prepare initial states per chain
  if (is.numeric(init)) {
    d <- length(init)
    init_mat <- matrix(rep(init, each = chains), nrow = chains, byrow = TRUE)
  } else if (is.matrix(init) || is.data.frame(init)) {
    init_mat <- as.matrix(init)
    chains_in <- nrow(init_mat)
    d <- ncol(init_mat)
    if (chains_in != chains) cli::cli_abort("`chains` ({chains}) must equal number of rows in `init` ({chains_in}).")
  } else {
    cli::cli_abort("`init` must be a numeric vector or a matrix/data.frame with one row per chain.")
  }

  # Proposal scale
  if (length(step) == 1L) step <- rep(step, d)
  if (length(step) != d) cli::cli_abort("`step` must be length 1 or length equal to number of parameters ({d}).")

  param_names <- colnames(init_mat)
  if (is.null(param_names) || any(param_names == "")) {
    param_names <- paste0("param", seq_len(d))
  }

  eval_target <- function(q) {
    # Use do.call so we can pass through dots safely
    res <- try(do.call(target_fn, c(list(data = data, params = q), dots)), silent = TRUE)
    if (inherits(res, "try-error")) {
      cli::cli_abort("`target_fn` failed to evaluate on parameters. Ensure it accepts (data, params, ...) and returns a tibble with `neg_log_prob`.")
    }
    as.numeric(res$neg_log_prob)
  }

  one_chain <- function(chain_id) {
    q <- init_mat[chain_id, ]
    out <- matrix(NA_real_, nrow = n_samples, ncol = d + 3L)
    colnames(out) <- c("chain", "iter", "accept", param_names)

    # current log prob
    U_curr <- eval_target(q)
    logp_curr <- -U_curr

    for (i in seq_len(n_samples)) {
      prop <- q + stats::rnorm(d, 0, step)
      U_prop <- eval_target(prop)
      logp_prop <- -U_prop
      acc <- as.integer(exp(logp_prop - logp_curr) > stats::runif(1))
      if (acc == 1L) {
        q <- prop
        logp_curr <- logp_prop
      }
      out[i, ] <- c(chain_id, i, acc, q)
    }
    out <- as.data.frame(out)
    out$chain <- as.integer(out$chain)
    out$iter <- as.integer(out$iter)
    out$accept <- as.integer(out$accept)
    tibble::as_tibble(out, .name_repair = "minimal")
  }

  chain_ids <- seq_len(chains)
  # Heuristic: parallelize when we have multiple chains and large per-chain work
  draws_list <- if (chains >= 2 && n_samples >= 1000) {
    purrr::map(
      chain_ids,
      purrr::in_parallel(\(chain_id) one_chain(chain_id), one_chain = one_chain)
    )
  } else {
    purrr::map(chain_ids, one_chain)
  }
  draws <- dplyr::bind_rows(draws_list)

  # Recompute log_prob for all rows from stored params (optionally in parallel)
  Q <- dplyr::select(draws, dplyr::all_of(param_names))
  compute_lp <- function(...) {
    q <- c(...)
    -eval_target(q)
  }
  vals <- if (nrow(Q) >= 10000) {
    purrr::pmap_dbl(
      Q,
      purrr::in_parallel(\(...) compute_lp(...), compute_lp = compute_lp)
    )
  } else {
    purrr::pmap_dbl(Q, compute_lp)
  }
  draws$log_prob <- vals

  draws
}

#' Basic split-Rhat diagnostic for a parameter
#'
#' Computes the Gelman-Rubin R-hat statistic for a single parameter using the
#' standard formula. Assumes each chain has the same number of draws.
#'
#' @param draws Tibble returned by [metropolis_sampler()].
#' @param param Name of the parameter column to analyze (string).
#' @return Numeric R-hat value.
#' @seealso [metropolis_sampler()]
#' @family mcmc
#' @export
mcmc_rhat <- function(draws, param) {
  if (!is.data.frame(draws)) cli::cli_abort("`draws` must be a data frame/tibble from metropolis_sampler().")
  if (!param %in% names(draws)) cli::cli_abort("Parameter `{param}` not found in `draws`. Available columns include: {paste(names(draws), collapse = ", ")}.")
  dsplit <- draws |>
    dplyr::select(chain, iter, !!rlang::sym(param)) |>
    dplyr::group_by(chain) |>
    dplyr::arrange(iter, .by_group = TRUE) |>
    dplyr::summarise(mean = mean(.data[[param]]), var = stats::var(.data[[param]]), n = dplyr::n(), .groups = "drop")
  if (length(unique(dsplit$n)) != 1L) cli::cli_abort("Each chain must have the same number of draws to compute R-hat.")
  n <- dsplit$n[1]
  m <- nrow(dsplit)
  B <- n * stats::var(dsplit$mean)
  W <- mean(dsplit$var)
  var_hat <- ((n - 1) / n) * W + (B / n)
  sqrt(var_hat / W)
}

#' Basic effective sample size (ESS) using autocorrelation
#'
#' Estimates ESS by combining per-chain autocorrelation sequences until the
#' first negative pair. This is a simple heuristic intended for smoke tests.
#'
#' @inheritParams mcmc_rhat
#' @param max_lag Maximum lag to consider when estimating autocorrelation.
#' @return Numeric ESS estimate.
#' @seealso [metropolis_sampler()]
#' @family mcmc
#' @export
mcmc_ess <- function(draws, param, max_lag = 100) {
  if (!is.data.frame(draws)) cli::cli_abort("`draws` must be a data frame/tibble from metropolis_sampler().")
  if (!param %in% names(draws)) cli::cli_abort("Parameter `{param}` not found in `draws`. Available columns include: {paste(names(draws), collapse = ", ")}.")
  dsplit <- draws |>
    dplyr::select(chain, iter, !!rlang::sym(param)) |>
    dplyr::group_by(chain) |>
    dplyr::arrange(iter, .by_group = TRUE)
  chains <- unique(dsplit$chain)
  ac_sums <- numeric(length(chains))
  n_vec <- numeric(length(chains))
  for (i in seq_along(chains)) {
    xi <- dsplit |>
      dplyr::filter(chain == chains[i]) |>
      dplyr::pull(!!rlang::sym(param))
    n_vec[i] <- length(xi)
    ac <- stats::acf(xi, plot = FALSE, lag.max = min(max_lag, length(xi) - 1))$acf[-1]
    # sum positive adjacent pairs
    s <- 0
    j <- 1
    while (j < length(ac)) {
      pair_sum <- ac[j] + ac[j + 1]
      if (pair_sum < 0) break
      s <- s + pair_sum
      j <- j + 2
    }
    ac_sums[i] <- s
  }
  n <- min(n_vec)
  m <- length(chains)
  ess <- m * n / (1 + 2 * mean(ac_sums))
  as.numeric(ess)
}



#' Finite-difference numeric gradient for a target function
#'
#' Computes the central-difference gradient of the negative log-posterior U
#' returned by a target function. The target must return a tibble with column
#' `neg_log_prob` for the given parameter vector.
#'
#' @param target_fn Function of the form `function(data, params, ...)` returning a
#'   tibble with `neg_log_prob` for the provided parameter vector.
#' @param data Data passed through to `target_fn`.
#' @param params Numeric vector of parameters.
#' @param eps Step size for finite differences (default 1e-6).
#' @param ... Additional arguments passed to `target_fn`.
#' @return Numeric vector of length equal to `params`, giving dU/dparams.
#' @examples
#' std_norm_target <- function(data, params) {
#'   x <- params[1]
#'   tibble::tibble(param1 = x, neg_log_prob = 0.5 * x^2)
#' }
#' numeric_grad(std_norm_target, data = NULL, params = 0)
#' @seealso [metropolis_sampler()]
#' @family mcmc
#' @export
numeric_grad <- function(target_fn, data, params, eps = 1e-6, ...) {
  if (!is.function(target_fn)) cli::cli_abort("`target_fn` must be a function like function(data, params, ...) returning a tibble with `neg_log_prob`.")
  if (!is.numeric(params)) cli::cli_abort("`params` must be a numeric vector.")
  p <- length(params)
  g <- numeric(p)
  fU <- function(q) as.numeric(target_fn(data = data, params = q, ...)$neg_log_prob)
  for (i in seq_len(p)) {
    e <- rep(0, p)
    e[i] <- eps
    g[i] <- (fU(params + e) - fU(params - e)) / (2 * eps)
  }
  if (!is.null(names(params))) names(g) <- names(params) else names(g) <- paste0("param", seq_len(p))
  g
}

#' Single Metropolis random-walk step
#'
#' Proposes a new parameter vector from a Gaussian random walk and accepts it
#' with probability min(1, exp(logp_prop - logp_curr)). Returns the new state.
#'
#' @inheritParams metropolis_sampler
#' @param current Numeric vector of current parameter values.
#' @return A list with elements: `state` (numeric vector), `accept` (0/1), and
#'   `log_prob` (log probability at `state`).
#' @examples
#' std_norm_target <- function(data, params) {
#'   x <- params[1]
#'   tibble::tibble(param1 = x, neg_log_prob = 0.5 * x^2)
#' }
#' step1 <- metropolis_step(std_norm_target, data = NULL, current = 0, step = 1)
#' @seealso [metropolis_sampler()]
#' @family mcmc
#' @export
metropolis_step <- function(target_fn, data, current, step, ...) {
  if (!is.function(target_fn)) cli::cli_abort("`target_fn` must be a function like function(data, params, ...) returning a tibble with `neg_log_prob`.")
  if (!is.numeric(current)) cli::cli_abort("`current` must be a numeric vector.")
  d <- length(current)
  if (length(step) == 1L) step <- rep(step, d)
  if (length(step) != d) cli::cli_abort("`step` must be length 1 or length equal to number of parameters ({d}).")
  eval_target <- function(q) as.numeric(target_fn(data = data, params = q, ...)$neg_log_prob)
  U_curr <- eval_target(current)
  logp_curr <- -U_curr
  prop <- current + stats::rnorm(d, 0, step)
  U_prop <- eval_target(prop)
  logp_prop <- -U_prop
  acc <- as.integer(exp(logp_prop - logp_curr) > stats::runif(1))
  if (acc == 1L) {
    list(state = prop, accept = 1L, log_prob = logp_prop)
  } else {
    list(state = current, accept = 0L, log_prob = logp_curr)
  }
}
