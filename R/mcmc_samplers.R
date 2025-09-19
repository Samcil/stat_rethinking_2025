#' Metropolis random-walk sampler and basic diagnostics
#'
#' Tidyverse-friendly Metropolis sampler that works with the target functions
#' in this package (functions that return a tibble with `neg_log_prob`). Returns
#' a tidy tibble of draws with per-iteration acceptance flags and log-prob.
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
#' # Define a 1D standard normal target
#' std_norm_target <- function(data, params) {
#'   x <- params[1]; tibble::tibble(param1 = x, neg_log_prob = 0.5 * x^2)
#' }
#' draws <- metropolis_sampler(std_norm_target, data = NULL, init = 0, n_samples = 100, step = 1, chains = 2)
#' @export
#' @importFrom tibble tibble
#' @importFrom stats rnorm runif var
metropolis_sampler <- function(target_fn, data, init, n_samples, step, chains = 1, seed = NULL, ...) {
  stopifnot(is.function(target_fn), n_samples >= 1L, chains >= 1L)
  if (!is.null(seed)) set.seed(seed)

  # Prepare initial states per chain
  if (is.numeric(init)) {
    d <- length(init)
    init_mat <- matrix(rep(init, each = chains), nrow = chains, byrow = TRUE)
  } else if (is.matrix(init) || is.data.frame(init)) {
    init_mat <- as.matrix(init)
    chains_in <- nrow(init_mat)
    d <- ncol(init_mat)
    stopifnot(chains_in == chains)
  } else {
    stop("init must be numeric vector or matrix/data.frame with one row per chain")
  }

  # Proposal scale
  if (length(step) == 1L) step <- rep(step, d)
  stopifnot(length(step) == d)

  param_names <- colnames(init_mat)
  if (is.null(param_names) || any(param_names == "")) {
    param_names <- paste0("param", seq_len(d))
  }

  one_chain <- function(chain_id) {
    q <- init_mat[chain_id, ]
    out <- matrix(NA_real_, nrow = n_samples, ncol = d + 3L)
    colnames(out) <- c("chain", "iter", "accept", param_names)

    # current log prob
    U_curr <- target_fn(data, q, ...)$neg_log_prob
    logp_curr <- -as.numeric(U_curr)

    for (i in seq_len(n_samples)) {
      prop <- q + stats::rnorm(d, 0, step)
      U_prop <- target_fn(data, prop, ...)$neg_log_prob
      logp_prop <- -as.numeric(U_prop)
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
    tibble::as_tibble(out, .name_repair = "minimal") |>
      dplyr::mutate(log_prob = dplyr::if_else(accept == 1L, NA_real_, NA_real_)) # placeholder; compute below
  }

  draws <- purrr::map_dfr(seq_len(chains), one_chain)
  # Recompute log_prob for all rows from stored params
  draws <- draws |>
    dplyr::group_by(chain) |>
    dplyr::mutate(
      log_prob = {
        Q <- dplyr::across(all_of(param_names))
        vals <- purrr::pmap_dbl(Q, function(...) {
          q <- c(...)
          -as.numeric(target_fn(data, q, ...)$neg_log_prob)
        })
        vals
      }
    ) |>
    dplyr::ungroup()

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
#' @export
#' @importFrom stats var
mcmc_rhat <- function(draws, param) {
  stopifnot(is.data.frame(draws), param %in% names(draws))
  dsplit <- draws |>
    dplyr::select(chain, iter, !!rlang::sym(param)) |>
    dplyr::group_by(chain) |>
    dplyr::arrange(iter, .by_group = TRUE) |>
    dplyr::summarise(mean = mean(.data[[param]]), var = stats::var(.data[[param]]), n = dplyr::n(), .groups = "drop")
  stopifnot(length(unique(dsplit$n)) == 1L)
  n <- dsplit$n[1]; m <- nrow(dsplit)
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
#' @export
#' @importFrom stats acf
mcmc_ess <- function(draws, param, max_lag = 100) {
  stopifnot(is.data.frame(draws), param %in% names(draws))
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
    s <- 0; j <- 1
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

