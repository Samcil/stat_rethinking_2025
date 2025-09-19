#' Simulation utilities (tidyverse-friendly)
#'
#' A small collection of simulation helpers refactored from the teaching
#' scripts into tidyverse-friendly, data-first APIs that return tibbles.
#'
#' - Predator–prey (Lotka–Volterra) time series via simple Euler updates,
#'   adapted from `sim_lynx_hare()` in scripts/19_gen_lin_madness.r
#' - Beta–Binomial posterior predictive draws, adapted from
#'   scripts/02_predictive_simulation.r
#' - Linear Gaussian causal DAG snippets for confounding/post-treatment bias,
#'   adapted from scripts/06_simulations_bad_controls.r
#'
#' @family simulations
#' @keywords internal
NULL

#' Simulate predator–prey (Lotka–Volterra) dynamics
#'
#' Deterministic Euler updates for a simple predator–prey system over
#' `n_steps` time steps.
#'
#' Model:
#'   H[t+1] = H[t] + dt * H[t] * (theta[1] - theta[2] * L[t])
#'   L[t+1] = L[t] + dt * L[t] * (theta[3] * H[t] - theta[4])
#'
#' @param n_steps Integer >= 1, number of time steps to simulate.
#' @param init Numeric length-2 c(L0, H0) initial abundances for Lynx (L) and Hare (H).
#' @param theta Numeric length-4 parameters c(rH, aHL, aLH, mL).
#'   See formula above.
#' @param dt Positive numeric step size for Euler updates (default 0.002).
#' @return A tibble with columns `step`, `L`, `H`.
#' @examples
#' sim <- simulate_lynx_hare(n_steps = 5, init = c(10, 20),
#'                           theta = c(0.5, 0.05, 0.025, 0.5), dt = 0.01)
#' @export
#' @importFrom tibble tibble
#' @importFrom cli cli_abort
simulate_lynx_hare <- function(n_steps, init, theta, dt = 0.002) {
  if (!is.numeric(n_steps) || length(n_steps) != 1L || n_steps < 1) cli::cli_abort("`n_steps` must be a single integer >= 1.")
  if (!is.numeric(init) || length(init) != 2L) cli::cli_abort("`init` must be a numeric vector of length 2: c(L0, H0).")
  if (!is.numeric(theta) || length(theta) != 4L) cli::cli_abort("`theta` must be numeric length 4: c(rH, aHL, aLH, mL).")
  if (!is.numeric(dt) || length(dt) != 1L || dt <= 0) cli::cli_abort("`dt` must be a single positive number.")

  L <- numeric(n_steps); H <- numeric(n_steps)
  L[1] <- init[1]; H[1] <- init[2]
  for (i in seq_len(n_steps - 1L)) {
    H[i + 1L] <- H[i] + dt * H[i] * (theta[1] - theta[2] * L[i])
    L[i + 1L] <- L[i] + dt * L[i] * (theta[3] * H[i] - theta[4])
  }
  tibble::tibble(step = seq_len(n_steps), L = L, H = H)
}

#' Beta–Binomial posterior predictive draws
#'
#' Draws posterior predictive counts by first sampling probabilities from a
#' Beta(alpha, beta) distribution, then Binomial(size, p) counts.
#'
#' @param n_draws Integer >= 1, number of draws.
#' @param size Non-negative integer Binomial size per draw.
#' @param alpha Positive numeric Beta alpha shape.
#' @param beta Positive numeric Beta beta shape.
#' @param seed Optional integer seed for reproducibility.
#' @return A tibble with columns `draw`, `p`, and `w` (success counts).
#' @examples
#' pp <- simulate_beta_binomial(n_draws = 1000, size = 9, alpha = 7, beta = 4)
#' @export
#' @importFrom tibble tibble
#' @importFrom stats rbeta rbinom
#' @importFrom cli cli_abort
simulate_beta_binomial <- function(n_draws, size, alpha, beta, seed = NULL) {
  if (!is.numeric(n_draws) || length(n_draws) != 1L || n_draws < 1) cli::cli_abort("`n_draws` must be a single integer >= 1.")
  if (!is.numeric(size) || length(size) != 1L || size < 0) cli::cli_abort("`size` must be a single non-negative integer.")
  if (!is.numeric(alpha) || alpha <= 0) cli::cli_abort("`alpha` must be positive numeric.")
  if (!is.numeric(beta) || beta <= 0) cli::cli_abort("`beta` must be positive numeric.")
  if (!is.null(seed)) set.seed(seed)

  p <- stats::rbeta(n_draws, alpha, beta)
  w <- stats::rbinom(n_draws, size = size, prob = p)
  tibble::tibble(draw = seq_len(n_draws), p = p, w = w)
}

#' Simulate linear DAG with post-treatment bias (bad controls)
#'
#' Simulates a simple linear-Gaussian system:
#'   X ~ Normal(0, 1)
#'   u ~ Normal(0, 1)
#'   Z ~ Normal(bXZ * X + u, 1)
#'   Y ~ Normal(bZY * Z + u, 1)
#' Returning a tibble of (X, Z, Y). Adapted from functions in
#' scripts/06_simulations_bad_controls.r.
#'
#' @param n Integer >= 1, number of observations.
#' @param bXZ Numeric, coefficient from X -> Z.
#' @param bZY Numeric, coefficient from Z -> Y.
#' @param seed Optional integer seed.
#' @return A tibble with columns `X`, `Z`, and `Y`.
#' @examples
#' d <- simulate_bad_controls_post_treatment(n = 1000, bXZ = 1, bZY = 1, seed = 1)
#' @export
#' @importFrom tibble tibble
#' @importFrom stats rnorm
#' @importFrom cli cli_abort
simulate_bad_controls_post_treatment <- function(n = 100, bXZ = 1, bZY = 1, seed = NULL) {
  if (!is.numeric(n) || length(n) != 1L || n < 1) cli::cli_abort("`n` must be a single integer >= 1.")
  if (!is.numeric(bXZ) || !is.numeric(bZY)) cli::cli_abort("`bXZ` and `bZY` must be numeric.")
  if (!is.null(seed)) set.seed(seed)

  X <- stats::rnorm(n)
  u <- stats::rnorm(n)
  Z <- stats::rnorm(n, mean = bXZ * X + u)
  Y <- stats::rnorm(n, mean = bZY * Z + u)
  tibble::tibble(X = X, Z = Z, Y = Y)
}

