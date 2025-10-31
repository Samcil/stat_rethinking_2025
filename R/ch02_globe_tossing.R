#' Simulate Globe Tossing Data
#'
#' Generate random globe toss observations (water vs. land) following
#' a specified true proportion of water.
#'
#' @param n Integer. Number of tosses to simulate.
#' @param prob_water Numeric. True proportion of water (0 to 1). Default is 0.7.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - toss: Toss number (1, 2, 3, ...)
#'   - result: Result ("W" for water, "L" for land)
#'   - result_numeric: Numeric encoding (1 for water, 0 for land)
#'   - cumulative_water: Cumulative count of water
#'   - cumulative_land: Cumulative count of land
#'
#' @family chapter02
#' @export
#'
#' @examples
#' # Simulate 10 globe tosses
#' tosses <- simulate_globe_tosses(10, prob_water = 0.7, seed = 42)
#'
#' # View cumulative counts
#' library(ggplot2)
#' ggplot(tosses, aes(x = toss, y = cumulative_water)) +
#'   geom_line() +
#'   geom_point()
simulate_globe_tosses <- function(n, prob_water = 0.7, seed = NULL) {
  checkmate::assert_int(n, lower = 1)
  validate_probability(prob_water, arg_name = "prob_water")
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    withr::with_seed(seed, {
      results <- rbinom(n, size = 1, prob = prob_water)
    })
  } else {
    results <- rbinom(n, size = 1, prob = prob_water)
  }
  
  tibble::tibble(
    toss = seq_len(n),
    result_numeric = results,
    result = ifelse(results == 1, "W", "L"),
    cumulative_water = cumsum(results),
    cumulative_land = cumsum(1 - results)
  )
}


#' Compute Beta Posterior Updates
#'
#' Calculate sequential Bayesian updates using Beta-Binomial conjugacy
#' for globe tossing data.
#'
#' @param observations Character or integer vector. Observations ("W"/"L" or 1/0).
#' @param prior_alpha Numeric. Alpha parameter of Beta prior. Default is 1 (uniform).
#' @param prior_beta Numeric. Beta parameter of Beta prior. Default is 1 (uniform).
#'
#' @return Tibble with columns:
#'   - toss: Observation number
#'   - observation: The observation at this step
#'   - alpha: Alpha parameter after this observation
#'   - beta: Beta parameter after this observation
#'   - posterior_mean: Mean of posterior (alpha / (alpha + beta))
#'   - posterior_mode: Mode of posterior ((alpha - 1) / (alpha + beta - 2))
#'
#' @family chapter02
#' @export
#'
#' @examples
#' # Simulate some tosses and compute posteriors
#' tosses <- simulate_globe_tosses(5, prob_water = 0.7, seed = 42)
#' posteriors <- compute_beta_updates(tosses$result)
#'
#' # Visualize posterior evolution
#' library(ggplot2)
#' ggplot(posteriors, aes(x = toss, y = posterior_mean)) +
#'   geom_line() +
#'   geom_point() +
#'   ylim(0, 1)
compute_beta_updates <- function(observations, prior_alpha = 1, prior_beta = 1) {
  checkmate::assert_vector(observations, min.len = 1)
  validate_positive(c(prior_alpha, prior_beta), allow_zero = FALSE)
  
  # Convert to numeric if character
  if (is.character(observations)) {
    obs_numeric <- ifelse(observations == "W", 1, 0)
  } else {
    obs_numeric <- as.integer(observations)
  }
  
  # Initialize
  alpha <- prior_alpha
  beta <- prior_beta
  
  # Track updates
  updates <- purrr::map_dfr(seq_along(obs_numeric), function(i) {
    # Update parameters
    alpha <<- alpha + obs_numeric[i]
    beta <<- beta + (1 - obs_numeric[i])
    
    # Calculate summaries
    posterior_mean <- alpha / (alpha + beta)
    posterior_mode <- if (alpha > 1 && beta > 1) {
      (alpha - 1) / (alpha + beta - 2)
    } else {
      NA_real_
    }
    
    tibble::tibble(
      toss = i,
      observation = observations[i],
      alpha = alpha,
      beta = beta,
      posterior_mean = posterior_mean,
      posterior_mode = posterior_mode
    )
  })
  
  updates
}


#' Generate Beta Posterior Density Data
#'
#' Create data for plotting Beta posterior distributions at each step
#' of sequential Bayesian updating.
#'
#' @param posterior_updates Tibble from \code{compute_beta_updates()}.
#' @param n_points Integer. Number of points for density curve. Default is 100.
#'
#' @return Tibble with columns:
#'   - toss: Observation number
#'   - p: Proportion water (x-axis values)
#'   - density: Posterior density at p
#'   - alpha: Alpha parameter
#'   - beta: Beta parameter
#'
#' @family chapter02
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' tosses <- simulate_globe_tosses(5, prob_water = 0.7, seed = 42)
#' posteriors <- compute_beta_updates(tosses$result)
#' density_data <- generate_posterior_density(posteriors)
#'
#' # Plot posterior evolution
#' ggplot(density_data, aes(x = p, y = density, group = toss)) +
#'   geom_line(aes(alpha = toss / max(toss))) +
#'   labs(x = "Proportion water", y = "Density") +
#'   theme_rethinking()
generate_posterior_density <- function(posterior_updates, n_points = 100) {
  validate_tibble(
    posterior_updates,
    required_cols = c("toss", "alpha", "beta")
  )
  checkmate::assert_int(n_points, lower = 10)
  
  p_grid <- seq(0, 1, length.out = n_points)
  
  purrr::map_dfr(seq_len(nrow(posterior_updates)), function(i) {
    row <- posterior_updates[i, ]
    
    tibble::tibble(
      toss = row$toss,
      p = p_grid,
      density = dbeta(p_grid, row$alpha, row$beta),
      alpha = row$alpha,
      beta = row$beta
    )
  })
}


#' Sample from Posterior Predictive Distribution
#'
#' Generate samples from the posterior predictive distribution for
#' future globe tosses given observed data.
#'
#' @param posterior_alpha Numeric. Alpha parameter of posterior Beta distribution.
#' @param posterior_beta Numeric. Beta parameter of posterior Beta distribution.
#' @param n_tosses Integer. Number of future tosses to predict.
#' @param n_samples Integer. Number of posterior predictive samples. Default is 1000.
#' @param seed Integer. Random seed for reproducibility. Default is NULL.
#'
#' @return Tibble with columns:
#'   - sample: Sample number
#'   - p_water: Sampled proportion of water
#'   - n_water: Number of water results in n_tosses
#'
#' @family chapter02
#' @export
#'
#' @examples
#' # After observing 6 water and 3 land
#' posterior_samples <- sample_posterior_predictive(
#'   posterior_alpha = 7,  # 1 + 6
#'   posterior_beta = 4,   # 1 + 3
#'   n_tosses = 9,
#'   n_samples = 1000,
#'   seed = 42
#' )
#'
#' # Plot distribution of predicted water counts
#' library(ggplot2)
#' ggplot(posterior_samples, aes(x = n_water)) +
#'   geom_histogram(binwidth = 1, fill = "steelblue") +
#'   labs(x = "Number of water in 9 tosses", y = "Count")
sample_posterior_predictive <- function(posterior_alpha, posterior_beta,
                                       n_tosses, n_samples = 1000,
                                       seed = NULL) {
  validate_positive(c(posterior_alpha, posterior_beta), allow_zero = FALSE)
  checkmate::assert_int(n_tosses, lower = 1)
  checkmate::assert_int(n_samples, lower = 1)
  
  if (!is.null(seed)) {
    checkmate::assert_int(seed)
    set.seed(seed)
  }
  
  # Sample from posterior
  p_samples <- rbeta(n_samples, posterior_alpha, posterior_beta)
  
  # For each p, sample predicted data
  predicted <- purrr::map_int(p_samples, function(p) {
    rbinom(1, size = n_tosses, prob = p)
  })
  
  tibble::tibble(
    sample = seq_len(n_samples),
    p_water = p_samples,
    n_water = predicted
  )
}


#' Compute Posterior Intervals
#'
#' Calculate credible intervals from Beta posterior distribution.
#'
#' @param alpha Numeric. Alpha parameter of Beta distribution.
#' @param beta Numeric. Beta parameter of Beta distribution.
#' @param prob Numeric. Probability mass for interval (0 to 1). Default is 0.89.
#'
#' @return Named numeric vector with lower and upper bounds.
#'
#' @family chapter02
#' @export
#'
#' @examples
#' # After observing 6 water and 3 land
#' compute_posterior_interval(alpha = 7, beta = 4, prob = 0.89)
compute_posterior_interval <- function(alpha, beta, prob = 0.89) {
  validate_positive(c(alpha, beta), allow_zero = FALSE)
  validate_probability(prob, arg_name = "prob")
  
  lower_tail <- (1 - prob) / 2
  upper_tail <- 1 - lower_tail
  
  c(
    lower = qbeta(lower_tail, alpha, beta),
    upper = qbeta(upper_tail, alpha, beta)
  )
}
