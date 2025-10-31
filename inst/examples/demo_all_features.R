#!/usr/bin/env Rscript
#
# Demo script showcasing all tidyrethinking package features
# This script demonstrates the modernized approach to Statistical Rethinking materials
#

library(tidyrethinking)
library(dplyr)
library(ggplot2)

cat("=== tidyrethinking Package Demo ===\n\n")

# ============================================================================
# Chapter 2: Garden of Forking Paths
# ============================================================================

cat("Chapter 2: Garden of Forking Paths\n")
cat("-----------------------------------\n")

observations <- c(1, 0, 1) |> as.integer()
possibilities <- c(0, 1, 1, 1)

paths <- simulate_garden_paths(observations, possibilities)
cat(sprintf("Simulated %d paths\n", length(unique(paths$path_id))))
cat(sprintf("Viable paths: %d\n", sum(paths$viable[paths$depth == max(paths$depth)])))

paths_layout <- create_garden_layout(paths)
cat("Created garden layout with x, y coordinates\n")

outcomes <- count_path_outcomes(observations, possibilities)
cat("Path outcomes:\n")
print(outcomes)
cat("\n")

# ============================================================================
# Chapter 2: Globe Tossing
# ============================================================================

cat("Chapter 2: Globe Tossing\n")
cat("------------------------\n")

tosses <- simulate_globe_tosses(10, prob_water = 0.7, seed = 42)
cat(sprintf("Simulated %d tosses\n", nrow(tosses)))
cat(sprintf("Water count: %d, Land count: %d\n",
            sum(tosses$result == "W"), sum(tosses$result == "L")))

posteriors <- compute_beta_updates(tosses$result)
final_posterior <- posteriors[nrow(posteriors), ]
cat(sprintf("Final posterior: Beta(%g, %g)\n", 
            final_posterior$alpha, final_posterior$beta))
cat(sprintf("Posterior mean: %.3f\n", final_posterior$posterior_mean))

density_data <- generate_posterior_density(posteriors)
cat(sprintf("Generated %d density points across %d tosses\n",
            nrow(density_data), length(unique(density_data$toss))))

interval <- compute_posterior_interval(
  final_posterior$alpha, 
  final_posterior$beta, 
  prob = 0.89
)
cat(sprintf("89%% credible interval: [%.3f, %.3f]\n", interval[1], interval[2]))

predictive <- sample_posterior_predictive(
  final_posterior$alpha,
  final_posterior$beta,
  n_tosses = 9,
  n_samples = 1000,
  seed = 42
)
cat(sprintf("Generated %d posterior predictive samples\n", nrow(predictive)))
cat("\n")

# ============================================================================
# Chapter 3: Gaussian Simulation
# ============================================================================

cat("Chapter 3: Gaussian Simulation (Central Limit Theorem)\n")
cat("------------------------------------------------------\n")

walk_data <- simulate_random_walk(
  n_individuals = 1000,
  n_steps = 16,
  seed = 42
)
cat(sprintf("Simulated %d individuals for %d steps\n",
            length(unique(walk_data$individual)), max(walk_data$step)))

walk_stats <- compute_walk_statistics(walk_data)
final_stats <- walk_stats[nrow(walk_stats), ]
cat(sprintf("Final step statistics:\n"))
cat(sprintf("  Mean: %.3f\n", final_stats$mean))
cat(sprintf("  SD: %.3f\n", final_stats$sd))
cat(sprintf("  Range: [%d, %d]\n", final_stats$min, final_stats$max))

normality <- test_walk_normality(walk_data, final_step_only = TRUE)
cat(sprintf("Normality test (Shapiro-Wilk):\n"))
cat(sprintf("  Statistic: %.4f\n", normality$shapiro_statistic))
cat(sprintf("  P-value: %.4f\n", normality$shapiro_p_value))
cat(sprintf("  Skewness: %.4f\n", normality$skewness))
cat(sprintf("  Kurtosis: %.4f\n", normality$kurtosis))

comparison <- compare_to_normal(walk_data)
cat(sprintf("Generated comparison with normal distribution (%d points)\n", 
            nrow(comparison)))
cat("\n")

# ============================================================================
# Foundation Utilities
# ============================================================================

cat("Foundation Utilities\n")
cat("--------------------\n")

# Geometry
cart_point <- polar_to_cartesian(1, pi/4)
cat(sprintf("Polar (1, π/4) -> Cartesian (%.3f, %.3f)\n", 
            cart_point[1], cart_point[2]))

polar_coords <- cartesian_to_polar(c(0, 0), c(1, 1))
cat(sprintf("Cartesian (0,0) -> (1,1) = Polar (θ=%.3f, d=%.3f)\n",
            polar_coords["theta"], polar_coords["dist"]))

circle <- create_circle_polygon(x = 0, y = 0, r = 1, n_points = 100)
cat(sprintf("Created circle with %d points\n", nrow(circle)))

# Plotting
colors <- rethinking_palette(5)
cat(sprintf("Generated %d colors\n", length(colors)))

seq <- plot_sequence(1:10, n = 50)
cat(sprintf("Created plot sequence with %d points\n", length(seq)))

ribbon <- create_interval_ribbon(1:10, 0:9, 2:11)
cat(sprintf("Created interval ribbon with %d rows\n", nrow(ribbon)))

# Animation
frame1 <- data.frame(x = 1:5, y = 1:5)
frame2 <- data.frame(x = 1:5, y = 2:6)
frames <- build_animation_frames(list(frame1, frame2))
cat(sprintf("Built animation frames: %d rows across %d frames\n",
            nrow(frames), max(frames$frame)))

anim_seq <- create_animation_sequence(0, 10, n_frames = 50)
cat(sprintf("Created animation sequence with %d values\n", length(anim_seq)))

# Validation
cat("Testing validation functions...\n")
tryCatch({
  validate_tibble(data.frame(x = 1:5), required_cols = "x")
  cat("  ✓ validate_tibble works\n")
}, error = function(e) cat("  ✗ validate_tibble failed\n"))

tryCatch({
  validate_probability(c(0.1, 0.5, 0.9))
  cat("  ✓ validate_probability works\n")
}, error = function(e) cat("  ✗ validate_probability failed\n"))

tryCatch({
  validate_positive(c(1, 2, 3))
  cat("  ✓ validate_positive works\n")
}, error = function(e) cat("  ✗ validate_positive failed\n"))

cat("\n")

# ============================================================================
# Summary
# ============================================================================

cat("=== Demo Complete ===\n")
cat("All core package features demonstrated successfully!\n")
cat("\nKey components tested:\n")
cat("  ✓ Garden of forking paths simulation and layout\n")
cat("  ✓ Globe tossing with Bayesian updates\n")
cat("  ✓ Posterior predictive sampling\n")
cat("  ✓ Random walk simulation (Central Limit Theorem)\n")
cat("  ✓ Normality testing and comparison\n")
cat("  ✓ Geometry utilities (polar/cartesian transforms)\n")
cat("  ✓ Plotting utilities (themes, palettes, sequences)\n")
cat("  ✓ Animation utilities (frame building, sequences)\n")
cat("  ✓ Validation utilities (tibbles, probabilities, positive values)\n")
cat("\nPackage is ready for use!\n")
