# tidyrethinking

<!-- badges: start -->
<!-- badges: end -->

**tidyrethinking** is a modern R package that provides tidyverse-aligned, modular functions for reproducing Statistical Rethinking course materials. It replaces ad-hoc scripts with pipe-friendly, well-documented workflows for Bayesian data analysis, causal inference, and statistical modeling.

## Overview

This package modernizes the visualization and analysis scripts from Richard McElreath's Statistical Rethinking course by:

- **Tidyverse design**: All functions follow tidyverse principles with pipe-friendly interfaces
- **Modular structure**: Reusable components instead of monolithic scripts
- **Comprehensive documentation**: Full roxygen2 documentation with examples
- **Extensive testing**: Unit tests for all major functionality
- **Modern visualization**: ggplot2 and gganimate instead of base graphics

## Installation

You can install the development version of tidyrethinking from GitHub:

```r
# install.packages("pak")
pak::pak("Samcil/stat_rethinking_2025")
```

## Key Features

### Chapter 2: Garden of Forking Data & Globe Tossing

Simulate and visualize Bayesian updating with the garden of forking paths metaphor:

```r
library(tidyrethinking)
library(ggplot2)
library(dplyr)

# Simulate garden of forking paths
observations <- c(1, 0, 1)  # water, land, water
possibilities <- c(0, 1, 1, 1)  # 1 land, 3 water marbles

paths <- simulate_garden_paths(observations, possibilities)
paths_layout <- create_garden_layout(paths)

# Visualize
ggplot(paths_layout, aes(x = x, y = y, group = path_id)) +
  geom_path(aes(alpha = viable)) +
  geom_point(aes(color = factor(value), size = viable)) +
  coord_fixed() +
  theme_rethinking()

# Globe tossing with Bayesian updates
tosses <- simulate_globe_tosses(10, prob_water = 0.7, seed = 42)
posteriors <- compute_beta_updates(tosses$result)
density_data <- generate_posterior_density(posteriors)

ggplot(density_data, aes(x = p, y = density, group = toss)) +
  geom_line(aes(color = toss)) +
  labs(x = "Proportion water", y = "Posterior density") +
  theme_rethinking()
```

### Chapter 3: Gaussian Distributions via Central Limit Theorem

Demonstrate how sums of random variables converge to normal distributions:

```r
# Simulate random walk
walk_data <- simulate_random_walk(
  n_individuals = 1000, 
  n_steps = 16, 
  seed = 42
)

# Plot final distribution
final_positions <- walk_data %>%
  filter(step == max(step))

ggplot(final_positions, aes(x = position)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  theme_rethinking()

# Compare to theoretical normal
comparison <- compare_to_normal(walk_data)

ggplot(comparison, aes(x = x)) +
  geom_line(aes(y = empirical_density, color = "Observed")) +
  geom_line(aes(y = theoretical_density, color = "Normal"), linetype = "dashed") +
  labs(y = "Density", color = "Distribution")
```

### Shared Utilities

The package includes foundational utilities used across chapters:

```r
# Geometry transformations
point <- polar_to_cartesian(dist = 1, theta = pi/4)
polar <- cartesian_to_polar(c(0, 0), c(1, 1))

# Plotting helpers
circle_data <- create_circle_polygon(x = 0, y = 0, r = 1)

# Color palettes
colors <- rethinking_palette(5, alpha = 0.7)

# Animation support
frames <- build_animation_frames(list(df1, df2, df3))
```

## Package Structure

The package is organized by chapter and theme:

- **Foundation utilities** (`geometry_utils.R`, `plot_utils.R`, `animation_utils.R`, `validation_utils.R`)
- **Chapter 2**: Garden of forking paths and globe tossing (`ch02_*.R`)
- **Chapter 3**: Gaussian simulations and linear models (`ch03_*.R`)
- Additional chapters following similar structure

## Design Principles

This package follows the [tidyverse style guide](https://style.tidyverse.org/) and embeds key principles:

1. **Snake_case naming** throughout
2. **Tibbles as primary data structure** for tidy data representation
3. **Explicit error messages** using `rlang::abort()` and `cli` for user feedback
4. **Comprehensive validation** using `checkmate` for all inputs
5. **Reproducibility** via seed parameters in stochastic functions

## Relationship to Original Scripts

The original scripts in the `scripts/` directory demonstrate course concepts but are:
- Monolithic (mixing simulation, modeling, plotting)
- Dependent on base graphics and `animation` package
- Less reusable across different contexts

This package extracts core functionality into composable, tested, documented functions while preserving the pedagogical value.

## Contributing

This is an educational package developed as part of Statistical Rethinking 2025. Contributions, issues, and suggestions are welcome!

## Acknowledgments

This package is based on materials from:

- **Richard McElreath's Statistical Rethinking** course and textbook
- The original `rethinking` R package
- Statistical Rethinking 2023 lecture materials

## License

MIT License - see LICENSE file for details.

## Related Resources

- [Statistical Rethinking textbook](https://xcelab.net/rm/statistical-rethinking/)
- [Lecture videos](https://www.youtube.com/playlist?list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus)
- [Original rethinking package](https://github.com/rmcelreath/rethinking/)
- [Course repository](https://github.com/Samcil/stat_rethinking_2025)
