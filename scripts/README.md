## Scripts Directory

This folder contains the original scripts used to create visualizations and examples in the Statistical Rethinking lectures.

### ⚠️ Migration to tidyrethinking Package

These scripts are being modernized into the `tidyrethinking` R package with:
- Tidyverse-aligned, pipe-friendly functions
- Comprehensive documentation and examples
- Extensive unit tests
- Modern ggplot2/gganimate visualizations

### Migration Status

#### ✅ Completed
- **Chapter 2**: Garden of forking paths and globe tossing
  - `02_garden_animation.r` → `simulate_garden_paths()`, `create_garden_layout()`
  - `02_globe_tossing_updating.r` → `simulate_globe_tosses()`, `compute_beta_updates()`
  - `02_predictive_simulation.r` → `sample_posterior_predictive()`

- **Chapter 3**: Gaussian distributions
  - `03_gaussian_generative_sim.r` → `simulate_random_walk()`, `compare_to_normal()`

- **Foundation utilities**:
  - Geometry functions: `polar_to_cartesian()`, `cartesian_to_polar()`, `create_circle_polygon()`
  - Plotting themes: `theme_rethinking()`, `rethinking_palette()`
  - Animation helpers: `build_animation_frames()`, `prepare_path_animation()`

#### 🚧 In Progress
- Chapters 4-7: Linear models, DAGs, confounds
- Chapters 8-11: MCMC, GLMs
- Chapters 12-19: Multilevel models, GPs, networks

### Using the Package

Instead of sourcing these scripts, install and use the package:

```r
# Install
pak::pak("Samcil/stat_rethinking_2025")

# Use functions
library(tidyrethinking)

# Example: Globe tossing
tosses <- simulate_globe_tosses(10, prob_water = 0.7, seed = 42)
posteriors <- compute_beta_updates(tosses$result)
```

### Original Scripts

These scripts remain available for reference and comparison but are no longer the recommended approach for reproducing course materials.
