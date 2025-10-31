post_title: "TidyRethinking Modernization PRD"
author1: "Project Team"
post_slug: "tidyrethinking-modernization-prd"
microsoft_alias: "samoh"
featured_image: "https://example.com/featured-image.png"
categories:
	- software
tags:
	- tidyverse
	- r
ai_note: "Generated with assistance from GitHub Copilot."
summary: "Product requirements for migrating stat_rethinking_2025 scripts into tidyverse-aligned, reusable tooling."
post_date: 2025-10-31
---

## 1. Product Vision
- Deliver a cohesive `tidyrethinking` experience that replaces ad-hoc scripts with pipe-friendly, documented functions.
- Preserve pedagogical storytelling while improving reproducibility, test coverage, and package ergonomics.
- Embed the tidyverse style guide (snake_case naming, explicit pronouns, spaces around operators, and curated pipes) across all exported APIs.

## 2. Success Criteria
- Chapter coverage: each script family has an equivalent exported workflow with roxygen documentation and runnable examples.
- Package quality: `devtools::check()` passes without warnings, vignettes render, and tests cover primary code paths.
- User experience: analysts can reproduce textbook figures by calling high-level helpers without manual Stan or animation loops.
- Adoption: `scripts/README.md` points to new APIs and legacy scripts are marked read-only once replacements are in place.

## 3. Non-Goals
- Rewriting textbook narrative content or homework answers.
- Publishing CRAN-ready release in this iteration (focus on internal package quality first).
- Changing statistical models beyond what is needed for modularization and validation.

## 4. Current State Summary
- Large monolithic scripts mix simulation, modeling, plotting, animations, and helper definitions.
- Duplicate geometry, animation, and plotting utilities appear across multiple chapters.
- Stan programs live inline within scripts, making reuse and testing difficult.
- Animations rely on base graphics (`ani.record`, manual loops) with heavy global state and side effects.
- Validation and documentation patterns vary; tidyverse style conventions are inconsistently applied.

## 5. Target Architecture
- Shared foundations inside `R/` provide geometry, animation, plotting, and Stan orchestration helpers.
- Chapter-focused modules expose parameterized workflows returning tibbles and ggplot objects.
- Stan files reside in `inst/stan` with cmdstanr wrappers and consistent data preprocessing.
- Tests in `tests/testthat/` validate simulations, model outputs, and visualization scaffolding (via snapshot tests where needed).
- Vignettes mirror book structure (causal demos, MCMC, GP, etc.) using Quarto for narrative workflows.

## 6. Feature Breakdown
### 6.1 Shared Foundations (Phase 1)
- Geometry utilities: consolidate polar/cartesian transforms, ellipse builders, and DAG layout helpers using snake_case naming.
- Animation utilities: `build_animation_frames()`, `render_animation()` bridge ggplot and gganimate while replacing inline `ani.record` usage.
- Plotting themes: surface consistent theme helpers and color palettes to reduce duplication.
- Dependency hygiene: update DESCRIPTION/NAMESPACE for animation, tidygraph, and cli usage.

### 6.2 Early Probability & Regression (Ch. 2–4)
- Garden of forking paths: `simulate_garden_paths()`, `animate_garden_paths()` return tidy frame sets.
- Globe tossing and predictive simulations: modular samplers with validation and plotting wrappers.
- Howell growth models: sampling, fitting, and animation split into discrete functions with shared ellipse helpers.
- Prior predictive linear and spline demos: parameterized generators with tidy outputs and gganimate visualizations.

### 6.3 Causal Diagrams & Confounds (Ch. 5–7)
- DAG presets and animators driven by configuration data, replacing `if (FALSE)` script blocks.
- Collider and bad control simulations returning tidy draws and summary metrics.
- Copernican and overfitting demos share geometry utilities and standardized animation interfaces.

### 6.4 MCMC & GLMs (Ch. 8–11)
- Random walk, HMC path, and posterior animation helpers with reusable plotting scaffolds.
- Binomial, Poisson, and ordered category workflows producing tidy draws and diagnostic plots.
- Consistent cmdstanr wrappers for all Stan models in these chapters.

### 6.5 Multilevel, GP, Networks, Measurement (Ch. 12–19)
- Multilevel models (cafe, Mundlak, Bangladesh) exposed via functions toggling centered versus non-centered parameterizations.
- Social network demos using tidygraph for generative simulations and analysis.
- Gaussian process utilities covering kernel visualization, spatial Kline, and phylogenetic models.
- Measurement error, missing data, and generalized linear "madness" workflows as modular simulation and modeling suites.

## 7. Implementation Plan
1. Phase 1 foundations remove duplicate helpers, establish testing scaffolds, and define tidyverse-aligned documentation templates.
2. Iterate through chapter groupings (Phases 2–6) delivering feature slices with accompanying tests and vignette updates.
3. After each phase run `styler::style_pkg()`, unit tests, and selective vignette renders to maintain quality.
4. Maintain a migration checklist in `scripts/README.md` identifying which scripts have modern replacements.
5. Freeze legacy scripts (read-only notice) once parity is achieved for each chapter block.

## 8. Testing & Validation Strategy
- Use `checkmate` or bespoke validation functions to enforce tidy inputs and explicit errors via `rlang::abort()`.
- Rely on `testthat` for deterministic computations; apply snapshot tests for plots and animations using vdiffr or png comparisons.
- For stochastic simulations, fix seeds via `withr::with_seed()` and validate summary statistics instead of raw draws.
- Ensure cmdstanr wrappers include `check_hmc_diagnostics()` hooks in examples and tests.
- Track coverage improvements inside `DOCUMENTATION_TESTING_LOG.md` after each phase.

## 9. Documentation & Developer Experience
- Roxygen2 docs with `@family` tags aligning by chapter or theme; auto-generate via `devtools::document()`.
- Quarto vignettes aligned to book sections: update existing `.qmd` files or add new ones (for example, `causal-demos.qmd`).
- Provide usage examples showcasing tidy inputs, tidy outputs, and references to Stan file locations.
- Update repository README and `scripts/README.md` with modernization status, onboarding steps, and deprecation notices.

## 10. Dependencies & Tooling
- Continue to rely on tidyverse, cmdstanr, and rethinking where necessary, but wrap rethinking calls for clarity.
- Introduce tidygraph or ggraph for network visualizations, gganimate for animations, and cli for messaging.
- Ensure cmdstan toolchain prerequisites appear in README and vignette prerequisites sections.

## 11. Risks & Mitigations
- Time constraints across 19 chapters: prioritize shared utilities and high-impact pedagogical workflows first.
- Animation parity: validate gganimate output against legacy visuals and retain fallback static plots if animation is not critical.
- Stan refactoring regressions: write integration tests comparing key posterior summaries before and after refactor.
- Package bloat from heavy dependencies: audit after each phase and remove unused packages; favor optional suggests where appropriate.

## 12. Milestones & Tracking
- M1 (Week 1): Foundations complete, tests scaffolded, PRD approved.
- M2 (Week 3): Chapters 2–7 APIs shipped with docs and tests, legacy scripts annotated.
- M3 (Week 6): Chapters 8–11 refactored; GP and network measurement plan validated.
- M4 (Week 9): Remaining chapters modernized; vignettes rendered; coverage targets documented.
- M5 (Week 10): Final QA via `devtools::check()`, documentation refresh, handover notes.

## 13. Open Questions
- Confirm appetite for deprecating inline Stan in favor of cmdstanr-only workflows within pedagogy.
- Determine whether to bundle sample datasets (for example, Bangladesh data) or reference external sources only.
- Assess need for interactive Shiny replacements for scripts that previously used `identify()` for user input.

## 14. Next Actions
- Review and sign off PRD.
- Kick off Phase 1 foundations with geometry and animation utilities.
- Schedule cadence for progress reviews (weekly stand-up and fortnightly demo of migrated chapter blocks).
