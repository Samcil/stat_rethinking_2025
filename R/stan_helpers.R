#' Stan model helpers (tidyverse-friendly)
#'
#' Utilities to locate, compile, and sample Stan models placed in
#' `inst/stan/`. These wrappers follow tidyverse design principles, use
#' `cli` for validation, and return tibbles when producing draws.
#'
#' These helpers prefer the cmdstanr backend. They do not start or install
#' CmdStan themselves. If cmdstanr is not available, compilation and
#' sampling helpers will informatively abort. Model path discovery always
#' works.
#'
#' @family stan_helpers
#' @keywords internal
NULL

#' Locate a Stan model file in the installed package
#'
#' @param name File name (with or without `.stan` extension), e.g. "08_mHMC" or
#'   "08_mHMC.stan".
#' @return A character scalar file path. Aborts if the file is not found.
#' @examples
#' # stan_model_path("08_mHMC")
#' @seealso [compile_stan_model()], [sample_stan_model()]
#' @family stan_helpers
#' @export
#' @importFrom cli cli_abort
stan_model_path <- function(name) {
  if (!is.character(name) || length(name) != 1L) cli::cli_abort("`name` must be a single string.")
  fname <- if (endsWith(name, ".stan")) name else paste0(name, ".stan")
  p <- system.file("stan", fname, package = "tidyrethinking")
  if (identical(p, "")) cli::cli_abort("Stan file `{fname}` not found under inst/stan in the installed package.")
  p
}

#' Compile a Stan model (cmdstanr)
#'
#' Compiles a Stan model by copying the .stan file into a temporary working
#' directory first (to avoid modifying files in the package directory), then
#' calling cmdstanr::cmdstan_model().
#'
#' @param name Model file name (with or without `.stan`) under inst/stan.
#' @param backend Only "cmdstanr" is currently supported.
#' @param copy_to_temp Logical; if TRUE (default), copy the .stan file to a
#'   temporary directory before compilation.
#' @param ... Additional arguments forwarded to cmdstanr::cmdstan_model().
#' @return A list with fields: backend = "cmdstanr", model (CmdStanModel),
#'   model_name, stan_file, work_dir.
#' @examples
#' if (FALSE) { # interactive() && rlang::is_installed("cmdstanr")
#'   m <- compile_stan_model("08_mHMC")
#' }
#' @seealso [stan_model_path()], [sample_stan_model()]
#' @family stan_helpers
#' @export
#' @importFrom cli cli_abort
compile_stan_model <- function(name, backend = c("cmdstanr"), copy_to_temp = TRUE, ...) {
  backend <- match.arg(backend)
  if (backend != "cmdstanr") cli::cli_abort("Only the 'cmdstanr' backend is supported at this time.")
  if (!rlang::is_installed("cmdstanr")) cli::cli_abort("The 'cmdstanr' package is not installed.")

  src <- stan_model_path(name)
  work_dir <- if (isTRUE(copy_to_temp)) tempfile(pattern = "stan_build_") else dirname(src)
  if (isTRUE(copy_to_temp)) {
    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
    dst <- file.path(work_dir, basename(src))
    file.copy(src, dst, overwrite = TRUE)
    stan_file <- dst
  } else {
    stan_file <- src
  }

  # try to compile using cmdstanr
  model <- try(cmdstanr::cmdstan_model(stan_file, ...), silent = TRUE)
  if (inherits(model, "try-error")) {
    cli::cli_abort("cmdstanr failed to compile model '{basename(stan_file)}'. Ensure CmdStan is installed and configured.")
  }
  list(
    backend = "cmdstanr",
    model = model,
    model_name = tools::file_path_sans_ext(basename(stan_file)),
    stan_file = stan_file,
    work_dir = work_dir
  )
}

#' Sample from a compiled Stan model (cmdstanr)
#'
#' If `model_or_name` is a string, it is resolved and compiled via
#' [compile_stan_model()] first. For cmdstanr, returns a tibble of draws.
#'
#' @param model_or_name A compiled object returned by [compile_stan_model()],
#'   or a string model name resolvable via [stan_model_path()].
#' @param data A named list of data for the Stan program.
#' @param chains Number of MCMC chains.
#' @param iter_warmup Iterations for warmup.
#' @param iter_sampling Iterations for sampling (post-warmup).
#' @param seed Optional integer seed.
#' @param variables Optional character vector of variable names to return.
#' @param ... Additional arguments forwarded to cmdstanr::model$sample().
#' @return A tibble of draws with chain and iteration columns (when available)
#'   and one column per requested parameter.
#' @examples
#' if (FALSE) { # interactive() && rlang::is_installed("cmdstanr")
#'   cm <- compile_stan_model("08_mHMC")
#'   draws <- sample_stan_model(cm, data = list(), chains = 2, iter_warmup = 100, iter_sampling = 100)
#' }
#' @seealso [stan_model_path()], [compile_stan_model()]
#' @family stan_helpers
#' @export
#' @importFrom tibble as_tibble
#' @importFrom cli cli_abort
sample_stan_model <- function(model_or_name,
                              data,
                              chains = 4,
                              iter_warmup = 1000,
                              iter_sampling = 1000,
                              seed = NULL,
                              variables = NULL,
                              ...) {
  if (is.character(model_or_name)) {
    compiled <- compile_stan_model(model_or_name)
  } else if (is.list(model_or_name) && identical(model_or_name$backend, "cmdstanr")) {
    compiled <- model_or_name
  } else {
    cli::cli_abort("`model_or_name` must be a model name (string) or a compiled object from compile_stan_model().")
  }
  if (!rlang::is_installed("cmdstanr")) cli::cli_abort("The 'cmdstanr' package is not installed.")

  fit <- compiled$model$sample(data = data, chains = chains,
                               iter_warmup = iter_warmup,
                               iter_sampling = iter_sampling,
                               seed = seed, ...)
  # Prefer a base data.frame format to avoid tight coupling with 'posterior'
  df <- try(fit$draws(variables = variables, format = "df"), silent = TRUE)
  if (inherits(df, "try-error")) {
    # Fallback to matrix then rebuild a tibble
    arr <- fit$draws(variables = variables, format = "draws_matrix")
    df <- as.data.frame(arr)
  }
  tibble::as_tibble(df)
}

