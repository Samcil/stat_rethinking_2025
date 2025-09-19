test_that("stan_model_path finds models and errors for missing", {
  p <- stan_model_path("08_mHMC")
  expect_true(file.exists(p))
  expect_error(stan_model_path("definitely_not_a_model"))
})

cmdstan_available <- function() {
  if (!rlang::is_installed("cmdstanr")) return(FALSE)
  out <- try(cmdstanr::cmdstan_available(error_on_NA = FALSE), silent = TRUE)
  isTRUE(out)
}

test_that("compile_stan_model compiles via cmdstanr when available", {
  skip_if_not(cmdstan_available(), "cmdstanr or CmdStan not available")
  cm <- compile_stan_model("08_mHMC", copy_to_temp = TRUE, quiet = TRUE)
  expect_type(cm, "list")
  expect_equal(cm$backend, "cmdstanr")
  expect_true(inherits(cm$model, "CmdStanModel"))
  expect_true(file.exists(cm$stan_file))
})

test_that("compile_stan_model validates backend option", {
  expect_error(compile_stan_model("08_mHMC", backend = "rstan"))
})

test_that("sample_stan_model validates inputs", {
  expect_error(sample_stan_model(list(bad = TRUE), data = list()), "model name")
  expect_error(sample_stan_model("definitely_not_a_model", data = list()))
})

