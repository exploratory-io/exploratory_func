context("test exp_bayes_ab_aggregated Conversion Rate validation (tam#39621)")

test_that("exp_bayes_ab_aggregated errors with a clear message when Conversion Rate is > 1 (e.g. an average, not a proportion)", {
  # Mirrors the issue's repro: a column like "average salary" used as
  # Conversion Rate is far above 1, which used to make the derived FALSE
  # count negative and fail deep inside exp_bayes_ab() with the cryptic
  # "invalid 'times' argument" instead of a message the user can act on.
  df <- tibble::tibble(cat = c('A', 'B'), n = c(100, 200), cr = c(85000, 92000))
  expect_error(
    df %>% exp_bayes_ab_aggregated(cat, cr, n, prior_mean = 0.1, prior_sd = 0.01, seed = 0),
    "Conversion Rate must be a proportion between 0 and 1",
    fixed = TRUE
  )
})

test_that("exp_bayes_ab_aggregated errors with a clear message when Conversion Rate is negative", {
  df <- tibble::tibble(cat = c('A', 'B'), n = c(100, 200), cr = c(-0.1, 0.2))
  expect_error(
    df %>% exp_bayes_ab_aggregated(cat, cr, n, prior_mean = 0.1, prior_sd = 0.01, seed = 0),
    "Conversion Rate must be a proportion between 0 and 1",
    fixed = TRUE
  )
})

test_that("exp_bayes_ab_aggregated still works for a valid 0-1 Conversion Rate (no regression)", {
  df <- tibble::tibble(cat = c('A', 'B'), n = c(100, 200), cr = c(0.22, 0.2))
  model_df <- df %>% exp_bayes_ab_aggregated(cat, cr, n, prior_mean = 0.1, prior_sd = 0.01, seed = 0)
  expect_true(is.list(model_df$model))
})

test_that("exp_bayes_ab_aggregated accepts boundary values 0 and 1 for Conversion Rate", {
  df <- tibble::tibble(cat = c('A', 'B'), n = c(100, 200), cr = c(0, 1))
  model_df <- df %>% exp_bayes_ab_aggregated(cat, cr, n, prior_mean = 0.1, prior_sd = 0.01, seed = 0)
  expect_true(is.list(model_df$model))
})

test_that("exp_bayes_ab_aggregated ignores NA Conversion Rate rows rather than erroring on them", {
  # Matches the existing convention in do_bayes_ab(), which filters NA rows
  # out before validating range, so a NA value is dropped, not flagged as
  # out-of-range.
  df <- tibble::tibble(cat = c('A', 'A', 'B'), n = c(100, 50, 200), cr = c(0.22, NA, 0.2))
  model_df <- df %>% exp_bayes_ab_aggregated(cat, cr, n, prior_mean = 0.1, prior_sd = 0.01, seed = 0)
  expect_true(is.list(model_df$model))
})
