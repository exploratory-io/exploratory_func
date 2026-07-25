# CHAID performance guardrails. Always run in the normal testthat suite.
# Budgets are ~2x measured Daily-machine times so parallel/loaded runs still
# pass: Mac M5 ~0.6/4.4/1.1s, Win Azure 4-core ~2.5/17/6.4s for the three cases.

make_chaid_data <- function(n, p, seed = 1) {
  set.seed(seed)
  df <- data.frame(row.names = seq_len(n))
  for (i in seq_len(p)) {
    if (i %% 2 == 0) {
      df[[paste0('v', i)]] <- sample(letters[1:10], n, replace = TRUE)
    } else {
      df[[paste0('v', i)]] <- rnorm(n)
    }
  }
  df$target <- ifelse(df$v2 %in% c('a', 'b', 'c') & df$v4 %in% c('a', 'b'),
                      'yes', sample(c('yes', 'no'), n, replace = TRUE))
  df
}

test_that('CHAID fits 10k x 20 within budget', {
  df <- make_chaid_data(10000, 20)
  elapsed <- system.time(suppressWarnings(
    chaid_fit(df, target = 'target', predictors = paste0('v', 1:20),
              max_depth = 4, min_split = 100, min_bucket = 30)
  ))[['elapsed']]
  expect_lt(elapsed, 20)
})

test_that('CHAID fits 100k x 50 within budget', {
  df <- make_chaid_data(100000, 50)
  elapsed <- system.time(suppressWarnings(
    chaid_fit(df, target = 'target', predictors = paste0('v', 1:50),
              max_depth = 4, min_split = 500, min_bucket = 100)
  ))[['elapsed']]
  expect_lt(elapsed, 180)
})

test_that('permutation importance on 20k x 20 stays fast', {
  # Before vectorizing prediction this took ~10 minutes (201 predictions, each a
  # per-row tree walk). Budget is 2x measured Win Daily (~6.4s) for parallel load.
  df <- make_chaid_data(20000, 20)
  predictors <- paste0('v', 1:20)
  model <- suppressWarnings(
    chaid_fit(df, target = 'target', predictors = predictors,
              max_depth = 4, min_split = 200, min_bucket = 50)
  )
  elapsed <- system.time(
    chaid_permutation_importance(model, df, 'target', predictors)
  )[['elapsed']]
  expect_lt(elapsed, 60)
})
