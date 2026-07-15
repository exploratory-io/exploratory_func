# CHAID performance guardrails. Skipped by default; run with
#   EXPLORATORY_RUN_PERF=1 Rscript -e '...test_file("tests/testthat/test_chaid_perf.R")'
# Budgets are generous relative to observed times (10k x 20 ~0.4s,
# 100k x 50 ~3s on an M1 Mac) so they catch regressions, not noise.

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
  skip_if(Sys.getenv('EXPLORATORY_RUN_PERF') == '', 'perf tests are opt-in')
  df <- make_chaid_data(10000, 20)
  elapsed <- system.time(suppressWarnings(
    chaid_fit(df, target = 'target', predictors = paste0('v', 1:20),
              max_depth = 4, min_split = 100, min_bucket = 30)
  ))[['elapsed']]
  expect_lt(elapsed, 10)
})

test_that('CHAID fits 100k x 50 within budget', {
  skip_if(Sys.getenv('EXPLORATORY_RUN_PERF') == '', 'perf tests are opt-in')
  df <- make_chaid_data(100000, 50)
  elapsed <- system.time(suppressWarnings(
    chaid_fit(df, target = 'target', predictors = paste0('v', 1:50),
              max_depth = 4, min_split = 500, min_bucket = 100)
  ))[['elapsed']]
  expect_lt(elapsed, 90)
})
