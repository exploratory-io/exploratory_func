context("One-Sample Proportion Test")

test_that("exact path matches binom.test", {
  df <- data.frame(outcome = c(rep(TRUE, 12), rep(FALSE, 88)))
  result <- exp_prop_test(df, outcome, p = 0.1, method = "exact")
  model <- result$model[[1]]
  expected <- binom.test(12, 100, p = 0.1)
  expect_equal(model$p_value %||% model$htest$p.value, expected$p.value)
  expect_equal(model$method_used, "Exact Binomial Test")
})

test_that("approximate path matches prop.test", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_prop_test(df, outcome, p = 0.4, method = "approximate")
  model <- result$model[[1]]
  expected <- prop.test(50, 100, p = 0.4, correct = FALSE)
  expect_equal(model$htest$p.value, expected$p.value)
  expect_equal(model$method_used, "Approximate Test (Normal)")
})

test_that("auto picks exact when n*p < 5", {
  # n=20, p=0.1 -> n*p = 2 < 5 -> exact
  df <- data.frame(outcome = c(rep(TRUE, 2), rep(FALSE, 18)))
  result <- exp_prop_test(df, outcome, p = 0.1, method = "auto")
  model <- result$model[[1]]
  expect_equal(model$method_used, "Exact Binomial Test")
})

test_that("auto picks approximate when n*p >= 5 and n*(1-p) >= 5", {
  # n=100, p=0.4 -> n*p = 40, n*(1-p) = 60, both >= 5 -> approximate
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_prop_test(df, outcome, p = 0.4, method = "auto")
  model <- result$model[[1]]
  expect_equal(model$method_used, "Approximate Test (Normal)")
})

test_that("alternative = greater", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_prop_test(df, outcome, p = 0.4, alternative = "greater", method = "approximate")
  model <- result$model[[1]]
  expected <- prop.test(50, 100, p = 0.4, alternative = "greater", correct = FALSE)
  expect_equal(model$htest$p.value, expected$p.value)
})

test_that("alternative = less", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_prop_test(df, outcome, p = 0.4, alternative = "less", method = "approximate")
  model <- result$model[[1]]
  expected <- prop.test(50, 100, p = 0.4, alternative = "less", correct = FALSE)
  expect_equal(model$htest$p.value, expected$p.value)
})

test_that("repeat-by: one model per group", {
  df <- data.frame(
    outcome = c(rep(TRUE, 10), rep(FALSE, 10), rep(TRUE, 8), rep(FALSE, 12)),
    g = c(rep("X", 20), rep("Y", 20))
  ) %>% dplyr::group_by(g)
  result <- exp_prop_test(df, outcome, p = 0.5, method = "approximate")
  expect_equal(nrow(result), 2)
})

test_that("tidy model type returns correct columns", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_prop_test(df, outcome, p = 0.4, method = "approximate")
  model <- result$model[[1]]
  tidied <- tidy(model, type = "model")
  expect_true("P Value" %in% names(tidied))
  expect_true("Observed Proportion" %in% names(tidied))
  expect_true("Benchmark Proportion" %in% names(tidied))
  expect_true("Result" %in% names(tidied))
})

test_that("tidy data type returns single row with percentages", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_prop_test(df, outcome, p = 0.4, method = "approximate")
  model <- result$model[[1]]
  ci_data <- tidy(model, type = "data")
  expect_equal(nrow(ci_data), 1)
  expect_true("Observed Proportion (%)" %in% names(ci_data))
  expect_true("Conf Low (%)" %in% names(ci_data))
  expect_true("Conf High (%)" %in% names(ci_data))
})

test_that("stress test with complex column name", {
  complex_name <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  df <- data.frame(x = c(TRUE, FALSE, TRUE, TRUE, FALSE))
  names(df) <- complex_name
  result <- exp_prop_test(df, !!rlang::sym(complex_name), p = 0.5, method = "approximate")
  expect_true(!is.null(result))
})
