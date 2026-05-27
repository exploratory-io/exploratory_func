context("Two-Sample Proportion Test")

test_that("approximate path matches prop.test", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  result <- exp_prop_test_2groups(df, outcome, group, method = "approximate")
  model <- result$model[[1]]

  expected <- prop.test(x = c(23, 12), n = c(50, 40), correct = FALSE)
  expect_equal(model$p_value, expected$p.value)
  expect_equal(model$method_used, "Approximate Test (Normal)")
})

test_that("exact path matches fisher.test", {
  df <- data.frame(
    outcome = c(rep(TRUE, 3), rep(FALSE, 7), rep(TRUE, 1), rep(FALSE, 4)),
    group = c(rep("A", 10), rep("B", 5))
  )
  result <- exp_prop_test_2groups(df, outcome, group, method = "exact")
  model <- result$model[[1]]

  expected <- fisher.test(matrix(c(3, 7, 1, 4), nrow = 2, byrow = TRUE))
  expect_equal(model$p_value, expected$p.value)
  expect_equal(model$method_used, "Fisher's Exact Test")
})

test_that("auto method selects fisher when expected cell < 5", {
  # Small counts -> some expected cells will be < 5
  df <- data.frame(
    outcome = c(rep(TRUE, 2), rep(FALSE, 3), rep(TRUE, 1), rep(FALSE, 4)),
    group = c(rep("A", 5), rep("B", 5))
  )
  result <- exp_prop_test_2groups(df, outcome, group, method = "auto")
  model <- result$model[[1]]
  expect_equal(model$method_used, "Fisher's Exact Test")
})

test_that("auto method selects approximate when expected cells all >= 5", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  result <- exp_prop_test_2groups(df, outcome, group, method = "auto")
  model <- result$model[[1]]
  expect_equal(model$method_used, "Approximate Test (Normal)")
})

test_that("alternative = greater", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  result <- exp_prop_test_2groups(df, outcome, group, alternative = "greater", method = "approximate")
  model <- result$model[[1]]

  expected <- prop.test(x = c(23, 12), n = c(50, 40), alternative = "greater", correct = FALSE)
  expect_equal(model$p_value, expected$p.value)
})

test_that("alternative = less", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  result <- exp_prop_test_2groups(df, outcome, group, alternative = "less", method = "approximate")
  model <- result$model[[1]]

  expected <- prop.test(x = c(23, 12), n = c(50, 40), alternative = "less", correct = FALSE)
  expect_equal(model$p_value, expected$p.value)
})

test_that("error when explanatory has 3+ unique values (no NA)", {
  df <- data.frame(
    outcome = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    group = c("A", "B", "C", "A", "B")
  )
  expect_error(exp_prop_test_2groups(df, outcome, group),
               "The explanatory variable needs to have 2 unique values.")
})

test_that("auto-filter NA as 3rd category when explanatory has 3 unique values including NA", {
  df <- data.frame(
    outcome = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 3), rep(FALSE, 7), TRUE, FALSE),
    group = c(rep("A", 10), rep("B", 10), NA, NA)
  )
  result <- exp_prop_test_2groups(df, outcome, group)
  expect_true(!is.null(result$model[[1]]))
  expect_equal(result$model[[1]]$nA + result$model[[1]]$nB, 20)
})

test_that("repeat-by: one model per group", {
  df <- data.frame(
    outcome = c(rep(TRUE, 10), rep(FALSE, 10), rep(TRUE, 8), rep(FALSE, 12)),
    group = c(rep("A", 20), rep("B", 20)),
    repeat_col = c(rep("X", 20), rep("Y", 20))
  ) %>% dplyr::group_by(repeat_col)
  result <- exp_prop_test_2groups(df, outcome, group, method = "approximate")
  expect_equal(nrow(result), 2)
})

test_that("tidy model type returns summary data frame", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  result <- exp_prop_test_2groups(df, outcome, group, method = "approximate")
  model <- result$model[[1]]
  tidied <- tidy(model, type = "model")
  expect_true("P Value" %in% names(tidied))
  expect_true("Group A" %in% names(tidied))
  expect_true("Group B" %in% names(tidied))
  expect_true("Result" %in% names(tidied))
})

test_that("tidy data type returns two-row CI data", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  result <- exp_prop_test_2groups(df, outcome, group, method = "approximate")
  model <- result$model[[1]]
  ci_data <- tidy(model, type = "data")
  expect_equal(nrow(ci_data), 2)
  expect_true("Proportion (%)" %in% names(ci_data))
  expect_true("Conf Low (%)" %in% names(ci_data))
  expect_true("Conf High (%)" %in% names(ci_data))
})

test_that("stress test with complex column names", {
  complex_name <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  df <- data.frame(
    TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,
    c("A", "A", "A", "B", "B", "B")
  )
  names(df) <- c(complex_name, "group")
  df[[complex_name]] <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  result <- exp_prop_test_2groups(df, !!rlang::sym(complex_name), group, method = "approximate")
  expect_true(!is.null(result))
})
