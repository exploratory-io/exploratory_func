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
  result <- exp_prop_test(df, `航空 会社 !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表`, p = 0.5, method = "approximate")
  expect_true(!is.null(result))
  expect_equal(result$model[[1]]$var_col, complex_name)
})

# --- numeric correctness of derived/exposed outputs ---

test_that("approximate confidence interval matches prop.test", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  expected <- prop.test(50, 100, p = 0.4, correct = FALSE)
  expect_equal(model$htest$conf.int[1], expected$conf.int[1])
  expect_equal(model$htest$conf.int[2], expected$conf.int[2])
})

test_that("exact confidence interval matches binom.test", {
  df <- data.frame(outcome = c(rep(TRUE, 12), rep(FALSE, 88)))
  model <- exp_prop_test(df, outcome, p = 0.1, method = "exact")$model[[1]]
  expected <- binom.test(12, 100, p = 0.1)
  expect_equal(model$htest$conf.int[1], expected$conf.int[1])
  expect_equal(model$htest$conf.int[2], expected$conf.int[2])
})

test_that("Cohen's h matches pwr::ES.h", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  expect_equal(model$cohens_h, pwr::ES.h(0.5, 0.4))
})

test_that("power matches pwr::pwr.p.test", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  expected <- pwr::pwr.p.test(h = pwr::ES.h(0.5, 0.4), n = 100,
                              sig.level = 0.05, alternative = "two.sided")$power
  expect_equal(model$power, expected)
})

test_that("tidy model exposes correct observed proportion, difference and CI values", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  tidied <- tidy(model, type = "model")
  expected <- prop.test(50, 100, p = 0.4, correct = FALSE)
  expect_equal(tidied$`Observed Proportion`, 0.5)
  expect_equal(tidied$`Benchmark Proportion`, 0.4)
  expect_equal(tidied$Difference, 0.5 - 0.4)
  expect_equal(tidied$`P Value`, expected$p.value)
  expect_equal(tidied$`Conf Low`, expected$conf.int[1])
  expect_equal(tidied$`Conf High`, expected$conf.int[2])
})

test_that("tidy model maps Test Direction from alternative", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  two   <- tidy(exp_prop_test(df, outcome, p = 0.4, alternative = "two.sided", method = "approximate")$model[[1]], type = "model")
  gt    <- tidy(exp_prop_test(df, outcome, p = 0.4, alternative = "greater",  method = "approximate")$model[[1]], type = "model")
  lt    <- tidy(exp_prop_test(df, outcome, p = 0.4, alternative = "less",     method = "approximate")$model[[1]], type = "model")
  expect_equal(two$`Test Direction`, "Different from benchmark")
  expect_equal(gt$`Test Direction`, "Greater than benchmark")
  expect_equal(lt$`Test Direction`, "Less than benchmark")
})

test_that("tidy model Result reflects significance against sig.level", {
  # obs 0.5 vs p 0.4, n=100 -> p approx 0.041 < 0.05 -> significant
  sig_model <- exp_prop_test(data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50))),
                             outcome, p = 0.4, method = "approximate")$model[[1]]
  # obs 0.5 vs p 0.45, n=100 -> p approx 0.315 -> not significant
  ns_model  <- exp_prop_test(data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50))),
                             outcome, p = 0.45, method = "approximate")$model[[1]]
  expect_lt(sig_model$htest$p.value, 0.05)
  expect_gt(ns_model$htest$p.value, 0.05)
  expect_equal(tidy(sig_model, type = "model")$Result, "Statistically significant.")
  expect_equal(tidy(ns_model, type = "model")$Result, "Not statistically significant.")
})

test_that("tidy data type percentages match the model CI", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  ci_data <- tidy(model, type = "data")
  expect_equal(ci_data$`Observed Proportion (%)`, model$observed_prop * 100)
  expect_equal(ci_data$`Conf Low (%)`, model$htest$conf.int[1] * 100)
  expect_equal(ci_data$`Conf High (%)`, model$htest$conf.int[2] * 100)
})

test_that("glance returns htest statistics", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  glanced <- glance(model)
  expect_true("p.value" %in% names(glanced))
  expect_equal(glanced$p.value, model$htest$p.value)
})

test_that("repeat-by: a failing group is captured and tidied as a Note", {
  df <- data.frame(
    outcome = c(rep(TRUE, 10), rep(FALSE, 10), rep(NA, 20)),
    g = c(rep("X", 20), rep("Y", 20))
  ) %>% dplyr::group_by(g)
  result <- exp_prop_test(df, outcome, p = 0.5, method = "approximate")
  expect_equal(nrow(result), 2)
  # group Y is all-NA -> error object captured per group, tidied to a Note
  note_tidied <- tidy(result$model[[2]], type = "model")
  expect_true("Note" %in% names(note_tidied))
})
