context("One-Sample t-Test")

# --- basic accuracy against base t.test ---

test_that("two.sided t-statistic and p-value match base t.test", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  result <- exp_one_sample_t_test(df, x, mu = 4.5)
  model <- result$model[[1]]
  expected <- t.test(vec, mu = 4.5, alternative = "two.sided")
  expect_equal(model$htest$statistic, expected$statistic)
  expect_equal(model$htest$p.value, expected$p.value)
})

test_that("alternative = greater matches base t.test", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  result <- exp_one_sample_t_test(df, x, mu = 4.5, alternative = "greater")
  model <- result$model[[1]]
  expected <- t.test(vec, mu = 4.5, alternative = "greater")
  expect_equal(model$htest$statistic, expected$statistic)
  expect_equal(model$htest$p.value, expected$p.value)
})

test_that("alternative = less matches base t.test", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  result <- exp_one_sample_t_test(df, x, mu = 5.5, alternative = "less")
  model <- result$model[[1]]
  expected <- t.test(vec, mu = 5.5, alternative = "less")
  expect_equal(model$htest$statistic, expected$statistic)
  expect_equal(model$htest$p.value, expected$p.value)
})

# --- Cohen's d and power ---

test_that("Cohen's d equals (mean - mu) / sd", {
  set.seed(7)
  vec <- rnorm(40, mean = 10, sd = 3)
  df <- data.frame(val = vec)
  model <- exp_one_sample_t_test(df, val, mu = 9)$model[[1]]
  expected_d <- (mean(vec) - 9) / sd(vec)
  expect_equal(model$cohens_d, expected_d)
})

test_that("power matches pwr::pwr.t.test one.sample", {
  set.seed(7)
  vec <- rnorm(40, mean = 10, sd = 3)
  df <- data.frame(val = vec)
  model <- exp_one_sample_t_test(df, val, mu = 9)$model[[1]]
  expected_power <- pwr::pwr.t.test(
    n = model$n, d = model$cohens_d, sig.level = 0.05,
    type = "one.sample", alternative = "two.sided"
  )$power
  expect_equal(model$power, expected_power)
})

test_that("power uses correct alternative for one-sided tests", {
  set.seed(7)
  vec <- rnorm(40, mean = 10, sd = 3)
  df <- data.frame(val = vec)
  model <- exp_one_sample_t_test(df, val, mu = 9, alternative = "greater")$model[[1]]
  expected_power <- pwr::pwr.t.test(
    n = model$n, d = model$cohens_d, sig.level = 0.05,
    type = "one.sample", alternative = "greater"
  )$power
  expect_equal(model$power, expected_power)
})

# --- confidence interval bounds ---

test_that("CI bounds match base t.test", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  model <- exp_one_sample_t_test(df, x, mu = 4.5)$model[[1]]
  expected <- t.test(vec, mu = 4.5, alternative = "two.sided")
  expect_equal(model$htest$conf.int[1], expected$conf.int[1])
  expect_equal(model$htest$conf.int[2], expected$conf.int[2])
})

test_that("conf.level controls the CI independently of sig.level", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  # conf.level = 0.99, sig.level stays 0.05
  model <- exp_one_sample_t_test(df, x, mu = 4.5, conf.level = 0.99)$model[[1]]
  expected <- t.test(vec, mu = 4.5, conf.level = 0.99)
  expect_equal(model$conf_level, 0.99)
  expect_equal(model$htest$conf.int[1], expected$conf.int[1])
  expect_equal(model$htest$conf.int[2], expected$conf.int[2])
  # sig.level still drives power / significance
  expect_equal(model$sig.level, 0.05)
})

test_that("conf.level defaults to 1 - sig.level when not specified", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  model <- exp_one_sample_t_test(df, x, mu = 4.5, sig.level = 0.01)$model[[1]]
  expected <- t.test(vec, mu = 4.5, conf.level = 0.99)
  expect_equal(model$conf_level, 0.99)
  expect_equal(model$htest$conf.int[1], expected$conf.int[1])
  expect_equal(model$htest$conf.int[2], expected$conf.int[2])
})

# --- NA exclusion ---

test_that("NA values are excluded before evaluation; n counts non-NA only", {
  vec_clean <- c(5.1, 4.8, 5.3, 4.9, 5.2, 5.0, 4.7, 5.1, 5.4, 4.6)
  df <- data.frame(x = c(vec_clean, NA, NA, NA))
  model <- exp_one_sample_t_test(df, x, mu = 5)$model[[1]]
  expected <- t.test(vec_clean, mu = 5)
  expect_equal(model$n, length(vec_clean))
  expect_equal(model$htest$p.value, expected$p.value)
  expect_equal(model$observed_mean, mean(vec_clean))
  # tidy must also surface the NA-excluded count
  tidied <- tidy(model, type = "model")
  expect_equal(tidied$`Number of Rows`, length(vec_clean))
})

# --- repeat-by (grouped) ---

test_that("repeat-by: one model per group", {
  df <- data.frame(
    x = c(rnorm(20, mean = 5), rnorm(20, mean = 7)),
    g = c(rep("A", 20), rep("B", 20))
  ) %>% dplyr::group_by(g)
  result <- exp_one_sample_t_test(df, x, mu = 5)
  expect_equal(nrow(result), 2)
})

test_that("repeat-by: grouped one-sided test is numerically correct per group", {
  # Combines repeat-by with a directional alternative (the ungrouped tests
  # cover greater/less; this fills the grouped x one-sided cell of the matrix).
  set.seed(3)
  vec_A <- rnorm(30, mean = 6, sd = 1)
  vec_B <- rnorm(30, mean = 4, sd = 1)
  df <- data.frame(
    x = c(vec_A, vec_B),
    g = c(rep("A", 30), rep("B", 30))
  ) %>% dplyr::group_by(g)
  result <- exp_one_sample_t_test(df, x, mu = 5, alternative = "greater")
  expect_equal(nrow(result), 2)

  model_A <- result$model[[which(result$g == "A")]]
  model_B <- result$model[[which(result$g == "B")]]
  exp_A <- t.test(vec_A, mu = 5, alternative = "greater")
  exp_B <- t.test(vec_B, mu = 5, alternative = "greater")
  expect_equal(model_A$htest$p.value, exp_A$p.value)
  expect_equal(model_B$htest$p.value, exp_B$p.value)
  expect_equal(model_A$alternative, "greater")
  expect_equal(tidy(model_A, type = "model")$`Test Direction`, "Greater than benchmark")
})

test_that("repeat-by: grouped results are numerically correct per group", {
  set.seed(1)
  vec_A <- rnorm(30, mean = 5, sd = 1)
  vec_B <- rnorm(30, mean = 8, sd = 1)
  df <- data.frame(
    x = c(vec_A, vec_B),
    g = c(rep("A", 30), rep("B", 30))
  ) %>% dplyr::group_by(g)
  result <- exp_one_sample_t_test(df, x, mu = 5)
  expect_equal(nrow(result), 2)

  model_A <- result$model[[which(result$g == "A")]]
  model_B <- result$model[[which(result$g == "B")]]
  exp_A <- t.test(vec_A, mu = 5)
  exp_B <- t.test(vec_B, mu = 5)
  expect_equal(model_A$htest$p.value, exp_A$p.value)
  expect_equal(model_B$htest$p.value, exp_B$p.value)
  expect_equal(model_A$n, 30L)
  expect_equal(model_B$n, 30L)
})

test_that("repeat-by: all-NA group is captured and tidied to a Note", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5, NA, NA, NA),
    g = c(rep("X", 5), rep("Y", 3))
  ) %>% dplyr::group_by(g)
  result <- exp_one_sample_t_test(df, x, mu = 0)
  expect_equal(nrow(result), 2)
  # group Y has no valid values -> error captured and tidied to a Note
  note_tidied <- tidy(result$model[[which(result$g == "Y")]], type = "model")
  expect_true("Note" %in% names(note_tidied))
})

test_that("repeat-by: group with exactly one non-NA value is captured and tidied to a Note", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5, 99),
    g = c(rep("X", 5), "Y")
  ) %>% dplyr::group_by(g)
  result <- exp_one_sample_t_test(df, x, mu = 0)
  expect_equal(nrow(result), 2)
  note_tidied <- tidy(result$model[[which(result$g == "Y")]], type = "model")
  expect_true("Note" %in% names(note_tidied))
})

# --- tidy(type = "model") ---

test_that("tidy model type returns the expected column set", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  model <- exp_one_sample_t_test(df, x, mu = 4.5)$model[[1]]
  tidied <- tidy(model, type = "model")
  expected_cols <- c(
    "Number of Rows", "Mean", "Std Deviation", "Std Error",
    "Hypothesized Mean", "Difference", "t Value", "DF", "P Value",
    "Conf Low", "Conf High", "Cohen's d", "Power",
    "Test Direction", "Result"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(tidied), info = paste("Missing column:", col))
  }
})

test_that("tidy model values are numerically correct", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  model <- exp_one_sample_t_test(df, x, mu = 4.5)$model[[1]]
  tidied <- tidy(model, type = "model")
  expected <- t.test(vec, mu = 4.5)
  expect_equal(tidied$`Number of Rows`, length(vec))
  expect_equal(tidied$`Mean`, mean(vec))
  expect_equal(tidied$`Std Deviation`, sd(vec))
  expect_equal(tidied$`Std Error`, sd(vec) / sqrt(length(vec)))
  expect_equal(tidied$`Hypothesized Mean`, 4.5)
  expect_equal(tidied$`Difference`, mean(vec) - 4.5)
  expect_equal(tidied$`t Value`, unname(expected$statistic))
  expect_equal(tidied$`DF`, unname(expected$parameter))
  expect_equal(tidied$`P Value`, expected$p.value)
  expect_equal(tidied$`Conf Low`, expected$conf.int[1])
  expect_equal(tidied$`Conf High`, expected$conf.int[2])
})

test_that("tidy model Test Direction maps correctly from alternative", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  two <- tidy(exp_one_sample_t_test(df, x, mu = 4.5, alternative = "two.sided")$model[[1]], type = "model")
  gt  <- tidy(exp_one_sample_t_test(df, x, mu = 4.5, alternative = "greater")$model[[1]],   type = "model")
  lt  <- tidy(exp_one_sample_t_test(df, x, mu = 5.5, alternative = "less")$model[[1]],      type = "model")
  expect_equal(two$`Test Direction`, "Different from benchmark")
  expect_equal(gt$`Test Direction`, "Greater than benchmark")
  expect_equal(lt$`Test Direction`, "Less than benchmark")
})

test_that("tidy model Result reflects significance against sig.level", {
  # mean = 10, mu = 0, sd = 1, n = 50 -> highly significant
  vec_sig <- rep(10, 50) + rnorm(50, sd = 0.1)
  df_sig <- data.frame(x = vec_sig)
  sig_model <- exp_one_sample_t_test(df_sig, x, mu = 0)$model[[1]]

  # mean ≈ 5, mu = 5, sd = 2, n = 30 -> p value will be large (not significant)
  set.seed(99)
  vec_ns <- rnorm(30, mean = 5, sd = 2)
  df_ns <- data.frame(x = vec_ns)
  ns_model <- exp_one_sample_t_test(df_ns, x, mu = mean(vec_ns))$model[[1]]

  expect_lt(sig_model$htest$p.value, 0.05)
  # p.value for testing against the exact sample mean is 1 (identical)
  expect_gte(ns_model$htest$p.value, 0.05)
  expect_equal(tidy(sig_model, type = "model")$Result, "Statistically significant.")
  expect_equal(tidy(ns_model, type = "model")$Result, "Not statistically significant.")
})

test_that("sig.level flips the significance Result on the same data", {
  # set.seed(42) rnorm(50, 5, 2) vs mu = 4.2 -> p approx 0.030. The identical
  # model is significant at sig.level 0.05 but not at 0.01, isolating the
  # effect of sig.level from the data and p-value.
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  sig_model <- exp_one_sample_t_test(df, x, mu = 4.2, sig.level = 0.05)$model[[1]]
  ns_model  <- exp_one_sample_t_test(df, x, mu = 4.2, sig.level = 0.01)$model[[1]]
  # Same underlying p-value, only sig.level differs.
  expect_equal(sig_model$htest$p.value, ns_model$htest$p.value)
  expect_gt(sig_model$htest$p.value, 0.01)
  expect_lt(sig_model$htest$p.value, 0.05)
  expect_equal(tidy(sig_model, type = "model")$Result, "Statistically significant.")
  expect_equal(tidy(ns_model, type = "model")$Result, "Not statistically significant.")
})

test_that("mu defaults to 0 when not specified", {
  set.seed(5)
  vec <- rnorm(40, mean = 2, sd = 1)
  df <- data.frame(x = vec)
  model <- exp_one_sample_t_test(df, x)$model[[1]]
  expected <- t.test(vec, mu = 0)
  expect_equal(model$mu, 0)
  expect_equal(model$htest$p.value, expected$p.value)
  expect_equal(tidy(model, type = "model")$`Hypothesized Mean`, 0)
})

# --- error handling (ungrouped) ---

test_that("ungrouped input with all-NA values raises an error", {
  # No grouping -> the per-group error is re-raised via stop(), not captured.
  df <- data.frame(x = as.numeric(c(NA, NA, NA)))
  expect_error(exp_one_sample_t_test(df, x, mu = 0))
})

test_that("ungrouped input with a single non-NA value raises an error", {
  # n = 1 < 2 -> not enough valid values; ungrouped so the error propagates.
  df <- data.frame(x = c(42, NA, NA))
  expect_error(exp_one_sample_t_test(df, x, mu = 0))
})

# --- tidy(type = "prob_dist") ---

test_that("tidy prob_dist marks the t statistic correctly", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  model <- exp_one_sample_t_test(df, x, mu = 4.5)$model[[1]]
  pd <- tidy(model, type = "prob_dist")
  expect_true(all(c("x", "y", "statistic", "critical", "p.value") %in% names(pd)))
  marked <- pd[which(pd$statistic), ]
  expect_equal(nrow(marked), 1)
  expected_t <- unname(model$htest$statistic)
  expect_equal(marked$x, expected_t)
  expect_equal(marked$p.value, model$htest$p.value)
})

test_that("tidy prob_dist critical region follows the alternative direction", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  # greater -> upper tail only
  gt <- tidy(exp_one_sample_t_test(df, x, mu = 4, alternative = "greater")$model[[1]], type = "prob_dist")
  expect_true(all(gt$x[which(gt$critical)] > 0))
  # less -> lower tail only
  lt <- tidy(exp_one_sample_t_test(df, x, mu = 6, alternative = "less")$model[[1]], type = "prob_dist")
  expect_true(all(lt$x[which(lt$critical)] < 0))
  # two.sided -> both tails
  ts <- tidy(exp_one_sample_t_test(df, x, mu = 4.5, alternative = "two.sided")$model[[1]], type = "prob_dist")
  expect_true(any(ts$x[which(ts$critical)] > 0) && any(ts$x[which(ts$critical)] < 0))
})

test_that("tidy prob_dist uses t-distribution (df column present)", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  model <- exp_one_sample_t_test(df, x, mu = 4.5)$model[[1]]
  pd <- tidy(model, type = "prob_dist")
  expect_true("df" %in% names(pd))
  expect_equal(unique(pd$df[!is.na(pd$df)]), unname(model$htest$parameter))
})

# --- tidy(type = "data") ---

test_that("tidy data type returns the raw data with the target column", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  model <- exp_one_sample_t_test(df, x, mu = 4.5)$model[[1]]
  data_out <- tidy(model, type = "data")
  expect_true("x" %in% names(data_out))
  expect_equal(nrow(data_out), 50)
  expect_equal(data_out$x, vec)
})

# --- glance ---

test_that("glance returns htest statistics with p.value", {
  set.seed(42)
  vec <- rnorm(50, mean = 5, sd = 2)
  df <- data.frame(x = vec)
  model <- exp_one_sample_t_test(df, x, mu = 4.5)$model[[1]]
  glanced <- glance(model)
  expect_true("p.value" %in% names(glanced))
  expect_equal(glanced$p.value, model$htest$p.value)
})

test_that("glance for error model returns a Note tibble", {
  df <- data.frame(
    x = c(1, NA, NA),
    g = c("X", "Y", "Y")
  ) %>% dplyr::group_by(g)
  result <- exp_one_sample_t_test(df, x, mu = 0)
  error_model <- result$model[[which(result$g == "Y")]]
  glanced <- glance(error_model)
  expect_true("Note" %in% names(glanced))
})

# --- stress test with complex column name ---

test_that("stress test with complex column name", {
  complex_name <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  set.seed(11)
  df <- data.frame(x = rnorm(20, mean = 3, sd = 1))
  names(df) <- complex_name
  result <- exp_one_sample_t_test(df, `航空 会社 !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表`, mu = 0)
  expect_true(!is.null(result))
  model <- result$model[[1]]
  expect_equal(model$var_col, complex_name)
  # tidy(type = "data") must round-trip the complex column name
  data_out <- tidy(model, type = "data")
  expect_true(complex_name %in% names(data_out))
  expect_equal(nrow(data_out), 20)
  # tidy(type = "model") must work without error
  tidied <- tidy(model, type = "model")
  expect_true("t Value" %in% names(tidied))
})
