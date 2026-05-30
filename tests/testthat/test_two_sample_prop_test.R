context("Two-Sample Proportion Test")

test_that("approximate path matches prop.test", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  result <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")
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
  result <- exp_two_sample_prop_test(df, outcome, group, method = "exact")
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
  result <- exp_two_sample_prop_test(df, outcome, group, method = "auto")
  model <- result$model[[1]]
  expect_equal(model$method_used, "Fisher's Exact Test")
})

test_that("auto method selects approximate when expected cells all >= 5", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  result <- exp_two_sample_prop_test(df, outcome, group, method = "auto")
  model <- result$model[[1]]
  expect_equal(model$method_used, "Approximate Test (Normal)")
})

test_that("alternative = greater", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  result <- exp_two_sample_prop_test(df, outcome, group, alternative = "greater", method = "approximate")
  model <- result$model[[1]]

  expected <- prop.test(x = c(23, 12), n = c(50, 40), alternative = "greater", correct = FALSE)
  expect_equal(model$p_value, expected$p.value)
})

test_that("alternative = less", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  result <- exp_two_sample_prop_test(df, outcome, group, alternative = "less", method = "approximate")
  model <- result$model[[1]]

  expected <- prop.test(x = c(23, 12), n = c(50, 40), alternative = "less", correct = FALSE)
  expect_equal(model$p_value, expected$p.value)
})

test_that("error when explanatory has 3+ unique values (no NA)", {
  df <- data.frame(
    outcome = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    group = c("A", "B", "C", "A", "B")
  )
  expect_error(exp_two_sample_prop_test(df, outcome, group),
               "The explanatory variable needs to have 2 unique values.")
})

test_that("auto-filter NA as 3rd category when explanatory has 3 unique values including NA", {
  df <- data.frame(
    outcome = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 3), rep(FALSE, 7), TRUE, FALSE),
    group = c(rep("A", 10), rep("B", 10), NA, NA)
  )
  result <- exp_two_sample_prop_test(df, outcome, group)
  expect_true(!is.null(result$model[[1]]))
  expect_equal(result$model[[1]]$nA + result$model[[1]]$nB, 20)
})

test_that("repeat-by: one model per group", {
  df <- data.frame(
    outcome = c(rep(TRUE, 10), rep(FALSE, 10), rep(TRUE, 8), rep(FALSE, 12)),
    group = c(rep("A", 20), rep("B", 20)),
    repeat_col = c(rep("X", 20), rep("Y", 20))
  ) %>% dplyr::group_by(repeat_col)
  result <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")
  expect_equal(nrow(result), 2)
})

test_that("tidy model type returns summary data frame", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  result <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")
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
  result <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")
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
    outcome = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    group = c("A", "A", "A", "B", "B", "B")
  )
  names(df)[1] <- complex_name
  result <- exp_two_sample_prop_test(df, `航空 会社 !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表`, group, method = "approximate")
  expect_true(!is.null(result))
  expect_equal(result$model[[1]]$var_col, complex_name)
})

# --- numeric correctness of derived/exposed outputs ---

test_that("Cohen's h matches pwr::ES.h", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  model <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")$model[[1]]
  expect_equal(model$cohens_h, pwr::ES.h(23 / 50, 12 / 40))
})

test_that("power matches pwr::pwr.2p2n.test", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  model <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")$model[[1]]
  expected <- pwr::pwr.2p2n.test(h = pwr::ES.h(23 / 50, 12 / 40), n1 = 50, n2 = 40,
                                 sig.level = 0.05, alternative = "two.sided")$power
  expect_equal(model$power, expected)
})

test_that("Z Value is the pooled two-sample z and is exposed before P Value", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  model <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")$model[[1]]
  p_pool <- (23 + 12) / (50 + 40)
  z <- (23 / 50 - 12 / 40) / sqrt(p_pool * (1 - p_pool) * (1 / 50 + 1 / 40))
  expect_equal(model$z, z)
  # prop.test's X-squared equals z^2 for the 2x2 (correct = FALSE) case.
  expected <- prop.test(x = c(23, 12), n = c(50, 40), correct = FALSE)
  expect_equal(model$z^2, unname(expected$statistic))
  # The summary table surfaces Z Value immediately before P Value.
  tidied <- tidy(model, type = "model")
  expect_equal(tidied$`Z Value`, z)
  expect_equal(which(names(tidied) == "Z Value") + 1, which(names(tidied) == "P Value"))
})

test_that("approximate difference CI matches prop.test", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  model <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")$model[[1]]
  expected <- prop.test(x = c(23, 12), n = c(50, 40), correct = FALSE)
  expect_equal(model$diff_low, expected$conf.int[1])
  expect_equal(model$diff_high, expected$conf.int[2])
})

test_that("NA values in the target column are excluded before evaluation", {
  # group A: 20 TRUE, 10 FALSE, 10 NA ; group B: 15 TRUE, 15 FALSE
  df <- data.frame(
    outcome = c(rep(TRUE, 20), rep(FALSE, 10), rep(NA, 10), rep(TRUE, 15), rep(FALSE, 15)),
    group = c(rep("A", 40), rep("B", 30))
  )
  model <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")$model[[1]]
  # NA rows are dropped: A becomes 20/30, B stays 15/30.
  expect_equal(model$nA, 30)
  expect_equal(model$xA, 20)
  expect_equal(model$nB, 30)
  expect_equal(model$xB, 15)
  expected <- prop.test(x = c(20, 15), n = c(30, 30), correct = FALSE)
  expect_equal(model$p_value, expected$p.value)
})

# --- glance ---

test_that("glance returns htest statistics", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  model <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")$model[[1]]
  glanced <- glance(model)
  expect_true("p.value" %in% names(glanced))
  expect_equal(glanced$p.value, model$p_value)
})

test_that("repeat-by: a failing group is captured and tidied as a Note", {
  # group Y has an all-NA outcome -> nothing left after NA removal -> error captured.
  df <- data.frame(
    outcome = c(rep(TRUE, 10), rep(FALSE, 10), rep(NA, 20)),
    group = c(rep("A", 10), rep("B", 10), rep("A", 10), rep("B", 10)),
    repeat_col = c(rep("X", 20), rep("Y", 20))
  ) %>% dplyr::group_by(repeat_col)
  result <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")
  expect_equal(nrow(result), 2)
  note_tidied <- tidy(result$model[[which(result$repeat_col == "Y")]], type = "model")
  expect_true("Note" %in% names(note_tidied))
})

# --- conf.level handling ---

test_that("conf.level controls the CI independently of sig.level", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  model <- exp_two_sample_prop_test(df, outcome, group, method = "approximate", conf.level = 0.99)$model[[1]]
  expected <- prop.test(x = c(23, 12), n = c(50, 40), correct = FALSE, conf.level = 0.99)
  expect_equal(model$conf_level, 0.99)
  expect_equal(model$diff_low, expected$conf.int[1])
  expect_equal(model$diff_high, expected$conf.int[2])
  # sig.level still drives the power / significance calculation.
  expect_equal(model$sig.level, 0.05)
})

test_that("conf.level defaults to 1 - sig.level when not specified", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  model <- exp_two_sample_prop_test(df, outcome, group, method = "approximate", sig.level = 0.01)$model[[1]]
  expected <- prop.test(x = c(23, 12), n = c(50, 40), correct = FALSE, conf.level = 0.99)
  expect_equal(model$conf_level, 0.99)
  expect_equal(model$diff_low, expected$conf.int[1])
  expect_equal(model$diff_high, expected$conf.int[2])
})

test_that("conf.level is reflected in the per-group CIs of the data chart", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  model <- exp_two_sample_prop_test(df, outcome, group, method = "approximate", conf.level = 0.99)$model[[1]]
  ciA <- prop.test(23, 50, correct = FALSE, conf.level = 0.99)$conf.int
  ciB <- prop.test(12, 40, correct = FALSE, conf.level = 0.99)$conf.int
  expect_equal(model$ciA[1], ciA[1])
  expect_equal(model$ciA[2], ciA[2])
  expect_equal(model$ciB[1], ciB[1])
  expect_equal(model$ciB[2], ciB[2])
})

# --- probability distribution (line) chart data ---

test_that("tidy prob_dist returns standard normal density with the marked z statistic", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  model <- exp_two_sample_prop_test(df, outcome, group, method = "approximate")$model[[1]]
  pd <- tidy(model, type = "prob_dist")
  expect_true(all(c("x", "y", "statistic", "critical", "p.value") %in% names(pd)))
  # Pooled two-sample z = (pA - pB) / sqrt(p_pool*(1-p_pool)*(1/nA + 1/nB)).
  p_pool <- (23 + 12) / (50 + 40)
  z <- (23 / 50 - 12 / 40) / sqrt(p_pool * (1 - p_pool) * (1 / 50 + 1 / 40))
  marked <- pd[which(pd$statistic), ]
  expect_equal(nrow(marked), 1)
  expect_equal(marked$x, z)
  expect_equal(marked$p.value, model$p_value)
  # It is the standard normal N(0, 1): centered at 0, peak density is dnorm(0).
  expect_equal(unique(pd$mean), 0)
  expect_equal(unique(pd$sd), 1)
  expect_true(max(pd$y, na.rm = TRUE) <= dnorm(0) + 1e-9)
})

test_that("tidy prob_dist always uses the normal distribution even for the exact method", {
  df <- data.frame(
    outcome = c(rep(TRUE, 3), rep(FALSE, 7), rep(TRUE, 1), rep(FALSE, 4)),
    group = c(rep("A", 10), rep("B", 5))
  )
  model <- exp_two_sample_prop_test(df, outcome, group, method = "exact")$model[[1]]
  expect_equal(model$method_used, "Fisher's Exact Test")
  pd <- tidy(model, type = "prob_dist")
  # z is still derived the same (pooled) way regardless of the method actually used.
  p_pool <- (3 + 1) / (10 + 5)
  z <- (3 / 10 - 1 / 5) / sqrt(p_pool * (1 - p_pool) * (1 / 10 + 1 / 5))
  marked <- pd[which(pd$statistic), ]
  expect_equal(nrow(marked), 1)
  expect_equal(marked$x, z)
})

test_that("tidy prob_dist critical region follows the alternative direction", {
  df <- data.frame(
    outcome = c(rep(TRUE, 23), rep(FALSE, 27), rep(TRUE, 12), rep(FALSE, 28)),
    group = c(rep("A", 50), rep("B", 40))
  )
  # greater -> rejection region is the upper tail only.
  gt <- tidy(exp_two_sample_prop_test(df, outcome, group, alternative = "greater", method = "approximate")$model[[1]], type = "prob_dist")
  expect_true(all(gt$x[which(gt$critical)] > 0))
  # less -> rejection region is the lower tail only.
  lt <- tidy(exp_two_sample_prop_test(df, outcome, group, alternative = "less", method = "approximate")$model[[1]], type = "prob_dist")
  expect_true(all(lt$x[which(lt$critical)] < 0))
  # two.sided -> both tails are flagged.
  ts <- tidy(exp_two_sample_prop_test(df, outcome, group, alternative = "two.sided", method = "approximate")$model[[1]], type = "prob_dist")
  expect_true(any(ts$x[which(ts$critical)] > 0) && any(ts$x[which(ts$critical)] < 0))
})
