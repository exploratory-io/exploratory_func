context("One-Sample Proportion Test")

test_that("exact path matches binom.test", {
  df <- data.frame(outcome = c(rep(TRUE, 12), rep(FALSE, 88)))
  result <- exp_one_sample_prop_test(df, outcome, p = 0.1, method = "exact")
  model <- result$model[[1]]
  expected <- binom.test(12, 100, p = 0.1)
  expect_equal(model$htest$p.value, expected$p.value)
  expect_equal(model$method_used, "Exact Binomial Test")
})

test_that("approximate path matches prop.test", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate")
  model <- result$model[[1]]
  expected <- prop.test(50, 100, p = 0.4, correct = FALSE)
  expect_equal(model$htest$p.value, expected$p.value)
  expect_equal(model$method_used, "Approximate Test (Normal)")
})

test_that("auto picks exact when n*p < 5", {
  # n=20, p=0.1 -> n*p = 2 < 5 -> exact
  df <- data.frame(outcome = c(rep(TRUE, 2), rep(FALSE, 18)))
  result <- exp_one_sample_prop_test(df, outcome, p = 0.1, method = "auto")
  model <- result$model[[1]]
  expect_equal(model$method_used, "Exact Binomial Test")
})

test_that("auto picks approximate when n*p >= 5 and n*(1-p) >= 5", {
  # n=100, p=0.4 -> n*p = 40, n*(1-p) = 60, both >= 5 -> approximate
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "auto")
  model <- result$model[[1]]
  expect_equal(model$method_used, "Approximate Test (Normal)")
})

test_that("alternative = greater", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_one_sample_prop_test(df, outcome, p = 0.4, alternative = "greater", method = "approximate")
  model <- result$model[[1]]
  expected <- prop.test(50, 100, p = 0.4, alternative = "greater", correct = FALSE)
  expect_equal(model$htest$p.value, expected$p.value)
})

test_that("alternative = less", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_one_sample_prop_test(df, outcome, p = 0.4, alternative = "less", method = "approximate")
  model <- result$model[[1]]
  expected <- prop.test(50, 100, p = 0.4, alternative = "less", correct = FALSE)
  expect_equal(model$htest$p.value, expected$p.value)
})

test_that("exact alternative = less matches binom.test", {
  # Exact method paired with a directional alternative (the approximate paths
  # cover greater/less above; this fills the exact x less cell of the matrix).
  df <- data.frame(outcome = c(rep(TRUE, 12), rep(FALSE, 88)))
  result <- exp_one_sample_prop_test(df, outcome, p = 0.1, alternative = "less", method = "exact")
  model <- result$model[[1]]
  expected <- binom.test(12, 100, p = 0.1, alternative = "less")
  expect_equal(model$htest$p.value, expected$p.value)
  expect_equal(model$htest$conf.int[1], expected$conf.int[1])
  expect_equal(model$htest$conf.int[2], expected$conf.int[2])
  expect_equal(model$method_used, "Exact Binomial Test")
})

test_that("repeat-by: one model per group", {
  df <- data.frame(
    outcome = c(rep(TRUE, 10), rep(FALSE, 10), rep(TRUE, 8), rep(FALSE, 12)),
    g = c(rep("X", 20), rep("Y", 20))
  ) %>% dplyr::group_by(g)
  result <- exp_one_sample_prop_test(df, outcome, p = 0.5, method = "approximate")
  expect_equal(nrow(result), 2)
})

test_that("repeat-by: grouped approximate output is numerically valid per group", {
  df <- data.frame(
    outcome = c(rep(TRUE, 30), rep(FALSE, 70),   # A: 30/100
                rep(TRUE, 60), rep(FALSE, 40),    # B: 60/100
                rep(TRUE, 5),  rep(FALSE, 95)),   # C: 5/100
    g = c(rep("A", 100), rep("B", 100), rep("C", 100))
  ) %>% dplyr::group_by(g)
  result <- exp_one_sample_prop_test(df, outcome, p = 0.5, method = "approximate")
  expect_equal(nrow(result), 3)

  expected_counts <- list(A = 30, B = 60, C = 5)
  for (grp in names(expected_counts)) {
    model <- result$model[[which(result$g == grp)]]
    x <- expected_counts[[grp]]
    expected <- prop.test(x, 100, p = 0.5, correct = FALSE)
    expect_equal(model$x, x)
    expect_equal(model$n, 100)
    expect_equal(model$observed_prop, x / 100)
    expect_equal(model$htest$p.value, expected$p.value)
    expect_equal(model$htest$conf.int[1], expected$conf.int[1])
    expect_equal(model$htest$conf.int[2], expected$conf.int[2])
    expect_equal(model$method_used, "Approximate Test (Normal)")
  }
})

test_that("repeat-by: grouped exact test with alternative = greater per group", {
  df <- data.frame(
    outcome = c(rep(TRUE, 30), rep(FALSE, 70),
                rep(TRUE, 60), rep(FALSE, 40),
                rep(TRUE, 5),  rep(FALSE, 95)),
    g = c(rep("A", 100), rep("B", 100), rep("C", 100))
  ) %>% dplyr::group_by(g)
  result <- exp_one_sample_prop_test(df, outcome, p = 0.1, method = "exact", alternative = "greater")
  expect_equal(nrow(result), 3)

  expected_counts <- list(A = 30, B = 60, C = 5)
  for (grp in names(expected_counts)) {
    model <- result$model[[which(result$g == grp)]]
    x <- expected_counts[[grp]]
    expected <- binom.test(x, 100, p = 0.1, alternative = "greater")
    expect_equal(model$htest$p.value, expected$p.value)
    expect_equal(model$htest$conf.int[1], expected$conf.int[1])
    expect_equal(model$htest$conf.int[2], expected$conf.int[2])
    expect_equal(model$method_used, "Exact Binomial Test")
    expect_equal(model$alternative, "greater")
  }
})

test_that("NA values in the target column are excluded before evaluation", {
  # 30 TRUE, 20 FALSE, 50 NA -> only the 50 non-NA rows are evaluated.
  df <- data.frame(outcome = c(rep(TRUE, 30), rep(FALSE, 20), rep(NA, 50)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.5, method = "approximate")$model[[1]]
  # Total Observations / Successes / Observed Proportion are all NA-excluded.
  expect_equal(model$n, 50)
  expect_equal(model$x, 30)
  expect_equal(model$observed_prop, 0.6)
  # The test statistic must match prop.test run on the NA-removed counts (30/50).
  expected <- prop.test(30, 50, p = 0.5, correct = FALSE)
  expect_equal(model$htest$p.value, expected$p.value)
  # The tidied model surfaces the NA-excluded Total Observations, not nrow(df).
  tidied <- tidy(model, type = "model")
  expect_equal(tidied$`Total Observations`, 50)
})

test_that("tidy model type returns correct columns", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate")
  model <- result$model[[1]]
  tidied <- tidy(model, type = "model")
  expect_true("P Value" %in% names(tidied))
  expect_true("Observed Proportion" %in% names(tidied))
  expect_true("Benchmark Proportion" %in% names(tidied))
  expect_true("Result" %in% names(tidied))
})

test_that("tidy data type returns the raw data with the target column", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  result <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate")
  model <- result$model[[1]]
  data_out <- tidy(model, type = "data")
  # The data-level charts (Error Bar Plot, Data Distribution) reference the
  # original target column, so it must be present with all original rows.
  expect_true("outcome" %in% names(data_out))
  expect_equal(nrow(data_out), 100)
  expect_equal(sum(data_out$outcome), 50)
})

test_that("stress test with complex column name", {
  complex_name <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  df <- data.frame(x = c(TRUE, FALSE, TRUE, TRUE, FALSE))
  names(df) <- complex_name
  result <- exp_one_sample_prop_test(df, `航空 会社 !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表`, p = 0.5, method = "approximate")
  expect_true(!is.null(result))
  expect_equal(result$model[[1]]$var_col, complex_name)
  # tidy(type = "data") must round-trip the complex column name so the
  # data-level charts can map ___TARGET_COLUMN_NAME___ to it.
  data_out <- tidy(result$model[[1]], type = "data")
  expect_true(complex_name %in% names(data_out))
  expect_equal(nrow(data_out), 5)
})

# --- numeric correctness of derived/exposed outputs ---

test_that("approximate confidence interval matches prop.test", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  expected <- prop.test(50, 100, p = 0.4, correct = FALSE)
  expect_equal(model$htest$conf.int[1], expected$conf.int[1])
  expect_equal(model$htest$conf.int[2], expected$conf.int[2])
})

test_that("exact confidence interval matches binom.test", {
  df <- data.frame(outcome = c(rep(TRUE, 12), rep(FALSE, 88)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.1, method = "exact")$model[[1]]
  expected <- binom.test(12, 100, p = 0.1)
  expect_equal(model$htest$conf.int[1], expected$conf.int[1])
  expect_equal(model$htest$conf.int[2], expected$conf.int[2])
})

test_that("Cohen's h matches pwr::ES.h", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  expect_equal(model$cohens_h, pwr::ES.h(0.5, 0.4))
})

test_that("power matches pwr::pwr.p.test", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  expected <- pwr::pwr.p.test(h = pwr::ES.h(0.5, 0.4), n = 100,
                              sig.level = 0.05, alternative = "two.sided")$power
  expect_equal(model$power, expected)
})

test_that("tidy model exposes correct observed proportion, difference and CI values", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  tidied <- tidy(model, type = "model")
  expected <- prop.test(50, 100, p = 0.4, correct = FALSE)
  expect_equal(tidied$`Observed Proportion`, 0.5)
  expect_equal(tidied$`Benchmark Proportion`, 0.4)
  expect_equal(tidied$Difference, 0.5 - 0.4)
  expect_equal(tidied$`P Value`, expected$p.value)
  expect_equal(tidied$`Conf Low`, expected$conf.int[1])
  expect_equal(tidied$`Conf High`, expected$conf.int[2])
})

test_that("Z Value is the standardized statistic and is exposed before P Value", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  z <- (0.5 - 0.4) / sqrt(0.4 * 0.6 / 100)
  expect_equal(model$z, z)
  # prop.test's X-squared equals z^2 for the one-sample (correct = FALSE) case.
  expected <- prop.test(50, 100, p = 0.4, correct = FALSE)
  expect_equal(model$z^2, unname(expected$statistic))
  # The summary table surfaces Z Value immediately before P Value.
  tidied <- tidy(model, type = "model")
  expect_equal(tidied$`Z Value`, z)
  expect_equal(which(names(tidied) == "Z Value") + 1, which(names(tidied) == "P Value"))
})

test_that("tidy model maps Test Direction from alternative", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  two   <- tidy(exp_one_sample_prop_test(df, outcome, p = 0.4, alternative = "two.sided", method = "approximate")$model[[1]], type = "model")
  gt    <- tidy(exp_one_sample_prop_test(df, outcome, p = 0.4, alternative = "greater",  method = "approximate")$model[[1]], type = "model")
  lt    <- tidy(exp_one_sample_prop_test(df, outcome, p = 0.4, alternative = "less",     method = "approximate")$model[[1]], type = "model")
  expect_equal(two$`Test Direction`, "Different from benchmark")
  expect_equal(gt$`Test Direction`, "Greater than benchmark")
  expect_equal(lt$`Test Direction`, "Less than benchmark")
})

test_that("tidy model Result reflects significance against sig.level", {
  # obs 0.5 vs p 0.4, n=100 -> p approx 0.041 < 0.05 -> significant
  sig_model <- exp_one_sample_prop_test(data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50))),
                             outcome, p = 0.4, method = "approximate")$model[[1]]
  # obs 0.5 vs p 0.45, n=100 -> p approx 0.315 -> not significant
  ns_model  <- exp_one_sample_prop_test(data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50))),
                             outcome, p = 0.45, method = "approximate")$model[[1]]
  expect_lt(sig_model$htest$p.value, 0.05)
  expect_gt(ns_model$htest$p.value, 0.05)
  expect_equal(tidy(sig_model, type = "model")$Result, "Statistically significant.")
  expect_equal(tidy(ns_model, type = "model")$Result, "Not statistically significant.")
})

test_that("tidy data type preserves the raw observations for the chart", {
  df <- data.frame(outcome = c(rep(TRUE, 12), rep(FALSE, 88)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.1, method = "exact")$model[[1]]
  data_out <- tidy(model, type = "data")
  expect_equal(nrow(data_out), 100)
  # mean of the logical column equals the observed proportion the chart plots.
  expect_equal(mean(data_out$outcome), model$observed_prop)
})

test_that("conf.level controls the CI independently of sig.level", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  # conf.level explicitly set to 0.99 while sig.level stays 0.05.
  model <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate", conf.level = 0.99)$model[[1]]
  expected <- prop.test(50, 100, p = 0.4, conf.level = 0.99, correct = FALSE)
  expect_equal(model$conf_level, 0.99)
  expect_equal(model$htest$conf.int[1], expected$conf.int[1])
  expect_equal(model$htest$conf.int[2], expected$conf.int[2])
  # sig.level still drives the power / significance calculation.
  expect_equal(model$sig.level, 0.05)
})

test_that("conf.level defaults to 1 - sig.level when not specified", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate", sig.level = 0.01)$model[[1]]
  expected <- prop.test(50, 100, p = 0.4, conf.level = 0.99, correct = FALSE)
  expect_equal(model$conf_level, 0.99)
  expect_equal(model$htest$conf.int[1], expected$conf.int[1])
  expect_equal(model$htest$conf.int[2], expected$conf.int[2])
})

test_that("sig.level flips the significance Result on the same data", {
  # obs 0.5 vs p 0.4, n=100 -> p approx 0.041. The identical model is
  # significant at sig.level 0.05 but not at 0.01, isolating sig.level's effect.
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  sig_model <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate", sig.level = 0.05)$model[[1]]
  ns_model  <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate", sig.level = 0.01)$model[[1]]
  # Same underlying p-value, only sig.level differs.
  expect_equal(sig_model$htest$p.value, ns_model$htest$p.value)
  expect_equal(tidy(sig_model, type = "model")$Result, "Statistically significant.")
  expect_equal(tidy(ns_model, type = "model")$Result, "Not statistically significant.")
})

test_that("glance returns htest statistics", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  glanced <- glance(model)
  expect_true("p.value" %in% names(glanced))
  expect_equal(glanced$p.value, model$htest$p.value)
})

test_that("repeat-by: a failing group is captured and tidied as a Note", {
  df <- data.frame(
    outcome = c(rep(TRUE, 10), rep(FALSE, 10), rep(NA, 20)),
    g = c(rep("X", 20), rep("Y", 20))
  ) %>% dplyr::group_by(g)
  result <- exp_one_sample_prop_test(df, outcome, p = 0.5, method = "approximate")
  expect_equal(nrow(result), 2)
  # group Y is all-NA -> error object captured per group, tidied to a Note
  note_tidied <- tidy(result$model[[2]], type = "model")
  expect_true("Note" %in% names(note_tidied))
})

# --- probability distribution (line) chart data ---

test_that("tidy prob_dist returns standard normal density with the marked z statistic", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  pd <- tidy(model, type = "prob_dist")
  expect_true(all(c("x", "y", "statistic", "critical", "p.value") %in% names(pd)))
  # The standardized statistic z = (phat - p0) / sqrt(p0 * (1 - p0) / n).
  z <- (0.5 - 0.4) / sqrt(0.4 * 0.6 / 100)
  marked <- pd[which(pd$statistic), ]
  expect_equal(nrow(marked), 1)
  expect_equal(marked$x, z)
  expect_equal(marked$p.value, model$htest$p.value)
  # It is the standard normal N(0, 1): centered at 0, peak density is dnorm(0).
  expect_equal(unique(pd$mean), 0)
  expect_equal(unique(pd$sd), 1)
  expect_true(max(pd$y, na.rm = TRUE) <= dnorm(0) + 1e-9)
})

test_that("tidy prob_dist always uses the normal distribution even for the exact method", {
  df <- data.frame(outcome = c(rep(TRUE, 12), rep(FALSE, 88)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.1, method = "exact")$model[[1]]
  expect_equal(model$method_used, "Exact Binomial Test")
  pd <- tidy(model, type = "prob_dist")
  # z is still derived the same way regardless of the method actually used.
  z <- (0.12 - 0.1) / sqrt(0.1 * 0.9 / 100)
  marked <- pd[which(pd$statistic), ]
  expect_equal(nrow(marked), 1)
  expect_equal(marked$x, z)
})

test_that("tidy prob_dist critical region follows the alternative direction", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  # greater -> rejection region is the upper tail only.
  gt <- tidy(exp_one_sample_prop_test(df, outcome, p = 0.4, alternative = "greater", method = "approximate")$model[[1]], type = "prob_dist")
  expect_true(all(gt$x[which(gt$critical)] > 0))
  # less -> rejection region is the lower tail only.
  lt <- tidy(exp_one_sample_prop_test(df, outcome, p = 0.4, alternative = "less", method = "approximate")$model[[1]], type = "prob_dist")
  expect_true(all(lt$x[which(lt$critical)] < 0))
  # two.sided -> both tails are flagged.
  ts <- tidy(exp_one_sample_prop_test(df, outcome, p = 0.4, alternative = "two.sided", method = "approximate")$model[[1]], type = "prob_dist")
  expect_true(any(ts$x[which(ts$critical)] > 0) && any(ts$x[which(ts$critical)] < 0))
})

# --- tidy(type = "prob_dist_prop") ---

test_that("tidy prob_dist_prop marks the observed proportion on the proportion scale", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.4, method = "approximate")$model[[1]]
  pd <- tidy(model, type = "prob_dist_prop")
  expect_true(all(c("x", "y", "statistic", "critical", "p.value") %in% names(pd)))
  # SE = sqrt(p0*(1-p0)/n); the marked point sits at the observed proportion (NOT z).
  se <- sqrt(0.4 * 0.6 / 100)
  marked <- pd[which(pd$statistic), ]
  expect_equal(nrow(marked), 1)
  expect_equal(marked$x, 0.5)
  expect_equal(marked$p.value, model$htest$p.value)
  # Normal centered at the benchmark proportion p0 with sd = SE.
  expect_equal(unique(pd$mean), 0.4)
  expect_equal(unique(pd$sd), se)
  expect_true(max(pd$y, na.rm = TRUE) <= dnorm(0.4, mean = 0.4, sd = se) + 1e-9)
})

test_that("tidy prob_dist_prop always uses the proportion-scale normal even for the exact method", {
  df <- data.frame(outcome = c(rep(TRUE, 12), rep(FALSE, 88)))
  model <- exp_one_sample_prop_test(df, outcome, p = 0.1, method = "exact")$model[[1]]
  expect_equal(model$method_used, "Exact Binomial Test")
  pd <- tidy(model, type = "prob_dist_prop")
  se <- sqrt(0.1 * 0.9 / 100)
  marked <- pd[which(pd$statistic), ]
  expect_equal(nrow(marked), 1)
  expect_equal(marked$x, 0.12)
  expect_equal(unique(pd$mean), 0.1)
  expect_equal(unique(pd$sd), se)
})

test_that("tidy prob_dist_prop critical region follows the alternative direction", {
  df <- data.frame(outcome = c(rep(TRUE, 50), rep(FALSE, 50)))
  p0 <- 0.4
  # greater -> rejection region is the upper tail only (above the benchmark proportion).
  gt <- tidy(exp_one_sample_prop_test(df, outcome, p = p0, alternative = "greater", method = "approximate")$model[[1]], type = "prob_dist_prop")
  expect_true(all(gt$x[which(gt$critical)] > p0))
  # less -> rejection region is the lower tail only.
  lt <- tidy(exp_one_sample_prop_test(df, outcome, p = p0, alternative = "less", method = "approximate")$model[[1]], type = "prob_dist_prop")
  expect_true(all(lt$x[which(lt$critical)] < p0))
  # two.sided -> both tails relative to the benchmark proportion.
  ts <- tidy(exp_one_sample_prop_test(df, outcome, p = p0, alternative = "two.sided", method = "approximate")$model[[1]], type = "prob_dist_prop")
  expect_true(any(ts$x[which(ts$critical)] > p0) && any(ts$x[which(ts$critical)] < p0))
})

test_that("tidy prob_dist_prop returns an empty tibble when the SE is 0/NA", {
  # n*p0 produces a valid SE normally; build a degenerate model object directly to
  # exercise the guard (the benchmark SE is 0 only for a degenerate p0).
  model <- structure(
    list(htest = list(p.value = NA_real_), p = 0.4, observed_prop = 0.5,
         se = 0, sig.level = 0.05, alternative = "two.sided"),
    class = c("one_sample_prop_test_exploratory", "list"))
  pd <- tidy(model, type = "prob_dist_prop")
  expect_equal(nrow(pd), 0)
})
