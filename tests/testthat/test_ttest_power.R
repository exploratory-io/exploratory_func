context("test ttest power analysis")

# Independent reference: smallest group-1 size whose power (computed by pwr, pooled df) reaches the target,
# with group 2 = ceiling(n1 * allocation_ratio). Brute force so it does not share code with the implementation.
ref_min_n1_pwr <- function(d, power, sig.level, alternative, allocation_ratio) {
  d_signed <- if (alternative == "less") -abs(d) else abs(d)
  for (n1 in 2:200000) {
    n2 <- ceiling(round(n1 * allocation_ratio, 8))  # exact: 0.7 -> 0.3/0.7 must not become 181 for n1 = 420
    if (n2 < 2) next
    p <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = d_signed, sig.level = sig.level, alternative = alternative)$power
    if (p >= power) return(n1)
  }
  NA
}

test_that("ttest power: spec example (diff 5, sd 20) matches power.t.test", {
  model_df <- mtcars %>% exp_ttest_power(a_ratio = 0.5, diff_to_detect = 5, common_sd = 20, sig.level = 0.05, power = 0.8, alternative = "two.sided")
  res <- model_df %>% tidy_rowwise(model, type = "summary")
  ref <- ceiling(power.t.test(delta = 5, sd = 20, sig.level = 0.05, power = 0.8, type = "two.sample", alternative = "two.sided")$n)
  expect_equal(res$`Required Sample Size (Group 1)`, ref)
  expect_equal(res$`Required Sample Size (Group 2)`, ref)
  expect_equal(res$`Required Sample Size`, 2 * ref)
  expect_equal(res$`Cohen's D`, 0.25)
  expect_equal(res$`Min. Detectable Difference`, 5)
  expect_equal(res$`Expected Std. Deviation`, 20)
  expect_equal(res$`Type 1 Error`, 0.05)
  expect_equal(res$`Type 2 Error`, 0.2)
  expect_equal(res$Power, 0.8)
  expect_equal(res$DF, 2 * ref - 2)
  # Achieved power is at least the target, and equals the independent power.t.test value at that n.
  expect_gte(res$`Achieved Power`, 0.8)
  expect_equal(res$`Achieved Power`, power.t.test(n = ref, delta = 5, sd = 20, sig.level = 0.05)$power, tolerance = 1e-6)
})

test_that("ttest power: summary column names and order", {
  model_df <- mtcars %>% exp_ttest_power(diff_to_detect = 5, common_sd = 20)
  res <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_equal(colnames(res), c("Min. Detectable Difference", "Expected Std. Deviation", "Cohen's D",
                                "Type 1 Error", "Type 2 Error", "Power", "Achieved Power", "DF",
                                "Required Sample Size (Group 1)", "Required Sample Size (Group 2)", "Required Sample Size"))
})

test_that("ttest power: power argument is exact (no 1 - beta float noise), Type 2 Error is 1 - power", {
  model_df <- mtcars %>% exp_ttest_power(diff_to_detect = 5, common_sd = 20, power = 0.9)
  res <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_equal(res$Power, 0.9)
  expect_equal(res$`Type 2 Error`, 0.1, tolerance = 1e-12)
  ref <- ceiling(power.t.test(delta = 5, sd = 20, power = 0.9)$n)
  expect_equal(res$`Required Sample Size (Group 1)`, ref)
})

test_that("ttest power: one-sided alternatives match power.t.test, sign of difference is ignored", {
  for (alt in c("greater", "less")) {
    for (diff in c(5, -5)) {
      model_df <- mtcars %>% exp_ttest_power(diff_to_detect = diff, common_sd = 20, alternative = alt)
      res <- model_df %>% tidy_rowwise(model, type = "summary")
      ref <- ceiling(power.t.test(delta = 5, sd = 20, power = 0.8, alternative = "one.sided")$n)
      expect_equal(res$`Required Sample Size (Group 1)`, ref, info = paste(alt, diff))
    }
  }
})

test_that("ttest power: a_ratio != 0.5 uses allocation_ratio = (1 - a_ratio) / a_ratio, checked against pwr", {
  for (a in c(0.3, 0.7)) {
    ratio <- (1 - a) / a
    model_df <- mtcars %>% exp_ttest_power(a_ratio = a, diff_to_detect = 5, common_sd = 20)
    res <- model_df %>% tidy_rowwise(model, type = "summary")
    n1 <- ref_min_n1_pwr(0.25, 0.8, 0.05, "two.sided", ratio)
    expect_equal(res$`Required Sample Size (Group 1)`, n1, info = as.character(a))
    expect_equal(res$`Required Sample Size (Group 2)`, ceiling(round(n1 * ratio, 8)), info = as.character(a))
    expect_equal(res$`Required Sample Size`, n1 + ceiling(round(n1 * ratio, 8)), info = as.character(a))
    p <- pwr::pwr.t2n.test(n1 = n1, n2 = ceiling(round(n1 * ratio, 8)), d = 0.25, sig.level = 0.05)$power
    expect_equal(res$`Achieved Power`, p, tolerance = 1e-6)
  }
})

test_that("ttest power: Welch (var_equal = FALSE)", {
  # With equal group sizes, Welch df equals pooled df, so the result is identical to the equal-variance one.
  eq <- mtcars %>% exp_ttest_power(a_ratio = 0.5, diff_to_detect = 5, common_sd = 20, var_equal = TRUE) %>% tidy_rowwise(model, type = "summary")
  we <- mtcars %>% exp_ttest_power(a_ratio = 0.5, diff_to_detect = 5, common_sd = 20, var_equal = FALSE) %>% tidy_rowwise(model, type = "summary")
  expect_equal(we$`Required Sample Size`, eq$`Required Sample Size`)
  expect_equal(we$DF, eq$DF)

  # Unbalanced: Welch df (Satterthwaite with equal SD) is smaller than pooled df, so it never needs fewer subjects
  # and the reported DF matches the textbook formula at the reported group sizes.
  eq <- mtcars %>% exp_ttest_power(a_ratio = 0.2, diff_to_detect = 5, common_sd = 20, var_equal = TRUE) %>% tidy_rowwise(model, type = "summary")
  we <- mtcars %>% exp_ttest_power(a_ratio = 0.2, diff_to_detect = 5, common_sd = 20, var_equal = FALSE) %>% tidy_rowwise(model, type = "summary")
  expect_gte(we$`Required Sample Size`, eq$`Required Sample Size`)
  n1 <- we$`Required Sample Size (Group 1)`
  n2 <- we$`Required Sample Size (Group 2)`
  v1 <- 1 / n1; v2 <- 1 / n2
  expect_equal(we$DF, (v1 + v2)^2 / (v1^2 / (n1 - 1) + v2^2 / (n2 - 1)), tolerance = 1e-9)
  expect_lt(we$DF, n1 + n2 - 2)
  # Independent power at the reported size (noncentral t with Welch df) reaches the target; one fewer in group 1 does not.
  pw <- function(n1, n2) {
    df <- (1 / n1 + 1 / n2)^2 / ((1 / n1)^2 / (n1 - 1) + (1 / n2)^2 / (n2 - 1))
    ncp <- 0.25 / sqrt(1 / n1 + 1 / n2)
    tc <- qt(0.975, df)
    pt(tc, df, ncp, lower.tail = FALSE) + pt(-tc, df, ncp)
  }
  expect_gte(pw(n1, n2), 0.8)
  expect_lt(pw(n1 - 1, ceiling((n1 - 1) * 4)), 0.8)
})

test_that("ttest power: very large effect needs the minimum sample size without uniroot errors", {
  res <- mtcars %>% exp_ttest_power(diff_to_detect = 1000, common_sd = 1) %>% tidy_rowwise(model, type = "summary")
  expect_equal(res$`Required Sample Size (Group 1)`, 2)
  expect_gte(res$`Achieved Power`, 0.8)
})

test_that("ttest power: invalid inputs give readable errors", {
  expect_error(exp_ttest_power(mtcars, diff_to_detect = 5, common_sd = 20, power = 1), "Desired Power must be between 0 and 1")
  expect_error(exp_ttest_power(mtcars, diff_to_detect = 5, common_sd = 20, power = 0), "Desired Power must be between 0 and 1")
  expect_error(exp_ttest_power(mtcars, diff_to_detect = 5, common_sd = 20, sig.level = 0), "Significance Level must be between 0 and 1")
  expect_error(exp_ttest_power(mtcars, diff_to_detect = 5, common_sd = 20, sig.level = 1), "Significance Level must be between 0 and 1")
  expect_error(exp_ttest_power(mtcars, diff_to_detect = 5, common_sd = 0), "Expected Std. Deviation must be greater than 0")
  expect_error(exp_ttest_power(mtcars, diff_to_detect = 5, common_sd = -1), "Expected Std. Deviation must be greater than 0")
  expect_error(exp_ttest_power(mtcars, diff_to_detect = 0, common_sd = 20), "Min. Detectable Difference must not be 0")
  expect_error(exp_ttest_power(mtcars, a_ratio = 0, diff_to_detect = 5, common_sd = 20), "Group Size Ratio must be greater than 0 and less than 1")
  expect_error(exp_ttest_power(mtcars, a_ratio = 1, diff_to_detect = 5, common_sd = 20), "Group Size Ratio must be greater than 0 and less than 1")
  expect_error(exp_ttest_power(mtcars, common_sd = 20), "Min. Detectable Difference is required")
  expect_error(exp_ttest_power(mtcars, diff_to_detect = 5), "Expected Std. Deviation is required")
  expect_error(exp_ttest_power(mtcars, diff_to_detect = NA_real_, common_sd = 20), "Min. Detectable Difference is required")
  # Effect so small that the target power is unreachable within the internal upper limit.
  expect_error(exp_ttest_power(mtcars, diff_to_detect = 1e-6, common_sd = 20), "Could not reach the target power")
})

test_that("ttest power: tidy n_to_power is consistent with pwr (pooled df)", {
  model_df <- mtcars %>% exp_ttest_power(a_ratio = 0.4, diff_to_detect = 5, common_sd = 20, alternative = "greater", n_start = 20, n_end = 600, n_step = 50)
  res <- model_df %>% tidy_rowwise(model, type = "n_to_power")
  expect_equal(colnames(res), c("n", "power"))
  expect_equal(res$n, seq(20, 600, by = 50))
  ref <- pwr::pwr.t2n.test(n1 = 0.4 * res$n, n2 = 0.6 * res$n, d = 0.25, sig.level = 0.05, alternative = "greater")$power
  expect_equal(res$power, ref, tolerance = 1e-8)

  # "less" is the mirror image of "greater".
  res_less <- mtcars %>% exp_ttest_power(a_ratio = 0.4, diff_to_detect = 5, common_sd = 20, alternative = "less", n_start = 20, n_end = 600, n_step = 50) %>%
    tidy_rowwise(model, type = "n_to_power")
  expect_equal(res_less$power, ref, tolerance = 1e-8)
  expect_true(all(diff(res$power) > 0))
})

test_that("ttest power: n_to_power with Welch uses unequal-variance df", {
  res <- mtcars %>% exp_ttest_power(a_ratio = 0.2, diff_to_detect = 5, common_sd = 20, var_equal = FALSE, n_start = 50, n_end = 50, n_step = 10) %>%
    tidy_rowwise(model, type = "n_to_power")
  n1 <- 10; n2 <- 40
  df <- (1 / n1 + 1 / n2)^2 / ((1 / n1)^2 / (n1 - 1) + (1 / n2)^2 / (n2 - 1))
  ncp <- 0.25 / sqrt(1 / n1 + 1 / n2)
  tc <- qt(0.975, df)
  expect_equal(res$power, pt(tc, df, ncp, lower.tail = FALSE) + pt(-tc, df, ncp), tolerance = 1e-10)
})

test_that("ttest power: density data shape and critical value", {
  # Skewed ratio, one-sided "less", and Welch must all work.
  mtcars %>% exp_ttest_power(a_ratio = 0.01, diff_to_detect = 5, common_sd = 20)
  mtcars %>% exp_ttest_power(a_ratio = 0.5, diff_to_detect = 5, common_sd = 20, alternative = "less")
  mtcars %>% exp_ttest_power(a_ratio = 0.3, diff_to_detect = 5, common_sd = 20, var_equal = FALSE)

  model_df <- mtcars %>% exp_ttest_power(diff_to_detect = 5, common_sd = 20)
  res <- model_df %>% tidy_rowwise(model, type = "density")
  expect_equal(colnames(res), c("x", "y", "ncp", "type", "critical", "df", "statistic"))
  # Alternative distribution is centered on the noncentrality parameter d / sqrt(1/n1 + 1/n2).
  s <- model_df %>% tidy_rowwise(model, type = "summary")
  n1 <- s$`Required Sample Size (Group 1)`; n2 <- s$`Required Sample Size (Group 2)`
  alt <- res[res$type == "Alternative", ]
  expect_equal(unique(alt$ncp), 0.25 / sqrt(1 / n1 + 1 / n2), tolerance = 1e-8)
  # Critical reference line row sits at the two-sided critical t for the required n.
  expect_equal(res$x[which(res$statistic)], qt(0.975, df = n1 + n2 - 2), tolerance = 1e-8)
})

test_that("ttest power: grouped data frame yields one model per group", {
  model_df <- mtcars %>% dplyr::group_by(am) %>% exp_ttest_power(diff_to_detect = 5, common_sd = 20)
  expect_equal(nrow(model_df), 2)
  res <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_equal(nrow(res), 2)
  expect_true("am" %in% colnames(res))
})

test_that("generate_ttest_density_data_for_power helper is unchanged", {
  res <- exploratory:::generate_ttest_density_data_for_power(0.2, 100, 100, 2, 100)
  expect_equal(colnames(res), c("x", "y", "ncp", "type", "critical", "df", "statistic"))
})

test_that("exp_ttest_power group 2 size has no floating point noise for slider group size ratios (#36528)", {
  # a_ratio comes from a 0.05-step slider. Group 2 must be the smallest integer >= n1 * (1 - a) / a, computed exactly.
  for (a in c(0.3, 0.35, 0.45, 0.6, 0.7, 0.85, 0.95)) {
    for (d in c(0.2, 0.35, 0.5, 0.8)) {
      res <- data.frame(x = 1) %>% exp_ttest_power(a_ratio = a, diff_to_detect = d, common_sd = 1) %>% tidy_rowwise(model, type = "summary")
      n1 <- res[["Required Sample Size (Group 1)"]]
      n2 <- res[["Required Sample Size (Group 2)"]]
      num <- round((1 - a) * 100); den <- round(a * 100)  # exact integer ratio, e.g. 30/70
      expect_equal(n2, (n1 * num + den - 1) %/% den, info = paste("a_ratio", a, "d", d))
    }
  }
})

test_that("exp_ttest_power sample size range stays whole numbers and rejects a bad step (#36528)", {
  res <- data.frame(x = 1) %>% exp_ttest_power(a_ratio = 0.15, diff_to_detect = 5, common_sd = 20, n_start = 10, n_end = 100, n_step = 10) %>% tidy_rowwise(model, type = "n_to_power")
  expect_true(all(res$n == round(res$n)))
  expect_true(min(res$n) >= 14)
  expect_error(data.frame(x = 1) %>% exp_ttest_power(diff_to_detect = 5, common_sd = 20, n_step = 0), "Step of the sample size range")
  expect_error(data.frame(x = 1) %>% exp_ttest_power(diff_to_detect = 5, common_sd = 20, n_step = NA_real_), "Step of the sample size range is required")
})

test_that("exp_ttest_power keeps the critical t value on the density chart for a huge effect (#36528)", {
  dens <- data.frame(x = 1) %>% exp_ttest_power(diff_to_detect = 100, common_sd = 1) %>% tidy_rowwise(model, type = "density")
  crit_row <- dens %>% dplyr::filter(!is.na(statistic) & statistic)
  expect_true(min(dens$x) <= -abs(crit_row$x[1]) && max(dens$x) >= abs(crit_row$x[1]))
})
