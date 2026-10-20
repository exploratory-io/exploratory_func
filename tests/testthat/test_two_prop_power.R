context("test two sample proportion power analysis")

# Independent reference for the achieved power of a two-sample proportion z-test with unpooled alternative variance
# and weighted pooled null variance (written out from the textbook definition, without the implementation's helper).
ref_prop_power <- function(p1, p2, n1, n2, sig.level, alternative) {
  pbar <- (n1 * p1 + n2 * p2) / (n1 + n2)
  sd0 <- sqrt(pbar * (1 - pbar) * (1 / n1 + 1 / n2))
  sd1 <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  d <- p2 - p1
  if (alternative == "two.sided") {
    za <- qnorm(1 - sig.level / 2)
    pnorm((d - za * sd0) / sd1) + pnorm((-d - za * sd0) / sd1)
  } else if (alternative == "greater") {
    pnorm((d - qnorm(1 - sig.level) * sd0) / sd1)
  } else {
    pnorm((-d - qnorm(1 - sig.level) * sd0) / sd1)
  }
}

test_that("two prop power: spec example (0.30, +0.05) matches power.prop.test and pwr", {
  model_df <- mtcars %>% exp_two_prop_power(baseline_prop = 0.30, diff_to_detect = 0.05, sig.level = 0.05, power = 0.8, alternative = "two.sided", a_ratio = 0.5)
  res <- model_df %>% tidy_rowwise(model, type = "summary")
  ref <- power.prop.test(p1 = 0.30, p2 = 0.35, sig.level = 0.05, power = 0.8, alternative = "two.sided")$n
  expect_lte(abs(res$`Required Sample Size (Group 1)` - ceiling(ref)), 1)
  expect_equal(res$`Required Sample Size (Group 1)`, ceiling(ref))
  # Cross-check against pwr (arcsine transformation, a slightly different approximation): within +-1% of sample size.
  ref_pwr <- pwr::pwr.2p.test(h = pwr::ES.h(0.35, 0.30), sig.level = 0.05, power = 0.8)$n
  expect_lte(abs(res$`Required Sample Size (Group 1)` - ref_pwr) / ref_pwr, 0.01)
  expect_equal(res$`Required Sample Size (Group 2)`, res$`Required Sample Size (Group 1)`)
  expect_equal(res$`Required Sample Size`, 2 * res$`Required Sample Size (Group 1)`)
  expect_equal(res$`Baseline Proportion`, 0.30)
  expect_equal(res$`Comparison Proportion`, 0.35)
  expect_equal(res$`Min. Detectable Difference`, 0.05)
  expect_equal(res$`Type 1 Error`, 0.05)
  expect_equal(res$`Type 2 Error`, 0.2)
  expect_equal(res$Power, 0.8)
  expect_gte(res$`Achieved Power`, 0.8)
  expect_equal(res$`Achieved Power`,
               power.prop.test(n = res$`Required Sample Size (Group 1)`, p1 = 0.30, p2 = 0.35, sig.level = 0.05)$power,
               tolerance = 1e-6)
})

test_that("two prop power: summary column names and order", {
  res <- mtcars %>% exp_two_prop_power(baseline_prop = 0.3, diff_to_detect = 0.05) %>% tidy_rowwise(model, type = "summary")
  expect_equal(colnames(res), c("Baseline Proportion", "Comparison Proportion", "Min. Detectable Difference",
                                "Type 1 Error", "Type 2 Error", "Power", "Achieved Power",
                                "Required Sample Size (Group 1)", "Required Sample Size (Group 2)", "Required Sample Size"))
})

test_that("two prop power: issue example (click rate 10% -> +2 points)", {
  res <- mtcars %>% exp_two_prop_power(baseline_prop = 0.10, diff_to_detect = 0.02) %>% tidy_rowwise(model, type = "summary")
  ref <- ceiling(power.prop.test(p1 = 0.10, p2 = 0.12, power = 0.8)$n)
  expect_equal(res$`Required Sample Size (Group 1)`, ref)
})

test_that("two prop power: a_ratio != 0.5 uses allocation_ratio = (1 - a_ratio) / a_ratio (group 1 = baseline)", {
  for (a in c(0.3, 0.7)) {
    ratio <- (1 - a) / a
    res <- mtcars %>% exp_two_prop_power(baseline_prop = 0.30, diff_to_detect = 0.05, a_ratio = a) %>% tidy_rowwise(model, type = "summary")
    n1 <- res$`Required Sample Size (Group 1)`
    n2 <- res$`Required Sample Size (Group 2)`
    expect_equal(n2, ceiling(round(n1 * ratio, 8)), info = as.character(a))
    expect_equal(res$`Required Sample Size`, n1 + n2, info = a)
    # Achieved power matches the independent calculation and reaches the target.
    expect_equal(res$`Achieved Power`, ref_prop_power(0.30, 0.35, n1, n2, 0.05, "two.sided"), tolerance = 1e-8, info = a)
    expect_gte(res$`Achieved Power`, 0.8)
    # pwr (arcsine) agrees on power at that allocation to within 2 points of power.
    p_pwr <- pwr::pwr.2p2n.test(h = pwr::ES.h(0.35, 0.30), n1 = n2, n2 = n1, sig.level = 0.05)$power
    expect_lt(abs(p_pwr - res$`Achieved Power`), 0.02)
    # Unbalanced design needs more subjects in total than the balanced one.
    bal <- mtcars %>% exp_two_prop_power(baseline_prop = 0.30, diff_to_detect = 0.05) %>% tidy_rowwise(model, type = "summary")
    expect_gt(res$`Required Sample Size`, bal$`Required Sample Size`)
  }
})

test_that("two prop power: one-sided alternatives", {
  gt <- mtcars %>% exp_two_prop_power(baseline_prop = 0.30, diff_to_detect = 0.05, alternative = "greater") %>% tidy_rowwise(model, type = "summary")
  ref <- ceiling(power.prop.test(p1 = 0.30, p2 = 0.35, power = 0.8, alternative = "one.sided")$n)
  expect_equal(gt$`Required Sample Size (Group 1)`, ref)
  expect_equal(gt$`Achieved Power`, ref_prop_power(0.30, 0.35, ref, ref, 0.05, "greater"), tolerance = 1e-8)

  ls <- mtcars %>% exp_two_prop_power(baseline_prop = 0.30, diff_to_detect = -0.05, alternative = "less") %>% tidy_rowwise(model, type = "summary")
  ref <- ceiling(power.prop.test(p1 = 0.30, p2 = 0.25, power = 0.8, alternative = "one.sided")$n)
  expect_equal(ls$`Required Sample Size (Group 1)`, ref)
  expect_equal(ls$`Comparison Proportion`, 0.25)
  expect_equal(ls$`Achieved Power`, ref_prop_power(0.30, 0.25, ref, ref, 0.05, "less"), tolerance = 1e-8)
})

test_that("two prop power: two-sided accepts a negative difference", {
  res <- mtcars %>% exp_two_prop_power(baseline_prop = 0.30, diff_to_detect = -0.05) %>% tidy_rowwise(model, type = "summary")
  expect_equal(res$`Comparison Proportion`, 0.25)
  ref <- ceiling(power.prop.test(p1 = 0.30, p2 = 0.25, power = 0.8)$n)
  expect_equal(res$`Required Sample Size (Group 1)`, ref)
})

test_that("two prop power: invalid inputs give readable errors", {
  f <- function(...) exp_two_prop_power(mtcars, ...)
  expect_error(f(baseline_prop = 0, diff_to_detect = 0.05), "Baseline Proportion must be between 0 and 1")
  expect_error(f(baseline_prop = 1, diff_to_detect = 0.05), "Baseline Proportion must be between 0 and 1")
  expect_error(f(baseline_prop = 0.95, diff_to_detect = 0.05), "comparison proportion must be between 0 and 1")
  expect_error(f(baseline_prop = 0.03, diff_to_detect = -0.05), "comparison proportion must be between 0 and 1")
  expect_error(f(baseline_prop = 0.3, diff_to_detect = -0.05, alternative = "greater"), "Group 2 is greater than Group 1")
  expect_error(f(baseline_prop = 0.3, diff_to_detect = 0.05, alternative = "less"), "Group 2 is less than Group 1")
  expect_error(f(baseline_prop = 0.3, diff_to_detect = 0), "Min. Detectable Difference must not be 0")
  expect_error(f(baseline_prop = 0.3, diff_to_detect = 0.05, power = 1), "Desired Power must be between 0 and 1")
  expect_error(f(baseline_prop = 0.3, diff_to_detect = 0.05, power = 0), "Desired Power must be between 0 and 1")
  expect_error(f(baseline_prop = 0.3, diff_to_detect = 0.05, sig.level = 1), "Significance Level must be between 0 and 1")
  expect_error(f(baseline_prop = 0.3, diff_to_detect = 0.05, a_ratio = 0), "Group Size Ratio must be greater than 0 and less than 1")
  expect_error(f(baseline_prop = 0.3, diff_to_detect = 0.05, a_ratio = 1), "Group Size Ratio must be greater than 0 and less than 1")
  expect_error(f(diff_to_detect = 0.05), "Baseline Proportion is required")
  expect_error(f(baseline_prop = 0.3), "Min. Detectable Difference is required")
  expect_error(f(baseline_prop = NA_real_, diff_to_detect = 0.05), "Baseline Proportion is required")
})

test_that("two prop power: tidy n_to_power matches the independent power formula", {
  res <- mtcars %>% exp_two_prop_power(baseline_prop = 0.3, diff_to_detect = 0.05, a_ratio = 0.4, alternative = "greater", n_start = 100, n_end = 2000, n_step = 300) %>%
    tidy_rowwise(model, type = "n_to_power")
  expect_equal(colnames(res), c("n", "power"))
  expect_equal(res$n, seq(100, 2000, by = 300))
  ref <- ref_prop_power(0.3, 0.35, 0.4 * res$n, 0.6 * res$n, 0.05, "greater")
  expect_equal(res$power, ref, tolerance = 1e-8)
  expect_true(all(diff(res$power) > 0))
})

test_that("two prop power: density data", {
  for (alt in c("two.sided", "greater")) {
    model_df <- mtcars %>% exp_two_prop_power(baseline_prop = 0.3, diff_to_detect = 0.05, alternative = alt)
    res <- model_df %>% tidy_rowwise(model, type = "density")
    expect_equal(colnames(res), c("x", "y", "ncp", "type", "critical", "df", "statistic"))
    expect_equal(levels(res$type), c("Null", "Alternative"))
    # The critical reference line is at the z critical value.
    za <- if (alt == "two.sided") qnorm(0.975) else qnorm(0.95)
    expect_equal(res$x[which(res$statistic)], za, tolerance = 1e-8)
    # Area of the alternative density beyond the cutoff equals the achieved power (rectangle rule on a fine grid).
    s <- model_df %>% tidy_rowwise(model, type = "summary")
    alt_d <- res[res$type == "Alternative", ]
    dx <- diff(alt_d$x)[1]
    upper <- sum(alt_d$y[alt_d$x >= za]) * dx
    lower <- if (alt == "two.sided") sum(alt_d$y[alt_d$x <= -za]) * dx else 0
    expect_equal(upper + lower, s$`Achieved Power`, tolerance = 0.01)
  }
  # "less" works and has a negative critical value.
  res <- mtcars %>% exp_two_prop_power(baseline_prop = 0.3, diff_to_detect = -0.05, alternative = "less") %>% tidy_rowwise(model, type = "density")
  expect_equal(res$x[which(res$statistic)], -qnorm(0.95), tolerance = 1e-8)
})

test_that("two prop power: grouped data frame yields one model per group", {
  model_df <- mtcars %>% dplyr::group_by(am) %>% exp_two_prop_power(baseline_prop = 0.3, diff_to_detect = 0.05)
  expect_equal(nrow(model_df), 2)
  res <- model_df %>% tidy_rowwise(model, type = "summary")
  expect_equal(nrow(res), 2)
})

test_that("exp_two_prop_power group 2 size has no floating point noise for slider group size ratios (#36528)", {
  for (a in c(0.3, 0.35, 0.45, 0.6, 0.7, 0.85, 0.95)) {
    for (diff in c(0.03, 0.05, 0.1)) {
      res <- data.frame(x = 1) %>% exp_two_prop_power(a_ratio = a, baseline_prop = 0.3, diff_to_detect = diff) %>% tidy_rowwise(model, type = "summary")
      n1 <- res[["Required Sample Size (Group 1)"]]
      n2 <- res[["Required Sample Size (Group 2)"]]
      num <- round((1 - a) * 100); den <- round(a * 100)
      expect_equal(n2, (n1 * num + den - 1) %/% den, info = paste("a_ratio", a, "diff", diff))
    }
  }
})

test_that("exp_two_prop_power refuses a difference that needs more than the sample size limit (#36528)", {
  expect_error(data.frame(x = 1) %>% exp_two_prop_power(baseline_prop = 0.3, diff_to_detect = 1e-6), "Could not reach")
})

test_that("exp_two_prop_power sample size range stays whole numbers and keeps the critical z on the chart (#36528)", {
  res <- data.frame(x = 1) %>% exp_two_prop_power(baseline_prop = 0.3, diff_to_detect = 0.05, a_ratio = 0.15, n_start = 10, n_end = 100, n_step = 10) %>% tidy_rowwise(model, type = "n_to_power")
  expect_true(all(res$n == round(res$n)))
  expect_error(data.frame(x = 1) %>% exp_two_prop_power(baseline_prop = 0.3, diff_to_detect = 0.05, n_step = -5), "Step of the sample size range")
  dens <- data.frame(x = 1) %>% exp_two_prop_power(baseline_prop = 0.01, diff_to_detect = 0.98, sig.level = 0.001) %>% tidy_rowwise(model, type = "density")
  crit_row <- dens %>% dplyr::filter(!is.na(statistic) & statistic)
  expect_true(min(dens$x) <= -abs(crit_row$x[1]) && max(dens$x) >= abs(crit_row$x[1]))
})
