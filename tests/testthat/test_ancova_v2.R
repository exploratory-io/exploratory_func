# Test ANCOVA Calculation V2 (run_ancova_v2), tam#38385 Phase 1.
# how to run this test: devtools::test(filter="ancova_v2")
#
# IMPORTANT CAVEAT (see the PR description for the full note): these tests
# were written and statically reviewed WITHOUT access to a working R/Rscript
# in the authoring sandbox, so they have never actually been executed. Where
# a test's pass/fail depends on the specific realization of random noise
# (e.g. "the global interaction test comes out non-significant"), the
# generator uses a large sample size, a conservative alpha, and/or an
# overwhelming effect size specifically to make the outcome robust to the
# exact seed -- but this has NOT been empirically confirmed by running the
# suite. Tests built on pure linear-algebra or algebraic-invariant
# properties (rank deficiency, Type-II-SS order invariance, centering
# shift-invariance, the pairwise-difference identity, residual degrees of
# freedom) do not depend on the random draw and are the ones we are most
# confident in even unexecuted.
context("ANCOVA V2 (run_ancova_v2), tam#38385")

# ------------------------------------------------------------
# Shared data generators
# ------------------------------------------------------------

#' Synthetic ANCOVA data with P covariates. `group_slope_offsets` (a named
#' list: covariate name -> named vector of per-group ADDITIONAL slope, on
#' top of the shared baseline slope) controls whether the true
#' data-generating process has group-varying slopes (interaction) or not
#' (parallel slopes, the default).
make_ancova_data <- function(n_per_group = 60, seed = 1,
                              group_levels = c("A", "B", "C"),
                              covariate_names = c("X1", "X2"),
                              covariate_slopes = NULL,
                              group_slope_offsets = NULL,
                              group_intercepts = NULL,
                              noise_sd = 5) {
  set.seed(seed)
  k <- length(group_levels)
  p <- length(covariate_names)
  if (is.null(covariate_slopes)) covariate_slopes <- rep(1, p)
  if (is.null(group_intercepts)) {
    group_intercepts <- stats::setNames(seq(0, by = 5, length.out = k), group_levels)
  }
  rows <- lapply(group_levels, function(g) {
    covs <- lapply(seq_len(p), function(j) stats::rnorm(n_per_group, mean = 10 * j, sd = 3))
    names(covs) <- covariate_names
    y <- rep(group_intercepts[[g]], n_per_group)
    for (j in seq_len(p)) {
      slope_j <- covariate_slopes[j]
      cov_name <- covariate_names[j]
      if (!is.null(group_slope_offsets) && !is.null(group_slope_offsets[[cov_name]])) {
        slope_j <- slope_j + group_slope_offsets[[cov_name]][[g]]
      }
      y <- y + slope_j * covs[[j]]
    }
    y <- y + stats::rnorm(n_per_group, sd = noise_sd)
    out <- as.data.frame(covs)
    out$group <- g
    out$y <- y
    out
  })
  df <- do.call(rbind, rows)
  df$group <- factor(df$group, levels = group_levels)
  df
}

# ------------------------------------------------------------
# Structural smoke test
# ------------------------------------------------------------

test_that("run_ancova_v2 returns the full result shape on a basic 2-covariate case", {
  df <- make_ancova_data(n_per_group = 40, seed = 1)
  res <- run_ancova_v2(df, outcome = "y", factor = "group", covariates = c("X1", "X2"), alpha = 0.05)

  expect_equal(res$analysis_status, "ok")
  expect_equal(res$calculation_version, 2)
  expect_equal(res$variables$outcome, "y")
  expect_equal(res$variables$factor, "group")
  expect_equal(unlist(res$variables$covariates), c("X1", "X2"))
  expect_equal(res$analysis_sample$n_used, nrow(df))
  expect_equal(res$analysis_sample$n_removed, 0)
  expect_equal(unlist(res$analysis_sample$factor_levels), c("A", "B", "C"))
  expect_equal(nrow(res$covariate_summary), 2)
  expect_true(all(res$covariate_summary$centered_reference_value == 0))
  # reference_value is the SAME reference point on the covariate's raw scale
  # (tam#38389 Q-7): charts drawing a grand-mean line on a raw x axis read it.
  expect_equal(res$covariate_summary$reference_value, res$covariate_summary$mean)
  expect_equal(res$covariate_summary$reference_value[[1]],
               mean(df$X1), tolerance = 1e-10)
  expect_equal(res$covariate_summary$reference_value[[2]],
               mean(df$X2), tolerance = 1e-10)
  # ... and it must agree with the reference point the adjusted means were
  # actually computed at, so a chart line and the reported means cannot drift.
  expect_equal(unname(unlist(res$adjusted_means$reference_covariates)),
               res$covariate_summary$reference_value, tolerance = 1e-10)
  expect_true(is.list(res$slope_homogeneity))
  expect_true(is.list(res$model_selection))
  expect_equal(res$metadata$alpha, 0.05)
  expect_equal(res$metadata$confidence_level, 0.95)
  expect_equal(res$metadata$ss_type, "II")
  expect_equal(res$metadata$pairwise_adjustment, "tukey")
  expect_equal(res$metadata$individual_interaction_p_adjustment, "holm")
  expect_true(nrow(res$raw_statistics) == 3)
  expect_equal(sum(res$raw_statistics$n), res$analysis_sample$n_used)
})

# ------------------------------------------------------------
# Test A: all covariates parallel -> global test non-significant, additive
# ------------------------------------------------------------

test_that("Test A: parallel slopes across all covariates -> homogeneous, final_model = additive", {
  # Large n + conservative alpha to make the null-case false-positive rate
  # tiny (see file header caveat: not empirically confirmed by execution).
  df <- make_ancova_data(n_per_group = 150, seed = 20260904,
                          covariate_names = c("X1", "X2", "X3"),
                          group_slope_offsets = NULL, noise_sd = 8)
  res <- run_ancova_v2(df, outcome = "y", factor = "group",
                        covariates = c("X1", "X2", "X3"), alpha = 0.001)

  expect_equal(res$analysis_status, "ok")
  expect_true(res$slope_homogeneity$estimable)
  expect_false(isTRUE(res$slope_homogeneity$global_test$significant))
  expect_equal(res$slope_homogeneity$status, "not_detected")
  expect_equal(res$model_selection$final_model_type, "additive")
  expect_true(res$model_selection$standard_ancova_valid)
  expect_true(!is.null(res$ancova_table))
  expect_true(!is.null(res$adjusted_means))
  expect_true(!is.null(res$pairwise_comparisons))
  expect_null(res$conditional_means_at_reference)
  expect_null(res$conditional_pairwise_at_reference)
  expect_equal(length(res$interaction_details), 0)
})

# ------------------------------------------------------------
# Test B: exactly one covariate has a group-varying slope
# ------------------------------------------------------------

test_that("Test B: one covariate group-varying slope -> interaction final model, not auto-dropped (Rule 7)", {
  df <- make_ancova_data(n_per_group = 100, seed = 7,
                          covariate_names = c("X1", "X2"),
                          group_slope_offsets = list(X1 = c(A = 0, B = 12, C = -12)),
                          noise_sd = 4)
  res <- run_ancova_v2(df, outcome = "y", factor = "group",
                        covariates = c("X1", "X2"), alpha = 0.05)

  expect_equal(res$analysis_status, "ok")
  expect_true(res$slope_homogeneity$estimable)
  expect_true(isTRUE(res$slope_homogeneity$global_test$significant))
  expect_equal(res$model_selection$final_model_type, "interaction")
  expect_false(res$model_selection$standard_ancova_valid)
  expect_null(res$ancova_table)
  expect_null(res$adjusted_means)
  expect_null(res$pairwise_comparisons)
  expect_true(!is.null(res$conditional_means_at_reference))
  expect_true(!is.null(res$conditional_pairwise_at_reference))

  # Rule 7: BOTH covariates must still appear -- no stepwise auto-drop.
  expect_equal(nrow(res$slope_homogeneity$covariate_tests), 2)
  expect_setequal(res$slope_homogeneity$covariate_tests$covariate, c("X1", "X2"))
  expect_equal(length(res$interaction_details), 2)
  interaction_covs <- vapply(res$interaction_details, function(x) x$covariate, character(1))
  expect_setequal(interaction_covs, c("X1", "X2"))

  # X1's diagnostic interaction test should be flagged significant.
  x1_row <- res$slope_homogeneity$covariate_tests[res$slope_homogeneity$covariate_tests$covariate == "X1", ]
  expect_true(isTRUE(x1_row$significant_raw))

  # Holm adjustment is always >= the raw p-value (monotonicity property,
  # true regardless of the specific data draw).
  expect_true(all(res$slope_homogeneity$covariate_tests$p_holm >=
                  res$slope_homogeneity$covariate_tests$p_raw - 1e-10))
})

# ------------------------------------------------------------
# Test D: model selection is driven ONLY by the global test (Rule 6/7)
# ------------------------------------------------------------

test_that("Test D (unit-level): select_ancova_model() ignores per-covariate results entirely", {
  # select_ancova_model()'s signature does not even accept covariate-level
  # test results -- this is an architectural guarantee of Rule 7, not just a
  # behavioral one. Confirm the decision follows the GLOBAL test alone.
  homogeneity_significant <- list(estimable = TRUE, significant = TRUE)
  homogeneity_not_significant <- list(estimable = TRUE, significant = FALSE)
  homogeneity_not_estimable <- list(estimable = FALSE)

  sel1 <- select_ancova_model(homogeneity_significant)
  expect_equal(sel1$final_model_type, "interaction")
  expect_false(sel1$standard_ancova_valid)

  sel2 <- select_ancova_model(homogeneity_not_significant)
  expect_equal(sel2$final_model_type, "additive")
  expect_true(sel2$standard_ancova_valid)

  sel3 <- select_ancova_model(homogeneity_not_estimable)
  expect_equal(sel3$final_model_type, "additive")
  expect_false(sel3$standard_ancova_valid)
  expect_equal(sel3$status, "not_estimable")
})

test_that("Test D (pipeline-level): jointly-significant interactions across many weak covariates drive selection", {
  # Several covariates each with a modest per-group slope offset -- the
  # GLOBAL test pools evidence across all of them. Full-pipeline numeric
  # outcome is UNVERIFIED (no live R in the authoring sandbox); the
  # unit-level test above is the one that pins Rule 7 deterministically.
  offsets <- list(
    X1 = c(A = 0, B = 3, C = -3),
    X2 = c(A = 0, B = -3, C = 3),
    X3 = c(A = 0, B = 3, C = 3),
    X4 = c(A = 0, B = -3, C = -3)
  )
  df <- make_ancova_data(n_per_group = 120, seed = 99,
                          covariate_names = c("X1", "X2", "X3", "X4"),
                          group_slope_offsets = offsets, noise_sd = 3)
  res <- run_ancova_v2(df, outcome = "y", factor = "group",
                        covariates = c("X1", "X2", "X3", "X4"), alpha = 0.05)
  expect_equal(res$analysis_status, "ok")
  # Regardless of significance outcome, every covariate must be present in
  # the diagnostics (Rule 7 -- no shrinkage).
  expect_equal(nrow(res$slope_homogeneity$covariate_tests), 4)
  expect_setequal(res$slope_homogeneity$covariate_tests$covariate, c("X1", "X2", "X3", "X4"))
  expect_equal(length(res$interaction_details) == 0, res$model_selection$final_model_type == "additive")
  # The final_model_type must be exactly consistent with the GLOBAL test's
  # significance flag -- this is what Rule 6/7 requires structurally,
  # regardless of what any single covariate's own p-value looks like.
  expected_type <- if (!res$slope_homogeneity$estimable) {
    "additive"
  } else if (isTRUE(res$slope_homogeneity$global_test$significant)) {
    "interaction"
  } else {
    "additive"
  }
  expect_equal(res$model_selection$final_model_type, expected_type)
})

# ------------------------------------------------------------
# Test E: covariate order invariance (Type II SS is order-independent for
# main effects; RSS of the full interaction model doesn't depend on term
# order either)
# ------------------------------------------------------------

test_that("Test E: reordering covariates leaves Factor/Covariate stats, adjusted means, pairwise, and the global test unchanged", {
  df <- make_ancova_data(n_per_group = 70, seed = 11, covariate_names = c("X1", "X2"))
  res1 <- run_ancova_v2(df, outcome = "y", factor = "group", covariates = c("X1", "X2"), alpha = 0.05)
  res2 <- run_ancova_v2(df, outcome = "y", factor = "group", covariates = c("X2", "X1"), alpha = 0.05)

  expect_equal(res1$analysis_status, "ok")
  expect_equal(res2$analysis_status, "ok")

  expect_equal(res1$slope_homogeneity$global_test$F, res2$slope_homogeneity$global_test$F, tolerance = 1e-8)
  expect_equal(res1$slope_homogeneity$global_test$p_value, res2$slope_homogeneity$global_test$p_value, tolerance = 1e-8)

  if (res1$model_selection$final_model_type == "additive") {
    t1 <- res1$ancova_table$terms[order(res1$ancova_table$terms$term_name), ]
    t2 <- res2$ancova_table$terms[order(res2$ancova_table$terms$term_name), ]
    expect_equal(t1$sum_squares, t2$sum_squares, tolerance = 1e-8)
    expect_equal(t1$F, t2$F, tolerance = 1e-8)
    expect_equal(t1$p_value, t2$p_value, tolerance = 1e-8)
    expect_equal(t1$partial_eta_squared, t2$partial_eta_squared, tolerance = 1e-8)

    am1 <- res1$adjusted_means$means[order(res1$adjusted_means$means$group), ]
    am2 <- res2$adjusted_means$means[order(res2$adjusted_means$means$group), ]
    expect_equal(am1$estimate, am2$estimate, tolerance = 1e-8)

    pw1 <- res1$pairwise_comparisons[order(res1$pairwise_comparisons$group1, res1$pairwise_comparisons$group2), ]
    pw2 <- res2$pairwise_comparisons[order(res2$pairwise_comparisons$group1, res2$pairwise_comparisons$group2), ]
    expect_equal(pw1$adjusted_difference, pw2$adjusted_difference, tolerance = 1e-8)
  }
})

# ------------------------------------------------------------
# Test F: shifting a covariate by a constant leaves centered results
# unchanged (mean-centering absorbs any additive shift into the intercept
# only -- an exact algebraic identity, not sensitive to the random draw)
# ------------------------------------------------------------

test_that("Test F: shifting covariate values by a constant does not change centered results", {
  df <- make_ancova_data(n_per_group = 70, seed = 12, covariate_names = c("X1", "X2"))
  df_shifted <- df
  df_shifted$X1 <- df_shifted$X1 + 100
  df_shifted$X2 <- df_shifted$X2 - 50

  res1 <- run_ancova_v2(df, outcome = "y", factor = "group", covariates = c("X1", "X2"), alpha = 0.05)
  res2 <- run_ancova_v2(df_shifted, outcome = "y", factor = "group", covariates = c("X1", "X2"), alpha = 0.05)

  expect_equal(res1$slope_homogeneity$global_test$F, res2$slope_homogeneity$global_test$F, tolerance = 1e-6)
  expect_equal(res1$slope_homogeneity$global_test$p_value, res2$slope_homogeneity$global_test$p_value, tolerance = 1e-6)

  if (res1$model_selection$final_model_type == "additive") {
    expect_equal(res1$ancova_table$terms$sum_squares, res2$ancova_table$terms$sum_squares, tolerance = 1e-6)
    expect_equal(res1$adjusted_means$means$estimate, res2$adjusted_means$means$estimate, tolerance = 1e-6)
    expect_equal(res1$pairwise_comparisons$adjusted_difference,
                 res2$pairwise_comparisons$adjusted_difference, tolerance = 1e-6)
  }
  # Raw-scale covariate means shift by exactly the constant applied.
  expect_equal(res2$covariate_summary$mean[res2$covariate_summary$name == "X1"] -
               res1$covariate_summary$mean[res1$covariate_summary$name == "X1"], 100, tolerance = 1e-8)
})

# ------------------------------------------------------------
# Test H: perfect covariate-covariate collinearity -> additive rank-deficient
# ------------------------------------------------------------

test_that("Test H: perfect collinearity (X2 = 2 * X1) -> analysis_status = error, ANCOVA_RANK_DEFICIENT", {
  df <- make_ancova_data(n_per_group = 40, seed = 5, covariate_names = c("X1"))
  df$X2 <- df$X1 * 2

  res <- run_ancova_v2(df, outcome = "y", factor = "group", covariates = c("X1", "X2"), alpha = 0.05)
  expect_equal(res$analysis_status, "error")
  expect_equal(res$error_code, "ANCOVA_RANK_DEFICIENT")
})

# ------------------------------------------------------------
# Test I: a covariate constant WITHIN one group (not overall) -> additive
# estimable, interaction NOT estimable ("not_estimable", never conflated
# with "non-significant"). See file-level design notes in R/ancova_v2.R
# for why a covariate constant within EVERY group would make the ADDITIVE
# model rank-deficient too (exact linear-algebra argument) -- this
# construction deliberately keeps genuine within-group variation in all
# groups except one.
# ------------------------------------------------------------

test_that("Test I: covariate constant within exactly one group -> not_estimable, not conflated with non-significant", {
  set.seed(31)
  n_per_group <- 40
  levels_ <- c("A", "B", "C")
  df <- do.call(rbind, lapply(levels_, function(g) {
    x1 <- if (g == "C") rep(50, n_per_group) else stats::rnorm(n_per_group, mean = 20, sd = 4)
    y <- 10 + 2 * x1 + stats::rnorm(n_per_group, sd = 3)
    data.frame(group = g, X1 = x1, y = y)
  }))
  df$group <- factor(df$group, levels = levels_)

  res <- run_ancova_v2(df, outcome = "y", factor = "group", covariates = c("X1"), alpha = 0.05)

  expect_equal(res$analysis_status, "ok")
  expect_false(res$slope_homogeneity$estimable)
  expect_equal(res$slope_homogeneity$status, "not_estimable")
  expect_true(is.na(res$slope_homogeneity$global_test$significant))
  expect_equal(res$model_selection$final_model_type, "additive")
  expect_false(res$model_selection$standard_ancova_valid)
  # Kept: additive model's results, not treated as "non-significant".
  expect_true(!is.null(res$ancova_table))
  expect_true(!is.null(res$adjusted_means))
  expect_true(any(grepl("could not be estimated", unlist(res$warnings), fixed = TRUE)))
})

# ------------------------------------------------------------
# Test J: different NA patterns per covariate -> one shared complete-case
# sample for every downstream computation
# ------------------------------------------------------------

test_that("Test J: different NA patterns per covariate all resolve to the same complete-case sample", {
  df <- make_ancova_data(n_per_group = 50, seed = 44, covariate_names = c("X1", "X2"))
  df$X1[1:5] <- NA
  df$X2[10:17] <- NA
  df$y[20:21] <- NA
  df$group[30] <- NA

  # Independent oracle computed directly in the test, not via run_ancova_v2.
  expected_n_used <- sum(stats::complete.cases(df[, c("y", "group", "X1", "X2")]))

  res <- run_ancova_v2(df, outcome = "y", factor = "group", covariates = c("X1", "X2"), alpha = 0.05)
  expect_equal(res$analysis_status, "ok")
  expect_equal(res$analysis_sample$n_used, expected_n_used)
  expect_equal(res$analysis_sample$n_original, nrow(df))
  expect_equal(res$analysis_sample$n_removed, nrow(df) - expected_n_used)
  expect_equal(sum(res$raw_statistics$n), expected_n_used)
})

# ------------------------------------------------------------
# Test K: adjusted means at reference == direct prediction at each
# covariate's raw-scale mean (independent lm()+predict() oracle)
# ------------------------------------------------------------

test_that("Test K: adjusted means equal a direct prediction at the covariates' raw-scale means", {
  df <- make_ancova_data(n_per_group = 60, seed = 61, covariate_names = c("X1", "X2"))
  res <- run_ancova_v2(df, outcome = "y", factor = "group", covariates = c("X1", "X2"), alpha = 0.05)
  skip_if(res$model_selection$final_model_type != "additive",
          "final model was interaction for this seed; Test K only applies to the additive case")

  direct_model <- stats::lm(y ~ group + X1 + X2, data = df)
  ref <- data.frame(group = factor(levels(df$group), levels = levels(df$group)),
                     X1 = mean(df$X1), X2 = mean(df$X2))
  direct_pred <- stats::predict(direct_model, newdata = ref)
  names(direct_pred) <- levels(df$group)

  means <- res$adjusted_means$means
  for (g in levels(df$group)) {
    expect_equal(means$estimate[means$group == g], unname(direct_pred[[g]]), tolerance = 1e-6)
  }
})

# ------------------------------------------------------------
# Test L: pairwise adjusted_difference == EMM(g1) - EMM(g2), within 1e-8
# ------------------------------------------------------------

test_that("Test L: pairwise adjusted_difference matches the EMM difference within 1e-8", {
  df <- make_ancova_data(n_per_group = 60, seed = 61, covariate_names = c("X1", "X2"))
  res <- run_ancova_v2(df, outcome = "y", factor = "group", covariates = c("X1", "X2"), alpha = 0.05)
  skip_if(res$model_selection$final_model_type != "additive",
          "final model was interaction for this seed")

  means <- res$adjusted_means$means
  emm_by_group <- stats::setNames(means$estimate, means$group)

  pw <- res$pairwise_comparisons
  for (i in seq_len(nrow(pw))) {
    expected_diff <- emm_by_group[[pw$group1[i]]] - emm_by_group[[pw$group2[i]]]
    expect_equal(pw$adjusted_difference[i], expected_diff, tolerance = 1e-8)
  }
})

# ------------------------------------------------------------
# Golden / snapshot tests: no pre-existing HR-attrition-style fixture was
# found anywhere in this repo (grepped for MonthlyIncome/JobLevel/
# Department/Attrition across tests/, R/, data/, data-raw/) -- these are
# freshly-built synthetic equivalents with the same shape (1 factor with 3
# levels = df 2, and either 1 or 3 numeric covariates).
# ------------------------------------------------------------

make_hr_like_data <- function(n_per_group = 80, seed = 2026, n_covariates = 1) {
  covariate_names <- paste0("JobLevel", seq_len(n_covariates))
  make_ancova_data(n_per_group = n_per_group, seed = seed,
                    group_levels = c("Sales", "R&D", "HR"),
                    covariate_names = covariate_names,
                    noise_sd = 6)
}

test_that("Golden test: 1 factor (df=2) + 1 covariate (df=1) -> additive residual df = N - 4", {
  df <- make_hr_like_data(n_per_group = 80, n_covariates = 1)
  names(df)[names(df) == "y"] <- "MonthlyIncome"
  names(df)[names(df) == "group"] <- "Department"
  res <- run_ancova_v2(df, outcome = "MonthlyIncome", factor = "Department",
                        covariates = "JobLevel1", alpha = 0.05)
  expect_equal(res$analysis_status, "ok")
  n_used <- res$analysis_sample$n_used
  if (res$model_selection$final_model_type == "additive") {
    # 1 intercept + 2 factor dummies (3 levels) + 1 covariate = 4 parameters.
    expect_equal(res$ancova_table$residual$df, n_used - 4)
  }
})

test_that("Golden test: 1 factor (df=2) + 3 covariates -> N, F/P, effect sizes, means, pairwise", {
  df <- make_hr_like_data(n_per_group = 90, n_covariates = 3)
  names(df)[names(df) == "y"] <- "MonthlyIncome"
  names(df)[names(df) == "group"] <- "Department"
  res <- run_ancova_v2(df, outcome = "MonthlyIncome", factor = "Department",
                        covariates = c("JobLevel1", "JobLevel2", "JobLevel3"), alpha = 0.05)

  expect_equal(res$analysis_status, "ok")
  expect_equal(res$analysis_sample$n_used, nrow(df))
  expect_true(is.numeric(res$slope_homogeneity$global_test$F))
  expect_true(is.numeric(res$slope_homogeneity$global_test$p_value))
  expect_equal(nrow(res$slope_homogeneity$covariate_tests), 3)

  if (res$model_selection$final_model_type == "additive") {
    # 1 intercept + 2 factor dummies + 3 covariates = 6 parameters.
    expect_equal(res$ancova_table$residual$df, nrow(df) - 6)
    expect_equal(nrow(res$ancova_table$terms), 4) # Factor + 3 covariates
    expect_true(all(res$ancova_table$terms$partial_eta_squared >= 0 &
                    res$ancova_table$terms$partial_eta_squared <= 1))
    expect_equal(nrow(res$adjusted_means$means), 3)
    expect_equal(nrow(res$pairwise_comparisons), 3) # C(3,2) pairs
  }
})

# ------------------------------------------------------------
# Stress-test name: complex (spaces / multibyte / symbol) column names for
# outcome, factor, AND covariate, per r-coding-best-practices /
# workflow.md Rule 2/7. Confirms formulas are built via reformulate() on
# SAFE internal names only, never by pasting these raw names.
# ------------------------------------------------------------

test_that("Stress-test names: complex outcome/factor/covariate names never touch the formula string", {
  stress_outcome <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  stress_factor <- "グループ !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 区分"
  stress_covariate <- "共変量 X !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 値"

  set.seed(71)
  n_per_group <- 40
  groups <- c("G1", "G2", "G3")
  df <- do.call(rbind, lapply(groups, function(g) {
    x <- stats::rnorm(n_per_group, mean = 10, sd = 3)
    y <- 5 + 2 * x + stats::rnorm(n_per_group, sd = 2)
    data.frame(group = g, x = x, y = y)
  }))
  df$group <- factor(df$group, levels = groups)
  df <- stats::setNames(df, c(stress_factor, stress_covariate, stress_outcome))

  res <- run_ancova_v2(df, outcome = stress_outcome, factor = stress_factor,
                        covariates = stress_covariate, alpha = 0.05)

  expect_equal(res$analysis_status, "ok")
  expect_equal(res$variables$outcome, stress_outcome)
  expect_equal(res$variables$factor, stress_factor)
  expect_equal(unlist(res$variables$covariates), stress_covariate)
  expect_equal(res$covariate_summary$name[[1]], stress_covariate)
  if (res$model_selection$final_model_type == "additive") {
    covariate_term <- res$ancova_table$terms[res$ancova_table$terms$term_type == "covariate", ]
    expect_equal(covariate_term$term_name, stress_covariate)
  }
})

test_that("Guard: R/ancova_v2.R never builds a formula by pasting a raw string (reformulate()-only construction)", {
  src_path <- file.path("..", "..", "R", "ancova_v2.R")
  if (!file.exists(src_path)) {
    src_path <- "R/ancova_v2.R"
  }
  skip_if_not(file.exists(src_path), "R/ancova_v2.R not found relative to the test working directory")
  src <- readLines(src_path, warn = FALSE)
  src <- paste(src, collapse = "\n")
  # No as.formula(paste(...)) / paste0(...'~'...) pattern anywhere in the file.
  expect_false(grepl("as\\.formula\\s*\\(\\s*paste", src))
  expect_true(grepl("reformulate", src, fixed = TRUE))
})
