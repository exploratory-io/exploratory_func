# ANCOVA Calculation V2 Phase 2 diagnostics (tam#38389).
# how to run this test: devtools::test(filter="ancova_v2_diagnostics")
#
# The numbered cases below are the spec's own regression tests (sections
# 66-76). Each asserts the PROPERTY the spec states, not the shape of the
# output -- a diagnostic layer that returns well-formed tables describing the
# wrong model would pass a shape check.
context("ANCOVA V2 diagnostics (Phase 2), tam#38389")

make_diag_data <- function(n_per_group = 60, seed = 5,
                           group_levels = c("A", "B", "C"),
                           slope_offsets = NULL, x1_slope = 2, x2_slope = 0.5,
                           noise_sd = 3, low_card_x1 = FALSE) {
  set.seed(seed)
  k <- length(group_levels)
  n <- n_per_group * k
  group <- factor(rep(group_levels, each = n_per_group), levels = group_levels)
  x1 <- if (low_card_x1) sample(1:5, n, replace = TRUE) else runif(n, 0, 10)
  x2 <- rnorm(n, 50, 8)
  intercepts <- stats::setNames(seq(0, by = 5, length.out = k), group_levels)
  extra <- if (is.null(slope_offsets)) rep(0, n) else
    unname(slope_offsets[as.character(group)]) * x1
  y <- intercepts[as.character(group)] + x1_slope * x1 + x2_slope * x2 + extra +
    rnorm(n, 0, noise_sd)
  data.frame(y = as.numeric(y), group = group, X1 = as.numeric(x1), X2 = x2,
             stringsAsFactors = FALSE)
}

rel_for <- function(res, covariate) {
  for (r in res$diagnostics$relationships) if (identical(r$covariate, covariate)) return(r)
  NULL
}

# ------------------------------------------------------------
# Test 1 (section 66): one covariate, no interaction detected
# ------------------------------------------------------------
test_that("Test 1: single covariate, homogeneous slopes -- additive final model, interaction-based chart", {
  df <- make_diag_data()
  res <- run_ancova_v2(df, "y", "group", "X1")

  expect_equal(res$slope_homogeneity$status, "not_detected")
  expect_equal(res$model_selection$final_model_type, "additive")
  # The two models are deliberately different: the chart is drawn from the
  # interaction fit even though the additive one was selected (section 15).
  expect_equal(res$diagnostics$residuals$source_model, "additive")
  rel <- rel_for(res, "X1")
  expect_true(rel$available)
  expect_equal(rel$metadata$source_model, "interaction")
  # A single covariate has nothing to adjust FOR, so the adjustment is the
  # identity -- and it must fall out of the same code path, not a special case
  # (section 26).
  expect_equal(rel$points$adjusted_y, rel$points$raw_y, tolerance = 1e-10)
  expect_equal(rel$metadata$other_covariates_reference, "not_applicable")
})

test_that("the relationship lines are NOT parallel even when the additive model was selected", {
  # The load-bearing rule of the whole diagnostics layer (sections 15, 82): the
  # chart a reader uses to judge whether the slopes are parallel must come from
  # the model that let them differ. Drawn from the additive model the lines are
  # parallel BY CONSTRUCTION, so the chart would answer its own question yes no
  # matter what the data said -- and it would look completely normal.
  df <- make_diag_data(seed = 13)
  res <- run_ancova_v2(df, "y", "group", c("X1", "X2"))
  expect_equal(res$model_selection$final_model_type, "additive")

  lines <- rel_for(res, "X1")$lines
  slope_of <- function(lv) {
    g <- lines[lines$factor_level == lv, ]
    (g$predicted_y[[nrow(g)]] - g$predicted_y[[1]]) / (g$x[[nrow(g)]] - g$x[[1]])
  }
  slopes <- vapply(unique(lines$factor_level), slope_of, numeric(1))
  # Additive lines would agree to floating-point noise; interaction lines carry
  # each group's own estimate, which differs even when the difference is not
  # significant.
  expect_gt(diff(range(slopes)), 1e-6)

  # ... and they agree with what emtrends reported for those same groups.
  emtrend_slopes <- rel_for(res, "X1")$slopes
  for (lv in names(slopes)) {
    expect_equal(slopes[[lv]],
                 emtrend_slopes$slope[as.character(emtrend_slopes$group) == lv][[1]],
                 tolerance = 1e-6)
  }
})

# ------------------------------------------------------------
# Test 2 (section 67): three covariates, no interaction detected
# ------------------------------------------------------------
test_that("Test 2: three covariates produce three relationship sets, each holding the others at the mean", {
  df <- make_diag_data()
  df$X3 <- rnorm(nrow(df), 20, 4)
  res <- run_ancova_v2(df, "y", "group", c("X1", "X2", "X3"))

  expect_equal(length(res$diagnostics$relationships), 3)
  expect_equal(vapply(res$diagnostics$relationships, function(r) r$covariate, character(1)),
               c("X1", "X2", "X3"))
  for (r in res$diagnostics$relationships) {
    expect_true(r$available)
    expect_equal(r$metadata$source_model, "interaction")
    expect_equal(r$metadata$other_covariates_reference, "grand_mean")
    # The reference value is the covariate's own raw grand mean.
    expect_equal(r$reference_value, mean(df[[r$covariate]]), tolerance = 1e-10)
  }
  expect_equal(res$diagnostics$residuals$source_model, "additive")
})

# ------------------------------------------------------------
# Test 3 (section 68): one covariate interacts strongly
# ------------------------------------------------------------
test_that("Test 3: a strong interaction in ONE covariate keeps every covariate's chart available", {
  df <- make_diag_data(seed = 9, slope_offsets = c(A = 0, B = 6, C = -6))
  df$X3 <- rnorm(nrow(df), 20, 4)
  res <- run_ancova_v2(df, "y", "group", c("X1", "X2", "X3"))

  expect_equal(res$slope_homogeneity$status, "detected")
  expect_equal(res$model_selection$final_model_type, "interaction")
  # No term is dropped just because its own test was not significant
  # (section 68) -- all three charts stay available.
  expect_equal(length(res$diagnostics$relationships), 3)
  for (r in res$diagnostics$relationships) expect_true(r$available)
  expect_equal(res$diagnostics$residuals$source_model, "interaction")
  # X1 is the one built to interact, so its per-group slopes must differ.
  slopes <- rel_for(res, "X1")$slopes
  expect_equal(nrow(slopes), 3)
  expect_gt(diff(range(slopes$slope)), 5)
})

# ------------------------------------------------------------
# Test 4 (section 69): the adjustment removes the other covariate's variation
# ------------------------------------------------------------
test_that("Test 4: adjusted points strip the variation a second, dominant covariate contributes", {
  set.seed(21)
  n <- 150
  group <- factor(rep(c("A", "B", "C"), each = 50))
  x1 <- runif(n, 0, 10)
  x2 <- rnorm(n, 0, 1)
  # X2's contribution dwarfs X1's, so raw points scatter enormously around the
  # X1 relationship while adjusted points should not.
  df <- data.frame(y = x1 + 100 * x2 + rnorm(n, 0, 0.5), group = group, X1 = x1, X2 = x2)
  res <- run_ancova_v2(df, "y", "group", c("X1", "X2"))

  rel <- rel_for(res, "X1")
  expect_true(rel$available)
  expect_lt(stats::sd(rel$points$adjusted_y), stats::sd(rel$points$raw_y) / 10)
})

# ------------------------------------------------------------
# Test 5 (section 70): the adjusted-point identity, to 1e-8
# ------------------------------------------------------------
test_that("Test 5: adjusted_y equals the reference prediction plus the observed residual", {
  df <- make_diag_data(seed = 12)
  res <- run_ancova_v2(df, "y", "group", c("X1", "X2"), keep_internals = TRUE)
  rel <- rel_for(res, "X1")
  internals <- res$internals
  ad <- internals$analysis_data

  fitted_observed <- as.numeric(stats::predict(internals$model_interaction, newdata = ad))
  residual <- ad[[internals$safe_y]] - fitted_observed
  reference <- ad
  reference[[internals$safe_xc[[2]]]] <- 0
  fitted_reference <- as.numeric(stats::predict(internals$model_interaction, newdata = reference))
  expected <- (fitted_reference + residual)[rel$points$row_id]

  expect_equal(rel$points$adjusted_y, expected, tolerance = 1e-8)
})

# ------------------------------------------------------------
# Test 6 (section 71): low-cardinality jitter is display-only
# ------------------------------------------------------------
test_that("Test 6: a 1-5 covariate is jittered for display only", {
  df <- make_diag_data(seed = 15, low_card_x1 = TRUE)
  res <- run_ancova_v2(df, "y", "group", "X1")
  rel <- rel_for(res, "X1")

  expect_true(res$covariate_summary$low_cardinality[[1]])
  expect_true(rel$metadata$low_cardinality_jitter)
  # x is the real value a tooltip shows; display_x is the nudged one.
  expect_true(all(rel$points$x %in% 1:5))
  expect_false(all(rel$points$display_x %in% 1:5))
  expect_lt(max(abs(rel$points$display_x - rel$points$x)), 1)
  # The lines are never jittered -- they are model output, not display.
  expect_true(all(rel$lines$x >= 1 & rel$lines$x <= 5))
})

test_that("a covariate with many distinct values is NOT jittered", {
  df <- make_diag_data(seed = 15)
  rel <- rel_for(run_ancova_v2(df, "y", "group", "X1"), "X1")
  expect_false(rel$metadata$low_cardinality_jitter)
  expect_equal(rel$points$display_x, rel$points$x)
})

# ------------------------------------------------------------
# Test 7 (section 72): lines stop at each group's own range
# ------------------------------------------------------------
test_that("Test 7: each group's regression line spans only that group's observed covariate range", {
  set.seed(31)
  a <- data.frame(group = "A", X1 = runif(60, 0, 10))
  b <- data.frame(group = "B", X1 = runif(60, 5, 20))
  df <- rbind(a, b)
  df$group <- factor(df$group)
  df$y <- ifelse(df$group == "A", 0, 5) + 2 * df$X1 + rnorm(nrow(df), 0, 2)
  res <- run_ancova_v2(df, "y", "group", "X1")
  lines <- rel_for(res, "X1")$lines

  for (lv in c("A", "B")) {
    own <- df$X1[df$group == lv]
    drawn <- lines$x[lines$factor_level == lv]
    expect_equal(min(drawn), min(own), tolerance = 1e-8)
    expect_equal(max(drawn), max(own), tolerance = 1e-8)
    expect_equal(length(drawn), ANCOVA_RELATIONSHIP_GRID_SIZE)
  }
  # No extrapolation: B starts where B's data starts, not where A's does.
  expect_gt(min(lines$x[lines$factor_level == "B"]), min(lines$x[lines$factor_level == "A"]))
})

# ------------------------------------------------------------
# Test 8 (section 73): the residual model follows the global test
# ------------------------------------------------------------
test_that("Test 8: residual diagnostics always come from the FINAL model", {
  homogeneous <- run_ancova_v2(make_diag_data(seed = 4), "y", "group", "X1")
  expect_equal(homogeneous$slope_homogeneity$status, "not_detected")
  expect_equal(homogeneous$diagnostics$residuals$source_model, "additive")

  heterogeneous <- run_ancova_v2(
    make_diag_data(seed = 4, slope_offsets = c(A = 0, B = 7, C = -7)), "y", "group", "X1")
  expect_equal(heterogeneous$slope_homogeneity$status, "detected")
  expect_equal(heterogeneous$diagnostics$residuals$source_model, "interaction")

  # ... and the relationship chart is the interaction model in BOTH cases.
  expect_equal(rel_for(homogeneous, "X1")$metadata$source_model, "interaction")
  expect_equal(rel_for(heterogeneous, "X1")$metadata$source_model, "interaction")
})

test_that("residual diagnostics describe the final model's own residuals", {
  df <- make_diag_data(seed = 6)
  res <- run_ancova_v2(df, "y", "group", c("X1", "X2"), keep_internals = TRUE)
  expected <- as.numeric(stats::rstandard(res$internals$final_model))
  got <- res$diagnostics$residuals$residual_vs_fitted$points$standardized_residual
  expect_equal(sort(got), sort(expected), tolerance = 1e-10)
  expect_equal(res$diagnostics$residuals$residual_vs_fitted$reference_y, 0)
  expect_gt(nrow(res$diagnostics$residuals$residual_vs_fitted$smoother), 0)
})

# ------------------------------------------------------------
# Test 9 (section 74): Q-Q behaviour, with no normality verdict
# ------------------------------------------------------------
test_that("Test 9: near-normal residuals hug the Q-Q reference line and heavy tails leave it", {
  normal_res <- run_ancova_v2(make_diag_data(n_per_group = 200, seed = 8), "y", "group", "X1")
  qq <- normal_res$diagnostics$qq
  qq <- normal_res$diagnostics$residuals$qq
  fitted_line <- qq$reference_line$intercept + qq$reference_line$slope * qq$points$theoretical
  normal_gap <- max(abs(qq$points$observed - fitted_line))

  heavy <- make_diag_data(n_per_group = 200, seed = 8)
  set.seed(99)
  heavy$y <- heavy$y + stats::rt(nrow(heavy), df = 2) * 12
  heavy_qq <- run_ancova_v2(heavy, "y", "group", "X1")$diagnostics$residuals$qq
  heavy_line <- heavy_qq$reference_line$intercept +
    heavy_qq$reference_line$slope * heavy_qq$points$theoretical
  heavy_gap <- max(abs(heavy_qq$points$observed - heavy_line))

  expect_lt(normal_gap, 1)
  expect_gt(heavy_gap, normal_gap * 2)
  # No verdict either way -- the spec forbids one (section 44).
  expect_null(normal_res$diagnostics$residuals$normal)
  expect_null(normal_res$diagnostics$residuals$normality_status)
})

# ------------------------------------------------------------
# Test 10 (section 75): statistics on all rows, charts on a sample
# ------------------------------------------------------------
test_that("Test 10: a large N is fitted in full and only the scatter points are sampled", {
  set.seed(77)
  n <- 60000
  group <- factor(sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.02, 0.49, 0.49)))
  x1 <- runif(n, 0, 10)
  df <- data.frame(y = as.numeric(group) * 2 + 2 * x1 + rnorm(n, 0, 3), group = group, X1 = x1)
  res <- run_ancova_v2(df, "y", "group", "X1")

  expect_equal(res$analysis_sample$n_used, n)
  rel <- rel_for(res, "X1")
  expect_equal(rel$metadata$n_total, n)
  expect_true(rel$metadata$sampled)
  expect_lte(rel$metadata$n_displayed, ANCOVA_MAX_POINTS_PER_CHART)
  expect_equal(nrow(rel$points), rel$metadata$n_displayed)
  # The rare group survives the sample -- the reason the sampling is stratified.
  expect_true("A" %in% rel$points$factor_level)

  rvf <- res$diagnostics$residuals$residual_vs_fitted
  expect_true(rvf$sampled)
  expect_lte(nrow(rvf$points), ANCOVA_MAX_POINTS_PER_CHART)
  # The smoother is computed from every row, so it is longer than the sample.
  expect_gt(nrow(rvf$smoother), 0)

  qq <- res$diagnostics$residuals$qq
  expect_equal(qq$n_total, n)
  expect_lte(nrow(qq$points), ANCOVA_QQ_MAX_POINTS)
  # Order statistics, not a random draw: the extremes are still present.
  expect_equal(min(qq$points$observed), min(stats::rstandard(stats::lm(
    y ~ group + I(X1 - mean(X1)), data = df))), tolerance = 1e-8)
})

# ------------------------------------------------------------
# Sampling helper, in isolation
# ------------------------------------------------------------
test_that("factor-stratified sampling keeps every level and hits the target size", {
  f <- factor(c(rep("big", 9800), rep("small", 200)))
  idx <- sample_ancova_scatter_points(f, 1000)
  expect_equal(length(idx), 1000)
  kept <- table(f[idx])
  expect_gt(kept[["small"]], 0)
  # Proportional, so the rare level is represented rather than swamped.
  expect_gte(kept[["small"]], 15)
  expect_true(!is.unsorted(idx))
})

test_that("sampling is a no-op below the cap", {
  f <- factor(rep(c("a", "b"), each = 10))
  expect_equal(sample_ancova_scatter_points(f, 5000), seq_len(20))
})

test_that("the per-covariate budget splits, floors and caps", {
  expect_equal(ancova_points_per_covariate(1), ANCOVA_MAX_POINTS_PER_CHART)
  expect_equal(ancova_points_per_covariate(20),
               floor(ANCOVA_DIAGNOSTIC_POINT_BUDGET / 20))
  # A very wide covariate list would otherwise allocate a handful of points per
  # chart; the floor keeps each chart readable instead.
  expect_equal(ancova_points_per_covariate(500), ANCOVA_MIN_POINTS_PER_COVARIATE)
})

# ------------------------------------------------------------
# Availability and metadata (sections 57, 60, 64, 77, 79)
# ------------------------------------------------------------
test_that("an unestimable interaction model reports the chart as unavailable, never a parallel-line stand-in", {
  set.seed(41)
  df <- make_diag_data(seed = 41)
  # Constant within one group -> the interaction model is rank-deficient.
  df$X1[df$group == "B"] <- 5
  res <- run_ancova_v2(df, "y", "group", "X1")

  expect_equal(res$slope_homogeneity$status, "not_estimable")
  rel <- rel_for(res, "X1")
  expect_false(rel$available)
  expect_equal(rel$reason, "interaction_model_not_estimable")
  expect_equal(nrow(rel$lines), 0)
  # Residual diagnostics still work -- one dead chart must not cost the rest
  # (section 64).
  expect_true(res$diagnostics$residuals$available)
})

test_that("the Phase 3 handoff fields and metadata are present and consistent", {
  res <- run_ancova_v2(make_diag_data(seed = 17), "y", "group", c("X1", "X2"))
  d <- res$diagnostics

  expect_equal(d$diagnostics_version, 1)
  expect_equal(d$homogeneity_status, res$slope_homogeneity$status)
  expect_equal(d$standard_ancova_valid, res$model_selection$standard_ancova_valid)
  expect_equal(d$diagnostic_model_type, res$model_selection$final_model_type)
  expect_true(d$diagnostics_available)
  expect_true(is.list(d$interaction_detected_covariates))

  expect_equal(d$metadata$relationship_model, "full_interaction")
  expect_equal(d$metadata$residual_model, "final_model")
  expect_equal(d$metadata$relationship_reference, "other_covariates_at_grand_mean")
  expect_equal(d$metadata$relationship_points, "partial_adjusted_observations")
  expect_equal(d$metadata$residual_type, "standardized")
  expect_equal(d$metadata$qq_distribution, "normal")
  expect_equal(d$metadata$scatter_sampling, "factor_stratified")
  # Independence cannot be judged from the data alone, so it is recorded as
  # untested rather than reported as satisfied (section 4).
  expect_false(d$metadata$independence_tested)
})

test_that("confidence bands use 1 - alpha and are a mean-response interval", {
  df <- make_diag_data(seed = 23)
  wide <- run_ancova_v2(df, "y", "group", "X1", alpha = 0.01)
  narrow <- run_ancova_v2(df, "y", "group", "X1", alpha = 0.10)

  wl <- rel_for(wide, "X1")$lines
  nl <- rel_for(narrow, "X1")$lines
  expect_equal(unique(wl$confidence_level), 0.99)
  expect_equal(unique(nl$confidence_level), 0.90)
  expect_equal(wl$predicted_y, nl$predicted_y, tolerance = 1e-10)
  # A smaller alpha widens the band; the fitted line itself does not move.
  expect_true(all((wl$ci_upper - wl$ci_lower) > (nl$ci_upper - nl$ci_lower)))
  # Mean-response, not prediction: the band is far narrower than the residual
  # spread it would have to cover to hold individual observations.
  expect_lt(mean(wl$ci_upper - wl$ci_lower), 4 * stats::sd(df$y))
})

test_that("the diagnostics never appear on the error path", {
  bad <- run_ancova_v2(data.frame(y = 1:5, group = "A", X1 = 1:5), "y", "group", "X1")
  expect_equal(bad$analysis_status, "error")
  expect_null(bad$diagnostics)
})
