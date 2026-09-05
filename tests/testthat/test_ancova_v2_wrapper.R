# exp_ancova() / tidy.ancova_v2_exploratory() -- the Analytics View entry point
# for ANCOVA Calculation V2 (tam#38389 Phase 1.5).
#
# The load-bearing property here is COLUMN-NAME PARITY with the V1
# tidy.anova_exploratory() surfaces: exp_ancova.json's chart preprocessors, the
# report's bind variables and the harness's approved answers are all written
# against those names, so a rename is indistinguishable from a broken chart.
# Several tests therefore run BOTH engines on the same data and diff the
# column names rather than asserting a hardcoded list, so they keep pinning the
# real contract if V1 ever moves.
context("ANCOVA V2 wrapper (exp_ancova), tam#38389")

make_wrapper_data <- function(n_per_group = 40, seed = 7,
                              group_levels = c("A", "B", "C"),
                              with_na = FALSE) {
  set.seed(seed)
  k <- length(group_levels)
  n <- n_per_group * k
  group <- factor(rep(group_levels, each = n_per_group), levels = group_levels)
  x1 <- runif(n, 0, 10)
  x2 <- rnorm(n, 50, 8)
  intercepts <- stats::setNames(seq(0, by = 6, length.out = k), group_levels)
  y <- intercepts[as.character(group)] + 2 * x1 + 0.5 * x2 + rnorm(n, 0, 3)
  df <- data.frame(y = as.numeric(y), group = group, X1 = x1, X2 = x2,
                   stringsAsFactors = FALSE)
  if (with_na) {
    df$X1[c(3, 11, 29)] <- NA_real_
  }
  df
}

fit_v2 <- function(df, covariates = c("X1", "X2"), ...) {
  exp_ancova(df, "y", "group", covariates = covariates, ...)
}

fit_v1 <- function(df, covariates = c("X1", "X2"), with_interaction = FALSE) {
  do.call(exp_anova, list(df, "y", "group", covariates = covariates,
                          with_interaction = with_interaction))
}

test_that("exp_ancova returns the rowwise model-data-frame shape tidy_rowwise requires", {
  df <- make_wrapper_data()
  ret <- fit_v2(df)

  expect_true("model" %in% colnames(ret))
  expect_equal(nrow(ret), 1)
  expect_true(is.list(ret$model))
  expect_true("ancova_v2_exploratory" %in% class(ret$model[[1]]))
  # tidy_rowwise() ungroups and map()s over the column, so this is the call the
  # chart preprocessors actually make.
  tidied <- tidy_rowwise(ret, model, type = "model")
  expect_true(nrow(tidied) > 0)
})

test_that("exp_ancova groups like exp_anova does", {
  df <- make_wrapper_data()
  df$region <- rep(c("east", "west"), length.out = nrow(df))
  ret <- df %>% dplyr::group_by(region) %>% fit_v2()

  expect_true(all(c("region", "model") %in% colnames(ret)))
  expect_equal(nrow(ret), 2)
  tidied <- tidy_rowwise(ret, model, type = "model")
  expect_true("region" %in% colnames(tidied))
  expect_equal(dplyr::n_distinct(tidied$region), 2)
})

test_that("type='model' matches V1's ANCOVA table columns", {
  df <- make_wrapper_data()
  v1 <- tidy_rowwise(fit_v1(df), model, type = "model")
  v2 <- tidy_rowwise(fit_v2(df), model, type = "model")

  expect_equal(colnames(v2), colnames(v1))
  # Same row vocabulary: one row per model term plus the two summary rows.
  expect_true(all(c("group", "X1", "X2", "(Residuals)", "(Total)") %in% v2$Variable))
  factor_row <- v2 %>% dplyr::filter(Variable == "group")
  expect_equal(nrow(factor_row), 1)
  expect_true(is.finite(factor_row$`F Value`[[1]]))
  expect_true(factor_row$`P Value`[[1]] >= 0 && factor_row$`P Value`[[1]] <= 1)
  # SS Ratio is a share of the corrected total, so the (Total) row is exactly 1.
  expect_equal((v2 %>% dplyr::filter(Variable == "(Total)"))$`SS Ratio`[[1]], 1)
  # Effect sizes are blank on the summary rows, as in V1.
  expect_true(is.na((v2 %>% dplyr::filter(Variable == "(Residuals)"))$`Eta Squared`[[1]]))
})

test_that("type='emmeans' matches V1's column set and describes the complete-case rows", {
  df <- make_wrapper_data(with_na = TRUE)
  v1 <- tidy_rowwise(fit_v1(df), model, type = "emmeans", sort_factor_levels = TRUE)
  v2 <- tidy_rowwise(fit_v2(df), model, type = "emmeans", sort_factor_levels = TRUE)

  expect_setequal(colnames(v2), colnames(v1))
  expect_equal(colnames(v2)[[1]], "group")
  expect_true(all(c("Mean (Adj)", "Std Error (Adj)", "Conf Low (Adj)",
                    "Conf High (Adj)", "DF", "Rows", "Mean", "Std Deviation",
                    "Std Error", "Conf Low", "Conf High", "Minimum", "Maximum")
                  %in% colnames(v2)))
  # The unadjusted Rows must count the SAME rows the adjusted columns were
  # computed from -- the whole point of V2's single analysis_data (tam#38216).
  complete_n <- sum(stats::complete.cases(df[, c("y", "group", "X1", "X2")]))
  expect_equal(sum(v2$Rows), complete_n)
  # The covariate columns carry the reference point the means were adjusted to.
  expect_true(all(c("X1", "X2") %in% colnames(v2)))
  expect_equal(unique(v2$X1), mean(df$X1[stats::complete.cases(df)], na.rm = TRUE),
               tolerance = 1e-8)
})

test_that("type='pairs' matches V1's columns and honors pairs_adjust", {
  df <- make_wrapper_data()
  v1 <- tidy_rowwise(fit_v1(df), model, type = "pairs", pairs_adjust = "tukey")
  v2 <- tidy_rowwise(fit_v2(df), model, type = "pairs", pairs_adjust = "tukey")

  expect_equal(colnames(v2), colnames(v1))
  expect_equal(nrow(v2), 3) # 3 groups -> 3 pairs
  expect_equal(unique(v2$Method), "Tukey's HSD Test")
  expect_true(all(v2$`Group 1` %in% levels(df$group)))
  expect_true(all(v2$`Group 2` %in% levels(df$group)))

  # The adjustment is a user setting, so it must actually change the P values
  # rather than always reporting the Tukey ones run_ancova_v2 stored.
  none <- tidy_rowwise(fit_v2(df), model, type = "pairs", pairs_adjust = "none")
  expect_equal(unique(none$Method), "Pairwise T-Test with No Adjustment")
  expect_true(all(none$`P Value` <= v2$`P Value` + 1e-12))
  # Compare as a RATIO, not with all.equal(): once values are smaller than
  # all.equal()'s own tolerance it switches to an absolute comparison, and
  # p-values this small then read as "equal" however far apart they are.
  expect_true(any(v2$`P Value` > none$`P Value` * 1.5))
  # ... while the point estimates are the same contrasts either way.
  expect_equal(none$`Adjusted Difference`, v2$`Adjusted Difference`, tolerance = 1e-10)
})

test_that("type='prob_dist' matches V1's columns", {
  df <- make_wrapper_data()
  v1 <- tidy_rowwise(fit_v1(df), model, type = "prob_dist")
  v2 <- tidy_rowwise(fit_v2(df), model, type = "prob_dist")

  expect_equal(colnames(v2), colnames(v1))
  expect_true(any(v2$statistic %in% TRUE))
  expect_equal(unique(v2$df1[!is.na(v2$df1)]), 2) # 3 groups -> 2 df
})

test_that("type='data' returns the analysis rows under the original names", {
  df <- make_wrapper_data(with_na = TRUE)
  v2 <- tidy_rowwise(fit_v2(df), model, type = "data", sort_factor_levels = TRUE)

  expect_equal(colnames(v2), c("y", "group", "X1", "X2"))
  # V1's type="data" keeps rows whose COVARIATE is NA (only the target is
  # filtered), so its row count and its own emmeans table disagree. V2's does
  # not: this table is the same row set every statistic was computed on.
  expect_equal(nrow(v2), sum(stats::complete.cases(df[, c("y", "group", "X1", "X2")])))
  expect_true(all(!is.na(v2$X1)))
})

test_that("type='levene' and type='shapiro' match V1's columns", {
  df <- make_wrapper_data()
  v1_lev <- tidy_rowwise(fit_v1(df), model, type = "levene", levene_test_center = "median")
  v2_lev <- tidy_rowwise(fit_v2(df), model, type = "levene", levene_test_center = "median")
  expect_equal(colnames(v2_lev), colnames(v1_lev))
  expect_equal(v2_lev$Method[[1]], "Brown-Forsythe Test")

  v2_lev_mean <- tidy_rowwise(fit_v2(df), model, type = "levene", levene_test_center = "mean")
  expect_equal(v2_lev_mean$Method[[1]], "Levene's Test")

  v1_sh <- tidy_rowwise(fit_v1(df), model, type = "shapiro")
  v2_sh <- tidy_rowwise(fit_v2(df), model, type = "shapiro")
  expect_equal(colnames(v2_sh), colnames(v1_sh))
  expect_equal(v2_sh$Method[[1]], "Shapiro-Wilk Normality Test")
})

test_that("the final model is chosen by the homogeneity test, not by a user toggle", {
  # Parallel slopes -> additive final model -> a standard ANCOVA table.
  df <- make_wrapper_data()
  model <- fit_v2(df)$model[[1]]
  expect_equal(model$result$model_selection$final_model_type, "additive")
  expect_true(model$result$model_selection$standard_ancova_valid)
  expect_equal(model$result$slope_homogeneity$status, "not_detected")

  # Strongly group-dependent slopes -> interaction final model. The standard
  # ANCOVA table is then withheld rather than reported from a model whose
  # premise it contradicts.
  set.seed(11)
  df2 <- make_wrapper_data(seed = 11)
  slope_by_group <- c(A = 0, B = 8, C = -8)
  df2$y <- df2$y + slope_by_group[as.character(df2$group)] * df2$X1
  model2 <- fit_v2(df2)$model[[1]]
  expect_equal(model2$result$model_selection$final_model_type, "interaction")
  expect_equal(model2$result$slope_homogeneity$status, "detected")

  tbl <- tidy_rowwise(fit_v2(df2), model, type = "model")
  expect_equal(colnames(tbl), "Note")
  expect_true(grepl("homogeneity", tbl$Note[[1]], fixed = TRUE))
  # ... and the charts that cannot be drawn from it degrade to empty rather
  # than to a wrong picture.
  expect_equal(nrow(tidy_rowwise(fit_v2(df2), model, type = "prob_dist")), 0)
  # The means/pairs surfaces still work -- they come from the interaction
  # model's conditional means at the covariate reference point.
  expect_true(nrow(tidy_rowwise(fit_v2(df2), model, type = "emmeans")) == 3)
  expect_true(nrow(tidy_rowwise(fit_v2(df2), model, type = "pairs")) == 3)
})

test_that("zero covariates delegates to exp_anova instead of failing", {
  df <- make_wrapper_data()
  ret <- exp_ancova(df, "y", "group", covariates = NULL)
  expect_true("anova_exploratory" %in% class(ret$model[[1]]))
  expect_equal(tidy_rowwise(ret, model, type = "model"),
               tidy_rowwise(fit_v1(df, covariates = NULL), model, type = "model"))

  # An all-blank covariates vector reaches R the same way from the UI.
  ret2 <- exp_ancova(df, "y", "group", covariates = c("", NA))
  expect_true("anova_exploratory" %in% class(ret2$model[[1]]))
})

test_that("the tidy surfaces read the same fit the reported statistics came from", {
  df <- make_wrapper_data()
  model <- fit_v2(df)$model[[1]]
  expect_true(inherits(model$internals$final_model, "lm"))
  expect_true(inherits(model$internals$model_additive, "lm"))
  expect_true(inherits(model$internals$model_interaction, "lm"))
  # Residual DF reported in the table must be the final model's own.
  tbl <- tidy_rowwise(fit_v2(df), model, type = "model")
  expect_equal((tbl %>% dplyr::filter(Variable == "(Residuals)"))$DF[[1]],
               model$internals$final_model$df.residual)
  # internals is deliberately NOT part of the serializable result contract.
  expect_null(model$result$internals)
})

test_that("column names with spaces, symbols and multibyte characters survive", {
  df <- make_wrapper_data()
  stress <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  names(df) <- c("売上 金額", "部 署", stress, "X2")
  ret <- exp_ancova(df, "売上 金額", "部 署",
                    covariates = c(stress, "X2"))
  tbl <- tidy_rowwise(ret, model, type = "model")
  expect_true(stress %in% tbl$Variable)
  expect_true("部 署" %in% tbl$Variable)

  data_tbl <- tidy_rowwise(ret, model, type = "data")
  expect_equal(colnames(data_tbl), c("売上 金額", "部 署", stress, "X2"))

  emm <- tidy_rowwise(ret, model, type = "emmeans")
  expect_equal(colnames(emm)[[1]], "部 署")
  expect_true(stress %in% colnames(emm))
})

test_that("the outlier filter runs on the target column before fitting", {
  df <- make_wrapper_data()
  df$y[[1]] <- 1e6
  kept <- nrow(tidy_rowwise(exp_ancova(df, "y", "group", covariates = c("X1", "X2"),
                                       outlier_filter_type = "iqr"),
                            model, type = "data"))
  expect_true(kept < nrow(df))
  unfiltered <- nrow(tidy_rowwise(fit_v2(df), model, type = "data"))
  expect_equal(unfiltered, nrow(df))
})

test_that("the V1 error strings tam's formatter matches are preserved", {
  df <- make_wrapper_data()
  one_group <- df %>% dplyr::filter(group == "A")
  expect_error(exp_ancova(one_group, "y", "group", covariates = c("X1", "X2")),
               "The explanatory variable needs to have 2 or more unique values.")

  all_na <- df
  all_na$y <- NA_real_
  expect_error(exp_ancova(all_na, "y", "group", covariates = c("X1", "X2")),
               "There is no data left after removing NA.")
})
