# ANCOVA Calculation V2 -- Analytics View entry point (tam#38389 Phase 1.5).
#
# run_ancova_v2() (R/ancova_v2.R) is the computation layer: it takes a plain
# data frame and returns a nested, serializable result list. The Analytics View
# cannot consume that directly -- every chart in exp_ancova.json is a
# `tidy_rowwise(model, type="...")` call, which requires
#
#   1. a ROWWISE data frame with one row per group and a list-column named
#      exactly `model` (broom_wrapper.R's tidy_rowwise resolves that name by
#      tidyselect NSE), and
#   2. an S3 `tidy` method on the object inside it.
#
# This file supplies both. The tidy types below deliberately reproduce
# tidy.anova_exploratory()'s OWN column names for the same report surfaces, so
# the template's preprocessors, the report's bind variables and the harness's
# approved answers keep their contract. Where V2 genuinely changed a number
# rather than a name, the difference is called out in a comment beside it.
#
# The model object keeps the fitted lm objects (via keep_internals = TRUE), so
# every tidy type -- and Phase 2's diagnostics -- reads the SAME fit the
# reported statistics came from. Nothing here refits a model.

#' Per-group ANCOVA V2 fit. Mirrors exp_anova()'s anova_each() preparation
#' (column subset, target-NA filter, outlier filter, group-count re-check) and
#' then delegates every statistic to run_ancova_v2().
#' @noRd
ancova_v2_each <- function(df, var1_col, var2_col, covariates, test_sig_level,
                           outlier_filter_type, outlier_filter_threshold,
                           common_var2_order, grouped_cols) {
  tryCatch({
    df <- df %>% dplyr::select(dplyr::all_of(c(var1_col, var2_col, covariates)))

  # Only the target column is NA-filtered here, exactly as exp_anova does.
  # run_ancova_v2() then applies ITS complete-case rule across target, factor
  # and every covariate, and every statistic it returns describes that one row
  # set (the tam#38216 / harness covariateNaRowCountDivergenceDefect fix).
  df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var1_col)))
  if (nrow(df) == 0) {
    stop("There is no data left after removing NA.")
  }

  if (!is.null(outlier_filter_type)) {
    # detect_outlier() returns "Lower"/"Upper"/"Normal" labels, not a logical --
    # same membership test exp_anova uses.
    is_outlier <- detect_outlier(df[[var1_col]], type = outlier_filter_type,
                                 threshold = outlier_filter_threshold) %in% c("Lower", "Upper")
    df <- df[!is_outlier, , drop = FALSE]
    if (nrow(df) == 0) {
      stop("There is no data left after removing NA.")
    }
  }

  if (dplyr::n_distinct(df[[var2_col]], na.rm = TRUE) < 2) {
    stop("The explanatory variable needs to have 2 or more unique values.")
  }

  res <- run_ancova_v2(df, outcome = var1_col, factor = var2_col,
                       covariates = covariates, alpha = test_sig_level,
                       keep_internals = TRUE)

  if (identical(res$analysis_status, "error")) {
    # run_ancova_v2 reports failures as data, not conditions. The Analytics
    # View needs a thrown error to surface one, so translate here rather than
    # letting a chart render an empty table with no explanation.
    stop(res$message)
  }

  model <- list(
    result = res,
    internals = res$internals,
    var1 = var1_col,
    var2 = var2_col,
    covariates = covariates,
    common_var2_order = common_var2_order,
    test_sig_level = test_sig_level,
    # The analysis row set, under the user's ORIGINAL column names -- what
    # type="data" hands the Means / Data Distribution / Linearity charts.
    dataframe = res$internals$analysis_data %>%
      dplyr::select(dplyr::all_of(c(res$internals$safe_y, res$internals$safe_factor,
                                    res$internals$safe_x))) %>%
      stats::setNames(c(var1_col, var2_col, covariates))
  )
  # internals is reachable as model$internals; drop the duplicate copy so the
  # result list stays the serializable shape run_ancova_v2 documents.
  model$result$internals <- NULL
  class(model) <- c("ancova_v2_exploratory", class(model))
  model
  }, error = function(e) {
    if (length(grouped_cols) > 0 && !grepl("EXP-ANA", e$message, fixed = TRUE)) {
      # Repeat-by analyses must keep errors local to the affected group, just
      # like exp_anova(). tidy.ancova_v2_exploratory() renders this object as a
      # Note row while the other groups remain available to the report.
      class(e) <- c("ancova_v2_exploratory", class(e))
      e
    } else {
      stop(e)
    }
  })
}

#' ANCOVA Calculation V2 for the Analytics View.
#'
#' Drop-in replacement for `exp_anova()`'s ANCOVA path. Returns the same
#' rowwise model-data-frame shape (`model` list-column) and offers the same
#' `tidy(x, type=)` surfaces, but every statistic comes from
#' `run_ancova_v2()`: one complete-case row set shared by all of them,
#' grand-mean-centered covariates, and a final model chosen by the global
#' homogeneity-of-slopes test rather than by a user toggle.
#'
#' With zero covariates the analysis is not an ANCOVA at all -- there is
#' nothing to adjust for -- and `run_ancova_v2()` requires at least one. That
#' case therefore delegates to `exp_anova()`, which degrades it to the
#' equivalent one-way ANOVA exactly as it does today.
#'
#' @param df A data frame, optionally grouped.
#' @param var1 Numeric outcome column.
#' @param var2 Categorical factor column.
#' @param covariates Character vector of numeric covariate column names.
#' @param func2 Preprocessing token for `var2` (Date/numeric extraction).
#' @param covariate_funs Named preprocessing tokens for the covariates.
#' @param test_sig_level Significance threshold; becomes `run_ancova_v2()`'s
#'   `alpha`, and therefore also the confidence level `1 - alpha` of every
#'   reported interval.
#' @param outlier_filter_type,outlier_filter_threshold Outlier filter applied
#'   to the target column only, before fitting.
#' @return A rowwise data frame with a `model` list-column of
#'   `ancova_v2_exploratory` objects.
#' @export
exp_ancova <- function(df, var1, var2, covariates = NULL, func2 = NULL,
                       covariate_funs = NULL, test_sig_level = 0.05,
                       outlier_filter_type = NULL, outlier_filter_threshold = NULL,
                       ...) {
  var1_ <- substitute(var1)
  var1_col <- if (class(var1_) == "name") col_name(var1_) else var1
  var2_ <- substitute(var2)
  var2_col <- if (class(var2_) == "name") col_name(var2_) else var2

  if (length(var2_col) != 1) {
    stop("ANCOVA supports exactly one explanatory variable.")
  }

  grouped_cols <- grouped_by(df)

  covariates <- covariates[!is.na(covariates) & nzchar(covariates)]
  if (length(covariates) == 0) {
    # Not an ANCOVA -- see the note above. Hand it to the one-way path intact.
    # do.call, not a direct call: exp_anova() resolves var1/var2 by
    # substitute(), so passing the local VARIABLES would make it read the
    # column names "var1_col"/"var2_col". do.call splices the values in.
    return(do.call(exp_anova, list(df, var1_col, var2_col, covariates = NULL,
                                   func2 = func2, test_sig_level = test_sig_level,
                                   outlier_filter_type = outlier_filter_type,
                                   outlier_filter_threshold = outlier_filter_threshold)))
  }

  if (!is.null(func2)) {
    for (i in 1:length(func2)) {
      if (lubridate::is.Date(df[[var2_col[i]]]) || lubridate::is.POSIXct(df[[var2_col[i]]])) {
        df <- df %>% dplyr::mutate(!!rlang::sym(var2_col[i]) := extract_from_date(!!rlang::sym(var2_col[i]), type = !!func2[i]))
      }
      else if (is.numeric(df[[var2_col[i]]])) {
        df <- df %>% dplyr::mutate(!!rlang::sym(var2_col[i]) := extract_from_numeric(!!rlang::sym(var2_col[i]), type = !!func2[i]))
      }
    }
  }

  if (dplyr::n_distinct(df[[var2_col]]) < 2) {
    stop("The explanatory variable needs to have 2 or more unique values.")
  }

  # Same coercion exp_anova applies: a logical or Date factor column otherwise
  # fails model fitting with an opaque message (tam#38168).
  col_var2 <- df[[var2_col]]
  if (is.numeric(col_var2) || is.logical(col_var2) || is.character(col_var2) ||
      lubridate::is.Date(col_var2) || lubridate::is.POSIXct(col_var2)) {
    df[[var2_col]] <- factor(col_var2)
  }

  if (!is.null(covariate_funs)) {
    df <- df %>% mutate_predictors(covariates, covariate_funs)
    covariates <- names(unlist(covariate_funs))
  }

  # Shared display order for the group levels, so the Means and Means
  # (Adjusted) charts line up. Computed on the whole (ungrouped) frame, as
  # exp_anova does.
  common_var2_order <- (df %>% dplyr::ungroup() %>%
    dplyr::group_by(!!rlang::sym(var2_col)) %>%
    dplyr::summarize(mean = mean(!!rlang::sym(var1_col), na.rm = TRUE)) %>%
    dplyr::arrange(dplyr::desc(mean)))[[var2_col]]

  do_on_each_group(df, ancova_v2_each,
                   params = quote(list(var1_col, var2_col, covariates, test_sig_level,
                                       outlier_filter_type, outlier_filter_threshold,
                                       common_var2_order, grouped_cols)),
                   name = "model", with_unnest = FALSE)
}

# ------------------------------------------------------------
# tidy method -- one tibble per report surface
# ------------------------------------------------------------

#' @noRd
ancova_v2_pairs_method_label <- function(pairs_adjust) {
  switch(pairs_adjust,
    "none" = "Pairwise T-Test with No Adjustment",
    "tukey" = "Tukey's HSD Test",
    "bonferroni" = "Pairwise T-Test with Bonferroni Correction",
    "sheffe" = "Sheffe's Method",
    "sidak" = "Pairwise T-Test with Sidak Correction",
    "dunnett" = "Dunnett's Test",
    "holm" = "Pairwise T-Test with Holm Correction",
    "hochberg" = "Pairwise T-Test with Hochberg Correction"
  )
}

#' The ANCOVA table, in tidy.anova_exploratory()'s own column names.
#'
#' One numeric difference from V1, deliberate: `Cohen's F` is Cohen's f
#' computed from PARTIAL eta-squared (`sqrt(pe2/(1-pe2))`), which is its
#' standard definition. V1 computed it from total eta-squared, so the two
#' disagree for any model with more than one term -- i.e. every ANCOVA.
#' @noRd
ancova_v2_model_table <- function(x) {
  res <- x$result
  tbl <- res$ancova_table
  if (is.null(tbl)) {
    # Homogeneity of regression slopes was rejected, so the final model is the
    # interaction model and a standard ANCOVA table would misdescribe it.
    return(tibble::tibble(Note = paste0(
      "The homogeneity-of-regression-slopes assumption was not met, so the standard ",
      "ANCOVA table is not reported. See the group-specific slopes instead.")))
  }
  total_ss <- tbl$corrected_total$sum_squares
  factor_name <- res$variables$factor

  terms <- tbl$terms %>%
    dplyr::transmute(
      `Variable` = ifelse(term_type == "factor", factor_name, term_name),
      `Sum of Squares` = sum_squares,
      `SS Ratio` = sum_squares / total_ss,
      `DF` = df,
      `Mean Square` = mean_square,
      `F Value` = F,
      `P Value` = p_value,
      `Eta Squared` = eta_squared,
      `Partial Eta Squared` = partial_eta_squared,
      `Cohen's F` = cohens_f_partial,
      `Omega Squared` = omega_squared
    )

  residual_row <- tibble::tibble(
    `Variable` = "(Residuals)",
    `Sum of Squares` = tbl$residual$sum_squares,
    `SS Ratio` = tbl$residual$sum_squares / total_ss,
    `DF` = tbl$residual$df,
    `Mean Square` = tbl$residual$mean_square,
    `F Value` = NA_real_, `P Value` = NA_real_, `Eta Squared` = NA_real_,
    `Partial Eta Squared` = NA_real_, `Cohen's F` = NA_real_, `Omega Squared` = NA_real_
  )
  total_row <- tibble::tibble(
    `Variable` = "(Total)",
    `Sum of Squares` = total_ss,
    `SS Ratio` = 1,
    `DF` = tbl$corrected_total$df,
    `Mean Square` = NA_real_,
    `F Value` = NA_real_, `P Value` = NA_real_, `Eta Squared` = NA_real_,
    `Partial Eta Squared` = NA_real_, `Cohen's F` = NA_real_, `Omega Squared` = NA_real_
  )
  dplyr::bind_rows(terms, residual_row, total_row)
}

#' Adjusted means joined to the unadjusted per-group statistics. Both sides
#' describe the SAME complete-case rows -- that is the whole point of V2's
#' single analysis_data (tam#38216).
#' @noRd
ancova_v2_emmeans_table <- function(x, sort_factor_levels) {
  res <- x$result
  adj <- if (!is.null(res$adjusted_means)) res$adjusted_means else res$conditional_means_at_reference
  if (is.null(adj)) {
    return(tibble::tibble())
  }
  var2_col <- x$var2

  adj_tbl <- adj$means %>%
    dplyr::transmute(
      !!rlang::sym(var2_col) := group,
      `Mean (Adj)` = estimate,
      `Std Error (Adj)` = standard_error,
      `Conf Low (Adj)` = confidence_lower,
      `Conf High (Adj)` = confidence_upper,
      `DF` = df
    )

  raw_tbl <- res$raw_statistics %>%
    dplyr::transmute(
      !!rlang::sym(var2_col) := group,
      `Rows` = n,
      `Mean` = mean,
      `Std Deviation` = sd,
      `Std Error` = se,
      `Conf Low` = ci_lower,
      `Conf High` = ci_upper,
      `Minimum` = min,
      `Maximum` = max
    )

  ret <- raw_tbl %>% dplyr::left_join(adj_tbl, by = var2_col)

  # The covariate values every adjusted mean was computed AT, one constant
  # column each -- emmeans' by-clause produces the same columns in V1.
  ref <- adj$reference_covariates
  for (cov_name in names(ref)) {
    ret[[cov_name]] <- ref[[cov_name]]
  }

  ret <- ret %>%
    dplyr::relocate(dplyr::any_of(names(ref)), .after = dplyr::all_of(var2_col)) %>%
    dplyr::relocate(dplyr::any_of(c("Mean (Adj)", "Std Error (Adj)",
                                    "Conf Low (Adj)", "Conf High (Adj)", "DF")),
                    .after = `Conf High`)

  if (sort_factor_levels && !is.null(x$common_var2_order)) {
    ret <- ret %>%
      dplyr::mutate(!!rlang::sym(var2_col) := forcats::fct_relevel(
        as.character(!!rlang::sym(var2_col)), as.character(x$common_var2_order)))
  }
  ret
}

#' Pairwise comparisons of the adjusted means.
#'
#' run_ancova_v2() reports Tukey-adjusted pairs; the Analytics View lets the
#' user pick the adjustment, so re-contrast the SAME emmGrid the reported means
#' came from rather than refitting anything. Falls back to the stored table
#' when the grid is unavailable.
#' @noRd
ancova_v2_pairs_table <- function(x, pairs_adjust) {
  res <- x$result
  emm <- x$internals$reported_emm
  method_label <- ancova_v2_pairs_method_label(pairs_adjust)

  if (!is.null(emm)) {
    pw <- emmeans::contrast(emm, method = "pairwise", adjust = pairs_adjust)
    ci <- stats::confint(pw, level = 1 - res$metadata$alpha)
    ret <- tibble::as_tibble(pw) %>%
      dplyr::mutate(conf.low = ci$lower.CL, conf.high = ci$upper.CL)
    levels_vec <- unlist(res$analysis_sample$factor_levels)
    pairs_split <- purrr::map_dfr(ret$contrast, function(label) {
      parts <- ancova_split_pair_label(label, levels_vec)
      tibble::tibble(`Group 1` = parts[[1]], `Group 2` = parts[[2]])
    })
    ret <- dplyr::bind_cols(pairs_split, ret %>% dplyr::select(-contrast))
    ret <- ret %>%
      dplyr::transmute(
        `Group 1`, `Group 2`,
        `Adjusted Difference` = estimate,
        `Conf Low` = conf.low,
        `Conf High` = conf.high,
        `Standard Error` = SE,
        `DF` = df,
        `t Value` = t.ratio,
        `P Value` = p.value,
        `Method` = method_label
      )
    return(ret)
  }

  stored <- if (!is.null(res$pairwise_comparisons)) res$pairwise_comparisons else res$conditional_pairwise_at_reference
  if (is.null(stored)) {
    return(tibble::tibble())
  }
  stored %>%
    dplyr::transmute(
      `Group 1` = group1,
      `Group 2` = group2,
      `Adjusted Difference` = adjusted_difference,
      `Conf Low` = confidence_lower,
      `Conf High` = confidence_upper,
      `Standard Error` = standard_error,
      `DF` = df,
      `t Value` = t_value,
      `P Value` = p_value,
      `Method` = ancova_v2_pairs_method_label("tukey")
    )
}

#' F distribution density for the factor's own test, for the Prob.
#' Distribution chart. Reuses generate_ftest_density_data() so the column
#' contract (x, y, statistic, p.value, critical, df1, df2) is identical to V1.
#' @noRd
ancova_v2_prob_dist <- function(x) {
  res <- x$result
  tbl <- res$ancova_table
  if (is.null(tbl)) {
    # No standard ANCOVA F test to draw -- see ancova_v2_model_table().
    return(tibble::tibble())
  }
  factor_row <- tbl$terms %>% dplyr::filter(term_type == "factor")
  if (nrow(factor_row) == 0) {
    return(tibble::tibble())
  }
  generate_ftest_density_data(
    factor_row$F[[1]],
    p.value = factor_row$p_value[[1]],
    df1 = factor_row$df[[1]],
    df2 = tbl$residual$df,
    sig_level = res$metadata$alpha)
}

#' @noRd
ancova_v2_levene_table <- function(x, levene_test_center) {
  center_fun <- if (levene_test_center == "mean") mean else median
  final_model <- x$internals$final_model
  group_col <- x$internals$analysis_data[[x$internals$safe_factor]]
  ret <- broom::tidy(car::leveneTest(stats::residuals(final_model), group_col,
                                     center = center_fun))
  ret <- ret %>% dplyr::rename(dplyr::any_of(c(`F Value` = "statistic",
                                               `P Value` = "p.value",
                                               `DF` = "df",
                                               `Residual DF` = "df.residual")))
  ret <- ret %>% dplyr::mutate(`Method` = if (levene_test_center == "mean") {
    "Levene's Test"
  } else {
    "Brown-Forsythe Test"
  })
  ret %>% dplyr::mutate(`Result` = ifelse(`P Value` < x$test_sig_level,
                                          "Homogeneity assumption is not valid.",
                                          "Homogeneity assumption is valid."))
}

#' @noRd
ancova_v2_shapiro_table <- function(x, shapiro_seed) {
  resid <- stats::residuals(x$internals$final_model)
  if (length(resid) > 5000) {
    if (!is.null(shapiro_seed)) {
      set.seed(shapiro_seed)
    }
    resid <- sample(resid, 5000)
  }
  ret <- broom::tidy(stats::shapiro.test(resid))
  ret$n <- length(resid)
  ret <- ret %>% dplyr::rename(dplyr::any_of(c(`W Value` = "statistic",
                                               `P Value` = "p.value",
                                               `Method` = "method",
                                               `Rows` = "n")))
  ret <- ret %>% dplyr::mutate(`Method` = "Shapiro-Wilk Normality Test")
  ret %>% dplyr::mutate(`Result` = ifelse(`P Value` < x$test_sig_level,
                                          "Normality assumption is not valid.",
                                          "Normality assumption is valid."))
}

#' Tidy an ANCOVA V2 model for the Analytics View.
#'
#' Supported types: "model" (ANCOVA table), "emmeans" (adjusted + unadjusted
#' means), "pairs", "prob_dist", "levene", "shapiro", and "data" (the analysis
#' rows). Any unrecognized type returns the data, matching
#' tidy.anova_exploratory()'s own catch-all.
#'
#' @param x An `ancova_v2_exploratory` model.
#' @param type Which report surface to return.
#' @param conf_level Unused; the confidence level is `1 - alpha`, fixed at fit
#'   time so every reported interval agrees. Accepted for signature parity.
#' @param pairs_adjust Multiple-comparison adjustment for `type="pairs"`.
#' @param levene_test_center "mean" or "median".
#' @param shapiro_seed Seed used when residuals are subsampled to 5000.
#' @param sort_factor_levels Order group levels by descending unadjusted mean.
#' @export
tidy.ancova_v2_exploratory <- function(x, type = "model", conf_level = 0.95,
                                       pairs_adjust = "none",
                                       levene_test_center = "median",
                                       shapiro_seed = 1,
                                       sort_factor_levels = FALSE) {
  if ("error" %in% class(x)) {
    message <- if (is.null(x$message) || x$message == "") as.character(x) else x$message
    if (type %in% c("model", "between", "within")) {
      return(tibble::tibble(Note = message))
    }
    return(tibble::tibble())
  }

  if (type %in% c("model", "between", "within")) {
    ancova_v2_model_table(x)
  }
  else if (type == "emmeans") {
    ancova_v2_emmeans_table(x, sort_factor_levels)
  }
  else if (type == "pairs") {
    ancova_v2_pairs_table(x, pairs_adjust)
  }
  else if (type == "prob_dist") {
    ancova_v2_prob_dist(x)
  }
  else if (type == "levene") {
    ancova_v2_levene_table(x, levene_test_center)
  }
  else if (type == "shapiro") {
    ancova_v2_shapiro_table(x, shapiro_seed)
  }
  else { # type == "data"
    ret <- x$dataframe
    if (sort_factor_levels && !is.null(x$common_var2_order)) {
      ret <- ret %>%
        dplyr::mutate(!!rlang::sym(x$var2) := forcats::fct_relevel(
          as.character(!!rlang::sym(x$var2)), as.character(x$common_var2_order)))
    }
    ret
  }
}
