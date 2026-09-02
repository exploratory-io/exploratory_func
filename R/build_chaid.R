# Analytics View integration layer for the CHAID engine in chaid.R.
#
# exp_chaid() wraps chaid_fit() in the standard model-data-frame shape (one row
# per group: `model`, `.test_index`, `source.data`) used by every Analytics View
# decision-tree/model template, and the augment/tidy/glance S3 methods below let
# the framework's generic preprocessors (prediction(), rf_evaluation_*,
# tidy_rowwise(), rf_partial_dependence()) dispatch on an `exploratory_chaid`
# model. This mirrors the exp_rpart() pattern in randomForest_tidiers.R, minus
# regression and SMOTE. Model-independent permutation importance is calculated
# from the training or held-out rows; partial dependence for the report chart
# is stored on the model the same way as CART.

#' Fit a CHAID classification tree as an Analytics View model data frame.
#'
#' @param df A data frame, optionally grouped.
#' @param target Target column (unquoted); character/factor/logical.
#' @param ... Predictor columns (unquoted), tidyselect-style.
#' @param target_fun Optional function name to transform the target.
#' @param predictor_funs Optional named list of predictor transformations.
#' @param alpha_split,alpha_merge,max_depth,min_split,min_bucket,min_node_proportion CHAID growth controls.
#' @param numeric_binning,numeric_bins Numeric predictor binning controls.
#' @param missing Missing-value handling (`as_category` or `exclude`).
#' @param chi_square Chi-square statistic (`pearson` or `likelihood_ratio`).
#' @param bonferroni Whether to apply Bonferroni correction.
#' @param allow_resplit Whether merged categories may be split again (stage 3).
#' @param max_categories Maximum predictor categories before a predictor is skipped.
#' @param max_nrow Row cap; data is sampled down to this before fitting.
#' @param target_n,predictor_n Category caps; excess categories are lumped into "Other".
#' @param binary_classification_threshold Probability threshold for the positive class.
#' @param importance_measure How variable importance is calculated. `permutation`
#'   (default) uses model-independent permutation importance; `firm` derives it
#'   from the flatness of each variable's partial-dependence curve. CHAID splits
#'   on chi-square significance rather than impurity, so it exposes no
#'   model-native impurity vector and therefore offers no `impurity` option.
#' @param max_pd_vars Max number of predictors for partial dependence charts.
#' @param pd_sample_size Row sample size used when computing partial dependence.
#' @param pd_grid_resolution Grid resolution for numeric partial dependence.
#' @param pd_with_bin_means Whether to overlay binned actual means on PD charts.
#' @param seed Random seed for sampling / splitting reproducibility.
#' @param test_rate Fraction of rows held out as test data.
#' @param test_split_type `random` or `ordered`.
#' @return A rowwise model data frame with `model`, `.test_index`, `source.data`.
#' @export
exp_chaid <- function(df,
                      target,
                      ...,
                      target_fun = NULL,
                      predictor_funs = NULL,
                      alpha_split = 0.05,
                      alpha_merge = 0.05,
                      max_depth = 3,
                      min_split = 50,
                      min_bucket = 20,
                      min_node_proportion = NULL,
                      numeric_binning = "quantile",
                      numeric_bins = 10,
                      missing = "as_category",
                      chi_square = "pearson",
                      bonferroni = TRUE,
                      allow_resplit = FALSE,
                      max_categories = 50,
                      max_nrow = 50000,
                      target_n = 20,
                      predictor_n = 12,
                      binary_classification_threshold = 0.5,
                      importance_measure = "permutation",
                      max_pd_vars = 20,
                      pd_sample_size = 500,
                      pd_grid_resolution = 20,
                      pd_with_bin_means = FALSE,
                      seed = 1,
                      test_rate = 0.0,
                      test_split_type = "random") {
  if (length(test_rate) != 1L || !is.numeric(test_rate) ||
      is.na(test_rate) || !is.finite(test_rate) || test_rate < 0 || 1 < test_rate) {
    stop("test_rate must be between 0 and 1")
  } else if (test_rate == 1) {
    stop("test_rate must be less than 1")
  }
  test_split_type <- match.arg(test_split_type, c("random", "ordered"))

  # NSE column selection, mirroring exp_rpart.
  target_col <- tidyselect::vars_select(names(df), !! rlang::enquo(target))
  if (length(target_col) != 1L) {
    stop("target must select exactly one column")
  }
  orig_selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))

  # tam #38166: numeric targets are supported via a One-way ANOVA F-test
  # (see chaid_fit()/merge_categories()/compute_anova_from_stats() in
  # chaid.R); Date/POSIXct/duration columns stay out of scope (spec section 3)
  # even though some are numeric-backed, hence chaid_is_numeric_target()
  # rather than a bare is.numeric() check.
  is_numeric_target_col <- chaid_is_numeric_target(df[[target_col]])

  target_funs <- NULL
  if (!is.null(target_fun)) {
    target_funs <- list(target_fun)
    names(target_funs) <- target_col
    df <- df %>% mutate_predictors(target_col, target_funs)
  }

  if (!is.null(predictor_funs)) {
    df <- df %>% mutate_predictors(orig_selected_cols, predictor_funs)
    selected_cols <- names(unlist(predictor_funs))
  } else {
    selected_cols <- orig_selected_cols
  }

  grouped_cols <- grouped_by(df)
  # Sort predictors so the fitted tree is stable against input column order.
  selected_cols <- stringr::str_sort(selected_cols)

  is_target_logical <- is.logical(df[[target_col]])
  # Capture the target's ordinal nature BEFORE cleaning coerces it to a plain
  # factor/character. Needed for the category-error distribution (ordinal
  # distance between predicted and actual category) in the report (#37155).
  is_target_ordered <- is.ordered(df[[target_col]])
  target_ordered_levels <- if (is_target_ordered) levels(df[[target_col]]) else NULL

  # tam #37177: cleanup_df() turns every character predictor into a factor whose
  # levels are just data-appearance order, so factor-ness has to be captured from
  # the ORIGINAL frame. Report tables keep a real factor's declared level order
  # and sort everything else alphabetically.
  original_factor_levels <- lapply(Filter(is.factor, df), levels)
  clean_ret <- cleanup_df(df, target_col, selected_cols, grouped_cols,
                          target_n, predictor_n, map_name = FALSE)
  clean_df <- clean_ret$clean_df
  name_map <- clean_ret$name_map
  clean_target_col <- clean_ret$clean_target_col
  clean_cols <- clean_ret$clean_cols

  each_func <- function(df) {
    tryCatch({
      if (!is.null(seed)) {
        set.seed(seed)
      }
      clean_df_ret <- cleanup_df_per_group(
        df, clean_target_col, max_nrow, clean_cols, name_map, predictor_n,
        revert_logical_levels = FALSE, filter_numeric_na = TRUE,
        # Keep a logical target logical so chaid_fit() sets TRUE-first levels.
        convert_logical = FALSE
      )
      if (is.null(clean_df_ret)) {
        return(NULL) # skip this group
      }
      df <- clean_df_ret$df
      c_cols <- clean_df_ret$c_cols
      if (length(c_cols) == 0) {
        stop("Invalid Predictors: Only one unique value.")
      }
      group_name_map <- clean_df_ret$name_map

      # Split training and test data.
      source_data <- df
      test_index <- sample_df_index(source_data, rate = test_rate,
                                    ordered = (test_split_type == "ordered"))
      df <- safe_slice(source_data, test_index, remove = TRUE)

      # tam #38166 spec section 36: a numeric target with a single distinct
      # value (SS_total == 0) still fits -- it simply produces a root-only
      # tree (grow_node()'s purity check), so this categorical-only guard is
      # skipped for a numeric target rather than erroring.
      if (!is_numeric_target_col) {
        unique_val <- unique(df[[clean_target_col]])
        if (length(unique_val[!is.na(unique_val)]) <= 1) {
          stop("Categorical Target Variable must have 2 or more unique values.")
        }
      }

      model <- chaid_fit(
        df, target = clean_target_col, predictors = c_cols,
        alpha_split = alpha_split, alpha_merge = alpha_merge,
        max_depth = max_depth, min_split = min_split, min_bucket = min_bucket,
        min_node_proportion = min_node_proportion,
        numeric_binning = numeric_binning, numeric_bins = numeric_bins,
        missing = missing, chi_square = chi_square, bonferroni = bonferroni,
        allow_resplit = allow_resplit,
        max_categories = max_categories
      )
      model$classification_type <- if (model$target_type == "numeric") {
        "regression"
      } else if (model$target_type == "logical") {
        "binary"
      } else {
        "multi"
      }
      model$original_factor_levels <- original_factor_levels

      # Store training actual / predicted so tidy() evaluation and conf_mat can
      # reuse the shared model-agnostic evaluation helpers.
      train_all <- chaid_predict(model, df, type = "all")
      if (model$target_type == "numeric") {
        # tam #38166: no class to predict -- keep the numeric actual/predicted
        # pair (for RMSE/R^2 evaluation) and the per-row node id (so
        # build_chaid_tree_nodes() can build each node's target histogram,
        # mirroring rpart's x$where).
        model$y <- as.numeric(df[[clean_target_col]])
        model$predicted_value <- train_all$.pred_value
        model$predicted_class <- train_all$.pred_class
        model$train_node_ids <- train_all$.chaid_node_id
      } else {
        actual <- factor(as.character(df[[clean_target_col]]), levels = model$class_levels)
        model$y <- actual
        model$predicted_class <- chaid_predicted_class(
          model, train_all, binary_classification_threshold
        )
        model$predicted_prob <- chaid_positive_probability(model, train_all)
        # Full class-probability matrix for report_metrics (Macro / One-vs-Rest AUCs).
        # Column names are the raw class levels so multiclass_auc_by_class() can use them.
        model$predicted_prob_matrix <- chaid_as_probability_matrix(
          train_all, model$class_levels
        )
      }

      # Metadata expected by the framework.
      model$terms_mapping <- names(group_name_map)
      names(model$terms_mapping) <- group_name_map

      importance_data <- if (length(test_index) > 0) {
        source_data[test_index, , drop = FALSE]
      } else {
        df
      }
      evaluation_data_label <- if (length(test_index) > 0) "Test" else "Training"

      if (is.null(max_pd_vars) || !is.finite(max_pd_vars) || max_pd_vars < 1) {
        max_pd_vars_eff <- 20
      } else {
        max_pd_vars_eff <- as.integer(max_pd_vars)
      }

      if (model$target_type == "numeric") {
        # tam #38166: `firm` importance and partial dependence both go through
        # `predict(type = "prob")`, which has no meaning for a numeric target
        # (see chaid_predict_prepared()) -- deferred as future work (PR body).
        # Importance uses an RMSE-drop permutation variant instead of
        # `chaid_permutation_importance()`'s log-loss.
        model$importance <- chaid_permutation_importance_numeric(
          model = model,
          data = importance_data,
          target = clean_target_col,
          predictors = c_cols,
          evaluation_data = evaluation_data_label,
          seed = seed,
          repeats = 10L
        )
        imp_vars <- chaid_partial_dependence_vars(
          model$importance, c_cols, model$terms_mapping, max_pd_vars_eff
        )
        # tam #38345: PD now works for a numeric target too (predict.fun above
        # switches to type = "value"), so the Analytics Report's
        # {{variable_effect}} / local_importance_regression chart has data --
        # the same contract exp_rpart's numeric report already uses. `firm`
        # importance stays on the RMSE-drop permutation variant regardless,
        # because calc_firm_from_pd() is written against class probabilities.
        model$partial_dependence <- partial_dependence.exploratory_chaid(
          model, clean_target_col, vars = imp_vars, data = df,
          n = c(pd_grid_resolution, min(nrow(df), pd_sample_size))
        )
      } else {
        # tam#37466: `firm` derives importance from the partial-dependence curves,
        # so unlike `permutation` it has to see the PD of EVERY predictor before it
        # can rank them. That inverts the order of the two steps below, exactly the
        # way exp_rpart does it: compute PD over all c_cols, run importance_firm(),
        # then trim imp_vars and shrink the PD data back down to max_pd_vars.
        # `identical()` keeps NULL and empty values on the permutation path, too.
        # This matters for callers that forward an optional UI setting.
        use_firm_importance <- identical(as.character(importance_measure), "firm") &&
          length(c_cols) > 1

        if (use_firm_importance) {
          imp_vars <- c_cols
        } else {
          model$importance <- chaid_permutation_importance(
            model = model,
            data = importance_data,
            target = clean_target_col,
            predictors = c_cols,
            evaluation_data = evaluation_data_label,
            seed = seed,
            repeats = 10L
          )
          imp_vars <- chaid_partial_dependence_vars(
            model$importance, c_cols, model$terms_mapping, max_pd_vars_eff
          )
        }

        # Partial dependence for Analytics Report {{variable_effect}} /
        # local_importance_binary (same contract as exp_rpart).
        model$partial_dependence <- partial_dependence.exploratory_chaid(
          model, clean_target_col, vars = imp_vars, data = df,
          n = c(pd_grid_resolution, min(nrow(df), pd_sample_size))
        )

        if (use_firm_importance) {
          model$importance <- chaid_firm_importance(
            # PD is always calculated from the fitted model's training rows.
            model$partial_dependence, model, imp_vars, "Training"
          )
          # Fall back to permutation when FIRM could not be computed (e.g. the
          # optional `mmpf` package is missing, so partial_dependence is NULL).
          if (is.null(model$importance)) {
            model$importance <- chaid_permutation_importance(
              model = model,
              data = importance_data,
              target = clean_target_col,
              predictors = c_cols,
              evaluation_data = evaluation_data_label,
              seed = seed,
              repeats = 10L
            )
          }
          imp_vars <- chaid_partial_dependence_vars(
            model$importance, c_cols, model$terms_mapping, max_pd_vars_eff
          )
          if (!is.null(model$partial_dependence)) {
            model$partial_dependence <- shrink_partial_dependence_data(
              model$partial_dependence, imp_vars
            )
          }
        }
      }

      model$imp_vars <- imp_vars
      # tam #38345: a numeric target gets the binned-mean "Actual" overlay too --
      # calc_partial_binning_data() takes the mean of the target within each
      # predictor bin, which is exactly the regression reading. Multiclass is
      # still excluded (handle_partial_dependence()'s partial_binning branch
      # handles regression and binary only).
      if (isTRUE(pd_with_bin_means) &&
          (isTRUE(is_target_logical) || identical(model$target_type, "numeric"))) {
        model$partial_binning <- calc_partial_binning_data(
          df, clean_target_col, imp_vars
        )
      }

      # formula_terms lets generic evaluation code find the target column name
      # (all.vars(model$formula_terms)[1]) in the test-evaluation path.
      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      fml <- stats::as.formula(paste0("`", clean_target_col, "` ~ ", rhs))
      model$formula_terms <- stats::terms(fml)
      attr(model$formula_terms, ".Environment") <- NULL
      model$orig_target_col <- target_col
      # CHAID also trains on cleanup_df(map_name = FALSE) names, so record the name
      # the training data carries alongside the original one (tam #37985).
      model$clean_target_col <- unname(clean_target_col)
      model$is_target_logical <- is_target_logical
      model$is_target_ordered <- is_target_ordered
      model$ordered_levels <- target_ordered_levels
      if (!is.null(target_funs)) {
        model$target_funs <- target_funs
      }
      if (!is.null(predictor_funs)) {
        model$orig_predictor_cols <- orig_selected_cols
        attr(predictor_funs, "LC_TIME") <- Sys.getlocale("LC_TIME")
        attr(predictor_funs, "sysname") <- Sys.info()[["sysname"]]
        attr(predictor_funs, "lubridate.week.start") <- getOption("lubridate.week.start")
        model$predictor_funs <- predictor_funs
      }
      model$sampled_nrow <- clean_df_ret$sampled_nrow

      list(model = model, test_index = test_index, source_data = source_data)
    }, error = function(e) {
      if (length(grouped_cols) > 0) {
        # Report per-group errors in the Summary table rather than aborting.
        class(e) <- c("chaid", class(e))
        list(model = e, test_index = NULL, source_data = NULL)
      } else {
        stop(e)
      }
    })
  }

  model_and_data_col <- "model_and_data"
  ret <- do_on_each_group(clean_df, each_func, name = model_and_data_col, with_unnest = FALSE)

  if (length(grouped_cols) > 0) {
    ret <- ret %>% tidyr::nest(-grouped_cols)
  } else {
    ret <- ret %>% tidyr::nest()
  }
  ret <- ret %>% dplyr::ungroup()

  ret <- ret %>%
    dplyr::mutate(model = purrr::imap(data, function(df, idx) {
      tryCatch(df[[model_and_data_col]][[1]]$model,
               error = function(e) stop(paste0(e$message, " (while extracting model from group ", idx, ")"), call. = FALSE))
    })) %>%
    dplyr::mutate(.test_index = purrr::imap(data, function(df, idx) {
      tryCatch(df[[model_and_data_col]][[1]]$test_index,
               error = function(e) stop(paste0(e$message, " (while extracting test_index from group ", idx, ")"), call. = FALSE))
    })) %>%
    dplyr::mutate(source.data = purrr::imap(data, function(df, idx) {
      tryCatch({
        d <- df[[model_and_data_col]][[1]]$source_data
        if (length(grouped_cols) > 0 && !is.null(d)) d %>% dplyr::select(-grouped_cols) else d
      }, error = function(e) stop(paste0(e$message, " (while extracting source.data from group ", idx, ")"), call. = FALSE))
    })) %>%
    dplyr::select(-data)

  if (length(grouped_cols) > 0) {
    ret <- ret %>% dplyr::rowwise(grouped_cols)
  } else {
    ret <- ret %>% dplyr::rowwise()
  }
  # .model passes viz-layer column-type validation; .model.chaid identifies the step.
  class(ret$model) <- c("list", ".model", ".model.chaid")
  ret
}

#' Predicted class for a CHAID model, applying the binary threshold.
#'
#' @param model A fitted `exploratory_chaid` model.
#' @param all_prediction Output of `chaid_predict(type = "all")`.
#' @param threshold Positive-class probability threshold (binary only).
#' @return A factor of predicted classes with levels `model$class_levels`.
chaid_predicted_class <- function(model, all_prediction, threshold = 0.5) {
  if (identical(model$classification_type, "binary")) {
    prob_true <- all_prediction[[".pred_prob_TRUE"]]
    labels <- ifelse(prob_true >= threshold, "TRUE", "FALSE")
  } else {
    labels <- as.character(all_prediction[[".pred_class"]])
  }
  factor(labels, levels = model$class_levels)
}

#' Positive-class (binary) or max-class (multiclass) probability.
#'
#' @param model A fitted `exploratory_chaid` model.
#' @param all_prediction Output of `chaid_predict(type = "all")`.
#' @return A numeric probability vector.
chaid_positive_probability <- function(model, all_prediction) {
  prob_cols <- grep("^\\.pred_prob_", names(all_prediction), value = TRUE)
  if (identical(model$classification_type, "binary")) {
    all_prediction[[".pred_prob_TRUE"]]
  } else {
    apply(as.matrix(all_prediction[, prob_cols, drop = FALSE]), 1, max)
  }
}

#' Convert CHAID probability output to a class-named matrix.
#'
#' `chaid_predict(type = "prob"|"all")` uses `.pred_prob_<class>` columns. The
#' shared Decision Tree report helpers (`multiclass_auc_by_class`,
#' `evaluate_by_class_report_metrics`) expect colnames to be the class levels.
#'
#' @param prediction A data frame / matrix from `chaid_predict`, or NULL.
#' @param class_levels Optional class order; columns are reordered when present.
#' @return A numeric matrix, or NULL when conversion is not possible.
chaid_as_probability_matrix <- function(prediction, class_levels = NULL) {
  if (is.null(prediction)) {
    return(NULL)
  }
  if (is.data.frame(prediction) || is.matrix(prediction)) {
    mat <- as.matrix(prediction)
  } else {
    return(NULL)
  }
  if (ncol(mat) == 0) {
    return(NULL)
  }
  colnames(mat) <- sub("^\\.pred_prob_", "", colnames(mat))
  if (!is.null(class_levels) && length(class_levels) > 0) {
    if (!all(class_levels %in% colnames(mat))) {
      return(NULL)
    }
    mat <- mat[, class_levels, drop = FALSE]
  }
  mat
}

#' Calculate FIRM variable importance for an `exploratory_chaid` model.
#'
#' `importance_firm()` is model-agnostic -- it reads only the partial-dependence
#' data frame and its `points` / `quantile_points` attributes -- so CHAID reuses
#' it verbatim, the same way ranger / rpart / xgboost / lightgbm / catboost do.
#' The only CHAID-specific work is conforming its 2-column output
#' (`variable`, `importance`, clean names) to the 7-column display-name schema
#' every other CHAID importance consumer expects (`chaid_partial_dependence_vars()`
#' reverse-maps display -> clean, and the report tables read the extra columns).
#'
#' @param partial_dependence Partial-dependence object from
#'   `partial_dependence.exploratory_chaid()`, covering every predictor.
#' @param model The fitted `exploratory_chaid` model (for `terms_mapping` and
#'   `classification_type`).
#' @param predictors Clean predictor column names the PD was computed over.
#' @param evaluation_data Source rows used for the partial-dependence curves.
#'   FIRM is derived from training partial dependence rather than held-out
#'   scoring, so callers should use `"Training"`.
#' @return A data frame with the stable CHAID importance schema, or `NULL` when
#'   FIRM cannot be calculated (no partial dependence available).
chaid_firm_importance <- function(partial_dependence, model, predictors,
                                  evaluation_data = 'Training') {
  if (is.null(partial_dependence) || length(predictors) == 0L) {
    return(NULL)
  }
  pdp_target_col <- if (identical(model$classification_type, "binary")) {
    "TRUE"
  } else {
    attr(partial_dependence, "target")
  }
  firm_df <- tryCatch(
    importance_firm(partial_dependence, pdp_target_col, predictors),
    error = function(e) NULL
  )
  if (is.null(firm_df) || !is.data.frame(firm_df) || nrow(firm_df) == 0L) {
    return(NULL)
  }

  # importance_firm() returns CLEAN names; the CHAID schema is display names.
  mapped_variable <- vapply(as.character(firm_df$variable), function(clean_name) {
    if (!is.null(model$terms_mapping) && clean_name %in% names(model$terms_mapping)) {
      return(unname(model$terms_mapping[[clean_name]]))
    }
    clean_name
  }, character(1), USE.NAMES = FALSE)

  result <- data.frame(
    variable = mapped_variable,
    importance = as.numeric(firm_df$importance),
    std_error = NA_real_,
    metric = 'firm',
    evaluation_data = evaluation_data,
    repeats = NA_integer_,
    stringsAsFactors = FALSE
  )
  result$rank <- ifelse(
    is.finite(result$importance),
    rank(-result$importance, ties.method = 'min'),
    NA_integer_
  )
  result %>%
    dplyr::arrange(is.na(rank), rank, variable) %>%
    dplyr::select(variable, importance, std_error, rank, metric,
                  evaluation_data, repeats)
}

#' Return an empty CHAID permutation-importance result.
#'
#' @return A data frame with the stable importance schema.
chaid_empty_permutation_importance <- function() {
  data.frame(
    variable = character(),
    importance = numeric(),
    std_error = numeric(),
    rank = integer(),
    metric = character(),
    evaluation_data = character(),
    repeats = integer(),
    stringsAsFactors = FALSE
  )
}

#' Calculate multiclass log loss for CHAID predictions.
#'
#' @param actual Actual target values.
#' @param prediction Output of chaid_predict(type = "all").
#' @param class_levels Model target levels.
#' @return A scalar log-loss value, or NA_real_ when no valid rows exist.
chaid_log_loss <- function(actual, prediction, class_levels) {
  probability_columns <- paste0('.pred_prob_', class_levels)
  if (!all(probability_columns %in% names(prediction))) {
    return(NA_real_)
  }
  actual_index <- match(as.character(actual), class_levels)
  probability_matrix <- as.matrix(prediction[, probability_columns, drop = FALSE])
  # Vectorized equivalent of apply(probability_matrix, 1, function(row) all(is.finite(row))).
  # This runs once per predictor per permutation repeat, so the row-wise apply was hot.
  valid <- !is.na(actual_index) & rowSums(!is.finite(probability_matrix)) == 0
  if (!any(valid)) {
    return(NA_real_)
  }
  row_index <- which(valid)
  probability <- probability_matrix[cbind(row_index, actual_index[valid])]
  probability <- pmax(pmin(probability, 1 - .Machine$double.eps), .Machine$double.eps)
  mean(-log(probability))
}

#' Calculate model-independent permutation importance for a CHAID model.
#'
#' @param model A fitted `exploratory_chaid` model.
#' @param data Evaluation rows containing target and predictors.
#' @param target Target column name.
#' @param predictors Predictor column names.
#' @param evaluation_data Label for the evaluation rows.
#' @param seed Random seed for reproducible permutations.
#' @param repeats Number of permutations per predictor.
#' @return A stable permutation-importance data frame.
chaid_permutation_importance <- function(model, data, target, predictors,
                                         evaluation_data = 'Training', seed = 1,
                                         repeats = 10L) {
  result <- chaid_empty_permutation_importance()
  if (!is.data.frame(data) || nrow(data) == 0 ||
      !target %in% names(data) || length(predictors) == 0) {
    return(result)
  }

  actual <- as.character(data[[target]])
  valid <- !is.na(actual) & actual %in% model$class_levels
  if (sum(valid) < 2L) {
    return(result)
  }
  evaluation_data_frame <- data[valid, , drop = FALSE]
  actual <- actual[valid]
  # Prepare the predictors and build the split lookup ONCE. Preparation is
  # element-wise, so permuting a prepared column is identical to preparing a
  # permuted column -- this just avoids redoing both for every repeat.
  prepared_data <- tryCatch(
    prepare_chaid_new_data(evaluation_data_frame, model),
    error = function(e) NULL
  )
  if (is.null(prepared_data)) {
    return(result)
  }
  split_index <- chaid_build_split_index(model)
  baseline_prediction <- tryCatch(
    chaid_predict_prepared(model, prepared_data, type = 'all',
                           split.index = split_index),
    error = function(e) NULL
  )
  if (is.null(baseline_prediction)) {
    return(result)
  }
  baseline_loss <- chaid_log_loss(actual, baseline_prediction, model$class_levels)
  if (!is.finite(baseline_loss)) {
    return(result)
  }

  if (length(seed) == 1L && is.finite(seed)) {
    set.seed(seed)
  } else {
    set.seed(1L)
  }
  repeat_count <- max(1L, as.integer(repeats))
  # Permuting a predictor the tree never splits on cannot change a single
  # prediction, so its drop is exactly 0 on every repeat (importance 0,
  # std_error 0). Detect those up front and skip their predictions entirely.
  # The RNG is still advanced identically, so every reported number stays
  # bit-for-bit the same as the per-predictor-prediction implementation.
  split_variables <- unique(unlist(
    lapply(model$.node_metadata, function(metadata) metadata$split_variable),
    use.names = FALSE
  ))
  evaluation_row_count <- nrow(prepared_data)
  rows <- lapply(predictors, function(variable) {
    if (!variable %in% names(evaluation_data_frame)) {
      return(NULL)
    }
    affects_prediction <- variable %in% split_variables &&
      variable %in% names(prepared_data)
    drops <- vapply(seq_len(repeat_count), function(iteration) {
      permutation <- sample.int(evaluation_row_count)
      if (!affects_prediction) {
        return(0)
      }
      permuted_data <- prepared_data
      permuted_data[[variable]] <- permuted_data[[variable]][permutation]
      permuted_prediction <- tryCatch(
        chaid_predict_prepared(model, permuted_data, type = 'all',
                               split.index = split_index),
        error = function(e) NULL
      )
      if (is.null(permuted_prediction)) {
        return(NA_real_)
      }
      permuted_loss <- chaid_log_loss(
        actual, permuted_prediction, model$class_levels
      )
      if (is.finite(permuted_loss)) permuted_loss - baseline_loss else NA_real_
    }, numeric(1))
    finite_drops <- drops[is.finite(drops)]
    if (length(finite_drops) == 0L) {
      importance <- NA_real_
      std_error <- NA_real_
    } else {
      importance <- mean(finite_drops)
      std_error <- if (length(finite_drops) > 1L) {
        stats::sd(finite_drops) / sqrt(length(finite_drops))
      } else {
        0
      }
    }
    mapped_variable <- if (!is.null(model$terms_mapping) &&
                           variable %in% names(model$terms_mapping)) {
      unname(model$terms_mapping[[variable]])
    } else {
      variable
    }
    data.frame(
      variable = mapped_variable,
      importance = importance,
      std_error = std_error,
      metric = 'log_loss',
      evaluation_data = evaluation_data,
      repeats = as.integer(repeat_count),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(result)
  }
  result <- dplyr::bind_rows(rows)
  result$rank <- ifelse(
    is.finite(result$importance),
    rank(-result$importance, ties.method = 'min'),
    NA_integer_
  )
  result <- result %>%
    dplyr::arrange(is.na(rank), rank, variable) %>%
    dplyr::select(variable, importance, std_error, rank, metric,
                  evaluation_data, repeats)
  result
}

#' Root-mean-squared-error loss for numeric CHAID predictions.
#'
#' @param actual Actual numeric target values.
#' @param predicted Predicted numeric values (`.pred_value` from
#'   `chaid_predict(type = "all")`).
#' @return A scalar RMSE, or NA_real_ when no valid rows exist.
chaid_rmse_loss <- function(actual, predicted) {
  valid <- is.finite(actual) & is.finite(predicted)
  if (!any(valid)) {
    return(NA_real_)
  }
  sqrt(mean((actual[valid] - predicted[valid])^2))
}

#' Model-independent permutation importance for a numeric-target CHAID model.
#'
#' Mirrors [chaid_permutation_importance()] (same permutation scheme, same
#' output schema, same "skip predictors the tree never split on" shortcut) but
#' scores each permutation by the RMSE INCREASE rather than the log-loss
#' increase, since a numeric target has no class probability to score. tam
#' #38166 (spec section 32-36's numeric branch has no importance requirement
#' of its own; this keeps `imp_vars`/the Importance report table populated
#' with a real, non-stubbed metric instead of leaving it empty).
#'
#' @param model A fitted `exploratory_chaid` model with `target_type == "numeric"`.
#' @param data Evaluation rows containing target and predictors.
#' @param target Target column name.
#' @param predictors Predictor column names.
#' @param evaluation_data Label for the evaluation rows.
#' @param seed Random seed for reproducible permutations.
#' @param repeats Number of permutations per predictor.
#' @return A stable permutation-importance data frame (`metric == 'rmse'`).
chaid_permutation_importance_numeric <- function(model, data, target, predictors,
                                                 evaluation_data = 'Training', seed = 1,
                                                 repeats = 10L) {
  result <- chaid_empty_permutation_importance()
  if (!is.data.frame(data) || nrow(data) == 0 ||
      !target %in% names(data) || length(predictors) == 0) {
    return(result)
  }

  actual <- as.numeric(data[[target]])
  valid <- is.finite(actual)
  if (sum(valid) < 2L) {
    return(result)
  }
  evaluation_data_frame <- data[valid, , drop = FALSE]
  actual <- actual[valid]
  prepared_data <- tryCatch(
    prepare_chaid_new_data(evaluation_data_frame, model),
    error = function(e) NULL
  )
  if (is.null(prepared_data)) {
    return(result)
  }
  split_index <- chaid_build_split_index(model)
  baseline_prediction <- tryCatch(
    chaid_predict_prepared(model, prepared_data, type = 'value',
                           split.index = split_index),
    error = function(e) NULL
  )
  if (is.null(baseline_prediction)) {
    return(result)
  }
  baseline_loss <- chaid_rmse_loss(actual, baseline_prediction)
  if (!is.finite(baseline_loss)) {
    return(result)
  }

  if (length(seed) == 1L && is.finite(seed)) {
    set.seed(seed)
  } else {
    set.seed(1L)
  }
  repeat_count <- max(1L, as.integer(repeats))
  split_variables <- unique(unlist(
    lapply(model$.node_metadata, function(metadata) metadata$split_variable),
    use.names = FALSE
  ))
  evaluation_row_count <- nrow(prepared_data)
  rows <- lapply(predictors, function(variable) {
    if (!variable %in% names(evaluation_data_frame)) {
      return(NULL)
    }
    affects_prediction <- variable %in% split_variables &&
      variable %in% names(prepared_data)
    drops <- vapply(seq_len(repeat_count), function(iteration) {
      permutation <- sample.int(evaluation_row_count)
      if (!affects_prediction) {
        return(0)
      }
      permuted_data <- prepared_data
      permuted_data[[variable]] <- permuted_data[[variable]][permutation]
      permuted_prediction <- tryCatch(
        chaid_predict_prepared(model, permuted_data, type = 'value',
                               split.index = split_index),
        error = function(e) NULL
      )
      if (is.null(permuted_prediction)) {
        return(NA_real_)
      }
      permuted_loss <- chaid_rmse_loss(actual, permuted_prediction)
      if (is.finite(permuted_loss)) permuted_loss - baseline_loss else NA_real_
    }, numeric(1))
    finite_drops <- drops[is.finite(drops)]
    if (length(finite_drops) == 0L) {
      importance <- NA_real_
      std_error <- NA_real_
    } else {
      importance <- mean(finite_drops)
      std_error <- if (length(finite_drops) > 1L) {
        stats::sd(finite_drops) / sqrt(length(finite_drops))
      } else {
        0
      }
    }
    mapped_variable <- if (!is.null(model$terms_mapping) &&
                           variable %in% names(model$terms_mapping)) {
      unname(model$terms_mapping[[variable]])
    } else {
      variable
    }
    data.frame(
      variable = mapped_variable,
      importance = importance,
      std_error = std_error,
      metric = 'rmse',
      evaluation_data = evaluation_data,
      repeats = as.integer(repeat_count),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(result)
  }
  result <- dplyr::bind_rows(rows)
  result$rank <- ifelse(
    is.finite(result$importance),
    rank(-result$importance, ties.method = 'min'),
    NA_integer_
  )
  result <- result %>%
    dplyr::arrange(is.na(rank), rank, variable) %>%
    dplyr::select(variable, importance, std_error, rank, metric,
                  evaluation_data, repeats)
  result
}

#' Choose predictor columns for CHAID partial dependence, by importance order.
#'
#' @param importance Importance table from `chaid_permutation_importance()`.
#' @param predictors Clean predictor column names present in the training frame.
#' @param terms_mapping Named character vector mapping clean -> display names.
#' @param max_pd_vars Maximum number of variables to keep.
#' @return Character vector of clean predictor names.
chaid_partial_dependence_vars <- function(importance, predictors, terms_mapping,
                                          max_pd_vars = 20L) {
  predictors <- as.character(predictors)
  max_pd_vars <- max(1L, as.integer(max_pd_vars))
  if (length(predictors) == 0L) {
    return(character())
  }
  if (is.null(importance) || !is.data.frame(importance) ||
      nrow(importance) == 0L || !"variable" %in% names(importance)) {
    return(predictors[seq_len(min(length(predictors), max_pd_vars))])
  }

  display_ordered <- as.character(importance$variable)
  clean_ordered <- vapply(display_ordered, function(display_name) {
    if (!is.null(terms_mapping) && length(terms_mapping) > 0) {
      hits <- names(terms_mapping)[terms_mapping == display_name]
      if (length(hits) >= 1L) {
        return(hits[[1]])
      }
    }
    display_name
  }, character(1), USE.NAMES = FALSE)
  clean_ordered <- unique(clean_ordered[clean_ordered %in% predictors])
  if (length(clean_ordered) == 0L) {
    return(predictors[seq_len(min(length(predictors), max_pd_vars))])
  }
  clean_ordered[seq_len(min(length(clean_ordered), max_pd_vars))]
}

#' Build a partial-dependence object for an `exploratory_chaid` model.
#'
#' Mirrors `partial_dependence.rpart()` so `handle_partial_dependence()` and
#' the Analytics View `rf_partial_dependence()` preprocessor work unchanged.
#'
#' @param fit A fitted `exploratory_chaid` model.
#' @param target Clean target column name.
#' @param vars Predictor column names to evaluate.
#' @param n Grid / sample sizes passed to `mmpf::marginalPrediction`.
#' @param interaction Whether to compute interactions (unused; always FALSE).
#' @param uniform Unused; kept for API parity with the rpart helper.
#' @param data Training data frame used for the grid and sampling.
#' @param ... Additional arguments forwarded to `mmpf::marginalPrediction`.
#' @return A `pd` data frame with attributes, or NULL if mmpf is unavailable.
partial_dependence.exploratory_chaid <- function(fit, target,
                                                 vars = colnames(data),
                                                 n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L),
                                                       nrow(data)),
                                                 interaction = FALSE,
                                                 uniform = TRUE,
                                                 data, ...) {
  if (!requireNamespace("mmpf", quietly = TRUE)) {
    return(NULL)
  }
  if (length(vars) == 0L) {
    return(NULL)
  }

  # tam #38345: a numeric target has no class-probability distribution
  # (chaid_predict_prepared() returns a zero-column frame for type = "prob"), so
  # PD predicts the node MEAN instead -- the same numeric-vector contract
  # partial_dependence.rpart() uses for its own regression case.
  is.numeric.target <- identical(fit$target_type, "numeric")

  # Default S3 predict() returns class labels; PD needs class probabilities with
  # the same column names (TRUE/FALSE or class levels) that handle_partial_dependence
  # expects from rpart/ranger.
  predict.fun <- function(object, newdata) {
    if (is.numeric.target) {
      return(as.numeric(predict(object, newdata, type = "value")))
    }
    prob <- as.data.frame(
      predict(object, newdata, type = "prob"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    colnames(prob) <- sub("^\\.pred_prob_", "", colnames(prob))
    as.matrix(prob)
  }

  points <- list()
  quantile_points <- list()
  for (cname in vars) {
    if (is.numeric(data[[cname]])) {
      coldata <- data[[cname]]
      minv <- min(coldata, na.rm = TRUE)
      maxv <- max(coldata, na.rm = TRUE)
      grid <- minv + (0:20) / 20 * (maxv - minv)
      quantile_grid <- stats::quantile(coldata, probs = 1:24 / 25)
      quantile_points[[cname]] <- quantile_grid
      points[[cname]] <- sort(unique(c(grid, quantile_grid)))
    } else {
      points[[cname]] <- unique(data[[cname]])
    }
  }

  args <- list(
    data = data,
    vars = vars,
    n = n,
    model = fit,
    points = points,
    predict.fun = predict.fun,
    ...
  )

  if (length(vars) > 1L && !isTRUE(interaction)) {
    pd <- data.table::rbindlist(sapply(vars, function(x) {
      args$vars <- x
      if ("points" %in% names(args)) {
        args$points <- args$points[x]
      }
      mp <- do.call(mmpf::marginalPrediction, args)
      # A vector-returning predict.fun leaves mmpf's own column name on the
      # prediction; handle_partial_dependence() finds it by attr(pd, "target").
      if (is.numeric.target) {
        names(mp)[ncol(mp)] <- target
      }
      mp
    }, simplify = FALSE), fill = TRUE)
    data.table::setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else {
    pd <- do.call(mmpf::marginalPrediction, args)
    if (is.numeric.target) {
      names(pd)[ncol(pd)] <- target
    }
  }

  attr(pd, "class") <- c("pd", "data.frame")
  attr(pd, "interaction") <- isTRUE(interaction)
  attr(pd, "target") <- if (is.numeric.target ||
                            identical(fit$classification_type, "binary")) {
    target
  } else {
    fit$class_levels
  }
  attr(pd, "vars") <- vars
  attr(pd, "points") <- points
  attr(pd, "quantile_points") <- quantile_points
  pd
}

#' Augment data with CHAID predictions (broom S3 method).
#'
#' Supports both the `data =` and `newdata =` calling conventions used by
#' prediction(); predictions are computed directly from the supplied rows, so no
#' precomputed train/test predictions are required. Adds `predicted_label`,
#' `predicted_probability`, and (multiclass) `predicted_probability_<class>`.
#'
#' @param x A fitted `exploratory_chaid` model.
#' @param data Rows to predict on (training / test path).
#' @param newdata Rows to predict on (new-data path).
#' @param data_type Ignored; predictions come from the supplied rows.
#' @param binary_classification_threshold Positive-class threshold (binary).
#' @param ... Unused.
#' @return The supplied data frame with prediction columns appended.
#' @export
augment.exploratory_chaid <- function(x, data = NULL, newdata = NULL,
                                      data_type = "training",
                                      binary_classification_threshold = 0.5, ...) {
  if ("error" %in% class(x)) {
    return(data.frame())
  }
  frame <- if (!is.null(newdata)) newdata else data
  if (is.null(frame)) {
    stop("data or newdata have to be indicated.")
  }
  if (nrow(frame) == 0) {
    return(frame)
  }

  # Replay predictor transformations for the new-data path.
  if (!is.null(newdata) && !is.null(x$predictor_funs)) {
    frame <- frame %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
  }

  all_prediction <- chaid_predict(x, frame, type = "all")

  if (identical(x$target_type, "numeric")) {
    # tam #38166 spec section 19 (Prediction Data): Predicted Value, Node ID,
    # and (when the actual target is present in `frame`) Residual = Actual -
    # Predicted.
    predicted_value_col <- avoid_conflict(colnames(frame), "predicted_value")
    node_id_col <- avoid_conflict(colnames(frame), "chaid_node_id")
    frame[[predicted_value_col]] <- all_prediction[[".pred_value"]]
    frame[[node_id_col]] <- all_prediction[[".chaid_node_id"]]
    target_col_in_frame <- if (!is.null(x$orig_target_col) &&
                                x$orig_target_col %in% names(frame)) {
      x$orig_target_col
    } else if (!is.null(x$clean_target_col) &&
               x$clean_target_col %in% names(frame)) {
      x$clean_target_col
    } else {
      NULL
    }
    if (!is.null(target_col_in_frame)) {
      residual_col <- avoid_conflict(colnames(frame), "residual")
      frame[[residual_col]] <- as.numeric(frame[[target_col_in_frame]]) -
        all_prediction[[".pred_value"]]
    }
    return(frame)
  }

  predicted_label <- as.character(chaid_predicted_class(
    x, all_prediction, binary_classification_threshold
  ))
  predicted_probability <- chaid_positive_probability(x, all_prediction)

  predicted_label_col <- avoid_conflict(colnames(frame), "predicted_label")
  predicted_probability_col <- avoid_conflict(colnames(frame), "predicted_probability")
  frame[[predicted_label_col]] <- predicted_label
  frame[[predicted_probability_col]] <- predicted_probability

  if (identical(x$classification_type, "multi")) {
    for (cls in x$class_levels) {
      col <- avoid_conflict(colnames(frame), paste0("predicted_probability_", cls))
      frame[[col]] <- all_prediction[[paste0(".pred_prob_", cls)]]
    }
  }
  frame
}

#' glance for a CHAID model (broom S3 method).
#'
#' @param x A fitted `exploratory_chaid` model.
#' @param pretty.name Whether to use display-friendly column names.
#' @param report_metrics Whether to include Decision Tree report extras
#'   (ROC AUC / PR AUC / …).
#' @param ... Unused.
#' @return A one-row model summary data frame.
#' @export
glance.exploratory_chaid <- function(x, pretty.name = FALSE, report_metrics = FALSE, ...) {
  if ("error" %in% class(x)) {
    return(data.frame(Note = x$message))
  }
  tidy.exploratory_chaid(x, type = "evaluation", pretty.name = pretty.name,
                         report_metrics = report_metrics, ...)
}

#' tidy for a CHAID model (broom S3 method).
#'
#' @param x A fitted `exploratory_chaid` model.
#' @param type One of `evaluation`, `evaluation_by_class`, `conf_mat`,
#'   `tree_nodes`, `node_summary`, `rules`, `category_merges`, `split_summary`,
#'   `category_error_distribution`, `numeric_intervals`, `importance`, or
#'   `partial_dependence`.
#' @param pretty.name Whether to use display-friendly column names.
#' @param binary_classification_threshold Positive-class threshold (binary).
#' @param report_metrics Whether to include Decision Tree report extras
#'   (ROC AUC / PR AUC / Balanced Accuracy / Specificity for binary;
#'   Macro AUCs for multiclass; One-vs-Rest AUCs for evaluation_by_class).
#' @param ... Unused.
#' @return A data frame whose shape depends on `type`.
#' @export
#' Shift node ids to the 0-based numbering shown to users.
#'
#' The model numbers nodes from 1 -- root = 1 is assumed in several places
#' (chaid_assign_nodes seeds its queue with it, root row counts are looked up by
#' it), so the model keeps 1-based ids and only the tidy output is shifted. SPSS
#' labels the root "Node 0", and every id the user sees comes through
#' tidy.exploratory_chaid, so shifting once here keeps the chart, the split /
#' evidence / merge / interval tables and the rules on one numbering.
#'
#' Only genuine id columns are shifted. `Parent Node Rows` (a count) and `Depth`
#' are deliberately not in the list.
#'
#' @param df A tidy output data frame.
#' @return The same frame with node id columns shifted to 0-based.
chaid_display_node_ids <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  for (col in c('node_id', 'parent_id', 'Node')) {
    if (col %in% names(df)) {
      df[[col]] <- as.integer(df[[col]]) - 1L
    }
  }
  df
}

tidy.exploratory_chaid <- function(x, type = "evaluation", pretty.name = FALSE,
                                   binary_classification_threshold = 0.5,
                                   report_metrics = FALSE, ...) {
  if ("error" %in% class(x) && type != "evaluation") {
    return(data.frame())
  }
  actual <- x$y
  # tam #38166: a numeric target has no class to re-threshold/predict -- most
  # `type`s below are classification-only and stay unreachable for it (the
  # types CHAID's own report/prediction pipeline actually calls for a numeric
  # model -- node_summary, rules, category_merges, split_summary,
  # numeric_intervals, importance, evaluation -- are all target-type-aware).
  if (identical(x$target_type, "numeric")) {
    return(chaid_display_node_ids(switch(
      type,
      evaluation = {
        if ("error" %in% class(x)) {
          return(data.frame(Note = x$message))
        }
        ret <- evaluate_regression_(
          data.frame(.pred = x$predicted_value, .actual = actual),
          ".pred", ".actual"
        )
        # Mirror the pretty-name mapping used elsewhere for regression
        # evaluation (rpart / xgboost / lightgbm regression `tidy()`).
        if (pretty.name) {
          ret <- ret %>%
            dplyr::rename(
              `R Squared` = r_squared,
              RMSE = root_mean_square_error,
              MAE = mean_absolute_error,
              MAPE = mean_absolute_percentage_error
            )
        }
        ret
      },
      tree_nodes = build_chaid_tree_nodes(x),
      node_summary = chaid_node_summary(x),
      rules = chaid_rule_table(x),
      category_merges = chaid_category_merge_table(x),
      split_summary = chaid_split_summary(x),
      category_error_distribution = chaid_category_error_distribution(x),
      numeric_intervals = chaid_numeric_intervals(x),
      importance = {
        if (is.null(x$importance)) chaid_empty_permutation_importance() else x$importance
      },
      partial_dependence = handle_partial_dependence(x),
      {
        stop(paste0("type ", type, " is not defined for a numeric CHAID target"))
      }
    )))
  }
  # Re-threshold binary labels so Settings → cut point updates F1 / Accuracy etc.
  # (ROC / PR AUC use predicted_prob and are threshold-independent.)
  predicted <- if (identical(x$classification_type, "binary") &&
                     !is.null(x$predicted_prob)) {
    factor(
      ifelse(x$predicted_prob >= binary_classification_threshold, "TRUE", "FALSE"),
      levels = x$class_levels
    )
  } else {
    x$predicted_class
  }
  chaid_display_node_ids(switch(
    type,
    evaluation = {
      if ("error" %in% class(x)) {
        return(data.frame(Note = x$message))
      }
      if (identical(x$classification_type, "binary")) {
        evaluate_binary_classification(actual, predicted, x$predicted_prob,
                                       pretty.name = pretty.name, is_rpart = FALSE,
                                       report_metrics = report_metrics)
      } else {
        ret <- evaluate_multi_(data.frame(predicted = predicted, actual = actual),
                               "predicted", "actual", pretty.name = pretty.name)
        # Mirror tidy.rpart: Macro ROC/PR AUC for the Decision Tree report (#37156).
        if (report_metrics) {
          balanced_accuracy <- multiclass_balanced_accuracy(actual, predicted)
          auc_by_class <- multiclass_auc_by_class(actual, x$predicted_prob_matrix)
          macro_roc_auc <- if (nrow(auc_by_class) > 0) mean(auc_by_class$roc_auc, na.rm = TRUE) else NA_real_
          macro_pr_auc <- if (nrow(auc_by_class) > 0) mean(auc_by_class$pr_auc, na.rm = TRUE) else NA_real_
          extra <- if (pretty.name) {
            tibble::tibble(`Balanced Accuracy` = balanced_accuracy,
                           `Macro ROC AUC` = macro_roc_auc,
                           `Macro PR AUC` = macro_pr_auc)
          } else {
            tibble::tibble(balanced_accuracy = balanced_accuracy,
                           macro_roc_auc = macro_roc_auc,
                           macro_pr_auc = macro_pr_auc)
          }
          ret <- dplyr::bind_cols(ret, extra)
        }
        ret
      }
    },
    evaluation_by_class = {
      per_level <- function(level) {
        evaluate_classification(actual, predicted, level, pretty.name = pretty.name)
      }
      ret <- dplyr::bind_rows(lapply(x$class_levels, per_level))
      if (report_metrics && nrow(ret) > 0) {
        ret <- dplyr::bind_cols(ret, evaluate_by_class_report_metrics(
          actual, predicted, x$class_levels, x$predicted_prob_matrix, pretty.name))
      }
      ret
    },
    conf_mat = {
      calc_conf_mat(actual, predicted)
    },
    tree_nodes = {
      build_chaid_tree_nodes(x)
    },
    node_summary = {
      chaid_node_summary(x)
    },
    rules = {
      chaid_rule_table(x)
    },
    category_merges = {
      chaid_category_merge_table(x)
    },
    split_summary = {
      chaid_split_summary(x)
    },
    category_error_distribution = {
      chaid_category_error_distribution(x)
    },
    numeric_intervals = {
      chaid_numeric_intervals(x)
    },
    importance = {
      if (is.null(x$importance)) chaid_empty_permutation_importance() else x$importance
    },
    partial_dependence = {
      handle_partial_dependence(x)
    },
    {
      stop(paste0("type ", type, " is not defined"))
    }
  ))
}

#' Build per-node data for the interactive decision tree chart.
#'
#' Emits the same schema as build_rpart_tree_nodes() so the existing N-ary tree
#' renderer draws CHAID's multiway splits with no front-end changes.
#'
#' @param x A fitted `exploratory_chaid` model.
#' @return A data frame with one row per node.
build_chaid_tree_nodes <- function(x) {
  nodes <- x$nodes
  edges <- x$edges
  root_n <- nodes$n[nodes$node_id == 1L]
  class_levels <- x$class_levels
  is_reg <- identical(x$target_type, "numeric")

  map_name <- function(v) {
    tm <- x$terms_mapping
    v <- as.character(v)
    if (!is.null(tm) && v %in% names(tm)) unname(tm[v]) else v
  }

  # Positive class first for a 2-class target (SPSS-style ordering).
  ord <- seq_along(class_levels)
  if (!is_reg && length(class_levels) == 2) {
    up <- toupper(class_levels)
    positive_idx <- if (setequal(up, c("FALSE", "TRUE"))) which(up == "TRUE")
                    else if (setequal(up, c("NO", "YES"))) which(up == "YES")
                    else NA_integer_
    if (!is.na(positive_idx)) {
      ord <- c(positive_idx, setdiff(seq_along(class_levels), positive_idx))
    }
  }

  # tam #38166: numeric-target-only. A shared-breaks target histogram per
  # node, mirroring build_rpart_tree_nodes()'s regression branch -- every
  # node's counts are on the SAME bins so shapes are comparable across the
  # tree. Node membership is reconstructed from x$train_node_ids (the leaf/
  # internal node each training row resolves to) walked up to the root via
  # `nodes$parent_id`, since CHAID node ids are not a binary heap (no `%/% 2`
  # shortcut the way rpart's are).
  dist_by_id <- NULL
  shared_breaks <- NULL
  if (is_reg) {
    yv <- x$y
    train_ids <- x$train_node_ids
    if (!is.null(yv) && length(yv) > 0 && !is.null(train_ids) &&
        length(train_ids) == length(yv)) {
      yfin <- yv[is.finite(yv)]
      if (length(yfin) >= 2 && diff(range(yfin)) > 0) {
        y_min <- min(yfin)
        y_max <- max(yfin)
        if (all(yfin == floor(yfin)) && (y_max - y_min) <= 30) {
          shared_breaks <- seq(y_min - 0.5, y_max + 0.5, by = 1)
        } else {
          shared_breaks <- seq(y_min, y_max, length.out = 21)
        }
      } else if (length(yfin) >= 1) {
        shared_breaks <- c(yfin[1] - 0.5, yfin[1] + 0.5)
      }
      if (!is.null(shared_breaks)) {
        nbin <- length(shared_breaks) - 1L
        parent_of <- stats::setNames(as.character(nodes$parent_id), as.character(nodes$node_id))
        dist_by_id <- list()
        for (r in seq_along(yv)) {
          v <- yv[r]
          if (!is.finite(v)) next
          bi <- .bincode(v, shared_breaks, include.lowest = TRUE)
          if (is.na(bi)) next
          nd <- train_ids[r]
          if (is.na(nd)) next
          repeat {
            key <- as.character(nd)
            cc <- dist_by_id[[key]]
            if (is.null(cc)) cc <- integer(nbin)
            cc[bi] <- cc[bi] + 1L
            dist_by_id[[key]] <- cc
            parent_key <- parent_of[[key]]
            if (is.null(parent_key) || is.na(parent_key)) break
            nd <- parent_key
          }
        }
      }
    }
  }
  dist_json_for <- function(id) {
    if (!is_reg || is.null(shared_breaks)) return(NA_character_)
    cc <- dist_by_id[[as.character(id)]]
    if (is.null(cc)) cc <- integer(length(shared_breaks) - 1L)
    as.character(jsonlite::toJSON(list(breaks = shared_breaks, counts = cc),
                                  digits = 10, auto_unbox = FALSE))
  }

  rows <- lapply(seq_len(nrow(nodes)), function(i) {
    id <- nodes$node_id[i]
    node_n <- nodes$n[i]
    if (is_reg) {
      class_json <- NA_character_
    } else {
      distribution <- nodes$class_distribution[[i]]
      counts <- as.numeric(distribution) * node_n
      arr <- lapply(ord, function(j) {
        list(label = class_levels[j], n = counts[j],
             pct = if (node_n > 0) counts[j] / node_n else 0)
      })
      class_json <- as.character(jsonlite::toJSON(arr, auto_unbox = TRUE, digits = NA))
    }

    # Split-test stats belong to nodes that actually split; NA everywhere else
    # (leaves, and nodes whose best candidate failed the alpha_split gate).
    is_split <- !isTRUE(nodes$is_terminal[i])

    edge_row <- which(edges$child_id == id)
    if (length(edge_row) == 1) {
      cond_column <- map_name(edges$split_variable[edge_row])
      original_categories <- strsplit(edges$original_categories[edge_row], " \\| ")[[1]]
      # tam #37177: a branch built from a run of contiguous numeric bins reads
      # as the range it covers ("<= 2317.6, (2317.6, 2695.8]" -> "<= 2695.8").
      # cond_value stays the collapsed bin/category labels so DTreeGenerator's
      # Show Detail filter (binLabelsToRangeConditions) can parse them. The
      # displayed edge_label is rewritten to the same readable inequalities
      # used by node_summary / rules ("給料 = <= 2695.8" -> "給料 <= 2695.8",
      # "給料 = (2695.8, 4228.8]" -> "2695.8 < 給料 <= 4228.8") so the
      # characteristic-groups Condition column and tree chart match CART.
      display_categories <- chaid_collapse_intervals(original_categories)
      edge_label <- chaid_readable_one_condition(
        paste0(cond_column, " in {",
               paste(display_categories, collapse = CHAID_GROUP_SEPARATOR), "}")
      )
      cond_value <- as.character(jsonlite::toJSON(as.character(display_categories)))
    } else {
      cond_column <- NA_character_
      edge_label <- ""
      cond_value <- NA_character_
    }

    data.frame(
      node_id = as.integer(id),
      parent_id = if (is.na(nodes$parent_id[i])) NA_integer_ else as.integer(nodes$parent_id[i]),
      depth = as.integer(nodes$depth[i]),
      is_leaf = nodes$is_terminal[i],
      edge_label = edge_label,
      predicted = as.character(nodes$predicted_class[i]),
      n = as.integer(node_n),
      pct = if (root_n > 0) node_n / root_n else 0,
      class_json = class_json,
      cond_column = cond_column,
      cond_operator = if (is.na(cond_column)) NA_character_ else "in",
      cond_value = cond_value,
      mean_value = if (is_reg) as.numeric(nodes$node_mean[i]) else NA_real_,
      sd_value = if (is_reg) as.numeric(nodes$node_sd[i]) else NA_real_,
      # sample SD (n-1 denominator, `node_sd`) -> population SD (n denominator)
      # so rmse_value reads as the within-node root-mean-squared deviation
      # from the mean, matching build_rpart_tree_nodes()'s sqrt(dev/n).
      rmse_value = if (is_reg && node_n > 0 && is.finite(nodes$node_sd[i])) {
        as.numeric(nodes$node_sd[i]) * sqrt(max(node_n - 1, 0) / node_n)
      } else {
        NA_real_
      },
      dist_json = dist_json_for(id),
      # CHAID split test at this node (NA for leaves). The interactive tree
      # renderer shows these on the splitting (parent) node, SPSS-style.
      p_value = if (is_split) nodes$p_value[i] else NA_real_,
      adjusted_p_value = if (is_split) nodes$adjusted_p_value[i] else NA_real_,
      split_statistic = if (is_split) nodes$split_statistic[i] else NA_real_,
      split_df = if (is_split) nodes$split_df[i] else NA_real_,
      split_df1 = if (is_split) nodes$split_df1[i] else NA_real_,
      split_df2 = if (is_split) nodes$split_df2[i] else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}
