# Analytics View integration layer for the CHAID engine in chaid.R.
#
# exp_chaid() wraps chaid_fit() in the standard model-data-frame shape (one row
# per group: `model`, `.test_index`, `source.data`) used by every Analytics View
# decision-tree/model template, and the augment/tidy/glance S3 methods below let
# the framework's generic preprocessors (prediction(), rf_evaluation_*,
# tidy_rowwise()) dispatch on an `exploratory_chaid` model. This mirrors the
# exp_rpart() pattern in randomForest_tidiers.R, minus regression, SMOTE, and
# partial dependence. Model-independent permutation importance is calculated
# from the training or held-out rows and stored as a compact result table.

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

  if (is.numeric(df[[target_col]])) {
    stop("CHAID supports categorical target variables only.")
  }

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

      unique_val <- unique(df[[clean_target_col]])
      if (length(unique_val[!is.na(unique_val)]) <= 1) {
        stop("Categorical Target Variable must have 2 or more unique values.")
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
      model$classification_type <- if (model$target_type == "logical") "binary" else "multi"
      model$original_factor_levels <- original_factor_levels

      # Store training actual / predicted so tidy() evaluation and conf_mat can
      # reuse the shared model-agnostic evaluation helpers.
      actual <- factor(as.character(df[[clean_target_col]]), levels = model$class_levels)
      train_all <- chaid_predict(model, df, type = "all")
      model$y <- actual
      model$predicted_class <- chaid_predicted_class(
        model, train_all, binary_classification_threshold
      )
      model$predicted_prob <- chaid_positive_probability(model, train_all)

      # Metadata expected by the framework.
      model$terms_mapping <- names(group_name_map)
      names(model$terms_mapping) <- group_name_map

      importance_data <- if (length(test_index) > 0) {
        source_data[test_index, , drop = FALSE]
      } else {
        df
      }
      model$importance <- chaid_permutation_importance(
        model = model,
        data = importance_data,
        target = clean_target_col,
        predictors = c_cols,
        evaluation_data = if (length(test_index) > 0) "Test" else "Training",
        seed = seed,
        repeats = 10L
      )
      # formula_terms lets generic evaluation code find the target column name
      # (all.vars(model$formula_terms)[1]) in the test-evaluation path.
      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      fml <- stats::as.formula(paste0("`", clean_target_col, "` ~ ", rhs))
      model$formula_terms <- stats::terms(fml)
      attr(model$formula_terms, ".Environment") <- NULL
      model$orig_target_col <- target_col
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
#' @param ... Unused.
#' @return A one-row model summary data frame.
#' @export
glance.exploratory_chaid <- function(x, pretty.name = FALSE, ...) {
  if ("error" %in% class(x)) {
    return(data.frame(Note = x$message))
  }
  tidy.exploratory_chaid(x, type = "evaluation", pretty.name = pretty.name, ...)
}

#' tidy for a CHAID model (broom S3 method).
#'
#' @param x A fitted `exploratory_chaid` model.
#' @param type One of `evaluation`, `evaluation_by_class`, `conf_mat`,
#'   `tree_nodes`, `node_summary`, `rules`, `category_merges`, `split_summary`,
#'   `category_error_distribution`, `numeric_intervals`, or `importance`.
#' @param pretty.name Whether to use display-friendly column names.
#' @param binary_classification_threshold Positive-class threshold (binary).
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
                                   binary_classification_threshold = 0.5, ...) {
  if ("error" %in% class(x) && type != "evaluation") {
    return(data.frame())
  }
  actual <- x$y
  predicted <- x$predicted_class
  chaid_display_node_ids(switch(
    type,
    evaluation = {
      if ("error" %in% class(x)) {
        return(data.frame(Note = x$message))
      }
      if (identical(x$classification_type, "binary")) {
        evaluate_binary_classification(actual, predicted, x$predicted_prob,
                                       pretty.name = pretty.name, is_rpart = FALSE)
      } else {
        evaluate_multi_(data.frame(predicted = predicted, actual = actual),
                        "predicted", "actual", pretty.name = pretty.name)
      }
    },
    evaluation_by_class = {
      per_level <- function(level) {
        evaluate_classification(actual, predicted, level, pretty.name = pretty.name)
      }
      dplyr::bind_rows(lapply(x$class_levels, per_level))
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

  map_name <- function(v) {
    tm <- x$terms_mapping
    v <- as.character(v)
    if (!is.null(tm) && v %in% names(tm)) unname(tm[v]) else v
  }

  # Positive class first for a 2-class target (SPSS-style ordering).
  ord <- seq_along(class_levels)
  if (length(class_levels) == 2) {
    up <- toupper(class_levels)
    positive_idx <- if (setequal(up, c("FALSE", "TRUE"))) which(up == "TRUE")
                    else if (setequal(up, c("NO", "YES"))) which(up == "YES")
                    else NA_integer_
    if (!is.na(positive_idx)) {
      ord <- c(positive_idx, setdiff(seq_along(class_levels), positive_idx))
    }
  }

  rows <- lapply(seq_len(nrow(nodes)), function(i) {
    id <- nodes$node_id[i]
    node_n <- nodes$n[i]
    distribution <- nodes$class_distribution[[i]]
    counts <- as.numeric(distribution) * node_n
    arr <- lapply(ord, function(j) {
      list(label = class_levels[j], n = counts[j],
           pct = if (node_n > 0) counts[j] / node_n else 0)
    })
    class_json <- as.character(jsonlite::toJSON(arr, auto_unbox = TRUE, digits = NA))

    # Split-test stats belong to nodes that actually split; NA everywhere else
    # (leaves, and nodes whose best candidate failed the alpha_split gate).
    is_split <- !isTRUE(nodes$is_terminal[i])

    edge_row <- which(edges$child_id == id)
    if (length(edge_row) == 1) {
      cond_column <- map_name(edges$split_variable[edge_row])
      original_categories <- strsplit(edges$original_categories[edge_row], " \\| ")[[1]]
      # tam #37177: a branch built from a run of contiguous numeric bins reads
      # as the range it covers ("<= 2317.6, (2317.6, 2695.8]" -> "<= 2695.8").
      # cond_value is collapsed IN LOCKSTEP because DTreeGenerator rebuilds the
      # edge label from cond_value, and its Show Detail filter
      # (binLabelsToRangeConditions, min-lo/max-hi) parses the collapsed labels
      # to the same range -- CHAID merges only ADJACENT ordered bins, so no gap
      # can make the two differ. Categorical members pass through untouched.
      display_categories <- chaid_collapse_intervals(original_categories)
      edge_label <- paste0(cond_column, " = ", paste(display_categories, collapse = ", "))
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
      mean_value = NA_real_,
      sd_value = NA_real_,
      rmse_value = NA_real_,
      dist_json = NA_character_,
      # CHAID split test at this node (NA for leaves). The interactive tree
      # renderer shows these on the splitting (parent) node, SPSS-style.
      p_value = if (is_split) nodes$p_value[i] else NA_real_,
      adjusted_p_value = if (is_split) nodes$adjusted_p_value[i] else NA_real_,
      split_statistic = if (is_split) nodes$split_statistic[i] else NA_real_,
      split_df = if (is_split) nodes$split_df[i] else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}
