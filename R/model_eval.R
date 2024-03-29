#' Non standard evaluation version of do_roc_
#' @param df Data frame
#' @param pred_prob Column name for probability
#' @param actual_val Column name for actual values
#' @export
do_roc <- function(df, pred_prob, actual_val, ...){
  pred_prob_col <- col_name(substitute(pred_prob))
  actual_val_col <- col_name(substitute(actual_val))
  do_roc_(df, pred_prob_col, actual_val_col, ...)
}

#' Return cordinations for ROC curve
#' @param df Data frame
#' @param pred_prob_col Column name for probability
#' @param actual_val_col Column name for actual values
#' @param grid Grid size to reduce data size for drawing chart.
#' @param with_auc When set to TRUE, AUC is calculated and added as a column of the output.
#' @export
do_roc_ <- function(df, pred_prob_col, actual_val_col, grid = NULL, with_auc = FALSE){
  validate_empty_data(df)

  group_cols <- grouped_by(df)

  tpr_col <- avoid_conflict(group_cols, "true_positive_rate")
  fpr_col <- avoid_conflict(group_cols, "false_positive_rate")

  do_roc_each <- function(df){
    # filter out NAs upfront.
    df <- df %>% dplyr::filter(!is.na(!!rlang::sym(pred_prob_col)) & !is.na(!!rlang::sym(actual_val_col)))
    df[[actual_val_col]] <- binary_label(df[[actual_val_col]])

    if (with_auc) { # Calculate AUC.
      auc <- auroc(df[[pred_prob_col]], df[[actual_val_col]])
    }

    # sort descending order by predicted probability
    arranged <- df[order(-df[[pred_prob_col]]), ]

    # and get actual values
    val <- arranged[[actual_val_col]]

    act_sum <- sum(val)

    fpr <- if (all(val)){
      # should be all zero but put 1 to draw a curve
      c(rep(0, length(val)), 1)
    } else {
      # cumulative rate of false case
      c(0, cumsum(!val) / (length(val) - act_sum))
    }

    tpr <- if (all(!val)) {
      # should be all zero but put 1 to draw a curve
      c(rep(0, length(val)), 1)
    } else {
      # cumulative rate of true case
      c(0, cumsum(val) / act_sum)
    }

    ret <- data.frame(
      tpr,
      fpr
    )

    if (!is.null(grid)) { # Apply grid to reduce data size for drawing chart.
      ret <- ret %>% dplyr::mutate(fpr = floor(fpr*grid)/grid) %>%
        dplyr::group_by(fpr) %>%
        dplyr::summarize(tpr=min(tpr)) %>%
        dplyr::select(tpr, fpr) # Column ornder is reverted here. Put the order back to original.
    }

    colnames(ret)[colnames(ret) == "tpr"] <- tpr_col
    colnames(ret)[colnames(ret) == "fpr"] <- fpr_col

    if (with_auc) {
      ret <- ret %>% dplyr::mutate(auc = auc)
    }

    ret
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(group_cols, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~do_roc_each(.)), tmp_col)) %>%
    dplyr::ungroup() %>%
    unnest_with_drop(!!rlang::sym(tmp_col))

  ret
}

#' Returns cordinations for time-dependent ROC curve for survival prediction.
#' @param df - Data frame
#' @param score_col - Column name for predicted event risk.
#' @param time_col - Column name for actual survival time.
#' @param status_col - Column name for actual event status. 
#' @param at - The time at which the time-dependent ROC is calculated.
#' @param grid - Grid size to reduce data size for drawing chart.
#' @param with_auc - When set to TRUE, AUC is calculated and added as a column of the output.
#' @param revert - If TRUE, the higher the score is, the lower the risk of event is.
#' @export
do_survival_roc_ <- function(df, score_col, time_col, status_col, at = NULL, grid = NULL, with_auc = FALSE, revert = FALSE){
  if (is.null(at)) {
    at <- attr(df, "pred_survival_time")
  }
  df <- df %>% filter(!(!!rlang::sym(time_col) < !!at & !(!!rlang::sym(status_col)))) %>% # Filter out censored rows with shorter survival time.
    mutate(dead = !!rlang::sym(time_col) < !!at | (!!rlang::sym(time_col) == !!at & !!rlang::sym(status_col))) # Add dead column that indicates if it was dead at the specified time.
  if (revert) {
    df <- df %>% dplyr::mutate(!!rlang::sym(score_col) := -!!rlang::sym(score_col))
  }
  do_roc_(df, score_col, "dead", grid = grid, with_auc = with_auc)
}

#' Non standard evaluation version of evaluate_binary_
#' @param df Model data frame that can work prediction
#' @param pred_prob Column name for probability
#' @param actual_val Column name for actual values
#' @export
evaluate_binary <- function(df, pred_prob, actual_val, ...){
  pred_prob_col <- col_name(substitute(pred_prob))
  actual_val_col <- col_name(substitute(actual_val))
  evaluate_binary_(df, pred_prob_col, actual_val_col, ...)
}

#' Calculate binary classification evaluation
#' @param df Data Frame
#' @param pred_prob_col Column name for probability
#' @param actual_val_col Column name for actual values
#' @export
evaluate_binary_ <- function(df, pred_prob_col, actual_val_col, threshold = "f_score"){
  validate_empty_data(df)

  evaluate_binary_each <- function(df){
    actual_val <- df[[actual_val_col]]
    pred_prob <- df[[pred_prob_col]]

    actual_val <- binary_label(actual_val)

    ret <- if (is.numeric(threshold)) {
      pred_label <- pred_prob >= threshold
      ret <- get_score(actual_val, pred_label)
      ret[["threshold"]] <- threshold
      ret
    } else {
      get_optimized_score(actual_val, pred_prob, threshold)
    }

    # calculate AUC
    AUC <- auroc(pred_prob, actual_val)
    auc_ret <- data.frame(AUC)

    ret <- cbind(auc_ret, ret)

    ret
  }

  group_cols <- grouped_by(df)

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(group_cols, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~evaluate_binary_each(.)), tmp_col)) %>%
    dplyr::ungroup() %>%
    unnest_with_drop(!!rlang::sym(tmp_col))

  ret
}

#' Non standard evaluation version of evaluate_regression_
#' @param df Data frame
#' @param pred_val Column name for predicted values
#' @param actual_val Column name for actual values
#' @export
evaluate_regression <- function(df, pred_val, actual_val){
  pred_val_col <- col_name(substitute(pred_val))
  actual_val_col <- col_name(substitute(actual_val))
  evaluate_regression_(df, pred_val_col, actual_val_col)
}

#' Calculate continuous regression evaluation
#' @param df Model data frame that can work prediction
#' @param pred_val_col Column name for predicted values
#' @param actual_val_col Column name for actual values
#' @export
evaluate_regression_ <- function(df, pred_val_col, actual_val_col){
  validate_empty_data(df)

  evaluate_regression_each <- function(df){

    fitted_val <- df[[pred_val_col]]
    actual_val <- df[[actual_val_col]]

    diff <- actual_val - fitted_val
    abs_diff <- abs(diff)
    diff_sq <- diff^2

    # zero should be removed to avoid Inf
    not_zero_index <- actual_val != 0
    mean_absolute_percentage_error <- if(length(not_zero_index) == 0){
      NA
    } else {
      diff_ratio <- abs(diff[not_zero_index] / actual_val[not_zero_index])
      100 * mean(diff_ratio, na.rm = TRUE)
    }

    mean_square_error <- mean(diff_sq, na.rm = TRUE)
    root_mean_square_error <- sqrt(mean_square_error)
    mean_absolute_error <- mean(abs_diff, na.rm = TRUE)

    # ref http://stats.stackexchange.com/questions/230556/calculate-r-square-in-r-for-two-vectors
    r_squared <- 1 - (sum(diff_sq, na.rm = TRUE)/sum((actual_val-mean(actual_val, na.rm = TRUE))^2, na.rm = TRUE))
    # ref http://scikit-learn.org/stable/modules/model_evaluation.html#regression-metrics
    explained_variance <- 1 - var(actual_val - fitted_val, na.rm = TRUE) / var(actual_val, na.rm = TRUE)

    data.frame(
      r_squared,
      explained_variance,
      mean_square_error,
      root_mean_square_error,
      mean_absolute_error,
      mean_absolute_percentage_error
    )
  }

  group_cols <- grouped_by(df)

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(group_cols, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~evaluate_regression_each(.)), tmp_col)) %>%
    dplyr::ungroup() %>%
    unnest_with_drop(!!rlang::sym(tmp_col))

  ret
}

#' Non standard evaluation version of evaluate_multi_
#' @param df Data frame
#' @param pred_label Predicted label colmun name
#' @param actual_val Actual label column name
#' @export
evaluate_multi <- function(df, pred_label, actual_val, ...) {
  pred_label_col <- col_name(substitute(pred_label))
  actual_val_col <- col_name(substitute(actual_val))
  evaluate_multi_(df, pred_label_col, actual_val_col, ...)
}

#' Evaluate multi classification
#' @param df Data frame
#' @param pred_label_col Predicted label colmun name
#' @param actual_val_col Actual label column name
#' @export
evaluate_multi_ <- function(df, pred_label_col, actual_val_col, pretty.name = FALSE, ...) {
  validate_empty_data(df)

  evaluate_multi_each <- function(df){
    actual_values_raw <- df[[actual_val_col]]
    # as.character() so that we can compare factors with same label but with different integer values as the same thing.
    actual_values <- as.character(actual_values_raw)
    # if actual_values is character, pred_values needs to be character too,
    # to get correct unique_label, since if one is char and another is factor
    # unique_label will count factor and character values separatedly.
    pred_values <- as.character(df[[pred_label_col]])

    # make values factor so that missing values can be included in conf_mat
    concat <- c(actual_values, pred_values)
    unique_label <- sort(unique(concat[!is.na(concat)]))
    exist_val <- !is.na(actual_values) & !is.na(pred_values)
    actual_fac <- factor(x = actual_values[exist_val], levels = unique_label)
    pred_fac <- factor(x = pred_values[exist_val], levels = unique_label)

    conf_mat <- table(actual_fac, pred_fac)

    TP <- diag(conf_mat)
    # row_sum - TP is FN
    row_sum <- rowSums(conf_mat)
    # col_sum - TP is FP
    col_sum <- colSums(conf_mat)

    recall <- TP / row_sum
    precision <- TP / col_sum

    # explanations about micro f1 and macro f1
    # http://rushdishams.blogspot.jp/2011/08/micro-and-macro-average-of-precision.html
    # http://blog.revolutionanalytics.com/2016/03/com_class_eval_metrics_r.html
    tp_sum <- sum(TP, na.rm=TRUE)
    fp_sum <- sum(col_sum, na.rm=TRUE) - tp_sum
    micro_recall <- tp_sum / sum(row_sum, na.rm=TRUE)
    micro_precision <- tp_sum / sum(col_sum, na.rm=TRUE)
    micro_f_score <- if (is.na(micro_recall + micro_precision) || micro_recall + micro_precision == 0){
      # no prediction was correct
      0
    } else {
      2 * (micro_recall * micro_precision) / (micro_recall + micro_precision)
    }

    denominator <- recall+precision
    f_score <- 2 * recall*precision/(denominator)
    f_score[is.na(f_score)] <- 0 # in case any denominator is zero

    macro_f_score <- mean(f_score, na.rm = TRUE)

    accuracy <- tp_sum / length(actual_fac)
    misclassification_rate <- 1 - accuracy

    # this is to change column name
    accuracy_rate <- accuracy
    n <- sum(!is.na(pred_values)) # Sample size for test.

    data.frame(
      micro_f_score,
      macro_f_score,
      accuracy_rate,
      misclassification_rate,
      n
    )
  }

  group_cols <- grouped_by(df)

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(group_cols, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~evaluate_multi_each(.)), tmp_col)) %>%
    dplyr::ungroup() %>%
    unnest_with_drop(!!rlang::sym(tmp_col))

  if (pretty.name){
    colnames(ret)[colnames(ret) == "micro_f_score"] <- "Micro-Averaged F Score"
    colnames(ret)[colnames(ret) == "macro_f_score"] <- "Macro-Averaged F Score"
    colnames(ret)[colnames(ret) == "accuracy_rate"] <- "Accuracy Rate"
    colnames(ret)[colnames(ret) == "misclassification_rate"] <- "Misclass. Rate"
    colnames(ret)[colnames(ret) == "n"] <- "Rows"
  }

  ret
}

# Generates Analytics View Summary Table for logistic/binomial regression. Handles Test Mode.
#' @export
evaluate_binary_training_and_test <- function(df, actual_val, threshold = "f_score", pretty.name = FALSE){
  actual_val_col <- col_name(substitute(actual_val)) # Get name of column as string.
  training_ret <- df %>% glance_rowwise(model, binary_classification_threshold = threshold)
  ret <- training_ret

  grouped_col <- colnames(df)[!colnames(df) %in% c("model", ".test_index", "source.data")]

  # Consider it test mode if any of the element of .test_index column has non-zero length, and work on generating Summary row for prediction on test data.
  if (purrr::some(df$.test_index, function(x){length(x)!=0})) {
    ret$is_test_data <- FALSE # Set is_test_data FALSE for training data. Add is_test_data column only when there are test data too.
    each_func <- function(df) {
      if (!is.data.frame(df)) {
        df <- tribble(~model, ~.test_index, ~source.data,
                      df$model, df$.test_index, df$source.data)
      }

      tryCatch({
        test_pred_ret <- prediction_binary(df, data = "test")
  
        # Evaluate the binary classification result.
        eret <- evaluate_binary_(test_pred_ret, "predicted_probability", actual_val_col, threshold = threshold)
  
        test_ret <- eret %>% dplyr::mutate(n = true_positive + false_positive + true_negative + false_negative,
                                           positives = true_positive + false_negative,
                                           negatives = true_negative + false_positive) %>%
                             dplyr::select(auc = AUC, f_score, accuracy_rate,
                                           misclassification_rate, precision, recall,
                                           n, positives, negatives)
        test_ret$is_test_data <- TRUE
        test_ret
      }, error = function(e){
        data.frame()
      })
    }

    target_df <- if (length(grouped_col) > 0) {
      df %>% group_by(!!!rlang::syms(grouped_col))
    } else {
      df
    }

    test_ret <- do_on_each_group(target_df, each_func, with_unnest = TRUE)
    ret <- ret %>% dplyr::bind_rows(test_ret)
  }

  # sort column order
  ret <- ret %>% dplyr::select(auc, f_score, accuracy_rate, misclassification_rate, precision,
                               recall, p.value, positives, negatives, n, logLik, AIC, BIC, # The order of positives, negatives, n is made the same as random forest and decision tree.
                               deviance, null.deviance, df.null, df.residual, everything())

  # Reorder columns. Bring group_by column first, and then is_test_data column, if it exists.
  if (!is.null(ret$is_test_data)) {
    if (length(grouped_col) > 0) {
      ret <- ret %>% dplyr::select(!!!rlang::syms(grouped_col), is_test_data, everything())
    }
    else {
      ret <- ret %>% dplyr::select(is_test_data, everything())
    }
  }
  else {
    if (length(grouped_col) > 0) {
      ret <- ret %>% dplyr::select(!!!rlang::syms(grouped_col), everything())
    }
  }

  if (pretty.name){
    colnames(ret)[colnames(ret) == "f_score"] <- "F1 Score"
    colnames(ret)[colnames(ret) == "accuracy_rate"] <- "Accuracy Rate"
    colnames(ret)[colnames(ret) == "misclassification_rate"] <- "Misclass. Rate"
    colnames(ret)[colnames(ret) == "precision"] <- "Precision"
    colnames(ret)[colnames(ret) == "recall"] <- "Recall"
    colnames(ret)[colnames(ret) == "auc"] <- "AUC"
    colnames(ret)[colnames(ret) == "n"] <- "Rows"
    colnames(ret)[colnames(ret) == "positives"] <- "Rows (TRUE)"
    colnames(ret)[colnames(ret) == "negatives"] <- "Rows (FALSE)"
    colnames(ret)[colnames(ret) == "p.value"] <- "P Value"
    colnames(ret)[colnames(ret) == "logLik"] <- "Log Likelihood"
    colnames(ret)[colnames(ret) == "deviance"] <- "Residual Deviance"
    colnames(ret)[colnames(ret) == "null.deviance"] <- "Null Deviance"
    colnames(ret)[colnames(ret) == "df.null"] <- "Null Model DF"
    colnames(ret)[colnames(ret) == "df.residual"] <- "Residual DF"

    base_cols <- colnames(ret)[stringr::str_detect(colnames(ret) , "_base$")]
    if (length(base_cols) > 0) {
      for (col in base_cols) {
        # Using gsub as opposed to str_replace, since str_replace seems to garble Japanese column name on Windows.
        colnames(ret)[colnames(ret) == col] <- paste0("Base Level of ", gsub("_base$", "", col))
      }
    }

    # Bring "Residual DF" after "Residual Deviance".
    if (all(c("Residual DF", "Residual Deviance") %in% colnames(ret))) {
      ret <- ret %>% dplyr::relocate(`Residual DF`, .after=`Residual Deviance`)
    }
  }

  if (length(grouped_col) > 0){
    ret <- ret %>% dplyr::arrange(!!!rlang::syms(grouped_col))
  }

  # Prettify is_test_data column. Note that column order is already taken care of.
  if (!is.null(ret$is_test_data) && pretty.name) {
    ret <- ret %>%
      dplyr::mutate(is_test_data = dplyr::if_else(is_test_data, "Test", "Training")) %>%
      dplyr::rename(`Data Type` = is_test_data)
  }
  ret
}


