#' Non standard evaluation version of do_roc_
#' @param df Data frame
#' @param pred_prob Column name for probability
#' @param actual_val Column name for actual values
#' @export
do_roc <- function(df, pred_prob, actual_val){
  pred_prob_col <- col_name(substitute(pred_prob))
  actual_val_col <- col_name(substitute(actual_val))
  do_roc_(df, pred_prob_col, actual_val_col)
}

#' Return cordinations for ROC curve
#' @param df Data frame
#' @param pred_prob_col Column name for probability
#' @param actual_val_col Column name for actual values
#' @export
do_roc_ <- function(df, pred_prob_col, actual_val_col){
  group_cols <- grouped_by(df)

  tpr_col <- avoid_conflict(group_cols, "true_positive_rate")
  fpr_col <- avoid_conflict(group_cols, "false_positive_rate")

  do_roc_each <- function(df){
    # sort descending order by predicted probability
    arranged <- df[order(-df[[pred_prob_col]]), ]

    # remove na and get actual values
    val <- arranged[[actual_val_col]][!is.na(arranged[[actual_val_col]])]
    if(is.factor(val)){
      # Need to subtract 1 because the first level in factor is regarded as 1
      # though it should be FALSE.
      val <- as.logical(as.integer(val) - 1)
    } else {
      val <- as.logical(val)
    }

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
    colnames(ret) <- c(tpr_col, fpr_col)
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
    tidyr::unnest_(tmp_col)

  ret
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

  evaluate_binary_each <- function(df){

    actual_val <- df[[actual_val_col]]
    pred_prob <- df[[pred_prob_col]]

    if(is.factor(actual_val)){
      # Need to subtract 1 because the first level in factor is regarded as 1
      # though it should be FALSE.
      actual_val <- as.logical(as.integer(actual_val) - 1)
    } else {
      actual_val <- as.logical(actual_val)
    }

    ret <- if (is.numeric(threshold)) {
      pred_label <- pred_prob >= threshold
      ret <- get_score(actual_val, pred_label)
      ret[["threshold"]] <- threshold
      ret
    } else {
      get_optimized_score(actual_val, pred_prob, threshold)
    }

    # calculate AUC from ROC
    roc <- df %>% do_roc_(actual_val_col = actual_val_col, pred_prob_col = pred_prob_col)
    # use numeric index so that it won't be disturbed by name change
    # 2 should be false positive rate (x axis) and 1 should be true positive rate (yaxis)
    # calculate the area under the plots
    AUC <- sum((roc[[2]] - dplyr::lag(roc[[2]])) * roc[[1]], na.rm = TRUE)
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
    tidyr::unnest_(tmp_col)

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
    tidyr::unnest_(tmp_col)

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
evaluate_multi_ <- function(df, pred_label_col, actual_val_col, ...) {

  evaluate_multi_each <- function(df){
    actual_values_raw <- df[[actual_val_col]]
    # as.character() is work around for the case actual_val_col is factor
    actual_values <- as.character(actual_values_raw)
    pred_values <- df[[pred_label_col]]

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
    micro_f_score <- 2 * (micro_recall * micro_precision) / (micro_recall + micro_precision)

    denominator <- recall+precision
    denominator[denominator==0] <- 1 # avoid deviding by 0
    f_score <- 2 * recall*precision/(denominator)

    macro_f_score <- mean(f_score, na.rm = TRUE)

    missclassification_error <- 1 - tp_sum / length(actual_fac)

    data.frame(
      micro_f_score,
      macro_f_score,
      missclassification_error
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
    tidyr::unnest_(tmp_col)

  ret
}
