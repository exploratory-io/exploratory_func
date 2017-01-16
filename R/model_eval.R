#' Non standard evaluation version of do_roc_
#' @param df Model data frame that can work prediction
#' @export
do_roc <- function(df, actual_val, pred_prob){
  actual_val_col <- col_name(substitute(actual_val))
  pred_prob_col <- col_name(substitute(pred_prob))
  do_roc_(df, actual_val_col, pred_prob_col)
}

#' Return cordinations for ROC curve
#' @param df Model data frame that can work prediction
#' @export
do_roc_ <- function(df, actual_val_col, pred_prob_col){
  group_cols <- grouped_by(df)

  tpr_col <- avoid_conflict(group_cols, "true_positive_rate")
  fpr_col <- avoid_conflict(group_cols, "false_positive_rate")

  do_roc_each <- function(df){
    arranged <- df[order(-df[[pred_prob_col]]), ]

    val <- arranged[[actual_val_col]][!is.na(arranged[[actual_val_col]])]
    if(is.factor(val)){
      # Need to subtract 1 because the first level in factor is regarded as 1
      # though it should be FALSE.
      val <- as.logical(as.integer(val) - 1)
    } else {
      val <- as.logical(val)
    }

    if (all(val)){
      tpr <- c(0, rep(1, length(val)), 1)
      fpr <- c(0, rep(0, length(val)), 1)
    } else if (all(!val)) {
      tpr <- c(0, rep(0, length(val)), 1)
      fpr <- c(0, rep(1, length(val)), 1)
    } else {

      pred_sum <- sum(val)

      tpr <- c(0, cumsum(val) / pred_sum)
      fpr <- c(0, cumsum(!val) / (length(val) - pred_sum))
    }

    ret <- data.frame(
      tpr,
      fpr
    )
    colnames(ret) <- c(tpr_col, fpr_col)
    ret
  }

  tmp_col <- avoid_conflict(group_cols, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~do_roc_each(.)), tmp_col)) %>%
    tidyr::unnest_(tmp_col)

  ret
}

#' Non standard evaluation version of eval_pred_bin_
#' @param df Model data frame that can work prediction
#' @export
eval_pred_bin <- function(df, actual_val, pred_prob, ...){
  actual_val_col <- col_name(substitute(actual_val))
  pred_prob_col <- col_name(substitute(pred_prob))
  eval_pred_bin_(df, actual_val_col, pred_prob_col, ...)
}

#' Calculate binary classification evaluation
#' @param df Model data frame that can work prediction
#' @export
eval_pred_bin_ <- function(df, actual_val_col, pred_prob_col, threshold = "f_score"){

  get_scores <- function(act_label, pred_label) {
    tp <- pred_label & act_label
    fp <- pred_label & !act_label
    tn <- !pred_label & !act_label
    fn <- !pred_label & act_label

    true_positive <- sum(tp, na.rm = TRUE)
    false_positive <- sum(fp, na.rm = TRUE)
    true_negative <- sum(tn, na.rm = TRUE)
    false_negative <- sum(fn, na.rm = TRUE)

    test_size <- true_positive + false_positive + true_negative + false_negative

    precision <- true_positive / sum(pred_label, na.rm = TRUE)
    sensitivity <- true_positive / sum(act_label, na.rm = TRUE)
    specificity <- true_negative / sum(!act_label, na.rm = TRUE)
    accuracy <- (true_positive + true_negative) / test_size
    f_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

    data.frame(
      f_score,
      accuracy,
      precision,
      sensitivity,
      specificity,
      true_positive,
      false_positive,
      true_negative,
      false_negative,
      test_size
    )
  }

  # threshold can be optimized to the result below
  accept_optimize <- c(
    "f_score",
    "accuracy",
    "precision",
    "sensitivity",
    "specificity"
  )

  eval_pred_bin_each <- function(df){

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
      ret <- get_scores(actual_val, pred_label)
      ret[["threshold"]] <- threshold
    } else {
      max_values <- NULL
      max_value <- -1
      for (thres in ((seq(101) - 1) / 100)){

        pred_label <- pred_prob >= thres

        score <- get_scores(actual_val, pred_label)

        if (!threshold %in% accept_optimize) {
          stop(paste0("threshold must be chosen from ", paste(accept_optimize, collapse = ", ")))
        } else if (is.nan(score[[threshold]])) {
          # if nan, pass to avoid error
        } else if (max_value < score[[threshold]]){
          max_values <- score
          max_values[["threshold"]] <- thres
          max_value <- score[[threshold]]
        }
      }
      max_values
    }

    roc <- df %>% do_roc_(actual_val_col = actual_val_col, pred_prob_col = pred_prob_col)

    # use numeric index so that it won't be disturbed by name change
    # 2 should be false positive rate (x axis) and 1 should be true positive rate
    # calculate the area under the plots
    AUC <- sum((roc[[2]] - dplyr::lag(roc[[2]])) * roc[[1]], na.rm = TRUE)
    ret[["AUC"]] <- AUC

    ret
  }

  group_cols <- grouped_by(df)

  tmp_col <- avoid_conflict(group_cols, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~eval_pred_bin_each(.)), tmp_col)) %>%
    tidyr::unnest_(tmp_col)

  ret
}

#' Non standard evaluation version of eval_pred_const_
#' @param df Model data frame that can work prediction
#' @export
eval_pred_cont <- function(df, actual_val, fitted){
  actual_val_col <- col_name(substitute(actual_val))
  fitted_col <- col_name(substitute(fitted))
  eval_pred_cont_(df, actual_val_col, fitted_col)
}

#' Calculate binary classification evaluation
#' @param df Model data frame that can work prediction
#' @export
eval_pred_cont_ <- function(df, actual_val_col, fitted_col){

  eval_pred_cont_each <- function(df){

    fitted_val <- df[[fitted_col]]
    actual_val <- df[[actual_val_col]]

    diff <- actual_val - fitted_val
    abs_diff <- abs(diff)
    diff_sq <- diff^2
    not_zero_index <- actual_val != 0
    mape <- if(length(not_zero_index) == 0){
      NA
    } else {
      diff_ratio <- abs(diff[not_zero_index] / actual_val[not_zero_index])
      mean(diff_ratio, na.rm = TRUE)
    }

    mse <- mean(diff_sq, na.rm = TRUE)
    rmse <- sqrt(mse)
    mae <- mean(abs_diff, na.rm = TRUE)
    data.frame(
      mse, rmse, mae, mape
    )
  }

  group_cols <- grouped_by(df)

  tmp_col <- avoid_conflict(group_cols, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~eval_pred_cont_each(.)), tmp_col)) %>%
    tidyr::unnest_(tmp_col)

  ret
}
