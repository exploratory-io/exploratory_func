#' Non standard evaluation version of do_roc_
#' @param df Model data frame that can work prediction
#' @export
do_roc <- function(df, actual_value, predicted_prob){
  actual_value_col <- col_name(substitute(actual_value))
  predicted_prob_col <- col_name(substitute(predicted_prob))
  do_roc_(df, actual_value_col, predicted_prob_col)
}

#' Return cordinations for ROC curve
#' @param df Model data frame that can work prediction
#' @export
do_roc_ <- function(df, actual_value_col, predicted_prob_col){
  group_cols <- grouped_by(df)

  tpr_col <- avoid_conflict(group_cols, "true_positive_rate")
  fpr_col <- avoid_conflict(group_cols, "false_positive_rate")

  do_roc_each <- function(df){
    arranged <- df[order(df[[predicted_prob_col]]), ]

    val <- arranged[[actual_value_col]][!is.na(arranged[[actual_value_col]])]
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
