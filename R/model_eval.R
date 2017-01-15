#' make plots for ROC curve
#' @param df Model data frame that can work prediction
#' @export
do_roc <- function(df){
  predictor <- all.vars(df$model[[1]]$formula)[1]
  group_cols <- colnames(df)[!colnames(df) %in% c(".test_index", "source.data", "model")]

  pred_ret <- prediction(df, type.predict = "response") %>%
    dplyr::group_by_(.dots = group_cols) %>%
    dplyr::select_(.dots = c(predictor, "fitted"))

  roc_col <- avoid_conflict(group_cols, ".roc")

  tpr_col <- avoid_conflict(group_cols, "TPR")
  fpr_col <- avoid_conflict(group_cols, "FPR")

  do_roc_each <- function(df){
    arranged <- df %>%
      dplyr::arrange_(.dots = "desc(fitted)")

    predictor_val <- arranged[[predictor]][!is.na(arranged[[predictor]])]
    if(is.factor(predictor_val)){
      # Need to subtract 1 because the first level in factor is regarded as 1
      # though it should be FALSE.
      predictor_val <- as.logical(as.integer(predictor_val) - 1)
    } else {
      predictor_val <- as.logical(predictor_val)
    }

    predictor_val <- predictor_val[!is.na(predictor_val)]

    if (all(predictor_val)){
      tpr <- c(0, rep(1, length(predictor_val)), 1)
      fpr <- c(0, rep(0, length(predictor_val)), 1)
    } else if (all(!predictor_val)) {
      tpr <- c(0, rep(0, length(predictor_val)), 1)
      fpr <- c(0, rep(1, length(predictor_val)), 1)
    } else {

      pred_sum <- sum(predictor_val)

      tpr <- c(0, cumsum(predictor_val) / pred_sum)
      fpr <- c(0, cumsum(!predictor_val) / (length(predictor_val) - pred_sum))
    }

    ret <- data.frame(
      tpr,
      fpr
    )
    colnames(ret) <- c(tpr_col, fpr_col)
    ret
  }
  ret <- pred_ret %>%
    dplyr::do_(.dots=setNames(list(~do_roc_each(.)), roc_col)) %>%
    tidyr::unnest_(roc_col)

  ret
}
