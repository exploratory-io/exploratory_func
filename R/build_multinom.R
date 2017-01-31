# build_multinom

#' Build multinom model data frame
#' @param data Data frame
#' @param ... Parameters for nnet::multinom
#' @export
build_multinom <- function(data, formula, ...){
  reserved_colnames <- reserved_colnames <- c(
    # for model_coef
    "y.level",
    "term",
    "estimate",
    "std_error",
    "t_ratio",
    "p_value",
    # for model_stats
    "edf",
    "deviance",
    "AIC"
  )
  build_model(data = data,
              formula = formula,
              model_func = nnet::multinom,
              reserved_colnames = reserved_colnames,
              ...)
}

#' augment for multinom model, which is not defined by broom
#' @param data Trained data
#' @param newdata Data to predict
#' @export
augment.multinom <- function(model, data = NULL, newdata = NULL) {

  predicted_label_col <- avoid_conflict(colnames(data), "predicted_label")

  if (is.null(newdata)) {
    # use trained data and get probabilities
    ret <- model$fitted.values %>%
      as.data.frame() %>%
      append_colnames(prefix = "predicted_probability_")

    # in case of training data, NA of terms in both right and left side should be removed
    vars <- all.vars(model$terms)
    data <- tidyr::drop_na_(data, vars)


    ret[[predicted_label_col]] <- stats::predict(model)
    if (!is.null(data)) {
      # append fitted values
      ret <- dplyr::bind_cols(data, ret)
    }
    ret
  } else {
    # use new data and get probabilities

    # in case of test data, NA of terms only in right hand side should be removed
    vars <- labels(model$terms)
    newdata <- tidyr::drop_na_(newdata, vars)

    prob_mat <-  stats::predict(model, newdata, type = "prob")
    # if newdata is one row, it becomes a vector,
    # so should be converted to matrix
    if (!is.matrix(prob_mat)) {
      mat <- matrix(prob_mat, ncol = length(prob_mat))
      colnames(mat) <- names(prob_mat)
      prob_mat <- mat
    }

    prob_label <- colnames(prob_mat)[max.col(prob_mat)]
    ret <- stats::predict(model, newdata, type = "prob")
    # if newdata is one row, it becomes a vector and
    # as.data.frame makes a long dataframe from it,
    # so it should be converted to a list
    if (!is.matrix(ret)) {
      ret <- as.list(ret)
    }
    ret <- ret %>%
      as.data.frame() %>%
      append_colnames(prefix = "predicted_probability_")
    ret[[predicted_label_col]] <- prob_label
    ret <- dplyr::bind_cols(newdata, ret)
  }
}
