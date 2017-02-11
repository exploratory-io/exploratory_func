# build_multinom

#' Build multinom model data frame
#' @param data Data frame
#' @param ... Parameters for nnet::multinom
#' @export
build_multinom <- function(data, formula, ...){
  # using lazyeval is needed for non standard evaluation arguments like weights
  lz_dots <- lazyeval::lazy_dots(...)
  lz_dots[["data"]] <- lazyeval::as.lazy(quote(data))
  lz_dots[["formula"]] <- lazyeval::lazy(formula)
  lz_dots[["model_func"]] <- lazyeval::as.lazy(quote(nnet::multinom))
  lz_dots[["reserved_colnames"]] <- lazyeval::as.lazy(quote(c(
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
  )))
  .call <- lazyeval::make_call(quote(build_model), lz_dots)
  .call$env <- environment()
  lazyeval::lazy_eval(.call)
}

#' augment for multinom model, which is not defined by broom
#' @param data Trained data
#' @param newdata Data to predict
#' @export
augment.multinom <- function(model, data = NULL, newdata = NULL) {

  predicted_label_col <- avoid_conflict(colnames(data), "predicted_label")
  predicted_prob_col <- avoid_conflict(colnames(data), "predicted_probability")

  if (is.null(newdata)) {
    # use trained data and get probabilities
    f_values <- model$fitted.values

    # f_values is one column matrix if the number of levels is 2
    if(ncol(f_values) == 1){
      f_values <- matrix(c(1-f_values[,1], f_values), ncol=2)
      colnames(f_values) <- model$lev
    }
    ret <- f_values %>%
      as.data.frame() %>%
      append_colnames(prefix = "predicted_probability_")

    # get max probabilities from each row
    max_prob <- f_values[(max.col(f_values) - 1) * nrow(f_values) + seq(nrow(f_values))]
    # predicted_prob_col is a column for probabilities of chosen values
    ret[[predicted_prob_col]] <- max_prob

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
    # if newdata is one row or classification labels are two,
    # it becomes a vector,
    # so should be converted to matrix
    if (!is.matrix(prob_mat)) {
      mat <- matrix(prob_mat, nrow = nrow(newdata))
      if(ncol(mat) == length(prob_mat)){
        # this is one row newdata case
        colnames(mat) <- names(prob_mat)
      } else {
        # two classification labels case
        # create both probability for positive and negative
        mat <- matrix(c(1-mat[,1], mat), ncol=2)
        colnames(mat) <- model$lev
        mat
      }
      prob_mat <- mat
    }

    prob_label <- colnames(prob_mat)[max.col(prob_mat)]
    # get max values from each row
    max_prob <- prob_mat[(max.col(prob_mat) - 1) * nrow(prob_mat) + seq(nrow(prob_mat))]

    ret <- prob_mat %>%
      as.data.frame() %>%
      append_colnames(prefix = "predicted_probability_")
    ret[[predicted_label_col]] <- prob_label
    ret[[predicted_prob_col]] <- max_prob
    ret <- dplyr::bind_cols(newdata, ret)
  }
}
