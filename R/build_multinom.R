# build_multinom

#' Build multinom model data frame
#' @param data Data frame
#' @param ... Parameters for nnet::multinom
#' @export
build_multinom <- function(data, formula, ...){
  build_model(data,
              model_func = nnet::multinom,
              formula = formula,
              reserved_colnames = c(
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
              ),
              ...)
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

#' Augment predicted values
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @importFrom tidytext augment
#' @export
augment.xgb.Booster <- function(x, data = NULL, newdata = NULL, ...) {
  if(!is.null(newdata)){
    data <- newdata
  }
  fml <- x$formula
  vars <- all.vars(fml)
  columns <- vars[-1]
  if (columns == '.') {
    columns <- names(data)[names(data) != all.vars(fml)[[1]]]
  }
  predicted <- stats::predict(x, as.matrix(data[columns]))
  data[["predicted_value"]] <- predicted
  data
}

#' Tidy method for xgboost output
#' @param x Fitted xgb.Booster model
#' @param type Can be "weight" or "log".
#' "weight" returns importances of features and "log" returns evaluation log.
#' @param ... Not used for now.
#' @importFrom tidytext tidy
#' @export
tidy.xgb.Booster <- function(x, type="weight", ...){
  if(type == "weight"){
    xgboost::xgb.importance(x$X_names,model=x) %>% as.data.frame()
  } else if (type == "log") {
    data.frame(x[["evaluation_log"]])
  }
}

#' Glance xgb.Booster model
#' @param x xgb.Booster model
#' @param ... Not used for now.
#' @export
#' @importFrom tidytext glance
#' @export
glance.xgb.Booster <- function(x, ...) {
  # data frame with
  # number of iteration
  # last value of evaluation_log
  # chosen objectives parameter
  data.frame(nitr = x[["niter"]], x["evaluation_log"][nrow(x[["evaluation_log"]]),2])
}
