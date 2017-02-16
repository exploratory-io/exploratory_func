#' formula version of xgboost
#' ref: https://www.r-bloggers.com/with-our-powers-combined-xgboost-and-pipelearner/
fml_xgboost <- function(data, formula, ...) {
  vars <- all.vars(formula)

  X_names <- vars[-1]
  y_name  <- vars[1]

  if (any(X_names == '.')) {
    X_names <- colnames(data)[names(data) != y_name]
  }

  X <- data.matrix(data[, X_names])
  y <- data[[y_name]]

  ret <- xgboost::xgboost(data = X, label = y, ...)
  ret$formula <- formula
  ret$X_names <- X_names
  ret
}

#' Glance xgb.Booster model
#' @param data Data frame
#' @param formula Formula for xgb.Booster training.
#' @param ... Arguments to be passed to xgboost::xgboost
#' @export
build_xgboost <- function(data, formula, ...) {
  build_model(data, model_func = fml_xgboost, formula = formula, ...)
}

#' Glance xgb.Booster model
#' @param x xgb.Booster model
#' @param ... Not used for now.
#' @export
glance.xgb.Booster <- function(x, ...) {
  # data frame with
  # number of iteration
  # last value of evaluation_log
  # chosen objectives parameter
  data.frame(nitr = x$niter, x$evaluation_log[nrow(x$evaluation_log),2], objectives = x$params$objectives)
}

#' Augment predicted values
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
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
#' @export
tidy.xgb.Booster <- function(x, type="weight", ...){
  if(type == "weight"){
    xgboost::xgb.importance(x$X_names,model=x) %>% as.data.frame()
  } else if (type == "log") {
    data.frame(x$evaluation_log)
  }
}
