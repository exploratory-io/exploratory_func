#' formula version of xgboost
#' ref: https://www.r-bloggers.com/with-our-powers-combined-xgboost-and-pipelearner/
data_xgboost <- function(data, x_names, y_name, weight = NULL, nrounds= 10, ...) {
  # data must be only numeric or logical
  data <- data %>% dplyr::select_if(function(col){is.numeric(col) || is.logical(col)})

  if(any(!x_names %in% colnames(data))){
    error_cols <- x_names[!x_names %in% colnames(data)]
    stop(paste0("non numeric column found " + paste0(error_cols, collapse = ", ")))
  }

  # weight argument should be evaluated using data name space
  weight <- if (!is.null(substitute(weight))) {
    lz <- lazyeval::as.lazy(substitute(weight))
    ret <- lazyeval::lazy_eval(lz, data = data)
    data <- data[!is.na(ret), ]
    ret[!is.na(ret)]
  } else {
    NULL
  }

  x <- data.matrix(data[, x_names])
  if(is.integer(x) || is.logical(x)){
    # x must be a double matrix
    x <- matrix(as.double(x), ncol = ncol(x))
  }

  y <- as.numeric(data[[y_name]])

  ret <- xgboost::xgboost(data = x, label = y, weight = weight, nrounds = nrounds, ...)
  ret$x_names <- x_names
  ret
}

#' formula version of xgboost (multinomial)
#' @export
xgboost_binary <- function(data, formula, output_type = "logistic", eval_metric = "auc", ...) {
  vars <- all.vars(formula)

  x_names <- vars[-1]
  y_name  <- vars[1]

  if (any(x_names == '.')) {
    x_names <- quantifiable_cols(data)
  }

  data <- tidyr::drop_na_(data, c(x_names, y_name))

  y_vals <- data[[y_name]]

  label_levels <- c(0, 1)
  if(is.logical(y_vals)) {
    label_levels <- c(FALSE, TRUE)
    y_vals <- as.numeric(y_vals)
  } else if (!all(y_vals %in% c(0, 1))){
    factored <- as.factor(data[[y_name]])
    label_levels <- levels(factored)
    if(length(label_levels) != 2){
      stop("target variable must have 2 unique values")
    }
    y_vals <- as.numeric(factored) - 1
  }

  data[[y_name]] <- y_vals
  objective <- paste0("binary:", output_type, sep = "")

  ret <- data_xgboost(data = data,
                      x_names = x_names,
                      y_name = y_name,
                      objective = objective,
                      eval_metric = eval_metric,
                      ...)
  class(ret) <- c("xgboost_binary", class(ret))
  ret$fml <- formula
  ret$x_names <- x_names
  ret$y_levels <- label_levels
  ret
}

#' formula version of xgboost (multinomial)
#' @export
xgboost_multi <- function(data, formula, output_type = "softprob", ...) {
  vars <- all.vars(formula)

  x_names <- vars[-1]
  y_name  <- vars[1]

  if (any(x_names == '.')) {
    x_names <- quantifiable_cols(data)
  }

  data <- tidyr::drop_na_(data, c(x_names, y_name))

  y_vals <- data[[y_name]]

  label_levels <- if(is.logical(y_vals)) {
    y_vals <- as.numeric(y_vals)
    c(FALSE, TRUE)
  } else if (is.factor(y_vals)) {
    y_vals <- as.numeric(y_vals) - 1
    sort(unique(data[[y_name]]))
  } else {
    factored <- as.factor(data[[y_name]])
    y_vals <- as.numeric(factored) - 1
    same_type(levels(factored), data[[y_name]])
  }

  data[[y_name]] <- y_vals

  objective <- paste0("multi:", output_type, sep = "")
  ret <- ret <- data_xgboost(data = data,
                             x_names = x_names,
                             y_name = y_name,
                             objective = objective,
                             num_class = length(label_levels),
                             ...)
  class(ret) <- c("xgboost_multi", class(ret))
  ret$fml <- formula
  ret$x_names <- x_names
  ret$y_levels <- label_levels
  ret
}

#' formula version of xgboost (regression)
#' @export
xgboost_reg <- function(data, formula, output_type = "linear", ...) {
  vars <- all.vars(formula)

  x_names <- vars[-1]
  y_name  <- vars[1]

  if (any(x_names == '.')) {
    x_names <- quantifiable_cols(data)
  }

  data[[y_name]] <- as.numeric(data[[y_name]])

  data <- tidyr::drop_na_(data, c(x_names, y_name))

  objective <- paste0("reg:", output_type, sep = "")

  ret <- data_xgboost(data = data, x_names = x_names, y_name = y_name,  objective = objective, ...)
  class(ret) <- c("xgboost_reg", class(ret))
  ret$fml <- formula
  ret$x_names <- x_names
  ret$output_type <- output_type
  ret
}

#' Augment predicted values
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @export
augment.xgboost_multi <- function(x, data = NULL, newdata = NULL, ...) {
  class(x) <- class(x)[class(x) != "xgboost_multi"]
  if(!is.null(newdata)){
    data <- newdata
  }

  mat <- as.matrix(data[x$x_names])

  # predict of xgboost expects double matrix as input
  if(is.integer(mat) || is.logical(mat)){
    mat <- matrix(as.numeric(mat), ncol = ncol(mat))
  }

  predicted <- stats::predict(x, mat)

  vars <- all.vars(x$fml)
  y_name <- vars[[1]]

  # create predicted labels for classification
  # based on factor levels and it's indice
  find_label <- function(ids, levels, original_data) {
    levels[ids]
  }

  obj <- x$params$objective
  if (obj == "multi:softmax") {
    predicted_label_col <- avoid_conflict(colnames(data), "predicted_label")
    predicted <- x$y_levels[predicted+1]
    data[[predicted_label_col]] <- predicted

  } else if (obj == "multi:softprob") {
    predicted_label_col <- avoid_conflict(colnames(data), "predicted_label")
    predicted_prob_col <- avoid_conflict(colnames(data), "predicted_probability")

    # predicted is a vector containing probabilities for each class
    probs <- matrix(predicted, nrow = length(x$y_levels)) %>% t()
    colnames(probs) <- x$y_levels
    predicted <- probs %>%
      as.data.frame() %>%
      append_colnames(prefix = "predicted_probability_")

    data <- cbind(data, predicted)

    colmax <- max.col(probs)

    # get max probabilities from each row
    max_prob <- probs[(colmax - 1) * nrow(probs) + seq(nrow(probs))]
    predicted_label <- x$y_levels[colmax]
    # predicted_prob_col is a column for probabilities of chosen values
    data[[predicted_prob_col]] <- max_prob
    data[[predicted_label_col]] <- predicted_label
  }
  data
}

#' Augment predicted values
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @export
augment.xgboost_binary <- function(x, data = NULL, newdata = NULL, threshold = 0.5, ...) {
  class(x) <- class(x)[class(x) != "xgboost_binary"]
  if(!is.null(newdata)){
    data <- newdata
  }

  mat <- as.matrix(data[x$x_names])

  # predict of xgboost expects double matrix as input
  if(is.integer(mat) || is.logical(mat)){
    mat <- matrix(as.numeric(mat), ncol = ncol(mat))
  }
  predicted <- stats::predict(x, mat)

  vars <- all.vars(x$fml)
  y_name <- vars[[1]]

  # create predicted labels for classification
  # based on factor levels and it's indice
  find_label <- function(ids, levels, original_data) {
    levels[ids]
  }

  obj <- x$params$objective
  predicted_label_col <- avoid_conflict(colnames(data), "predicted_label")
  predicted_prob_col <- avoid_conflict(colnames(data), "predicted_probability")
  if (obj == "binary:logistic") {
    predicted_prob_col <- avoid_conflict(colnames(data), "predicted_probability")
    predicted_label <- x$y_levels[(predicted>threshold) + 1]
    data[[predicted_prob_col]] <- predicted
    data[[predicted_label_col]] <- predicted_label
  } else if (obj == "binary:logitraw") {
    predicted_val_col <- avoid_conflict(colnames(data), "predicted_value")

    prob <- boot::inv.logit(predicted)
    predicted_label <- x$y_levels[(predicted>threshold) + 1]

    data[[predicted_val_col]] <- predicted
    data[[predicted_prob_col]] <- prob
    data[[predicted_label_col]] <- predicted_label
  }
  data
}

#' Augment predicted values
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @export
augment.xgb.Booster <- function(x, data = NULL, newdata = NULL, ...) {
  class(x) <- class(x)[class(x) != "xgboost_binary" &
                         class(x) != "xgboost_multi" &
                         class(x) != "xgboost_reg"]
  if(!is.null(newdata)){
    data <- newdata
  }
  if(is.null(x$x_names)) {
    stop("model must be created from fml_xgboost to augment")
  }

  mat <- as.matrix(data[x$x_names])

  # predict of xgboost expects double matrix as input
  if(is.integer(mat) || is.logical(mat)){
    mat <- matrix(as.numeric(mat), ncol = ncol(mat))
  }

  predicted <- stats::predict(x, mat)
  predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
  data[[predicted_value_col]] <- predicted
  data
}

#' Tidy method for xgboost output
#' @param x Fitted xgb.Booster model
#' @param type Can be "weight" or "log".
#' "weight" returns importances of features and "log" returns evaluation log.
#' @param ... Not used for now.
#' @export
tidy.xgb.Booster <- function(x, type="weight", pretty.name = FALSE, ...){
  # xgboost_? class must be removed for xgboost::xgb.importance to work
  class(x) <- class(x)[class(x)!="xgboost_binary" &
                         class(x)!="xgboost_multi" &
                         class(x)!="xgboost_reg"]
  ret <- tryCatch({
    ret <- xgboost::xgb.importance(x$x_names,model=x) %>% as.data.frame()
    if (pretty.name) {
      colnames(ret)[colnames(ret)=="Gain"] <- "Importance"
      colnames(ret)[colnames(ret)=="Cover"] <- "Coverage"
    } else {
      colnames(ret)[colnames(ret)=="Feature"] <- "feature"
      colnames(ret)[colnames(ret)=="Gain"] <- "importance"
      colnames(ret)[colnames(ret)=="Cover"] <- "coverage"
      colnames(ret)[colnames(ret)=="Frequency"] <- "frequency"
    }
    ret
  }, error = function(e){
    data.frame(Error = e$message)
  })
  ret
}

#' Glance for xgb.Booster model
#' @param x xgb.Booster model
#' @param ... Not used for now.
#' @export
glance.xgb.Booster <- function(x, pretty.name = FALSE, ...) {
  # data frame with
  # number of iteration
  # with chosen evaluation metrics
  ret <- x$evaluation_log %>% as.data.frame()
  if(pretty.name){
    colnames(ret)[colnames(ret) == "iter"] <- "Number of Iteration"
    colnames(ret)[colnames(ret) == "train_rmse"] <- "Root Mean Square Error"
    colnames(ret)[colnames(ret) == "train_mae"] <- "Mean Absolute Error"
    colnames(ret)[colnames(ret) == "train_logloss"] <- "Negative Log Likelihood"
    colnames(ret)[colnames(ret) == "train_error"] <- "Missclassification Rate" # this is for binary
    colnames(ret)[colnames(ret) == "train_merror"] <- "Missclassification Rate" # this is for multiclass
    colnames(ret)[colnames(ret) == "train_mlogloss"] <- "Multiclass Logloss"
    colnames(ret)[colnames(ret) == "train_auc"] <- "AUC"
    colnames(ret)[colnames(ret) == "train_ndcg"] <- "Normalized Discounted Cumulative Gain"
    colnames(ret)[colnames(ret) == "train_map"] <- "Mean Average Precision"
    colnames(ret)[colnames(ret) == "train_map"] <- "Mean Average Precision"
    colnames(ret)[colnames(ret) == "train_gamma_nloglik"] <- "Gamma Negative Log Likelihood"
    colnames(ret)[colnames(ret) == "train_gamma_deviance"] <- "Gamma Deviance"
  } else {
    colnames(ret)[colnames(ret) == "iter"] <- "number_of_iteration"
    colnames(ret)[colnames(ret) == "train_rmse"] <- "root_mean_square_error"
    colnames(ret)[colnames(ret) == "train_mae"] <- "mean_absolute_error"
    colnames(ret)[colnames(ret) == "train_logloss"] <- "negative_log_likelihood"
    colnames(ret)[colnames(ret) == "train_error"] <- "missclassification_rate" # this is for binary
    colnames(ret)[colnames(ret) == "train_merror"] <- "missclassification_rate" # this is for multiclass
    colnames(ret)[colnames(ret) == "train_mlogloss"] <- "multiclass_logloss"
    colnames(ret)[colnames(ret) == "train_auc"] <- "auc"
    colnames(ret)[colnames(ret) == "train_ndcg"] <- "normalized_discounted_cumulative_gain"
    colnames(ret)[colnames(ret) == "train_map"] <- "mean_average_precision"
    colnames(ret)[colnames(ret) == "train_map"] <- "mean_average_precision"
    colnames(ret)[colnames(ret) == "train_gamma_nloglik"] <- "gamma_negative_log_likelihood"
    colnames(ret)[colnames(ret) == "train_gamma_deviance"] <- "gamma_deviance"
  }
  ret
}

