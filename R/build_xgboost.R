#' formula version of xgboost
#' ref: https://www.r-bloggers.com/with-our-powers-combined-xgboost-and-pipelearner/
#' @export
fml_xgboost <- function(data, formula, weight = NULL, params = list(), ...) {
  vars <- all.vars(formula)
  is_multi <- params$objective == "multi:softmax" ||
    params$objective == "multi:softprob"

  is_binary <- params$objective == "binary:logistic" ||
    params$objective == "binary:logitraw" ||
    params$objective == "reg:logistic"

  # case of params$objective is empty
  if (is.na(is_multi)) {
    is_multi <- FALSE
  }
  if (is.na(is_binary)) {
    is_binary <- FALSE
  }

  x_names <- vars[-1]
  y_name  <- vars[1]

  # these parameter will be stored in models,
  # so that values can be back to original state in prediction
  y_fct_level <- NULL
  factorized <- FALSE
  if(is.character(data[[y_name]])){
    if(!(is_multi || is_binary)){
      stop("target must be numeric for regression")
    }
    output_type <- "character"
    y_fct <- as.factor(data[[y_name]])
    # labels should start with 0, so there is -1
    data[[y_name]] <- as.integer(y_fct) - 1
    factorized <- TRUE
  } else if (is.factor(data[[y_name]])){
    if(!(is_multi || is_binary)){
      stop("target must be numeric for regression")
    }
    output_type <- "factor"
    y_fct <- data[[y_name]]
    # labels should start with 0, so there is -1
    data[[y_name]] <- as.integer(y_fct) - 1
    factorized <- TRUE
  } else if (is.logical(data[[y_name]])) {
    output_type <- "logical"
    y_fct <- as.factor(data[[y_name]])
    # labels should start with 0, so there is -1
    data[[y_name]] <- as.integer(y_fct) - 1
    factorized <- TRUE
  } else if(is.numeric(data[[y_name]])){
    output_type <- "numeric"
    if(is_multi) {
      y_fct <- as.factor(data[[y_name]])
      # labels should start with 0, so there is -1
      data[[y_name]] <- as.integer(y_fct) - 1
      factorized <- TRUE
    }
  } else {
    stop("Type of target variable is not supported.")
  }

  if (factorized) {
    if(is_multi){
      y_fct_level <- levels(y_fct)
      if(is.null(params[["num_class"]])){
        params[["num_class"]] <- length(y_fct_level)
      }
    } else if (is_binary) {
      if (length(levels(y_fct)) != 2) {
        stop("number of labels must be 2 for binary classification")
      } else {
        y_fct_level <- levels(y_fct)
      }
    }
  }
  # data must be only numeric
  data <- data %>% dplyr::select_if(function(col){is.numeric(col) || is.logical(col)})

  if (any(x_names == '.')) {
    x_names <- colnames(data)[names(data) != y_name]
  }
  for (x_name in x_names) {
    data <- data[!is.na(data[[x_name]]), ]
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

  y <- data[[y_name]]

  ret <- xgboost::xgboost(data = x, label = y, weight = weight, params = params, ...)
  ret$fml <- formula
  ret$x_names <- x_names
  ret$y_fct_level <- y_fct_level
  ret$output_type <- output_type
  ret
}

#' Create regression model by xgboost
#' @param data Data frame
#' @param formula Formula for xgb.Booster training.
#' @param ... Arguments to be passed to xgboost::xgboost
#' @export
build_xgboost_reg <- function(data, formula, output_type = "linear", nrounds = 10, params = list(), ...) {
  .dots <- lazyeval::dots_capture(...)
  objective <- paste0("reg:", output_type, sep = "")
  params[["objective"]] <- objective
  # lazyevaluation must be used for params argument to work correctly
  .dots[["params"]] <- lazyeval::lazy(params)
  .dots[["formula"]] <- lazyeval::lazy(formula)
  build_model_(
    data,
    model_func = fml_xgboost,
    nrounds = nrounds,
    .dots = .dots)
}

#' Create regression model by xgboost
#' @param data Data frame
#' @param formula Formula for xgb.Booster training.
#' @param ... Arguments to be passed to xgboost::xgboost
#' @export
build_xgboost <- function(data, formula, nrounds = 10, params = list(), ...) {
  build_model(
    data,
    model_func = fml_xgboost,
    formula = formula,
    nrounds = nrounds,
    params = params, ...)
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
  if(is.null(x$x_names)) {
    stop("model must be created from fml_xgboost to augment")
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
    if(is.character(original_data)){
      levels[ids]
    } else if (is.factor(original_data)){
      factor(levels[ids], levels)
    } else if (is.logical(original_data)) {
      as.logical(levels[ids])
    } else if(is.numeric(original_data)){
      as.numeric(levels[ids])
    } else {
      ids
    }
  }

  obj <- x$params$objective
  if(!is.null(obj)){
    if (obj == "multi:softmax") {
      predicted_label_col <- avoid_conflict(colnames(data), "predicted_label")
      predicted <- find_label(predicted+1, x$y_fct_level, data[[y_name]])
      data[[predicted_label_col]] <- predicted

    } else if (obj == "multi:softprob") {
      predicted_label_col <- avoid_conflict(colnames(data), "predicted_label")
      predicted_prob_col <- avoid_conflict(colnames(data), "predicted_probability")

      y_fct_level <- x$y_fct_level
      # predicted is a vector containing probabilities for each class
      probs <- matrix(predicted, nrow = length(y_fct_level)) %>% t()
      colnames(probs) <- y_fct_level
      predicted <- probs %>%
        as.data.frame() %>%
        append_colnames(prefix = "predicted_probability_")

      data <- cbind(data, predicted)

      colmax <- max.col(probs)

      # get max probabilities from each row
      max_prob <- probs[(colmax - 1) * nrow(probs) + seq(nrow(probs))]
      predicted_label <- find_label(colmax, y_fct_level, data[[y_name]])
      # predicted_prob_col is a column for probabilities of chosen values
      data[[predicted_prob_col]] <- max_prob
      data[[predicted_label_col]] <- predicted_label
    } else {
      data[["predicted_value"]] <- predicted
    }
  } else {
    data[["predicted_value"]] <- predicted
  }
  data
}

#' Tidy method for xgboost output
#' @param x Fitted xgb.Booster model
#' @param type Can be "weight" or "log".
#' "weight" returns importances of features and "log" returns evaluation log.
#' @param ... Not used for now.
#' @export
tidy.xgb.Booster <- function(x, type="weight", ...){
  ret <- xgboost::xgb.importance(x$x_names,model=x) %>% as.data.frame()
  colnames(ret)[colnames(ret)=="Gain"] <- "Importance"
  ret
}

#' Glance for xgb.Booster model
#' @param x xgb.Booster model
#' @param ... Not used for now.
#' @export
glance.xgb.Booster <- function(x, ...) {
  # data frame with
  # number of iteration
  # with chosen evaluation metrics
  ret <- x$evaluation_log %>% as.data.frame()
  colnames(ret)[colnames(ret) == "iter"] <- "number_of_iteration"
  colnames(ret)[colnames(ret) == "train_rmse"] <- "root_mean_square_error"
  colnames(ret)[colnames(ret) == "train_mae"] <- "mean_absolute_error"
  colnames(ret)[colnames(ret) == "train_logloss"] <- "negative_log_likelihood"
  colnames(ret)[colnames(ret) == "train_error"] <- "binary_missclassification_rate"
  colnames(ret)[colnames(ret) == "train_merror"] <- "missclassification_rate"
  colnames(ret)[colnames(ret) == "train_mlogloss"] <- "multiclass_logloss"
  colnames(ret)[colnames(ret) == "train_auc"] <- "auc"
  colnames(ret)[colnames(ret) == "train_ndcg"] <- "normalized_discounted_cumulative_gain"
  colnames(ret)[colnames(ret) == "train_map"] <- "mean_average_precision"
  ret
}

