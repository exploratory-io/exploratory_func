#' formula version of xgboost
#' @export
fml_xgboost <- function(data, formula, nrounds= 10, weights = NULL, watchlist_rate = 0, ...) {
  term <- terms(formula, data = data)
  # do.call is used to substitute weight
  md_frame <- do.call(model.frame, list(term, data = data, weights = substitute(weights)))
  if(nrow(md_frame) == 0){
    stop("No valid data to create xgboost model.")
  } else {
    md_mat <- tryCatch({
      model.matrix(term, data = md_frame, contrasts = FALSE)
    }, error = function(e){
      if(e$message == "contrasts can be applied only to factors with 2 or more levels") {
        stop("more than 1 unique values are expected for categorical columns assigned as predictors")
      }
      stop(e)
    })
    weight <- model.weights(md_frame)

    y <- model.response(md_frame)


    ret <- if(watchlist_rate != 0.0) {
      index <- sample(seq(nrow(md_mat)), ceiling(nrow(md_mat) * watchlist_rate))
      watch_mat <- xgboost::xgb.DMatrix(data = safe_slice(md_mat ,index), label = y[index])
      train_mat <- xgboost::xgb.DMatrix(data = safe_slice(md_mat ,index, remove = TRUE), label = y[-index])
      xgboost::xgb.train(data = train_mat, watchlist = list(train = train_mat, validation = watch_mat), label = y, weight = weight, nrounds = nrounds, ...)
    } else {
      xgboost::xgboost(data = md_mat, label = y, weight = weight, nrounds = nrounds, ...)
    }
    ret$terms <- term
    ret$x_names <- colnames(md_mat)
    ret$model.matrix <- md_mat
    ret$label <- y
    ret
  }
}

#' formula version of xgboost (multinomial)
#' @param output_type Type of output. Can be "logistic" or "logitraw"
#' The explanation is in https://www.r-bloggers.com/with-our-powers-combined-xgboost-and-pipelearner/
#' @export
xgboost_binary <- function(data, formula, output_type = "logistic", eval_metric = "auc", params = list(), ...) {

  # there can be more than 2 eval_metric
  metric_list <- list()
  default_metrics <- c("auc", "error", "logloss")
  for (metric in default_metrics) {
    if (eval_metric == metric) {
      # indicated metric is first
      metric_list <- append(list(eval_metric = metric), metric_list)
    } else {
      metric_list <- append(metric_list, list(eval_metric = metric))
    }
  }
  if (!eval_metric %in% default_metrics) {
    metric_list <- append(list(eval_metric = eval_metric), metric_list)
  }
  params <- append(metric_list, params)

  vars <- all.vars(formula)

  y_name  <- vars[[1]]

  y_vals <- data[[y_name]]

  # this is used to get back original values from predicted output
  label_levels <- c(0, 1)
  if(is.logical(y_vals)) {
    label_levels <- c(FALSE, TRUE)
    y_vals <- as.numeric(y_vals)
  } else if (!all(y_vals[!is.na(y_vals)] %in% c(0, 1))){
    # there are values that are not 0 or 1, so should be mapped as factor
    factored <- as.factor(data[[y_name]])
    label_levels <- same_type(levels(factored), data[[y_name]])
    if(length(label_levels) != 2){
      stop("target variable must have 2 unique values")
    }
    y_vals <- as.numeric(factored) - 1
  }

  data[[y_name]] <- y_vals
  objective <- paste0("binary:", output_type, sep = "")

  ret <- tryCatch({
    fml_xgboost(data = data,
                formula = formula,
                objective = objective,
                params = params,
                ...)
  }, error = function(e){
    if(stringr::str_detect(e$message, "Check failed: !auc_error AUC: the dataset only contains pos or neg samples")){
      stop("The target only contains positive or negative values")
    }
    stop(e)
  })
  # add class to control S3 methods
  class(ret) <- c("xgboost_binary", class(ret))
  ret$y_levels <- label_levels
  ret
}

#' formula version of xgboost (multinomial)
#' @param output_type Type of output. Can be "softprob" or "softmax"
#' The explanation is in https://www.r-bloggers.com/with-our-powers-combined-xgboost-and-pipelearner/
#' @export
xgboost_multi <- function(data, formula, output_type = "softprob", eval_metric = "merror", params = list(), ...) {
  # there can be more than 2 eval_metric
  metric_list <- list()
  default_metrics <- c("merror", "mlogloss")
  for (metric in default_metrics) {
    if (eval_metric == metric) {
      # indicated metric is first
      metric_list <- append(list(eval_metric = metric), metric_list)
    } else {
      metric_list <- append(metric_list, list(eval_metric = metric))
    }
  }
  if (!eval_metric %in% default_metrics) {
    metric_list <- append(list(eval_metric = eval_metric), metric_list)
  }
  params <- append(metric_list, params)

  vars <- all.vars(formula)

  y_name  <- vars[[1]]
  y_vals <- data[[y_name]]

  # this is used to get back original values from predicted output
  label_levels <- if(is.logical(y_vals)) {
    y_vals <- as.numeric(y_vals)
    c(FALSE, TRUE)
  } else if (is.factor(y_vals)) {
    y_vals <- as.numeric(y_vals) - 1
    # this is sorted unique factor
    # will be used to re-construct factor from index vector with the same level
    sort(unique(data[[y_name]]))
  } else {
    factored <- as.factor(data[[y_name]])
    y_vals <- as.numeric(factored) - 1
    same_type(levels(factored), data[[y_name]])
  }

  data[[y_name]] <- y_vals

  objective <- paste0("multi:", output_type, sep = "")
  ret <- fml_xgboost(data = data,
                     formula = formula,
                     objective = objective,
                     num_class = length(label_levels),
                     params = params,
                     ...)
  # add class to control S3 methods
  class(ret) <- c("xgboost_multi", class(ret))
  ret$fml <- formula
  ret$y_levels <- label_levels
  ret
}

#' formula version of xgboost (regression)
#' @param output_type Type of output. Can be "linear", "logistic", "gamma" or "tweedie"
#' The explanation is in https://www.r-bloggers.com/with-our-powers-combined-xgboost-and-pipelearner/
#' @export
xgboost_reg <- function(data, formula, output_type = "linear", eval_metric = "rmse", params = list(), ...) {
  # there can be more than 2 eval_metric
  metric_list <- list()
  default_metrics <- c("rmse", "mae")
  for (metric in default_metrics) {
    if (eval_metric == metric) {
      # indicated metric is first
      metric_list <- append(list(eval_metric = metric), metric_list)
    } else {
      metric_list <- append(metric_list, list(eval_metric = metric))
    }
  }
  if (!eval_metric %in% default_metrics) {
    metric_list <- append(list(eval_metric = eval_metric), metric_list)
  }
  params <- append(metric_list, params)

  vars <- all.vars(formula)

  y_name  <- vars[1]

  data[[y_name]] <- as.numeric(data[[y_name]])

  objective <- paste0("reg:", output_type, sep = "")

  ret <- fml_xgboost(data = data,
                     formula = formula,
                     objective = objective,
                     params = params, ...)
  # add class to control S3 methods
  class(ret) <- c("xgboost_reg", class(ret))
  ret$fml <- formula
  ret
}

#' Augment predicted values
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @export
augment.xgboost_multi <- function(x, data = NULL, newdata = NULL, ...) {
  class(x) <- class(x)[class(x) != c("xgboost_multi")]

  if(!is.null(x$terms)){

    ret_data <- if(!is.null(newdata)){
      newdata
    } else {
      data
    }

    # todo: check missing value behaviour
    mat <- model.matrix(x$terms, ret_data)
    predicted <- stats::predict(x, mat)

    vars <- all.vars(x$terms)
    y_name <- vars[[1]]

    # create predicted labels for classification
    # based on factor levels and it's indice
    find_label <- function(ids, levels, original_data) {
      levels[ids]
    }
    obj <- x$params$objective
    if (obj == "multi:softmax") {
      predicted_label_col <- avoid_conflict(colnames(ret_data), "predicted_label")
      predicted <- x$y_levels[predicted+1]
      # fill rows with NA
      ret_data[[predicted_label_col]] <- fill_vec_NA(as.integer(rownames(mat)), predicted, max_index = nrow(ret_data))
    } else if (obj == "multi:softprob") {
      predicted_label_col <- avoid_conflict(colnames(ret_data), "predicted_label")
      predicted_prob_col <- avoid_conflict(colnames(ret_data), "predicted_probability")

      # predicted is a vector containing probabilities for each class
      probs <- matrix(predicted, nrow = length(x$y_levels)) %>% t()
      probs <- fill_mat_NA(as.numeric(rownames(mat)), probs, nrow(ret_data))
      colnames(probs) <- x$y_levels
      predicted <- probs %>%
        as.data.frame() %>%
        append_colnames(prefix = "predicted_probability_")

      ret_data <- cbind(ret_data, predicted)

      colmax <- max.col(probs)

      # get max probabilities from each row
      max_prob <- probs[(colmax - 1) * nrow(probs) + seq(nrow(probs))]
      predicted_label <- x$y_levels[colmax]
      # predicted_prob_col is a column for probabilities of chosen values
      ret_data[[predicted_prob_col]] <- max_prob
      ret_data[[predicted_label_col]] <- predicted_label
    }
    ret_data
  } else {
    augment(x, data = data, newdata = newdata, ...)
  }
}

#' Augment predicted values for binary task
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @export
augment.xgboost_binary <- function(x, data = NULL, newdata = NULL, ...) {
  class(x) <- class(x)[!class(x) %in% c("xgboost_binary", "xgb.Booster.formula")]
  if(!is.null(x$terms)){
    ret_data <- if(!is.null(newdata)){
      newdata
    } else {
      data
    }

    mat <- model.matrix(x$terms, data = ret_data)
    # this is to find omitted indice for NA
    row_index <- as.numeric(rownames(mat))
    predicted <- fill_vec_NA(row_index, stats::predict(x, mat), max_index = nrow(ret_data))

    vars <- all.vars(x$terms)
    y_name <- vars[[1]]

    # create predicted labels for classification
    # based on factor levels and it's indice
    find_label <- function(ids, levels, original_data) {
      levels[ids]
    }

    obj <- x$params$objective
    predicted_label_col <- avoid_conflict(colnames(ret_data), "predicted_label")
    predicted_prob_col <- avoid_conflict(colnames(ret_data), "predicted_probability")
    prob <- if (obj == "binary:logistic") {
      predicted_prob_col <- avoid_conflict(colnames(ret_data), "predicted_probability")
      ret_data[[predicted_prob_col]] <- predicted
      predicted
    } else if (obj == "binary:logitraw") {
      predicted_val_col <- avoid_conflict(colnames(ret_data), "predicted_value")

      # binary:logitraw returns logit values
      prob <- boot::inv.logit(predicted)

      ret_data[[predicted_val_col]] <- predicted
      ret_data[[predicted_prob_col]] <- prob
      prob
    } else {
      stop(paste0("object type ", obj, " is not supported"))
    }

    ret_data
  } else {
    augment(x, data = data, newdata = newdata)
  }
}

#' Augment predicted values
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @export
augment.xgboost_reg <- function(x, data = NULL, newdata = NULL, ...) {
  class(x) <- class(x)[class(x) != "xgboost_reg" &
                       class(x) != "xgb.Booster.formula"]

  if(!is.null(x$terms)){
    ret_data <- if(!is.null(newdata)){
      data <- newdata
    } else {
      data
    }

    mat_data <- model.matrix(x$terms, data = ret_data)

    predicted <- stats::predict(x, mat_data)
    predicted_value_col <- avoid_conflict(colnames(ret_data), "predicted_value")
    ret_data[[predicted_value_col]] <- predicted
    ret_data
  } else {
    augment(x, data = data, newdata = newdata)
  }
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
                         class(x) != "xgboost_reg" &
                         class(x) != "fml_xgboost"]
  if(!is.null(newdata)){
    data <- newdata
  }
  mat_data <- if(!is.null(x$x_names)) {
    data[x$x_names]
  } else {
    # use all data if there is no x_names
    data
  }

  mat <- as.matrix(mat_data)

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
                         class(x)!="xgboost_reg" &
                         class(x) != "fml_xgboost"]
  ret <- tryCatch({
    ret <- xgboost::xgb.importance(feature_names = x$x_names,
                                   model=x,
                                   data = x$model.matrix,
                                   label = x$label) %>% as.data.frame()
    if (pretty.name) {
      colnames(ret)[colnames(ret)=="Gain"] <- "Importance"
      colnames(ret)[colnames(ret)=="Cover"] <- "Coverage"
      colnames(ret)[colnames(ret)=="RealCover"] <- "Real Coverage"
      colnames(ret)[colnames(ret)=="RealCover %"] <- "Real Coverage %"
    } else {
      colnames(ret)[colnames(ret)=="Feature"] <- "feature"
      colnames(ret)[colnames(ret)=="Gain"] <- "importance"
      colnames(ret)[colnames(ret)=="Cover"] <- "coverage"
      colnames(ret)[colnames(ret)=="Frequency"] <- "frequency"
      colnames(ret)[colnames(ret)=="Split"] <- "split"
      colnames(ret)[colnames(ret)=="RealCover"] <- "real_coverage"
      colnames(ret)[colnames(ret)=="RealCover %"] <- "real_coverage_%"
      colnames(ret)[colnames(ret)=="Weight"] <- "weight"
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
    # this can be train_error@{threshold}
    with_train_error <- stringr::str_detect(colnames(ret), "^train_error")
    colnames(ret)[with_train_error] <- stringr::str_replace(colnames(ret)[with_train_error], "^train_error", "Misclassification Rate")
    colnames(ret)[colnames(ret) == "train_merror"] <- "Misclassification Rate" # this is for multiclass
    colnames(ret)[colnames(ret) == "train_mlogloss"] <- "Multiclass Logloss"
    colnames(ret)[colnames(ret) == "train_auc"] <- "AUC"
    # this can be train_ndcg@{threshold}
    with_train_ndcg <- stringr::str_detect(colnames(ret), "^train_ndcg")
    colnames(ret)[with_train_ndcg] <- stringr::str_replace(colnames(ret)[with_train_ndcg], "^train_ndcg", "Normalized Discounted Cumulative Gain")
    # this can be train_map@{threshold}
    with_train_map <- stringr::str_detect(colnames(ret), "^train_map")
    colnames(ret)[with_train_map] <- stringr::str_replace(colnames(ret)[with_train_map], "^train_map", "Mean Average Precision")
    colnames(ret)[colnames(ret) == "train_gamma_nloglik"] <- "Gamma Negative Log Likelihood"
    colnames(ret)[colnames(ret) == "train_gamma_deviance"] <- "Gamma Deviance"

    colnames(ret)[colnames(ret) == "validation_rmse"] <- "Validation Root Mean Square Error"
    colnames(ret)[colnames(ret) == "validation_mae"] <- "Validation Mean Absolute Error"
    colnames(ret)[colnames(ret) == "validation_logloss"] <- "Validation Negative Log Likelihood"
    # this can be validation_error@{threshold}
    with_validation_error <- stringr::str_detect(colnames(ret), "^validation_error")
    colnames(ret)[with_validation_error] <- stringr::str_replace(colnames(ret)[with_validation_error], "^validation_error", "Validation Misclassification Rate")
    colnames(ret)[colnames(ret) == "validation_merror"] <- "Validation Misclassification Rate" # this is for multiclass
    colnames(ret)[colnames(ret) == "validation_mlogloss"] <- "Validation Multiclass Logloss"
    colnames(ret)[colnames(ret) == "validation_auc"] <- "Validation AUC"
    # this can be validation_ndcg@{threshold}
    with_validation_ndcg <- stringr::str_detect(colnames(ret), "^validation_ndcg")
    colnames(ret)[with_validation_ndcg] <- stringr::str_replace(colnames(ret)[with_validation_ndcg], "^validation_ndcg", "Validation Normalized Discounted Cumulative Gain")
    # this can be validation_map@{threshold}
    with_validation_map <- stringr::str_detect(colnames(ret), "^validation_map")
    colnames(ret)[with_train_map] <- stringr::str_replace(colnames(ret)[with_train_map], "^train_map", "Validation Mean Average Precision")
    colnames(ret)[colnames(ret) == "validation_ndcg"] <- "Validation Normalized Discounted Cumulative Gain"
    colnames(ret)[colnames(ret) == "validation_map"] <- "Validation Mean Average Precision"
    colnames(ret)[colnames(ret) == "validation_gamma_nloglik"] <- "Validation Gamma Negative Log Likelihood"
    colnames(ret)[colnames(ret) == "validation_gamma_deviance"] <- "Validation Gamma Deviance"
  } else {
    colnames(ret)[colnames(ret) == "iter"] <- "number_of_iteration"
    colnames(ret)[colnames(ret) == "train_rmse"] <- "root_mean_square_error"
    colnames(ret)[colnames(ret) == "train_mae"] <- "mean_absolute_error"
    colnames(ret)[colnames(ret) == "train_logloss"] <- "negative_log_likelihood"
    # this can be train_error@{threshold}
    with_train_error <- stringr::str_detect(colnames(ret), "^train_error")
    colnames(ret)[with_train_error] <- stringr::str_replace(colnames(ret)[with_train_error], "^train_error", "misclassification_rate")
    colnames(ret)[colnames(ret) == "train_merror"] <- "misclassification_rate" # this is for multiclass
    colnames(ret)[colnames(ret) == "train_mlogloss"] <- "multiclass_logloss"
    colnames(ret)[colnames(ret) == "train_auc"] <- "auc"
    # this can be train_ndcg@{threshold}
    with_train_ndcg <- stringr::str_detect(colnames(ret), "^train_ndcg")
    colnames(ret)[with_train_ndcg] <- stringr::str_replace(colnames(ret)[with_train_ndcg], "^train_ndcg", "normalized_discounted_cumulative_gain")
    # this can be train_map@{threshold}
    with_train_map <- stringr::str_detect(colnames(ret), "^train_map")
    colnames(ret)[with_train_map] <- stringr::str_replace(colnames(ret)[with_train_map], "^train_map", "mean_average_precision")
    colnames(ret)[colnames(ret) == "train_gamma_nloglik"] <- "gamma_negative_log_likelihood"
    colnames(ret)[colnames(ret) == "train_gamma_deviance"] <- "gamma_deviance"

    colnames(ret)[colnames(ret) == "validation_rmse"] <- "validation_root_mean_square_error"
    colnames(ret)[colnames(ret) == "validation_mae"] <- "validation_mean_absolute_error"
    colnames(ret)[colnames(ret) == "validation_logloss"] <- "validation_negative_log_likelihood"
    # this can be validation_error@{threshold}
    with_validation_error <- stringr::str_detect(colnames(ret), "^validation_error")
    colnames(ret)[with_validation_error] <- stringr::str_replace(colnames(ret)[with_validation_error], "^validation_error", "validation_misclassification_rate")
    colnames(ret)[colnames(ret) == "validation_merror"] <- "validation_misclassification_rate" # this is for multiclass
    colnames(ret)[colnames(ret) == "validation_mlogloss"] <- "validation_multiclass_logloss"
    colnames(ret)[colnames(ret) == "validation_auc"] <- "validation_auc"
    # this can be validation_ndcg@{threshold}
    with_validation_ndcg <- stringr::str_detect(colnames(ret), "^validation_ndcg")
    colnames(ret)[with_validation_ndcg] <- stringr::str_replace(colnames(ret)[with_validation_ndcg], "^validation_ndcg", "validation_normalized_discounted_cumulative_gain")
    # this can be validation_map@{threshold}
    with_validation_map <- stringr::str_detect(colnames(ret), "^validation_map")
    colnames(ret)[with_train_map] <- stringr::str_replace(colnames(ret)[with_train_map], "^train_map", "validation_mean_average_precision")
    colnames(ret)[colnames(ret) == "validation_gamma_nloglik"] <- "validation_gamma_negative_log_likelihood"
    colnames(ret)[colnames(ret) == "validation_gamma_deviance"] <- "validation_gamma_deviance"
  }
  ret
}

