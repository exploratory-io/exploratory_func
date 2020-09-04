#' formula version of xgboost
#' @param data Dataframe to create model
#' @param formula Formula for model
#' @param nrounds Maximum number of iteration of training
#' @param weights Weight of data for modeling
#' @param watchlist_rate Ratio of validation data to watch learning process
#' @param na.action How to handle data with na
#' Can be na.omit, na.pass, na.fail
#' @param sparse If matrix should be sparse.
#' As default, it becomes sparse if there is any categorical value.
#' @export
fml_xgboost <- function(data, formula, nrounds= 10, weights = NULL, watchlist_rate = 0, na.action = na.pass, sparse = NULL, ...) {
  term <- terms(formula, data = data)
  # do.call is used to substitute weights
  df_for_model_matrix <- tryCatch({
    do.call(model.frame, list(term, data = data, weights = substitute(weights), na.action = na.action))
  }, error = function(e){
    if(e$message == "missing values in object"){
      # this happens when na.action argument is na.fail
      stop("There are NAs in the training data. You might want to set 'na.action' parameter to 'na.pass' or impute NAs manually.")
    }
    stop(e)
  })
  if(nrow(df_for_model_matrix) == 0){
    # this might happen when na.action is na.omit
    stop("No valid data to create xgboost model after removing NA.")
  } else {
    y <- model.response(df_for_model_matrix)

    # NA in y causes an error
    df_for_model_matrix <- df_for_model_matrix[!is.na(y), ]
    y <- y[!is.na(y)]

    md_mat <- tryCatch({
      if(is.null(sparse)){
        sparse <- FALSE
        # If any variable is factor, it uses sparse matrix
        for(var in all.vars(lazyeval::f_rhs(term))){
          if(is.factor(data[[var]])) {
            sparse <- TRUE
          }
        }
      }

      if(sparse){
        tryCatch({
          Matrix::sparse.model.matrix(term, data = df_for_model_matrix)
        }, error = function(e){
          if (e$message == "fnames == names(mf) are not all TRUE"){
            # if there are not clean column names like including spaces or special characters,
            # Matrix::sparse.model.matrix causes this error
            stop("Invalid column names are found. Please run clean_names function beforehand.")
          }
          stop(e)
        })
      } else {
        model.matrix(term, data = df_for_model_matrix, contrasts = FALSE)
      }
    }, error = function(e){
      if(e$message == "contrasts can be applied only to factors with 2 or more levels") {
        stop("more than 1 unique values are expected for categorical columns assigned as predictors")
      }
      stop(e)
    })
    weight <- model.weights(df_for_model_matrix)

    ret <- if(watchlist_rate != 0.0) {
      if (watchlist_rate < 0 ||  1 <= watchlist_rate) {
        stop("watchlist_rate must be between 0 and 1")
      }
      # create validation data set
      index <- sample(seq(nrow(md_mat)), ceiling(nrow(md_mat) * watchlist_rate))
      watch_mat <- xgboost::xgb.DMatrix(data = safe_slice(md_mat ,index), label = y[index])
      train_mat <- xgboost::xgb.DMatrix(data = safe_slice(md_mat ,index, remove = TRUE), label = y[-index])

      xgboost::xgb.train(data = train_mat, watchlist = list(train = train_mat, validation = watch_mat), label = y, weight = weight, nrounds = nrounds, ...)
    } else {
      xgboost::xgboost(data = md_mat, label = y, weight = weight, nrounds = nrounds, ...)
    }
    ret$terms <- term
    # To avoid saving a huge environment when caching with RDS.
    attr(ret$terms,".Environment") <- NULL
    ret$x_names <- colnames(md_mat)
    ret$is_sparse <- sparse
    pred_cnames <- all.vars(term)[-1]
    # this is how categorical columns are casted to columns in matrix
    # this is needed in augment, so that matrix with the same levels
    # can be created
    ret$xlevels <- .getXlevels(term, df_for_model_matrix)
    ret
  }
}

#' formula version of xgboost (multinomial)
#' @param output_type Type of output. Can be "logistic" or "logitraw"
#' The explanation is in https://www.r-bloggers.com/with-our-powers-combined-xgboost-and-pipelearner/
#' @export
xgboost_binary <- function(data, formula, output_type = "logistic", eval_metric = "auc", params = list(), ...) {

  # there can be more than 2 eval_metric
  # by creating eval_metric parameters in params list
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

  original_colnames <- colnames(data)
  # this attribute will be used to get back original column names
  terms_mapping <- original_colnames
  names(terms_mapping) <- original_colnames # No column name change. Just to work with the rest of the code.
  ret$terms_mapping <- terms_mapping
  ret
}

#' formula version of xgboost (multinomial)
#' @param output_type Type of output. Can be "softprob" or "softmax"
#' The explanation is in https://www.r-bloggers.com/with-our-powers-combined-xgboost-and-pipelearner/
#' @export
xgboost_multi <- function(data, formula, output_type = "softprob", eval_metric = "merror", params = list(), ...) {
  # there can be more than 2 eval_metric
  # by creating eval_metric parameters in params list
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
  ret$y_levels <- label_levels

  original_colnames <- colnames(data)
  # this attribute will be used to get back original column names
  terms_mapping <- original_colnames
  names(terms_mapping) <- original_colnames # No column name change. Just to work with the rest of the code.
  ret$terms_mapping <- terms_mapping
  ret
}

#' formula version of xgboost (regression)
#' @param output_type Type of output. Can be "linear", "logistic", "gamma" or "tweedie"
#' The explanation is in https://www.r-bloggers.com/with-our-powers-combined-xgboost-and-pipelearner/
#' @export
xgboost_reg <- function(data, formula, output_type = "linear", eval_metric = "rmse", params = list(), tweedie_variance_power = 1.5, ...) {
  # there can be more than 2 eval_metric
  # by creating eval_metric parameters in params list
  metric_list <- list()
  default_metrics <- c("rmse", "mae")
  if(output_type == "gamma"){
    default_metrics <- c(default_metrics, "gamma-nloglik", "gamma-deviance")
  }
  if(output_type == "tweedie"){
    tvp <- if(!is.null(params$tweedie_variance_power)){
      params$tweedie_variance_power
    } else {
      params$tweedie_variance_power <- tweedie_variance_power
      tweedie_variance_power
    }
    default_metrics <- c(default_metrics, paste0("tweedie-nloglik@", tvp))
  }
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
                     params = params,
                     ...)
  # add class to control S3 methods
  class(ret) <- c("xgboost_reg", class(ret))

  original_colnames <- colnames(data)
  # this attribute will be used to get back original column names
  terms_mapping <- original_colnames
  names(terms_mapping) <- original_colnames # No column name change. Just to work with the rest of the code.
  ret$terms_mapping <- terms_mapping
  ret
}

#' Augment predicted values
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @export
augment.xgboost_multi <- function(x, data = NULL, newdata = NULL, ...) {
  loadNamespace("xgboost") # This is necessary for predict() to successfully figure out which function to call internally.

  predictor_variables <- all.vars(x$terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]

  class(x) <- class(x)[class(x) != c("xgboost_multi")]

  # create clean name data frame because the model learned by those names
  original_data <- if(!is.null(newdata)){
    newdata
  } else {
    data
  }

  cleaned_data <- original_data

  cleaned_data <- cleaned_data %>% dplyr::select(predictor_variables_orig)
  # Rename columns to the normalized ones used while learning.
  colnames(cleaned_data) <- predictor_variables

  # Align factor levels including Others and (Missing) to the model. TODO: factor level order can be different from the model training data. Is this ok?
  cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)

  na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
  if (length(na_row_numbers) > 0) {
    cleaned_data <- cleaned_data[-na_row_numbers,]
  }

  if (nrow(cleaned_data) == 0) {
    return(data.frame())
  }

  # Run prediction.
  predicted <- predict_xgboost(x, cleaned_data)


  obj <- x$params$objective
  if (obj == "multi:softmax") {
    predicted_label_col <- avoid_conflict(colnames(original_data), "predicted_label")
    # fill rows with NA
    original_data[[predicted_label_col]] <- predicted
  } else if (obj == "multi:softprob") {
    predicted_label_col <- avoid_conflict(colnames(original_data), "predicted_label")
    predicted_prob_col <- avoid_conflict(colnames(original_data), "predicted_probability")

    colmax <- max.col(predicted)

    # get max probabilities from each row
    max_prob <- predicted[(colmax - 1) * nrow(predicted) + seq(nrow(predicted))]
    max_prob <- restore_na(max_prob, na_row_numbers)
    predicted_label <- x$y_levels[colmax]
    predicted_label <- restore_na(predicted_label, na_row_numbers)

    original_data <- ranger.set_multi_predicted_values(original_data, as.data.frame(predicted), predicted_label, na_row_numbers)

    # predicted_prob_col is a column for probabilities of chosen values
    original_data[[predicted_prob_col]] <- max_prob
  }

  original_data
}

#' Augment predicted values for binary task
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @export
augment.xgboost_binary <- function(x, data = NULL, newdata = NULL, ...) {
  loadNamespace("xgboost") # This is necessary for predict() to successfully figure out which function to call internally.
  
  predictor_variables <- all.vars(x$terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]
  
  # create clean name data frame because the model learned by those names
  original_data <- if(!is.null(newdata)){
    newdata
  } else {
    data
  }
  
  cleaned_data <- original_data

  cleaned_data <- cleaned_data %>% dplyr::select(predictor_variables_orig)
  # Rename columns to the normalized ones used while learning.
  colnames(cleaned_data) <- predictor_variables

  # Align factor levels including Others and (Missing) to the model. TODO: factor level order can be different from the model training data. Is this ok?
  cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)

  na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
  if (length(na_row_numbers) > 0) {
    cleaned_data <- cleaned_data[-na_row_numbers,]
  }

  if (nrow(cleaned_data) == 0) {
    return(data.frame())
  }

  # Run prediction.
  predicted_val <- predict_xgboost(x, cleaned_data)
  
  # Inserting once removed NA rows
  predicted <- restore_na(predicted_val, na_row_numbers)

  obj <- x$params$objective
  predicted_label_col <- avoid_conflict(colnames(original_data), "predicted_label")
  predicted_prob_col <- avoid_conflict(colnames(original_data), "predicted_probability")
  prob <- if (obj == "binary:logistic") {
    predicted_prob_col <- avoid_conflict(colnames(original_data), "predicted_probability")
    original_data[[predicted_prob_col]] <- predicted
    predicted
  } else if (obj == "binary:logitraw") {
    predicted_val_col <- avoid_conflict(colnames(original_data), "predicted_value")
  
    # binary:logitraw returns logit values
    prob <- boot::inv.logit(predicted)
  
    original_data[[predicted_val_col]] <- predicted
    original_data[[predicted_prob_col]] <- prob
    prob
  } else {
    stop(paste0("object type ", obj, " is not supported"))
  }
  
  original_data
}

#' Augment predicted values
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @export
augment.xgboost_reg <- function(x, data = NULL, newdata = NULL, data_type = "training", ...) {
  loadNamespace("xgboost") # This is necessary for predict() to successfully figure out which function to call internally.

  predicted_value_col <- avoid_conflict(colnames(newdata), "predicted_value")
  predictor_variables <- all.vars(x$terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]

  if(!is.null(newdata)) { # Unlike ranger case, there is no prediction result kept in the model in case of xgboost.
    # create clean name data frame because the model learned by those names
    original_data <- if(!is.null(newdata)){
      newdata
    } else {
      data
    }

    cleaned_data <- original_data

    cleaned_data <- cleaned_data %>% dplyr::select(predictor_variables_orig)
    # Rename columns to the normalized ones used while learning.
    colnames(cleaned_data) <- predictor_variables

    # Align factor levels including Others and (Missing) to the model. TODO: factor level order can be different from the model training data. Is this ok?
    cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)

    na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
    if (length(na_row_numbers) > 0) {
      cleaned_data <- cleaned_data[-na_row_numbers,]
    }

    if (nrow(cleaned_data) == 0) {
      return(data.frame())
    }

    # Run prediction.
    predicted_val <- predict_xgboost(x, cleaned_data)

    # Inserting once removed NA rows
    original_data[[predicted_value_col]] <- restore_na(predicted_val, na_row_numbers)

    original_data
  } else if (!is.null(data)) { #TODO: For Analytics View. Copiled from code for ranger. Adjust for xgboost.
    switch(data_type,
      training = {
        predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
        # Inserting removed NA rows should not be necessary since we don't remove NA rows after test/training split.
        predicted <- extract_predicted.xgboost(x)
        data[[predicted_value_col]] <- predicted
        data
      },
      test = {
        predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
        # Inserting removed NA rows
        predicted_nona <- extract_predicted.xgboost(x, type="test")
        predicted_nona <- restore_na(predicted_nona, attr(x$prediction_test, "unknown_category_rows_index"))
        predicted <- restore_na(predicted_nona, attr(x$prediction_test, "na.action"))
        data[[predicted_value_col]] <- predicted
        data
      })
  }
}

#' @export
augment.xgboost_exp <- function(x, data = NULL, newdata = NULL, ...) {
  if ("xgboost_reg" %in% class(x)) {
    augment.xgboost_reg(x, data, newdata, ...)
  }
  else if ("xgboost_binary" %in% class(x)) {
    augment.xgboost_binary(x, data, newdata, ...)
  }
  else if ("xgboost_multi" %in% class(x)) {
    augment.xgboost_multi(x, data, newdata, ...)
  }
}

#' Augment predicted values
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @export
augment.xgb.Booster <- function(x, data = NULL, newdata = NULL, ...) {
  loadNamespace("xgboost") # This is necessary for predict() to successfully figure out which function to call internally.
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
                                   model=x) %>% as.data.frame()
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
      colnames(ret)[colnames(ret)=="RealCover %"] <- "real_coverage_pct"
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
    colnames(ret)[colnames(ret) == "train_rmse"] <- "RMSE"
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
    # this can be train_tweedie_nloglik@{rho}
    with_train_tweedie_nloglik <- stringr::str_detect(colnames(ret), "^train_tweedie_nloglik")
    colnames(ret)[with_train_tweedie_nloglik] <- stringr::str_replace(colnames(ret)[with_train_tweedie_nloglik], "^train_tweedie_nloglik", "Tweedie Negative Log Likelihood")
    colnames(ret)[colnames(ret) == "train_gamma_nloglik"] <- "Gamma Negative Log Likelihood"
    colnames(ret)[colnames(ret) == "train_gamma_deviance"] <- "Gamma Deviance"

    colnames(ret)[colnames(ret) == "validation_rmse"] <- "Validation RMSE"
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
    # this can be validation_tweedie_nloglik@{rho}
    with_validation_tweedie_nloglik <- stringr::str_detect(colnames(ret), "^validation_tweedie_nloglik")
    colnames(ret)[with_validation_tweedie_nloglik] <- stringr::str_replace(colnames(ret)[with_validation_tweedie_nloglik], "^validation_tweedie_nloglik", "Validation Tweedie Negative Log Likelihood")
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
    # this can be train_tweedie_nloglik@{rho}
    with_train_tweedie_nloglik <- stringr::str_detect(colnames(ret), "^train_tweedie_nloglik")
    colnames(ret)[with_train_tweedie_nloglik] <- stringr::str_replace(colnames(ret)[with_train_tweedie_nloglik], "^train_tweedie_nloglik", "tweedie_negative_log_likelihood")
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
    # this can be validation_tweedie_nloglik@{rho}
    with_validation_tweedie_nloglik <- stringr::str_detect(colnames(ret), "^validation_tweedie_nloglik")
    colnames(ret)[with_validation_tweedie_nloglik] <- stringr::str_replace(colnames(ret)[with_validation_tweedie_nloglik], "^validation_tweedie_nloglik", "validation_tweedie_negative_log_likelihood")
    colnames(ret)[colnames(ret) == "validation_gamma_nloglik"] <- "validation_gamma_negative_log_likelihood"
    colnames(ret)[colnames(ret) == "validation_gamma_deviance"] <- "validation_gamma_deviance"
  }
  ret
}

# XGBoost prediction function that takes data frame rather than matrix.
predict_xgboost <- function(model, df) {
  y_name <- all.vars(model$terms)[[1]]
  if(is.null(df[[y_name]])){
    # if there is no column in the formula (even if it's response variable),
    # model.matrix function causes an error
    # so create the column with 0
    df[[y_name]] <- rep(0, nrow(df))
  }

  mat_data <- if(!is.null(model$is_sparse) && model$is_sparse){
    Matrix::sparse.model.matrix(model$terms, data = model.frame(df, na.action = na.pass, xlev = model$xlevels))
  } else {
    model.matrix(model$terms, model.frame(df, na.action = na.pass, xlev = model$xlevels))
  }

  predicted <- stats::predict(model, mat_data)

  obj <- model$params$objective

  if (obj == "multi:softprob") {
    # predicted is a vector containing probabilities for each class
    probs <- matrix(predicted, nrow = length(model$y_levels)) %>% t()
    # probs <- fill_mat_NA(as.numeric(rownames(mat_data)), probs, nrow(df)) # code from xgboost step.
    probs <- fill_mat_NA(1:nrow(df), probs, nrow(df)) # Not sure if this is necessary. TODO: check.
    colnames(probs) <- model$y_levels
    predicted <- probs
    # We return matrix in this case.
    # Converting it to data.frame is the caller's responsibility.
  }
  else if (obj == "multi:softmax") {
    predicted <- model$y_levels[predicted+1]
    # fill rows with NA
    predicted <- fill_vec_NA(as.integer(rownames(mat_data)), predicted, nrow(df))
  }
  else { # TODO: Here we assume all other cases returns vector prediction result.
    # model.matrix removes rows with NA and stats::predict returns a matrix
    # whose number of rows is the same with its size,
    # so the result should be filled by NA
    predicted <- fill_vec_NA(as.integer(rownames(mat_data)), predicted, nrow(df))
  }
  predicted
}


partial_dependence.xgboost <- function(fit, vars = colnames(data),
  n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)),
  classification = FALSE, interaction = FALSE, uniform = TRUE, data, ...) {

  target = all.vars(fit$terms)[[1]]

  predict.fun = function(object, newdata) {
    if (!classification) {
      predict_xgboost(object, newdata)
    } else {
      # Returned prediction probability matrix works here as is.
      predict_xgboost(object, newdata)
    }
  }

  args = list(
    "data" = data,
    "vars" = vars,
    "n" = n,
    "model" = fit,
    "uniform" = uniform,
    "predict.fun" = predict.fun,
    ...
  )
  
  if (length(vars) > 1L & !interaction) {
    pd = rbindlist(sapply(vars, function(x) {
      args$vars = x
      if ("points" %in% names(args))
        args$points = args$points[x]
      mp = do.call(mmpf::marginalPrediction, args)
      #if (fit$treetype == "Regression")
      if (!classification)
        names(mp)[ncol(mp)] = target
      mp
    }, simplify = FALSE), fill = TRUE)
    data.table::setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else {
    pd = do.call(mmpf::marginalPrediction, args)
    #if (fit$treetype == "Regression")
    if (!classification) # TODO: If we give "regression" for binary classification, would it make sense here?
      names(pd)[ncol(pd)] = target
  }

  attr(pd, "class") = c("pd", "data.frame")
  attr(pd, "interaction") = interaction == TRUE
  #attr(pd, "target") = if (fit$treetype != "Classification") target else levels(fit$predictions)
  attr(pd, "target") = if (!classification) target else levels(fit$predictions)
  attr(pd, "vars") = vars
  pd
}

# This function should return following 2 columns.
# - variable - Name of variable
# - importance - Importance of the variable
# Rows should be sorted by importance in descending order.
importance_xgboost <- function(model) {
  imp <- tidy.xgb.Booster(model)
  ret <- imp %>% dplyr::rename(variable=feature)
  ret <- ret %>% dplyr::mutate(variable = stringr::str_extract(variable,'c\\d+_')) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarize(importance = sum(importance, na.rm=TRUE)) #TODO: Does sum make sense to aggregate this importance?
  ret <- ret %>% dplyr::arrange(-importance)
  ret
}

extract_actual.xgboost <- function(x, type = "training") {
  if (type == "training") {
    actual <- x$df[[all.vars(x$terms)[[1]]]]
  }
  else {
    actual <- x$df_test[[all.vars(x$terms)[[1]]]]
  }
  actual
}

extract_predicted.xgboost <- function(x, type = "training") {
  if (type == "training") {
    predicted <- x$prediction_training
  }
  else {
    predicted <- x$prediction_test
  }
  predicted
}

extract_predicted_binary_labels.xgboost <- function(x, threshold = 0.5, type = "training") {
  if (type == "training") {
    predicted <- x$prediction_training > threshold
  }
  else {
    predicted <- x$prediction_test > threshold
  }
  predicted
}

extract_predicted_multiclass_labels.xgboost <- function(x, type = "training") {
  if (type == "training") {
    predicted <- x$y_levels[apply(x$prediction_training, 1, which.max)]
  }
  else {
    predicted <- x$y_levels[apply(x$prediction_test, 1, which.max)]
  }
  predicted
}

get_prediction_type.xgboost <- function(x) {
  if ("xgboost_reg" %in% class(x)) {
    ret <- "regression"
  }
  if (x$classification_type == "binary") {
    ret <- "binary"
  }
  else {
    ret <- "multiclass"
  }
  ret
}


# Clean up data frame for test
# Removes NAs in predictors - TODO: Is this necessary given our preprocessing before this?
# Removes categorical values that do not appear in training data.
# Returns cleaned data frame.
# attr(ret, "na_row_numbers") - vector of row numbers of NA rows.
# attr(ret, "unknown_category_rows_index") - logical vector that is index of where the unknown category rows are after removing NA rows.
cleanup_df_for_test <- function(df_test, df_train, c_cols) {
  na_row_numbers_test <- ranger.find_na(c_cols, data = df_test)
  df_test_clean <- df_test
  if (length(na_row_numbers_test) > 0) {
    df_test_clean <- df_test_clean[-na_row_numbers_test,]
  }

  names(c_cols) <- NULL # This is necessary to make select in the following line work.
  unknown_category_rows_index_vector <- get_unknown_category_rows_index_vector(df_test_clean, df_train %>% dplyr::select(!!!rlang::syms(c_cols)))
  df_test_clean <- df_test_clean[!unknown_category_rows_index_vector, , drop = FALSE] # 2nd arg must be empty.
  unknown_category_rows_index <- get_row_numbers_from_index_vector(unknown_category_rows_index_vector)

  attr(df_test_clean, "na_row_numbers") <- na_row_numbers_test
  attr(df_test_clean, "unknown_category_rows_index") <- unknown_category_rows_index
  df_test_clean
}

#' Build XGBoost model for Analytics View.
#' @export
exp_xgboost <- function(df,
                        target,
                        ...,
                        max_nrow = 50000, # Down from 200000 when we added partial dependence
                        # max_sample_size = NULL, # Half of max_nrow. down from 100000 when we added partial dependence
                        # ntree = 20,
                        # nodesize = 12,
                        nrounds = 10,
                        target_n = 20,
                        predictor_n = 12, # So that at least months can fit in it.
                        smote = FALSE,
                        smote_target_minority_perc = 40,
                        smote_max_synth_perc = 200,
                        smote_k = 5,
                        # importance_measure = "permutation", # "permutation" or "impurity".
                        max_pd_vars = NULL,
                        # Number of most important variables to calculate partial dependences on. 
                        # By default, when Boruta is on, all Confirmed/Tentative variables.
                        # 12 when Boruta is off.
                        pd_sample_size = 500,
                        pd_grid_resolution = 20,
                        pd_with_bin_means = FALSE, # Default is FALSE for backward compatibility on the server
                        # with_boruta = FALSE,
                        # boruta_max_runs = 20, # Maximal number of importance source runs.
                        # boruta_p_value = 0.05, # Boruta recommends using the default 0.01 for P-value, but we are using 0.05 for consistency with other functions of ours.
                        seed = 1,
                        test_rate = 0.0,
                        test_split_type = "random" # "random" or "ordered"
                        ){
  if(!is.null(seed)){
    set.seed(seed)
  }

  if(test_rate < 0 | 1 < test_rate){
    stop("test_rate must be between 0 and 1")
  } else if (test_rate == 1){
    stop("test_rate must be less than 1")
  }

  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  target_col <- tidyselect::vars_select(names(df), !! rlang::enquo(target))
  # this evaluates select arguments like starts_with
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  # Sort predictors so that the result of permutation importance is stable against change of column order.
  selected_cols <- sort(selected_cols)

  grouped_cols <- grouped_by(df)

  # Remember if the target column was originally numeric or logical before converting type.
  is_target_numeric <- is.numeric(df[[target_col]])
  is_target_logical <- is.logical(df[[target_col]])

  orig_levels <- NULL
  if (is.factor(df[[target_col]])) {
    orig_levels <- levels(df[[target_col]])
  }
  else if (is.logical(df[[target_col]])) {
    orig_levels <- c("TRUE","FALSE")
  }

  clean_ret <- cleanup_df(df, target_col, selected_cols, grouped_cols, target_n, predictor_n)

  clean_df <- clean_ret$clean_df
  name_map <- clean_ret$name_map
  clean_target_col <- clean_ret$clean_target_col
  clean_cols <- clean_ret$clean_cols

  # if target is numeric, it is regression but
  # if not, it is classification
  classification_type <- get_classification_type(clean_df[[clean_target_col]])

  each_func <- function(df) {
    tryCatch({
      # If we are to do SMOTE, do not down sample here and let exp_balance handle it so that we do not sample out precious minority data.
      unique_val <- unique(df[[clean_target_col]])
      if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
        sample_size <- NULL
      }
      else {
        sample_size <- max_nrow
      }
      # XGBoost can work with NAs in numeric predictors. TODO: verify it.
      # Also, no need to convert logical to factor unlike ranger.
      clean_df_ret <- cleanup_df_per_group(df, clean_target_col, sample_size, clean_cols, name_map, predictor_n, filter_numeric_na=FALSE, convert_logical=FALSE)
      if (is.null(clean_df_ret)) {
        return(NULL) # skip this group
      }
      df <- clean_df_ret$df
      c_cols <- clean_df_ret$c_cols
      if  (length(c_cols) == 0) {
        # Previous version of message - stop("The selected predictor variables are invalid since they have only one unique values.")
        stop("Invalid Predictors: Only one unique value.") # Message is made short so that it fits well in the Summary table.
      }
      name_map <- clean_df_ret$name_map

      # apply smote if this is binary classification
      unique_val <- unique(df[[clean_target_col]])
      if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
        df <- df %>% exp_balance(clean_target_col, target_size = max_nrow, target_minority_perc = smote_target_minority_perc, max_synth_perc = smote_max_synth_perc, k = smote_k)
        df <- df %>% dplyr::select(-synthesized) # Remove synthesized column added by exp_balance(). TODO: Handle it better. We might want to show it in resulting data.
      }

      # split training and test data
      source_data <- df
      test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
      df <- safe_slice(source_data, test_index, remove = TRUE)
      if (test_rate > 0) {
        df_test <- safe_slice(source_data, test_index, remove = FALSE)
      }

      # Restore source_data column name to original column name
      rev_name_map <- names(name_map)
      names(rev_name_map) <- name_map
      colnames(source_data) <- rev_name_map[colnames(source_data)]

      # build formula for randomForest
      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      fml <- as.formula(paste(clean_target_col, " ~ ", rhs))

      if (is_target_logical) {
        model <- xgboost_binary(df, fml, nrounds=nrounds) # TODO: Add XGBoost specific parameters.
      }
      else if(is_target_numeric) {
        model <- xgboost_reg(df, fml, nrounds=nrounds) # TODO: Add XGBoost specific parameters.
      }
      else {
        model <- xgboost_multi(df, fml, nrounds=nrounds) # TODO: Add XGBoost specific parameters.
      }
      class(model) <- c("xgboost_exp", class(model))

      model$prediction_training <- predict_xgboost(model, df)

      if (test_rate > 0) {
        df_test_clean <- cleanup_df_for_test(df_test, df, c_cols)
        na_row_numbers_test <- attr(df_test_clean, "na_row_numbers")
        unknown_category_rows_index <- attr(df_test_clean, "unknown_category_rows_index")

        prediction_test <- predict_xgboost(model, df_test_clean)

        attr(prediction_test, "na.action") <- na_row_numbers_test
        attr(prediction_test, "unknown_category_rows_index") <- unknown_category_rows_index
        model$prediction_test <- prediction_test
        model$df_test <- df_test_clean
      }

      # return partial dependence
      if (length(c_cols) > 1) { # Calculate importance only when there are multiple variables.
        imp_df <- importance_xgboost(model)
        model$imp_df <- imp_df
        imp_vars <- imp_df$variable
      }
      else {
        error <- simpleError("Variable importance requires two or more variables.")
        model$imp_df <- error
        imp_vars <- c_cols # Just use c_cols as is for imp_vars to calculate partial dependence anyway.
      }
      if (is.null(max_pd_vars)) {
        max_pd_vars <- 20 # Number of most important variables to calculate partial dependences on. This used to be 12 but we decided it was a little too small.
      }

      imp_vars <- as.character(imp_vars) # for some reason imp_vars is converted to factor at this point. turn it back to character.
      model$imp_vars <- imp_vars
      # Second element of n argument needs to be less than or equal to sample size, to avoid error.
      if (length(imp_vars) > 0) {
        model$partial_dependence <- partial_dependence.xgboost(model, vars=imp_vars, data=df,
                                                               n=c(pd_grid_resolution, min(nrow(df), pd_sample_size)),
                                                               classification=!(is_target_numeric||is_target_logical)) # We treat binary classification as a regression to predict probability here.
        if (pd_with_bin_means && (is_target_logical || is_target_numeric)) {
          # We calculate means of bins only for logical or numeric target to keep the visualization simple.
          model$partial_binning <- calc_partial_binning_data(df, clean_target_col, imp_vars)
        }
      }
      else {
        model$partial_dependence <- NULL
      }

      # these attributes are used in tidy of randomForest
      model$classification_type <- classification_type
      model$orig_levels <- orig_levels
      model$terms_mapping <- names(name_map)
      names(model$terms_mapping) <- name_map
      # model$y <- model.response(df) TODO: what was this??
      model$df <- df
      model$formula_terms <- terms(fml)
      model$sampled_nrow <- clean_df_ret$sampled_nrow
      list(model = model, test_index = test_index, source_data = source_data)
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # In repeat-by case, we report group-specific error in the Summary table,
        # so that analysis on other groups can go on.
        class(e) <- c("xgboost_exp", class(e))
        list(model = e, test_index = NULL, source_data = NULL)
      } else {
        stop(e)
      }
    })
  }

  model_and_data_col <- "model_and_data"
  ret <- do_on_each_group(clean_df, each_func, name = model_and_data_col, with_unnest = FALSE)

  # It is necessary to nest in order to retrieve the result stored in model_and_data_col.
  # If there is a group column, omit the group column from nest, otherwise nest the whole
  if (length(grouped_cols) > 0) {
    ret <- ret %>% tidyr::nest(-grouped_cols)
  } else {
    ret <- ret %>% tidyr::nest()
  }

  ret <- ret %>% dplyr::ungroup() # Remove rowwise grouping so that following mutate works as expected.
  # Retrieve model, test index and source data stored in model_and_data_col column (list) and store them in separate columns
  ret <- ret %>% dplyr::mutate(model = purrr::map(data, function(df){
            df[[model_and_data_col]][[1]]$model
          })) %>%
          dplyr::mutate(.test_index = purrr::map(data, function(df){
            df[[model_and_data_col]][[1]]$test_index
          })) %>%
          dplyr::mutate(source.data = purrr::map(data, function(df){
            data <- df[[model_and_data_col]][[1]]$source_data
            if (length(grouped_cols) > 0 && !is.null(data)) {
              data %>% dplyr::select(-grouped_cols)
            } else {
              data
            }
          })) %>%
          dplyr::select(-data)

  # Rowwise grouping has to be redone with original grouped_cols, so that summarize(tidy(model)) later can add back the group column.
  if (length(grouped_cols) > 0) {
    ret <- ret %>% dplyr::rowwise(grouped_cols)
  } else {
    ret <- ret %>% dplyr::rowwise()
  }

  # If all the groups are errors, it would be hard to handle resulting data frames
  # at the chart preprocessors. Hence, we instead stop the processing here
  # and just throw the error from the first group.
  if (purrr::every(ret$model, function(x) {"error" %in% class(x)})) {
    stop(ret$model[[1]])
  }

  ret
}

# This is used from Analytics View only when classification type is regression.
#' @export
glance.xgboost_exp <- function(x, pretty.name = FALSE, ...) {
  if ("error" %in% class(x)) {
    ret <- data.frame(Note = x$message)
    return(ret)
  }
  if ("xgboost_reg" %in% class(x)) {
    glance.ranger.method <- glance.xgboost_exp.regression
  }
  else {
    stop("glance.xgboost_exp should not be called for classification")
  }
  ret <- glance.ranger.method(x, pretty.name = pretty.name, ...)
  ret
}

#' @export
glance.xgboost_exp.regression <- function(x, pretty.name, ...) {
  predicted <- extract_predicted.xgboost(x)
  actual <- extract_actual.xgboost(x)
  root_mean_square_error <- rmse(predicted, actual)
  rsq <- r_squared(actual, predicted)
  n <- length(actual)
  ret <- data.frame(
    # root_mean_square_error = sqrt(x$prediction.error),
    # r_squared = x$r.squared
    root_mean_square_error = root_mean_square_error,
    r_squared = rsq,
    n = n
  )

  if(pretty.name){
    map = list(
      `RMSE` = as.symbol("root_mean_square_error"),
      `R Squared` = as.symbol("r_squared"),
      `Number of Rows` = as.symbol("n")
    )
    ret <- ret %>%
      dplyr::rename(!!!map)
  }
  ret
}

#' @export
#' @param type "importance", "evaluation" or "conf_mat". Feature importance, evaluated scores or confusion matrix of training data.
tidy.xgboost_exp <- function(x, type = "importance", pretty.name = FALSE, binary_classification_threshold = 0.5, ...) {
  if ("error" %in% class(x) && type != "evaluation") {
    ret <- data.frame()
    return(ret)
  }
  switch(
    type,
    importance = {
      if ("error" %in% class(x$imp_df)) {
        # Permutation importance is not supported for the family and link function, or skipped because there is only one variable.
        # Return empty data.frame to avoid error.
        ret <- data.frame()
        return(ret)
      }
      ret <- x$imp_df
      ret <- ret %>% dplyr::mutate(variable = x$terms_mapping[variable]) # map variable names to original.
      ret
    },
    evaluation = {
      # Delegate showing error for failed models to grance().
      if ("error" %in% class(x)) {
        return(glance(x, pretty.name = pretty.name, ...))
      }
      # get evaluation scores from training data
      actual <- extract_actual.xgboost(x)
      if(is.numeric(actual)){
        glance(x, pretty.name = pretty.name, ...)
      } else {
        if (x$classification_type == "binary") {
          predicted <- extract_predicted_binary_labels.xgboost(x, threshold = binary_classification_threshold)
          predicted_probability <- extract_predicted.xgboost(x)
          ret <- evaluate_binary_classification(actual, predicted, predicted_probability, pretty.name = pretty.name)
        }
        else {
          predicted <- extract_predicted_multiclass_labels.xgboost(x)
          ret <- evaluate_multi_(data.frame(predicted=predicted, actual=actual), "predicted", "actual", pretty.name = pretty.name)
        }
        ret
      }
    },
    conf_mat = {
      # return confusion matrix
      actual <- extract_actual.xgboost(x)
      if (x$classification_type == "binary") {
        predicted <- extract_predicted_binary_labels.xgboost(x, threshold = binary_classification_threshold)
      }
      else {
        predicted <- extract_predicted_multiclass_labels.xgboost(x)
      }

      ret <- data.frame(
        actual_value = actual,
        predicted_value = predicted
      ) %>%
        dplyr::filter(!is.na(predicted_value))

      # get count if it's classification
      ret <- ret %>%
        dplyr::group_by(actual_value, predicted_value) %>%
        dplyr::summarize(count = n()) %>%
        dplyr::ungroup()

      ret
    },
    partial_dependence = {
      ret <- handle_partial_dependence(x)
      ret
    },
    {
      stop(paste0("type ", type, " is not defined"))
    })
}

