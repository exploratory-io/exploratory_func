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
            stop("EXP-ANA-3 :: [] :: Invalid column names are found. Please run clean_names function beforehand.")
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
  # but specify only one so that it is clear what is used for early stopping.
  metric_list <- list(eval_metric = eval_metric)
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
    label_levels <- to_same_type(levels(factored), data[[y_name]])
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
  # but specify only one so that it is clear what is used for early stopping.
  metric_list <- list(eval_metric = eval_metric)
  params <- append(metric_list, params)

  vars <- all.vars(formula)

  y_name  <- vars[[1]]
  y_vals <- data[[y_name]]

  # this is used to get back original values from predicted output
  label_levels <- if(is.logical(y_vals)) {
    c(FALSE, TRUE)
  } else if (is.factor(y_vals)) {
    # this is sorted unique factor
    # will be used to re-construct factor from index vector with the same level
    sort(unique(data[[y_name]]))
  } else {
    factored <- as.factor(data[[y_name]])
    to_same_type(levels(factored), data[[y_name]])
  }

  if(is.logical(y_vals)) {
    y_vals <- as.numeric(y_vals)
  } else if (is.factor(y_vals)) {
    # Map the levels to integers from 0 to (number of levels - 1)
    mapping <- 0:(length(label_levels)-1)
    names(mapping) <- as.character(label_levels)
    y_vals <- mapping[as.character(y_vals)]
  } else {
    factored <- as.factor(data[[y_name]])
    y_vals <- as.numeric(factored) - 1
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
xgboost_reg <- function(data, formula, output_type = "linear", eval_metric = NULL, params = list(), tweedie_variance_power = 1.5, ...) {
  # There can be more than 2 eval_metric
  # by creating eval_metric parameters in params list,
  # but specify only one so that it is clear what is used for early stopping.
  if (is.null(eval_metric)) {
    if(output_type == "gamma"){
      eval_metric <- "gamma-nloglik"
    }
    else if(output_type == "tweedie"){
      tvp <- if(!is.null(params$tweedie_variance_power)){
        params$tweedie_variance_power
      } else {
        params$tweedie_variance_power <- tweedie_variance_power
        tweedie_variance_power
      }
      eval_metric <- paste0("tweedie-nloglik@", tvp)
    }
    else {
      eval_metric <- "rmse"
    }
  }
  metric_list <- list(eval_metric = eval_metric)
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
augment.xgboost_multi <- function(x, data = NULL, newdata = NULL, data_type = "training", ...) {
  loadNamespace("xgboost") # This is necessary for predict() to successfully figure out which function to call internally.

  predictor_variables <- all.vars(x$terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]

  if(!is.null(newdata)) { # Unlike ranger case, there is no prediction result kept in the model in case of xgboost.
    # Replay the mutations on predictors.
    if(!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    class(x) <- class(x)[class(x) != c("xgboost_multi")]

    # create clean name data frame because the model learned by those names
    original_data <- if(!is.null(newdata)){
      newdata
    } else {
      data
    }

    cleaned_data <- original_data %>% dplyr::select(predictor_variables_orig)
    # Rename columns to the normalized ones used while learning.
    colnames(cleaned_data) <- predictor_variables

    # Align factor levels including Others and (Missing) to the model. TODO: factor level order can be different from the model training data. Is this ok?
    if (!is.null(x$df)) { # Model on Analytics Step does not have x$df.
      cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)
    }

    # For new data prediction, xgboost can predict with NAs in the predictors. Disable NA filtering for now.
    # na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
    na_row_numbers <- c()
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
      predicted_prob_col <- avoid_conflict(colnames(original_data), "predicted_probability")

      colmax <- max.col(predicted)

      # get max probabilities from each row
      max_prob <- predicted[(colmax - 1) * nrow(predicted) + seq(nrow(predicted))]
      max_prob <- restore_na(max_prob, na_row_numbers)
      predicted_label <- x$y_levels[colmax]
      predicted_label <- restore_na(predicted_label, na_row_numbers)

      original_data <- ranger.set_multi_predicted_values(original_data, as.data.frame(predicted), predicted_label, max_prob, na_row_numbers)
    }
    original_data
  } else if (!is.null(data)) { # For Analytics View.
    if (nrow(data) == 0) { #TODO: better place to do this check?
      return(data.frame())
    }
    data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col()) # Bring the target column to the last so that it is next to the predicted value in the output.
    predicted_prob_col <- avoid_conflict(colnames(data), "predicted_probability")
    switch(data_type,
      training = {
        # Inserting removed NA rows should not be necessary since we don't remove NA rows after test/training split.
        predicted_value <- extract_predicted_multiclass_labels(x, type="training")

        predicted <- extract_predicted(x, type="training")

        # predicted_prob_col is a column for probabilities of chosen values
        colmax <- max.col(predicted)
        max_prob <- predicted[(colmax - 1) * nrow(predicted) + seq(nrow(predicted))]

        data <- ranger.set_multi_predicted_values(data, as.data.frame(predicted), predicted_value, max_prob, c())
        data
      },
      test = {
        predicted_value_nona <- extract_predicted_multiclass_labels(x, type="test")
        predicted_value_nona <- restore_na(predicted_value_nona, attr(x$prediction_test, "unknown_category_rows_index"))
        predicted_value <- restore_na(predicted_value_nona, attr(x$prediction_test, "na.action"))

        predicted <- extract_predicted(x, type="test")

        # predicted_prob_col is a column for probabilities of chosen values
        colmax <- max.col(predicted)
        max_prob_nona <- predicted[(colmax - 1) * nrow(predicted) + seq(nrow(predicted))]
        max_prob_nona <- restore_na(max_prob_nona, attr(x$prediction_test, "unknown_category_rows_index"))
        max_prob <- restore_na(max_prob_nona, attr(x$prediction_test, "na.action"))

        data <- ranger.set_multi_predicted_values(data, as.data.frame(predicted), predicted_value, max_prob, attr(x$prediction_test, "na.action"), attr(x$prediction_test, "unknown_category_rows_index"))
        data
      })
  }
}

#' Augment predicted values for binary task
#' @param x xgb.Booster model
#' @param data Data frame used to train xgb.Booster
#' @param newdata New data frame to predict
#' @param ... Not used for now.
#' @export
augment.xgboost_binary <- function(x, data = NULL, newdata = NULL, data_type = "training", binary_classification_threshold = 0.5, ...) {
  loadNamespace("xgboost") # This is necessary for predict() to successfully figure out which function to call internally.
  
  predictor_variables <- all.vars(x$terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]
  
  # create clean name data frame because the model learned by those names
  if(!is.null(newdata)) { # Unlike ranger case, there is no prediction result kept in the model in case of xgboost.
    # Replay the mutations on predictors.
    if(!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    original_data <- newdata
  
    cleaned_data <- original_data %>% dplyr::select(predictor_variables_orig)
    # Rename columns to the normalized ones used while learning.
    colnames(cleaned_data) <- predictor_variables

    # Align factor levels including Others and (Missing) to the model.
    if (!is.null(x$df)) { # Model on Analytics Step does not have x$df.
      cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)
    }

    # For new data prediction, xgboost can predict with NAs in the predictors. Disable NA filtering for now.
    # na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
    na_row_numbers <- c()
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
    predicted_val_col <- avoid_conflict(colnames(original_data), "predicted_label")
    predicted_prob_col <- avoid_conflict(colnames(original_data), "predicted_probability")
    prob <- if (obj == "binary:logistic") {
      original_data[[predicted_prob_col]] <- predicted
      original_data[[predicted_val_col]] <- predicted > binary_classification_threshold
      predicted
    } else if (obj == "binary:logitraw") {
    
      # binary:logitraw returns logit values
      prob <- boot::inv.logit(predicted)
    
      original_data[[predicted_prob_col]] <- prob
      original_data[[predicted_val_col]] <- predicted
      prob
    } else {
      stop(paste0("object type ", obj, " is not supported"))
    }
    
    original_data
  } else if (!is.null(data)) { # For Analytics View.
    if (nrow(data) == 0) { #TODO: better place to do this check?
      return(data.frame())
    }
    data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col()) # Bring the target column to the last so that it is next to the predicted value in the output.
    predicted_value_col <- avoid_conflict(colnames(data), "predicted_label")
    predicted_probability_col <- avoid_conflict(colnames(data), "predicted_probability")
    switch(data_type,
      training = {
        # Inserting removed NA rows should not be necessary since we don't remove NA rows after test/training split.
        predicted_prob <- extract_predicted(x)
        predicted_value <- extract_predicted_binary_labels(x, threshold = binary_classification_threshold)
      },
      test = {
        predicted_prob_nona <- extract_predicted(x, type="test")
        predicted_value_nona <- extract_predicted_binary_labels(x, type="test", threshold = binary_classification_threshold)

        predicted_prob_nona <- restore_na(predicted_prob_nona, attr(x$prediction_test, "unknown_category_rows_index"))
        predicted_value_nona <- restore_na(predicted_value_nona, attr(x$prediction_test, "unknown_category_rows_index"))

        predicted_prob <- restore_na(predicted_prob_nona, attr(x$prediction_test, "na.action"))
        predicted_value <- restore_na(predicted_value_nona, attr(x$prediction_test, "na.action"))
      })
    data[[predicted_value_col]] <- predicted_value
    data[[predicted_probability_col]] <- predicted_prob
    data
  }
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
    # Replay the mutations on predictors.
    if(!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    # create clean name data frame because the model learned by those names
    original_data <- if(!is.null(newdata)){
      newdata
    } else {
      data
    }

    cleaned_data <- original_data %>% dplyr::select(predictor_variables_orig)
    # Rename columns to the normalized ones used while learning.
    colnames(cleaned_data) <- predictor_variables

    # Align factor levels including Others and (Missing) to the model.
    if (!is.null(x$df)) { # Model on Analytics Step does not have x$df.
      cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)
    }

    # For new data prediction, xgboost can predict with NAs in the predictors. Disable NA filtering for now.
    # na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
    na_row_numbers <- c()
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
  } else if (!is.null(data)) { # For Analytics View.
    if (nrow(data) == 0) { #TODO: better place to do this check?
      return(data.frame())
    }
    data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col()) # Bring the target column to the last so that it is next to the predicted value in the output.
    switch(data_type,
      training = {
        predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
        # Inserting removed NA rows should not be necessary since we don't remove NA rows after test/training split.
        predicted <- extract_predicted(x)
        data[[predicted_value_col]] <- predicted
        data
      },
      test = {
        predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
        # Inserting removed NA rows
        predicted_nona <- extract_predicted(x, type="test")
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

prettify_xgboost_evaluation_log <- function(df, pretty.name=FALSE) {
  ret <- df
  if(pretty.name){
    colnames(ret)[colnames(ret) == "iter"] <- "Number of Iteration"
    colnames(ret)[colnames(ret) == "train_rmse"] <- "RMSE"
    colnames(ret)[colnames(ret) == "train_mae"] <- "Mean Absolute Error"
    colnames(ret)[colnames(ret) == "train_logloss"] <- "Negative Log Likelihood"
    # this can be train_error@{threshold}
    with_train_error <- stringr::str_detect(colnames(ret), "^train_error")
    colnames(ret)[with_train_error] <- stringr::str_replace(colnames(ret)[with_train_error], "^train_error", "Misclass. Rate")
    colnames(ret)[colnames(ret) == "train_merror"] <- "Misclass. Rate" # this is for multiclass
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
    colnames(ret)[with_validation_error] <- stringr::str_replace(colnames(ret)[with_validation_error], "^validation_error", "Validation Misclass. Rate")
    colnames(ret)[colnames(ret) == "validation_merror"] <- "Validation Misclass. Rate" # this is for multiclass
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

#' Glance for xgb.Booster model
#' @param x xgb.Booster model
#' @param ... Not used for now.
#' @export
glance.xgb.Booster <- function(x, pretty.name = FALSE, ...) {
  # data frame with
  # number of iteration
  # with chosen evaluation metrics
  ret <- x$evaluation_log %>% as.data.frame()
  ret <- prettify_xgboost_evaluation_log(ret, pretty.name=TRUE)
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

# Calculates permutation importance for regression by xgboost.
calc_permutation_importance_xgboost_regression <- function(fit, target, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(data, var, target, fit, nperm = 1, # By default, it creates 100 permuted data sets. We do just 1 for performance.
                                predict.fun = function(object,newdata){predict_xgboost(object, newdata)},
                                # For some reason, default loss.fun, which is mean((x - y)^2) returns NA, even with na.rm=TRUE. Rewrote it with sum() to avoid the issue.
                                loss.fun = function(x,y){sum((x - y)^2, na.rm = TRUE)/length(x)})
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble::tibble(variable=vars, importance=pmax(importances, 0)) # Show 0 for negative importance, which can be caused by chance in case of permutation importance.
  importances_df <- importances_df %>% dplyr::arrange(-importance)
  importances_df
}

calc_permutation_importance_xgboost_binary <- function(fit, target, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(data, var, target, fit, nperm = 1, # By default, it creates 100 permuted data sets. We do just 1 for performance.
                                predict.fun = function(object,newdata){predict_xgboost(object, newdata)},
                                loss.fun = function(x,y){-sum(log(1- abs(x - y[[1]])), na.rm = TRUE)} # Negative-log-likelihood-based loss function.
                                # loss.fun = function(x,y){-auroc(x,y[[1]])} # AUC based. y is actually a single column data.frame rather than a vector. TODO: Fix it in permutationImportance() to make it a vector.
                                )
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble(variable=vars, importance=pmax(importances, 0)) # Show 0 for negative importance, which can be caused by chance in case of permutation importance.
  importances_df <- importances_df %>% dplyr::arrange(-importance)
  importances_df
}

calc_permutation_importance_xgboost_multiclass <- function(fit, target, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(data, var, target, fit, nperm = 1, # By default, it creates 100 permuted data sets. We do just 1 for performance.
                                predict.fun = function(object,newdata){predict_xgboost(object, newdata)},
                                # loss.fun = function(x,y){1-sum(colnames(x)[max.col(x)]==y[[1]], na.rm=TRUE)/length(y[[1]])} # misclassification rate
                                loss.fun = function(x,y){sum(-log(x[match(y[[1]][row(x)], colnames(x))==col(x)]), na.rm = TRUE)} # Negative log likelihood. https://ljvmiranda921.github.io/notebook/2017/08/13/softmax-and-the-negative-log-likelihood/
                                )
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble(variable=vars, importance=pmax(importances, 0)) # Show 0 for negative importance, which can be caused by chance in case of permutation importance.
  importances_df <- importances_df %>% dplyr::arrange(-importance)
  importances_df
}


# TODO: Make this function model-agnostic and consolidate. There are similar code for lm/glm, ranger, rpart, and xgboost.
# Builds partial dependence data.
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

  # Generate grid points based on quantile, so that FIRM calculated based on it would make good sense even when there are some outliers.
  points <- list()
  quantile_points <- list()
  for (cname in vars) {
    if (is.numeric(data[[cname]])) {
      coldata <- data[[cname]]
      minv <- min(coldata, na.rm=TRUE)
      maxv <- max(coldata, na.rm=TRUE)
      grid <- minv + (0:20)/20 * (maxv - minv)
      quantile_grid <- quantile(coldata, probs=1:24/25)
      quantile_points[[cname]] <- quantile_grid
      points[[cname]] <- sort(unique(c(grid, quantile_grid)))
    }
    else {
      points[[cname]] <- unique(data[[cname]])
    }
  }

  args = list(
    "data" = data,
    "vars" = vars,
    "n" = n,
    "model" = fit,
    "points" = points,
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
  # Since levels(fit$y_levels) returns unused levels, it has to be as.character(fit$y_levels).
  attr(pd, "target") = if (!classification) target else as.character(fit$y_levels)
  attr(pd, "vars") = vars
  attr(pd, "points") = points
  attr(pd, "quantile_points") = quantile_points
  pd
}

# This function should return following 2 columns.
# - variable - Name of variable
# - importance - Importance of the variable
# Rows should be sorted by importance in descending order.
importance_xgboost <- function(model) {
  imp <- tidy.xgb.Booster(model)
  if ("Error" %in% colnames(imp)) { # In case of Error, return error object to report this error without giving up on the entire model.
    ret <- simpleError(imp$Error)
    return(ret)
  }
  ret <- imp %>% dplyr::rename(variable=feature)
  ret <- ret %>% dplyr::mutate(variable = stringr::str_extract(variable,'c\\d+_')) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarize(importance = sum(importance, na.rm=TRUE)) #TODO: Does sum make sense to aggregate this importance?
  ret <- ret %>% dplyr::arrange(-importance)
  ret
}

# Model specific S3 functions.
extract_actual <- function(x, ...) {UseMethod("extract_actual", x)}
extract_predicted <- function(x, ...) {UseMethod("extract_predicted", x)}
extract_predicted_binary_labels <- function(x, ...) {UseMethod("extract_predicted_binary_labels", x)}
extract_predicted_multiclass_labels <- function(x, ...) {UseMethod("extract_predicted_multiclass_labels", x)}
get_prediction_type <- function(x, ...) {UseMethod("get_prediction_type", x)}

extract_actual.xgboost_exp <- function(x, type = "training") {
  if (type == "training") {
    actual <- x$df[[all.vars(x$terms)[[1]]]]
  }
  else {
    actual <- x$df_test[[all.vars(x$terms)[[1]]]]
  }
  actual
}

extract_predicted.xgboost_exp <- function(x, type = "training") {
  if (type == "training") {
    predicted <- x$prediction_training
  }
  else {
    predicted <- x$prediction_test
  }
  predicted
}
extract_predicted.xgboost_reg <- extract_predicted.xgboost_exp
extract_predicted.xgboost_binary <- extract_predicted.xgboost_exp
extract_predicted.xgboost_multi <- extract_predicted.xgboost_exp


extract_predicted_binary_labels.xgboost_exp <- function(x, threshold = 0.5, type = "training") {
  if (type == "training") {
    predicted <- x$prediction_training > threshold
  }
  else {
    predicted <- x$prediction_test > threshold
  }
  predicted
}
extract_predicted_binary_labels.xgboost_binary <- extract_predicted_binary_labels.xgboost_exp

extract_predicted_multiclass_labels.xgboost_exp <- function(x, type = "training") {
  if (type == "training") {
    predicted <- x$y_levels[apply(x$prediction_training, 1, which.max)]
  }
  else {
    predicted <- x$y_levels[apply(x$prediction_test, 1, which.max)]
  }
  predicted
}
extract_predicted_multiclass_labels.xgboost_multi <- extract_predicted_multiclass_labels.xgboost_exp

get_prediction_type.xgboost_exp <- function(x) {
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
  predicted <- extract_predicted(x)
  actual <- extract_actual(x)
  root_mean_square_error <- rmse(predicted, actual)
  rsq <- r_squared(actual, predicted)
  n <- length(actual)
  ret <- data.frame(
    # root_mean_square_error = sqrt(x$prediction.error),
    # r_squared = x$r.squared
    r_squared = rsq,
    root_mean_square_error = root_mean_square_error,
    n = n
  )

  if(pretty.name){
    map = list(
      `R Squared` = as.symbol("r_squared"),
      `RMSE` = as.symbol("root_mean_square_error"),
      `Rows` = as.symbol("n")
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
      actual <- extract_actual(x)
      if(is.numeric(actual)){
        glance(x, pretty.name = pretty.name, ...)
      } else {
        if (x$classification_type == "binary") {
          predicted <- extract_predicted_binary_labels(x, threshold = binary_classification_threshold)
          predicted_probability <- extract_predicted(x)
          ret <- evaluate_binary_classification(actual, predicted, predicted_probability, pretty.name = pretty.name)
        }
        else {
          predicted <- extract_predicted_multiclass_labels(x)
          ret <- evaluate_multi_(data.frame(predicted=predicted, actual=actual), "predicted", "actual", pretty.name = pretty.name)
        }
        ret
      }
    },
    evaluation_by_class = { # We assume this is called only for multiclass classification.
      # Delegate showing error for failed models to grance().
      if ("error" %in% class(x)) {
        return(glance(x, pretty.name = pretty.name, ...))
      }
      # get evaluation scores from training data
      actual <- extract_actual(x)
      predicted <- extract_predicted_multiclass_labels(x)

      per_level <- function(level) {
        ret <- evaluate_classification(actual, predicted, level, pretty.name = pretty.name)
        ret
      }
      dplyr::bind_rows(lapply(levels(actual), per_level))
    },
    conf_mat = {
      # return confusion matrix
      actual <- extract_actual(x)
      if (x$classification_type == "binary") {
        predicted <- extract_predicted_binary_labels(x, threshold = binary_classification_threshold)
      }
      else {
        predicted <- extract_predicted_multiclass_labels(x)
      }

      ret <- calc_conf_mat(actual, predicted)
      ret
    },
    partial_dependence = {
      ret <- handle_partial_dependence(x)
      ret
    },
    evaluation_log = {
      # data frame with
      # number of iteration
      # with chosen evaluation metrics
      ret <- x$evaluation_log %>% as.data.frame()
      ret <- ret %>% tidyr::pivot_longer(cols = c(-iter))
      ret <- ret %>% tidyr::separate(col = "name", into=c("type","name"))
      ret
    },
    {
      stop(paste0("type ", type, " is not defined"))
    })
}
