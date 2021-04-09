#' Random Forest wrapper for regression
#' Differences from randomForest::randomForest
#' * When . is used in right hand side of formula,
#'   only numeric/logical columns are used as predictors.
#' * terms_mapping attribute is added to model
#'   for keeping mapping of original column names and cleaned-up column names.
#' @export
randomForestReg <- function(data, formula, na.action = na.omit, ...) {
  target_col <- all.vars(formula)[[1]]

  if(!is.numeric(data[[target_col]])){
    stop("Target must be numeric column")
  }

  original_colnames <- colnames(data)

  target_col_index <- which(colnames(data) == target_col)

  # randomForest must take clean names
  data <- janitor::clean_names(data)
  updated_colnames <- colnames(data)
  names(updated_colnames) <- original_colnames

  # get target col as clean name
  target_col <- colnames(data)[target_col_index]

  if("." %in% all.vars(lazyeval::f_rhs(formula))){
    # somehow, mixing numeric and categorical predictors causes an error in randomForest
    # so use only numeric or logical columns
    cols <- quantifiable_cols(data)
    vars <- cols[!cols %in% target_col]
    formula <- as.formula(paste0(target_col, " ~ ", paste0(vars, collapse = " + ")))
  } else {
    vars <- all.vars(formula)
    newvars <- updated_colnames[vars]
    formula <- as.formula(paste0(newvars[[1]], " ~ ", paste0(newvars[-1], collapse = " + ")))
  }

  ret <- tryCatch({
    randomForest::randomForest(formula = formula, data = data, na.action = na.action, ...)
  }, error = function(e){
    if (e$message == "NA/NaN/Inf in foreign function call (arg 1)"){
      # TODO: Should find the root cause of this error because indicating
      # numerical and categorical predictors doesn't always cause this error
      stop("Categorical and numerical predictors can't be used at the same time.")
    }
    stop(e)
  })

  # this attribute will be used to get back original column names
  terms_mapping <- original_colnames
  names(terms_mapping) <- updated_colnames
  ret$terms_mapping <- terms_mapping

  ret
}

#' Random Forest wrapper for classification
#' This is needed because boolean target starts regression task,
#' not classification task
#' Differences from randomForest::randomForest
#' * When . is used in right hand side of formula,
#'   only numeric/logical columns are used as predictors.
#' * terms_mapping attribute is added to model
#'   for keeping mapping of original column names and cleaned-up column names.
#' @export
randomForestBinary <- function(data, formula, na.action = na.omit, ...) {
  target_col <- all.vars(formula)[[1]]
  original_val <- data[[target_col]]
  original_colnames <- colnames(data)

  target_col_index <- which(colnames(data) == target_col)

  # randomForest must take clean names
  data <- janitor::clean_names(data)
  updated_colnames <- colnames(data)
  # this will be used to get original colmn names later
  names(updated_colnames) <- original_colnames
  # get target col as clean name
  target_col <- colnames(data)[target_col_index]

  data[[target_col]] <- as.factor(data[[target_col]])

  if(!is.logical(original_val) &&
     length(levels(data[[target_col]] )) != 2){
    stop("There should be 2 unique values for binary classification.")
  }

  if("." %in% all.vars(lazyeval::f_rhs(formula))){
    # somehow, mixing numeric and categorical predictors causes an error in randomForest
    # so use only numeric or logical columns
    cols <- quantifiable_cols(data)
    vars <- cols[!cols %in% target_col]
    formula <- as.formula(paste0(target_col, " ~ ", paste0(vars, collapse = " + ")))
  } else {
    vars <- all.vars(formula)
    newvars <- updated_colnames[vars]
    formula <- as.formula(paste0(newvars[[1]], " ~ ", paste0(newvars[-1], collapse = " + ")))
  }

  ret <- tryCatch({
    randomForest::randomForest(formula = formula, data = data, na.action = na.omit, ...)
  }, error = function(e){
    if (e$message == "NA/NaN/Inf in foreign function call (arg 1)"){
      stop("Categorical and numerical predictors can't be used at the same time.")
    }
    stop(e)
  })

  # this attribute will be used to get back original column names
  terms_mapping <- original_colnames
  names(terms_mapping) <- updated_colnames
  ret$terms_mapping <- terms_mapping
  ret$classification_type = "binary"

  ret
}

#' Random Forest wrapper for classification
#' This is needed because boolean target starts regression task,
#' not classification task
#' Differences from randomForest::randomForest
#' * When . is used in right hand side of formula,
#'   only numeric/logical columns are used as predictors.
#' * terms_mapping attribute is added to model
#'   for keeping mapping of original column names and cleaned-up column names.
#' @export
randomForestMulti <- function(data, formula, na.action = na.omit, ...) {
  target_col <- all.vars(formula)[[1]]
  original_val <- data[[target_col]]
  original_colnames <- colnames(data)

  target_col_index <- which(colnames(data) == target_col)

  # randomForest must take clean names
  data <- janitor::clean_names(data)
  updated_colnames <- colnames(data)
  names(updated_colnames) <- original_colnames

  # get target col as clean name
  target_col <- colnames(data)[target_col_index]

  data[[target_col]] <- as.factor(data[[target_col]])

  if("." %in% all.vars(lazyeval::f_rhs(formula))){
    # somehow, mixing numeric and categorical predictors causes an error in randomForest
    # so use only numeric or logical columns
    cols <- quantifiable_cols(data)
    vars <- cols[!cols %in% target_col]
    formula <- as.formula(paste0(target_col, " ~ ", paste0(vars, collapse = " + ")))
  } else {
    vars <- all.vars(formula)
    newvars <- updated_colnames[vars]
    formula <- as.formula(paste0(newvars[[1]], " ~ ", paste0(newvars[-1], collapse = " + ")))
  }

  ret <- tryCatch({
    randomForest::randomForest(formula = formula, data = data, na.action = na.omit, ...)
  }, error = function(e){
    if (e$message == "NA/NaN/Inf in foreign function call (arg 1)"){
      stop("Categorical and numerical predictors can't be used at the same time.")
    }
    stop(e)
  })

  if(is.logical(original_val) ||
     length(levels(data[[target_col]] )) == 2){
    ret$classification_type = "binary"
  } else {
    ret$classification_type = "multi"
  }

  # this attribute will be used to get back original column names
  terms_mapping <- original_colnames
  names(terms_mapping) <- updated_colnames
  ret$terms_mapping <- terms_mapping

  ret
}

# Common routine use for binary/multinomial classification and regression
# TODO: Make it a common routine and use it from calc_feature_imp too.
rangerCore <- function(data, formula, na.action = na.omit,
                       importance_mode = "permutation",
                       model_type = "regression",
                       # Default for num.tree, sample.fraction, min.node.size are
                       # made same as the Analytics View (calc_feature_imp).
                       num.tree = 20,
                       sample.fraction = 0.5,
                       min.node.size = 12,
                       ...) {
  target_col <- all.vars(formula)[[1]]
  original_val <- data[[target_col]]
  original_colnames <- colnames(data)

  target_col_index <- which(colnames(data) == target_col)

  # randomForest must take clean names
  data <- janitor::clean_names(data)
  updated_colnames <- colnames(data)
  # this will be used to get original column names later
  names(updated_colnames) <- original_colnames
  # get target col as clean name
  target_col <- colnames(data)[target_col_index]

  if(model_type %in% c("classification_binary", "classification_multi")){
    if (is.logical(data[[target_col]])) {
      # Convert logical to factor to make it work with ranger.
      # For ranger, we consider the first level to be TRUE. So set levels that way.
      # Keep this logic consistent with predict_value_from_prob and augment.ranger.classification
      data[[target_col]] <- factor(data[[target_col]], levels=c("TRUE","FALSE"))
    }
    else {
      data[[target_col]] <- as.factor(data[[target_col]])
    }
  }

  if(!is.logical(original_val) &&
     length(levels(data[[target_col]] )) != 2 && model_type == "classification_binary"){
    stop("There should be 2 unique values for binary classification.")
  }

  if("." %in% all.vars(lazyeval::f_rhs(formula))){
    original_target_col <- all.vars(formula)[1]
    vars <- original_colnames[original_colnames != original_target_col]
    newvars <- updated_colnames[vars]
  } else {
    vars <- all.vars(formula)[-1]
    newvars <- updated_colnames[vars]
  }
  formula <- as.formula(paste0(target_col, " ~ ", paste0(newvars, collapse = " + ")))

  # ranger::ranger can't build model when there are NA values in data
  # The ranger::ranger will generate an error if it contains NA for both explanatory variables and target variables.
  # Therefore, it is necessary to exclude rows that have NA values in any of those columns.
  # Since it is necessary to save the information of the column excluded by NA and use it later,
  # the true/false value of which row has NA value once is judged.
  names(newvars) <- NULL
  na_row_numbers <- ranger.find_na(c(target_col, newvars), data)

  # remove NA rows because ranger can't treat NA values.
  data <- data %>% dplyr::select(target_col, newvars) %>% filter_all(all_vars(!is.na(.)))

  ret <- tryCatch({
    ranger::ranger(formula = formula,
                   data = data,
                   probability = stringr::str_detect(model_type, "classification"),
                   keep.inbag = TRUE,
                   importance = importance_mode,
                   num.tree = num.tree,
                   sample.fraction = sample.fraction,
                   min.node.size = min.node.size,
                   ...)
  }, error = function(e){
    if (e$message == "NA/NaN/Inf in foreign function call (arg 1)"){
      stop("Categorical and numerical predictors can't be used at the same time.")
    }
    stop(e)
  })

  # prediction result in the ranger model (ret$predictions) is for some reason different from and worse than
  # the prediction separately done with the same training data.
  # Make prediction with training data here and keep it, so that we can use this separate prediction for prediction, evaluation, etc.
  ret$prediction_training <- predict(ret, data)
  # this attribute will be used to get back original column names
  terms_mapping <- original_colnames
  names(terms_mapping) <- updated_colnames
  ret$terms_mapping <- terms_mapping

  if(model_type == "classification_binary"){
    ret$classification_type = "binary"
  } else if (model_type == "classification_multi") {
    ret$classification_type = "multi"
  }

  # ranger::ranger has no "na.action" attributes
  ret[["na.action"]] <- na_row_numbers

  # use this attributes at augment.ranger. ranger object already have an attribute named temrs, which has just only column names
  ret$formula_terms <- terms(formula)

  # To avoid saving a huge environment when caching with RDS.
  attr(ret$formula_terms,".Environment") <- NULL

  # store actual values of target column
  ret$y <- data %>% dplyr::pull(target_col)

  ret
}

#' Random Forest wrapper for regression by ranger packages
#' ranger::ranger don't compute importance by default. So importance_mode args is needed.
#' @export
rangerReg <- function(data, formula, na.action = na.omit, importance_mode = "permutation", ...) {
  rangerCore(data, formula, na.action = na.omit,
             importance_mode = importance_mode,
             model_type = "regression", ...)
}

#' Random Forest wrapper for classification by ranger package
#' @export
rangerBinary <- function(data, formula, na.action = na.omit, importance_mode = "permutation", ...) {
  rangerCore(data, formula, na.action = na.omit,
             importance_mode = importance_mode,
             model_type = "classification_binary", ...)
}

#' Random Forest wrapper for classification by ranger package
#' This is needed because boolean target starts regression task,
#' not classification task
#' @export
rangerMulti <- function(data, formula, na.action = na.omit, importance_mode = "permutation", ...) {
  rangerCore(data, formula, na.action = na.omit,
             importance_mode = importance_mode,
             model_type = "classification_multi", ...)
}

# these are from https://github.com/mdlincoln/broom/blob/e3cdf5f3363ab9514e5b61a56c6277cb0d9899fd/R/rf_tidiers.R
#' tidy for randomForest model
#' @export
tidy.randomForest <- function(x, ...) {
  # switch methods based on type attribute of the model
  tidy.randomForest.method <- switch(x[["type"]],
                                     "classification" = tidy.randomForest.classification,
                                     "regression" = tidy.randomForest.regression,
                                     "unsupervised" = tidy.randomForest.unsupervised)
  tidy.randomForest.method(x, ...)
}

#' tidy for randomForest model
#' @export
tidy.randomForest.formula <- tidy.randomForest

#' tidy for randomForest model
#' @param type "importance", "evaluation" or "conf_mat". Feature importance, evaluated scores or confusion matrix of training data.
#' @export
tidy.randomForest.classification <- function(x, pretty.name = FALSE, type = "importance", ...) {
  if (type == "importance") {
    imp_m <- as.data.frame(x[["importance"]]) %>%
      tibble::rownames_to_column()

    if(!is.null(x$terms_mapping)) {
      # these terms names might be cleaned by janitor::clean_names
      # so reverse them
      # as.character is to be safe by converting from factor. With factor, reverse mapping result will be messed up.
      imp_m[[1]] <- x$terms_mapping[as.character(imp_m[[1]])]
    }

    if (ncol(imp_m) > 3){
      names(imp_m) <- if(pretty.name){

        c("Term",
          # first column names and last 2 column names are excluded
          paste( "Class", tail(head(names(imp_m), -2), -1), sep = " "),
          "Mean Decrease Accuracy",
          "Mean Decrease Gini")
      } else {
        # first column names and last 2 column names are excluded
        c("term",
          # first column names and last 2 column names are excluded
          paste("class", tail(head(names(imp_m), -2), -1), sep = "_"),
          "mean_decrease_accuracy",
          "mean_decrease_gini")
      }
    } else {
      names(imp_m) <- if(pretty.name){
        c("Term", "Mean Decrease Gini")
      } else {
        c("term", "mean_decrease_gini")
      }
    }

    # When run with importance = FALSE, randomForest() does not calculate
    # importanceSD. Issue a warning.
    if (is.null(x[["importanceSD"]])) {
      warning("Only MeanDecreaseGini is available from this model. Run randomforest(..., importance = TRUE) for more detailed results")
      imp_m
    } else {
      imp_sd <- as.data.frame(x[["importanceSD"]])
      names(imp_sd) <- if(pretty.name){
        ret <- paste("Sd", names(imp_sd), sep = " ")
        ret[ret == "Sd MeanDecreaseAccuracy"] <- "Sd Mean Decrease Accuracy"
        ret
      } else {
        ret <- paste("sd", names(imp_sd), sep = "_")
        ret[ret == "sd_MeanDecreaseAccuracy"] <- "sd_mean_decrease_accuracy"
        ret
      }

      dplyr::bind_cols(imp_m, imp_sd)
    }
  } else if (type == "evaluation") {
    # get evaluation scores from training data
    actual <- x[["y"]]
    predicted <- x[["predicted"]]

    per_level <- function(level) {
      ret <- evaluate_classification(actual, predicted, level, pretty.name = pretty.name)
      ret
    }

    if(x$classification_type == "binary") {
      ret <- per_level(levels(actual)[2])
      # remove class column
      ret <- ret[, 2:6]
      ret
    } else {
      dplyr::bind_rows(lapply(levels(actual), per_level))
    }
  } else if (type == "conf_mat") {
    ret <- data.frame(
      actual_value = x$y,
      predicted_value = x$predicted
    ) %>%
      dplyr::filter(!is.na(predicted_value)) %>%
      dplyr::group_by(actual_value, predicted_value) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::ungroup()

    ret
  }
}

#' tidy for randomForest model
#' @export
tidy.randomForest.regression <- function(x, pretty.name = FALSE, ...) {
  imp_m <- as.data.frame(x[["importance"]]) %>%
    tibble::rownames_to_column()

  if(!is.null(x$terms_mapping)) {
    # these terms names might be cleaned by janitor::clean_names
    # so reverse them
    # as.character is to be safe by converting from factor. With factor, reverse mapping result will be messed up.
    imp_m[[1]] <- x$terms_mapping[as.character(imp_m[[1]])]
  }

  colnames(imp_m) <- if(pretty.name) {
    if (ncol(imp_m) == 3){
      c("Term", "Mean Decrease in Accuracy", "Mean Decrease in Mean Square Error")
    } else {
      # this is for localImp = FALSE in randomForest argument
      c("Term", "Mean Decrease in Mean Square Error")
    }
  } else {
    if (ncol(imp_m) == 3){
      c("term", "mean_decrease_in_accuracy", "mean_decrease_in_mean_square_error")
    } else {
      # this is for localImp = FALSE in randomForest argument
      c("term", "mean_decrease_in_mean_square_error")
    }
  }
  imp_sd <- x[["importanceSD"]]

  if (is.null(imp_sd)) {
    warning("Only IncNodePurity is available from this model. Run randomforest(..., importance = TRUE) for more detailed results")
  }
  else {
    if(pretty.name) {
      imp_m[["Importance Standard Error"]] <- imp_sd
    } else {
      imp_m[["importance_standard_error"]] <- imp_sd
    }
  }
  imp_m
}

#' This is unsupervised case. Yet to implement in client.
#' @export
tidy.randomForest.unsupervised <- function(x, ...) {
  imp_m <- as.data.frame(x[["importance"]])
  imp_m <- broom::fix_data_frame(imp_m)
  names(imp_m) <- rename_groups(names(imp_m))
  imp_sd <- x[["importanceSD"]]

  if (is.null(imp_sd)) {
    warning("Only MeanDecreaseGini is available from this model. Run randomforest(..., importance = TRUE) for more detailed results")
  } else {
    imp_sd <- as.data.frame(imp_sd)
    names(imp_sd) <- paste("sd", names(imp_sd), sep = "_")
  }

  dplyr::bind_cols(imp_m, imp_sd)
}


#' @export
glance.randomForest <- function(x, ...) {

  glance.method <- switch(x[["type"]],
                          "classification" = glance.randomForest.classification,
                          "regression" = glance.randomForest.regression,
                          "unsupervised" = glance.randomForest.unsupervised)

  glance.method(x, ...)
}

#' glance for randomForest model
#' @export
glance.randomForest.formula <- glance.randomForest

#' glance for randomForest model
#' @export
glance.randomForest.classification <- function(x, pretty.name = FALSE,  ...) {
  actual <- x[["y"]]
  predicted <- x[["predicted"]]

  per_level <- function(level) {
    # calculate evaluation scores for each level
    tp <- sum(actual == level & predicted == level)
    tn <- sum(actual != level & predicted != level)
    fp <- sum(actual != level & predicted == level)
    fn <- sum(actual == level & predicted != level)

    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    accuracy <- (tp + tn) / (tp + tn + fp + fn)
    f_score <- 2 * ((precision * recall) / (precision + recall))

    ret <- data.frame(
      f_score,
      accuracy,
      1- accuracy,
      precision,
      recall
    )

    names(ret) <- if(pretty.name){
      paste(level, c("F Score", "Accuracy", "Misclassification Rate", "Precision", "Recall"), sep = " ")
    } else {
      paste(level, c("f_score", "accuracy", "misclassification_rate", "precision", "recall"), sep = "_")
    }
    ret
  }

  dplyr::bind_cols(lapply(levels(actual), per_level))
}

#' glance for randomForest model
#' @export
glance.randomForest.regression <- function(x, pretty.name = FALSE, ...) {
  # these are mean values of each tree
  mean_mse <- mean(x[["mse"]])
  mean_rsq <- mean(x[["rsq"]])

  ret <- data.frame(mean_mse, mean_rsq)

  if(pretty.name){
    colnames(ret) <- c("Mean of Mean Square Error", "Mean of R Square")
  } else {
    colnames(ret) <- c("mean_of_mean_square_error", "mean_of_r_square")
  }
  ret
}

#' glance for randomForest model
#' @export
glance.randomForest.unsupervised <- function(x, ...) {
  data.frame(Message = "No information for unsupervised randomForest models")
}

#' augment for randomForest model
#' @export
augment.randomForest <- function(x, data = NULL, ...) {

  # Extract data from model
  # This is from https://github.com/mdlincoln/broom/blob/e3cdf5f3363ab9514e5b61a56c6277cb0d9899fd/R/rf_tidiers.R
  if (is.null(data)) {
    if (is.null(x$call$data)) {
      list <- lapply(all.vars(x$call), as.name)
      data <- eval(as.call(list(quote(data.frame),list)), parent.frame())
    } else {
      data <- eval(x$call$data, parent.frame())
    }
  }

  augment.randomForest.method <- switch(x[["type"]],
                                        "classification" = augment.randomForest.classification,
                                        "regression" = augment.randomForest.regression,
                                        "unsupervised" = augment.randomForest.unsupervised)
  augment.randomForest.method(x, data, ...)
}

#' augment for randomForest model
#' @export
augment.randomForest.formula <- augment.randomForest

#' augment for randomForest model
#' @export
augment.randomForest.classification <- function(x, data = NULL, newdata = NULL, ...) {
  y_name <- all.vars(x$terms)[[1]]

  if(!is.null(newdata)){
    # janitor::clean_names is called in randomForestClassify,
    # so it should be called here too
    cleaned_data <- janitor::clean_names(newdata)

    predicted_value_col <- avoid_conflict(colnames(newdata), "predicted_value")

    if(is.null(x$classification_type)){
      # just append predicted labels
      predicted_val <- predict(x, cleaned_data)
      newdata[[predicted_value_col]] <- predicted_val
      newdata
    } else if (x$classification_type == "binary") {
      # append predicted probability of positive
      predicted_val <- predict(x, cleaned_data, type = "prob")[, 2]
      newdata[[predicted_value_col]] <- predicted_val
      newdata
    } else if (x$classification_type == "multi") {
      # append predicted probability for each class, max and labels at max values
      probs <- predict(x, cleaned_data, type = "prob")
      ret <- get_multi_predicted_values(probs, newdata[[y_name]])
      newdata <- dplyr::bind_cols(newdata, ret)
    }
  } else if (!is.null(data)) {
    # create clean name data frame because the model learned by those names
    cleaned_data <- janitor::clean_names(data)

    # When na.omit is used, case-wise model attributes will only be calculated
    # for complete cases in the original data. All columns returned with
    # augment() must be expanded to the length of the full data, inserting NA
    # for all missing values.
    oob_col <- avoid_conflict(colnames(data), "out_of_bag_times")

    # These are from https://github.com/mdlincoln/broom/blob/e3cdf5f3363ab9514e5b61a56c6277cb0d9899fd/R/rf_tidiers.R
    # create index of eliminated rows (na_at) by na.action from model.
    # since prediction output may have fewer rows than original data because of na.action,
    # we cannot augment the data just by binding columns.
    n_data <- nrow(data)
    if (is.null(x[["na.action"]])) {
      na_at <- rep(FALSE, times = n_data)
    } else {
      na_at <- seq_len(n_data) %in% as.integer(x[["na.action"]])
    }

    oob_times <- rep(NA_integer_, times = n_data)
    oob_times[!na_at] <- x[["oob.times"]]

    predicted <- rep(NA, times = n_data)
    predicted[!na_at] <- x[["predicted"]]
    predicted <- to_same_type(levels(x[["y"]])[predicted], cleaned_data[[y_name]])

    votes <- x[["votes"]]
    full_votes <- matrix(data = NA, nrow = n_data, ncol = ncol(votes))
    full_votes[which(!na_at),] <- votes
    colnames(full_votes) <- colnames(votes)
    full_votes <- as.data.frame(full_votes)
    names(full_votes) <- avoid_conflict(colnames(data), paste("votes", names(full_votes), sep = "_"))

    local_imp <- x[["localImportance"]]
    full_imp <- NULL

    if (!is.null(local_imp)) {
      full_imp <- matrix(data = NA_real_, nrow = nrow(local_imp), ncol = n_data)
      full_imp[, which(!na_at)] <- local_imp
      rownames(full_imp) <- rownames(local_imp)
      full_imp <- as.data.frame(t(full_imp))
      names(full_imp) <- avoid_conflict(colnames(data),
                                        paste("local_inportance", names(full_imp), sep = "_"))
    } else {
      warning("casewise importance measures are not available. Run randomForest(..., localImp = TRUE) for more detailed results.")
    }

    data <- dplyr::bind_cols(data, full_votes, full_imp)
    data[[oob_col]] <- oob_times

    # this appending part is modified
    predicted_prob <- predict(x, type = "prob")
    if(is.null(x$classification_type)){
      # just append predicted label
      data[[predicted_value_col]] <- predicted
    } else if(!is.null(x$classification_type) &&
       x$classification_type == "binary"){
      # append predicted probability
      predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
      predicted_prob <- predicted_prob[, 2]
      data[[predicted_value_col]][!na_at] <- predicted_prob
    } else if (x$classification_type == "multi"){
      # append predicted probability for each class, max and labels at max values
      ret <- get_multi_predicted_values(predicted_prob, cleaned_data[[y_name]])
      for (i in 1:length(ret)) { # for each column
        # this is basically bind_cols with na_at taken into account.
        data[[colnames(ret)[[i]]]][!na_at] <- ret[[i]]
      }
    }

    data
  } else {
    stop("data or newdata have to be indicated.")
  }
}

#' augment for randomForest model
#' @export
augment.randomForest.regression <- function(x, data = NULL, newdata = NULL, ...) {
  predicted_value_col <- avoid_conflict(colnames(newdata), "predicted_value")

  if(!is.null(newdata)) {
    # create clean name data frame because the model learned by those names
    cleaned_data <- janitor::clean_names(newdata)

    predicted_val <- predict(x, cleaned_data)
    newdata[[predicted_value_col]] <- predicted_val

    newdata
  } else if (!is.null(data)) {
    predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
    oob_col <- avoid_conflict(colnames(data), "out_of_bag_times")

    # These are from https://github.com/mdlincoln/broom/blob/e3cdf5f3363ab9514e5b61a56c6277cb0d9899fd/R/rf_tidiers.R
    # TODO: This seems to be taking care of NA cases. Should review this part later.
    n_data <- nrow(data)
    na_at <- seq_len(n_data) %in% as.integer(x[["na.action"]])

    oob_times <- rep(NA_integer_, times = n_data)
    oob_times[!na_at] <- x[["oob.times"]]

    predicted <- rep(NA_real_, times = n_data)
    predicted[!na_at] <- x[["predicted"]]

    local_imp <- x[["localImportance"]]
    full_imp <- NULL

    if (!is.null(local_imp)) {
      full_imp <- matrix(data = NA_real_, nrow = nrow(local_imp), ncol = n_data)
      full_imp[, which(!na_at)] <- local_imp
      rownames(full_imp) <- rownames(local_imp)
      full_imp <- as.data.frame(t(full_imp))
      names(full_imp) <- avoid_conflict(colnames(data),paste("local_importance", names(full_imp), sep = "_"))
    } else {
      warning("casewise importance measures are not available. Run randomForest(..., localImp = TRUE) for more detailed results.")
    }

    data <- dplyr::bind_cols(data, full_imp)
    data[[oob_col]] <- oob_times
    data[[predicted_value_col]] <- predicted
    data
  }
}

#' augment for randomForest model
#' @export
augment.randomForest.unsupervised <- function(x, data, ...) {

  # When na.omit is used, case-wise model attributes will only be calculated
  # for complete cases in the original data. All columns returned with
  # augment() must be expanded to the length of the full data, inserting NA
  # for all missing values.

  # These are from https://github.com/mdlincoln/broom/blob/e3cdf5f3363ab9514e5b61a56c6277cb0d9899fd/R/rf_tidiers.R
  # TODO: This seems to be taking care of NA cases. Should review this part later.
  n_data <- nrow(data)
  if (is.null(x[["na.action"]])) {
    na_at <- rep(FALSE, times = n_data)
  } else {
    na_at <- seq_len(n_data) %in% as.integer(x[["na.action"]])
  }

  oob_times <- rep(NA_integer_, times = n_data)
  oob_times[!na_at] <- x[["oob.times"]]


  votes <- x[["votes"]]
  full_votes <- matrix(data = NA, nrow = n_data, ncol = ncol(votes))
  full_votes[which(!na_at),] <- votes
  colnames(full_votes) <- colnames(votes)
  full_votes <- as.data.frame(full_votes)
  names(full_votes) <- paste("votes", names(full_votes), sep = "_")

  predicted <- ifelse(full_votes[[1]] > full_votes[[2]], "1", "2")

  d <- data.frame(oob_times = oob_times, fitted = predicted)
  d <- dplyr::bind_cols(d, full_votes)
  names(d) <- paste0(".", names(d))
  dplyr::bind_cols(data, d)
}

#' augment for randomForest model
#' @export
augment.randomForest <- augment.randomForest.formula

#' augment for randomForest(ranger) model
#' @export
augment.ranger <- function(x, data = NULL, newdata = NULL, ...) {
  loadNamespace("ranger") # This is necessary for predict() to successfully figure out which function to call internally.
  if ("error" %in% class(x)) {
    ret <- data.frame()
    return(ret)
  }
  augment.ranger.method <- switch(x$treetype,
                                  "Classification" = augment.ranger.classification,
                                  "Probability estimation" = augment.ranger.classification,
                                  "Regression" = augment.ranger.regression)
  augment.ranger.method(x, data, newdata, ...)
}

align_predictor_factor_levels <- function(newdata, model_df, predictor_cols) {
  cleaned_data <- newdata
  # Align factor levels including Others and (Missing) to the model.
  for (i in 1:length(predictor_cols)) {
    predictor_col <- predictor_cols[i]
    training_predictor <- model_df[[predictor_col]]
    if (is.factor(training_predictor) || is.character(training_predictor)) {
      if (is.factor(training_predictor)) {
        training_predictor_levels <- levels(training_predictor)
      }
      else if (is.character(training_predictor)) {
        training_predictor_levels <- unique(training_predictor)
      }
      # ordered factor here causes error in xgboost. Make it not ordered.
      ret <- forcats::fct_explicit_na(forcats::fct_other(factor(cleaned_data[[predictor_col]], ordered=FALSE), keep=training_predictor_levels))
      # In case model does not know (Missing) level, do fct_other again. (Missing) will be absorbed in Other.
      ret <- forcats::fct_other(ret, keep=training_predictor_levels)
      # If "Other" is not included in the model levels, replace them with NA. They will be handled as NA rows.
      if ("Other" %nin% training_predictor_levels) {
        ret <- dplyr::na_if(ret, "Other")
      }
      if (is.factor(training_predictor)) { # Set the same levels as the training data including the level order.
        ret <- factor(ret, levels=training_predictor_levels)
      }
      else { # Cast it back to character, just like the training data.
        ret <- as.character(ret)
      }
      cleaned_data[[predictor_col]] <- ret
    }
  }
  cleaned_data
}

#' augment for randomForest model
#' @export
augment.ranger.classification <- function(x, data = NULL, newdata = NULL, data_type = "training", binary_classification_threshold = 0.5, ...) {
  # Get name of original target columns by reverse-mapping the name in formula.
  y_name <- x$terms_mapping[all.vars(x$formula_terms)[[1]]]

  # Get names of original predictor columns by reverse-mapping the names in formula.
  predictor_variables <- all.vars(x$formula_terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]

  threshold <- NULL
  if (x$classification_type == "binary") {
    threshold <- binary_classification_threshold
  }

  if(!is.null(newdata)){
    # Replay the mutations on predictors.
    if(!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    y_value <- newdata[[y_name]] # TODO: Does this make sense for newdata case, where target variable values are generally unknown??

    # create clean name data frame because the model learned by those names
    # This select() also renames columns since predictor_variables_orig is a named vector.
    cleaned_data <- newdata %>% dplyr::select(predictor_variables_orig)

    # Align factor levels including Others and (Missing) to the model. TODO: factor level order can be different from the model training data. Is this ok?
    cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)

    na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
    if (length(na_row_numbers) > 0) {
      cleaned_data <- cleaned_data[-na_row_numbers,]
    }

    # Run prediction.
    pred_res <- predict(x, cleaned_data)

    predicted_label_nona <- predict_value_from_prob(x$forest$levels,
                                                           pred_res$predictions,
                                                           y_value, threshold = threshold)
    # Inserting once removed NA rows
    predicted_value <- restore_na(predicted_label_nona, na_row_numbers)

    predicted_label_col <- avoid_conflict(colnames(newdata), "predicted_label")
    predicted_probability_col <- avoid_conflict(colnames(newdata), "predicted_probability")

    if(is.null(x$classification_type)){
      # just append predicted labels
      newdata[[predicted_label_col]] <- predicted_value
    } else if (x$classification_type == "binary") {
      newdata[[predicted_label_col]] <- predicted_value
      predictions <- pred_res$predictions

      # With ranger, 1st category always is the one to be considered "TRUE",
      # and the probability for it is the probability for the binary classification.
      # (For logistic regression, it is different, but here for ranger for now, for simplicity, we choose this behavior.)
      # Keep this logic consistent with predict_value_from_prob
      predicted_prob <- pred_res$predictions[, 1]
      # Inserting once removed NA rows
      predicted_prob <- restore_na(predicted_prob, na_row_numbers)
      newdata[[predicted_probability_col]] <- predicted_prob
    } else if (x$classification_type == "multi") {
      # append predicted probability for each class, max and labels at max values
      # Inserting once removed NA rows
      predicted_prob <- restore_na(apply(pred_res$predictions, 1 , max), na_row_numbers)
      newdata <- ranger.set_multi_predicted_values(newdata, pred_res$predictions, predicted_value, predicted_prob, na_row_numbers)
    }
    newdata
  } else if (!is.null(data)) {
    if (!is.null(x$orig_target_col)) { # This is only for Analytics View.
      data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col()) # Bring the target column to the last so that it is next to the predicted value in the output.
    }
    if (nrow(data) == 0) {
      # Handle the case where, for example, test_rate is 0 here,
      # rather than trying to make it pass through following code, which can be complex.
      return (data)
    }
    # create clean name data frame because the model learned by those names
    cleaned_data <- data
    y_value <- cleaned_data[[y_name]]
    predicted_label_col <- avoid_conflict(colnames(data), "predicted_label")
    predicted_probability_col <- avoid_conflict(colnames(data), "predicted_probability")

    switch(data_type,
      training = {
        predicted_label_nona <- predict_value_from_prob(x$forest$levels,
                                                        x$prediction_training$predictions,
                                                        y_value, threshold = threshold)
        predicted_value <- restore_na(predicted_label_nona, x$na.action)
      },
      test = {
        predicted_label_nona <- predict_value_from_prob(x$forest$levels,
                                                        x$prediction_test$predictions,
                                                        y_value, threshold = threshold)
        # Restore NAs for removed rows that had unknown categorical predictor values.
        # Note that this is necessary only for test data, and not for training data.
        predicted_label_nona <- restore_na(predicted_label_nona, attr(x$prediction_test, "unknown_category_rows_index"))
        predicted_value <- restore_na(predicted_label_nona, attr(x$prediction_test, "na.action"))
      })

    if(!is.null(x$classification_type) && x$classification_type == "binary"){
      switch(data_type,
        training = {
          # append predicted probability
          predictions <- x$prediction_training$predictions
          # With ranger, 1st category always is the one to be considered "TRUE",
          # and the probability for it is the probability for the binary classification.
          # Keep this logic consistent with predict_value_from_prob
          predicted_prob <- restore_na(predictions[, 1], x$na.action)
        },
        test = {
          predictions <- x$prediction_test$predictions
          # With ranger, 1st category always is the one to be considered "TRUE",
          # and the probability for it is the probability for the binary classification.
          # Keep this logic consistent with predict_value_from_prob
          predicted_prob_nona <- predictions[, 1]
          predicted_prob_nona <- restore_na(predicted_prob_nona, attr(x$prediction_test, "unknown_category_rows_index"))
          predicted_prob <- restore_na(predicted_prob_nona, attr(x$prediction_test, "na.action"))
        })
      data[[predicted_label_col]] <- predicted_value
      data[[predicted_probability_col]] <- predicted_prob
      data
    } else if (x$classification_type == "multi"){
      switch(data_type,
        training = {
          # Inserting once removed NA rows
          predicted_prob <- restore_na(apply(x$prediction_training$predictions, 1 , max), x$na.action)
          data <- ranger.set_multi_predicted_values(data, x$prediction_training$predictions, predicted_value, predicted_prob, x$na.action)
        },
        test = {
          # Inserting once removed NA rows
          predicted_prob_nona <- apply(x$prediction_test$predictions, 1 , max)
          predicted_prob_nona <- restore_na(predicted_prob_nona, attr(x$prediction_test, "unknown_category_rows_index"))
          predicted_prob <- restore_na(predicted_prob_nona, attr(x$prediction_test, "na.action"))
          data <- ranger.set_multi_predicted_values(data, x$prediction_test$predictions, predicted_value, predicted_prob, attr(x$prediction_test, "na.action"), attr(x$prediction_test, "unknown_category_rows_index"))
        })
      data
    }
  } else {
    stop("data or newdata have to be indicated.")
  }
}

#' @param data_type - "training" or "test", Which type of prediction result included inside the model to augment the data.
#' @export
augment.ranger.regression <- function(x, data = NULL, newdata = NULL, data_type = "training", ...){
  predicted_value_col <- avoid_conflict(colnames(newdata), "predicted_value")
  predictor_variables <- all.vars(x$formula_terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]

  if(!is.null(newdata)) {
    # Replay the mutations on predictors.
    if(!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    # This select() also renames columns since predictor_variables_orig is a named vector.
    cleaned_data <- newdata %>% dplyr::select(predictor_variables_orig)

    # Align factor levels including Others and (Missing) to the model. TODO: factor level order can be different from the model training data. Is this ok?
    cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)

    na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
    if (length(na_row_numbers) > 0) {
      cleaned_data <- cleaned_data[-na_row_numbers,]
    }

    # Run prediction.
    predicted_val <- predict(x, cleaned_data)$predictions

    # Inserting once removed NA rows
    newdata[[predicted_value_col]] <- restore_na(predicted_val, na_row_numbers)

    newdata
  } else if (!is.null(data)) {
    if (!is.null(x$orig_target_col)) { # This is only for Analytics View.
      data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col()) # Bring the target column to the last so that it is next to the predicted value in the output.
    }
    switch(data_type,
      training = {
        predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
        # Inserting once removed NA rows
        predicted <- restore_na(x$prediction_training$predictions, x$na.action)
        data[[predicted_value_col]] <- predicted
        data
      },
      test = {
        predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
        # Inserting once removed NA rows
        predicted_nona <- x$prediction_test$predictions
        predicted_nona <- restore_na(predicted_nona, attr(x$prediction_test, "unknown_category_rows_index"))
        predicted <- restore_na(predicted_nona, attr(x$prediction_test, "na.action"))
        data[[predicted_value_col]] <- predicted
        data
      })
  }
}

#' augment for rpart model
#' @export
augment.rpart <- function(x, data = NULL, newdata = NULL, ...) {
  loadNamespace("rpart") # This is necessary for predict() to successfully figure out which function to call internally.
  if ("error" %in% class(x)) {
    ret <- data.frame()
    return(ret)
  }
  # Extract data from model
  # This is from https://github.com/mdlincoln/broom/blob/e3cdf5f3363ab9514e5b61a56c6277cb0d9899fd/R/rf_tidiers.R
  if (is.null(data)) {
    if (is.null(x$call$data)) {
      list <- lapply(all.vars(x$call), as.name)
      data <- eval(as.call(list(quote(data.frame),list)), parent.frame())
    }
  }

  augment.rpart.method <- switch(x$classification_type,
                                  "binary" = augment.rpart.classification,
                                  "multi" = augment.rpart.classification,
                                  "regression" = augment.rpart.regression)
  augment.rpart.method(x, data, newdata, ...)
}

augment.rpart.classification <- function(x, data = NULL, newdata = NULL, data_type = "training", binary_classification_threshold = 0.5, ...) {
  # For rpart, terms_mapping is turned off in exp_rpart, so that we can display original column names in tree image.
  predictor_variables <- all.vars(x$terms)[-1]
  y_name <- x$terms_mapping[all.vars(x$terms)[[1]]]

  # Get names of original predictor columns by reverse-mapping the names in formula.
  predictor_variables <- all.vars(x$terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]

  threshold <- NULL
  if (x$classification_type == "binary") {
    threshold <- binary_classification_threshold
  }

  if (!is.null(newdata)) {
    # Replay the mutations on predictors.
    if(!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    # create clean name data frame because the model learned by those names
    cleaned_data <- newdata

    # This select() also renames columns since predictor_variables_orig is a named vector.
    # everything() is to keep the other columns in the output. #TODO: What if names of the other columns conflicts with our temporary name, c1_, c2_...?
    cleaned_data <- cleaned_data %>% dplyr::select(predictor_variables_orig, everything())

    # Align factor levels including Others and (Missing) to the model. TODO: factor level order can be different from the model training data. Is this ok?
    cleaned_data <- align_predictor_factor_levels(cleaned_data, attr(x, "xlevels"), predictor_variables)

    na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
    if (length(na_row_numbers) > 0) {
      cleaned_data <- cleaned_data[-na_row_numbers,]
    }

    # Run prediction.
    pred_res <- predict(x, cleaned_data)

    predicted_label_nona <- predict_value_from_prob(attr(x, "ylevels"),
                                                    pred_res,
                                                    NULL, # y_value
                                                    threshold = threshold)

    # Inserting once removed NA rows
    predicted_value <- restore_na(predicted_label_nona, na_row_numbers)

    predicted_label_col <- avoid_conflict(colnames(newdata), "predicted_label")
    predicted_probability_col <- avoid_conflict(colnames(newdata), "predicted_probability")

    if (x$classification_type == "binary") {
      newdata[[predicted_label_col]] <- predicted_value

      # Since now we consider only logical column as the tarted for binary classification, the label for "TRUE" class should be always "TRUE".
      predicted_prob <- pred_res[, "TRUE"]
      # Inserting once removed NA rows
      predicted_prob <- restore_na(predicted_prob, na_row_numbers)
      newdata[[predicted_probability_col]] <- predicted_prob
      newdata
    } else if (x$classification_type == "multi") {
      # append predicted probability for each class, max and labels at max values
      # Inserting once removed NA rows
      predicted_prob <- restore_na(apply(pred_res, 1 , max), na_row_numbers)
      newdata <- ranger.set_multi_predicted_values(newdata, pred_res, predicted_value, predicted_prob, na_row_numbers)
      newdata
    }
    newdata
  } else if (!is.null(data)) {
    data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col()) # Bring the target column to the last so that it is next to the predicted value in the output.
    if (nrow(data) == 0) {
      # Handle the case where, for example, test_rate is 0 here,
      # rather than trying to make it pass through following code, which can be complex.
      return (data)
    }
    y_value <- attributes(x)$ylevels[x$y]
    predicted_label_col <- avoid_conflict(colnames(data), "predicted_label")
    predicted_probability_col <- avoid_conflict(colnames(data), "predicted_probability")
    switch(data_type,
      training = {
        predicted_value_nona <- x$predicted_class
        predicted_value <- restore_na(predicted_value_nona, x$na.action)
        # binary case and multiclass case are both handled inside this func.
        predicted_probability_nona <- get_predicted_probability_rpart(x)
        predicted_probability <- restore_na(predicted_probability_nona, x$na.action)
      },
      test = {
        predicted_value_nona <- x$predicted_class_test
        predicted_value_nona <- restore_na(predicted_value_nona, x$unknown_category_rows_index_test)
        predicted_value <- restore_na(predicted_value_nona, x$na_row_numbers_test)
        # binary case and multiclass case are both handled inside this func.
        predicted_probability_nona <- get_predicted_probability_rpart(x, data_type = "test")
        predicted_probability_nona <- restore_na(predicted_probability_nona, x$unknown_category_rows_index_test)
        predicted_probability <- restore_na(predicted_probability_nona, x$na_row_numbers_test)
      })

    data[[predicted_label_col]] <- predicted_value
    data[[predicted_probability_col]] <- predicted_probability
    data

  } else {
    stop("data or newdata have to be indicated.")
  }
}

augment.rpart.regression <- function(x, data = NULL, newdata = NULL, data_type = "training", ...) {
  # For rpart, terms_mapping is turned off in exp_rpart, so that we can display original column names in tree image.
  predicted_value_col <- avoid_conflict(colnames(newdata), "predicted_value")
  predictor_variables <- all.vars(x$formula_terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]

  if(!is.null(newdata)) {
    # Replay the mutations on predictors.
    if(!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    # create clean name data frame because the model learned by those names
    # This select() also renames columns since predictor_variables_orig is a named vector.
    cleaned_data <- newdata %>% dplyr::select(predictor_variables_orig)

    # Align factor levels including Others and (Missing) to the model. TODO: factor level order can be different from the model training data. Is this ok?
    cleaned_data <- align_predictor_factor_levels(cleaned_data, attr(x, "xlevels"), predictor_variables)

    na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
    if (length(na_row_numbers) > 0) {
      cleaned_data <- cleaned_data[-na_row_numbers,]
    }

    # Run prediction.
    predicted_val <- predict(x, cleaned_data)

    # Inserting once removed NA rows
    newdata[[predicted_value_col]] <- restore_na(predicted_val, na_row_numbers)

    newdata
  } else if (!is.null(data)) {
    data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col()) # Bring the target column to the last so that it is next to the predicted value in the output.
    switch(data_type,
      training = {
        predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
        predicted <- restore_na(predict(x), x$na.action)
        data[[predicted_value_col]] <- predicted
        data
      },
      test = {
        predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
        # Inserting once removed NA rows
        predicted_nona <- x$prediction_test
        predicted_nona <- restore_na(predicted_nona, x$unknown_category_rows_index_test)
        predicted <- restore_na(predicted_nona, x$na_row_numbers_test)
        data[[predicted_value_col]] <- predicted
        data
      })

  }
}

#' In multiclass classification, add prediction probability to each class as a column to data
#' @param data - data to predict multiclass
#' @param x - Model object that is the return value of ranger::ranger
#' @param na_row_numbers - Numeric vector of which row of data has NA
#' @param pred_prob_col - Column name suffix of predicted probability column for each class name
#' @param pred_value_col - Column name for storing prediction class of multiclass classification
ranger.set_multi_predicted_values <- function(data,
                                              predictions,
                                              predicted_value,
                                              predicted_prob,
                                              na_row_numbers,
                                              unknown_category_row_numbers=NULL,
                                              pred_prob_col="predicted_probability",
                                              pred_value_col="predicted_label") {
  data[[pred_value_col]] <- predicted_value
  data[[pred_prob_col]] <- predicted_prob
  ret <- predictions
  for (i in 1:length(colnames(ret))) { # for each column
    # this is basically bind_cols with na_at taken into account.
    colname <- stringr::str_c(pred_prob_col, colnames(ret)[i], sep="_")

    # Inserting once removed NA rows
    prob_data_bycol_nona <- ret[, i] # Do not add drop=FALSE since we want vector here.
    if (!is.null(unknown_category_row_numbers)) {
      prob_data_bycol_nona <- restore_na(prob_data_bycol_nona, unknown_category_row_numbers)
    }
    prob_data_bycol <- restore_na(prob_data_bycol_nona, na_row_numbers)
    data[[colname]] <- prob_data_bycol
  }
  data
}

#' returns the number of the row containing the NA value of data as a numeric vector
#' @param variables - column name to use for prediction (determine if any of this column contains NA)
#' @param data - data to predict
#' @param na_index - Boolean vectors whether or not NA is included (Default: NULL)
ranger.find_na <- function(variables, data, na_index = NULL){
  na_atrow_index <- if (is.null(na_index)) {
    ranger.find_na_index(variables, data)
  } else {
    na_index
  }
  na_row_numbers <- seq_len(nrow(data))[na_atrow_index]

  return(na_row_numbers)
}

#' Returns TRUE / FALSE vectors whether each row contains the NA value in any of the column values specified in variables
#' @param variables - column name to use for prediction (determine if any of this column contains NA)
#' @param data - data to predict
ranger.find_na_index <- function(variables, data) {
  data <- data %>% dplyr::select(variables)
  ret <- purrr::reduce(data, function(x,y){x|is.infinite(y)|is.na(y)},.init=rep(FALSE,nrow(data)))
  ret
}

#' Return the highest probability label from the matrix of predicted probabilities
#' @param levels_var - Factor level of label to predict
#' @param pred - Matrix of prediction probabilities
#' @param y_value - Actual value to be predicted
predict_value_from_prob <- function(levels_var, pred, y_value, threshold = NULL) {
  # We assume threshold is given only for binary case.
  if (is.null(threshold)) { # multiclass case. Return the value with maximum probability.
    to_same_type(levels_var[apply(pred, 1, which.max)], y_value)
  }
  else { # binary case
    # pred (x$predictions) is 2-diminsional matrix with 2 columns for the 2 categories.
    # Values in the matrix is the probabilities.
    true_index <- match("TRUE",colnames(pred)) # Find which index is TRUE
    if (is.na(true_index)) {
      false_index <- match("FALSE",colnames(pred)) # Find which index is FALSE 
      if (is.na(false_index)) {
        # For old analytics step that can take non-logical column for binary classification. Treat the first value as TRUE.
        true_index <- 1
      }
      else {
        # take care of the case where x$predictions has only 1 column and it is FALSE. possible when there are only one value in training data.
        true_index <- 0
      }
    }
    predicted <- apply(pred, 1, function(x){
      if (true_index == 1) {
        x[1] > threshold
      }
      else if (true_index == 0) { # FALSE only case explained above.
        1 - x[1] > threshold
      }
      else { # true_index == 2
        x[2] > threshold
      }
    })
    predicted
  }
}

rename_groups <- function(n) {
  ifelse(grepl("^\\d", n), paste0("group_", n), n)
}

#' wrapper for tidy type importance
#' @export
rf_importance <- function(data, ...) {
  tidy_rowwise(data, model, type = "importance", ...)
}

#' wrapper for tidy type evaluation
#' @export
rf_evaluation <- function(data, ...) {
  ret <- tidy_rowwise(data, model, type = "evaluation", ...)
  if (!is.null(ret$Note)) {
    # Bring Note column to the last.
    # It is hard to control the position of Note column inside tidy, and hence we do it here.
    ret <- ret %>% dplyr::select(-Note, everything(), Note)
  }
  ret
}

#' wrapper for tidy type evaluation_by_class
#' @export
rf_evaluation_by_class <- function(data, ...) {
  tidy_rowwise(data, model, type = "evaluation_by_class", ...)
}

# Returns Analytics View model summary table for ranger, rpart, and xgboost.
# TODO: This function should be promoted to a generic model evaluation function.
# Generates Analytics View Summary table for ranger and rpart.
#' wrapper for tidy type evaluation
#' @export
rf_evaluation_training_and_test <- function(data, type = "evaluation", pretty.name = FALSE, binary_classification_threshold = 0.5, ...) {
  # Filter out the rows from failed models.
  # This is working depending on rowwise grouping. (Note for when we move out of it.)
  filtered <- data %>% dplyr::filter(!is.null(model) & !"error" %in% class(model))
  if (nrow(filtered) == 0) { # No valid models were returned.
    return(data.frame())
  }

  model <-  filtered$model[[1]]
  test_index <- filtered$.test_index[[1]]

  # Get evaluation for training part. Just passing down to rf_evaluation does it, since it is done off of data embeded in the model.
  # Here we use data with failed model too (data) as opposed to the one without them (filtered),
  # so that we can report the error on the Note column of Summary table.
  # Generally speaking, for training data part, we rely on model's glance/tidy function, expecting it to give model specific metrics,
  # though for conf_map, we might want to write one implementation here, to avoid having to write the same thing for each model. TODO: Do it.
  if (!is.null(model)) {
    training_ret <- switch(type,
                           evaluation = rf_evaluation(data, pretty.name = pretty.name, binary_classification_threshold = binary_classification_threshold, ...),
                           evaluation_by_class = rf_evaluation_by_class(data, pretty.name = pretty.name, binary_classification_threshold = binary_classification_threshold, ...),
                           conf_mat = data %>% tidy_rowwise(model, type = "conf_mat", binary_classification_threshold = binary_classification_threshold, ...))
    if (length(test_index) > 0 && nrow(training_ret) > 0) {
      training_ret$is_test_data <- FALSE
    }
  } else {
    training_ret <- data.frame()
  }

  ret <- training_ret

  grouped_col <- colnames(data)[!colnames(data) %in% c("model", ".test_index", "source.data")]

  # Execute evaluation if there is test data
  if (length(test_index) > 0) {

    each_func <- function(df) {
      # With the way this is called, df becomes list rather than data.frame.
      # Make it data.frame again so that prediction() can be applied on it.
      if (!is.data.frame(df)) {
        df <- tibble::tribble(~model, ~.test_index, ~source.data,
                              df$model, df$.test_index, df$source.data)
      }
      if (is.null(df$model[[1]])) { # model is NULL. Skip this group.
        return(data.frame())
      }

      # Extract test prediction result embedded in the model.
      test_pred_ret <- df %>% prediction(data = "test", ...)

      tryCatch({ #TODO: Not too sure why this tryCatch is needed. It could hide errors that should be properly reported.
        model_object <- df$model[[1]]
        if ("xgboost_exp" %in% class(model_object)) {
          actual <- extract_actual(model_object, type = "test")
        }
        else {
          actual_col <- model$terms_mapping[all.vars(model$formula_terms)[1]]
          actual <- test_pred_ret[[actual_col]]
        }

        test_ret <- switch(type,
          evaluation = {
            if (is.numeric(actual)) {
              if ("xgboost_exp" %in% class(model_object)) {
                predicted <- extract_predicted(model_object, type = "test")
              }
              else {
                predicted <- test_pred_ret$predicted_value
              }
              root_mean_square_error <- rmse(actual, predicted)
              test_n <- sum(!is.na(predicted)) # Sample size for test.

              # null_model_mean is mean of training data.
              if ("xgboost_exp" %in% class(model_object)) {
                null_model_mean <- mean(extract_actual(model_object, type = "trainig"), na.rm=TRUE)
              }
              else if ("rpart" %in% class(model_object)) { # rpart case
                null_model_mean <- mean(model_object$y, na.rm=TRUE)
              }
              else { # ranger case
                null_model_mean <- mean(model_object$df[[all.vars(model_object$formula_terms)[[1]]]], na.rm=TRUE)
              }

              rsq <- r_squared(actual, predicted, null_model_mean)
              ret <- data.frame(
                                root_mean_square_error = root_mean_square_error,
                                r_squared = rsq,
                                n = test_n
                                )

              if(pretty.name){
                map = list(
                           `RMSE` = as.symbol("root_mean_square_error"),
                           `R Squared` = as.symbol("r_squared"),
                           `Number of Rows` = as.symbol("n")
                           )
                ret <- ret %>% dplyr::rename(!!!map)
              }

              ret
            } else {
              predicted <- NULL # Just declaring variable.
              if (model_object$classification_type == "binary") { # Make it model agnostic.
                if ("xgboost_exp" %in% class(model_object)) {
                  predicted <- extract_predicted_binary_labels(model_object, type = "test", threshold = binary_classification_threshold) # If threshold is specified in ..., take it.
                  predicted_probability <- extract_predicted(model_object, type = "test")
                }
                else {
                  predicted <- test_pred_ret$predicted_label
                  predicted_probability <- test_pred_ret$predicted_probability
                }
                is_rpart <- "rpart" %in% class(model_object)
                ret <- evaluate_binary_classification(actual, predicted, predicted_probability, pretty.name = pretty.name, is_rpart = is_rpart)
              }
              else {
                if ("xgboost_exp" %in% class(model_object)) {
                  predicted <- extract_predicted_multiclass_labels(model_object, type = "test")
                }
                else {
                  predicted <- test_pred_ret$predicted_label
                }
                ret <- evaluate_multi_(data.frame(predicted = predicted, actual = actual),
                                       "predicted", "actual", pretty.name = pretty.name)
              }
              ret
            }
          },
          evaluation_by_class = { # We assume this is called only for multiclass classification.
            if ("xgboost_exp" %in% class(model_object)) {
              predicted <- extract_predicted_multiclass_labels(model_object, type = "test")
            }
            else {
              predicted <- test_pred_ret$predicted_label
            }
            per_level <- function(klass) {
              ret <- evaluate_classification(actual, predicted, klass, pretty.name = pretty.name)
            }

            dplyr::bind_rows(lapply(levels(actual), per_level))
          },
          conf_mat = {
            model_object <- df$model[[1]]
            if ("xgboost_exp" %in% class(model_object)) {
              if (get_prediction_type(model_object) == "binary") {
                predicted <- extract_predicted_binary_labels(model_object, type = "test", threshold = binary_classification_threshold)
              }
              else {
                predicted <- extract_predicted_multiclass_labels(model_object, type = "test")
              }
            }
            else {
              predicted <- test_pred_ret$predicted_label
            }
            ret <- calc_conf_mat(actual, predicted)
            ret
          })
      }, error = function(e) {
        data.frame()
      })
    }

    # data is already grouped rowwise, but to get group column value on the output, we need to group it explicitly with the group column.
    if (length(grouped_col) > 0) {
      # Use data without failed model (filtered) here, since the routine for test data cannot handle such rows.
      data <- filtered %>% group_by(!!!rlang::syms(grouped_col))
    }

    test_ret <- do_on_each_group(data, each_func, with_unnest = TRUE)

    if (nrow(test_ret) > 0) {
      test_ret$is_test_data <- TRUE
      ret <- ret %>% dplyr::bind_rows(test_ret)
    }
  }


  # Sort data by class and is_test_data if there is more than one row of evaluation results for
  # both training data and test data or training data
  if (!is.null(ret$is_test_data) && nrow(ret) > 0) {
    ret <- switch(type,
             evaluation = {
               ret %>% dplyr::arrange(is_test_data, .by_group = TRUE)
             },
             evaluation_by_class = {
               if (pretty.name) {
                 ret %>% dplyr::arrange(Class, is_test_data, .by_group = TRUE)
               } else {
                 ret %>% dplyr::arrange(class, is_test_data, .by_group = TRUE)
               }
             },
             conf_mat = {
               ret
             })
  }

  # Prettify is_test_data column. Do this after the above arrange calls, since it looks at is_test_data column.
  if (!is.null(ret$is_test_data) && pretty.name) {
    ret <- ret %>% dplyr::select(is_test_data, everything()) %>%
      dplyr::mutate(is_test_data = dplyr::if_else(is_test_data, "Test", "Training")) %>%
      dplyr::rename(`Data Type` = is_test_data)
  }
  ret
}

#' wrapper for tidy type partial dependence
#' @export
rf_partial_dependence <- function(df, ...) { # TODO: write test for this.
  res <- df %>% tidy_rowwise(model, type="partial_dependence", ...)
  if (nrow(res) == 0) {
    return(data.frame()) # Skip the rest of processing by returning empty data.frame.
  }
  grouped_col <- grouped_by(res) # When called from analytics view, this should be a single column or empty.
                                 # grouped_by has to be on res rather than on df since dplyr::group_vars
                                 # does not work on rowwise-grouped data frame.

  if (length(grouped_col) > 0) {
    res <- res %>% dplyr::ungroup() # ungroup to mutate group_by column.

    # Folloing is not necessary since we separately display partial dependence plot for each group since v5.5.
    # add variable name to the group_by column, so that chart is repeated by the combination of group_by column and variable name.
    # res[[grouped_col]] <- paste(as.character(res[[grouped_col]]), res$x_name)

    res[[grouped_col]] <- forcats::fct_inorder(factor(res[[grouped_col]])) # set order to appear as facets
    res <- res %>% dplyr::group_by(!!!rlang::syms(grouped_col)) # put back group_by for consistency
  }
  else {
    res$x_name <- forcats::fct_inorder(factor(res$x_name)) # set order to appear as facets
  }
  # gather we did after edarf::partial_dependence call turned x_value into factor if not all variables were in a same data type like numeric.
  # to keep the numeric or factor order (e.g. Sun, Mon, Tue) of x_value in the resulting chart, we do fct_inorder here while x_value is in order.
  # the first factor() is for the case x_value is not already a factor, to avoid error from fct_inorder()
  res <- res %>% dplyr::mutate(x_value = forcats::fct_inorder(factor(x_value))) # TODO: if same number appears for different variables, order will be broken.
  res
}

ubSMOTE2 <- function(X,Y, max_synth_perc=200, target_minority_perc=40, target_size=NULL, k=5, ...) {
  if(!is.factor(Y))
    stop("Y has to be a factor")
  if(is.vector(X))
    stop("X cannot be a vector")

  data<-cbind(X,Y)
  id.1 <- which(Y == 1)

  # Get minority size
  minority_size = length(id.1)
  # Get majoirty size
  majority_size = NROW(data) - minority_size

  row.has.na<-function(X)
    return(apply(X,1,function(x){any(is.na(x))}))

  smote_minority <- function(data, synth_perc, k) {
    if (synth_perc < 100) { # ubSmoteExs throws error if synth_perc is less than 100.
                            # Work it around by synthesizing at least 100% and sample then.
      synth_perc_ <- 100
    }
    else {
      synth_perc_ <- synth_perc
    }
    newExs <- unbalanced::ubSmoteExs(data[id.1,], "Y", synth_perc_, k)
    row.is.na<-row.has.na(newExs)

    if(any(row.is.na)) {
      newExs<-newExs[!row.is.na, ]
      colnames(newExs)<-colnames(data)
      cat("WARNING: NAs generated by SMOTE removed \n")
    }

    if (synth_perc < 100) { # ubSmoteExs throws error if synth_perc is less than 100.
                            # Work it around by sampling.
      nrow_to_synth <- as.integer(nrow(data) * synth_perc / 100)
      if (nrow(newExs) > nrow_to_synth) {
        newExs <- newExs %>% sample_rows(nrow_to_synth)
      }
    }
    newExs <- newExs %>% dplyr::mutate(synthesized = TRUE)
    newExs
  }

  sample_majority <- function(data, size) {
    # get the undersample of the "majority class" examples
    selMaj <- sample((1:NROW(data))[-id.1], size, replace=F)

    ret <- data[selMaj,]
    ret
  }

  sample_minority <- function(data, size) {
    # get the undersample of the "minority class" examples
    selMin <- sample((1:NROW(data))[id.1], size, replace=F)

    ret <- data[selMin,]
    ret
  }

  # Size of minority data after synthesizing to the max.
  max_minority_size <- minority_size * (100 + max_synth_perc) / 100

  if (is.null(target_size)) {
    if (minority_size / (minority_size + majority_size) >= target_minority_perc / 100) {
      # Already enough minority for the ratio even without SMOTE.
      # No Action. Already above target minority ratio.
      newdataset <- data %>% dplyr::mutate(synthesized = FALSE)
    }
    else if (max_minority_size / (max_minority_size + majority_size) >= target_minority_perc / 100) {
      # Enough minority with SMOTE. SMOTE necessary number of minority rows.
      target_minority_size <- majority_size / (100 / target_minority_perc - 1)
      synth_perc <- (target_minority_size - minority_size) / minority_size * 100
      newExs <- smote_minority(data, synth_perc, k)
      newdataset <- dplyr::bind_rows(data, newExs)
    }
    else {
      # Not enough minority even with SMOTE
      # SMOTE to the limit and sample down to make target ratio
      newExs <- smote_minority(data, max_synth_perc, k)
      # get the undersample of the "majority class" examples
      target_majority_size <- as.integer((nrow(newExs) + minority_size) / target_minority_perc * (100 - target_minority_perc))
      majority_data <- sample_majority(data, target_majority_size)
      minority_data <- data[id.1,]

      # the final data set (the undersample + the rare cases + the smoted exs)
      newdataset <- dplyr::bind_rows(majority_data, minority_data, newExs)
    }
  }
  else {
    target_majority_size <- as.integer(target_size * (1 - target_minority_perc / 100))
    target_minority_size <- as.integer(target_size * (target_minority_perc / 100))
    if (minority_size >= target_minority_size) {
      # Enough minority
      if (majority_size >= target_majority_size) {
        # Enough majority. Sample down both
        majority_data <- sample_majority(data, target_majority_size)
        minority_data <- sample_minority(data, target_minority_size)
        newdataset <- dplyr::bind_rows(majority_data, minority_data)
        newdataset <- newdataset %>% dplyr::mutate(synthesized = FALSE)
      }
      else {
        # Not enough majority.
        # No Action. Already above target minority ratio.
        newdataset <- data %>% dplyr::mutate(synthesized = FALSE)
      }
    }
    else {
      # Not enough minority
      if (majority_size >= target_majority_size) {
        # Enough majority. SMOTE and Sample down
        if (max_minority_size / (max_minority_size + target_majority_size) >= target_minority_perc / 100) {
          # Enough Minority With SMOTE. SMOTE and sample down
          synth_perc <- (target_minority_size - minority_size) / minority_size * 100
          newExs <- smote_minority(data, synth_perc, k)
          majority_data <- sample_majority(data, target_majority_size)
          minority_data <- data[id.1,]
          newdataset <- dplyr::bind_rows(majority_data, minority_data, newExs)
        }
        else {
          # Not Enough Minority even with SMOTE.
          # SMOTE to the limit and sample down to make target ratio.
          newExs <- smote_minority(data, max_synth_perc, k)
          # get the undersample of the "majority class" examples
          target_majority_size <- as.integer((nrow(newExs) + minority_size) / target_minority_perc * (100 - target_minority_perc))
          majority_data <- sample_majority(data, target_majority_size)
          minority_data <- data[id.1,]
          # the final data set (the undersample + the rare cases + the smoted exs)
          newdataset <- dplyr::bind_rows(majority_data, minority_data, newExs)
        }
      }
      else {
        # Not enough majority. SMOTE to make target ratio.
        if (minority_size / (majority_size + minority_size) >= target_minority_perc / 100) {
          # Already enough minority for the ratio even without SMOTE.
          # No Action.  Already above target minority ratio.
          newdataset <- data %>% dplyr::mutate(synthesized = FALSE)
        }
        else if (max_minority_size / (max_minority_size + majority_size) >= target_minority_perc / 100) {
          # Enough Minority With SMOTE. Just SMOTE minority.
          target_minority_size <- majority_size / (100 / target_minority_perc - 1)
          synth_perc <- (target_minority_size - minority_size) / minority_size * 100
          newExs <- smote_minority(data, synth_perc, k)
          newdataset <- dplyr::bind_rows(data, newExs)
        }
        else {
          # Not Enough Minority even with SMOTE.
          # SMOTE to the limit and sample down to make target ratio
          newExs <- smote_minority(data, max_synth_perc, k)
          # get the undersample of the "majority class" examples
          target_majority_size <- as.integer((nrow(newExs) + minority_size) / target_minority_perc * (100 - target_minority_perc))
          majority_data <- sample_majority(data, target_majority_size)
          minority_data <- data[id.1,]

          # the final data set (the undersample + the rare cases + the smoted exs)
          newdataset <- dplyr::bind_rows(majority_data, minority_data, newExs)
        }
      }
    }
  }
  # Fill NA in synthesized with FALSE.
  newdataset <- newdataset %>% mutate(synthesized = if_else(is.na(synthesized), FALSE, synthesized))
  #shuffle the order of instances
  newdataset<-newdataset[sample(1:NROW(newdataset)), ]
  newdataset
}

#' applies SMOTE to a data frame
#' @param target - the binary value column that becomes target of model later. can be logical, factor, character or numeric.
#' @export
exp_balance <- function(df,
                     target,
                     target_minority_perc = 40,
                     target_size = 50000,
                     max_synth_perc = 200,
                     k = 5,
                     max_nrow=50000, # max_nrow, sample is pre-5.1 legacy. They are ignored now.
                     sample=TRUE,
                     ...,
                     seed = NULL
                     ) {
  if(!is.null(seed)){
    set.seed(seed)
  }
  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  target_col <- tidyselect::vars_select(names(df), !! rlang::enquo(target))
  was_target_logical <- is.logical(df[[target_col]]) # record if the target was logical originally and turn it back to logical if so.
  was_target_character <- is.character(df[[target_col]])
  was_target_factor <- is.factor(df[[target_col]])
  was_target_numeric <- is.numeric(df[[target_col]])
  if (was_target_numeric) { # if target is numeric, make it factor first, to remember original values as factor levels and set it back later.
    df[[target_col]] <- factor(df[[target_col]])
  }
  orig_levels_order <- NULL
  if (was_target_factor || was_target_numeric) { # if target is factor, remember original factor order and set it back later.
    orig_levels_order <- levels(df[[target_col]])
  }
  grouped_col <- grouped_by(df)

  each_func <- function(df) {

    # Old pre 5.1 code, which used to sample data.
    # We do not do this any more to avoid sampling precious minority data.
    #
    # sample data since smote can be slow when data is big.
    # if (sample) {
    #   df <- df %>% sample_rows(max_nrow)
    # }

    factorized_cols <- c()
    for(col in colnames(df)){
      if(col == target_col) { # skip target_col
      }
      else if(!is.factor(df[[col]]) && !is.numeric(df[[col]])) {
        factorized_cols <- c(factorized_cols, col)
        # columns other than numeric have to be factor. otherwise ubSMOTE throws mysterious error like "invalid 'labels'; length 0 should be 1 or 2"
        # also, turn NA into explicit level. Otherwise ubSMOTE throws "invalid 'labels'; length 0 should be 1 or 2" for this case too.
        df <- df %>% dplyr::mutate(!!rlang::sym(col):=forcats::fct_explicit_na(as.factor(!!rlang::sym(col))))
      }
      else if(is.factor(df[[col]])) {
        # if already factor, just turn NAs into explicit levels.
        if (is.ordered(df[[col]])) {
          # if ordered, make it not ordered, since ordered factor columns are filled with NAs by ubSMOTE().
          df <- df %>% dplyr::mutate(!!rlang::sym(col):=forcats::fct_explicit_na(factor(!!rlang::sym(col), ordered=FALSE)))
        }
        else {
          # if not ordered, just turn NAs into explicit levels.
          df <- df %>% dplyr::mutate(!!rlang::sym(col):=forcats::fct_explicit_na(!!rlang::sym(col)))
        }
      }
    }

    # Remember integer column so that we can bring it back to integer later.
    integer_cols <- c()
    for(col in colnames(df)){
      if(col == target_col) { # skip target_col
      }
      else if(is_integer(df[[col]])) {
        integer_cols <- c(integer_cols, col)
      }
    }

    # record orig_df at this point so that the data type reverting works fine later when we have to return this instead of smoted df.
    orig_df <- df

    # filter rows after recording orig_df.
    for(col in colnames(df)){
      if(col == target_col) {
        # filter NA rows from target column, regardless of the type.
        df <- df %>% dplyr::filter(!is.na(df[[col]]))
      }
      else if(is.numeric(df[[col]])) {
        # for numeric cols, filter NA rows. With NAs, ubSMOTE throws mysterious error like "invalid 'labels'; length 0 should be 1 or 2"
        # TODO: we should probably warn if more than half of rows are filtered by a column, so that user can remove that column.
        df <- df %>% dplyr::filter(!is.na(df[[col]]) & !is.infinite(df[[col]]))
      }
    }
    if (nrow(df) == 0) { # if no rows are left, give up smote and return original df.
      df_balanced <- orig_df # TODO: we should throw error and let user know which columns with NAs to remove.
    }
    else if (n_distinct(df[[target_col]]) < 2) {
      # TODO: add test for this case.
      # if filtering NAs makes unique values of target col less than 2, give up smote and return original df.
      df_balanced <- orig_df # TODO: we should throw error and let user know which columns with NAs to remove.
    }
    else {
      input  <- df[, !(names(df) %in% target_col), drop=FALSE] # drop=FALSE is to prevent input from turning into vector when only one column is left.
      output <- factor(df[[target_col]])
      output <- forcats::fct_infreq(output)
      orig_levels <- levels(output)
      levels(output) <- c("0", "1")
      df_balanced <- ubSMOTE2(input, output,
                              target_minority_perc = target_minority_perc,
                              target_size = target_size,
                              max_synth_perc = max_synth_perc,
                              k = k,
                              ...) # defaults are, max_synth_perc=200, target_minority_perc=40, target_size=NULL, k = 5

      # revert the name changes made by ubSMOTE.
      colnames(df_balanced) <- c(colnames(input), target_col, "synthesized")

      # verify that df_balanced still keeps 2 unique values. it seems that SMOTE sometimes undersamples majority too much till it becomes 0.
      unique_val <- unique(df_balanced[[target_col]])
      if (length(unique_val[!is.na(unique_val)]) <= 1) {
        # in this case, give up SMOTE and return original. TODO: look into how to prevent this.
        df_balanced <- orig_df
      }
      levels(df_balanced[[target_col]]) <- orig_levels # set original labels before turning it into 0/1.
    }


    if (was_target_logical) {
      df_balanced[[target_col]] <- as.logical(df_balanced[[target_col]]) # turn it back to logical.
    }
    if (was_target_character) {
      df_balanced[[target_col]] <- as.character(df_balanced[[target_col]]) # turn it back to character
    }
    if (!is.null(orig_levels_order)) { # if target was factor, set original factor order. note this is different from orig_levels.
      df_balanced[[target_col]] <- forcats::fct_relevel(df_balanced[[target_col]], orig_levels_order)
    }
    if (was_target_numeric) {
      # turn it back to numeric. as.character is necessary to get back to original values rather than the factor level integer.
      df_balanced[[target_col]] <- as.numeric(as.character(df_balanced[[target_col]]))
    }

    for(col in factorized_cols) { # set factorized columns back to character. TODO: take care of other types.
      df_balanced[[col]] <- as.character(df_balanced[[col]])
    }

    # Round to make original integer columns back to integer.
    # Not doing so has some drawback especially in tree-based models.
    # https://github.com/scikit-learn-contrib/imbalanced-learn/issues/154
    # TODO: Consider SMOTE-NC.
    for(col in integer_cols) {
      df_balanced[[col]] <- round(df_balanced[[col]])
    }

    df_balanced
  }

  tmp_col <- avoid_conflict(grouped_col, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~each_func(.)), tmp_col)) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(!!rlang::sym(tmp_col))

  # grouping should be kept
  if(length(grouped_col) != 0){
    ret <- dplyr::group_by(ret, !!!rlang::syms(grouped_col))
  }
  # bring target_col at the beginning
  ret <- ret %>% select(!!rlang::sym(target_col), dplyr::everything())
  ret
}

get_classification_type <- function(v) {
  # if target is numeric, it is regression but
  # if not, it is classification
  if (!is.numeric(v)) {
    if (!is.logical(v)) {
      # Even if number of unique number is 2, we treat it as multi-class as opposed to binary,
      # since our logic for binary classification depends on the condition that the values are TRUE and FALSE.
      classification_type <- "multi"
    }
    else {
      classification_type <- "binary"
    }
  }
  else {
    classification_type <- "regression"
  }
}

cleanup_df <- function(df, target_col, selected_cols, grouped_cols, target_n, predictor_n, map_name=TRUE) {
  # drop unrelated columns so that SMOTE later does not have to deal with them.
  # select_ was not able to handle space in target_col. let's do it in base R way.
  df <- df[,colnames(df) %in% c(grouped_cols, selected_cols, target_col), drop=FALSE]

  # remove grouped col or target col
  selected_cols <- setdiff(selected_cols, c(grouped_cols, target_col))

  if (any(c(target_col, selected_cols) %in% grouped_cols)) {
    stop("grouping column is used as variable columns")
  }

  if (target_n < 2) {
    stop("Max # of categories for target var must be at least 2.")
  }

  if (predictor_n < 2) {
    stop("Max # of categories for explanatory vars must be at least 2.")
  }

  # cols will be filtered to remove invalid columns
  cols <- selected_cols

  # randomForest fails if columns are not clean
  clean_df <- df

  if (map_name) {
    # This mapping will be used to restore column names
    # We used to use janitor::clean_names(df), but
    # since we are setting the names back anyway,
    # we can use names like c1, c2 which is guaranteed to be safe
    # regardless of original column names.
    name_map <- paste0("c", 1:length(colnames(df)), "_")
  }
  else {
    # Do only mapping with minimum name changes to avoid error, which is explained below.
    # we need this mode for rpart since the plotting will be done directly from the model
    # and we want original column names to appear.
    # fortunately, rpart seems to be robust enough against strange column names
    # even on sjis windows.
    # just create a map with almost same names so that the rest of the code works.

    # Cleaning of column names for mmpf::marginalPrediction (for partial dependence). Comma is not handled well. Replace them with '.'.
    # ()"' and spaces are known to be ok as of version 5.5.2.
    name_map <- gsub('[,]', '.', colnames(df))
  }
  names(name_map) <- colnames(df)

  # clean_names changes column names
  # without chaning grouping column name
  # information in the data frame
  # and it causes an error,
  # so the value of grouping columns
  # should be still the names of grouping columns
  name_map[grouped_cols] <- grouped_cols
  colnames(clean_df) <- name_map

  clean_target_col <- name_map[target_col]
  clean_cols <- name_map[cols]

  if (!is.numeric(clean_df[[clean_target_col]]) && !is.logical(clean_df[[clean_target_col]])) {
    # limit the number of levels in factor by fct_lump
    clean_df[[clean_target_col]] <- forcats::fct_lump(
      as.factor(clean_df[[clean_target_col]]), n = target_n, ties.method="first"
    )
  }

  ret <- new.env()
  ret$clean_df <- clean_df
  ret$name_map <- name_map
  ret$clean_target_col <- clean_target_col
  ret$clean_cols <- clean_cols
  ret
}

cleanup_df_per_group <- function(df, clean_target_col, max_nrow, clean_cols, name_map, predictor_n, revert_logical_levels=TRUE, filter_numeric_na=FALSE, convert_logical=TRUE) {
  df <- preprocess_regression_data_before_sample(df, clean_target_col, clean_cols,
                                                 filter_predictor_numeric_na=filter_numeric_na)
  clean_cols <- attr(df, 'predictors') # predictors are updated (removed) in preprocess_pre_sample. Catch up with it.
  # sample the data because randomForest takes long time
  # if data size is too large
  sampled_nrow <- NULL
  if (!is.null(max_nrow) && nrow(df) > max_nrow) {
    # Record that sampling happened.
    sampled_nrow <- max_nrow
    df <- df %>% sample_rows(max_nrow)
  }

  if (convert_logical && is.logical(df[[clean_target_col]])) {
    # we need to convert logical to factor since na.roughfix only works for numeric or factor.
    # for logical set TRUE, FALSE level order for better visualization. but only do it when
    # the target column actually has both TRUE and FALSE, since edarf::partial_dependence errors out if target
    # factor column has more levels than actual data.
    # error from edarf::partial_dependence looks like following.
    #   Error in factor(x, seq_len(length(unique(data[[target]]))), levels(data[[target]])) : invalid 'labels'; length 2 should be 1 or 1
    if (length(unique(df[[clean_target_col]])) >= 2) {
      if (revert_logical_levels) { # random forest case. For viz, revert order
        levels <- c("TRUE","FALSE")
      }
      else { # rpart case, we cannot revert because it messes up probability displayed on rpart.plot image.
        levels <- c("FALSE","TRUE")
      }
      df[[clean_target_col]] <- factor(df[[clean_target_col]], levels=levels)
    }
    else {
      df[[clean_target_col]] <- factor(df[[clean_target_col]])
    }
  }

  df <- preprocess_regression_data_after_sample(df, clean_target_col, clean_cols, predictor_n = predictor_n, name_map = name_map)
  c_cols <- attr(df, 'predictors') # predictors are updated (added and/or removed) in preprocess_post_sample. Catch up with it.
  name_map <- attr(df, 'name_map')

  ret <- new.env()
  ret$df <- df
  ret$c_cols <- c_cols
  ret$name_map <- name_map
  ret$sampled_nrow <- sampled_nrow
  ret
}

# Extract importance history as a data framea with decisions for each variable.
# Shadow variables are filtered out.
extract_importance_history_from_boruta <- function(x) {
  res <- tidyr::gather(as.data.frame(x$ImpHistory), "variable","importance")
  decisions <- data.frame(variable=names(x$finalDecision), decision=x$finalDecision)
  res <- res %>% dplyr::left_join(decisions, by = "variable")
  # Remove rows with NA, which are shadow variables.
  # Also remove -Inf importance which seems to mean removed rejected variables in the iterations toward the end.
  # If we kept those -Inf values, the x-axis ordering of boxplot would be messed up.
  res <- res %>% dplyr::filter(decision %in% c("Confirmed", "Tentative", "Rejected") & importance != -Inf)
  res
}

# Extract vector of column names in the order of importance.
# Used to determine variables to run partial dependency on.
extract_important_variables_from_boruta <- function(x) {
  res <- extract_importance_history_from_boruta(x)
  # res <- res %>% dplyr::filter(decision != "Rejected") # We used to filter out rejected variables, but now we don't for consistency with others like lm.
  res <- res %>% dplyr::group_by(variable) %>% dplyr::summarize(importance = median(importance, na.rm = TRUE))
  res <- res %>% dplyr::arrange(desc(importance))
  res$variable
}

# This function should return following 2 columns.
# - variable - Name of variable
# - importance - Importance of the variable
# Rows should be sorted by importance in descending order.
# Avoid the name importance.ranger since it exists.
importance_ranger <- function(model) {
  imp <- ranger::importance(model)
  imp_df <- tibble::tibble( # Use tibble since data.frame() would make variable factors, which breaks things in following steps.
    variable = names(imp),
    importance = imp
  ) %>% dplyr::arrange(-importance)
  imp_df <- imp_df %>% dplyr::mutate(importance = pmax(importance, 0)) # Show 0 for negative importance, which can be caused by chance in case of permutation importance.
  imp_df
}

#' Get feature importance for multi class classification using randomForest
#' @export
calc_feature_imp <- function(df,
                             target,
                             ...,
                             target_fun = NULL,
                             predictor_funs = NULL,
                             max_nrow = 50000, # Down from 200000 when we added partial dependence
                             max_sample_size = NULL, # Half of max_nrow. down from 100000 when we added partial dependence
                             ntree = 20,
                             nodesize = 12,
                             target_n = 20,
                             predictor_n = 12, # So that at least months can fit in it.
                             smote = FALSE,
                             smote_target_minority_perc = 40,
                             smote_max_synth_perc = 200,
                             smote_k = 5,
                             importance_measure = "permutation", # "permutation", "firm", or "impurity".
                             max_pd_vars = NULL,
                             # Number of most important variables to calculate partial dependences on. 
                             # By default, when Boruta is on, all Confirmed/Tentative variables.
                             # 12 when Boruta is off.
                             pd_sample_size = 500,
                             pd_grid_resolution = 20,
                             pd_with_bin_means = FALSE, # Default is FALSE for backward compatibility on the server
                             with_boruta = FALSE,
                             boruta_max_runs = 20, # Maximal number of importance source runs.
                             boruta_p_value = 0.05, # Boruta recommends using the default 0.01 for P-value, but we are using 0.05 for consistency with other functions of ours.
                             seed = 1,
                             test_rate = 0.0,
                             test_split_type = "random" # "random" or "ordered"
                             ){

  if(test_rate < 0 | 1 < test_rate){
    stop("test_rate must be between 0 and 1")
  } else if (test_rate == 1){
    stop("test_rate must be less than 1")
  }

  # For now, if FIRM is specified with Boruta, which is not supported, we run Boruta with permutation importance instead.
  if (with_boruta && importance_measure == "firm") {
    importance_measure <- "permutation"
  }

  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  target_col <- tidyselect::vars_select(names(df), !! rlang::enquo(target))
  # this evaluates select arguments like starts_with
  orig_selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))

  target_funs <- NULL
  if (!is.null(target_fun)) {
    target_funs <- list(target_fun)
    names(target_funs) <- target_col
    df <- df %>% mutate_predictors(target_col, target_funs)
  }

  if (!is.null(predictor_funs)) {
    df <- df %>% mutate_predictors(orig_selected_cols, predictor_funs)
    selected_cols <- names(unlist(predictor_funs))
  }
  else {
    selected_cols <- orig_selected_cols
  }

  grouped_cols <- grouped_by(df)

  # Sort predictors so that the result of permutation importance is stable against change of column order.
  selected_cols <- stringr::str_sort(selected_cols)

  # Remember if the target column was originally numeric or logical before converting type.
  is_target_logical_or_numeric <- is.numeric(df[[target_col]]) || is.logical(df[[target_col]])

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
      if(!is.null(seed)){
        set.seed(seed)
      }

      # If we are to do SMOTE, do not down sample here and let exp_balance handle it so that we do not sample out precious minority data.
      unique_val <- unique(df[[clean_target_col]])
      if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
        sample_size <- NULL
      }
      else {
        sample_size <- max_nrow
      }
      # Training actually works without filtering numeric NA, but predict on ranger fails with NAs.
      # To keep distribution of training data and test data on par with each other, we are filtering them from training data too.
      # https://github.com/imbs-hl/ranger/pull/109
      filter_numeric_na = test_rate > 0
      clean_df_ret <- cleanup_df_per_group(df, clean_target_col, sample_size, clean_cols, name_map, predictor_n, filter_numeric_na=filter_numeric_na)
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
      model_df <- model.frame(fml, data = df, na.action = randomForest::na.roughfix)

      # all or max_sample_size data will be used for randomForest
      # to grow a tree
      if (is.null(max_sample_size)) { # default to half of max_nrow
        max_sample_size = max_nrow/2
      }
      sample.fraction <- min(c(max_sample_size / max_nrow, 1))

      if (with_boruta || # Run only either Boruta or ranger::importance.
          importance_measure == "firm" || # FIRM does not depend on importance result from ranger.
          length(c_cols) <= 1) { # Calculate importance only when there are multiple variables.
        ranger_importance_measure <- "none"
      }
      else {
        # "permutation" or "impurity".
        ranger_importance_measure <- importance_measure
      }
      model <- ranger::ranger(
        fml,
        data = model_df,
        importance = ranger_importance_measure,
        num.trees = ntree,
        min.node.size = nodesize,
        keep.inbag=TRUE,
        sample.fraction = sample.fraction,
        probability = (classification_type %in% c("multi", "binary"))
      )
      # prediction result in the ranger model (ret$predictions) is for some reason different from and worse than
      # the prediction separately done with the same training data.
      # Make prediction with training data here and keep it, so that we can use this separate prediction for prediction, evaluation, etc.
      model$prediction_training <- predict(model, model_df)

      if (test_rate > 0) {
        df_test_clean <- cleanup_df_for_test(df_test, df, c_cols)
        na_row_numbers_test <- attr(df_test_clean, "na_row_numbers")
        unknown_category_rows_index <- attr(df_test_clean, "unknown_category_rows_index")

        prediction_test <- predict(model, df_test_clean)
        # TODO: Following current convention for the name na.action to keep na row index, but we might want to rethink.
        # We do not keep this for training since na.roughfix should fill values and not delete rows.
        attr(prediction_test, "na.action") <- na_row_numbers_test
        attr(prediction_test, "unknown_category_rows_index") <- unknown_category_rows_index
        model$prediction_test <- prediction_test
      }

      if (with_boruta) { # Run only either Boruta or ranger::importance.
        if (importance_measure == "impurity") {
          getImp <- Boruta::getImpRfGini
        }
        else { # default to equivalent of "permutation". This includes the case where "firm" is specified.
          getImp <- Boruta::getImpRfZ
        }
        model$boruta <- Boruta::Boruta(
          fml,
          data = model_df,
          doTrace = 0,
          maxRuns = boruta_max_runs + 1, # It seems Boruta stops at maxRuns - 1 iterations. Add 1 to be less confusing.
          pValue = boruta_p_value,
          getImp = getImp,
          # Following parameters are to be relayed to ranger::ranger through Boruta::Boruta, then Boruta::getImpRfZ.
          num.trees = ntree,
          min.node.size = nodesize,
          sample.fraction = sample.fraction,
          probability = (classification_type == "binary") # build probability tree for AUC only for binary classification.
        )
        # These attributes are used in tidy. They are also at ranger level, but we are making Boruta object self-contained.
        model$boruta$classification_type <- classification_type
        model$boruta$orig_levels <- orig_levels
        model$boruta$terms_mapping <- names(name_map)
        names(model$boruta$terms_mapping) <- name_map
        class(model$boruta) <- c("Boruta_exploratory", class(model$boruta))
        imp_vars <- extract_important_variables_from_boruta(model$boruta)
        if (is.null(max_pd_vars)) {
          max_pd_vars <- 20 # Number of most important variables to calculate partial dependences on. This used to be 12 but we decided it was a little too small.
        }
        # max_pd_vars is not applied by default with Boruta.
        if (length(imp_vars) > 0) {
          imp_vars <- imp_vars[1:min(length(imp_vars), max_pd_vars)] # take max_pd_vars most important variables
        }
      }
      else if (importance_measure %in% c("permutation", "impurity")) { # Make use of output from ranger in these cases.
        # return partial dependence
        if (length(c_cols) > 1) { # Calculate importance only when there are multiple variables.
          imp_df <- importance_ranger(model)
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
        imp_vars <- imp_vars[1:min(length(imp_vars), max_pd_vars)] # take max_pd_vars most important variables
      }
      else {
        # Skip inp_vars filtering for FIRM, since we need to calculate PDP for all variables to get FIRM for all variables.
        imp_vars <- c_cols # Just use c_cols as is for imp_vars to calculate partial dependence.
      }
      imp_vars <- as.character(imp_vars) # for some reason imp_vars is converted to factor at this point. turn it back to character.
      model$imp_vars <- imp_vars
      # Second element of n argument needs to be less than or equal to sample size, to avoid error.
      if (length(imp_vars) > 0) {
        model$partial_dependence <- partial_dependence.ranger(model, vars=imp_vars, data=model_df, n=c(pd_grid_resolution, min(model$num.samples, pd_sample_size)))
      }
      else {
        model$partial_dependence <- NULL
      }

      if (importance_measure == "firm") { # If importance measure is FIRM, we calculate them now, after PDP is calculated.
        if (length(c_cols) > 1) { # Calculate importance only when there are multiple variables.
          pdp_target_col <- if (classification_type == "binary") {
            "TRUE"
          }
          else {
            attr(model$partial_dependence, "target")
          }
          imp_df <- importance_firm(model$partial_dependence, pdp_target_col, imp_vars)
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
        imp_vars <- imp_vars[1:min(length(imp_vars), max_pd_vars)] # take max_pd_vars most important variables
        model$imp_vars <- imp_vars
        # Shrink the partial dependence data keeping only the important variables.
        model$partial_dependence <- shrink_partial_dependence_data(model$partial_dependence, imp_vars)
      }

      if (length(imp_vars) > 0) {
        if (pd_with_bin_means && is_target_logical_or_numeric) {
          # We calculate means of bins only for logical or numeric target to keep the visualization simple.
          model$partial_binning <- calc_partial_binning_data(model_df, clean_target_col, imp_vars)
        }
      }

      # these attributes are used in tidy of randomForest
      model$classification_type <- classification_type
      model$orig_levels <- orig_levels
      model$terms_mapping <- names(name_map)
      names(model$terms_mapping) <- name_map
      model$y <- model.response(model_df)
      model$df <- model_df
      # To avoid saving a huge environment when caching with RDS.
      attr(attr(model$df, "terms"), ".Environment") <- NULL
      model$formula_terms <- terms(fml)
      # To avoid saving a huge environment when caching with RDS.
      attr(model$formula_terms,".Environment") <- NULL
      model$sampled_nrow <- clean_df_ret$sampled_nrow

      model$orig_target_col <- target_col # Used for relocating columns as well as for applying function.
      if (!is.null(target_funs)) {
        model$target_funs <- target_funs
      }
      if (!is.null(predictor_funs)) {
        model$orig_predictor_cols <- orig_selected_cols
        attr(predictor_funs, "LC_TIME") <- Sys.getlocale("LC_TIME")
        attr(predictor_funs, "sysname") <- Sys.info()[["sysname"]] # Save platform name (e.g. Windows) since locale name might need conversion for the platform this model will be run on.
        attr(predictor_funs, "lubridate.week.start") <- getOption("lubridate.week.start")
        model$predictor_funs <- predictor_funs
      }

      list(model = model, test_index = test_index, source_data = source_data)
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # In repeat-by case, we report group-specific error in the Summary table,
        # so that analysis on other groups can go on.
        class(e) <- c("ranger", class(e))
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

#' TODO: not really for external use. hide it.
#' TODO: use this other places doing similar thing.
#' @export
#' @param multi_class - TRUE when we need class and size, which we show for multiclass classification case.
evaluate_classification <- function(actual, predicted, class, multi_class = TRUE, pretty.name = FALSE) { #TODO user better name for class not to confuse with class()
  if (length(actual) != length(predicted)) {
    stop("Assertion: actual and predicted have different length in evaluate_classification.")
  }
  tp <- sum(actual == class & predicted == class, na.rm = TRUE)
  tn <- sum(actual != class & predicted != class, na.rm = TRUE)
  fp <- sum(actual != class & predicted == class, na.rm = TRUE)
  fn <- sum(actual == class & predicted != class, na.rm = TRUE)

  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)

  accuracy <- (tp + tn) / (tp + fp + tn + fn)

  f_score <- 2 * ((precision * recall) / (precision + recall))
  # This avoids NaN. If both precision and recall are 0, it makes sense for f score to be 0.
  # is.nan() on precision and recall is necessary to avoid error here.
  if(!is.nan(precision) && !is.nan(recall) && precision + recall == 0) {
    f_score <- 0
  }

  if (multi_class) {
    n <- sum(actual == class, na.rm = TRUE)

    ret <- data.frame(
      class,
      f_score,
      accuracy,
      1- accuracy,
      precision,
      recall,
      n
    )
  }
  else { # class, n is not necessary when it is binary classification with TRUE/FALSE
    nt <- tp + fn
    nf <- fp + tn
    ret <- data.frame(
      f_score,
      accuracy,
      1- accuracy,
      precision,
      recall,
      nt,
      nf
    )
  }

  names(ret) <- if(pretty.name){
    if (multi_class) {
      c("Class", "F Score", "Accuracy Rate", "Misclassification Rate", "Precision", "Recall", "Number of Rows")
    } else {
      c("F Score", "Accuracy Rate", "Misclassification Rate", "Precision", "Recall", "Number of Rows for TRUE", "Number of Rows for FALSE")
    }
  } else {
    if (multi_class) {
      c("class", "f_score", "accuracy_rate", "misclassification_rate", "precision", "recall", "n")
    } else {
      c("f_score", "accuracy_rate", "misclassification_rate", "precision", "recall", "n_true", "n_false")
    }
  }
  ret
}

#' @export
# not really an external function but exposing for sharing with rpart.R TODO: find better way.
evaluate_binary_classification <- function(actual, predicted, predicted_probability, pretty.name = FALSE, is_rpart = FALSE) {
  # calculate AUC from ROC
  if (is_rpart && is.factor(actual) && "TRUE" %in% levels(actual)) { # target was logical and converted to factor.
    # For rpart, level for "TRUE" is 2, and that does not work with the logic in else clause.
    # For ranger, even if level for label "TRUE" is 2, we treat level 1 as TRUE, for simplicity for now, which can be handled by the else clause.
    actual_for_roc <- actual == "TRUE"
  }
  else {
    actual_for_roc <- as.integer(actual)==1
  }
  auc <- auroc(predicted_probability, actual_for_roc)

  if (is.logical(actual)) { # For xgboost, where logical works as is, convert it to a factor so that the logic originally implemented for ranger works for it.
    actual <- factor(actual, levels=c("TRUE", "FALSE"))
  }

  if ((is.factor(actual) && "TRUE" %in% levels(actual))) { # target was logical and converted to factor.
    if (is_rpart) {
      # For rpart, level for "TRUE" is 2, and that does not work with the logic in else clause.
      true_class <- "TRUE"
    }
    else {
      # For ranger, even if level for label "TRUE" is 2, we always treat level 1 as TRUE, for simplicity for now.
      true_class <- levels(actual)[[1]]
    }
    # Since multi_class = FALSE is specified, Number of Rows is not added here. Will add later.
    ret <- evaluate_classification(actual, predicted, true_class, multi_class = FALSE, pretty.name = pretty.name)
  }
  else { # Because get_classification_type() considers it binary classification only when target is logical, it should never come here, but cowardly keeping the code for now.
    ret <- evaluate_multi_(data.frame(predicted=predicted, actual=actual), "predicted", "actual", pretty.name = pretty.name)
  }
  if (pretty.name) {
    ret <- dplyr::bind_cols(tibble::tibble(AUC = auc), ret)
  }
  else {
    ret <- dplyr::bind_cols(tibble::tibble(auc = auc), ret)
  }
  # Add Number of Rows here for the case ret came from evaluate_classification(multi_class = FALSE).
  if (is.null(ret$n) && is.null(ret$`Number of Rows`)) {
    sample_n <- sum(!is.na(predicted)) # Sample size for test.
    ret <- ret %>% dplyr::mutate(n = !!sample_n)
    if(pretty.name){
      ret <- ret %>% dplyr::rename(`Number of Rows` = n)
    }
  }
  ret
}

#' @export
#' @param type "importance", "evaluation" or "conf_mat". Feature importance, evaluated scores or confusion matrix of training data.
tidy.ranger <- function(x, type = "importance", pretty.name = FALSE, binary_classification_threshold = 0.5, ...) {
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
      else if (is.null(x$imp_df)) { # This means it is for Step, as opposed to Analytics View. TODO: We might want to separate function for Step and Analytics View.
        tryCatch({
          imp <- ranger::importance(x)
          ret <- tibble::tibble( # Use tibble since data.frame() would make variable factors, which breaks things in following steps.
            variable = x$terms_mapping[names(imp)],
            importance = imp
          ) %>% dplyr::arrange(-importance)

        }, error = function(e){	
          ret <<- data.frame()	
        })
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
      actual <- x$y
      if(is.numeric(actual)){
        glance(x, pretty.name = pretty.name, ...)
      } else {
        if (x$classification_type == "binary") {
          predicted <- predict_value_from_prob(NULL, x$prediction_training$predictions, NULL, threshold = binary_classification_threshold)
          predicted_probability <- x$prediction_training$predictions[,1]
          ret <- evaluate_binary_classification(actual, predicted, predicted_probability, pretty.name = pretty.name)
        }
        else {
          predicted <- predict_value_from_prob(x$forest$levels, x$prediction_training$predictions, x$y)
          ret <- evaluate_multi_(data.frame(predicted=predicted, actual=actual), "predicted", "actual", pretty.name = pretty.name)
        }
        ret
      }
    },
    evaluation_by_class = {
      # get evaluation scores from training data
      actual <- x$y
      if (x$classification_type == "binary") {
        predicted <- predict_value_from_prob(NULL, x$prediction_training$predictions, NULL, threshold = binary_classification_threshold)
      }
      else {
        predicted <- predict_value_from_prob(x$forest$levels, x$prediction_training$predictions, x$y)
      }

      per_level <- function(level) {
        ret <- evaluate_classification(actual, predicted, level, pretty.name = pretty.name)
        ret
      }
      dplyr::bind_rows(lapply(levels(actual), per_level))
    },
    conf_mat = {
      # return confusion matrix
      if (x$classification_type == "binary") {
        predicted <- predict_value_from_prob(NULL, x$prediction_training$predictions, NULL, threshold = binary_classification_threshold)
      }
      else {
        predicted <- predict_value_from_prob(x$forest$levels, x$prediction_training$predictions, x$y)
      }

      ret <- calc_conf_mat(x$y, predicted)
      ret
    },
    scatter = {
      # return actual and predicted value pairs
      if (x$classification_type == "binary") {
        predicted <- predict_value_from_prob(NULL, x$prediction_training$predictions, NULL, threshold = binary_classification_threshold)
      }
      else if (x$classification_type == "mutli") {
        predicted <- predict_value_from_prob(x$forest$levels, x$prediction_training$predictions, x$y)
      } else {
        # classification type is regression
        predicted <- x$prediction_training$predictions
      }
      ret <- data.frame(
        expected_value = x$y,
        predicted_value = predicted
      ) %>%
        dplyr::filter(!is.na(predicted_value))

      ret
    },
    partial_dependence = {
      ret <- handle_partial_dependence(x)
      ret
    },
    boruta = {
      if (!is.null(x$boruta)) {
        ret <- tidy.Boruta_exploratory(x$boruta)
      }
      else {
        ret <- data.frame() # Skip by returning empty data.frame.
      }
      ret
    },
    {
      stop(paste0("type ", type, " is not defined"))
    }
  )
}

# This is used from Analytics View only when classification type is regression.
#' @export
glance.ranger <- function(x, pretty.name = FALSE, ...) {
  if ("error" %in% class(x)) {
    ret <- data.frame(Note = x$message)
    return(ret)
  }
  glance.ranger.method <- switch(x[["treetype"]],
                                        "Classification" = glance.ranger.classification,
                                        "Probability estimation" = glance.ranger.classification,
                                        "Regression" = glance.ranger.regression)
  ret <- glance.ranger.method(x, pretty.name = pretty.name, ...)
  ret
}

#' @export
glance.ranger.regression <- function(x, pretty.name, ...) {
  predicted <- x$prediction_training$predictions
  actual <- x$y
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
      `Number of Rows` = as.symbol("n")
    )
    ret <- ret %>%
      dplyr::rename(!!!map)
  }
  ret
}


# This is used only for step, and not for Analytics View. TODO: We might want to unify the code.
# For Analytics View, tidy.ranger(type="evaluation"), which is called from rf_evaluation_training_and_test() is used for the Summary table.
#' @export
glance.ranger.classification <- function(x, pretty.name, ...) {
  # Both actual and predicted have no NA values.
  actual <- x$y
  # Without threshold specified, predict_value_from_prob here always assumes it is a multiclass classification,
  # rather than binary classification, and just returns whichever has higher probability,
  # but this is ok since we don't have threshold to specify here anyway for now.
  predicted <- predict_value_from_prob(x$forest$levels, x$prediction_training$predictions, x$y)
  levels(predicted) <- levels(actual)

  # Composes data.frame of binary classification evaluation summary.
  single_stat <- function(act, pred) {
    #       predicted
    #actual   TRUE FALSE
    #  TRUE    tp    fn
    #  FALSE   fp    tn

    conf_mat <- table(act, pred)
    tp <- conf_mat[1]
    tn <- conf_mat[4]
    fn <- conf_mat[3]
    fp <- conf_mat[2]
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    accuracy <- (tp + tn) / (tp + tn + fp + fn)
    f_score <- 2 * ((precision * recall) / (precision + recall))

    ret <- data.frame(f_score, accuracy, 1 - accuracy, precision, recall)
    names(ret) <- if (pretty.name) {
      c("F Score", "Accuracy Rate", "Misclassification Rate", "Precision", "Recall")
    } else {
      c("f_score", "accuracy_rate", "misclassification_rate", "precision", "recall")
    }
    ret

  }

  if (x$classification_type == "binary") {
    ret <- single_stat(actual, predicted)
    pred_prob <- x$prediction_training$predictions[,levels(actual)[1]]
    actual_val <- actual == levels(actual)[1] # Make it a logical
    # calculate AUC
    auc <- auroc(pred_prob, actual_val)
    auc_ret <- data.frame(auc)
    if (pretty.name) {
      auc_ret <- auc_ret %>% dplyr::rename(AUC=auc)
    }
    ret <- cbind(auc_ret, ret)
    ret

  } else {
    # Multiclass classification metrics
    evaluate_multi_(data.frame(predicted=predicted, actual=actual), "predicted", "actual", pretty.name = pretty.name)
  }
}

# This is used from Analytics View only when classification type is regression.
#' @export
glance.rpart <- function(x, pretty.name = FALSE, ...) {
  if ("error" %in% class(x)) {
    ret <- data.frame(Note = x$message)
    return(ret)
  }
  actual <- x$y
  predicted <- predict(x)
  rmse_val <- rmse(actual, predicted)
  r_squared_val <- r_squared(actual, predicted)
  ret <- data.frame(
    r_squared = r_squared_val,
    root_mean_square_error = rmse_val,
    n = length(x$y)
  )

  if(pretty.name){
    map = list(
      `R Squared` = as.symbol("r_squared"),
      `RMSE` = as.symbol("root_mean_square_error"),
      `Number of Rows` = as.symbol("n")
    )
    ret <- ret %>%
      dplyr::rename(!!!map)
  }
  ret
}

# TODO: Make this function model-agnostic and consolidate. There are similar code for lm/glm, ranger, rpart, and xgboost.
# Builds partial_dependency object for ranger. Originally from edarf:::partial_dependence.ranger.
partial_dependence.ranger <- function(fit, vars = colnames(data),
  n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L),
        nrow(data)), # entire given data is used by default, but this will override it when calling it.
  interaction = FALSE, uniform = TRUE, data, ...) {

  target = strsplit(strsplit(as.character(fit$call), "formula")[[2]], " ~")[[1]][[1]]

  predict.fun = function(object, newdata) {
    predict(object, data = newdata)$predictions
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
      if (fit$treetype == "Regression")
        names(mp)[ncol(mp)] = target
      mp
    }, simplify = FALSE), fill = TRUE)
    data.table::setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else {
    pd = do.call(mmpf::marginalPrediction, args)
    if (fit$treetype == "Regression")
      names(pd)[ncol(pd)] = target
  }

  attr(pd, "class") = c("pd", "data.frame")
  attr(pd, "interaction") = interaction == TRUE
  attr(pd, "target") = if (fit$treetype == "Regression") target else colnames(fit$predictions)
  attr(pd, "vars") = vars
  attr(pd, "points") = points
  attr(pd, "quantile_points") = quantile_points
  pd
}

# TODO: Make this function model-agnostic and consolidate. There are similar code for lm/glm, ranger, rpart, and xgboost.
# Builds partial_dependency object for rpart with same structure (a data.frame with attributes.) as edarf::partial_dependence.
partial_dependence.rpart = function(fit, target, vars = colnames(data),
  n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)), # Keeping same default of 25 as edarf::partial_dependence, although we usually overwrite from callers.
  interaction = FALSE, uniform = TRUE, data, ...) {

  predict.fun <- function(object, newdata) {
    # Within our use cases, rpart always returns numeric values or probability
    # even with multiclass classification.
    # In that case, just calling predict is enough.
    # edarf code for ranger does something more for classification case,
    # but it is not necessary here.
    ret <- predict(object, newdata)
    ret
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
  
  if (length(vars) > 1L & !interaction) { # More than one variables are there. Iterate calling mmpf::marginalPrediction.
    pd = rbindlist(sapply(vars, function(x) {
      args$vars = x
      if ("points" %in% names(args))
        args$points = args$points[x]
      mp = do.call(mmpf::marginalPrediction, args)
      if (fit$classification_type == "regression")
        names(mp)[ncol(mp)] = target
      mp
    }, simplify = FALSE), fill = TRUE)
    data.table::setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else {
    pd = do.call(mmpf::marginalPrediction, args)
    if (fit$classification_type == "regression")
      names(pd)[ncol(pd)] = target
  }

  attr(pd, "class") = c("pd", "data.frame")
  attr(pd, "interaction") = interaction == TRUE
  attr(pd, "target") = if (fit$classification_type %in% c("regression", "binary")) target else attr(fit,"ylevels")
  attr(pd, "vars") = vars
  attr(pd, "points") = points
  attr(pd, "quantile_points") = quantile_points
  pd
}

#' @export
exp_rpart <- function(df,
                      target,
                      ...,
                      target_fun = NULL,
                      predictor_funs = NULL,
                      max_nrow = 50000, # down from 200000 when we added partial dependence
                      target_n = 20,
                      predictor_n = 12, # so that at least months can fit in it.
                      binary_classification_threshold = 0.5,
                      smote = FALSE,
                      smote_target_minority_perc = 40,
                      smote_max_synth_perc = 200,
                      smote_k = 5,
                      importance_measure = "impurity", # "firm", or "impurity". Defaulting to impurity for backward compatibility for pre-6.5.
                      max_pd_vars = 20,
                      pd_sample_size = 500,
                      pd_grid_resolution = 20,
                      pd_with_bin_means = FALSE, # Default is FALSE for backward compatibility on the server
                      seed = 1,
                      minsplit = 20, # The minimum number of observations that must exist in a node in order for a split to be attempted. Passed down to rpart()
                      minbucket = round(minsplit/3), # The minimum number of observations in any terminal node. Passed down to rpart()
                      cp = 0.01, # Complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted. Passed down to rpart()
                      maxdepth = 30, # Set the maximum depth of any node of the final tree, with the root node counted as depth 0. Passed down to rpart()
                      test_rate = 0.0,
                      test_split_type = "random" # "random" or "ordered"
                      ) {
  if(test_rate < 0 | 1 < test_rate){
    stop("test_rate must be between 0 and 1")
  } else if (test_rate == 1){
    stop("test_rate must be less than 1")
  }

  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  target_col <- tidyselect::vars_select(names(df), !! rlang::enquo(target))
  # this evaluates select arguments like starts_with
  orig_selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))

  target_funs <- NULL
  if (!is.null(target_fun)) {
    target_funs <- list(target_fun)
    names(target_funs) <- target_col
    df <- df %>% mutate_predictors(target_col, target_funs)
  }

  if (!is.null(predictor_funs)) {
    df <- df %>% mutate_predictors(orig_selected_cols, predictor_funs)
    selected_cols <- names(unlist(predictor_funs))
  }
  else {
    selected_cols <- orig_selected_cols
  }

  grouped_cols <- grouped_by(df)

  # Sort predictors so that the result of permutation importance is stable against change of column order.
  selected_cols <- stringr::str_sort(selected_cols)

  # Remember if the target column was originally numeric or logical before converting type.
  is_target_logical_or_numeric <- is.numeric(df[[target_col]]) || is.logical(df[[target_col]])

  clean_ret <- cleanup_df(df, target_col, selected_cols, grouped_cols, target_n, predictor_n, map_name=FALSE)

  clean_df <- clean_ret$clean_df
  name_map <- clean_ret$name_map
  clean_target_col <- clean_ret$clean_target_col
  clean_cols <- clean_ret$clean_cols

  classification_type <- get_classification_type(clean_df[[clean_target_col]])

  each_func <- function(df) {
    tryCatch({
      if(!is.null(seed)){
        set.seed(seed)
      }

      # If we are to do SMOTE, do not down sample here and let exp_balance handle it so that we do not sample out precious minority data.
      unique_val <- unique(df[[clean_target_col]])
      if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
        sample_size <- NULL
      }
      else {
        sample_size <- max_nrow
      }
      # especially multiclass classification seems to take forever when number of unique values of predictors are many.
      # fct_lump is essential here.
      # http://grokbase.com/t/r/r-help/051sayg38p/r-multi-class-classification-using-rpart
      clean_df_ret <- cleanup_df_per_group(df, clean_target_col, sample_size, clean_cols, name_map, predictor_n, revert_logical_levels=FALSE, filter_numeric_na=TRUE)
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
      # if target is categorical (not numeric) and only 1 unique value (or all NA), throw error.
      if ("numeric" %nin% class(clean_target_col) &&
          "integer" %nin% class(clean_target_col) &&
          length(unique_val[!is.na(unique_val)]) <= 1) {
        # We might want to skip and show the status in Summary when we support Repeat By,
        # but for now just throw error to show.
        stop("Categorical Target Variable must have 2 or more unique values.")
      }

      # split training and test data
      source_data <- df
      test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
      df <- safe_slice(source_data, test_index, remove = TRUE)
      if (test_rate > 0) {
        df_test <- safe_slice(source_data, test_index, remove = FALSE)
      }

      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      fml <- as.formula(paste0("`", clean_target_col, "`", " ~ ", rhs))
      model <- rpart::rpart(fml, df, minsplit = minsplit, minbucket = minbucket, cp = cp, maxdepth = maxdepth)
      model$classification_type <- classification_type
      if (classification_type %in% c("binary", "multi")) {
        # we need to call this here rather than in tidy(),
        # since there is randomness involved in breaking tie
        # of multiclass classification.
        model$predicted_class <- get_predicted_class_rpart(model,
                                                           binary_classification_threshold = binary_classification_threshold)
      }
      model$terms_mapping <- names(name_map)
      names(model$terms_mapping) <- name_map
      model$formula_terms <- terms(fml)
      # To avoid saving a huge environment when caching with RDS.
      attr(model$formula_terms,".Environment") <- NULL
      model$sampled_nrow <- clean_df_ret$sampled_nrow

      # Find list of important variables and run partial dependence on them.
      if (length(c_cols) <= 1) { # Show importance only when there are multiple variables.
        error <- simpleError("Variable importance requires two or more variables.")
        model$imp_df <- error
        imp_vars <- c_cols
      }
      else if (importance_measure == "impurity" && !is.null(model$variable.importance)) { # It is possible variable.importance is missing for example when no split happened.
        imp <- model$variable.importance
        imp_vars <- names(imp) # model$variable.importance is already sorted by importance.
        imp_vars <- imp_vars[1:min(length(imp_vars), max_pd_vars)] # Keep only max_pd_vars most important variables
        model$imp_df <- tibble::tibble(
          variable = names(imp),
          importance = imp
        )
      }
      else if (importance_measure == "firm") {
        # Skip inp_vars filtering for FIRM, since we need to calculate PDP for all variables to get FIRM for all variables.
        imp_vars <- c_cols
      }
      else {
        error <- simpleError("Variable importance is not available because there is no split in the decision tree.")
        model$imp_df <- error
        imp_vars <- c_cols[1:min(length(c_cols), max_pd_vars)] # Keep only max_pd_vars first cols since we have no way to know importance.
      }

      model$partial_dependence <- partial_dependence.rpart(model, clean_target_col, vars=imp_vars, data=df, n=c(pd_grid_resolution, min(nrow(df), pd_sample_size)))
      model$imp_vars <- imp_vars # keep imp_vars in the model for ordering of charts based on the importance.

      if (importance_measure == "firm") { # If importance measure is FIRM, we calculate them now, after PDP is calculated.
        if (length(c_cols) > 1) { # Calculate importance only when there are multiple variables.
          pdp_target_col <- if (classification_type == "binary") {
            "TRUE"
          }
          else {
            attr(model$partial_dependence, "target")
          }
          imp_df <- importance_firm(model$partial_dependence, pdp_target_col, imp_vars)
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
        imp_vars <- imp_vars[1:min(length(imp_vars), max_pd_vars)] # take max_pd_vars most important variables
        model$imp_vars <- imp_vars
        # Shrink the partial dependence data keeping only the important variables.
        model$partial_dependence <- shrink_partial_dependence_data(model$partial_dependence, imp_vars)
      }

      if (pd_with_bin_means && is_target_logical_or_numeric) {
        # We calculate means of bins only for logical or numeric target to keep the visualization simple.
        model$partial_binning <- calc_partial_binning_data(df, clean_target_col, imp_vars)
      }

      if (test_rate > 0) {
        # Handle NA rows for test. For training, rpart seems to automatically handle it, and row numbers of
        # removed rows are stored in model$na.action.
        df_test_clean <- cleanup_df_for_test(df_test, df, c_cols)
        na_row_numbers_test <- attr(df_test_clean, "na_row_numbers")
        unknown_category_rows_index <- attr(df_test_clean, "unknown_category_rows_index")

        model$prediction_test <- predict(model, df_test_clean)
        model$na_row_numbers_test <- na_row_numbers_test
        model$unknown_category_rows_index_test <- unknown_category_rows_index
        if (classification_type %in% c("binary", "multi")) {
          model$predicted_class_test <- get_predicted_class_rpart(model, data_type = "test",
                                                                  binary_classification_threshold = binary_classification_threshold)
        }
      }

      model$orig_target_col <- target_col # Used for relocating columns as well as for applying function.
      if (!is.null(target_funs)) {
        model$target_funs <- target_funs
      }
      if (!is.null(predictor_funs)) {
        model$orig_predictor_cols <- orig_selected_cols
        attr(predictor_funs, "LC_TIME") <- Sys.getlocale("LC_TIME")
        attr(predictor_funs, "sysname") <- Sys.info()[["sysname"]] # Save platform name (e.g. Windows) since locale name might need conversion for the platform this model will be run on.
        attr(predictor_funs, "lubridate.week.start") <- getOption("lubridate.week.start")
        model$predictor_funs <- predictor_funs
      }

      if (!is.null(model$terms)) {
        attr(model$terms,".Environment") <- NULL
      }

      list(model = model, test_index = test_index, source_data = source_data)
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # In repeat-by case, we report group-specific error in the Summary table,
        # so that analysis on other groups can go on.
        class(e) <- c("rpart", class(e))
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

  # add special class .model to pass column type validation at viz layer.
  # also add .model.rpart so that a step created by this function is viewable with Exploratory for debugging.
  class(ret$model) <- c("list", ".model", ".model.rpart")
  ret
}

get_binary_predicted_probability_rpart <- function(x, data_type = "training") {
  if (class(x$y) == "logical") {
    # predict(x) is numeric vector of probability of being TRUE.
    if (data_type == "training") {
      predicted <- predict(x)
    }
    else {
      predicted <- x$prediction_test
    }
  }
  else {
    # predict(x) is 2-diminsional matrix with 2 columns for the 2 categories. values in the matrix is the probabilities.
    # Return the probability for the value for the 1st column.
    true_index <- 1

    # We convert logical to factor with level of "FALSE", "TRUE". We had to keep this order not to mess up tree image for rpart.
    # In this case, we need to return the probability of being TRUE, which is the 2nd column.

    ylevels <- attr(x,"ylevels")
    if (ylevels[[1]] == "FALSE" && ylevels[[2]] == "TRUE") {
      true_index <- 2
    }

    if (data_type == "training") {
      predicted <- predict(x)[,true_index]
    }
    else {
      predicted <- x$prediction_test[,true_index]
    }
  }
  predicted
}

get_multiclass_predicted_value_from_probability_rpart <- function(x, data = NULL, data_type = 'training') {
  ylevels <- attr(x,"ylevels")
  predicted_mat <- if (is.data.frame(data)) {
    predict(x, data)
  } else {
    if (data_type == 'training') {
      predict(x)
    }
    else {
      x$prediction_test
    }
  }
  # ties are broken randomly to be even.
  # TODO: move this to model building step so that there is no randomness in analytics viz preprocessor.
  predicted_idx <- max.col(predicted_mat, ties.method = "random")
  predicted <- factor(ylevels[predicted_idx], levels=ylevels)
  predicted
}

get_multiclass_predicted_probability_rpart <- function(x, data = NULL, data_type = 'training') {
  ylevels <- attr(x,"ylevels")
  predicted_mat <- if (is.data.frame(data)) {
    predict(x, data)
  } else {
    if (data_type == 'training') {
      predict(x)
    }
    else {
      x$prediction_test
    }
  }
  # ties are broken randomly to be even.
  # TODO: move this to model building step so that there is no randomness in analytics viz preprocessor.
  predicted <- apply(predicted_mat, 1, max)
  predicted
}

get_actual_class_rpart <- function(x) {
  if (x$classification_type == "binary") {
    if (class(x$y) == "logical") {
      ylevels <- c("TRUE", "FALSE")
      actual <- factor(x$y, levels=ylevels)
    }
    else {
      ylevels <- attr(x,"ylevels")
      actual <- factor(ylevels[x$y], levels=ylevels)
    }
  }
  else {
    # multiclass case
    ylevels <- attr(x,"ylevels")
    # TODO: rpart returns probability of each class, but we are not fully making use of them.
    actual <- factor(ylevels[x$y], levels=ylevels)
  }
  actual
}

get_class_levels_rpart <- function(x) {
  if (x$classification_type == "binary") {
    if (class(x$y) == "logical") { # TODO: is this really possible for rpart? looks this returns "integer" when original input is logical.
      ylevels <- c("TRUE", "FALSE")
    }
    else {
      ylevels <- attr(x,"ylevels")
    }
  }
  else {
    # multiclass case
    ylevels <- attr(x,"ylevels")
  }
  ylevels
}

get_predicted_class_rpart <- function(x, data_type = "training", binary_classification_threshold = 0.5) {
  if (x$classification_type == "binary") {
    ylevels <- attr(x,"ylevels")
    if (data_type == "training") {
      predicted <- predict_value_from_prob(ylevels,
                                           predict(x),
                                           NULL, threshold = binary_classification_threshold)
    }
    else {
      predicted <- predict_value_from_prob(ylevels,
                                           x$prediction_test,
                                           NULL, threshold = binary_classification_threshold)
    }
  }
  else {
    predicted <- get_multiclass_predicted_value_from_probability_rpart(x, data_type = data_type)
  }
  predicted
}

get_predicted_probability_rpart <- function(x, data_type = "training") {
  if (x$classification_type == "binary") {
    predicted <- get_binary_predicted_probability_rpart(x, data_type = data_type)
  }
  else {
    predicted <- get_multiclass_predicted_probability_rpart(x, data_type = data_type)
  }
  predicted
}

#' @export
#' @param type "importance", "evaluation" or "conf_mat". Feature importance, evaluated scores or confusion matrix of training data.
tidy.rpart <- function(x, type = "importance", pretty.name = FALSE, ...) {
  if ("error" %in% class(x) && type != "evaluation") {
    ret <- data.frame()
    return(ret)
  }
  switch(
    type,
    importance = {
      if (is.null(x$imp_df) || "error" %in% class(x$imp_df)) {
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

      if(x$classification_type == "regression"){
        actual <- x$y
        glance(x, pretty.name = pretty.name, ...)
      } else {
        actual <- get_actual_class_rpart(x)
        predicted <- x$predicted_class
        # unlike ranger, x$y of rpart in this case is integer. convert it to factor to make use of common functions.
        if (x$classification_type == "binary") {
          if (class(x$y) == "logical") {
            predicted_probability <- predict(x)
          }
          else if (is.factor(actual) && "TRUE" %in% levels(actual)) {
            # For rpart, we convert logical to facter with levels of "FALSE", "TRUE" in this order.
            # In this case, probability for TRUE is on the 2nd column.
            predicted_probability <- predict(x)[,2]
          }
          else {
            predicted_probability <- predict(x)[,1]
          }
          ret <- evaluate_binary_classification(actual, predicted, predicted_probability, pretty.name = pretty.name, is_rpart = TRUE)
        }
        else {
          # multiclass case
          # TODO: rpart returns probability of each class, but we are not fully making use of them.
          ret <- evaluate_multi_(data.frame(predicted=predicted, actual=actual), "predicted", "actual", pretty.name = pretty.name)
        }
        ret
      }
    },
    evaluation_by_class = {
      # get evaluation scores from training data

      actual <- get_actual_class_rpart(x)
      predicted <- x$predicted_class
      ylevels <- get_class_levels_rpart(x)

      per_level <- function(level) {
        ret <- evaluate_classification(actual, predicted, level, pretty.name = pretty.name)
        ret
      }
      # for rpart, factor levels for logical case is kept in FALSE, TRUE order not to mess up rpart.plot.
      # so need to revert it now, right before visualization.
      if (length(ylevels) == 2 & all(ylevels == c("FALSE", "TRUE"))) {
        ylevels <- c("TRUE", "FALSE")
      }
      dplyr::bind_rows(lapply(ylevels, per_level))
    },
    conf_mat = {
      # return confusion matrix
      actual <- get_actual_class_rpart(x)
      predicted <- x$predicted_class

      # for rpart, factor levels for logical case is kept in FALSE, TRUE order not to mess up rpart.plot.
      # so need to revert it now, right before visualization.
      if (is.factor(actual) && length(levels(actual)) == 2 && all(levels(actual) == c("FALSE","TRUE"))) {
        actual <- forcats::fct_rev(actual)
      }
      if (is.factor(predicted) && length(levels(predicted)) == 2 && all(levels(predicted) == c("FALSE","TRUE"))) {
        predicted <- forcats::fct_rev(predicted)
      }

      ret <- calc_conf_mat(actual, predicted)
      ret
    },
    scatter = { # we assume this is called only for regression.
      predicted <- predict(x)
      ret <- data.frame(
        expected_value = x$y,
        predicted_value = predicted
      ) %>%
        dplyr::filter(!is.na(predicted_value))

      ret
    },
    partial_dependence = {
      ret <- handle_partial_dependence(x)
      ret
    },
    {
      stop(paste0("type ", type, " is not defined"))
    }
  )
}

tidy.Boruta_exploratory <- function(x, ...) {
  res <- extract_importance_history_from_boruta(x)
  # as.character is to be safe by converting from factor. With factor, reverse mapping result will be messed up.
  res$variable <- x$terms_mapping[as.character(res$variable)] # Map variable names back to original.
  res <- res %>% dplyr::mutate(variable = forcats::fct_reorder(variable, importance, .fun = median, .desc = TRUE))
  # Reorder types of decision in the order of more important to less important.
  res <- res %>% dplyr::mutate(decision = forcats::fct_relevel(decision, "Confirmed", "Tentative", "Rejected"))
  res
}

glance.Boruta_exploratory <- function(x, pretty.name = FALSE, ...) {
  res <- data.frame(iterations = nrow(x$ImpHistory), time_taken = as.numeric(x$timeTaken), p_value = x$pValue)
  if (pretty.name) {
    res <- res %>% dplyr::rename(Iterations = iterations, `Time Taken (Second)` = time_taken, `P Value Threshold` = p_value)
  }
  res
}
