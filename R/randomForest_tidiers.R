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
      imp_m[[1]] <- x$terms_mapping[imp_m[[1]]]
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

    per_level <- function(class) {
      tp <- sum(actual == class & predicted == class, na.rm = TRUE)
      tn <- sum(actual != class & predicted != class, na.rm = TRUE)
      fp <- sum(actual != class & predicted == class, na.rm = TRUE)
      fn <- sum(actual == class & predicted != class, na.rm = TRUE)

      precision <- tp / (tp + fp)
      # this avoids NA
      if(tp+fp == 0) {
        precision <- 0
      }

      recall <- tp / (tp + fn)
      # this avoids NA
      if(tn+fn == 0) {
        recall <- 0
      }

      accuracy <- (tp + tn) / (tp + fp + tn + fn)

      f_score <- 2 * ((precision * recall) / (precision + recall))
      # this avoids NA
      if(precision + recall == 0) {
        f_score <- 0
      }

      data_size <- sum(actual == class)

      ret <- data.frame(
        class,
        f_score,
        accuracy,
        1- accuracy,
        precision,
        recall,
        data_size
      )

      names(ret) <- if(pretty.name){
        c("Class", "F Score", "Accuracy Rate", "Missclassification Rate", "Precision", "Recall", "Data Size")
      } else {
        c("class", "f_score", "accuracy_rate", "missclassification_rate", "precision", "recall", "data_size")
      }
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
      dplyr::summarize(count = n())

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
    imp_m[[1]] <- x$terms_mapping[imp_m[[1]]]
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
  imp_m <- fix_data_frame(imp_m)
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

  per_level <- function(class) {
    # calculate evaluation scores for each class
    tp <- sum(actual == class & predicted == class)
    tn <- sum(actual != class & predicted != class)
    fp <- sum(actual != class & predicted == class)
    fn <- sum(actual == class & predicted != class)

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
      paste(class, c("F Score", "Precision", "Missclassification Rate", "Recall", "Accuracy"), sep = " ")
    } else {
      paste(class, c("f_score", "precision", "missclassification_rate", "recall", "accuracy"), sep = "_")
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
    # TODO: This seems to be taking care of NA cases. Should review this part later
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
    predicted <- same_type(levels(x[["y"]])[predicted], cleaned_data[[y_name]])

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
      data[[predicted_value_col]] <- predicted_prob
    } else if (x$classification_type == "multi"){
      # append predicted probability for each class, max and labels at max values
      ret <- get_multi_predicted_values(predicted_prob, cleaned_data[[y_name]])
      data <- dplyr::bind_cols(data, ret)
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

rename_groups <- function(n) {
  ifelse(grepl("^\\d", n), paste0("group_", n), n)
}

#' wrapper for tidy type importance
#' @export
rf_importance <- function(data, ...) {
  tidy(data, model, type = "importance", ...)
}

#' wrapper for tidy type importance
#' @export
rf_evaluation <- function(data, ...) {
  tidy(data, model, type = "evaluation", ...)
}

#' get feature importance for multi class classification using randomForest
#' @export
calc_feature_imp <- function(df,
                             target,
                             ...,
                             max_nrow = 100000,
                             samplesize = 100,
                             ntree = 20,
                             nodesize = 12,
                             target_n = 10,
                             predictor_n = 6){
  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  target_col <- dplyr::select_var(names(df), !! rlang::enquo(target))
  # this evaluates select arguments like starts_with
  selected_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  if (any(c(target_col, cols) %in% grouped_cols)) {
    stop("grouping column is used as variable columns")
  }

  # remove NA because it's not permitted for randomForest
  df <- df %>%
    dplyr::filter(!is.na(!!target_col))

  # cols will be filtered to remove invalid columns
  cols <- selected_cols
  for (col in selected_cols) {
    if(all(is.na(df[[col]]))){
      # remove columns if they are all NA
      cols <- setdiff(cols, col)
    } else {
      if(!is.numeric(df[[col]]) && !is.logical(df[[col]])) {
        # convert data to factor if predictors are not numeric or logical
        # and limit the number of levels in factor by fct_lump
        df[[col]] <- forcats::fct_lump(as.factor(df[[col]]), n=predictor_n)
      }
    }
  }

  # randomForest fails if columns are not clean
  clean_df <- janitor::clean_names(df)
  # this mapping will be used to restore column names
  name_map <- colnames(clean_df)
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

  # limit the number of levels in factor by fct_lump
  clean_df[[clean_target_col]] <- forcats::fct_lump(
    as.factor(clean_df[[clean_target_col]]), n = target_n
  )

  # build formula for randomForest
  rhs <- paste0("`", clean_cols, "`", collapse = " + ")
  fml <- as.formula(paste(clean_target_col, " ~ ", rhs))

  each_func <- function(df) {
    tryCatch({
      # sample the data because randomForest takes long time
      # if data size is too large
      if (nrow(df) > max_nrow) {
        df <- df %>%
          dplyr::sample_n(max_nrow)
      }

      # Return NULL if there is only one row
      # for a class of target variable because
      # rondomForest enters infinite loop
      # in such case.
      # The group with NULL is removed when
      # unnesting the result
      for (level in levels(df[[target_col]])) {
        if(sum(df[[target_col]] == level, na.rm = TRUE) == 1) {
          return(NULL)
        }
      }

      rf <- randomForest::randomForest(
        fml,
        data = df,
        importance = FALSE,
        samplesize = samplesize,
        nodesize=nodesize,
        ntree = ntree,
        na.action = randomForest::na.roughfix # replace NA with median (numeric) or mode (categorical)
      )
      # these attributes are used in tidy of randomForest
      rf$classification_type <- "multi"
      rf$terms_mapping <- names(name_map)
      names(rf$terms_mapping) <- name_map
      rf
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # ignore the error if
        # it is caused by subset of
        # grouped data frame
        # to show result of
        # data frames that succeed
        NULL
      } else {
        stop(e)
      }
    })
  }

  do_on_each_group(clean_df, each_func, name = "model", with_unnest = FALSE)
}
