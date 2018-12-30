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
      ret <- evaluate_classification(actual, predicted, class, pretty.name = pretty.name)
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
      paste(class, c("F Score", "Precision", "Misclassification Rate", "Recall", "Accuracy"), sep = " ")
    } else {
      paste(class, c("f_score", "precision", "misclassification_rate", "recall", "accuracy"), sep = "_")
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

rename_groups <- function(n) {
  ifelse(grepl("^\\d", n), paste0("group_", n), n)
}

#' wrapper for tidy type importance
#' @export
rf_importance <- function(data, ...) {
  broom::tidy(data, model, type = "importance", ...)
}

#' wrapper for tidy type importance
#' @export
rf_evaluation <- function(data, ...) {
  broom::tidy(data, model, type = "evaluation", ...)
}

#' wrapper for tidy type importance
#' @export
rf_evaluation_by_class <- function(data, ...) {
  broom::tidy(data, model, type = "evaluation_by_class", ...)
}

#' wrapper for tidy type partial dependence
#' @export
rf_partial_dependence <- function(df, ...) { # TODO: write test for this.
  res <- df %>% broom::tidy(model, type="partial_dependence", ...)
  grouped_col <- grouped_by(df) # when called from analytics view, this should be a single column or empty.
  if (length(grouped_col) > 0) {
    res <- res %>% dplyr::ungroup() # ungroup to mutate group_by column.
    # add variable name to the group_by column, so that chart is repeated by the combination of group_by column and variable name.
    res[[grouped_col]] <- paste(as.character(res[[grouped_col]]), res$x_name)
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

#' applies SMOTE to a data frame
#' @param target - the binary value column that becomes target of model later. can be logical, factor, character or numeric.
#' @export
exp_balance <- function(df,
                     target,
                     max_nrow=50000,
                     sample=TRUE,
                     verbose = FALSE,
                     ...
                     ) {
  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  target_col <- dplyr::select_var(names(df), !! rlang::enquo(target))
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

    # sample data since smote can be slow when data is big.
    if (sample && nrow(df) > max_nrow) {
      df <- df %>% dplyr::sample_n(max_nrow)
    }

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
      df_balanced <- unbalanced::ubSMOTE(input, output, verbose=verbose, ...) # defaults are, perc.over = 200, perc.under = 200, k = 5
      df_balanced <- as.data.frame(df_balanced)

      # revert the name changes made by ubSMOTE.
      colnames(df_balanced) <- c(colnames(input), target_col)

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
    df_balanced
  }

  tmp_col <- avoid_conflict(grouped_col, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~each_func(.)), tmp_col)) %>%
    dplyr::ungroup() %>%
    tidyr::unnest_(tmp_col)

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
      if (length(unique(v)) == 2) {
        classification_type <- "binary" 
      }
      else {
        classification_type <- "multi" 
      }
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

  # remove NA because it's not permitted for randomForest
  df <- df %>%
    dplyr::filter(!is.na(!!target_col))

  # cols will be filtered to remove invalid columns
  cols <- selected_cols

  for (col in selected_cols) {
    if(all(is.na(df[[col]]))){
      # remove columns if they are all NA
      cols <- setdiff(cols, col)
      df[[col]] <- NULL # drop the column so that SMOTE will not see it. 
    }
  }

  if (map_name) {
    # randomForest fails if columns are not clean
    clean_df <- df

    # This mapping will be used to restore column names
    # We used to use janitor::clean_names(df), but
    # since we are setting the names back anyway,
    # we can use names like c1, c2 which is guaranteed to be safe
    # regardless of original column names.
    name_map <- paste0("c",1:length(colnames(df)))
    names(name_map) <- colnames(df)

    # clean_names changes column names
    # without chaning grouping column name
    # information in the data frame
    # and it causes an error,
    # so the value of grouping columns
    # should be still the names of grouping columns
    name_map[grouped_cols] <- grouped_cols
    colnames(clean_df) <- name_map
  }
  else {
    # do not do mapping.
    # we need this mode for rpart since the plotting will be done directly from the model
    # and we want original column names to appear.
    # fortunately, rpart seems to be robust enough against strange column names
    # even on sjis windows.
    # just create a map with same names so that the rest of the code works.
    clean_df <- df
    name_map <- colnames(df)
    names(name_map) <- colnames(df)
  }

  clean_target_col <- name_map[target_col]
  clean_cols <- name_map[cols]

  if (!is.numeric(clean_df[[clean_target_col]]) && !is.logical(clean_df[[clean_target_col]])) {
    # limit the number of levels in factor by fct_lump
    clean_df[[clean_target_col]] <- forcats::fct_explicit_na(forcats::fct_lump(
      as.factor(clean_df[[clean_target_col]]), n = target_n, ties.method="first"
    ))
  }

  ret <- new.env()
  ret$clean_df <- clean_df
  ret$name_map <- name_map
  ret$clean_target_col <- clean_target_col
  ret$clean_cols <- clean_cols
  ret
}

cleanup_df_per_group <- function(df, clean_target_col, max_nrow, clean_cols, name_map, predictor_n, revert_logical_levels=TRUE) {
  if (is.factor(df[[clean_target_col]])) { # to avoid error in edarf::partial_dependence(), remove levels that is not used in this group.
    df[[clean_target_col]] <- forcats::fct_drop(df[[clean_target_col]])
  }
  # sample the data because randomForest takes long time
  # if data size is too large
  if (nrow(df) > max_nrow) {
    df <- df %>%
      dplyr::sample_n(max_nrow)
  }

  if (is.logical(df[[clean_target_col]])) {
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

  c_cols <- clean_cols
  for(col in clean_cols){
    if(lubridate::is.Date(df[[col]]) || lubridate::is.POSIXct(df[[col]])) {
      c_cols <- setdiff(c_cols, col)

      absolute_time_col <- avoid_conflict(colnames(df), paste0(col, "_abs_time"))
      wday_col <- avoid_conflict(colnames(df), paste0(col, "_day_of_week"))
      day_col <- avoid_conflict(colnames(df), paste0(col, "_day_of_month"))
      yday_col <- avoid_conflict(colnames(df), paste0(col, "_day_of_year"))
      month_col <- avoid_conflict(colnames(df), paste0(col, "_month"))
      year_col <- avoid_conflict(colnames(df), paste0(col, "_year"))
      new_name <- c(absolute_time_col, wday_col, day_col, yday_col, month_col, year_col)
      names(new_name) <- paste(
        names(name_map)[name_map == col],
        c(
          "_abs_time",
          "_day_of_week",
          "_day_of_month",
          "_day_of_year",
          "_month",
          "_year"
        ), sep="")

      name_map <- c(name_map, new_name)

      c_cols <- c(c_cols, absolute_time_col, wday_col, day_col, yday_col, month_col, year_col)
      df[[absolute_time_col]] <- as.numeric(df[[col]])
      df[[wday_col]] <- lubridate::wday(df[[col]], label=TRUE)
      df[[day_col]] <- lubridate::day(df[[col]])
      df[[yday_col]] <- lubridate::yday(df[[col]])
      df[[month_col]] <- lubridate::month(df[[col]], label=TRUE)
      df[[year_col]] <- lubridate::year(df[[col]])
      if(lubridate::is.POSIXct(df[[col]])) {
        hour_col <- avoid_conflict(colnames(df), paste0(col, "_hour"))
        new_name <- c(hour_col)
        names(new_name) <- paste(
          names(name_map)[name_map == col],
          c(
            "_hour"
          ), sep="")
        name_map <- c(name_map, new_name)

        c_cols <- c(c_cols, hour_col)
        df[[hour_col]] <- factor(lubridate::hour(df[[col]])) # treat hour as category
      }
      df[[col]] <- NULL # drop original Date/POSIXct column to pass SMOTE later.
    } else if(is.factor(df[[col]])) {
      # if the data is factor, respect the levels and keep first 10 levels, and make others "Others" level.
      if (length(levels(df[[col]])) >= predictor_n + 2) {
        df[[col]] <- forcats::fct_other(df[[col]], keep=levels(df[[col]])[1:predictor_n])
      }
    } else if(!is.numeric(df[[col]])) {
      # convert data to factor if predictors are not numeric.
      # and limit the number of levels in factor by fct_lump.
      # we need to convert logical to factor too since na.roughfix only works for numeric or factor.
      df[[col]] <- forcats::fct_explicit_na(forcats::fct_lump(as.factor(df[[col]]), n=predictor_n, ties.method="first"))
    } else {
      # filter Inf/-Inf to avoid following error from ranger.
      # Error in seq.default(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length.out) : 'from' must be a finite number
      df <- df %>% dplyr::filter(!is.infinite(.[[col]]))
    }
  }

  # This check should be done after all possible filtering.
  # Return NULL if there is only one row
  # for a class of target variable because
  # rondomForest enters infinite loop
  # in such case.
  # The group with NULL is removed when
  # unnesting the result
  # TODO: Should we just filter such row and go on??
  for (level in levels(df[[clean_target_col]])) {
    if(sum(df[[clean_target_col]] == level, na.rm = TRUE) == 1) {
      return(NULL)
    }
  }


  # remove columns with only one unique value
  cols_copy <- c_cols
  for (col in cols_copy) {
    unique_val <- unique(df[[col]])
    if (length(unique_val[!is.na(unique_val)]) <= 1) {
      c_cols <- setdiff(c_cols, col)
      df[[col]] <- NULL # drop the column so that SMOTE will not see it. 
    }
  }

  ret <- new.env()
  ret$df <- df
  ret$c_cols <- c_cols
  ret$name_map <- name_map
  ret
}

#' get feature importance for multi class classification using randomForest
#' @export
calc_feature_imp <- function(df,
                             target,
                             ...,
                             max_nrow = 50000, # down from 200000 when we added partial dependence
                             max_sample_size = NULL, # half of max_nrow. down from 100000 when we added partial dependence
                             ntree = 20,
                             nodesize = 12,
                             target_n = 20,
                             predictor_n = 12, # so that at least months can fit in it.
                             smote = FALSE,
                             max_pd_vars = 10, # number of most important variables to calculate partial dependences on.
                             seed = NULL
                             ){
  if(!is.null(seed)){
    set.seed(seed)
  }
  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  target_col <- dplyr::select_var(names(df), !! rlang::enquo(target))
  # this evaluates select arguments like starts_with
  selected_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

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
      clean_df_ret <- cleanup_df_per_group(df, clean_target_col, max_nrow, clean_cols, name_map, predictor_n)
      if (is.null(clean_df_ret)) {
        return(NULL) # skip this group
      }
      df <- clean_df_ret$df
      c_cols <- clean_df_ret$c_cols
      name_map <- clean_df_ret$name_map

      # apply smote if this is binary classification
      unique_val <- unique(df[[clean_target_col]])
      if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
        df <- df %>% exp_balance(clean_target_col, sample=FALSE) # since we already sampled, no further sample.
      }

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

      rf <- ranger::ranger(
        fml,
        data = model_df,
        importance = "impurity",
        num.trees = ntree,
        min.node.size = nodesize,
        sample.fraction = sample.fraction,
        probability = (classification_type == "binary") # build probability tree for AUC only for binary classification.
      )

      # return partial dependence
      imp <- ranger::importance(rf)
      imp_df <- data.frame(
        variable = names(imp),
        importance = imp
      ) %>% dplyr::arrange(-importance)
      imp_vars <- imp_df$variable
      # code to separate numeric and categorical. keeping it for now for possibility of design change
      # imp_vars_tmp <- imp_df$variable
      # imp_vars <- character(0)
      # if (var.type == "numeric") {
      #   # keep only numeric variables from important ones
      #   for (imp_var in imp_vars_tmp) {
      #     if (is.numeric(model_df[[imp_var]])) {
      #       imp_vars <- c(imp_vars, imp_var)
      #     }
      #   }
      # }
      # else {
      #   # keep only non-numeric variables from important ones
      #   for (imp_var in imp_vars_tmp) {
      #     if (!is.numeric(model_df[[imp_var]])) {
      #       imp_vars <- c(imp_vars, imp_var)
      #     }
      #   }
      # }
      imp_vars <- imp_vars[1:min(length(imp_vars), max_pd_vars)] # take max_pd_vars most important variables
      imp_vars <- as.character(imp_vars) # for some reason imp_vars is converted to factor at this point. turn it back to character.
      rf$imp_vars <- imp_vars
      rf$partial_dependence <- edarf::partial_dependence(rf, vars=imp_vars, data=model_df, n=c(20,20))

      # these attributes are used in tidy of randomForest
      rf$classification_type <- classification_type
      rf$orig_levels <- orig_levels
      rf$terms_mapping <- names(name_map)
      names(rf$terms_mapping) <- name_map
      rf$y <- model.response(model_df)
      rf$df <- model_df
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

#' TODO: not really for external use. hide it.
#' TODO: use this other places doing similar thing.
#' @export
#' @param multi_class - TRUE when we need class and data_size, which we show for multiclass classification case.
evaluate_classification <- function(actual, predicted, class, multi_class = TRUE, pretty.name = FALSE) { #TODO user better name for class not to confuse with class()
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
  if(tp+fn == 0) {
    recall <- 0
  }

  accuracy <- (tp + tn) / (tp + fp + tn + fn)

  f_score <- 2 * ((precision * recall) / (precision + recall))
  # this avoids NA
  if(precision + recall == 0) {
    f_score <- 0
  }

  if (multi_class) {
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
  }
  else { # class, data_size is not necessary when it is binary classification with TRUE/FALSE
    ret <- data.frame(
      f_score,
      accuracy,
      1- accuracy,
      precision,
      recall
    )
  }

  names(ret) <- if(pretty.name){
    if (multi_class) {
      c("Class", "F Score", "Accuracy Rate", "Misclassification Rate", "Precision", "Recall", "Data Size")
    } else {
      c("F Score", "Accuracy Rate", "Misclassification Rate", "Precision", "Recall")
    }
  } else {
    if (multi_class) {
      c("class", "f_score", "accuracy_rate", "misclassification_rate", "precision", "recall", "data_size")
    } else {
      c("f_score", "accuracy_rate", "misclassification_rate", "precision", "recall")
    }
  }
  ret
}

# with binary probability prediction model from ranger, take the level with bigger probability as the predicted value.
#' @export
# not really an external function but exposing for test. TODO: find better way.
get_binary_predicted_value_from_probability <- function(x) {
  # x$predictions is 2-diminsional matrix with 2 columns for the 2 categories. values in the matrix is the probabilities.
  # TODO: thought x$predictions was 3 dimensinal array with tree dimension from the doc and independently running ranger,
  # but looks like it is already averaged? look into it.
  predicted <- factor(x$forest$levels[apply(x$predictions, 1, function(x){
    if(is.na(x[2])){ # take care of the case where x$predictions has only 1 column. possible when there are only one value in training data.
      1
    }
    else {
      if(x[1]>x[2]) 1 else 2
    }
  })], levels=x$forest$levels)
  predicted
}

#' @export
# not really an external function but exposing for sharing with rpart.R TODO: find better way.
evaluate_binary_classification <- function(actual, predicted, predicted_probability, pretty.name = FALSE) {
  # calculate AUC from ROC
  roc_df <- data.frame(actual = (as.integer(actual)==1), predicted_probability = predicted_probability)
  roc <- roc_df %>% do_roc_(actual_val_col = "actual", pred_prob_col = "predicted_probability")
  # use numeric index so that it won't be disturbed by name change
  # 2 should be false positive rate (x axis) and 1 should be true positive rate (yaxis)
  # calculate the area under the plots
  auc <- sum((roc[[2]] - dplyr::lag(roc[[2]])) * roc[[1]], na.rm = TRUE)
  if (is.factor(actual) && "TRUE" %in% levels(actual)) { # target was logical and converted to factor.
    ret <- evaluate_classification(actual, predicted, "TRUE", multi_class = FALSE, pretty.name = pretty.name)
  }
  else {
    ret <- evaluate_multi_(data.frame(predicted=predicted, actual=actual), "predicted", "actual", pretty.name = pretty.name)
  }
  if (pretty.name) {
    ret <- ret %>% mutate(AUC = auc)
  }
  else {
    ret <- ret %>% mutate(auc = auc)
  }
  ret
}

#' @export
#' @param type "importance", "evaluation" or "conf_mat". Feature importance, evaluated scores or confusion matrix of training data.
tidy.ranger <- function(x, type = "importance", pretty.name = FALSE, ...) {
  switch(
    type,
    importance = {
      # return variable importance
      imp <- ranger::importance(x)

      ret <- data.frame(
        variable = x$terms_mapping[names(imp)],
        importance = imp,
        stringsAsFactors = FALSE
      )

      ret
    },
    evaluation = {
      # get evaluation scores from training data
      actual <- x$y

      if(is.numeric(actual)){
        glance(x, pretty.name = pretty.name, ...)
      } else {
        if (x$classification_type == "binary") {
          predicted <- get_binary_predicted_value_from_probability(x)
          predicted_probability <- x$predictions[,1]
          ret <- evaluate_binary_classification(actual, predicted, predicted_probability, pretty.name = pretty.name)
        }
        else {
          predicted <- x$predictions
          ret <- evaluate_multi_(data.frame(predicted=predicted, actual=actual), "predicted", "actual", pretty.name = pretty.name)
        }
        ret
      }
    },
    evaluation_by_class = {
      # get evaluation scores from training data
      actual <- x$y
      if (x$classification_type == "binary") {
        predicted <- get_binary_predicted_value_from_probability(x)
      }
      else {
        predicted <- x$predictions
      }

      per_level <- function(class) {
        ret <- evaluate_classification(actual, predicted, class, pretty.name = pretty.name)
        ret
      }
      dplyr::bind_rows(lapply(levels(actual), per_level))
    },
    conf_mat = {
      # return confusion matrix
      if (x$classification_type == "binary") {
        predicted <- get_binary_predicted_value_from_probability(x)
      }
      else {
        predicted <- x$predictions
      }

      ret <- data.frame(
        actual_value = x$y,
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
    scatter = {
      # return actual and predicted value pairs
      if (x$classification_type == "binary") {
        predicted <- get_binary_predicted_value_from_probability(x)
      }
      else {
        predicted <- x$predictions
      }
      ret <- data.frame(
        expected_value = x$y,
        predicted_value = predicted
      ) %>%
        dplyr::filter(!is.na(predicted_value))

      ret
    },
    partial_dependence = {
      # return partial dependence
      ret <- x$partial_dependence
      var_cols <- colnames(ret)
      var_cols <- var_cols[1:(length(var_cols)-1)] # remove the last column which is the target column in case of regression.
      var_cols <- var_cols[var_cols %in% colnames(x$df)] # to get list of predictor columns, compare with training df.
      for (var_col in var_cols) {
        if (is.numeric(ret[[var_col]])) {
          ret[[var_col]] <- signif(ret[[var_col]], digits=4) # limit digits before we turn it into a factor.
        }
      }
      ret <- ret %>% tidyr::gather_("x_name", "x_value", var_cols, na.rm = TRUE, convert = FALSE)
      # sometimes x_value comes as numeric and not character, and it was causing error from bind_rows internally done
      # in tidy().
      ret <- ret %>% dplyr::mutate(x_value = as.character(x_value))
      # convert must be FALSE for y to make sure y_name is always character. otherwise bind_rows internally done
      # in tidy() to bind outputs from different groups errors out because y_value can be, for example, mixture of logical and character.
      ret <- ret %>% tidyr::gather("y_name", "y_value", -x_name, -x_value, na.rm = TRUE, convert = FALSE)
      ret <- ret %>% dplyr::mutate(x_name = forcats::fct_relevel(x_name, x$imp_vars)) # set factor level order so that charts appear in order of importance.
      # set order to ret and turn it back to character, so that the order is kept when groups are bound.
      # if it were kept as factor, when groups are bound, only the factor order from the first group would be respected.
      ret <- ret %>% dplyr::arrange(x_name) %>% dplyr::mutate(x_name = as.character(x_name))

      # Set original factor level back so that legend order is correct on the chart.
      # In case of logical, c("TRUE","FALSE") is stored in orig_level, so that we can
      # use it here either way.
      if (!is.null(x$orig_levels)) {
        ret <- ret %>%  dplyr::mutate(y_name = factor(y_name, levels=x$orig_levels))
      }

      # create mapping from column name (x_name) to facet chart type based on whether the column is numeric.
      chart_type_map <-c()
      for(col in colnames(x$df)) {
        chart_type_map <- c(chart_type_map, is.numeric(x$df[[col]]))
      }
      chart_type_map <- ifelse(chart_type_map, "line", "scatter")
      names(chart_type_map) <- colnames(x$df)
      
      ret <- ret %>%  dplyr::mutate(chart_type = chart_type_map[x_name])
      ret <- ret %>% dplyr::mutate(x_name = x$terms_mapping[x_name]) # map variable names to original.
      ret
    },
    {
      stop(paste0("type ", type, " is not defined"))
    })
}

# This is used from Analytics View only when classification type is regression.
#' @export
glance.ranger <- function(x, pretty.name = FALSE, ...) {
  ret <- data.frame(
    root_mean_square_error = sqrt(x$prediction.error),
    r_squared = x$r.squared
  )

  if(pretty.name){
    map = list(
      `Root Mean Square Error` = as.symbol("root_mean_square_error"),
      `R Squared` = as.symbol("r_squared")
    )
    ret <- ret %>%
      dplyr::rename(!!!map)
  }

  ret
}

# This is used from Analytics View only when classification type is regression.
#' @export
glance.rpart <- function(x, pretty.name = FALSE, ...) {
  actual <- x$y
  predicted <- predict(x)
  rmse_val <- rmse(actual, predicted)
  r_squared_val <- r_squared(actual, predicted)
  ret <- data.frame(
    root_mean_square_error = rmse_val,
    r_squared = r_squared_val
  )

  if(pretty.name){
    map = list(
      `Root Mean Square Error` = as.symbol("root_mean_square_error"),
      `R Squared` = as.symbol("r_squared")
    )
    ret <- ret %>%
      dplyr::rename(!!!map)
  }
  ret
}


#' @export
exp_rpart <- function(df,
                      target,
                      ...,
                      max_nrow = 50000, # down from 200000 when we added partial dependence
                      target_n = 20,
                      predictor_n = 12, # so that at least months can fit in it.
                      smote = FALSE,
                      seed = NULL
                      ) {
  if(!is.null(seed)){
    set.seed(seed)
  }
  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  target_col <- dplyr::select_var(names(df), !! rlang::enquo(target))
  # this evaluates select arguments like starts_with
  selected_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  clean_ret <- cleanup_df(df, target_col, selected_cols, grouped_cols, target_n, predictor_n, map_name=FALSE)

  clean_df <- clean_ret$clean_df
  name_map <- clean_ret$name_map
  clean_target_col <- clean_ret$clean_target_col
  clean_cols <- clean_ret$clean_cols

  classification_type <- get_classification_type(clean_df[[clean_target_col]])

  each_func <- function(df) {
    tryCatch({
      # especially multiclass classification seems to take forever when number of unique values of predictors are many.
      # fct_lump is essential here.
      # http://grokbase.com/t/r/r-help/051sayg38p/r-multi-class-classification-using-rpart
      clean_df_ret <- cleanup_df_per_group(df, clean_target_col, max_nrow, clean_cols, name_map, predictor_n, revert_logical_levels=FALSE)
      if (is.null(clean_df_ret)) {
        return(NULL) # skip this group
      }
      df <- clean_df_ret$df
      c_cols <- clean_df_ret$c_cols
      name_map <- clean_df_ret$name_map

      # apply smote if this is binary classification
      unique_val <- unique(df[[clean_target_col]])
      if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
        df <- df %>% exp_balance(clean_target_col, sample=FALSE) # since we already sampled, no further sample.
      }
      # if target is categorical (not numeric) and only 1 unique value (or all NA), throw error.
      if ("numeric" %nin% class(clean_target_col) &&
          "integer" %nin% class(clean_target_col) &&
          length(unique_val[!is.na(unique_val)]) <= 1) {
        # We might want to skip and show the status in Summary when we support Repeat By,
        # but for now just throw error to show.
        stop("Categorical Target Variable must have 2 or more unique values.")
      }

      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      fml <- as.formula(paste0("`", clean_target_col, "`", " ~ ", rhs))
      model <- rpart::rpart(fml, df)
      model$classification_type <- classification_type
      if (classification_type %in% c("binary", "multi")) {
        # we need to call this here rather than in tidy(),
        # since there is randomness involved in breaking tie
        # of multiclass classification.
        model$predicted_class <- get_predicted_class_rpart(model)
      }
      model$terms_mapping <- names(name_map)
      names(model$terms_mapping) <- name_map
      model
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

  ret <- do_on_each_group(clean_df, each_func, name = "model", with_unnest = FALSE)
  # add special class .model to pass column type validation at viz layer.
  # also add .model.rpart so that a step created by this function is viewable with Exploratory for debugging.
  class(ret$model) <- c("list", ".model", ".model.rpart")
  ret
}

# with binary probability prediction model from ranger, take the level with bigger probability as the predicted value.
#' @export
# not really an external function but exposing for test. TODO: find better way.
get_binary_predicted_value_from_probability_rpart <- function(x) {
  if (class(x$y) == "logical") {
    # predict(x) is numeric vector of probability of being TRUE.
    ylevels <- c("TRUE", "FALSE")
    predicted <- factor(predict(x) > 0.5, levels=ylevels)
  }
  else {
    # predict(x) is 2-diminsional matrix with 2 columns for the 2 categories. values in the matrix is the probabilities.
    ylevels <- attr(x,"ylevels")
    predicted <- factor(ylevels[apply(predict(x), 1, function(x){if(x[1]>x[2]) 1 else 2})], levels=ylevels)
  }
  predicted
}

get_multiclass_predicted_value_from_probability_rpart <- function(x) {
  ylevels <- attr(x,"ylevels")
  predicted_mat <- predict(x)
  # ties are broken randomly to be even.
  # TODO: move this to model building step so that there is no randomness in analytics viz preprocessor.
  predicted_idx <- max.col(predicted_mat, ties.method = "random")
  predicted <- factor(ylevels[predicted_idx], levels=ylevels)
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

get_predicted_class_rpart <- function(x) {
  if (x$classification_type == "binary") {
    predicted <- get_binary_predicted_value_from_probability_rpart(x)
  }
  else {
    predicted <- get_multiclass_predicted_value_from_probability_rpart(x)
  }
  predicted
}

#' @export
#' @param type "importance", "evaluation" or "conf_mat". Feature importance, evaluated scores or confusion matrix of training data.
tidy.rpart <- function(x, type = "importance", pretty.name = FALSE, ...) {
  switch(
    type,
    importance = {
      # return variable importance

      imp <- x$variable.importance

      ret <- data.frame(
        variable = x$terms_mapping[names(imp)],
        importance = imp,
        stringsAsFactors = FALSE
      )

      ret
    },
    evaluation = {
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
          else {
            predicted_probability <- predict(x)[,1]
          }
          ret <- evaluate_binary_classification(actual, predicted, predicted_probability, pretty.name = pretty.name)
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

      per_level <- function(class) {
        ret <- evaluate_classification(actual, predicted, class, pretty.name = pretty.name)
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
    scatter = { # we assume this is called only for regression.
      predicted <- predict(x)
      ret <- data.frame(
        expected_value = x$y,
        predicted_value = predicted
      ) %>%
        dplyr::filter(!is.na(predicted_value))

      ret
    },
    {
      stop(paste0("type ", type, " is not defined"))
    }
  )
}
