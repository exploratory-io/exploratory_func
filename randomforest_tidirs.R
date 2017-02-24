# these are from https://github.com/mdlincoln/broom/blob/e3cdf5f3363ab9514e5b61a56c6277cb0d9899fd/R/rf_tidiers.R
#' @export
tidy.randomForest <- function(x, ...) {
  tidy.randomForest.method <- switch(x[["type"]],
                                     "classification" = tidy.randomForest.classification,
                                     "regression" = tidy.randomForest.regression,
                                     "unsupervised" = tidy.randomForest.unsupervised)
  tidy.randomForest.method(x, ...)
}

#' @export
tidy.randomForest.formula <- tidy.randomForest

#' @export
tidy.randomForest.classification <- function(x, ...) {
  imp_m <- as.data.frame(x[["importance"]])
  if (ncol(imp_m) > 1)
    names(imp_m) <- c(paste("class", head(names(imp_m), -2), sep = "_"), "MeanDecreaseAccuracy", "MeanDecreaseGini")
  imp_m <- fix_data_frame(imp_m)

  # When run with importance = FALSE, randomForest() does not calculate
  # importanceSD. Issue a warning.
  if (is.null(x[["importanceSD"]])) {
    warning("Only MeanDecreaseGini is available from this model. Run randomforest(..., importance = TRUE) for more detailed results")
    imp_m
  } else {
    imp_sd <- as.data.frame(x[["importanceSD"]])
    names(imp_sd) <- paste("sd", names(imp_sd), sep = "_")

    dplyr::bind_cols(imp_m, imp_sd)
  }
}

#' @export
tidy.randomForest.regression <- function(x, ...) {
  imp_m <- as.data.frame(x[["importance"]])
  imp_m <- fix_data_frame(imp_m)
  imp_sd <- x[["importanceSD"]]

  if (is.null(imp_sd))
    warning("Only IncNodePurity is available from this model. Run randomforest(..., importance = TRUE) for more detailed results")

  imp_m$imp_sd <- imp_sd
  imp_m
}

#' @export
rf_importance <- function(df) {
  tidy(df, model)
}

#' @export
glance.randomForest <- function(x, ...) {

  glance.method <- switch(x[["type"]],
                          "classification" = glance.randomForest.classification,
                          "regression" = glance.randomForest.regression,
                          "unsupervised" = glance.randomForest.unsupervised)

  glance.method(x, ...)
}

#' @export
glance.randomForest.formula <- glance.randomForest

#' @export
glance.randomForest.classification <- function(x, ...) {
  actual <- x[["y"]]
  predicted <- x[["predicted"]]

  per_level <- function(l) {
    tp <- sum(actual == l & predicted == l)
    tn <- sum(actual != l & predicted != l)
    fp <- sum(actual != l & predicted == l)
    fn <- sum(actual == l & predicted != l)

    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    accuracy <- (tp + tn) / (tp + tn + fp + fn)
    f_measure <- 2 * ((precision * recall) / (precision + recall))

    ml <- list(precision, recall, accuracy, f_measure)
    names(ml) <- paste(l, c("precision", "recall", "accuracy", "f_measure"), sep = "_")
    as.data.frame(ml)
  }

  dplyr::bind_cols(lapply(levels(actual), per_level))
}

#' @export
glance.randomForest.regression <- function(x, ...) {
  mean_mse <- mean(x[["mse"]])
  mean_rsq <- mean(x[["rsq"]])
  data.frame(mean_mse = mean_mse, mean_rsq = mean_rsq)
}

#' @export
glance.randomForest.unsupervised <- function(x, ...) {
  stop("glance() is not implemented for unsupervised randomForest models")
}

#' @export
augment.randomForest <- function(x, data = NULL, ...) {

  # Extract data from model
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

#' @export
augment.randomForest.formula <- augment.randomForest

#' @export
augment.randomForest.classification <- function(x, data, ...) {

  # When na.omit is used, case-wise model attributes will only be calculated
  # for complete cases in the original data. All columns returned with
  # augment() must be expanded to the length of the full data, inserting NA
  # for all missing values.

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
  predicted <- factor(predicted, labels = levels(x[["y"]]))

  votes <- x[["votes"]]
  full_votes <- matrix(data = NA, nrow = n_data, ncol = ncol(votes))
  full_votes[which(!na_at),] <- votes
  colnames(full_votes) <- colnames(votes)
  full_votes <- as.data.frame(full_votes)
  names(full_votes) <- paste("votes", names(full_votes), sep = "_")

  local_imp <- x[["localImportance"]]
  full_imp <- NULL

  if (!is.null(local_imp)) {
    full_imp <- matrix(data = NA_real_, nrow = nrow(local_imp), ncol = n_data)
    full_imp[, which(!na_at)] <- local_imp
    rownames(full_imp) <- rownames(local_imp)
    full_imp <- as.data.frame(t(full_imp))
    names(full_imp) <- paste("li", names(full_imp), sep = "_")
  } else {
    warning("casewise importance measures are not available. Run randomForest(..., localImp = TRUE) for more detailed results.")
  }

  d <- data.frame(oob_times = oob_times, fitted = predicted)
  d <- dplyr::bind_cols(d, full_votes, full_imp)
  names(d) <- paste0(".", names(d))
  dplyr::bind_cols(data, d)
}

#' @export
augment.randomForest.regression <- function(x, data, ...) {

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
    names(full_imp) <- paste("li", names(full_imp), sep = "_")
  } else {
    warning("casewise importance measures are not available. Run randomForest(..., localImp = TRUE) for more detailed results.")
  }

  d <- data.frame(oob_times = oob_times, fitted = predicted)
  d <- dplyr::bind_cols(d, full_imp)
  names(d) <- paste0(".", names(d))
  dplyr::bind_cols(data, d)
}

#' @export
augment.randomForest.unsupervised <- function(x, data, ...) {

  # When na.omit is used, case-wise model attributes will only be calculated
  # for complete cases in the original data. All columns returned with
  # augment() must be expanded to the length of the full data, inserting NA
  # for all missing values.

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

#' @export
augment.randomForest <- augment.randomForest.formula
