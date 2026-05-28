#' Prepare a CatBoost predictor frame.
#'
#' @param df Data frame containing predictor columns.
#' @param predictor_cols Predictor column names in model order.
#' @return A list with `data` and zero-based `cat_features`.
catboost_prepare_predictor_frame <- function(df, predictor_cols) {
  predictor_data <- df[, predictor_cols, drop = FALSE]
  cat_features <- integer()

  for (idx in seq_along(predictor_cols)) {
    col <- predictor_cols[[idx]]
    if (is.character(predictor_data[[col]]) || is.factor(predictor_data[[col]]) || is.logical(predictor_data[[col]])) {
      predictor_data[[col]] <- as.factor(predictor_data[[col]])
      cat_features <- c(cat_features, idx - 1L)
    }
  }

  list(
    data = predictor_data,
    cat_features = if (length(cat_features) > 0) as.integer(cat_features) else NULL
  )
}

catboost_resolve_loss_function <- function(classification_type,
                                           output_type_regression,
                                           output_type_binary,
                                           output_type_multiclass) {
  switch(classification_type,
    regression = {
      if (tolower(output_type_regression) == "regression") {
        "RMSE"
      } else {
        output_type_regression
      }
    },
    binary = "Logloss",
    multiclass = "MultiClass",
    multi = "MultiClass",
    "RMSE"
  )
}

catboost_resolve_eval_metric <- function(classification_type,
                                         eval_metric_regression,
                                         eval_metric_binary,
                                         eval_metric_multiclass) {
  switch(classification_type,
    regression = eval_metric_regression,
    binary = eval_metric_binary,
    multiclass = eval_metric_multiclass,
    multi = eval_metric_multiclass,
    eval_metric_regression
  )
}

#' Build CatBoost training parameters.
#'
#' @return A CatBoost params list.
catboost_build_params <- function(classification_type,
                                  iterations,
                                  od_wait,
                                  depth,
                                  learning_rate,
                                  l2_leaf_reg,
                                  border_count,
                                  rsm,
                                  boosting_type,
                                  bootstrap_type,
                                  bagging_temperature,
                                  subsample,
                                  task_type,
                                  devices,
                                  random_strength,
                                  leaf_estimation_method,
                                  leaf_estimation_iterations,
                                  score_function,
                                  output_type_regression,
                                  eval_metric_regression,
                                  output_type_binary,
                                  eval_metric_binary,
                                  output_type_multiclass,
                                  eval_metric_multiclass,
                                  seed) {
  boosting_type <- match.arg(boosting_type, c("Plain", "Ordered"))
  bootstrap_type <- match.arg(bootstrap_type, c("Bayesian", "Bernoulli", "MVS", "Poisson", "No"))
  task_type <- match.arg(task_type, c("CPU", "GPU"))
  leaf_estimation_method <- match.arg(leaf_estimation_method, c("Newton", "Gradient"))
  score_function <- match.arg(score_function, c("Cosine", "L2", "NewtonL2", "NewtonCosine"))

  warning_messages <- character()
  if (identical(boosting_type, "Ordered") && rsm < 1) {
    warning_messages <- c(
      warning_messages,
      "`rsm` is ignored under `boosting_type = 'Ordered'`; forcing rsm = 1."
    )
    rsm <- 1.0
  }

  incompatible_score <- score_function %in% c("NewtonL2", "NewtonCosine") &&
    identical(leaf_estimation_method, "Gradient")
  if (incompatible_score) {
    warning_messages <- c(
      warning_messages,
      paste0(
        "`score_function = '", score_function, "'` requires ",
        "`leaf_estimation_method = 'Newton'`; coercing `score_function` to 'Cosine'."
      )
    )
    score_function <- "Cosine"
  }

  if (length(warning_messages) > 0) {
    warning(paste(warning_messages, collapse = " "), call. = FALSE)
  }

  params <- list(
    iterations = iterations,
    depth = depth,
    learning_rate = learning_rate,
    l2_leaf_reg = l2_leaf_reg,
    border_count = border_count,
    rsm = rsm,
    boosting_type = boosting_type,
    bootstrap_type = bootstrap_type,
    od_type = "Iter",
    od_wait = od_wait,
    random_seed = seed,
    task_type = task_type,
    random_strength = random_strength,
    leaf_estimation_method = leaf_estimation_method,
    score_function = score_function,
    loss_function = catboost_resolve_loss_function(
      classification_type,
      output_type_regression,
      output_type_binary,
      output_type_multiclass
    ),
    eval_metric = catboost_resolve_eval_metric(
      classification_type,
      eval_metric_regression,
      eval_metric_binary,
      eval_metric_multiclass
    )
  )

  if (identical(bootstrap_type, "Bayesian")) {
    params$bagging_temperature <- bagging_temperature
  } else if (bootstrap_type %in% c("Bernoulli", "MVS", "Poisson")) {
    params$subsample <- subsample
  }

  if (!is.null(leaf_estimation_iterations)) {
    params$leaf_estimation_iterations <- leaf_estimation_iterations
  }
  if (identical(task_type, "GPU") && !is.null(devices) && nzchar(devices)) {
    params$devices <- devices
  }

  params
}

catboost_train_with_gpu_guard <- function(learn_pool, test_pool, params) {
  tryCatch(
    catboost::catboost.train(
      learn_pool = learn_pool,
      test_pool = test_pool,
      params = params
    ),
    error = function(e) {
      if (identical(params$task_type, "GPU") &&
          grepl("CUDA|GPU|cuda", conditionMessage(e), ignore.case = TRUE)) {
        stop("EXP_CATBOOST_GPU_UNAVAILABLE: ", conditionMessage(e), call. = FALSE)
      }
      stop(e)
    }
  )
}

catboost_set_default_param <- function(params, name, value) {
  if (is.null(params[[name]])) {
    params[[name]] <- value
  }
  params
}

catboost_update_valid_data_target <- function(valid_data, y_name, convert_fun) {
  if (is.null(valid_data)) {
    return(NULL)
  }
  update_one <- function(df) {
    if (!is.null(df[[y_name]])) {
      df[[y_name]] <- convert_fun(df[[y_name]])
    }
    df
  }
  if (is.data.frame(valid_data)) {
    return(update_one(valid_data))
  }
  if (is.list(valid_data)) {
    return(lapply(valid_data, update_one))
  }
  valid_data
}

catboost_read_metric_file <- function(path, prefix) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }
  ret <- tryCatch(
    utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (is.null(ret) || ncol(ret) == 0) {
    return(NULL)
  }
  colnames(ret)[[1]] <- "iter"
  metric_cols <- setdiff(colnames(ret), "iter")
  colnames(ret)[colnames(ret) %in% metric_cols] <- paste0(prefix, "_", metric_cols)
  ret
}

catboost_build_evaluation_log <- function(model) {
  train_dir <- model$train_dir
  if (is.null(train_dir)) {
    return(NULL)
  }

  learn_log <- catboost_read_metric_file(file.path(train_dir, "learn_error.tsv"), "train")
  test_log <- catboost_read_metric_file(file.path(train_dir, "test_error.tsv"), "validation")

  if (is.null(learn_log) && is.null(test_log)) {
    return(NULL)
  }
  if (is.null(learn_log)) {
    return(test_log)
  }
  if (is.null(test_log)) {
    return(learn_log)
  }

  merge(learn_log, test_log, by = "iter", all = TRUE)
}

#' Formula version of CatBoost.
#'
#' @param data Dataframe to create model.
#' @param formula Formula for model.
#' @param iterations Maximum number of training iterations.
#' @param weights Weight of data for modeling.
#' @param watchlist_rate Ratio of validation data to watch learning process.
#' @param na.action How to handle data with na.
#' @param valid_data Extra validation data to evaluate during training.
#' @param params CatBoost training parameters.
#' @export
fml_catboost <- function(data,
                         formula,
                         iterations = 100,
                         weights = NULL,
                         watchlist_rate = 0,
                         na.action = na.pass,
                         valid_data = NULL,
                         params = list(),
                         ...) {
  loadNamespace("catboost")

  term <- terms(formula, data = data)
  df_for_model <- tryCatch({
    do.call(model.frame, list(term, data = data, weights = substitute(weights), na.action = na.action))
  }, error = function(e) {
    if (e$message == "missing values in object") {
      stop("There are NAs in the training data. You might want to set 'na.action' parameter to 'na.pass' or impute NAs manually.")
    }
    stop(e)
  })

  if (nrow(df_for_model) == 0) {
    stop("No valid data to create catboost model after removing NA.")
  }

  y <- model.response(df_for_model)
  df_for_model <- df_for_model[!is.na(y), , drop = FALSE]
  y <- y[!is.na(y)]

  if (nrow(df_for_model) == 0) {
    stop("No valid data to create catboost model after removing NA in target.")
  }

  y_name <- all.vars(term)[[1]]
  predictor_cols <- setdiff(colnames(df_for_model), y_name)
  prepared <- catboost_prepare_predictor_frame(df_for_model, predictor_cols)

  validation_pool <- NULL
  validation_data <- NULL
  if (!is.null(valid_data)) {
    if (is.data.frame(valid_data)) {
      validation_data <- valid_data
    } else if (is.list(valid_data) && length(valid_data) > 0) {
      validation_data <- valid_data[[1]]
    }
  } else if (watchlist_rate > 0 && nrow(df_for_model) > 1) {
    validation_index <- sample_df_index(df_for_model, rate = watchlist_rate, ordered = FALSE)
    validation_data <- safe_slice(df_for_model, validation_index, remove = FALSE)
    df_for_model <- safe_slice(df_for_model, validation_index, remove = TRUE)
    y <- df_for_model[[y_name]]
    prepared <- catboost_prepare_predictor_frame(df_for_model, predictor_cols)
  }

  learn_pool <- catboost::catboost.load_pool(
    data = prepared$data,
    label = y,
    cat_features = prepared$cat_features
  )

  if (!is.null(validation_data) && nrow(validation_data) > 0) {
    validation_model_frame <- tryCatch({
      model.frame(term, data = validation_data, na.action = na.pass)
    }, error = function(e) validation_data)
    validation_y <- validation_model_frame[[y_name]]
    validation_prepared <- catboost_prepare_predictor_frame(validation_model_frame, predictor_cols)
    validation_pool <- catboost::catboost.load_pool(
      data = validation_prepared$data,
      label = validation_y,
      cat_features = validation_prepared$cat_features
    )
  }

  params <- catboost_set_default_param(params, "iterations", iterations)
  if (is.null(params$train_dir)) {
    train_dir <- tempfile("catboost_train_")
    dir.create(train_dir, recursive = TRUE, showWarnings = FALSE)
    params$train_dir <- train_dir
  } else {
    train_dir <- params$train_dir
  }
  if (is.null(params$logging_level)) {
    params$logging_level <- "Silent"
  }

  booster <- catboost_train_with_gpu_guard(learn_pool, validation_pool, params)

  ret <- list(
    booster = booster,
    terms = term,
    x_names = predictor_cols,
    xlevels = .getXlevels(term, df_for_model),
    df = df_for_model,
    params = params,
    train_pool = learn_pool,
    validation_pool = validation_pool,
    cat_features = prepared$cat_features,
    train_dir = train_dir
  )
  class(ret) <- c("fml_catboost", "catboost_exp")
  ret$evaluation_log <- catboost_build_evaluation_log(ret)
  ret
}

#' Formula version of CatBoost (binary classification).
#' @export
catboost_binary <- function(data, formula, output_type = "probability", eval_metric = "AUC", params = list(), ...) {
  loadNamespace("catboost")

  vars <- all.vars(formula)
  y_name <- vars[[1]]
  y_vals <- data[[y_name]]

  label_levels <- c(0, 1)
  if (is.logical(y_vals)) {
    label_levels <- c(FALSE, TRUE)
    y_vals <- as.numeric(y_vals)
  } else if (!all(y_vals[!is.na(y_vals)] %in% c(0, 1))) {
    factored <- as.factor(data[[y_name]])
    label_levels <- to_same_type(levels(factored), data[[y_name]])
    if (length(label_levels) != 2) {
      stop("target variable must have 2 unique values")
    }
    y_vals <- as.numeric(factored) - 1
  }

  data[[y_name]] <- y_vals
  convert_valid_y <- function(vals) {
    if (is.logical(vals)) {
      return(as.numeric(vals))
    }
    if (all(vals[!is.na(vals)] %in% c(0, 1))) {
      return(vals)
    }
    mapping <- c(0, 1)
    names(mapping) <- as.character(label_levels)
    as.numeric(mapping[as.character(vals)])
  }
  params <- catboost_set_default_param(params, "loss_function", "Logloss")
  params <- catboost_set_default_param(params, "eval_metric", eval_metric)

  dots <- list(...)
  if (!is.null(dots$valid_data)) {
    dots$valid_data <- catboost_update_valid_data_target(dots$valid_data, y_name, convert_valid_y)
  }

  ret <- do.call(fml_catboost, c(list(data = data, formula = formula, params = params), dots))
  class(ret) <- c("catboost_binary", "catboost_exp", setdiff(class(ret), "catboost_exp"))
  ret$y_levels <- label_levels
  ret$output_type <- output_type
  ret$terms_mapping <- stats::setNames(colnames(data), colnames(data))
  ret
}

#' Formula version of CatBoost (multiclass classification).
#' @export
catboost_multi <- function(data, formula, output_type = "softprob", eval_metric = "MultiClass", params = list(), ...) {
  loadNamespace("catboost")

  vars <- all.vars(formula)
  y_name <- vars[[1]]
  y_vals <- data[[y_name]]

  label_levels <- if (is.logical(y_vals)) {
    c(FALSE, TRUE)
  } else if (is.factor(y_vals)) {
    sort(unique(data[[y_name]]))
  } else {
    factored <- as.factor(data[[y_name]])
    to_same_type(levels(factored), data[[y_name]])
  }

  if (is.logical(y_vals)) {
    y_vals <- as.numeric(y_vals)
  } else if (is.factor(y_vals)) {
    mapping <- 0:(length(label_levels) - 1)
    names(mapping) <- as.character(label_levels)
    y_vals <- mapping[as.character(y_vals)]
  } else {
    factored <- as.factor(data[[y_name]])
    y_vals <- as.numeric(factored) - 1
  }

  data[[y_name]] <- y_vals
  convert_valid_y <- function(vals) {
    if (is.logical(vals)) {
      return(as.numeric(vals))
    }
    mapping <- 0:(length(label_levels) - 1)
    names(mapping) <- as.character(label_levels)
    as.numeric(mapping[as.character(vals)])
  }
  params <- catboost_set_default_param(params, "loss_function", "MultiClass")
  params <- catboost_set_default_param(params, "eval_metric", eval_metric)

  dots <- list(...)
  if (!is.null(dots$valid_data)) {
    dots$valid_data <- catboost_update_valid_data_target(dots$valid_data, y_name, convert_valid_y)
  }

  ret <- do.call(fml_catboost, c(list(data = data, formula = formula, params = params), dots))
  class(ret) <- c("catboost_multi", "catboost_exp", setdiff(class(ret), "catboost_exp"))
  ret$y_levels <- label_levels
  ret$output_type <- output_type
  ret$terms_mapping <- stats::setNames(colnames(data), colnames(data))
  ret
}

#' Formula version of CatBoost (regression).
#' @export
catboost_reg <- function(data, formula, output_type = "regression", eval_metric = "RMSE", params = list(), ...) {
  loadNamespace("catboost")

  vars <- all.vars(formula)
  y_name <- vars[[1]]
  data[[y_name]] <- as.numeric(data[[y_name]])

  loss_function <- if (tolower(output_type) == "regression") "RMSE" else output_type
  params <- catboost_set_default_param(params, "loss_function", loss_function)
  params <- catboost_set_default_param(params, "eval_metric", eval_metric)

  dots <- list(...)
  if (!is.null(dots$valid_data)) {
    dots$valid_data <- catboost_update_valid_data_target(dots$valid_data, y_name, as.numeric)
  }

  ret <- do.call(fml_catboost, c(list(data = data, formula = formula, params = params), dots))
  class(ret) <- c("catboost_reg", "catboost_exp", setdiff(class(ret), "catboost_exp"))
  ret$output_type <- output_type
  ret$terms_mapping <- stats::setNames(colnames(data), colnames(data))
  ret
}

catboost_prediction_type <- function(model, type = c("prob", "class", "raw", "response")) {
  type <- match.arg(type)
  if ("catboost_reg" %in% class(model)) {
    return("RawFormulaVal")
  }
  switch(type,
    prob = "Probability",
    class = "Class",
    raw = "RawFormulaVal",
    response = if ("catboost_binary" %in% class(model) || "catboost_multi" %in% class(model)) "Probability" else "RawFormulaVal"
  )
}

predict_catboost <- function(model, df, type = c("prob", "class", "raw", "response")) {
  loadNamespace("catboost")

  type <- match.arg(type)
  y_name <- all.vars(model$terms)[[1]]
  if (is.null(df[[y_name]])) {
    df[[y_name]] <- rep(0, nrow(df))
  }

  model_frame <- model.frame(model$terms, data = df, na.action = na.pass, xlev = model$xlevels)
  predictor_cols <- model$x_names
  prepared <- catboost_prepare_predictor_frame(model_frame, predictor_cols)
  pool <- catboost::catboost.load_pool(
    data = prepared$data,
    cat_features = prepared$cat_features
  )

  pred <- catboost::catboost.predict(
    model = model$booster,
    pool = pool,
    prediction_type = catboost_prediction_type(model, type)
  )

  if ("catboost_binary" %in% class(model) && type %in% c("prob", "response")) {
    if (is.matrix(pred) && ncol(pred) >= 2) {
      pred <- pred[, 2]
    }
  }

  if ("catboost_multi" %in% class(model) && type %in% c("prob", "response")) {
    pred <- as.matrix(pred)
    if (!is.null(model$y_levels) && ncol(pred) == length(model$y_levels)) {
      colnames(pred) <- model$y_levels
    }
  }

  pred
}

#' @export
predict.catboost_exp <- function(object, newdata, type = c("prob", "class", "raw", "response"), ...) {
  predict_catboost(object, newdata, type = type)
}

augment_catboost_newdata <- function(x, newdata, type = c("regression", "binary", "multiclass"), binary_classification_threshold = 0.5) {
  type <- match.arg(type)
  predictor_variables <- all.vars(x$terms)[-1]
  predictor_variables_orig <- x$terms_mapping[predictor_variables]

  if (!is.null(x$predictor_funs)) {
    newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
  }

  original_data <- newdata
  cleaned_data <- original_data %>% dplyr::select(predictor_variables_orig)
  colnames(cleaned_data) <- predictor_variables

  if (!is.null(x$df)) {
    cleaned_data <- align_predictor_factor_levels(cleaned_data, x$df, predictor_variables)
  }

  if (nrow(cleaned_data) == 0) {
    return(data.frame())
  }

  if (type == "regression") {
    predicted_value_col <- avoid_conflict(colnames(original_data), "predicted_value")
    original_data[[predicted_value_col]] <- predict_catboost(x, cleaned_data, type = "response")
  } else if (type == "binary") {
    predicted_value_col <- avoid_conflict(colnames(original_data), "predicted_label")
    predicted_probability_col <- avoid_conflict(colnames(original_data), "predicted_probability")
    predraw <- identical(x$output_type, "rawscore")
    predicted_val <- predict_catboost(x, cleaned_data, type = if (predraw) "raw" else "prob")
    if (predraw) {
      prob <- boot::inv.logit(predicted_val)
      original_data[[predicted_probability_col]] <- prob
      original_data[[predicted_value_col]] <- predicted_val
    } else {
      original_data[[predicted_probability_col]] <- predicted_val
      original_data[[predicted_value_col]] <- predicted_val > binary_classification_threshold
    }
  } else {
    predicted <- predict_catboost(x, cleaned_data, type = "prob")
    colmax <- max.col(predicted)
    max_prob <- predicted[(colmax - 1) * nrow(predicted) + seq(nrow(predicted))]
    predicted_label <- x$y_levels[colmax]
    original_data <- ranger.set_multi_predicted_values(original_data, as.data.frame(predicted), predicted_label, max_prob, c())
  }

  original_data
}

#' @export
augment.catboost_reg <- function(x, data = NULL, newdata = NULL, data_type = "training", ...) {
  if (!is.null(newdata)) {
    return(augment_catboost_newdata(x, newdata, type = "regression"))
  }
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col())
  predicted_value_col <- avoid_conflict(colnames(data), "predicted_value")
  if (data_type == "test") {
    predicted_nona <- extract_predicted(x, type = "test")
    predicted_nona <- restore_na(predicted_nona, attr(x$prediction_test, "unknown_category_rows_index"))
    predicted <- restore_na(predicted_nona, attr(x$prediction_test, "na.action"))
  } else {
    predicted <- extract_predicted(x, type = "training")
  }
  data[[predicted_value_col]] <- predicted
  data
}

#' @export
augment.catboost_binary <- function(x, data = NULL, newdata = NULL, data_type = "training", binary_classification_threshold = 0.5, ...) {
  if (!is.null(newdata)) {
    return(augment_catboost_newdata(x, newdata, type = "binary", binary_classification_threshold = binary_classification_threshold))
  }
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col())
  predicted_value_col <- avoid_conflict(colnames(data), "predicted_label")
  predicted_probability_col <- avoid_conflict(colnames(data), "predicted_probability")
  if (data_type == "test") {
    predicted_prob_nona <- extract_predicted(x, type = "test")
    predicted_value_nona <- extract_predicted_binary_labels(x, type = "test", threshold = binary_classification_threshold)
    predicted_prob_nona <- restore_na(predicted_prob_nona, attr(x$prediction_test, "unknown_category_rows_index"))
    predicted_value_nona <- restore_na(predicted_value_nona, attr(x$prediction_test, "unknown_category_rows_index"))
    predicted_prob <- restore_na(predicted_prob_nona, attr(x$prediction_test, "na.action"))
    predicted_value <- restore_na(predicted_value_nona, attr(x$prediction_test, "na.action"))
  } else {
    predicted_prob <- extract_predicted(x, type = "training")
    predicted_value <- extract_predicted_binary_labels(x, threshold = binary_classification_threshold, type = "training")
  }
  data[[predicted_value_col]] <- predicted_value
  data[[predicted_probability_col]] <- predicted_prob
  data
}

#' @export
augment.catboost_multi <- function(x, data = NULL, newdata = NULL, data_type = "training", ...) {
  if (!is.null(newdata)) {
    return(augment_catboost_newdata(x, newdata, type = "multiclass"))
  }
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  data <- data %>% dplyr::relocate(!!rlang::sym(x$orig_target_col), .after = last_col())
  if (data_type == "test") {
    predicted_value_nona <- extract_predicted_multiclass_labels(x, type = "test")
    predicted_value_nona <- restore_na(predicted_value_nona, attr(x$prediction_test, "unknown_category_rows_index"))
    predicted_value <- restore_na(predicted_value_nona, attr(x$prediction_test, "na.action"))

    predicted <- extract_predicted(x, type = "test")
    colmax <- max.col(predicted)
    max_prob_nona <- predicted[(colmax - 1) * nrow(predicted) + seq(nrow(predicted))]
    max_prob_nona <- restore_na(max_prob_nona, attr(x$prediction_test, "unknown_category_rows_index"))
    max_prob <- restore_na(max_prob_nona, attr(x$prediction_test, "na.action"))
    ranger.set_multi_predicted_values(
      data, as.data.frame(predicted), predicted_value, max_prob,
      attr(x$prediction_test, "na.action"), attr(x$prediction_test, "unknown_category_rows_index")
    )
  } else {
    predicted_value <- extract_predicted_multiclass_labels(x, type = "training")
    predicted <- extract_predicted(x, type = "training")
    colmax <- max.col(predicted)
    max_prob <- predicted[(colmax - 1) * nrow(predicted) + seq(nrow(predicted))]
    ranger.set_multi_predicted_values(data, as.data.frame(predicted), predicted_value, max_prob, c())
  }
}

#' @export
augment.catboost_exp <- function(x, data = NULL, newdata = NULL, ...) {
  if ("catboost_reg" %in% class(x)) {
    augment.catboost_reg(x, data, newdata, ...)
  } else if ("catboost_binary" %in% class(x)) {
    augment.catboost_binary(x, data, newdata, ...)
  } else if ("catboost_multi" %in% class(x)) {
    augment.catboost_multi(x, data, newdata, ...)
  }
}

importance_catboost <- function(model, type = "PredictionValuesChange") {
  loadNamespace("catboost")

  imp <- tryCatch({
    catboost::catboost.get_feature_importance(
      model = model$booster,
      pool = model$train_pool,
      type = type
    )
  }, error = function(e) {
    return(simpleError(e$message))
  })
  if ("error" %in% class(imp)) {
    return(imp)
  }

  ret <- tibble::tibble(
    variable = model$x_names,
    importance = as.numeric(imp),
    importance_type = type
  )
  ret <- ret %>% dplyr::arrange(-importance)
  ret
}

calc_permutation_importance_catboost_regression <- function(fit, target, vars, data) {
  if (!requireNamespace("mmpf", quietly = TRUE)) {
    return(simpleError("Package 'mmpf' is not available. Permutation importance cannot be calculated."))
  }
  importances <- purrr::map(as.list(vars), function(var) {
    mmpf::permutationImportance(
      data, var, target, fit, nperm = 1,
      predict.fun = function(object, newdata) { predict_catboost(object, newdata) },
      loss.fun = function(x, y) { sum((x - y) ^ 2, na.rm = TRUE) / length(x) }
    )
  })
  importances <- purrr::flatten_dbl(importances)
  tibble::tibble(variable = vars, importance = pmax(importances, 0), importance_type = "permutation") %>%
    dplyr::arrange(-importance)
}

calc_permutation_importance_catboost_binary <- function(fit, target, vars, data) {
  if (!requireNamespace("mmpf", quietly = TRUE)) {
    return(simpleError("Package 'mmpf' is not available. Permutation importance cannot be calculated."))
  }
  importances <- purrr::map(as.list(vars), function(var) {
    mmpf::permutationImportance(
      data, var, target, fit, nperm = 1,
      predict.fun = function(object, newdata) { predict_catboost(object, newdata) },
      loss.fun = function(x, y) { -sum(log(1 - abs(x - y[[1]])), na.rm = TRUE) }
    )
  })
  importances <- purrr::flatten_dbl(importances)
  tibble::tibble(variable = vars, importance = pmax(importances, 0), importance_type = "permutation") %>%
    dplyr::arrange(-importance)
}

calc_permutation_importance_catboost_multiclass <- function(fit, target, vars, data) {
  if (!requireNamespace("mmpf", quietly = TRUE)) {
    return(simpleError("Package 'mmpf' is not available. Permutation importance cannot be calculated."))
  }
  importances <- purrr::map(as.list(vars), function(var) {
    mmpf::permutationImportance(
      data, var, target, fit, nperm = 1,
      predict.fun = function(object, newdata) { predict_catboost(object, newdata) },
      loss.fun = function(x, y) {
        sum(-log(x[match(y[[1]][row(x)], colnames(x)) == col(x)]), na.rm = TRUE)
      }
    )
  })
  importances <- purrr::flatten_dbl(importances)
  tibble::tibble(variable = vars, importance = pmax(importances, 0), importance_type = "permutation") %>%
    dplyr::arrange(-importance)
}

partial_dependence.catboost <- function(fit, vars = colnames(data),
                                        n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)),
                                        classification = FALSE, interaction = FALSE, uniform = TRUE, data, ...) {
  if (!requireNamespace("mmpf", quietly = TRUE)) {
    return(NULL)
  }

  target <- all.vars(fit$terms)[[1]]
  predict.fun <- function(object, newdata) {
    predict_catboost(object, newdata)
  }

  points <- list()
  quantile_points <- list()
  for (cname in vars) {
    if (is.numeric(data[[cname]])) {
      coldata <- data[[cname]]
      minv <- min(coldata, na.rm = TRUE)
      maxv <- max(coldata, na.rm = TRUE)
      grid <- minv + (0:20) / 20 * (maxv - minv)
      quantile_grid <- stats::quantile(coldata, probs = 1:24 / 25)
      quantile_points[[cname]] <- quantile_grid
      points[[cname]] <- sort(unique(c(grid, quantile_grid)))
    } else {
      points[[cname]] <- unique(data[[cname]])
    }
  }

  args <- list(
    data = data,
    vars = vars,
    n = n,
    model = fit,
    points = points,
    predict.fun = predict.fun,
    ...
  )

  if (length(vars) > 1L & !interaction) {
    pd <- data.table::rbindlist(sapply(vars, function(x) {
      args$vars <- x
      if ("points" %in% names(args)) args$points <- args$points[x]
      mp <- do.call(mmpf::marginalPrediction, args)
      if (!classification) names(mp)[ncol(mp)] <- target
      mp
    }, simplify = FALSE), fill = TRUE)
    data.table::setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else {
    pd <- do.call(mmpf::marginalPrediction, args)
    if (!classification) names(pd)[ncol(pd)] <- target
  }

  attr(pd, "class") <- c("pd", "data.frame")
  attr(pd, "interaction") <- interaction == TRUE
  attr(pd, "target") <- if (!classification) target else as.character(fit$y_levels)
  attr(pd, "vars") <- vars
  attr(pd, "points") <- points
  attr(pd, "quantile_points") <- quantile_points
  pd
}

#' Build CatBoost model for Analytics View.
#' @export
exp_catboost <- function(df,
                         target,
                         ...,
                         target_fun = NULL,
                         predictor_funs = NULL,
                         max_nrow = 50000,
                         iterations = 100,
                         od_wait = 20,
                         depth = 6,
                         learning_rate = 0.03,
                         l2_leaf_reg = 3,
                         border_count = 254,
                         rsm = 1.0,
                         boosting_type = "Plain",
                         bootstrap_type = "Bayesian",
                         bagging_temperature = 1.0,
                         subsample = 0.8,
                         task_type = "CPU",
                         devices = NULL,
                         random_strength = 1,
                         leaf_estimation_method = "Newton",
                         leaf_estimation_iterations = NULL,
                         score_function = "Cosine",
                         output_type_regression = "regression",
                         eval_metric_regression = "RMSE",
                         output_type_binary = "probability",
                         eval_metric_binary = "AUC",
                         output_type_multiclass = "softprob",
                         eval_metric_multiclass = "MultiClass",
                         target_n = 20,
                         predictor_n = 12,
                         smote = FALSE,
                         smote_target_minority_perc = 40,
                         smote_max_synth_perc = 200,
                         smote_k = 5,
                         smote_keep_synthetic = TRUE,
                         importance_measure = "permutation",
                         max_pd_vars = NULL,
                         pd_sample_size = 500,
                         pd_grid_resolution = 20,
                         pd_with_bin_means = TRUE,
                         seed = 1,
                         test_rate = 0.0,
                         test_split_type = "random",
                         watchlist_rate = 0) {
  loadNamespace("catboost")

  if (test_rate < 0 | 1 < test_rate) {
    stop("test_rate must be between 0 and 1")
  } else if (test_rate == 1) {
    stop("test_rate must be less than 1")
  }

  target_col <- tidyselect::vars_select(names(df), !!rlang::enquo(target))
  orig_selected_cols <- tidyselect::vars_select(names(df), !!!rlang::quos(...))

  target_funs <- NULL
  if (!is.null(target_fun)) {
    target_funs <- list(target_fun)
    names(target_funs) <- target_col
    df <- df %>% mutate_predictors(target_col, target_funs)
  }

  if (!is.null(predictor_funs)) {
    df <- df %>% mutate_predictors(orig_selected_cols, predictor_funs)
    selected_cols <- names(unlist(predictor_funs))
  } else {
    selected_cols <- orig_selected_cols
  }

  grouped_cols <- grouped_by(df)
  selected_cols <- stringr::str_sort(selected_cols)

  is_target_numeric <- is.numeric(df[[target_col]])
  is_target_logical <- is.logical(df[[target_col]])

  orig_levels <- NULL
  if (is.factor(df[[target_col]])) {
    orig_levels <- levels(df[[target_col]])
  } else if (is.logical(df[[target_col]])) {
    orig_levels <- c("TRUE", "FALSE")
  }

  clean_ret <- cleanup_df(df, target_col, selected_cols, grouped_cols, target_n, predictor_n)
  clean_df <- clean_ret$clean_df
  name_map <- clean_ret$name_map
  clean_target_col <- clean_ret$clean_target_col
  clean_cols <- clean_ret$clean_cols
  classification_type <- get_classification_type(clean_df[[clean_target_col]])

  each_func <- function(df) {
    tryCatch({
      if (!is.null(seed)) {
        set.seed(seed)
      }

      unique_val <- unique(df[[clean_target_col]])
      sample_size <- if (smote && length(unique_val[!is.na(unique_val)]) == 2) NULL else max_nrow
      orig_predictor_classes <- capture_df_column_classes(df, clean_cols)

      numeric_pred_cols <- clean_cols[sapply(clean_cols, function(col) is.numeric(df[[col]]))]
      if (length(numeric_pred_cols) > 0) {
        inf_flags <- lapply(numeric_pred_cols, function(col) is.infinite(df[[col]]))
        rows_with_inf <- rowSums(do.call(cbind, inf_flags)) > 0
        inf_removed_rows <- sum(rows_with_inf)
      } else {
        inf_removed_rows <- 0
      }
      if (inf_removed_rows == nrow(df)) {
        stop("All rows were removed due to Inf values in predictors. Please check your data.")
      }

      clean_df_ret <- cleanup_df_per_group(
        df, clean_target_col, sample_size, clean_cols, name_map, predictor_n,
        filter_numeric_na = FALSE, convert_logical = FALSE
      )
      if (is.null(clean_df_ret)) {
        return(NULL)
      }

      df <- clean_df_ret$df
      c_cols <- clean_df_ret$c_cols
      if (length(c_cols) == 0) {
        stop("Invalid Predictors: Only one unique value.")
      }
      name_map <- clean_df_ret$name_map
      inf_removed_message <- if (inf_removed_rows > 0) {
        paste0("Note: ", inf_removed_rows, " row(s) with Inf values in predictors were automatically removed.")
      } else {
        NULL
      }

      source_data <- df
      test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
      df <- safe_slice(source_data, test_index, remove = TRUE)

      df_test_temp <- NULL
      if (test_rate > 0) {
        df_test_temp <- safe_slice(source_data, test_index, remove = FALSE)
      }
      df_train_original <- df

      unique_val <- unique(df[[clean_target_col]])
      smote_applied <- FALSE
      if (smote && length(unique_val[!is.na(unique_val)]) == 2) {
        df <- df %>% exp_balance(clean_target_col, target_size = max_nrow, target_minority_perc = smote_target_minority_perc, max_synth_perc = smote_max_synth_perc, k = smote_k)
        smote_applied <- "synthesized" %in% colnames(df)
        if (smote_keep_synthetic && smote_applied) {
          if (test_rate > 0) {
            df_test_temp$synthesized <- FALSE
            source_data <- bind_rows(df, df_test_temp)
            test_index <- (nrow(df) + 1):nrow(source_data)
          } else {
            source_data <- df
          }
        } else if (smote_applied && "synthesized" %in% names(df)) {
          df <- df %>% dplyr::select(-synthesized)
        }
      }

      df_test <- NULL
      df_test_clean <- NULL
      na_row_numbers_test <- NULL
      unknown_category_rows_index <- NULL
      valid_data <- NULL
      if (test_rate > 0) {
        df_test <- safe_slice(source_data, test_index, remove = FALSE)
        df_test_clean <- cleanup_df_for_test(df_test, df, c_cols)
        na_row_numbers_test <- attr(df_test_clean, "na_row_numbers")
        unknown_category_rows_index <- attr(df_test_clean, "unknown_category_rows_index")
        if (watchlist_rate == 0 && !is.null(df_test_clean) && nrow(df_test_clean) > 0) {
          valid_data <- list(test = df_test_clean)
        }
      }

      rev_name_map <- names(name_map)
      names(rev_name_map) <- name_map
      new_names <- rev_name_map[colnames(source_data)]
      colnames(source_data) <- ifelse(is.na(new_names), colnames(source_data), new_names)

      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      fml <- as.formula(paste(clean_target_col, " ~ ", rhs))

      params <- catboost_build_params(
        classification_type = if (is_target_logical) "binary" else if (is_target_numeric) "regression" else "multiclass",
        iterations = iterations,
        od_wait = od_wait,
        depth = depth,
        learning_rate = learning_rate,
        l2_leaf_reg = l2_leaf_reg,
        border_count = border_count,
        rsm = rsm,
        boosting_type = boosting_type,
        bootstrap_type = bootstrap_type,
        bagging_temperature = bagging_temperature,
        subsample = subsample,
        task_type = task_type,
        devices = devices,
        random_strength = random_strength,
        leaf_estimation_method = leaf_estimation_method,
        leaf_estimation_iterations = leaf_estimation_iterations,
        score_function = score_function,
        output_type_regression = output_type_regression,
        eval_metric_regression = eval_metric_regression,
        output_type_binary = output_type_binary,
        eval_metric_binary = eval_metric_binary,
        output_type_multiclass = output_type_multiclass,
        eval_metric_multiclass = eval_metric_multiclass,
        seed = seed
      )

      if (is_target_logical) {
        model <- catboost_binary(
          df, fml,
          iterations = iterations,
          watchlist_rate = watchlist_rate,
          output_type = output_type_binary,
          eval_metric = eval_metric_binary,
          params = params,
          valid_data = valid_data
        )
      } else if (is_target_numeric) {
        model <- catboost_reg(
          df, fml,
          iterations = iterations,
          watchlist_rate = watchlist_rate,
          output_type = output_type_regression,
          eval_metric = eval_metric_regression,
          params = params,
          valid_data = valid_data
        )
      } else {
        model <- catboost_multi(
          df, fml,
          iterations = iterations,
          watchlist_rate = watchlist_rate,
          output_type = output_type_multiclass,
          eval_metric = eval_metric_multiclass,
          params = params,
          valid_data = valid_data
        )
      }

      model$evaluation_log <- catboost_build_evaluation_log(model)
      model$prediction_training <- if (smote_applied && !smote_keep_synthetic) {
        predict_catboost(model, df_train_original)
      } else {
        predict_catboost(model, df)
      }

      if (test_rate > 0) {
        if (is.null(df_test_clean) || nrow(df_test_clean) == 0) {
          df_test_clean <- if (!is.null(df_test) && is.data.frame(df_test)) df_test[0, , drop = FALSE] else data.frame()
        }
        prediction_test <- predict_catboost(model, df_test_clean)
        attr(prediction_test, "na.action") <- na_row_numbers_test
        attr(prediction_test, "unknown_category_rows_index") <- unknown_category_rows_index
        model$prediction_test <- prediction_test
        model$df_test <- df_test_clean
      }

      if (is.null(max_pd_vars)) {
        max_pd_vars <- 20
      }
      max_pd_vars <- suppressWarnings(as.integer(max_pd_vars))
      if (is.na(max_pd_vars) || max_pd_vars < 0) {
        max_pd_vars <- 0
      }

      if (length(c_cols) > 1) {
        if (importance_measure == "permutation") {
          model$imp_df <- if (is_target_logical) {
            calc_permutation_importance_catboost_binary(model, clean_target_col, c_cols, df)
          } else if (is_target_numeric) {
            calc_permutation_importance_catboost_regression(model, clean_target_col, c_cols, df)
          } else {
            calc_permutation_importance_catboost_multiclass(model, clean_target_col, c_cols, df)
          }
        } else if (importance_measure == "firm") {
          model$imp_df <- NULL
        } else {
          native_type <- switch(importance_measure,
            catboost_prediction_values_change = "PredictionValuesChange",
            catboost_loss_function_change = "LossFunctionChange",
            "PredictionValuesChange"
          )
          model$imp_df <- importance_catboost(model, native_type)
        }

        if (importance_measure == "firm") {
          imp_vars <- c_cols
        } else if ("error" %in% class(model$imp_df)) {
          imp_vars <- c_cols[seq_len(min(length(c_cols), max_pd_vars))]
        } else {
          imp_vars <- model$imp_df$variable
          imp_vars <- imp_vars[seq_len(min(length(imp_vars), max_pd_vars))]
        }
      } else {
        model$imp_df <- simpleError("Variable importance requires two or more variables.")
        imp_vars <- c_cols
      }

      imp_vars <- as.character(imp_vars)
      model$imp_vars <- imp_vars
      if (length(imp_vars) > 0) {
        model$partial_dependence <- partial_dependence.catboost(
          model, vars = imp_vars, data = df,
          n = c(pd_grid_resolution, min(nrow(df), pd_sample_size)),
          classification = !(is_target_numeric || is_target_logical)
        )
      } else {
        model$partial_dependence <- NULL
      }

      if (importance_measure == "firm") {
        if (length(c_cols) > 1) {
          if (is.null(model$partial_dependence)) {
            model$imp_df <- simpleError("Package 'mmpf' is not available. FIRM importance cannot be calculated.")
          } else {
            pdp_target_col <- attr(model$partial_dependence, "target")
            model$imp_df <- importance_firm(model$partial_dependence, pdp_target_col, imp_vars)
            imp_vars <- model$imp_df$variable
            imp_vars <- imp_vars[seq_len(min(length(imp_vars), max_pd_vars))]
            model$imp_vars <- imp_vars
            model$partial_dependence <- shrink_partial_dependence_data(model$partial_dependence, imp_vars)
          }
        } else {
          model$imp_df <- simpleError("Variable importance requires two or more variables.")
          imp_vars <- c_cols
        }
      }

      if (length(imp_vars) > 0 && pd_with_bin_means && (is_target_logical || is_target_numeric)) {
        model$partial_binning <- calc_partial_binning_data(df, clean_target_col, imp_vars)
      }

      model$classification_type <- classification_type
      model$orig_levels <- orig_levels
      model$terms_mapping <- names(name_map)
      names(model$terms_mapping) <- name_map
      model$df <- df
      model$formula_terms <- terms(fml)
      attr(model$formula_terms, ".Environment") <- NULL
      model$sampled_nrow <- clean_df_ret$sampled_nrow
      model$orig_target_col <- target_col
      model$call_info <- list(categorical_features = length(model$cat_features))

      if (!is.null(target_funs)) {
        model$target_funs <- target_funs
      }
      if (!is.null(predictor_funs)) {
        model$orig_predictor_cols <- orig_selected_cols
        attr(predictor_funs, "LC_TIME") <- Sys.getlocale("LC_TIME")
        attr(predictor_funs, "sysname") <- Sys.info()[["sysname"]]
        attr(predictor_funs, "lubridate.week.start") <- getOption("lubridate.week.start")
        model$predictor_funs <- predictor_funs
      }
      model$orig_predictor_classes <- orig_predictor_classes

      if (!is.null(inf_removed_message)) {
        model$inf_removed_rows <- inf_removed_rows
        model$inf_removed_message <- inf_removed_message
      }

      list(model = model, test_index = test_index, source_data = source_data)
    }, error = function(e) {
      if (length(grouped_cols) > 0) {
        class(e) <- c("catboost_exp", class(e))
        list(model = e, test_index = NULL, source_data = NULL)
      } else {
        stop(e)
      }
    })
  }

  model_and_data_col <- "model_and_data"
  ret <- do_on_each_group(clean_df, each_func, name = model_and_data_col, with_unnest = FALSE)
  if (length(grouped_cols) > 0) {
    ret <- ret %>% tidyr::nest(-grouped_cols)
  } else {
    ret <- ret %>% tidyr::nest()
  }

  ret <- ret %>% dplyr::ungroup()
  ret <- ret %>%
    dplyr::mutate(model = purrr::imap(data, function(df, idx) {
      tryCatch(df[[model_and_data_col]][[1]]$model, error = function(e) {
        stop(paste0(e$message, " (while extracting model from group ", idx, ")"), call. = FALSE)
      })
    })) %>%
    dplyr::mutate(.test_index = purrr::imap(data, function(df, idx) {
      tryCatch(df[[model_and_data_col]][[1]]$test_index, error = function(e) {
        stop(paste0(e$message, " (while extracting test_index from group ", idx, ")"), call. = FALSE)
      })
    })) %>%
    dplyr::mutate(source.data = purrr::imap(data, function(df, idx) {
      tryCatch({
        data <- df[[model_and_data_col]][[1]]$source_data
        if (length(grouped_cols) > 0 && !is.null(data)) data %>% dplyr::select(-grouped_cols) else data
      }, error = function(e) {
        stop(paste0(e$message, " (while extracting source.data from group ", idx, ")"), call. = FALSE)
      })
    })) %>%
    dplyr::select(-data)

  if (length(grouped_cols) > 0) {
    ret <- ret %>% dplyr::rowwise(grouped_cols)
  } else {
    ret <- ret %>% dplyr::rowwise()
  }

  if (purrr::every(ret$model, function(x) { "error" %in% class(x) })) {
    stop(ret$model[[1]])
  }

  ret
}

#' @export
glance.catboost_exp <- function(x, pretty.name = FALSE, ...) {
  if ("error" %in% class(x)) {
    return(data.frame(Note = x$message))
  }
  if (!("catboost_reg" %in% class(x))) {
    stop("glance.catboost_exp should not be called for classification")
  }
  predicted <- extract_predicted(x)
  actual <- extract_actual(x)
  root_mean_square_error <- rmse(predicted, actual)
  rsq <- r_squared(actual, predicted)
  n <- length(actual)
  ret <- data.frame(r_squared = rsq, root_mean_square_error = root_mean_square_error, n = n)
  if (pretty.name) {
    map <- list(`R Squared` = as.symbol("r_squared"), `RMSE` = as.symbol("root_mean_square_error"), `Rows` = as.symbol("n"))
    ret <- ret %>% dplyr::rename(!!!map)
  }
  if (!is.null(x$inf_removed_rows) && x$inf_removed_rows > 0) {
    ret$Note <- x$inf_removed_message
  }
  ret
}

catboost_prediction_frame <- function(x, type = "training", binary_classification_threshold = 0.5) {
  actual <- extract_actual(x, type = type)
  is_test_data <- identical(type, "test")

  if ("catboost_reg" %in% class(x)) {
    return(data.frame(
      predicted_value = extract_predicted(x, type = type),
      actual_value = actual,
      is_test_data = is_test_data
    ))
  }

  if ("catboost_binary" %in% class(x)) {
    return(data.frame(
      predicted_value = extract_predicted_binary_labels(x, threshold = binary_classification_threshold, type = type),
      predicted_probability = extract_predicted(x, type = type),
      actual_value = actual,
      is_test_data = is_test_data
    ))
  }

  predicted <- extract_predicted(x, type = type)
  colmax <- max.col(predicted)
  data.frame(
    predicted_value = extract_predicted_multiclass_labels(x, type = type),
    predicted_probability = predicted[(colmax - 1) * nrow(predicted) + seq(nrow(predicted))],
    actual_value = actual,
    is_test_data = is_test_data
  )
}

catboost_prediction_training_and_test <- function(x, binary_classification_threshold = 0.5) {
  ret <- catboost_prediction_frame(x, type = "training", binary_classification_threshold = binary_classification_threshold)
  if (!is.null(x$prediction_test) && !is.null(x$df_test)) {
    ret <- dplyr::bind_rows(
      ret,
      catboost_prediction_frame(x, type = "test", binary_classification_threshold = binary_classification_threshold)
    )
  }
  ret
}

#' @export
#' @param type "importance", "evaluation", "conf_mat", "partial_dependence", "partial_binning", "evaluation_log"
tidy.catboost_exp <- function(x, type = "importance", pretty.name = FALSE, binary_classification_threshold = 0.5, ...) {
  if ("error" %in% class(x) && type != "evaluation") {
    return(data.frame())
  }
  switch(type,
    importance = {
      if ("error" %in% class(x$imp_df)) {
        return(data.frame(variable = character(), importance = numeric(), importance_type = character()))
      }
      ret <- x$imp_df
      ret <- ret %>% dplyr::mutate(variable = x$terms_mapping[variable])
      ret
    },
    evaluation = {
      if ("error" %in% class(x)) {
        return(glance(x, pretty.name = pretty.name, ...))
      }
      actual <- extract_actual(x)
      if (is.numeric(actual)) {
        ret <- glance(x, pretty.name = pretty.name, ...)
      } else if (x$classification_type == "binary") {
        predicted <- extract_predicted_binary_labels(x, threshold = binary_classification_threshold)
        predicted_probability <- extract_predicted(x)
        ret <- evaluate_binary_classification(actual, predicted, predicted_probability, pretty.name = pretty.name)
      } else {
        predicted <- extract_predicted_multiclass_labels(x)
        ret <- evaluate_multi_(data.frame(predicted = predicted, actual = actual), "predicted", "actual", pretty.name = pretty.name)
      }
      if (!is.null(x$inf_removed_rows) && x$inf_removed_rows > 0) {
        ret$Note <- if ("Note" %in% colnames(ret)) paste(ret$Note, x$inf_removed_message, sep = " ") else x$inf_removed_message
      }
      ret
    },
    evaluation_by_class = ,
    summary_table_by_class = {
      if ("error" %in% class(x)) {
        return(glance(x, pretty.name = pretty.name, ...))
      }
      actual <- extract_actual(x)
      predicted <- extract_predicted_multiclass_labels(x)
      per_level <- function(level) {
        evaluate_classification(actual, predicted, level, pretty.name = pretty.name)
      }
      dplyr::bind_rows(lapply(levels(actual), per_level))
    },
    conf_mat = ,
    confusion_matrix = {
      actual <- extract_actual(x)
      if (x$classification_type == "binary") {
        predicted <- extract_predicted_binary_labels(x, threshold = binary_classification_threshold)
      } else {
        predicted <- extract_predicted_multiclass_labels(x)
      }
      calc_conf_mat(actual, predicted)
    },
    partial_dependence = {
      handle_partial_dependence(x)
    },
    partial_binning = {
      if (is.null(x$partial_binning)) data.frame() else x$partial_binning
    },
    prediction_training_and_test = {
      catboost_prediction_training_and_test(x, binary_classification_threshold = binary_classification_threshold)
    },
    roc = {
      actual <- extract_actual(x)
      predicted_probability <- extract_predicted(x)
      do_roc_(data.frame(actual = actual, predicted_probability = predicted_probability), "actual", "predicted_probability")
    },
    summary_table = {
      tidy.catboost_exp(x, type = "evaluation", pretty.name = pretty.name, binary_classification_threshold = binary_classification_threshold, ...)
    },
    evaluation_log = {
      if (is.null(x$evaluation_log)) {
        return(data.frame())
      }
      ret <- x$evaluation_log %>% as.data.frame()
      ret <- ret %>% tidyr::pivot_longer(cols = c(-iter))
      ret <- ret %>% tidyr::separate(col = "name", into = c("type", "name"), sep = "_", extra = "merge", fill = "right")
      colnames(ret)[colnames(ret) == "iter"] <- "Iter"
      ret
    },
    {
      stop(paste0("type ", type, " is not defined"))
    }
  )
}

extract_actual.catboost_exp <- function(x, type = "training") {
  if (type == "training") {
    x$df[[all.vars(x$terms)[[1]]]]
  } else {
    x$df_test[[all.vars(x$terms)[[1]]]]
  }
}

extract_predicted.catboost_exp <- function(x, type = "training") {
  if (type == "training") x$prediction_training else x$prediction_test
}

extract_predicted.catboost_reg <- extract_predicted.catboost_exp
extract_predicted.catboost_binary <- extract_predicted.catboost_exp
extract_predicted.catboost_multi <- extract_predicted.catboost_exp

extract_predicted_binary_labels.catboost_exp <- function(x, threshold = 0.5, type = "training") {
  if (type == "training") (x$prediction_training > threshold) else (x$prediction_test > threshold)
}
extract_predicted_binary_labels.catboost_binary <- extract_predicted_binary_labels.catboost_exp

extract_predicted_multiclass_labels.catboost_exp <- function(x, type = "training") {
  pred <- if (type == "training") x$prediction_training else x$prediction_test
  x$y_levels[apply(pred, 1, which.max)]
}
extract_predicted_multiclass_labels.catboost_multi <- extract_predicted_multiclass_labels.catboost_exp

get_prediction_type.catboost_exp <- function(x) {
  if ("catboost_reg" %in% class(x)) {
    return("regression")
  }
  if (x$classification_type == "binary") "binary" else "multiclass"
}
