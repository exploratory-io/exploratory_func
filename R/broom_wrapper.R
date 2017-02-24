#' glance for lm
#' @export
glance_lm <- broom::glance

#' glance for glm
#' @export
glance_glm <- broom::glance

#' glance for kmeans
#' @export
glance_kmeans <- broom::glance

#' tidy for lm
#' @export
tidy_lm <- broom::tidy
#' tidy for glm
#' @export
tidy_glm <- broom::tidy
#' tidy for kmeans
#' @export
tidy_kmeans <- broom::tidy

#' augment for lm
#' @export
augment_lm <- broom::augment
#' augment for glm
#' @export
augment_glm <- broom::augment
#' augment for kmeans
#' @export
augment_kmeans <- function(df, model, data){
  model_col <- col_name(substitute(model))
  data_col <- col_name(substitute(data))
  if(!model_col %in% colnames(df)){
    stop(paste(model_col, "is not in column names"), sep=" ")
  }
  if(!data_col %in% colnames(df)){
    stop(paste(data_col, "is not in column names"), sep=" ")
  }
  ret <- tryCatch({
    # use do.call to evaluate data_col from a variable
    augment_func <- get("augment", asNamespace("broom"))
    ret <- do.call(augment_func, list(df, model_col, data=data_col))
    # cluster column is factor labeled "1", "2"..., so convert it to integer to avoid confusion
    ret[[ncol(ret)]] <- as.integer(ret[[ncol(ret)]])
    ret
  },
  error = function(e){
    loadNamespace("dplyr")

    if(grepl("arguments imply differing number of rows",e$message)){
      # bind .cluster column refering subject names
      grouped_col <- grouped_by(df)
      cluster_col <- avoid_conflict(grouped_col, ".cluster")

      augment_each <- function(df_each){
        source_data <- df_each[[data_col]]
        kmeans_obj <- df_each[[model_col]]
        if(!is.data.frame(source_data)){
          source_data <- source_data[[1]]
          kmeans_obj <- kmeans_obj[[1]]
        }

        subject_colname <- attr(kmeans_obj, "subject_colname")
        index <- as.factor(source_data[[subject_colname]])
        source_data[[cluster_col]] <- kmeans_obj$cluster[index]
        source_data
      }

      (df %>%  dplyr::do_(.dots=setNames(list(~augment_each(.)), model_col)) %>%  tidyr::unnest_(model_col))
    } else {
      stop(e)
    }
  })
  # update .cluster to cluster or cluster.new if it exists
  colnames(ret)[[ncol(ret)]] <- avoid_conflict(colnames(ret), "cluster")
  ret
}

#' apply data frame with model to a data frame
#' @param df Data frame to predict
#' @param model_df Data frame that has model
#' @param ... Additional argument to be passed to broom::augment
#' @export
add_prediction <- function(df, model_df, ...){
  # parsing arguments of add_prediction and getting optional arguemnt for augment in ...
  cll <- match.call()
  aug_args <- expand_args(cll, exclude = c("df", "model_df"))

  get_result_with_response <- function(model_df, df, aug_args){
    # Use formula to support expanded aug_args (especially for type.predict for logistic regression)
    # because ... can't be passed to a function inside mutate directly.
    aug_fml <- if(aug_args == ""){
      as.formula("~list(broom::augment(model, newdata = df))")
    } else {
      as.formula(paste0("~list(broom::augment(model, newdata = df, ", aug_args, "))"))
    }
    model_df %>%
      # result of aug_fml will be stored in .text_index column
      # .test_index is used because model_df has it and won't be used here
      dplyr::mutate_(.dots = list(.test_index = aug_fml)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(.test_index = purrr::map2(.test_index, model, function(d, m){
        # add predicted_response to the result data frame
        add_response(d, m, "predicted_response")
      })) %>%
      tidyr::unnest(.test_index)
  }

  # if type.predict argument is not indicated in this function
  # and models have $family$linkinv (basically, glm models have it),
  # both fitted link value column and response value column should appear in the result
  with_response <- !("type.predict" %in% names(cll)) & !is.null(model_df[["model"]][[1]]$family) & !is.null(model_df[["model"]][[1]]$family$linkinv)

  ret <- tryCatch({
    if(with_response){
      # this function is defined beforehand to avoid code duplication
      get_result_with_response(model_df, df, aug_args)
    } else {
      broom::augment(model_df, model, newdata = df, ...)
    }
  }, error = function(e){
    if (grepl("arguments imply differing number of rows: ", e$message)) {
      # In this case, df has categories that aren't in model.
      # For example, a model that was created by "category" column which has "a", "b"
      # causes an error if df has "category" columnm which has "c".

      filtered_data <- df

      for(model in model_df[["model"]]){
        # remove rows that have categories that aren't in model
        # otherwise, broom::augment causes an error
        for (cname in colnames(model$model)) {
          # just filter categorical (not numeric) columns
          if (!is.numeric(model$model[[cname]])){
            filtered_data <- filtered_data[filtered_data[[cname]] %in% model$model[[cname]], ]
          }
        }
      }

      if (nrow(filtered_data) == 0) {
        stop("not enough information to predict in data frame")
      }

      if(with_response){
        # this function is defined beforehand to avoid code duplication
        get_result_with_response(model_df, filtered_data, aug_args)
      } else {
        broom::augment(model_df, model, newdata = filtered_data, ...)
      }
    } else {
      stop(e$message)
    }
  })
  colnames(ret)[colnames(ret) == ".fitted"] <- avoid_conflict(colnames(ret), "predicted_value")
  colnames(ret)[colnames(ret) == ".se.fit"] <- avoid_conflict(colnames(ret), "standard_error")

  ret
}

#' assign cluster number to each rows
#' @export
assign_cluster <- function(df, source_data){
  df_cnames <- colnames(df)
  # columns that are not model related are regarded as grouping column
  grouping_cols <- df_cnames[!df_cnames %in% c("model", "source.data")]

  # remove na from selected column (column names from cluster centers)
  # without this, augment throws an error with different number of data
  center_cnames <- colnames(df[["model"]][[1]]$centers)
  for (center_cname in center_cnames) {
    source_data <- source_data[!is.na(source_data[[center_cname]]), ]
  }

  source <- if(any(colnames(source_data) %in% grouping_cols)){
    # nest the source data by each group
    source_data %>%
      dplyr::group_by_(.dots = grouping_cols) %>%
      tidyr::nest()
  } else {
    # put one value column so that all data can be nested
    source_data %>%
      dplyr::mutate(data = 1) %>%
      dplyr::group_by(data) %>%
      tidyr::nest()
  }

  # drop unnecessary columns
  df <- dplyr::select_(df, .dots = c("model", grouping_cols))
  # augment data by each row
  # ungroup is needed because grouping is reseted by bind_cols

  joined <- if(length(grouping_cols) > 0) {
    # need this because do and nest has different row order,
    # so grouping_col should be used for index
    dplyr::inner_join(df, source, by = grouping_cols)
  } else {
    dplyr::bind_cols(df, source)
  }

  ret <- joined %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    broom::augment(model, data = data)
  # change factor to numeric
  ret[[".cluster"]] <- as.numeric(ret[[".cluster"]])
  colnames(ret)[colnames(ret) == ".cluster"] <- avoid_conflict(colnames(ret), "cluster")
  ret
}

#' tidy wrapper for kmeans
#' @export
cluster_info <- function(df){
  # , col.names = names(model[["cluster"]])
  ret <- df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(model = purrr::map(model, function(m){
      info <- data.frame(
        Size = m$size,
        Withinss = m$withinss
      )
      ret <- m$centers %>%
        as.data.frame()
      colnames(ret) <- paste("Center", colnames(ret))
      ret <- ret %>%
        dplyr::bind_cols(info)
      ret
    })) %>%
    tidyr::unnest(model)
}

#' glance wrapper for kmeans
#' @export
kmeans_info <- function(df){
  ret <- broom::glance(df, model)
  colnames(ret)[colnames(ret) == "totss"] <- "Total Sum of Squares"
  colnames(ret)[colnames(ret) == "tot.withinss"] <- "Total Sum of Squares within Clusters"
  colnames(ret)[colnames(ret) == "betweenss"] <- "Total Sum of Squares between Clusters"
  colnames(ret)[colnames(ret) == "iter"] <- "Number of Iterations"
  ret
}

#' augment using source data and test index
#' @param df Data frame that has model and .test_index.
#' @param data "training" or "test". Which source data should be used.
#' @param ... Additional argument to be passed to broom::augment
#' @export
prediction <- function(df, data = "training", conf_int = 0.95, ...){

  if (!data %in% c("test", "training")) {
    stop('data argument must be "test" or "training"')
  }

  if (!all(c("source.data", ".test_index", "model") %in% colnames(df))) {
    stop('input is not model data frame"')
  }

  test <- data == "test"

  df_cnames <- colnames(df)

  # columns other than "source.data", ".test_index" and "model" should be regarded as grouping columns
  # this should be kept after running prediction
  grouping_cols <- df_cnames[!df_cnames %in% c("source.data", ".test_index", "model")]

  # parsing arguments of prediction and getting optional arguemnt for augment in ...
  cll <- match.call()
  aug_args <- expand_args(cll, exclude = c("df", "test", "data"))

  # if type.predict argument is not indicated in this function
  # and models have $family$linkinv (basically, glm models have it),
  # both fitted link value column and response value column should appear in the result

  with_response <- !("type.predict" %in% names(cll)) && "family" %in% names(df[["model"]][[1]]) && !is.null(df[["model"]][[1]]$family$linkinv)

  ret <- if(test){
    # augment by test data

    # check if there is test data
    test_sizes <- vapply(df[[".test_index"]], function(test){
      length(test)
    }, FUN.VALUE = 0)

    if(all(test_sizes==0)){
      stop("no test data found")
    }

    # Use formula to support expanded aug_args (especially for type.predict for logistic regression)
    # because ... can't be passed to a function inside mutate directly.
    # If test is TRUE, this uses newdata as an argument and if not, uses data as an argument.
    aug_fml <- if(aug_args == ""){
      as.formula("~list(broom::augment(model, newdata = source.data))")
    } else {
      as.formula(paste0("~list(broom::augment(model, newdata = source.data, ", aug_args, "))"))
    }
    data_to_augment <- df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(source.data = purrr::map2(source.data, .test_index, function(df, index){
        # keep data only in test_index
        safe_slice(df, index)
      })) %>%
      dplyr::select(-.test_index)

    augmented <- tryCatch({
      data_to_augment %>%
        dplyr::rowwise() %>%
        # evaluate the formula of augment and "source.data" column will have it
        dplyr::mutate_(.dots = list(source.data = aug_fml))
    }, error = function(e){
      if (grepl("arguments imply differing number of rows: ", e$message)) {
        data_to_augment %>%
          dplyr::mutate(source.data = purrr::map2(source.data, model, function(df, model){
            # remove rows that have categories that aren't in training data
            # otherwise, broom::augment causes an error
            filtered_data <- df
            for (cname in colnames(model$model)) {
              filtered_data <- filtered_data[filtered_data[[cname]] %in% model$model[[cname]], ]
            }

            if(nrow(filtered_data) == 0){
              stop("no data found that can be predicted by the model")
            }

            filtered_data
          })) %>%
          dplyr::rowwise() %>%
          # evaluate the formula of augment and "data" column will have it
          dplyr::mutate_(.dots = list(source.data = aug_fml))
      } else {
        stop(e$message)
      }
    })

    if (with_response){
      augmented <- augmented %>%
        dplyr::ungroup() %>%
        dplyr::mutate(source.data = purrr::map2(source.data, model, add_response))
    }

    ret <- augmented %>%
      dplyr::select(-model) %>%
      tidyr::unnest(source.data)

  } else {
    # augment by trainig data

    # Use formula to support expanded aug_args (especially for type.predict for logistic regression)
    # because ... can't be passed to a function inside mutate directly.
    # If test is FALSE, this uses data as an argument and if not, uses newdata as an argument.
    aug_fml <- if(aug_args == ""){
      as.formula("~list(broom::augment(model, data = source.data))")
    } else {
      as.formula(paste0("~list(broom::augment(model, data = source.data, ", aug_args, "))"))
    }
    augmented <- df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(source.data = purrr::map2(source.data, .test_index, function(df, index){
        # remove data in test_index
        safe_slice(df, index, remove = TRUE)
      })) %>%
      dplyr::select(-.test_index) %>%
      dplyr::rowwise() %>%
      # evaluate the formula of augment and "data" column will have it
      dplyr::mutate_(.dots = list(source.data = aug_fml))

    if (with_response){
      augmented <- augmented %>%
        dplyr::ungroup() %>%
        dplyr::mutate(source.data = purrr::map2(source.data, model, add_response))
    }

    augmented %>%
      dplyr::select(-model) %>%
      tidyr::unnest(source.data)
  }

  # add confidence interval if conf_int is not null and there are .fitted and .se.fit
  if (!is.null(conf_int) & ".se.fit" %in% colnames(ret) & ".fitted" %in% colnames(ret)) {
    if (conf_int < 0 | conf_int > 1){
      stop("conf_int must be between 0 and 1")
    }
    conf_low_colname <- avoid_conflict(colnames(ret), "conf_low")
    conf_high_colname <- avoid_conflict(colnames(ret), "conf_high")
    lower <- (1-conf_int)/2
    higher <- 1-lower
    ret[[conf_low_colname]] <- get_confint(ret[[".fitted"]], ret[[".se.fit"]], conf_int = lower)
    ret[[conf_high_colname]] <- get_confint(ret[[".fitted"]], ret[[".se.fit"]], conf_int = higher)

    # move confidece interval columns next to standard error
    ret <- move_col(ret, conf_low_colname, which(colnames(ret) == ".se.fit") + 1)
    ret <- move_col(ret, conf_high_colname, which(colnames(ret) == conf_low_colname) + 1)
  }

  colnames(ret)[colnames(ret) == ".fitted"] <- avoid_conflict(colnames(ret), "predicted_value")
  colnames(ret)[colnames(ret) == ".se.fit"] <- avoid_conflict(colnames(ret), "standard_error")
  colnames(ret)[colnames(ret) == ".resid"] <- avoid_conflict(colnames(ret), "residuals")
  colnames(ret)[colnames(ret) == ".hat"] <- avoid_conflict(colnames(ret), "hat")
  colnames(ret)[colnames(ret) == ".sigma"] <- avoid_conflict(colnames(ret), "residual_standard_deviation")
  colnames(ret)[colnames(ret) == ".cooksd"] <- avoid_conflict(colnames(ret), "cooks_distance")
  colnames(ret)[colnames(ret) == ".std.resid"] <- avoid_conflict(colnames(ret), "standardised_residuals")

  dplyr::group_by_(ret, .dots = grouping_cols)
}

#' prediction wrapper to set predicted labels
#' @param df Data frame to predict. This should have model column.
#' @param threshold Threshold value for predicted probability or what to optimize. It can be "f_score", "accuracy", "precision", "sensitivity" or "specificity" to optimize.
#' @export
prediction_binary <- function(df, threshold = 0.5, ...){
  ret <- prediction(df, ...)

  first_model <- df[["model"]][[1]]

  # converting conf_low and conf_high from regression values
  # to probability values
  if(any(class(first_model) %in% "glm")){
    if (!is.null(first_model$family)) {
      # linkinv is a function to convert regression values
      # to response values (inverse of logit for logistic regression)
      if (!is.null(first_model$family$linkinv)) {
        if (any(colnames(ret) %in% "conf_low")) {
          conf_low_vec <- first_model$family$linkinv(ret[["conf_low"]])
          ret[["conf_low"]] <- NULL
          ret[["conf_low"]] <- conf_low_vec
        }
        if (any(colnames(ret) %in% "conf_high")) {
          conf_high_vec <- first_model$family$linkinv(ret[["conf_high"]])
          ret[["conf_high"]] <- NULL
          ret[["conf_high"]] <- conf_high_vec
        }
      }
    }
  }

  # get actual value column name from model formula
  actual_col <- if(!is.null(first_model$formula)) {
    all.vars(first_model$formula)[[1]]
  } else {
    # this is for xgboost model
    if ("xgb.Booster" %in% class(first_model)) {
      all.vars(first_model$fml)[[1]]
    } else {
      stop(paste0(class(first_model)[[1]], " is not supported by prediction_binary"))
    }
  }

  actual_val <- ret[[actual_col]]
  actual_logical <- as.logical(as.numeric(actual_val))

  prob_col_name <- if ("predicted_response" %in% colnames(ret)) {
    "predicted_response"
  } else if ("predicted_probability" %in% colnames(ret)){
    "predicted_probability"
  } else {
    "predicted_value"
  }

  thres <- if (!is.numeric(threshold)) {
    opt <- get_optimized_score(actual_logical, ret[[prob_col_name]], threshold)
    opt[["threshold"]]
  } else {
    threshold
  }

  predicted <- ret[[prob_col_name]] >= thres

  label <- if (is.logical(actual_val)) {
    predicted
  } else if (is.numeric(actual_val)) {
    as.numeric(predicted)
  } else if (is.factor(actual_val)){
    # create a factor vector with the same levels as actual_val
    # predicted is logical, so should +1 to make it index
    factor(levels(actual_val)[as.numeric(predicted) + 1], levels(actual_val))
  }

  ret[["predicted_label"]] <- label
  colnames(ret)[colnames(ret) == prob_col_name] <- "predicted_probability"

  ret
}

#' prediction wrapper to add predicted_probability (probability of "death"), predicted_status,
#' and actual_status, when time is specified.
#' @param df Data frame to predict. This should have model column.
#' @param time The point of time at which we predict survival of subjects.
#' @param threshold The threshold probability to determine predicted_status.
#' @export
prediction_coxph <- function(df, time = NULL, threshold = 0.5, ...){
  # TODO: need to make sure prediction.type is set to "lp"
  # when time is specified.
  ret <- prediction(df, ...)

  # if time is not specified return prediction() result as is.
  if (is.null(time)) {
    return(ret)
  }

  # extract variables for time and status from model formula
  surv_vars <- all.vars(lazyeval::f_lhs(df$model[[1]]$formula))
  time_colname <- surv_vars[[1]]
  status_colname <- surv_vars[[2]]

  # get group columns.
  # we assume that columns of model df other than the ones with reserved name are all group columns.
  model_df_colnames = colnames(df)
  group_by_names <- model_df_colnames[!model_df_colnames %in% c("source.data", ".test_index", "model")]

  # when pre-grouped, prediction() result is grouped.
  # but when not pre-grouped, it is without grouping.
  # in that case, we need to group it by something to work with following operations with nest/mutate/map.
  if (length(group_by_names) == 0) {
    ret <- ret %>% dplyr::mutate(dummy_group_col = 1) %>% dplyr::group_by(dummy_group_col)
  }

  # push ret in df so that we can work on ret and source.data at the same time in folliwing mutate() of df.
  nested_ret_df <- ret %>% tidyr::nest()
  df[["ret"]] <- nested_ret_df$data

  # group df. rowwise nature of df is stripped here.
  if (length(group_by_names) == 0) {
    # need to group by something to work with following operations with mutate/map.
    df <- df %>% dplyr::mutate(dummy_group_col = 1) %>% dplyr::group_by(dummy_group_col)
  }
  else {
    df <- df %>% dplyr::group_by_(group_by_names)
  }

  # add predicted_probability, actual_status, and predicted_status
  ret <- df %>%
    dplyr::mutate(ret = purrr::map2(ret, model, function(ret, model) {
      # basehaz returns base cumulative hazard.
      bh <- survival::basehaz(model)
      # create a function to interpolate function that returns cumulative hazard.
      bh_fun <- approxfun(bh$time, bh$hazard)
      cumhaz_base = bh_fun(time)
      # transform linear predictor (predicted_value) into predicted_probability.
      # predicted_probability is 1 - (y of survival curve).
      ret <- ret %>% dplyr::mutate(predicted_probability = 1 - exp(-cumhaz_base * exp(predicted_value)))
      ret
    }))
  ret <- ret %>% dplyr::select_(c("ret", group_by_names))
  ret <- ret %>% tidyr::unnest()

  # set it back to non-group-by state that is same as predict() output.
  if (length(group_by_names) == 0) {
    ret <- ret %>% dplyr::ungroup() %>% dplyr::select(-dummy_group_col)
  }

  true_value = TRUE
  if (is.numeric(ret[[status_colname]])) {
    if (any(ret[[status_colname]] == 2)) {
      true_value = 2
    }
    else {
      true_value = 1
    }
  }

  ret <- ret %>% dplyr::mutate(predicted_status = predicted_probability > threshold)
  if (length(group_by_names) > 0) {
    ret <- ret %>% ungroup() # next line does not work under group_by state, probably because of using dot. so ungroup it once.
  }
  ret <- ret %>% dplyr::mutate(actual_status = if_else((.[[time_colname]] <= time & .[[status_colname]] == true_value), TRUE, if_else(.[[time_colname]] >= time, FALSE, NA)))
  if (length(group_by_names) > 0) {
    ret <- ret %>% dplyr::group_by_(group_by_names) # group it back again.
  }

  if (is.numeric(true_value)) {
    ret$predicted_status <- as.numeric(ret$predicted_status)
    ret$actual_status <- as.numeric(ret$actual_status)
    if (true_value == 2) {
      ret$predicted_status <- ret$predicted_status + 1
      ret$actual_status <- ret$actual_status + 1
    }
  }
  ret
}

#' tidy wrapper for lm and glm
#' @export
model_coef <- function(df, pretty.name = FALSE, conf_int = NULL, ...){
  dots <- list(...)

  ret <- if (!is.null(conf_int)) {
    if (conf_int == "default") {

      level <- dots$conf.level

      if (is.null(level)) {
        # default confidence level
        level <- 0.95
      }

      df %>%
        dplyr::ungroup() %>%
        dplyr::mutate(model = purrr::map(model, function(model){
          # use confint.default for performance
          tidy_ret <- broom::tidy(model, ...)

          # calculate confidence interval by estimate and std.error
          if(any(colnames(tidy_ret) %in% "estimate") & any(colnames(tidy_ret) %in% "std.error")){
            level_low <- (1-level)/2
            level_high <- 1-level_low

            tidy_ret[["conf.low"]] <- get_confint(tidy_ret[["estimate"]], tidy_ret[["std.error"]], level_low)
            tidy_ret[["conf.high"]] <- get_confint(tidy_ret[["estimate"]], tidy_ret[["std.error"]], level_high)
          }
          tidy_ret

        })) %>%
        tidyr::unnest(model)
    } else {
      # broom::tidy uses confint.lm and it uses profile, so "profile" is used in conf_int to swith how to get confint
      profile <- conf_int == "profile"
      broom::tidy(df, model, conf.int = profile, pretty.name = pretty.name, ...)
    }
  } else {
    broom::tidy(df, model, pretty.name = pretty.name, ...)
  }

  if ("glm" %in% class(df$model[[1]])) {
    if (!is.null(df$model[[1]]$family)) {
      if (df$model[[1]]$family == "binomial"){
        ret <- ret %>% dplyr::mutate(odds_ratio = exp(estimate))
      }
    }
  }
  if ("coxph" %in% class(df$model[[1]])) {
    ret <- ret %>% dplyr::mutate(hazard_ratio = exp(estimate))
  }
  if ("multinom" %in% class(df$model[[1]])) {
    # estimate should be odds_ratio and logarithm of estimate should be new estimate
    # conf_low and conf_high should be also logarithm of themselves
    odds_ratio <- ret[["estimate"]]
    ret[["estimate"]] <- log(ret[["estimate"]])
    ret[["odds_ratio"]] <- odds_ratio
    if("conf.low" %in% colnames(ret)){
      ret[["conf.low"]] <- log(ret[["conf.low"]])
    }
    if("conf.high" %in% colnames(ret)){
      ret[["conf.high"]] <- log(ret[["conf.high"]])
    }
  }

  if (pretty.name){
    colnames(ret)[colnames(ret) == "term"] <- "Term"
    colnames(ret)[colnames(ret) == "statistic"] <- "t Ratio"
    colnames(ret)[colnames(ret) == "p.value"] <- "P Value"
    colnames(ret)[colnames(ret) == "std.error"] <- "Std Error"
    colnames(ret)[colnames(ret) == "estimate"] <- "Estimate"
    colnames(ret)[colnames(ret) == "conf.low"] <- "Conf Low"
    colnames(ret)[colnames(ret) == "conf.high"] <- "Conf High"
    colnames(ret)[colnames(ret) == "hazard_ratio"] <- "Hazard Ratio"
    colnames(ret)[colnames(ret) == "odds_ratio"] <- "Odds Ratio"
    colnames(ret)[colnames(ret) == "y.level"] <- "Predicted Label"
  } else {
    colnames(ret)[colnames(ret) == "statistic"] <- "t_ratio"
    colnames(ret)[colnames(ret) == "p.value"] <- "p_value"
    colnames(ret)[colnames(ret) == "std.error"] <- "std_error"
    colnames(ret)[colnames(ret) == "conf.low"] <- "conf_low"
    colnames(ret)[colnames(ret) == "conf.high"] <- "conf_high"
    colnames(ret)[colnames(ret) == "y.level"] <- "predicted_label"
  }
  # tidy() on coxph keeps .test_index and source.data that we added. Let's drop it.
  if(".test_index" %in% colnames(ret)){
    ret <- ret %>% dplyr::select(-.test_index)
  }
  if("source.data" %in% colnames(ret)){
    ret <- ret %>% dplyr::select(-source.data)
  }
  ret
}

#' glance wrapper
#' @export
model_stats <- function(df, pretty.name = FALSE, ...){
  ret <- broom::glance(df, model, pretty.name = pretty.name, ...)

  formula_vars <- if (any(c("multinom", "lm", "glm") %in% class(df$model[[1]]))) {
    # lm, glm (including logistic), multinom has a formula class attribute $terms.
    # dot (.) is espanded into actual names there, which is convenient for our purpose of finding factor variables.
    all.vars(df$model[[1]]$terms)
  } else if("coxph" %in% class(df$model[[1]])) {
    # coxph has $formula, but it can have dot (.) in it, which is not good for extracting variable names.
    # use $assign instead.
    names(df$model[[1]]$assign)
  } else {
    NULL
  }

  # get group columns.
  # we assume that columns of model df other than the ones with reserved name are all group columns.
  model_df_colnames = colnames(df)
  group_by_names <- model_df_colnames[!model_df_colnames %in% c("source.data", ".test_index", "model")]

  # when pre-grouped, each row of glance result is actually a group.
  # but when not pre-grouped, the only-one row is not a group.
  # in that case, we need to group it by something to work with following operations with nest/mutate/map.
  if (length(group_by_names) == 0) {
    ret <- ret %>% dplyr::mutate(dummy_group_col = 1) %>% dplyr::group_by(dummy_group_col)
  }

  # push ret in df so that we can work on ret and source.data at the same time in folliwing mutate() of df.
  nested_ret_df <- ret %>% tidyr::nest()
  df[["ret"]] <- nested_ret_df$data

  # group df. rowwise nature of df is stripped here.
  if (length(group_by_names) == 0) {
    # need to group by something to work with following operations with mutate/map.
    df <- df %>% dplyr::mutate(dummy_group_col = 1) %>% dplyr::group_by(dummy_group_col)
  }
  else {
    df <- df %>% dplyr::group_by_(group_by_names)
  }

  ret <- df %>%
    dplyr::mutate(ret = purrr::map2(ret, source.data, function(ret, source_data) {
      # for each factor variable in the formula, add base level info column to ret.
      if(!is.null(formula_vars)){
        for(var in formula_vars) {
          if(is.factor(source_data[[var]])) {
            if(pretty.name) {
              ret[paste0("Base Level of ", var)] <- levels(source_data[[var]])[[1]]
            }
            else {
              ret[paste0(var, "_base")] <- levels(source_data[[var]])[[1]]
            }
          }
        }
      }
      ret
    })) %>%
    dplyr::select_(c("ret", group_by_names)) %>%
    tidyr::unnest()

  # set it back to non-group-by state that is same as glance() output.
  if (length(group_by_names) == 0) {
    ret <- ret %>% dplyr::ungroup() %>% dplyr::select(-dummy_group_col)
  }


  # adjust column name style
  if(pretty.name){
    colnames(ret)[colnames(ret) == "r.squared"] <- "R Square"
    colnames(ret)[colnames(ret) == "adj.r.squared"] <- "R Square Adj"
    colnames(ret)[colnames(ret) == "sigma"] <- "Root Mean Square Error"
    colnames(ret)[colnames(ret) == "statistic"] <- "F Ratio"
    colnames(ret)[colnames(ret) == "p.value"] <- "P Value"
    colnames(ret)[colnames(ret) == "df"] <- "DF"
    colnames(ret)[colnames(ret) == "logLik"] <- "Log Likelihood"
    colnames(ret)[colnames(ret) == "deviance"] <- "Deviance"
    colnames(ret)[colnames(ret) == "df.residual"] <- "Residual DF"
    # for glm
    colnames(ret)[colnames(ret) == "null.deviance"] <- "Null Deviance"
    colnames(ret)[colnames(ret) == "df.null"] <- "DF for Null Model"
    # for multinom
    colnames(ret)[colnames(ret) == "edf"] <- "Effective Number of DF"
    # for coxph
    colnames(ret)[colnames(ret) == "n"] <- "Number of Rows"
    colnames(ret)[colnames(ret) == "nevent"] <- "Number of Events"
    colnames(ret)[colnames(ret) == "statistic.log"] <- "Likelihood Ratio Test"
    colnames(ret)[colnames(ret) == "p.value.log"] <- "Likelihood Ratio Test P Value"
    colnames(ret)[colnames(ret) == "statistic.sc"] <- "Score Test"
    colnames(ret)[colnames(ret) == "p.value.sc"] <- "Score Test P Value"
    colnames(ret)[colnames(ret) == "statistic.wald"] <- "Wald Test"
    colnames(ret)[colnames(ret) == "p.value.wald"] <- "Wald Test P Value"
    colnames(ret)[colnames(ret) == "r.squared.max"] <- "R Square Max"
    colnames(ret)[colnames(ret) == "concordance"] <- "Concordance"
    colnames(ret)[colnames(ret) == "std.error.concordance"] <- "Std Error Concordance"


    colnames(ret)[colnames(ret) =="number_of_iteration"] <- "Number of Iteration"
    colnames(ret)[colnames(ret) =="root_mean_square_error"] <- "Root Mean Square Error"
    colnames(ret)[colnames(ret) =="mean_absolute_error"] <- "Mean Absolute Error"
    colnames(ret)[colnames(ret) =="negative_log_likelihood"] <- "Negative Log Likelihood"
    colnames(ret)[colnames(ret) =="binary_missclassification_rate"] <- "Binary Missclassification Rate"
    colnames(ret)[colnames(ret) =="missclassification_rate"] <- "Missclassification Rate"
    colnames(ret)[colnames(ret) =="multiclass_logloss"] <- "Multiclass Logloss"
    colnames(ret)[colnames(ret) =="auc"] <- "AUC"
    colnames(ret)[colnames(ret) =="normalized_discounted_cumulative_gain"] <- "Normalized Discounted Cumulative Gain"
    colnames(ret)[colnames(ret) =="mean_average_precision"] <- "Mean Average Precision"
  }else{
    colnames(ret)[colnames(ret) == "r.squared"] <- "r_square"
    colnames(ret)[colnames(ret) == "adj.r.squared"] <- "r_square_adj"
    colnames(ret)[colnames(ret) == "sigma"] <- "root_mean_square_error"
    colnames(ret)[colnames(ret) == "statistic"] <- "f_ratio"
    colnames(ret)[colnames(ret) == "p.value"] <- "p_value"
    colnames(ret)[colnames(ret) == "logLik"] <- "log_likelihood"
    colnames(ret)[colnames(ret) == "df.residual"] <- "residual_df"
    # for glm
    colnames(ret)[colnames(ret) == "null.deviance"] <- "null_deviance"
    colnames(ret)[colnames(ret) == "df.null"] <- "df_for_null_model"
    # for multinom
    colnames(ret)[colnames(ret) == "edf"] <- "effective_number_of_df"
    # for coxph
    colnames(ret)[colnames(ret) == "nevent"] <- "n_event"
    colnames(ret)[colnames(ret) == "statistic.log"] <- "likelihood_ratio_test"
    colnames(ret)[colnames(ret) == "p.value.log"] <- "likelihood_ratio_test_p_value"
    colnames(ret)[colnames(ret) == "statistic.sc"] <- "score_test"
    colnames(ret)[colnames(ret) == "p.value.sc"] <- "score_test_p_value"
    colnames(ret)[colnames(ret) == "statistic.wald"] <- "wald_test"
    colnames(ret)[colnames(ret) == "p.value.wald"] <- "wald_test_p_value"
    colnames(ret)[colnames(ret) == "r.squared.max"] <- "r_square_max"
    colnames(ret)[colnames(ret) == "std.error.concordance"] <- "std_error_concordance"
    colnames(ret)[colnames(ret) == "AIC"] <- "aic"
    colnames(ret)[colnames(ret) == "BIC"] <- "bic"
  }

  ret
}

#' tidy after converting model to anova
#' @export
model_anova <- function(df, pretty.name = FALSE){
  ret <- suppressWarnings({
    # this causes warning for Deviance, Resid..Df, Resid..Dev in glm model
    df %>% dplyr::mutate(model = list(anova(model))) %>% broom::tidy(model)
  })
  if(pretty.name){
    colnames(ret)[colnames(ret) == "term"] <- "Term"
    colnames(ret)[colnames(ret) == "sumsq"] <- "Sum of Squares"
    colnames(ret)[colnames(ret) == "meansq"] <- "Mean Square"
    colnames(ret)[colnames(ret) == "statistic"] <- "F Ratio"
    colnames(ret)[colnames(ret) == "p.value"] <- "P Value"
    colnames(ret)[colnames(ret) == "df"] <- "DF"
    # for glm anova
    colnames(ret)[colnames(ret) == "Resid..Df"] <- "Residual DF"
    colnames(ret)[colnames(ret) == "Resid..Dev"] <- "Residual Deviance"
    # for coxph anova
    colnames(ret)[colnames(ret) == "loglik"] <- "Log Likelihood"
    colnames(ret)[colnames(ret) == "Pr...Chi.."] <- "P Value" # looks like this means P value for chisquare test.
  } else {
    colnames(ret)[colnames(ret) == "sumsq"] <- "sum_of_squares"
    colnames(ret)[colnames(ret) == "meansq"] <- "mean_square"
    colnames(ret)[colnames(ret) == "statistic"] <- "f_ratio"
    colnames(ret)[colnames(ret) == "p.value"] <- "p_value"
    # for glm anova
    colnames(ret)[colnames(ret) == "Deviance"] <- "deviance"
    colnames(ret)[colnames(ret) == "Resid..Df"] <- "residual_df"
    colnames(ret)[colnames(ret) == "Resid..Dev"] <- "residual_deviance"
    # for coxph anova
    colnames(ret)[colnames(ret) == "loglik"] <- "log_likelihood"
    colnames(ret)[colnames(ret) == "Pr...Chi.."] <- "p_value"
  }
  ret
}

#' tidy after converting model to survfit
#' @param newdata Data frame with rows that represent cohorts to simulate
#' @export
prediction_survfit <- function(df, newdata = NULL, ...){
  caller <- match.call()
  # this expands dots arguemtns to character
  arg_char <- expand_args(caller, exclude = c("df"))
  if (arg_char != "") {
    fml <- as.formula(paste0("~list(survival::survfit(model, ", arg_char, "))"))
  } else {
    fml <- as.formula(paste0("~list(survival::survfit(model))"))
  }
  ret <- df %>% dplyr::mutate_(.dots = list(model = fml)) %>% broom::tidy(model)

  # if newdata exists and has more than one row, make output data frame tidy.
  # original output is in wide-format with columns like estimate.1, estimate.2, ...
  if (!is.null(newdata) && nrow(newdata) > 1) {
    # first, unite columns for a cohort into one column, so that gather at the next step works.
    united_colnames = c()
    for (i in 1:nrow(newdata)){
      united_colname = paste0("est", i)
      ret <- ret %>% unite_(united_colname, c(paste0("estimate.",i), paste0("std.error.",i), paste0("conf.high.",i),paste0("conf.low.",i)), sep="_", remove=TRUE)
      united_colnames = c(united_colnames, united_colname)
    }
    # gather the united values into key column (.cohort.temp) and value column (.val.temp)
    gathered <- ret %>% gather_(".cohort.temp", ".val.temp", united_colnames)
    # separte the value column to reverse the effect of unite() we did before.
    ret <- gathered %>% separate_(".val.temp",c("estimate", "std_error", "conf_high", "conf_low"),sep="_")
    # convert characterized data back to numeric.
    ret <- ret %>% mutate(estimate = as.numeric(estimate), std_error = as.numeric(std_error), conf_high = as.numeric(conf_high), conf_low = as.numeric(conf_low))
    # replace the cohort name with a string that is a concatenation of values that represents the cohort.
    cohorts_labels <- newdata %>% unite(label, everything())
    for (i in 1:nrow(newdata)){
      ret <- ret %>% mutate(.cohort.temp = if_else(paste0("est", i) == .cohort.temp, cohorts_labels$label[[i]], .cohort.temp))
    }
    # replace the .cohort.temp column name with name like "age_sex".
    colnames(ret)[colnames(ret) == ".cohort.temp"] <- paste0(colnames(newdata), collapse = "_")
  }

  colnames(ret)[colnames(ret) == "n.risk"] <- "n_risk"
  colnames(ret)[colnames(ret) == "n.event"] <- "n_event"
  colnames(ret)[colnames(ret) == "n.censor"] <- "n_censor"
  colnames(ret)[colnames(ret) == "std.error"] <- "std_error"
  colnames(ret)[colnames(ret) == "conf.low"] <- "conf_low"
  colnames(ret)[colnames(ret) == "conf.high"] <- "conf_high"
  ret
}

#' tidy after generating survfit
#' @export
do_survfit <- function(df, time, status, ...){
  # need to compose formula with non-standard evaluation.
  # simply using time and status in formula here results in a formula that literally looks at
  # "time" columun and "status" column, which is not what we want.
  fml <- as.formula(paste0("survival::Surv(`", substitute(time), "`,`", substitute(status), "`) ~ 1"))
  ret <- df %>% build_model(model_func = survival::survfit, formula = fml, ...) %>% broom::tidy(model)
  colnames(ret)[colnames(ret) == "n.risk"] <- "n_risk"
  colnames(ret)[colnames(ret) == "n.event"] <- "n_event"
  colnames(ret)[colnames(ret) == "n.censor"] <- "n_censor"
  colnames(ret)[colnames(ret) == "std.error"] <- "std_error"
  colnames(ret)[colnames(ret) == "conf.low"] <- "conf_low"
  colnames(ret)[colnames(ret) == "conf.high"] <- "conf_high"
  ret
}

#' tidy after converting model to confint
#' @export
model_confint <- function(df, ...){
  caller <- match.call()
  # this expands dots arguemtns to character
  arg_char <- expand_args(caller, exclude = c("df"))
  if (arg_char != "") {
    fml <- as.formula(paste0("~list(stats::confint(model, ", arg_char, "))"))
  } else {
    fml <- as.formula(paste0("~list(stats::confint(model))"))
  }
  ret <- df %>% dplyr::mutate_(.dots = list(model = fml)) %>% broom::tidy(model)
  colnames(ret)[colnames(ret) == ".rownames"] <- "Term"
  # original columns are like X0.5..   X99.5.., so replace X to Prob and remove trailing dots
  new_p_cnames <- stringr::str_replace(colnames(ret)[(ncol(ret)-1):ncol(ret)], "X", "Prob ") %>%
    stringr::str_replace("\\.+$", "")
  colnames(ret)[(ncol(ret)-1):ncol(ret)] <- new_p_cnames
  ret
}
