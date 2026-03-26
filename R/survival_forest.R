# Extracts survival rates at specific time from survival curves.
# survival - Matrix of survival curves. Rows represents observations and columns represents points of time.
# unique_death_times - Numeric vector of unique death times.
# at - Time at which the survival rates should be extracted from survival curves.
extract_survival_rate_at <- function(survival, unique_death_times, at) {
  index <- sum(unique_death_times <= at)
  survival[, index]
}

# Calculate weighted mean survival time from predicted survival curve.
calc_mean_survival <- function(survival, unique_death_times) {
  # If the survival rate at the last unique death time is not 0,
  # we assume that the survivers lived one more term, just so that we can calculate the finite mean.
  # Thus the unique.death.times is appended with max(unique.death.times)+1.
  ret <- -matrixStats::rowDiffs(cbind(1,survival,0)) %*% c(unique_death_times, max(unique_death_times)+1)
  ret
}

# Permutation importance. The one from ranger seems too unstable for our use. Maybe it's based on OOB prediction?
calc_permutation_importance_ranger_survival <- function(fit, time_col, status_col, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(data, vars=var, y=time_col, model=fit, nperm=5, # Since the result seems too unstable with nperm=1, where we use elsewhere, here we use 5.
                                predict.fun = function(object, newdata) {
                                  predicted <- predict(object,data=newdata)
                                  # Use the weighted mean predicted survival time as the predicted value. To evaluate prediction performance, we will later calcualte concordance based on it.
                                  mean_survival <- calc_mean_survival(predicted$survival, predicted$unique.death.times)
                                  tibble::tibble(x=mean_survival,status=newdata[[status_col]])
                                },
                                loss.fun = function(x,y){ # Use 1 - concordance as loss function.
                                  df <- x %>% dplyr::mutate(time=!!y[[1]])
                                  1 - survival::concordance(survival::Surv(time, status)~x,data=df)$concordance
                                })
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble::tibble(variable=vars, importance=pmax(importances, 0)) # Show 0 for negative importance, which can be caused by chance in case of permutation importance.
  importances_df <- importances_df %>% dplyr::arrange(-importance)
  importances_df
}

# Calculates survival curves with Kaplan-Meier with strata specified by vars argument.
calc_survival_curves_with_strata <- function(df, time_col, status_col, vars, orig_predictor_classes) {
  # Create map from variable name to chart type. TODO: Eliminate duplicated code.
  chart_type_map <- c()
  x_type_map <-c()
  for(col in colnames(df)) {
    if (is.numeric(df[[col]])) {
      x_type <- "numeric"
      chart_type <- "line"
    }
    # Since we turn logical into factor in preprocess_regression_data_after_sample(), detect them accordingly.
    else if (is.factor(df[[col]]) && all(levels(df[[col]]) %in% c("TRUE", "FALSE", "(Missing)"))) {
      x_type <- "logical"
      chart_type <- "scatter"
    }
    # Since we turn character into factor in preprocess_regression_data_after_sample(), look at the recorded original class.
    else if (!is.null(orig_predictor_classes) && "factor" %in% orig_predictor_classes[[col]]) {
      x_type <- "factor"
      chart_type <- "scatter"
    }
    else {
      x_type <- "character" # Since we turn charactors into factor in preprocessing (fct_lump) and cannot distinguish the original type at this point, for now, we treat both factors and characters as "characters" here.
      chart_type <- "scatter"
    }
    x_type_map <- c(x_type_map, x_type)
    chart_type_map <- c(chart_type_map, chart_type)
  }
  names(x_type_map) <- colnames(df)
  names(chart_type_map) <- colnames(df)

  vars_list <- as.list(vars)
  curve_dfs_list <- purrr::map(vars_list, function(var) {
    if (is.numeric(df[[var]])) { # categorize numeric column into 10 bins.
      grouped <- df %>% dplyr::mutate(.temp.bin.column=cut(!!rlang::sym(var), breaks=10)) %>% dplyr::group_by(.temp.bin.column)
      df <- grouped %>% dplyr::mutate(!!rlang::sym(var):=mean(!!rlang::sym(var), na.rm=TRUE))
    }
    fml <- as.formula(paste0("survival::Surv(`", time_col, "`,`", status_col, "`) ~ `", var, "`"))
    fit <- survival::survfit(fml, data = df)
    ret <- broom:::tidy.survfit(fit)
    ret
  })
  # Bind the data frames in the list. fill=TRUE is needed because it is possible that strata column is missing for some of the data frames
  # if var has only one value for all the rows.
  ret <- data.table::rbindlist(curve_dfs_list, fill=TRUE)
  ret <- ret %>% tidyr::separate(strata, into = c('variable','value'), sep='=') %>% rename(survival=estimate, period=time)
  ret <- ret %>% dplyr::mutate(chart_type = chart_type_map[variable])
  ret <- ret %>% dplyr::mutate(x_type = x_type_map[variable])
  ret
}

# Calculates partial dependence. The output is survival curves at different values of the specified variables.
partial_dependence.ranger_survival_exploratory <- function(fit, time_col, vars = colnames(data),
  n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)), # Keeping same default of 25 as edarf::partial_dependence, although we usually overwrite from callers.
  interaction = FALSE, uniform = TRUE, data, ...) {
  times <- sort(unique(data[[time_col]])) # Keep vector of actual times to map time index to actual time later.
  # Add time 0 if times does not start with 0.
  time_zero_added <- FALSE
  if (times[1] != 0) {
    times <- c(0, times)
    time_zero_added <- TRUE
  }

  predict.fun <- function(object, newdata) {
    res <- predict(object, data=newdata)$survival
    # If time 0 is added, add the column of 100% survival.
    if (time_zero_added) {
      res <- cbind(rep(1,nrow(res)),res)
    }
    res
  }

  aggregate.fun <- function(x) {
    mean(x)
  }

  args = list(
    "data" = data,
    "vars" = vars,
    "n" = n,
    "model" = fit,
    "uniform" = uniform,
    "predict.fun" = predict.fun,
    "aggregate.fun" = aggregate.fun,
    ...
  )
  
  if (length(vars) > 1L & !interaction) { # More than one variables are there. Iterate calling mmpf::marginalPrediction.
    pd = data.table::rbindlist(sapply(vars, function(x) {
      args$vars = x
      if ("points" %in% names(args))
        args$points = args$points[x]
      if (!is.numeric(data[[x]])) { # If categorical, cover all categories in the data.
        n_tmp <- args$n
        n_tmp[1] <- length(unique(data[[x]]))
        args$n <- n_tmp
      }
      else if (all(data[[x]] %% 1 == 0)) { # Adjust for integer with a few unique values
        n_uniq <- max(data[[x]]) - min(data[[x]]) + 1
        if (n_uniq <= 5) {
          n_tmp <- args$n
          n_tmp[1] <- n_uniq
          args$n <- n_tmp
        }
      }
      mp = do.call(mmpf::marginalPrediction, args)
      mp
    }, simplify = FALSE), fill = TRUE)
    data.table::setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else {
    if (!is.numeric(data[[vars]])) { # If categorical, cover all categories in the data.
      n_tmp <- args$n
      n_tmp[1] <- length(unique(data[[vars]]))
      args$n <- n_tmp
    }
    else if (all(data[[vars]] %% 1 == 0)) { # Adjust for integer with a few unique values
      n_uniq <- max(data[[vars]]) - min(data[[vars]]) + 1
      if (n_uniq <= 5) {
        n_tmp <- args$n
        n_tmp[1] <- n_uniq
        args$n <- n_tmp
      }
    }
    pd = do.call(mmpf::marginalPrediction, args)
  }

  attr(pd, "class") = c("pd", "data.frame")
  attr(pd, "interaction") = interaction == TRUE
  attr(pd, "vars") = vars
  # Format of pd looks like this:
  #        trt age        V1        V2        V3        V4        V5        V6        V7        V8        V9       V10
  # 1 1.000000  NA 0.9984156 0.9971480 0.9867692 0.9843847 0.9631721 0.9357729 0.9165525 0.8988693 0.8679139 0.8532433
  # 2 1.111111  NA 0.9984156 0.9971480 0.9867692 0.9843847 0.9631721 0.9357729 0.9165525 0.8988693 0.8679139 0.8532433
  # 3 1.222222  NA 0.9984156 0.9971480 0.9867692 0.9843847 0.9631721 0.9357729 0.9165525 0.8988693 0.8679139 0.8532433
  ret <- pd %>% tidyr::pivot_longer(matches('^V[0-9]+$'),names_to = 'period', values_to = 'survival')
  ret <- ret %>% dplyr::mutate(period = as.numeric(stringr::str_remove(period,'^V')))
  # Format of ret looks like this:
  #     trt   age period survival
  #   <dbl> <dbl>  <dbl>    <dbl>
  # 1     1    NA      1    0.997
  # 2     1    NA      2    0.995
  # 3     1    NA      3    0.984
  # 4     1    NA      4    0.981

  chart_type_map <- c()
  for(col in colnames(ret)) {
    chart_type_map <- c(chart_type_map, is.numeric(ret[[col]]))
  }
  chart_type_map <- ifelse(chart_type_map, "line", "scatter")
  names(chart_type_map) <- colnames(ret)

  # To avoid "Error: Can't convert <double> to <character>." from pivot_longer we need to use values_transform rather than values_ptype. https://github.com/tidyverse/tidyr/issues/980
  ret <- ret %>% tidyr::pivot_longer(c(-period, -survival) ,names_to = 'variable', values_to = 'value', values_transform = list(value=as.character), values_drop_na=TRUE)
  # Format of ret looks like this:
  #   period survival variable value
  #   <chr>     <dbl> <chr>    <dbl>
  # 1 1         0.997 trt          1
  # 2 2         0.995 trt          1
  # 3 3         0.984 trt          1
  # 4 4         0.981 trt          1
  # 5 5         0.963 trt          1
  # 6 6         0.938 trt          1
  # 7 7         0.931 trt          1
  # 8 8         0.920 trt          1
  # 9 9         0.902 trt          1
  #10 10        0.896 trt          1
  ret <- ret %>%  dplyr::mutate(period = (!!times)[period]) # Map back period from index to actual time.
  ret <- ret %>%  dplyr::mutate(chart_type = chart_type_map[variable])
  ret
}


#' special version of tidy.coxph function to use with build_coxph.fast.
#' @export
tidy.ranger_survival_exploratory <- function(x, type = 'importance', ...) { #TODO: add test
  if ("error" %in% class(x)) {
    ret <- data.frame()
    return(ret)
  }
  switch(type,
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
    partial_dependence_survival_curve = {
      ret <- x$partial_dependence
      ret <- ret %>% dplyr::group_by(variable) %>% tidyr::nest() %>%
        dplyr::mutate(data = purrr::map(data,function(df){ # Show only 5 lines out of 9 lines for survival curve.
          if (df$chart_type[[1]] == 'line') {
            ret <- df %>% dplyr::mutate(value_index=as.integer(forcats::fct_inorder(value)))
            # %% 2 is to show only 5 lines out of 9 lines for survival curves variated by a numeric variable.
            if (max(ret$value_index) >= 9) { # It is possible that value index is less than 9 for integer with 5 or less unique values.
              ret <- ret %>% dplyr::filter(value_index %% 2 == 1) %>% dplyr::mutate(value_index=ceiling(value_index/2))
            }
            ret
          }
          else {
            df %>% dplyr::mutate(value_index=as.integer(forcats::fct_inorder(value))) %>% dplyr::mutate(value_index=value_index+5)
          }
        })) %>% tidyr::unnest() %>% dplyr::ungroup() %>% dplyr::mutate(value_index=factor(value_index)) # Make value_index a factor to control color.

      # Reduce number of unique x-axis values for better chart drawing performance, and not to overflow it.
      grid <- 40
      divider <- max(ret$period) %/% grid
      if (divider >= 2) {
        ret <- ret %>% dplyr::mutate(period = period %/% divider * divider) %>%
          dplyr::group_by(variable, value, chart_type, value_index, period) %>%
          dplyr::summarize(survival=max(survival)) %>%
          dplyr::ungroup()
      }

      ret <- ret %>% dplyr::mutate(variable = forcats::fct_relevel(variable, !!x$imp_vars)) # set factor level order so that charts appear in order of importance.
      # set order to ret and turn it back to character, so that the order is kept when groups are bound.
      # if it were kept as factor, when groups are bound, only the factor order from the first group would be respected.
      ret <- ret %>% dplyr::arrange(variable) %>% dplyr::mutate(variable = as.character(variable))
      ret <- ret %>% dplyr::mutate(chart_type = 'line')
      ret <- ret %>% dplyr::mutate(variable = x$terms_mapping[variable]) # map variable names to original.
      ret
    },
    partial_dependence = {
      ret <- x$partial_dependence
      pred_survival_time <- x$pred_survival_time
      ret <- ret %>%
        dplyr::filter(period <= !!pred_survival_time) %>% # Extract the latest period that does not exceed pred_survival_time
        dplyr::group_by(variable, value) %>% dplyr::filter(period == max(period)) %>% dplyr::ungroup() %>%
        dplyr::mutate(type='Prediction')
      actual <- x$survival_curves %>%
        dplyr::filter(period <= !!pred_survival_time) %>% # Extract the latest period that does not exceed pred_survival_time
        dplyr::group_by(variable, value) %>% dplyr::filter(period == max(period)) %>% dplyr::ungroup() %>%
        dplyr::mutate(type='Actual')
      ret <- actual %>% dplyr::bind_rows(ret) # actual rows need to come first for the order of chart drawing.
      ret <- ret %>% dplyr::mutate(variable = forcats::fct_relevel(variable, !!x$imp_vars)) # set factor level order so that charts appear in order of importance.
      # set order to ret and turn it back to character, so that the order is kept when groups are bound.
      # if it were kept as factor, when groups are bound, only the factor order from the first group would be respected.
      ret <- ret %>% dplyr::arrange(variable) %>% dplyr::mutate(variable = as.character(variable))
      ret <- ret %>% survival_pdp_sort_categorical()
      ret <- ret %>% dplyr::mutate(variable = x$terms_mapping[variable]) # map variable names to original.
      ret
    })
}

glance.ranger_survival_exploratory <- function(x, data_type = "training", ...) {
  if (data_type == "training") {
    if (!is.null(x$concordance)) {
      tibble::tibble(Concordance=x$concordance$concordance, `Std Error Concordance`=sqrt(x$concordance$var), `Time-dependent AUC`=x$auc, `Rows`=nrow(x$df), `Rows (TRUE)`=x$nevent)
    }
    else {
      data.frame()
    }
  }
  else { # data_type == "test"
    if (!is.null(x$concordance_test)) {
      tibble::tibble(Concordance=x$concordance_test$concordance, `Std Error Concordance`=sqrt(x$concordance_test$var), `Time-dependent AUC`=x$auc_test, `Rows`=nrow(x$df_test), `Rows (TRUE)`=x$test_nevent)
    }
    else {
      data.frame()
    }
  }
}

# Extracts and interpolates survival rate from survival_mat, for the time specified for each observation by time_vec. 
# survival_mat - Predicted survival curve data obtained from the prediction. 
# time_vec - Time to extract the prediction from the survival_mat for each observation.
# time_index_fun - Function to convert time into the column index + 1 of the survival_mat matrix.
#                  +1 is because we insert a column for time 0 to the matrix to cover the time before the first event. 
survival_time_to_predicted_rate <- function(survival_mat, time_vec, time_index_fun) {
  index_vec <- time_index_fun(time_vec)
  index_vec_floor <- floor(index_vec)
  index_vec_ceiling <- ceiling(index_vec)
  # Will use this index_vec_residual for interpolation between the values for index_vec_floor and index_vec_ceiling.
  index_vec_residual <- index_vec - index_vec_floor
  # 2-column matrix to pass to the survival_mat_adjusted to extract its elements as a vector.
  index_matrix_floor <- cbind(1:length(time_vec), index_vec_floor)
  index_matrix_ceiling <- cbind(1:length(time_vec), index_vec_ceiling)
  # Insert a column for time 0 to cover the time between 0 and the first event.
  survival_mat_adjusted <- cbind(rep(1,nrow(survival_mat)), survival_mat)
  # Extract the values from the matrix as vectors.
  survival_rate_floor <- survival_mat_adjusted[index_matrix_floor]
  survival_rate_ceiling <- survival_mat_adjusted[index_matrix_ceiling]
  # interpolate between survival_rate_floor and survival_rate_ceiling.
  survival_rate <- survival_rate_floor + (survival_rate_ceiling - survival_rate_floor)*index_vec_residual
  survival_rate
}

survival_rate_to_predicted_time <- function(survival_mat, pred_survival_rate, index_time_fun) {
  time_indice <- rep(NA_real_, nrow(survival_mat))
  for (i in 1:nrow(survival_mat)) {
    # Since survival curve monotonously decreases, we can use findInterval that does O(logN) binary search.
    idx <- findInterval(-pred_survival_rate, -survival_mat[i,])
    # Interpolate if possible.
    # When pred_survival_rate is larger than the first survival rate that appear in survivL_mat[i,].
    if (idx == 0) {
      denom <- survival_mat[i,idx+1] - 1.0
      if (!is.na(denom) && denom != 0) {
        idx <- idx + (pred_survival_rate - 1.0)/denom
      }
    }
    # General case
    else if (!is.na(idx) && idx < ncol(survival_mat)) {
      denom <- survival_mat[i,idx+1] - survival_mat[i,idx]
      if (!is.na(denom) && denom != 0) {
        idx <- idx + (pred_survival_rate - survival_mat[i,idx])/denom
      }
    }
    time_indice[i] <- idx
  }
  res <- index_time_fun(time_indice)
  res
}

#' @export
augment.ranger_survival_exploratory <- function(x, newdata = NULL, data_type = "training",
                                                base_time = NULL, base_time_type = "max", pred_time = NULL, # For point-in-time-based survival rate prediction. base_time_type can be "value", "max", or "today".
                                                pred_survival_rate = NULL, # For survival-rate-based event time prediction.
                                                pred_survival_time = NULL, pred_survival_threshold = NULL, ...) { # For survival-time-based survival rate prediction.
  # For predict() to find the prediction method, ranger needs to be loaded beforehand.
  # This becomes necessary when the model was restored from rds, and model building has not been done in the R session yet.
  loadNamespace("ranger")
  if ("error" %in% class(x)) {
    ret <- data.frame(Note = x$message)
    return(ret)
  }
  if(!is.null(newdata)) {
    # Replay the mutations on predictors.
    if(!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    predictor_variables <- attr(x$df,"predictors")
    # If start time column is in newdata, use it.
    if (!is.null(x$clean_start_time_col) && x$terms_mapping[x$clean_start_time_col] %in% colnames(newdata)) {
      predictor_variables <- c(predictor_variables, x$clean_start_time_col)
    }
    predictor_variables_orig <- x$terms_mapping[predictor_variables]

    # Rename columns via predictor_variables_orig, which is a named vector.
    #TODO: What if names of the other columns conflicts with our temporary name, c1_, c2_...?
    cleaned_data <- newdata %>% dplyr::rename(predictor_variables_orig)

    # Align factor levels including Others and (Missing) to the model. TODO: factor level order can be different from the model training data. Is this ok?
    cleaned_data <- align_predictor_factor_levels(cleaned_data, x$xlevels, predictor_variables)

    na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
    if (length(na_row_numbers) > 0) {
      cleaned_data <- cleaned_data[-na_row_numbers,]
    }
    data <- cleaned_data
    pred <- predict(x, data=data)
  }
  else if (data_type == "training") {
    data <- x$df
    pred <- x$prediction_training
  }
  else { # data_type == "test"
    if (!is.null(x$df_test)) {
      data <- x$df_test
      pred <- x$prediction_test
    }
    else {
      return(data.frame())
    }
  }

  if (is.null(pred_survival_time)) {
    pred_survival_time <- x$pred_survival_time
  }
  if (is.null(pred_survival_threshold)) {
    pred_survival_threshold <- x$pred_survival_threshold
  }

  # Predict survival probability on a specific date.
  # or predict the day that the survival rate drops to the specified value (pred_survival_rate).
  # Used for prediction step based on the model from the Analytics View.
  if (!is.null(pred_time) || !is.null(pred_survival_rate)) {
    # Common logic between point-of-time-based survival rate prediction and rate-based event time prediction.
    time_unit_days <- get_time_unit_days(x$time_unit)
    # Function to convert time to index. Extended to cover 0.
    time_index_fun <- approxfun(c(0, x$unique.death.times), 1:(length(x$unique.death.times)+1))
    # Predict survival probability on the specified date (pred_time).
    if (!is.null(pred_time)) {
      if (base_time_type == "max") {
        base_time <- as.Date(max(cleaned_data[[x$clean_start_time_col]])) # as.Date is to take care of POSIXct column.
      }
      else if (base_time_type == "today") {
        base_time <- lubridate::today()
      } # if base_time_type is "value", use the argument value as is.
      # For casting the time for prediction to an integer days, use ceil to compensate that we ceil in the preprocessing.
      pred_time <- base_time + lubridate::days(ceiling(pred_time * time_unit_days));
      data <- data %>% dplyr::mutate(base_survival_time = as.numeric(!!base_time - as.Date(!!rlang::sym(x$clean_start_time_col)), units = "days")/time_unit_days)
      # as.Date is to handle the case where the start time column is in POSIXct.
      data <- data %>% dplyr::mutate(prediction_survival_time = as.numeric(pred_time - as.Date(!!rlang::sym(x$clean_start_time_col)), units = "days")/time_unit_days)
      base_survival_rate <- survival_time_to_predicted_rate(pred$survival, data$base_survival_time, time_index_fun)
      target_survival_rate <- survival_time_to_predicted_rate(pred$survival, data$prediction_survival_time, time_index_fun)
      data$predicted_survival_rate <- target_survival_rate / base_survival_rate
      # NA means that the specified time is not covered by the predicted survival curve. 
      data <- data %>% dplyr::mutate(note = if_else(is.na(predicted_survival_rate), "Out of range of the predicted survival curve.", NA_character_))
      data <- data %>% dplyr::mutate(base_time = !!base_time)
      data <- data %>% dplyr::mutate(prediction_time = !!pred_time)
    }
    # Predict the day that the survival rate drops to the specified value. (pred_survival_rate should be there.)
    else {
      data <- data %>% dplyr::mutate(survival_rate_for_prediction = !!pred_survival_rate)
      # Predicted survival period
      index_time_fun <- approxfun(0:length(x$unique.death.times), c(0, x$unique.death.times))
      data$predicted_survival_time <- survival_rate_to_predicted_time(pred$survival, pred_survival_rate, index_time_fun)
      # NA means that the specified survival rate is not covered by the predicted survival curve. 
      data <- data %>% dplyr::mutate(note = if_else(is.na(predicted_survival_time), "Didn't meet the threshold.", NA_character_))
      # For casting the survival time to an integer days, use floor to compensate that we ceil in the preprocessing.
      data <- data %>% dplyr::mutate(predicted_event_time = as.Date(!!rlang::sym(x$clean_start_time_col)) + lubridate::days(floor(predicted_survival_time*time_unit_days)))
    }
  }
  # Predict survival probability on the specified duration (pred_survival_time). Still used in the Analytics View itself.
  else {
    unique_death_times <- x$forest$unique.death.times
    survival_time <- max(unique_death_times[unique_death_times <= pred_survival_time])
    survival_time_index <- match(survival_time, unique_death_times)
    predicted_survival_rate <- pred$survival[,survival_time_index]
    predicted_survival <- predicted_survival_rate > pred_survival_threshold
    data$prediction_survival_time <- pred_survival_time
    data$predicted_survival_rate <- predicted_survival_rate
    data$predicted_survival <- predicted_survival
  }
  ret <- data
  # Move those columns as the last columns.
  ret <- ret %>% dplyr::relocate(any_of(c("base_time", "base_survival_time", "prediction_time", "prediction_survival_time",
                                          "predicted_survival_rate", "predicted_survival",
                                          "survival_rate_for_prediction", "predicted_survival_time", "predicted_event_time", "note")), .after=last_col())
  colnames(ret)[colnames(ret) == "predicted_survival"] <- "Predicted Survival"
  colnames(ret)[colnames(ret) == "base_survival_time"] <- "Base Survival Time"
  colnames(ret)[colnames(ret) == "prediction_survival_time"] <- "Prediction Survival Time"
  colnames(ret)[colnames(ret) == "base_time"] <- "Base Time"
  colnames(ret)[colnames(ret) == "prediction_time"] <- "Prediction Time"
  colnames(ret)[colnames(ret) == "predicted_survival_rate"] <- "Predicted Survival Rate"
  colnames(ret)[colnames(ret) == "survival_rate_for_prediction"] <- "Survival Rate for Prediction"
  colnames(ret)[colnames(ret) == "predicted_survival_time"] <- "Predicted Survival Time"
  colnames(ret)[colnames(ret) == "predicted_event_time"] <- "Predicted Event Time"
  colnames(ret)[colnames(ret) == "note"] <- "Note"

  # Convert column names back to the original.
  for (i in 1:length(x$terms_mapping)) {
    converted <- names(x$terms_mapping)[i]
    original <- x$terms_mapping[i]
    colnames(ret)[colnames(ret) == converted] <- original
  }

  colnames(ret)[colnames(ret) == ".time"] <- "Survival Time" # .time is a column name generated by our Command Generator.
  ret
}
