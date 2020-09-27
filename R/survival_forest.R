

calc_permutation_importance_ranger_survival <- function(fit, time_col, status_col, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(data, vars=var, y=time_col, model=fit, nperm=1,
                                predict.fun = function(object, newdata) {
                                  tibble::tibble(x=rowSums(predict(object,data=newdata)$survival, na.rm=TRUE),status=newdata[[status_col]])
                                },
                                # Use minus log likelyhood (Efron) as loss function, since it is what Cox regression tried to optimise. 
                                loss.fun = function(x,y){
                                  df <- x %>% dplyr::mutate(time=!!y)
                                  1 - survival::concordance(survival::Surv(time, status)~x,data=df)$concordance
                                })
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble::tibble(variable=vars, importance=importances)
  importances_df <- importances_df %>% dplyr::arrange(-importance)
  importances_df
}

# Calculates survival curves with Kaplan-Meier with strata specified by vars argument.
calc_survival_curves_with_strata <- function(df, time_col, status_col, vars) {
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
  ret <- data.table::rbindlist(curve_dfs_list)
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

  predict.fun <- function(object, newdata) {
    predict(object, data=newdata)$survival
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

#' builds cox model quickly by way of sampling or fct_lumn, for analytics view.
#' @export
exp_survival_forest <- function(df,
                    time,
                    status,
                    ...,
                    max_nrow = 50000, # With 50000 rows, taking 6 to 7 seconds on late-2016 Macbook Pro.
                    max_sample_size = NULL, # Half of max_nrow.
                    ntree = 20,
                    nodesize = 12,
                    predictor_n = 12, # so that at least months can fit in it.
                    importance_measure = "permutation", # "permutation" or "impurity".
                    max_pd_vars = NULL,
                    pd_sample_size = 500,
                    pred_survival_time = NULL,
                    predictor_outlier_filter_type = NULL,
                    predictor_outlier_filter_threshold = NULL,
                    seed = 1,
                    test_rate = 0.0,
                    test_split_type = "random" # "random" or "ordered"
                    ){
  # TODO: cleanup code only aplicable to randomForest. this func was started from copy of calc_feature_imp, and still adjusting for lm. 

  # using the new way of NSE column selection evaluation
  # ref: http://dplyr.tidyverse.org/articles/programming.html
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  time_col <- tidyselect::vars_select(names(df), !! rlang::enquo(time))
  status_col <- tidyselect::vars_select(names(df), !! rlang::enquo(status))
  # this evaluates select arguments like starts_with
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  # Sort predictors so that the result of permutation importance is stable against change of column order.
  selected_cols <- sort(selected_cols)

  grouped_cols <- grouped_by(df)

  # remove grouped col or time/status col
  selected_cols <- setdiff(selected_cols, c(grouped_cols, time_col, status_col))

  if(test_rate < 0 | 1 < test_rate){
    stop("test_rate must be between 0 and 1")
  } else if (test_rate == 1){
    stop("test_rate must be less than 1")
  }

  if (any(c(time_col, status_col, selected_cols) %in% grouped_cols)) {
    stop("grouping column is used as variable columns")
  }

  if (predictor_n < 2) {
    stop("Max # of categories for explanatory vars must be at least 2.")
  }

  if(!is.null(seed)){
    set.seed(seed)
  }

  # check status_col.
  if (!is.logical(df[[status_col]])) {
    stop(paste0("Status column (", status_col, ")  must be logical."))
  }

  # check time_col
  if (!is.numeric(df[[time_col]])) {
    stop(paste0("Time column (", time_col, ") must be numeric"))
  }

  if (is.null(pred_survival_time)) {
    # By default, use mean of observations with event.
    # median gave a point where survival rate was still predicted 100% in one of our test case.
    pred_survival_time <- mean((df %>% dplyr::filter(!!rlang::sym(status_col)))[[time_col]], na.rm=TRUE)
    # Pick maximum of existing values equal or less than the actual mean.
    pred_survival_time <- max((df %>% dplyr::filter(!!rlang::sym(time_col) <= !!pred_survival_time))[[time_col]], na.rm=TRUE)
  }

  # cols will be filtered to remove invalid columns
  cols <- selected_cols

  for (col in selected_cols) {
    if(all(is.na(df[[col]]))){
      # remove columns if they are all NA
      cols <- setdiff(cols, col)
      df[[col]] <- NULL # drop the column so that SMOTE will not see it. 
    }
  }

  # randomForest fails if columns are not clean. TODO is this needed?
  #clean_df <- janitor::clean_names(df)
  clean_df <- df # turn off clean_names for lm
  # Replace column names with names like c1_, c2_...
  # _ is so that name part and value part of categorical coefficient can be separated later,
  # even with values that starts with number like "9E".
  names(clean_df) <- paste0("c",1:length(colnames(clean_df)), "_")
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

  clean_status_col <- name_map[status_col]
  clean_time_col <- name_map[time_col]
  clean_cols <- name_map[cols]

  each_func <- function(df) {
    tryCatch({
      df <- df %>%
        dplyr::filter(!is.na(df[[clean_status_col]])) # this form does not handle group_by. so moved into each_func from outside.

      df <- preprocess_regression_data_before_sample(df, clean_time_col, clean_cols)
      clean_cols <- attr(df, 'predictors') # predictors are updated (removed) in preprocess_pre_sample. Catch up with it.

      # sample the data for performance if data size is too large.
      sampled_nrow <- NULL
      if (!is.null(max_nrow) && nrow(df) > max_nrow) {
        # Record that sampling happened.
        sampled_nrow <- max_nrow
        df <- df %>% sample_rows(max_nrow)
      }

      # Remove outliers if specified so.
      # This has to be done before preprocess_regression_data_after_sample, since it can remove rows and reduce number of unique values,
      # just like sampling.
      df <- remove_outliers_for_regression_data(df, clean_time_col, clean_cols,
                                                NULL, #target_outlier_filter_type
                                                NULL, #target_outlier_filter_threshold
                                                predictor_outlier_filter_type,
                                                predictor_outlier_filter_threshold)

      df <- preprocess_regression_data_after_sample(df, clean_time_col, clean_cols, predictor_n = predictor_n, name_map = name_map)
      c_cols <- attr(df, 'predictors') # predictors are updated (added and/or removed) in preprocess_post_sample. Catch up with it.
      name_map <- attr(df, 'name_map')

      # split training and test data
      source_data <- df
      test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
      df <- safe_slice(source_data, test_index, remove = TRUE)
      if (test_rate > 0) {
        df_test <- safe_slice(source_data, test_index, remove = FALSE)
        unknown_category_rows_index_vector <- get_unknown_category_rows_index_vector(df_test, df)
        df_test <- df_test[!unknown_category_rows_index_vector, , drop = FALSE] # 2nd arg must be empty.
        unknown_category_rows_index <- get_row_numbers_from_index_vector(unknown_category_rows_index_vector)
      }

      # build formula for survival forest.
      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      fml <- as.formula(paste0("survival::Surv(`", clean_time_col, "`, `", clean_status_col, "`) ~ ", rhs))
      # all or max_sample_size data will be used for randomForest
      # to grow a tree
      if (is.null(max_sample_size)) { # default to half of max_nrow
        max_sample_size = max_nrow/2
      }
      sample.fraction <- min(c(max_sample_size / max_nrow, 1))
      rf <- ranger::ranger(fml, data = df, importance = importance_measure,
        num.trees = ntree,
        min.node.size = nodesize,
        keep.inbag=TRUE,
        sample.fraction = sample.fraction)
      # these attributes are used in tidy of randomForest TODO: is this good for lm too?
      rf$terms_mapping <- names(name_map)
      names(rf$terms_mapping) <- name_map
      rf$sampled_nrow <- sampled_nrow

      if (length(c_cols) > 1) { # Show importance only when there are multiple variables.
        # get importance to decide variables for partial dependence
        if (F) {
        imp <- ranger::importance(rf)
        imp_df <- tibble::tibble( # Use tibble since data.frame() would make variable factors, which breaks things in following steps.
          variable = names(imp),
          importance = imp
        ) %>% dplyr::arrange(-importance)
        }
        imp_df <- calc_permutation_importance_ranger_survival(rf, clean_time_col, clean_status_col, c_cols, df)
        rf$imp_df <- imp_df
        imp_vars <- imp_df$variable
      }
      else {
        error <- simpleError("Variable importance requires two or more variables.")
        rf$imp_df <- error
        imp_vars <- c_cols
      }

      if (is.null(max_pd_vars)) {
        max_pd_vars <- 20 # Number of most important variables to calculate partial dependences on. This used to be 12 but we decided it was a little too small.
      }
      imp_vars <- imp_vars[1:min(length(imp_vars), max_pd_vars)] # take max_pd_vars most important variables
      rf$imp_vars <- imp_vars
      rf$partial_dependence <- partial_dependence.ranger_survival_exploratory(rf, clean_time_col, vars = imp_vars, n = c(9, min(nrow(df), pd_sample_size)), data = df) # grid of 9 is convenient for both PDP and survival curves.
      rf$pred_survival_time <- pred_survival_time
      rf$survival_curves <- calc_survival_curves_with_strata(df, clean_time_col, clean_status_col, imp_vars)

      if (test_rate > 0) {
        # TODO: Adjust the following code from build_lm.fast for this function.
        # Note: Do not pass df_test like data=df_test. This for some reason ends up predict returning training data prediction.
        # rf$prediction_test <- predict(rf, df_test, se.fit = TRUE)
        # rf$unknown_category_rows_index <- unknown_category_rows_index
      }
      rf$test_index <- test_index
      rf$source_data <- source_data

      # add special lm_coxph class for adding extra info at glance().
      class(rf) <- c("ranger_survival_exploratory", class(rf))
      rf
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # In repeat-by case, we report group-specific error in the Summary table,
        # so that analysis on other groups can go on.
        class(e) <- c("ranger_survival_exploratory", class(e))
        e
      } else {
        stop(e)
      }
    })
  }

  do_on_each_group(clean_df, each_func, name = "model", with_unnest = FALSE)
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

#' @export
augment.ranger_survival_exploratory <- function(x, ...) {
  if ("error" %in% class(x)) {
    ret <- data.frame(Note = x$message)
    return(ret)
  }
  data <- x$source_data
  pred <- predict(x, data=data)

  pred_survival_time <- x$pred_survival_time
  unique_death_times <- x$forest$unique.death.times
  survival_time <- max(unique_death_times[unique_death_times <= pred_survival_time])
  survival_time_index <- match(survival_time, unique_death_times)
  predicted_survival <- pred$survival[,survival_time_index]
  ret <- data
  ret$`Survival Time for Prediction`<- pred_survival_time
  ret$`Predicted Survival Rate`<- predicted_survival

  # Convert column names back to the original.
  for (i in 1:length(x$terms_mapping)) {
    converted <- names(x$terms_mapping)[i]
    original <- x$terms_mapping[i]
    colnames(ret)[colnames(ret) == converted] <- original
  }

  colnames(ret)[colnames(ret) == ".time"] <- "Survival Time" # .time is a column name generated by our Command Generator.
  ret
}
