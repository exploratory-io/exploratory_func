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
                    start_time = NULL,
                    end_time = NULL,
                    time_unit = "day",
                    predictor_funs = NULL,
                    max_nrow = 50000, # With 50000 rows, taking 6 to 7 seconds on late-2016 Macbook Pro.
                    max_sample_size = NULL, # Half of max_nrow.
                    ntree = 20,
                    nodesize = 12,
                    predictor_n = 12, # so that at least months can fit in it.
                    importance_measure = "permutation", # "permutation" or "impurity".
                    max_pd_vars = NULL,
                    pd_sample_size = 500,
                    pred_survival_time = NULL,
                    pred_survival_threshold = 0.5,
                    predictor_outlier_filter_type = NULL,
                    predictor_outlier_filter_threshold = NULL,
                    seed = 1,
                    test_rate = 0.0,
                    test_split_type = "random" # "random" or "ordered"
                    ){
  # TODO: cleanup code only aplicable to randomForest. this func was started from copy of calc_feature_imp, and still adjusting for lm. 

  if (importance_measure == "permutation") { # For permutation importance, we turn off ranger's importance calculation adn use our own implementation.
    importance_measure_ranger <- "none"
  }
  else {
    importance_measure_ranger <- importance_measure
  }

  # using the new way of NSE column selection evaluation
  # ref: http://dplyr.tidyverse.org/articles/programming.html
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  time_col <- tidyselect::vars_select(names(df), !! rlang::enquo(time))
  start_time_col <- NULL
  end_time_col <- NULL
  if (length(time_col) == 0) { # This means time was NULL
    start_time_col <- tidyselect::vars_select(names(df), !! rlang::enquo(start_time))
    end_time_col <- tidyselect::vars_select(names(df), !! rlang::enquo(end_time))
    time_unit_days <- get_time_unit_days(time_unit, df[[start_time_col]], df[[end_time_col]])
    time_unit <- attr(time_unit_days, "label") # Get label like "day", "week".
    # We are ceiling survival time to make it integer in the specified time unit, just like we do for Survival Curve analytics view.
    # This is to make resulting survival curve to have integer data point in the specified time unit. TODO: Think if this really makes sense here for Cox and Survival Forest.
    df <- df %>% dplyr::mutate(.time = ceiling(as.numeric(!!rlang::sym(end_time_col) - !!rlang::sym(start_time_col), units = "days")/time_unit_days))
    time_col <- ".time"
  }
  status_col <- tidyselect::vars_select(names(df), !! rlang::enquo(status))
  # this evaluates select arguments like starts_with
  orig_selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))

  if (!is.null(predictor_funs)) {
    df <- df %>% mutate_predictors(orig_selected_cols, predictor_funs)
    selected_cols <- names(unlist(predictor_funs))
  }
  else {
    selected_cols <- orig_selected_cols
  }
  # Sort predictors so that the result of permutation importance is stable against change of column order.
  selected_cols <- stringr::str_sort(selected_cols)

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
  clean_start_time_col <- NULL
  clean_end_time_col <- NULL
  if (!is.null(start_time_col)) {
    clean_start_time_col <- name_map[start_time_col]
    clean_end_time_col <- name_map[end_time_col]
  }

  each_func <- function(df) {
    tryCatch({
      if(!is.null(seed)){
        set.seed(seed)
      }

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

      # Temporarily remove unused columns for uniformity. TODO: Revive them when we do that across the product.
      clean_cols_without_names <- clean_cols
      names(clean_cols_without_names) <- NULL # remove names to eliminate renaming effect of select.
      if (is.null(clean_start_time_col)) {
        df <- df %>% dplyr::select(!!!rlang::syms(clean_cols_without_names), !!rlang::sym(clean_time_col), rlang::sym(clean_status_col))
      }
      else {
        df <- df %>% dplyr::select(!!!rlang::syms(clean_cols_without_names), !!rlang::sym(clean_start_time_col), !!rlang::sym(clean_end_time_col), !!rlang::sym(clean_time_col), rlang::sym(clean_status_col))
      }

      df <- preprocess_regression_data_after_sample(df, clean_time_col, clean_cols, predictor_n = predictor_n, name_map = name_map)
      c_cols <- attr(df, 'predictors') # predictors are updated (added and/or removed) in preprocess_post_sample. Catch up with it.
      name_map <- attr(df, 'name_map')

      source_data <- df

      # split training and test data
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
      model <- ranger::ranger(fml, data = df, importance = importance_measure_ranger,
        num.trees = ntree,
        min.node.size = nodesize,
        keep.inbag=TRUE,
        sample.fraction = sample.fraction)
      # these attributes are used in tidy of randomForest TODO: is this good for lm too?
      model$terms_mapping <- names(name_map)
      names(model$terms_mapping) <- name_map
      model$sampled_nrow <- sampled_nrow

      if (length(c_cols) > 1) { # Show importance only when there are multiple variables.
        # get importance to decide variables for partial dependence
        if (importance_measure != "permutation") {
          imp <- ranger::importance(model)
          imp_df <- tibble::tibble( # Use tibble since data.frame() would make variable factors, which breaks things in following steps.
            variable = names(imp),
            importance = imp
            ) %>% dplyr::arrange(-importance)
        }
        else { # For permutation, we use our own implementation. The one from ranger seems too unstable for our use. Maybe it's based on OOB prediction?
          imp_df <- calc_permutation_importance_ranger_survival(model, clean_time_col, clean_status_col, c_cols, df)
        }
        model$imp_df <- imp_df
        imp_vars <- imp_df$variable
      }
      else {
        error <- simpleError("Variable importance requires two or more variables.")
        model$imp_df <- error
        imp_vars <- c_cols
      }

      if (is.null(max_pd_vars)) {
        max_pd_vars <- 20 # Number of most important variables to calculate partial dependences on. This used to be 12 but we decided it was a little too small.
      }
      imp_vars <- imp_vars[1:min(length(imp_vars), max_pd_vars)] # take max_pd_vars most important variables
      model$imp_vars <- imp_vars
      model$partial_dependence <- partial_dependence.ranger_survival_exploratory(model, clean_time_col, vars = imp_vars, n = c(9, min(nrow(df), pd_sample_size)), data = df) # grid of 9 is convenient for both PDP and survival curves.
      model$pred_survival_time <- pred_survival_time
      model$pred_survival_threshold <- pred_survival_threshold
      model$survival_curves <- calc_survival_curves_with_strata(df, clean_time_col, clean_status_col, imp_vars)

      # Calculate concordance.
      # Concordance by model$survival is too bad, most likely because it is out-of-bag prediction. We explictly predict with training data to calculate training concordance.
      prediction_training <- predict(model, data=df)
      model$prediction_training <- prediction_training
      concordance_df <- tibble::tibble(x=calc_mean_survival(prediction_training$survival, prediction_training$unique.death.times), time=df[[clean_time_col]], status=df[[clean_status_col]])

      # The concordance is (d+1)/2, where d is Somers' d. https://cran.r-project.org/web/packages/survival/vignettes/concordance.pdf
      model$concordance <- survival::concordance(survival::Surv(time, status)~x,data=concordance_df)

      model$auc <- survival_auroc(extract_survival_rate_at(prediction_training$survival, prediction_training$unique.death.times, pred_survival_time),
                               df[[clean_time_col]], df[[clean_status_col]], pred_survival_time,
                               revert=TRUE)

      # Add cleaned column names. Used for prediction of survival rate on the specified day, and date the survival probability drops to the specified rate. 
      model$clean_start_time_col <- clean_start_time_col
      model$clean_end_time_col <- clean_end_time_col
      model$clean_time_col <- clean_time_col
      model$clean_status_col <- clean_status_col
      model$time_unit <- time_unit

      if (test_rate > 0) {
        df_test_clean <- cleanup_df_for_test(df_test, df, c_cols)
        na_row_numbers_test <- attr(df_test_clean, "na_row_numbers")
        unknown_category_rows_index <- attr(df_test_clean, "unknown_category_rows_index")

        prediction_test <- predict(model, data=df_test_clean)
        # TODO: Following current convention for the name na.action to keep na row index, but we might want to rethink.
        # We do not keep this for training since na.roughfix should fill values and not delete rows. TODO: Is this comment valid here for survival forest?
        attr(prediction_test, "na.action") <- na_row_numbers_test
        attr(prediction_test, "unknown_category_rows_index") <- unknown_category_rows_index
        model$prediction_test <- prediction_test
        model$df_test <- df_test_clean

        # Calculate concordance.
        concordance_df_test <- tibble::tibble(x=calc_mean_survival(prediction_test$survival, prediction_test$unique.death.times), time=df_test_clean[[clean_time_col]], status=df_test_clean[[clean_status_col]])
        # The concordance is (d+1)/2, where d is Somers' d. https://cran.r-project.org/web/packages/survival/vignettes/concordance.pdf
        model$concordance_test <- survival::concordance(survival::Surv(time, status)~x,data=concordance_df_test)

        model$auc_test <- survival_auroc(extract_survival_rate_at(prediction_test$survival, prediction_test$unique.death.times, pred_survival_time),
                                      df_test_clean[[clean_time_col]], df_test_clean[[clean_status_col]], pred_survival_time,
                                      revert=TRUE)

        model$test_nevent <- sum(df_test_clean[[clean_status_col]], na.rm=TRUE)
      }
      model$df <- df
      model$nevent <- sum(df[[clean_status_col]], na.rm=TRUE)

      if (!is.null(predictor_funs)) {
        model$orig_predictor_cols <- orig_selected_cols
        attr(predictor_funs, "LC_TIME") <- Sys.getlocale("LC_TIME")
        attr(predictor_funs, "sysname") <- Sys.info()[["sysname"]] # Save platform name (e.g. Windows) since locale name might need conversion for the platform this model will be run on.
        attr(predictor_funs, "lubridate.week.start") <- getOption("lubridate.week.start")
        model$predictor_funs <- predictor_funs
      }

      # add special lm_coxph class for adding extra info at glance().
      class(model) <- c("ranger_survival_exploratory", class(model))
      model
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

  ret <- do_on_each_group(clean_df, each_func, name = "model", with_unnest = FALSE)
  # Pass down survival time used for prediction. This is for the post-processing for time-dependent ROC.
  attr(ret, "pred_survival_time") <- pred_survival_time
  # Pass down time unit for prediction step UI.
  attr(ret, "time_unit") <- time_unit
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
      tibble::tibble(Concordance=x$concordance$concordance, `Std Error Concordance`=sqrt(x$concordance$var), `Time-dependent AUC`=x$auc, `Number of Rows`=nrow(x$df), `Number of Events`=x$nevent)
    }
    else {
      data.frame()
    }
  }
  else { # data_type == "test"
    if (!is.null(x$concordance_test)) {
      tibble::tibble(Concordance=x$concordance_test$concordance, `Std Error Concordance`=sqrt(x$concordance_test$var), `Time-dependent AUC`=x$auc_test, `Number of Rows`=nrow(x$df_test), `Number of Events`=x$test_nevent)
    }
    else {
      data.frame()
    }
  }
}

survival_time_to_predicted_rate <- function(survival_mat, time_vec, time_index_fun) {
  index_vec <- time_index_fun(time_vec)
  index_vec_floor <- floor(index_vec)
  index_vec_ceiling <- ceiling(index_vec)
  index_vec_residual <- index_vec - index_vec_floor
  index_matrix_floor <- cbind(1:length(time_vec), index_vec_floor)
  index_matrix_ceiling <- cbind(1:length(time_vec), index_vec_ceiling)
  survival_rate_floor <- survival_mat[index_matrix_floor]
  survival_rate_ceiling <- survival_mat[index_matrix_ceiling]
  # interpolate between survival_rate_floor and survival_rate_ceiling.
  survival_rate <- survival_rate_floor + (survival_rate_ceiling - survival_rate_floor)*index_vec_residual
  survival_rate
}

survival_rate_to_predicted_time <- function(survival_mat, pred_survival_rates) {
  time_indice <- rep(NA_real_, nrow(survival_mat))
  for (i in 1:nrow(survival_mat)) {
    # Since survival curve monotonously decreases, we can use findInterval that does O(logN) binary search.
    idx <- findInterval(-pred_survival_rates[i], -survival_mat[i,])
    # Interpolate if possible.
    if (idx < ncol(survival_mat)) {
      denom <- survival_mat[i,idx+1] - survival_mat[i,idx]
      if (denom != 0) {
        idx <- idx + (pred_survival_rates[i] - survival_mat[i,idx])/denom
      }
    }
    time_indice[i] <- idx
  }
  time_indice
}

#' @export
augment.ranger_survival_exploratory <- function(x, newdata = NULL, data_type = "training",
                                                pred_time = NULL, pred_time_type = "value", # For point-in-time-based survival rate prediction. pred_time_type can be "value", "from_max", or "from_today".
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
    # If start/end time column is in newdata, use it.
    if (!is.null(x$clean_start_time_col) && x$terms_mapping[x$clean_start_time_col] %in% colnames(newdata)) {
      predictor_variables <- c(predictor_variables, x$clean_start_time_col)
    }
    if (!is.null(x$clean_end_time_col) && x$terms_mapping[x$clean_end_time_col] %in% colnames(newdata)) {
      predictor_variables <- c(predictor_variables, x$clean_end_time_col)
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

  # Predict survival probability on the specified date (pred_time),
  # or predict the day that the survival rate drops to the specified value (pred_survival_rate).
  # Used for prediction step based on the model from the Analytics View.
  if (!is.null(pred_time) || !is.null(pred_survival_rate)) {
    browser()
    # Common logic between point-of-time-based survival rate prediction and rate-based event time prediction.
    time_unit_days <- get_time_unit_days(x$time_unit)
    if (x$clean_end_time_col %in% colnames(data)) {
      # End time column is in the input. Calculate current_survival_time based off of it. 
      data <- data %>% dplyr::mutate(current_survival_time = as.numeric(!!rlang::sym(x$clean_end_time_col) - !!rlang::sym(x$clean_start_time_col), units = "days")/time_unit_days)
    }
    else {
      # End time column is not in the input. Assume that all we know is the observation started at the start Calculate current_survival_time based off of it. 
      data <- data %>% dplyr::mutate(current_survival_time = 0)
    }

    time_index_fun <- approxfun(x$unique.death.times, 1:length(x$unique.death.times))
    # Predict survival probability on the specified date (pred_time).
    if (!is.null(pred_time)) {
      if (pred_time_type == "from_max") {
        browser()
        # For casting the time for prediction to an integer days, use ceil to compensate that we ceil in the preprocessing.
        max_time <- max(c(data[[x$clean_start_time_col]], data[[x$clean_end_time_col]]))
        pred_time <- max_time + lubridate::days(ceiling(pred_time * time_unit_days));
      }
      else if (pred_time_type == "from_today") {
        # For casting the time for prediction to an integer days, use ceil to compensate that we ceil in the preprocessing.
        pred_time <- lubridate::today() + lubridate::days(ceiling(pred_time * time_unit_days));
      }
      browser()
      data <- data %>% dplyr::mutate(survival_time_for_prediction = as.numeric(pred_time - !!rlang::sym(x$clean_start_time_col), units = "days")/time_unit_days)
      browser()
      # TODO: Do the rest.
      current_survival_rate <- survival_time_to_predicted_rate(pred$survival, data$current_survival_time, time_index_fun)
      target_survival_rate <- survival_time_to_predicted_rate(pred$survival, data$survival_time_for_prediction, time_index_fun)
      data$predicted_survival_rate <- target_survival_rate / current_survival_rate
      if (x$clean_status_col %in% colnames(data)) {
        # If survival_time_for_prediction is earlier than time 1, return 1.0. If status column is there, drop the rate for dead observations to 0.
        data <- data %>% dplyr::mutate(predicted_survival_rate = if_else(current_survival_time > survival_time_for_prediction, 1.0, if_else(!!rlang::sym(x$clean_status_col), 0, predicted_survival_rate)))
      }
      else {
        # If survival_time_for_prediction is earlier than current_survival_time, return 1.0.
        data <- data %>% dplyr::mutate(predicted_survival_rate = if_else(current_survival_time > survival_time_for_prediction, 1.0, predicted_survival_rate))
      }
      data <- data %>% dplyr::mutate(time_for_prediction = pred_time)
    }
    # Predict the day that the survival rate drops to the specified value. (pred_survival_rate should be there.)
    else {
      browser()
      data <- data %>% dplyr::mutate(survival_rate_for_prediction = !!pred_survival_rate)

      # Experiment - get time from rate
      pred_survival_rates <- rep(pred_survival_rate, nrow(data))
      browser()
      time_indice <- survival_rate_to_predicted_time(pred$survival, pred_survival_rates)
      browser()
    }
  }

  browser()
  unique_death_times <- x$forest$unique.death.times
  survival_time <- max(unique_death_times[unique_death_times <= pred_survival_time])
  survival_time_index <- match(survival_time, unique_death_times)
  predicted_survival_rate <- pred$survival[,survival_time_index]
  predicted_survival <- predicted_survival_rate > pred_survival_threshold
  browser()
  ret <- data
  ret$`Survival Time for Prediction`<- pred_survival_time
  ret$`Predicted Survival Rate`<- predicted_survival_rate
  ret$`Predicted Survival`<- predicted_survival

  # Convert column names back to the original.
  for (i in 1:length(x$terms_mapping)) {
    converted <- names(x$terms_mapping)[i]
    original <- x$terms_mapping[i]
    colnames(ret)[colnames(ret) == converted] <- original
  }

  colnames(ret)[colnames(ret) == ".time"] <- "Survival Time" # .time is a column name generated by our Command Generator.
  ret
}
