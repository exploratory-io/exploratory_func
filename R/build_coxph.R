#' coxph wrapper with do
#' @return deta frame which has coxph model
#' @param data Data frame to be used as data
#' @param formula Formula for coxph
#' @param ... Parameters to be passed to coxph function
#' @param keep.source Whether source should be kept in source.data column
#' @param augment Whether the result should be augmented immediately
#' @param group_cols A vector with columns names to be used as group columns
#' @param test_rate Ratio of test data
#' @param seed Random seed to control test data sampling
#' @export
build_coxph <- function(data, formula, max_categories = NULL, min_group_size = NULL, ...){
  if(!is.null(min_group_size)) {
    # a group too small (like 2) causes "non-conformable arguments" error in building model.
    # this allows us to filter them out.
    # used for Analytics page.
    data <- data %>% dplyr::filter(n() >= min_group_size)
  }
  preprocess_group_cols <- grouped_by(data)
  if(!is.null(max_categories)) {
    for (col in colnames(data)) {
      if(col %nin% preprocess_group_cols && !is.numeric(data[[col]]) && !is.logical(data[[col]])) {
        # convert data to factor if predictors are not numeric or logical
        # and limit the number of levels in factor by fct_lump
        # used for Analytics page.
        # TODO: should this be done for each group_by group?
        data[[col]] <- forcats::fct_lump(as.factor(data[[col]]), n = max_categories)
      }
    }
  }
  build_model(data = data,
              formula = formula,
              model_func = survival::coxph,
              reserved_colnames =  c(
                # model_coef can add following columns at the next step
                "y.level",
                "term",
                "estimate",
                "std_error",
                "t_ratio",
                "p_value",
                # model_stats can add following columns at the next step
                "edf",
                "deviance",
                "AIC",
                # prediction_survfit can add following columns at the next step
                "time",
                "n.risk",
                "n_risk",
                "n.event",
                "n_event",
                "n.censor",
                "n_censor",
                "estimate",
                "std.error",
                "std_error",
                "conf.high",
                "conf_high",
                "conf.low",
                "conf_low"
              ),
              ...)
}

# Calculates partial dependence for Cox regression.
partial_dependence.coxph_exploratory <- function(fit, time_col, vars = colnames(data),
  n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)), # Keeping same default of 25 as edarf::partial_dependence, although we usually overwrite from callers.
  interaction = FALSE, uniform = TRUE, data, ...) {
  times <- sort(unique(data[[time_col]])) # Keep vector of actual times to map time index to actual time later.

  predict.fun <- function(object, newdata) {
    res <- broom::tidy(survival::survfit(object, newdata = newdata))
    #    time n.risk n.event n.censor estimate.1 estimate.2 estimate.3 estimate.4 estimate.5 estimate.6
    #   <dbl>  <dbl>   <dbl>    <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>      <dbl>
    # 1     5    228       1        0      0.994      0.994      0.995      0.995      0.995      0.994
    # 2    11    227       3        0      0.975      0.978      0.982      0.981      0.980      0.975
    # 3    12    224       1        0      0.969      0.972      0.977      0.977      0.975      0.969
    # 4    13    223       2        0      0.957      0.961      0.968      0.967      0.966      0.957

    # TODO: There is info on confidence interval, but we are not making use of them here.
    est <- as.data.frame(t(res %>% select(starts_with('estimate.'))))
    #                   V1        V2        V3        V4        V5        V6        V7        V8
    # estimate.1 0.9937747 0.9751551 0.9689481 0.9565364 0.9503338 0.9441345 0.9379359 0.9317387
    # estimate.2 0.9943782 0.9775431 0.9719241 0.9606775 0.9550519 0.9494257 0.9437965 0.9381650
    # estimate.3 0.9954157 0.9816588 0.9770575 0.9678330 0.9632114 0.9585842 0.9539495 0.9493078
    # estimate.4 0.9953371 0.9813465 0.9766677 0.9672892 0.9625909 0.9578874 0.9531767 0.9484592
    # estimate.5 0.9950931 0.9803774 0.9754587 0.9656028 0.9606673 0.9557276 0.9507815 0.9458296
    high <- as.data.frame(t(res %>% select(starts_with('conf.high.'))))
    high <- high %>% dplyr::rename_at(vars(starts_with('V')), funs(stringr::str_replace(., 'V', 'H')))
    low <- as.data.frame(t(res %>% select(starts_with('conf.low.'))))
    low <- low %>% dplyr::rename_at(vars(starts_with('V')), funs(stringr::str_replace(., 'V', 'L')))
    res <- dplyr::bind_cols(est, high, low)
    res
  }

  aggregate.fun <- function(x) {
    # na.rm needs to be FALSE, since even when only one of the sample has
    # NA confidende interval, the mean conf int of the rest would not cleanly match with the mean
    # of survival rate. (Survival rate typically would not be NA even when confidence interval is NA.)
    mean(x, na.rm = FALSE)
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
  #        trt age        V1        V2        V3        V4        V5           H1        H2           L1        L2        L3 
  # 1 1.000000  NA 0.9984156 0.9971480 0.9867692 0.9843847 0.9631721... 0.9357729 0.9165525... 0.8988693 0.8679139 0.8532433
  # 2 1.111111  NA 0.9984156 0.9971480 0.9867692 0.9843847 0.9631721... 0.9357729 0.9165525... 0.8988693 0.8679139 0.8532433
  # 3 1.222222  NA 0.9984156 0.9971480 0.9867692 0.9843847 0.9631721... 0.9357729 0.9165525... 0.8988693 0.8679139 0.8532433
  ret <- pd %>% tidyr::pivot_longer(matches('^[VHL][0-9]+$'),names_to = 'period', values_to = 'survival')
  # Format of ret looks like this:
  #   trt   age period survival
  # <dbl> <dbl>  <dbl>    <dbl>
  #     1    NA     V1    0.997
  #     1    NA     V2    0.995
  # ...
  #     1    NA     H1    1.000
  #     1    NA     H2    0.999
  # ...
  #     1    NA     L1    0.984
  #     1    NA     L2    0.981
  ret <- ret %>% tidyr::separate(period, sep=1, into=c("type","period"), remove=TRUE) 
  ret <- ret %>% dplyr::mutate(period = as.numeric(period))
  # Format of ret looks like this:
  #     trt   age period survival
  #   <dbl> <dbl>  <dbl>    <dbl>
  # 1     1    NA      1    0.997
  # 2     1    NA      2    0.995
  # 3     1    NA      3    0.984
  # 4     1    NA      4    0.981
  ret <- ret %>% tidyr::pivot_wider(names_from="type", values_from="survival")
  # Format of ret looks like this:
  #  `age` `sex` period     V     H     L
  # <dbl> <fct>  <dbl>  <dbl> <dbl> <dbl>
  #    39 <NA>       1  0.996     1 0.988
  #    39 <NA>       2  0.992     1 0.979
  #    39 <NA>       3  0.988     1 0.972
  #    39 <NA>       4  0.984     1 0.964
  ret <- ret %>% dplyr::rename(survival=V, conf.high=H, conf.low=L)

  chart_type_map <- c()
  for(col in colnames(ret)) {
    chart_type_map <- c(chart_type_map, is.numeric(ret[[col]]))
  }
  chart_type_map <- ifelse(chart_type_map, "line", "scatter")
  names(chart_type_map) <- colnames(ret)

  ret <- ret %>% tidyr::pivot_longer(c(-period, -survival, -conf.high, -conf.low) ,names_to = 'variable', values_to = 'value', values_ptype = list(value=character()), values_drop_na=TRUE)
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

# Efron's partial log likelihood approximation.
# We use this as the cost function for permutation importance of Cox regression.
# References:
# http://www.math.ucsd.edu/~rxu/math284/slect5.pdf
# https://bydmitry.github.io/efron-tensorflow.html
# https://en.wikipedia.org/wiki/Proportional_hazards_model
# 
# lp - Linear predictor
calc_efron_log_likelihood <- function(lp, time, status) { # TODO: Add a test to validate the outcome of this function.
  if (is.data.frame(time)) { # Since mmpf::permutationImportance passes down time as tibble, convert it to vector. TODO: Do this at more appropriate place.
    time <- time[[1]]
  }
  tmp_df <- tibble::tibble(time=time,
                           status=as.numeric(status), # Handle logical status. TODO: Handle it better.
                           lp=lp,
                           theta=exp(lp))
  tmp_df <- tmp_df %>% dplyr::group_by(time) %>%
    dplyr::summarize(sum_lp_event = sum(lp*status),
                     sum_theta_event = sum(theta*status),
                     num_event = sum(status),
                     sum_theta = sum(theta))

  tmp_df <- tmp_df %>% dplyr::mutate(sum_theta_risk = rev(cumsum(rev(sum_theta))))

  # contrib is contribution to the log likelihood from a point of time.
  tmp_df <- tmp_df %>% dplyr::mutate(contrib = purrr::pmap(list(sum_lp_event, sum_theta_event, num_event, sum_theta_risk),
                                                    function(sum_lp_event, sum_theta_event, num_event, sum_theta_risk) {
    if (num_event == 0) return(0)

    subtraction_term <- 0
    for (l in 0:(num_event-1)) {
      subtraction_term <- subtraction_term + log(sum_theta_risk - l/num_event*sum_theta_event)
    }
    contrib <- sum_lp_event - subtraction_term
    contrib
  }))
  # Sum up the contributions to come up with the log likelihood.
  sum(purrr::flatten_dbl(tmp_df$contrib), na.rm = TRUE)
}

# Calculates permutation importance for Cox regression.
calc_permutation_importance_coxph <- function(fit, time_col, status_col, vars, data) {
   var_list <- as.list(vars)
   importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(data, vars=var, y=time_col, model=fit, nperm=1,
                                predict.fun = function(object,newdata){as.matrix(tibble(lp=predict(object,newdata=newdata),status=newdata[[status_col]]))},
                                # Use minus log likelyhood (Efron) as loss function, since it is what Cox regression tried to optimise. 
                                loss.fun = function(x,y){-calc_efron_log_likelihood(x[,1], y, x[,2])})
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble(term=vars, importance=importances)
  importances_df
}

#' builds cox model quickly by way of sampling or fct_lumn, for analytics view.
#' @export
build_coxph.fast <- function(df,
                    time,
                    status,
                    ...,
                    max_nrow = 50000, # With 50000 rows, taking 6 to 7 seconds on late-2016 Macbook Pro.
                    predictor_n = 12, # so that at least months can fit in it.
                    max_pd_vars = NULL,
                    pd_sample_size = 25, # Because of performance issue, this is kept small unlike other models for which we usually use 500.
                    pred_survival_time = NULL,
                    seed = 1,
                    test_rate = 0.0,
                    test_split_type = "random" # "random" or "ordered"
                    ){
  # TODO: cleanup code only aplicable to randomForest. this func was started from copy of calc_feature_imp, and still adjusting for lm. 

  # using the new way of NSE column selection evaluation
  # ref: http://dplyr.tidyverse.org/articles/programming.html
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  time_col <- dplyr::select_var(names(df), !! rlang::enquo(time))
  status_col <- dplyr::select_var(names(df), !! rlang::enquo(status))
  # this evaluates select arguments like starts_with
  selected_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))

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
  if (!is.numeric(df[[status_col]]) && !is.logical(df[[status_col]])) {
    stop(paste0("Status column (", status_col, ")  must be logical or numeric with values of 1 (dead) or 0 (alive)."))
  }
  if (is.numeric(df[[status_col]])) {
    unique_val <- unique(df[[status_col]])
    sorted_unique_val <- sort(unique_val[!is.na(unique_val)])
    if (!all(sorted_unique_val == c(0,1)) & !all(sorted_unique_val == c(1,2))) {
      # we allow 1,2 too since survivial works with it, but we are not promoting it for simplicity.
      stop(paste0("Status column (", status_col, ")  must be logical or numeric with values of 1 (dead) or 0 (alive)."))
    }
  }

  # check time_col
  if (!is.numeric(df[[time_col]])) {
    stop(paste0("Time column (", time_col, ") must be numeric"))
  }

  if (is.null(pred_survival_time)) { # By default, use median.
    pred_survival_time <- median(df[[time_col]], na.rm=TRUE)
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

      # build formula for lm
      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      fml <- as.formula(paste0("survival::Surv(`", clean_time_col, "`, `", clean_status_col, "`) ~ ", rhs))
      model <- survival::coxph(fml, data = df)
      # these attributes are used in tidy of randomForest TODO: is this good for lm too?
      model$terms_mapping <- names(name_map)
      names(model$terms_mapping) <- name_map
      model$sampled_nrow <- sampled_nrow

      if (length(c_cols) > 1) {
        model$permutation_importance <- calc_permutation_importance_coxph(model, clean_time_col, clean_status_col, c_cols, df)
        # get importance to decide variables for partial dependence
        imp_df <- model$permutation_importance
        imp_df <- imp_df %>% dplyr::arrange(-importance)
        imp_vars <- imp_df$term
      }
      else {
        imp_vars <- c_cols
      }

      if (is.null(max_pd_vars)) {
        max_pd_vars <- 20 # Number of most important variables to calculate partial dependences on. This used to be 12 but we decided it was a little too small.
      }
      imp_vars <- imp_vars[1:min(length(imp_vars), max_pd_vars)] # take max_pd_vars most important variables
      model$imp_vars <- imp_vars
      model$partial_dependence <- partial_dependence.coxph_exploratory(model, clean_time_col, vars = imp_vars, n = c(9, min(nrow(df), pd_sample_size)), data = df) # grid of 9 is convenient for both PDP and survival curves.
      model$pred_survival_time <- pred_survival_time
      model$survival_curves <- calc_survival_curves_with_strata(df, clean_time_col, clean_status_col, imp_vars)

      tryCatch({
        model$vif <- calc_vif(model, model$terms_mapping)
      }, error = function(e){
        model$vif <<- e
      })

      if (test_rate > 0) {
        # TODO: Adjust the following code from build_lm.fast for this function.
        # Note: Do not pass df_test like data=df_test. This for some reason ends up predict returning training data prediction.
        # model$prediction_test <- predict(model, df_test, se.fit = TRUE)
        # model$unknown_category_rows_index <- unknown_category_rows_index
      }
      model$test_index <- test_index
      model$source_data <- source_data

      # add special lm_coxph class for adding extra info at glance().
      class(model) <- c("coxph_exploratory", class(model))
      model
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # In repeat-by case, we report group-specific error in the Summary table,
        # so that analysis on other groups can go on.
        class(e) <- c("coxph_exploratory", class(e))
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
tidy.coxph_exploratory <- function(x, pretty.name = FALSE, type = 'coefficients', ...) { #TODO: add test
  if ("error" %in% class(x)) {
    ret <- data.frame()
    return(ret)
  }

  switch(type,
    permutation_importance = {
      if (is.null(x$permutation_importance)) {
        return(data.frame())
      }
      ret <- x$permutation_importance
      # Add p.value column.
      coef_df <- broom:::tidy.coxph(x)
      ret <- ret %>% mutate(p.value=purrr::map(term, function(var) {
        get_var_min_pvalue(var, coef_df, x)
      }))
      # Map variable names back to the original.
      # as.character is to be safe by converting from factor. With factor, reverse mapping result will be messed up.
      ret$term <- x$terms_mapping[as.character(ret$term)]
      ret
    },
    partial_dependence_survival_curve = {
      ret <- x$partial_dependence
      ret <- ret %>% dplyr::group_by(variable) %>% tidyr::nest() %>%
        mutate(data = purrr::map(data,function(df){
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
          group_by(variable, value, chart_type, value_index, period) %>%
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
        filter(period <= !!pred_survival_time) %>% # Extract the latest period that does not exceed pred_survival_time
        group_by(variable, value) %>% filter(period == max(period)) %>% ungroup() %>%
        select(-conf.high, -conf.low) %>% # Temporarily remove confidence interval to be uniform with other Analytics Views.
        mutate(type='Prediction')
      actual <- x$survival_curves %>%
        filter(period <= !!pred_survival_time) %>% # Extract the latest period that does not exceed pred_survival_time
        group_by(variable, value) %>% filter(period == max(period)) %>% ungroup() %>%
        mutate(type='Actual')
      ret <- actual %>% dplyr::bind_rows(ret) # actual rows need to come first for the order of chart drawing.
      ret <- ret %>% dplyr::mutate(variable = forcats::fct_relevel(variable, !!x$imp_vars)) # set factor level order so that charts appear in order of importance.
      # set order to ret and turn it back to character, so that the order is kept when groups are bound.
      # if it were kept as factor, when groups are bound, only the factor order from the first group would be respected.
      ret <- ret %>% dplyr::arrange(variable) %>% dplyr::mutate(variable = as.character(variable))
      ret <- ret %>% dplyr::mutate(variable = x$terms_mapping[variable]) # map variable names to original.
      ret
    },
    coefficients = {
      ret <- broom:::tidy.coxph(x) # it seems that tidy.lm takes care of glm too
      ret <- ret %>% dplyr::mutate(
        hazard_ratio = exp(estimate)
      )
      base_level_table <- xlevels_to_base_level_table(x$xlevels)
      ret <- ret %>% dplyr::left_join(base_level_table, by="term")

      # Rows with NA estimates are due to perfect multicollinearity. Explain it in Note column.
      # https://www.rdocumentation.org/packages/survival/versions/2.44-1.1/topics/coxph - Take a look at explanation for singular.ok.
      if (any(is.na(ret$estimate))) {
        ret <- ret %>% dplyr::mutate(note=if_else(is.na(estimate), "Dropped most likely due to perfect multicollinearity.", NA_character_))
      }
      # Map coefficient names back to original.
      ret$term <- map_terms_to_orig(ret$term, x$terms_mapping)
      if (pretty.name){
        colnames(ret)[colnames(ret) == "term"] <- "Term"
        colnames(ret)[colnames(ret) == "statistic"] <- "t Ratio"
        colnames(ret)[colnames(ret) == "p.value"] <- "P Value"
        colnames(ret)[colnames(ret) == "std.error"] <- "Std Error"
        colnames(ret)[colnames(ret) == "estimate"] <- "Coefficient"
        colnames(ret)[colnames(ret) == "conf.low"] <- "Conf Low"
        colnames(ret)[colnames(ret) == "conf.high"] <- "Conf High"
        colnames(ret)[colnames(ret) == "hazard_ratio"] <- "Hazard Ratio"
        colnames(ret)[colnames(ret) == "base.level"] <- "Base Level"
        colnames(ret)[colnames(ret) == "note"] <- "Note"
      } else {
        colnames(ret)[colnames(ret) == "statistic"] <- "t_ratio"
        colnames(ret)[colnames(ret) == "p.value"] <- "p_value"
        colnames(ret)[colnames(ret) == "std.error"] <- "std_error"
        colnames(ret)[colnames(ret) == "conf.low"] <- "conf_low"
        colnames(ret)[colnames(ret) == "conf.high"] <- "conf_high"
      }
      ret
    },
    vif = {
      if (!is.null(x$vif) && "error" %nin% class(x$vif)) {
        ret <- vif_to_dataframe(x)
      }
      else {
        ret <- data.frame() # Skip output for this group. TODO: Report error in some way.
      }
      ret
    }
  )
}

#' special version of glance.coxph function to use with build_coxph.fast.
#' @export
glance.coxph_exploratory <- function(x, pretty.name = FALSE, ...) { #TODO: add test
  if ("error" %in% class(x)) {
    ret <- data.frame(Note = x$message)
    return(ret)
  }
  ret <- broom:::glance.coxph(x, pretty.name = pretty.name, ...)

  if(pretty.name) {
    colnames(ret)[colnames(ret) == "r.squared"] <- "R Squared"
    colnames(ret)[colnames(ret) == "adj.r.squared"] <- "Adj R Squared"
    colnames(ret)[colnames(ret) == "sigma"] <- "RMSE"
    colnames(ret)[colnames(ret) == "statistic"] <- "F Ratio"
    colnames(ret)[colnames(ret) == "p.value"] <- "P Value"
    colnames(ret)[colnames(ret) == "df"] <- "Degree of Freedom"
    colnames(ret)[colnames(ret) == "logLik"] <- "Log Likelihood"
    colnames(ret)[colnames(ret) == "deviance"] <- "Deviance"
    colnames(ret)[colnames(ret) == "df.residual"] <- "Residual DF"
    # for coxph
    colnames(ret)[colnames(ret) == "n"] <- "Number of Rows"
    colnames(ret)[colnames(ret) == "nevent"] <- "Number of Events"
    colnames(ret)[colnames(ret) == "statistic.log"] <- "Likelihood Ratio Test"
    colnames(ret)[colnames(ret) == "p.value.log"] <- "Likelihood Ratio Test P Value"
    colnames(ret)[colnames(ret) == "statistic.sc"] <- "Score Test"
    colnames(ret)[colnames(ret) == "p.value.sc"] <- "Score Test P Value"
    colnames(ret)[colnames(ret) == "statistic.wald"] <- "Wald Test"
    colnames(ret)[colnames(ret) == "p.value.wald"] <- "Wald Test P Value"
    colnames(ret)[colnames(ret) == "r.squared.max"] <- "R Squared Max"
    colnames(ret)[colnames(ret) == "concordance"] <- "Concordance"
    colnames(ret)[colnames(ret) == "std.error.concordance"] <- "Std Error Concordance"
  }
  ret
}

#' @export
augment.coxph_exploratory <- function(x, ...) {
  if ("error" %in% class(x)) {
    ret <- data.frame(Note = x$message)
    return(ret)
  }
  # TODO: handle training/test split.
  data <- x$source_data
  ret <- broom:::augment.coxph(x, data = data, ...)

  time <- x$pred_survival_time
  # basehaz returns base cumulative hazard.
  bh <- survival::basehaz(x)
  # create a function to interpolate function that returns cumulative hazard.
  bh_fun <- approxfun(bh$time, bh$hazard)
  cumhaz_base = bh_fun(time)
  # transform linear predictor (.fitted) into predicted_probability.
  # predicted_probability is 1 - (y of survival curve).
  ret <- ret %>% dplyr::mutate(time_for_prediction = time,
                               predicted_probability = 1 - exp(-cumhaz_base * exp(.fitted)))

  # Prettify names.
  colnames(ret)[colnames(ret) == ".fitted"] <- "Linear Predictor"
  colnames(ret)[colnames(ret) == ".se.fit"] <- "Std Error"
  colnames(ret)[colnames(ret) == ".resid"] <- "Residual"
  colnames(ret)[colnames(ret) == ".resid"] <- "Residual"
  colnames(ret)[colnames(ret) == "time_for_prediction"] <- "Survival Time for Prediction"
  colnames(ret)[colnames(ret) == "predicted_probability"] <- "Predicted Survival Rate"

  # Convert column names back to the original.
  for (i in 1:length(x$terms_mapping)) {
    converted <- names(x$terms_mapping)[i]
    original <- x$terms_mapping[i]
    colnames(ret)[colnames(ret) == converted] <- original
  }

  colnames(ret)[colnames(ret) == ".time"] <- "Survival Time" # .time is a column name generated by our Command Generator.
  ret
}
