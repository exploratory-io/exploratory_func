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
calc_efron_log_likelihood <- function(lp, time, status) {
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
  sum(purrr::flatten_dbl(tmp_df$contrib))
}

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
                    seed = 1
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
        # dplyr::filter(!is.na(!!target_col))  TODO: this was not filtering, and replaced it with the next line. check other similar places.
        # for numeric cols, filter NA rows, because lm will anyway do this internally, and errors out
        # if the remaining rows are with single value in any predictor column.
        # filter Inf/-Inf too to avoid error at lm.
        dplyr::filter(!is.na(df[[clean_time_col]]) & !is.infinite(df[[clean_time_col]])) # this form does not handle group_by. so moved into each_func from outside.
      df <- df %>%
        dplyr::filter(!is.na(df[[clean_time_col]])) # this form does not handle group_by. so moved into each_func from outside.

      # sample the data for performance if data size is too large.
      sampled_nrow <- NULL
      if (!is.null(max_nrow) && nrow(df) > max_nrow) {
        # Record that sampling happened.
        sampled_nrow <- max_nrow
        df <- df %>% sample_rows(max_nrow)
      }

      c_cols <- clean_cols
      for(col in clean_cols){
        if(lubridate::is.Date(df[[col]]) || lubridate::is.POSIXct(df[[col]])) {
          c_cols <- setdiff(c_cols, col)

          absolute_time_col <- avoid_conflict(colnames(df), paste0(col, "_abs_time"))
          wday_col <- avoid_conflict(colnames(df), paste0(col, "_w_"))
          day_col <- avoid_conflict(colnames(df), paste0(col, "_day_of_month"))
          yday_col <- avoid_conflict(colnames(df), paste0(col, "_day_of_year"))
          month_col <- avoid_conflict(colnames(df), paste0(col, "_m_"))
          year_col <- avoid_conflict(colnames(df), paste0(col, "_year"))
          new_name <- c(absolute_time_col, wday_col, day_col, yday_col, month_col, year_col)
          names(new_name) <- paste(
            names(name_map)[name_map == col],
            c(
              "_abs_time",
              "_w_",
              "_day_of_month",
              "_day_of_year",
              "_m_",
              "_year"
            ), sep="")

          name_map <- c(name_map, new_name)

          c_cols <- c(c_cols, absolute_time_col, wday_col, day_col, yday_col, month_col, year_col)
          df[[absolute_time_col]] <- as.numeric(df[[col]])
          # turn it into unordered factor since if it is ordered factor, the name of term is broken
          df[[wday_col]] <- factor(lubridate::wday(df[[col]], label=TRUE), ordered=FALSE)
          df[[day_col]] <- lubridate::day(df[[col]])
          df[[yday_col]] <- lubridate::yday(df[[col]])
          # turn it into unordered factor since if it is ordered factor, the name of term is broken
          df[[month_col]] <- factor(lubridate::month(df[[col]], label=TRUE), ordered=FALSE)
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
          # 1. if the data is factor, respect the levels and keep first 10 levels, and make others "Others" level.
          # 2. if the data is ordered factor, turn it into unordered. For ordered factor,
          #    coxph takes polynomial terms (Linear, Quadratic, Cubic, and so on) and use them as variables,
          #    which we do not want for this function.
          if (length(levels(df[[col]])) >= predictor_n + 2) {
            df[[col]] <- forcats::fct_other(factor(df[[col]], ordered=FALSE), keep=levels(df[[col]])[1:predictor_n])
          }
          else {
            df[[col]] <- factor(df[[col]], ordered=FALSE)
          }
        } else if(is.logical(df[[col]])) {
          # 1. convert data to factor if predictors are logical. (as.factor() on logical always puts FALSE as the first level, which is what we want for predictor.)
          # 2. turn NA into (Missing) factor level so that lm will not drop all the rows.
          df[[col]] <- forcats::fct_explicit_na(as.factor(df[[col]]))
        } else if(!is.numeric(df[[col]])) {
          # 1. convert data to factor if predictors are not numeric or logical.
          # 2. sort levels by frequency so that base level is the most frequent category.
          # 3. limit the number of levels in factor by fct_lump.
          #    we use ties.method to handle the case where there are many unique values. (without it, they all survive fct_lump.)
          # 4. turn NA into (Missing) factor level so that lm will not drop all the rows.
          df[[col]] <- forcats::fct_explicit_na(forcats::fct_lump(forcats::fct_infreq(as.factor(df[[col]])), n=predictor_n, ties.method="first"))
        } else {
          # for numeric cols, filter NA rows, because lm will anyway do this internally, and errors out
          # if the remaining rows are with single value in any predictor column.
          # filter Inf/-Inf too to avoid error at lm.
          df <- df %>% dplyr::filter(!is.na(df[[col]]) & !is.infinite(df[[col]]))
        }
      }

      # remove columns with only one unique value
      cols_copy <- c_cols
      for (col in cols_copy) {
        unique_val <- unique(df[[col]])
        if (length(unique_val[!is.na(unique_val)]) == 1) {
          c_cols <- setdiff(c_cols, col)
          df[[col]] <- NULL # drop the column so that SMOTE will not see it. 
        }
      }
      if (length(c_cols) == 0) {
        # Previous version of message - stop("The selected predictor variables are invalid since they have only one unique values.")
        stop("Invalid Predictors: Only one unique value.") # Message is made short so that it fits well in the Summary table.
      }

      # build formula for lm
      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      # TODO: This clean_target_col is actually not a cleaned column name since we want lm to show real name. Clean up our variable name.
      # TODO: see if the above is appropriate for coxph
      fml <- as.formula(paste0("survival::Surv(`", clean_time_col, "`, `", clean_status_col, "`) ~ ", rhs))
      rf <- survival::coxph(fml, data = df)
      # these attributes are used in tidy of randomForest TODO: is this good for lm too?
      rf$terms_mapping <- names(name_map)
      names(rf$terms_mapping) <- name_map
      rf$sampled_nrow <- sampled_nrow

      rf$permutation_importance <- calc_permutation_importance_coxph(rf, clean_time_col, clean_status_col, c_cols, df)

      # get importance to decice variables for partial dependence
      imp_df <- rf$permutation_importance
      imp_df <- imp_df %>% dplyr::arrange(-importance)
      imp_vars <- imp_df$term
      if (is.null(max_pd_vars)) {
        max_pd_vars <- 20 # Number of most important variables to calculate partial dependences on. This used to be 12 but we decided it was a little too small.
      }
      imp_vars <- imp_vars[1:min(length(imp_vars), max_pd_vars)] # take max_pd_vars most important variables
      rf$partial_dependence <- partial_dependence.coxph_exploratory(rf, clean_time_col, vars = imp_vars, n = c(9, 25), data = df) # grid of 9 is convenient for both PDP and survival curves.

      rf$survival_curves <- calc_survival_curves_with_strata(df, clean_time_col, clean_status_col, imp_vars)
      # add special lm_coxph class for adding extra info at glance().
      class(rf) <- c("coxph_exploratory", class(rf))
      rf
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
tidy.coxph_exploratory <- function(x, pretty.name = FALSE, type = 'coefficients', pd_survival_time = NULL, ...) { #TODO: add test
  if ("error" %in% class(x)) {
    ret <- data.frame()
    return(ret)
  }

  switch(type,
    permutation_importance = {
      ret <- x$permutation_importance
      # Map variable names back to the original.
      # as.character is to be safe by converting from factor. With factor, reverse mapping result will be messed up.
      ret$term <- x$terms_mapping[as.character(ret$term)]
      ret
    },
    partial_dependence_survival_curve = {
      ret <- x$partial_dependence
      ret <- ret %>% group_by(variable) %>% nest() %>%
        mutate(data = purrr::map(data,function(df){ # Show only 5 lines out of 9 lines for survival curve.
          if (df$chart_type[[1]] == 'line') {
            df %>% mutate(value_index=as.integer(fct_inorder(value))) %>% filter(value_index %% 2 == 1)
          }
          else {
            df
          }
        })) %>% unnest()
      ret <- ret %>% mutate(chart_type = 'line')
      ret <- ret %>% dplyr::mutate(variable = x$terms_mapping[variable]) # map variable names to original.
      ret
    },
    partial_dependence = {
      ret <- x$partial_dependence
      if (is.null(pd_survival_time)) { # By default, use median.
        pd_survival_time <- quantile(ret$period, 0.5, type=1)
      }
      ret <- ret %>% 
        filter(period <= !!pd_survival_time) %>% # Extract the latest period that does not exceed pd_survival_time
        group_by(variable, value) %>% filter(period == max(period)) %>% ungroup() %>%
        mutate(type='Prediction')
      actual <- x$survival_curves %>%
        filter(period <= !!pd_survival_time) %>% # Extract the latest period that does not exceed pd_survival_time
        group_by(variable, value) %>% filter(period == max(period)) %>% ungroup() %>%
        mutate(type='Actual')
      ret <- ret %>% dplyr::bind_rows(actual)
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
  ret <- broom:::glance.coxph(x, model, pretty.name = pretty.name, ...)

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
