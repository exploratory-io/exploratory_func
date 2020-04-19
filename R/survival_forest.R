calc_survival_curves_with_strata <- function(df, time_col, status_col, vars) {
  # Create map from variable name to chart type. TODO: Eliminate duplicated code.
  chart_type_map <- c()
  for(col in colnames(df)) {
    chart_type_map <- c(chart_type_map, is.numeric(df[[col]]))
  }
  chart_type_map <- ifelse(chart_type_map, "line", "scatter")
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
  ret
}

partial_dependence.ranger_survival_exploratory <- function(fit, vars = colnames(data),
  n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)), # Keeping same default of 25 as edarf::partial_dependence, although we usually overwrite from callers.
  interaction = FALSE, uniform = TRUE, data, ...) {

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
      mp = do.call(mmpf::marginalPrediction, args)
      mp
    }, simplify = FALSE), fill = TRUE)
    data.table::setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else {
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
  ret <- pd %>% pivot_longer(matches('^V[0-9]+$'),names_to = 'period', values_to = 'survival')
  ret <- ret %>% mutate(period = as.numeric(str_remove(period,'^V')))
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

  ret <- ret %>% pivot_longer(c(-period, -survival) ,names_to = 'variable', values_to = 'value', values_ptype = list(value=character()), values_drop_na=TRUE)
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
                    predictor_n = 12, # so that at least months can fit in it.
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
        dplyr::filter(!is.na(df[[time_col]]) & !is.infinite(df[[time_col]])) # this form does not handle group_by. so moved into each_func from outside.
      df <- df %>%
        dplyr::filter(!is.na(df[[time_col]])) # this form does not handle group_by. so moved into each_func from outside.

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
      rf <- ranger::ranger(fml, data = df, importance = 'permutation')
      # these attributes are used in tidy of randomForest TODO: is this good for lm too?
      rf$terms_mapping <- names(name_map)
      names(rf$terms_mapping) <- name_map
      rf$sampled_nrow <- sampled_nrow

      rf$partial_dependence <- partial_dependence.ranger_survival_exploratory(rf, vars = clean_cols, n = c(5, 25), data = df)
      rf$survival_curves <- calc_survival_curves_with_strata(df, clean_time_col, clean_status_col, clean_cols)

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
      class(x) <- 'ranger' # This seems to be necessary to make ranger::importance work, eliminating ranger_survival_exploratory.
      importance_vec <- ranger::importance(x)
      ret <- tibble::tibble(variable=names(importance_vec), importance=importance_vec)
      ret
    },
    partial_dependence_survival_curve = {
      ret <- x$partial_dependence
      ret <- ret %>% mutate(chart_type = 'line')
      ret
    },
    partial_dependence = {
      ret <- x$partial_dependence
      period_to_plot <- quantile(ret$period, 0.5, type=1)
      ret <- ret %>% dplyr::filter(period == !!period_to_plot) %>%
        mutate(type='Prediction')
      actual <- x$survival_curves %>%
        filter(period <= !!period_to_plot) %>%
        group_by(variable, value) %>% filter(period == max(period)) %>% ungroup() %>%
        mutate(type='Actual')
      ret <- ret %>% dplyr::bind_rows(actual)
      ret
    })
}
