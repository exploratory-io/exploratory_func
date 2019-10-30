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

#' builds cox model quickly by way of sampling or fct_lumn, for analytics view.
#' @export
build_coxph.fast <- function(df,
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
      # add special lm_coxph class for adding extra info at glance().
      class(rf) <- c("coxph_exploratory", class(rf))
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

#' special version of tidy.coxph function to use with build_coxph.fast.
#' @export
tidy.coxph_exploratory <- function(x, pretty.name = FALSE, ...) { #TODO: add test
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

  if (pretty.name){
    colnames(ret)[colnames(ret) == "term"] <- "Term"
    colnames(ret)[colnames(ret) == "statistic"] <- "t Ratio"
    colnames(ret)[colnames(ret) == "p.value"] <- "P Value"
    colnames(ret)[colnames(ret) == "std.error"] <- "Std Error"
    colnames(ret)[colnames(ret) == "estimate"] <- "Estimate"
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

#' special version of glance.coxph function to use with build_coxph.fast.
#' @export
glance.coxph_exploratory <- function(x, pretty.name = FALSE, ...) { #TODO: add test
  ret <- broom:::glance.coxph(x, model, pretty.name = pretty.name, ...)

  for(var in names(x$xlevels)) { # extract base levels info on factor/character columns from lm model
    if(pretty.name) {
      ret[paste0("Base Level of ", var)] <- x$xlevels[[var]][[1]]
    }
    else {
      ret[paste0(var, "_base")] <- x$xlevels[[var]][[1]]
    }
  }
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
