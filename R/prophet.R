# Wrapper functions around prophet.

# Our time_unit argument is based on floor_date, but we also need to
# pass down the same info to seq.Date/seq.POSIXct, and to do so,
# some values needs to be converted.
to_time_unit_for_seq <- function(time_unit) {
  if (time_unit == "minute") {
    "min"
  }
  else if (time_unit == "second") {
    "sec"
  }
  else {
    time_unit
  }
}

#' NSE version of do_prophet_
#' @export
do_prophet <- function(df, time, value = NULL, periods = 10, holiday = NULL, ...){
  time_col <- col_name(substitute(time))
  value_col <- col_name(substitute(value))
  holiday_col <- col_name(substitute(holiday))
  do_prophet_(df, time_col, value_col, periods, holiday_col = holiday_col, ...)
}

#' Forecast time series data
#' @param df - Data frame
#' @param time_col - Column that has time data
#' @param value_col - Column that has value data
#' @param periods - Number of time periods (e.g. days. unit is determined by time_unit) to forecast.
#' @param time_unit - "second"/"sec", "minute"/"min", "hour", "day", "week", "month", "quarter", or "year".
#' @param include_history - Whether to include history data in forecast or not.
#' @param fun.aggregate - Function to aggregate values.
#' @param na_fill_type - Type of NA fill:
#'                       NULL - Skip NA fill. Default behavior.
#'                       "previous" - Fill with previous non-NA value.
#'                       "value" - Fill with the value of na_fill_value.
#'                       "interpolate" - Linear interpolation.
#'                       "spline" - Spline interpolation.
#' @param na_fill_value - Value to fill NA when na_fill_type is "value"
#' @param ... - extra values to be passed to prophet::prophet. listed below.
#' @param growth - This parameter used to specify type of Trend, which can be "linear" or "logistic",
#'        but now we determine this automatically by cap. It is here just to avoid throwing error from prophet,
#'        (about doubly specifying grouth param by our code and by "...") when old caller calls with this parameter.
#' @param cap - Achievable Maximum Capacity of the value to forecast.
#'        https://facebookincubator.github.io/prophet/docs/forecasting_growth.html
#'        It can be numeric or data frame. When numeric, the value is used as cap for both modeling and forecasting.
#'        When it is a data frame, it should be a future data frame with cap column for forecasting.
#'        When this is specified, the original data frame (df) should also have cap column for modeling.
#'        When either a numeric or a data frame is specified, growth argument for prophet becomes "logistic",
#'        as opposed to default "linear".
#' @param seasonality.prior.scale - Strength of seasonality. Default is 10.
#' @param yearly.seasonality - Whether to return yearly seasonality data.
#' @param weekly.seasonality - Whether to return weekly seasonality data.
#' @param n.changepoints - Number of potential changepoints. Default is 25.
#' @param changepoint.prior.scale - Flexibility of automatic changepoint selection. Default is 0.05.
#' @param changepoints - list of potential changepoints.
#' @param holidays.prior.scale - Strength of holiday effect. Default is 10.
#' @param holidays - Holiday definition data frame.
#' @param mcmc.samples - MCMC samples for full bayesian inference. Default is 0.
#' @param interval.width - Width of uncertainty intervals.
#' @param uncertainty.samples - Number of simulations made for calculating uncertainty intervals. Default is 1000.
#' @export
do_prophet_ <- function(df, time_col, value_col = NULL, periods = 10, time_unit = "day", include_history = TRUE, test_mode = FALSE,
                        fun.aggregate = sum, na_fill_type = NULL, na_fill_value = 0,
                        cap = NULL, floor = NULL, growth = NULL, weekly.seasonality = TRUE, yearly.seasonality = TRUE,
                        daily.seasonality = "auto",
                        holiday_col = NULL, holidays = NULL,
                        regressors = NULL, funs.aggregate.regressors = NULL, regressors_na_fill_type = NULL, regressors_na_fill_value = 0, ...){
  validate_empty_data(df)

  # Pseudo code of preprocessing:
  # ----
  # floor_date
  # if (!is.null(regressors)) {
  #   separate df into history and future
  #   aggregate future df
  # }
  # aggregate history df
  # if (test_mode) {
  #   separate history df into training df and test df based on periods
  # } else {
  #   training df is history df as is
  # }

  # we are making default for weekly/yearly.seasonality TRUE since 'auto' does not behave well.
  # it seems that there are cases that weekly.seasonality is turned off as a side-effect of yearly.seasonality turned off.
  # if that happens, since no seasonality is on, prophet forecast result becomes just a linear trend line,
  # which does not look convincing.
  # since there seems to be cases where 'auto' on yearly.seasonality triggers this situation, we are using TRUE as default.
  # we have not seen any issue on 'auto' on weekly.seasonality, but are not using it for now just to be careful.

  loadNamespace("dplyr")
  # For some reason this needs to be library() instead of loadNamespace() to avoid error.
  # Bug in prophet?
  library("prophet")

  grouped_col <- grouped_by(df)

  if (time_unit == "min") {
    time_unit <- "minute"
  }
  else if (time_unit == "sec") {
    time_unit <- "second"
  }

  # column name validation
  if(!time_col %in% colnames(df)){
    stop(paste0(time_col, " is not in column names"))
  }

  if(time_col %in% grouped_col){
    stop(paste0(time_col, " is grouped. Please ungroup it."))
  }

  if(!is.null(value_col)){
    if (!value_col %in% colnames(df)){
      stop(paste0(value_col, " is not in column names"))
    }
    if(value_col %in% grouped_col){
      stop(paste0(value_col, " is grouped. Please ungroup it."))
    }
  }

  if(!is.null(holiday_col) && is.null(value_col)){
      stop("Value column must be specified to make forecast with Holiday column.")
  }

  if (!is.null(cap) && !is.data.frame(cap) && !is.null(floor) && cap <= floor) {
    # validate this case. otherwise, the error will be misterious "missing value where TRUE/FALSE needed".
    stop("cap must be greater than floor.")
  }

  # To filter NAs on regressor columns
  filter_args <- list() # default empty list
  if (!is.null(regressors)) {
    filter_args <- purrr::map(regressors, function(cname) {
      quo(!is.na(UQ(rlang::sym(cname))))
    })
    names(filter_args) <- NULL
  }

  # Compose arguments to pass to dplyr::summarise.
  summarise_args <- list() # default empty list
  regressor_output_cols <- NULL # Just declaring variable
  if (!is.null(regressors) && !is.null(funs.aggregate.regressors)) {
    summarise_args <- purrr::map2(funs.aggregate.regressors, regressors, function(func, cname) {
      quo(UQ(func)(UQ(rlang::sym(cname))))
    })
    if (!is.null(names(regressors))) {
      regressor_output_cols <- names(regressors)
    }
    else {
      regressor_output_cols <- regressors
    }
    names(summarise_args) <- regressor_output_cols
  }

  # remove rows with NA time
  df <- df[!is.na(df[[time_col]]), ]

  do_prophet_each <- function(df){
    tryCatch({
      # filter the part of external holidays df for this group.
      holidays_df <- NULL
      if (!is.null(holidays)) {
        holidays_df <- holidays
        for (a_grouped_col in grouped_col) {
          if (!is.null(holidays_df[[a_grouped_col]])) {
            holidays_df <- holidays_df[holidays_df[[a_grouped_col]] == df[[a_grouped_col]][[1]],]
          }
        }
      }
      # filter the part of external cap df (future df) for this group.
      cap_df <- NULL
      if (!is.null(cap) && is.data.frame(cap)) {
        cap_df <- cap
        for (a_grouped_col in grouped_col) {
          if (!is.null(cap_df[[a_grouped_col]])) {
            cap_df <- cap_df[cap_df[[a_grouped_col]] == df[[a_grouped_col]][[1]],]
          }
        }
      }
  
      df[[time_col]] <- if (time_unit %in% c("day", "week", "month", "quarter", "year")) {
        # Take care of issue that happened in anomaly detection here for prophet too.
        # In this case, convert (possibly) from POSIXct to Date first.
        # If we did this without converting POSIXct to Date, floor_date works, but later at complete stage,
        # data on day-light-saving days would be skipped, since the times seq.POSIXt gives and floor_date does not match.
        # We give the time column's timezone to as.Date, so that the POSIXct to Date conversion is done
        # based on that timezone.
        lubridate::floor_date(as.Date(df[[time_col]], tz = lubridate::tz(df[[time_col]])), unit = time_unit)
      } else {
        lubridate::floor_date(df[[time_col]], unit = time_unit)
      }
  
      # extract holiday df from main df
      if (is.null(holidays_df) && !is.null(holiday_col)) {
        holidays_df <- df %>%
          dplyr::transmute(
            ds = UQ(rlang::sym(time_col)),
            holiday = UQ(rlang::sym(holiday_col))
          ) %>%
          dplyr::group_by(ds) %>%
          dplyr::summarise(holiday = first(holiday[!is.na(holiday)])) %>% # take first non-NA value for aggregation.
          dplyr::filter(!is.na(holiday))
        # If holiday column is logical, create holiday df with only TRUE rows, with single value "Holiday".
        # If it is numeric, first convert to logical (0:FALSE, Others:TRUE), then do the above.
        if (is.logical(holidays_df$holiday) || is.numeric(holidays_df$holiday)) {
          holidays_df <- holidays_df %>% dplyr::mutate(holiday = as.logical(holiday)) %>% dplyr::filter(holiday) %>% dplyr::mutate(holiday = "Holiday")
        }
        # Passing empty dataframe causes prophet error: "Column `ds` is of unsupported type NULL". Set it back to NULL if empty.
        if (nrow(holidays_df) == 0) {
          holidays_df <- NULL
        }
      }
  
      if(!is.null(grouped_col)){
        # drop grouping columns
        df <- df[, !colnames(df) %in% grouped_col]
      }
  
      aggregated_future_data <- NULL
      # Extra regressor case. separate the df into history and future based on the value is filled or not.
      # Exception is when it value column is not specified (forecast is about number of rows.) AND it is test mode.
      # In this case, we treat entire data as history data, and just let test mode logic to separate it into training and test.
      if (!is.null(regressors) && (!is.null(value_col) || !test_mode)) {
        # filter NAs on regressor columns
        df <- df %>% dplyr::filter(!!!filter_args)
        future_df <- df # keep all rows before df is filtered out.
        if (!is.null(value_col)) { # if value_col is there consider rows with values to be history data.
          df <- df %>% dplyr::filter(!is.na(UQ(rlang::sym(value_col)))) # keep the rows that has values. the ones that do not are for future regressors
        }
        else { # if value_col does not exist, use period to determine the boundary between history and future.
          if (time_unit %in% c("second", "sec")) {
            time_unit_func <- lubridate::seconds
          }
          else if (time_unit %in% c("minute", "min")) {
            time_unit_func <- lubridate::minutes
          }
          else if (time_unit == "hour") {
            time_unit_func <- lubridate::hours
          }
          else if (time_unit == "day") {
            time_unit_func <- lubridate::days
          }
          else if (time_unit == "week") {
            time_unit_func <- lubridate::weeks
          }
          else if (time_unit == "month") {
            time_unit_func <- base::months
          }
          else if (time_unit == "quarter") {
            time_unit_func <- function(x) {
              base::months(3 * x)
            }
          }
          else { # assuming it is year.
            time_unit_func <- lubridate::years
          }
          # Keep the rows older than the history/future boundary, as the history data.
          df <- df %>% dplyr::filter(!!rlang::sym(time_col) <= (max(!!rlang::sym(time_col)) - time_unit_func(!!periods)))
        }
        max_floored_date <- max(df[[time_col]])
        future_df <- future_df %>% dplyr::filter(UQ(rlang::sym(time_col)) > max_floored_date)
  
        # No future external regressor data is provided. For test mode, this is fine, but when it is not, this is a problem.
        if(nrow(future_df) == 0 && !test_mode) {
          # ignore the error if
          # it is caused by subset of
          # grouped data frame
          # to show result of
          # data frames that succeed
          if(is.null(grouped_col) || length(grouped_col) == 0) {
            # Terminology is not consistent here, but we are calling extra regressor "external predictor" on the UI.
            stop("External predictors require future data, or to be run in Test Mode.")
          }
        }
  
        # TODO: in test mode, this is not really necessary. optimize.
        aggregated_future_data <- future_df %>%
          dplyr::transmute( # Keep only time column and regressor columns in future data frame.
            ds = UQ(rlang::sym(time_col)),
            !!!rlang::syms(unname(regressors)) # unname is necessary to avoid error when regressors is named vector.
          ) %>%
          dplyr::group_by(ds) %>%
          dplyr::summarise(!!!summarise_args)
      }
      else if (!is.null(holiday_col)) { # even if there is no extra regressor, if holiday column is there, we need to strip future holiday rows.
        df <- df %>% dplyr::filter(!is.na(UQ(rlang::sym(value_col)))) # keep the rows that has values. the ones that do not are future holiday rows. 
      }
      else if(!is.null(value_col)) { # no-extra regressor case. if value column is specified (i.e. value is not number of rows), filter NA rows.
        df <- df[!is.na(df[[value_col]]), ]
      }
  
  
      # note that prophet only takes columns with predetermined names like ds, y, cap, as input
      aggregated_data <- if (!is.null(value_col) && ("cap" %in% colnames(df))) {
        # preserve cap column if it is there, so that cap argument as future data frame works.
        # apply same aggregation as value to cap.
        df %>%
          dplyr::transmute(
            ds = UQ(rlang::sym(time_col)),
            value = UQ(rlang::sym(value_col)),
            cap_col = cap,
            !!!rlang::syms(unname(regressors)) # this should be able to handle regressor=NULL case fine.
          ) %>%
          # remove NA so that we do not pass data with NA, NaN, or 0 to prophet, which we are not very sure what would happen.
          # we saw a case where rstan crashes with the last row with 0 y value.
          dplyr::filter(!is.na(value)) %>%
          dplyr::group_by(ds) %>%
          dplyr::summarise(y = fun.aggregate(value), cap = fun.aggregate(cap_col), !!!summarise_args)
      } else if (!is.null(value_col)){
        df %>%
          dplyr::transmute(
            ds = UQ(rlang::sym(time_col)),
            value = UQ(rlang::sym(value_col)),
            !!!rlang::syms(unname(regressors)) # this should be able to handle regressor=NULL case fine.
          ) %>%
          dplyr::filter(!is.na(value)) %>% # remove NA so that we do not pass data with NA, NaN, or 0 to prophet
          dplyr::group_by(ds) %>%
          dplyr::summarise(y = fun.aggregate(value), !!!summarise_args)
      } else { # value_col is not specified. The forecast is about number of rows.
        # Note: We ignore cap column in this case for now.
        df %>%
          dplyr::transmute(
            ds = UQ(rlang::sym(time_col)),
            !!!rlang::syms(unname(regressors)) # this should be able to handle regressor=NULL case fine.
          ) %>%
          dplyr::group_by(ds) %>%
          dplyr::summarise(y = n(), !!!summarise_args)
      }
  
      # Fill time column and/or regressor columns as specified by arguments.
      # TODO: Check if this would not have daylight saving days issue we had with anomaly detection.
      if (!is.null(na_fill_type) || !is.null(regressors_na_fill_type)) {
        # complete the date time with NA
        aggregated_data <- if(inherits(aggregated_data$ds, "Date")){
          aggregated_data %>%
            tidyr::complete(ds = seq.Date(min(ds), max(ds), by = to_time_unit_for_seq(time_unit)))
        } else if(inherits(aggregated_data$ds, "POSIXct")) {
          aggregated_data %>%
            tidyr::complete(ds = seq.POSIXt(min(ds), max(ds), by = to_time_unit_for_seq(time_unit)))
        } else {
          stop("time must be Date or POSIXct.")
        }
        # fill NAs in y with zoo
        aggregated_data <- aggregated_data %>% dplyr::mutate(y = fill_ts_na(y, ds, type = na_fill_type, val = na_fill_value))
        for (regressor_col in regressor_output_cols) {
          aggregated_data <- aggregated_data %>% dplyr::mutate(!!sym(regressor_col) := fill_ts_na(!!sym(regressor_col), ds, type = !!regressors_na_fill_type, val = !!regressors_na_fill_value))
        }
      }
  
  
      if (time_unit %in% c("week", "month", "quarter", "year")) { # if time_unit is larger than day (the next level is week), having weekly.seasonality does not make sense.
        weekly.seasonality <- FALSE
        daily.seasonality <- FALSE
      }
      else if (time_unit %in% c("day")) { # if time_unit is larger than hour (the next level is day), having daily.seasonality does not make sense.
        daily.seasonality <- FALSE
      }
      # disabling this logic for now, since setting yearly.seasonality FALSE disables weekly.seasonality too.
      # if (time_unit == "year") { # if time_unit is year (the largest unit), having yearly.seasonality does not make sense.
      #   yearly.seasonality = FALSE
      # }
  
      if (test_mode) {
        # Remove end of aggregated_data as test data to make training data.
  
        # Fill aggregated_data$ds with missing data/time.
        # This is necessary to make forecast period correspond with test period in test mode when there is missing date/time in original aggregated_data$ds.
        # Note that this is only for the purpose of correctly determine where to start test period, and we remove those filled data once that purpose is met.
  
        # Create periodical sequence of time to fill missing date/time
        if (time_unit %in% c("hour", "minute", "second")) { # Use seq.POSIXt for unit smaller than day.
          ts <- seq.POSIXt(as.POSIXct(min(aggregated_data$ds)), as.POSIXct(max(aggregated_data$ds)), by=to_time_unit_for_seq(time_unit))
          if (lubridate::is.Date(aggregated_data$ds)) {
            ts <- as.Date(ts)
          }
        }
        else { # Use seq.Date for unit of day or larger. Using seq.POSIXct for month does not always give first day of month.
          ts <- seq.Date(as.Date(min(aggregated_data$ds)), as.Date(max(aggregated_data$ds)), by=to_time_unit_for_seq(time_unit))
          if (!lubridate::is.Date(aggregated_data$ds)) {
            ts <- as.POSIXct(ts)
          }
        }
        ts_df <- data.frame(ds=ts)
        # ts_df has to be the left-hand side to keep the row order according to time order.
        filled_aggregated_data <- dplyr::full_join(ts_df, aggregated_data, by = c("ds" = "ds"))
        
        training_data <- filled_aggregated_data
        if (periods > nrow(training_data)) {
          stop("The time period set for the Test period is longer than the entire data.")
        }
        training_data <- training_data %>% head(-periods)
  
        # we got correct set of training data by filling missing date/time,
        # but now, filter them out again.
        # by doing so, we affect future table, and skip prediction (interpolation)
        # for all missing date/time, which could be expensive if the training data is sparse.
        # keep the last row even if it does not have training data, to mark the end of training period, which is the start of test period.
        training_data <- training_data %>% dplyr::filter(!is.na(y) | row_number() == n())
      }
      else {
        training_data <- aggregated_data
      }
  
      if (!is.null(cap) && is.data.frame(cap)) {
        # in this case, cap is the future data frame with cap, specified by user.
        # this is a back door to allow user to specify cap column.
        if (!is.null(cap$cap)) {
          growth <- "logistic"
        }
        else {
          growth <- "linear"
          # if future data frame is without cap, use it just as a future data frame.
        }
        m <- prophet::prophet(training_data, fit = FALSE, growth = growth,
                              daily.seasonality = daily.seasonality, weekly.seasonality = weekly.seasonality, yearly.seasonality = yearly.seasonality, holidays = holidays_df, ...)
        # add regressors to the model.
        if (!is.null(regressor_output_cols)) {
          for (regressor in regressor_output_cols) {
            m <- add_regressor(m, regressor)
          }
        }
        m <- fit.prophet(m, training_data)
        forecast <- stats::predict(m, cap_df)
      }
      else {
        if (!is.null(cap)) { # set cap if it is there
          training_data[["cap"]] <- cap
          if (!is.null(floor)) { # set floor if it is there
            training_data[["floor"]] <- floor
          }
        }
        if (!is.null(cap)) { # if cap is set, use logistic. otherwise use linear.
          growth <- "logistic"
        }
        else {
          growth <- "linear"
        }
        m <- prophet::prophet(training_data, fit = FALSE, growth = growth,
                              daily.seasonality = daily.seasonality, weekly.seasonality = weekly.seasonality, yearly.seasonality = yearly.seasonality, holidays = holidays_df, ...)
        if (!is.null(regressor_output_cols)) {
          for (regressor in regressor_output_cols) {
            m <- add_regressor(m, regressor)
          }
        }
        m <- fit.prophet(m, training_data)
        if (time_unit == "hour") {
          time_unit_for_future_dataframe = 3600
        }
        else if (time_unit == "minute") {
          time_unit_for_future_dataframe = 60
        }
        else if (time_unit == "second") {
          time_unit_for_future_dataframe = 1
        }
        else {
          time_unit_for_future_dataframe = time_unit
        }
        future <- prophet::make_future_dataframe(m, periods = periods, freq = time_unit_for_future_dataframe, include_history = include_history) #includes past dates
        if (!is.null(regressor_output_cols)) {
          regressor_data <- aggregated_data %>%
            dplyr::select(-y) %>%
            dplyr::bind_rows(aggregated_future_data)
          if (lubridate::is.Date(regressor_data$ds)) { # make ds POSIXct so that inner_join works. TODO: is is possible that future$ds is POSIXlt??
            regressor_data$ds <- as.POSIXct(regressor_data$ds)
          }
          future <- future %>%
            # inner_join to keep only rows with regressor values.
            # this works for test mode too, since aggregated_future_data part is ignored by inner_join.
            dplyr::inner_join(regressor_data, by=c('ds'='ds'))
        }
        if (!is.null(cap)) { # set cap to future table too, if it is there
          future[["cap"]] <- cap
          if (!is.null(floor)) { # set floor if it is there
            future[["floor"]] <- floor
          }
        }
        forecast <- stats::predict(m, future)
      }
      # with prophet 0.2.1, now forecast$ds is POSIXct. Cast it to Date when necessary so that full_join works.
      if (lubridate::is.Date(aggregated_data$ds)) {
        forecast$ds <- as.Date(forecast$ds)
      }
  
      # Add is_test_data column before joining aggregated original data so that the end of forecast is correctly marked as test data.
      if (test_mode) {
        ret <- forecast %>% dplyr::mutate(is_test_data = dplyr::row_number() > n() - periods) # FALSE for training period, TRUE for test period.
      }
      else {
        ret <- forecast
      }
  
      # Join original aggregated dataframe to forecast dataframe.
      # Extra regressor columns will conflinct in names. We add _effect to the ones from forecast.
      # TODO: Can we safely assume that all conflicts are from extra regressors?
      # If there is future part of aggregated data, bind it too so that extra regressor values for future are also in the output.
      if (!is.null(aggregated_future_data)) {
        ret <- ret %>% dplyr::full_join(dplyr::bind_rows(aggregated_data, aggregated_future_data), by = c("ds" = "ds"), suffix = c("_effect", ""))
      }
      else {
        ret <- ret %>% dplyr::full_join(aggregated_data, by = c("ds" = "ds"), suffix = c("_effect", ""))
      }
      # drop cap_scaled column, which is just scaled capacity, which does not seem informative.
      if ("cap_scaled" %in% colnames(ret)) {
        ret <- ret %>% dplyr::select(-cap_scaled)
      }
      # TODO: Maybe we should take average when MCMC is used and there are multiple delta values for each channge point.
      if (!is.numeric(m$changepoints)) { # m$changepoints seems to become numeric single 0 when empty.
        changepoints_df <- data.frame(ds = m$changepoints, trend_change = m$params$delta[1,])
        # m$changepoints is POSIXct. Cast it to Date when original data (aggregated_data$ds) is Date so that left_join works.
        if (lubridate::is.Date(aggregated_data$ds)) {
          changepoints_df$ds <- as.Date(changepoints_df$ds)
        }
        ret <- ret %>% dplyr::left_join(changepoints_df, by = c("ds" = "ds"))
      }
      else {
        # there is no changepoint.
        ret <- ret %>% dplyr::mutate(trend_change = NA_real_)
      }
  
      # adjust order of output columns
      if ("cap.y" %in% colnames(ret)) { # cap.y exists only when cap is used.
        if ("yearly_upper" %in% colnames(ret)) { # yearly_upper/lower exists only when yearly.seasonality is TRUE
          if ("weekly_upper" %in% colnames(ret)) { # weekly_upper/lower exists only when weekly.seasonality is TRUE
            ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                         yearly, yearly_lower, yearly_upper,
                                         weekly, weekly_lower, weekly_upper,
                                         cap.y, cap.x,
                                         dplyr::everything())
          }
          else {
            ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                         yearly, yearly_lower, yearly_upper,
                                         cap.y, cap.x,
                                         dplyr::everything())
          }
        }
        else {
          if ("weekly_upper" %in% colnames(ret)) { # weekly_upper/lower exists only when weekly.seasonality is TRUE
            ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                         weekly, weekly_lower, weekly_upper,
                                         cap.y, cap.x,
                                         dplyr::everything())
          }
          else {
            ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                         cap.y, cap.x,
                                         dplyr::everything())
          }
        }
      }
      else {
        if ("yearly_upper" %in% colnames(ret)) { # yearly_upper/lower exists only when yearly.seasonality is TRUE
          if ("weekly_upper" %in% colnames(ret)) { # weekly_upper/lower exists only when weekly.seasonality is TRUE
            ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                         yearly, yearly_lower, yearly_upper,
                                         weekly, weekly_lower, weekly_upper,
                                         dplyr::everything())
          }
          else {
            ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                         yearly, yearly_lower, yearly_upper,
                                         dplyr::everything())
          }
        }
        else {
          if ("weekly_upper" %in% colnames(ret)) { # weekly_upper/lower exists only when weekly.seasonality is TRUE
            ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                         weekly, weekly_lower, weekly_upper,
                                         dplyr::everything())
          }
          else {
            ret <- ret %>% dplyr::select(ds, y, yhat, yhat_upper, yhat_lower, trend, trend_upper, trend_lower,
                                         dplyr::everything())
          }
        }
      }
      if (test_mode) { # Bring is_test_data column to the last
        ret <- ret %>% dplyr::select(-is_test_data, is_test_data)
      }
  
      # revive original column names (time_col, value_col)
      if (time_col != "ds") { # if time_col happens to be "ds", do not do this, since it will make the column name "ds.new".
        colnames(ret)[colnames(ret) == "ds"] <- avoid_conflict(colnames(ret), time_col)
      }
      if (is.null(value_col)) {
        value_col <- "count"
      }
      if (value_col != "y") { # if value_col happens to be "y", do not do this, since it will make the column name "y.new".
        colnames(ret)[colnames(ret) == "y"] <- avoid_conflict(colnames(ret), value_col)
      }
  
      # adjust column name style
      colnames(ret)[colnames(ret) == "yhat"] <- avoid_conflict(colnames(ret), "forecasted_value")
      colnames(ret)[colnames(ret) == "yhat_upper"] <- avoid_conflict(colnames(ret), "forecasted_value_high")
      colnames(ret)[colnames(ret) == "yhat_lower"] <- avoid_conflict(colnames(ret), "forecasted_value_low")
      colnames(ret)[colnames(ret) == "trend_upper"] <- avoid_conflict(colnames(ret), "trend_high")
      colnames(ret)[colnames(ret) == "trend_lower"] <- avoid_conflict(colnames(ret), "trend_low")
      colnames(ret)[colnames(ret) == "yearly_upper"] <- avoid_conflict(colnames(ret), "yearly_high")
      colnames(ret)[colnames(ret) == "yearly_lower"] <- avoid_conflict(colnames(ret), "yearly_low")
      colnames(ret)[colnames(ret) == "weekly_upper"] <- avoid_conflict(colnames(ret), "weekly_high")
      colnames(ret)[colnames(ret) == "weekly_lower"] <- avoid_conflict(colnames(ret), "weekly_low")
      colnames(ret)[colnames(ret) == "cap.x"] <- avoid_conflict(colnames(ret), "cap_forecast")
      colnames(ret)[colnames(ret) == "cap.y"] <- avoid_conflict(colnames(ret), "cap_model")
      ret
    }, error = function(e){
      if(length(grouped_col) > 0) {
        # ignore the error if
        # it is caused by subset of
        # grouped data frame
        # to show result of
        # data frames that succeed.
        # For debugging purpose, return one row with error message in note column.
        data.frame(note = e$message)
      } else {
        stop(e)
      }
    })
  }

  # Calculation is executed in each group.
  # Storing the result in this name_col and
  # unnesting the result.
  # name_col is not conflicting with grouping columns
  # thanks to avoid_conflict that is used before,
  # this doesn't overwrite grouping columns.
  tmp_col <- avoid_conflict(colnames(df), "tmp_col")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~do_prophet_each(.)), tmp_col)) %>%
    dplyr::ungroup()
  ret <- ret %>%  unnest_with_drop_(tmp_col)

  if (length(grouped_col) > 0) {
    ret <- ret %>% dplyr::group_by(!!!rlang::syms(grouped_col))
  }
  ret
}
