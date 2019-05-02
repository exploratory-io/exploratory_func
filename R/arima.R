#' @export
do_arima <- function(df, time, value = NULL, periods = 10, ...){
  time_col <- col_name(substitute(time))
  value_col <- col_name(substitute(value))

  do_arima_(df, time_col, value_col, periods, ...)
}

#' Forecast time series data by ARIMA model
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
#' @export
do_arima_ <- function(df, time_col, value_col = NULL, periods = 10,
                     time_unit = "day", include_history = TRUE, test_mode = FALSE,
                     fun.aggregate = sum, na_fill_type = NULL, na_fill_value = 0, ...){
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("forecast")

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

  # remove rows with NA time
  df <- df[!is.na(df[[time_col]]), ]

  do_arima_each <- function(df){
    df[[time_col]] <- if (time_unit %in% c("day", "week", "month", "quarter", "year")) {
      # Take care of issue that happened in anomaly detection here for forecast too.
      # In this case, convert (possibly) from POSIXct to Date first.
      # If we did this without converting POSIXct to Date, floor_date works, but later at complete stage,
      # data on day-light-saving days would be skipped, since the times seq.POSIXt gives and floor_date does not match.
      # We give the time column's timezone to as.Date, so that the POSIXct to Date conversion is done
      # based on that timezone.
      lubridate::floor_date(as.Date(df[[time_col]], tz = lubridate::tz(df[[time_col]])), unit = time_unit)
    } else {
      lubridate::floor_date(df[[time_col]], unit = time_unit)
    }

    if(!is.null(grouped_col)){
      # drop grouping columns
      df <- df[, !colnames(df) %in% grouped_col]
    }

    # remove rows with NA value_col
    df <- df[!is.na(df[[value_col]]), ]

    aggregated_data <- if (!is.null(value_col)){
      df %>%
        dplyr::transmute(
          ds = UQ(rlang::sym(time_col)),
          value = UQ(rlang::sym(value_col)),
        ) %>%
        dplyr::filter(!is.na(value)) %>% # remove NA so that we do not pass data with NA, NaN, or 0 to arima
        dplyr::group_by(ds) %>%
        dplyr::summarise(y = fun.aggregate(value))
    } else {
      data.frame(
        ds = df[[time_col]]
      ) %>%
        dplyr::group_by(ds) %>%
        dplyr::summarise(y = n())
    }

    if (!is.null(na_fill_type)) {
      # complete the date time with NA
      aggregated_data <- if(inherits(aggregated_data$ds, "Date")){
        aggregated_data %>%
          tidyr::complete(ds = seq.Date(min(ds), max(ds), by = time_unit))
      } else if(inherits(aggregated_data$ds, "POSIXct")) {
        aggregated_data %>%
          tidyr::complete(ds = seq.POSIXt(min(ds), max(ds), by = time_unit))
      } else {
        stop("time must be Date or POSIXct.")
      }
      # fill NAs in y with zoo
      aggregated_data <- aggregated_data %>% dplyr::mutate(y = fill_ts_na(y, ds, type = na_fill_type, val = na_fill_value))
    }

    if (test_mode) {
      # Remove end of aggregated_data as test data to make training data.

      # Fill aggregated_data$ds with missing data/time.
      # This is necessary to make forecast period correspond with test period in test mode when there is missing date/time in original aggregated_data$ds.
      # Note that this is only for the purpose of correctly determine where to start test period, and we remove those filled data once that purpose is met.
      ts <- create_ts_seq(aggregated_data$ds, min, max, time_unit)

      ts_df <- data.frame(ds=ts)
      # ts_df has to be the left-hand side to keep the row order according to time order.
      filled_aggregated_data <- dplyr::full_join(ts_df, aggregated_data, by = c("ds" = "ds"))
      
      training_data <- filled_aggregated_data
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

    # TODO: set approciate parameters
    m <- forecast::auto.arima(training_data[, "y"],
                              d=1,
                              seasonal=FALSE,
                              stepwise=FALSE)

    forecast_obj <- forecast::forecast(m, h=periods, level=c(95))
    forecast_df <- as_tibble(forecast_obj)
    ret <- training_data

    # Extract fitted values for training data.
    ret$forecast <- as.numeric(m$fitted) 
    forecast_rows <- tibble(ds=create_ts_seq(ret$ds, max, max, time_unit, start_add=1, to_add=periods),
                            forecast=forecast_df[["Point Forecast"]],
                            upper=forecast_df[["Hi 95"]],
                            lower=forecast_df[["Lo 95"]])

    if (test_mode){
      ret$is_test_data <- FALSE
      forecast_rows$y <- tail(filled_aggregated_data, periods)[["y"]]
      forecast_rows$is_test_data <- TRUE 
    }

    ret <- ret %>% dplyr::bind_rows(forecast_rows)

    # revive original column names (time_col, value_col)
    if (time_col != "ds") { # if time_col happens to be "ds", do not do this, since it will make the column name "ds.new".
      time_col <- avoid_conflict(colnames(ret), time_col)
      colnames(ret)[colnames(ret) == "ds"] <- time_col 
    }
    if (is.null(value_col)) {
      value_col <- "count"
    }
    if (value_col != "y") { # if value_col happens to be "y", do not do this, since it will make the column name "y.new".
      value_col <- avoid_conflict(colnames(ret), value_col) 
      colnames(ret)[colnames(ret) == "y"] <- value_col 
    }

    # adjust column name style
    colnames(ret)[colnames(ret) == "forecast"] <- avoid_conflict(colnames(ret), "forecasted_value")
    colnames(ret)[colnames(ret) == "upper"] <- avoid_conflict(colnames(ret), "forecasted_value_high")
    colnames(ret)[colnames(ret) == "lower"] <- avoid_conflict(colnames(ret), "forecasted_value_low")

    if (test_mode) {
      ret <- ret %>% dplyr::select(-is_test_data, is_test_data)
    }

    ret
  }

  # Calculation is executed in each group.
  # Storing the result in this name_col and
  # unnesting the result.
  # name_col is not conflicting with grouping columns
  # thanks to avoid_conflict that is used before,
  # this doesn't overwrite grouping columns.
  tmp_col <- avoid_conflict(colnames(df), "tmp_col")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~do_arima_each(.)), tmp_col)) %>%
    dplyr::ungroup()
  ret <- ret %>% unnest_with_drop_(tmp_col)

  if (length(grouped_col) > 0) {
    ret <- ret %>% dplyr::group_by(!!!rlang::syms(grouped_col))
  }
  ret
}

create_ts_seq <- function(ds, start_func, to_func, time_unit, start_add=0, to_add=0){
  if (time_unit == "minute") {
    time_unit_for_seq <- "min"
  }
  else if (time_unit == "second") {
    time_unit_for_seq <- "sec"
  }
  else {
    time_unit_for_seq <- time_unit
  }
  # Create periodical sequence of time to fill missing date/time
  if (time_unit %in% c("hour", "minute", "second")) { # Use seq.POSIXt for unit smaller than day.
    ts <- seq.POSIXt(as.POSIXct(start_func(ds) + start_add), as.POSIXct(to_func(ds) + to_add), by=time_unit_for_seq)
    if (lubridate::is.Date(aggregated_data$ds)) {
      ts <- as.Date(ts)
    }
  }
  else { # Use seq.Date for unit of day or larger. Using seq.POSIXct for month does not always give first day of month.
    ts <- seq.Date(as.Date(start_func(ds) + start_add), as.Date(to_func(ds) + to_add), by=time_unit_for_seq)
    if (!lubridate::is.Date(ds)) {
      ts <- as.POSIXct(ts)
    }
  }

  ts
 }

