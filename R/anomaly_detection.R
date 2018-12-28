
#' NSE version of do_anomaly_detection_
#' @export
do_anomaly_detection <- function(df, time, value = NULL, ...){
  time_col <- col_name(substitute(time))
  value_col <- col_name(substitute(value))
  do_anomaly_detection_(df, time_col, value_col, ...)
}

#' Detect anomaly data
#' @param df Data frame
#' @param time_col Column that has time data
#' @param value_col Column that has value data
#' @param time_unit Time unit for aggregation.
#' @param fun.aggregate Function to aggregate values.
#' @param direction Direction of anomaly. Positive ("posi"), Negative ("neg") or "both".
#' @param longterm Increase anom detection efficacy for time series that are greater than a month.
#' This automatically becomes TRUE if the data is longer than 30 days.
#' @param e_value Whether expected values should be returned.
#' @param na_fill_type - Type of NA fill:
#'                       "previous" - Fill with previous non-NA value.
#'                       "value" - Fill with the value of na_fill_value.
#'                       "interpolate" - Linear interpolation.
#'                       "spline" - Spline interpolation.
#'                       NULL - Skip NA fill. Use this only when you know there is no NA.
#' @param na_fill_value - Value to fill NA when na_fill_type is "value"
#' @param ... extra values to be passed to AnomalyDetection::AnomalyDetectionTs.
#' @export
do_anomaly_detection_ <- function(
  df,
  time_col,
  value_col = NULL,
  time_unit = "day",
  fun.aggregate = sum,
  direction="both",
  e_value=TRUE,
  longterm = NULL,
  # The default is previous. It used to be zero fill, but we found it inconvenient,
  # since it often creates false anomalies in data like stock price where there is no data for weekend.
  na_fill_type = "previous", 
  na_fill_value = 0,
  ...){
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("AnomalyDetection")

  grouped_col <- grouped_by(df)

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
    df <- df[!is.na(df[[value_col]]), ]
  }

  if(lubridate::is.Date(df[[time_col]])) {
    if (time_unit %nin% c("day")) {
      stop("Aggregation level has to be day for Date.")
    }
  } else if(lubridate::is.POSIXct(df[[time_col]])) {
    if (time_unit %nin% c("day", "hour", "min", "sec")) {
      stop("Aggregation level has to be day, hour, min, or sec for POSIXct.")
    }
  } else {
    stop(paste0(time_col, " is not either Date or POSIXct."))
  }

  # remove NA data
  df <- df[!is.na(df[[time_col]]), ]

  if(!direction %in% c("both", "pos", "neg")){
    stop("direction must be 'both', 'pos' or 'neg'")
  }

  pos_anom_col <- avoid_conflict(colnames(df), "is_positive_anomaly")
  pos_val_col <- avoid_conflict(colnames(df), "positive_anomalies")
  neg_anom_col <- avoid_conflict(colnames(df), "is_negative_anomaly")
  neg_val_col <- avoid_conflict(colnames(df), "negative_anomalies")
  exp_val_col <- avoid_conflict(colnames(df), "expected_value")

  # this logic is duplicated between positive and negative direction, so
  # integrated into a function and used in do_anomaly_detection_each
  get_anomalies <- function(data, exp_value_tmp, direction, e_value, value_col, ...){
    # exp_value_tmp is temporary expected values to be overwritten
    anom <- tryCatch({
      AnomalyDetection::AnomalyDetectionTs(data, direction = direction, e_value = e_value, ...)$anoms
    }, error = function(e){
      # found a weired error by twitter data, so should be investigated later
      # filed an issue in https://github.com/exploratory-io/tam/issues/4935
    })
    if(!is.null(anom) && nrow(anom) > 0) {
      # set timezone to timestamp.
      # otherwise fails to find anomaly time in data
      # because of timezone difference
      ret <- data[[time_col]] %in% lubridate::force_tz(as.POSIXct(anom$timestamp), lubridate::tz(data[[time_col]]))
      # values of timestamps are regarded as anomaly values
      # NA_real_(NA compatible with numeric valeus) is used for non-anomaly data
      val <- ifelse(ret, data[[value_col]], NA_real_)
      if(e_value){
        # replace anomaly values with expected values to create expected base line.
        expected_val <- exp_value_tmp
        anom_idx <- 1
        for (i in 1:length(ret)) {
          if (ret[[i]]) {
            expected_val[[i]] <- anom$expected_value[[anom_idx]]
            anom_idx <- anom_idx + 1
          }
        }
      }
    } else {
      # no anomaly case
      ret <- rep(FALSE, nrow(data))
      val <- rep(NA_real_, nrow(data))
      expected_val <- exp_value_tmp
    }
    list(
      ret = ret,
      val = val,
      expected_val = expected_val
    )
  }

  do_anomaly_detection_each <- function(df){
    if(!is.null(grouped_col)){
      # drop grouping columns
      df <- df[, !colnames(df) %in% grouped_col]
    }

    time_col_values <- if (time_unit %in% c("day")) {
      # In this case, convert (possibly) from POSIXct to Date first.
      # If we did this without converting POSIXct to Date, floor_date works, but later at complete stage,
      # data day-light-saving days would be skipped, since the times seq.POSIXt gives and floor_date does not match.
      # We give the time column's timezone to as.Date, so that the POSIXct to Date conversion is done
      # based on that timezone.
      as.Date(df[[time_col]], tz = lubridate::tz(df[[time_col]]))
    } else {
      lubridate::floor_date(df[[time_col]], unit = time_unit)
    }

    aggregated_data <- if (!is.null(value_col)){
      data.frame(
        time = time_col_values,
        value = df[[value_col]]
      ) %>%
        dplyr::filter(!is.na(value)) %>% # filter out NA so that aggregate function does not need to handle NA
        dplyr::group_by(time) %>%
        dplyr::summarise(val = fun.aggregate(value))
    } else {
      value_col <- avoid_conflict(time_col, "count")
      data.frame(
        time = time_col_values
      ) %>%
        dplyr::group_by(time) %>%
        dplyr::summarise(val = n())
    }

    # complete the date time with NA
    aggregated_data <- if(inherits(aggregated_data$time, "Date")){
      aggregated_data %>%
        tidyr::complete(time = seq.Date(min(time), max(time), by = time_unit))
    } else if(inherits(aggregated_data$time, "POSIXct")) {
      aggregated_data %>%
        tidyr::complete(time = seq.POSIXt(min(time), max(time), by = time_unit))
    } else {
      stop("time must be Date or POSIXct.")
    }

    # fill na with zoo
    time_points_vec <- aggregated_data[["time"]]
    values_vec <- aggregated_data[["val"]]
    filled_values_vec <- fill_ts_na(values_vec, time_points_vec, type = na_fill_type, val = na_fill_value)
    aggregated_data <- data.frame(time=time_points_vec, val=filled_values_vec)

    if (is.null(longterm)){
      # set longterm to TRUE if the data range is
      # longer than 30 days (one month)
      timerange <- max(aggregated_data$time, na.rm = TRUE) - min(aggregated_data$time, na.rm = TRUE)
      longterm <- as.numeric(timerange, unit = "days") > 30
    }

    colnames(aggregated_data) <- c(time_col, value_col)

    # time column should be posixct, otherwise AnomalyDetection::AnomalyDetectionTs throws an error.
    # tz="GMT" given to as.POSIXct does not really matter since as.POSIXct on Date seems to always
    # interpret input Date as GMT, and output POSIXct is always with default timezone.
    # To sync the date part of output POSIXct to the input Date, we need to convert it back to GMT
    # with with_tz.
    if (!lubridate::is.POSIXct(aggregated_data[[time_col]])) {
      aggregated_data[[time_col]] <- lubridate::with_tz(as.POSIXct(aggregated_data[[time_col]], tz="GMT"), tzone="GMT")
    }

    data_for_anom <- aggregated_data

    # this will be overwritten by expected values
    expected_values <- aggregated_data[[value_col]]

    if(direction == "both" || direction == "pos"){
      pos <- get_anomalies(data_for_anom, expected_values, "pos", e_value, value_col, longterm = longterm, ...)
      aggregated_data[[pos_anom_col]] <- pos$ret
      aggregated_data[[pos_val_col]] <- pos$val
      expected_values <- pos$expected_val
    }

    if(direction == "both" || direction == "neg"){
      neg <- get_anomalies(data_for_anom, expected_values, "neg", e_value, value_col, longterm = longterm, ...)
      aggregated_data[[neg_anom_col]] <- neg$ret
      aggregated_data[[neg_val_col]] <- neg$val
      expected_values <- neg$expected_val
    }
    if (e_value) {
      aggregated_data[[exp_val_col]] <- expected_values
    }
    aggregated_data
  }

  # Calculation is executed in each group.
  # Storing the result in this name_col and
  # unnesting the result.
  # name_col is not conflicting with grouping columns
  # thanks to avoid_conflict that is used before,
  # this doesn't overwrite grouping columns.
  tmp_col <- avoid_conflict(colnames(df), "tmp_col")
  test <- df %>%
    dplyr::do_(.dots=setNames(list(~do_anomaly_detection_each(.)), tmp_col)) %>%
    dplyr::ungroup() %>%
    unnest_with_drop_(tmp_col)
}
