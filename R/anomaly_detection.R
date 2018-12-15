
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
#'                       "spline" - Spline interpolation.
#'                       "interpolate" - Linear interpolation.
#'                       "previous" - Fill with last previous non-NA value.
#'                       "value" - Fill with the value of na_fill_value.
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
  na_fill_type = "value",
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

    aggregated_data <- if (!is.null(value_col)){
      data.frame(
        time = lubridate::floor_date(df[[time_col]], unit = time_unit),
        value = df[[value_col]]
      ) %>%
        dplyr::filter(!is.na(value)) %>% # filter out NA so that aggregate function does not need to handle NA
        dplyr::group_by(time) %>%
        dplyr::summarise(val = fun.aggregate(value))
    } else {
      value_col <- avoid_conflict(time_col, "count")
      data.frame(
        time = lubridate::floor_date(df[[time_col]], unit = time_unit)
      ) %>%
        dplyr::group_by(time) %>%
        dplyr::summarise(count = n())
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

    # keep time_col column, since we will drop it in the next step,
    # but will need it to compose zoo object.
    time_points_vec <- aggregated_data[["time"]]

    # drop time_col.
    input_df <- aggregated_data[, colnames(aggregated_data) != "time"]

    df_zoo <- zoo::zoo(input_df, time_points_vec)
    # fill NAs in the input
    # when some date or time are missing,
    # AnomalyDetection::AnomalyDetectionTs throws this error
    # "Anom detection needs at least 2 periods worth of data"
    if (na_fill_type == "spline") {
      df_zoo <- zoo::na.spline(df_zoo)
    }
    else if (na_fill_type == "interpolate") {
      df_zoo <- zoo::na.approx(df_zoo)
    }
    else if (na_fill_type == "previous") {
      df_zoo <- zoo::na.locf(df_zoo)
    }
    # TODO: Getting this error with some input with na.StructTS().
    #       Error in rowSums(tsSmooth(StructTS(y))[, -2]) : 'x' must be an array of at least two dimensions
    #
    # else if (na_fill_type == "StructTS") {
    #   df_zoo <- zoo::na.StructTS(df_zoo)
    # }
    else if (na_fill_type == "value") {
      df_zoo <- zoo::na.fill(df_zoo, na_fill_value)
    }
    else if (is.null(na_fill_type)) {
      # skip when it is NULL. this is for the case caller is confident that
      # there is no NA and want to skip overhead of checking for NA.
    }
    else {
      stop(paste0(na_fill_type, " is not a valid na_fill_type option."))
    }
    aggregated_data <- df_zoo %>%
      as.data.frame() %>%
      dplyr::mutate(time = zoo::index(df_zoo)) %>%
      # bring time column first
      dplyr::select(time, everything())

    if (is.null(longterm)){
      # set longterm to TRUE if the data range is
      # longer than 30 days (one month)
      timerange <- max(aggregated_data$time, na.rm = TRUE) - min(aggregated_data$time, na.rm = TRUE)
      longterm <- as.numeric(timerange, unit = "days") > 30
    }

    colnames(aggregated_data) <- c(time_col, value_col)

    # time column should be posixct, otherwise AnomalyDetection::AnomalyDetectionTs throws an error
    if (!is.POSIXct(aggregated_data[[time_col]])) {
      aggregated_data[[time_col]] <- with_tz(as.POSIXct(aggregated_data[[time_col]], tz="GMT"), tzone="GMT")
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
