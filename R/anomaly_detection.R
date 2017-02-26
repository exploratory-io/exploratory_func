
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
#' @param e_value Whether expected values should be returned.
#' @param ... extra values to be passed to AnomalyDetection::AnomalyDetectionTs.
#' @export
do_anomaly_detection_ <- function(df, time_col, value_col = NULL, time_unit = "day", fun.aggregate = sum, direction="both", e_value=TRUE, ...){

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

  pos_anom_col <- avoid_conflict(colnames(df), "pos_anomaly")
  pos_val_col <- avoid_conflict(colnames(df), "pos_value")
  neg_anom_col <- avoid_conflict(colnames(df), "neg_anomaly")
  neg_val_col <- avoid_conflict(colnames(df), "neg_value")
  exp_val_col <- avoid_conflict(colnames(df), "expected_value")

  # this logic is duplicated between positive and negative direction, so
  # integrated into a function and used in do_anomaly_detection_each
  get_anomalies <- function(data, exp_value_tmp, direction, e_value, value_col, ...){
    # exp_value_tmp is temporary expected values to be overwritten
    anom <- tryCatch({
      AnomalyDetection::AnomalyDetectionTs(data, direction = direction, e_value = e_value, ...)$anoms
    }, error = function(e){
      if(e$message == "Anom detection needs at least 2 periods worth of data") {
        stop("Not enough data to detect anomaly. Try smaller time unit.")
      }
    })
    if(nrow(anom) > 0) {
      ret <- data[[time_col]] %in% as.POSIXct(anom$timestamp)
      # values of timestamps are regarded as anomaly values
      # NA_real_(NA compatible with numeric valeus) is used for non-anomaly data
      val <- ifelse(ret, data[[value_col]], NA_real_)
      if(e_value){
        # positive anomaly values overwrite expected_values
        expected_val <- ifelse(ret, anom[["expected_value"]], exp_value_tmp)
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
        time = lubridate::round_date(df[[time_col]], unit = time_unit),
        value = df[[value_col]]
      ) %>%
        dplyr::group_by(time) %>%
        dplyr::summarise(val = fun.aggregate(value))
    } else {
      value_col <- avoid_conflict(time_col, "count")
      data.frame(
        time = lubridate::round_date(df[[time_col]], unit = time_unit)
      ) %>%
        dplyr::group_by(time) %>%
        dplyr::summarise(count = n())
    }

    colnames(aggregated_data) <- c(time_col, value_col)

    # time column should be posixct, otherwise AnomalyDetection::AnomalyDetectionTs throws an error
    aggregated_data[[time_col]] <- as.POSIXct(aggregated_data[[time_col]])
    data_for_anom <- aggregated_data

    # this will be overwritten by expected values
    expected_values <- aggregated_data[[value_col]]

    if(direction == "both" || direction == "pos"){
      pos <- get_anomalies(data_for_anom, expected_values, "pos", e_value, value_col, ...)
      aggregated_data[[pos_anom_col]] <- pos$ret
      aggregated_data[[pos_val_col]] <- pos$val
      expected_values <- pos$expected_val
    }

    if(direction == "both" || direction == "neg"){
      neg <- get_anomalies(data_for_anom, expected_values, "neg", e_value, value_col, ...)
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
  test <- (df %>%  dplyr::do_(.dots=setNames(list(~do_anomaly_detection_each(.)), tmp_col)))
  test %>%  tidyr::unnest_(tmp_col)
}
