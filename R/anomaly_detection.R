
#' NSE version of do_anomaly_detection_
#' @export
do_anomaly_detection <- function(df, time, value, ...){
  time_col <- col_name(substitute(time))
  value_col <- col_name(substitute(value))
  do_anomaly_detection_(df, time_col, value_col, ...)
}

#' Detect anomaly data
#' @param df Data frame
#' @param time_col Column that has time data
#' @param value_col Column that has value data
#' @param direction Direction of anomaly. Positive ("posi"), Negative ("neg") or "both".
#' @param e_value Whether expected values should be returned.
#' @param ... extra values to be passed to AnomalyDetection::AnomalyDetectionTs.
#' @export
do_anomaly_detection_ <- function(df, time_col, value_col, direction="both", e_value=TRUE, ...){
  loadNamespace("dplyr")
  loadNamespace("AnomalyDetection")

  if(!direction %in% c("both", "pos", "neg")){
    stop("direction must be 'both', 'pos' or 'neg'")
  }

  pos_anom_col <- avoid_conflict(colnames(df), "pos_anomaly")
  pos_val_col <- avoid_conflict(colnames(df), "pos_value")
  neg_anom_col <- avoid_conflict(colnames(df), "neg_anomaly")
  neg_val_col <- avoid_conflict(colnames(df), "neg_value")
  exp_val_col <- avoid_conflict(colnames(df), "expected_value")

  grouped_col <- grouped_by(df)

  do_anomaly_detection_each <- function(df){
    if(!is.null(grouped_col)){
      # drop grouping columns
      df <- df[, !colnames(df) %in% grouped_col]
    }

    # AnomalyDetection::AnomalyDetectionTs expects
    # a data frame whose first column is time column
    # and second column is value column, so it's created
    data <- df[, c(time_col, value_col)]
    # time column should be posixct, otherwise AnomalyDetection::AnomalyDetectionTs throws an error
    data[[time_col]] <- as.POSIXct(data[[time_col]])

    # this will be overwritten by expected values
    expected_values <- df[[value_col]]

    if(direction == "both" || direction == "pos"){
      # this returns anomaly timestamp in timestamp column
      pos_anom <- AnomalyDetection::AnomalyDetectionTs(data, direction = "pos", e_value = e_value, ...)$anoms
      if(nrow(pos_anom) > 0) {
        pos_ret <- data[[time_col]] %in% as.POSIXct(pos_anom$timestamp)
        # values of timestamps are regarded as anomaly values
        # NA_real_(NA compatible with numeric valeus) is used for non-anomaly data
        pos_val <- ifelse(pos_ret, data[[value_col]], NA_real_)
        if(e_value){
          # positive anomaly values overwrite expected_values
          expected_values <- ifelse(pos_ret, pos_anom[["expected_value"]], expected_values)
        }
      } else {
        # no anomaly case
        pos_ret <- rep(FALSE, nrow(df))
        pos_val <- rep(NA_real_, nrow(df))
      }
      df[[pos_anom_col]] <- pos_ret
      df[[pos_val_col]] <- pos_val
    }

    if(direction == "both" || direction == "neg"){
      # this returns anomaly timestamp in timestamp column
      neg_anom <- AnomalyDetection::AnomalyDetectionTs(data, direction = "neg", e_value = e_value,...)$anoms
      if(nrow(neg_anom) > 0) {
        neg_ret <- data[[time_col]] %in% as.POSIXct(neg_anom$timestamp)
        # values of timestamps are regarded as anomaly values
        # NA_real_(NA compatible with numeric valeus) is used for non-anomaly data
        neg_val <- ifelse(neg_ret, data[[value_col]], NA_real_)
        if(e_value){
          # negative anomaly values overwrite expected_values
          expected_values <- ifelse(neg_ret, neg_anom[["expected_value"]], expected_values)
        }
      } else {
        # no anomaly case
        neg_ret <- rep(FALSE, nrow(df))
        neg_val <- rep(NA_real_, nrow(df))
      }
      df[[neg_anom_col]] <- neg_ret
      df[[neg_val_col]] <- neg_val
    }
    if (e_value) {
      df[[exp_val_col]] <- expected_values
    }
    df
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
