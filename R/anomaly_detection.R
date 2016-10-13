#' @export
do_anomaly_detection <- function(df, value, time = NULL, ...){
  loadNamespace("dplyr")
  loadNamespace("AnomalyDetection")

  time_col <- col_name(substitute(time))
  value_col <- col_name(substitute(value))

  result_col <- avoid_conflict(colnames(df), "anomaly")

  do_anomaly_detection_each <- function(df){
    if(is.null(time_col)){
      anom <- AnomalyDetection::AnomalyDetectionVec(data[[value_col]], ...)
    } else {
      data <- df[, c(time_col, value_col)]
      anom <- AnomalyDetection::AnomalyDetectionTs(data, ...)
      ret <- df[[time_col]] %in% anom$anoms$timestamp
    }
    df[[result_col]] <- ret
    df
  }
  test <- (df %>%  dplyr::do_(.dots=setNames(list(~do_anomaly_detection_each(.)), result_col)))
  test %>%  tidyr::unnest_(result_col)
}

#' @export
detect_anomaly <- function(value, time = NULL, ...){
  if(is.null(time)){
    anom <- AnomalyDetection::AnomalyDetectionVec(value, ...)$anom
    ret <- seq(length(value)) %in% anom$index
  } else {
    data <- data.frame(time, value)
    anom <- AnomalyDetection::AnomalyDetectionTs(data, ...)$anom
    ret <- time %in% as.POSIXct(anom$timestamp)
  }
  ret
}
