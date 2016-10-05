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
