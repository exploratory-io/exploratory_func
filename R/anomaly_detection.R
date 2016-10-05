#' @export
do_anomaly_detection <- function(df, time, value, ...){
  loadNamespace("dplyr")
  loadNamespace("AnomalyDetection")

  time_col <- col_name(substitute(time))
  value_col <- col_name(substitute(value))

  do_anomaly_detection_each <- function(df){
    data <- df[, c(time_col, value_col)]
    ret <- AnomalyDetection::AnomalyDetectionTs(data, ...)$anoms
  }

  result_col <- "result"
  (df %>%  dplyr::do_(.dots=setNames(list(~do_anomaly_detection_each(.)), result_col)) %>%  tidyr::unnest_(result_col))
}
