#' @export
do_anomaly_detection <- function(df, time, value, direction="both", e_value=TRUE, ...){
  loadNamespace("dplyr")
  loadNamespace("AnomalyDetection")

  if(!direction %in% c("both", "pos", "neg")){
    stop("derection must be 'both', 'pos' or 'neg'")
  }

  time_col <- col_name(substitute(time))
  value_col <- col_name(substitute(value))

  pos_anom_col <- avoid_conflict(colnames(df), "pos_anomaly")
  pos_val_col <- avoid_conflict(colnames(df), "pos_value")
  neg_anom_col <- avoid_conflict(colnames(df), "neg_anomaly")
  neg_val_col <- avoid_conflict(colnames(df), "neg_value")
  exp_val_col <- avoid_conflict(colnames(df), "expected_value")

  do_anomaly_detection_each <- function(df){
    data <- df[, c(time_col, value_col)]
    expected_values <- df[[value_col]]
    if(direction == "both" || direction == "pos"){
      pos_anom <- AnomalyDetection::AnomalyDetectionTs(data, direction = "pos", e_value = e_value, ...)$anoms
      if(nrow(pos_anom) > 0) {
        pos_ret <- data[[time_col]] %in% as.POSIXct(pos_anom$timestamp)
        pos_val <- ifelse(pos_ret, data[[value_col]], NA_real_)
        if(e_value){
          expected_values <- ifelse(pos_ret, pos_anom[["expected_value"]], expected_values)
        }
      } else {
        pos_ret <- rep(FALSE, nrow(df))
        pos_val <- rep(NA, nrow(df))
      }
      df[[pos_anom_col]] <- pos_ret
      df[[pos_val_col]] <- pos_val
    }

    if(direction == "both" || direction == "neg"){
      neg_anom <- AnomalyDetection::AnomalyDetectionTs(data, direction = "neg", e_value = e_value,...)$anoms
      if(nrow(neg_anom) > 0) {
        neg_ret <- data[[time_col]] %in% as.POSIXct(neg_anom$timestamp)
        neg_val <- ifelse(neg_ret, data[[value_col]], NA)
        if(e_value){
          expected_values <- ifelse(neg_ret, neg_anom[["expected_value"]], expected_values)
        }
      } else {
        neg_ret <- rep(FALSE, nrow(df))
        neg_val <- rep(NA, nrow(df))
      }
      df[[neg_anom_col]] <- neg_ret
      df[[neg_val_col]] <- neg_val
    }
    if (e_value) {
      df[[exp_val_col]] <- expected_values
    }
    df
  }
  tmp_col <- avoid_conflict(colnames(df), "tmp_col")
  test <- (df %>%  dplyr::do_(.dots=setNames(list(~do_anomaly_detection_each(.)), tmp_col)))
  test %>%  tidyr::unnest_(tmp_col)
}
