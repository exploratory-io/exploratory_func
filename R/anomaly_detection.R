#' @export
do_anomaly_detection <- function(df, value, time, direction="both", e_value=TRUE, ...){
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
    expected_values <- NULL
    if(direction == "both" || direction == "pos"){
      pos_anom <- AnomalyDetection::AnomalyDetectionTs(data, direction = "pos", e_value = e_value, ...)$anoms
      pos_ret <- data[[time_col]] %in% as.POSIXct(pos_anom$timestamp)
      pos_val <- ifelse(pos_ret, data[[value_col]], NA)
      df[[pos_anom_col]] <- pos_ret
      df[[pos_val_col]] <- pos_val
      if(e_value){
        df[[exp_val_col]] <- ifelse(pos_ret, pos_anom[["expected_value"]], NA)
      }
    }

    if(direction == "both" || direction == "neg"){
      neg_anom <- AnomalyDetection::AnomalyDetectionTs(data, direction = "neg", e_value = e_value,...)$anoms
      neg_ret <- data[[time_col]] %in% as.POSIXct(neg_anom$timestamp)
      neg_val <- ifelse(neg_ret, data[[value_col]], NA)
      df[[neg_anom_col]] <- neg_ret
      df[[neg_val_col]] <- neg_val
      if(e_value){
        df[[exp_val_col]] <- ifelse(neg_ret, neg_anom[["expected_value"]], df[[exp_val_col]])
      }
    }
    df
  }
  tmp_col <- avoid_conflict(colnames(df), "tmp_col")
  test <- (df %>%  dplyr::do_(.dots=setNames(list(~do_anomaly_detection_each(.)), tmp_col)))
  test %>%  tidyr::unnest_(tmp_col)
}

#' @export
detect_anomaly <- function(value, time = NULL, ...){
  if(is.null(time)){
    anom <- AnomalyDetection::AnomalyDetectionVec(value, ...)$anom
    ret <- seq(length(value)) %in% anom$index
  } else {
    data <- data.frame(time, value)
    anom <- AnomalyDetection::AnomalyDetectionTs(data, ...)$anom
    ret <- ifelse(time %in% as.POSIXct(anom$timestamp), value, NA)
  }
  ret
}
