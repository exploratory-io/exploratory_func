#' Find outlier values
#' @export
find_outlier <- function (vec) {
  q <- quantile(vec, na.rm = TRUE)
  IQR <- q[4]-q[2]
  upper_whisker <- q[4]+1.5*IQR
  lower_whisker <- q[2]-1.5*IQR

  (vec < lower_whisker) | (vec > upper_whisker)
}
