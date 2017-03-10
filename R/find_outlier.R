#' Find outlier values
#' ref: https://www.r-bloggers.com/outlier-detection-and-treatment-with-r/
#' @export
find_outlier <- function (vec) {
  q <- quantile(vec, na.rm = TRUE)
  # q is with 0%, 25%, 50%, 75%, 100% quartiles
  # IQR is difference between 75% and 25%
  IQR <- q[4]-q[2]
  upper_whisker <- q[4]+1.5*IQR
  lower_whisker <- q[2]-1.5*IQR

  (vec < lower_whisker) | (vec > upper_whisker)
}
