#' Find outlier values
#' ref: https://www.r-bloggers.com/outlier-detection-and-treatment-with-r/
#' @export
detect_outlier <- function (vec, type = "iqr", threshold = NULL) {
  ret <- factor(rep("normal", length(vec)),
                levels = c("lower", "normal", "upper"),
                ordered=TRUE)

  type <- tolower(type)

  switch(type, iqr = {
    q <- quantile(vec, na.rm = TRUE)
    # q is with 0%, 25%, 50%, 75%, 100% quartiles
    # IQR is difference between 75% and 25%
    IQR <- q[4]-q[2]
    upper_whisker <- q[4]+1.5*IQR
    lower_whisker <- q[2]-1.5*IQR

    ret[(vec < lower_whisker)] <- "lower"
    ret[(vec > upper_whisker)] <- "upper"
  }, percentile = {
    if(is.null(threshold)){
      threshold <- 0.95
    }
    if (threshold <= 0 || threshold >= 1) {
      stop("threshold must be between 0 and 1")
    } else if (threshold < 0.5) {
      threshold <- 1-threshold
    }
    q <- quantile(vec, probs = c(1-threshold, threshold), na.rm = TRUE)
    ret[(vec < q[1])] <- "lower"
    ret[(vec > q[2])] <- "upper"
  }, standard_deviation = {
    if(is.null(threshold)){
      threshold <- 2
    }
    if (threshold <= 0) {
      stop("threshold must be larger than 0")
    }
    m <- mean(vec, na.rm = TRUE)
    s <- sd(vec, na.rm = TRUE)
    ret[(vec < m - threshold * s) ] <- "lower"
    ret[(vec > m + threshold * s)] <- "upper"
  })
  ret
}
