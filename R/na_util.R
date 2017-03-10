#' Guess missing values by lm
#' @export
predict_na <- function(target, ...) {
  # list(...) is a list of vectors
  df <- as.data.frame(list(...))
  df$target <- target
  lm_model <- lm(data = df, target ~ ., na.action = na.omit)
  ret <- target
  ret[is.na(ret)] <- suppressWarnings({
    # predict where target is NA
    predict(lm_model, df[is.na(target),])
  })

  ret
}
