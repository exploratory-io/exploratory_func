#' Guess missing values by lm
#' @export
impute_na <- function(target, ..., type = "lm_predict") {
  ret <- switch(type, lm_predict = {
    # list(...) is a list of vectors
    df <- as.data.frame(list(...))
    df$target <- target
    lm_model <- lm(data = df, target ~ ., na.action = na.omit)
    # even if the original column is integer,
    # predicted values can be double and they are appropriate
    # so it's converted to double
    ret <- as.double(target)
    try({
      ret[is.na(ret)] <- suppressWarnings({
        # predict where target is NA
        predict(lm_model, df[is.na(target),])
      })
    })
    ret
  }, mean = {
    val <- mean(target, na.rm = TRUE)
    target[is.na(target)] <- val
    target
  }, median = {
    val <- median(target, na.rm = TRUE)
    target[is.na(target)] <- val
    target
  })
  ret
}
