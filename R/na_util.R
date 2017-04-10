#' Guess missing values by lm
#' @param target Target vector whose NA is filled
#' @param type This can be "mean", "median", "predict", "value" or aggregate function
#' @param val This is effective when type is "value" or "predict".
#' When type is "value", NA is replaced by this value.
#' When type is "predict", this is used to predict NA.
#' @param ... Additional vectors to be used to pridict NA when type is lm_predict
#' @export
impute_na <- function(target, type = mean, val = 0, ...) {
  if(typeof(type) == "closure"){
    # type is function in this case
    val <- type(target[!is.na(target)])
    if(length(val) != 1){
      stop("type function must return one value")
    }
    target[is.na(target)] <- val
    target
  } else {
    switch(type, predict = {
      if(val == 0 || length(val) != length(target)){
        # this is when no predictor columns are chosen
        stop("Please choose predictor columns")
      }
      # list(val, ...) is a list of vectors to predict NA values
      df <- as.data.frame(list(val, ...))
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
    }, value = {
      if(length(val) == 1){
        # if val is length 1, na is filled with the value
        target[is.na(target)] <- val
      } else {
        # if val is not length 1,
        # NA in target is replaced with the value in the same position
        target <- dplyr::coalesce(target, val)
      }
      target
    }, {
      stop(paste0(type, " is not supported as type"))
    })
  }
}
