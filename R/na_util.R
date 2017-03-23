#' Guess missing values by lm
#' @param target Target vector whose NA is filled
#' @param ... Vectors to be used to pridict NA when type is lm_predict
#' @param type This can be "mean", "median", "predict", "value" or aggregate function
#' @paramm val This is effective when type is "value". NA is replaced by this value.
#' @export
impute_na <- function(target, ..., type = mean, val = 0) {
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
      # list(...) is a list of vectors
      df <- as.data.frame(list(...))
      if(nrow(df) == 0){
        # this is when no predictor columns are chosen
        stop("Please choose predictor columns")
      }
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
      target[is.na(target)] <- val
      target
    }, {
      stop(paste0(type, " is not supported as type"))
    })
  }
}
