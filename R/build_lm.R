#' lm wrapper with do
#' @return deta frame which has lm model
#' @param data Data frame to be used as data
#' @param formula Formula for lm
#' @param ... Parameters to be passed to lm function
#' @param keep.source Whether source should be kept in source.data column
#' @param augment Whether the result should be augmented immediately
#' @param group_cols A vector with columns names to be used as group columns
#' @export
build_lm <- function(data, formula, ..., keep.source = TRUE, augment = FALSE, group_cols = NULL){
  if(!is.null(group_cols)){
    data <- dplyr::group_by_(data, .dots =  group_cols)
  }

  grouped_col <- grouped_by(data)

  model_col <- avoid_conflict(grouped_col, "model")
  source_col <- avoid_conflict(grouped_col, "source.data")

  raw_call <- match.call()
  # get named list or arguments (-1 means removing this function name)
  cl <- as.list(match.call())[-1]
  # keep just ... argument
  nodots <- as.list(match.call(expand.dots = FALSE))[-1]
  cl[names(nodots)] <- NULL
  # put only formula
  cl$formula <- formula
  build_glm_each <- function(df){
    cl$data <- df
    model <- do.call(lm, cl)
    # do.call expands the argument parameters
    # like weights = c(2L, 3L, ...), not using variable names
    # so call section in summary(model) becomes large.
    # call parameter is replaced by the input of build_lm
    model$call <- raw_call
    model
  }

  ret <- tryCatch({
    if(keep.source || augment){
      data %>% dplyr::do_(.dots = setNames(list(~build_glm_each(.), ~(.)), c(model_col, source_col)))
    } else {
      data %>% dplyr::do_(.dots = setNames(list(~build_glm_each(.)), model_col))
    }
  }, error = function(e){
    if(e$message == "contrasts can be applied only to factors with 2 or more levels"){
      stop("more than 2 unique values are needed for categorical predictor columns")
    }
    if(e$message == "0 (non-NA) cases"){
      stop("no data after removing NA")
    }
    stop(e$message)
  })
  if(augment){
    ret <- do.call(broom::augment, list(ret, model_col, source_col))
  } else {
    class(ret[[model_col]]) <- c("list", ".model", ".model.lm")
  }
  ret
}
