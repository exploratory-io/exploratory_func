#' lm wrapper with do
#' @return deta frame which has lm model
#' @param data Data frame to be used as data
#' @param formula Formula for lm
#' @param ... Parameters to be passed to lm function
#' @param keep.source Whether source should be kept in source.data column
#' @param augment Whether the result should be augmented immediately
#' @param group_cols A vector with columns names to be used as group columns
#' @export
build_lm <- function(data, ..., keep.source = TRUE, augment = FALSE, group_cols = NULL){
  if(!is.null(group_cols)){
    data <- dplyr::group_by_(data, .dots =  group_cols)
  }

  grouped_col <- grouped_by(data)

  model_col <- avoid_conflict(grouped_col, "model")
  source_col <- avoid_conflict(grouped_col, "source.data")

  caller <- match.call()
  # this expands dots arguemtns to character
  arg_char <- expand_args(caller, exclude = c("data", "keep.source", "augment", "group_cols"))
  # put it into a formula
  fml <- as.formula(paste0("~stats::lm(data = ., ", arg_char, ")"))

  ret <- tryCatch({
    if(keep.source || augment){
      ret <- data %>% dplyr::do_(.dots = setNames(list(fml, ~(.)), c(model_col, source_col)))
      class(ret[[source_col]]) <- c("list", ".source.data")
      ret
    } else {
      data %>% dplyr::do_(.dots = setNames(list(fml), model_col))
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
    # do.call is used because augment tries to regard "model_col" and "source_col"
    # as column names as non standard evaluation
    ret <- do.call(broom::augment, list(ret, model_col, source_col))
  } else {
    class(ret[[model_col]]) <- c("list", ".model", ".model.lm")
  }
  ret
}
