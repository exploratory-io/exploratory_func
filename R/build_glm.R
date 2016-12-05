#' glm wrapper with do
#' @return deta frame which has glm model
#' @param data Data frame to be used as data
#' @param formula Formula for glm
#' @param ... Parameters to be passed to glm function
#' @param keep.source Whether source should be kept in source.data column
#' @param augment Whether the result should be augmented immediately
#' @param group_cols A vector with columns names to be used as group columns
#' @export
build_glm <- function(data, formula, ..., keep.source = TRUE, augment = FALSE, group_cols = NULL){
  # deal with group columns by index because those names might be changed
  group_col_index <- colnames(data) %in% group_cols

  # change column names to avoid name conflict when tidy or glance are executed
  colnames(data)[group_col_index] <- avoid_conflict(
    c(
      "model", ".train_index",
      # for tidy
      "term", "estimate", "std.error", "statistic", "p.value",
      # for glance
      "null.deviance",
      "df.null", "logLik", "AIC", "BIC", "deviance", "df.residual"
    ),
    colnames(data)[group_col_index],
    ".group"
  )

  # make column names unique
  colnames(data) <- make.unique(colnames(data), sep = "")

  if(!is.null(group_cols)){
    data <- dplyr::group_by_(data, .dots =  colnames(data)[group_col_index])
  }

  model_col <- "model"
  source_col <- "source.data"

  caller <- match.call()
  # this expands dots arguemtns to character
  arg_char <- expand_args(caller, exclude = c("data", "keep.source", "augment", "group_cols"))
  # put it into a formula
  fml <- as.formula(paste0("~stats::glm(data = ., ", arg_char, ")"))

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
    stop(e$message)
  })
  if(augment){
    # do.call is used because augment tries to regard "model_col" and "source_col"
    # as column names as non standard evaluation
    ret <- do.call(broom::augment, list(ret, model_col, source_col))
  } else {
    class(ret[[model_col]]) <- c("list", ".model", ".model.glm")
  }
  ret
}
