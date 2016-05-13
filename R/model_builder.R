#'
#'
#'@return do wrapper function
do_data <- function(funcname) {
  ret <- function(df, ..., keep.source = TRUE){
    loadNamespace("dplyr")
    if (keep.source) {
      output <- df %>% dplyr::do(.model = do.call(funcname, list(data = ., ...)), .source.data = (.))
    } else {
      output <- df %>% dplyr::do(.model= do.call(funcname, list(data = ., ...)))
    }

    class(output$.model) <- c("list", paste0("model-", funcname))
    output
  }
  ret
}

#'@export
do_lm <- do_data("lm")

#'@export
do_glm <- do_data("glm")
