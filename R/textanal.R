#' Function for Text Analysis Analytics View
#' @export
exp_textanal <- function(df, ...) {
  each_func <- function(df) {
    model <- list()
    class(model) <- 'textanal_exploratory'
    model
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' extracts results from textanal_exploratory object as a dataframe
#' @export
#' @param type - Type of output.
tidy.textanal_exploratory <- function(x, type="x", ...) {
  if (type == "x") {
    res <- tibble::tibble(x=1)
  }
  else if (type == "y") {
    res <- tibble::tibble(y=1)
  }
  res
}
