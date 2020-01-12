#' Use a row as column names
#' @param df Data frame
#' @param row_index Row index to use as column names
#' @param prefix Prefix for new column names
#' @param clean_name If janitor::clean_names should be used
#' @export
row_as_header <- function(df, row_index = 1, prefix = "", clean_names = TRUE){
  validate_empty_data(df)

  loadNamespace("stringr")
  loadNamespace("janitor")
  header_row <- df[row_index, ]
  # make values to character
  # vapply is used because as.character should be used for each column.
  # otherwise, factor is converted to integer.
  names <- vapply(header_row, function(val){as.character(val)}, FUN.VALUE="")
  if (prefix != ""){
    names <- stringr::str_c(prefix, names)
  }
  # remove a row based on row_index
  ret <- safe_slice(df, row_index, remove = TRUE)
  colnames(ret) <- names
  if (clean_names) {
    ret <- janitor::clean_names(ret, case="parsed")
  }
  ret
}
