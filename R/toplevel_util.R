#' Use a row as column names
#' @param df Data frame
#' @param row_index Row index to use as column names
#' @param prefix Prefix for new column names
#' @param clean_name If janitor::clean_names should be used
#' @param convert_to_ascii If non-ASCII column names should be converted to ASCII.
#' @param guess_data_type If result column data types should be re-evaluated.
#' @export
row_as_header <- function(df, row_index = 1, prefix = "", clean_names = TRUE, convert_to_ascii = FALSE, guess_data_type = FALSE){
  validate_empty_data(df)
  if (row_index == 0) { # there is no 0th row so just return the data frame as is.
    df
  } else {
    loadNamespace("stringr")
    loadNamespace("janitor")
    if (row_index < 0) { # it means from the bottom
      row_index <- nrow(df) + row_index + 1; # -1 means the last row (i.e. nrow(df)) so adjust it by adding 1
    }
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
      # Pass ascii as FALSE to prevent non-ascii column names converted to ascii.
      ret <- janitor::clean_names(ret, case="parsed", ascii = convert_to_ascii)
    }
    if (guess_data_type) {
      ret <- readr::type_convert(ret)
    }
    ret
  }
}
