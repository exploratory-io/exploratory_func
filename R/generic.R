#'
#'

#' This function can clean the given data frame. It actually does 
#' 1) split a column with a data.frame vector into seprate columns 
#' 2) repair column names such as columns with NA for column names, 
#' or duplicate column names. 
#'
#' @param x data frame
#' @return cleaned data frame
#' @export
clean_data_frame <- function(x) {
  tibble::repair_names(jsonlite::flatten(x)) 
}
