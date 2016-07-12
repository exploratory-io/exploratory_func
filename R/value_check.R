#' @export
is_empty <- function(vec){
  stringr::str_length(stringr::str_trim(vec))==0 | is.na(vec)
}
