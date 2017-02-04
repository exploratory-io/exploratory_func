#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @importFrom tibble add_row
#' @export
tibble::add_row

#' @importFrom urltools param_remove
#' @export
urltools::param_remove

#' @importFrom urltools url_encode
#' @export
urltools::url_encode

#' @importFrom urltools url_decode
#' @export
urltools::url_decode

#' @importFrom anonymizer anonymize
#' @export
anonymize <- function(algo = "sha256", seed = 0, chars = letters, n_chars = 5L, ...){
  anonymizer::anonymize(.algo = algo, .seed = seed, .chars = chars, .n_chars = n_chars, ...)
}
