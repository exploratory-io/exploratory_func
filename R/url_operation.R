#' get suffix from url string
#' @export
get_url_suffix <- function(url) {
  urltools::suffix_extract(urltools::url_parse(url)$domain)$suffix
}

#' get subdomain from url string
#' @export
get_url_subdomain <- function(url) {
  urltools::suffix_extract(urltools::url_parse(url)$domain)$subdomain
}

#' get decoded query parameter from url string
#' @export
get_url_param <- function(url, parameter_name){
  param <- urltools::param_get(url, parameter_name)[[parameter_name]]
  # if NA is passed to URLdecode, it is removed, so only not NA values should be used
  param[!is.na(param)] <- URLdecode(param[!is.na(param)])
  param
}
