#' get domain from url string
#' @export
url_domain <- function(url) {
  loadNamespace("urltools")
  urltools::domain(url)
}

#' get fragment from url string
#' @export
url_fragment <- function(url) {
  loadNamespace("urltools")
  urltools::fragment(url)
}

#' get decoded query parameters from url string
#' @export
url_parameters <- function(url) {
  loadNamespace("urltools")
  params <- urltools::parameters(url)
  # if NA is passed to URLdecode, it is removed, so only not NA values should be used
  # URLdecode is not vectorized, so vapply has to be used
  params[!is.na(params)] <- vapply(params[!is.na(params)], URLdecode, FUN.VALUE = "")
  params
}

#' get path from url string
#' @export
url_path <- function(url) {
  loadNamespace("urltools")
  urltools::path(url)
}

#' get port from url string
#' @export
url_port <- function(url) {
  loadNamespace("urltools")
  urltools::port(url)
}

#' get scheme from url string
#' @export
url_scheme <- function(url) {
  loadNamespace("urltools")
  urltools::scheme(url)
}

#' get suffix from url string
#' @export
url_suffix <- function(url) {
  loadNamespace("urltools")
  urltools::suffix_extract(urltools::url_parse(url)$domain)$suffix
}

#' get subdomain from url string
#' @export
url_subdomain <- function(url) {
  loadNamespace("urltools")
  urltools::suffix_extract(urltools::url_parse(url)$domain)$subdomain
}

#' get top-level domain from url string
#' @export
url_tld <- function(url) {
  loadNamespace("urltools")
  urltools::tld_extract(urltools::url_parse(url)$domain)$tld
}

#' get decoded query parameter from url string
#' @export
url_param <- function(url, parameter_name){
  loadNamespace("urltools")
  param <- urltools::param_get(url, parameter_name)[[parameter_name]]
  # if NA is passed to URLdecode, it is removed, so only not NA values should be used
  # URLdecode is not vectorized, so vapply has to be used
  param[!is.na(param)] <- vapply(param[!is.na(param)], URLdecode, FUN.VALUE = "")
  param
}
