#'
#'

#'
#'@return maxmind function with closure
maxmind_closure <- function(type){
  loadNamespace("rgeolocate")
  function(ip){
    rgeolocatefile <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
    df <- rgeolocate::maxmind(ip, rgeolocatefile, type)
    df[[type]]
  }
}

#' @param ip IP address
#' @return Country name
#' @export
get_country <- maxmind_closure("country_name")
