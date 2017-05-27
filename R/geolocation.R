#'
#'

#'Create maxmind wrapper function
#'@param type Returning data type from maxmind. This can be "continent_name", "country_name", "country_code", "region_name", "city_name", "city_geoname_id", "timezone", "longitude", "latitude" and "connection"
#'@return Vector output from maxmind
maxmind_closure <- function(type){
  function(ip){
    loadNamespace("rgeolocate")
    rgeolocatefile <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
    df <- rgeolocate::maxmind(ip, rgeolocatefile, type)
    df[[type]]
  }
}

#'Convert IP address to country name
#'@param ip IP address
#'@return Country name
#'@export
ip_to_country <- maxmind_closure("country_name")

#' @export
countrycode <- countrycode::countrycode
