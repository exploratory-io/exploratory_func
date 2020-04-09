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


#' countrycode wrapper. 
#' 
#' There is one special origin type "flex". By setting this, you can pass
#' the country data in either "iso2c", "iso3c" or "country.name" format. 
#' Multiple formats can be mixed.  
#'  
#' @export
countrycode <- function(sourcevar, origin, destination, warn = TRUE, nomatch = NA, 
  custom_dict = NULL, custom_match = NULL, origin_regex = FALSE) {
  loadNamespace("countrycode")
      
  if (origin=="flex") {
    df <- data.frame(q=sourcevar) %>%
      mutate(iso2c= countrycode::countrycode(q, origin = "iso2c", destination = destination, warn=F),
             iso3c= countrycode::countrycode(q, origin = "iso3c", destination = destination, warn=F), 
             name = countrycode::countrycode(q, origin = "country.name", destination = destination, warn=F),
             a=coalesce(iso2c, iso3c, name))
    df$a
  } else {
    countrycode::countrycode(sourcevar, origin, destination, warn, nomatch, 
      custom_dict, custom_match, origin_regex)
  }
}
