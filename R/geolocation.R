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
  custom_dict = NULL, custom_match = NULL, origin_regex = NULL) {
  loadNamespace("countrycode")

  if (origin=="flex") {
    df <- data.frame(q=sourcevar) %>%
      mutate(iso2c= countrycode::countrycode(q, origin = "iso2c", destination = destination, warn=F),
             iso3c= countrycode::countrycode(q, origin = "iso3c", destination = destination, warn=F),
             name = countrycode::countrycode(q, origin = "country.name", destination = destination, warn=F),
             a=coalesce(iso2c, iso3c, name))
    df$a
  } else {
    # Manually override the results for specific countries
    override <- c("East Europe", "East Europe", "East Europe", "West Asia")
    if (origin == "country.name") {
      names(override) <- c("estonia", "latvia", "lithuania", "iran")
    } else if (origin == "iso2c") {
      names(override) <- c("ee", "lv", "lt", "ir")
    } else if (origin == "iso3c") {
      names(override) <- c("est", "lva", "ltu", "irn")
    } else if (origin == "iso3n") {
      names(override) <- c(233, 418, 440, 364)
    } else if (origin == "imf") {
      names(override) <- c(939, 941, 946, 429)
    } else if (origin == "fao") {
      names(override) <- c(63, 119, 126, 102)
    } else if (origin == "ioc") {
      names(override) <- c("est", "lat", "ltu", "iri")
    } else if (origin == "un") {
      names(override) <- c(233, 428, 440, 364)
    } else if (origin == "wb") {
      names(override) <- c("est", "lva", "ltu", "irn")
    }

    if (destination == "region11") {
      res <- countrycode::countrycode(sourcevar, origin, "region23", warn, nomatch,
                                      custom_dict, custom_match, origin_regex)
      res <- case_when(res == "Australia and New Zealand" ~ "Oceania",
                res == "Caribbean" ~ "Central America",
                res == "Eastern Africa" ~ "Africa",
                res == "Eastern Asia" ~ "East Asia",
                res == "Eastern Europe" ~ "East Europe",
                res == "Melanesia" ~ "Pacific",
                res == "Micronesia" ~ "Pacific",
                res == "Middle Africa" ~ "Africa",
                res == "Northern Africa" ~ "Africa",
                res == "Northern America" ~ "North America",
                res == "Northern Europe" ~ "West Europe",
                res == "Polynesia" ~ "Pacific",
                res == "South-Eastern Asia" ~ "East Asia",
                res == "Southern Africa" ~ "Africa",
                res == "Southern Asia" ~ "Central Asia",
                res == "Southern Europe" ~ "West Europe",
                res == "Western Africa" ~ "Africa",
                res == "Western Asia" ~ "West Asia",
                res == "Western Europe" ~ "West Europe",
                TRUE ~ res
      )

      # Apply the overrides
      res <- ifelse(stringr::str_to_lower(sourcevar) %in% names(override), override[stringr::str_to_lower(sourcevar)], res)
    } else if (destination == "region23") {
      # Get the result using countrycode with custom dictionary
      res <- countrycode::countrycode(sourcevar, origin, destination, warn, nomatch,
                        custom_dict, custom_match, origin_regex)
      # Apply the overrides
      res <- ifelse(stringr::str_to_lower(sourcevar) %in% names(override), override[stringr::str_to_lower(sourcevar)], res)
    } else {
      res <- countrycode::countrycode(sourcevar, origin, destination, warn, nomatch,
                  custom_dict, custom_match, origin_regex)
    }
    res
  }
}
