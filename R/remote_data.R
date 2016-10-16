#' tokenFileId is a unique value per data frame and is used to create a token cache file
#' @export
getGoogleTokenForConsole <- function(tokenFileId, useCache=TRUE){
  loadNamespace("googleAuthR")
  loadNamespace("httr")
  loadNamespace("stringr")

  cacheOption = getOption("tam.oauth_token_cache")
  # tam.oauth_token_cache is RDS file path (~/.exploratory/projects/<projectid>/rdata/placeholder.rds)
  # for each data frame, create token cache as
  # ~/.exploratory/projects/<projectid>/rdata/<tokenFileId_per_dataframe>_ga_token.rds
  tokenPath = stringr::str_replace(cacheOption, "placeholder.rds", stringr::str_c(tokenFileId, "_gc_token.rds"))
  # since Auth from RGoogleAnalytics does not work well
  # switch to use oauth_app and oauth2.0_token
  token <- NULL
  if(useCache == TRUE && file.exists(tokenPath)){
    token <- readRDS(tokenPath)
  } else {
    if(useCache == FALSE){
      # set cacheOption as FALSE so that it forces to creaet a new token
      cacheOption = FALSE
    }
    token <- googleAuthR::gar_auth(new_user = !cacheOption)
    # Save the token object for future sessions
    saveRDS(token, file=tokenPath)
  }
  token
}

#' API to refresh token
#' @export
refreshGoogleTokenForConsole <- function(tokenFileId){
  getGoogleTokenForConsole(tokenFileId, FALSE)
}

#' Access google search console data
#' @export
getGoogleConsole <- function(siteURL,
                             startDate = "",
                             endDate = "",
                             dimensions = "",
                             searchType = 'web',
                             dimensionFilterExp = "",
                             aggregationType = "auto",
                             rowLimit = 1000,
                             prettyNames = TRUE,
                             walk_data = "byBatch"){
  loadNamespace("searchConsoleR")
  # plugins in exploratory passes empty string for text input, so turn it to NULL
  if(dimensionFilterExp == ""){
    dimensionFilterExp <- NULL
  }
  if(start_date == ""){
    start_date <- Sys.Date() - 93
  }
  if(end_date == ""){
    end_date <- Sys.Date() - 3
  }

  # multible values from lov editor passes comma separated strings, so split it
  if(dimensions == ""){
    dimensions <- NULL
  } else {
    loadNamespace("stringr")
    dimensions <- stringr::str_split(dimensions, ",")[[1]]
  }

  searchConsoleR::search_analytics(
    siteURL = siteURL,
    startDate = startDate,
    endDate = endDate,
    dimensions = dimensions,
    dimensionFilterExp = dimensionFilterExp,
    aggregationType = aggregationType)

}

#' API to get profile for current oauth token
#' @export
getGoogleConsoleWebsites <- function(tokenFileId){
  # exploratory plugin passes tokenFileId, so it has to be in argument
  searchConsoleR::list_websites()
}
