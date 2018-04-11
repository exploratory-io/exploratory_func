#' API to get OAuth token
#' @return OAuth token
#' @param clientId OAuth client ID to get a OAuth token
#' @param secret OAuth secret to get a OAuth token
#' @param appName Applicatoin Name for OAuth
#' @param endpointType end point type that is passed to httr
#' @param scopeList list of permissions that the OAuth token requires
#' @param tokenFileName name of the RDS file tht stores the OAuth token
#' @param tokenFileId for backward compatiblity only. If this is empty, oauth process starts.
#' @export
getOAuthToken <- function(clientId, secret, appName, endpointType, scopeList, tokenFileName = "", tokenFileId = "", useCache = TRUE, version="2.0"){
  loadNamespace("httr")
  loadNamespace("stringr")

  # get a token RDS location for Global
  globalCacheOption = getOption("tam.global_oauth_token_cache")
  # for globl oauth token, the name should be unique par Data Source so use ga_token for google analytics token
  globalTokenPath = stringr::str_replace(globalCacheOption, "placeholder.rds", tokenFileName)

  # For backward compatibility
  cacheOption = getOption("tam.oauth_token_cache")
  # tam.oauth_token_cache is RDS file path (~/.exploratory/projects/<projectid>/rdata/placeholder.rds)
  # for each data frame, create token cache as
  # ~/.exploratory/projects/<projectid>/rdata/<tokenFileId_per_dataframe>_ga_token.rds
  tokenPath = stringr::str_replace(cacheOption, "placeholder.rds", stringr::str_c(tokenFileId, "_", tokenFileName))

  # since Auth from RGoogleAnalytics does not work well
  # switch to use oauth_app and oauth2.0_token
  token <- NULL
  # first check global token
  if(useCache == TRUE && length(globalTokenPath) == 1 && file.exists(globalTokenPath)){
    token <- readRDS(globalTokenPath)
  } else if(useCache == TRUE && length(tokenPath) == 1 && file.exists(tokenPath)){ # then fallback to local for backward compatibility
    token <- readRDS(tokenPath)
  } else { # get a new token.
    myapp <- httr::oauth_app(appName, clientId, secret)
    if(useCache == FALSE){
      # set cacheOption as FALSE so that it forces to creaet a new token
      cacheOption = FALSE
    }
    if(version == "2.0"){
      token <- httr::oauth2.0_token(httr::oauth_endpoints(endpointType), myapp,
                                    scope = scopeList, cache = FALSE)
    } else if (version == "1.0") {
      token <- httr::oauth1.0_token(httr::oauth_endpoints(endpointType), myapp, cache = FALSE)
    }
    # Save the token object for future sessions if globalTokenPath is set
    if(length(globalTokenPath) == 1 && globalTokenPath != ""){
      saveRDS(token, file=globalTokenPath)
    }
  }
  token
}

#' tokenFileId is a unique value per data farme and is used to create a token cache file
#' @export
getGoogleTokenForAnalytics <- function(tokenFileId = "", useCache=TRUE){
  if(!requireNamespace("RGoogleAnalytics")){stop("package RGoogleAnalytics must be installed")}
  # As per Kan, this can be hard coded since Google limits acces per ViewID (tableID) and
  # not by clientID
  clientId <- "1066595427418-aeppbdhi7bj7g0osn8jpj4p6r9vus7ci.apps.googleusercontent.com"
  secret <-  "wGVbD4fttv_shYreB3PXcjDY"
  appName = "google"
  endpointType = "google"
  # retrieve token info from environment
  # main purpose is to enable server refresh
  token_info <- getTokenInfo("googleanalytics")
  if(is.null(token_info)){
    tokenFileName ="ga_token.rds"
    scopeList = c("https://www.googleapis.com/auth/analytics.readonly")
    token = getOAuthToken(clientId, secret, appName, endpointType, scopeList, tokenFileName, tokenFileId, useCache)
    RGoogleAnalytics::ValidateToken(token)
    token
  } else {
    HttrOAuthToken2.0$new(
      authorize = "https://accounts.google.com/o/oauth2/auth",
      access = "https://accounts.google.com/o/oauth2/token",
      validate = "https://www.googleapis.com/oauth2/v1/tokeninfo",
      revoke = "https://accounts.google.com/o/oauth2/revoke",
      appname = appName,
      credentials = list(
        access_token = token_info$access_token,
        refresh_token = token_info$refresh_token,
        token_type = token_info$token_type,
        expiry_date = token_info$expiry_date
      )
    )
  }
}



#' API to refresh token
#' @export
refreshGoogleTokenForAnalytics <- function(tokenFileId){
  getGoogleTokenForAnalytics(tokenFileId, FALSE)
}

#' API to refresh token
#' For backward compatibility
#' @export
refreshGoogleTokenForAnalysis <- function(tokenFileId){
  getGoogleTokenForAnalytics(tokenFileId, FALSE)
}

#' tokenFileId is a unique value per data farme and is used to create a token cache file
#' @export
getGoogleTokenForSheet <- function(tokenFileId="", useCache=TRUE){
  # As per Kan, this can be hard coded since Google limits acces per ViewID (tableID) and
  # not by clientID
  clientId <- "1066595427418-aeppbdhi7bj7g0osn8jpj4p6r9vus7ci.apps.googleusercontent.com"
  secret <-  "wGVbD4fttv_shYreB3PXcjDY"
  appName = "google"
  endpointType = "google"
  # retrieve token info from environment
  # main purpose is to enable server refresh
  token_info <- getTokenInfo("googlesheet") # this should be googlesheets but our plunin is already named googlesheet
  if(!is.null(token_info)){
    HttrOAuthToken2.0$new(
      authorize = "https://accounts.google.com/o/oauth2/auth",
      access = "https://accounts.google.com/o/oauth2/token",
      validate = "https://www.googleapis.com/oauth2/v1/tokeninfo",
      revoke = "https://accounts.google.com/o/oauth2/revoke",
      appname = appName,
      credentials = list(
        access_token = token_info$access_token,
        refresh_token = token_info$refresh_token,
        token_type = token_info$token_type,
        expires_in = token_info$expires_in
      )
    )
  } else {
    tokenFileName ="gs_token.rds"
    scopeList <- c("https://spreadsheets.google.com/feeds","https://www.googleapis.com/auth/drive")
    token = getOAuthToken(clientId, secret, appName, endpointType, scopeList, tokenFileName, tokenFileId, useCache)
    token
  }
}


#' API to refresh token
#' @export
refreshGoogleTokenForSheet <- function(tokenFileId){
  getGoogleTokenForSheet(tokenFileId, FALSE)
}

#' tokenFileId is a unique value per data farme and is used to create a token cache file
#' @export
getTwitterToken <- function(tokenFileId="", useCache=TRUE){
  if(!requireNamespace("rtweet")){stop("package rtweet must be installed.")}
  consumer_key = "kYrHnqx62YaCoy6g0x967BGBq"
  consumer_secret = "wqP7VhX5yDEGzLL3eHSbT2wDlJvs4OitruIkd18CQZGzsySFuX"
  appName = "twitter"
  endpointType = "twitter"
  # retrieve token info from environment
  # main purpose is to enable server refresh
  token_info <- getTokenInfo("twitter")
  if(!is.null(token_info)){
    HttrOAuthToken1.0$new(
      request = "https://api.twitter.com/oauth/request_token",
      authorize = "https://api.twitter.com/oauth/authenticate",
      access = "https://api.twitter.com/oauth/access_token",
      appname = "twitter",
      key = consumer_key,
      secret = consumer_secret,
      credentials = list(
        oauth_token = token_info$oauth_token,
        oauth_token_secret = token_info$oauth_token_secret,
        user_id = token_info$user_id,
        screen_name = token_info$screen_name,
        x_auth_expires = token_info$x_auth_expires
      )
    )
  } else {
    tokenFileName ="twitter_token.rds"
    # Get OAuth credentials (For twitter use OAuth1.0)
    getOAuthToken(consumer_key, consumer_secret, appName, endpointType, "", tokenFileName, tokenFileId, useCache, version = "1.0")
  }
}

#' API to refresh token
#' @export
refreshTwitterToken <- function(tokenFileId){
  getTwitterToken(tokenFileId, FALSE)
}

#' tokenFileId is a unique value per data farme and is used to create a token cache file
#' @export
getGoogleTokenForBigQuery <- function(tokenFileId="", useCache=TRUE){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}

  clientId <- "1066595427418-aeppbdhi7bj7g0osn8jpj4p6r9vus7ci.apps.googleusercontent.com"
  secret <-  "wGVbD4fttv_shYreB3PXcjDY"
  scopeList = c("https://www.googleapis.com/auth/bigquery",
            "https://www.googleapis.com/auth/cloud-platform",
            "https://www.googleapis.com/auth/devstorage.read_write")
  appName = "google"
  endpointType = "google"
  # retrieve token info from environment
  # main purpose is to enable server refresh
  token_info <- getTokenInfo("googlebigquery") # this should be googlesheets but our plunin is already named googlebigquery
  if(!is.null(token_info)){
    HttrOAuthToken2.0$new(
      authorize = "https://accounts.google.com/o/oauth2/auth",
      access = "https://accounts.google.com/o/oauth2/token",
      validate = "https://www.googleapis.com/oauth2/v1/tokeninfo",
      revoke = "https://accounts.google.com/o/oauth2/revoke",
      appname = appName,
      credentials = list(
        access_token = token_info$access_token,
        refresh_token = token_info$refresh_token,
        token_type = token_info$token_type,
        expires_in = token_info$expires_in
      )
    )
  } else {
  tokenFileName ="bigquery_token.rds"
    getOAuthToken(clientId, secret, appName, endpointType, scopeList, tokenFileName, tokenFileId, useCache)
  }
}

#' API to refresh token
#' @export
refreshGoogleTokenForBigQuery <- function(tokenFileId){
  getGoogleTokenForBigQuery(tokenFileId, FALSE)
}



