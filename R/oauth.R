
#' tokenFileId is a unique value per data farme and is used to create a token cache file
getGoogleTokenForAnalytics <- function(tokenFileId = "", useCache=TRUE){
  if(!requireNamespace("RGoogleAnalytics")){stop("package RGoogleAnalytics must be installed")}
  appName = "google"
  # retrieve token info from environment
  # main purpose is to enable server refresh
  token_info <- getTokenInfo("googleanalytics")
  if(is.null(token_info)){
    stop("OAuth token is not set for Google Analytics.")
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


#' tokenFileId is a unique value per data farme and is used to create a token cache file
getGoogleTokenForSheet <- function(tokenFileId="", useCache=TRUE){
  # As per Kan, this can be hard coded since Google limits acces per ViewID (tableID) and
  # not by clientID
  appName = "google"
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
    stop("OAuth token is not set for Google Sheets.")
  }
}

#' API to refresh token
#' @export
refreshGoogleTokenForSheet <- function(tokenFileId){
  getGoogleTokenForSheet(tokenFileId, FALSE)
}

getGoogleTokenForDrive <- function(tokenFileId = "", useCache=TRUE){
  appName = "google"
  # retrieve token info from environment
  # main purpose is to enable server refresh
  token_info <- getTokenInfo("googledrive")
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
    stop("OAuth token is not set for Google Drive")
  }
}


#' API to refresh token
#' @export
refreshGoogleTokenForDrive <- function(tokenFileId = ""){
  getGoogleTokenForDrive(tokenFileId = tokenFileId, FALSE)
}

getSalesforceToken <- function(tokenFileId = "", useCache=TRUE){
  appName = "salesforce"
  # retrieve token info from environment
  # main purpose is to enable server refresh
  token_info <- getTokenInfo("salesforce")
  if(!is.null(token_info)){
    HttrOAuthToken2.0$new(
      authorize = "https://login.salesforce.com/services/oauth2/authorize",
      access = "https://login.salesforce.com/services/oauth2/token",
      revoke = "https://login.salesforce.com/services/oauth2/revoke",
      appname = appName,
      credentials = list(
        access_token = token_info$access_token,
        refresh_token = token_info$refresh_token,
        signature = token_info$signature,
        scope = token_info$scope,
        id_token = token_info$id_token,
        instance_url = token_info$instance_url,
        id = token_info$id,
        token_type = token_info$token_type,
        # Salesforcer uses issued_at and it forces refresh if it's became old.
        # Since we manage OAuth token in server side, set the the future (10 years from now) time to prevent unwanted OAuth token refresh triggered from R.
        issued_at = as.character(as.numeric(lubridate::now() + lubridate::years(10)) * 10)
      )
    )
  } else {
    stop("OAuth token is not set for Salesforce")
  }
}


#' tokenFileId is a unique value per data farme and is used to create a token cache file
getTwitterToken <- function(tokenFileId="", useCache=TRUE){
  if(!requireNamespace("rtweet")){stop("package rtweet must be installed.")}
  appName = "twitter"
  # retrieve token info from environment
  # main purpose is to enable server refresh
  token_info <- getTokenInfo("twitter")
  if(!is.null(token_info)){
    HttrOAuthToken1.0$new(
      request = "https://api.twitter.com/oauth/request_token",
      authorize = "https://api.twitter.com/oauth/authenticate",
      access = "https://api.twitter.com/oauth/access_token",
      appname = "twitter",
      key = "kYrHnqx62YaCoy6g0x967BGBq",
      secret = token_info$consumer_sc,
      credentials = list(
        oauth_token = token_info$oauth_token,
        oauth_token_secret = token_info$oauth_token_secret,
        user_id = token_info$user_id,
        screen_name = token_info$screen_name,
        x_auth_expires = token_info$x_auth_expires
      )
    )
  } else {
    stop("OAuth token is not set for Twitter.")
  }
}


#' tokenFileId is a unique value per data frame and is used to create a token cache file
getGoogleTokenForBigQuery <- function(tokenFileId="", useCache=TRUE){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  # To workaround Error in the HTTP2 framing layer
  # set below config (see https://github.com/jeroen/curl/issues/156)
  httr::set_config(httr::config(http_version = 0))

  appName = "google"
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
    stop("OAuth token is not set for Google BigQuery.")
  }
}


getGoogleTokenForCloudStorage <- function(useCache=TRUE){
  if(!requireNamespace("googleCloudStorageR")){stop("package googleCloudStorageR must be installed.")}
  # To workaround Error in the HTTP2 framing layer
  # set below config (see https://github.com/jeroen/curl/issues/156)
  httr::set_config(httr::config(http_version = 0))

  appName = "google"
  # retrieve token info from environment
  # main purpose is to enable server refresh
  token_info <- getTokenInfo("googlecloudstorage")
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
    stop("OAuth token is not set for Google Cloud Storage")
  }
}

#' API to refresh token
#' @export
refreshGoogleTokenForCloudStorage <- function(){
  getGoogleTokenForCloudStorage(FALSE)
}


