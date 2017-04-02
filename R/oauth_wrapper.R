#' @export
HttrOAuthToken2.0 <- R6::R6Class("HttrOAuthToken2.0", inherit = httr::Token2.0, list(
  initialize = function(
    ...,
    appname,
    key = NULL,
    secret = NULL,
    credentials = list()
  ){
    self$endpoint <- httr::oauth_endpoint(
      ...
    )
    # token is created directly from access token,
    # so these parameters are created by dummy parameters
    if(is.null(key)){
      key <- "dummy"
    }
    if(is.null(secret)){
      secret <- "dummy"
    }
    self$app <- httr::oauth_app(
      appname = appname,
      key = key,
      secret = secret
    )
    self$credentials <- credentials
    # this is needed to use this token in httr::GET function
    self$params$as_header <- TRUE
  }
))

#' @export
HttrOAuthToken1.0 <- R6::R6Class("HttrOAuthToken1.0", inherit = httr::Token1.0, list(
  initialize = function(
    ...,
    appname,
    key = NULL,
    secret = NULL,
    credentials = list()
  ){
    self$endpoint <- httr::oauth_endpoint(
      ...
    )
    # token is created directly from access token,
    # so these parameters are created by dummy parameters
    if(is.null(key)){
      key <- "dummy"
    }
    if(is.null(secret)){
      secret <- "dummy"
    }
    self$app <- httr::oauth_app(
      appname = appname,
      key = key,
      secret = secret
    )
    self$credentials <- credentials
    # this is needed to use this token in httr::GET function
    self$params$as_header <- TRUE
  }
))
