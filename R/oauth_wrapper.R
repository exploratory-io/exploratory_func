#' @export
HttrOAuthToken2.0 <- R6::R6Class("HttrOAuthToken2.0", inherit = httr::Token2.0, list(
  initialize = function(
    ...,
    appname,
    key,
    secret,
    credentials = list()
  ){
    self$endpoint <- httr::oauth_endpoint(
      ...
    )
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


