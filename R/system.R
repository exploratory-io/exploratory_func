# Set cache path for oauth token cachefile
setOAuthTokenCacheOptions <- function(path){
  options(tam.oauth_token_cache = path)
}

# API to take care of read/save password for each plugin type and user name combination
# if password argument is null, it means we need to retrieve password from RDS file
# so the return value is password from RDS file.
# if password argument is not null, then it means it creates a new password or updates existing one
# so the password is saved to RDS file and the password is returned to caller
saveOrReadPassword = function(source, username, password){
  # read stored password
  pass = readPasswordRDS(source, username)
  # if stored password is null (i.e. new cteation) or stored password is different from previous (updating password from UI)
  if(is.null(pass)) {
    #if not stored yet, get it from UI
    pass = password
    savePasswordRDS(source, username, pass)
  } else if (!is.null(pass) & password != "" & pass != password) {
    #if passord is different from previous one, then  update it
    pass = password
    savePasswordRDS(source, username, pass)
  }
  pass
}

# API to save a psssword to RDS file
# password file is consturcted with <source>_<username>.rds format_
savePasswordRDS = function(sourceName, userName, password){
  loadNamespace("sodium")
  loadNamespace("stringr")
  cryptoKeyPhrase = getOption("tam.crypto_key")
  key <- sodium::hash(charToRaw(cryptoKeyPhrase))
  noncePhrase = getOption("tam.nonce")
  nonce <- sodium::hash(charToRaw(noncePhrase), size=24)
  msg <- serialize(password, NULL)
  cipher <- sodium::data_encrypt(msg, key, nonce)
  saveRDS(cipher, file= stringr::str_c("../rdata/", sourceName, "_", userName, ".rds"))
}

# API to read a password from RDS
# password file is consturcted with <source>_<username>.rds format_
readPasswordRDS = function(sourceName, userName){
  loadNamespace("sodium")
  loadNamespace("stringr")
  passwordFlePath <- stringr::str_c("../rdata/", sourceName, "_", userName, ".rds")
  password <- NULL
  if(file.exists(passwordFlePath)){
    # tryCatch so that we can handle decription failure
    tryCatch({
      cryptoKeyPhrase = getOption("tam.crypto_key")
      key <- sodium::hash(charToRaw(cryptoKeyPhrase))
      noncePhrase = getOption("tam.nonce")
      nonce <- sodium::hash(charToRaw(noncePhrase), size=24)

      cipher <- readRDS(passwordFlePath)
      msg <- sodium::data_decrypt(cipher, key, nonce)
      password <-unserialize(msg)
    }, warning = function(w) {
    }, error = function(e) {
    })
  }
  password
}

#' github issues plugin script
#' @export
getGithubIssues <- function(username, password, owner, repository){
  # read stored password
  loadNamespace("stringr")
  loadNamespace("httr")
  loadNamespace("dplyr")
  pass = saveOrReadPassword("github", username, password)

  # Body
  endpoint <- stringr::str_c("https://api.github.com/repos/", owner, "/", repository, "/issues")
  pages <- list()
  is_next <- TRUE
  i <- 1
  while(is_next){
    res <- httr::GET(endpoint,
               query = list(state = "all", per_page = 100, page = i),
               authenticate(username, pass))
    jsondata <- httr::content(res, type = "text", encoding = "UTF-8")
    github_df <- jsonlite::fromJSON(jsondata, flatten = TRUE)
    pages[[i]] <- github_df

    # check if link exists
    if(is.null(res$headers$link)){
      is_next <- FALSE
    } else {
      is_next <- stringr::str_detect(res$headers$link, "rel=\"next\"")
      i <- i + 1
    }
  }
  issues <- dplyr::bind_rows(pages)
}

# tokenFileId is a unique value per data farme and is used to create a token cache file
#' @export
getGoogleTokenForAnalytics <- function(tokenFileId, useCache=TRUE){
  if(!requireNamespace("RGoogleAnalytics")){stop("package RGoogleAnalytics must be installed")}
  loadNamespace("httr")
  loadNamespace("stringr")
  # As per Kan, this can be hard coded since Google limits acces per ViewID (tableID) and
  # not by clientID
  clientId <- "1066595427418-aeppbdhi7bj7g0osn8jpj4p6r9vus7ci.apps.googleusercontent.com"
  secret <-  "wGVbD4fttv_shYreB3PXcjDY"
  cacheOption = getOption("tam.oauth_token_cache")
  # tam.oauth_token_cache is RDS file path (~/.exploratory/projects/<projectid>/rdata/placeholder.rds)
  # for each data frame, create token cache as
  # ~/.exploratory/projects/<projectid>/rdata/<tokenFileId_per_dataframe>_ga_token.rds
  tokenPath = stringr::str_replace(cacheOption, "placeholder.rds", stringr::str_c(tokenFileId, "_ga_token.rds"))
  # since Auth from RGoogleAnalytics does not work well
  # switch to use oauth_app and oauth2.0_token
  token <- NULL
  if(useCache == TRUE && file.exists(tokenPath)){
    token <- readRDS(tokenPath)
  } else {
    myapp <- httr::oauth_app("google", clientId, secret)
    if(useCache == FALSE){
      # set cacheOption as FALSE so that it forces to creaet a new token
      cacheOption = FALSE
    }
    token <- httr::oauth2.0_token(httr::oauth_endpoints("google"), myapp,
                            scope = "https://www.googleapis.com/auth/analytics.readonly", cache = FALSE)
    # Save the token object for future sessions
    saveRDS(token, file=tokenPath)
  }
  RGoogleAnalytics::ValidateToken(token)
  token
}

# API to refresh token
#' @export
refreshGoogleTokenForAnalysis <- function(tokenFileId){
  getGoogleTokenForAnalytics(tokenFileId, FALSE)
}

# API to get profile for current oauth token
#' @export
getGoogleProfile <- function(tokenFileId){
  if(!requireNamespace("RGoogleAnalytics")){stop("package RGoogleAnalytics must be installed.")}
  try({
    token <- getGoogleTokenForAnalytics(tokenFileId);
    RGoogleAnalytics::GetProfiles(token);
  })
}

#' @export
getGoogleAnalytics <- function(tableId, lastNDays, dimensions, metrics, tokenFileId){
  if(!requireNamespace("RGoogleAnalytics")){stop("package RGoogleAnalytics must be installed.")}
  loadNamespace("lubridate")

  token <- getGoogleTokenForAnalytics(tokenFileId)
  start_date <- as.character(lubridate::today() - lubridate::days(lastNDays))
  #end_date <- as.character(lubridate::today() - lubridate::days(1))
  end_date <- as.character(lubridate::today())
  query.list <- RGoogleAnalytics::Init(start.date = start_date,
                     end.date = end_date,
                     dimensions = dimensions,
                     metrics = metrics,
                     max.results = 10000,
                     table.id = tableId)

  ga.query <- RGoogleAnalytics::QueryBuilder(query.list)
  ga.data <- RGoogleAnalytics::GetReportData(ga.query, token)
  ga.data
}


# tokenFileId is a unique value per data farme and is used to create a token cache file
#' @export
getGoogleTokenForSheet <- function(tokenFileId, useCache=TRUE){
  loadNamespace("httr")
  loadNamespace("stringr")
  # As per Kan, this can be hard coded since Google limits acces per ViewID (tableID) and
  # not by clientID
  clientId <- "1066595427418-aeppbdhi7bj7g0osn8jpj4p6r9vus7ci.apps.googleusercontent.com"
  secret <-  "wGVbD4fttv_shYreB3PXcjDY"
  cacheOption = getOption("tam.oauth_token_cache")
  # tam.oauth_token_cache is path ~/.exploratory/projects/<projectid>/rdata/placeholder.rds is the rds file templatettr cache
  # for each data set, create token cache as
  # ~/.exploratory/projects/<projectid>/rdata/<tokenFileId_per_dataframe>_gs_token.rds
  tokenPath = stringr::str_replace(cacheOption, "placeholder.rds", stringr::str_c(tokenFileId, "_gs_token.rds"))
  # use oauth_app and oauth2.0_token
  token <- NULL
  if(useCache == TRUE && file.exists(tokenPath)){
    token <- readRDS(tokenPath)
  } else {
    myapp <- httr::oauth_app("google", clientId, secret)
    # scope is same as gs_auth does
    scope_list <- c("https://spreadsheets.google.com/feeds","https://www.googleapis.com/auth/drive")
    token <- httr::oauth2.0_token(httr::oauth_endpoints("google"), myapp,
                            scope = scope_list, cache = FALSE)
    # Save the token object for future sessions
    saveRDS(token, file=tokenPath)
  }
  token
}

# API to refresh token
#' @export
refreshGoogleTokenForSheet <- function(tokenFileId){
  getGoogleTokenForSheet(tokenFileId, FALSE)
}

#' @export
getGoogleSheet <- function(title, sheetNumber, skipNRows, treatTheseAsNA, firstRowAsHeader, commentChar, tokenFileId){
  if(!requireNamespace("googlesheets")){stop("package googlesheets must be installed.")}
  token <- getGoogleTokenForSheet(tokenFileId)
  googlesheets::gs_auth(token)
  gsheet <- googlesheets::gs_title(title)
  df <- gsheet %>% googlesheets::gs_read(ws = sheetNumber, skip = skipNRows, na = treatTheseAsNA, col_names = firstRowAsHeader, comment = commentChar)
  df
}

# API to get a list of available google sheets
#' @export
getGoogleSheetList <- function(tokenFileId){
  if(!requireNamespace("googlesheets")){stop("package googlesheets must be installed.")}
  token = getGoogleTokenForSheet(tokenFileId)
  googlesheets::gs_auth(token)
  googlesheets::gs_ls()
}

#' @export
queryMongoDB <- function(host, port, database, collection, username, password, query = "{}", isFlatten){
  if(!requireNamespace("mongolite")){stop("package mongolite must be installed.")}
  loadNamespace("stringr")
  loadNamespace("jsonlite")
  # read stored password
  pass = saveOrReadPassword("mongodb", username, password)
  if (stringr::str_length(username) > 0) {
    url = stringr::str_c("mongodb://", username, ":", pass, "@", host, ":", as.character(port), "/", database)
  }
  else {
    url = stringr::str_c("mongodb://", host, ":", as.character(port), "/", database)
  }
  con <- mongolite::mongo(collection, url = url)
  data <- con$find(query = query)
  result <-data
  if (isFlatten) {
    result <- jsonlite::flatten(data)
  }
  if (nrow(result)==0) {
    stop("No Data Found");
  } else {
    result
  }
}

#' @export
queryMySQL <- function(host, port, databaseName, username, password, numOfRows = -1, query){
  if(!requireNamespace("RMySQL")){stop("package RMySQL must be installed.")}
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}

  # read stored password
  pass = saveOrReadPassword("mysql", username, password)

  drv <- DBI::dbDriver("MySQL")
  conn = RMySQL::dbConnect(drv, dbname = databaseName, username = username,
                   password = pass, host = host, port = port)
  resultSet <- RMySQL::dbSendQuery(conn, query)
  df <- RMySQL::dbFetch(resultSet, n = numOfRows)
  RMySQL::dbClearResult(resultSet)
  RMySQL::dbDisconnect(conn)
  df
}

#' @export
queryPostgres <- function(host, port, databaseName, username, password, numOfRows = -1, query){
  if(!requireNamespace("RPostgreSQL")){stop("package RPostgreSQL must be installed.")}
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
  # read stored password
  pass = saveOrReadPassword("postgres", username, password)

  drv <- DBI::dbDriver("PostgreSQL")
  conn = RPostgreSQL::dbConnect(drv, dbname = databaseName, user = username,
                   password = pass, host = host, port = port)
  resultSet <- RPostgreSQL::dbSendQuery(conn, query)
  df <- DBI::dbFetch(resultSet, n = numOfRows)
  RPostgreSQL::dbClearResult(resultSet)
  RPostgreSQL::dbDisconnect(conn)
  df
}

# tokenFileId is a unique value per data farme and is used to create a token cache file
#' @export
getTwitterToken <- function(tokenFileId, useCache=TRUE){
  if(!requireNamespace("twitteR")){stop("package twitteR must be installed.")}
  loadNamespace("httr")
  loadNamespace("stringr")

  consumer_key = "0lWpnop0HLfWRbpkDEJ0XA"
  consumer_secret = "xYNUMALkRnvuT3vls48LW7k2XK1l9xjZTLnRv2JaFaM"

  cacheOption = getOption("tam.oauth_token_cache")
  # tam.oauth_token_cache is RDS file path (~/.exploratory/projects/<projectid>/rdata/placeholder.rds)
  # for each data frame, create token cache as
  # ~/.exploratory/projects/<projectid>/rdata/<tokenFileId_per_dataframe>_ga_token.rds
  tokenPath = stringr::str_replace(cacheOption, "placeholder.rds", stringr::str_c(tokenFileId, "_twitter_token.rds"))

  twitter_token <- NULL
  if(useCache == TRUE && file.exists(tokenPath)){
    twitter_token <- readRDS(tokenPath)
  } else {
    myapp <- httr::oauth_app("twitter", key = consumer_key, secret = consumer_secret)
    # Get OAuth credentials (For twitter use OAuth1.0)
    twitter_token <- httr::oauth1.0_token(httr::oauth_endpoints("twitter"), myapp, cache = FALSE)
    # Save the token object for future sessions
    saveRDS(twitter_token, file=tokenPath)
  }
  twitter_token
}

# API to refresh token
#' @export
refreshTwitterToken <- function(tokenFileId){
  getTwitterToken(tokenFileId, FALSE)
}

#' @export
getTwitter <- function(n=200, lang=NULL,  lastNDays=30, searchString, tokenFileId){
  if(!requireNamespace("twitteR")){stop("package twitteR must be installed.")}
  loadNamespace("lubridate")

  twitter_token = getTwitterToken(tokenFileId)
  twitteR::use_oauth_token(twitter_token)
  # this parameter needs to be character with YYYY-MM-DD format
  # to get the latest tweets, pass NULL for until
  until = NULL
  since = as.character(lubridate::today() - lubridate::days(lastNDays))
  locale = NULL
  geocode = NULL
  sinceID = NULL
  maxID = NULL
  # hard cocde it as recent for now
  resultType = "recent"
  retryOnRateLimit = 120

  tweetList <- twitteR::searchTwitter(searchString, n, lang, since, until, locale, geocode, sinceID, maxID, resultType, retryOnRateLimit)
  # conver list to data frame
  if(length(tweetList)>0){
    twitteR::twListToDF(tweetList)
  } else {
    stop('No Tweets found.')
  }
}
