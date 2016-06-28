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
  cryptoKeyPhrase = getOption("tam.crypto_key")
  key <- sodium::hash(charToRaw(cryptoKeyPhrase))
  noncePhrase = getOption("tam.nonce")
  nonce <- sodium::hash(charToRaw(noncePhrase), size=24)
  msg <- serialize(password, NULL)
  cipher <- sodium::data_encrypt(msg, key, nonce)
  saveRDS(cipher, file= str_c("../rdata/", sourceName, "_", userName, ".rds"))
}

# API to read a password from RDS
# password file is consturcted with <source>_<username>.rds format_
readPasswordRDS = function(sourceName, userName){
  loadNamespace("sodium")
  passwordFlePath <- str_c("../rdata/", sourceName, "_", userName, ".rds")
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
  pass = saveOrReadPassword("github", username, password)

  # Body
  endpoint <- str_c("https://api.github.com/repos/", owner, "/", repository, "/issues")
  pages <- list()
  is_next <- TRUE
  i <- 1
  while(is_next){
    res <- GET(endpoint,
               query = list(state = "all", per_page = 100, page = i),
               authenticate(username, pass))
    jsondata <- content(res, type = "text", encoding = "UTF-8")
    github_df <- jsonlite::fromJSON(jsondata, flatten = TRUE)
    pages[[i]] <- github_df

    # check if link exists
    if(is.null(res$headers$link)){
      is_next <- FALSE
    } else {
      is_next <- str_detect(res$headers$link, "rel=\"next\"")
      i <- i + 1
    }
  }
  issues <- bind_rows(pages)
}

# tokenFileId is a unique value per data farme and is used to create a token cache file
#' @export
getGoogleTokenForAnalytics <- function(tokenFileId, useCache=TRUE){
  require(RGoogleAnalytics)
  require(lubridate)
  require(stringr)
  # As per Kan, this can be hard coded since Google limits acces per ViewID (tableID) and
  # not by clientID
  clientId <- "1066595427418-aeppbdhi7bj7g0osn8jpj4p6r9vus7ci.apps.googleusercontent.com"
  secret <-  "wGVbD4fttv_shYreB3PXcjDY"
  cacheOption = getOption("tam.oauth_token_cache")
  # tam.oauth_token_cache is RDS file path (~/.exploratory/projects/<projectid>/rdata/placeholder.rds)
  # for each data frame, create token cache as
  # ~/.exploratory/projects/<projectid>/rdata/<tokenFileId_per_dataframe>_ga_token.rds
  tokenPath = str_replace(cacheOption, "placeholder.rds", str_c(tokenFileId, "_ga_token.rds"))
  # since Auth from RGoogleAnalytics does not work well
  # switch to use oauth_app and oauth2.0_token
  token <- NULL
  if(useCache == TRUE && file.exists(tokenPath)){
    token <- readRDS(tokenPath)
  } else {
    myapp <- oauth_app("google", clientId, secret)
    if(useCache == FALSE){
      # set cacheOption as FALSE so that it forces to creaet a new token
      cacheOption = FALSE
    }
    token <- oauth2.0_token(oauth_endpoints("google"), myapp,
                            scope = "https://www.googleapis.com/auth/analytics.readonly", cache = FALSE)
    # Save the token object for future sessions
    saveRDS(token, file=tokenPath)
  }
  ValidateToken(token)
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
  require(RGoogleAnalytics)
  require(lubridate)
  try({
    token <- getGoogleTokenForAnalytics(tokenFileId);
    GetProfiles(token);
  })
}

#' @export
getGoogleAnalytics <- function(tableId, lastNDays, dimensions, metrics, tokenFileId){
  require(RGoogleAnalytics)
  require(lubridate)

  token <- getGoogleTokenForAnalytics(tokenFileId)
  start_date <- as.character(today() - days(lastNDays))
  #end_date <- as.character(today() - days(1))
  end_date <- as.character(today())
  query.list <- Init(start.date = start_date,
                     end.date = end_date,
                     dimensions = dimensions,
                     metrics = metrics,
                     max.results = 10000,
                     table.id = tableId)

  ga.query <- QueryBuilder(query.list)
  ga.data <- GetReportData(ga.query, token)
  ga.data
}


# tokenFileId is a unique value per data farme and is used to create a token cache file
#' @export
getGoogleTokenForSheet <- function(tokenFileId, useCache=TRUE){
  require(httr)
  require(stringr)
  # As per Kan, this can be hard coded since Google limits acces per ViewID (tableID) and
  # not by clientID
  clientId <- "1066595427418-aeppbdhi7bj7g0osn8jpj4p6r9vus7ci.apps.googleusercontent.com"
  secret <-  "wGVbD4fttv_shYreB3PXcjDY"
  cacheOption = getOption("tam.oauth_token_cache")
  # tam.oauth_token_cache is path ~/.exploratory/projects/<projectid>/rdata/placeholder.rds is the rds file templatettr cache
  # for each data set, create token cache as
  # ~/.exploratory/projects/<projectid>/rdata/<tokenFileId_per_dataframe>_gs_token.rds
  tokenPath = str_replace(cacheOption, "placeholder.rds", str_c(tokenFileId, "_gs_token.rds"))
  # use oauth_app and oauth2.0_token
  token <- NULL
  if(useCache == TRUE && file.exists(tokenPath)){
    token <- readRDS(tokenPath)
  } else {
    myapp <- oauth_app("google", clientId, secret)
    # scope is same as gs_auth does
    scope_list <- c("https://spreadsheets.google.com/feeds","https://www.googleapis.com/auth/drive")
    token <- oauth2.0_token(oauth_endpoints("google"), myapp,
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
  require(googlesheets)
  token <- getGoogleTokenForSheet(tokenFileId)
  gs_auth(token)
  gsheet <- gs_title(title)
  df <- gsheet %>% gs_read(ws = sheetNumber, skip = skipNRows, na = treatTheseAsNA, col_names = firstRowAsHeader, comment = commentChar)
  df
}

# API to get a list of available google sheets
#' @export
getGoogleSheetList <- function(tokenFileId){
  require(googlesheets)
  token = getGoogleTokenForSheet(tokenFileId)
  gs_auth(token)
  gs_ls()
}

#' @export
queryMongoDB <- function(host, port, database, collection, username, password, query = "{}", isFlatten){
  library(mongolite)
  require(stringr)
  # read stored password
  pass = saveOrReadPassword("mongodb", username, password)
  if (str_length(username) > 0) {
    url = str_c("mongodb://", username, ":", pass, "@", host, ":", as.character(port), "/", database)
  }
  else {
    url = str_c("mongodb://", host, ":", as.character(port), "/", database)
  }
  con <- mongo(collection, url = url)
  data <- con$find(query = query)
  result <-data
  if (isFlatten) {
    result <- flatten(data)
  }
  if (nrow(result)==0) {
    stop("No Data Found");
  } else {
    result
  }
}

#' @export
queryMySQL <- function(host, port, databaseName, username, password, numOfRows = -1, query){
  library(RMySQL)
  # read stored password
  pass = saveOrReadPassword("mysql", username, password)

  drv <- dbDriver("MySQL")
  conn = dbConnect(drv, dbname = databaseName, username = username,
                   password = pass, host = host, port = port)
  resultSet <- dbSendQuery(conn, query)
  df <- dbFetch(resultSet, n = numOfRows)
  dbClearResult(resultSet)
  dbDisconnect(conn)
  df
}

#' @export
queryPostgres <- function(host, port, databaseName, username, password, numOfRows = -1, query){
  library(RPostgreSQL)
  # read stored password
  pass = saveOrReadPassword("postgres", username, password)

  drv <- dbDriver("PostgreSQL")
  conn = dbConnect(drv, dbname = databaseName, user = username,
                   password = pass, host = host, port = port)
  resultSet <- dbSendQuery(conn, query)
  df <- dbFetch(resultSet, n = numOfRows)
  dbClearResult(resultSet)
  dbDisconnect(conn)
  df
}

queryRedshift <- function(host, port, databaseName, username, password, numOfRows = -1, query){
  library(RPostgreSQL)
  # read stored password
  pass = saveOrReadPassword("redshift", username, password)

  drv <- dbDriver("PostgreSQL")
  conn = dbConnect(drv, dbname = databaseName, user = username,
                   password = pass, host = host, port = port)
  resultSet <- dbSendQuery(conn, query)
  df <- dbFetch(resultSet, n = numOfRows)
  dbClearResult(resultSet)
  dbDisconnect(conn)
  df
}

# tokenFileId is a unique value per data farme and is used to create a token cache file
getTwitterToken <- function(tokenFileId, useCache=TRUE){
  require(twitteR)
  require(httr)

  consumer_key = "0lWpnop0HLfWRbpkDEJ0XA"
  consumer_secret = "xYNUMALkRnvuT3vls48LW7k2XK1l9xjZTLnRv2JaFaM"

  cacheOption = getOption("tam.oauth_token_cache")
  # tam.oauth_token_cache is RDS file path (~/.exploratory/projects/<projectid>/rdata/placeholder.rds)
  # for each data frame, create token cache as
  # ~/.exploratory/projects/<projectid>/rdata/<tokenFileId_per_dataframe>_ga_token.rds
  tokenPath = str_replace(cacheOption, "placeholder.rds", str_c(tokenFileId, "_twitter_token.rds"))

  twitter_token <- NULL
  if(useCache == TRUE && file.exists(tokenPath)){
    twitter_token <- readRDS(tokenPath)
  } else {
    myapp <- oauth_app("twitter", key = consumer_key, secret = consumer_secret)
    # Get OAuth credentials (For twitter use OAuth1.0)
    twitter_token <- oauth1.0_token(oauth_endpoints("twitter"), myapp, cache = FALSE)
    # Save the token object for future sessions
    saveRDS(twitter_token, file=tokenPath)
  }
  twitter_token
}

# API to refresh token
refreshTwitterToken <- function(tokenFileId){
  getTwitterToken(tokenFileId, FALSE)
}

#' @export
getTwitter <- function(n=200, lang=NULL,  lastNDays=30, searchString, tokenFileId){
  require(twitteR)
  require(lubridate)

  twitter_token = getTwitterToken(tokenFileId)
  use_oauth_token(twitter_token)
  # this parameter needs to be character with YYYY-MM-DD format
  # to get the latest tweets, pass NULL for until
  until = NULL
  since = as.character(today() - days(lastNDays))
  locale = NULL
  geocode = NULL
  sinceID = NULL
  maxID = NULL
  # hard cocde it as recent for now
  resultType = "recent"
  retryOnRateLimit = 120

  tweetList <- searchTwitter(searchString, n, lang, since, until, locale, geocode, sinceID, maxID, resultType, retryOnRateLimit)
  # conver list to data frame
  if(length(tweetList)>0){
    twListToDF(tweetList)
  } else {
    stop('No Tweets found.')
  }
}
