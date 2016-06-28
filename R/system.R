#' github issues plugin script
#' @export
getGithubIssues <- function(username, password, owner, repository){
  # read stored password
  pass = `_tam_saveOrReadPassword`("github", username, password)

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
`_tam_getGoogleTokenForAnalytics` <- function(tokenFileId, useCache=TRUE){
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
`_tam_refreshGoogleTokenForAnalysis` <- function(tokenFileId){
  `_tam_getGoogleTokenForAnalytics`(tokenFileId, FALSE)
}

# API to get profile for current oauth token
`_tam_getGoogleProfile` <- function(tokenFileId){
  require(RGoogleAnalytics)
  require(lubridate)
  try({
    token <- `_tam_getGoogleTokenForAnalytics`(tokenFileId);
    GetProfiles(token);
  })
}

#' @export
getGoogleAnalytics <- function(tableId, lastNDays, dimensions, metrics, tokenFileId){
  require(RGoogleAnalytics)
  require(lubridate)

  token <- `_tam_getGoogleTokenForAnalytics`(tokenFileId)
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

#' @export
queryMongoDB <- function(host, port, database, collection, username, password, query = "{}", isFlatten){
  library(mongolite)
  require(stringr)
  # read stored password
  pass = `_tam_saveOrReadPassword`("mongodb", username, password)
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
  pass = `_tam_saveOrReadPassword`("mysql", username, password)

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
  pass = `_tam_saveOrReadPassword`("postgres", username, password)

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
  pass = `_tam_saveOrReadPassword`("redshift", username, password)

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
`_tam_getTwitterToken` <- function(tokenFileId, useCache=TRUE){
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
`_tam_refreshTwitterToken` <- function(tokenFileId){
  `_tam_getTwitterToken`(tokenFileId, FALSE)
}

#' @export
getTwitter <- function(n=200, lang=NULL,  lastNDays=30, searchString, tokenFileId){
  require(twitteR)
  require(lubridate)

  twitter_token = `_tam_getTwitterToken`(tokenFileId)
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
