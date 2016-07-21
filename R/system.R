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
  noncePhrase = getOption("tam.nonce")
  # These might be null if it's called from outside of desktop
  if(is.null(cryptoKeyPhrase) | is.null(noncePhrase)){
    NULL
  } else {
    key <- sodium::hash(charToRaw(cryptoKeyPhrase))
    nonce <- sodium::hash(charToRaw(noncePhrase), size=24)
    msg <- serialize(password, NULL)
    cipher <- sodium::data_encrypt(msg, key, nonce)
    saveRDS(cipher, file= stringr::str_c("../rdata/", sourceName, "_", userName, ".rds"))
  }
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
               httr::authenticate(username, pass))
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
getGoogleAnalytics <- function(tableId, lastNDays, dimensions, metrics, tokenFileId, paginate_query=FALSE){
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
  ga.data <- RGoogleAnalytics::GetReportData(ga.query, token, paginate_query = paginate_query)
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
queryMongoDB <- function(host, port, database, collection, username, password, query = "{}", isFlatten, limit=100000){
  if(!requireNamespace("mongolite")){stop("package mongolite must be installed.")}
  loadNamespace("stringr")
  loadNamespace("jsonlite")
  if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}

  # read stored password
  pass = saveOrReadPassword("mongodb", username, password)
  if (stringr::str_length(username) > 0) {
    url = stringr::str_c("mongodb://", username, ":", pass, "@", host, ":", as.character(port), "/", database)
  }
  else {
    url = stringr::str_c("mongodb://", host, ":", as.character(port), "/", database)
  }
  con <- mongolite::mongo(collection, url = url)
  data <- con$find(query = GetoptLong::qq(query), limit=limit)
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
  if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}

  # read stored password
  pass = saveOrReadPassword("mysql", username, password)

  drv <- DBI::dbDriver("MySQL")
  conn = RMySQL::dbConnect(drv, dbname = databaseName, username = username,
                   password = pass, host = host, port = port)
  resultSet <- RMySQL::dbSendQuery(conn, GetoptLong::qq(query))
  df <- RMySQL::dbFetch(resultSet, n = numOfRows)
  RMySQL::dbClearResult(resultSet)
  RMySQL::dbDisconnect(conn)
  df
}

#' @export
queryPostgres <- function(host, port, databaseName, username, password, numOfRows = -1, query){
  if(!requireNamespace("RPostgreSQL")){stop("package RPostgreSQL must be installed.")}
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
  if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}

  # read stored password
  pass = saveOrReadPassword("postgres", username, password)

  drv <- DBI::dbDriver("PostgreSQL")
  conn = RPostgreSQL::dbConnect(drv, dbname = databaseName, user = username,
                   password = pass, host = host, port = port)
  resultSet <- RPostgreSQL::dbSendQuery(conn, GetoptLong::qq(query))
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

#' @export
# tokenFileId is a unique value per data farme and is used to create a token cache file
getGoogleTokenForBigQuery <- function(tokenFileId, useCache=TRUE){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  loadNamespace("stringr")
  loadNamespace("httr")
  clientId <- "465736758727.apps.googleusercontent.com"
  secret <- "fJbIIyoIag0oA6p114lwsV2r"
  cacheOption = getOption("tam.oauth_token_cache")
  # tam.oauth_token_cache is RDS file path (~/.exploratory/projects/<projectid>/rdata/placeholder.rds)
  # for each data frame, create token cache as
  # ~/.exploratory/projects/<projectid>/rdata/<tokenFileId_per_dataframe>_ga_token.rds
  tokenPath = stringr::str_replace(cacheOption, "placeholder.rds", str_c(tokenFileId, "_bigquery_token.rds"))
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
                                  scope = c("https://www.googleapis.com/auth/bigquery", "https://www.googleapis.com/auth/cloud-platform"), cache = FALSE)
    # Save the token object for future sessions
    saveRDS(token, file=tokenPath)
  }
  token
}

#' @export
# API to refresh token
refreshGoogleTokenForBigQuery <- function(tokenFileId){
  getGoogleTokenForBigQuery(tokenFileId, FALSE)
}

#' @export
# API to submit a Google Big Query Job
submitGoogleBigQueryJob <- function(project, sqlquery, destination_table, tokenFieldId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}
  #GetoptLong uses stringr and str_c is called without stringr:: so need to use "require" instead of "requireNamespace"
  if(!require("stringr")){stop("package stringr must be installed.")}

  token <- getGoogleTokenForBigQuery(tokenFieldId)
  bigrquery::set_access_cred(token)
  # pass desitiona_table to support large data
  job <- bigrquery::insert_query_job(GetoptLong::qq(sqlquery), project, destination_table = destination_table)
  job <- bigrquery::wait_for(job)
  isCacheHit <- job$statistics$query$cacheHit
  # if cache hit case, totalBytesProcessed info is not available. So set is as -1
  totalBytesProcessed <- ifelse(isCacheHit, -1, job$statistics$totalBytesProcessed)
  # if cache hit case, recordsWritten info is not avalable. So set it as -1
  numOfRowsProcessed <- ifelse(isCacheHit, -1, job$statistics$query$queryPlan[[1]]$recordsWritten)
  dest <- job$configuration$query$destinationTable
  result <- data.frame(tableId = dest$tableId, datasetId = dest$datasetId, numOfRows = numOfRowsProcessed, totalBytesProcessed = totalBytesProcessed)
}

#' @export
# API to get a data from google BigQuery table
getDataFromGoogleBigQueryTable <- function(project, dataset, table, page_size = 10000, max_page, tokenFileId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId)
  bigrquery::set_access_cred(token)

  bigrquery::list_tabledata(project, dataset, table, page_size = page_size,
                 table_info = NULL, max_pages = max_page)
}

#' @export
# API to get a data from google BigQuery table
saveGoogleBigQueryResultAs <- function(projectId, sourceDatasetId, sourceTableId, targetDatasetId, targetTableId, tokenFileId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId)
  bigrquery::set_access_cred(token)

  src <- list(project_id = projectId, dataset_id = sourceDatasetId, table_id = sourceTableId)
  dest <- list(project_id = projectId, dataset_id = targetDatasetId, table_id = targetTableId)
  bigrquery::copy_table(src, dest)
}

#' @export
executeGoogleBigQuery <- function(project, sqlquery, destination_table, page_size = 10000, max_page = 10, tokenFileId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}

  token <- getGoogleTokenForBigQuery(tokenFileId)
  bigrquery::set_access_cred(token)
  bigrquery::query_exec(GetoptLong::qq(sqlquery), project = project, destination_table = destination_table, page_size = page_size, max_page = max_page)
}

#' @export
# API to get projects for current oauth token
getGoogleBigQueryProjects <- function(tokenFileId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  tryCatch({
    token <- getGoogleTokenForBigQuery(tokenFileId);
    bigrquery::set_access_cred(token)
    projects <- bigrquery::list_projects();
    ifelse(is.null(projects), c(""), projects)
  }, error = function(err){
    c("")
  })
}

#' @export
# API to get datasets for a project
getGoogleBigQueryDataSets <- function(project, tokenFileId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  tryCatch({
    token <- getGoogleTokenForBigQuery(tokenFileId);
    bigrquery::set_access_cred(token)
    resultdatasets <- bigrquery::list_datasets(project);
    ifelse(is.null(resultdatasets), c(""), resultdatasets)
  }, error = function(err){
     c("")
  })
}


#' @export
# API to get tables for current project, data set
getGoogleBigQueryTables <- function(project, dataset, tokenFileId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  tryCatch({
    token <- getGoogleTokenForBigQuery(tokenFileId);
    bigrquery::set_access_cred(token)
    tables <- bigrquery::list_tables(project, dataset);
    ifelse(is.null(tables), c(""), tables)
  }, error = function(err){
    c("")
  })
}
#' Parses all the 'scrapable' html tables from the web page.
#' @param {string} web page url to scrape
#' @param {string} web page encoding 
#' @return html nodes
#' @export
parse_html_tables <- function(url, encoding = NULL) {
  loadNamespace("rvest"); 
  loadNamespace("xml2"); 
  if (is.null(encoding)) {
    rvest::html_nodes(xml2::read_html(url) ,"table")
  } else {
    rvest::html_nodes(xml2::read_html(url, encoding=encoding) ,"table")
  }
}

#' Scrapes one of html tables from the web page specified by the url,
#' and returns a data frame as a result.
#' @param web page url to scrape
#' @param {string} table index number 
#' @param {string} either use the 1st row as a header or not. TRUE or FALSE
#' @param {string} web page encoding 
#' @export
scrape_html_table <- function(url, index, heading, encoding = NULL) {
  loadNamespace("rvest"); 
  loadNamespace("tibble"); 
  .htmltables <- parse_html_tables(url, encoding)
  tibble::repair_names(rvest::html_table(.htmltables[[index]], fill=TRUE ,header=heading))
}


# function to convert labelled class to factoror
# see https://github.com/exploratory-io/tam/issues/1481
#' @export
handleLabelledColumns = function(df){
  is_labelled <- which(lapply(df, class) == "labelled")
  df[is_labelled] <- lapply(df[is_labelled], as_factor)
  df
}

# Checks and tells the given data is whether in ndjson format
# by looking at that the first line is a valid json or not.
# x - URL or file path
isNDJSON <- function(x) {
  loadNamespace("jsonlite")
  con <- getConnectionObject(x)
  line <- readLines(con, n=1, warn=FALSE)
  close(con)
  tryCatch (
    {
      # It errors out if the line is invalid json.
      obj <- jsonlite::fromJSON(line)
      return (TRUE)
    },
    error=function(cond){
      return (FALSE)
    }
  )
}

# Construct a data frame from json or ndjson data
# ndjson stands for "Newline Delimited JSON" and
# each line of ndjson data is a valid json value.
# http://ndjson.org/
#
# ndjson format is popular and used in many places such as
# Yelp academic data is based on.
#
# jsonlite::fromJSON can read standard json but not ndjson.
# jsonlite::stream_in can read ndjson but not standard json.
# This function internally detects the data type and
# calls the appropriate function to read the data
# and construct a data from either json or ndjson data.
#
# x: URL or file path to json/ndjson file
# flatten: TRUE or FALSE. Used only json case
# limit: Should limit the number of rows to retrieve. Not used now
# since underlying technology (jsonlite) doesn't support it.
#' @export
convertFromJSON <- function(x, flatten=TRUE, limit=0) {
  loadNamespace("jsonlite")
  if (isNDJSON(x) == TRUE) {
    con2 <- getConnectionObject(x)
    df <- jsonlite::stream_in(con2, pagesize=1000, verbose=FALSE)

    # In case of connectinng to url, the following close call may fail with;
    # Error in close.connection(con2) : invalid connection
    # so here we catch the error here not to block the process.
    tryCatch (
      {
        close(con2)
      },
      error=function(cond){
      }
    )
    if (flatten == TRUE) {
      df <- jsonlite::flatten(df)
    }
    return (df)
  } else {
    df <- jsonlite::fromJSON(x, flatten=flatten)
    return (df)
  }
}

# This function converts the given data frame object to JSON.
# The benefit of using this function is that it can converts
# the column data types that cannot be serialized by toJSON
# to safe ones.
convertToJSON  <- function(x) {
  loadNamespace("jsonlite")
  .tmp.tojson <- x
  isdifftime <- sapply(.tmp.tojson, is.difftime)
  .tmp.tojson[isdifftime] <- lapply(.tmp.tojson[isdifftime], function(y) as.numeric(y))
  isperiod <- sapply(.tmp.tojson, is.period)
  .tmp.tojson[isperiod] <- lapply(.tmp.tojson[isperiod], function(y) as.character(y))
  jsonlite::toJSON(.tmp.tojson)
}

# Gives you a connection object based on the given
# file locator string. It supports file path or URL now.
# x - URL or file path
getConnectionObject <- function(x) {
  loadNamespace("stringr")
  if (stringr::str_detect(x, "://")) {
    return(url(x))
  } else {
    return(file(x, open = "r"))
  }
}

# Run the type convert if it is a data frame.
typeConvert <- function(x) {
  loadNamespace("readr")
  if (is.data.frame(x))  readr::type_convert(x) else x
}

# Create a data frame from the given object that can be transformed to data frame.
#' @export
toDataFrame <- function(x) {
  if(is.data.frame(x)) {
    df <- x
  } else if (is.matrix(x)) {
    df <- as.data.frame(x, stringsAsFactors = FALSE)
  } else {
    # just in case for other data type case in future
    df <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  return(typeConvert(df))
}

# API to create a temporary environment for RDATA staging
#' @export
createTempEnvironment <- function(){
  new.env(parent = globalenv())
}

# API to get a list of data frames from a RDATA
#' @export
getObjectListFromRdata <- function(rdata_path, temp.space){
  # load RDATA to temporary env to prevent the polluation on global objects
  temp.object <- load(rdata_path,temp.space)
  # get list of ojbect loaded to temporary env
  objectlist <- ls(envir=temp.space)
  result <- lapply(objectlist, function(x){
    # only get a object whose class is data.frame
    if("data.frame" %in% class(get(x,temp.space))){
      x
    }
  })
  if(!is.null(result) & length(result)>0){
    unlist(result)
  } else {
    c("");
  }
}

# API to get a data frame object from RDATA
#' @export
getObjectFromRdata <- function(rdata_path, object_name){
  # load RDATA to temporary env to prevent the polluation on global objects
  temp.space = createTempEnvironment()
  load(rdata_path,temp.space)
  # get list of ojbect loaded to temporary env
  obj <- get(object_name,temp.space)
  # remote temporary env
  rm(temp.space)
  obj
}



#' This function can clean the given data frame. It actually does 
#' 1) split a column with a data.frame vector into seprate columns 
#' 2) repair column names such as columns with NA for column names, 
#' or duplicate column names. 
#'
#' @param x data frame
#' @return cleaned data frame
#' @export
clean_data_frame <- function(x) {
  tibble::repair_names(jsonlite::flatten(x)) 
}

#' This checks name conflict and attach the file if there isn't any conflict
#' @export
checkSourceConflict <- function(files){
  ret <- list()
  for (file in files){
    ret[[file]] <- tryCatch({
      env <- new.env()
      source(file, local=env)
      attached_objects <- ls(env)
      list(names = attached_objects)
    }, error = function(e){
      list(error = e[["message"]])
    })
  }
  ret
}
