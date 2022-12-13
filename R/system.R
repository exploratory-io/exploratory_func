# environment to keep variables for users
user_env <- new.env()
# environment to keep values to create connection
user_env$token_info <- new.env()

# environment to keep location of downloaded remote files.
user_env$downloads <- new.env()

#' get oauth token info from key
#' @export
getTokenInfo <- function(token_key){
  user_env$token_info[[token_key]]
}

#' set oauth token info
#' @export
setTokenInfo <- function(token_key, value) {
  user_env$token_info[[token_key]] <- value
}

setDownloadedFilePath <- function(hash, filePath){
  user_env$downloads[[hash]] <- filePath
}

getDownloadedFilePath <- function(hash){
 user_env$downloads[[hash]]
}

# hashmap in which we keep active connections to databases etc.
connection_pool <- new.env()

user_env$pool_connection <- FALSE;

#' set connection pool mode. TRUE means on and FALSE means off.
#' @export
setConnectionPoolMode <- function(val) {
  user_env$pool_connection <- val
  if (!val) {
    # clear odbc pooled connections when turning off connection pooling mode
    keys <- ls(connection_pool)
    lapply(keys, function(key) {
      if (startsWith(key, "odbc")) {
        tryCatch({ # try to close connection and ignore error
          conn <- connection_pool[[key]]
          RODBC::odbcClose(conn)
        }, warning = function(w) {
        }, error = function(e) {
        })
        rm(list = key, envir = connection_pool)
      } else if (startsWith(key, "dbiodbc") || startsWith(key, "teradata")) {
        tryCatch({ # try to close connection and ignore error
          conn <- connection_pool[[key]]
          DBI::dbDisconnect(conn)
        }, warning = function(w) {
        }, error = function(e) {
        })
      }
    })
  }
}

#' get connection pool mode. for test purpose.
#' @export
getConnectionPoolMode <- function() {
  user_env$pool_connection
}

#' Set cache path for oauth token cachefile
setOAuthTokenCacheOptions <- function(path){
  options(tam.oauth_token_cache = path)
}

#' On windows, since user input string like SQL or search string for twitter are in windows code page,
#' (windows R does not support UTF-8 as part of locale.)
#' we need to convert it to UTF-8 before sending it on the wire.
convertUserInputToUtf8 <- function(inputString) {
  if (Sys.info()[["sysname"]] == "Windows") {
    lc_ctype_locale = Sys.getlocale("LC_CTYPE") # returns string like "Japanese_Japan.932"
    lc_ctype_locale_tokens = base::strsplit(lc_ctype_locale, "\\.")[[1]] # c("Japanese_Japan", "932"). [[1]] is necessary since strsplit return is nested.
    if (length(lc_ctype_locale_tokens) == 2) { # check length to avoid out of bound error.
      encoding = lc_ctype_locale_tokens[[2]] # extracts "932"
      inputString = base::iconv(inputString, from = encoding, to = "UTF-8")
    }
  }
  inputString
}

#' API to take care of read/save password for each plugin type and user name combination
#' if password argument is null, it means we need to retrieve password from RDS file
#' so the return value is password from RDS file.
#' if password argument is not null, then it means it creates a new password or updates existing one
#' so the password is saved to RDS file and the password is returned to caller
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

#' API to save a psssword to RDS file
#' password file is consturcted with <source>_<username>.rds format_
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

#' API to read a password from RDS
#' password file is consturcted with <source>_<username>.rds format_
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
getGithubIssues <- function(username, password, owner, repository, ...){
  # read stored password
  loadNamespace("stringr")
  loadNamespace("httr")
  loadNamespace("dplyr")

  # Body
  endpoint <- stringr::str_c("https://api.github.com/repos/", owner, "/", repository, "/issues")
  pages <- list()
  is_next <- TRUE
  i <- 1
  while(is_next){
    res <- httr::GET(endpoint,
               query = list(state = "all", per_page = 100, page = i),
               httr::authenticate(username, password))
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


getMongoURL <- function(host = NULL, port, database, username, pass, isSSL=FALSE, authSource=NULL, cluster = NULL, timeout = NULL, additionalParams = NULL, ...) {
  loadNamespace("stringr")
  loadNamespace("urltools")

  if (stringr::str_length(username) > 0) {
    if(!is.null(pass) && pass != ''){
      # mongodb connection URL uses @ as a separator so need to encode password for those special characters.
      pass = urltools::url_encode(pass)
    }
    if(!is.null(host) && host != ''){
      url = stringr::str_c("mongodb://", username, ":", pass, "@", host, ":", as.character(port), "/", database)
    } else if (!is.null(cluster) && cluster != ''){
      url = stringr::str_c("mongodb://", username, ":", pass, "@", cluster, "/", database)
    }
  }
  else {
    if(!is.null(host) & host != ''){
      url = stringr::str_c("mongodb://", host, ":", as.character(port), "/", database)
    } else if (!is.null(cluster) && cluster != ''){
      url = stringr::str_c("mongodb://", cluster, "/", database)
    }
  }
  if(isSSL){
    url = stringr::str_c(url, "?ssl=true")
  }
  if(!is.null(authSource) && authSource != ""){
    if(isSSL){
      url = stringr::str_c(url, "&authSource=", authSource)
    } else {
      url = stringr::str_c(url, "?authSource=", authSource)
    }
  }
  if(!is.null(additionalParams) && additionalParams != ''){
    if(isSSL || !is.null(authSource) && authSource != "") {
      url = stringr::str_c(url, "&", additionalParams)
    } else {
      url = stringr::str_c(url, "?", additionalParams)
    }
  }
  if(!is.null(timeout) && timeout != ''){
    if(stringr::str_detect(url, '\\?')) {
      url = stringr::str_c(url, '&socketTimeoutMS=', timeout)
    } else {
      url = stringr::str_c(url, '?socketTimeoutMS=', timeout)
    }
  }
  return (url)
}

# Wrapper around glue::glue() to resolve our @{} parameter notation.
# We need this since glue::glue() escapes '}}' into '}', which makes it
# hard to use inside js code. Even in SQL, expression like '{d @{date_param}}'
# which would happen in SQL Server's SQL would not work with the original
# glue::glue() behavior.
glue_exploratory <- function(text, .transformer, .envir = parent.frame()) {
  # Internally, replace a{ and } with <<< and >>>.
  text <- stringr::str_replace_all(text, "\\@\\{([^\\}]+)\\}", "<<<\\1>>>")
  # Then, call glue::glue().
  ret <- glue::glue(text, .transformer = .transformer, .open = "<<<", .close = ">>>", .envir = .envir)
  ret
}
# This assumes that @{} parameter notation is already resolved before calling the glue_salesforce.
# Wrapper around glue::glue() to resolve our ${} notation used for Salesforce Filter.
glue_salesforce <- function(text) {
  # Internally, replace ${ and } with <<< and >>>.
  text <- stringr::str_replace_all(text, "\\$\\{([^\\}]+)\\}", "<<<\\1>>>")
  # Then, call glue::glue().
  ret <- glue::glue(text, .transformer = glue_salesforce_internal, .open = "<<<", .close = ">>>")
  ret
}

# Get variable's configured default value.
get_variable_config <- function(variable_name, config_name, envir) {
  code <- paste0("if(is.null(exploratory_env$.config$`", variable_name, "`)){NULL}else{exploratory_env$.config$`", variable_name, "`$`", config_name, "`}")
  ret <- eval(parse(text = code), envir)
  ret
}

# glue transformer for mongo js query.
# supports character, factor, logical, Date, POSIXct, POSIXlt, and numeric.
js_glue_transformer <- function(expr, envir) {
  # expr is 'param1, quote=FALSE, escape=FALSE' if the whole placeholder is '@{param1, quote=FALSE, escape=FALSE}'
  tokens <- stringr::str_split(expr, ',')
  tokens <- tokens[[1]]
  name <- tokens[1]
  values <- NULL

  # Parse arguments part. e.g. 'quote=FALSE, escape=FALSE'
  if (length(tokens) > 1) {
    args <- tokens[2:length(tokens)] # Index 1 that is eliminated is the name of the variable.
    args <- stringr::str_split(args, '=')
    args <- purrr::map(args, trimws)
    names <- purrr::map(args, function(x){x[1]})
    values <- purrr::map(args, function(x){x[2]})
    names(values) <- names
  }
  if (!is.null(values) && !is.null(values$quote)) {
    if (values$quote %in% c("FALSE", "F", "false", "NO", "No", "no")) {
      quote <- ''
    }
    else if (values$quote %in% c("TRUE", "T", "true", "YES", "Yes", "yes")) {
      # TRUE means same as default, which is double quote.
      quote <- '"'
    }
    else if (grepl("^'.*'$", values$quote)) { # Single quoted.
      quote <- sub("^'", "", values$quote)
      quote <- sub("'$", "", quote)
    }
    else if (grepl('^".*"$', values$quote)) { # Double quoted.
      quote <- sub('^"', "", values$quote)
      quote <- sub('"$', "", quote)
    }
    else {
      quote <- NULL # Check default config for the parameter.
    }
  }
  else {
    quote <- NULL # Check default config for the parameter.
  }

  if (!is.null(values) && !is.null(values$escape)) {
    if (values$escape %in% c("FALSE", "F", "false", "NO", "No", "no")) {
      escape <- ''
    }
    else if (values$escape %in% c("TRUE", "T", "true", "YES", "Yes", "yes")) {
      # TRUE means same as default, which is double quote.
      escape <- '"'
    }
    else if (grepl("^'.*'$", values$escape)) { # Single quoted.
      escape <- sub("^'", "", values$escape)
      escape <- sub("'$", "", escape)
    }
    else if (grepl('^".*"$', values$escape)) { # Double quoted.
      escape <- sub('^"', "", values$escape)
      escape <- sub('"$', "", escape)
    }
    else {
      escape <- NULL # Check default config for the parameter.
    }
  }
  else {
    escape <- NULL # Check default config for the parameter.
  }

  # Trim white spaces.
  name <- trimws(name)

  # Extract the vector index from name[index] expression if it exists.
  name_index <- stringr::str_split(name,'[\\[\\]]')[[1]]
  name <- name_index[1]
  index <- name_index[2]

  # Strip quote by ``.
  should_strip <- grepl("^`.+`$", name)
  if (should_strip) {
    name <- sub("^`", "", name)
    name <- sub("`$", "", name)
  }
  code <- paste0("exploratory_env$`", name, "`")
  if (!is.na(index)) { # Apply the vector index if it is specified.
    code <- paste0(code, '[', index, ']')
  }

  val <- eval(parse(text = code), envir)

  # Check the default config for the variable.
  if (is.null(quote)) {
    quote <- get_variable_config(name, "quote", envir)
    if (is.null(quote)) {
      if (is.numeric(val)) {
        quote <- '' # No quote by default for numeric.
      }
      else {
        quote <- '"' # Double quote by default
      }
    }
  }
  if (is.null(escape)) {
    escape <- get_variable_config(name, "escape", envir)
    if (is.null(escape)) {
      escape <- quote # Match with quote by default.
    }
  }

  if (length(val) == 0) { # Empty vector case. NULL in R is same as empty vector, but since is.null(as.character(c(0))) returns FALSE, this has to be done by length().
    val <- ""
  }
  else if (is.numeric(val)) {
    # Do not convert number to scientific notation.
    val <- format(val, scientific = FALSE)
    val <- paste0(quote, val, quote)
  }
  else if (is.character(val) || is.factor(val)) {
    if (escape == '"') { # Escape for double quote
      val <- gsub("\\", "\\\\", val, fixed=TRUE)
      val <- gsub("\"", "\\\"", val, fixed=TRUE)
    }
    else if (escape == "'") { # Escape for single quote
      val <- gsub("\\", "\\\\", val, fixed=TRUE)
      val <- gsub("'", "\\'", val, fixed=TRUE)
    }
    val <- paste0(quote, val, quote)
  }
  else if (is.logical(val)) {
    val <- ifelse(val, "true", "false")
  }
  else if (lubridate::is.Date(val) || lubridate::is.POSIXt(val)) {
    val <- paste0("new Date(\"", as.character(val), "\")")
  }
  # Interpret NA to null.
  # https://docs.mongodb.com/manual/tutorial/query-for-null-fields/
  val <- ifelse(is.na(val), "null", val)

  # for numeric it should work as is. expression like 1e+10 works on js too.
  glue::glue_collapse(val, sep=", ")
}

glue_salesforce_internal <- function(expr, envir){
  val <- eval(parse(text = expr))
  glue::glue_collapse(val, sep=", ")
}

# Common routine for sql_glue_transformer and bigquery_glue_transformer.
sql_glue_transformer_internal <- function(expr, envir, bigquery=FALSE, salesforce=FALSE) {
  tokens <- stringr::str_split(expr, ',')
  tokens <- tokens[[1]]
  name <- tokens[1]
  values <- NULL

  # Parse arguments part. e.g. @{param1, quote=FALSE}
  if (length(tokens) > 1) {
    args <- tokens[2:length(tokens)]
    args <- stringr::str_split(args, '=')
    args <- purrr::map(args, trimws)
    names <- purrr::map(args, function(x){x[1]})
    values <- purrr::map(args, function(x){x[2]})
    names(values) <- names
  }
  if (!is.null(values) && !is.null(values$quote)) {
    if (values$quote %in% c("FALSE", "F", "false", "NO", "No", "no")) {
      quote <- ''
    }
    else if (values$quote %in% c("TRUE", "T", "true", "YES", "Yes", "yes")) {
      # TRUE means same as default, which is single quote.
      quote <- "'"
    }
    else if (grepl("^'.*'$", values$quote)) { # Single quoted.
      quote <- sub("^'", "", values$quote)
      quote <- sub("'$", "", quote)
    }
    else if (grepl('^".*"$', values$quote)) { # Double quoted.
      quote <- sub('^"', "", values$quote)
      quote <- sub('"$', "", quote)
    }
    else {
      quote <- NULL # Check default config for the parameter.
    }
  }
  else {
    quote <- NULL # Check default config for the parameter.
  }

  if (!is.null(values) && !is.null(values$escape)) {
    if (values$escape %in% c("FALSE", "F", "false", "NO", "No", "no")) {
      escape <- ''
    }
    else if (values$escape %in% c("TRUE", "T", "true", "YES", "Yes", "yes")) {
      # TRUE means same as default, which is single quote.
      escape <- "'"
    }
    else if (grepl("^'.*'$", values$escape)) { # Single quoted.
      escape <- sub("^'", "", values$escape)
      escape <- sub("'$", "", escape)
    }
    else if (grepl('^".*"$', values$escape)) { # Double quoted.
      escape <- sub('^"', "", values$escape)
      escape <- sub('"$', "", escape)
    }
    else {
      escape <- NULL # Check default config for the parameter.
    }
  }
  else {
    escape <- NULL # Check default config for the parameter.
  }

  # Trim white spaces.
  name <- trimws(name)

  # Extract the vector index from name[index] expression if it exists.
  name_index <- stringr::str_split(name,'[\\[\\]]')[[1]]
  name <- name_index[1]
  index <- name_index[2]

  # Strip quote by ``.
  should_strip <- grepl("^`.+`$", name)
  if (should_strip) {
    name <- sub("^`", "", name)
    name <- sub("`$", "", name)
  }

  code <- paste0("exploratory_env$`", name, "`")
  if (!is.na(index)) { # Apply the vector index if it is specified.
    code <- paste0(code, '[', index, ']')
  }

  val <- eval(parse(text = code), envir)

  # Check the default config for the variable.
  if (is.null(quote)) {
    quote <- get_variable_config(name, "quote", envir)
    if (is.null(quote)) {
      if (is.numeric(val)) {
        quote <- '' # No quote by default for numeric.
      }
      else {
        quote <- "'" # Single quote by default
      }
    }
  }
  if (is.null(escape)) {
    escape <- get_variable_config(name, "escape", envir)
    if (is.null(escape)) {
      escape <- quote # Match with quote by default.
    }
  }

  if (length(val) == 0) { # Empty vector case. NULL in R is same as empty vector, but since is.null(as.character(c(0))) returns FALSE, this has to be done by length().
    # Print "NULL" string.
    val <- "NULL" # With PostgreSQL, "IN (NULL)" is valid while "IN ()" is syntax error. TODO: Test other databases.
  }
  else if (is.numeric(val)) {
    # Do not convert number to scientific notation.
    val <- format(val, scientific = FALSE)
    val <- paste0(quote, val, quote)
  }
  else if (is.character(val) || is.factor(val)) {
    if (bigquery) {
      if (escape == '"') { # Escape for double quote
        # escape for Standard SQL for bigquery
        # https://cloud.google.com/bigquery/docs/reference/standard-sql/lexical
        val <- gsub("\\", "\\\\", val, fixed=TRUE) # Escape literal backslash
        val <- gsub("\"", "\\\"", val, fixed=TRUE) # Escape literal double quote
      }
      else if (escape == "'") { # Escape for single quote
        # escape for Standard SQL for bigquery
        # https://cloud.google.com/bigquery/docs/reference/standard-sql/lexical
        val <- gsub("\\", "\\\\", val, fixed=TRUE) # Escape literal backslash
        val <- gsub("\'", "\\\'", val, fixed=TRUE) # Escape literal single quote
      }
    }
    else {
      # escape for SQL
      # TODO: check if this makes sense for Dremio and Athena
      if (escape == '"') { # Escape for double quote (Checked that SQL Server's double quote works this way.)
        val <- gsub('"', '""', val, fixed=TRUE)
      }
      else if (escape == "'") { # Escape for single quote
        val <- gsub("'", "''", val, fixed=TRUE) # both Oracle and SQL Server escapes single quote by doubling them.
      }
    }
    val <- paste0(quote, val, quote)
  }
  else if (lubridate::is.Date(val)) {
    val <- as.character(val)
    if (salesforce == FALSE) { # for salesforce, date should not be quoted.
      val <- paste0(quote, val, quote) # Athena and PostgreSQL quotes date with single quote. e.g. '2019-01-01'
    }
  }
  else if (lubridate::is.POSIXt(val)) {
    if (salesforce) { # Need to format it as YYYY-MM-DDThh:mm:ssZ
      # ref https://developer.salesforce.com/docs/atlas.en-us.soql_sosl.meta/soql_sosl/sforce_api_calls_soql_select_dateformats.htm
      val <- format(val, "%Y-%m-%dT%H:%M:%S%z")
    } else {
      val <- as.character(val)
      val <- paste0(quote, val, quote) # Athena and PostgreSQL quotes timestamp with single quote. e.g. '2019-01-01 00:00:00'
    }
  }

  # TODO: How should we handle logical?
  #       Does expression like 1e+10 work?
  # TODO: Need to handle NA here. Find out appropriate way.
  # We always collapse, unlike glue_sql.
  glue::glue_collapse(val, sep=", ")
}

sql_glue_transformer <- function(expr, envir) {
  sql_glue_transformer_internal(expr, envir)
}

bigquery_glue_transformer <- function(expr, envir) {
  sql_glue_transformer_internal(expr, envir, bigquery=TRUE)
}

salesforce_glue_transformer <- function(expr, envir) {
  sql_glue_transformer_internal(expr, envir, salesforce=TRUE)
}

#' @export
queryMongoDB <- function(host = NULL, port = "", database, collection, username, password, query = "{}", flatten,
                         limit=100, isSSL=FALSE, authSource=NULL, fields="{}", sort="{}",
                         skip=0, queryType = "find", pipeline="{}", cluster = NULL, timeout = NULL, additionalParams = NULL, connectionString = NULL, sslClientCertKey = NULL, subType = NULL, ...){
  if(!requireNamespace("mongolite")){stop("package mongolite must be installed.")}
  loadNamespace("jsonlite")

  # read stored password
  # get connection from connection pool
  con <- getDBConnection("mongodb", host, port, database, username, password, collection = collection,
                         isSSL = isSSL, authSource = authSource, cluster = cluster, additionalParams = additionalParams,
                         timeout = timeout, connectionString = connectionString, sslClientCertKey = sslClientCertKey, subType = subType)
  if(fields == ""){
    fields = "{}"
  }
  if(sort == ""){
    sort = "{}"
  }
  data <- NULL
  tryCatch({
    if(queryType == "aggregate"){
      pipeline <- convertUserInputToUtf8(pipeline)
      # set .envir = parent.frame() to get variables from users environment, not papckage environment
      pipeline <- glue_exploratory(pipeline, .transformer=js_glue_transformer, .envir = parent.frame())
      # convert js query into mongo JSON, which mongolite understands.
      pipeline <- jsToMongoJson(pipeline)
      data <- con$aggregate(pipeline = pipeline)
    } else if (queryType == "find") {
      query <- convertUserInputToUtf8(query)
      fields <- convertUserInputToUtf8(fields)
      sort <- convertUserInputToUtf8(sort)
      # set .envir = parent.frame() to get variables from users environment, not papckage environment
      query <- glue_exploratory(query, .transformer=js_glue_transformer, .envir = parent.frame())
      # convert js query into mongo JSON, which mongolite understands.
      query <- jsToMongoJson(query)
      fields <- jsToMongoJson(fields)
      sort <- jsToMongoJson(sort)
      data <- con$find(query = query, limit=limit, fields=fields, sort = sort, skip = skip)
    }
  }, error = function(err) {
    clearDBConnection("mongodb", host, port, database, username, collection = collection, isSSL = isSSL, authSource = authSource, connectionString = connectionString, sslClientCertKey = sslClientCertKey)
    stop(err)
  })
  result <-data
  if (flatten) {
    result <- jsonlite::flatten(data)
  }
  if (nrow(result)==0) {
    # possibly this is an error. clear connection once.
    clearDBConnection("mongodb", host, port, database, username, collection = collection, isSSL = isSSL, authSource = authSource, connectionString = connectionString, sslClientCertKey = sslClientCertKey)
    stop("No Data Found");
  } else {
    result
  }
}

#' Returns a data frame that has names of the collections in its "name" column.
#' @export
getMongoCollectionNames <- function(host = "", port = "", database = "", username = "",
                                    password ="", isSSL=FALSE, authSource=NULL, cluster = NULL, timeout = "", additionalParams = "", connectionString = NULL, sslClientCertKey = NULL, subType = NULL, ...){
  collection = "test" # dummy collection name. mongo command seems to work even if the collection does not exist.
  loadNamespace("jsonlite")
  if(!requireNamespace("mongolite")){stop("package mongolite must be installed.")}
  con <- getDBConnection("mongodb", host, port, database, username, password, collection = collection,
                         isSSL = isSSL, authSource = authSource, cluster = cluster, additionalParams = additionalParams,
                         timeout = timeout, connectionString = connectionString, sslClientCertKey = sslClientCertKey, subType = subType)
  # command to list collections.
  # con$command is our addition in our mongolite fork.
  result <- con$run(command = '{"listCollections":1}')
  # need to check existence of ok column of result dataframe first to avoid error in error check.
  if (!("ok" %in% names(result)) || !result$ok) {
    clearDBConnection("mongodb", host, port, database, username, collection = collection, isSSL = isSSL, authSource = authSource, cluster = cluster, additionalParams = additionalParams, connectionString = connectionString, sslClientCertKey = sslClientCertKey)
    stop("listCollections command failed");
  }
  # TODO: does "firstBatch" mean it is possible there are more?
  # if so, where does it appear in the result?
  return(as.data.frame(result$cursor$firstBatch))
}

#' Returns the total number of rows stored in the target table.
#' At this moment only mongdb is supported.
#' @export
getMongoCollectionNumberOfRows <- function(host = NULL, port = "", database = "",
                                           username = "", password = "", collection = "",
                                           isSSL=FALSE, authSource=NULL, cluster = NULL, additionalParams = "",
                                           timeout = NULL, connectionString = NULL, sslClientCertKey = NULL, subType = NULL, ...){
  loadNamespace("jsonlite")
  if(!requireNamespace("mongolite")){stop("package mongolite must be installed.")}
  con <- getDBConnection("mongodb", host, port, database, username, password, collection = collection,
                         isSSL = isSSL, authSource = authSource, cluster = cluster, additionalParams = additionalParams,
                         timeout = timeout, connectionString = connectionString, sslClientCertKey = sslClientCertKey, subType = subType)
  tryCatch({
    result <- con$count()
  }, error = function(err) {
    clearDBConnection("mongodb", host, port, database, username, collection = collection, isSSL = isSSL, authSource = authSource, cluster = cluster, additionalParams = additionalParams, connectionString = connectionString, sslClientCertKey = sslClientCertKey)
    stop(err)
  })
  return(result)
}

createAmazonAthenaConnectionString <- function(driver = "", region = "", authenticationType = "IAM Credentials", s3OutputLocation = "", user = "", password = "", additionalParams = "", timezone = "", endpointOverride = "", workgroup = "",
                                               useProxy = 0, proxyHost="", proxyPort = -1, proxyUID = "", proxyPWD = "", ...) {
  loadNamespace("stringr")
  # if platform is Linux use predefined one
  if(Sys.info()["sysname"]=="Linux"){
    driver <-  "/opt/simba/athenaodbc/lib/64/libathenaodbc_sb64.so";
  }
  # Generate connection string as a key to pool connection.
  connectionString <- stringr::str_c("AwsRegion=",  region, ";AuthenticationType=", authenticationType, ";uid=", user,
                                     ";pwd=", password, ";S3OutputLocation=", s3OutputLocation, ";driver=", driver)
  if(additionalParams != "") {
    connectionString <- stringr::str_c(connectionString, ";", additionalParams)
  }
  if (timezone == "") {
    timezone <- "UTC" # if timezone is not provided use UTC as default timezone. This is also the default for odbc::dbConnect.
  }
  connectionString <- stringr::str_c(connectionString, ";timezone=", timezone)
  connectionString <- stringr::str_c(connectionString, ";timezone_out=", timezone)

  if (endpointOverride != "") {
    connectionString <- stringr::str_c(connectionString, ";EndpointOverride=", endpointOverride)
  }

  if (workgroup != "") {
    connectionString <- stringr::str_c(connectionString, ";workgroup=", workgroup)
  }

  if (useProxy == 1) {
    connectionString <- stringr::str_c(connectionString, ";UseProxy=", useProxy)
    if (proxyHost != "") {
      connectionString <- stringr::str_c(connectionString, ";ProxyHost=", proxyHost)
    }
    if (proxyPort != -1) {
      connectionString <- stringr::str_c(connectionString, ";ProxyPort=", proxyPort)
    }
    if (proxyUID != "") {
      connectionString <- stringr::str_c(connectionString, ";ProxyUID=", proxyUID)
    }
    if (proxyPWD != "") {
      connectionString <- stringr::str_c(connectionString, ";ProxyPWD=", proxyPWD)
    }
  }

  # For Windows, set encoding to make sure non-ascii data is handled properly.
  # ref: https://github.com/r-dbi/odbc/issues/153
  if (is.win <- Sys.info()['sysname'] == 'Windows') {
    loc <- Sys.getlocale(category = "LC_CTYPE")
    # loc looks like "Japanese_Japan.932", so split it with dot ".".
    encoding <- stringr::str_split(loc, pattern = "\\.")
    if (length(encoding[[1]]) == 2 && encoding[[1]][[2]] != "utf8") {
      connectionString <- stringr::str_c(connectionString, ";encoding=", encoding[[1]][[2]])
    }
  }
  connectionString
}

#' Returns a Amazon Athena connection.
#' @export
getAmazonAthenaConnection <- function(driver = "", region = "", authenticationType = "IAM Credentials", s3OutputLocation = "",
                                      user = "", password = "", additionalParams = "", timezone = "", endpointOverride = "", workgroup = "",
                                      useProxy = 0, proxyHost = "", proxyPort = -1, proxyUID = "", proxyPWD = "", ...) {
  loadNamespace("odbc")
  loadNamespace("stringr")
  if(!requireNamespace("odbc")){stop("package odbc must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  connectionString <- createAmazonAthenaConnectionString(driver = driver,
                                                         region = region,
                                                         authenticationType = authenticationType,
                                                         s3OutputLocation = s3OutputLocation,
                                                         user = user,
                                                         password = password,
                                                         additionalParams = additionalParams,
                                                         timezone = timezone,
                                                         endpointOverride = endpointOverride,
                                                         workgroup = workgroup,
                                                         useProxy = useProxy,
                                                         proxyHost = proxyHost,
                                                         proxyPort = proxyPort,
                                                         proxyUID = proxyUID,
                                                         proxyPWD = proxyPWD)

  conn <- NULL
  if (user_env$pool_connection) {
    conn <- connection_pool[[connectionString]]
  }
  if (is.null(conn)) {
    if (timezone == "") {
      timezone <- "UTC" # if timezone is not provided use UTC as default timezone. This is also the default for odbc::dbConnect.
    }

    loc <- Sys.getlocale(category = "LC_CTYPE")
    # loc looks like "Japanese_Japan.932", so split it with dot ".".
    encoding <- stringr::str_split(loc, pattern = "\\.")

    # For Windows, set encoding to make sure non-ascii data is handled properly.
    # ref: https://github.com/r-dbi/odbc/issues/153
    if (is.win <- Sys.info()['sysname'] == 'Windows' && length(encoding[[1]]) == 2 && encoding[[1]][[2]] != "utf8") {
      # For the legacy mode where we use non-UTF8 Windows encoding.
      # Encoding and Timezone need to be passed as explicit arguments.
      conn <- DBI::dbConnect(odbc::odbc(),
                             encoding           = encoding[[1]][[2]],
                             timezone           = timezone,
                             timezone_out       = timezone,
                             .connection_string = connectionString)
    } else {
      # Timezone needs to be passed as a explicit argument.
      conn <- DBI::dbConnect(odbc::odbc(),
                             timezone           = timezone,
                             timezone_out       = timezone,
                             .connection_string = connectionString)
    }
    if (user_env$pool_connection) { # pool connection if connection pooling is on.
      connection_pool[[connectionString]] <- conn
    }
  }
  conn
}

#' Clears AWS Athena Connection.
#' @export
clearAmazonAthenaConnection <- function(driver = "", region = "", authenticationType = "IAM Credentials", s3OutputLocation = "",
                                        user = "", password = "", additionalParams = "", timezone = "", endpointOverride = "", workgroup = "",
                                        useProxy = 0, proxyHost = "",  proxyPort = -1, proxyUID = "", proxyPWD = "", ...){

  key <- createAmazonAthenaConnectionString(driver = driver,
                                            region = region,
                                            authenticationType = authenticationType,
                                            s3OutputLocation = s3OutputLocation,
                                            user = user,
                                            password = password,
                                            additionalParams = additionalParams,
                                            timezone = timezone,
                                            endpointOverride = endpointOverride,
                                            workgroup = workgroup,
                                            useProxy = useProxy,
                                            proxyHost = proxyHost,
                                            proxyPort = proxyPort,
                                            proxyUID = proxyUID,
                                            proxyPWD = proxyPWD)

  conn <- connection_pool[[key]]
  if (!is.null(conn)) {
    tryCatch({ # try to close connection and ignore error
      DBI::dbDisconnect(conn)
    }, warning = function(w) {
    }, error = function(e) {
    })
  }
  rm(list = key, envir = connection_pool)
}

#' Returns specified connection from pool if it exists in the pool.
#' If not, new connection is created and returned.
#' @export
getDBConnection <- function(type, host = NULL, port = "", databaseName = "", username = "", password = "", catalog = "", schema = "", dsn="", additionalParams = "",
                            collection = "", isSSL = FALSE, authSource = NULL, cluster = NULL, timeout = NULL, connectionString = NULL, driver = NULL, timezone = "",
                            subType = NULL, sslClientCertKey = "", sslCA = "") {

  drv = NULL
  conn = NULL
  key = NULL

  if (type == "mongodb") {
    if(!requireNamespace("mongolite")){stop("package mongolite must be installed.")}
    loadNamespace("jsonlite")
    if(!is.null(connectionString) && connectionString != '' && (is.null(subType) || subType == '' || subType == 'connectionString')) {
      # make sure to include collection as a key since connection varies per collection.
      key <- paste(connectionString, collection, sep = ":")
    } else if(!is.null(host) && host != ''){
      key <- paste("mongodb", host, port, databaseName, collection, username, toString(isSSL), authSource, additionalParams, sslClientCertKey, sep = ":")
    } else if (!is.null(cluster) && cluster != '') {
      key <- paste("mongodb", cluster, databaseName, collection, username, toString(isSSL), authSource, additionalParams, sslClientCertKey, sep = ":")
    }

    conn <- connection_pool[[key]]
    if (!is.null(conn)){
      # command to ping to check connection validity.
      # con$command is our addition in our mongolite fork.
      result <- conn$run(command = '{"ping":1}')
      # need to check existence of ok column of result dataframe first to avoid error in error check.
      if (!("ok" %in% names(result)) || !result$ok) {
        rm(conn) # this disconnects connection
        conn <- NULL
        # fall through to getting new connection.
      }
    }
    if (is.null(conn)) {
      if(!is.null(connectionString) && connectionString != '' && (is.null(subType) || subType == '' || subType == 'connectionString')) {
        # if connection string is provided, use it for the url.
        url <- connectionString
      } else {
        url <- getMongoURL(host = host, port = port, database = databaseName, username = username, pass = password, isSSL = isSSL, authSource = authSource, cluster = cluster, additionalParams = additionalParams, timeout = timeout)
      }
      if(!is.null(sslClientCertKey) && sslClientCertKey != '') { # Connect with ssl client cert.
        if (file.exists(sslClientCertKey)) {
          cert <- openssl::read_cert(sslClientCertKey) # Extract the cert from the pem file.
          # Extract the key from the pem file. Just passing the file path of the pem that includes both cert and key to mongolite::ssl_options() causes error.
          private_key <- openssl::read_pem(sslClientCertKey)$`RSA PRIVATE KEY`
          if (is.null(private_key)) stop("Cannot find private key in the pem file.")
          private_key <- openssl::read_key(private_key)
          conn <- mongolite::mongo(collection, url = url, options = mongolite::ssl_options(cert = cert, key = private_key))
        }
        else { # If cert/key file is missing, which can happen especially on the server, just try connecting without it.
          conn <- mongolite::mongo(collection, url = url)
        }
      }
      else {
        conn <- mongolite::mongo(collection, url = url)
      }
      connection_pool[[key]] <- conn
    }
  } else if(type == "mysql" || type == "aurora") {
    if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
    if(!requireNamespace("RMariaDB")){stop("package RMariaDB must be installed.")}
    # use same key "mysql" for aurora too, since it uses
    # queryMySQL() too, which uses the key "mysql"

    # When the Amazon Aurora data source is executed on Linux, it's possible that sslCA parameter is defined, for this case switch it to use seeded pem file for now.
    # Also, when getDBConnection is called from queryMySQL, the type argument is set as "mysql" for both MariaDB and Aurora, so stop checking type
    # and simply check if sslCA is empty string or not.
    if(Sys.info()["sysname"] == "Linux" && sslCA != ""){
      sslCA <- "/etc/ssl/certs/rds-combined-ca-bundle.pem";
    }
    key <- paste("mysql", host, port, databaseName, username, timezone, sslCA, sep = ":")
    conn <- connection_pool[[key]]
    if (!is.null(conn)){
      tryCatch({
        # test connection and at the same time set up the session with the server to use utf8.
        result <- DBI::dbGetQuery(conn,"set names utf8") # This should return empty data.frame.
        if (!is.data.frame(result)) { # it can fail by returning NULL rather than throwing error.
          tryCatch({ # try to close connection and ignore error
            DBI::dbDisconnect(conn)
          }, warning = function(w) {
          }, error = function(e) {
          })
          conn <- NULL
          # fall through to getting new connection.
        }
      }, error = function(err) {
        tryCatch({ # try to close connection and ignore error
          DBI::dbDisconnect(conn)
        }, warning = function(w) {
        }, error = function(e) {
        })
        conn <- NULL
        # fall through to getting new connection.
      })
    }
    # if the connection is null or the connection is invalid, create a new one.
    if (is.null(conn) || !DBI::dbIsValid(conn)) {
      # To avoid integer64 handling issues in charts, etc., use numeric as the R type to receive bigint data rather than default integer64 by specifying bigint argument.
      if (timezone != "") {# if Timezone is set use it for timezone and timezone_out
        if (sslCA != "") { # if sslCA is set, pass it as ssl.ca
          conn = RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = databaseName, username = username, timezone = timezone, timezone_out = timezone,
                                     password = password, host = host, port = port, bigint = "numeric", ssl.ca = sslCA)
        } else { # if sslCA is not set, do not set it since passing empty string causes Error : Failed to connect: SSL connection error: No such file or directory
          conn = RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = databaseName, username = username, timezone = timezone, timezone_out = timezone,
                                     password = password, host = host, port = port, bigint = "numeric")
        }
      } else {# if sslCA is set, pass it as ssl.ca
        if (sslCA != "") {
          conn = RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = databaseName, username = username,
                                     password = password, host = host, port = port, bigint = "numeric", ssl.ca = sslCA)
        } else {# if sslCA is not set, do not set it since passing empty string causes Error : Failed to connect: SSL connection error: No such file or directory
          conn = RMariaDB::dbConnect(RMariaDB::MariaDB(), dbname = databaseName, username = username,
                                     password = password, host = host, port = port, bigint = "numeric")
        }
      }
      connection_pool[[key]] <- conn
    }
  } else if (type == "postgres" || type == "redshift" || type == "vertica") {
    if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
    if(!requireNamespace("RPostgres")){stop("package RPostgres must be installed.")}
    # use same key "postgres" for redshift and vertica too, since they use
    # queryPostgres() too, which uses the key "postgres"
    key <- paste("postgres", host, port, databaseName, username, timezone, sep = ":")
    conn <- connection_pool[[key]]
    if (!is.null(conn)){
      tryCatch({
        # test connection
        result <- DBI::dbGetQuery(conn,"select 1")
        if (!is.data.frame(result)) { # it can fail by returning NULL rather than throwing error.
          tryCatch({ # try to close connection and ignore error
            DBI::dbDisconnect(conn)
          }, warning = function(w) {
          }, error = function(e) {
          })
          conn <- NULL
          # fall through to getting new connection.
        }
      }, error = function(err) {
        tryCatch({ # try to close connection and ignore error
          DBI::dbDisconnect(conn)
        }, warning = function(w) {
        }, error = function(e) {
        })
        conn <- NULL
        # fall through to getting new connection.
      })
    }
    # if the connection is null or the connection is invalid, create a new one.
    if (is.null(conn) || !DBI::dbIsValid(conn)) {
      drv <- RPostgres::Postgres()
      if (timezone != "") { # if Timezone is set, use it for timezone and timezone_out arguments.
        conn <- RPostgres::dbConnect(drv, dbname=databaseName, user = username, timezone = timezone, timezone_out = timezone,
                                     password = password, host = host, port = port, bigint = "numeric")
      } else {
        conn <- RPostgres::dbConnect(drv, dbname=databaseName, user = username,
                                     password = password, host = host, port = port, bigint = "numeric")
      }
      connection_pool[[key]] <- conn
    }
  } else if (type == "presto" || type == "treasuredata") {
    if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
    if(!requireNamespace("RPresto")){stop("package Presto must be installed.")}
    # use the same key "presto" for presto and treasuredata since they both use "presto".
    key <- paste("presto", host, port, catalog, schema, username, timezone, sep = ":")
    conn <- connection_pool[[key]]
    if (!is.null(conn)){
      tryCatch({
        # test connection
        result <- RPresto::dbSendQuery(conn,"select 1")
        if (!is.data.frame(result)) { # it can fail by returning NULL rather than throwing error.
          tryCatch({ # try to close connection and ignore error
            RPresto::dbDisconnect(conn)
          }, warning = function(w) {
          }, error = function(e) {
          })
          conn <- NULL
          # fall through to getting new connection.
        }
      }, error = function(err) {
        tryCatch({ # try to close connection and ignore error
          RPresto::dbDisconnect(conn)
        }, warning = function(w) {
        }, error = function(e) {
        })
        conn <- NULL
        # fall through to getting new connection.
      })
    }
    # if the connection is null or the connection is invalid, create a new one.
    if (is.null(conn) || !DBI::dbIsValid(conn)) {
      loadNamespace("RPresto")
      drv <- RPresto::Presto()
      # To workaround Presto Authentication issue, set X-Presto-User to http header.
      # Please refer https://github.com/prestodb/RPresto/issues/103 for details.
      httr::set_config(
        httr::add_headers("X-Presto-User"=username)
      )
      if (timezone != "") { #if timezone is set, use it for session.timezone.
        conn <- RPresto::dbConnect(drv, user = username,
                                   password = password, host = host, port = port, schema = schema, catalog = catalog, session.timezone = timezone)
      } else {
        conn <- RPresto::dbConnect(drv, user = username,
                                   password = password, host = host, port = port, schema = schema, catalog = catalog, session.timezone = Sys.timezone(location = TRUE))
      }
      connection_pool[[key]] <- conn
    }
  } else if (type == "odbc") {
    # do those package loading only when we need to use odbc in this if statement,
    # so that we will not have error at our server environmemnt where RODBC is not there.
    if(!requireNamespace("RODBC")){stop("package RODBC must be installed.")}

    loadNamespace("RODBC")
    connect <- function() {
      if(dsn != ""){ # for Dremio DSN and other DSN based ODBC connection.
        connstr <- stringr::str_c("RODBC::odbcConnect(dsn = '", dsn , "'")
        if(username != ""){
          connstr <- stringr::str_c(connstr, ", uid = '", username, "'")
        }
        if(password != ""){
          connstr <- stringr::str_c(connstr, ", pwd = '", password, "'")
        }
        if(additionalParams == ""){
          connstr <- stringr::str_c(connstr, ")")
        } else {
          connstr <- stringr::str_c(connstr, ",", additionalParams, ")")
        }
        conn <- eval(parse(text=connstr))
      } else if (host != "") { # for dremio direct access
        # Until Dremio ODBC Driver 1.3.14.1043 for Mac, Dremio ODBC driver's name was
        # "Dremio ODBC Driver" on Mac, but it changed to "Dremio Connector", which is same as the Window version
        # of their ODBC driver, at this version.
        # So we no longer need to switch ODBC driver name by OS.
        # We handled this change at v4.1.0.4 by releasing Mac only patch.
        connstr <- "DRIVER=Dremio Connector"
        connstr <- stringr::str_c(connstr, ";HOST=", host, ";ConnectionType=Direct;AuthenticationType=Plain;Catalog=DREMIO;PORT=", port, ";UID=", username, ";PWD=", password)
        conn <- RODBC::odbcDriverConnect(connstr)
      }
      if (conn == -1) {
        # capture warning and throw error with the message.
        # odbcConnect() returns -1 and does not stop execution even if connection fails.
        # TODO capture.output() might cause error on windows with multibyte chars.
        stop(paste("ODBC connection failed.", capture.output(warnings())))
      }

      # For some reason, calling RODBC::sqlTables() works around Actual Oracle Driver for Mac issue
      # that it always returns 0 rows.
      # Since we want this to be done without sacrificing performance,
      # we are adding dummy catalog/schema condition to make it return nothing.
      # Since it does not have performance impact, we are just calling it
      # unconditionally rather than first checking which ODBC driver is used for the connection.
      RODBC::sqlTables(conn, catalog = "dummy", schema = "dummy")
      conn
    }
    # Check pool only when connection pooling is on. To avoid getting error from timed-out connection,
    # we use connection pooling for ODBC only while data source dialog is open.
    # TODO: We may be able to check connection instead by RODBC::sqlTable() or something instead of this,
    # but we are not very sure of a sure way to check connection for all possible types of ODBC databases.
    if (user_env$pool_connection) {
      key <- paste(type, dsn, host, username, additionalParams, driver, sep = ":")
      conn <- connection_pool[[key]]
    }
    if (is.null(conn)) {
      conn <- connect()
      if (user_env$pool_connection) { # pool connection if connection pooling is on.
        connection_pool[[key]] <- conn
      }
    }
  } else if (type == "dbiodbc") {
    # do those package loading only when we need to use odbc in this if statement,
    # so that we will not have error at our server environment where odbc is not there.
    if(!requireNamespace("odbc")){stop("package odbc must be installed.")}

    loadNamespace("odbc")
    hoststr = ""
    if(!is.null(host)) {
      hoststr = host;
    }
    driverstr = ""
    if(!is.null(driver)) {
      driverstr = driver;
    }
    key <- paste(type, subType, dsn, hoststr, username, additionalParams, driverstr, timezone, connectionString, sep = ":")
    conn <- connection_pool[[key]]
    conn <- NULL
    if (!is.null(conn)){
      tryCatch({
        # test connection
        result <- DBI::dbGetQuery(conn,"select 1")
        if (!is.data.frame(result)) { # it can fail by returning NULL rather than throwing error.
          tryCatch({ # try to close connection and ignore error
            DBI::dbDisconnect(conn)
          }, warning = function(w) {
          }, error = function(e) {
          })
          conn <- NULL
          # fall through to getting new connection.
        } else if (!DBI::dbIsValid(conn)) {
          tryCatch({ # try to close connection and ignore error
            DBI::dbDisconnect(conn)
          }, warning = function(w) {
          }, error = function(e) {
          })
          conn <- NULL
        }
      }, error = function(err) {
        tryCatch({ # try to close connection and ignore error
          DBI::dbDisconnect(conn)
        }, warning = function(w) {
        }, error = function(e) {
        })
        conn <- NULL
        # fall through to getting new connection.
      })
    }
    connect <- function() {
      loc <- Sys.getlocale(category = "LC_CTYPE")
      # loc looks like "Japanese_Japan.932", so split it with dot ".".
      encoding <- stringr::str_split(loc, pattern = "\\.")

      if(is.null(subType) || subType == '' || subType == "dsn"){ # for dsn based connection case.
        connstr <- stringr::str_c("DBI::dbConnect(odbc::odbc(), dsn = '", dsn , "'")
        if(username != ""){
          connstr <- stringr::str_c(connstr, ", uid = '", username, "'")
        }
        if(password != ""){
          connstr <- stringr::str_c(connstr, ", pwd = '", password, "'")
        }


        # For Windows, set encoding to make sure non-ascii data is handled properly.
        # ref: https://github.com/r-dbi/odbc/issues/153
        if (is.win <- Sys.info()['sysname'] == 'Windows' && length(encoding[[1]]) == 2 && encoding[[1]][[2]] != "utf8") {
          # encoding looks like: [1] "Japanese_Japan" "932" so check the second part exists or not.
          connstr <- stringr::str_c(connstr, ", encoding = '", encoding[[1]][[2]], "'")
        }
        if (timezone != "") { # if timezone is set, use it for timezone and timezone_out arguments.
          connstr <- stringr::str_c(connstr, ", timezone = '", timezone, "'")
          connstr <- stringr::str_c(connstr, ", timezone_out = '", timezone, "'")
        }
        if(additionalParams == ""){
          connstr <- stringr::str_c(connstr, ")")
        } else {
          connstr <- stringr::str_c(connstr, ",", additionalParams, ")")
        }
        conn <- eval(parse(text=connstr))
      } else if (subType == "conn_str_kv") { # for key/value connection string case. (e.g. host=server1)
        connectionString <- ""
        hasArgument <- FALSE
        if (!is.null(driver) && driver != "") {
          connectionString <- stringr::str_c(connectionString, "Driver=", driver)
          hasArgument = TRUE
        }
        if (!is.null(host) && host != "") {
          if (hasArgument) {
            connectionString <- stringr::str_c(connectionString, ";")
          }
          connectionString <- stringr::str_c(connectionString, "Server=", host)
          hasArgument <- TRUE
        }
        if (port != "") {
          if (hasArgument) {
            connectionString <- stringr::str_c(connectionString, ";")
          }
          connectionString <- stringr::str_c(connectionString, "Port=", port)
          hasArgument <- TRUE
        }
        if (databaseName != "") {
          if (hasArgument) {
            connectionString <- stringr::str_c(connectionString, ";")
          }
          connectionString <- stringr::str_c(connectionString, "Database=", databaseName)
          hasArgument <- TRUE
        }
        if (username != "") {
          if (hasArgument) {
            connectionString <- stringr::str_c(connectionString, ";")
          }
          connectionString <- stringr::str_c(connectionString, "UID=", username)
          hasArgument <- TRUE
        }
        if (password != "") {
          if (hasArgument) {
            connectionString <- stringr::str_c(connectionString, ";")
          }
          connectionString <- stringr::str_c(connectionString, "PWD=", password)
          hasArgument <- TRUE
        }
        if (additionalParams != "") {
          if (hasArgument) {
            connectionString <- stringr::str_c(connectionString, ";")
          }
          # The "additionalParams" is passed as 'a=1,b=2,c=3'. Replace"," with ";" so that connection string becomes a=1;b=2;c=3
          connectionString <- stringr::str_c(connectionString,stringr::str_replace(additionalParams, ",", ";"));
        }
        if (is.win <- Sys.info()['sysname'] == 'Windows' && length(encoding[[1]]) == 2 && encoding[[1]][[2]] != "utf8") {
          # encoding looks like: [1] "Japanese_Japan" "932" so check the second part exists or not.
          if (timezone != "") {
            conn <- DBI::dbConnect(odbc::odbc(),
                                   .connection_string = connectionString,
                                   encoding = encoding[[1]][[2]],
                                   timezone = timezone,
                                   timezone_out = timezone,
                                   bigint = "numeric")
          } else {
            conn <- DBI::dbConnect(odbc::odbc(),
                                   .connection_string = connectionString,
                                   encoding = encoding[[1]][[2]],
                                   bigint = "numeric")
          }
        } else if (timezone != "") {
          conn <- DBI::dbConnect(odbc::odbc(),
                                 .connection_string = connectionString,
                                 timezone = timezone,
                                 timezone_out = timezone,
                                 bigint = "numeric")
        } else {
          conn <- DBI::dbConnect(odbc::odbc(),
                                 .connection_string = connectionString,
                                 bigint = "numeric")
        }
      } else if (!is.null(connectionString) && connectionString != '' && (is.null(subType) || subType == '' || subType == 'conn_str_text')) { # For manually entered connection string case.
        if (is.win <- Sys.info()['sysname'] == 'Windows' && length(encoding[[1]]) == 2 && encoding[[1]][[2]] != "utf8") {
          # encoding looks like: [1] "Japanese_Japan" "932" so check the second part exists or not.
          if (timezone != "") { # both encoding and timezone.
            conn <- DBI::dbConnect(odbc::odbc(),
                                   .connection_string = connectionString,
                                   encoding = encoding[[1]][[2]],
                                   timezone = timezone,
                                   timezone_out = timezone,
                                   bigint = "numeric")
          } else { # encoding only
            conn <- DBI::dbConnect(odbc::odbc(),
                                   .connection_string = connectionString,
                                   encoding = encoding[[1]][[2]],
                                   bigint = "numeric")
          }
        } else if (timezone != "") { # no encoding but timezone.
          conn <- DBI::dbConnect(odbc::odbc(),
                                 .connection_string = connectionString,
                                 timezone = timezone,
                                 timezone_out = timezone,
                                 bigint = "numeric")
        } else { # no encoding no timezone.
          conn <- DBI::dbConnect(odbc::odbc(),
                                 .connection_string = connectionString,
                                 bigint = "numeric")
        }
      }
      if (is.null(conn)) {
        # capture warning and throw error with the message.
        # odbcConnect() returns -1 and does not stop execution even if connection fails.
        # TODO capture.output() might cause error on windows with multibyte chars.
        stop(paste("ODBC connection failed.", capture.output(warnings())))
      }
      conn
    }
    if (is.null(conn)) {
      conn <- connect()
      if (user_env$pool_connection) { # pool connection if connection pooling is on.
        connection_pool[[key]] <- conn
      }
    }
  } else if (type == "teradata" || type == "access") {
    # do those package loading only when we need to use odbc in this if statement,
    # so that we will not have error at our server environment where odbc is not there.
    if(!requireNamespace("odbc")){stop("package odbc must be installed.")}

    loadNamespace("odbc")
    hoststr = ""
    if(!is.null(host)) {
      hoststr = host;
    }
    key <- paste(type, dsn, hoststr, username, additionalParams, driver, timezone, sep = ":")
    conn <- connection_pool[[key]]
    conn <- NULL
    if (!is.null(conn)){
      tryCatch({
        # test connection
        result <- DBI::dbGetQuery(conn,"select 1")
        if (!is.data.frame(result)) { # it can fail by returning NULL rather than throwing error.
          tryCatch({ # try to close connection and ignore error
            DBI::dbDisconnect(conn)
          }, warning = function(w) {
          }, error = function(e) {
          })
          conn <- NULL
          # fall through to getting new connection.
        } else if (!DBI::dbIsValid(conn)) {
          tryCatch({ # try to close connection and ignore error
            DBI::dbDisconnect(conn)
          }, warning = function(w) {
          }, error = function(e) {
          })
          conn <- NULL
        }
      }, error = function(err) {
        tryCatch({ # try to close connection and ignore error
          DBI::dbDisconnect(conn)
        }, warning = function(w) {
        }, error = function(e) {
        })
        conn <- NULL
        # fall through to getting new connection.
      })
    }
    connect <- function() {
      if(dsn != ""){ #
        connstr <- stringr::str_c("DBI::dbConnect(odbc::odbc(), dsn = '", dsn , "'")
        if(username != ""){
          connstr <- stringr::str_c(connstr, ", uid = '", username, "'")
        }
        if(password != ""){
          connstr <- stringr::str_c(connstr, ", pwd = '", password, "'")
        }

        loc <- Sys.getlocale(category = "LC_CTYPE")
        # loc looks like "Japanese_Japan.932", so split it with dot ".".
        encoding <- stringr::str_split(loc, pattern = "\\.")

        # For Windows, set encoding to make sure non-ascii data is handled properly.
        # ref: https://github.com/r-dbi/odbc/issues/153
        if (is.win <- Sys.info()['sysname'] == 'Windows' && length(encoding[[1]]) == 2 && encoding[[1]][[2]] != "utf8") {
          # encoding looks like: [1] "Japanese_Japan" "932" so check the second part exists or not.
          connstr <- stringr::str_c(connstr, ", encoding = '", encoding[[1]][[2]], "'")
        }
        if (timezone != "") { # if timezone is set, use it for timezone and timezone_out arguments.
          connstr <- stringr::str_c(connstr, ", timezone = '", timezone, "'")
          connstr <- stringr::str_c(connstr, ", timezone_out = '", timezone, "'")
        }
        if(additionalParams == ""){
          connstr <- stringr::str_c(connstr, ")")
        } else {
          connstr <- stringr::str_c(connstr, ",", additionalParams, ")")
        }
        conn <- eval(parse(text=connstr))
      } else if (host != "") {
        # TODO: Implement direct connect
      }
      if (is.null(conn)) {
        # capture warning and throw error with the message.
        # odbcConnect() returns -1 and does not stop execution even if connection fails.
        # TODO capture.output() might cause error on windows with multibyte chars.
        stop(paste("ODBC connection failed.", capture.output(warnings())))
      }
      conn
    }
    if (is.null(conn)) {
      conn <- connect()
      if (user_env$pool_connection) { # pool connection if connection pooling is on.
        connection_pool[[key]] <- conn
      }
    }
  } else if (type == "mssqlserver") {# The type sqlserver is already used for RODBC based one and "mssqlserver" is passed from Exploratory Desktop.

    # If the platform is Linux, set the below predefined driver installed on Collaboration Server
    # so that this data soure can be scheduled.
    if(Sys.info()["sysname"]=="Linux"){
      driver <-  "ODBC Driver 18 for SQL Server";
    }
    # mssqlserver uses odbc package instead of RODBC for two reasons:
    # 1) The "ODBC Driver 17 for SQL Server" driver does not work on Mac with RODBC.
    # 2) To allow users to schedule the data source on server side.
    # So make sure odbc package is installed.
    if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
    if(!requireNamespace("odbc")){stop("package odbc must be installed.")}
    key <- paste("mssqlserver", host, port, databaseName, username, timezone, additionalParams, sep = ":")
    conn <- connection_pool[[key]]
    if (!is.null(conn)){
      tryCatch({
        # test connection
        result <- DBI::dbGetQuery(conn,"select 1")
        if (!is.data.frame(result)) { # it can fail by returning NULL rather than throwing error.
          tryCatch({ # try to close connection and ignore error
            DBI::dbDisconnect(conn)
          }, warning = function(w) {
          }, error = function(e) {
          })
          conn <- NULL
          # fall through to getting new connection.
        }
      }, error = function(err) {
        tryCatch({ # try to close connection and ignore error
          DBI::dbDisconnect(conn)
        }, warning = function(w) {
        }, error = function(e) {
        })
        conn <- NULL
        # fall through to getting new connection.
      })
    }
    # if the connection is null or the connection is invalid, create a new one.
    if (is.null(conn) || !DBI::dbIsValid(conn)) {
      # For Windows, set encoding to make sure non-ascii data is handled properly.
      # ref: https://github.com/r-dbi/odbc/issues/153
      if (timezone == "") {
        timezone <- "UTC" # if timezone is not provided use UTC as default timezone. This is also the default for odbc::dbConnect.
      }

      loc <- Sys.getlocale(category = "LC_CTYPE")
      # loc looks like "Japanese_Japan.932", so split it with dot ".".
      encoding <- stringr::str_split(loc, pattern = "\\.")
      connectionString <- stringr::str_c(
        "Driver=", driver, ";Server=tcp:", host, ",", port, ";Database=", databaseName,
        ";Uid=", username, ";Pwd=", password
      );
      if (additionalParams != "") {
        connectionString <- stringr::str_c(connectionString, ";", additionalParams);
      }

      if (is.win <- Sys.info()['sysname'] == 'Windows' && length(encoding[[1]]) == 2 && encoding[[1]][[2]] != "utf8") {
        # encoding looks like: [1] "Japanese_Japan" "932" so check the second part exists or not.
        conn <- DBI::dbConnect(odbc::odbc(),
                              .connection_string = connectionString,
                               encoding = encoding[[1]][[2]],
                               timezone = timezone,
                               timezone_out = timezone,
                               bigint = "numeric"
        )
      } else { # Without encoding
        conn <- DBI::dbConnect(odbc::odbc(),
                               .connection_string = connectionString,
                               timezone = timezone,
                               timezone_out = timezone,
                               bigint = "numeric"
        )
      }
      connection_pool[[key]] <- conn
    }
  } else if (type == "snowflake") {
    # If the platform is Linux, set the below predefined driver installed on Collaboration Server
    # so that this data source can be scheduled.
    if (Sys.info()["sysname"] == "Linux") {
      # The driver is passed as an argument when the API is called from Exploratory Desktop,
      # However,it overwrites the value for desktop with the Linux driver when run on server.
      # ref: https://docs.snowflake.com/en/user-guide/odbc-linux.html
      driver <-  "/usr/lib64/snowflake/odbc/lib/libSnowflake.so";
    }
    if (!requireNamespace("DBI")) {
      stop("package DBI must be installed.")
    }
    if (!requireNamespace("odbc")) {
      stop("package odbc must be installed.")
    }
    if (is.null(port) || port == "") {
      # https://docs.snowflake.com/en/user-guide/odbc-parameters.html
      port <- 443 # snowflake default port.
    }
    if (timezone == "") {
      timezone <- "UTC" # if timezone is not provided use UTC as default timezone. This is also the default for odbc::dbConnect.
    }

    key <- paste("snowflake", host, port, databaseName, username, timezone, additionalParams, sep = ":")
    conn <- connection_pool[[key]]
    if (!is.null(conn)) {
      tryCatch({
        # test connection
        result <- DBI::dbGetQuery(conn,"select 1")
        if (!is.data.frame(result)) { # it can fail by returning NULL rather than throwing error.
          tryCatch({ # try to close connection and ignore error
            DBI::dbDisconnect(conn)
          }, warning = function(w) {
          }, error = function(e) {
          })
          conn <- NULL
          # fall through to getting new connection.
        }
      }, error = function(err) {
        tryCatch({ # try to close connection and ignore error
          DBI::dbDisconnect(conn)
        }, warning = function(w) {
        }, error = function(e) {
        })
        conn <- NULL
        # fall through to getting new connection.
      })
    }
    # if the connection is null or the connection is invalid, create a new one.
    if (is.null(conn) || !DBI::dbIsValid(conn)) {

      loc <- Sys.getlocale(category = "LC_CTYPE")
      # loc looks like "Japanese_Japan.932", so split it with dot ".".
      encoding <- stringr::str_split(loc, pattern = "\\.")

      connectionString <- stringr::str_c(
        "Driver=", driver, ";Server=", host, ";Port=", port, ";Database=", databaseName,
        ";UID=", username, ";PWD=", password
      );
      if (additionalParams != "") {
        connectionString <- stringr::str_c(connectionString, ";", additionalParams);
      }

      if (is.win <- Sys.info()['sysname'] == 'Windows' && length(encoding[[1]]) == 2 && encoding[[1]][[2]] != "utf8") {
          # encoding looks like: [1] "Japanese_Japan" "932" so check the second part exists or not.
        conn <- DBI::dbConnect(odbc::odbc(),
                               .connection_string = connectionString,
                               encoding = encoding[[1]][[2]],
                               timezone = timezone,
                               timezone_out = timezone,
                               bigint = "numeric"
        )
      } else { # Without encoding
        conn <- DBI::dbConnect(odbc::odbc(),
                               .connection_string = connectionString,
                               timezone = timezone,
                               timezone_out = timezone,
                               bigint = "numeric"
        )
      }
      connection_pool[[key]] <- conn
    }
  }
  conn
}

#' Clears specified connection from the pool.
#' When there is an error from a connection, we should call this so that next call to getDBConnection
#' would return a newly created connection.
#' @export
clearDBConnection <- function(type, host = NULL, port = NULL, databaseName, username, catalog = "", schema = "", dsn="", additionalParams = "",
                              collection = "", isSSL = FALSE, authSource = NULL, cluster = NULL, connectionString = NULL, timezone = "",
                              sslClientCertKey = "", sslCA = "", subType = NULL, driver = "") {
  key <- ""
  if (type %in% c("mongodb")) {
    if(!is.na(connectionString) && connectionString != '') {
      # make sure to include collection as a key since connection varies per collection.
      key <- paste(connectionString, collection, sep = ":")
    } else if(!is.na(host) && host != ''){
      key <- paste("mongodb", host, port, databaseName, collection, username, toString(isSSL), authSource, additionalParams, sslClientCertKey, sep = ":")
    } else if (!is.na(cluster) && cluster != '') {
      key <- paste("mongodb", cluster, databaseName, collection, username, toString(isSSL), authSource, additionalParams, sslClientCertKey, sep = ":")
    }
    conn <- connection_pool[[key]]
    if (!is.null(conn)) {
      rm(conn)
    }
  }
  else if (type %in% c("postgres", "redshift", "vertica")) {
    # they use common key "postgres"
    key <- paste("postgres", host, port, databaseName, username, timezone, sep = ":")
    conn <- connection_pool[[key]]
    if (!is.null(conn)) {
      tryCatch({ # try to close connection and ignore error
        DBI::dbDisconnect(conn)
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
  }
  else if (type %in% c("mysql", "aurora")) {
    # they use common key "mysql"
    key <- paste("mysql", host, port, databaseName, username, timezone, sslCA, sep = ":")
    conn <- connection_pool[[key]]
    if (!is.null(conn)) {
      tryCatch({ # try to close connection and ignore error
        DBI::dbDisconnect(conn)
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
  }
  else if (type %in% c("presto", "treasuredata")) {
    # they use common key "presto"
    key <- paste("presto", host, port, catalog, schema, username, timezone, sep = ":")
    conn <- connection_pool[[key]]
    if (!is.null(conn)) {
      tryCatch({ # try to close connection and ignore error
        RPresto::dbDisconnect(conn)
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
  }
  else if(type %in% c("odbc","dbiodbc", "teradata", "access")) { # odbc
    if (type == "dbiodbc") {
      hoststr = ""
      if(!is.null(host)) {
        hoststr = host;
      }
      key <- paste(type, subType, dsn, hoststr, username, additionalParams, driver, timezone, connectionString, sep = ":")
    } else {
      key <- paste(type, dsn, username, additionalParams, timezone, sep = ":")
    }
    conn <- connection_pool[[key]]
    if (!is.null(conn)) {
      tryCatch({ # try to close connection and ignore error
        if(type == "dbiodbc" || type == "teradata") {
          DBI::dbDisconnect(conn)
        } else {
          RODBC::odbcClose(conn)
        }
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
  }
  else if (type %in% c("mssqlserver")) {
    key <- paste("mssqlserver", host, port, databaseName, username, timezone, sep = ":")
  }
  else if (type %in% c("snowflake")) {
    if (is.null(port) || port == "") {
      # https://docs.snowflake.com/en/user-guide/odbc-parameters.html
      port <- 443 # snowflake default port.
    }
    if (timezone == "") {
      timezone <- "UTC" # if timezone is not provided use UTC as default timezone. This is also the default for odbc::dbConnect.
    }
    key <- paste("snowflake", host, port, databaseName, username, timezone, sep = ":")
  }
  rm(list = key, envir = connection_pool) # Even if there is no matching key, this is harmless.
}

isConnecitonPoolEnabled <- function(type){
  type %in% c("dbiodbc", "odbc", "postgres", "redshift", "vertica", "mysql", "aurora", "presto", "treasuredata", "mssqlserver", "snowflake", "teradata")
}

getListOfTablesWithODBC <- function(conn){
  topLevels <- odbc::odbcListObjects(conn)
  schemas <- NULL
  df <- topLevels %>% dplyr::distinct(type)
  check <- df$type == c("catalog")
  if (all(check) == TRUE){
    # Desktop side shows only Schema and Tables so ignore the catalog and create a flat schema list.
    schemas <- purrr::map_dfr(topLevels$name, function(x){odbc::odbcListObjects(conn, catalog = x)}) %>% dplyr::distinct(name, type)
  } else {
    schemas <- topLevels
  }
  tables <- purrr::map_dfr(schemas$name, function(x){
    tryCatch({
      df <- data.frame(table_name = odbc::dbListTables(conn, schema = x))
      df <- df %>% mutate(schema_name = x)
      df
    }, error=function(condition){
      # If the user does not have access permission, some database throws an error
      # if this is the case, just ignore.
      data.frame()
    })
  })
  tables
}

#' @export
getListOfTables <- function(type, host, port, databaseName = NULL, username, password, catalog = "", schema = "", sslCA = ""){
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
  conn <- getDBConnection(type, host, port, databaseName, username, password, catalog, schema, sslCA = sslCA)

  tryCatch({
    tables <- DBI::dbListTables(conn)
  }, error = function(err) {
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection(type, host, port, databaseName, username, catalog = catalog, schema = schema)
    if (!isConnecitonPoolEnabled(type)) { # only if conn pool is not used yet
      tryCatch({ # try to close connection and ignore error
        DBI::dbDisconnect(conn)
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
    stop(err)
  })
  if (!!isConnecitonPoolEnabled(type)) { # only if conn pool is not used yet
    tryCatch({ # try to close connection and ignore error
      DBI::dbDisconnect(conn)
    }, warning = function(w) {
    }, error = function(e) {
    })
  }
  tables
}

#' @export
getListOfColumns <- function(type, host, port, databaseName, username, password, table){
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
  conn <- getDBConnection(type, host, port, databaseName, username, password)
  tryCatch({
    columns <- DBI::dbListFields(conn, table)
  }, error = function(err) {
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection(type, host, port, databaseName, username)
    if (!!isConnecitonPoolEnabled(type)) { # only if conn pool is not used yet
      tryCatch({ # try to close connection and ignore error
        DBI::dbDisconnect(conn)
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
    stop(err)
  })
  if (!!isConnecitonPoolEnabled(type)) { # only if conn pool is not used yet
    tryCatch({ # try to close connection and ignore error
      DBI::dbDisconnect(conn)
    }, warning = function(w) {
    }, error = function(e) {
    })
  }
  columns
}

#' API to execute a query that can be handled with DBI
#' @export
executeGenericQuery <- function(type, host, port, databaseName, username, password, query, catalog = "", schema = "", numOfRows = -1, timezone = "", sslCA = ""){
  if (type %in% c("mysql", "aurora")) { # In case of MySQL, just use queryMySQL, since it has workaround to read multibyte column names without getting garbled.
    df <- queryMySQL(host, port, databaseName, username, password, numOfRows = numOfRows, query, timezone = timezone, sslCA = sslCA)
    df <- readr::type_convert(df)
    # It is hackish, but to read multibyte character data correctly, type_convert helps for some reason.
    # There is small chance of column getting converted to unwanted type, but for our usage, that is unlikely, and being able to read multibyte outweighs the potential drawback.
    return(df)
  }
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
  conn <- getDBConnection(type, host, port, databaseName, username, password, catalog = catalog, schema = schema, timezone = timezone)
  tryCatch({
    query <- convertUserInputToUtf8(query)
    # set envir = parent.frame() to get variables from users environment, not papckage environment
    resultSet <- DBI::dbSendQuery(conn, glue_exploratory(query, .transformer = sql_glue_transformer, .envir = parent.frame()))
    df <- DBI::dbFetch(resultSet, n = numOfRows)
  }, error = function(err) {
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection(type, host, port, databaseName, username, catalog = catalog, schema = schema, timezone = timezone, sslCA = sslCA)
    if (!!isConnecitonPoolEnabled(type)) { # only if conn pool is not used yet
      tryCatch({ # try to close connection and ignore error
        DBI::dbDisconnect(conn)
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
    stop(err)
  })
  DBI::dbClearResult(resultSet)
  if (!!isConnecitonPoolEnabled(type)) { # only if conn pool is not used yet
    tryCatch({ # try to close connection and ignore error
      DBI::dbDisconnect(conn)
    }, warning = function(w) {
    }, error = function(e) {
    })
  }
  df
}

#' @export
queryNeo4j <- function(host, port,  username, password, query, isSSL = FALSE, ...){
  if(!requireNamespace("RNeo4j")){stop("package RNeo4j must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  url <- ifelse(isSSL == TRUE,  "https://" , "http://");
  url <- stringr::str_c(url, host,":",  port,  "/db/data");

  graph <- NULL
  if(!is.null(username) && !is.null(password)){
    graph = RNeo4j::startGraph(url, username = username, password = password)
  } else {
    graph = RNeo4j::startGraph(url)
  }
  query <- convertUserInputToUtf8(query)
  df <- RNeo4j::cypher(graph, query)
  df
}


#' @export
queryMySQL <- function(host, port, databaseName, username, password, numOfRows = -1, query, timezone = "", sslCA = "", ...){
  if(!requireNamespace("RMariaDB")){stop("package RMariaDB must be installed.")}
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}

  conn <- getDBConnection(type = "mysql", host = host, port = port, databaseName = databaseName, username = username, password = password, timezone = timezone, sslCA = sslCA)
  tryCatch({
    query <- convertUserInputToUtf8(query)
    # set envir = parent.frame() to get variables from users environment, not papckage environment
    resultSet <- RMariaDB::dbSendQuery(conn, glue_exploratory(query, .transformer = sql_glue_transformer, .envir = parent.frame()))
    df <- RMariaDB::dbFetch(resultSet, n = numOfRows)
  }, error = function(err) {
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection("mysql", host, port, databaseName, username, timezone = timezone)
    stop(err)
  })
  RMariaDB::dbClearResult(resultSet)
  colnames(df) <- iconv(colnames(df),from = "utf8", to = "utf8") # Work around to read multibyte column names without getting garbled.
  df
}

#' @export
queryPostgres <- function(host, port, databaseName, username, password, numOfRows = -1, query, timezone = "", ...){
  if(!requireNamespace("RPostgres")){stop("package RPostgres must be installed.")}
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}

  conn <- getDBConnection(type = "postgres", host = host, port = port, databaseName = databaseName, username = username, password = password, timezone = timezone)

  tryCatch({
    query <- convertUserInputToUtf8(query)
    # set envir = parent.frame() to get variables from users environment, not papckage environment
    # glue_sql does not quote Date or POSIXct. Let's use our sql_glue_transformer here.
    query <- glue_exploratory(query, .transformer=sql_glue_transformer, .envir = parent.frame())
    resultSet <- RPostgres::dbSendQuery(conn, query)
    df <- DBI::dbFetch(resultSet, n = numOfRows)
  }, error = function(err) {
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection("postgres", host, port, databaseName, username, timezone = timezone)
    stop(err)
  })
  RPostgres::dbClearResult(resultSet)
  df
}

#' @export
queryAmazonAthena <- function(driver = "", region = "", authenticationType = "IAM Credentials", s3OutputLocation = "",
                              user = "", password = "", additionalParams = "", query = "", numOfRows = -1,
                              stringsAsFactors = FALSE, as.is = TRUE, timezone = "", endpointOverride = "", workgroup = "",
                              useProxy = 0, proxyHost="", proxyPort = -1, proxyUID = "", proxyPWD = "", ...){
  if(!requireNamespace("odbc")){stop("package RODBC must be installed.")}
  conn <- getAmazonAthenaConnection(driver = driver, region = region, authenticationType = authenticationType,
                                    s3OutputLocation = s3OutputLocation, user = user, password = password,
                                    additionalParams = additionalParams, timezone = timezone,
                                    endpointOverride = endpointOverride, workgroup = workgroup,
                                    useProxy = useProxy, proxyHost = proxyHost, proxyPort = proxyPort, proxyUID = proxyUID, proxyPWD = proxyPWD)
  tryCatch({
    # For backward compatibility, if 0 is passed as numOfRows, change it to -1.
    # Previously with RODBC package, passing 0 means getting all rows. With odbc package, it needs to be -1 to get all rows.
    if (numOfRows == 0) {
      numOfRows = -1;
    }
    query <- convertUserInputToUtf8(query)
    # set envir = parent.frame() to get variables from users environment, not papckage environment
    query <- glue_exploratory(query, .transformer=sql_glue_transformer, .envir = parent.frame())
    resultSet <- DBI::dbSendQuery(conn, query)
    df <- DBI::dbFetch(resultSet, n = numOfRows)
    if (!is.data.frame(df)) {
      # when it is error, RODBC::sqlQuery() does not stop() (throw) with error most of the cases.
      # in such cases, df is a character vecter rather than a data.frame.
      clearAmazonAthenaConnection(driver = driver, region = region, authenticationType = authenticationType, s3OutputLocation = s3OutputLocation,
                        user = user, password = password, additionalParams = additionalParams, endpointOverride = endpointOverride,
                        workgroup = workgroup, useProxy = useProxy, proxyHost = proxyHost, proxyPort = proxyPort, proxyUID = proxyUID, proxyPWD = proxyPWD)
      stop(paste(df, collapse = "\n"))
    }
    if (!user_env$pool_connection) {
      # close connection if not pooling.
      tryCatch({ # try to close connection and ignore error
        DBI::dbDisconnect(conn)
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
  }, error = function(err) {
    # for some cases like conn not being an open connection, sqlQuery still throws error. handle it here.
    # clear connection in pool so that new connection will be used for the next try
    clearAmazonAthenaConnection(driver = driver, region = region, authenticationType = authenticationType, s3OutputLocation = s3OutputLocation,
                       user = user, password = password, additionalParams = additionalParams, endpointOverride = endpointOverride, workgroup = workgroup,
                       useProxy = useProxy, proxyHost = proxyHost, proxyPort = proxyPort, proxyUID = proxyUID, proxyPWD = proxyPWD)
    stop(err)
  })
  DBI::dbClearResult(resultSet)
  df
}


#' API to query ODBC database
#' @export
#' @param dsn - Data Source Name for the ODBC
#' @param username - Usernaame of the database
#' @param password - Password of the database
#' @param additionalParams - Additional parameters
#' @param numOfRows - Nuber of rows in result. 0 means fetch all rows
#' @param query - SQL query
#' @param stringsAsFactors - Flag to tell if you want to convert character data type to factor data type in result.
#' @param host - Server where the database is running.
#' @param port - Database port number
#' @param as.is - Flag to tell if you honor data types from ODBC
#' @param dataBaseName - For MS SQL Server - name of the SQL Database
#' @param driver - For MS SQL Server - namme of the ODBC driver
#' @param type - For MS SQL Server "mssqlserver" is passed as type. For others,"odbc" is passed as type.
#' @param catalog - For Snowflake's Warehouse.
#' @param timezone - For database session timezone.
#'
queryODBC <- function(dsn="", username="", password="", additionalParams="", numOfRows = 0, query, stringsAsFactors = FALSE, host="", port="", as.is = TRUE, databaseName="", driver = "", type = "", catalog = "", timezone = "", connectionString = "",  subType = "", ...){
  if(type == "") {
    type <- "odbc"
  }
  # if the type is dbiodbc (i.e. odbc package) and numOfRows is 0, it means it's migrated from RODBC
  # To fetch the all rows, odbc expects -1 instead of 0. So update the numOfRows as -1.
  if (type == "dbiodbc" && numOfRows == 0) {
    numOfRows = -1;
  }
  conn <- getDBConnection(type = type, host = host, port = port, NULL, username = username, password = password, dsn = dsn, additionalParams = additionalParams, databaseName = databaseName, driver = driver, catalog = catalog, timezone = timezone, connectionString = connectionString, subType = subType)
  tryCatch({
    query <- convertUserInputToUtf8(query)
    # set envir = parent.frame() to get variables from users environment, not papckage environment
    query <- glue_exploratory(query, .transformer=sql_glue_transformer, .envir = parent.frame())
    # now odbc package is used for MS SQL Server Data Source so use DBI APIs.
    # The type sqlserver is already used for RODBC based one so "mssqlserver" is passed from Exploratory Desktop.
    if (type == "mssqlserver" || type == "dbiodbc" || type == "snowflake" || type == "teradata" || type == "access") {
      if(!requireNamespace("odbc")){stop("package odbc must be installed.")}
      reset <- NULL
      resultSet <- DBI::dbSendQuery(conn, query)
      df <- DBI::dbFetch(resultSet, n = numOfRows)
    } else if(type == "odbc") { # For RODBC based ODBC Data Soruces, use RODBC API.
      if(!requireNamespace("RODBC")){stop("package RODBC must be installed.")}
      df <- RODBC::sqlQuery(conn, query, as.is = as.is, max = numOfRows, stringsAsFactors=stringsAsFactors)
    }

    if (!is.data.frame(df)) {
      # when it is error, RODBC::sqlQuery() does not stop() (throw) with error most of the cases.
      # in such cases, df is a character vecter rather than a data.frame.
      clearDBConnection(type, NULL, NULL, NULL, username, dsn = dsn, additionalParams = additionalParams)
      stop(paste(df, collapse = "\n"))
    }
    if (!user_env$pool_connection) {
      # close connection if not pooling.
      tryCatch({ # try to close connection and ignore error
        if(type == "odbc") {
          RODBC::odbcClose(conn)
        } else {
          DBI::dbDisconnect(conn)
        }
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
  }, error = function(err) {
    # for some cases like conn not being an open connection, sqlQuery still throws error. handle it here.
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection(type, NULL, NULL, NULL, username, dsn = dsn, additionalParams = additionalParams)
    stop(err)
  })
  # mssqlserver uses odbc package and DBI package
  # and it gets result set with DBI package.
  # So make sure to clear the result set.
  # For RDOBC based case, it does not use result set.
  if (type == "mssqlserver" || type == "dbiodbc" || type == "snowflake" || type == "teradata" || type == "access") {
    DBI::dbClearResult(resultSet)
  }
  if (type == "access") { # clear access connection so that lock file is removed.
    tryCatch({
      clearDBConnection(type, NULL, NULL, NULL, username, dsn = dsn, additionalParams = additionalParams)
      odbc::dbDisconnect(conn)
    })
  }
  df
}


#' Access twitter serch api
#' @param n - Maximum number of tweets.
#' @param lang - Language to filter result.
#' @param lastNDays - From how many days ago tweets should be searched.
#' @param searchString - Query to search.
#' @param tokenFileId - File id for aut
#' @param withSentiment - Whether there should be sentiment column caluculated by get_sentiment.
#' @param includeRts - Whether result should include retweets or not.
#' @export
getTwitter <- function(n=200, lang=NULL,  lastNDays=7, searchString, tokenFileId=NULL, withSentiment = FALSE, includeRts = FALSE, ...){
  if(!requireNamespace("rtweet")){stop("package rtweet must be installed.")}
  loadNamespace("lubridate")
  twitter_token = getTwitterToken(tokenFileId)
  twitter_token = rtweet:::check_token(twitter_token);
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

  # convert search string to UTF-8 before sending it on the wire on windows.
  searchString <- convertUserInputToUtf8(searchString)
  tweetList <- rtweet::search_tweets(q = searchString, token = twitter_token, n = n, lang = lang, verbose = TRUE, since = since,
                                     unitl = until, locale = locale, geocode = geocode, include_rts = includeRts,
                                     type = resultType,  retryonratelimit=TRUE)
  if(length(tweetList)>0){
    if(withSentiment){
      # calculate sentiment
      tweetList %>% dplyr::mutate(sentiment = get_sentiment(text))
    } else {
      tweetList
    }
  } else {
    stop('No Tweets found.')
  }

}


#' API to submit a Google Big Query Job
#' @export
submitGoogleBigQueryJob <- function(project, sqlquery, destination_table, write_disposition = "WRITE_TRUNCATE", tokenFieldId, useStandardSQL = FALSE,  ...){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  token <- getGoogleTokenForBigQuery(tokenFieldId)
  bigrquery::set_access_cred(token)
  sqlquery <- convertUserInputToUtf8(sqlquery)
  # pass desitiona_table to support large data
  # check if the query contains special key word for standardSQL
  # If we do not pass the useLegaySql argument, bigrquery set TRUE for it, so we need to expliclity set it to make standard SQL work.
  isStandardSQL <- stringr::str_detect(sqlquery, "#standardSQL")
  if(!isStandardSQL && useStandardSQL){
    isStandardSQL = TRUE; # honor value provided by paramerer
  }
  # set envir = parent.frame() to get variables from users environment, not papckage environment
  sqlquery <- glue_exploratory(sqlquery, .transformer=bigquery_glue_transformer, .envir = parent.frame())
  job <- bigrquery::bq_perform_query(query = sqlquery, billing = project,  use_legacy_sql = !isStandardSQL)
  bigrquery::bq_job_wait(job)
  meta <- bigrquery::bq_job_meta(job)
  isCacheHit <- meta$statistics$query$cacheHit
  # if cache hit case, totalBytesProcessed info is not available. So set it as -1
  totalBytesProcessed <- ifelse(isCacheHit, -1, meta$statistics$totalBytesProcessed)
  # if cache hit case, recordsWritten info is not avalable. So set it as -1
  numOfRowsProcessed <- ifelse(isCacheHit, -1, meta$statistics$query$queryPlan[[1]]$recordsWritten)
  dest <- meta$configuration$query$destinationTable
  result <- data.frame(tableId = dest$tableId, datasetId = dest$datasetId, numOfRows = numOfRowsProcessed, totalBytesProcessed = totalBytesProcessed)
}

#' API to get a data from google BigQuery table
#' @export
getDataFromGoogleBigQueryTable <- function(project, dataset, table, page_size = 10000, max_page, tokenFileId, max_connections = 8){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  # Remember original scipen
  original_scipen = getOption("scipen")
  tryCatch({
    # Workaround ref: https://github.com/r-dbi/bigrquery/issues/395
    # Temporary override it with 20 to workaround: Invalid value at 'start_index' (TYPE_UINT64), "1e+05" [invalid] error
    options(scipen = 20)
    token <- getGoogleTokenForBigQuery(tokenFileId)
    bigrquery::set_access_cred(token)
    tb <- bigrquery::bq_table(project = project, dataset = dataset, table = table)
    # Since Exploratory Desktop does not handle int64, convert int64 to numeric by passing bigint = "numeric" ref: https://bigrquery.r-dbi.org/reference/bq_table_download.html
    bigrquery::bq_table_download(tb,  page_size = page_size, max_results = max_page, bigint = "numeric", quiet = TRUE, max_connections = max_connections)
  }, finally = {
    # Set original scipen
    options(scipen = original_scipen)
  })
}

#' API to extract data from google BigQuery table to Google Cloud Storage
#' @export
extractDataFromGoogleBigQueryToCloudStorage <- function(project, dataset, table, destinationUri, tokenFileId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId)
  bigrquery::set_access_cred(token)
  # call forked bigrquery for submitting extract job
  table <- bigrquery::bq_table(project, dataset, table = table)
  job <- bigrquery::bq_perform_extract(x = table, destination_uris = destinationUri, print_header = TRUE, destination_format = "CSV", compression = "GZIP")
  job <- bigrquery::bq_job_wait(job)
  job
}

#' API to download data from Google Storage to client and create a data frame from it
#' @export
downloadDataFromGoogleCloudStorage <- function(bucket, folder, download_dir, tokenFileId){
  if(!requireNamespace("googleCloudStorageR")){stop("package googleCloudStorageR must be installed.")}
  if(!requireNamespace("googleAuthR")){stop("package googleAuthR must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId)
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)
  googleCloudStorageR::gcs_global_bucket(bucket)
  objects <- googleCloudStorageR::gcs_list_objects()
  # set bucket
  googleCloudStorageR::gcs_global_bucket(bucket)
  objects <- googleCloudStorageR::gcs_list_objects()
  # for each file extracted from Google BigQuery to Google Cloud Storage,
  # download the file to local temporary direcotry.
  # then delete the extracted files from Google Cloud Storage.
  lapply(objects$name, function(name){
    if(stringr::str_detect(name,stringr::str_c(folder, "/"))){
      googleCloudStorageR::gcs_get_object(name, overwrite = TRUE, saveToDisk = stringr::str_c(download_dir, "/", stringr::str_replace(name, stringr::str_c(folder, "/"),"")))
      googleCloudStorageR::gcs_delete_object(name, bucket = bucket)
    }
  });
  files <- list.files(path=download_dir, pattern = ".gz");
  # pass progress as FALSE to prevent SIGPIPE error on Exploratory Desktop.
  # To avoid the issue that bind_rows throws an error due to column data type mismatch,
  # First, import all the csv files with column data types as character, then convert the column data types with readr::type_convert.
  df <- lapply(files, function(file){readr::read_csv(stringr::str_c(download_dir, "/", file), col_types = readr::cols(.default = "c"), progress = FALSE)}) %>% dplyr::bind_rows() %>% readr::type_convert()
}

#' API to get a list of buckets from Google Cloud Storage
#' @export
listGoogleCloudStorageBuckets <- function(project, tokenFileId = "", service = "bigquery"){
  if(!requireNamespace("googleCloudStorageR")){stop("package googleCloudStorageR must be installed.")}
  if(!requireNamespace("googleAuthR")){stop("package googleAuthR must be installed.")}
  token <- '';
  if (service == "cloudstorage") {
    token <- getGoogleTokenForCloudStorage();
  } else if (service == "bigquery") {
    token <- getGoogleTokenForBigQuery(tokenFileId)
  }
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)
  # make sure to pass "noAcl" as projection so that Google won't limit maxResults as 200.
  # ref: https://cloud.google.com/storage/docs/json_api/v1/buckets/list
  googleCloudStorageR::gcs_list_buckets(projectId = project, maxResults = 1000, projection = c("noAcl"))
}

#' API to get a data from google BigQuery table
#' @export
saveGoogleBigQueryResultAs <- function(projectId, sourceDatasetId, sourceTableId, targetProjectId, targetDatasetId, targetTableId, tokenFileId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId)
  bigrquery::set_access_cred(token)

  src <- list(project_id = projectId, dataset_id = sourceDatasetId, table_id = sourceTableId)
  dest <- list(project_id = targetProjectId, dataset_id = targetDatasetId, table_id = targetTableId)
  bigrquery::copy_table(src, dest)
}

#' Get data from google big query
#' @param bucketProjectId - Google Cloud Storage/BigQuery project id
#' @param dataSet - Google BigQuery data tht your query result table is associated with
#' @param table - Google BigQuery table where query result is saved
#' @param bucket - Google Cloud Storage Bucket
#' @param folder - Folder under Google Cloud Storage Bucket where temp files are extracted.
#' @param tokenFileId - file id for auth token
#' @export
getDataFromGoogleBigQueryTableViaCloudStorage <- function(bucketProjectId, dataSet, table, bucket, folder, tokenFileId, ...){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  # submit a job to extract query result to cloud storage
  uri = stringr::str_c('gs://', bucket, "/", folder, "/", "exploratory_temp*.gz")
  job <- exploratory::extractDataFromGoogleBigQueryToCloudStorage(project = bucketProjectId, dataset = dataSet, table = table, uri,tokenFileId);
  # download tgzip file to client
  df <- exploratory::downloadDataFromGoogleCloudStorage(bucket = bucket, folder=folder, download_dir = tempdir(), tokenFileId = tokenFileId)
}

#' Get data from google big query
#' @param projectId - Google BigQuery project id
#' @param query - SQL query to get data
#' @param destinationTable - Google BigQuery table where query result is saved
#' @param pageSize - Number of items per page.
#' @param maxPage - maximum number of pages to retrieve.
#' @param writeDeposition - controls how your BigQuery write operation applies to an existing table.
#' @param tokenFileId - file id for auth token
#' @param bqProjectId - Id of the Project where Google Cloud Storage Bucket belongs
#' @param csBucket - Google Cloud Storage Bucket
#' @param bucketFolder - Folder under Google Cloud Storage Bucket where temp files are extracted.
#' @export
executeGoogleBigQuery <- function(project, query, destinationTable, pageSize = 100000, maxPage = 10, writeDisposition = "WRITE_TRUNCATE", tokenFileId, bqProjectId, csBucket=NULL, bucketFolder=NULL, max_connections = 8, useStandardSQL = FALSE, ...){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId)
  # Remember original scipen
  original_scipen = getOption("scipen")
  tryCatch({
    # Workaround ref: https://github.com/r-dbi/bigrquery/issues/395
    # Temporary override it with 20 to workaround: Invalid value at 'start_index' (TYPE_UINT64), "1e+05" [invalid] error
    options(scipen = 20)
    df <- NULL
    # if bucket is set, use Google Cloud Storage for extract and download
    if(!is.null(csBucket) && !is.na(csBucket) && csBucket != "" && !is.null(bucketFolder) && !is.na(bucketFolder) && bucketFolder != ""){
      # destination_table looks like 'exploratory-bigquery-project:exploratory_dataset.exploratory_bq_preview_table'
      dataSetTable = stringr::str_split(stringr::str_replace(destinationTable, stringr::str_c(bqProjectId,":"),""),"\\.")
      dataSet = dataSetTable[[1]][1]
      table = dataSetTable[[1]][2]
      bqtable <- NULL
      # submit a query to get a result (for refresh data frame case)
      # convertUserInputToUtf8 API call for query is taken care of by exploratory::submitGoogleBigQueryJob
      # so just pass query as is.
      result <- exploratory::submitGoogleBigQueryJob(project = bqProjectId, sqlquery = query, tokenFieldId =  tokenFileId, useStandardSQL = useStandardSQL);
      # extranct result from Google BigQuery to Google Cloud Storage and import
      # Since Google might assign new tableId and datasetId, always get datasetId and tableId from the job result (result is a data frame).
      # To get the only one value for datasetId and tableId, use dplyr::first.
      df <- getDataFromGoogleBigQueryTableViaCloudStorage(bqProjectId, as.character(dplyr::first(result$datasetId)), as.character(dplyr::first(result$tableId)), csBucket, bucketFolder, tokenFileId)
    } else {
      # direct import case (for refresh data frame case)

      # bigquery::set_access_cred is deprecated, however, switching to bigquery::bq_auth forces the oauth token refresh
      # inside of it. We don't want this since Exploratory Desktop always sends a valid oauth token and use it without refreshing it.
      # so for now, stick to bigrquery::set_access_cred
      bigrquery::set_access_cred(token)
      # check if the query contains special key word for standardSQL
      # If we do not pass the useLegaySql argument, bigrquery set TRUE for it, so we need to expliclity set it to make standard SQL work.
      isStandardSQL <- stringr::str_detect(query, "#standardSQL")
      if(!isStandardSQL && useStandardSQL) { # honor value provided by parameter
        isStandardSQL = TRUE;
      }
      # make sure to convert query to UTF8
      query <- convertUserInputToUtf8(query)
      # set envir = parent.frame() to get variables from users environment, not papckage environment
      query <- glue_exploratory(query, .transformer=bigquery_glue_transformer, .envir = parent.frame())
      tb <- bigrquery::bq_project_query(x = project, query = query, quiet = TRUE, use_legacy_sql = !isStandardSQL)
      # Since Exploratory Desktop does not handle int64, convert int64 to numeric by passing bigint = "numeric" ref: https://bigrquery.r-dbi.org/reference/bq_table_download.html
      df <- bigrquery::bq_table_download(x = tb, max_results = Inf, page_size = pageSize, bigint = "numeric", max_connections = max_connections, quiet = TRUE)
    }
    df
  }, finally = {
    # Set original scipen
    options(scipen = original_scipen)
  })
}

#' API to get billing projects for the OAuth token set to the current session.
#' The resulting billing projects are common between Big Query and Cloud Storage.
#' Since this API is called from Exploratory Desktop for both Big Query Setup UI and
#' Google Cloud Storage File list, it controls which OAuth token should be used for this
#' by the service argument.
#'
#' @export
getGoogleBigQueryProjects <- function(tokenFileId="", service = "bigquery"){
  if (!requireNamespace("bigrquery")) {
    stop("package bigrquery must be installed.")
  }
  warningMessage <- NULL
  warningHandler <- function(w){
    warningMessage <<- w
  }
  main <- function(){
    token <- ''
    if (service == "cloudstorage") {
      token <- getGoogleTokenForCloudStorage();
    } else if (service == "bigquery") {
      token <- getGoogleTokenForBigQuery(tokenFileId);
    }
    bigrquery::set_access_cred(token)
    bigrquery::bq_projects(page_size = 100, max_pages = Inf, warn = TRUE)
  }
  projects <- withCallingHandlers(main(), warning = warningHandler)
  # If the warning message contains "Unable to refresh token: invalid_client",
  # it means the OAuth token is not valid so raise the "OAuth token is not set for Google BigQuery" error that triggers
  # Exploratory Desktop OAuth token recovery process.
  if (stringr::str_detect(warningMessage$message, "Unable to refresh token: invalid_client")) {
    stop("OAuth token is not set for Google BigQuery")
  }
  projects
}

#' API to get datasets for a project
#' @export
getGoogleBigQueryDataSets <- function(project, tokenFileId=""){
  if (!requireNamespace("bigrquery")) {
    stop("package bigrquery must be installed.")
  }
  warningMessage <- NULL
  warningHandler <- function(w){
    warningMessage <<- w
  }
  main <- function(){
    token <- getGoogleTokenForBigQuery(tokenFileId);
    bigrquery::set_access_cred(token)
    # make sure to pass max_pages as Inf to get all the datasets
    resultdatasets <- bigrquery::bq_project_datasets(project, page_size=1000, max_pages=Inf);
    lapply(resultdatasets, function(x){x$dataset})
  }
  dataSets <- withCallingHandlers(main(), warning = warningHandler)
  # If the warning message contains "Unable to refresh token: invalid_client",
  # it means the OAuth token is not valid so raise the "OAuth token is not set for Google BigQuery" error that triggers
  # Exploratory Desktop OAuth token recovery process.
  if (stringr::str_detect(warningMessage$message, "Unable to refresh token: invalid_client")) {
    stop("OAuth token is not set for Google BigQuery")
  }
  dataSets
}

#' API to get tables for current project, data set
#' @export
getGoogleBigQueryTables <- function(project, dataset, tokenFileId=""){
  if (!requireNamespace("bigrquery")) {
    stop("package bigrquery must be installed.")
  }
  warningMessage <- NULL
  warningHandler <- function(w){
    warningMessage <<- w
  }
  main <- function(){
    token <- getGoogleTokenForBigQuery(tokenFileId);
    bigrquery::set_access_cred(token)
    # if we do not pass max_results (via page_size argument), it only returnss 50 items. so explicitly set it.
    # See https://cloud.google.com/bigquery/docs/reference/rest/v2/tabledata/list for max_results
    # If we pass large value to max_results (via page_size argument) like 1,000,000, Google BigQuery gives
    # Error: Invalid value at 'max_results.value' (TYPE_UINT32), "1e+06" [badRequest]
    # so set 10,000 as the default value.
    # Below is just getting a list of table names and not the actual table data.
    bqdataset <- bigrquery::bq_dataset(project = project, dataset = dataset)
    tables <- bigrquery::bq_dataset_tables(bqdataset, page_size = 10000);
    lapply(tables, function(x){x$table})
  }
  tables <- withCallingHandlers(main(), warning = warningHandler)
  # If the warning message contains "Unable to refresh token: invalid_client",
  # it means the OAuth token is not valid so raise the "OAuth token is not set for Google BigQuery" error that triggers
  # Exploratory Desktop OAuth token recovery process.
  if (stringr::str_detect(warningMessage$message, "Unable to refresh token: invalid_client")) {
    stop("OAuth token is not set for Google BigQuery")
  }
  tables
}

#' API to get table info
#' @export
getGoogleBigQueryTable <- function(project, dataset, table, tokenFileId=""){
  if (!requireNamespace("bigrquery")) {
    stop("package bigrquery must be installed.")
  }
  warningMessage <- NULL
  warningHandler <- function(w){
    warningMessage <<- w
  }
  main <- function(){
    token <- getGoogleTokenForBigQuery(tokenFileId);
    bigrquery::set_access_cred(token)
    table <- bigrquery::bq_table(project = project, dataset = dataset, table = table)
    bigrquery::bq_table_meta(table);
  }
  table <- withCallingHandlers(main(), warning = warningHandler)
  # If the warning message contains "Unable to refresh token: invalid_client",
  # it means the OAuth token is not valid so raise the "OAuth token is not set for Google BigQuery" error that triggers
  # Exploratory Desktop OAuth token recovery process.
  if (stringr::str_detect(warningMessage$message, "Unable to refresh token: invalid_client")) {
    stop("OAuth token is not set for Google BigQuery")
  }
  table
}

#' API to get tables for current project, data set
#' @export
deleteGoogleBigQueryTable <- function(project, dataset, table, tokenFileId=""){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId);
  bigrquery::set_access_cred(token)
  bigrquery::delete_table(project, dataset, table);
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
  original_locale = Sys.getlocale(category = "LC_CTYPE")
  tryCatch({
    .htmltables <- parse_html_tables(url, encoding)
    if (Sys.info()["sysname"] == "Windows") {
      # For Windows, temporary change LC_CTYPE to C to workaround the "invalid multibyte string" error raised from rvest.
      # please see https://github.com/r-lib/devtools/issues/544 for locale "C" workround for invalid multibyte string.
      Sys.setlocale(category="LC_CTYPE", locale="C");
    }
    res <- tibble::repair_names(rvest::html_table(.htmltables[[index]], fill=TRUE ,header=heading))
  }, finally = {
    if (Sys.info()["sysname"] == "Windows") {
      # Set back original LC_TYPE
      Sys.setlocale(category="LC_CTYPE", locale=original_locale);
    }
  })
  res
}


#' function to convert labelled class to factor
#' @export
#' @param df -  data frame
#' @param convertLabelledNumericToFactor - if this is TRUE, it converts the labelled numeric columns as Factor.
#' if this is FALSE, it converts labelled numeric columns as numeric.
handleLabelledColumns = function(df, convertLabelledNumericToFactor = FALSE){
  # check if column class is labelled or haven_labelled, and convert them to factor.
  # If labelled or haven_labelled are not converted to factor, applying jsonlite::toJSON to the data frame fails.
  is_labelled <- lapply(df, function(x){ any(class(x) %in% c("labelled", "haven_labelled"))})
  is_labelled <- unlist(is_labelled)
  df[is_labelled] <- lapply(df[is_labelled], function(x){
    if (convertLabelledNumericToFactor == FALSE & is.numeric(x)) {
      as.numeric(x)
    } else {
      haven::as_factor(x)
    }
  })
  df
}

#' Checks and tells the given data is whether in ndjson format
#' by looking at that the first line is a valid json or not.
#' x - URL or file path
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

#' Construct a data frame from json or ndjson data
#' ndjson stands for "Newline Delimited JSON" and
#' each line of ndjson data is a valid json value.
#' http://ndjson.org/
#'
#' ndjson format is popular and used in many places such as
#' Yelp academic data is based on.
#'
#' jsonlite::fromJSON can read standard json but not ndjson.
#' jsonlite::stream_in can read ndjson but not standard json.
#' This function internally detects the data type and
#' calls the appropriate function to read the data
#' and construct a data from either json or ndjson data.
#'
#' x: URL or file path to json/ndjson file
#' flatten: TRUE or FALSE. Used only json case
#' limit: Should limit the number of rows to retrieve. Not used now
#' since underlying technology (jsonlite) doesn't support it.
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

#' This function converts the given data frame object to JSON.
#' The benefit of using this function is that it can converts
#' the column data types that cannot be serialized by toJSON
#' to safe ones.
convertToJSON  <- function(x) {
  loadNamespace("jsonlite")
  .tmp.tojson <- x
  isdifftime <- sapply(.tmp.tojson, is.difftime)
  .tmp.tojson[isdifftime] <- lapply(.tmp.tojson[isdifftime], function(y) as.numeric(y))
  isperiod <- sapply(.tmp.tojson, is.period)
  .tmp.tojson[isperiod] <- lapply(.tmp.tojson[isperiod], function(y) as.character(y))
  jsonlite::toJSON(.tmp.tojson)
}

#' Workaround for toJSON output getting affected by SJIS damemoji characters
#' when LC_CTYPE is set to Windows Code Page 932 (SJIS).
#' Switch LC_CTYPE to Windows Code Page 1252 (Latin-1) before calling jsonlite::toJSON,
#' and switch it back to the original setting when done.
#' We do this since the JSON output is not broken under LC_CTYPE of Code Page 1252.
#' Since JSON output is in UTF-8 even on Windows, we should not have to go through
#' SJIS on the output path in the first place.
#' @export
toJSON <- function(...) {
  tryCatch({
    if (Sys.info()["sysname"] == "Windows") {
      orig_locale <- Sys.getlocale("LC_CTYPE")
      Sys.setlocale("LC_CTYPE", "English_United States.1252")
    }
    res <- jsonlite::toJSON(...)
  },
  finally={
    if (Sys.info()["sysname"] == "Windows") {
      Sys.setlocale("LC_CTYPE", orig_locale)
    }
  })
  res
}

#' Gives you a connection object based on the given
#' file locator string. It supports file path or URL now.
#' x - URL or file path
getConnectionObject <- function(x) {
  loadNamespace("stringr")
  if (stringr::str_detect(x, "://")) {
    return(url(x))
  } else {
    return(file(x, open = "r"))
  }
}

#' Run the type convert if it is a data frame.
typeConvert <- function(x) {
  loadNamespace("readr")
  if (is.data.frame(x))  readr::type_convert(x) else x
}

#' Create a data frame from the given object that can be transformed to data frame.
#' @export
toDataFrame <- function(x, guessDataType = TRUE) {
  if(is.data.frame(x)) {
    df <- x
  } else if (is.matrix(x)) {
    df <- as.data.frame(x, stringsAsFactors = FALSE)
  } else {
    # just in case for other data type case in future
    df <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  # if guessDataType is FALSE, return data frame as is.
  if(guessDataType == FALSE){
    df
  } else {
    typeConvert(df)
  }
}

#' API to create a temporary environment for RDATA staging
#' @export
createTempEnvironment <- function(){
  new.env(parent = globalenv())
}

#' API to get a list of data frames from a RDATA
#' @export
getObjectListFromRdata <- function(rdata_path, temp.space){
  # load RDATA to temporary env to prevent the pollution on global objects
  path <- rdata_path
  if (stringr::str_detect(rdata_path, "^https://") ||
      stringr::str_detect(rdata_path, "^http://") ||
      stringr::str_detect(rdata_path, "^ftp://")) {

    path <- download_data_file(rdata_path, "rdata")
  }
  temp.object <- load(path,temp.space)
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

#' API to get a data frame object from RDATA
#' @export
getObjectFromRdata <- function(rdata_path, object_name){
  # load RDATA to temporary env to prevent the polluation on global objects
  path <- rdata_path
  if (stringr::str_detect(rdata_path, "^https://") ||
      stringr::str_detect(rdata_path, "^http://") ||
      stringr::str_detect(rdata_path, "^ftp://")) {

    path <- download_data_file(rdata_path, "rdata")
  }
  temp.space = createTempEnvironment()
  load(path,temp.space)
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
  df <- tibble::repair_names(jsonlite::flatten(x))
  original_names <- names(df)
  # Remove tab, new line, carriage return, and backslash from column names
  clean_names <- original_names  %>% gsub("[\r\n\t\\]", "", .)
  names(df) <- clean_names
  # Remove row names.
  row.names(df) <- NULL
  df
}

#' Wrapper function for Janitor's clean_names
#' On top of janitor::clean_names "case" options, this wrapper API supports the below two cases too.
#' - remove_space which removes all the white spaces in column names
#' - trim_space which trims the trailing and leading white spaces in column names.
#'
clean_names <- function(dat, ...){
  dots <- list(...)
  # if remove_space option is passed, remove all spaces from the column names.
  if(length(dots) > 0 && dots$case == "remove_space") {
    dat %>% dplyr::rename_all(function(.) stringr::str_remove_all(., "[[:blank:]]"))
  } else if (length(dots) > 0 && dots$case == "trim_space") {
    dat %>% dplyr::rename_all(function(.) stringr::str_trim(., side = "both"))
  } else {
    janitor::clean_names(dat, ...)
  }
}

#' This checks name conflict and attach the file if there isn't any conflict
#' @export
checkSourceConflict <- function(files, encoding="UTF-8"){
  ret <- list()
  for (file in files){
    ret[[file]] <- tryCatch({
      env <- new.env()
      source(file, local=env, encoding = encoding)
      attached_objects <- ls(env)
      list(names = attached_objects)
    }, error = function(e){
      list(error = e[["message"]])
    })
  }
  ret
}

#' Returns US state names, abbreviations, numeric codes, divisions, or regions based on US state data.
#'
#' Example:
#' > exploratory::statecode(c("NY","CA", "IL"), "name")
#' [1] "New York"   "California" "Illinois"
#' > exploratory::statecode(c("New York","California","Illinois"), "code")
#' [1] "NY" "CA" "IL"
#' > exploratory::statecode(c("New York","California","Illinois"), "num_code")
#' [1] "36" "06" "17"
#'
#' @param input vector of US state names, abbreviations, or numeric codes.
#' @param output_type one of "alpha_code", "num_code", "name", "division", or "region"
#' @return character vector
#' @export
statecode <- function(input = input, output_type = output_type) {
  output_types <- c("alpha_code", "num_code", "name", "division", "region")
  if (!output_type %in% output_types) {
     stop("Output type not supported")
  }
  # get rid of space, period, apostrophe, hiphen, number, etc to normalize inputs.
  # e.g. California[4] is normalized as california
  input_normalized <- stringr::str_remove_all(input, "[:punct:]|[:digit:]|[:space:]") %>%  stringr::str_to_lower()
  # in case input is US state code.
  input_normalized_number <- stringr::str_remove_all(input, "[^0-9]")

  # return matching state info.
  # state_name_id_map data frame has all those state info plus normalized_name as the search key.
  # it first searches by US state names or abbreviations (normalized) then try with US State code.
  name_match <- match(input_normalized, state_name_id_map$normalized_name)
  key <- ifelse(!is.na(name_match),
                name_match, # if match found for normalized name, use it
                match(input_normalized_number, state_name_id_map$normalized_name)) # else try the state code match.
  res <- as.character(state_name_id_map[[output_type]][key])
}

#' It can add 'longitude' and 'latitude' columns to a data frame which has
#' a column with US state 2-letter codes.
#'
#' Example:
#' > exploratory::geocode_us_state(data.frame(state=c("CA", "NY")), "state")
#' state  longitude latitude
#' 1    CA -119.41793 36.77826
#' 2    NY  -74.21793 43.29943
#'
geocode_us_state <- function(df, statecode_colname) {
  mapping <- "state"
  names(mapping) <- statecode_colname
  df %>% left_join(us_state_coordinates, by=mapping)
}

#' It can add 'longitude' and 'latitude' columns to a data frame which has
#' a column with US state and county FIPS codes.
#'
#' Example:
#' > exploratory::geocode_us_county(data.frame(code=c("01003", "13005")), "code")
#'    code longitude latitude
#' 1 01003 -87.74607 30.65922
#' 2 13005 -82.38786 31.56333
geocode_us_county <- function(df, fipscode_colname) {
  mapping <- "fips"
  names(mapping) <- fipscode_colname
  df %>% left_join(us_county_coordinates, by=mapping)
}

#' It can add 'longitude' and 'latitude' columns to a data frame which has
#' a column with ISO 2-letter country codes.
#'
#' Example:
#' > exploratory::geocode_world_country(data.frame(code=c("JP", "GB")), "code")
#'   code  longitude latitude
#' 1   JP 138.252924 36.20482
#' 2   GB  -3.435973 55.37805
#'
geocode_world_country <- function(df, countrycode_colname, center.pacific.ocean=FALSE) {
  mapping <- "iso2c"
  names(mapping) <- countrycode_colname
  if (center.pacific.ocean) {
    df %>% left_join(world_country_coordinates_po_centered, by=mapping)
  } else {
    df %>% left_join(world_country_coordinates, by=mapping)
  }
}

#' It can add 'longitude' and 'latitude' columns to a data frame which has
#' a column with Japan prefecture names.
#'
#' Example:
#'
#' > exploratory::geocode_japan_prefecture(data.frame(name=c("東京","北海道")), "name")
#'     name longitude latitude
#' 1   東京  139.6917 35.68949
#' 2 北海道  141.3468 43.06461
#'
geocode_japan_prefecture <- function(df, prefecture_colname) {
  mapping <- "name"
  names(mapping) <- prefecture_colname
  df %>% left_join(jp_prefecture_coordinates, by=mapping)
}

#' Converts Japan prefecture names into various formats.
#' Currently, with output_type ="name", it converts names into the short name format,
#' which has a name without the suffix such as "-to", "-ken".
#' And with output_type = "code", it converts names into the prefecture code.
#'
#' Example:
#' > prefecturecode(c("東京都", "京都", "Kanagawa-ken", "Iwate", "あいち", "Kōchi", "gunma"), output_type="name")
#' [1] "東京"   "京都"   "神奈川" "岩手"   "愛知"   "高知"    "群馬"

prefecturecode <- function(prefecture, output_type="name") {
  if (output_type == "code") { # covert prefecture name to prefecture code
    loadNamespace("zipangu")
    prefecture = exploratory::prefecturecode(prefecture, output_type = "name")
    targetDF <- data.frame(prefecture_normalized = prefecture)
    answerDF <- zipangu::jpnprefs
    # create prefecture_normalized column from a prefecture_kanji column and removes "都", "府", "県" from it.
    answerDF <- answerDF %>% dplyr::mutate(prefecture_normalized = stringr::str_remove(prefecture_kanji, "[\u90FD\u5E9C\u770C]$"))
    # Join data frames by prefecture_normalized so that it can get a corresponding prefecture code.
    result <- targetDF %>% dplyr::left_join(answerDF, by = "prefecture_normalized")
    result$jis_code
  } else if (output_type == "name") {
    loadNamespace("stringr")
    # TODO: support other output types.
    # Clean up the input.
    pref_normalized <- stringr::str_trim(tolower(prefecture))
    # Remove trailing "tofuken". Do not remove "do" from "Hokkaido" and "to" from "Kyoto" (in Japanese).
    pref_normalized <- dplyr::if_else(pref_normalized=="\u4EAC\u90FD", pref_normalized, gsub("[\u90FD\u5E9C\u770C]$", "", pref_normalized))
    # Remove trailing "tofuken". Do not remove "do" from "Hokkaido" (in Roma-ji).
    pref_normalized <- gsub("[_ \\.\\-](to|fu|hu|ken)$", "", pref_normalized)
    # Convert "o" with macron to simple "o".
    pref_normalized <- gsub("\u014D", "o", pref_normalized)
    # jp_prefecture_name_id_map uses gumma as a mapping key, so to take care of "gunma" properly, convert "gunma" to "gumma.
    pref_normalized <- dplyr::if_else(pref_normalized=="gunma", "gumma" ,pref_normalized)
    # Return the matching IDs.
    return (as.character(jp_prefecture_name_id_map$id[match(pref_normalized, jp_prefecture_name_id_map$name)]))
  } else { #for other case, just return the prefecture.
    prefecture
  }
}

#' Returns city codes from the prefecture and city names.
#' Original geocode data is from https://geolonia.github.io/japanese-addresses/
#' @param prefecture Prefecture name
#' @param city City name.
#' @return 5-digit city code in a character vector
#' @export
city_code_japan <- function(prefecture, city) {
  name <- stringr::str_c(prefecture, city)
  # return matching city code.
  jp_city_name_code_map$code[match(name, jp_city_name_code_map$name)]
}

#' It adds the 'longitude' and 'latitude' columns to the given data frame
#' that contains the 5-digit Japan city code column.
#'
#' Original geocode data is from https://geolonia.github.io/japanese-addresses/
#' @param city_code_colname City code column name in the data frame.
#' @return data frame.
#' @export
geocode_japan_city <- function(df, city_code_colname) {
  mapping <- "code"
  names(mapping) <- c(city_code_colname)
  df %>% left_join(jp_city_coordinates, by=mapping)
}

#' Converts pair of state name and county name into county ID,
#' which is concatenation of FIPS state code and FIPS county code.
#'
#' Example:
#' > countycode(c("California", "CA"),c("San Francisco", "San Francisco"))
#' [1] "06075" "06075"
#' > countycode(c("MD", "MD", "MD"),c("Baltimore", "Baltimore City", "City of Baltimore"))
#' [1] "24005" "24510" "24510"
#'
#' @param state state name, or 2 letter state code
#' @param county county name. For an independent city that has a county with the same name, prefix with "City of " or suffix with " City".
#' @return character vector
#' @export
countycode <- function(state = state, county = county) {
  loadNamespace("stringr")
  # lower case and get rid of space, period, apostrophe, and hiphen to normalize inputs.
  state_normalized <- gsub("[ \\.\\'\\-]", "", tolower(state))
  county_normalized <- gsub("[ \\.\\'\\-]", "", tolower(county))
  # if county starts with "City of ", remove it and suffix with " City" to normalize it.
  county_normalized <- dplyr::if_else(stringr::str_detect(county_normalized, "^cityof"), paste0(stringr::str_sub(county_normalized, 7), "city"), county_normalized)
  county_normalized <- gsub("county$", "", county_normalized)
  # concatenate state name and county name.
  state_county <- stringr::str_c(state_normalized, " ", county_normalized)
  # return matching county ID.
  return (county_name_id_map$id[match(state_county, county_name_id_map$name)])
}

#' It selects the columns that matches with the given strings.
#' Invalid column names will be just ignored.
#'
#' Usage:
#' > mtcars %>% select_columns('mpg', 'abc', 'mt', 'wt')
#' mpg    wt
#' Mazda RX4           21.0 2.620
#' Mazda RX4 Wag       21.0 2.875
#' Datsun 710          22.8 2.320
#' Hornet 4 Drive      21.4 3.215
#'               :
#'               :
#'
#' @param x data frame
#' @param ... column name strings
#' @param exclude If you set it to TRUE, list of columns will be excluded.
#' @return data frame
#' @export
select_columns <- function(x, ..., exclude=FALSE) {
  if (exclude==TRUE) {
    col <- colnames(x)[colnames(x) %nin% list(...)]
  } else {
    col <- colnames(x)[colnames(x) %in% list(...)]
  }
  df <- x[, col]

  # If it selects only 1 column against the normal data.frame
  # the df becomes a vector, not data.frame. In that case,
  # we need to cast it. Note that if it is against dplyr tbl dataframe,
  # it works just fine and returns a data.frame object.
  if (!is.data.frame(df)){
    df <- data.frame(df)
    colnames(df) <- col
  }
  return (df)
}

#' API to clear excel cache file
#' @param url
#' @export
clear_cache_file <- function(url){
  options(tam.should.cache.datafile = FALSE)
  hash <- digest::digest(url, "md5", serialize = FALSE)
  tryCatch({
    filepath <- eval(as.name(hash))
    do.call(rm, c(as.name(hash)),envir = .GlobalEnv)
    unlink(filepath)
  }, error = function(e){
  })
}

#' API to download remote data file (excel, csv) from URL and cache it if necessary
#' it uses tempfile https://stat.ethz.ch/R-manual/R-devel/library/base/html/tempfile.html
#' and a R variable with name of hashed url is assigned to the path given by tempfile.
#' @param url
download_data_file <- function(url, type){
  shouldCacheFile <- getOption("tam.should.cache.datafile")
  filepath <- NULL
  hash <- digest::digest(url, "md5", serialize = FALSE)
  tryCatch({
    filepath <- getDownloadedFilePath(hash)
  }, error = function(e){
    # if url hash is not set as global vaiarlbe yet, it raises error that says object not found
    # which can be ignored
    filepath <- NULL
  })
  # Check if cached excel filepath exists for the URL
  if(!is.null(shouldCacheFile) && isTRUE(shouldCacheFile) && !is.null(filepath)){
    filepath
  } else {
    ext <- stringr::str_to_lower(tools::file_ext(url))
    # if no extension, assume the file extension as xlsx
    if(ext == ""){
      if(type == "excel"){
        ext = "xlsx"
      } else if (type == "csv") {
        ext = "csv"
      } else if (type == "rdata") {
        ext = "rdata"
      } else if (type == "log") {
        ext = "log"
      }
    }
    tmp <- tempfile(fileext = stringr::str_c(".", ext))

    # In case of using Rserve on linux, somehow it doesn't create a temporary
    # directory specified by tempdir() which is used as a part of temp file
    # path generated by tempfile(). So if you try to use that temp file path,
    # dump some data into it for example, it will fail because no such path
    # found. This function fails with the same reason at download.file below.
    #
    # It works fine from the R command line on linux, and it works
    # fine all the time on Mac and Windows regardless Rserv or not.
    #
    # The following command is harmless even if you have the directory already.
    # http://stackoverflow.com/questions/4216753/check-existence-of-directory-and-create-if-doesnt-exist
    dir.create(tempdir(), showWarnings = FALSE)

    tryCatch({
      # Get current timeout sec. Default is 60 sec.
      originalTimeout <- options("timeout")
      # Increase timeout to 10 minutes (600 sec)
      options("timeout" = 600)
      # Download file to tempoprary location
      download.file(url, destfile = tmp, mode = "wb")
    }, error = function(cond){
       stop(cond)
    }
    ,finally = {
      # Set the original timeout
      options("timeout" = originalTimeout)
    })
    # cache file
    if(!is.null(shouldCacheFile) && isTRUE(shouldCacheFile)){
      setDownloadedFilePath(hash, tmp)
    }
    tmp
  }
}

#'API that searches and imports multiple same structure Excel Sheets in the Excel Book and merge them to a single data frame
#'@export
searchAndReadExcelFileMultiSheets <- function(file, forPreview = FALSE, pattern = "", col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL, convertDataTypeToChar = TRUE, ...) {
  # search condition is case insensitive. (ref: https://www.regular-expressions.info/modifiers.html, https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r)
  sheets <- readxl::excel_sheets(file)
  sheets <- sheets <- sheets[stringr::str_detect(sheets, pattern)]
  if (length(sheets) == 0) {
    stop(paste0('EXP-DATASRC-3 :: ', jsonlite::toJSON(file), ' :: There is no file in the folder that matches with the specified condition.')) # TODO: escape folder name.
  }
  exploratory::read_excel_file_multi_sheets(file = file, forPreview = forPreview, sheets = sheets, col_names = col_names, col_types = col_types, na = na, skip = skip, trim_ws = trim_ws, n_max = n_max,
                                use_readxl = use_readxl, detectDates = detectDates, skipEmptyRows = skipEmptyRows, skipEmptyCols = skipEmptyCols, check.names = check.names,
                                tzone = tzone, convertDataTypeToChar = convertDataTypeToChar)

}
#'API that searches and imports multiple same structure Excel files and merge them to a single data frame
#'@export
searchAndReadExcelFiles <- function(folder, forPreview = FALSE, pattern = "", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL, convertDataTypeToChar = TRUE, ...) {
  # search condition is case insensitive. (ref: https://www.regular-expressions.info/modifiers.html, https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r)
  if (!dir.exists(folder)) {
    stop(paste0('EXP-DATASRC-2 :: ', jsonlite::toJSON(folder), ' :: The folder does not exist.')) # TODO: escape folder name.
  }
  files <- list.files(path = folder, pattern = stringr::str_c("(?i)", pattern), full.names = T)
  if (length(files) == 0) {
    stop(paste0('EXP-DATASRC-3 :: ', jsonlite::toJSON(folder), ' :: There is no file in the folder that matches with the specified condition.')) # TODO: escape folder name.
  }
  exploratory::read_excel_files(files = files, forPreview = forPreview, sheet = sheet, col_names = col_names, col_types = col_types, na = na, skip = skip, trim_ws = trim_ws, n_max = n_max,
                                use_readxl = use_readxl, detectDates = detectDates, skipEmptyRows = skipEmptyRows, skipEmptyCols = skipEmptyCols, check.names = check.names,
                                tzone = tzone, convertDataTypeToChar = convertDataTypeToChar)

}

#'API that imports multiple same structure Excel sheets in the Excel Book and merge them to a single data frame
#'@export
read_excel_file_multi_sheets <- function(file, forPrevew = FALSE, sheets = c(1), col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL, convertDataTypeToChar = TRUE, ...) {
  # set name to the files so that it can be used for the "id" column created by purrr::map_dfr.
  sheets <- setNames(as.list(sheets), sheets)
  df <- purrr::map_dfr(sheets, .f = ~exploratory::read_excel_file(
                       path = file,
                       sheet = .x,
                       col_names = col_names,
                       col_types = col_types,
                       na = na,
                       skip = skip,
                       trim_ws = trim_ws,
                       n_max = n_max,
                       use_readxl = use_readxl,
                       detectDates = detecDates,
                       skipEmptyRows = skipEmptyRows,
                       skipEmptyCols = skipEmptyCols,
                       check.names = check.names,
                       tzone = tzone, convertDataTypeToChar = convertDataTypeToChar),
                       .id = "exp.file.id") %>% mutate(exp.file.id = basename(exp.file.id)) # extract file name from full path with basename.
  id_col <- avoid_conflict(colnames(df), "id")
  # copy internal exp.file.id to the id column.
  df[[id_col]] <- df[["exp.file.id"]]
  # drop internal column and move the id column to the very beginning.
  df %>% dplyr::select(!!rlang::sym(id_col), dplyr::everything(), -exp.file.id)
}


#'API that imports multiple same structure Excel files and merge it to a single data frame
#'@export
read_excel_files <- function(files, forPreview = FALSE, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL, convertDataTypeToChar = TRUE, ...) {
    # for preview mode, just use the first file.
    if (forPreview & length(files) > 0) {
      files <- files[1]
    }
    # set name to the files so that it can be used for the "id" column created by purrr::map_dfr.
    files <- setNames(as.list(files), files)
    df <- purrr::map_dfr(files, exploratory::read_excel_file, sheet = sheet,
           col_names = col_names,
           col_types = col_types,
           na = na,
           skip = skip,
           trim_ws = trim_ws,
           n_max = n_max,
           use_readxl = use_readxl,
           detectDates = detecDates,
           skipEmptyRows = skipEmptyRows,
           skipEmptyCols = skipEmptyCols,
           check.names = check.names,
           tzone = tzone, convertDataTypeToChar = convertDataTypeToChar,
           .id = "exp.file.id") %>% mutate(exp.file.id = basename(exp.file.id)) # extract file name from full path with basename.
    id_col <- avoid_conflict(colnames(df), "id")
    # copy internal exp.file.id to the id column.
    df[[id_col]] <- df[["exp.file.id"]]
    # drop internal column and move the id column to the very beginning.
    df %>% dplyr::select(!!rlang::sym(id_col), dplyr::everything(), -exp.file.id)
}

#'Wrapper for openxlsx::read.xlsx (in case of .xlsx file) and readxl::read_excel (in case of old .xls file)
#'Use openxlsx::read.xlsx since it's memory footprint is less than that of readxl::read_excel and this creates benefit for users with less memory like Windows 32 bit users.
#'@export
read_excel_file <- function(path, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf, use_readxl = NULL, detectDates = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE, check.names = FALSE, tzone = NULL, convertDataTypeToChar = FALSE, ...){
  loadNamespace("openxlsx")
  loadNamespace('readxl')
  loadNamespace('stringr')
  if(is.null(use_readxl)){
    # check if it's running on 32bit machine.
    if(.Machine$sizeof.pointer == 4) {
      # to avoid out of memory use openxlsx instead of readxl
      use_readxl = FALSE
    } else {
      use_readxl = TRUE
    }
  }
  tryCatch({
    df <- NULL
    # for .xlsx file extension
    if(stringr::str_detect(path, '\\.xlsx') & use_readxl == FALSE) {
      # On Windows, if the path has multibyte chars, work around error from readxl::read_excel by copying the file to temp directory.
      if (Sys.info()[["sysname"]] == "Windows" && grepl("[^ -~]", path)) {
        new_path <- tempfile(fileext = stringr::str_c(".", tools::file_ext(path)))
        file.copy(path, new_path)
        if (n_max != Inf) {
          df <- openxlsx::read.xlsx(xlsxFile = new_path, rows=(skip+1):n_max, sheet = sheet, colNames = col_names, na.strings = na, skipEmptyRows = skipEmptyRows, skipEmptyCols = skipEmptyCols , check.names = check.names, detectDates = detectDates)
        } else {
          df <- openxlsx::read.xlsx(xlsxFile = new_path, sheet = sheet, colNames = col_names, startRow = skip+1, na.strings = na, skipEmptyRows = skipEmptyRows, skipEmptyCols = skipEmptyCols, check.names = check.names, detectDates = detectDates)
        }
        # Preserve original column name for backward compatibility (ref: https://github.com/awalker89/openxlsx/issues/102)
        # Calling read.xlsx again looks inefficient, but it seems this is the only solution suggested in the above issue page.
        colnames(df) <- openxlsx::read.xlsx(xlsxFile = new_path, sheet = sheet, rows = (skip+1), check.names = FALSE, colNames = FALSE) %>% as.character()
        file.remove(new_path)
      }
      else {
        if (n_max != Inf) {
          df <- openxlsx::read.xlsx(xlsxFile = path, rows=(skip+1):n_max, sheet = sheet, colNames = col_names, na.strings = na, skipEmptyRows = skipEmptyRows, skipEmptyCols = skipEmptyCols , check.names = check.names, detectDates = detectDates)
        } else {
          df <- openxlsx::read.xlsx(xlsxFile = path, sheet = sheet, colNames = col_names, startRow = skip+1, na.strings = na, skipEmptyRows = skipEmptyRows, skipEmptyCols = skipEmptyCols, check.names = check.names, detectDates = detectDates)
        }
        # Preserve original column name for backward comaptibility (ref: https://github.com/awalker89/openxlsx/issues/102)
        # Calling read.xlsx again looks inefficient, but it seems this is the only solution suggested in the above issue page.
        colnames(df) <- openxlsx::read.xlsx(xlsxFile = path, sheet = sheet, rows = (skip+1), check.names = FALSE, colNames = FALSE) %>% as.character()
      }
      # trim white space needs to be done first since it cleans column names
      if(trim_ws == TRUE) {
        # use trimws from base to remove ending and trailing white space for character columns
        df <- df %>% dplyr::mutate(dplyr::across(is.character, trimws))
      }
      if(col_names == FALSE) {
        # For backward compatibility, use X__1, X__2, .. for default column names
        columnNames <- paste("X", 1:ncol(df), sep = "__")
        colnames(df) <- columnNames
      }
    } else { # for old .xls file extension
      if (stringr::str_detect(path, "^https://") ||
          stringr::str_detect(path, "^http://") ||
          stringr::str_detect(path, "^ftp://")) {
        # need to download first since readxl::read_excel cannot work with URL.
        tmp <- download_data_file(path, "excel")
        df <- readxl::read_excel(tmp, sheet = sheet, col_names = col_names, col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max)
      } else {
        # On Windows, if the path has multibyte chars, work around error from readxl::read_excel by copying the file to temp directory.
        if (Sys.info()[["sysname"]] == "Windows" && grepl("[^ -~]", path)) {
          new_path <- tempfile(fileext = stringr::str_c(".", tools::file_ext(path)))
          file.copy(path, new_path)
          df <- readxl::read_excel(new_path, sheet = sheet, col_names = col_names, col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max)
          file.remove(new_path)
        }
        else {
          # If it's local file without multibyte path, simply call readxl::read_excel
          df <- readxl::read_excel(path, sheet = sheet, col_names = col_names, col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max)
        }
      }
      if(col_names == FALSE) {
        # For backward compatibility, use X__1, X__2, .. for default column names
        columnNames <- paste("X", 1:ncol(df), sep = "__")
        colnames(df) <- columnNames
      }
    }
    # readxl always uses UTC for POSIXct timezone so when converting POSIXct to Date, make sure to set tz as UTC
    # e.g.
    # > psx <- as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
    # > psx
    # [1] "2020-01-01 UTC"
    # > as.Date(psx, tz = "America/Los_Angeles")
    # [1] "2019-12-31"  # this is not what we want when converting the POSIXct to Date.
    # > as.Date(psx, tz = "UTC")
    # [1] "2020-01-01"  # this is the date we want when converting the POXIct to Date.
    #
    df <- df %>% dplyr::mutate_at(vars(colnames(df)[col_types == "date"]), funs(as.Date(., tz="UTC")))
    if(!is.null(tzone)) { # if timezone is specified, force the timezeon to POSIXct columns
      df <- df %>% dplyr::mutate_if(lubridate::is.POSIXct, funs(lubridate::force_tz(., tzone=tzone)))
    }

    # When this API is called from getExcelFilesFromS3, getExcelFilesFromGoogleDrive, and read_excel_files,
    # by default the convertDataTypeToChar is set as TRUE to covert the resulting data frame columns' data type as character.
    # This is required to make sure that merging the Excel based data frames doesn't error out due to column data types mismatch.
    # Once the data frames merging is done, readr::type_convert is called from Exploratory Desktop to restore the column data types.
    # We don't want to rely on readxl::read_excel's col_types argument "text" since this option converts Date or POSIXct column data as number instead of "2021-01-01" style text.
    if (convertDataTypeToChar) {
      df <- df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character));
    }
    df
  }, error = function(e) {
    # The error message for non-existent file looks like this - "`path` does not exist: '<full-path>'"
    # (The single quotes in the above actually are curly quotes. I'm avoiding typing them here not to break the format of this code.)
    if (stringr::str_detect(stringr::str_to_lower(e$message), "`path` does not exist")) {
      stop(paste0('EXP-DATASRC-14 :: ', jsonlite::toJSON(path), ' :: The file does not exist.'))
    } else if (stringr::str_detect(stringr::str_to_lower(e$message), "cannot open url")) { # The actual error looks like this - "cannot open URL '<url>'"
      stop(paste0('EXP-DATASRC-15 :: ', jsonlite::toJSON(c(path, e$message)), ' :: Failed to download from the URL.'))
    } else {
      stop(paste0('EXP-DATASRC-13 :: ', jsonlite::toJSON(c(path, e$message)), ' :: Failed to import file.'))
    }
  })
}

#'Wrapper for readxl::excel_sheets to support remote file
#'@export
get_excel_sheets <- function(path){
  loadNamespace("readxl")
  loadNamespace("stringr")
  if (stringr::str_detect(path, "^https://") ||
      stringr::str_detect(path, "^http://") ||
      stringr::str_detect(path, "^ftp://")) {
    tmp <- download_data_file(path, "excel")
    readxl::excel_sheets(tmp)
  } else {
    # On Windows, if the path has multibyte chars, work around error from readxl::excel_sheets by copying the file to temp directory.
    if (Sys.info()[["sysname"]] == "Windows" && grepl("[^ -~]", path)) {
      new_path <- tempfile(fileext = stringr::str_c(".", tools::file_ext(path)))
      file.copy(path, new_path)
      ret <- readxl::excel_sheets(new_path)
      file.remove(new_path)
      ret
    }
    else {
      # If it's local file without multibyte path, simply call readxl::read_sheets.
      readxl::excel_sheets(path)
    }
  }
}

#'API that search and imports multiple same structure CSV files and merge it to a single data frame
#'@export
searchAndReadDelimFiles <- function(folder, pattern = "", forPreview = FALSE, delim, quote = '"',
                                        escape_backslash = FALSE, escape_double = TRUE,
                                        col_names = TRUE, col_types = readr::cols(.default = readr::col_character()),
                                        locale = readr::default_locale(),
                                        na = c("", "NA"), quoted_na = TRUE,
                                        comment = "", trim_ws = FALSE,
                                        skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                                        progress = interactive(), with_api_key = FALSE) {
  # search condition is case insensitive. (ref: https://www.regular-expressions.info/modifiers.html, https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r)
  if (!dir.exists(folder)) {
    stop(paste0('EXP-DATASRC-2 :: ', jsonlite::toJSON(folder), ' :: The folder does not exist.')) # TODO: escape folder name.
  }
  files <- list.files(path = folder, pattern = stringr::str_c("(?i)", pattern), full.names = T)
  if (length(files) == 0) {
    stop(paste0('EXP-DATASRC-3 :: ', jsonlite::toJSON(folder), ' :: There is no file in the folder that matches with the specified condition.')) # TODO: escape folder name.
  }
  exploratory::read_delim_files(files = files, forPreview = forPreview, delim = delim, quote = quote,
                                escape_backslash = escape_backslash, escape_double = escape_double,
                                col_names = col_names, col_types = col_types,
                                locale = locale,
                                na = na, quoted_na = quoted_na,
                                comment = comment, trim_ws = trim_ws,
                                skip = skip, n_max = n_max, guess_max = guess_max,
                                progress = progress, with_api_key = with_api_key)

}
#'API that imports multiple same structure CSV files and merge it to a single data frame
#'
#'For col_types parameter, by default it forces character to make sure that merging the CSV based data frames doesn't error out due to column data types mismatch.
# Once the data frames merging is done, readr::type_convert is called from Exploratory Desktop to restore the column data types.

#'@export
read_delim_files <- function(files, forPreview = FALSE, delim, quote = '"',
                              escape_backslash = FALSE, escape_double = TRUE,
                              col_names = TRUE, col_types = readr::cols(.default = readr::col_character()),
                              locale = readr::default_locale(),
                              na = c("", "NA"), quoted_na = TRUE,
                              comment = "", trim_ws = FALSE,
                              skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                              progress = interactive(), with_api_key = FALSE) {

    # for preview mode, just use the first file.
    if (forPreview & length(files) > 0) {
      files <- files[1]
    }

    # set name to the files so that it can be used for the "id" column created by purrr:map_dfr.
    files <- setNames(as.list(files), files)
    df <- purrr::map_dfr(files, exploratory::read_delim_file, delim = delim, quote = quote,
                   escape_backslash = escape_backslash, escape_double = escape_double,
                   col_names = col_names, col_types = col_types,
                   locale = locale,
                   na = na, quoted_na = quoted_na,
                   comment = comment, trim_ws = trim_ws,
                   skip = skip, n_max = n_max, guess_max = guess_max,
                   progress = progress, with_api_key = with_api_key, .id = "exp.file.id") %>% mutate(exp.file.id = basename(exp.file.id))  # extract file name from full path with basename and create file.id column.
    id_col <- avoid_conflict(colnames(df), "id")
    # copy internal exp.file.id to the id column.
    df[[id_col]] <- df[["exp.file.id"]]
    # drop internal column and move the id column to the very beginning.
    df %>% dplyr::select(!!rlang::sym(id_col), dplyr::everything(), -exp.file.id)

}

#'Wrapper for readr::read_delim to support remote file
#'@export
read_delim_file <- function(file, delim, quote = '"',
                            escape_backslash = FALSE, escape_double = TRUE,
                            col_names = TRUE, col_types = NULL,
                            locale = readr::default_locale(),
                            na = c("", "NA"), quoted_na = TRUE,
                            comment = "", trim_ws = FALSE,
                            skip = 0, n_max = Inf, guess_max = min(1000, n_max),
                            progress = interactive(), with_api_key = FALSE, data_text = NULL){
  loadNamespace("readr")
  loadNamespace("stringr")

  is_free_input_text = !is.null(data_text)
  # For remote file
  if (!is_free_input_text && (
      stringr::str_detect(file, "^https://") ||
      stringr::str_detect(file, "^http://") ||
      stringr::str_detect(file, "^ftp://"))) {
    if(with_api_key){
      token <- exploratory::getTokenInfo("exploratory-data-catalog")
      if(!is.null(token)) {
        # append access_token to the URL
        if(stringr::str_detect(file, "\\?") || stringr::str_detect(file, "\\&")) {
          file <- stringr::str_c(file, "&api_key=", token)
        } else {
          file <- stringr::str_c(file, "?api_key=", token)
        }
      }
    }
    tryCatch({
      tmp <- download_data_file(file, "csv")
    }, error = function(e) {
      stop(paste0('EXP-DATASRC-15 :: ', jsonlite::toJSON(c(file, e$message)), ' :: Failed to download from the URL.'))
    })
    tryCatch({ # try to close connection and ignore error
      readr::read_delim(tmp, delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, col_names = col_names, col_types = col_types,
                        locale = locale, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress)
    }, error = function(e) {
      # For the case it's running on Linux (Collaboration Server), show more user friendly message.
      # For Exploratory Desktop, it's already taken care of by Desktop so just show the error message as is.
      # When an incorrect encoding is used, "Error in make.names(x) : invalid multibyte string 1" error message is returned.
      if(Sys.info()["sysname"]=="Linux" && stringr::str_detect(stringr::str_to_lower(e$message), "invalid multibyte")) {
        if(locale$encoding == "Shift_JIS") {
          msg <- "The encoding of the file may be CP932 instead of Shift_JIS. Select CP932 as encoding and try again.";
          stop(paste0('EXP-DATASRC-13 :: ', jsonlite::toJSON(c(file, msg)), ' :: Failed to import file.'))
        } else if (locale$encoding == "CP932") {
          stop("The encoding of the file may be Shift_JIS instead of CP932. Select Shift_JIS as encoding and try again.");
        } else {
          stop(stringr::str_c("The encoding of the file may not be ", locale$encoding, ". Select other encoding and try again."));
        }
      } else if (stringr::str_detect(stringr::str_to_lower(e$message), "does not exist")) { #for the case Error: Error : '/tmp/RtmpVAk1Jf/filed3636522650.csv' does not exist.
        stop(stringr::str_c("Could not read data from ", file)); # Show the original URL name in the error message.
      } else {
        stop(paste0('EXP-DATASRC-13 :: ', jsonlite::toJSON(c(file, e$message)), ' :: Failed to import file.'))
      }
    })
  } else { # for local file or free input text cases.
    data <- ""
    if (is_free_input_text) { # for free input text
      # For Windows, make sure to convert text to UTF-8
      if (Sys.info()["sysname"] == "Windows") {
        data_text <- exploratory:::convertUserInputToUtf8(data_text)
        locale$encoding <- "UTF-8"
      }
      # call I() to data_text so that readr::read_delimit can hand the text data.
      data <- I(data_text)
    } else { # if it's local file simply call readr::read_delim
      # reading through file() is to be able to read files with path that includes multibyte chars.
      # without it, error is thrown from inside read_delim.
      if(stringi::stri_enc_mark(file) != "ASCII"){
        data <- file(file)
      } else {
        data <- file
      }
    }
    # for Free Text case (i.e. is_free_text = TRUE), file is actually a data so do not call file()
    tryCatch({ # try to close connection and ignore error
      readr::read_delim(data, delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, col_names = col_names, col_types = col_types,
                        locale = locale, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress)
    }, error = function(e) {
      # For the case it's running on Linux (Collaboration Server), show more user friendly message.
      # For Exploraotry Desktkop, it's already taken care of by Desktop so just show the error message as is.
      # When an incorrect encoding is used, "Error in make.names(x) : invalid multibyte string 1" error message is returned.
      if(stringr::str_detect(stringr::str_to_lower(e$message), "invalid multibyte")) {
        if(locale$encoding == "Shift_JIS") {
          msg <- "The encoding of the file may be CP932 instead of Shift_JIS. Select CP932 as encoding and try again.";
          stop(paste0('EXP-DATASRC-13 :: ', jsonlite::toJSON(c(file, msg)), ' :: Failed to import file.'))
        } else if (locale$encoding == "CP932") {
          msg <- "The encoding of the file may be Shift_JIS instead of CP932. Select Shift_JIS as encoding and try again.";
          stop(paste0('EXP-DATASRC-13 :: ', jsonlite::toJSON(c(file, msg)), ' :: Failed to import file.'))
        } else {
          msg <- stringr::str_c("The encoding of the file may not be ", locale$encoding, ". Select other encoding and try again.");
          stop(paste0('EXP-DATASRC-13 :: ', jsonlite::toJSON(c(file, msg)), ' :: Failed to import file.'))
        }
      } else if (stringr::str_detect(stringr::str_to_lower(e$message), "cannot open the connection")) {
        stop(paste0("EXP-DATASRC-1 :: ", jsonlite::toJSON(file), " ::  Failed to read file."))
      } else if (stringr::str_detect(stringr::str_to_lower(e$message), "does not exist")) {
        stop(paste0('EXP-DATASRC-14 :: ', jsonlite::toJSON(file), ' :: The file does not exist.'))
      } else {
        stop(paste0('EXP-DATASRC-13 :: ', jsonlite::toJSON(c(file, e$message)), ' :: Failed to import file.'))
      }
    })

  }
}

#'Wrapper for readr::guess_encoding to support remote file
#'@export
guess_csv_file_encoding <- function(file,  n_max = 1e4, threshold = 0.20){
  loadNamespace("readr")
  loadNamespace("stringr")
  if (stringr::str_detect(file, "^https://") ||
      stringr::str_detect(file, "^http://") ||
      stringr::str_detect(file, "^ftp://")) {
    tmp <- download_data_file(file, "csv")
    readr::guess_encoding(tmp, n_max, threshold)
  } else {
    # If it's local file simply call readr::read_delim.
    # Reading through read_lines_raw(file()) is to be able to read files with path that includes multibyte chars.
    # without it, error is thrown from inside guess_encoding.
    # Since file() call has about 1 sec overhead at first read (most likely read-ahead), we do this selectively
    # only when multibyte characters are in the path.
    if(stringi::stri_enc_mark(file) == "ASCII"){
      encode <- readr::guess_encoding(file, n_max=n_max, threshold=threshold)
    } else {
      encode <- readr::guess_encoding(readr::read_lines_raw(file(file), n_max=n_max), threshold=threshold)
    }
    encode
  }
}

#'Wrapper for readr::read_log to support remote file
#'@export
read_log_file <- function(file, col_names = FALSE, col_types = NULL,
                          skip = 0, n_max = Inf, progress = FALSE){
  loadNamespace("readr")
  loadNamespace("stringr")
  if (stringr::str_detect(file, "^https://") ||
      stringr::str_detect(file, "^http://") ||
      stringr::str_detect(file, "^ftp://")) {
    tmp <- download_data_file(file, "log")
    readr::read_log(tmp, col_names = col_names, col_types = col_types, skip = skip, n_max = n_max, progress = progress)
  } else {
    # if it's local file simply call readr::read_log
    readr::read_log(file, col_names = col_names, col_types = col_types, skip = skip, n_max = n_max, progress = progress)
  }
}

#'Wrapper for readRDS to support remote file
#'@export
read_rds_file <- function(file, refhook = NULL){
  loadNamespace("stringr")
  if (stringr::str_detect(file, "^https://") ||
      stringr::str_detect(file, "^http://") ||
      stringr::str_detect(file, "^ftp://")) {
    # for remote RDS, need to call url and gzcon before pass it to readRDS
    tryCatch({
      readRDS(gzcon(url(file)), refhook)
    }, error = function(e) {
      stop(paste0('EXP-DATASRC-13 :: ', jsonlite::toJSON(c(file, e$message)), ' :: Failed to import file.'))
    })
  } else {
    # if it's local file simply call read_rds
    tryCatch({
      readRDS(file, refhook)
    }, error = function(e) {
      if (stringr::str_detect(e$message, "cannot open the connection")) {
        # Assuming that this means the file is missing.
        # Strictly speaking, it might happen when the file is broken as a gzip file, but we can't distinguish between them from the error.
        stop(paste0('EXP-DATASRC-14 :: ', jsonlite::toJSON(file), ' :: The file does not exist.'))
      }
      else {
        stop(paste0('EXP-DATASRC-13 :: ', jsonlite::toJSON(c(file, e$message)), ' :: Failed to import file.'))
      }
    })
  }
}

#'API that search and imports multiple same structure parquet files and merge it to a single data frame
#'@export
searchAndReadParquetFiles <- function(folder, forPreview = FALSE, pattern, files, col_select = NULL){
  # search condition is case insensitive. (ref: https://www.regular-expressions.info/modifiers.html, https://stackoverflow.com/questions/5671719/case-insensitive-search-of-a-list-in-r)
  if (!dir.exists(folder)) {
    stop(paste0('EXP-DATASRC-2 :: ', jsonlite::toJSON(folder), ' :: The folder does not exist.')) # TODO: escape folder name.
  }
  files <- list.files(path = folder, pattern = stringr::str_c("(?i)", pattern), full.names = T)
  if (length(files) == 0) {
    stop(paste0('EXP-DATASRC-3 :: ', jsonlite::toJSON(folder), ' :: There is no file in the folder that matches with the specified condition.')) # TODO: escape folder name.
  }
  read_parquet_files(files, forPreview = forPreview, col_select = col_select)
}

#'API that imports multiple same structure parquet files and merge it to a single data frame
#'@export
read_parquet_files <- function(files, forPreview = FALSE, col_select = NULL) {
  # for preview mode, just use the first file.
  if (forPreview & length(files) > 0) {
    files <- files[1]
  }
  # set name to the files so that it can be used for the "id" column created by purrr:map_dfr.
  files <- setNames(as.list(files), files)
  df <- purrr::map_dfr(files, exploratory::read_parquet_file, col_select = col_select, .id = "exp.file.id") %>% mutate(exp.file.id = basename(exp.file.id))  # extract file name from full path with basename and create file.id column.
  id_col <- avoid_conflict(colnames(df), "id")
  # copy internal exp.file.id to the id column.
  df[[id_col]] <- df[["exp.file.id"]]
  # drop internal column and move the id column to the very beginning.
  df %>% dplyr::select(!!rlang::sym(id_col), dplyr::everything(), -exp.file.id)
}


#' Wrapper for read_parquet to support remote file.
#' @export
read_parquet_file <- function(file, col_select = NULL) {
  loadNamespace("arrow")
  tf <- NULL
  res <- NULL
  if (stringr::str_detect(file, "^https://") ||
      stringr::str_detect(file, "^http://") ||
      stringr::str_detect(file, "^ftp://")) {

    # Download the remote parquet file to the local temp file.
    tf <- tempfile()
    # Remove on exit.
    on.exit(unlink(tf))
    tryCatch({
      # mode="wb" for binary download
      utils::download.file(file, tf, mode = "wb")
    }, error = function(e) {
      stop(paste0('EXP-DATASRC-15 :: ', jsonlite::toJSON(c(file, e$message)), ' :: Failed to download from the URL.'))
    })
    # Read the local parquet file.
    tryCatch({
      if (is.null(col_select)) {
        res <- read_parquet_file_internal(tf)
      } else {
        res <- read_parquet_file_internal(tf, col_select = col_select)
      }
    }, error = function(e) {
      stop(paste0('EXP-DATASRC-13 :: ', jsonlite::toJSON(c(file, e$message)), ' :: Failed to import file.'))
    })
  } else {
    tryCatch({
      if (is.null(col_select)) {
        res <- read_parquet_file_internal(file)
      } else {
        res <- read_parquet_file_internal(file, col_select = col_select)
      }
    }, error = function(e) {
      # Error message for non-existent file case looks like this - "Error : IOError: Failed to open local file '<full filepath>'. Detail: [errno 2] No such file or directory"
      if (stringr::str_detect(e$message, "No such file or directory")) {
        stop(paste0('EXP-DATASRC-14 :: ', jsonlite::toJSON(file), ' :: The file does not exist.'))
      }
      else {
        stop(paste0('EXP-DATASRC-13 :: ', jsonlite::toJSON(c(file, e$message)), ' :: Failed to import file.'))
      }
    })
  }
  res
}

# Wrapper around arrow::read_parquet to work around https://issues.apache.org/jira/browse/ARROW-13860 by applying group_by column stored in the parquet file
# as one of the class names.
read_parquet_file_internal <- function(filepath, col_select = NULL) {
  res <- NULL
  if (is.null(col_select)) {
    res <- arrow::read_parquet(filepath)
  } else {
    res <- arrow::read_parquet(filepath, col_select = col_select)
  }
  if (stringr::str_starts(class(res)[1], '...grouped_by_')) {
    group_col <- stringr::str_remove(class(res)[1], '^\\.\\.\\.grouped_by_')
    res <- res %>% dplyr::group_by(!!rlang::sym(group_col)) # applying group_by seems to remove the ...grouped_by_... class.
  }
  res
}

#'Wrapper for readr::read_lines to support vector to data frame conversion
#'It seems readr::read_lines uses -1 for n_max to get all the data.
#'It does not align with the other readr functions that uses Inf for all the data but we have to follow existing read_lines behavior.
#'@export
read_raw_lines <- function(file, locale = readr::default_locale(), na = character(),
                            skip = 0, n_max = -1L, progress = FALSE){
  loadNamespace("readr")
  line <- readr::read_lines(file, locale = locale, na = na, skip = skip, n_max = n_max, progress = progress)
  # use line as column name
  df <- data.frame(line = line, stringsAsFactors = FALSE)
}

#'Wrapper for dplyr::filter to support successive calls instead of single filter
#'call with multiple conditions.
#
#'@export
filter_cascade <- function(.data, ...){
  # ref: https://github.com/tidyverse/dplyr/blob/5d23cb8d87111ead96a09deb43154610263e7854/R/filter.R#L119
  dots <- dplyr:::dplyr_quosures(...)
  dplyr:::check_filter(dots)
  df <- .data
  for(i in 1:length(dots)) {
    expr <- rlang::quo_get_expr(dots[[i]])
    df <- df %>% dplyr::filter(eval(expr))
  }
  df
}

#'API to load economic data from FRED (Federal Reserve Bank Economic Data)
#'@param series_id - e.g. UNRATE
#'@param date_start - Start Date for the query. This is optional field.
#'@param date_end - End Date for the query. By default it's today.
#'@export
load_fred <- function(series_id, date_start = "", date_end = "", password) {
  loadNamespace("fredr")
  fredr::fredr_set_key(password)
  # Desktop passes empty string if end date is not selected. For this case fallback to today.
  if (date_end == "") {
    date_end <- lubridate::today()
  } else {
    date_end <- lubridate::ymd(date_end)
  }
  # date_start is an optional parameter, so if it's not specified, execute the query without the start_date.
  if (date_start == "") {
    fredr::fredr(
      series_id = series_id,
      observation_end = date_end
    )
  } else {
    fredr::fredr(
      series_id = series_id,
      observation_start = lubridate::ymd(date_start),
      observation_end = date_end
    )
  }
}


# Names of functions that uses column specifications, but would never reference outside data frame, such as select.
# Collected from the doc of dplyr, tidyr, and our command menu.
select_and_friends <- c('arrange', 'select', 'rename', 'relocate', 'reorder_cols',
  'group_by', 'gather', 'spread', 'pivot_longer', 'pivot_wider', 'complete', 'expand', 'extract',
  'unnest', 'unnest_longer', 'unnest_wider', 'nest', 'hoist', 'nest_legacy', 'unnest_legacy', 'pack', 'unpack',
  'separate', 'unite', 'separate_rows', 'pivot',
  'top_n',
  'distinct',
  'get_dupes',
  'drop_na',
  'fill',
  'exp_balance',
  'one_hot',
  'do_cor',
  'do_dist',
  'do_cosine_sim.kv',
  'do_survfit',
  'do_market_impact',
  'do_cmdscale',
  'do_svd',
  'exp_ts_cluster',
  'do_anomaly_detection',
  'exp_bayes_ab',
  'evaluate_regression',
  'evaluate_binary',
  'evaluate_multi',
  'do_roc',
  'build_multinom',
  'do_t.test',
  'do_var.test',
  'do_chisq.test',
  'build_lm',
  'build_lr',
  'build_multinom',
  'build_glm',
  'build_model',
  'build_coxph',
  'do_tokenize',
  'do_ngram',
  'do_tf_idf',
  'pair_count')
# Names of functions that uses column specifications or reference with the column names, and could also reference outside data frames, such as mutate.
# Collected from the doc of dplyr, and our command menu.
mutate_and_friends <- c('mutate_group', 'mutate', 'mutate_at', 'mutate_all', 'mutate_if', 'transmute', 'summarize_group',
  'summarize', 'summarize_at', 'summarize_all', 'summarize_if', 'summarise', 'summarise_at', 'summarise_all', 'summarise_if', 'filter',
  'do_prophet') # do_prophet is here to handle reference to holiday data frame.

get_refs_in_call_args_after_pipe <- function(call_name_str,
                                  args,
                                  inside_mutate_and_friends = FALSE,
                                  inside_bang = FALSE, # Passes down the state of inside a single bang.
                                  inside_bang_bang = FALSE) { # Passes down the state of inside a consecutive bang bang.
  if (is.null(call_name_str)) { # :: operator as in bit64::is.integer64 gives NULL call name for some reason.
    # We assume it is a :: operator, and in this case there should not be a reference since the args are namespace and function name.
    res <- c()
  }
  else if (call_name_str %in% select_and_friends) {
    res <- c()
  }
  else {
    # State transitions on inside_mutate_and_friends, inside_bang, and inside_bang_bang.
    # It seems rlang::parse_expr does not recognize !! as one function, and rather recognize it as 2 separate bangs.
    # So we need a state machine to detect it.
    if (inside_mutate_and_friends) {
      if (call_name_str == '!') {
        if (!inside_bang) {
          inside_bang <- TRUE
        }
        else { # Already inside a bang.
          inside_bang_bang <- TRUE
        }
      }
      else { # This call is not !.
        if (inside_bang && !inside_bang_bang) { # Got a ! but inside it was not !. Reset the state.
          inside_bang <- FALSE
        }
      }
    }
    if (call_name_str %in% mutate_and_friends) {
      inside_mutate_and_friends <- TRUE
    }

    if (call_name_str == 'library') { # Ignore symbol inside library() since it is a library name.
      return(c())
    }

    if (call_name_str == '$') { # Ignore after $ since it should be a name inside the first arg.
      args <- args[1]
    }

    if (call_name_str %in% c('<-', '=')) { # Assignment should not count as a reference.
      args <- args[-1]
    }


    args <- purrr::discard(args, function(arg) { # Remove empty names that are formed by empty arg. e.g. func(a, ,b). It leads purr::reduce to throw error.
      rlang::is_symbol(arg) && as.character(arg) == ''
    })

    res <- purrr::reduce2(args, names(args), function(names, arg, arg_name) {
      if (rlang::is_symbol(arg)) {
        if (inside_mutate_and_friends &&
            !inside_bang_bang &&
            !call_name_str == '$' &&
            !(call_name_str == 'do_prophet' && arg_name == 'holidays')) {
          # If inside mutate and friends, skip the name since it would be reference to a column.
          # Exceptions are...
          # - when it is inside !! (bang bang), which means it is a reference to the outside environment.
          # - when this is a name before $, which makes it likely to be a data frame name.
          #   Since the exception for $ is prone to false positive, we might fade it out some time, but for now we
          #   do this to ameliorate the impact of the breaking change to strictly require !! for outside object reference.
          # - holidays arg of do_prophet, which is meant for holiday data frame. (While picking up the holiday table, we don't want to pick up time column, etc.)
          names
        }
        else {
          c(names, as.character(arg))
        }
      }
      else if (rlang::is_call(arg)) {
        c(names, get_refs_in_call(arg, inside_mutate_and_friends, inside_bang, inside_bang_bang,
                                  TRUE) # after_pipe
        )
      }
      else {
        names
      }
    }, .init = c())
  }
  res
}

get_refs_in_call_args_basic <- function(call_name_str, args) {
  if (call_name_str == 'library') { # Ignore symbol inside library() since it is a library name.
    return(c())
  }

  if (call_name_str == '$') { # Ignore after $ since it should be a name inside the first arg.
    args <- args[1]
  }

  if (call_name_str %in% c('<-', '=')) { # Assignment should not count as a reference.
    args <- args[-1]
  }

  args <- purrr::discard(args, function(arg) { # Remove empty names that are formed by empty arg. e.g. func(a, ,b). It leads purr::reduce to throw error.
    rlang::is_symbol(arg) && as.character(arg) == ''
  })

  res <- purrr::reduce2(args, names(args), function(names, arg, arg_name) {
    if (rlang::is_symbol(arg)) {
      c(names, as.character(arg))
    }
    else if (rlang::is_call(arg)) {
      c(names, get_refs_in_call(arg))
    }
    else {
      names
    }
  }, .init = c())
  res
}

# Returns names that references outside objects (most likely data frames) from the call.
get_refs_in_call <- function(call,
                             inside_mutate_and_friends = FALSE,
                             inside_bang = FALSE, # Passes down the state of inside a single bang.
                             inside_bang_bang = FALSE, # Passes down the state of inside a consecutive bang bang.
                             after_pipe = FALSE
                             ) {
  args <- rlang::call_args(call)
  call_name_str <- rlang::call_name(call)
  if (after_pipe) {
    res <- get_refs_in_call_args_after_pipe(call_name_str, args,
                                            inside_mutate_and_friends = inside_mutate_and_friends,
                                            inside_bang = inside_bang,
                                            inside_bang_bang = inside_bang_bang)
  }
  else if (is.null(call_name_str)) { # :: operator as in bit64::is.integer64 gives NULL call name for some reason.
    # We assume it is a :: operator, and in this case there should not be a reference since the args are namespace and function name.
    res <- c()
  }
  else if (call_name_str == '%>%') {
    res1 <- get_refs_in_call_args_basic(call_name_str, args[1])
    res2 <- get_refs_in_call_args_after_pipe(call_name_str, args[-1])
    res <- c(res1, res2)
  }
  else if (call_name_str %in% c(select_and_friends, mutate_and_friends)) {
    res1 <- get_refs_in_call_args_basic(call_name_str, args[1])
    res2 <- get_refs_in_call_args_after_pipe(call_name_str, args[-1])
    res <- c(res1, res2)
  }
  else {
    res <- get_refs_in_call_args_basic(call_name_str, args)
  }
  res
}

# Returns names that references outside objects (most likely data frames) from the script.
# priv_step_df - The data frame of the previous step. Refs to the columns of it are not considered outside refs.
get_refs_in_script <- function(script, after_pipe = TRUE) {
  exprs <- NULL
  tryCatch({
    exprs <- rlang::parse_exprs(script)
  }, error = function(e) { # Ignore parse error and return NULL.
  })
  if (is.null(exprs)) {
    NULL
  }
  else {
    res <- purrr::reduce(exprs, function(names, expr) {
      if (rlang::is_call(expr)) {
        c(names, get_refs_in_call(expr, after_pipe = after_pipe))
      }
      else if (rlang::is_symbol(expr)) {
        c(names, as.character(expr))
      }
    }, .init = c())
    res
  }
}

# Function to split numbers into groups by equal data range.
# Mainly for chart bucketing function.
#
# Enhanced and customized version of cut function
# include.lowest=TRUE by default to include the lowest value with
# the custom breaks based on min/max value in order to avoid
# having a smaller value than the min value in the actual value.
exp_cut <- function(x, breaks=5, labels=NULL, dig.lab=3, zero.to.center=FALSE, include.lowest=TRUE, right=TRUE, lower.range=NA, upper.range=NA, include.outside.range=TRUE) {
  # Return as is if x is empty.
  if (length(x) == 0) {
    return (x)
  }
  # If it is not numeric, return a vector of NAs. Since pivot table now
  # accepts non-numeric for measure values, we need this handling.
  if (!is.numeric(x)) {
    return (rep(NA, length(x)))
  }

  tryCatch({
    #
    # If you run the cut function against the 1 length numeric vector which value is '0',
    # then cut complains like this.
    #
    # > cut(c(0), breaks=5, label=F)
    # Error in cut.default(c(0), breaks = 5, label = F) :
    #  'breaks' are not unique
    #
    # Actually, it happens even with length more than 1 if all of vector values are 0 (#4965)
    #
    # > cut(c(0, 0, 0), breaks=5, label=F)
    # Error in cut.default(c(0), breaks = 5, label = F) :
    #  'breaks' are not unique
    #
    #
    # If na.rm=FALSE, it return NA if it includes NA and if statement complains.
    if (all(x==0, na.rm=TRUE)) {
      if (is.null(labels)) {
        # mimics the default output like '(0,19835.25]'
        # TODO: handle length>1 case later
        v <- as.factor(c('(0,0]'))
      } else {
        # In case of labels=FALSE case.
        # It handles NA as NA, and zero as center value of the given breaks.
        #
        # Expected output:
        # > `_tam_cut`(c(0,NA,0,NA), breaks=5, label=F)
        # [1]  3 NA  3 NA
        # > `_tam_cut`(c(0,0), breaks=5, label=F)
        # [1] 3 3
        # > `_tam_cut`(c(NA,NA), breaks=5, label=F)
        # [1] NA NA
        v <- ifelse(is.na(x), NA, as.integer(ceiling(breaks/2)))
      }
      return(v)
    } else {
      minv <- min(x, na.rm=TRUE)
      maxv <- max(x, na.rm=TRUE)
      # If min and max values of given vector are the same, then seq function
      # will generate a vector with the same values. That vector cannot be
      # used for cut custom breaks since the cut function complains if
      # given vector values are not unique.
      if (minv == maxv) {
        # If that's the case, just call cut function with giving number of
        # breaks param as it used to work before.
        breaks.with.inf <- breaks
        # Extreme case, in case all values are all in either -Inf or Inf.
        if(is.infinite(minv)) {
          breaks.with.inf <- c(-Inf, Inf)
        }
        return (cut(x, breaks=breaks.with.inf, labels=labels, dig.lab=dig.lab, include.lowest=include.lowest, right=right))
      } else {
        # cut function doesn't work with infinite values without explicitly
        # specifying the infinite values in the breaks.
        #
        # Examples:
        # Sample data = c(1,2,3,4,5,6,7,8,9,10,Inf)
        #
        # If zero.to.center=FALSE:
        # - create a value set without Inf values: c(1,2,3,4,5,6,7,8,9,10)
        # - create breaks without Inf values: c(1,4,7,10)
        # - append -Inf/Inf on the end like : c(1,4,7.10,Inf)
        #
        # - If both ends are infinite, c(-Inf,1,2,3,4,5,6,7,8,9,10,Inf)
        # - create breaks without Inf values: c(1,5,10)
        # - append Inf on both ends: c(-Inf,1,5.10,Inf)
        #
        #
        # If zero.to.center=TRUE:
        # Sample data = c(1,2,3,4,5,6,7,8,9,10,Inf)
        # - create a value set without Inf values: c(1,2,3,4,5,6,7,8,9,10)
        # - get the max of abs value, 10.
        # - create a bucket between -max and max values: c(-10,0,10)
        # - append -Inf/Inf on both ends: c(-Inf,-10,0.10,Inf)
        #
        if (is.infinite(minv) || is.infinite(maxv)) {

          lenout <- breaks
          x.no.inf <- x[!is.infinite(x)]
          # If there are only infinite values, add some dummy values.
          # It is ok since all values are either -Inf or Inf so those won't
          # fit in this range anyway.
          if (length(x.no.inf) == 0) {
            x.no.inf=c(-10, 10)
          }

          minv.no.inf <- min(x.no.inf, na.rm=TRUE)
          maxv.no.inf <- max(x.no.inf, na.rm=TRUE)
          if (zero.to.center) {
            # Reduce the bucket size to add infinite values on both ends.
            lenout <- lenout - 1
            maxv.no.inf <- max(abs(maxv.no.inf), abs(minv.no.inf))
            minv.no.inf <- -maxv.no.inf
          } else {
            # If there's only 1 value available after taking off infinites, we add
            # another value to have 2 different values for cut. This is because
            # if you pass the same value for min and max for seq, it returns a vector with
            # the same values like c(1,1,1,1,1). The cut function will complain about this
            # because breaks are not uniq.
            if (minv.no.inf == maxv.no.inf) {
              maxv.no.inf <- abs(maxv.no.inf)
              minv.no.inf <- -maxv.no.inf
            }
            if  (is.infinite(minv) && is.infinite(maxv)) {
              # Reduce the bucket size to add infinite values on both ends.
              lenout <- lenout - 1
            }
          }

          # Set custom range if specified.
          if (!is.na(upper.range)) {
            maxv.no.inf <- upper.range
          }
          if (!is.na(lower.range)) {
            minv.no.inf <- lower.range
          }

          breaks.with.inf <- seq(minv.no.inf, maxv.no.inf,  length.out=lenout)
          if (zero.to.center) {
            breaks.with.inf <- c( -Inf, breaks.with.inf, Inf)
          } else {
            if (is.infinite(minv)) {
              breaks.with.inf <- c( -Inf, breaks.with.inf)
            }
            if (is.infinite(maxv)) {
              breaks.with.inf <- c(breaks.with.inf, Inf)
            }
          }

          # If include.outside.range is TRUE and range value is specified,
          # add the -Inf and Inf in the breaks.
          if (include.outside.range) {
            if (!is.infinite(minv) && !is.na(lower.range)) {
              breaks.with.inf <- c(-Inf, breaks.with.inf)
            }
            if (!is.infinite(maxv) && !is.na(upper.range)) {
              breaks.with.inf <- c(breaks.with.inf, Inf)
            }
          }

          return (cut(x, breaks=breaks.with.inf, labels=labels, dig.lab=dig.lab, include.lowest=include.lowest, right=right))
        } else {
          if (zero.to.center) {
            maxv <- max(abs(maxv), abs(minv))
            minv <- -maxv
          }

          # Set custom range if specified.
          if (!is.na(upper.range)) {
            maxv <- upper.range
          }
          if (!is.na(lower.range)) {
            minv <- lower.range
          }

          break_points = seq(minv, maxv, length.out = breaks+1)
          if (length(unique(break_points)) != breaks+1) {
            # this happens when minv and maxv is very close and there is not enough expressible floating point values between them.
            # using such break_points leads to an 'breaks are not unique' error. avoid it by spreading the range for break points a little.
            base = max(abs(minv),abs(maxv))
            break_points = seq(minv-(base*0.000000001), maxv+(base*0.000000001), length.out = breaks+1)
          }

          # If include.outside.range option is specified, add the -Inf and
          # Inf in the breaks. If it is already there, do nothing.
          if (include.outside.range) {
            if (!is.infinite(minv) && !is.na(lower.range)) {
              break_points <- c( -Inf, break_points)
            }
            if (!is.infinite(maxv) && !is.na(upper.range)) {
              break_points <- c(break_points, Inf)
            }
          }

          return (cut(x, breaks=break_points, labels=labels, dig.lab=dig.lab, include.lowest=include.lowest, right=right))
        }
      }
    }
  }, error = function(e) {
    # Add a tag at the end of the error message to tell where it comes from.
    # We will eventually remove this workaround once we implement the chart
    # data query step-by-step execution. #13715
    stop(paste0(e$message, " (Binning)"))
  })
}

# Cut number by the step specified.
# @param x
# @param step
# @param lower.range Min value to start the bucketing.
#                    If you don't specify, min(x) will be used.
# @param upper.range Max value to end the bucketing.
#                    If you don't specify, max(x) will be used.
# @param include.outside.range If you set it to TRUE, it will create buckets
#                              for outside of the upper and lower ranges.
exp_cut_by_step <- function(x, step=NA, lower.range=NA, upper.range=NA, include.outside.range=TRUE, right=TRUE, ...) {
  # If it is not numeric, return a vector of NAs. Since pivot table now
  # accepts non-numeric for measure values, we need this handling.
  if (!is.numeric(x)) {
    return (rep(NA, length(x)))
  }

  # Remove NA, NaN, Inf, -Inf.
  x.finite <- x[is.finite(x)]

  # In case there are only NA, NaN, Inf or -Inf values in the vector.
  if (length(x.finite) == 0) {
    # The following cut command works like the following.
    # > x
    # [1] -Inf  Inf   NA  NaN
    # > cut(x, breaks=c(-Inf, Inf), include.lowest=T)
    # [1] [-Inf, Inf] [-Inf, Inf] <NA>        <NA>
    # Levels: [-Inf, Inf]
    return (cut(x, breaks=c(-Inf, Inf), include.lowest=T))
  }

  lower <- lower.range
  upper <- upper.range

  # Use the min value for the lower range if not specified.
  if (is.na(lower.range)) {
    # Remove inf, NA, NaN etc from the range.
    lower <- min(x.finite)
  }
  # Use the max value for the upper range if not specified.
  if (is.na(upper.range)) {
    # Remove inf, NA, NaN etc from the range.
    upper <- max(x.finite)
  }
  # If step is not specified, set the default step which divides
  # the group into 5.
  if (is.na(step)) {
    # If all values are the same, set 1 to avoid the crash.
    if (upper == lower) {
      step <- 1
    } else {
      step <- ceiling(abs(upper - lower) / 5)
    }
  }

  # Create breaks by specifying cut points.
  breaks <- seq(lower, upper, by=step)

  # min/max values without NA. Those can include Inf/-Inf.
  min.x <- min(x, na.rm=TRUE)
  max.x <- max(x, na.rm=TRUE)

  # If the max.breaks doesn't include the upper range value,
  # add one more bucket. For example, if you set step=10 for 0:15,
  # we should create 2 buckets, 0-10, 10-20.
  if (max(breaks) < upper) {
    breaks <- c(breaks, max(breaks) + step)
  }

  # Include -Inf/Inf if include.outside.range is TRUE.
  if (include.outside.range) {
    # Add lower outside range if;
    # - lower.range is explicitly specified and lower.range is
    #   larger than the min value OR
    # - lower.range is not specified and right=TRUE. In this case,
    #   the min value won't be included in the buckets so we need
    #   to add the lower outside range for the min value #25625.
    #   If all values are the same, we should treat it as a special
    #   case so we skip it in that case.
    if (!is.na(lower.range)) {
      if (right == TRUE && min.x <= min(breaks)) {
        breaks <- c(-Inf, breaks)
      } else if (right == FALSE && min.x < min(breaks)) {
        breaks <- c(-Inf, breaks)
      }
    }
    # lower.range is NULL.
    else if (right == TRUE && upper != lower) {
      breaks <- c(-Inf, breaks)
    }
    # Add upper outside range if;
    # - upper.range is explicitly specified and upper range is
    #   smaller than the max value OR
    # - upper.range is not specified and right=FALSE. In this case,
    #   the max value won't be included in the buckets so we need
    #   to add the upper outside range for the max value #25625.
    #   If all values are the same, we should treat it as a special
    #   case so we skip it in that case.
    if (!is.na(upper.range)) {
      if (right == TRUE && max.x > max(breaks)) {
        breaks <- c(breaks, Inf)
      } else if (right == FALSE && max.x >= max(breaks)) {
        breaks <- c(breaks, Inf)
      }
    }
    # upper.range is NULL.
    else if (right == FALSE && upper != lower) {
      breaks <- c(breaks, Inf)
    }
  }

  # If there are -Inf values, include -Inf in the break.
  # If it is already there, just ignore it.
  if (min.x == -Inf && min(breaks) != -Inf) {
    breaks <- c(-Inf, breaks)
  }
  # If there are Inf values, include Inf in the break.
  # If it is already there, just ignore it.
  if (max.x == Inf && max(breaks) != Inf) {
    breaks <- c(breaks, Inf)
  }

  # In that case, pass breaks=2 to let cut command to break it into 2.
  if (length(breaks) == 1) {
    breaks <- 2
  }

  # Call cut with the constructed breaks.
  cut(x, breaks=breaks, right=right, ...)
}

