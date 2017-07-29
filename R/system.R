# environment to keep variables for users
user_env <- new.env()
# environment to keep values to create connection
user_env$token_info <- new.env()

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


getMongoURL <- function(host, port, database, username, pass, isSSL=FALSE, authSource=NULL) {
  loadNamespace("stringr")
  loadNamespace("urltools")

  if (stringr::str_length(username) > 0) {
    if(!is.null(pass) && pass != ''){
      # mongodb connection URL uses @ as a separator so need to encode password for those special characters.
      pass = urltools::url_encode(pass)
    }
    url = stringr::str_c("mongodb://", username, ":", pass, "@", host, ":", as.character(port), "/", database)
  }
  else {
    url = stringr::str_c("mongodb://", host, ":", as.character(port), "/", database)
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
  return (url)
}


#' @export
queryMongoDB <- function(host, port, database, collection, username, password, query = "{}", isFlatten, limit=0, isSSL=FALSE, authSource=NULL, fields="{}", sort="{}", skip=0, queryType = "find", pipeline="{}"){
  if(!requireNamespace("mongolite")){stop("package mongolite must be installed.")}
  loadNamespace("jsonlite")
  if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}

  # read stored password
  pass = saveOrReadPassword("mongodb", username, password)
  # get connection from connection pool
  con <- getDBConnection("mongodb", host, port, database, username, pass, collection = collection, isSSL = isSSL, authSource = authSource)
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
      # set envir = parent.frame() to get variables from users environment, not papckage environment
      data <- con$aggregate(pipeline = GetoptLong::qq(pipeline, envir = parent.frame()))
    } else if (queryType == "find") {
      query <- convertUserInputToUtf8(query)
      fields <- convertUserInputToUtf8(fields)
      sort <- convertUserInputToUtf8(sort)
      # set envir = parent.frame() to get variables from users environment, not papckage environment
      data <- con$find(query = GetoptLong::qq(query, envir = parent.frame()), limit=limit, fields=fields, sort = sort, skip = skip)
    }
  }, error = function(err) {
    clearDBConnection("mongodb", host, port, database, username, collection = collection, isSSL = isSSL, authSource = authSource)
    stop(err)
  })
  result <-data
  if (isFlatten) {
    result <- jsonlite::flatten(data)
  }
  if (nrow(result)==0) {
    # possibly this is an error. clear connection once.
    clearDBConnection("mongodb", host, port, database, username, collection = collection, isSSL = isSSL, authSource = authSource)
    stop("No Data Found");
  } else {
    result
  }
}

#' Returns a data frame that has names of the collections in its "name" column.
#' @export
getMongoCollectionNames <- function(host, port, database, username, password, isSSL=FALSE, authSource=NULL){
  collection = "test" # dummy collection name. mongo command seems to work even if the collection does not exist.
  loadNamespace("jsonlite")
  if(!requireNamespace("mongolite")){stop("package mongolite must be installed.")}
  pass = saveOrReadPassword("mongodb", username, password)
  con <- getDBConnection("mongodb", host, port, database, username, pass, collection = collection, isSSL = isSSL, authSource = authSource)
  # command to list collections.
  # con$command is our addition in our mongolite fork.
  result <- con$command(command = '{"listCollections":1}')
  # need to check existence of ok column of result dataframe first to avoid error in error check.
  if (!("ok" %in% colnames(result)) || !result$ok) {
    clearDBConnection("mongodb", host, port, database, username, collection = collection, isSSL = isSSL, authSource = authSource)
    stop("listCollections command failed");
  }
  # TODO: does "firstBatch" mean it is possible there are more?
  # if so, where does it appear in the result?
  return(as.data.frame(result$cursor$firstBatch))
}

#' Returns the total number of rows stored in the target table.
#' At this moment only mongdb is supported.
#' @export
getMongoCollectionNumberOfRows <- function(host, port, database, username, password, collection, isSSL=FALSE, authSource=NULL){
  loadNamespace("jsonlite")
  if(!requireNamespace("mongolite")){stop("package mongolite must be installed.")}
  pass = saveOrReadPassword("mongodb", username, password)
  con <- getDBConnection("mongodb", host, port, database, username, pass, collection = collection, isSSL = isSSL, authSource = authSource)
  tryCatch({
    result <- con$count()
  }, error = function(err) {
    clearDBConnection("mongodb", host, port, database, username, collection = collection, isSSL = isSSL, authSource = authSource)
    stop(err)
  })
  return(result)
}


#' Returns specified connection from pool if it exists in the pool.
#' If not, new connection is created and returned.
#' @export
getDBConnection <- function(type, host, port, databaseName, username, password, catalog = "", schema = "", dsn="", additionalParams = "",
                            collection = "", isSSL = FALSE, authSource = NULL) {

  drv = NULL
  conn = NULL
  if(type == "mongodb") {
    if(!requireNamespace("mongolite")){stop("package mongolite must be installed.")}
    loadNamespace("jsonlite")
    if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}
    key <- paste("mongodb", host, port, databaseName, collection, username, toString(isSSL), authSource, sep = ":")
    conn <- connection_pool[[key]]
    if (!is.null(conn)){
      # command to ping to check connection validity.
      # con$command is our addition in our mongolite fork.
      result <- conn$command(command = '{"ping":1}')
      # need to check existence of ok column of result dataframe first to avoid error in error check.
      if (!("ok" %in% colnames(result)) || !result$ok) {
        rm(conn) # this disconnects connection
        conn <- NULL
        # fall through to getting new connection.
      }
    }
    if (is.null(conn)) {
      url = getMongoURL(host, port, databaseName, username, password, isSSL, authSource)
      conn <- mongolite::mongo(collection, url = url)
      connection_pool[[key]] <- conn
    }
  } else if(type == "mysql" || type == "aurora") {
    if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
    if(!requireNamespace("RMySQL")){stop("package RMySQL must be installed.")}
    # use same key "mysql" for aurora too, since it uses
    # queryMySQL() too, which uses the key "mysql"
    key <- paste("mysql", host, port, databaseName, username, sep = ":")
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
    if (is.null(conn)) {
      drv <- DBI::dbDriver("MySQL")
      conn = RMySQL::dbConnect(drv, dbname = databaseName, username = username,
                               password = password, host = host, port = port)
      connection_pool[[key]] <- conn
    }
  } else if (type == "postgres" || type == "redshift" || type == "vertica") {
    if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
    if(!requireNamespace("RPostgreSQL")){stop("package RPostgreSQL must be installed.")}
    # use same key "postgres" for redshift and vertica too, since they use
    # queryPostgres() too, which uses the key "postgres"
    key <- paste("postgres", host, port, databaseName, username, sep = ":")
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
    if (is.null(conn)) {
      drv <- DBI::dbDriver("PostgreSQL")
      pg_dsn = paste0(
        'dbname=', databaseName, ' ',
        'sslmode=prefer'
      )
      conn <- RPostgreSQL::dbConnect(drv, dbname=pg_dsn, user = username,
                                     password = password, host = host, port = port)
      connection_pool[[key]] <- conn
    }
  } else if (type == "presto") {
    if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
    if(!requireNamespace("RPresto")){stop("package Presto must be installed.")}
    loadNamespace("RPresto")
    drv <- RPresto::Presto()
    conn <- RPresto::dbConnect(drv, user = username, password = password, host = host, port = port, schema = schema, catalog = catalog, session.timezone = Sys.timezone(location = TRUE))
  } else if (type == "odbc") {
    # do those package loading only when we need to use odbc in this if statement,
    # so that we will not have error at our server environemnt where RODBC is not there.
    if(!requireNamespace("RODBC")){stop("package RODBC must be installed.")}
    if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}

    loadNamespace("RODBC")
    connect <- function() {
      connstr <- stringr::str_c("RODBC::odbcConnect(dsn = '",dsn, "',uid = '", username, "', pwd = '", password, "'")
      if(additionalParams == ""){
        connstr <- stringr::str_c(connstr, ")")
      } else {
        connstr <- stringr::str_c(connstr, ",", additionalParams, ")")
      }
      conn <- eval(parse(text=connstr))
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
      key <- paste("odbc", dsn, username, additionalParams, sep = ":")
      conn <- connection_pool[[key]]
    }
    if (is.null(conn)) {
      conn <- connect()
      if (user_env$pool_connection) { # pool connection if connection pooling is on.
        connection_pool[[key]] <- conn
      }
    }
  }
  conn
}

#' Clears specified connection from the pool.
#' When there is an error from a connection, we should call this so that next call to getDBConnection
#' would return a newly created connection.
#' @export
clearDBConnection <- function(type, host, port, databaseName, username, catalog = "", schema = "", dsn="", additionalParams = "",
                              collection = "", isSSL = FALSE, authSource = NULL) {
  if (type %in% c("odbc", "postgres", "redshift", "vertica", "mysql", "aurora")) { #TODO: implement for other types too
    if (type %in% c("mongodb")) {
      key <- paste("mongodb", host, port, databaseName, collection, username, toString(isSSL), authSource, sep = ":")
      conn <- connection_pool[[key]]
      if (!is.null(conn)) {
        rm(conn)
      }
    }
    else if (type %in% c("postgres", "redshift", "vertica")) {
      # they use common key "postgres"
      key <- paste("postgres", host, port, databaseName, username, sep = ":")
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
      key <- paste("mysql", host, port, databaseName, username, sep = ":")
      conn <- connection_pool[[key]]
      if (!is.null(conn)) {
        tryCatch({ # try to close connection and ignore error
          DBI::dbDisconnect(conn)
        }, warning = function(w) {
        }, error = function(e) {
        })
      }
    }
    else { # odbc
      key <- paste("odbc", dsn, username, additionalParams, sep = ":")
      conn <- connection_pool[[key]]
      if (!is.null(conn)) {
        tryCatch({ # try to close connection and ignore error
          RODBC::odbcClose(conn)
        }, warning = function(w) {
        }, error = function(e) {
        })
      }
    }
    rm(list = key, envir = connection_pool)
  }
}

#' @export
getListOfTables <- function(type, host, port, databaseName = NULL, username, password, catalog = "", schema = ""){
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
  if (type == "presto") {
    loadNamespace("RPresto")
    drv <- RPresto::Presto()
    conn <- RPresto::dbConnect(drv, schema = schema, catalog = catalog, user = username, host = host, port = port)
  } else {
    conn <- getDBConnection(type, host, port, databaseName, username, password)
  }
  tryCatch({
    tables <- DBI::dbListTables(conn)
  }, error = function(err) {
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection(type, host, port, databaseName, username, catalog = catalog, schema = schema)
    if (!type %in% c("odbc", "postgres", "redshift", "vertica", "mysql", "aurora")) { # only if conn pool is not used yet
      tryCatch({ # try to close connection and ignore error
        DBI::dbDisconnect(conn)
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
    stop(err)
  })
  if (!type %in% c("odbc", "postgres", "redshift", "vertica", "mysql", "aurora")) { # only if conn pool is not used yet
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
    if (!type %in% c("odbc", "postgres", "redshift", "vertica", "mysql", "aurora")) { # only if conn pool is not used yet
      tryCatch({ # try to close connection and ignore error
        DBI::dbDisconnect(conn)
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
    stop(err)
  })
  if (!type %in% c("odbc", "postgres", "redshift", "vertica", "mysql", "aurora")) { # only if conn pool is not used yet
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
executeGenericQuery <- function(type, host, port, databaseName, username, password, query, catalog = "", schema = ""){
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
  conn <- getDBConnection(type, host, port, databaseName, username, password, catalog = catalog, schema = schema)
  tryCatch({
    query <- convertUserInputToUtf8(query)
    # set envir = parent.frame() to get variables from users environment, not papckage environment
    resultSet <- DBI::dbSendQuery(conn, GetoptLong::qq(query, envir = parent.frame()))
    df <- DBI::dbFetch(resultSet)
  }, error = function(err) {
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection(type, host, port, databaseName, username, catalog = catalog, schema = schema)
    if (!type %in% c("odbc", "postgres", "redshift", "vertica", "mysql", "aurora")) { # only if conn pool is not used yet
      tryCatch({ # try to close connection and ignore error
        DBI::dbDisconnect(conn)
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
    stop(err)
  })
  DBI::dbClearResult(resultSet)
  if (!type %in% c("odbc", "postgres", "redshift", "vertica", "mysql", "aurora")) { # only if conn pool is not used yet
    tryCatch({ # try to close connection and ignore error
      DBI::dbDisconnect(conn)
    }, warning = function(w) {
    }, error = function(e) {
    })
  }
  df
}

#' @export
queryNeo4j <- function(host, port,  username, password, query, isSSL = FALSE){
  if(!requireNamespace("RNeo4j")){stop("package RNeo4j must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  url <- ifelse(isSSL == TRUE,  "https://" , "http://");
  url <- stringr::str_c(url, host,":",  port,  "/db/data");

  graph <- NULL
  if(!is.null(username) && !is.null(password)){
    # read stored password
    pass = saveOrReadPassword("neo4j", username, password)
    graph = RNeo4j::startGraph(url, username = username, password = pass)
  } else {
    graph = RNeo4j::startGraph(url)
  }
  query <- convertUserInputToUtf8(query)
  df <- RNeo4j::cypher(graph, query)
  df
}


#' @export
queryMySQL <- function(host, port, databaseName, username, password, numOfRows = -1, query){
  if(!requireNamespace("RMySQL")){stop("package RMySQL must be installed.")}
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
  if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}

  # read stored password
  pass = saveOrReadPassword("mysql", username, password)

  conn <- getDBConnection("mysql", host, port, databaseName, username, pass)
  tryCatch({
    DBI::dbGetQuery(conn,"set names utf8")
    query <- convertUserInputToUtf8(query)
    # set envir = parent.frame() to get variables from users environment, not papckage environment
    resultSet <- RMySQL::dbSendQuery(conn, GetoptLong::qq(query, envir = parent.frame()))
    df <- RMySQL::dbFetch(resultSet, n = numOfRows)
  }, error = function(err) {
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection("mysql", host, port, databaseName, username)
    stop(err)
  })
  RMySQL::dbClearResult(resultSet)
  df
}

#' @export
queryPostgres <- function(host, port, databaseName, username, password, numOfRows = -1, query){
  if(!requireNamespace("RPostgreSQL")){stop("package RPostgreSQL must be installed.")}
  if(!requireNamespace("DBI")){stop("package DBI must be installed.")}
  if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}

  # read stored password
  pass = saveOrReadPassword("postgres", username, password)
  conn <- getDBConnection("postgres", host, port, databaseName, username, pass)

  tryCatch({
    query <- convertUserInputToUtf8(query)
    # set envir = parent.frame() to get variables from users environment, not papckage environment
    resultSet <- RPostgreSQL::dbSendQuery(conn, GetoptLong::qq(query, envir = parent.frame()))
    df <- DBI::dbFetch(resultSet, n = numOfRows)
  }, error = function(err) {
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection("postgres", host, port, databaseName, username)
    stop(err)
  })
  RPostgreSQL::dbClearResult(resultSet)
  df
}

#' @export
queryODBC <- function(dsn,username, password, additionalParams, numOfRows = 0, query){
  if(!requireNamespace("RODBC")){stop("package RODBC must be installed.")}
  if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}

  conn <- getDBConnection("odbc", NULL, NULL, NULL, username, password, dsn = dsn, additionalParams = additionalParams)
  tryCatch({
    query <- convertUserInputToUtf8(query)
    # set envir = parent.frame() to get variables from users environment, not papckage environment
    df <- RODBC::sqlQuery(conn, GetoptLong::qq(query, envir = parent.frame()), max = numOfRows)
    if (!is.data.frame(df)) {
      # when it is error, RODBC::sqlQuery() does not stop() (throw) with error most of the cases.
      # in such cases, df is a character vecter rather than a data.frame.
      clearDBConnection("odbc", NULL, NULL, NULL, username, dsn = dsn, additionalParams = additionalParams)
      stop(paste(df, collapse = "\n"))
    }
    if (!user_env$pool_connection) {
      # close connection if not pooling.
      tryCatch({ # try to close connection and ignore error
        RODBC::odbcClose(conn)
      }, warning = function(w) {
      }, error = function(e) {
      })
    }
  }, error = function(err) {
    # for some cases like conn not being an open connection, sqlQuery still throws error. handle it here.
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection("odbc", NULL, NULL, NULL, username, dsn = dsn, additionalParams = additionalParams)
    stop(err)
  })
  df
}


#' Access twitter serch api
#' @param n - Maximum number of tweets.
#' @param lang - Language to filter result.
#' @param lastNDays - From how many days ago tweets should be searched.
#' @param searchString - Query to search.
#' @param tokenFileId - File id for aut
#' @param withSentiment - Whether there should be sentiment column caluculated by get_sentiment.
#' @export
getTwitter <- function(n=200, lang=NULL,  lastNDays=30, searchString, tokenFileId=NULL, withSentiment = FALSE){
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

  # convert search string to UTF-8 before sending it on the wire on windows.
  searchString <- convertUserInputToUtf8(searchString)
  tweetList <- twitteR::searchTwitter(searchString, n, lang, since, until, locale, geocode, sinceID, maxID, resultType, retryOnRateLimit)
  # conver list to data frame
  if(length(tweetList)>0){
    ret <- twitteR::twListToDF(tweetList)
    if(withSentiment){
      # calculate sentiment
      ret %>% dplyr::mutate(sentiment = get_sentiment(text))
    } else {
      ret
    }
  } else {
    stop('No Tweets found.')
  }
}



#' API to submit a Google Big Query Job
#' @export
submitGoogleBigQueryJob <- function(project, sqlquery, destination_table, write_disposition = "WRITE_TRUNCATE", tokenFieldId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  #GetoptLong uses stringr and str_c is called without stringr:: so need to use "require" instead of "requireNamespace"
  if(!require("stringr")){stop("package stringr must be installed.")}

  token <- getGoogleTokenForBigQuery(tokenFieldId)
  bigrquery::set_access_cred(token)
  sqlquery <- convertUserInputToUtf8(sqlquery)
  # pass desitiona_table to support large data
  # check if the query contains special key word for standardSQL
  # If we do not pass the useLegaySql argument, bigrquery set TRUE for it, so we need to expliclity set it to make standard SQL work.
  isStandardSQL <- stringr::str_detect(sqlquery, "#standardSQL")
  # set envir = parent.frame() to get variables from users environment, not papckage environment
  job <- bigrquery::insert_query_job(GetoptLong::qq(sqlquery, envir = parent.frame()), project, destination_table = destination_table, write_disposition = write_disposition, useLegacySql = isStandardSQL == FALSE)
  job <- bigrquery::wait_for(job)
  isCacheHit <- job$statistics$query$cacheHit
  # if cache hit case, totalBytesProcessed info is not available. So set it as -1
  totalBytesProcessed <- ifelse(isCacheHit, -1, job$statistics$totalBytesProcessed)
  # if cache hit case, recordsWritten info is not avalable. So set it as -1
  numOfRowsProcessed <- ifelse(isCacheHit, -1, job$statistics$query$queryPlan[[1]]$recordsWritten)
  dest <- job$configuration$query$destinationTable
  result <- data.frame(tableId = dest$tableId, datasetId = dest$datasetId, numOfRows = numOfRowsProcessed, totalBytesProcessed = totalBytesProcessed)
}

#' API to get a data from google BigQuery table
#' @export
getDataFromGoogleBigQueryTable <- function(project, dataset, table, page_size = 10000, max_page, tokenFileId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId)
  bigrquery::set_access_cred(token)

  bigrquery::list_tabledata(project, dataset, table, page_size = page_size,
                 table_info = NULL, max_pages = max_page)
}

#' API to extract data from google BigQuery table to Google Cloud Storage
#' @export
extractDataFromGoogleBigQueryToCloudStorage <- function(project, dataset, table, destinationUri, tokenFileId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId)
  bigrquery::set_access_cred(token)
  # call forked bigrquery for submitting extract job
  job <- bigrquery::insert_extract_job(project, dataset, table, destinationUri,
                                print_header=TRUE, field_delimiter=",", destination_format="CSV", compression="GZIP")
  job <- bigrquery::wait_for(job)
  job
}

#' API to download data from Google Storage to client and create a data frame from it
#' @export
downloadDataFromGoogleCloudStorage <- function(bucket, folder, download_dir, tokenFileId){
  if(!requireNamespace("googleCloudStorageR")){stop("package googleCloudStorageR must be installed.")}
  if(!requireNamespace("googleAuthR")){stop("package googleAuthR must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId)
  googleAuthR::gar_auth(token = token)
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
      googleCloudStorageR::gcs_get_object(name, saveToDisk = str_c(download_dir, "/", stringr::str_replace(name, stringr::str_c(folder, "/"),"")))
      googleCloudStorageR::gcs_delete_object(name, bucket = bucket)
    }
  });
  files <- list.files(path=download_dir, pattern = ".gz");
  df <- lapply(files, function(file){readr::read_csv(stringr::str_c(download_dir, "/", file))}) %>% dplyr::bind_rows()
}

#' API to get a list of buckets from Google Cloud Storage
#' @export
listGoogleCloudStorageBuckets <- function(project, tokenFileId){
  if(!requireNamespace("googleCloudStorageR")){stop("package googleCloudStorageR must be installed.")}
  if(!requireNamespace("googleAuthR")){stop("package googleAuthR must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId)
  googleAuthR::gar_auth(token = token)
  googleCloudStorageR::gcs_list_buckets(projectId = project, projection = c("full"))
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
getDataFromGoogleBigQueryTableViaCloudStorage <- function(bucketProjectId, dataSet, table, bucket, folder, tokenFileId){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  # submit a job to extract query result to cloud storage
  uri = stringr::str_c('gs://', bucket, "/", folder, "/", "exploratory_temp*.gz")
  job <- exploratory::extractDataFromGoogleBigQueryToCloudStorage(project = bucketProjectId, dataset = dataSet, table = table, uri,tokenFileId);
  # wait for extract to be done
  job <- bigrquery::wait_for(job)
  # download tgzip file to client
  df <- exploratory::downloadDataFromGoogleCloudStorage(bucket = bucket, folder=folder, download_dir = tempdir(), tokenFileId = tokenFileId)
}

#' Get data from google big query
#' @param projectId - Google BigQuery project id
#' @param sqlquery - SQL query to get data
#' @param destination_table - Google BigQuery table where query result is saved
#' @param page_size - Number of items per page.
#' @param max_page - maximum number of pages to retrieve.
#' @param write_deposition - controls how your BigQuery write operation applies to an existing table.
#' @param tokenFileId - file id for auth token
#' @param bucketProjectId - Id of the Project where Google Cloud Storage Bucket belongs
#' @param bucket - Google Cloud Storage Bucket
#' @param folder - Folder under Google Cloud Storage Bucket where temp files are extracted.
#' @export
executeGoogleBigQuery <- function(project, sqlquery, destination_table, page_size = 100000, max_page = 10, write_disposition = "WRITE_TRUNCATE", tokenFileId, bucketProjectId, bucket=NULL, folder=NULL){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  if(!requireNamespace("GetoptLong")){stop("package GetoptLong must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  token <- getGoogleTokenForBigQuery(tokenFileId)

  df <- NULL
  # if bucket is set, use Google Cloud Storage for extract and download
  if(!is.null(bucket) && !is.na(bucket) && bucket != "" && !is.null(folder) && !is.na(folder) && folder != ""){
    # destination_table looks like 'exploratory-bigquery-project:exploratory_dataset.exploratory_bq_preview_table'
    dataSetTable = stringr::str_split(stringr::str_replace(destination_table, stringr::str_c(bucketProjectId,":"),""),"\\.")
    dataSet = dataSetTable[[1]][1]
    table = dataSetTable[[1]][2]
    bqtable <- NULL
    sqlquery <- convertUserInputToUtf8(sqlquery)
    # submit a query to get a result (for refresh data frame case)
    result <- exploratory::submitGoogleBigQueryJob(bucketProjectId, sqlquery, destination_table, write_disposition = "WRITE_TRUNCATE", tokenFileId);
    # extranct result from Google BigQuery to Google Cloud Storage and import
    df <- getDataFromGoogleBigQueryTableViaCloudStorage(bucketProjectId, dataSet, table, bucket, folder, tokenFileId)
  } else {
    # direct import case (for refresh data frame case)
    bigrquery::set_access_cred(token)
    # check if the query contains special key word for standardSQL
    # If we do not pass the useLegaySql argument, bigrquery set TRUE for it, so we need to expliclity set it to make standard SQL work.
    isStandardSQL <- stringr::str_detect(sqlquery, "#standardSQL")
    # set envir = parent.frame() to get variables from users environment, not papckage environment
    df <- bigrquery::query_exec(GetoptLong::qq(sqlquery, envir = parent.frame()), project = project, destination_table = destination_table,
                                page_size = page_size, max_page = max_page, write_disposition = write_disposition, useLegacySql = isStandardSQL == FALSE)
  }
  df
}

#' API to get projects for current oauth token
#' @export
getGoogleBigQueryProjects <- function(tokenFileId=""){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  tryCatch({
    token <- getGoogleTokenForBigQuery(tokenFileId);
    bigrquery::set_access_cred(token)
    projects <- bigrquery::list_projects();
  }, error = function(err){
    c("")
  })
}

#' API to get datasets for a project
#' @export
getGoogleBigQueryDataSets <- function(project, tokenFileId=""){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  tryCatch({
    token <- getGoogleTokenForBigQuery(tokenFileId);
    bigrquery::set_access_cred(token)
    resultdatasets <- bigrquery::list_datasets(project);
  }, error = function(err){
     c("")
  })
}

#' API to get tables for current project, data set
#' @export
getGoogleBigQueryTables <- function(project, dataset, tokenFileId=""){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  tryCatch({
    token <- getGoogleTokenForBigQuery(tokenFileId);
    bigrquery::set_access_cred(token)
    # if we do not pass max_results, it only returnss 50 items. so explicitly set it.
    tables <- bigrquery::list_tables(project, dataset, page_size=1000000);
  }, error = function(err){
    c("")
  })
}

#' API to get table info
#' @export
getGoogleBigQueryTable <- function(project, dataset, table, tokenFileId=""){
  if(!requireNamespace("bigrquery")){stop("package bigrquery must be installed.")}
  token <- getGoogleTokenForBigQuery(tokenFileId);
  bigrquery::set_access_cred(token)
  table <- bigrquery::get_table(project, dataset, table);
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
  .htmltables <- parse_html_tables(url, encoding)
  tibble::repair_names(rvest::html_table(.htmltables[[index]], fill=TRUE ,header=heading))
}


#' function to convert labelled class to factoror
#' see https://github.com/exploratory-io/tam/issues/1481
#' @export
handleLabelledColumns = function(df){
  is_labelled <- which(lapply(df, class) == "labelled")
  df[is_labelled] <- lapply(df[is_labelled], haven::as_factor)
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
  # remove tab, new line, carriage return from column names
  clean_names <- original_names  %>% gsub("[\r\n\t]", "", .)
  names(df) <- clean_names
  df
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
  # lower case and get rid of space, period, apostrophe, and hiphen to normalize inputs.
  input_normalized <- gsub("[ \\.\\'\\-]", "", tolower(input))
  # return matching state info.
  # state_name_id_map data frame has all those state info plus normalized_name as the search key.
  return (as.character(state_name_id_map[[output_type]][match(input_normalized, state_name_id_map$normalized_name)]))
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
#' @return data frame
#' @export
select_columns <- function(x, ...) {
  col <- colnames(x)[colnames(x) %in% list(...)]
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
    filepath <- eval(as.name(hash))
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

    # download file to tempoprary location
    download.file(url, destfile = tmp, mode = "wb")
    # cache file
    if(!is.null(shouldCacheFile) && isTRUE(shouldCacheFile)){
      assign(hash, tmp, envir = .GlobalEnv)
    }
    tmp
  }
}

#'Wrapper for readxl::read_excel to support remote file
#'@export
read_excel_file <- function(path, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0, trim_ws = TRUE, n_max = Inf){
  loadNamespace("readxl")
  loadNamespace("stringr")
  if (stringr::str_detect(path, "^https://") ||
      stringr::str_detect(path, "^http://") ||
      stringr::str_detect(path, "^ftp://")) {
    tmp <- download_data_file(path, "excel")
    readxl::read_excel(tmp, sheet = sheet, col_names = col_names, col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max)
  } else {
    # if it's local file simply call readxl::read_excel
    readxl::read_excel(path, sheet = sheet, col_names = col_names, col_types = col_types, na = na, trim_ws = trim_ws, skip = skip, n_max = n_max)
  }
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
    # if it's local file simply call readxl::read_excel
    readxl::excel_sheets(path)
  }
}

#'Wrapper for readr::read_delim to support remote file
#'@export
read_delim_file <- function(file, delim, quote = '"',
                            escape_backslash = FALSE, escape_double = TRUE,
                            col_names = TRUE, col_types = NULL,
                            locale = readr::default_locale(),
                            na = c("", "NA"), quoted_na = TRUE,
                            comment = "", trim_ws = FALSE,
                            skip = 0, n_max = Inf, guess_max = min(1000, n_max), progress = interactive()){
  loadNamespace("readr")
  loadNamespace("stringr")
  if (stringr::str_detect(file, "^https://") ||
      stringr::str_detect(file, "^http://") ||
      stringr::str_detect(file, "^ftp://")) {
    tmp <- download_data_file(file, "csv")
    readr::read_delim(tmp, delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, col_names = col_names, col_types = col_types,
                      locale = locale, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress)
  } else {
    # if it's local file simply call readr::read_delim
    readr::read_delim(file, delim, quote = quote, escape_backslash = escape_backslash, escape_double = escape_double, col_names = col_names, col_types = col_types,
                      locale = locale, na = na, quoted_na = quoted_na, comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, progress = progress)
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
    # if it's local file simply call readr::read_delim
    readr::guess_encoding(file, n_max, threshold)
  }
}

#'Wrapper for readr::read_log to support remote file
#'@export
read_log_file <- function(file, col_names = FALSE, col_types = NULL,
                          skip = 0, n_max = Inf, progress = interactive()){
  loadNamespace("readr")
  loadNamespace("stringr")
  if (stringr::str_detect(file, "^https://") ||
      stringr::str_detect(file, "^http://") ||
      stringr::str_detect(file, "^ftp://")) {
    tmp <- download_data_file(file, "log")
    readr::read_log(tmp, col_names, col_types, skip, n_max, progress)
  } else {
    # if it's local file simply call readr::read_log
    readr::read_log(file, col_names, col_types, skip, n_max, progress)
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
    readRDS(gzcon(url(file)), refhook)
  } else {
    # if it's local file simply call read_rds
    readRDS(file, refhook)
  }
}
