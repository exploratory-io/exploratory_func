#' @export
queryPresto <- function(host, port, username, password = "", schema, catalog, numOfRows = -1, query, ...){
  if(!requireNamespace("httr")){stop("package httr must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}
  if(!requireNamespace("RPresto")){stop("package RPresto must be installed.")}


  # read stored password
  pass <- saveOrReadPassword("presto", username, password)
  conn <- getDBConnection("presto", host = host, port = port, databaseName = "", username = username, password = pass, catalog = catalog, schema = schema, dsn="", additionalParams = "",
                          collection = "", isSSL = FALSE, authSource = NULL, cluster = NULL, timeout = NULL)
  tryCatch({
    query <- convertUserInputToUtf8(query)
    # set envir = parent.frame() to get variables from users environment, not package environment
    # glue_sql does not quote Date or POSIXct. Let's use our sql_glue_transformer here.
    query <- glue_exploratory(query, .transformer=sql_glue_transformer, .envir = parent.frame())
    resultSet <- RPresto::dbSendQuery(conn,query)
    df <- DBI::dbFetch(resultSet, n = numOfRows)
  }, error = function(err) {
    # clear connection in pool so that new connection will be used for the next try
    clearDBConnection("presto", host, port, databaseName, username)
    stop(err)
  })
  RPresto::dbClearResult(resultSet)
  df
}
