#' @export
queryPresto <- function(host, port, username, password, schema, catalog, numOfRows = -1, query, ...){

  # read stored password
  pass = saveOrReadPassword("presto", username, password)
  drv <- RPresto::Presto()
  conn <- RPresto::dbConnect(drv, user = username,
                                password = pass, host = host, port = port, schema = schema, catalog = catalog, session.timezone = Sys.timezone(location = TRUE))
  # set envir = parent.frame() to get variables from users environment, not papckage environment
  resultSet <- RPresto::dbSendQuery(conn, glue::glue_sql(query, .con = conn, .envir = parent.frame()))
  df <- DBI::dbFetch(resultSet, n = numOfRows)
  RPresto::dbClearResult(resultSet)
  RPresto::dbDisconnect(conn)
  df
}
