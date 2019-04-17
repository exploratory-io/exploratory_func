#' @export
queryPresto <- function(host, port, username, password = "", schema, catalog, numOfRows = -1, query, ...){
  if(!requireNamespace("httr")){stop("package httr must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}
  if(!requireNamespace("RPresto")){stop("package RPresto must be installed.")}

  # read stored password
  pass = saveOrReadPassword("presto", username, password)
  drv <- RPresto::Presto()
  # To workaround Presto Authentication issue, set X-Presto-User to http header.
  # Please refer https://github.com/prestodb/RPresto/issues/103 for details.
  httr::set_config(
    httr::add_headers(stringr::str_c("X-Presto-User", username, sep = "="))
  )
  conn <- RPresto::dbConnect(drv, user = username,
                                password = pass, host = host, port = port, schema = schema, catalog = catalog, session.timezone = Sys.timezone(location = TRUE))
  # set envir = parent.frame() to get variables from users environment, not papckage environment
  resultSet <- RPresto::dbSendQuery(conn, glue::glue_sql(query, .con = conn, .envir = parent.frame()))
  df <- DBI::dbFetch(resultSet, n = numOfRows)
  RPresto::dbClearResult(resultSet)
  RPresto::dbDisconnect(conn)
  df
}
