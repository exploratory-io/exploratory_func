#' API to get JSESSION ID from Incorta
#' @param server - incorta server (i.e http://<host>:<port>/incorta)
#' @param tenant - tenant
#' @param user - user id
#' @param pass - password.
#' @export
getIncortaJSessionID <- function(server, tenant, user, pass){
  loadNamespace("httr")
  loadNamespace("stringr")
  loadNamespace("dplyr")
  r <- httr::POST(url = stringr::str_c(server, "/authservice/login"), body = list(tenant = tenant, user = user, pass = pass))
  jsessionId <-  (r$cookies %>% filter(name == "JSESSIONID") %>% select(value))$value
  jsessionId
}

#' API to get XSRF-TOKEN from Incorta
#' @param server - incorta server (i.e http://<host>:<port>/incorta)
#' @param jsessionId - JSESSIONID.
#' @export
getIncortaXsrfToken <- function(server, jsessionId){
  loadNamespace("httr")
  loadNamespace("stringr")
  loadNamespace("dplyr")
  res <- httr::GET(url = stringr::str_c(server, "/service/user/isLoggedIn"), httr::set_cookies(` JSESSIONID` = jsessionId))
  xsrfToken <- (res$cookies %>% filter(name == "XSRF-TOKEN") %>% select(value))$value
  xsrfToken
}

#' API to get schemas (JSON format) from Incorta
#' @param server - incorta server (i.e http://<host>:<port>/incorta)
#' @param jsessionId - JSESSIONID.
#' @param xsrfToken - XSRF-TOKEN
#' @export
getIncortaSchemas <- function(server, jsessionId, xsrfToken){
  loadNamespace("httr")
  loadNamespace("stringr")
  loadNamespace("jsonlite")
  h <- c(xsrfToken)
  names(h) <- "X-XSRF-TOKEN"
  resSchemas <- httr::GET(url = str_c(server, "/service/schema/getSchemas"),
                          httr::set_cookies(` JSESSIONID` = jsessionId, `XSRF-TOKEN` = xsrfToken),
                          httr::add_headers(.headers = h))
  schemas <- httr::content(resSchemas)
  jsonlite::toJSON(schemas)
}

#' API to get schema (XML string) from Incorta
#' @param server - incorta server (i.e http://<host>:<port>/incorta)
#' @param jsessionId - JSESSIONID.
#' @param xsrfToken - XSRF-TOKEN
#' @param schemaId - ID of the schema
#' @export
getIncortaSchema <- function(server, jsessionId, xsrfToken, schemaId){
  loadNamespace("httr")
  loadNamespace("stringr")
  h <- c(xsrfToken)
  names(h) <- "X-XSRF-TOKEN"
  resSchema <- httr::GET(url = str_c(server, "/service/schema/getSchema", "?schemaId=", schemaId),
                         httr::set_cookies(` JSESSIONID` = jsessionId, `XSRF-TOKEN` = xsrfToken),
                         httr::add_headers(.headers = h))
  # convert XML as text
  schema <- httr::content(resSchema, type = "text", encoding = "UTF-8")
  schema
}

#' API to get data (XML string) from Incorta with Token
#' @param server - incorta server (i.e http://<host>:<port>/incorta)
#' @param jsessionId - JSESSIONID.
#' @param xsrfToken - XSRF-TOKEN
#' @param schemaId - ID of the schema
#' @export
getIncortaDataWithToken <- function(server, jsessionId, xsrfToken, query){
  h <- c(xsrfToken)
  names(h) <- "X-XSRF-TOKEN"
  data <- httr::POST(url = str_c(server, "/service/viewer"),
                     body = list(sample = FALSE, odbc = FALSE, outputFormat = 'json', code = query),
                     httr::set_cookies(` JSESSIONID` = jsessionId, `XSRF-TOKEN` = xsrfToken),
                     httr::add_headers(.headers = h))
  result <- httr::content(data,  as = "parsed", type = "application/json")
  if(!is.null(result$error)){
    stop(result$error)
  }
  if (is.null(result$comp) | is.null(result$comp[[1]]$rowTree)) {
    stop("No Data Found");
  }
  columnNames <- result$comp[[1]]$labels
  # In result json,rows are availale under result$comp[[1]]$rowTree[[1]]
  df <- dplyr::bind_rows(lapply(result$comp[[1]]$rowTree[[1]], exploratory::toDataFrame))
  colnames(df) <- columnNames
  df
}

#' API to get data (XML string) from Incorta with Token
#' @param server - incorta server (i.e http://<host>:<port>/incorta)
#' @param jsessionId - JSESSIONID.
#' @param xsrfToken - XSRF-TOKEN
#' @param schemaId - ID of the schema
#' @export
queryIncorta <- function(server, tenant, user, pass, query){
  jsessionId <- getIncortaJSessionID(server, tenant = tenant, user = user, pass = pass)
  xsrfToken <- getIncortaXsrfToken(server = server, jsessionId = jsessionId)
  h <- c(xsrfToken)
  names(h) <- "X-XSRF-TOKEN"
  data <- httr::POST(url = str_c(server, "/service/viewer"),
                     body = list(sample = FALSE, odbc = FALSE, outputFormat = 'json', code = query),
                     httr::set_cookies(` JSESSIONID` = jsessionId, `XSRF-TOKEN` = xsrfToken),
                     httr::add_headers(.headers = h))
  result <- httr::content(data,  as = "parsed", type = "application/json")
  if(!is.null(result$error)){
    stop(result$error)
  }
  if (is.null(result$comp) | is.null(result$comp[[1]]$rowTree)) {
    stop("No Data Found");
  }
  columnNames <- result$comp[[1]]$labels
  # In result json,rows are availale under result$comp[[1]]$rowTree[[1]]
  df <- dplyr::bind_rows(lapply(result$comp[[1]]$rowTree[[1]], exploratory::toDataFrame))
  colnames(df) <- columnNames
  df
}
