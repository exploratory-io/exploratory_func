#' API to login to Salesforce
#' export
#' @param sever - (optional) login server
#' @param username - Salesforce login user name
#' @param password - Salesforce login password
#' @param securityToken - (optional) security token if required
#' This API takes care of Salesforce authentication and should be called before calling any Salesforce related APIs.
loginToSalesforce <- function(server = NULL, username, password, securityToken = NULL){
  if (!requireNamespace("salesforcer")) {
    stop("package salesforcer must be installed.")
  }
  if (is.null(server)) { # if login server was not provided, try it with the default login server.
    server = "https://login.salesforce.com"
  }
  if (is.null(securityToken)) {
    salesforcer::sf_auth(login_url = server, username = username, password = password, cache = FALSE)
  } else {
    salesforcer::sf_auth(login_url = server, username = username, password = password, security_token = securityToken, cache = FALSE)
  }
}

#' API to get list of Standard and Custom Objects from Salesforce
#' export
#' @param sever - (optional) login server
#' @param username - Salesforce login user name
#' @param password - Salesforce login password
#' @param securityToken - (optional) security token if required
querySaleseforceMetadata <- function(server = NULL, username, password, securityToken = NULL){
  if (!requireNamespace("salesforcer")) {
    stop("package salesforcer must be installed.")
  }
  loginToSalesforce(server = server, username = username, password = password, securityToken = securityToken)
  # Standard Objects are implementation of CustomObjects, so pass CustomObjects as the type to get both Standard Objects and Custom Objects.
  # ref: https://github.com/StevenMMortimer/salesforcer#using-the-metadata-api
  my_queries <- list(list(type='CustomObject'))
  metadata_info <- salesforcer::sf_list_metadata(queries=my_queries)
  metadata_info
}

#' API to get Table Details (i.e. column names, column data types, etc)
#' export
#' @param sever - (optional) login server
#' @param username - Salesforce login user name
#' @param password - Salesforce login password
#' @param securityToken - (optional) security token if required
#' @param table - name of the table that you want to get details.
querySaleseforceTableDetails <- function(server = NULL, username, password, securityToken = NULL, table){
  if (!requireNamespace("salesforcer")) {
    stop("package salesforcer must be installed.")
  }
  loginToSalesforce(server = server, username = username, password = password, securityToken = securityToken)
  salesforcer::sf_describe_object_fields(table)
}

#' API to execute Salesforce Object Query Language (SOQL)
#' export
#' @param sever - (optional) login server
#' @param username - Salesforce login user name
#' @param password - Salesforce login password
#' @param securityToken - (optional) security token to login
#' @param query - SOQL query.
querySalesforceDataWithQuery <- function(server = NULL, username, password, securityToken = NULL, query = "", guessType = TRUE){
  if (!requireNamespace("salesforcer")) {
    stop("package salesforcer must be installed.")
  }
  # increase batch size to improve performance
  queryControl <- salesforcer::sf_control(QueryOptions = list(batchSize = 2000))
  loginToSalesforce(server = server, username = username, password = password, securityToken = securityToken)
  query <- glue_exploratory(query, .transformer=salesforce_glue_transformer, .envir = parent.frame())
  salesforcer::sf_query(soql = query, control = queryControl, guess_types = guessType)
}

#' API to execute Salesforce Object Query Language (SOQL)
#' export
#' @param sever - (optional) login server
#' @param username - Salesforce login user name
#' @param password - Salesforce login password
#' @param securityToken - (optional) security token to login
#' @param table - The table you want to get data
#' @param columns - list of columns that you want to get data from the table.
querySalesforceDataFromTable <- function(server = NULL, username, password, securityToken = NULL, table = NULL, columns = NULL, guessType = TRUE, conditions = NULL, limit = NULL){
  if (!requireNamespace("salesforcer")) {
    stop("package salesforcer must be installed.")
  }
  if (is.null(table)) {
    stop("Please set table.")
  }
  if (is.null(columns)) {
    stop("Please set columns.")
  }
  query <- stringr::str_c("SELECT ", stringr::str_c(columns, collapse = ", "), " FROM ", table)
  if (!is.null(conditions)) {
    query <- stringr::str_c(query, " ", conditions)
  }
  if (!is.null(limit)) {
    query <- stringr::str_c(query, " LIMIT ", limit)
  }
  querySalesforceDataWithQuery(server = server, username = username, password = password, securityToken = securityToken, query = query)
}
