#' API to login to Salesforce
#' export
#' @param sever - (optional) login server
#' @param username - Salesforce login user name
#' @param password - Salesforce login password
#' @param securityToken - (optional) security token if required
#' This API
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
querySalesforceData <- function(server = NULL, username, password, securityToken = NULL, query, pageSize = 2000, guessType = TRUE){
  if (!requireNamespace("salesforcer")) {
    stop("package salesforcer must be installed.")
  }
  queryControl <- salesforcer::sf_control(QueryOptions = list(batchSize = pageSize))
  loginToSalesforce(server = server, username = username, password = password, securityToken = securityToken)
  salesforcer::sf_query(soql = query, control = queryControl, guess_types = guessType)
}
