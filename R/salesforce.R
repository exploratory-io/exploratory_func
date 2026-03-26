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
  token <- NULL
  tryCatch({
    token <- exploratory::getSalesforceToken()
  },error = function(e){
    # Since it returns error when the token is not set from Exploratory Desktop, ignore the error.
    if (e$message == "OAuth token is not set for Salesforce") {
      # ignore
    } else {
      stop(e$message);
    }
  })
  if (!is.null(token)) {
    salesforcer::sf_auth(login_url = server, token = token, cache = FALSE)
  } else if (is.null(securityToken)) {
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
querySalesforceMetadata <- function(server = NULL, username, password, securityToken = NULL){
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
querySalesforceTableDetails <- function(server = NULL, username, password, securityToken = NULL, table){
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
#' @param apiType - it could be either REST, SOAP, Bulk 1.0, or Bulk 2.0. By default it's REST.
#' Bulk can handle a large data set but it has limitation (ref: https://developer.salesforce.com/docs/atlas.en-us.api_asynch.meta/api_asynch/asynch_api_concepts_limits.htm)
querySalesforceDataWithQuery <- function(server = NULL, username, password, securityToken = NULL, query = "", guessType = TRUE, apiType = "REST"){
  if (!requireNamespace("salesforcer")) {
    stop("package salesforcer must be installed.")
  }
  # increase batch size to improve performance
  queryControl <- salesforcer::sf_control(QueryOptions = list(batchSize = 2000))
  loginToSalesforce(server = server, username = username, password = password, securityToken = securityToken)
  query <- glue_exploratory(query, .transformer=salesforce_glue_transformer, .envir = parent.frame())
  salesforcer::sf_query(soql = query, control = queryControl, guess_types = guessType, api_type = apiType)
}


  if (!is.null(limit)) {
    query <- stringr::str_c(query, " LIMIT ", limit)
  }
  df <- querySalesforceDataWithQuery(server = server, username = username, password = password, securityToken = securityToken, query = query)
  resultNames <- colnames(df)
  # Check if there are any columns dropped from the query result.
  coldiff <- setdiff(columns, resultNames)
  colLength <- length(coldiff)
  # Bring back the dropped column by adding it with all NA.
  if (colLength > 0) { # Check if there is missing column.
    for(i in 1:colLength) {
      col <- coldiff[i]
      dataType <- dataTypes[which(columns == col)]
      if (dataType == "character") {
        df[col] <- as.character(NA)
      } else if (dataType == "numeric") {
        df[col] <- as.numeric(NA)
      } else if (dataType == "Date") {
        df[col] <- as.Date(NA)
      } else if (dataType == "POSIXct") {
        df[col] <- as.POSIXct(NA)
      } else {
        df[col] <- NA
      }
    }
  }
  # Make sure resulting column order is as same as column order selected in UI.
  df %>% dplyr::select(columns)
}
