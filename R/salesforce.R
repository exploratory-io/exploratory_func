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
#' @param dataType - list of column data types for the above columns parameter
#' @param guessType - flag to tell that dataType should be detected. If FALSE is passed, all the data types become character.
#' @param conditions - (optional) SOQL where conditions.
#' @param limit - number of rows to return.
#' @param logicalOperator (optional) - required if the conditions are set. By default it uses "and" logical operator to construction SOQL where clause. It also supports "or" operator.
querySalesforceDataFromTable <- function(server = NULL, username, password, securityToken = NULL, table = NULL, columns = NULL, dataTypes = NULL, guessType = TRUE, conditions = NULL, limit = NULL, logicalOperator = "and"){
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
    conditionLength = length(conditions)
    conditionCount <- 0
    whereClause <- ""
    for(i in 1:conditionLength) {
      # Check if the filter condition uses parameter (i.e. detect @{param})
      hasParameter <- stringr::str_detect(conditions[i], "\\@\\{([^\\}]+)\\}")
      # make sure to resolve parameter @{} syntax.
      condition <- glue_exploratory(conditions[i], .transformer=salesforce_glue_transformer, .envir = parent.frame())
      # make sure to resolve ${} syntax for Salesforce filter.
      condition <- glue_salesforce(condition)
      # When the IN (NULL) condition is detected after resolving parameter, remove the condition to support select "ALL" option.
      # NOTE: IN (@{PARAM}) became IN (NULL) when nothing is selected from UI and Salesforce SOQL does not allow using IS_NULL function,
      # so remove the "Column IN (NULL)" condition.
      if (hasParameter && stringr::str_detect(condition, "IN \\(NULL\\)$")) {
        # do not append the condition.
      } else if (!exploratory::is_empty(condition)){ # if it's not empty string (i.e. ""), append the condition.
        conditionCount = conditionCount + 1;
        if (conditionCount == 1) {
          whereClause <-condition
        } else {
          # At this point whereClause looks like WHERE Col = 'A', so append the next condtion (e.g. Col2 = 'B') and make it as WHERE Col = 'A' AND Col2 = 'B'
          whereClause <- stringr::str_c(whereClause, " ", logicalOperator, " ", condition)
        }
      }
    }
    if (conditionCount > 0) {
      query <- stringr::str_c(query, " WHERE ", whereClause)
    }
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
