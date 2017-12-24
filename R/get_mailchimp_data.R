#' Get mailchimp data
#' @param endpoint Name of target data to access under api.mailchimp.com
#' e.g. "reports", "lists/members"
#' @param date_type Type of date_since argument. Can be "exact", "days", "weeks", "months" or "years".
#' "exact" uses exact date like "2016-01-01".
#' "days", "weeks", "months" or "years" uses a number and get data since that much time ago.
#' @param date_since From when data should be returned.
#' @param include_empty This works only when endpoint is export/1.0/campaignSubscriberActivity.
#' If set to TRUE, a record for every email address sent to will be returned even if there is no activity data.
#' @export
get_mailchimp_data <- function(endpoint = "explort/1.0/list", date_type = "exact", date_since = NULL, include_empty = TRUE, ...){
  token_info <- getTokenInfo("mailchimp")
  # this is data center id like "us-13"
  dc <- ""
  api_key <- if(
    !is.null(token_info) &&
    !is.null(token_info$access_token) &&
    !is.null(token_info$dc)
  ){
    # this is the case when valid access token is set.
    dc <- token_info$dc
    token_info$access_token
  } else {
    stop("No access token is set.")
  }

  if(!is.null(date_since)){
    # if date_type is "exact", date_since is regarded as date string
    # and if it's type unit like "days" or "weeks", the value date_since is
    # subtracted from today and it's regarded as date_since
    if(date_type != "exact"){
      if(!date_type %in% c("days", "weeks", "months", "years")){
        stop("date_type must be \"days\", \"weeks\", \"months\", \"years\" or \"exact\"")
      }
      date_since <- lubridate::today() - lubridate::period(as.numeric(date_since), units = date_type)
      if(is.na(date_since)){
        stop("Value for Date Range is invalid. Please put a number.")
      }
    } else {
      # format validation to check if it can be regarded as Date format
      date_since <- tryCatch({
        as.Date(date_since)
      }, error = function(e){
        stop("Value for Date Range can't be recognized as date. It should be \"2016-08-26\", for example")
      })
    }
  }

  # keys are endpoints and values are query parameters to filter data
  date_filter_params <- list(
    "automations" = "since_create_time",
    "campaigns" = "since_create_time",
    "file-manager/files" = "since_created_at",
    "file-manager/folders" = "since_created_at",
    "lists" = "since_date_created",
    "lists/members" = "since_timestamp_opt",
    "reports" = "since_send_time", # there was no date parameter
    "templates" = "since_created_at"
  )

  # this base query is common query among endpoints
  base_query <- list(count = 1000)
  # set date filtering query depending on the endpoint
  with_filter_query <- if(!is.null(date_since) &&
                    !is.null(date_filter_params[[endpoint]])){
    query <- base_query
    query[[date_filter_params[[endpoint]]]] <- date_since
    query
  } else {
    base_query
  }

  ret <- if (endpoint == "export/1.0/list") {
    # get members list from export api

    # first, get these fields about lists from REST api
    with_filter_query$fields <- paste0(
      c(
        "total_items",
        "lists.id",
        "lists.name",
        "lists.date_created",
        "lists.stats"
      ), collapse = ",")
    ret <- access_api(with_filter_query, dc, api_key, "lists")

    # throw an error if there is no report data
    # to prevent assigning colnames to NULL
    if(length(ret) == 0){
      stop("No data found.")
    }

    # clean up data frame column names removing "stats." prefix
    colnames(ret) <- stringr::str_replace(colnames(ret), "^stats\\.", "")

    ret <- ret %>%
      dplyr::select(-member_count, -merge_field_count, -last_sub_date, -last_unsub_date)

    ids <- ret$id
    # put "list_" suffix to colum names
    # to make it clear that the columns are from list data
    colnames(ret) <- paste0("list_", colnames(ret))

    # get member data for each list from export API
    ret$data <- lapply(ids, function(id){
      export_members(id, dc, api_key, date_since)
    })
    # there might be empty data and tidyr::unnest causes an error
    # , so unnest_without_empty is used
    ret %>%
      unnest_without_empty(data)
  } else if (endpoint == "export/1.0/campaignSubscriberActivity") {
    # get email activities from export api

    # first, get these fields about reports from REST api
    with_filter_query$fields <- paste0(
      c(
        "total_items",
        "reports.id",
        "reports.list_id",
        "reports.list_name",
        "reports.emails_sent",
        "reports.send_time",
        "reports.campaign_title",
        "reports.opens"
      ),
    collapse = ",")
    ret <- access_api(with_filter_query, dc, api_key,"reports")

    # throw an error if there is no report data
    # to prevent assigning colnames to NULL
    if(length(ret) == 0){
      stop("No data found.")
    }

    # clean up data frame column names removing "opens." prefix
    colnames(ret) <- stringr::str_replace(colnames(ret), "^opens\\.", "")

    ids <- ret$id
    # put "eport_" suffix to colum names
    # to make it clear that the columns are from report data
    colnames(ret) <- paste0("report_", colnames(ret))
    # get activity data for each list from export API
    ret$data <- lapply(ids, function(id){
      export_activity(id, dc, api_key, date_since, include_empty)
    })
    # there might be empty data and tidyr::unnest causes an error
    # , so unnest_without_empty is used
    ret %>%
      unnest_without_empty(data)
  } else if(endpoint == "lists/members"){
    # get member info from REST api for each list
    # this is for Exploratory Desktop 3.3 backward compatibility
    ret <- access_api(with_filter_query, dc = dc, apikey = api_key, endpoint = "lists")
    ids <- ret$id
    endpoints <- paste("lists", "/", ids, "/", "members", sep = "")
    ret$data <- lapply(endpoints, function(endpoint){
      access_api(with_filter_query, dc = dc, apikey = api_key, endpoint = endpoint)
    })

    # there might be empty data and tidyr::unnest causes an error
    # , so unnest_without_empty is used
    ret %>%
      unnest_without_empty(data)

  } else {
    access_api(with_filter_query, dc, api_key, endpoint)
  }

  if(is.null(ret)){
    stop("No data found.")
  } else {
    ret
  }
}

#' Access data from REST api
#' @param query Query parameters for API access
#' @param dc Data center id
#' @param apikey Access key for API access
#' @param path API path to access
access_api <- function(query, dc, apikey, endpoint){
  query$offset <- 0
  ret <- list()
  # get last endpoint to get the name of fetched objects
  split <- stringr::str_split(endpoint, "/")
  key <- if(length(split[[1]]) > 1) {
    if(stringr::str_detect(endpoint, "/email-activity$")){
      "emails"
    } else {
      # this is used when endpoint is something like ecommerce/stores
      # because in that case, "stores" is used as the key
      tail(split[[1]], 1)
    }
  } else {
    endpoint
  }

  if(is.null(query$fields)){
    # _links is urls for the actions of the data like deleting
    # , so it's not important for data analysis and excluded from fields
    query$exclude_fields <- paste0(key, "._links")
  }

  while(TRUE){
    url <- paste0("https://", dc,".api.mailchimp.com/3.0/", endpoint)

    res <- httr::GET(url, httr::authenticate("any", apikey), query = query)

    from_json <- res %>% httr::content(as = "text") %>% jsonlite::fromJSON()

    data <- tryCatch({
      from_json[[key]] %>% jsonlite::flatten()
    }, error = function(e){
      NULL
    })

    if(is.null(data) || length(data) == 0) {
      break()
    }

    # somehow, offset query in /templates doesn't work,
    # so check if access won't exceeds total number of the items
    if(from_json$total_items >= query$offset + nrow(data)){
      ret <- append(ret, list(data))
      query$offset <- query$offset + query$count
    } else {
      break()
    }
  }

  if(length(ret) == 0){
    # prevent applying bind_rows to NULL or empty list
    NULL
  } else {
    do.call(dplyr::bind_rows, ret)
  }
}

#' Access members export API
#' @param id List ID
#' @param dc Data center id
#' @param apikey Access key for API access
#' @param date_since From when members data should be returned
export_members <- function(id, dc, apikey, date_since){
  url <- paste0("https://", dc, ".api.mailchimp.com/export/1.0/list/", sep = "")
  res <- httr::POST(
    url,
    body = list(
      apikey = apikey,
      id = id,
      since = date_since
    )
  )
  text <- httr::content(res, as = "text")

  # objects are separated by "\n" (ndjson format)
  # , so parse them line by line
  split <- stringr::str_split(text, "\n")[[1]]
  # first object is header
  header <- jsonlite::fromJSON(split[1])
  main <- split[-1]
  # there are sometimes empty strings, so they are removed
  row_data <- lapply(main[!is_empty(main)], function(line){
    parsed <- jsonlite::fromJSON(line)
    setNames(as.data.frame(as.list(parsed), stringsAsFactors = FALSE), header)
  })
  ret <- if(length(row_data) > 0){
    janitor::clean_names(do.call(dplyr::bind_rows, row_data))
  } else {
    list()
  }
  if(length(ret) == 0){
    ret
  } else {
    ret %>%
      dplyr::rename(
        unique_email_id = euid,
        ip_signup = confirm_ip,
        signup_time = optin_time,
        country_code = cc
      ) %>%
      select(-leid, -notes) %>%
      dplyr::mutate_at(dplyr::vars(
        signup_time,
        confirm_time,
        last_changed
      ), lubridate::ymd_hms)
  }
}

#' Access activities export API
#' @param id Report ID
#' @param dc Data center id
#' @param apikey Access key for API access
#' @param date_since From when members data should be returned
#' @param include_empty  If set to TRUE, a record for every email address sent to will be returned even if there is no activity data
export_activity <- function(id, dc, apikey, date_since, include_empty){
  url <- paste0("https://", dc, ".api.mailchimp.com/export/1.0/campaignSubscriberActivity/", sep = "")
  res <- httr::POST(
    url,
    body = list(
      apikey = apikey,
      id = id,
      include_empty = include_empty,
      since = date_since
    )
  )
  text <- httr::content(res, as = "text")
  # objects are separated by "\n" (ndjson format)
  # , so parse them line by line
  split <- stringr::str_split(text, "\n")[[1]]
  # there are sometimes empty strings, so they are removed
  row_data <- lapply(split[!is_empty(split)], function(line){
    parsed <- tryCatch({
      jsonlite::fromJSON(line)
    }, error = function(e){
      stop(line)
      NULL
    })

    if(is.null(parsed)){
      list()
    } else {
      # key of the object is the email address
      email <- names(parsed)
      activity <- list(parsed[[email]])
      ret <- data.frame(email = email)
      ret$activity <- activity
      ret
    }
  })
  ret <- do.call(dplyr::bind_rows, row_data)
}
