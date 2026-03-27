
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
  # For "since" parameter, it needs to be character instead of Date.
  res <- httr::POST(
    url,
    body = list(
      apikey = apikey,
      id = id,
      since = as.character(date_since)
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
      dplyr::mutate(dplyr::across(c(
        signup_time,
        confirm_time,
        last_changed
      ), lubridate::ymd_hms))
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
  # For "since" parameter, it needs to be character instead of Date.
  res <- httr::POST(
    url,
    body = list(
      apikey = apikey,
      id = id,
      include_empty = include_empty,
      since = as.character(date_since)
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
