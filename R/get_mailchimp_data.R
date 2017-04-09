#' Get mailchimp data
#' @export
get_mailchimp_data <- function(api_key, endpoint, date_since){
  # there is a server area information in api_key
  area <- tryCatch({
    stringr::str_split(api_key, "-")[[1]][[2]]
  }, error = function(e){
    stop("API key is invalid")
  })
  # key is endpoint and value is query parameter to filter data
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

  access_api <- function(query, path){
    query$offset <- 0
    ret <- list()
    while(TRUE){
      url <- paste0("https://", area,".api.mailchimp.com/3.0/", path)

      res <- httr::GET(url, httr::authenticate("any", api_key), query = query)

      from_json <- res %>% httr::content(as = "text") %>% jsonlite::fromJSON()

      split <- stringr::str_split(path, "/")

      if(length(split[[1]]) > 1) {
        key <- tail(split[[1]], 1)
      } else {
        key <- path
      }

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
      NULL
    } else {
      do.call(dplyr::bind_rows, ret)
    }
  }

  base_query <- list(count = 100)
  # set filtering query depending on endpoint
  with_filter_query <- if(!is.null(date_since) &&
                    !is.null(date_filter_params[[endpoint]])){
    query <- base_query
    query[[date_filter_params[[endpoint]]]] <- date_since
    query
  } else {
    base_query
  }

  ret <- if(stringr::str_detect(endpoint, "^lists/")){
    # In this case, access path can be lists/{list_id}/members,
    # so first, get all lists and get members from their ids

    # set filtering query to list access query
    list_query <- base_query
    if(!is.null(date_since)){
      list_query[[date_filter_params[["lists"]]]] <- date_since
    }
    ret <- access_api(list_query, "lists")
    split <- stringr::str_split(endpoint, "/")
    if (length(split[[1]]) == 1){
      ret
    } else {
      list_ids <- ret$id
      paths <- paste("lists/", list_ids, "/", split[[1]][[2]], sep = "")
      data <- lapply(paths, function(path){
        access_api(with_filter_query, path)
      })
      do.call(dplyr::bind_rows, data)
    }
  } else {
    ret <- access_api(with_filter_query, endpoint)
  }

  if(is.null(ret)){
    stop("No data found.")
  } else {
    ret
  }
}
