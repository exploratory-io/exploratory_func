#' Get mailchimp data
#' @param endpoint Name of target data to access under api.mailchimp.com
#' e.g. "reports", "lists/members"
#' @param date_since Filter data by date
#' @param before From how many days/weeks/months before the data should be returned
#' @param units Time unit for before argument. Can be "days", "weeks", "months" and "years".
#' @param simple If TRUE, returns only limited fields from api.
#' @param include_empty This works only when endpoint is reports/email-activity.
#' If set to “true” a record for every email address sent to will be returned even if there is no activity data.
#' @export
get_mailchimp_data <- function(endpoint, date_since = NULL, before = NULL, units = "days", simple = TRUE, include_empty = FALSE){
  token_info <- getTokenInfo("mailchimp")
  area <- ""
  api_key <- if(
    !is.null(token_info) &&
    !is.null(token_info$access_token) &&
    !is.null(token_info$dc)
  ){
    area <- token_info$dc
    token_info$access_token
  } else {
    stop("No access token is set.")
  }

  if(is.null(date_since) && !is.null(before)){
    date_since = lubridate::today() - lubridate::period(before, units = units)
  }

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
    if(simple){
      if(path == "reports"){
        excluded_fields <- c(
          "reports.ecommerce",
          "reports.delivery_status",
          "reports.share_report",
          "reports.delivery_status",
          "reports.industry_stats",
          "reports.facebook_likes",
          "reports.timeseries",
          "reports._links"
        )
        if (endpoint == "reports/email-activity"){
          excluded_fields <- c(
            excluded_fields,
            "reports.type",
            "reports.list_id",
            "reports.list_name",
            "reports.bounces",
            "reports.subject_line",
            "reports.emails_sent",
            "reports.abuse_reports",
            "reports.unsubscribed",
            "reports.bounces",
            "reports.forwards",
            "reports.opens",
            "reports.clicks",
            "reports.list_stats"
          )
        }
        query$exclude_fields <- paste0(excluded_fields, collapse = ",")
      } else if (path == "lists") {
        excluded_fields <- c(
          "lists.web_id",
          "lists.use_archive_bar",
          "lists.notify_on_subscribe",
          "lists.notify_on_unsubscribe",
          "lists.subscribe_url_short",
          "lists.subscribe_url_long",
          "lists.beamer_address"
        )
        if (endpoint == "lists/members"){
          excluded_fields <- c(
            excluded_fields,
            "lists.permission_reminder",
            "lists.date_created",
            "lists.list_rating",
            "lists.email_type_option",
            "lists.visibility",
            "lists.contact",
            "lists.campaign_defaults",
            "lists.stats"
          )
        }
        query$exclude_fields <- paste0(excluded_fields, collapse = ",")
      }
    }
    ret <- list()
    while(TRUE){
      url <- paste0("https://", area,".api.mailchimp.com/3.0/", path)

      res <- httr::GET(url, httr::authenticate("any", api_key), query = query)

      from_json <- res %>% httr::content(as = "text") %>% jsonlite::fromJSON()

      # get last path to get the name of fetched objects
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

  base_query <- list(count = 1000)
  # set filtering query depending on endpoint
  with_filter_query <- if(!is.null(date_since) &&
                    !is.null(date_filter_params[[endpoint]])){
    query <- base_query
    query[[date_filter_params[[endpoint]]]] <- date_since
    query
  } else {
    base_query
  }

  ret <- if(stringr::str_detect(endpoint, "/")){
    split <- stringr::str_split(endpoint, "/")
    ret <- access_api(with_filter_query, split[[1]][[1]])
    ids <- ret$id
    if(endpoint == "reports/email-activity"){
      # this is to get activities in a campaign
      export_activity <- function(id, dc, apikey){
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
        split <- stringr::str_split(text, "\n")[[1]]
        # remove last value because it is just an empty string
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
            ret <- data.frame(campain_id = id, email = email)
            ret$activity <- activity
            ret
          }
        })
        do.call(dplyr::bind_rows, row_data)
      }
      ret$data <- lapply(ids, function(id){
        export_activity(id, dc = area, apikey = api_key)
      })
    } else if(endpoint == "lists/members" && simple){
      export_members <- function(id, dc, apikey){
        url <- paste0("https://", dc, ".api.mailchimp.com/export/1.0/list/", sep = "")
        res <- httr::POST(
          url,
          body = list(
            apikey = apikey,
            id = id
          )
        )
        text <- httr::content(res, as = "text")
        split <- stringr::str_split(text, "\n")[[1]]
        header <- jsonlite::fromJSON(split[1])
        main <- split[-1]
        # remove last value because it is just an empty string
        row_data <- lapply(main[!is_empty(main)], function(line){
          parsed <- jsonlite::fromJSON(line)
          setNames(as.data.frame(as.list(parsed), stringsAsFactors = FALSE), header)
        })
        if(length(row_data) > 0){
          janitor::clean_names(do.call(dplyr::bind_rows, row_data))
        } else {
          list()
        }
      }

      ret$data <- lapply(ids, function(id){
        export_members(id, dc = area, apikey = api_key)
      })
      ret <- ret %>%
        dplyr::mutate(list_id = id) %>% # keep id column to prevent fail of select later
        dplyr::rename(list_name = name)
    } else {
      endpoints <- paste(split[[1]][[1]], "/", ids, "/", split[[1]][[2]], sep = "")
      ret$data <- lapply(endpoints, function(endpoint){
        access_api(with_filter_query, endpoint)
      })
    }
    filtered <- ret %>%
      dplyr::select(-id) %>% # remove id because it's duplicated in nested parameters
      dplyr::filter(exploratory::list_n(data) > 0)
    if(nrow(filtered) == 0){
      stop("No data found")
    }
    filtered %>%
      tidyr::unnest(data)
  } else {
    access_api(with_filter_query, endpoint)
  }


  if(is.null(ret)){
    stop("No data found.")
  } else {
    ret
  }
}
