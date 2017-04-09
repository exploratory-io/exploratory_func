#' Get data from intercom API
#' @param app_id App ID
#' @param key API Key
#' @export
get_intercom_data <- function(endpoint,
                              date_since = NULL,
                              paginate = NULL){
  url <- paste0("https://api.intercom.io/", endpoint)
  data_list <- list()
  page <- 1

  query <- list()

  if(!is.null(date_since)){
    # put from how many days ago the data sould be fetched
    query$date_since <- as.integer(
      difftime(
        lubridate::today(),
        as.POSIXct(date_since),
        units = "days"
        )
      )
  }

  token_info <- getTokenInfo("intercom")
  access_token <- if(!is.null(token_info) && !is.null(token_info$access_token)){
    token_info$access_token
  } else {
    stop("No access token is set.")
  }
  token <- HttrOAuthToken2.0$new(
    authorize = "https://app.intercom.io/oauth",
    access = "https://api.intercom.io/auth/eagle/token",
    appname = "intercom",
    credentials = list(
      access_token = access_token
    )
  )

  while(TRUE){
    res <- httr::GET(url, token)
    from_json <- res %>% httr::content(as = "text") %>% jsonlite::fromJSON(flatten = TRUE)
    ret <- from_json[[endpoint]]
    if(length(ret) == 0){
      stop("No data found.")
    }
    data_list <- append(data_list, list(ret))

    if(!is.null(from_json[["pages"]]) &&
       !is.null(from_json[["pages"]][["next"]])){
      # if paginate is NULL,
      # paginate until no result is returned
      if(is.null(paginate) || page < paginate){
        page <- page + 1
        url <- from_json[["pages"]][["next"]]
        Sys.sleep(0.2)
      } else {
        break()
      }
    } else {
      break()
    }
  }
  ret <- do.call(dplyr::bind_rows, data_list)

  # convert unixtime integer column to datetime
  for(column in colnames(ret)){
    if(
      grepl("_at$", column)
      &&
      is.integer(ret[[column]])
    ){
      ret[[column]] <- unixtime_to_datetime(ret[[column]])
    }
  }

  ret
}
