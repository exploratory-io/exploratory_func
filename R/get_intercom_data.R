#' Get data from intercom API
#' @param app_id App ID
#' @param key API Key
#' @export
get_intercom_data <- function(app_id, key, endpoint, paginate = NULL){
  url <- paste0("https://api.intercom.io/", endpoint)
  data_list <- list()
  page <- 1
  while(TRUE){
    res <- httr::GET(url, httr::authenticate(app_id, key))
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
