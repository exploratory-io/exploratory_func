#' Get data from stripe API
#' @export
get_stripe_data <- function(
  endpoint
){
  # these were once arguments
  limit = 100
  paginate = NULL

  token_info <- exploratory::getTokenInfo("stripe")
  access_token <- if(!is.null(token_info) && !is.null(token_info$access_token)){
    token_info$access_token
  } else {
    stop("No access token is set.")
  }

  token <- exploratory::HttrOAuthToken2.0$new(
    authorize = "https://connect.stripe.com/oauth/authorize",
    access = "https://connect.stripe.com/oauth/token",
    revoke = "https://connect.stripe.com/oauth/deauthorize",
    appname = "stripe",
    credentials = list(
      access_token = access_token
    )
  )
  url <- paste0("https://api.stripe.com/v1/", endpoint)

  get_data <- function(query){
    res <- httr::GET(url,
                     query = query,
                     token
    )
    if(httr::status_code(res) != 200){
      stop(paste0("Error Response: ", httr::content(res, as = "text")))
    }
    from_json <- res %>% httr::content(as = "text") %>% jsonlite::fromJSON(flatten = TRUE)
    if(length(from_json$data) == 0){
      stop("No data found.")
    }
    from_json$data
  }

  query <- list(limit = limit)

  ret <- list()
  last_id <- NULL
  i <- 0
  while(TRUE){
    if(!is.null(last_id)){
      # search data after the last id in fetched data
      query$starting_after <- last_id
    }
    data <- tryCatch({
      get_data(query)
    }, error = function(e){
      if(stringr::str_detect(e$message, "^Error Response:")){
        stop(e)
      }
      NULL
    })
    if(is.null(data)){
      break()
    } else {
      last_id <- tail(data$id, 1)
      # this is to avoid duplicated rownames error when binding
      rownames(data) <- c()
      row.names(data) <- c()
      ret <- append(ret, list(data))
    }
    i <- i + 1
    if(!is.null(paginate) && i >= paginate){
      break()
    }
    Sys.sleep(0.2)
  }
  ret <- do.call(dplyr::bind_rows, ret)

  # convert unixtime integer column to datetime
  for(column in colnames(ret)){
    if(
      (
        grepl("date$", column) ||
        grepl("period_end$", column) ||
        grepl("period_start$", column) ||
        grepl("at$", column) ||
        grepl("created$", column) ||
        grepl("start$", column) ||
        grepl("end$", column) ||
        grepl("payment_attempt$", column)
      )
      &&
      is.integer(ret[[column]])
    ){
      ret[[column]] <- exploratory::unixtime_to_datetime(ret[[column]])
    }
  }

  ret
}
