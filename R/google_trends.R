#' access google trends
#' @export
getGoogleTrends <- function(user, password, query = "", type = "trend", last = "5y", geo = ""){
  loadNamespace("gtrendsR")
  # data countries must be imported because it's used in gtrendsR::gtrends
  data("countries", package = "gtrendsR", envir = environment())
  # this doesn't return error even if login fails, so capture the message
  # if it's successful, it's character(0)
  message <- capture.output(gtrendsR::gconnect(user, password))
  if(!identical(message, character(0))){
    if(message == "Google login failed! Check your login information.NULL"){
      stop("Google login failed. Check Google Account and Password")
    }
    stop(message)
  }
  if(last == "all"){
    res <- NULL
    start_date <- as.Date("2004-01-01")
    end_date <- as.Date(Sys.time())
  } else if(!last %in% c("1h", "4h", "1d", "7d", "all")){
    # c("1h", "4h", "1d", "7d") are supported by default
    loadNamespace("lubridate")
    now <- Sys.time()
    start_date <- switch (last,
                          `5y` = {
                            lubridate::year(now) <- lubridate::year(now) - 5
                            as.Date(now)
                          },
                          `30d` = {
                            lubridate::day(now) <- lubridate::day(now) - 30
                            as.Date(now)
                          },
                          `90d` = {
                            lubridate::day(now) <- lubridate::day(now) - 90
                            as.Date(now)
                          },
                          `1y` = {
                            lubridate::year(now) <- lubridate::year(now) - 1
                            as.Date(now)
                          }
    )
    end_date <- as.Date(Sys.time())
    res <- NULL
  } else {
    res <- last
    # these values won't be evaluated but put to avoid undefined parameters
    start_date <- as.Date("2004-01-01")
    end_date <- as.Date(Sys.time())
  }
  if( geo == ""){
    geo <- NULL
  }

  ret <- gtrendsR::gtrends(query = query, res = res, start_date = start_date, end_date = end_date, geo = geo)
  keys <- names(ret)

  if(type == "top_regions"){
    key <- keys[startsWith(keys, "Top.regions") | startsWith(keys, "Top.subregions")]
    dplyr::bind_rows(ret[key])
  } else if (type == "top_cities"){
    key <- keys[startsWith(keys, "Top.cities")]
    dplyr::bind_rows(ret[key])
  } else if (type == "trend"){
    trend <- ret[[type]]
    # use query and geo arguments as column names to prevent garbled characters
    queries <- stringr::str_replace(stringr::str_trim(query), " ", "_")
    cols <- paste(queries, rep(geo, length(queries)), sep = ".")
    colnames(trend)[(ncol(trend)-length(cols)+1):ncol(trend)] <- cols
    trend
  } else {
    stop("Currently, type must be top_regions, top_cities or trends")
  }
}
