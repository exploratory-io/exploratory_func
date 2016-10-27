#' Access google trends
#' @param user - User id.
#' @param password - Password for the account.
#' @param query - Vector of queries.
#' @param type - Output type. Can be "top_regions", "top_cities" or "trends".
#' @param last - From when the data should be retreived. Currently supported parameters are "1h", "4h", "1d", "7d", "5y", "30d", "90d", "1y" and "all" (from 2004)
#' @param geo - Region codes. It's listed in countries data set in gtrendsR.
#' @export
getGoogleTrends <- function(user, password, query = "", type = "trend", last = "5y", geo = ""){
  loadNamespace("gtrendsR")
  loadNamespace("tidyr")

  password <- saveOrReadPassword("googletrends", user, password)

  # this doesn't return error even if login fails, so capture the message
  # if it's successful, it's character(0)
  message <- capture.output(gtrendsR::gconnect(user, password))
  if(!identical(message, character(0))){
    if(message == "Google login failed! Check your login information.NULL"){
      stop("Google login failed. Please verify username and password. Note that two-factor authentication is not supported.")
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

  # gtrendsR must be loaded because data(countries) is called in the function
  loaded <- "gtrendsR" %in% names(sessionInfo()$otherPkgs)
  require("gtrendsR")
  ret <- gtrendsR::gtrends(query = query, res = res, start_date = start_date, end_date = end_date, geo = geo)
  if(!loaded){
    # detach it if it was not originally loaded
    detach("package:gtrendsR", unload=TRUE)
  }

  keys <- names(ret)

  if(type == "top_regions"){
    # If it's top regions, the data frames are duplicated, so the first one is enough
    key <- keys[startsWith(keys, "Top.regions") | startsWith(keys, "Top.subregions")][[1]]
    # gather columns except for the first column (names of region) to make it easy to visualise
    tidyr::gather_(ret[[key]], "keyword", "trend", colnames(ret[[key]])[2:ncol(ret[[key]])], na.rm = TRUE)
  } else if (type == "top_cities"){
    key <- keys[startsWith(keys, "Top.cities")]
    bind_data <- dplyr::bind_rows(ret[key])
    # gather columns except for the first column (names of cities) to make it easy to visualise
    tidyr::gather_(bind_data, "keyword", "trend", colnames(bind_data)[2:ncol(bind_data)], na.rm = TRUE)
  } else if (type == "trend"){
    trend <- ret[[type]]
    # use query and geo arguments as column names to prevent garbled characters
    # spaces should be changed into . by gtrendsR, so doing the same
    queries <- stringr::str_replace(stringr::str_trim(query), " +", ".")
    cols <- if(!is.null(geo)){
      # put geo into column names and separate it later
      # spaces in queries are converted to . by gtrends, so space can be used safely
      paste(queries, geo, sep = " ")
    } else {
      queries
    }
    colnames(trend)[(ncol(trend)-length(cols)+1):ncol(trend)] <- cols
    # gather columns except for time columns to make it easy to visualise
    ret <- tidyr::gather_(trend, "keyword", "trend", cols, na.rm = TRUE)
    if(!is.null(geo)){
      # separates geo and keywords that were column names
      ret <- tidyr::separate_(ret, "keyword", c("keyword", "geo"), sep = " ")
    }
    ret
  } else {
    stop("Currently, type must be top_regions, top_cities or trends")
  }
}
