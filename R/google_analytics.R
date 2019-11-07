#' API to get profile for current oauth token
#' @export
getGoogleProfile <- function(tokenFileId = ""){
  if(!requireNamespace("googleAnalyticsR")){stop("package googleAnalyticsR must be installed.")}
  if(!requireNamespace("googleAuthR")){stop("package googleAuthR must be installed.")}

  token <- getGoogleTokenForAnalytics(tokenFileId);
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)
  googleAnalyticsR::ga_account_list()
}

getGoogleAnayticsSegmentList <- function(){
  if(!requireNamespace("googleAnalyticsR")){stop("package googleAnalyticsR must be installed.")}
  if(!requireNamespace("googleAuthR")){stop("package googleAuthR must be installed.")}

  token <- getGoogleTokenForAnalytics(tokenFileId);
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)
  googleAnalyticsR::ga_segment_list()
}

#' @export
getGoogleAnalytics <- function(tableId, lastNDays = 30, dimensions, metrics, tokenFileId = NULL,
                               paginate_query=FALSE, segments = NULL, dateRangeType = "lastNDays",
                               lastN = NULL, startDate = NULL, endDate = NULL, ...){
  if(!requireNamespace("RGoogleAnalytics")){stop("package RGoogleAnalytics must be installed.")}
  loadNamespace("lubridate")
  # if segment is not null and empty string, pass it as NULL
  # NOTE: null can be passed for data frame created with old version so need to explicitly check it.
  # Also if we do not set !is.null(segments) before hand, if(segment == '') part fails
  # with Error in if (segments == “”) { : argument is of length zero
  if(!is.null(segments) && segments == ''){
    segments = NULL
  }
  token <- getGoogleTokenForAnalytics(tokenFileId)

  if(dateRangeType == "lastNDays") {
    if(is.null(lastN)) {
      lastN <- lastNDays
    }
    startDate <- as.character(lubridate::today() - (lastN - 1));
  } else if (dateRangeType == "lastNWeeks") {
    startDate <- as.character(lubridate::today() - lubridate::weeks(lastN));
  } else if (dateRangeType == "lastNMonths") {
    startDate <- as.character(lubridate::today() - months(lastN));
  } else if (dateRangeType == "lastNYears") {
    startDate <- as.character(lubridate::today() - lubridate::years(lastN));
  }

  if(is.null(startDate)) {
    startDate <- as.character(lubridate::today() - lubridate::days(lastNDays))
  }
  if(is.null(endDate)) {
    endDate <- as.character(lubridate::today())
  }

  query.list <- RGoogleAnalytics::Init(start.date = startDate,
                                       end.date = endDate,
                                       dimensions = dimensions,
                                       metrics = metrics,
                                       segments = segments,
                                       max.results = 10000,
                                       table.id = tableId)

  ga.query <- RGoogleAnalytics::QueryBuilder(query.list)
  ga.data <- RGoogleAnalytics::GetReportData(ga.query, token, paginate_query = paginate_query)

  if("date" %in% colnames(ga.data)){
    # modify date column to Date object from integer like 20140101
    loadNamespace("lubridate")
    ga.data <- ga.data %>% dplyr::mutate( date = lubridate::ymd(date) )
  }

  if("dateHour" %in% colnames(ga.data)){
    # modify date column to POSIXct object from integer like 2014010101
    loadNamespace("lubridate")
    ga.data <- ga.data %>% dplyr::mutate( dateHour = lubridate::ymd_h(dateHour) )
  }

  if("sessionCount" %in% colnames(ga.data)){
    # sessionCount is sometimes returned as character and numeric other times.
    # let's always cast it to numeric
    ga.data <- ga.data %>% dplyr::mutate( sessionCount = as.numeric(sessionCount) )
  }

  if("daysSinceLastSession" %in% colnames(ga.data)){
    # sessionCount is sometimes returned as character and numeric other times.
    # let's always cast it to numeric
    ga.data <- ga.data %>% dplyr::mutate( daysSinceLastSession = as.numeric(daysSinceLastSession) )
  }

  ga.data
}
