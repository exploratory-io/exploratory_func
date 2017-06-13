#' API to get profile for current oauth token
#' @export
getGoogleProfile <- function(tokenFileId = ""){
  if(!requireNamespace("RGoogleAnalytics")){stop("package RGoogleAnalytics must be installed.")}
  try({
    token <- getGoogleTokenForAnalytics(tokenFileId);
    RGoogleAnalytics::GetProfiles(token);
  })
}

#' @export
getGoogleAnalytics <- function(tableId, lastNDays, dimensions, metrics, tokenFileId = NULL, paginate_query=FALSE, segments = NULL){
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
  start_date <- as.character(lubridate::today() - lubridate::days(lastNDays))
  #end_date <- as.character(lubridate::today() - lubridate::days(1))
  end_date <- as.character(lubridate::today())
  query.list <- RGoogleAnalytics::Init(start.date = start_date,
                                       end.date = end_date,
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
