#' API to get profile for current oauth token
#' @export
getGoogleProfile <- function(tokenFileId = ""){
  if(!requireNamespace("googleAnalyticsR")){stop("package googleAnalyticsR must be installed.")}
  if(!requireNamespace("googleAuthR")){stop("package googleAuthR must be installed.")}

  token <- getGoogleTokenForAnalytics(tokenFileId);
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)
  df <- googleAnalyticsR::ga_account_list()
  if (nrow(df) > 0) {
    accountId <- df$accountId
    webPropertyId <- df$webPropertyId
    viewId <- df$viewId
    argList <- list(accountId, webPropertyId, viewId)
    # Get timezone for each viewId
    newdf <- purrr::pmap_dfr(argList, function(accountId, webPropertyId, viewId){
      data.frame(googleAnalyticsR::ga_view(accountId, webPropertyId, viewId)) %>% dplyr::filter(effective == "READ_AND_ANALYZE") %>% dplyr::select(id, accountId, webPropertyId, timezone)
    })
    # Join the timezone column to the original data frame.
    df <- df %>% dplyr::left_join(newdf, by = c("accountId" = "accountId", "webPropertyId" = "webPropertyId", "viewId" = "id"))
  }
  df
}

getGoogleAnayticsSegmentList <- function(){
  if(!requireNamespace("googleAnalyticsR")){stop("package googleAnalyticsR must be installed.")}
  if(!requireNamespace("googleAuthR")){stop("package googleAuthR must be installed.")}

  token <- getGoogleTokenForAnalytics(tokenFileId);
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)
  googleAnalyticsR::ga_segment_list()
}

#' @export
#' @param tableId - GA's table id (a.k.a viewID).
#' @param lastNDays - Deprecated. Use dateRangeType and lastN arguments instead.
#' @param dimensions - GA's dimensions
#' @param metrics - GA's metrics.
#' @param tokenFileId - Optional. for GA data source created with old Exploratory Desktop.
#' @param paginate_query - for pagination
#' @param segments - GA's segments
#' @param dateRangeType - Either "lastNDays", "lastNWeeks", "lastNMonths", "lastNYears", or "since"
#' @param lastN - Corresponding numeric value for the lastNxx duration.
#' @param startDate - When dateRangeType is "since", specify start date
#' @param endDate - When dateRangeType is "since", you can provide end date. "today" will be used if it's not provided.
#' @param tzone - timezone applied to POSIXct column (force_tz)
#' @param tzonForDisplay - timezone for displaing POSIXct column (with_tz)
getGoogleAnalytics <- function(tableId, lastNDays = 30, dimensions, metrics, tokenFileId = NULL,
                               paginate_query=FALSE, segments = NULL, dateRangeType = "lastNDays",
                               lastN = NULL, startDate = NULL, endDate = NULL, tzone = NULL, tzoneForDisplay = NULL,...){
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

  # When calculating startDate, to avoid the result becomes NA like below two cases,
  # use %m-% instead of - for years and months.
  #
  #> lubridate::today() - months(lastN) # if today is 2020-03-30 and lastN is 1, it returns NA
  # [1] NA
  # > lubridate::today() - lubridate::years(lastN) # if today is 2020-02-29 it returns NA
  # [1] NA

  if(dateRangeType == "lastNDays") {
    if(is.null(lastN)) {
      # For backward compatibility for Exploratory Desktop older than version 5.4.1
      # Previously it only supported last N Days.
      # so set lastN with lastNDays value.
      lastN <- lastNDays
    }
    startDate <- as.character(lubridate::today() - lubridate::days(lastN - 1));
  } else if (dateRangeType == 'lastNDaysExcludeToday'){
    startDate <- as.character(lubridate::today() - lubridate::days(lastN));
    endDate <- as.character(lubridate::today() - lubridate::days(1));
  } else if (dateRangeType == "lastNWeeks") {
    startDate <- as.character(lubridate::today() - lubridate::weeks(lastN));
  } else if (dateRangeType == "lastNWeeksExcludeThisWeek") {
    startDate <- as.character(floor_date(today(), unit = "week") - lubridate::weeks(lastN));
    endDate <- as.character(floor_date(today(), unit = "week") - days(1));
  } else if (dateRangeType == 'lastNWeeksExcludeToday') {
    startDate <- as.character(lubridate::today() - lubridate::weeks(lastN));
    endDate <- as.character(lubridate::today() - lubridate::days(1));
  } else if (dateRangeType == "lastNMonths") {
    startDate <- as.character(lubridate::today() %m-% months(lastN)); # use base months function since lubridate does not have it.
  } else if (dateRangeType == "lastNMonthsExcludeThisMonth") {
    startDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "month") %m-% months(lastN)); # use base months function since lubridate does not have it.
    endDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "month") - days(1))
  } else if (dateRangeType == "lastNMonthsExcludeThisWeek"){
    startDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "week") %m-% months(lastN)); # use base months function since lubridate does not have it.
    endDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "week") - days(1))
  } else if (dateRangeType == "lastNMonthsExcludeToday") {
    startDate <- as.character(lubridate::today() %m-% months(lastN)); # use base months function since lubridate does not have it.
    endDate <- as.character(lubridate::today() - lubridate::days(1))
  } else if (dateRangeType == "lastNQuarters") {
    startDate <- as.character(lubridate::today() %m-% months(lastN*3));
  } else if (dateRangeType == "lastNQuartersExcludeThisQuarter") {
    startDate <- as.character(lubridate::floor_date(today(), unit = "quarter") %m-% months(lastN*3));
    endDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "quarter") - days(1));
  } else if (dateRangeType == "lastNQuartersExcludeThisMonth"){
    startDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "month") %m-% months(lastN*3));
    endDate <- as.character(lubridate::floor_date(today(), unit = "month") - days(1));
  } else if (dateRangeType == "lastNQuartersExcludeThisWeek") {
    startDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "week") %m-% months(lastN*3));
    endDate <-as.character(lubridate::floor_date(lubridate::today(), unit = "weeek") - days(1));
  } else if (dateRangeType == "lastNQuartersExcludeToday") {
    startDate <- as.character(lubridate::today() %m-% months(lastN*3));
    endDate <-as.character(lubridate::today() - days(1));
  } else if (dateRangeType == "lastNYears") {
    startDate <- as.character(lubridate::today() %m-% lubridate::years(lastN));
  } else if (dateRangeType == "lastNYearsExcludeThisYear") {
    startDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "year") %m-% lubridate::years(lastN));
    endDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "year") - days(1));
  } else if (dateRangeType == "lastNYearsExcludeThisQuarter") {
    startDate <- as.character(lubridate::floor_date(today(), unit = "quarter") %m-% lubridate::years(lastN));
    endDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "quarter") - days(1));
  } else if (dateRangeType == "lastNYearsExcludeThisMonth"){
    startDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "month") %m-% lubridate::years(lastN));
    endDate <- as.character(lubridate::floor_date(today(), unit = "month") - days(1));
  } else if (dateRangeType == "lastNYearsExcludeThisWeek") {
    startDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "week") %m-% lubridate::years(lastN));
    endDate <- as.character(lubridate::floor_date(lubridate::today(), unit = "weeek") - days(1));
  } else if (dateRangeType == "lastNYearsExcludeToday") {
    startDate <- as.character(lubridate::today() %m-% lubridate::years(lastN));
    endDate <- as.character(lubridate::today() - days(1));
  } else if (dateRangeType == "yesterday") {
    startDate = as.character(lubridate::today() - 1);
    endDate = startDate;
  } else if (dateRangeType == "today") {
    startDate = as.character(lubridate::today());
    endDate = startDate;
  } else if (dateRangeType == "thisWeekToYesterday") {
    startDate = as.character(lubridate::floor_date(lubridate::today(), "week"));
    endDate = as.character(lubridate::today() - 1);
  } else if (dateRangeType == "thisMonthToYesterday") {
    startDate = as.character(lubridate::floor_date(lubridate::today(), "month"));
    endDate = as.character(lubridate::today() - 1);
  } else if (dateRangeType == "thisYearToYesterday") {
    startDate = as.character(lubridate::floor_date(lubridate::today(), "year"));
    endDate = as.character(lubridate::today() - 1);
  } else if (dateRangeType == "lastWeekToYesterday") {
    startDate = as.character(lubridate::floor_date(lubridate::today(),"week") - lubridate::weeks(1));
    endDate = as.character(lubridate::today() - 1);
  } else if (dateRangeType == "lastMonthToYesterday") {
    startDate = as.character(lubridate::floor_date(lubridate::today(), "month") %m-% months(1));
    endDate = as.character(lubridate::today() - 1);
  } else if (dateRangeType == "lastYearToYesterday") {
    startDate = as.character(lubridate::floor_date(lubridate::today(), "year") %m-% lubridate::years(1));
    endDate = as.character(lubridate::today() - 1);
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

  if("dateHourMinute" %in% colnames(ga.data)){
    # modify date column to POSIXct object from integer like 202001210000
    loadNamespace("lubridate")
    ga.data <- ga.data %>% dplyr::mutate( dateHourMinute = lubridate::ymd_hms(dateHourMinute, truncated = 1) )
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
  if(!is.null(tzone)) { # if timezone is specified, apply the timezeon to POSIXct columns
    ga.data <- ga.data %>% dplyr::mutate_if(lubridate::is.POSIXct, funs(lubridate::force_tz(., tzone=tzone)))
  }
  if (!is.null(tzoneForDisplay)) {# if timezone for display is specified, convert the timezeon with with_tz
    ga.data <- ga.data %>% dplyr::mutate_if(lubridate::is.POSIXct, funs(lubridate::with_tz(., tzone=tzoneForDisplay)))
  }

  ga.data
}
