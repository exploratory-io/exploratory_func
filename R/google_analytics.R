#' API to get profile for current oauth token
#' @export
getGoogleProfile <- function(tokenFileId = ""){
  if(!requireNamespace("googleAnalyticsR")){stop("package googleAnalyticsR must be installed.")}
  if(!requireNamespace("googleAuthR")){stop("package googleAuthR must be installed.")}

  token <- getGoogleTokenForAnalytics(tokenFileId);
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)
  # Get V3 account list.
  df <- googleAnalyticsR::ga_account_list()
  if (nrow(df) > 0) { # if there are V3 accounts, only select below columns.
    df <- df %>% dplyr::select(accountId, accountName, viewId, viewName, webPropertyId, webPropertyName)
  } else {
    # It means there are no v3 accounts, so create an empty data frame without columns and bind this to a v4 account data frame.
    df <- tibble::tibble()
  }
  # get V4 Account
  v4df <- data.frame()
  tryCatch({
    # googleAnalyticsR::ga_account_list("ga4") throws an error if the google account only has access to V3 accounts so surround it with tryCatch
    v4df <- googleAnalyticsR::ga_account_list("ga4")
  }, error = function(err) {
    # if we detect "API Data failed to parse" message, we can ignore it.
    if (stringr::str_detect(err$message, 'API Data failed to parse')){
      # do nothing.
    } else {
      stop(err)
    }
  })
  if (nrow(v4df) > 0) {
    v4df <- v4df %>% dplyr::rename(accountName = account_name, webPropertyId = propertyId, webPropertyName = property_name)
    df <- df %>% dplyr::bind_rows(v4df)
  }

  df
}
#' Helper API to get properites from response.
parse_webproperty_list <- function(x) {
  x$properties
}
#' API to get V4 property
#' ref: https://developers.google.com/analytics/devguides/config/admin/v1/rest/v1alpha/properties/list
#' @param accountId - account id.
#'
getGoogleAnalyticsV4Property <- function(accountId){
  accountId <- as.character(accountId)
  filterStr <- stringr::str_c("parent:accounts/", accountId)
  url <- "https://analyticsadmin.googleapis.com/v1alpha/properties"
  web_prop <- googleAuthR:::gar_api_generator(url,
                                              "GET",
                                              pars_args = list(
                                                filter = filterStr
                                              ),
                                              data_parse_function = parse_webproperty_list)

  web_prop()
}

getGoogleAnalyticsSegmentList <- function(){
  if(!requireNamespace("googleAnalyticsR")){stop("package googleAnalyticsR must be installed.")}
  if(!requireNamespace("googleAuthR")){stop("package googleAuthR must be installed.")}

  token <- getGoogleTokenForAnalytics(tokenFileId);
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)
  googleAnalyticsR::ga_segment_list()
}

#' API to get time zone information for the property
#'
#' @param accountId - account id.
#' @param webPropertyId - property id
#' @param viewId - for V3 only.
#'
getGoogleAnalyticsTimeZoneInfo <- function(accountId, webPropertyId, viewId = ""){
  token <- getGoogleTokenForAnalytics();
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)
  if (viewId == "") { # it means v4
    res <- exploratory:::getGoogleAnalyticsV4Property(accountId)
    df <- data.frame(res)
    # Make sure to filter the result by webPropertyId. (NOTE: name column contains property id as properties/123345 style.)
    df <- df %>% dplyr::filter(stringr::str_detect(name, webPropertyId))
    # for V4, timezone is stored in timeZone
    df$timeZone
  } else {
    res <- googleAnalyticsR::ga_view(accountId, webPropertyId, viewId)
    df <- data.frame(res)
    # Timezone info is set for COLLABORATE,EDIT, MANAGE_USERS, and READ_AND_ANALYZE but we only need timezone for READ_AND_ANALYZE.
    df <- df %>% dplyr::filter(effective == "READ_AND_ANALYZE")
    # for V3, timezone is stored in timezone
    df$timezone
  }
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
#' @param tzonForDisplay - timezone for displaying POSIXct column (with_tz)
#' @param samplingLevel - Sampling Level (for V3)
getGoogleAnalytics <- function(tableId, lastNDays = 30, dimensions, metrics, tokenFileId = NULL,
                               paginate_query=FALSE, segments = NULL, dateRangeType = "lastNDays",
                               lastN = NULL, startDate = NULL, endDate = NULL, tzone = NULL, tzoneForDisplay = NULL, isV4 = FALSE, samplingLevel = "DEFAULT", ...){
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
  googleAuthR::gar_auth(token = token, skip_fetch = TRUE)
  # dimension/metrics are passed as ga:country, ga:dateHour so we want to convert it as c("country", "dateHour")
  metrics <- unlist(strsplit(stringr::str_replace_all(metrics, "ga:", ""), split = ","))
  dimensions = unlist(strsplit(stringr::str_replace_all(dimensions, "ga:", ""), split = ","))
  if (isV4) {
    # ref: https://code.markedmondson.me/googleAnalyticsR/articles/reporting-ga4.html
    ga.data <- googleAnalyticsR::ga_data(
      tableId,
      date_range = c(startDate, endDate),
      metrics = metrics,
      dimensions = dimensions,
      limit = -1
    )
  } else {
    ga.data <- googleAnalyticsR::google_analytics_3(id = tableId,
                                                   start = startDate,
                                                   end = endDate,
                                                   metrics = metrics,
                                                   dimensions = dimensions,
                                                   segment = segments,
                                                   max_results = 99999999,
                                                   samplingLevel=samplingLevel)
  }

  if("date" %in% colnames(ga.data)){
    # modify date column to Date object from integer like 20140101
    ga.data <- ga.data %>% dplyr::mutate(date = lubridate::ymd(date))
  }

  if("dateHour" %in% colnames(ga.data)){
    # modify date column to POSIXct object from integer like 2014010101
    ga.data <- ga.data %>% dplyr::mutate(dateHour = lubridate::ymd_h(dateHour))
  }


  if("dateHourMinute" %in% colnames(ga.data)){
    # modify date column to POSIXct object from integer like 202001210000
    ga.data <- ga.data %>% dplyr::mutate(dateHourMinute = lubridate::ymd_hms(dateHourMinute, truncated = 1))
  }

  if("firstSessionDate" %in% colnames(ga.data)){
    # modify date column to Date object from integer like 20140101
    ga.data <- ga.data %>% dplyr::mutate(firstSessionDate = lubridate::ymd(firstSessionDate))
  }

  if("sessionCount" %in% colnames(ga.data)){
    # sessionCount is sometimes returned as character and numeric other times.
    # let's always cast it to numeric
    ga.data <- ga.data %>% dplyr::mutate(sessionCount = as.numeric(sessionCount))
  }

  if("daysSinceLastSession" %in% colnames(ga.data)){
    # sessionCount is sometimes returned as character and numeric other times.
    # let's always cast it to numeric
    ga.data <- ga.data %>% dplyr::mutate( daysSinceLastSession = as.numeric(daysSinceLastSession) )
  }
  if(!is.null(tzone)) { # if timezone is specified, apply the timezeon to POSIXct columns
    ga.data <- ga.data %>% dplyr::mutate(across(where(lubridate::is.POSIXct), ~ lubridate::force_tz(.x, tzone=tzone)))
  }
  if (!is.null(tzoneForDisplay)) {# if timezone for display is specified, convert the timezeon with with_tz
    ga.data <- ga.data %>% dplyr::mutate(across(where(lubridate::is.POSIXct), ~ lubridate::with_tz(.x, tzone=tzoneForDisplay)))
  }

  ga.data
}
