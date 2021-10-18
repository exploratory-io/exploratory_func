#' Wrapper API for riem_measure data soruces
#' @export
get_riem_measures <- function(station = "SFO", date_start = "2020-01-01", date_end = NULL, full_columns = "Yes", tzone = ""){
  loadNamespace("riem")
  if(is.null(date_end)){
    date_end <- as.character(Sys.Date())
  }
  # Since we cannot use POSIXct for start_date and end_date
  # We give little bit of buffers for start and end date and use filter to get the exact date range.
  startDate <- ""
  endDate <- ""
  if (tzone != "") {
    startDate <- lubridate::ymd_hms(stringr::str_c(date_start, " 00:00:00"), tz = tzone)
    # Get data from a day before from start_date to workaround the timezone difference.
    date_start <- as.character(as.Date(startDate - lubridate::days(1)))
    endDate <- lubridate::ymd_hms(stringr::str_c(date_end, " 23:59:59"), tz = tzone)
    # Get data until the day after the end_date to workaround the timezone difference.
    date_end <- as.character(as.Date(endDate + lubridate::days(1)))
  } else {
    # default is UTC
    startDate <- lubridate::ymd_hms(stringr::str_c(date_start, " 00:00:00"), tz = "UTC")
    endDate <- lubridate::ymd_hms(stringr::str_c(date_end, " 23:59:59"), tz = "UTC")
  }

  df <- riem_measures(station = station, date_start = date_start, date_end = date_end)
  if (tzone != "") {# if timezone for display is specified, convert the timezone with with_tz
    df <- df %>% dplyr::mutate_if(lubridate::is.POSIXct, funs(lubridate::with_tz(., tzone=tzone)))
  }
  # make sure to filter the data to the exact date range
  df <- df %>% dplyr::filter(valid >= startDate & valid <= endDate)

  if(full_columns == "Yes") {
    df
  } else {
    df %>% mutate(tmpc = (tmpf - 32) * 5/9, dwpc = (dwpf - 32) * 5/9) %>%
      select(station, valid, tmpf, tmpc, dwpf, dwpc, relh, drct, sknt, p01i, alti, mslp, vsby, gust, lon, lat) %>%
      rename(
        Station = station,
        Time = valid,
        Temperature_F = tmpf,
        Temperature_C = tmpc,
        Dew_Point_Temp_F = dwpf,
        Dew_Point_Temp_C = dwpc,
        Humidity = relh,
        Wind_Direction = drct,
        Wind_Speed_Knot = sknt,
        Precipitation_Inch = p01i,
        Pressure_Altimeter_Inch = alti,
        Sea_Level_Pressure_Millibar = mslp,
        Visibility_Mile = vsby,
        Wind_Gust_Knot = gust,
        Longitude = lon,
        Latitude = lat
      )
  }
}

#' Wrapper API for riem_stations
#' @export
riem_stations_exp <- function(network = NULL) {
  df <- riem::riem_stations(network)
  df <- df %>% dplyr::mutate(name = dplyr::case_when(
    id == "RJOA" ~ "Hiroshima-shi",
    id == "RJOO" ~ "Osaka (Itami)",
    id == "RJOY" ~ "Osaka (Yao)",
    id == "RJBB" ~ "Kansai International",
    id == "RJCO" ~ "Sapporo (Okadama)",
    id == "RJCC" ~ "Sapporo (New Chitose)",
    id == "RJAA" ~ "Tokyo (Narita)",
    id == "RJTT" ~ "Tokyo (Haneda)",
    id == "RJTF" ~ "Tokyo (Chofu)",
    TRUE ~ name
  )
  )
  df
}

#' Wrapper API for tidyquant data source
#' @export
execute_tidyquant <- function(stocks = NULL, from = NULL, to = NULL) {
  loadNamespace("tidyquant")
  loadNamespace("stringr")
  loadNamespace("lubridate")

  if(is.null(from)){
    # 1 year ago
    from <- lubridate::today() - lubridate::years(1)
  }
  if(is.null(to)){
    to <- lubridate::today()
  }

  if (is.null(stocks) || stocks == "") {
    # If no stocks identified, return all SP500
    ret <- tidyquant::tq_index("SP500") %>%
      select(1) %>%
      tidyquant::tq_get(get = "stock.prices", from = from, to = to)
  } else {
    # If stocks listed, parse and return
    ret <- stringr::str_trim(stocks) %>%
      unlist() %>%
      tidyquant::tq_get(get = "stock.prices", from = from, to = to)
  }
  ret
}
