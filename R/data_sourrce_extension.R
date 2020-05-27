#' Wrapper API for riem_measure data soruces
#' @export
get_riem_measures <- function(station = "SFO", date_start = "2020-01-01", date_end = NULL){
  loadNamespace("riem")

  if(is.null(date_end)){
    date_end <- as.character(Sys.Date())
  }
  riem::riem_measures(station = station, date_start = date_start, date_end = date_end)
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
