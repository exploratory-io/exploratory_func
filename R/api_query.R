#' Formats the values of an API query parameter list so that they survive httr's
#' string conversion.
#'
#' httr composes a query string with as.character() on each value. Since R 4.3,
#' as.character() on a double no longer respects options(scipen): it picks whichever
#' representation is shorter, so a value with few significant digits silently turns into
#' scientific notation.
#'
#'   as.character(1782000000)  # "1.782e+09"
#'   as.character(1781999999)  # "1781999999"
#'
#' APIs that expect an integer reject the former -- Stripe answers
#' "Invalid integer: 1.782e+09" for created[gte]. Because whether a value is "round"
#' enough to flip depends on the value itself, this fails intermittently (for unixtimes
#' derived from today(), on some days only), which makes it look like a flaky API.
#'
#' Every httr query list in this package goes through this function; the test
#' "every httr query argument is wrapped in to_api_query()" enforces that.
#'
#' Non-numeric values, and numeric vectors holding NA/NaN/Inf, are passed through
#' unchanged so httr keeps handling them exactly as before.
#'
#' @param query A named list of query parameters, or NULL.
#' @return The same list with finite numeric values formatted in fixed notation.
to_api_query <- function(query) {
  if (is.null(query)) {
    return(query)
  }
  lapply(query, function(value) {
    if (is.numeric(value) && length(value) > 0 && all(is.finite(value))) {
      format(value, scientific = FALSE, trim = TRUE, digits = 15)
    } else {
      value
    }
  })
}
