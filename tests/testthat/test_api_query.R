context("test to_api_query")

# Since R 4.3, as.character() on a double ignores options(scipen) and picks the shortest
# representation, so a "round" value silently becomes scientific notation. httr composes
# query strings with as.character(), so such a value reaches the API as "1.782e+09" and an
# API expecting an integer rejects it (Stripe: "Invalid integer: 1.782e+09"). Whether a value
# flips depends on the value itself, so this fails only on some days -- it looks like a flaky
# datasource until someone reads the response body.
test_that("to_api_query keeps numbers out of scientific notation", {
  # The premise. If a future R makes as.character() safe again, this is the test that says so.
  expect_equal(as.character(1782000000), "1.782e+09")

  q <- exploratory:::to_api_query(list(created = 1782000000, limit = 100))
  expect_equal(q$created, "1782000000")
  expect_equal(q$limit, "100")

  # Every day of the next 10 years, for every date_type the UI offers.
  days <- seq(lubridate::today(), lubridate::today() + lubridate::days(3653), by = "day")
  formatted <- unlist(exploratory:::to_api_query(list(t = as.numeric(as.POSIXct(days)))))
  expect_false(any(grepl("e", formatted, fixed = TRUE)))
  expect_true(all(grepl("^[0-9]+$", formatted)))
})

test_that("to_api_query leaves everything else untouched", {
  expect_null(exploratory:::to_api_query(NULL))
  expect_equal(exploratory:::to_api_query(list()), list())

  q <- exploratory:::to_api_query(list(
    s = "customers",
    b = TRUE,
    v = c(1, 2, 3),
    na = NA_real_,          # non-finite: passed through so httr behaves as before
    inf = Inf,
    frac = 0.123456789,     # decimals must not be rounded away
    neg = -1782000000
  ))
  expect_equal(q$s, "customers")
  expect_true(q$b)
  expect_equal(q$v, c("1", "2", "3"))
  expect_true(is.na(q$na))
  expect_equal(q$inf, Inf)
  expect_equal(q$frac, "0.123456789")
  expect_equal(q$neg, "-1782000000")
})

# The formatting helper only protects the call sites that actually use it. This walks the
# package sources and fails if any httr request passes a query that did not go through it --
# so a new datasource cannot reintroduce the Stripe failure.
test_that("every httr query argument is wrapped in to_api_query()", {
  # testthat runs with cwd = tests/testthat; allow a direct run from the repo root too.
  # An installed package has no R/*.R sources, so there the check simply skips.
  candidates <- file.path(c(file.path("..", ".."), "."), "R")
  r_dir <- Find(function(d) file.exists(file.path(d, "get_stripe_data.R")), candidates)
  skip_if(is.null(r_dir), "package sources are not available (installed package: no R/*.R)")

  is_http_call <- function(fn) {
    fn_text <- paste(deparse(fn), collapse = "")
    fn_text %in% c("httr::GET", "httr::POST", "httr::PUT", "httr::PATCH", "httr::DELETE", "httr::VERB")
  }
  is_wrapped <- function(arg) {
    is.call(arg) && identical(paste(deparse(arg[[1]]), collapse = ""), "to_api_query")
  }

  offenders <- character(0)
  for (file in list.files(r_dir, pattern = "\\.R$", full.names = TRUE)) {
    exprs <- tryCatch(parse(file), error = function(e) NULL)
    if (is.null(exprs)) next
    walk <- function(e) {
      if (!is.call(e)) return(invisible(NULL))
      if (is_http_call(e[[1]]) && "query" %in% names(e) && !is_wrapped(e[["query"]])) {
        offenders <<- c(offenders, paste0(
          basename(file), ": ", paste(deparse(e[["query"]]), collapse = " ")))
      }
      # Index rather than bind: an argument can be the empty symbol (df[, 1]), and binding
      # that to a variable raises "argument is missing". A nested function definition is
      # itself a call, so this covers function bodies too.
      parts <- as.list(e)
      for (idx in seq_along(parts)) {
        if (is.call(parts[[idx]])) walk(parts[[idx]])
      }
      invisible(NULL)
    }
    for (e in exprs) walk(e)
  }

  expect_equal(offenders, character(0),
    info = paste0("Wrap these httr query arguments in to_api_query() -- an unwrapped numeric ",
                  "reaches the API in scientific notation: ", paste(offenders, collapse = "; ")))
})
