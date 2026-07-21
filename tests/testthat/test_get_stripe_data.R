context("test get_stripe_data")

test_that("as_integer_param never emits scientific notation", {
  # Since R 4.3, as.character() on a double ignores options(scipen) and picks the
  # shortest representation, so a "round" unixtime becomes scientific notation.
  # Stripe (and other APIs taking integer query params) rejects that with
  # "Invalid integer: 1.782e+09". The daily test only failed on the days whose
  # date_since happened to land on such a value.
  expect_equal(as.character(1782000000), "1.782e+09") # guards the premise of this test
  expect_equal(as_integer_param(1782000000), "1782000000")
  expect_equal(as_integer_param(1781999999), "1781999999")
  expect_equal(as_integer_param(1e9), "1000000000")

  # Every day within the next 10 years must format in fixed notation, for each
  # date_type the UI offers.
  days <- seq(lubridate::today(), lubridate::today() + lubridate::days(3653), by = "day")
  unixtimes <- as.numeric(as.POSIXct(days))
  formatted <- as_integer_param(unixtimes)
  expect_false(any(grepl("e", formatted, fixed = TRUE)))
  expect_true(all(grepl("^[0-9]+$", formatted)))
})
