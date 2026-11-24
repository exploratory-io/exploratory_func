context("HttrOAuthToken cache path (tam#39034)")

# httr's Token$initialize normally normalizes cache_path via use_cache(); the wrapper skips it,
# leaving the R6 default FALSE, which made cache() fail with "invalid 'file' argument".
test_that("HttrOAuthToken2.0 cache() does not touch the file system", {
  token <- exploratory:::HttrOAuthToken2.0$new(
    authorize = "https://example.com/auth", access = "https://example.com/token",
    appname = "google",
    credentials = list(access_token = "a", refresh_token = "r", token_type = "Bearer"))
  expect_null(token$cache_path)
  expect_error(token$cache(), NA)
})

test_that("HttrOAuthToken1.0 has no cache path", {
  token <- exploratory:::HttrOAuthToken1.0$new(
    request = "https://example.com/request", authorize = "https://example.com/auth",
    access = "https://example.com/token", appname = "twitter",
    credentials = list(oauth_token = "a", oauth_token_secret = "s"))
  expect_null(token$cache_path)
  expect_error(token$cache(), NA)
})
