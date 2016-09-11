context("test url tools wrappers")

test_that("suffix", {
  url <- c(
    "https://twitter.com/reddit?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor",
    "http://www.computerhope.com/jargon/num/domains.htm"
    )
  ret <- get_url_suffix(url)
  expect_equal(ret, c("com", "com"))
})

test_that("subdomain", {
  url <- c(
    "https://twitter.com/reddit?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor",
    "http://www.computerhope.com/jargon/num/domains.htm"
  )
  ret <- get_url_subdomain(url)
  expect_equal(ret, c(NA, "www"))
})

test_that("param", {
  url <- c(
    "https://twitter.com/reddit?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor",
    "http://www.computerhope.com/jargon/num/domains.htm"
  )
  ret <- get_url_param(url, "ref_src")
  expect_equal(ret, c("twsrc^google|twcamp^serp|twgr^author", NA))
})
