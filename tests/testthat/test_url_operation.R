context("test url tools wrappers")












test_that("url encode", {
  url <- c(
    " ", NA
  )
  ret <- url_encode(url)
  expect_equal(ret, c("%20", NA))
})

test_that("url decode", {
  url <- c(
    "%20%e3%80%80%e6%9e%97", NA
  )
  # expect_equal on windows with decoded string does not work well.
  # encoding it again for result comparison.
  ret <- url_encode(url_decode(url))
  expect_equal(ret, c("%20%e3%80%80%e6%9e%97", NA))
})

test_that("url param removal", {
  ret <- param_remove(c("http://me.com/test?testp=1&testp2=xxx&param1=1&param2=2",NA),keys = c("testp","param1","param2"))
  expect_equal(ret, c("http://me.com/test?testp2=xxx", NA))
})
