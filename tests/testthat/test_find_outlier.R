context("test find_outlier")

test_that("find_outlier", {
  data <- data.frame(
    value <- c(3, runif(10), -4)
  )

  ret <- data %>%
    dplyr::mutate(is_outlier = find_outlier(value))

  expect_equal(ret$is_outlier, c(TRUE, rep(FALSE, 10), TRUE))

})
