context("test detect_outlier")

test_that("detect_outlier", {
  data <- data.frame(
    value = c(c(3, runif(10), -4), (c(3, runif(10), -4) + 3)),
    group = rep(c(1, 2), each = 12)
  )

  ret <- data %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(is_outlier = detect_outlier(value))

  expect_equal(ret$is_outlier, c(c(TRUE, rep(FALSE, 10), TRUE),c(TRUE, rep(FALSE, 10), TRUE)))

})
