context("test detect_outlier")

test_that("detect_outlier", {
  set.seed(1)
  data <- data.frame(
    value = c(c(3, runif(10), -4), (c(3, runif(10), -4) + 3)),
    group = rep(c(1, 2), each = 12)
  )

  ret <- data %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(is_outlier = detect_outlier(value))

  expect_equal(ret$is_outlier, factor(c(c("upper", rep("normal", 10), "lower"),c("upper", rep("normal", 10), "lower")),
                                      levels = c("lower", "normal", "upper"),
                                      ordered=TRUE))

})

test_that("detect_outlier with quantile", {
  set.seed(1)
  data <- data.frame(
    value = c(c(3, runif(10), -4), (c(3, runif(10), -4) + 3)),
    group = rep(c(1, 2), each = 12)
  )

  ret <- data %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(is_outlier = detect_outlier(value, type = "percentile", threshold = 0.95))

  expect_equal(ret$is_outlier, factor(c(c("upper", rep("normal", 10), "lower"),c("upper", rep("normal", 10), "lower")),
                                      levels = c("lower", "normal", "upper"),
                                      ordered=TRUE))

})

test_that("detect_outlier with quantile", {
  set.seed(1)
  data <- data.frame(
    value = c(c(3, runif(10), -4), (c(3, runif(10), -4) + 3)),
    group = rep(c(1, 2), each = 12)
  )

  ret <- data %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(is_outlier = detect_outlier(value, type = "percentile", threshold = 0.05))

  expect_equal(ret$is_outlier, factor(c(c("upper", rep("normal", 10), "lower"),c("upper", rep("normal", 10), "lower")),
                                      levels = c("lower", "normal", "upper"),
                                      ordered=TRUE))

})

test_that("detect_outlier with standard_deviation", {
  set.seed(1)
  data <- data.frame(
    value = c(c(3, runif(10), -4), (c(3, runif(10), -4) + 3)),
    group = rep(c(1, 2), each = 12)
  )

  ret <- data %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(is_outlier = detect_outlier(value, type = "standard_deviation", threshold = 1.63))
  expect_equal(ret$is_outlier, factor(c(c("upper", rep("normal", 10), "lower"),c("upper", rep("normal", 10), "lower")),
                                      levels = c("lower", "normal", "upper"),
                                      ordered=TRUE))
})
