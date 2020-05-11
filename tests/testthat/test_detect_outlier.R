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

  expect_equal(ret$is_outlier, factor(c(c("Upper", rep("Normal", 10), "Lower"),c("Upper", rep("Normal", 10), "Lower")),
                                      levels = c("Lower", "Normal", "Upper"),
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

  expect_equal(ret$is_outlier, factor(c(c("Upper", rep("Normal", 10), "Lower"),c("Upper", rep("Normal", 10), "Lower")),
                                      levels = c("Lower", "Normal", "Upper"),
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

  expect_equal(ret$is_outlier, factor(c(c("Upper", rep("Normal", 10), "Lower"),c("Upper", rep("Normal", 10), "Lower")),
                                      levels = c("Lower", "Normal", "Upper"),
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
  expect_equal(ret$is_outlier, factor(c(c("Upper", rep("Normal", 10), "Lower"),c("Upper", rep("Normal", 10), "Lower")),
                                      levels = c("Lower", "Normal", "Upper"),
                                      ordered=TRUE))
})


test_that("detect_outlier with NAs", {
  set.seed(1)
  data <- data.frame(
    value = c(c(3, runif(10), -4, NA), (c(3, runif(10), -4) + 3), NA),
    group = rep(c(1, 2), each = 13)
  )

  ret <- data %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(is_outlier = detect_outlier(value))

  expect_equal(ret$is_outlier, factor(c(c("Upper", rep("Normal", 10), "Lower", NA),c("Upper", rep("Normal", 10), "Lower", NA)),
                                      levels = c("Lower", "Normal", "Upper"),
                                      ordered=TRUE))

})

test_that("detect_outlier with quantile with NAs", {
  set.seed(1)
  data <- data.frame(
    value = c(c(3, runif(10), -4, NA), (c(3, runif(10), -4) + 3), NA),
    group = rep(c(1, 2), each = 13)
  )

  ret <- data %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(is_outlier = detect_outlier(value, type = "percentile", threshold = 0.95))

  expect_equal(ret$is_outlier, factor(c(c("Upper", rep("Normal", 10), "Lower", NA),c("Upper", rep("Normal", 10), "Lower", NA)),
                                      levels = c("Lower", "Normal", "Upper"),
                                      ordered=TRUE))

})

test_that("detect_outlier with quantile with NAs", {
  set.seed(1)
  data <- data.frame(
    value = c(c(3, runif(10), -4, NA), (c(3, runif(10), -4) + 3), NA),
    group = rep(c(1, 2), each = 13)
  )

  ret <- data %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(is_outlier = detect_outlier(value, type = "percentile", threshold = 0.05))

  expect_equal(ret$is_outlier, factor(c(c("Upper", rep("Normal", 10), "Lower", NA),c("Upper", rep("Normal", 10), "Lower", NA)),
                                      levels = c("Lower", "Normal", "Upper"),
                                      ordered=TRUE))

})

test_that("detect_outlier with standard_deviation with NAs", {
  set.seed(1)
  data <- data.frame(
    value = c(c(3, runif(10), -4, NA), (c(3, runif(10), -4) + 3), NA),
    group = rep(c(1, 2), each = 13)
  )

  ret <- data %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(is_outlier = detect_outlier(value, type = "standard_deviation", threshold = 1.63))
  expect_equal(ret$is_outlier, factor(c(c("Upper", rep("Normal", 10), "Lower", NA),c("Upper", rep("Normal", 10), "Lower", NA)),
                                      levels = c("Lower", "Normal", "Upper"),
                                      ordered=TRUE))
})
