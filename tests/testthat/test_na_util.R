context("test na_util")

test_that("test impute_na", {
  test_data <- data.frame(
    rep(c(NA, NA, seq(7), NA), 10),
    rep(c(10, seq(8), NA), 10),
    rep(c(NA, seq(8), NA), 10),
    letters[rep(c(NA, seq(3), NA), 20)],
    letters[rep(seq(2), each = 50)],
    letters[rep(seq(4), each = 25)]
  )
  colnames(test_data) <- c("col 1", "col-2", "col_3", "chars", "group1", "group2")
  ret1 <- test_data %>%
    dplyr::group_by(group1) %>%
    dplyr::mutate(filled_na = impute_na(`col 1`, `col-2`, col_3, chars, type = "predict"))

  expect_true(any(is.na(test_data[["col 1"]]) & !is.na(ret1[["filled_na"]])))

  ret2 <- test_data %>%
    dplyr::group_by(group2) %>%
    dplyr::mutate(filled_na = impute_na(`col 1`, `col-2`, col_3, chars, type = "predict"))

  # result should be changed by group_by
  expect_true(any(ret1[["filled_na"]][!is.na(ret1[["filled_na"]])] != ret2[["filled_na"]][!is.na(ret2[["filled_na"]])]))
})

test_that("test impute_na", {
  test_data <- data.frame(
    rep(c(NA, NA, seq(7), NA), 10),
    rep(c(10, seq(8), NA), 10),
    rep(c(NA, seq(8), NA), 10),
    letters[rep(c(NA, seq(3), NA), 20)],
    letters[rep(seq(2), each = 50)],
    letters[rep(seq(4), each = 25)]
  )
  colnames(test_data) <- c("col 1", "col-2", "col_3", "chars", "group1", "group2")
  expect_error({
    test_data %>%
      dplyr::group_by(group1) %>%
      dplyr::mutate(filled_na = impute_na(`col 1`, type = "predict"))
  }, "Please choose predictor columns")
})

test_that("test impute_na with mean", {
  test_data <- data.frame(
    rep(c(NA, NA, seq(7), NA), 10),
    rep(c(10, seq(8), NA), 10),
    rep(c(NA, seq(8), NA), 10),
    letters[rep(c(NA, seq(3), NA), 20)]
  )
  colnames(test_data) <- c("col 1", "col-2", "col_3", "chars")
  ret <- test_data %>%
    dplyr::mutate(filled_na = impute_na(`col 1`, type = "mean"))

  val <- mean(test_data[["col 1"]], na.rm = TRUE)
  expect_equal(ret[["filled_na"]], rep(c(val, val, seq(7), val), 10))
})

test_that("test impute_na with mean", {
  test_data <- data.frame(
    rep(c(NA, NA, seq(7), NA), 10),
    rep(c(10, seq(8), NA), 10),
    rep(c(NA, seq(8), NA), 10),
    letters[rep(c(NA, seq(3), NA), 20)]
  )
  colnames(test_data) <- c("col 1", "col-2", "col_3", "chars")
  ret <- test_data %>%
    dplyr::mutate(filled_na = impute_na(`col 1`, type = "median"))

  val <- median(test_data[["col 1"]], na.rm = TRUE)
  expect_equal(ret[["filled_na"]], rep(c(val, val, seq(7), val), 10))
})

test_that("test impute_na with sd", {
  test_data <- data.frame(
    rep(c(NA, NA, seq(7), NA), 10),
    rep(c(10, seq(8), NA), 10),
    rep(c(NA, seq(8), NA), 10),
    letters[rep(c(NA, seq(3), NA), 20)]
  )
  colnames(test_data) <- c("col 1", "col-2", "col_3", "chars")
  ret <- test_data %>%
    dplyr::mutate(filled_na = impute_na(`col 1`, type = sd))

  val <- sd(test_data[["col 1"]], na.rm = TRUE)
  expect_equal(ret[["filled_na"]], rep(c(val, val, seq(7), val), 10))
})

test_that("test impute_na with val", {
  test_data <- data.frame(
    rep(c(NA, NA, seq(7), NA), 10),
    rep(c(10, seq(8), NA), 10),
    rep(c(NA, seq(8), NA), 10),
    letters[rep(c(NA, seq(3), NA), 20)]
  )
  colnames(test_data) <- c("col 1", "col-2", "col_3", "chars")
  ret <- test_data %>%
    dplyr::mutate(filled_na = impute_na(`col 1`, type = 0))

  val <- 0
  expect_equal(ret[["filled_na"]], rep(c(val, val, seq(7), val), 10))
})

test_that("test impute_na with undefined type", {
  test_data <- data.frame(
    rep(c(NA, NA, seq(7), NA), 10),
    rep(c(10, seq(8), NA), 10),
    rep(c(NA, seq(8), NA), 10),
    letters[rep(c(NA, seq(3), NA), 20)]
  )
  colnames(test_data) <- c("col 1", "col-2", "col_3", "chars")
  expect_error({
    ret <- test_data %>%
      dplyr::mutate(filled_na = impute_na(`col 1`, type = "va"))
  }, "va is not supported as type")
})

