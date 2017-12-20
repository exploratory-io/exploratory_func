context("test na_util")

test_that("test impute_na with factor", {
  input <- factor(c(letters[1:5], NA_character_, letters[1:5], NA_character_))

  ret1 <- impute_na(input, type = "value", val = "new")
  expect_equal(ret1, as.factor(c(letters[1:5], "new", letters[1:5], "new")))

  ret2 <- impute_na(input, type = "value", val = letters[seq(length(input))])
  expect_equal(ret2, as.factor(c(letters[1:5], letters[6], letters[1:5], letters[12])))

})

test_that("test impute_na with factor with values already included in original factor", {
  input <- factor(c(letters[1:5], NA_character_, letters[1:5], NA_character_))

  ret <- impute_na(input, type = "value", val = "a")
  expect_equal(ret, as.factor(c(letters[1:5], "a", letters[1:5], "a")))
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

test_that("test impute_na with median", {
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
    dplyr::mutate(filled_na = impute_na(`col 1`, type = "value", val = 0))

  val <- 0
  expect_equal(ret[["filled_na"]], rep(c(val, val, seq(7), val), 10))
})

test_that("test impute_na with POSIXct", {
  test_data <- data.frame(
    rep(as.POSIXct(as.Date(c("2013-01-01", "2014-01-01", "2015-01-01", NA, "2016-01-01"))), 20)
  )
  colnames(test_data) <- "col 1"
  ret <- test_data %>%
    dplyr::mutate(filled_na = impute_na(`col 1`, type = "value", val = as.POSIXct(as.Date("2013-01-01"))))

  expect_equal(ret[["filled_na"]], rep(as.POSIXct(as.Date(c("2013-01-01", "2014-01-01", "2015-01-01", "2013-01-01", "2016-01-01"))), 20))
})

test_that("test impute_na with a column", {
  test_data <- data.frame(
    rep(c("2013-01-01", "2014-01-01", "2015-01-01", NA, "2016-01-01")),
    rep(c("2013-01-01", "2014-01-01", NA, "2015-01-01", "2016-01-01"))
  )
  colnames(test_data) <- c("col 1", "col-2")
  ret <- test_data %>%
    dplyr::mutate(filled_na = impute_na(`col 1`, type = "value", val = `col-2`))

  expect_equal(ret[["filled_na"]], as.factor(c("2013-01-01", "2014-01-01", "2015-01-01", "2015-01-01", "2016-01-01")))
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

test_that("test fill_between", {
  test_data <- data.frame(
    c(NA, 1, NA, 2, NA),
    c(NA, "a", NA, "b", NA),
    factor(c(NA, "a", NA, "b", NA)),
    stringsAsFactors=FALSE
  )
  colnames(test_data) <- c("col 1", "col-2", "col_3")
  ret <- test_data %>%
    fill_between(`col 1`,`col-2`, col_3)
  expect_equal(ret[["col 1"]], c(NA, 1, 1, 2, NA))
  expect_equal(ret[["col-2"]], c(NA, "a", "a", "b", NA))
  expect_equal(ret[["col_3"]], factor(c(NA, "a", "a", "b", NA)))
})

test_that("test fill_between with .direction=up", {
  test_data <- data.frame(
    c(NA, 1, NA, 2, NA),
    c(NA, "a", NA, "b", NA),
    factor(c(NA, "a", NA, "b", NA)),
    stringsAsFactors=FALSE
  )
  colnames(test_data) <- c("col 1", "col-2", "col_3")
  ret <- test_data %>%
    fill_between(`col 1`,`col-2`, col_3, .direction="up")
  expect_equal(ret[["col 1"]], c(NA, 1, 2, 2, NA))
  expect_equal(ret[["col-2"]], c(NA, "a", "b", "b", NA))
  expect_equal(ret[["col_3"]], factor(c(NA, "a", "b", "b", NA)))
})

test_that("test fill_between with group_by", {
  test_data <- data.frame(
    c(NA,1,NA,2,NA,1,NA,2,NA,NA),
    c("a","a","a","a","a","b","b","b","b","b"),
    stringsAsFactors=FALSE
  )
  #TODO when this is "col 1", "col-2", this test fails at group_by_ inside fill_between.
  colnames(test_data) <- c("col1", "col2")
  ret <- test_data %>%
    group_by(`col2`) %>%
    fill_between(`col1`)
  expect_equal(ret[["col1"]],
               c(NA,1,1,2,NA,1,1,2,NA,NA))
})
