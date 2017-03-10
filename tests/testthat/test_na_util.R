context("test na_util")

test_that("test predict_na", {
  test_data <- data.frame(
    rep(c(NA, NA, seq(7), NA), 10),
    rep(c(10, seq(8), NA), 10),
    rep(c(NA, seq(8), NA), 10),
    letters[rep(c(NA, seq(3), NA), 20)]
  )
  colnames(test_data) <- c("col 1", "col-2", "col_3", "chars")
  ret <- test_data %>%
    dplyr::mutate(filled_na = predict_na(`col 1`, `col-2`, col_3, chars))

  expect_true(any(is.na(test_data[["col 1"]]) & !is.na(ret[["filled_na"]])))
})
