context("test one_hot")

test_that("test one_hot",{
  test_data <- data.frame(
    char = letters[rep(seq(3), 4)],
    char2 = letters[rep(c(NA, seq(2)), 4)],
    date = as.Date(c("2011-01-01", "2012-01-01")),
    num = seq(12)
  )

  ret <- test_data %>%
    one_hot(dplyr::starts_with("char"), sep = "-")
  expect_true(all(colnames(ret) != "char"))
  expect_true(is.logical(ret[["char-b"]]))
})

test_that("test some data types", {
  test_data <- data.frame(
    char = letters[rep(seq(3), 4)],
    char2 = letters[rep(c(NA, seq(2)), 4)],
    date = as.Date(c("2011-01-01", "2012-01-01")),
    num = seq(12)
  )

  ret <- test_data %>%
    one_hot(num, date, sep = "-")
  expect_true(is.logical(ret[["num-3"]]))
})
