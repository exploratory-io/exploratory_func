context("test imported functions")

test_that("anonymize", {
  vec <- c("Hideaki", NA, "Hideaki Hayashi", "Hideaki", NA, "Hideaki Hayashi")
  ret <- anonymize(vec)
  expect_equal(ret[1:3], ret[4:6]) # verify that same name goes to same anonymized name.
})

test_that("add_row", {
  df <- data.frame(x = 1:3, y = 3:1)
  ret <- add_row(df, x = 4, y = NA)
  expect_equal(ret$x[[4]], 4)
  expect_equal(ret$y[[4]], NA_integer_)
})
