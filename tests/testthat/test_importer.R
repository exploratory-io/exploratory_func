context("test imported functions")

test_that("anonymize", {
  vec <- c("Hideaki", "林 秀明", NA, "Hideaki Hayashi", "Hideaki", "林 秀明", NA, "Hideaki Hayashi")
  ret <- anonymize(vec)
  expect_equal(ret[1:4], ret[5:8]) # verify that same name goes to same anonymized name.
})

test_that("add_row", {
  df <- data.frame(x = 1:3, y = 3:1)
  ret <- add_row(df, x = 4, y = NA)
  expect_equal(ret$x[[4]], 4)
  expect_equal(ret$y[[4]], NA)
})
