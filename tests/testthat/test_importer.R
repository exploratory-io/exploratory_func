context("test imported functions")

test_that("anonymize", {
  vec <- c("Hideaki", "林 秀明", NA, "Hideaki Hayashi", "Hideaki", "林 秀明", NA, "Hideaki Hayashi")
  ret <- anonymize(vec)
  expect_equal(ret[1:4], ret[5:8])
})
