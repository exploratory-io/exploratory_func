# how to run this test:
# devtools::test(filter="smote")

# TODO with those small data, ubSMOTE is not returning valid result.
# write test that make ubSMOTE return valid result with larger data.

context("test smote function exp_balance()")

test_that("test exp_balance with numeric", {
  sample_data <- data.frame(
    y = c(3, 4, 4, 4, 4, 4),
    num = runif(6)
  )
  res <- exp_balance(sample_data, y)
  expect_true("data.frame" %in% class(res))
  expect_equal("numeric" ,class(res$y))
  expect_equal(c(3,4) ,sort(unique(res$y)))
})

test_that("test exp_balance with character", {
  sample_data <- data.frame(
    y = c("a", "b", "b", "b", "b", "b"),
    num = runif(6),
    stringsAsFactors=FALSE
  )
  res <- exp_balance(sample_data, y)
  expect_true("data.frame" %in% class(res))
  expect_equal("character" ,class(res$y))
  expect_equal(c("a","b") ,sort(unique(res$y)))
})

test_that("test exp_balance with factor", {
  sample_data <- data.frame(
    y = factor(c("a", "b", "b", "b", "b", "b")),
    num = runif(6)
  )
  res <- exp_balance(sample_data, y)
  expect_true("data.frame" %in% class(res))
  expect_equal(class(res$y), "factor")
  expect_equal(levels(res$y), c("a","b"))
})

test_that("test exp_balance with logical", {
  sample_data <- data.frame(
    y = c(TRUE, rep(FALSE,5)),
    num = runif(6)
  )
  res <- exp_balance(sample_data, y)
  expect_true("data.frame" %in% class(res))
  expect_equal(class(res$y), "logical")
  expect_equal(any(is.na(res$y)), FALSE) # no NA is expected
})
