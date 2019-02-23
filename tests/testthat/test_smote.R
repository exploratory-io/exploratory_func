# how to run this test:
# devtools::test(filter="smote")

# TODO with those small data, ubSMOTE is not returning valid result.
# write test that make ubSMOTE return valid result with larger data.

context("test smote function exp_balance()")


test_that("test exp_balance with numeric, already enough minority, without target size", {
  sample_data <- data.frame(
    y = c(rep(3, 49), rep(4, 51)),
    num = runif(100)
  )
  res <- exp_balance(sample_data, y, target_size = NULL)
  expect_true("data.frame" %in% class(res))
  expect_true("synthesized" %in% names(res))
  expect_equal("numeric" ,class(res$y))
  expect_equal(c(3,4) ,sort(unique(res$y)))
  expect_equal(nrow(res), 100) # res should be as is.
  expect_equal(nrow(res[res$y==3,]), 49) # res should be as is.
})

test_that("test exp_balance with numeric, enough minority with SMOTE, without target size", {
  sample_data <- data.frame(
    y = c(rep(3, 10), rep(4, 30)),
    num = runif(40)
  )
  res <- exp_balance(sample_data, y, target_size = NULL)
  expect_true("data.frame" %in% class(res))
  expect_true("synthesized" %in% names(res))
  expect_equal("numeric" ,class(res$y))
  expect_equal(c(3,4) ,sort(unique(res$y)))
  expect_lte(nrow(res), 50)
  expect_gt(nrow(res[res$y==3,]), 10)
})

test_that("test exp_balance with numeric, not enough minority even with SMOTE, without target size", {
  sample_data <- data.frame(
    y = c(rep(3, 5), rep(4, 50)),
    num = runif(55)
  )
  res <- exp_balance(sample_data, y, target_size = NULL)
  expect_true("data.frame" %in% class(res))
  expect_true("synthesized" %in% names(res))
  expect_equal("numeric" ,class(res$y))
  expect_equal(c(3,4) ,sort(unique(res$y)))
  expect_lte(nrow(res), 38) # Minority is SMOTEd and majority is sampled. Expectation with some slack.
  expect_gt(nrow(res[res$y==3,]), 7)
})

test_that("test exp_balance with numeric, enough minority and not enough majority, with target size", {
  sample_data <- data.frame(
    y = c(rep(3, 49), rep(4, 51)),
    num = runif(100)
  )
  res <- exp_balance(sample_data, y, target_size=100)
  expect_true("data.frame" %in% class(res))
  expect_true("synthesized" %in% names(res))
  expect_equal("numeric" ,class(res$y))
  expect_equal(c(3,4) ,sort(unique(res$y)))
  expect_equal(nrow(res), 100) # res should be as is
  expect_equal(nrow(res[res$y==3,]), 49)
})

test_that("test exp_balance with numeric, enough minority and majority, with target size", {
  sample_data <- data.frame(
    y = c(rep(3, 49), rep(4, 51)),
    num = runif(100)
  )
  res <- exp_balance(sample_data, y, target_size=50)
  expect_true("data.frame" %in% class(res))
  expect_true("synthesized" %in% names(res))
  expect_equal("numeric" ,class(res$y))
  expect_equal(c(3,4) ,sort(unique(res$y)))
  expect_equal(nrow(res), 50) # Both minority and majority should be sampled down.
  expect_equal(nrow(res[res$y==3,]), 20)
})

test_that("test exp_balance with numeric, not enough minority but enough with smote, and enough majority, with target size", {
  sample_data <- data.frame(
    y = c(rep(3, 12), rep(4, 88)),
    num = runif(100)
  )
  res <- exp_balance(sample_data, y, target_size=50)
  expect_true("data.frame" %in% class(res))
  expect_true("synthesized" %in% names(res))
  expect_equal("numeric" ,class(res$y))
  expect_equal(c(3,4) ,sort(unique(res$y)))
  expect_lt(nrow(res), 55) # Minority is SMOTEd and majority is sampled down.
  expect_gt(nrow(res), 45)
  expect_gt(nrow(res[res$y==3,]), 15)
})

test_that("test exp_balance with numeric, not enough minority even with smote, and enough majority, with target size", {
  sample_data <- data.frame(
    y = c(rep(3, 5), rep(4, 95)),
    num = runif(100)
  )
  res <- exp_balance(sample_data, y, target_size=50, k=3) # Smaller k, since original minority size is small.
  expect_true("data.frame" %in% class(res))
  expect_true("synthesized" %in% names(res))
  expect_equal("numeric" ,class(res$y))
  expect_equal(c(3,4) ,sort(unique(res$y)))
  # Minority is SMOTEd to the limit and majority is sampled down to make the ratio.
  # Ideal result: minority 15, majority 22.5
  # This one is rather unstable most likely because of too small minority.
  expect_lte(nrow(res), 40)
  expect_gte(nrow(res), 30)
  expect_gt(nrow(res[res$y==3,]), 12)
})

test_that("test exp_balance with numeric, not enough minority and not enough majority, but enough minority for target ratio, with target size", {
  sample_data <- data.frame(
    y = c(rep(3, 40), rep(4, 60)),
    num = runif(100)
  )
  res <- exp_balance(sample_data, y, target_size=200)
  expect_true("data.frame" %in% class(res))
  expect_true("synthesized" %in% names(res))
  expect_equal("numeric" ,class(res$y))
  expect_equal(c(3,4) ,sort(unique(res$y)))
  expect_equal(nrow(res), 100) # res should be as is
  expect_equal(nrow(res[res$y==3,]), 40)
})

test_that("test exp_balance with numeric, not enough minority and not enough majority, but enough minority with SMOTE for target ratio, with target size", {
  sample_data <- data.frame(
    y = c(rep(3, 35), rep(4, 65)),
    num = runif(100)
  )
  res <- exp_balance(sample_data, y, target_size=200)
  expect_true("data.frame" %in% class(res))
  expect_true("synthesized" %in% names(res))
  expect_equal("numeric" ,class(res$y))
  expect_equal(c(3,4) ,sort(unique(res$y)))
  # Minority should be SMOTEd to make ratio
  expect_lte(nrow(res), 130)
  expect_gte(nrow(res), 1)
  expect_gt(nrow(res[res$y==3,]), 3)
})

test_that("test exp_balance with numeric, not enough minority and not enough majority, not enough minority even with SMOTE for target ratio, with target size", {
  sample_data <- data.frame(
    y = c(rep(3, 10), rep(4, 90)),
    num = runif(100)
  )
  res <- exp_balance(sample_data, y, target_size=200)
  expect_true("data.frame" %in% class(res))
  expect_true("synthesized" %in% names(res))
  expect_equal("numeric" ,class(res$y))
  expect_equal(c(3,4) ,sort(unique(res$y)))
  # Minority should be SMOTEd to the limit and majority should be sampled to make ratio.
  expect_lte(nrow(res), 85)
  expect_gte(nrow(res), 65)
  expect_gt(nrow(res[res$y==3,]), 25)
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

test_that("test exp_balance with ordered factor with NA as a predictors", {
  sample_data <- data.frame(
    y = factor(c("a", "b", "b", "b", "b", "b")),
    x = factor(c("A", "A", "B", "B", NA, "B"), ordered=TRUE),
    num = runif(6)
  )
  res <- exp_balance(sample_data, y)
  expect_true("data.frame" %in% class(res))
  expect_equal(class(res$x), "factor")
  expect_equal(levels(res$x), c("A", "B", "(Missing)"))
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
