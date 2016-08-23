context("test arules function")

test_that("test do_apriori", {
  test_df <- data.frame(
    name = rep(paste("name",seq(20), sep=""), each=3),
    product = rep(paste("product",seq(5), sep=""), 12),
    number = seq(60)
  )
  ret <- suppressWarnings({
    do_apriori(test_df, name, product, min_support=0.000000000000000001)
  })
  expect_equal(colnames(ret), c("lhs", "rhs", "support", "confidence", "lift"))
  expect_true(is.character(ret[, "lhs"] ))
  expect_true(any(ret[, "lhs"] == ""))
})

test_that("test do_apriori with lhs", {
  test_df <- data.frame(
    name = rep(paste("name",seq(20), sep=""), each=3),
    product = rep(paste("product",seq(5), sep=""), 12),
    number = seq(60)
  )

  ret <- suppressWarnings({
    do_apriori(test_df, name, product, min_support=0.001, lhs="name1")
  })
  expect_equal(colnames(ret), c("lhs", "rhs", "support", "confidence", "lift"))
  expect_true(all(ret[, "lhs"] == "name1"))
})

test_that("test do_apriori with lhs", {
  test_df <- data.frame(
    name = rep(paste("name",seq(20), sep=""), each=3),
    product = rep(paste("product",seq(5), sep=""), 12),
    number = seq(60)
  )

  expect_error({
    do_apriori(test_df, name, product, min_support=0.9)
  })
})
