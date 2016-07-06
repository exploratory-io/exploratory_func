

test_that("test do_apriori", {
  test_df <- data.frame(
    name = rep(paste("name",seq(20), sep=""), each=3),
    product = rep(paste("product",seq(5), sep=""), 12),
    number = seq(60)
  )

  ret <- do_apriori(test_df, name, product, min_support=0.6, lhs="4")
  expect_equal(colnames(ret), c("lhs", "rhs", "support", "confidence", "lift"))
  expect_true(all(stringr::str_detect(ret$lhs, "4") | ret$lhs == ""))
})
