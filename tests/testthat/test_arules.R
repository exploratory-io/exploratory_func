context("test arules function")

test_that("test do_apriori", {
  test_df <- data.frame(
    name = rep(paste("name",seq(20), sep=""), each=3),
    product = rep(paste("product",seq(5), sep=""), 12),
    number = seq(60)
  )
  test_df <- test_df %>% rename(`na me`=name, `pro duct`=product)
  ret <- suppressWarnings({
    do_apriori(test_df, `na me`, `pro duct`, min_support=0.000000000000000001)
  })
  expect_equal(colnames(ret), c("lhs", "rhs", "support", "confidence", "lift"))
  expect_true(is.character(ret[, "lhs"] ))
  expect_true(!any(ret[, "lhs"] == "")) # There should be at least 1 lhs item since by default minimum number of lhs is 1.
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
  get_arules_graph_data(ret)
})

test_that("test do_apriori with lhs and rhs", {
  test_df <- data.frame(
    name = rep(paste("name",seq(20), sep=""), each=3),
    product = rep(paste("product",seq(5), sep=""), 12),
    number = seq(60)
  )

  ret <- suppressWarnings({
    do_apriori(test_df, name, product, min_support=0.001, lhs="name1", rhs="name8")
  })
  expect_equal(colnames(ret), c("lhs", "rhs", "support", "confidence", "lift"))
  expect_true(all(ret[, "lhs"] == "name1" & ret[, "rhs"] == "name8"))
  get_arules_graph_data(ret)
})

test_that("test do_apriori with no matching rule", {
  test_df <- data.frame(
    name = rep(paste("name",seq(20), sep=""), each=3),
    product = rep(paste("product",seq(5), sep=""), 12),
    number = seq(60)
  )

  expect_error({
    do_apriori(test_df, name, product, min_support=0.9, lhs="name1", rhs="name8")
  }, "No matching rule was found")
})
