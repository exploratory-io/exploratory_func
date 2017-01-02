context("language support")

test_that("text replacement", {
  text <- c("半角スペ2  全角スペ2　　", "半角2!!全角2！！")

  ret1 <- convert_zen_han(text, to="han")

  expect_equal(ret1, c("半角スペ2  全角スペ2  ", "半角2!!全角2!!"))

  ret2 <- convert_zen_han(text, to="zen")

  expect_equal(ret2, c("半角スペ２　　全角スペ２　　", "半角２！！全角２！！"))

})
