context("test pair_count")

test_that("pair_count", {
  test_df <- data.frame(
    seq = rep(seq(2), 5),
    let = rep(letters[seq(5)], each = 2)
  )
  ret <- test_df %>% pair_count(seq, let)
  expect_equal(nrow(ret), 10)
  expect_true(all(ret[["n"]] == 2))
  ret <- test_df %>% pair_count(seq, let, unique_pair = FALSE,  self = TRUE, sort = TRUE)
  expect_equal(nrow(ret), 25)
  expect_true(all(ret[["n"]] == 2))
})
