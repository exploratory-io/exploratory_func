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

test_that("pair_count", {
  test_df <- data.frame(
    group = c(rep(c("group1", "group2", "group3"), each = 5), "group3"),
    chars = c(
      "b", "a", "d", "b", "a",
      "a", "d", "d", "d", "c",
      "a", "c", "c", "d", "d", NA
    ),
    stringsAsFactors = FALSE
  )
  ret1 <- pair_count(test_df, group, chars)

  expect_equal(ret1[["value1"]], c("b", "b", "a", "a", "d", "a", "d","c"))
  expect_equal(ret1[["value2"]], c("a", "d", "d", "c", "c", NA, NA, NA))
  expect_equal(ret1[["n"]], c(1, 1, 3, 2, 2, 1, 1, 1))

  ret2 <- pair_count(test_df, group, chars, self = TRUE, sort = TRUE, unique_pair = FALSE)

  expect_equal(ret2[[1]], c("a", "d", "a", "d", "c", "c", "a", "d", "c", "b", "a", "d", "b", NA, "b", NA, NA, "a", "d", "c", NA))
  expect_equal(ret2[[2]], c("a", "a", "d", "d", "a", "d", "c", "c", "c", "b", "b", "b", "a", "a", "d", "d", "c", NA, NA, NA, NA))
  expect_equal(ret2[[3]], c(3, 3, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
})
