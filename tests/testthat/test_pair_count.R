context("test pair_count")

test_that("pair_count", {
  test_df <- data.frame(
    seq = rep(seq(2), 5),
    let = rep(letters[seq(5)], each = 2)
  )
  test_df <- test_df %>% rename(`se q`=seq, `le t`=let)
  ret <- test_df %>% pair_count(`se q`, `le t`, distinct = FALSE, sort = FALSE)
  expect_equal(nrow(ret), 20)
  expect_true(all(ret[["value"]] == 2))
  ret <- test_df %>% pair_count(`se q`, `le t`, distinct = FALSE,  diag = TRUE, sort = TRUE)
  expect_equal(nrow(ret), 25)
  expect_true(all(ret[["value"]] == 2))
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
  ret1 <- pair_count(test_df, group, chars, distinct = FALSE, sort = FALSE)

  expect_equal(ret1[["chars.x"]], c("a", "a", "a", "a", "b", "c", "c",
                                   "d", "b", "c", "d", NA, "d", "d", NA, NA))
  expect_equal(ret1[["chars.y"]], c("b", "c",
                                   "d", NA, "d", "d", NA, NA, "a", "a", "a", "a", "b", "c", "c",
                                   "d"))
  expect_equal(ret1[["value"]],  c(1, 2, 3, 1, 1, 2, 1, 1, 1, 2, 3, 1, 1, 2, 1, 1))

  ret2 <- pair_count(test_df, group, chars, diag = TRUE, sort = TRUE, distinct = FALSE)

  expect_equal(ret2[[1]], c("a", "a", "d", "d", "a", "c", "c", "c", "d", "a", "a", "b", "b", "c", "d", NA, "b", NA, "d", NA, NA))
  expect_equal(ret2[[2]], c("a", "d", "d", "a", "c", "c", "d", "a", "c", "b", NA, "b", "d", NA, NA, NA, "a", "a", "b", "c", "d"))
  expect_equal(ret2[[3]], c(3, 3, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

  ret3 <- pair_count(test_df, group, chars, diag = TRUE, sort = TRUE, distinct = FALSE, unite = TRUE)
  expect_equal(ret3[[1]], c("a_a","a_d","d_d", "d_a", "a_c","c_c","c_d","c_a","d_c","a_b","a_NA", "b_b","b_d","c_NA","d_NA","NA_NA","b_a","NA_a","d_b","NA_c","NA_d" ))

  test_group_df <- data.frame(
    gender = c("M", "F", "F", "M", "M", "M", "M", "F", "M", "F", "F", "M", "F","M", "F", "M"),
    group = c(rep(c("group1", "group2", "group3"), each = 5), "group3"),
    chars = c(
      "b", "a", "d", "b", "a",
      "a", "d", "d", "d", "c",
      "a", "c", "c", "d", "d", NA
    ),
    stringsAsFactors = FALSE
  )
  ret4 <- test_group_df %>% pair_count(group, chars, distinct = TRUE, sort = FALSE, unite = TRUE, group_by = gender)
  # gender chars value
  # <chr>  <chr> <dbl>
  # 1 F      a_c       1
  # 2 F      a_d       2
  # 3 F      c_d       2
  # 4 M      a_b       1
  # 5 M      a_d       1
  # 6 M      c_d       1
  # 7 M      c_NA      1
  # 8 M      d_NA      1
  expect_equal(ret4[[1]], c("F", "F", "F", "M", "M", "M", "M", "M"))
  expect_equal(ret4[[2]], c("a_c",  "a_d",  "c_d",  "a_b",  "a_d",  "c_d",  "c_NA", "d_NA"))
  expect_equal(ret4[[3]], c(1, 2, 2, 1, 1, 1, 1, 1))

})
