context("test toplevel_util")

test_that("test row_as_header", {
  test_df <- data.frame(
    num1 = seq(3),
    num2 = -seq(3),
    char = letters[seq(3)],
    stringsAsFactors = FALSE
  )

  ret <- row_as_header(test_df, row_index = 2)
  ret2 <- row_as_header(test_df, row_index = 2, clean_names = FALSE)

  expect_equal(ret, structure(
    list(x2 = c(1L, 3L), x_2 = c(-1L, -3L), b = c("a", "c")),
    .Names = c("x2", "x_2", "b"),
    row.names = c(1L, 3L),
    class = "data.frame"))

  expect_equal(ret2, structure(
    list(`2` = c(1L, 3L), `-2` = c(-1L, -3L), b = c("a", "c")),
    .Names = c("2", "-2", "b"),
    row.names = c(1L, 3L),
    class = "data.frame"))
})
