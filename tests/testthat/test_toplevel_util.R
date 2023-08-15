context("test toplevel_util")

test_that("test row_as_header", {
  test_df <- data.frame(
    num1 = seq(3),
    num2 = -seq(3),
    char = letters[seq(3)],
    non_ascii = c("あ!@","い$%","う^&"),
    stringsAsFactors = FALSE
  )

  ret <- row_as_header(test_df, row_index = 2)
  ret2 <- row_as_header(test_df, row_index = 2, clean_names = FALSE)
  ret3 <- row_as_header(test_df, row_index = 2, clean_names = TRUE, convert_to_ascii = TRUE)

  expect_equal(ret, structure(
    list(x2 = c(1L, 3L), x_2 = c(-1L, -3L), b = c("a", "c"), い = c("あ!@","う^&")),
    .Names = c("X2", "X2_2", "b", "い_percent"),
    row.names = c(1L, 3L),
    class = "data.frame"))

  expect_equal(ret2, structure(
    list(`2` = c(1L, 3L), `-2` = c(-1L, -3L), b = c("a", "c"), "い$%" = c("あ!@","う^&")),
    .Names = c("2", "-2", "b", "い$%"),
    row.names = c(1L, 3L),
    class = "data.frame"))
  expect_equal(ret3, structure(
    list(x2 = c(1L, 3L), x2_2 = c(-1L, -3L), b = c("a", "c"), i_percent = c("あ!@","う^&")),
    .Names = c("X2", "X2_2", "b", "i_percent"),
    row.names = c(1L, 3L),
    class = "data.frame"))
  test_df2 <- readr::read_csv("X1,X2
                              1000,2000
                              2000,5000
                              Sales,Quantity")
  ret4 <- row_as_header(test_df2, row_index = -1, clean_names = TRUE, guess_data_type = TRUE)
  expect_equal(colnames(ret4), c("Sales", "Quantity"))
  expect_equal(class(ret4$Sales), "numeric")
  expect_equal(class(ret4$Quantity), "numeric")
})

test_that("test row_as_header with factor", {
  test_df <- data.frame(
    num1 = seq(3),
    num2 = -seq(3),
    char = letters[seq(3)]
  )

  ret <- row_as_header(test_df, row_index = 2)
  ret2 <- row_as_header(test_df, row_index = 2, clean_names = FALSE)

  expect_equal(c("X2", "X2_2", "b"), colnames(ret))
  expect_equal(c("2", "-2", "b"), colnames(ret2))
})

