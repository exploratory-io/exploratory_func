context("test combine_multiple_response_column function")

test_that("combine_multiple_response_column basic remove_prefix mode", {
  df <- data.frame(
    id = 1:4,
    q_a = c(1, 0, 1, 0),
    q_b = c(0, 1, 1, 0),
    q_c = c(0, 0, 1, 0)
  )

  ret <- combine_multiple_response_column(
    df,
    columns = c("q_a", "q_b", "q_c"),
    output_column = "combined",
    option_name_type = "remove_prefix",
    option_name_prefix = "q_"
  )

  expect_equal(ret$combined, c("a", "b", "a,b,c", NA_character_))
})

test_that("combine_multiple_response_column full_column_name mode", {
  df <- data.frame(
    id = 1:3,
    q_a = c(1, 0, 1),
    q_b = c(0, 1, 1)
  )

  ret <- combine_multiple_response_column(
    df,
    columns = c("q_a", "q_b"),
    output_column = "combined",
    option_name_type = "full_column_name"
  )

  expect_equal(ret$combined, c("q_a", "q_b", "q_a,q_b"))
})

test_that("combine_multiple_response_column with custom selected_value (character)", {
  df <- data.frame(
    id = 1:3,
    opt_a = c("Y", "N", "Y"),
    opt_b = c("N", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  ret <- combine_multiple_response_column(
    df,
    columns = c("opt_a", "opt_b"),
    output_column = "combined",
    selected_value = "Y",
    option_name_type = "remove_prefix",
    option_name_prefix = "opt_"
  )

  expect_equal(ret$combined, c("a", "b", "a,b"))
})

test_that("combine_multiple_response_column with logical columns and selected_value = TRUE", {
  df <- data.frame(
    id = 1:3,
    opt_a = c(TRUE, FALSE, TRUE),
    opt_b = c(FALSE, TRUE, TRUE)
  )

  ret <- combine_multiple_response_column(
    df,
    columns = c("opt_a", "opt_b"),
    output_column = "combined",
    selected_value = TRUE,
    option_name_type = "remove_prefix",
    option_name_prefix = "opt_"
  )

  expect_equal(ret$combined, c("a", "b", "a,b"))
})

test_that("combine_multiple_response_column with custom separator and no_selection text", {
  df <- data.frame(
    id = 1:2,
    q_a = c(1, 0),
    q_b = c(1, 0)
  )

  ret <- combine_multiple_response_column(
    df,
    columns = c("q_a", "q_b"),
    output_column = "combined",
    option_name_type = "remove_prefix",
    option_name_prefix = "q_",
    separator = " / ",
    no_selection = "None Selected"
  )

  expect_equal(ret$combined, c("a / b", "None Selected"))
})

test_that("combine_multiple_response_column with remove_original = TRUE drops source columns", {
  df <- data.frame(
    id = 1:2,
    q_a = c(1, 0),
    q_b = c(0, 1),
    other = c("x", "y"),
    stringsAsFactors = FALSE
  )

  ret <- combine_multiple_response_column(
    df,
    columns = c("q_a", "q_b"),
    output_column = "combined",
    option_name_type = "remove_prefix",
    option_name_prefix = "q_",
    remove_original = TRUE
  )

  expect_equal(names(ret), c("id", "other", "combined"))
  expect_equal(ret$combined, c("a", "b"))
})

test_that("combine_multiple_response_column treats NA as not selected", {
  df <- data.frame(
    id = 1:3,
    q_a = c(1, NA, NA),
    q_b = c(NA, 1, NA)
  )

  ret <- combine_multiple_response_column(
    df,
    columns = c("q_a", "q_b"),
    output_column = "combined",
    option_name_type = "remove_prefix",
    option_name_prefix = "q_"
  )

  expect_equal(ret$combined, c("a", "b", NA_character_))
})

test_that("combine_multiple_response_column errors when a source column does not exist", {
  df <- data.frame(id = 1:2, q_a = c(1, 0))

  expect_error(
    combine_multiple_response_column(
      df,
      columns = c("q_a", "q_missing"),
      output_column = "combined",
      option_name_type = "remove_prefix",
      option_name_prefix = "q_"
    ),
    "do not exist"
  )
})

test_that("combine_multiple_response_column errors when output_column equals a source column", {
  df <- data.frame(id = 1:2, q_a = c(1, 0), q_b = c(0, 1))

  expect_error(
    combine_multiple_response_column(
      df,
      columns = c("q_a", "q_b"),
      output_column = "q_a",
      option_name_type = "remove_prefix",
      option_name_prefix = "q_"
    ),
    "must be different"
  )
})

test_that("combine_multiple_response_column errors when option_name_prefix does not match every column", {
  df <- data.frame(id = 1:2, q_a = c(1, 0), other_b = c(0, 1))

  expect_error(
    combine_multiple_response_column(
      df,
      columns = c("q_a", "other_b"),
      output_column = "combined",
      option_name_type = "remove_prefix",
      option_name_prefix = "q_"
    ),
    "does not match the beginning"
  )
})

test_that("combine_multiple_response_column errors when option_name_prefix is missing for remove_prefix", {
  df <- data.frame(id = 1:2, q_a = c(1, 0), q_b = c(0, 1))

  expect_error(
    combine_multiple_response_column(
      df,
      columns = c("q_a", "q_b"),
      output_column = "combined",
      option_name_type = "remove_prefix"
    ),
    "must be specified"
  )
})

test_that("combine_multiple_response_column handles column names with spaces, multibyte characters, and symbols", {
  col_a <- r"(航空 会社 !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表_a)"
  col_b <- r"(航空 会社 !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表_b)"
  prefix <- r"(航空 会社 !"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表_)"

  df <- data.frame(id = 1:2, x = c(1, 0), y = c(0, 1))
  names(df) <- c("id", col_a, col_b)

  ret <- combine_multiple_response_column(
    df,
    columns = c(col_a, col_b),
    output_column = "combined",
    option_name_type = "remove_prefix",
    option_name_prefix = prefix
  )

  expect_equal(ret$combined, c("a", "b"))
})

test_that("combine_multiple_response_column errors when no_selection is not character/NA", {
  df <- data.frame(id = 1:2, q_a = c(1, 0), q_b = c(0, 1))

  expect_error(
    combine_multiple_response_column(
      df,
      columns = c("q_a", "q_b"),
      output_column = "combined",
      option_name_type = "remove_prefix",
      option_name_prefix = "q_",
      no_selection = 0
    ),
    "no_selection must be a character string or NA"
  )
})

test_that("combine_multiple_response_column errors when columns contains duplicates", {
  df <- data.frame(id = 1:2, q_a = c(1, 0), q_b = c(0, 1))

  expect_error(
    combine_multiple_response_column(
      df,
      columns = c("q_a", "q_a", "q_b"),
      output_column = "combined",
      option_name_type = "remove_prefix",
      option_name_prefix = "q_"
    ),
    "columns must not contain duplicates"
  )
})

test_that("combine_multiple_response_column output order follows the columns argument order, not the data frame's physical column order", {
  df <- data.frame(
    id = 1L,
    q_a = 1,
    q_b = 1,
    q_c = 1
  )

  ret <- combine_multiple_response_column(
    df,
    columns = c("q_c", "q_a", "q_b"),
    output_column = "combined",
    option_name_type = "remove_prefix",
    option_name_prefix = "q_"
  )

  expect_equal(ret$combined, "c,a,b")
})

test_that("combine_multiple_response_column accepts a bare NA (not NA_character_) as no_selection", {
  df <- data.frame(
    id = 1:3,
    q_a = c(1, NA, NA),
    q_b = c(NA, 1, NA)
  )

  ret <- combine_multiple_response_column(
    df,
    columns = c("q_a", "q_b"),
    output_column = "combined",
    option_name_type = "remove_prefix",
    option_name_prefix = "q_",
    no_selection = NA
  )

  expect_equal(ret$combined, c("a", "b", NA_character_))
})
