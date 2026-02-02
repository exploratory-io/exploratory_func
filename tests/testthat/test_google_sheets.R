context("test for google_sheets functions")

test_that("normalizeDataForGoogleSheetsExport", {
  df <- read_rds_file("https://www.dropbox.com/s/06jmg6k4lnb61p2/flight300.rds?dl=1")
  df <- df %>% mutate(tstint64 = bit64::as.integer64(1234))
  df_cleaned <- df %>% exploratory::normalizeDataForGoogleSheetsExport()
  expect_equal(class(df_cleaned$`hms 表`), "numeric")
  expect_equal(class(df_cleaned$`difftime 表`), "numeric")
  expect_equal(class(df_cleaned$tstint64), "numeric")
  expect_equal(class(df_cleaned$numAllNA), "logical")
  expect_equal(class(df_cleaned$numAllNaN), "numeric")
  expect_equal(class(df_cleaned$numAllInf), "numeric")
  expect_equal(class(df_cleaned$numAllMixed), "numeric")
})

# Tests for col_types padding helper function (Issue #33700)
test_that(".pad_col_types_for_column_mismatch pads col_types correctly", {
  # Test case 1: Standard error message with column count
  error_msg <- "Length of `col_types` is not compatible with columns found in sheets:\n- 14 column types specified.\n- 14 un-skipped column types specified.\n- But there are 15 columns found in sheets."
  result <- exploratory:::.pad_col_types_for_column_mismatch("cinTDDDDDDDDDT", error_msg)
  expect_equal(result, "cinTDDDDDDDDDT?")  # Original 14 chars + 1 '?' for the extra column

  # Test case 2: Multiple extra columns
  error_msg2 <- "Length of `col_types` is not compatible with columns found in sheets:\n- 5 column types specified.\n- 5 un-skipped column types specified.\n- But there are 8 columns found in sheets."
  result2 <- exploratory:::.pad_col_types_for_column_mismatch("cinTD", error_msg2)
  expect_equal(result2, "cinTD???")  # Original 5 chars + 3 '?' for extra columns

  # Test case 3: Error message doesn't match expected pattern
  error_msg3 <- "Some other error message"
  result3 <- exploratory:::.pad_col_types_for_column_mismatch("cinT", error_msg3)
  expect_null(result3)

  # Test case 4: col_types already matches or exceeds actual columns (edge case)
  error_msg4 <- "Length of `col_types` is not compatible with columns found in sheets:\n- 5 column types specified.\n- But there are 3 columns found in sheets."
  result4 <- exploratory:::.pad_col_types_for_column_mismatch("cinTD", error_msg4)
  expect_null(result4)  # Should return NULL since actual < specified
})
