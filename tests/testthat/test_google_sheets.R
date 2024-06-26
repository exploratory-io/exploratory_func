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
