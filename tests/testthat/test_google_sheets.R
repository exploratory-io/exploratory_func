context("test for google_sheets functions")

test_that("normalizeDataForGoogleSheetsExport", {
  df <- read_rds_file("https://www.dropbox.com/s/06jmg6k4lnb61p2/flight300.rds?dl=1")
  df_cleaned <- df %>% exploratory::normalizeDataForGoogleSheetsExport()
  expect_equal(class(df_cleaned$`hms 表`), "numeric")
  expect_equal(class(df_cleaned$`difftime 表`), "numeric")
  expect_equal(class(df_cleaned$numAllInf), "logical")
})
