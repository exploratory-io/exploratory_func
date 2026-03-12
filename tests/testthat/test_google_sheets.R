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

test_that("normalizeDataForGoogleSheetsExport with local data", {
  skip_if_not_installed("bit64")
  df <- data.frame(
    num_col = c(1.0, Inf, -Inf, 4.0),
    diff_col = as.difftime(c(10, 20, 30, 40), units = "secs"),
    period_col = lubridate::seconds(c(100, 200, 300, 400)),
    int64_col = bit64::as.integer64(c(100, 200, 300, 400))
  )
  result <- exploratory::normalizeDataForGoogleSheetsExport(df)
  # Inf values should become NA
  expect_true(is.na(result$num_col[2]))
  expect_true(is.na(result$num_col[3]))
  expect_equal(result$num_col[1], 1.0)
  expect_equal(result$num_col[4], 4.0)
  # difftime should become numeric
  expect_equal(class(result$diff_col), "numeric")
  # period should become numeric
  expect_equal(class(result$period_col), "numeric")
  # integer64 should become numeric
  expect_equal(class(result$int64_col), "numeric")
})
