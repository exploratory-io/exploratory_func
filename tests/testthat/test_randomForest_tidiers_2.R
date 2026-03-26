context("test tidiers for ranger randomForest")

testdata_dir <- tempdir()
testdata_filename <- "airline_2013_10_tricky_v3_5k.csv" 
testdata_file_path <- paste0(testdata_dir, '/', testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  flight <- flight %>% slice_sample(n=5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}

test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(NA, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, NA, 361, 507, 1020, 1092, 342, NA, 1184, 545),
      FNUMBER= c(21, NA, 6, 87, 23, 12, 3, 0, 13, 1, 85, 82, 31, 57, 20, 12, 42, 49, NA, 45)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE", "FNUMBER"))
test_that("ranger.find_na", {
  df <- structure(
    list(x = 1:10, y = rep(TRUE, 10)),
    row.names = c(NA, 10L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("x", "y")
  )
  expected_na_at <- 1
  df[expected_na_at, 1] <- NA
  expect_equal(exploratory:::ranger.find_na("x", df), expected_na_at)

  expected_na_at <- c(1, 2, 5, 10)
  df[expected_na_at, 1] <- NA
  expect_equal(exploratory:::ranger.find_na("x", df), expected_na_at)
})


