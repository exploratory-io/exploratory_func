# how to run this test:
# devtools::test(filter="rpart_3")
# This is same test as test_rpart_2.R with group_by.

context("test tidiers for rpart. (trainig/test with group_by)")

testdata_dir <- tempdir()
testdata_filename <- "airline_2013_10_tricky_v3.csv" 
testdata_file_path <- paste0(testdata_dir, '/', testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv"
} else {
  testdata_file_path
}
if (!exists("flight_downloaded")) {
  flight_downloaded <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()
  write.csv(flight_downloaded, testdata_file_path)
  flight <- flight_downloaded
} else {
  flight <- flight_downloaded
}

# Add group_by. Cases without group_by is covered in test_randomForest_tidiers_3.R.
flight <- flight %>% slice_sample(n=5000) %>% group_by(`CAR RIER`)






