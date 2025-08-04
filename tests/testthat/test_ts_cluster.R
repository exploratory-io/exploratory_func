# how to run this test:
# devtools::test(filter="ts_cluster")

context("test time series clustering")

testdata_dir <- tempdir()
testdata_filename <- "airline_2013_10_tricky_v3_5k.csv"
testdata_file_path <- paste0(testdata_dir, '/', testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

gdp_data <- arrow::read_parquet("https://www.dropbox.com/scl/fi/ywgdaynw4no8sjjydpdhw/GDP_Japan.parquet?rlkey=5wa3199f9pxznvn4tdsrd20e9&dl=1")


if (!testdata_filename %in% list.files(testdata_dir)) {
  set.seed(1)
  flight <- flight %>% slice_sample(n=5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}

test_that("exp_ts_cluster elbow method mode", {
  # Set max_centers to 6 to avoid known error under investigation. #25298
  model_df <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, output="model", elbow_method_mode=TRUE, max_centers=6)
  ret <- model_df %>% tidy_rowwise(model, type="elbow_method")
  expect_equal(colnames(ret), c("n_center","av_dist","iter","converged"))
})

test_that("exp_ts_cluster elbow method mode with algorithm that requires window_size", {
  model_df <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, distance='dtw_lb', output="model", centroid = 'pam', elbow_method_mode=TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="elbow_method")
  expect_equal(colnames(ret), c("n_center","av_dist","iter","converged"))
})

test_that("exp_ts_cluster model output", {
  model_df <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, output="model")
  ret <- model_df %>% tidy_rowwise(model)
  expect_equal(colnames(ret), c("FL DATE","CAR RIER","ARR DELAY","Cluster"))
  expect_equal(sort(unique(ret$Cluster)), c(1,2,3))

  ret <- model_df %>% tidy_rowwise(model, type="summary")
  ret <- model_df %>% tidy_rowwise(model, type="aggregated")
  expect_equal(colnames(ret), c("category","time","value"))
})

test_that("exp_ts_cluster basic", {
  ret <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`)
  expect_equal(colnames(ret), c("FL DATE","CAR RIER","ARR DELAY","Cluster"))
  expect_equal(sort(unique(ret$Cluster)), c(1,2,3))
})

# This currently fails. Look into it.
# test_that("exp_ts_cluster with dtw2 and sdtw_cent.", {
#   ret <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, distance="dtw2", centroid="sdtw_cent")
#   expect_equal(colnames(ret), c("FL DATE","CAR RIER","ARR DELAY","Cluster"))
#   expect_equal(sort(unique(ret$Cluster)), c(1,2,3))
# })

test_that("exp_ts_cluster with dtw_lb, where we set default window.size internally.", {
  ret <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, distance="dtw_lb")
  expect_equal(colnames(ret), c("FL DATE","CAR RIER","ARR DELAY","Cluster"))
  expect_equal(sort(unique(ret$Cluster)), c(1,2,3))
})

test_that("exp_ts_cluster with moving average", {
  ret <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, roll_mean_window=3)
  expect_equal(colnames(ret), c("FL DATE","CAR RIER","ARR DELAY","Cluster"))
  expect_equal(sort(unique(ret$Cluster)), c(1,2,3))
})

test_that("exp_ts_cluster with normalize", {
  ret <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, normalize = "center_and_scale")
  expect_equal(colnames(ret), c("FL DATE","CAR RIER","ARR DELAY","ARR DELAY_normalized","Cluster"))
  expect_equal(sort(unique(ret$Cluster)), c(1,2,3))

  ret <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, normalize = "center_and_scale", with_centroids = TRUE) # Test with centroid output.
  expect_equal((ret %>% dplyr::filter(`CAR RIER`=="Centroid 1"))$Cluster[[1]], 1) # Centroid 1 should belong to Cluster 1.
  expect_equal(colnames(ret), c("FL DATE","CAR RIER","ARR DELAY","ARR DELAY_normalized","Cluster"))
  expect_equal(sort(unique(ret$Cluster)), c(1,2,3))
})

test_that("exp_ts_cluster with aggregated number of rows by missing value column", {
  ret <- flight %>% exp_ts_cluster(`FL DATE`, , `CAR RIER`)
  expect_equal(colnames(ret), c("FL DATE","CAR RIER","Number_of_Rows","Cluster"))
  expect_equal(sort(unique(ret$Cluster)), c(1,2,3))
})

test_that("exp_ts_cluster with aggregated number of rows by specifying NULL for value column", {
  ret <- flight %>% exp_ts_cluster(`FL DATE`, NULL, `CAR RIER`)
  expect_equal(colnames(ret), c("FL DATE","CAR RIER","Number_of_Rows","Cluster"))
  expect_equal(sort(unique(ret$Cluster)), c(1,2,3))
})

test_that("exp_ts_cluster with other variables to aggregate", {
  ret <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, variables=c(`ORI GIN_mode`="ORI GIN"), funs.aggregate.variables=c("get_mode"))
  expect_equal(colnames(ret), c("FL DATE", "CAR RIER", "ARR DELAY", "ORI GIN_mode", "Cluster"))
  expect_equal(sort(unique(ret$Cluster)), c(1,2,3))
})

test_that("exp_ts_cluster with max_category_na_ratio with model output", {
  model_df <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, max_category_na_ratio=0.001, output="model") # Setting extremely low max_category_na_ratio for test.
  ret <- model_df %>% tidy_rowwise(model, type="summary")
  expect_equal(colnames(ret), c("cluster","size","av_dist","Note"))
  ret <- model_df %>% tidy_rowwise(model, type="aggregated")
})

test_that("exp_ts_cluster with max_category_na_ratio with data frame output", {
  df <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, max_category_na_ratio=0.001) # Setting extremely low max_category_na_ratio for test.
  expect_equal(colnames(df), c("FL DATE","CAR RIER","ARR DELAY","Cluster"))
  expect_equal(sort(unique(df$Cluster)), c(1,2,3))
})

test_that("exp_ts_cluster with max_category_na_ratio for step", {
  expect_error({
    ret <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, max_category_na_ratio=0) # Setting zero max_category_na_ratio for test.
  }, "There is not enough data left")
})

test_that("exp_ts_cluster with max_category_na_ratio for analytics view", {
  model_df <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, max_category_na_ratio=0, stop_for_no_data=FALSE, output="model") # Setting zero max_category_na_ratio for test.
  expect_true("data.frame" %in% class(attr(model_df$model[[1]],"aggregated_data")))
  ret <- model_df %>% tidy_rowwise(model)
  expect_equal(nrow(ret), 0)
  ret <- model_df %>% tidy_rowwise(model, type="summary")
  expect_equal(nrow(ret), 0)
})

test_that("exp_ts_cluster with elbow method mode with max_category_na_ratio for analytics view", {
  model_df <- flight %>% exp_ts_cluster(`FL DATE`, `ARR DELAY`, `CAR RIER`, max_category_na_ratio=0, elbow_method_mode=TRUE, stop_for_no_data=FALSE, output="model") # Setting zero max_category_na_ratio for test.
  ret <- model_df %>% tidy_rowwise(model, type="elbow_method")
  expect_equal(nrow(ret), 0)
  ret <- model_df %>% tidy_rowwise(model, type="summary")
  expect_equal(nrow(ret), 0)
})

test_that("exp_ts_cluster with very few clusters with elbow method mode", {
  model_df <- gdp_data %>% exp_ts_cluster(`date`, `value`, `ID`, time_unit = "quarter", fun.aggregate = mean, na_fill_type = c("extend", "previous", "extend"), with_centroids = TRUE, distance = "dtw2", centroid = "dba", normalize = "center_and_scale", output = "model", stop_for_no_data = FALSE, elbow_method_mode = TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="elbow_method")
  expect_equal(nrow(ret), 3)
  ret <- model_df %>% tidy_rowwise(model, type="summary")
  expect_equal(nrow(ret), 4)
})
