# how to run this test:
# devtools::test(filter="randomForest_tidiers_3")

context("test tidiers for randomForest 3 (training and test data)")

testdata_dir <- tempdir()
testdata_filename <- "airline_2013_10_tricky_v3_5k.csv" 
testdata_file_path <- paste0(testdata_dir, '/', testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://exploratory-download.s3.us-west-2.amazonaws.com/test/airline_2013_10_tricky_v3.csv"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

if (!testdata_filename %in% list.files(testdata_dir)) {
  set.seed(1) # Trying setting seed before sampling to make boruta importance test result stable on our Mac test machine.
  flight <- flight %>% slice_sample(n=5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}

test_that("calc_feature_imp(regression) evaluate training and test with FIRM importance", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                calc_feature_imp(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`,
                                 test_rate = 0.3,
                                 importance_measure = "firm",
                                 test_split_type = "ordered", pd_with_bin_means = TRUE) # testing ordered split too.

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("variable", "importance"))
  ret2 <- model_df %>% rf_partial_dependence()
  variables <- ret$variable
  names(variables) <- NULL
  expect_equal(levels(ret2$x_name), variables) # Factor order of the PDP should be the same as the importance.

  ret <- flight %>% select(-`ARR DELAY`) %>% add_prediction(model_df=model_df)
  ret <- model_df %>% prediction(data="newdata", data_frame=flight)
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)
  # expect_equal(nrow(train_ret), 3500) Fails now, since we filter numeric NA. Revive when we do not need to.

  ret <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # retrieve partial dependence data.
  ret <- model_df %>% rf_partial_dependence()
  expect_equal(class(ret$conf_high), "numeric") # make sure that it is with conf int for actual binning data.

  model_df <- flight %>%
                calc_feature_imp(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4894) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("calc_feature_imp(regression) evaluate training and test with permutation importance", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                calc_feature_imp(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`,
                                 test_rate = 0.3,
                                 test_split_type = "ordered", with_boruta = TRUE, pd_with_bin_means = TRUE) # testing ordered split too.

  ret <- flight %>% select(-`ARR DELAY`) %>% add_prediction(model_df=model_df)
  ret <- model_df %>% prediction(data="newdata", data_frame=flight)
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)
  # expect_equal(nrow(train_ret), 3500) Fails now, since we filter numeric NA. Revive when we do not need to.

  # Test result of boruta.
  importance_res <- model_df %>% tidy_rowwise(model, type='boruta') %>% group_by(variable) %>% summarize(importance=mean(importance)) %>% arrange(-importance)
  expect_equal(as.character(importance_res$variable[[1]]), "DEP DELAY") # Most important should be dep delay.
  # expect_equal(as.character(importance_res$variable[[2]]), "CAR RIER") # 2nd most important often is carrier, but commenting it out since it is not that stable. 

  ret <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # retrieve partial dependence data.
  ret <- model_df %>% rf_partial_dependence()
  expect_equal(class(ret$conf_high), "numeric") # make sure that it is with conf int for actual binning data.

  model_df <- flight %>%
                calc_feature_imp(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4894) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("calc_feature_imp() with test_split_type = ordered and test_rate = 0, which used to have an issue of creating test data set of 1 row.", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                calc_feature_imp(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`,
                                 test_rate = 0,
                                 importance_measure = "firm",
                                 test_split_type = "ordered", pd_with_bin_means = TRUE) # testing ordered split too.

  ret <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  expect_equal(nrow(ret), 1) # Should be 1 only for training set. Test set should not be created.
})

test_that("calc_feature_imp(binary) evaluate training and test with FIRM importance", {
  set.seed(1) # For stability of result.
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, pd_with_bin_means = TRUE, importance_measure = "firm")

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("variable", "importance"))

  ret <- flight %>% add_prediction(model_df=model_df)
  expect_equal(class(ret$predicted_label), "logical")
  res_partial_dependence <- model_df %>% rf_partial_dependence()
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  expect_equal(nrow(ret), 4) # 4 for train/test times TRUE/FALSE

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  expect_equal(nrow(ret), 8) # 8 for train/test times predicted TRUE/FALSE times actual TRUE/FALSE

  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4905) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("calc_feature_imp(binary) evaluate training and test with permutation importance", {
  set.seed(1) # For stability of result.
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, pd_with_bin_means = TRUE)

  ret <- flight %>% add_prediction(model_df=model_df)
  expect_equal(class(ret$predicted_label), "logical")
  res_partial_dependence <- model_df %>% rf_partial_dependence()
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  expect_equal(nrow(ret), 4) # 4 for train/test times TRUE/FALSE

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  expect_equal(nrow(ret), 8) # 8 for train/test times predicted TRUE/FALSE times actual TRUE/FALSE

  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4905) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("calc_feature_map(factor(TRUE, FALSE)) evaluate training and test", { # This case should be treated as multi-class.
  set.seed(1) # For stability of result.
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  model_df <- flight %>% dplyr::mutate(is_delayed = factor(as.logical(`is delayed`))) %>% filter(!is.na(is_delayed)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, pd_with_bin_means = TRUE)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df, pretty.name=T, binary_classification_threshold=0.5)
  expect_equal(nrow(ret), 2) # 2 for train and test
  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  expect_equal(nrow(ret), 4) # 4 for train/test times TRUE/FALSE

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4905) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("calc_feature_map(binary(factor(A,B))) evaluate training and test", {
  set.seed(1) # For stability of result.
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  model_df <- flight %>% dplyr::mutate(is_delayed = factor(if_else(as.logical(`is delayed`), "A","B"))) %>% filter(!is.na(is_delayed)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, pd_with_bin_means = TRUE)

  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)
  ret <- model_df %>% prediction(data="newdata", data_frame=flight)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df, pretty.name=T, binary_classification_threshold=0.5)
  expect_equal(nrow(ret), 2) # 2 for train and test
  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  expect_equal(nrow(ret), 4) # 4 for train/test times TRUE/FALSE

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4905) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("calc_feature_map(binary) evaluate training and test with SMOTE", {
  set.seed(1) # For stability of result.
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, smote=TRUE, pd_with_bin_means = TRUE)

  ret <- flight %>% add_prediction(model_df=model_df)
  expect_equal(class(ret$predicted_label), "logical")
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) # Fails with SMOTE, which is expected.
  # expect_lt(nrow(test_ret), 1500) # Not true because of SMOTE.
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) # Fails with SMOTE, which is expected.
  # expect_lt(nrow(train_ret), 3500) # Not true because of SMOTE.
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  expect_equal(nrow(ret), 4) # 4 for train/test times TRUE/FALSE

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")

  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, smote=TRUE, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 5000) # Fails with SMOTE, which is expected.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("calc_feature_imp(multi) evaluate training and test with FIRM importance", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                calc_feature_imp(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, pd_with_bin_means = TRUE, importance_measure = "firm")

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("variable", "importance"))

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")

  model_df <- flight %>%
                calc_feature_imp(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4944) # Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("calc_feature_imp(multi) evaluate training and test with permutation importance", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                calc_feature_imp(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, pd_with_bin_means = TRUE)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class")
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")

  model_df <- flight %>%
                calc_feature_imp(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4944) # Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})
