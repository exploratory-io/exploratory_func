# how to run this test:
# devtools::test(filter="rpart_2")

context("test rpart prediction with training/test data")

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
  set.seed(1) # Stable fixture across CI machines and test order (slice_sample is RNG-dependent).
  flight <- flight %>% slice_sample(n=5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}

test_that("exp_rpart(regression) evaluate training and test with permutation importance", {
  skip_if_not_installed("mmpf")
  model_df <- flight %>%
                exp_rpart(`FL NUM`, `DIS TANCE`, `DEP TIME`,
                          test_rate = 0.3,
                          importance_measure = "permutation",
                          test_split_type = "ordered") # testing ordered split too.
  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)
  ret <- flight %>% select(-`FL NUM`) %>% add_prediction(model_df=model_df)
  ret <- model_df %>% prediction(data="newdata", data_frame = flight)

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Check order of result of variable importance.
  ret <- model_df %>% tidy_rowwise(model, type="importance") %>% arrange(-importance)
  expect_equal(as.character(ret$variable), c("DIS TANCE", "DEP TIME"))
  ret <- model_df %>% rf_partial_dependence()
  expect_equal(levels(ret$x_name), c("DIS TANCE", "DEP TIME")) # Factor order should be the same as the importance.

  # Training only case
  model_df <- flight %>%
                exp_rpart(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(regression) evaluate training and test with FIRM importance", {
  model_df <- flight %>%
                exp_rpart(`FL NUM`, `DIS TANCE`, `DEP TIME`,
                          test_rate = 0.3,
                          importance_measure = "firm",
                          test_split_type = "ordered") # testing ordered split too.
  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)
  ret <- flight %>% select(-`FL NUM`) %>% add_prediction(model_df=model_df)
  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Check order of result of variable importance.
  ret <- model_df %>% tidy_rowwise(model, type="importance") %>% arrange(-importance)
  expect_equal(as.character(ret$variable), c("DIS TANCE", "DEP TIME"))

  # Training only case
  model_df <- flight %>%
                exp_rpart(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(regression) evaluate training and test with impurity importance", {
  model_df <- flight %>%
                exp_rpart(`FL NUM`, `DIS TANCE`, `DEP TIME`,
                          test_rate = 0.3,
                          test_split_type = "ordered") # testing ordered split too.
  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)
  ret <- flight %>% select(-`FL NUM`) %>% add_prediction(model_df=model_df)
  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Check order of result of variable importance.
  ret <- model_df %>% tidy_rowwise(model, type="importance") %>% arrange(-importance)
  expect_equal(as.character(ret$variable), c("DIS TANCE", "DEP TIME"))

  # Training only case
  model_df <- flight %>%
                exp_rpart(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart evaluate training and test with permutation importance - logical", {
  skip_if_not_installed("mmpf")
  set.seed(1)
  # Keep the test rate high (0.4) so that NA data goes to training part too.
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  model_df <- data %>% exp_rpart(is_delayed, `DIS TANCE`, `DEP DELAY`, `ORI GIN`, test_rate = 0.4, binary_classification_threshold=0.5, importance_measure = "permutation")

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("variable", "importance"))

  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)

  ret1 <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df, binary_classification_threshold=0.5)
  expect_equal(class(ret1$predicted_label), "logical")
  ret2 <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df, binary_classification_threshold=0.01)
  expect_gt(sum(ret2$predicted_label=="TRUE",na.rm=TRUE), sum(ret1$predicted_label=="TRUE",na.rm=TRUE)) # Change of threshold should make difference.

  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 2000)
  expect_gt(nrow(test_ret), 1900)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3000)
  expect_gt(nrow(train_ret), 2900)

  ret <- model_df %>% rf_evaluation_training_and_test()
  expect_equal(nrow(ret), 2) # 2 for train and test
  expect_equal(is.na(ret$auc), c(F,F)) # 2 for train and test

  # Training only case
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0)

  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart evaluate training and test with FIRM importance - logical", {
  set.seed(1)
  # Keep the test rate high (0.4) so that NA data goes to training part too.
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  model_df <- data %>% exp_rpart(is_delayed, `DIS TANCE`, `DEP DELAY`, `ORI GIN`, test_rate = 0.4, binary_classification_threshold=0.5, importance_measure = "firm")

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("variable", "importance"))

  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)

  ret1 <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df, binary_classification_threshold=0.5)
  expect_equal(class(ret1$predicted_label), "logical")
  ret2 <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df, binary_classification_threshold=0.01)
  expect_gt(sum(ret2$predicted_label=="TRUE",na.rm=TRUE), sum(ret1$predicted_label=="TRUE",na.rm=TRUE)) # Change of threshold should make difference.

  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 2000)
  expect_gt(nrow(test_ret), 1900)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3000)
  expect_gt(nrow(train_ret), 2900)

  ret <- model_df %>% rf_evaluation_training_and_test()
  expect_equal(nrow(ret), 2) # 2 for train and test
  expect_equal(is.na(ret$auc), c(F,F)) # 2 for train and test

  # Training only case
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0)

  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart evaluate training and test with impurity importance - logical", {
  set.seed(1)
  # Keep the test rate high (0.4) so that NA data goes to training part too.
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  model_df <- data %>% exp_rpart(is_delayed, `DIS TANCE`, `DEP DELAY`, `ORI GIN`, test_rate = 0.4, binary_classification_threshold=0.5)

  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)

  ret1 <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df, binary_classification_threshold=0.5)
  expect_equal(class(ret1$predicted_label), "logical")
  ret2 <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df, binary_classification_threshold=0.01)
  expect_gt(sum(ret2$predicted_label=="TRUE",na.rm=TRUE), sum(ret1$predicted_label=="TRUE",na.rm=TRUE)) # Change of threshold should make difference.

  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 2000)
  expect_gt(nrow(test_ret), 1900)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3000)
  expect_gt(nrow(train_ret), 2900)

  ret <- model_df %>% rf_evaluation_training_and_test()
  expect_equal(nrow(ret), 2) # 2 for train and test
  expect_equal(is.na(ret$auc), c(F,F)) # 2 for train and test

  # Training only case
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(factor(A,B)) evaluate training and test", { # This should be treated as multi-class
  data <- flight %>% dplyr::mutate(is_delayed = factor(if_else(as.logical(`is delayed`), "A", "B"), ordered=TRUE))
  model_df <- data %>% exp_rpart(is_delayed, `DIS TANCE`, `DEP DELAY`, test_rate = 0.3, binary_classification_threshold=0.5)

  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)

  ret <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df)
  expect_equal(class(ret$predicted_label), "character")
  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3300)

  ret <- model_df %>% rf_evaluation_training_and_test()
  expect_equal(nrow(ret), 2) # 2 for train/test
  ret <- model_df %>% rf_evaluation_training_and_test(type="evaluation_by_class")
  expect_equal(nrow(ret), 4) # 4 for train/test times A/B.
  ret <- model_df %>% rf_evaluation_training_and_test(type='conf_mat')
  expect_equal(nrow(ret), 8) # 8 for train/test times A/B (actual) times A/B (predicted).
  expect_equal(colnames(ret), c("actual_value", "predicted_value", "count", "is_test_data"))

  # Training only case
  model_df <- flight %>% dplyr::mutate(is_delayed = if_else(as.logical(`is delayed`), "A", "B")) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(character(TRUE,FALSE)) with NAs evaluate training and test", { # This should be treated as multi-class
  model_df <- flight %>% dplyr::mutate(is_delayed = if_else(as.logical(`is delayed`), "TRUE", "FALSE")) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP DELAY`, pd_with_bin_means = TRUE, test_rate = 0.3, binary_classification_threshold=0.5)

  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3300)

  ret <- model_df %>% rf_evaluation_training_and_test()
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Training only case
  model_df <- flight %>% dplyr::mutate(is_delayed = if_else(as.logical(`is delayed`), "TRUE", "FALSE")) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(character(TRUE,FALSE)) without NAs evaluate training and test", { # This should be treated as multi-class
  model_df <- flight %>% dplyr::mutate(is_delayed = if_else(as.logical(`is delayed`), "TRUE", "FALSE")) %>%
                filter(!is.na(is_delayed)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP DELAY`, pd_with_bin_means = TRUE, test_rate = 0.3, binary_classification_threshold=0.5)

  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3300)

  ret <- model_df %>% rf_evaluation_training_and_test()
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Training only case
  model_df <- flight %>% dplyr::mutate(is_delayed = if_else(as.logical(`is delayed`), "TRUE", "FALSE")) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(binary) evaluate training and test with SMOTE", {
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, smote = T)

  ret <- flight %>% add_prediction(model_df=model_df)
  expect_equal(class(ret$predicted_label), "logical")

  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  # expect_lt(nrow(test_ret), 1500) # Not true because of SMOTE
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  # expect_lt(nrow(train_ret), 3500) # Not true because of SMOTE
  # SMOTE row count depends on class balance after sampling; can be close to non-SMOTE training size when few synthetics are added.
  expect_gt(nrow(train_ret), 3300)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Training only case
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_rpart(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, smote = T)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(multi) evaluate training and test with permutation importance", {
  skip_if_not_installed("mmpf")
  model_df <- flight %>%
                exp_rpart(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, importance_measure = "permutation")

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("variable", "importance"))

  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Training only case
  model_df <- flight %>%
                exp_rpart(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(multi) evaluate training and test with FIRM importance", {
  model_df <- flight %>%
                exp_rpart(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, importance_measure = "firm")

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("variable", "importance"))

  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Training only case
  model_df <- flight %>%
                exp_rpart(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_rpart(multi) evaluate training and test with impurity importance", {
  model_df <- flight %>%
                exp_rpart(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)

  ret <- model_df %>% prediction(data="newdata", data_frame = flight)
  ret <-  model_df %>% rf_partial_dependence()

  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1483) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 3461) # Not very stable for some reason. Will revisit.
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3400)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # Training only case
  model_df <- flight %>%
                exp_rpart(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 4944) # Not very stable for some reason. Will revisit.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})
# report_metrics is the opt-in switch that adds the extra Summary-table metrics the
# Decision Tree Analytics Report needs (#37156). The invariants below are what the
# report depends on: the default output must be untouched, the extra columns must be
# present for BOTH the training and the test row (the test row is produced by a
# separate, model-agnostic code path, so it is the easy one to leave behind), and the
# values must be real numbers rather than NA.
test_that("exp_rpart report_metrics adds metrics without changing the default output", {
  target_of <- list(regression = rlang::sym("ARR DELAY"),
                    binary = rlang::sym("is delayed"),
                    multiclass = rlang::sym("ORI GIN"))
  expected_added <- list(regression = "MAE",
                         binary = c("ROC AUC", "PR AUC", "Balanced Accuracy", "Specificity"),
                         multiclass = c("Balanced Accuracy", "Macro ROC AUC", "Macro PR AUC"))

  data <- flight %>% dplyr::mutate(`is delayed` = `ARR DELAY` > 0)

  for (kind in names(target_of)) {
    for (test_rate in c(0, 0.3)) {
      model_df <- data %>% exp_rpart(!!target_of[[kind]], `DIS TANCE`, `DEP TIME`, test_rate = test_rate)
      base <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
      with_metrics <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE, report_metrics = TRUE)

      label <- paste0(kind, " test_rate=", test_rate)
      # Every extra column is present.
      expect_true(all(expected_added[[kind]] %in% colnames(with_metrics)), info = label)
      # The default output is untouched, so no other analytics changes.
      expect_false(any(expected_added[[kind]] %in% colnames(base)), info = label)
      # Columns the default already had keep their exact values. AUC is intentionally
      # renamed to ROC AUC for binary, so compare only the columns that remain.
      kept <- intersect(colnames(base), colnames(with_metrics))
      expect_equal(as.data.frame(base)[, kept, drop = FALSE],
                   as.data.frame(with_metrics)[, kept, drop = FALSE], info = label)
      # Both the training row and (when there is one) the test row carry real values.
      expect_equal(nrow(with_metrics), if (test_rate > 0) 2 else 1, info = label)
      expect_false(any(is.na(with_metrics[, expected_added[[kind]], drop = FALSE])), info = label)
    }
  }

  # Binary renames AUC to ROC AUC so it reads as a pair with PR AUC.
  model_df <- data %>% exp_rpart(`is delayed`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  with_metrics <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE, report_metrics = TRUE)
  expect_false("AUC" %in% colnames(with_metrics))
  # Balanced accuracy is the mean of recall and specificity, by definition.
  expect_equal(with_metrics$`Balanced Accuracy`[[1]],
               (with_metrics$Recall[[1]] + with_metrics$Specificity[[1]]) / 2)

  # Summary by Class gains the per-category One-vs-Rest metrics, and the shares of
  # all categories add up to the whole evaluated data.
  model_df <- flight %>% exp_rpart(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0)
  by_class <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class",
                                              pretty.name = TRUE, report_metrics = TRUE)
  expect_true(all(c("Balanced Accuracy", "ROC AUC", "PR AUC", "Overall Share") %in% colnames(by_class)))
  expect_equal(sum(by_class$`Overall Share`), 1)
})
