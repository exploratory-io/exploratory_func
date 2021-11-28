# how to run this test:
# devtools::test(filter="rpart_2")

context("test rpart prediction with training/test data")

testdata_dir <- tempdir()
testdata_filename <- "airline_2013_10_tricky_v3_5k.csv" 
testdata_file_path <- paste0(testdata_dir, testdata_filename)

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  "https://www.dropbox.com/s/f47baw5f3v0xoll/airline_2013_10_tricky_v3.csv?dl=1"
} else {
  testdata_file_path
}

flight <- exploratory::read_delim_file(filepath, ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

filepath <- if (!testdata_filename %in% list.files(testdata_dir)) {
  flight <- flight %>% slice_sample(n=5000)
  write.csv(flight, testdata_file_path) # save sampled-down data for performance.
}

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

test_that("exp_rpart(character(A,B)) evaluate training and test", { # This should be treated as multi-class
  data <- flight %>% dplyr::mutate(is_delayed = if_else(as.logical(`is delayed`), "A", "B"))
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
  expect_equal(nrow(ret), 2) # 2 for train and test
  ret <- model_df %>% rf_evaluation_training_and_test(type="evaluation_by_class")
  expect_equal(nrow(ret), 4) # 2 for train and test times A/B.

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
  expect_gt(nrow(train_ret), 3400)

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
