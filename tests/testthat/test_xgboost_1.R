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

test_that("exp_xgboost(regression) evaluate training and test with FIRM importance", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                exp_xgboost(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`, `FL DATE`,
                            predictor_funs=list(`CAR RIER`="none", `ORI GIN`="none", `DEP DELAY`="none", `AIR TIME`="none", list(`FL DATE_y`="year", `FL DATE_m`="monname", `FL DATE_dom`="day", `FL DATE_dow`="wday")),
                            test_rate = 0.3,
                            test_split_type = "ordered", pd_with_bin_means = TRUE, # testing ordered split too.
                            watchlist_rate = 0.1,
                            importance_measure = "firm")
  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)

  ret <- flight %>% select(-`ARR DELAY`) %>% add_prediction(model_df=model_df)
  ret <- model_df %>% prediction(data="newdata", data_frame=flight)

  ret <- model_df %>% tidy_rowwise(model, type="evaluation_log")
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3300)
  # expect_equal(nrow(train_ret), 3500) Fails now, since we filter numeric NA. Revive when we do not need to.

  # Check result of variable importance 
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(as.character(ret$variable[[1]]), "DEP DELAY") # Most important should be dep delay.

  ret <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # retrieve partial dependence data.
  ret <- model_df %>% rf_partial_dependence()
  expect_equal(class(ret$conf_high), "numeric") # make sure that it is with conf int for actual binning data.

  model_df <- flight %>%
                exp_xgboost(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4894) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_xgboost(regression) evaluate training and test with permutation importance", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                exp_xgboost(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`, `FL DATE`,
                            predictor_funs=list(`CAR RIER`="none", `ORI GIN`="none", `DEP DELAY`="none", `AIR TIME`="none", list(`FL DATE_y`="year", `FL DATE_m`="monname", `FL DATE_dom`="day", `FL DATE_dow`="wday")),
                                 test_rate = 0.3,
                                 test_split_type = "ordered", pd_with_bin_means = TRUE, # testing ordered split too.
                                 watchlist_rate = 0.1)
  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)

  ret <- flight %>% select(-`ARR DELAY`) %>% add_prediction(model_df=model_df)
  ret <- model_df %>% prediction(data="newdata", data_frame=flight)

  ret <- model_df %>% tidy_rowwise(model, type="evaluation_log")
  ret <- model_df %>% prediction(data="training_and_test")
  test_ret <- ret %>% filter(is_test_data==TRUE)
  # expect_equal(nrow(test_ret), 1500) Fails now, since we filter numeric NA. Revive when we do not need to.
  expect_lt(nrow(test_ret), 1500)
  expect_gt(nrow(test_ret), 1400)
  train_ret <- ret %>% filter(is_test_data==FALSE)
  expect_lt(nrow(train_ret), 3500)
  expect_gt(nrow(train_ret), 3300)
  # expect_equal(nrow(train_ret), 3500) Fails now, since we filter numeric NA. Revive when we do not need to.

  # Check result of variable importance 
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(as.character(ret$variable[[1]]), "DEP DELAY") # Most important should be dep delay.
  ret2 <- model_df %>% rf_partial_dependence()
  variables <- ret$variable
  names(variables) <- NULL
  expect_equal(levels(ret2$x_name), variables) # Factor order of the PDP should be the same as the importance.

  ret <- rf_evaluation_training_and_test(model_df, pretty.name = TRUE)
  expect_equal(nrow(ret), 2) # 2 for train and test

  # retrieve partial dependence data.
  ret <- model_df %>% rf_partial_dependence()
  expect_equal(class(ret$conf_high), "numeric") # make sure that it is with conf int for actual binning data.

  model_df <- flight %>%
                exp_xgboost(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4894) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_xgboost - regression - evaluate training and test with locale conversion", {
  set.seed(1) # For stability of result.
  orig_locale <- Sys.getlocale("LC_TIME")
  tryCatch({
    # Set Japanese locale for test. 
    Sys.setlocale("LC_TIME", "ja_JP.UTF-8")
    model_df <- flight %>%
                  exp_xgboost(`ARR DELAY`, `CAR RIER`, `ORI GIN`, `DEP DELAY`, `AIR TIME`, `FL DATE`,
                              predictor_funs=list(`CAR RIER`="none", `ORI GIN`="none", `DEP DELAY`="none", `AIR TIME`="none", list(`FL DATE_y`="year", `FL DATE_m`="monname", `FL DATE_dom`="day", `FL DATE_dow`="wday")),
                                   test_rate = 0.3,
                                   test_split_type = "ordered", pd_with_bin_means = TRUE, # testing ordered split too.
                                   watchlist_rate = 0.1)

    if (Sys.info()[["sysname"]] == "Windows") {
      Sys.setlocale("LC_TIME", "English_United States.1252")
    }
    else {
      Sys.setlocale("LC_TIME", "en_US.UTF-8")
    }

    ret <- flight %>% select(-`ARR DELAY`) %>% add_prediction(model_df=model_df)
    expect_equal(nrow(ret), 5000)
  },
  finally= {
    Sys.setlocale("LC_TIME", orig_locale)
  })
})

test_that("exp_xgboost evaluate training and test with FIRM importance - binary", {
  set.seed(1) # For stability of result.
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  model_df <- data %>% exp_xgboost(is_delayed, `DIS TANCE`, `DEP TIME`,
                            predictor_funs=list(`DIS TANCE`="none", `DEP TIME`="none"),
                            test_rate = 0.3, pd_with_bin_means = TRUE,
                            importance_measure = "firm")

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("variable", "importance"))

  ret1 <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df, binary_classification_threshold=0.5)
  expect_equal(class(ret1$predicted_label), "logical")
  ret2 <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df, binary_classification_threshold=0.01)
  expect_gt(sum(ret2$predicted_label==TRUE,na.rm=TRUE), sum(ret1$predicted_label==TRUE,na.rm=TRUE)) # Change of threshold should make difference.
  ret <- model_df %>% prediction(data="newdata", data_frame=flight)

  ret <- rf_evaluation_training_and_test(model_df, binary_classification_threshold = 0.5)
  expect_true(all(c("auc", "f_score", "accuracy_rate", "misclassification_rate", "precision", "recall") %in% colnames(ret)))
  expect_equal(nrow(ret), 2) # 2 for train and test
  expect_gt(ret$auc[[1]], 0.5) # If this is not true, TRUE/FALSE may be reverted.

  #ret <- model_df %>% glance_rowwise(model)
  ret <- model_df %>% tidy_rowwise(model, type="conf_mat")
  ret <- model_df %>% tidy_rowwise(model, type="partial_dependence")

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

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  expect_equal(nrow(ret), 8) # 8 for training/test * actual(TRUE/FALSE) * predicted(TRUE/FALSE)
  expect_equal(n_distinct(ret$is_test_data), 2) # There should be both training and test.

  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_xgboost(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4905) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train

  # ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class") # Not implemented.
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("exp_xgboost evaluate training and test with permutation importance - binary", {
  set.seed(1) # For stability of result.
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  data <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`))
  model_df <- data %>% exp_xgboost(is_delayed, `DIS TANCE`, `DEP TIME`,
                            predictor_funs=list(`DIS TANCE`="none", `DEP TIME`="none"),
                            test_rate = 0.3, pd_with_bin_means = TRUE)

  ret1 <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df, binary_classification_threshold=0.5)
  expect_equal(class(ret1$predicted_label), "logical")
  ret2 <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df, binary_classification_threshold=0.01)
  expect_gt(sum(ret2$predicted_label==TRUE,na.rm=TRUE), sum(ret1$predicted_label==TRUE,na.rm=TRUE)) # Change of threshold should make difference.
  ret <- model_df %>% prediction(data="newdata", data_frame=flight)

  ret <- rf_evaluation_training_and_test(model_df, binary_classification_threshold = 0.5)
  expect_true(all(c("auc", "f_score", "accuracy_rate", "misclassification_rate", "precision", "recall") %in% colnames(ret)))
  expect_equal(nrow(ret), 2) # 2 for train and test
  expect_gt(ret$auc[[1]], 0.5) # If this is not true, TRUE/FALSE may be reverted.

  #ret <- model_df %>% glance_rowwise(model)
  ret <- model_df %>% tidy_rowwise(model, type="conf_mat")
  ret <- model_df %>% tidy_rowwise(model, type="partial_dependence")
  ret <- model_df %>% tidy_rowwise(model, type="importance")

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

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  expect_equal(nrow(ret), 8) # 8 for training/test * actual(TRUE/FALSE) * predicted(TRUE/FALSE)
  expect_equal(n_distinct(ret$is_test_data), 2) # There should be both training and test.

  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_xgboost(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4905) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train

  # ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class") # Not implemented.
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("exp_xgboost - factor of TRUE/FALSE - evaluate training and test", { # This case should be treated as multi-class.
  set.seed(1) # For stability of result.
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  data <- flight %>% dplyr::mutate(is_delayed = factor(as.logical(`is delayed`))) %>% filter(!is.na(is_delayed))
  model_df <- data %>% exp_xgboost(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, pd_with_bin_means = TRUE)

  ret <- model_df %>% prediction(data="training_and_test", pretty.name=TRUE)

  ret <- data %>% select(-is_delayed) %>% add_prediction(model_df=model_df)
  expect_true(all(c("predicted_probability_FALSE","predicted_probability_TRUE","predicted_label","predicted_probability") %in% colnames(ret)))
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
  #ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class") # Not implemented.
  #expect_equal(nrow(ret), 4) # 4 for train/test times TRUE/FALSE

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_xgboost(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4905) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train

  #ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class") # Not implemented.
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("exp_xgboost(binary(factor(A,B))) evaluate training and test with group_by", {
  set.seed(1) # For stability of result.
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  flight <- flight %>% group_by(`is UA or AA`)
  model_df <- flight %>% dplyr::mutate(is_delayed = factor(if_else(as.logical(`is delayed`), "A","B"))) %>% filter(!is.na(is_delayed)) %>%
                exp_xgboost(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, pd_with_bin_means = TRUE)
  ret <- rf_evaluation_training_and_test(model_df, pretty.name=T, binary_classification_threshold=0.5)
  expect_true(all(ret$`Data Type` == rep(c("Training", "Test"),3)))
})

test_that("exp_xgboost(binary(factor(A,B))) evaluate training and test", {
  set.seed(1) # For stability of result.
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  model_df <- flight %>% dplyr::mutate(is_delayed = factor(if_else(as.logical(`is delayed`), "A","B"))) %>% filter(!is.na(is_delayed)) %>%
                exp_xgboost(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, pd_with_bin_means = TRUE)

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
  ret <- model_df %>% rf_partial_dependence()
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_xgboost(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4905) # Less than 5000 because of NAs in the target variable. Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train

  #ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class") # Not implemented.
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("exp_xgboost(binary) evaluate training and test with SMOTE", {
  set.seed(1) # For stability of result.
  # `is delayed` is not logical for some reason.
  # To test binary prediction, need to cast it into logical.
  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_xgboost(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, smote=TRUE, pd_with_bin_means = TRUE)

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

  #ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class") # Not implemented.
  #expect_equal(nrow(ret), 4) # 4 for train/test times TRUE/FALSE

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")

  model_df <- flight %>% dplyr::mutate(is_delayed = as.logical(`is delayed`)) %>%
                exp_xgboost(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0, smote=TRUE, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  # expect_equal(nrow(train_ret), 5000) # Fails with SMOTE, which is expected.

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train

  #ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class") # Not implemented.
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
})

test_that("exp_xgboost(multi) evaluate training and test with FIRM importance", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                exp_xgboost(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, pd_with_bin_means = TRUE,
                            importance_measure = "firm")

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

  #ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class") # Not implemented.
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")

  model_df <- flight %>%
                exp_xgboost(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4944) # Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})

test_that("exp_xgboost(multi) evaluate training and test with FIRM importance with groups", {
  set.seed(1) # For stability of result.
  # By grouping with `is UA or AA`, and using `DEST STATE ABR`, testing the case where there are unused target factor levels within groups. #965
  flight <- flight %>% group_by(`is UA or AA`)
  model_df <- flight %>%
                exp_xgboost(`DEST STATE ABR`, `DIS TANCE`, `DEP TIME`, `ORIGIN STATE ABR`, test_rate = 0.3, pd_with_bin_means = TRUE,
                            importance_measure = "firm")

  # Check variable importance output.
  ret <- model_df %>% tidy_rowwise(model, type="importance")
  expect_equal(colnames(ret), c("is UA or AA", "variable", "importance"))

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
  expect_equal(nrow(ret), 6) # 2 for train and test * 3 groups

  #ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class") # Not implemented.
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")

  model_df <- flight %>%
                exp_xgboost(`DEST STATE ABR`, `DIS TANCE`, `DEP TIME`, `ORIGIN STATE ABR`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4944) # Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 3) # 1 for train * 3 groups
})

test_that("exp_xgboost(multi) evaluate training and test with permutation importance", {
  set.seed(1) # For stability of result.
  model_df <- flight %>%
                exp_xgboost(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3, pd_with_bin_means = TRUE)

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

  #ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class") # Not implemented.
  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat")

  model_df <- flight %>%
                exp_xgboost(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0, pd_with_bin_means = TRUE)
  ret <- model_df %>% prediction(data="training_and_test")
  train_ret <- ret %>% filter(is_test_data==FALSE)
  #expect_equal(nrow(train_ret), 4944) # Linux seems to have different result. Work around for now.
  expect_lt(nrow(train_ret), 5000)

  ret <- rf_evaluation_training_and_test(model_df)
  expect_equal(nrow(ret), 1) # 1 for train
})
