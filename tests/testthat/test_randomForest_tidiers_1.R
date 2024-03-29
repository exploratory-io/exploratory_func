# how to run this test:
# devtools::test(filter="randomForest_tidiers")

context("test tidiers for randomForest")

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

test_that("test exp_balance with character", {
  sample_data <- data.frame(
    y = c("a", "b", "b", "b", "b", "b"),
    num = runif(6)
  )
  res <- exp_balance(sample_data, y)
  expect_true("data.frame" %in% class(res))
})

test_that("test exp_balance with factor", {
  sample_data <- data.frame(
    y = factor(c("a", "b", "b", "b", "b", "b")),
    num = runif(6)
  )
  res <- exp_balance(sample_data, y)
  expect_true("data.frame" %in% class(res))
  expect_equal(class(res$y), "factor")
  expect_equal(levels(res$y), c("a","b"))
})

test_that("test exp_balance with logical", {
  sample_data <- data.frame(
    y = c(TRUE, rep(FALSE,5)),
    num = runif(6)
  )
  res <- exp_balance(sample_data, y)
  expect_true("data.frame" %in% class(res))
  expect_equal(class(res$y), "logical")
  expect_equal(any(is.na(res$y)), FALSE) # no NA is expected
})

# randomForest package had an infinite loop issue with this case,
# but ranger, which we use instead of randomForest now, does not have this problem.
# We keep testing this just in case.
test_that("test calc_feature_imp when the number of rows of classes is one", {
  sample_data <- data.frame(
    y = c("a", "b", "b", "b", "b", "c"),
    num = runif(6),
    num2 = runif(6)
  )

  model_df <- sample_data %>%
    calc_feature_imp(y, num, num2, importance_measure = "impurity")
  ret <- model_df %>% rf_importance()

  expect_equal(nrow(ret), 2)
})

test_that("test calc_feature_imp when there in only one predictor", {
  sample_data <- data.frame(
    y = c("a", "b", "b", "b", "b", "c"),
    num = runif(6)
  )

  model_df <- sample_data %>%
    calc_feature_imp(y, num, importance_measure = "impurity")
  ret <- model_df %>% rf_importance()
  # expect_equal(nrow(ret), 0) # Empty data frame should be returned.
  expect_equal(ncol(ret), 0) # For some reason it is ncol rather than nrow that is 0.
})

test_that("test calc_feature_imp predicting multi-class", {
  set.seed(0)
  nrow <- 100
  test_data <- data.frame(
    target = c(NA_character_, sample(letters[1:4], nrow-2, replace = TRUE), NA_character_),
    cat_10 = sample(c(letters[1:10], NA_character_), nrow, replace = TRUE),
    cat_25 = sample(letters[1:25], nrow, replace = TRUE),
    num_1 = runif(nrow),
    num_2 = runif(nrow),
    Group = rbinom(nrow, 2, 0.5)
  ) %>%
    # check if colname with space works
    # creating those columns in data.frame replaces spaces with .
    rename(`Tar get` = "target", `cat 10` = cat_10, `num 1` = num_1)


  # target is character
  # this case is to test rf_partial_dependence() output.
  model_df <- test_data %>%
    dplyr::group_by(Group) %>%
    calc_feature_imp(`Tar get`,
                     `cat 10`,
                     cat_25,
                      `num 1`,
                      num_2,
                      with_boruta=FALSE, max_pd_vars=3, # with_boruta must be FALSE, in this case to test rf_partial_dependence,
                                                        # because, with boruta, there will be no significant variable,
                                                        # and no partial dependence data will be returned.
                      test_rate = 0.3)
  
  ret <- model_df %>% prediction(data="training_and_test")
  conf_mat <- tidy_rowwise(model_df, model, type = "conf_mat", pretty.name = TRUE) # Old pre-5.3 way. For backward compatibility on the server.
  conf_mat <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  # ret <- model_df %>% rf_importance() # Skip this because Boruta is on.
  ret <- model_df %>% rf_partial_dependence()
  # Check the content of group column.
  expect_true(stringr::str_detect(as.character(ret$Group[1]), stringr::regex("[0-2]$")))


  # target is character
  model_df <- test_data %>%
    dplyr::group_by(Group) %>%
    calc_feature_imp(`Tar get`,
                     `cat 10`,
                     cat_25,
                      `num 1`,
                      num_2,
                      with_boruta=TRUE, max_pd_vars=3,
                      test_rate = 0.3)
  
  ret <- model_df %>% prediction(data="training_and_test")
  conf_mat <- tidy_rowwise(model_df, model, type = "conf_mat", pretty.name = TRUE) # Old pre-5.3 way. For backward compatibility on the server.
  conf_mat <- rf_evaluation_training_and_test(model_df, type = "conf_mat")
  # ret <- model_df %>% rf_importance() # Skip this because Boruta is on.
  ret <- model_df %>% rf_partial_dependence()
  # Check that format of Group column is good for our Analytics View.
  # expect_true(stringr::str_detect(as.character(ret$Group[1]), stringr::regex("[0-2] cat\\s10$|_25$"))) # This does not work since there is no significant variables.
  ret <- model_df %>% rf_evaluation(pretty.name=TRUE) # TODO test that output is different from binary classification with TRUE/FALSE
  ret <- model_df %>% rf_evaluation_by_class(pretty.name=TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="boruta")

  # make target facter and try again
  factor_test_data <- test_data %>% mutate(`Tar get`=factor(`Tar get`))
  model_df <- factor_test_data %>%
    dplyr::group_by(Group) %>%
    calc_feature_imp(`Tar get`,
                     `cat 10`,
                     cat_25,
                      `num 1`,
                      num_2, with_boruta=TRUE)

  conf_mat <- tidy_rowwise(model_df, model, type = "conf_mat", pretty.name = TRUE)
  # ret <- model_df %>% rf_importance() # Skip this because Boruta is on
  ret <- model_df %>% rf_partial_dependence()
  ret <- model_df %>% rf_evaluation(pretty.name=TRUE) # TODO test that output is different from binary classification with TRUE/FALSE
  ret <- model_df %>% rf_evaluation_by_class(pretty.name=TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="boruta")

  # make target ordered facter and try again
  ordered_factor_test_data <- test_data %>% mutate(`Tar get`=factor(`Tar get`, ordered=TRUE))
  model_df <- ordered_factor_test_data %>%
    dplyr::group_by(Group) %>%
    calc_feature_imp(`Tar get`,
                     `cat 10`,
                     cat_25,
                      `num 1`,
                      num_2, with_boruta=TRUE)

  conf_mat <- tidy_rowwise(model_df, model, type = "conf_mat", pretty.name = TRUE)
  # ret <- model_df %>% rf_importance() # Skip this because Boruta is on
  ret <- model_df %>% rf_partial_dependence()
  ret <- model_df %>% rf_evaluation(pretty.name=TRUE) # TODO test that output is different from binary classification with TRUE/FALSE
  ret <- model_df %>% rf_evaluation_by_class(pretty.name=TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="boruta")
})

test_that("test calc_feature_imp predicting logical", {
  set.seed(0)
  nrow <- 100
  test_data <- data.frame(
    target = c(NA, sample(c(TRUE,FALSE), nrow-2, replace = TRUE), NA),
    cat_10 = sample(c(letters[1:10], NA_character_), nrow, replace = TRUE),
    cat_25 = sample(letters[1:25], nrow, replace = TRUE),
    num_1 = runif(nrow),
    num_2 = runif(nrow),
    Group = rbinom(nrow, 2, 0.5)
  ) %>%
    rename(`Tar get` = "target") # check if colname with space works

  model_df <- test_data %>%
    dplyr::group_by(Group) %>%
    calc_feature_imp(`Tar get`,
                      dplyr::starts_with("cat_"),
                      num_1,
                      num_2, predictor_n = 6, with_boruta=TRUE)
  conf_mat <- model_df %>% tidy_rowwise(model, type = "conf_mat", pretty.name = TRUE)

  # ret <- model_df %>% rf_importance() Skip this because Boruta is on
  ret <- model_df %>% rf_partial_dependence()
  ret <- model_df %>% rf_evaluation(pretty.name=TRUE) # TODO test that output is different from multiclass classification
  ret <- model_df %>% rf_evaluation_by_class(pretty.name=TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="boruta")
  # factor order should be TRUE then FALSE.
  expect_equal(levels(conf_mat$actual_value)[1], "TRUE")
  expect_equal(levels(conf_mat$predicted_value)[1], "TRUE")

})

test_that("test calc_feature_imp with group_by where a group has only TRUE rows while the other have both TRUE/FALSE", {
  # if a group has only TRUE rows and factor level has both TRUE/FALSE, edarf::partial_dependence wourd error out.
  # we adjust factor level for each group to avoid it. this is a test for that logic.
  set.seed(0)
  nrow <- 100
  test_data <- data.frame(
    target = c(rep(TRUE, 30), sample(c(TRUE,FALSE), nrow - 30, replace = TRUE)), # first 30 is the group that has only TRUE rows.
    cat_10 = sample(c(letters[1:10], NA_character_), nrow, replace = TRUE),
    cat_25 = sample(letters[1:25], nrow, replace = TRUE),
    num_1 = runif(nrow),
    num_2 = runif(nrow),
    Group = c(rep(1,30), rep(0,nrow-30)) # first 30 is the group that has only TRUE rows.
  ) %>%
    rename(`Tar get` = "target") # check if colname with space works

  model_df <- test_data %>%
    dplyr::group_by(Group) %>%
    calc_feature_imp(`Tar get`,
                      dplyr::starts_with("cat_"),
                      num_1,
                      num_2, with_boruta=TRUE)

  # ret <- model_df %>% rf_importance() # Skip this because Boruta is on
  ret <- model_df %>% rf_partial_dependence()
  ret <- model_df %>% rf_evaluation(pretty.name=TRUE) # TODO test that output is different from multiclass classification
  ret <- model_df %>% rf_evaluation_by_class(pretty.name=TRUE)
  ret <- model_df %>% tidy_rowwise(model, type="boruta")
})

test_that("test randomForest with multinomial classification", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  model_ret <- build_model(test_data,
                           model_func = randomForestMulti,
                           formula = CARRIER ~ DISTANCE,
                           test_rate = 0.2)
  coef_ret <- model_coef(model_ret)
  model_stats <- model_stats(model_ret, pretty.name = TRUE)
  pred_train_ret <- prediction(model_ret, data = "training")
  pred_test_ret <- prediction(model_ret, data = "test")
  pred_test_ret <- prediction(model_ret, data = "newdata", data_frame = test_data %>% select(-CARRIER))
})

test_that("test randomForest with binary classification", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["IS AA"]] <- test_data$CARRIER == "AA" # test target column name with space
  model_ret <- build_model(test_data,
                           model_func = randomForestBinary,
                           formula = `IS AA` ~ DISTANCE,
                           test_rate = 0.2)
  coef_ret <- model_coef(model_ret)
  model_stats <- model_stats(model_ret, pretty.name = TRUE)
  pred_train_ret <- prediction_binary(model_ret, data = "training", threshold = "f_score") # test f_score which had issue with target column name with space once.
  pred_test_ret <- prediction_binary(model_ret, data = "test")
  pred_test_ret <- prediction_binary(model_ret, data = "newdata", data_frame = test_data)
})

test_that("test randomForest with regression without localImp", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  model_ret <- build_model(test_data,
                           model_func = randomForestBinary,
                           formula = IS_AA ~ DISTANCE,
                           localImp = FALSE,
                           test_rate = 0.2)
  coef_ret <- model_coef(model_ret, pretty.name = TRUE)
  stats_ret <- model_stats(model_ret)
  pred_train_ret <- prediction(model_ret, data = "training")
  pred_test_ret <- prediction(model_ret, data = "test")
  pred_test_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
})

test_that("test randomForest with regression", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["IS_AA"]] <- as.integer(test_data$CARRIER == "AA")
  model_ret <- build_model(test_data,
                           model_func = randomForestReg,
                           formula = IS_AA ~ DISTANCE,
                           localImp = TRUE,
                           test_rate = 0.3)
  coef_ret <- model_coef(model_ret, pretty.name = TRUE)
  stats_ret <- model_stats(model_ret)
  pred_train_ret <- prediction(model_ret, data = "training")
  pred_test_ret <- prediction(model_ret, data = "test")
  pred_test_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
})

test_that("test randomForest with unsupervised", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  model_ret <- build_model(test_data,
                           model_func = randomForest::randomForest,
                           formula = ~ DISTANCE,
                           test_rate = 0.3)

  coef_ret <- model_coef(model_ret)
  prediction_ret <- prediction(model_ret)
})

test_that("test randomForest with unsupervied by 3 classes", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))
  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  model_ret <- build_model(test_data,
                           model_func = randomForest::randomForest,
                           formula = ~ DISTANCE + CANCELLED,
                           ntree = 3, proximity = TRUE,
                           test_rate = 0.3)

  coef_ret <- model_coef(model_ret)
  prediction_ret <- prediction(model_ret)
})

test_that("test randomForest with multinomial classification", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["IS_AA"]] <- as.factor(test_data$CARRIER == "AA")
  model_ret <- build_model(test_data,
                           model_func = randomForestMulti,
                           formula = CARRIER ~ DISTANCE,
                           localImp = TRUE,
                           test_rate = 0.2)
  coef_ret <- model_coef(model_ret)
  model_stats <- model_stats(model_ret, pretty.name = TRUE)
  pred_train_ret <- prediction(model_ret, data = "training")
  pred_test_ret <- prediction(model_ret, data = "test")
  pred_test_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
})

test_that("test randomForest with multinomial classification", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", NA), # test NA handling
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["IS_AA"]] <- test_data$CARRIER == "AA"
  model_ret <- build_model(test_data,
                           model_func = randomForestMulti,
                           formula = CARRIER ~ poly(DISTANCE, 3),
                           localImp = TRUE,
                           test_rate = 0.2)
  coef_ret <- model_coef(model_ret)
  model_stats <- model_stats(model_ret, pretty.name = TRUE)
  pred_train_ret <- prediction(model_ret, data = "training")
  pred_test_ret <- prediction(model_ret, data = "test")
  pred_test_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
})

test_that("test evaluate_classification", {
  actual <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0)
  predicted <- c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0)
  ret <- evaluate_classification(actual, predicted, 1)
  expect_equal(class(ret), "data.frame")
})

test_that("calc_feature_map(regression) evaluate training and test", {
  model_df <- flight %>%
              dplyr::group_by(`CAR RIER`) %>%
                calc_feature_imp(`FL NUM`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)
  ret <- rf_evaluation_training_and_test(model_df)
  ret <- model_df %>% prediction_training_and_test()
})

test_that("calc_feature_map(binary) evaluate training and test", {
  model_df <- flight %>% dplyr::mutate(is_delayed = as.factor(`is delayed`)) %>%
              dplyr::group_by(`CAR RIER`) %>%
                calc_feature_imp(is_delayed, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)
  ret <- rf_evaluation_training_and_test(model_df, test_rate = 0.3)
  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class", test_rate = 0.3)

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat", test_rate = 0.3)
  ret <- model_df %>% prediction_training_and_test()
})

test_that("calc_feature_map(multi) evaluate training and test", {
  model_df <- flight %>% dplyr::group_by(`CAR RIER`) %>%
                calc_feature_imp(`ORI GIN`, `DIS TANCE`, `DEP TIME`, test_rate = 0.3)
  ret <- rf_evaluation_training_and_test(model_df, test_rate = 0.3, pretty.name = TRUE)
  ret <- rf_evaluation_training_and_test(model_df, type = "evaluation_by_class", test_rate = 0.3)

  ret <- rf_evaluation_training_and_test(model_df, type = "conf_mat", test_rate = 0.3)
  ret <- model_df %>% prediction_training_and_test()
})

test_that("calc_feature_map() error handling for predictor with single unique value", {
  expect_error({
    model_df <- flight %>% mutate(Const=1) %>%
      calc_feature_imp(`FL NUM`, Const, test_rate = 0.3)
  }, "Invalid Predictors: Only one unique value.")
})
