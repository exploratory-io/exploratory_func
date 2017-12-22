context("test build_multinom")

test_that("test another data frame prediction by multinom", {
  train_data <- structure(list(age = c(66L, 44L, 21L, 78L, 28L, 40L, 61L, 60L,
                                       43L, 49L, 52L, 25L, 58L, 46L, 40L, 32L, 22L, 23L, 17L, 24L),
                               workclass = c("Local-gov", "Private", "Private", NA, "Private",
                                             "State-gov", "Private", "Private", "Local-gov", "Private",
                                             "Local-gov", "Private", "Private", "Self-emp-inc", "Private",
                                             "Self-emp-not-inc", "Private", "Private", "Private", "Private"),
                               education = c("7th-8th", "Masters", "Some-college", "Bachelors",
                                             "7th-8th", "Some-college", "HS-grad", "Some-college", "Some-college",
                                             "HS-grad", "Prof-school", "Masters", "HS-grad", "Some-college",
                                             "HS-grad", "Bachelors", "HS-grad", "Prof-school", "11th",
                                             "Some-college"),
                               `education-num` = c("4", "14", NA, "13",
                                                   "4", NA, "9", NA, NA, "9", NA, "14", "9", NA, "9", "13",
                                                   "9", NA, "7", NA),
                               `marital-status` = c("Widowed", "Divorced",
                                                    "Never-married", "Husband", "Divorced", "Never-married",
                                                    "Married-civ-spouse", "Widowed", "Married-civ-spouse", "Married-civ-spouse",
                                                    "Husband", "Never-married", "Married-civ-spouse",
                                                    "Married-civ-spouse", "Husband", "Divorced", "Husband",
                                                    "Never-married", "Husband", "Married-civ-spouse"),
                               occupation = c("Other-service", "Exec-managerial", "Sales",
                                              NA, "Other-service", "Adm-clerical", "Other-service", "Sales",
                                              "Transport-moving", "Adm-clerical", "Protective-serv", "Prof-specialty",
                                              "Craft-repair", "Exec-managerial", "Machine-op-inspct", "Exec-managerial",
                                              "Other-service", "Farming-fishing", "Other-service", "Craft-repair"),
                               relationship = c("Not-in-family", "Husband", "Not-in-family",
                                                "Unmarried", "Unmarried", "Not-in-family", "Husband", "Unmarried",
                                                "Unmarried", "Husband", "Husband", "Wife", "Not-in-family",
                                                "Husband", "Husband", "Unmarried", "Own-child", "Not-in-family",
                                                "Own-child", "Not-in-family"),
                               race = c("White", "White", "White",
                                        "Black", "White", "White", "Black", "White", "White", "White",
                                        "White", "White", "White", "White", "White", "Black", "White",
                                        "White", "White", "White"),
                               sex = c("Female", "Male", "Male",
                                       "Male", "Female", "Female", "Male", "Female", "Male", "Female",
                                       "Male", "Male", "Male", "Male", "Male", "Female", "Female",
                                       "Female", "Male", "Male"),
                               `capital-gain` = c(0, 10520, 0, 0, 0, 0, 0, 0, 0, 0, 3137, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                               `capital-loss` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                               `hours-per-week` = c(20, 45, 40, 3, 50, 38, 40, 27, 17, 30, 42, 30, 40, 40, 40, 30, 35, 50, 12, 65),
                               `native-country` = c("United-States", "United-States", "United-States", "United-States", "United-States",
                                                    "United-States", "Mexico", "United-States", "United-States", "Mexico",
                                                    "United-States", "Mexico", "United-States", "United-States", "United-States",
                                                    "United-States", "United-States", "United-States", "United-States", "United-States"),
                               is_greater_than_50k = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                                                       FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  ),
  .Names = c("age", "workclass", "education", "education-num", "marital-status", "occupation", "relationship",
             "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "is_greater_than_50k"),
  row.names = c(NA, -20L), class = c("tbl_df", "tbl", "data.frame"))

  test_data <- structure(list(age = c(29L, 42L, 41L, 28L, 26L, 40L, 25L, 19L,
                                      40L, 29L),
                              workclass = c("Private", "Private", "Private", "Private",
                                            "Private", "Local-gov", "Private", "Private", "Private", "Private"
                              ),
                              education = c("HS-grad", "Bachelors", "HS-grad", "HS-grad",
                                            "Some-college", "HS-grad", "HS-grad", "HS-grad", "Prof-school",
                                            "HS-grad"),
                              `education-num` = c("9", "13", "9", "9", NA, "9", "9", "9", "15", "9"),
                              `marital-status` = c("Divorced", "Married-civ-spouse", "Married-civ-spouse",
                                                   "Husband", "Never-married", "Never-married",
                                                   "Never-married", "Never-married", "Married-civ-spouse",
                                                   "Married-civ-spouse"),
                              occupation = c("Adm-clerical", "Tech-support", "Machine-op-inspct", "Adm-clerical",
                                             "Exec-managerial", "Adm-clerical", "Adm-clerical", "Other-service",
                                             "Craft-repair", "Craft-repair"
                              ),
                              relationship = c("Unmarried", "Husband", "Husband", "Husband", "Not-in-family",
                                               "Own-child", "Not-in-family", "Own-child", "Husband", "Husband"
                              ),
                              race = c("White", "White", "White", "White", "White", "White", "Black", "White",
                                       "White", "White"
                              ),
                              sex = c("Female", "Male", "Male", "Male", "Male", "Female", "Male", "Female",
                                      "Male", "Male"),
                              `capital-gain` = c(0, 0, 0, 0, 0, 0, 0, 0, 5178, 0),
                              `capital-loss` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                              `hours-per-week` = c(50, 45, 40, 40, 40, 40, 40, 40, 40, 60),
                              `native-country` = c("United-States", "United-States", "United-States", "United-States",
                                                   "United-States", "United-States", "United-States", "United-States",
                                                   "United-States", "United-States"),
                              is_greater_than_50k = c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
                         .Names = c("age", "workclass", "education", "education-num", "marital-status", "occupation", "relationship",
                                    "race", "sex", "capital-gain", "capital-loss", "hours-per-week", "native-country", "is_greater_than_50k"
                         ),
                         row.names = c(NA, -10L), class = c("tbl_df", "tbl", "data.frame"))
  train_data <- train_data %>% rename(`capital gain`=`capital-gain`)
  model_ret <- train_data %>%
    dplyr::group_by(sex) %>%
    dplyr::mutate(`education-num` = as.numeric(`education-num`)) %>%
    dplyr::select(age, `hours-per-week`, `capital-loss`, `capital gain`, relationship, `education-num`) %>%
    build_multinom(formula = relationship ~ age+`hours-per-week`+`capital-loss`+`capital gain`+`education-num`)

  expect_error({
    prediction(model_ret, data = "newdata", data_frame = test_data)
  })

  test_data[["education-num"]] <- as.numeric(test_data[["education-num"]])
  test_data <- test_data %>% rename(`capital gain`=`capital-gain`)
  prediction_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
  expect_true(nrow(prediction_ret) > 1)
})

test_that("test nnet build_model", {
  test_data <- data.frame(
    label = c(rep(letters[1:3], 12), NA, "c", "c"),
    num = seq(39),
    num2 = 39 - seq(39),
    weight = seq(39)/39,
    term = rep(letters[1:3], each = 13),
    stringsAsFactors = FALSE
  )

  model_df <- build_multinom(
    test_data,
    formula = label ~ num + num2,
    group_cols = "term",
    weights = weight,
    test_rate = 0.4)
  expect_true(any(colnames(model_df) %in% "term.group"))

  # TODO: This test is commented out because
  # model_coef to multinom model fails with unknown reason
  # because of dplyr upgrade to 0.7.1
  #coef_ret <- model_coef(model_df, conf_int = "default")
  stats_ret <- model_stats(model_df)

  #expect_equal(nrow(coef_ret), 18)
  #expect_true(any(colnames(coef_ret) %in% "conf_low"))
  #expect_true(any(colnames(coef_ret) %in% "conf_high"))

  expect_equal(nrow(stats_ret), 3)


  prediction_training_ret <- prediction(model_df, data = "training")
  prediction_ret <- prediction(model_df, data = "test")
  expect_equal(levels(prediction_ret$predicted_label), c("c", "a", "b"))
  expect_equal(levels(prediction_ret$predicted_label), levels(prediction_ret$label))

  evaluation_ret <- evaluate_multi(prediction_ret, predicted_label, label)

  expect_equal(nrow(evaluation_ret), 3)
  expect_equal(ncol(evaluation_ret), 5)

})

test_that("test nnet build_model", {
  test_data <- data.frame(
    label = c(rep(letters[1:3], 12), NA, "a", "a"),
    num = seq(39),
    num2 = 39 - seq(39),
    weight = seq(39)/39,
    term = rep(letters[1:3], each = 13),
    stringsAsFactors = FALSE
  )

  test_data <- test_data %>% dplyr::filter(label %in% c("a", "b"))

  model_df <- build_multinom(
    test_data,
    formula = label ~ num + num2,
    group_cols = "term",
    # weights = weight,
    test_rate = 0.4)

  # these should work without error to 2 class classification
  prediction_train_ret <- prediction(model_df, data = "training")
  prediction_test_ret <- prediction(model_df, data = "test")

})

test_that("test group error message", {
  test_data <- data.frame(
    label = c(rep(letters[1:3], 12), NA, "a", "a"),
    num = seq(39),
    num2 = 39 - seq(39),
    weight = seq(39)/39,
    term = rep(letters[1:3], each = 13),
    stringsAsFactors = FALSE
  )

  test_data <- test_data %>%
    dplyr::filter(label %in% c("a", "b")) %>%
    dplyr::group_by(term)
  expect_error({
    build_multinom(
      test_data,
      formula = term ~ num + num2,
      # weights = weight,
      test_rate = 0.4)
  }, "grouped column is used \\(term\\)")
})


test_that("test group error message", {
  test_data <- data.frame(
    label = c(rep(letters[1:3], 12), NA, "b", "c"),
    num = seq(39),
    num2 = 39 - seq(39),
    weight = seq(39)/39,
    term = rep(letters[1:3], each = 13),
    stringsAsFactors = FALSE
  )

  model_df <- build_multinom(test_data, formula = label ~ num + num2, test_rate = 0.4)

  prediction_ret <- prediction(model_df)

  expect_equal(levels(prediction_ret[["label"]]), c("b", "c", "a"))

})


