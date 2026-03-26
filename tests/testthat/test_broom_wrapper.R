context("test broom wrappers")
set.seed(0)
test_df <- data.frame(vec1 = seq(10), vec2 = seq(10), random = runif(10, min=0, max=10))

test_that("test data frame prediction by xgboost with group", {
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
                         row.names = c(NA, -10L), class = c("tbl_df", "tbl", "data.frame")) %>%
    dplyr::slice(1)

  model_ret <- train_data %>%
    dplyr::group_by(sex) %>%
    dplyr::mutate(`education-num` = as.numeric(`education-num`)) %>%
    dplyr::rename(`hours per week` = `hours-per-week`) %>%
    dplyr::select(age, `hours per week`, `capital-loss`, `capital-gain`, relationship, `education-num`) %>%
    build_model(
      model_func = xgboost_reg,
      formula = age ~ `hours per week`+`capital-loss`+`capital-gain`+relationship+`education-num`,
      nrounds = 5,
      sparse = FALSE
    )

  test_data[["education-num"]] <- as.numeric(test_data[["education-num"]])
  test_data <- test_data %>% dplyr::rename(`hours per week` = `hours-per-week`)
  prediction_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
  expect_true(!"source.data" %in% colnames(prediction_ret))
})

test_that("test data frame prediction by xgboost with group", {
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
  model_ret <- train_data %>%
    dplyr::group_by(sex) %>%
    dplyr::mutate(`education-num` = as.numeric(`education-num`)) %>%
    dplyr::select(age, `hours-per-week`, `capital-loss`, `capital-gain`, relationship, `education-num`) %>%
    build_model(
      model_func = xgboost_reg,
      formula = age ~ .,
      nrounds = 5,
      sparse = FALSE
    )

  test_data[["education-num"]] <- as.numeric(test_data[["education-num"]])
  prediction_ret <- prediction(model_ret, data = "newdata", data_frame = test_data)
  expect_true(all(!is.na(prediction_ret$predicted_value)))

  model_ret2 <- train_data %>%
    dplyr::filter(sex == "Male") %>%
    dplyr::select(-sex) %>%
    dplyr::mutate(`education-num` = as.numeric(`education-num`)) %>%
    dplyr::select(age, `hours-per-week`, `capital-loss`, `capital-gain`, relationship, `education-num`) %>%
    build_model(
      model_func = xgboost_reg,
      formula = age ~ .,
      nrounds = 5,
      sparse = FALSE
    )

  prediction_ret2 <- prediction(model_ret2, data = "newdata", data_frame = test_data)
  prediction_ret3 <- model_ret %>%
    dplyr::filter(sex == "Male") %>%
    prediction(data = "newdata", data_frame = test_data)
  expect_equal(prediction_ret3$predicted_value[!is.na(prediction_ret3$predicted_value)], prediction_ret2$predicted_value[!is.na(prediction_ret2$predicted_value)])
})

