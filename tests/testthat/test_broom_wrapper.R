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

test_that("test augment_lm newdata argument", {
  result <- (
    test_df
    %>%  build_lm(vec1~random)
    %>%  augment_lm(model, newdata=data.frame(random=c(1,3)))
  )
  expect_equal(dim(result)[[1]], 2)
})

test_that("test if tidy_lm arguments work", {
  result <- (
    test_df
    %>%  build_lm(vec1~random)
    %>%  tidy_lm(model)
  )
  result_ <- (
    test_df
    %>%  build_lm(vec1~log(random))
    %>%  tidy_lm(model, conf.int = TRUE, conf.level = 0.5, quick=TRUE)
  )
  expect_true(all(result[["estimate"]] != result_[["estimate"]]))
  expect_equal(ncol(result_), 2)
})

test_that("do_kmeans.kv augment", {
  loadNamespace("dplyr")
  test_df <- data.frame(
    group=rep(paste("group", seq(2)), each=9),
    subject=rep(paste("sub", rep(seq(3), each=3)), each=2),
    key=rep(paste("dim", rep(seq(3))), each=2),
    value=seq(3), stringsAsFactors = F
  )
  result <- test_df %>%
    dplyr::group_by(group) %>%
    build_kmeans.kv(subject, key, value, keep.source=TRUE, centers=1, augment = FALSE) %>%
    augment_kmeans(model, source.data)
  expect_true(is.integer(result[["cluster"]]))
  expect_true(all(result[["cluster"]] == 1))
})

test_that("do_kmeans.kv augment", {
  loadNamespace("dplyr")
  test_df <- data.frame(
    group=rep(paste("group", seq(2)), each=9),
    subject=rep(paste("sub", rep(seq(3), each=3)), each=2),
    key=rep(paste("dim", rep(seq(3))), each=2),
    value=seq(3), stringsAsFactors = F
  )
  result <- (
    test_df
    %>%  dplyr::group_by(group)
    %>%  build_kmeans.kv(subject, key, value, keep.source=TRUE, centers=1, augment = FALSE)
    %>%  augment_kmeans(model, source.data)
  )
  expect_true(is.integer(result[["cluster"]]))
  expect_true(all(result[["cluster"]] == 1))
})



test_that("predict lm with new data", {
  loadNamespace("dplyr")
  fit_df <- data.frame(
    model=rep(paste("group", seq(2)), each=9),
    num1 = seq(18),
    num2 = 30-seq(18)
  )
  add_df <- data.frame(
    group=rep(paste("group", seq(2)), each=10),
    num1 = seq(20),
    num2 = 30-seq(20)
  )

  model_data <- fit_df %>% dplyr::group_by(model) %>% build_lm(num1 ~ num2, group_cols = "model")

  fit <- add_df %>% dplyr::group_by(group) %>% add_prediction(model_df = model_data)

  expect_equal(nrow(fit), 20 * 2)
  expect_equal(names(fit), c("model.group", "group", "num1", "num2", "predicted_value", "standard_error", "conf_low", "conf_high"))
})

test_that("predict lm with new data", {
  loadNamespace("dplyr")
  fit_df <- data.frame(
    group = rep(paste("group", seq(2)), each=15),
    num1 = seq(30),
    num2 = 10- (seq(30) %% 2)
  )

  model_data <- fit_df %>% build_lm(num1 ~ num2, group_cols = "group")

  coef_ret <- model_data %>% model_coef(conf.int = TRUE)
  expect_equal(colnames(coef_ret), c("group", "term", "estimate", "std_error", "t_ratio", "p_value", "conf_low", "conf_high"))

  stats_ret <- model_data %>% model_stats()
  expect_equal(colnames(stats_ret), c("group", "r_squared", "adj_r_squared", "root_mean_square_error",
                                      "f_ratio", "p_value", "df", "log_likelihood",
                                      "aic", "bic", "deviance", "residual_df"))

  anova_ret <- model_data %>% model_anova()
  expect_equal(colnames(anova_ret), c("group", "term", "df", "sum_of_squares", "mean_square", "f_ratio", "p_value"))

  confint_ret <- model_data %>% model_confint(level = 0.99)
  expect_equal(colnames(confint_ret), c("group", "Term", "Prob 0.5", "Prob 99.5"))
})

test_that("assign_cluster", {
  test_df <- data.frame(
    na=rep(c(NA, 5, 1, 4), 5),
    group=paste("group",rep(c(1, 2, 3, 4), each=5), sep=""),
    col=rep(seq(5), 4))
  test_df <- dplyr::filter(test_df, group != "group2" | col != 4)
  ret <- build_kmeans(test_df, na, col, fill = 1, augment = FALSE, keep.source = FALSE)

  ret <- assign_cluster(ret, test_df)
  expect_true(is.numeric(ret[["cluster"]]))
})

test_that("cluster_data", {
  test_df_group1 <- data.frame(
    with_na_group1 = rep(c(NA, 5, 1, 4), 5),
    with_na_group2 = rep(c(4, 5, 1, 4), 5),
    g = 1
  )
  test_df_group2 <- data.frame(
    with_na_group1 = rep(c(4, 8, 1, 2), 5),
    with_na_group2 = rep(c(NA, 5, 1, 4), 5),
    g = 2
  )
  test_df <- dplyr::bind_rows(test_df_group1, test_df_group2) %>% dplyr::group_by(g)
  kmeans_ret <- build_kmeans(test_df, with_na_group1, with_na_group2, fill = 1, augment = FALSE, keep.source = FALSE)
  cluster_ret <- cluster_info(kmeans_ret)
  expect_equal(colnames(cluster_ret),
               c("g", "Center with_na_group1", "Center with_na_group2", "Size", "Withinss"))

  ret <- kmeans_info(kmeans_ret)
  expect_equal(colnames(ret), c("g", "Total Sum of Squares", "Total Sum of Squares within Clusters",
                                "Total Sum of Squares between Clusters", "Number of Iterations"
  ))

  assigned_ret <- kmeans_ret %>% assign_cluster(test_df)
  expect_equal(colnames(assigned_ret), c("g", "with_na_group1", "with_na_group2", "cluster"))

})

test_that("test prediction with group", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  # duplicate rows to make some predictable data
  # otherwise, the number of rows of the result of prediction becomes 0
  for (i in seq(5)){
    test_data <- dplyr::bind_rows(test_data, test_data)
  }

  grouped <- dplyr::group_by(test_data, CARRIER)

  model_data <- build_lm(grouped, CANCELLED ~ DISTANCE, test_rate = 0.6)

  ret <- prediction(model_data, type.predict = "response", pretty.name = TRUE)
  expect_equal(grouped_by(ret), "CARRIER")
})

test_that("test prediction with group conf int", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  # duplicate rows to make some predictable data
  # otherwise, the number of rows of the result of prediction becomes 0
  for (i in seq(5)){
    test_data <- dplyr::bind_rows(test_data, test_data)
  }

  grouped <- dplyr::group_by(test_data, CARRIER)

  model_data <- build_lm(grouped, CANCELLED ~ DISTANCE, test_rate = 0.6)

  ret <- prediction(model_data, type.predict = "response", conf_int = 0.99, pretty.name = TRUE)
  expect_equal(grouped_by(ret), "CARRIER")
})

test_that("test prediction with group", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  # duplicate rows to make some predictable data
  # otherwise, the number of rows of the result of prediction becomes 0
  for (i in seq(5)){
    test_data <- dplyr::bind_rows(test_data, test_data)
  }

  grouped <- dplyr::group_by(test_data, CARRIER)

  model_data <- build_lm(grouped, CANCELLED ~ DISTANCE, test_rate = 0.6)

  ret <- prediction(model_data, type.predict = "response", pretty.name = TRUE)
  expect_equal(grouped_by(ret), "CARRIER")
})

test_that("test prediction binary", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  # duplicate rows to make some predictable data
  # otherwise, the number of rows of the result of prediction becomes 0
  for (i in seq(5)){
    test_data <- dplyr::bind_rows(test_data, test_data)
  }

  test_data[["CANCELLED"]] <- as.factor(test_data[["CANCELLED"]])

  model_data <- build_lr(test_data, CANCELLED ~ DISTANCE, test_rate = 0.2)

  coef_ret <- model_coef(model_data, conf_int = "default")

  prediction_train_ret <- prediction_binary(model_data, data = "training")
  expect_true(any(colnames(prediction_train_ret) %in% "predicted_label"))

})

test_that("test loess", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  test_data[["rand"]] <- runif(nrow(test_data), min=-5, max = 5)

  for (i in seq(5)){
    test_data <- dplyr::bind_rows(test_data, test_data)
  }

  model_data <- build_model(test_data, model_func = loess, formula = rand ~ DISTANCE, test_rate = 0.2)

  prediction_ret <- prediction(model_data)

  expect_true(any(colnames(prediction_ret) %in% "conf_low"))
  expect_true(any(colnames(prediction_ret) %in% "conf_high"))
})

test_that("test gam", {
  test_data <- structure(
    list(
      CANCELLED = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0),
      `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
      CARRIER = c("DL", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL"),
      DISTANCE = c(1587, 173, 646, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545)), row.names = c(NA, -20L),
    class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED", "Carrier Name", "CARRIER", "DISTANCE"))

  for (i in seq(5)){
    test_data <- dplyr::bind_rows(test_data, test_data)
  }

  test_data[["rand"]] <- runif(nrow(test_data), min=-5, max = 5)

  model_data <- build_model(test_data, model_func = mgcv::gam, formula = rand ~ s(DISTANCE), test_rate = 0.2)

  prediction_ret <- prediction(model_data)

  expect_true(any(colnames(prediction_ret) %in% "conf_low"))
  expect_true(any(colnames(prediction_ret) %in% "conf_high"))
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

test_that("test prediction_training_and_test by glm", {
  test_data <- structure(
      list(
        `CANCELLED X` = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "Y", "N", "Y", "N"),
        `Carrier Name` = c("Delta Air Lines", "American Eagle", "American Airlines", "Southwest Airlines", "SkyWest Airlines", "Southwest Airlines", "Southwest Airlines", "Delta Air Lines", "Southwest Airlines", "Atlantic Southeast Airlines", "American Airlines", "Southwest Airlines", "US Airways", "US Airways", "Delta Air Lines", "Atlantic Southeast Airlines", NA, "Atlantic Southeast Airlines", "Delta Air Lines", "Delta Air Lines"),
        CARRIER = factor(c("AA", "MQ", "AA", "DL", "MQ", "AA", "DL", "DL", "MQ", "AA", "AA", "WN", "US", "US", "DL", "EV", "9E", "EV", "DL", "DL")), # test with factor with NA
        # testing filtering of Inf, -Inf, NA here.
        DISTANCE = c(10, 12, 12, 187, 273, 1062, 583, 240, 1123, 851, 852, 862, 361, 507, 1020, 1092, 342, 489, 1184, 545),
        ARR_TIME = c(10, 32, 321, 342, 123, 98, 10, 21, 80, 211, 121, 87, 821, 213, 213, 923, 121, 76, 34, 50),
        DERAY_TIME = c(12, 42, 321, 31, 3, 43, 342, 764, 123, 43, 50, 12, 876, 12, 34, 45, 84, 25, 87, 352, 10)
        ), row.names = c(NA, -20L),
      class = c("tbl_df", "tbl", "data.frame"), .Names = c("CANCELLED X", "Carrier Name", "CARRIER", "DISTANCE", "ARR_TIME", "DERAY_TIME"))
  
  test_data$klass <- c(rep("A", 10), rep("B", 10))
  model_ret <- test_data %>% build_lm.fast(`DISTANCE`,
                                     `ARR_TIME`,
                                     `DERAY_TIME`,
                                     `Carrier Name`,
                                     model_type = "lm",
                                     test_rate = 0.2)
  ret <- model_ret %>% prediction_training_and_test(.)
  expected_cols <- c("Carrier.Name", "DISTANCE", "ARR_TIME",
                     "DERAY_TIME", "predicted_value", "standard_error",
                     "conf_low", "conf_high", "residuals", "hat",
                     "residual_standard_deviation", "cooks_distance",
                     "standardised_residuals", "is_test_data")
  expect_equal(colnames(ret), expected_cols)
  grp_model_ret <- test_data %>% dplyr::group_by(klass) %>%
                     build_lm.fast(`DISTANCE`,
                                   `ARR_TIME`,
                                   `DERAY_TIME`,
                                   `Carrier Name`,
                                   model_type = "lm",
                                   test_rate = 0.2)
  grp_ret <- grp_model_ret %>% prediction_training_and_test(.)
  expected_cols <- c("klass", "Carrier.Name", "DISTANCE",
                     "ARR_TIME", "DERAY_TIME", "predicted_value",
                     "standard_error", "conf_low", "conf_high", "residuals",
                     "hat", "residual_standard_deviation",
                     "cooks_distance", "standardised_residuals", "is_test_data")
  expect_equal(colnames(grp_ret), expected_cols)
})

