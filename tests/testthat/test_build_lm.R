context("test build_lm")

test_that("test build_lm summary output ", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    weight = seq(20) + 100,
    category = rep(letters[1:4], 5),
    with_NA = rep(c(letters[5:6], NA, NA), 5)
  )
  trial <- test_df %>% build_lm(num1 ~ num2 + category + with_NA, weights = weight)

  expect_equal(colnames(trial), c(".test_index", "source.data", "model"))

  res <- capture.output(summary(trial$model[[1]]))
  expect_lt(length(res), 50) # the output of summary should be less than 50 lines
})

test_that("test build_lm with keep.source FALSE ", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    weight = seq(20) + 100,
    category = rep(letters[1:4], 5),
    with_NA = rep(c(letters[5:6], NA, NA), 5)
  )
  trial <- test_df %>% build_lm(num1 ~ num2 + category + with_NA, weights = weight, keep.source = FALSE)

  expect_equal(colnames(trial), c(".test_index", "model"))
})

test_that("test build_lm with grouped ", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    group1 = rep(letters[1:4], 5),
    group2 = rep(letters[1:2], each = 10)
  )
  trial <- test_df %>% build_lm(num1 ~ num2, group_cols = c("group1", "group2"))
  expect_equal(length(trial[["group2"]]), 8)
  expect_equal(length(trial[["group1"]]), 8)
})

test_that("test build_lm with augment TRUE", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 10,
    group1 = rep(letters[1:4], 5),
    group2 = rep(letters[1:2], each = 10)
  )
  trial <- test_df %>% build_lm(num1 ~ num2, group_cols = c("group1", "group2"), augment = TRUE)
  expect_equal(length(trial[["group2"]]), 20)
  expect_equal(length(trial[["group1"]]), 20)
})

test_that("test name conflict avoid", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 11,
    estimate = rep(letters[1:4], 5),
    model = rep(letters[1:2], each = 10),
    model.group = rep(letters[1:2], each = 10)
  )

  lm_model <- test_df %>%
    build_lm(num1 ~ num2, group_cols = c("estimate", "model", "model.group"))

  expect_equal(colnames(lm_model), c("estimate.group", "model.group", "model.group1", ".test_index", "source.data", "model"))

  trial <- suppressWarnings({
    lm_model %>%
      broom::tidy(model)
  })

  expect_equal(colnames(trial), c("estimate.group", "model.group", "model.group1",
                                  "term", "estimate", "std.error", "statistic", "p.value"))
})

test_that("build_lm with evaluation", {
  test_df = data.frame(
    num1 = seq(20) / 10.0,
    num2 = seq(20) - 11,
    group = rep(letters[1:2], each = 10)
  )

  lm_model <- test_df %>%
    build_lm(num1 ~ num2, group_cols = c("group"), test_rate = 0.1)

  evaluated <- lm_model %>%
    prediction(test_df)

  expect_equal(colnames(evaluated), c("group", "num1", "num2", "Fitted", "Standard Error"))

  test_eval <- lm_model %>%
    prediction(test_df, test = FALSE)

  expect_equal(colnames(test_eval), c("group", "num1", "num2",
                                      "Fitted", "Standard Error", "Residuals",
                                      "Hat", "Residual Standard Deviation", "Cooks Distance",
                                      "Standardised Residuals"
                                      ))

})
