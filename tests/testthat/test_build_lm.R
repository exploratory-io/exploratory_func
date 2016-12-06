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

  expect_equal(colnames(trial), c("model", ".test_index", "source.data"))

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

  expect_equal(colnames(trial), c("model", ".test_index"))
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

  expect_equal(colnames(lm_model), c("estimate.group", "model.group", "model.group1", "model", ".test_index", "source.data"))

  trial <- suppressWarnings({
    lm_model %>%
      broom::tidy(model)
  })

  expect_equal(colnames(trial), c("estimate.group", "model.group", "model.group1",
                                  "term", "estimate", "std.error", "statistic", "p.value"))
})

