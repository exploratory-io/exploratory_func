# how to run this test:
# devtools::test(filter="model_target_col_name_cleaning")
#
# tam #37985: cleanup_df(map_name = FALSE) replaces commas in column names with
# dots (mmpf::marginalPrediction cannot handle commas), so exp_rpart / exp_chaid
# train on -- and hand back a source.data carrying -- the CLEANED target column
# name, while the model records the ORIGINAL one in orig_target_col. Every
# consumer that looks the target up inside source.data by orig_target_col broke
# as soon as the target column name contained a comma: augment.rpart.* aborted
# the whole chart with "Can't select columns that don't exist", and
# dtree_report_multiclass_probabilities silently returned zero rows.

context("test target column name resolution against cleaned model data")

# The canonical stress-test column name (see tam .claude/rules/workflow.md rule 7).
# The comma after "+" is what triggers the cleaning.
tricky_target <- '遅れ た !"#$%&\'()*+, -./:;<=>?@[]^_\'{|}~ 表'
cleaned_target <- gsub('[,]', '.', tricky_target)

make_model_test_df <- function(target_col, target_type = "multi", n = 150) {
  set.seed(1)
  df <- data.frame(num1 = rnorm(n), num2 = runif(n), stringsAsFactors = FALSE)
  df[["cat pred"]] <- sample(letters[1:3], n, replace = TRUE)
  df[[target_col]] <- switch(target_type,
    multi = factor(sample(c("Yes", "No", "Maybe"), n, replace = TRUE)),
    bin = sample(c(TRUE, FALSE), n, replace = TRUE),
    num = rnorm(n))
  # Put the target FIRST on purpose. cleanup_df() preserves the incoming column
  # order, so a target that is already last makes the relocate a no-op and the
  # "target ends up next to the predicted value" assertion cannot tell a working
  # resolver from one that always gives up.
  df[, c(target_col, "num1", "num2", "cat pred"), drop = FALSE]
}

build_model <- function(fun, target_col, target_type) {
  df <- make_model_test_df(target_col, target_type)
  expr <- sprintf("df %%>%% %s(`%s`, num1, num2, `cat pred`, test_rate = 0.3)",
                  fun, gsub("`", "\\\\`", target_col))
  eval(parse(text = expr), list(df = df))
}

test_that("resolve_model_target_col prefers the stored clean name, then falls back", {
  data_cleaned <- data.frame(a = 1)
  colnames(data_cleaned) <- cleaned_target

  # 1. clean_target_col wins when it is present in the data.
  expect_equal(
    exploratory:::resolve_model_target_col(
      list(clean_target_col = cleaned_target, orig_target_col = tricky_target), data_cleaned),
    cleaned_target)

  # 2. A model cached by an older version has no clean_target_col -- the cleaned
  #    form of orig_target_col has to be derived.
  expect_equal(
    exploratory:::resolve_model_target_col(list(orig_target_col = tricky_target), data_cleaned),
    cleaned_target)

  # 3. When the data really carries the original name (every restore-the-names
  #    path, and every name that needs no cleaning), that name is used as-is.
  data_orig <- data.frame(a = 1)
  colnames(data_orig) <- tricky_target
  expect_equal(
    exploratory:::resolve_model_target_col(list(orig_target_col = tricky_target), data_orig),
    tricky_target)

  # 4. Genuinely absent -> NULL, so callers can degrade instead of aborting.
  expect_null(exploratory:::resolve_model_target_col(
    list(orig_target_col = "nope"), data.frame(other = 1)))
  expect_null(exploratory:::resolve_model_target_col(NULL, data_cleaned))
  expect_null(exploratory:::resolve_model_target_col(list(orig_target_col = tricky_target), NULL))

  # vars_select() returns a NAMED character; the name must not leak into the
  # column reference the callers build from this.
  named <- c(x = tricky_target)
  expect_null(names(exploratory:::resolve_model_target_col(
    list(orig_target_col = named), data_orig)))
})

test_that("relocate_target_col_last moves the target last and no-ops when absent", {
  df <- data.frame(first = 1:2, second = 3:4, stringsAsFactors = FALSE)
  df[[cleaned_target]] <- c("a", "b")
  df$predicted_value <- c("a", "b")
  df <- df[, c(cleaned_target, "first", "second", "predicted_value")]

  moved <- exploratory:::relocate_target_col_last(
    df, list(orig_target_col = tricky_target))
  expect_equal(colnames(moved),
               c("first", "second", "predicted_value", cleaned_target))

  # No target column at all -- return the frame untouched rather than erroring.
  unrelated <- data.frame(first = 1:2, second = 3:4)
  expect_equal(
    exploratory:::relocate_target_col_last(unrelated, list(orig_target_col = tricky_target)),
    unrelated)
})

test_that("exp_rpart prediction works when the target column name contains a comma", {
  for (target_type in c("multi", "bin", "num")) {
    model_df <- build_model("exp_rpart", tricky_target, target_type)

    # The failure reported in tam #37985 -- this aborted the Target Distribution
    # chart (and every other training_and_test chart) before the fix.
    both <- model_df %>% prediction(data = "training_and_test")
    expect_equal(nrow(both), 150)
    expect_true(cleaned_target %in% colnames(both))
    # The relocate has to actually HAPPEN, not merely avoid erroring: the target
    # column is moved past every predictor so it ends up next to the predicted
    # value. Asserting only "no error" would pass on a resolver that always
    # returned NULL.
    target_pos <- match(cleaned_target, colnames(both))
    predictor_pos <- match(c("num1", "num2", "cat pred"), colnames(both))
    expect_true(all(target_pos > predictor_pos),
                info = paste0(target_type, ": ", paste(colnames(both), collapse = " | ")))

    expect_equal(nrow(model_df %>% prediction(data = "test")), 45)
    expect_gt(nrow(model_df %>% prediction(data = "training")), 0)
  }
})

test_that("exp_rpart multiclass probability report is non-empty for a comma target name", {
  model_df <- build_model("exp_rpart", tricky_target, "multi")
  ret <- exploratory:::dtree_report_multiclass_probabilities(model_df)
  # Returned an empty frame before the fix, silently blanking the per-category
  # probability / ROC / PR charts.
  expect_equal(nrow(ret), 150 * 3)
  expect_equal(sort(unique(as.character(ret$Category))), c("Maybe", "No", "Yes"))
})

test_that("exp_chaid prediction works when the target column name contains a comma", {
  for (target_type in c("multi", "bin")) {
    model_df <- build_model("exp_chaid", tricky_target, target_type)
    expect_equal(nrow(model_df %>% prediction(data = "training_and_test")), 150)
    expect_equal(nrow(model_df %>% prediction(data = "test")), 45)
  }
})

test_that("a plain target column name is unaffected", {
  # Negative control: nothing about the ordinary path changes.
  model_df <- build_model("exp_rpart", "target", "multi")
  both <- model_df %>% prediction(data = "training_and_test")
  expect_equal(nrow(both), 150)
  expect_true("target" %in% colnames(both))
  expect_equal(nrow(exploratory:::dtree_report_multiclass_probabilities(model_df)), 150 * 3)
})

test_that("other model types keep working with a comma in the target column name", {
  # calc_feature_imp / xgboost / lightgbm / catboost restore the original column
  # names on source.data, so they were never broken -- pin that they stay that way
  # now that they share the same resolver.
  for (fun in c("calc_feature_imp", "exp_xgboost", "exp_lightgbm", "exp_catboost")) {
    model_df <- build_model(fun, tricky_target, "bin")
    both <- model_df %>% prediction(data = "training_and_test")
    expect_equal(nrow(both), 150)
    expect_true(tricky_target %in% colnames(both),
                info = paste0(fun, " should keep the original target column name"))
  }
})
