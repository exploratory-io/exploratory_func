test_that("matches the mockup preview exactly (single column, all defaults)", {
  # Reproduces the "複数回答を行に分割" mockup preview: 5 respondents, one MA
  # column, defaults for every toggle (trim/exclude_empty/dedupe_within_row/
  # add_row_id/keep_other_columns all TRUE) -> 11 output rows.
  df <- data.frame(
    `顧客ID` = c("C001", "C002", "C003", "C004", "C005"),
    `購入カテゴリー` = c("食品,日用品", "家電,日用品", "食品,衣類", "衣類,家電", "日用品,衣類,食品"),
    `性別` = c("女性", "男性", "女性", "男性", "女性"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  result <- df %>% exp_multiple_answers_to_longer(`購入カテゴリー`, sep = ",")

  expect_equal(nrow(result), 11)
  expect_equal(names(result), c("Original Row ID", "Question", "Answer", "顧客ID", "性別"))
  expect_equal(result$`Original Row ID`, c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5))
  expect_true(all(result$Question == "購入カテゴリー"))
  expect_equal(result$Answer, c("食品", "日用品", "家電", "日用品", "食品", "衣類", "衣類", "家電", "日用品", "衣類", "食品"))
  expect_equal(result$`顧客ID`, c("C001", "C001", "C002", "C002", "C003", "C003", "C004", "C004", "C005", "C005", "C005"))
  expect_equal(result$`性別`, c("女性", "女性", "男性", "男性", "女性", "女性", "男性", "男性", "女性", "女性", "女性"))
})

test_that("multiple columns stack additively, no cartesian blow-up", {
  # Respondent 1 picks 2 stores, 2 categories, 2 usages => naive chained
  # separate_rows() would multiply to 2*2*2=8 rows; stacking (this function's
  # design) yields 2+2+2=6 rows: one row per (question, selected value).
  df <- data.frame(
    id = c(1),
    store = c("Yamada,Edion"),
    category = c("TV,PC"),
    usage = c("Daily,Weekly"),
    stringsAsFactors = FALSE
  )
  result <- df %>% exp_multiple_answers_to_longer(store, category, usage, sep = ",")

  expect_equal(nrow(result), 6)
  expect_equal(sort(unique(result$Question)), c("category", "store", "usage"))
  expect_equal(sort(result$Answer[result$Question == "store"]), c("Edion", "Yamada"))
  expect_equal(sort(result$Answer[result$Question == "category"]), c("PC", "TV"))
  expect_equal(sort(result$Answer[result$Question == "usage"]), c("Daily", "Weekly"))
})

test_that("trim_ws = TRUE (default) trims whitespace around split values", {
  df <- data.frame(id = c(1), store = c("Yamada, Edion , 	Bic Camera"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",")
  expect_equal(sort(result$Answer), c("Bic Camera", "Edion", "Yamada"))
})

test_that("trim_ws = FALSE preserves surrounding whitespace", {
  df <- data.frame(id = c(1), store = c("Yamada, Edion"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",", trim_ws = FALSE)
  expect_equal(result$Answer, c("Yamada", " Edion"))
})

test_that("exclude_empty = TRUE (default) drops blank answers", {
  df <- data.frame(id = c(1), store = c("Yamada,,Edion"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",")
  expect_equal(sort(result$Answer), c("Edion", "Yamada"))
})

test_that("exclude_empty = FALSE keeps blank answers", {
  df <- data.frame(id = c(1), store = c("Yamada,,Edion"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",", exclude_empty = FALSE)
  expect_equal(nrow(result), 3)
  expect_true("" %in% result$Answer)
})

test_that("dedupe_within_row = TRUE (default) collapses a repeated answer in one cell to one row", {
  df <- data.frame(id = c(1), store = c("Yamada,Yamada,Edion"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",")
  expect_equal(sort(result$Answer), c("Edion", "Yamada"))
})

test_that("dedupe_within_row = FALSE keeps the repeated answer as separate rows", {
  df <- data.frame(id = c(1), store = c("Yamada,Yamada,Edion"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",", dedupe_within_row = FALSE)
  expect_equal(nrow(result), 3)
  expect_equal(sort(result$Answer), c("Edion", "Yamada", "Yamada"))
})

test_that("add_row_id = FALSE omits the row-id column", {
  df <- data.frame(id = c(1), store = c("Yamada,Edion"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",", add_row_id = FALSE)
  expect_false("Original Row ID" %in% names(result))
  expect_equal(names(result), c("Question", "Answer", "id"))
})

test_that("add_row_id = TRUE uses a custom row_id_col name", {
  df <- data.frame(id = c(1), store = c("Yamada,Edion"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",", row_id_col = "元の行ID")
  expect_true("元の行ID" %in% names(result))
})

test_that("keep_other_columns = FALSE drops passthrough columns", {
  df <- data.frame(id = c(1), name = c("Alice"), store = c("Yamada,Edion"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",", keep_other_columns = FALSE)
  expect_false("id" %in% names(result))
  expect_false("name" %in% names(result))
  expect_equal(names(result), c("Original Row ID", "Question", "Answer"))
})

test_that("keep_other_columns = FALSE and add_row_id = FALSE yields only Question/Answer", {
  df <- data.frame(id = c(1), store = c("Yamada,Edion"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",", keep_other_columns = FALSE, add_row_id = FALSE)
  expect_equal(names(result), c("Question", "Answer"))
})

test_that("output column names are customizable", {
  df <- data.frame(id = c(1), store = c("Yamada,Edion"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",", question_col = "設問", answer_col = "値")
  expect_true(all(c("設問", "値") %in% names(result)))
  expect_false(any(c("Question", "Answer") %in% names(result)))
})

test_that("custom separator other than comma works", {
  df <- data.frame(id = c(1), store = c("Yamada;Edion"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ";")
  expect_equal(sort(result$Answer), c("Edion", "Yamada"))
})

test_that("multibyte and symbol-bearing column names and choice values round-trip", {
  complex_col <- "店舗 \"レビュー\" #1"
  df <- data.frame(id = c(1), stringsAsFactors = FALSE)
  df[[complex_col]] <- "ヤマダ電機(池袋),エディオン/渋谷"
  names(df)[2] <- complex_col

  result <- df %>% exp_multiple_answers_to_longer(!!rlang::sym(complex_col), sep = ",")

  expect_equal(nrow(result), 2)
  expect_true(all(result$Question == complex_col))
  expect_true("ヤマダ電機(池袋)" %in% result$Answer)
  expect_true("エディオン/渋谷" %in% result$Answer)
})

test_that("errors clearly when no target column is selected", {
  df <- data.frame(id = c(1), store = c("Yamada"), stringsAsFactors = FALSE)
  expect_error(
    df %>% exp_multiple_answers_to_longer(sep = ","),
    "At least one column"
  )
})

test_that("errors clearly when Question and Answer column names are not distinct", {
  df <- data.frame(id = c(1), store = c("Yamada"), stringsAsFactors = FALSE)
  expect_error(
    df %>% exp_multiple_answers_to_longer(store, sep = ",", question_col = "X", answer_col = "X"),
    "must be distinct"
  )
})

test_that("errors clearly when an output column name collides with a passthrough column", {
  df <- data.frame(id = c(1), Question = c("x"), store = c("Yamada"), stringsAsFactors = FALSE)
  expect_error(
    df %>% exp_multiple_answers_to_longer(store, sep = ","),
    "already exist"
  )
})

test_that("no collision error when keep_other_columns = FALSE even if a passthrough name would have collided", {
  df <- data.frame(id = c(1), Question = c("x"), store = c("Yamada"), stringsAsFactors = FALSE)
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",", keep_other_columns = FALSE)
  expect_equal(names(result), c("Original Row ID", "Question", "Answer"))
})
