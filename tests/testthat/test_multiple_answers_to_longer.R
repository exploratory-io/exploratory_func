test_that("single MA column, keep = both, produces one long row per respondent x selection", {
  df <- data.frame(
    id = c(1, 2),
    store = c("Yamada,Edion", "Yamada"),
    stringsAsFactors = FALSE
  )
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",")

  expect_equal(nrow(result), 4)
  expect_true(all(c("id", "Question", "Selection", "Answer") %in% names(result)))
  expect_false("store" %in% names(result))

  row1 <- result[result$id == 1 & result$Selection == "Yamada", ]
  row2 <- result[result$id == 1 & result$Selection == "Edion", ]
  row3 <- result[result$id == 2 & result$Selection == "Yamada", ]
  row4 <- result[result$id == 2 & result$Selection == "Edion", ]

  expect_equal(row1$Answer, 1)
  expect_equal(row1$Question, "store")
  expect_equal(row2$Answer, 1)
  expect_equal(row3$Answer, 1)
  expect_equal(row4$Answer, 0)  # id=2 did not choose Edion but "both" keeps the 0 row
})

test_that("keep = one drops the Answer == 0 rows", {
  df <- data.frame(
    id = c(1, 2),
    store = c("Yamada,Edion", "Yamada"),
    stringsAsFactors = FALSE
  )
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",", keep = "one")

  expect_equal(nrow(result), 3)
  expect_true(all(result$Answer == 1))
  expect_false(any(result$id == 2 & result$Selection == "Edion"))
})

test_that("multiple MA columns do not cartesian-blow-up the row count", {
  # Respondent 1 picks 2 stores, 2 categories, 2 usages => naive chained
  # separate_rows() would multiply to 2*2*2=8 rows; the correct long-format
  # transform yields 2+2+2=6 rows (one per selection, per question).
  df <- data.frame(
    id = c(1),
    store = c("Yamada,Edion"),
    category = c("TV,PC"),
    usage = c("Daily,Weekly"),
    stringsAsFactors = FALSE
  )
  result <- df %>% exp_multiple_answers_to_longer(store, category, usage, sep = ",", keep = "one")

  expect_equal(nrow(result), 6)
  expect_equal(sort(unique(result$Question)), c("category", "store", "usage"))
  expect_equal(sort(result$Selection[result$Question == "store"]), c("Edion", "Yamada"))
  expect_equal(sort(result$Selection[result$Question == "category"]), c("PC", "TV"))
  expect_equal(sort(result$Selection[result$Question == "usage"]), c("Daily", "Weekly"))
})

test_that("multiple MA columns, keep = both, independently fills 0s per question", {
  df <- data.frame(
    id = c(1, 2),
    store = c("Yamada,Edion", "Yamada"),
    category = c("TV", "TV,PC"),
    stringsAsFactors = FALSE
  )
  result <- df %>% exp_multiple_answers_to_longer(store, category, sep = ",")

  # 2 questions x 2 respondents x 2 selections each = 8 rows total
  expect_equal(nrow(result), 8)

  id1_category_pc <- result[result$id == 1 & result$Question == "category" & result$Selection == "PC", ]
  expect_equal(id1_category_pc$Answer, 0)

  id2_store_edion <- result[result$id == 2 & result$Question == "store" & result$Selection == "Edion", ]
  expect_equal(id2_store_edion$Answer, 0)
})

test_that("passthrough columns not selected as target are preserved", {
  df <- data.frame(
    id = c(1),
    name = c("Alice"),
    store = c("Yamada,Edion"),
    stringsAsFactors = FALSE
  )
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",")

  expect_true("name" %in% names(result))
  expect_equal(unique(result$name), "Alice")
})

test_that("output column names are customizable", {
  df <- data.frame(
    id = c(1),
    store = c("Yamada,Edion"),
    stringsAsFactors = FALSE
  )
  result <- df %>% exp_multiple_answers_to_longer(
    store,
    sep = ",",
    question_col = "設問",
    selection_col = "選択肢",
    answer_col = "値"
  )

  expect_true(all(c("設問", "選択肢", "値") %in% names(result)))
  expect_false(any(c("Question", "Selection", "Answer") %in% names(result)))
})

test_that("custom separator other than comma works", {
  df <- data.frame(
    id = c(1),
    store = c("Yamada;Edion"),
    stringsAsFactors = FALSE
  )
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ";")

  expect_equal(nrow(result), 2)
  expect_equal(sort(result$Selection), c("Edion", "Yamada"))
})

test_that("whitespace around separator is trimmed by default", {
  df <- data.frame(
    id = c(1),
    store = c("Yamada, Edion , 	Bic Camera"),
    stringsAsFactors = FALSE
  )
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",")

  expect_equal(sort(result$Selection), c("Bic Camera", "Edion", "Yamada"))
})

test_that("blank/NA selections do not create a phantom NA choice column", {
  df <- data.frame(
    id = c(1, 2, 3),
    store = c("Yamada", "", NA_character_),
    stringsAsFactors = FALSE
  )
  result <- df %>% exp_multiple_answers_to_longer(store, sep = ",")

  expect_false(any(is.na(result$Selection)))
  expect_false("NA" %in% result$Selection)
  # Respondents 2 and 3 answered nothing for this single question, so they
  # contribute no rows at all (there is no other question to fill 0s against).
  expect_equal(sort(unique(result$id)), c(1))
})

test_that("multibyte and symbol-bearing column names and choice values round-trip", {
  complex_col <- "店舗 \"レビュー\" #1"
  df <- data.frame(
    id = c(1),
    stringsAsFactors = FALSE
  )
  df[[complex_col]] <- "ヤマダ電機(池袋),エディオン/渋谷"
  names(df)[2] <- complex_col

  result <- df %>% exp_multiple_answers_to_longer(!!rlang::sym(complex_col), sep = ",")

  expect_equal(nrow(result), 2)
  expect_true(all(result$Question == complex_col))
  expect_true("ヤマダ電機(池袋)" %in% result$Selection)
  expect_true("エディオン/渋谷" %in% result$Selection)
})

test_that("errors clearly when no target column is selected", {
  df <- data.frame(id = c(1), store = c("Yamada"), stringsAsFactors = FALSE)
  expect_error(
    df %>% exp_multiple_answers_to_longer(sep = ","),
    "At least one column"
  )
})

test_that("errors clearly when output column names collide with a passthrough column", {
  df <- data.frame(id = c(1), Question = c("x"), store = c("Yamada"), stringsAsFactors = FALSE)
  expect_error(
    df %>% exp_multiple_answers_to_longer(store, sep = ","),
    "already exist"
  )
})

test_that("errors clearly when the three output column names are not distinct", {
  df <- data.frame(id = c(1), store = c("Yamada"), stringsAsFactors = FALSE)
  expect_error(
    df %>% exp_multiple_answers_to_longer(store, sep = ",", question_col = "X", selection_col = "X"),
    "must be distinct"
  )
})
