context("test string operation functions")

test_df <- data.frame(char = c("Hello world!", "This is a data frame for test. This is second sentence."), stringsAsFactors = FALSE)

test_that("test get_stop_words", {
  result <- get_stopwords()
  expect_true(any(result == "a"))
})

test_that("test get_sentiment", {
  result <- get_sentiment("good")
  expect_equal(result, "positive")
})

test_that("test get_sentiment", {
  result <- get_sentiment("bad", lexicon = "AFINN")
  expect_equal(result, -3)
})

test_that("do_tokenize should work", {
  result <- test_df %>%
    do_tokenize(char)
  expect_equal(result$.token[[1]], "hello")
})

test_that("do_tokenize should work with output", {
  result <- test_df %>%
    do_tokenize(char, output=sentence, token="sentences")
  expect_equal(result$sentence[[2]], "this is a data frame for test.")
})

test_that("calc_tfidf", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", letters[1:8]))
  result <- (
    test_df %>%
      dplyr::mutate(idf=calc_idf(id, word))
  )
  expect_equal(head(result$idf,2), c(log(2/3), log(2/3)))
})

test_that("calc_tfidf smooth_idf FALSE", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", letters[1:8]))
  result <- (
    test_df %>%
      dplyr::mutate(idf=calc_idf(id, word, smooth_idf=FALSE))
  )
  expect_equal(head(result$idf,2), c(0, 0))
})
