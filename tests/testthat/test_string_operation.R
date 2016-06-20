context("test string operation functions")

test_df <- data.frame(char = c("Hello world!", "This is a data frame for test. This is second sentence."), stringsAsFactors = FALSE)

test_that("is_stopword", {
  test_vec <- c("the", "yourself", "Test", "test")
  result <- is_stopword(test_vec)
  expect_equal(result, c(T, T, F, F))
})

test_that("is_digit", {
  test_vec <- c("the", "333", "T est", "1.23", "22_22")
  result <- is_digit(test_vec)
  expect_equal(result, c(F, T, F, F, F))
})

test_that("is_alphabet", {
  test_vec <- c("the", "333", "T est", "1.23", "22_22")
  result <- is_alphabet(test_vec)
  expect_equal(result, c(T, F, F, F, F))
})

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

test_that("do_tokenize with drop=F", {
  result <- test_df %>%
    do_tokenize(char, drop=F)
  expect_equal(result$token[[1]], "hello")
  expect_equal(ncol(result), 4)
})

test_that("do_tokenize with token=words", {
  result <- test_df %>%
    do_tokenize(char, token="words")
  expect_equal(result$token[[1]], "hello")
  expect_equal(ncol(result), 3)
})

test_that("do_tokenize when names conflict", {
  df <- test_df
  df$document_id <- seq(nrow(df))
  result <- df %>%
    do_tokenize(char, token="words")
  expect_equal(result$token[[1]], "hello")
  expect_equal(ncol(result), 4)
  expect_equal(colnames(result)[[2]],"document_id.new")
})

test_that("do_tokenize with token=sentence", {
  result <- test_df %>%
    do_tokenize(char, token="sentences")
  expect_equal(result$token[[1]], "hello world!")
  expect_equal(ncol(result), 1)
})

test_that("do_tokenize should work with output", {
  result <- test_df %>%
    do_tokenize(char, output=sentence, token="sentences")
  expect_equal(result$sentence[[2]], "this is a data frame for test.")
})

test_that("calc_idf", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", letters[1:8]))
  result <- calc_idf(test_df$id, test_df$word)
  expect_equal(head(result$.df,2), c(2, 2))
  expect_equal(head(result$.idf,2), c(0, 0))
})

test_that("calc_tf weight binary", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", letters[1:8]))
  result <- calc_tf(test_df, id,word, weight="binary")
  expect_true(is.logical(result$tf))
  expect_equal(colnames(result)[[1]], "id")
  expect_equal(colnames(result)[[2]], "word")
  expect_equal(colnames(result)[[3]], "count_per_doc")
  expect_equal(colnames(result)[[4]], "tf")
})

test_that("calc_idf smooth_idf FALSE", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", letters[1:8]))
  result <- result <- calc_idf(test_df$id, test_df$word)
  expect_equal(head(result$.idf,2), c(0, 0))
})

test_that("do_tfidf", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", "this", letters[1:7]))
  result <- (
    test_df %>%
      do_tfidf(id, word)
  )
  expect_equal(head(result$tfidf,2), c(log(2/1)/5, log(2/1)/5))
})

test_that("do_ngram", {
  loadNamespace("dplyr")
  df <- data.frame(
    doc=paste("doc", rep(c(1,2), each=10)) ,
    sentence=rep(seq(5), each=4),
    token=paste("token",rep(c(1,2),10), sep=""),
    stringsAsFactors = F)

  ungrouped <- df %>%  do_ngram(token)
  grouped <- df %>%  dplyr::group_by(doc, sentence) %>%  do_ngram(token)
  expect_equal(ncol(ungrouped), 1)
  expect_equal(ncol(grouped), 3)
})

test_that("sentimentr", {
  if(requireNamespace("sentimentr")){
    sentences <- c(
      "I feel bad.",
      "I'm not so happy",
      "You look very cheerful."
    )
    ret <- sentimentr::sentiment(sentences)
  }
})
