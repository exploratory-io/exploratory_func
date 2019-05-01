context("test string operation functions")

test_df <- data.frame(input = c("Hello world!", "This is a data frame for test. This is second sentence.", NA), stringsAsFactors = FALSE)

test_that("is_stopword", {
  test_vec <- c("the", "yourself", "Test", "test")
  result <- is_stopword(test_vec, exclude = "the", include = "Test")
  expect_equal(result, c(FALSE, TRUE, TRUE, FALSE))
})

test_that("check languages", {
  languages <- c(
    "danish",
    "dutch",
    "english",
    "finnish",
    "french",
    "german",
    "hungarian",
    "italian",
    "norwegian",
    "portuguese",
    "russian",
    "spanish",
    "swedish",
    "japanese",
    "english_SMART",
    "english_snowball",
    "english_onix"
  )

  for (lang in languages){
    # this should succeeds without error
    get_stopwords(lang = lang)
  }
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

test_that("test word_to_sentiment", {
  result <- word_to_sentiment("good")
  expect_equal(result, "positive")
})

test_that("test word_to_sentiment", {
  result <- word_to_sentiment("bad", lexicon = "AFINN")
  expect_equal(result, -3)
})

test_that("test word_to_sentiment to groupd_df", {
  # this is added because this function was once very slow for grouped data
  # see https://github.com/exploratory-io/exploratory_func/pull/106 for details
  df <- data.frame(
    text = c("good", "sad", letters[1:(10000 * 3 - 2)]),
    group = rep(seq(10000), 3),
    stringsAsFactors = FALSE
  )

  ret <- df %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(sent = word_to_sentiment(text))
  expect_true(is.character(ret[["sent"]]))
})

test_that("do_tokenize with drop=FALSE", {
  result <- test_df %>%
    do_tokenize(input, drop=F)
  expect_equal(result$token[[1]], "hello")
  expect_equal(ncol(result), 4)
})

test_that("do_tokenize with keep_cols = TRUE", {
  test_df <- data.frame(
    input = c("Hello world!", "This is a data frame for test. This is second sentence."),
    extra_col = seq(2),
    stringsAsFactors = FALSE)
  result <- test_df %>%
    do_tokenize(input, keep_cols = TRUE, drop = TRUE)
  expect_equal(result$token[[1]], "hello")
  expect_equal(ncol(result), 4)
})

test_that("do_tokenize with keep_cols = TRUE with sentences", {
  test_df <- data.frame(
    input = c("Hello world!", "This is a data frame for test. This is second sentence."),
    extra_col = seq(2),
    stringsAsFactors = FALSE)
  result <- test_df %>%
    do_tokenize(input, drop=FALSE, token = "sentences", keep_cols = TRUE)
  expect_equal(result$token[[1]], "hello world!")
  expect_equal(ncol(result), 4)
})

test_that("do_tokenize with token=words", {
  result <- test_df %>%
    do_tokenize(input, token="words", keep_cols = TRUE)
  expect_equal(result$token[[1]], "hello")
  expect_equal(ncol(result), 3)
})

test_that("do_tokenize when names conflict", {
  df <- test_df
  df$document_id <- seq(nrow(df))
  result <- df %>%
    do_tokenize(input, token="words", keep_cols = TRUE)
  expect_equal(result$token[[1]], "hello")
  expect_equal(ncol(result), 4)
  expect_equal(colnames(result)[[2]],"document_id.new")
})

test_that("do_tokenize with token=sentence", {
  result <- test_df %>%
    do_tokenize(input, token="sentences")
  expect_equal(result$token[[1]], "hello world!")
  expect_equal(ncol(result), 2)
})

test_that("do_tokenize should work with output", {
  result <- test_df %>%
    do_tokenize(input, output=sentence, token="sentences")
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
  test_df <- data.frame(id=rep(c(1,2), 5))
  test_df["doc id"] <- rep(c(1,2), 5)
  test_df["colname with space"] <- c("this", "this", "this", letters[1:7])
  result <- (
    test_df %>%
      do_tfidf(`doc id`, `colname with space`)
  )
  expect_equal(result$tfidf[c(1,5)], c(2/(sqrt(2^2*3)), 2/(sqrt(2^2*4))))
})


test_that("do_tfidf with bach tick arg", {
  test_df <- setNames(data.frame(rep(c(1,2), 5), c("this", "this", "this", letters[1:7])), c("id", "cname with space"))
  result <- (
    test_df %>%
      do_tfidf(id, `cname with space`, norm = FALSE, tf_weight="raw")
  )
  expect_equal(head(result$tfidf,2), c(log(2/1), log(2/1)))
})

test_that("do_tfidf no norm", {
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", "this", letters[1:7]))
  result <- (
    test_df %>%
      do_tfidf(id, word, norm = FALSE, tf_weight="raw")
  )
  expect_equal(head(result$tfidf,2), c(log(2/1), log(2/1)))
})

test_that("do_tfidf l2", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", "this", letters[1:7]))
  result <- (
    test_df %>%
      do_tfidf(id, word, norm="l2")
  )
  ret <- (result %>%  dplyr::group_by(id)  %>%  dplyr::summarize(l=sqrt(sum(tfidf^2))))
  expect_true(all(ret$l==1))
})

test_that("do_tfidf l1", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", "this", letters[1:7]))
  result <- (
    test_df %>%
      do_tfidf(id, word, norm="l1")
  )
  ret <- (result %>%  dplyr::group_by(id)  %>%  dplyr::summarize(l=sum(tfidf)))
  expect_true(all(ret$l==1))
})

test_that("do_tfidf tf_weight=raw", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", "this", letters[1:7]))
  result <- (
    test_df %>%
      do_tfidf(id, word, tf_weight="raw")
  )
  ret <- (result %>%  dplyr::group_by(id)  %>%  dplyr::summarize(l=sqrt(sum(tfidf^2))))
  expect_true(all(ret$l==1))
})

test_that("do_tfidf tf_weight=log_scale", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", "this", letters[1:7]))
  result <- (
    test_df %>%
      do_tfidf(id, word, tf_weight="log_scale")
  )
  ret <- (result %>%  dplyr::group_by(id)  %>%  dplyr::summarize(l=sqrt(sum(tfidf^2))))
  expect_true(all(ret$l==1))
})

test_that("do_tfidf tf_weight=binary", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", "this", letters[1:7]))
  result <- (
    test_df %>%
      do_tfidf(id, word, tf_weight="binary")
  )
  ret <- (result %>%  dplyr::group_by(id)  %>%  dplyr::summarize(l=sqrt(sum(tfidf^2))))
  expect_true(all(ret$l==1))
})

test_that("do_ngram", {
  loadNamespace("dplyr")
  df <- data.frame(
    doc=paste("doc", rep(c(1,2), each=10)) ,
    token=paste("token",rep(c(1,2),10), sep=""),
    sentence=rep(seq(5), each=4),
    stringsAsFactors = F)

  ret <- df %>%  do_ngram(token, sentence, doc, maxn = 3)
  expect_equal(colnames(ret), c("doc", "sentence", "gram", "token"))
  expect_true(any(ret[["gram"]] == 1))
  expect_true(is.integer(ret[["gram"]]))
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

test_that("get_sentiment", {
  if(requireNamespace("sentimentr")){
    sentences <- c(
      "I feel bad.",
      "I'm not so happy",
      "You look very cheerful."
    )
    ret <- get_sentiment(sentences)
    expect_equal(ret, c(-0.4330127, -0.3750000, 0.6750000), tolerance=0.05)
  }
})

test_that("stem_word", {
  ret <- stem_word(c("impingement","feline"))
  expect_equal(ret, c("imping", "felin"))
})

test_that("parse_character", {
  ret <- exploratory::parse_character(c(1, 2))
  expect_equal(ret, c("1", "2"))
})

test_that("parse_number", {
  # Parse characters
  ret <- exploratory::parse_number(c("1", "2.1"))
  expect_equal(ret, c(1, 2.1))
  expect_true(is.vector(ret))
  # Pass through input that is already numeric.
  ret <- exploratory::parse_number(c(1, 2.1))
  expect_equal(ret, c(1, 2.1))
  expect_true(is.vector(ret))
})

test_that("parse_logical", {
  ret <- exploratory::parse_logical(c(TRUE, FALSE))
  expect_equal(ret, c(TRUE, FALSE))
})

