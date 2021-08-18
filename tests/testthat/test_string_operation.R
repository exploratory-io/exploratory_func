context("test string operation functions")

# Small data for easier deterministic result checking.
test_df_small <- data.frame(input = c("Hello world!", "This is a data frame for test. This is second sentence.", NA), stringsAsFactors = FALSE)
# Data from twitter search. This happens to include invalid UTF-8 byte sequences too.
test_df <- exploratory::read_delim_file("https://www.dropbox.com/s/w1fh7j8iq6g36ry/Twitter_No_Spectator_Olympics_Ja.csv?dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)
test_df <- test_df %>% rename(input=text) # Rename so that it has same column name as test_df_small.

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
    "tamil", # from tidystopwords
    "english_SMART",
    "english_snowball",
    "english_onix"
  )

  expect_error({
    for (lang in languages){
      # this should succeeds without error
      get_stopwords(lang = lang)
    }
  }, NA)
})

test_that("is_digit", {
  test_vec <- c("the", "333", "T est", "1.23", "22_22")
  result <- exploratory:::is_digit(test_vec)
  expect_equal(result, c(F, T, F, F, F))
})

test_that("is_alphabet", {
  test_vec <- c("the", "333", "T est", "1.23", "22_22")
  result <- is_alphabet(test_vec)
  expect_equal(result, c(T, F, F, F, F))
})

test_that("test get_stopwords", {
  result <- get_stopwords()
  expect_true(any(result == "a"))
  expect_true(any(result == "amp"))
  expect_true(any(result == "http"))
  expect_true(any(result == "https"))
  expect_true(any(result == "t.co"))
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

test_that("do_tokenize with tokenize_tweets=TRUE and with_sentence_id=TRUE", {
  result <- test_df %>%
    do_tokenize(input, tokenize_tweets=T, with_sentence_id=T)
  expect_equal(colnames(result), c("document_id", "sentence_id", "token"))
})

test_that("do_tokenize with tokenize_tweets=TRUE and with_sentence_id=FALSE", {
  result <- test_df %>%
    do_tokenize(input, tokenize_tweets=T, with_sentence_id=F)
  expect_equal(colnames(result), c("document_id", "token"))
})

test_that("do_tokenize with with_sentence_id=FALSE", {
  result <- test_df %>%
    do_tokenize(input, with_sentence_id=F)
  expect_equal(colnames(result), c("document_id", "token"))
})

test_that("do_tokenize with drop=FALSE", {
  result <- test_df %>%
    do_tokenize(input, drop=F)
  expect_equal(result$token[[1]], "\u30AA\u30EA\u30F3\u30D4\u30C3\u30AF")
  expect_equal(ncol(result), 4)
})

test_that("do_tokenize with compound_tokens", {
  test_df <- data.frame(
    input = c("Hello world!", "This is a data frame for test. This is second sentence."),
    extra_col = seq(2),
    stringsAsFactors = FALSE)
  result <- test_df %>%
    do_tokenize(input, compound_tokens=c("Hello world", "data frame"))
  expect_equal(c("hello world", "data frame") %in% result$token, c(T, T))
})

test_that("do_tokenize with Japanese stopwords", {
  test_df <- data.frame(
    input = c('\u9ce5\u304c\u98DB\u3076'), # Tori-ga-tobu - Bird flies
    hiragana_word_length_to_remove = 0) # To really test the default stopwords, removal of short hiragane has to be disabled.
  result <- test_df %>%
    do_tokenize(input, stopwords_lang="japanese")
  expect_equal(c('\u9ce5','\u98DB\u3076'), # 'Tori', 'tobu' - Stop word 'ga' should be removed.
               result$token)
})

test_that("do_tokenize with stopwords and stopwords_to_remove", {
  test_df <- data.frame(
    input = c("Hello world!", "This is a data frame for test. This is second sentence."),
    extra_col = seq(2),
    stringsAsFactors = FALSE)
  result <- test_df %>%
    do_tokenize(input, stopwords_lang="english", stopwords=c("World"), stopwords_to_remove=c("is", "hello"))
  expect_equal(c("is", "hello") %in% result$token, c(T, T))
  expect_equal(c("world") %in% result$token, c(F))
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
  expect_equal(result$token[[1]], "Hello world!")
  expect_equal(ncol(result), 4)
})

test_that("do_tokenize_icu with keep_cols = TRUE with sentences", {
  test_df <- data.frame(
    input = c("Hello world!", "This is a data frame for test. This is second sentence."),
    extra_col = seq(2),
    stringsAsFactors = FALSE)
  result <- test_df %>%
    do_tokenize_icu(input, drop=FALSE, token = "word", keep_cols = TRUE)
  expect_equal(result$token[[1]], "hello")
  expect_equal(ncol(result), 5)
})

test_that("do_tokenize_icu with summary_level = all", {
  test_df <- data.frame(
    input = c("Hello world!", "This is a data frame for test. This is second sentence. Hello Hello!"),
    extra_col = seq(2),
    stringsAsFactors = FALSE)
  result <- test_df %>%
    do_tokenize_icu(input, drop=TRUE, token = "word", keep_cols = FALSE, summary_level = "all", with_id = FALSE, sort_by = "count")
  #  token   count
  #  <chr>   <int>
  #1 hello       3
  #2 is          2
  #3 this        2
  #4 a           1
  #5 data        1
  #6 for         1
  #7 frame       1
  #8 second      1
  #9 sentenc     1
  #10 test       1
  #11 world      1
  expect_equal(result$token[[1]], "hello")
  expect_equal(result$count[[1]], 3)
  expect_equal(nrow(result), 11)
})

test_that("do_tokenize with URLs and twitter social tags", {
  test_df <- data.frame(
    input = c("@ExploratoryData and #rstats see: https://cran.r-project.org \uff10\uff11\uff12")) # With test to strip full-width number.
  result <- test_df %>% do_tokenize(input, tokenize_tweets = TRUE, remove_url = FALSE, remove_twitter = FALSE)
  expect_equal(result$token, c("@ExploratoryData", "and", "#rstats", "see", "https://cran.r-project.org"))
  result <- test_df %>% do_tokenize(input, tokenize_tweets = TRUE)
  expect_equal(result$token, c("and", "see"))
  result <- test_df %>% do_tokenize(input) # By default, tokenize_words rather than tokenize_tweets is used for speed.
  expect_equal(result$token, c("exploratorydata", "and", "rstats", "see", "https", "cran.r", "project.org"))
})

test_that("do_tokenize with remove_numbers", {
  test_df <- data.frame(
    input = c("12345 aaa", "12aabb33", "123456 34567 88999"),
    extra_col = seq(3),
    stringsAsFactors = FALSE)
  result <- test_df %>%
    do_tokenize(input, drop=FALSE, keep_cols = TRUE, remove_numbers = TRUE)
  expect_equal(result$token[[1]], "aaa")
  expect_equal(result$token[[2]], "12aabb33")
})

test_that("do_tokenize with remove_punct", {
  test_df <- data.frame(
    input = c("#1 )*^%$ 2345 ^&*()", ":;:+-][", "00:01:00"),
    extra_col = seq(3),
    stringsAsFactors = FALSE)
  result <- test_df %>%
    do_tokenize(input, drop=FALSE, keep_cols = TRUE, remove_punct = FALSE, remove_numbers = FALSE)
  expect_equal(result$token[[1]], "#")
})

test_that("do_tokenize with token=words", {
  result <- test_df %>%
    do_tokenize(input, token="words", keep_cols = TRUE)
  expect_equal(result$token[[1]], "\u30AA\u30EA\u30F3\u30D4\u30C3\u30AF")
  expect_equal(ncol(result), 6)
})

test_that("do_tokenize when names conflict", {
  df <- test_df
  df$document_id <- seq(nrow(df))
  result <- df %>%
    do_tokenize(input, token="words", keep_cols = TRUE)
  expect_equal(result$token[[1]], "\u30AA\u30EA\u30F3\u30D4\u30C3\u30AF")
  expect_equal(ncol(result), 6)
  expect_equal(colnames(result)[[1]],"document_id") # If document_id is in the input, it is overwritten.
})

test_that("do_tokenize with token=sentence", {
  result <- test_df %>%
    do_tokenize(input, token="sentences")
  expect_equal(ncol(result), 2)
})

test_that("do_tokenize with token=sentence (with content check)", {
  result <- test_df_small %>%
    do_tokenize(input, token="sentences")
  expect_equal(result$token[[1]], "Hello world!")
  expect_equal(ncol(result), 2)
})

test_that("do_tokenize should work with output", {
  result <- test_df %>%
    do_tokenize(input, output="sentence", token="sentences")
  expect_equal(ncol(result), 2)
})

test_that("do_tokenize should work with output (with content check)", {
  result <- test_df_small %>%
    do_tokenize(input, output="sentence", token="sentences")
  expect_equal(result$sentence[[2]], "This is a data frame for test.")
})

test_that("calc_tfidf", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", letters[1:8]))
  result <- exploratory:::calc_tfidf(test_df$id, test_df$word)
  expect_equal(head(result$.df,2), c(2, 2))
  expect_equal(head(result$.tfidf,2), c(0, 0))
})

test_that("calc_tf weight binary", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", letters[1:8]))
  result <- exploratory:::calc_tf(test_df, id,word, weight="binary")
  expect_true(is.logical(result$tf))
  expect_equal(colnames(result)[[1]], "id")
  expect_equal(colnames(result)[[2]], "word")
  expect_equal(colnames(result)[[3]], "count_per_doc")
  expect_equal(colnames(result)[[4]], "tf")
})

test_that("calc_tfidf smooth_idf FALSE", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", letters[1:8]))
  result <- exploratory:::calc_tfidf(test_df$id, test_df$word)
  expect_equal(head(result$.tfidf,2), c(0, 0))
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

test_that("do_tfidf count column", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2, 3, 4, 5), 2), word=c("this", "this", "this", "this", "this", letters[1:5]), count = c(1,2,3,4,5,6,7,8,9,10))
  result <- (
    test_df %>%
      do_tfidf(id, word, tf_weight="binary", count = count)
  )
  expect_true(max(result$count_per_doc)==10)
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
  # Parse factor
  factor_data <- as.factor(c(1,2,3,4,5))
  ret <- exploratory::parse_number(factor_data)
  expect_equal(ret, c(1, 2, 3, 4, 5))
  expect_true(is.vector(ret))
  # Pass through input that is already numeric.
  ret <- exploratory::parse_number(c(1, 2.1))
  expect_equal(ret, c(1, 2.1))
  expect_true(is.vector(ret))
})

test_that("parse_logical", {
  ret <- exploratory::parse_logical(c(TRUE, FALSE))
  expect_equal(ret, c(TRUE, FALSE))
  # Parse character
  factor_data <- as.factor(c("TRUE", "FALSE"))
  ret <- exploratory::parse_logical(factor_data)
  expect_equal(ret, c(TRUE, FALSE))
  # Parse factor
  factor_data <- as.factor(c(0,1))
  ret <- exploratory::parse_logical(factor_data)
  expect_equal(ret, c(FALSE, TRUE))
})

test_that("str_extract_inside", {
  # bracket ()
  ret <- exploratory::str_extract_inside("abc(defgh)ijk", begin = "(", end =")", include_special_chars = FALSE)
  expect_equal(ret, c("defgh"))
  # curly bracket {}
  ret <- exploratory::str_extract_inside("abc(defgh)ijk", begin = "{", end ="}", include_special_chars = FALSE)
  expect_equal(ret, NA_character_)
  # curly bracket {}
  ret <- exploratory::str_extract_inside("abc{123456}ijk", begin = "{", end ="}", include_special_chars = FALSE)
  expect_equal(ret, "123456")
  # curly bracket []
  ret <- exploratory::str_extract_inside("abc[123456]ijk", begin = "[", end ="]", include_special_chars = FALSE)
  expect_equal(ret, "123456")
  # double quote ""
  ret <- exploratory::str_extract_inside('abc"123456"ijk', begin = '"', end = '"', include_special_chars = FALSE)
  expect_equal(ret, "123456")
  # single quote ''
  ret <- exploratory::str_extract_inside("abc'123456'ijk", begin = "'", end = "'", include_special_chars = FALSE)
  expect_equal(ret, "123456")
  # percent %
  ret <- exploratory::str_extract_inside("abc%123456%ijk", begin = "%", end = "%", include_special_chars = FALSE)
  expect_equal(ret, "123456")
  # percent $
  ret <- exploratory::str_extract_inside("abc$123456$ijk", begin = "$", end = "$", include_special_chars = FALSE)
  expect_equal(ret, "123456")
  # percent * $
  ret <- exploratory::str_extract_inside("abc*123456$ijk", begin = "*", end = "$", include_special_chars = FALSE)
  expect_equal(ret, "123456")

  tryCatch({
    ret <- exploratory::str_extract_inside("abc*123456$ijk", begin = "{{", end = "}")
  }, error = function(e){
    expect_equal(e$message, "The begin argument must be one character.")
  })

  tryCatch({
    ret <- exploratory::str_extract_inside("abc*123456$ijk", begin = "n", end = "}")
  }, error = function(e){
    expect_equal(e$message, "The begin argument must be symbol such as (, {, [.")
  })

  tryCatch({
    ret <- exploratory::str_extract_inside("abc*123456$ijk", begin = "{", end = "}}")
  }, error = function(e){
    expect_equal(e$message, "The end argument must be one character.")
  })

  tryCatch({
    ret <- exploratory::str_extract_inside("abc*123456$ijk", begin = "{", end = "z")
  }, error = function(e){
    expect_equal(e$message, "The end argument must be symbol such as ), }, ].")
  })


})

test_that("str_remove", {
  ret <- exploratory::str_remove("test group", "group", remove_extra_space = TRUE)
  expect_equal(ret, "test")
})

test_that("str_remove_all", {
  ret <- exploratory::str_remove_all("test capital group", "group|capital", remove_extra_space = TRUE)
  expect_equal(ret, "test")
})

test_that("str_remove_inside", {
  # bracket ()
  ret <- exploratory::str_remove_inside("abc(defgh)ijk", begin = "(", end =")")
  expect_equal(ret, c("abcijk"))
  # curly bracket {}
  ret <- exploratory::str_remove_inside("abc(defgh)ijk", begin = "{", end ="}")
  expect_equal(ret,c("abc(defgh)ijk"))
  # curly bracket {}
  ret <- exploratory::str_remove_inside("abc{123456}ijk", begin = "{", end ="}")
  expect_equal(ret, "abcijk")
  # curly bracket []
  ret <- exploratory::str_remove_inside("abc[123456]ijk", begin = "[", end ="]")
  expect_equal(ret, "abcijk")
  # double quote ""
  ret <- exploratory::str_remove_inside('abc"123456"ijk', begin = '"', end = '"')
  expect_equal(ret, "abcijk")
  # single quote ''
  ret <- exploratory::str_remove_inside("abc'123456'ijk", begin = "'", end = "'")
  expect_equal(ret, "abcijk")
  # percent %
  ret <- exploratory::str_remove_inside("abc%123456%ijk", begin = "%", end = "%")
  expect_equal(ret, "abcijk")
  # percent $
  ret <- exploratory::str_remove_inside("abc$123456$ijk", begin = "$", end = "$")
  expect_equal(ret, "abcijk")
  # percent * $
  ret <- exploratory::str_remove_inside("abc*123456$ijk", begin = "*", end = "$")
  expect_equal(ret, "abcijk")

  ret <- exploratory::str_remove_inside("abc(123)4(56$ij)k", begin = "(", end = ")", all = TRUE)
  expect_equal(ret, "abc4k")

  ret <- exploratory::str_remove_inside("abc[12(34)56]ijk", begin = "[", end = "]", all = TRUE)
  expect_equal(ret, "abcijk")

  ret <- exploratory::str_remove_inside("abc[12(34)56]ijk", begin = "(", end = ")", all = TRUE)
  expect_equal(ret, "abc[1256]ijk")

  ret <- exploratory::str_remove_inside("abc(123(456)$ij)k", begin = "(", end = ")", all = TRUE)
  ret <- exploratory::str_remove_inside(ret)
  expect_equal(ret, "abck")

  tryCatch({
    ret <- exploratory::str_remove_inside("abc*123456$ijk", begin = "{{", end = "}")
  }, error = function(e){
    expect_equal(e$message, "The begin argument must be one character.")
  })

  tryCatch({
    ret <- exploratory::str_remove_inside("abc*123456$ijk", begin = "n", end = "}")
  }, error = function(e){
    expect_equal(e$message, "The begin argument must be symbol such as (, {, [.")
  })

  tryCatch({
    ret <- exploratory::str_remove_inside("abc*123456$ijk", begin = "{", end = "}}")
  }, error = function(e){
    expect_equal(e$message, "The end argument must be one character.")
  })

  tryCatch({
    ret <- exploratory::str_remove_inside("abc*123456$ijk", begin = "{", end = "z")
  }, error = function(e){
    expect_equal(e$message, "The end argument must be symbol such as ), }, ].")
  })


})

test_that("str_replace_inside", {
  # bracket ()
  ret <- exploratory::str_replace_inside("abc(defgh)ijk", begin = "(", end =")", rep = "AA")
  expect_equal(ret, c("abcAAijk"))
  # curly bracket {}
  ret <- exploratory::str_replace_inside("abc(defgh)ijk", begin = "{", end ="}", rep = "BB")
  expect_equal(ret,c("abc(defgh)ijk"))
  # curly bracket {}
  ret <- exploratory::str_replace_inside("abc{123456}ijk", begin = "{", end ="}", rep = "CC")
  expect_equal(ret, "abcCCijk")
  # curly bracket []
  ret <- exploratory::str_replace_inside("abc[123456]ijk", begin = "[", end ="]", rep = "DD")
  expect_equal(ret, "abcDDijk")
  # double quote ""
  ret <- exploratory::str_replace_inside('abc"123456"ijk', begin = '"', end = '"', rep = "EE")
  expect_equal(ret, "abcEEijk")
  # single quote ''
  ret <- exploratory::str_replace_inside("abc'123456'ijk", begin = "'", end = "'", rep = "FF")
  expect_equal(ret, "abcFFijk")
  # percent %
  ret <- exploratory::str_replace_inside("abc%123456%ijk", begin = "%", end = "%", rep = "GG")
  expect_equal(ret, "abcGGijk")
  # percent $
  ret <- exploratory::str_replace_inside("abc$123456$ijk", begin = "$", end = "$", rep = "HH")
  expect_equal(ret, "abcHHijk")
  # percent * $
  ret <- exploratory::str_replace_inside("abc*123456$ijk", begin = "*", end = "$", rep = "II")
  expect_equal(ret, "abcIIijk")

  ret <- exploratory::str_replace_inside("abc(123)4(56$ij)k", begin = "(", end = ")", all = TRUE, rep = "JJ")
  expect_equal(ret, "abcJJ4JJk")

  ret <- exploratory::str_replace_inside("abc[12(34)56]ijk", begin = "[", end = "]", all = TRUE, rep = "KK")
  expect_equal(ret, "abcKKijk")

  ret <- exploratory::str_replace_inside("abc[12(34)56]ijk", begin = "(", end = ")", all = TRUE, rep = "LL")
  expect_equal(ret, "abc[12LL56]ijk")

  ret <- exploratory::str_replace_inside("abc(123(456)$ij)k", begin = "(", end = ")", all = TRUE, rep = "MM")
  ret <- exploratory::str_replace_inside(ret, begin = "(", end = ")", rep = "MM")
  expect_equal(ret, "abcMMk")

  tryCatch({
    ret <- exploratory::str_remove_inside("abc*123456$ijk", begin = "{{", end = "}")
  }, error = function(e){
    expect_equal(e$message, "The begin argument must be one character.")
  })

  tryCatch({
    ret <- exploratory::str_remove_inside("abc*123456$ijk", begin = "n", end = "}")
  }, error = function(e){
    expect_equal(e$message, "The begin argument must be symbol such as (, {, [.")
  })

  tryCatch({
    ret <- exploratory::str_remove_inside("abc*123456$ijk", begin = "{", end = "}}")
  }, error = function(e){
    expect_equal(e$message, "The end argument must be one character.")
  })

  tryCatch({
    ret <- exploratory::str_remove_inside("abc*123456$ijk", begin = "{", end = "z")
  }, error = function(e){
    expect_equal(e$message, "The end argument must be symbol such as ), }, ].")
  })

})

test_that("str_remove_emoji", {
  text = c("Hello\uD83D\uDE00", # Smile Face
           "Hello\uD83D\uDC4D", # Thumbs Up
           "Hello\u2757\ufe0f", # exclamation mark with variation selector (\ufe0f).
           "Hello\ud83c\udf99\ufe0f", # 2-character emoji (studio microphone) with variation selector
           "Hello\U0001f481\u200d\u2640\ufe0f") # Female greeting
  ret <- exploratory::str_remove_emoji(text)
  expect_equal(ret, c("Hello", "Hello", "Hello", "Hello", "Hello"))
})

test_that("str_remove_word", {
  ret <- exploratory::str_remove_word("Sequoia Capital China, Qiming Venture Partners, Tencent Holdings", -1, sep = "\\s*\\,\\s*")
  expect_equal(ret, c("Sequoia Capital China, Qiming Venture Partners"))
  ret <- exploratory::str_remove_word("Sequoia Capital China, Qiming Venture Partners, Tencent Holdings", 1, sep = "\\s*\\,\\s*")
  expect_equal(ret, c("Qiming Venture Partners, Tencent Holdings"))
  ret <- exploratory::str_remove_word("Sequoia Capital, Qiming Venture Partners, Tencent Holdings (China Space)", -1, sep = "\\s+")
  expect_equal(ret, c("Sequoia Capital, Qiming Venture Partners, Tencent Holdings (China"))

})

test_that("str_replace_word", {
  ret <- exploratory::str_replace_word("Sequoia Capital China, Qiming Venture Partners, Tencent Holdings", -1, sep = "\\s*\\,\\s*", rep = "Last One")
  expect_equal(ret, c("Sequoia Capital China, Qiming Venture Partners, Last One"))
  ret <- exploratory::str_replace_word("Sequoia Capital China, Qiming Venture Partners, Tencent Holdings", 1, sep = "\\s*\\,\\s*", rep = "First One")
  expect_equal(ret, c("First One, Qiming Venture Partners, Tencent Holdings"))
})

test_that("str_replace_url", {
  ret <- exploratory::str_replace_url("Check out what I just added to my closet on Poshmark: Uniqlo Girls Uniform Pants. https://t.co/tMfGP512D2 via @poshmarkapp #shopmycloset", "New York")
  expect_equal(ret, c("Check out what I just added to my closet on Poshmark: Uniqlo Girls Uniform Pants. New York via @poshmarkapp #shopmycloset"))
})

test_that("str_remove_url", {
  ret <- exploratory::str_remove_url("Check out what I just added to my closet on Poshmark: Uniqlo Girls Uniform Pants. https://t.co/tMfGP512D2 via @poshmarkapp #shopmycloset", "New York")
  expect_equal(ret, "Check out what I just added to my closet on Poshmark: Uniqlo Girls Uniform Pants.  via @poshmarkapp #shopmycloset")
})

test_that("str_extract_url", {
  ret <- exploratory::str_extract_url("Check out what I just added to my closet on Poshmark: Uniqlo Girls Uniform Pants. https://t.co/tMfGP512D2 via @poshmarkapp #shopmycloset", "New York")
  expect_equal(ret, list("https://t.co/tMfGP512D2"))
})

test_that("str_logical", {
  ret <- exploratory::str_logical(c("yes", "yEs", "yeS", " YEs", "YeS ", "yES", "YES","no", "No", "nO", "NO ", NA))
  expect_equal(ret, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, NA))
  ret <- exploratory::str_logical(as.factor(c("yes", "yEs", "yeS", " YEs", "YeS ", "yES", "YES","no", "No", "nO", "NO ", NA)))
  expect_equal(ret, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, NA))
  ret <- exploratory::str_logical(c("true", "tRue", "trUe", "truE", "TRue", "TrUe", "TruE", "TRUe", "TRuE", "TrUE", "tRUE", "TRUE","false", "FALSE", NA))
  expect_equal(ret, c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, NA))
  ret <- exploratory::str_logical(c("1", "0", "0", "1", "1", "1", "1", "0", "1", "1", "1", "1","1", "0", NA))
  expect_equal(ret, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, NA))
  ret <- exploratory::str_logical(c(1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1,1, 0, NA))
  expect_equal(ret, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, NA))
  ret <- exploratory::str_logical(as.integer(c(1, 0, 0, 2, 2, 3, 4, 0, 100, 1000, 10, 11,12, 0, NA)))
  expect_equal(ret, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, NA))
  ret <- exploratory::str_logical(as.double(c(1.1, 0, 0, 2.2, 2.3, 3.5, 4.3, 0, 100.01, 1000.1234, 10.34343, 11.11,12.89, 0, NA)))
  expect_equal(ret, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, NA))
  ret <- exploratory::str_logical(as.numeric(c(1.1, 0, 0, 2.2, 2.3, 3.5, 4.3, 0, 100.01, 1000.1234, 10.34343, 11.11,12.89, 0, NA)))
  expect_equal(ret, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, NA))
  ret <- exploratory::str_logical(bit64::as.integer64(c(1.1, 0, 0, 2.2, 2.3, 3.5, 4.3, 0, 100.01, 1000.1234, 10.34343, 11.11,12.89, 0, NA)))
  expect_equal(ret, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, NA))
  ret <- exploratory::str_logical(c("Sign Up", "Not Sign Up", "Not Sign Up", "sign Up", "sign up", "SIGN UP", "Sign UP", "Not Sign Up", "Sign Up", "Sign Up", "Sign Up", "Sign Up","Sign Up", "Not Sign Up", NA), true_value = "Sign Up")
  expect_equal(ret, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, NA))
  ret <- exploratory::str_logical(c("yes", "ddd", "cc", "ee", "1", "0", 1, 0, "true", "false", "aa", "","", NA, NA))
  expect_equal(ret, c(TRUE, NA, NA, NA, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, NA, NA, NA, NA, NA))
  ret <- exploratory::str_logical(c("yes", "ddd", "cc", "ee", "1", "0", 1, 0, "true", "false", "YES", "","", NA, NA), true_value = "yes")
  expect_equal(ret, c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, NA, NA))
  ret <- exploratory::str_logical(c(NA, NA, NA, NA, NA, NA, NA, NA))
  expect_equal(ret, c(NA, NA, NA, NA, NA, NA, NA, NA))
})

test_that("str_detect", {
  ret <- exploratory::str_detect(c("Test", "test", "ATe"), "Te")
  expect_equal(ret, c(TRUE, FALSE, TRUE))
  ret <- exploratory::str_detect(c("Test", "test", "tEST", "abc"), "Te", ignore_case = TRUE)
  expect_equal(ret, c(TRUE, TRUE, TRUE, FALSE))
  # When pattern is empty string, it should always match.
  ret <- exploratory::str_detect(c("Test", "test", "tEST", "abc"), "")
  expect_equal(ret, c(TRUE, TRUE, TRUE, TRUE))
  ret <- exploratory::str_detect(c("Test", "test", "tEST", "abc"), "", ignore_case = TRUE)
  expect_equal(ret, c(TRUE, TRUE, TRUE, TRUE))
  ret <- exploratory::str_detect(c("Test", "test", "tEST", "abc"), "", negate =TRUE, ignore_case = TRUE)
  expect_equal(ret, c(FALSE, FALSE, FALSE, FALSE))
  ret <- exploratory::str_detect(c("Aabc", "baadd", "dddd"), stringr::regex(stringr::str_c("AA"), ignore_case=TRUE))
  expect_equal(ret, c(TRUE, TRUE, FALSE))

  # below is the original tests from stringr::str_detect
  expect_equal(exploratory::str_detect(NA, "x"), NA)
  expect_equal(exploratory::str_detect(character(), "x"), logical())
  expect_equal(exploratory::str_detect("ab", c("a", "b", "c")), c(T, T, F))
  expect_equal(exploratory::str_detect(c("ca", "ab"), c("a", "c")), c(T, F))
  # negation works
  expect_equal(exploratory::str_detect("ab", c("a", "b", "c"), negate = TRUE), c(F, F, T))
  expect_false(exploratory::str_detect("ab", "AB"))
  expect_true(exploratory::str_detect("ab", stringr::regex("AB", TRUE)))

  expect_true(exploratory::str_detect("abc", "ab[c]"))
  expect_false(exploratory::str_detect("abc", stringr::fixed("ab[c]")))
  expect_true(exploratory::str_detect("ab[c]", stringr::fixed("ab[c]")))
  expect_true(exploratory::str_detect("ab[c]", stringr::coll("ab[c]")))

  expect_true(exploratory::str_detect("abc", "(?x)a b c"))
})

test_that("str_remove_range", {
  ret <- exploratory::str_remove_range(c("Aaron Bergman", "Justin Ritter", "Craig Reiter"), -4, -3)
  expect_equal(ret, c("Aaron Beran", "Justin Rier", "Craig Reer"))
  ret2 <- exploratory::str_remove_range(c("Aaron Bergman", "Justin Ritter", "Craig Reiter"), 1, 3)
  expect_equal(ret2, c("on Bergman", "tin Ritter", "ig Reiter"))
  ret2 <- exploratory::str_remove_range(c("Aaron Bergman", "Justin Ritter", "Craig Reiter"), -3, -9)
  expect_equal(ret2, c("Aaron Bergman", "Justin Ritter", "Craig Reiter"))
})

test_that("str_replace_range: negative index", {
  ret <- exploratory::str_replace_range(c("Aaron Bergman", "Justin Ritter", "Craig Reiter"), -4, -3, "AAA")
  expect_equal(ret, c("Aaron BerAAAan", "Justin RiAAAer", "Craig ReAAAer"))
  ret2 <- exploratory::str_replace_range(c("Aaron Bergman", "Justin Ritter", "Craig Reiter"), 1, 3, "AAA")
  expect_equal(ret2, c("AAAon Bergman", "AAAtin Ritter", "AAAig Reiter"))
  ret <- exploratory::str_replace_range(c("Aaron Bergman", "Justin Ritter", "Craig Reiter"), -3, -9, "AAA")
  expect_equal(ret, c("Aaron Bergman", "Justin Ritter", "Craig Reiter"))
})
