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
  expect_equal(result$token[[1]], "hello world")
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
  expect_equal(result$token[[1]], "hello world")
  expect_equal(ncol(result), 2)
})

test_that("do_tokenize should work with output", {
  result <- test_df %>%
    do_tokenize(input, output=sentence, token="sentences")
  expect_equal(result$sentence[[2]], "this is a data frame for test")
})

test_that("calc_idf", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", letters[1:8]))
  result <- exploratory:::calc_idf(test_df$id, test_df$word)
  expect_equal(head(result$.df,2), c(2, 2))
  expect_equal(head(result$.idf,2), c(0, 0))
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

test_that("calc_idf smooth_idf FALSE", {
  loadNamespace("dplyr")
  test_df <- data.frame(id=rep(c(1,2), 5), word=c("this", "this", letters[1:8]))
  result <- result <- exploratory:::calc_idf(test_df$id, test_df$word)
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
  # Smile Face and Thumbs Up.
  text = c("\uD83D\uDE00", "\uD83D\uDC4D")
  ret <- exploratory::str_remove_emoji(text)
  expect_equal(ret, list("",""))
})

test_that("str_remove_word", {
  ret <- exploratory::str_remove_word("Sequoia Capital China, Qiming Venture Partners, Tencent Holdings", -1, sep = "\\s*\\,\\s*")
  expect_equal(ret, c("Sequoia Capital China, Qiming Venture Partners"))
  ret <- exploratory::str_remove_word("Sequoia Capital China, Qiming Venture Partners, Tencent Holdings", 1, sep = "\\s*\\,\\s*")
  expect_equal(ret, c("Qiming Venture Partners, Tencent Holdings"))
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

