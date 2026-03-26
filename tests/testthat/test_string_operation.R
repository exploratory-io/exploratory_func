context("test string operation functions")

# Small data for easier deterministic result checking.
test_df_small <- data.frame(input = c("Hello world!", "This is a data frame for test. This is second sentence.", NA), stringsAsFactors = FALSE)
# Data from twitter search. This happens to include invalid UTF-8 byte sequences too.
test_df <- exploratory::read_delim_file("https://www.dropbox.com/s/w1fh7j8iq6g36ry/Twitter_No_Spectator_Olympics_Ja.csv?dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)
test_df <- test_df %>% rename(input=text) # Rename so that it has same column name as test_df_small.


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


test_that("test get_stopwords", {
  result <- get_stopwords()
  expect_true(any(result == "a"))
  expect_true(any(result == "amp"))
  expect_true(any(result == "http"))
  expect_true(any(result == "https"))
  expect_true(any(result == "t.co"))
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


test_that("str_detect", {
  ret <- exploratory::str_detect(c("Test", "test", "ATe"), "Te")
  expect_equal(ret, c(TRUE, FALSE, TRUE))
  ret <- exploratory::str_detect(c("Test", "test", "tEST", "abc"), "Te", ignore_case = TRUE)
  expect_equal(ret, c(TRUE, TRUE, TRUE, FALSE))
  # When pattern is empty string, it used to always match, but since stringr 1.5.0, it returns error.
  # so we introduce the allow_empty_pattern so that setting this to TRUE behaves as same as stringr::str_detect.
  # If this allow_empty_pattern is TRUE, it behaves as same as pre-1.5.0
  expect_error({
    ret <- exploratory::str_detect(c("Test", "test", "tEST", "abc"), "", allow_empty_pattern = FALSE)
  })
  expect_error({
    ret <- exploratory::str_detect(c("Test", "test", "tEST", "abc"), "", ignore_case = TRUE, allow_empty_pattern = FALSE)
  })
  expect_error({
    ret <- exploratory::str_detect(c("Test", "test", "tEST", "abc"), "", negate =TRUE, ignore_case = TRUE, allow_empty_pattern = FALSE)
  })
  ret <- exploratory::str_detect(c("Test", "test", "tEST", "abc"), "")
  expect_equal(ret, c(TRUE, TRUE, TRUE,TRUE))
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


test_that("str_replace_range: negative index", {
  ret <- exploratory::str_replace_range(c("Aaron Bergman", "Justin Ritter", "Craig Reiter"), -4, -3, "AAA")
  expect_equal(ret, c("Aaron BerAAAan", "Justin RiAAAer", "Craig ReAAAer"))
  ret2 <- exploratory::str_replace_range(c("Aaron Bergman", "Justin Ritter", "Craig Reiter"), 1, 3, "AAA")
  expect_equal(ret2, c("AAAon Bergman", "AAAtin Ritter", "AAAig Reiter"))
  ret <- exploratory::str_replace_range(c("Aaron Bergman", "Justin Ritter", "Craig Reiter"), -3, -9, "AAA")
  expect_equal(ret, c("Aaron Bergman", "Justin Ritter", "Craig Reiter"))
})




test_that("str_replace_before", {
  ret <- exploratory::str_replace_before(c("kei@exploratory.io", "hideaki@exploratory.io", "hide@exploratory.io"), sep = "@", rep = "dev")
  expect_equal(ret, c("devexploratory.io", "devexploratory.io", "devexploratory.io"))
  ret <- exploratory::str_replace_before(c("kei@exploratory.io", "hideaki@exploratory.io", "hide@exploratory.io"), sep = "@", rep = "dev", include_sep = FALSE)
  expect_equal(ret, c("dev@exploratory.io", "dev@exploratory.io", "dev@exploratory.io"))
})

test_that("str_replace_after", {
  ret <- exploratory::str_replace_after(c("kei@exploratory.io", "hideaki@exploratory.io", "hide@exploratory.io"), sep = "@", rep = "test")
  expect_equal(ret, c("keitest", "hideakitest", "hidetest"))
  ret <- exploratory::str_replace_after(c("kei@exploratory.io", "hideaki@exploratory.io", "hide@exploratory.io"), sep = "@", rep = "test", include_sep = FALSE)
  expect_equal(ret, c("kei@test", "hideaki@test", "hide@test"))
})



