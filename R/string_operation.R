#' Check if the token is in stopwords.
#' @param token Character to be checked if it's stopword.
#' @param lang Type of stopwords.
#' One of
#' "danish",
#' "dutch",
#' "english",
#' "english_snowball",
#' "english_smart",
#' "english_onix",
#' "finnish",
#' "french",
#' "german",
#' "hungarian",
#' "italian",
#' "japanese",
#' "norwegian",
#' "portuguese",
#' "russian",
#' "spanish",
#' "swedish",
#' "smart",
#' "snowball",
#' "onix"
#' @param include Values that should be included as stopwords
#' @param exclude Values that should be excluded from stopwords
#' @return Logical vector if the token is in stop words or not.
#' @export
is_stopword <- function(token, lang = "english", include = c(), exclude = c(), ...){
  if(lang == "japanese") { # for Japanese, assume the token is stopword if it's one letter
    token %in% get_stopwords(lang, include = include, exclude = exclude, ...) || str_detect(token, "^[\\\u3040-\\\u309f]$")
  }
  token %in% get_stopwords(lang, include = include, exclude = exclude, ...)
}

#' Check if the word is digits.
#' @param word Character to be checked if it's digits.
#' @return Logical vector if the word is digits or not.
is_digit <- function(word){
  loadNamespace("stringr")
  stringr::str_detect(word, "^[[:digit:]]+$")
}

#' Check if the word is digits.
#' @param word Character to be checked if it's digits.
#' @return Logical vector if the word is digits or not.
#' @export
is_alphabet <- function(word){
  loadNamespace("stringr")
  # To treat non-ascii characters as FALSE, use [a-zA-Z]
  # instead of [:alpha:]
  stringr::str_detect(word, "^[a-zA-Z]+$")
}

#' Get vector of stopwords
#' @param lang Type of stopwords.
#' One of
#' "danish",
#' "dutch",
#' "english",
#' "english_snowball",
#' "english_smart",
#' "english_onix",
#' "finnish",
#' "french",
#' "german",
#' "hungarian",
#' "italian",
#' "japanese",
#' "norwegian",
#' "portuguese",
#' "russian",
#' "spanish",
#' "swedish"
#' @param include Values that should be included as stop words
#' @param exclude Values that should be excluded from stop words
#' @param is_twitter flag that tells if you want to get twitter related stopwords such as http and https.
#' @return vector of stop words.
#' @export
get_stopwords <- function(lang = "english", include = c(), exclude = c(), is_twitter = TRUE){
  if(!requireNamespace("tidystopwords")){stop("package tidystopwords must be installed.")}
  lang <- tolower(lang)
  stopwords <- if (lang %in% c(
    "english_snowball",
    "english_onix",
    "english_smart",
    "japanese")){
    # these data are created from data-raw/create_internal_data.R
    get(paste0("stopwords_", lang))
  } else {
    loadNamespace("tm")
    tm::stopwords(kind = lang)
  }
  # if is_twitter argument is true, append exploratory_stopwords which contains stopwords for twitter
  if(is_twitter) {
    stopwords <- append(stopwords, exploratory_stopwords)
  }
  # tidystopwords required language name with Titlt Case so make sure it's title case.
  ret <- c(stopwords[!stopwords %in% exclude], include, tidystopwords::generate_stoplist(stringr::str_to_title(lang)))
  ret
}

#' Get sentiments of words
#' @param words Vector of words to check sentiment.
#' @param lexicon Type of sentiment. One of "nrc" "bing" "AFINN".
#' @return Vector of sentiment.
#' @export
word_to_sentiment <- function(words, lexicon="bing"){
  loadNamespace("tidytext")
  loadNamespace("dplyr")
  # get data saved internally in this package by chosen lexicon
  sentiment <- get(paste0("sentiment_", lexicon))
  # sentiment is named vector (for "bing" and "AFINN")
  # or named list (for "nrc" because it can have many sentiment types for one word)
  # this is faster than using left join
  ret <- sentiment[words]
  # remove the name
  names(ret) <- NULL
  ret
}

#' Tokenize text with ICU.
#' @param df Data frame
#' @param text Set a column of which you want to tokenize.
#' @param token Select the unit of token from "character" or "word".
#' @param keep_cols Whether existing columns should be kept or not.
#' @param drop Whether input column should be removed.
#' @param with_id Whether output should contain original document id in each document.
#' @param output Set a column name for the new column to store the tokenized values.
#' @return Data frame with tokenized column.
#' @export
do_tokenize_icu <- function(df, text_col, token = "word", keep_cols = FALSE,
                                 drop = TRUE, with_id = TRUE, output = token,
                                 remove_punc = TRUE, remove_numbers = TRUE,
                                 remove_hyphens = FALSE, remove_separators = TRUE,
                                 remove_symbols = TRUE, remove_twitter = TRUE,
                                 remove_url = TRUE, stopwords_lang = NULL, ...){

  if(!requireNamespace("quanteda")){stop("package quanteda must be installed.")}
  if(!requireNamespace("dplyr")){stop("package dplyr must be installed.")}
  if(!requireNamespace("tidyr")){stop("package tidyr must be installed.")}
  if(!requireNamespace("stringr")){stop("package stringr must be installed.")}

  # Always put document_id to know what document the tokens are from
  doc_id <- avoid_conflict(colnames(df), "document_id")
  output_col <- avoid_conflict(colnames(df), col_name(substitute(output)))
  # For the output column names (i.e. "token" and "count"), make sure that
  # these column names become unique in case we keep original columns.
  count_col <- "count"
  token_col <- output_col
  if(keep_cols) {
    count_col <- avoid_conflict(colnames(df), "count")
  }
  # This is SE version of dplyr::mutate(df, doc_id = row_number())
  df <- dplyr::mutate_(df, .dots=setNames(list(~row_number()),doc_id))
  orig_input_col <- col_name(substitute(text_col))
  textData <- df %>% dplyr::select(orig_input_col) %>% dplyr::rename("text" = orig_input_col)
  # Create a corpus from the text column then tokenize.
  tokens <- quanteda::corpus(textData) %>%
    quanteda::tokens(what = token, remove_punc = remove_punc, remove_numbers = remove_numbers,
                     remove_symbols = remove_symbols, remove_twitter = remove_twitter,
                     remove_hyphens = remove_hyphens, remove_separators = remove_separators,
                     remove_url = remove_url)
  dfm <- tokens %>% quanteda::tokens_wordstem() %>%
    quanteda::dfm()
  # Now convert result dfm to a data frame
  resultTemp <- quanteda::convert(dfm, to = "data.frame")
  # The first column name returned by quanteda::convert is always "document" so rename it to avoid it conflicts with other tokens
  docCol <- resultTemp[c(1)]
  # Exclude the "document" column
  result <- resultTemp[c(-1)]
  # Then bring the column back as internal document column
  result$document.exp.token.col <- docCol$document

  # Convert the data frame from "wide" to "long" format by tidyr::gather.
  result <- result %>% tidyr::gather(key = !!token_col, value = !!count_col, which(sapply(., is.numeric)), na.rm = TRUE, convert = TRUE) %>%
    # Exclude unused tokens for the document.
    dplyr::filter(!!as.name(count_col) > 0) %>%
    # The internal document column value looks like "text100". Remove text part to make it numeric.
    dplyr::mutate(document_id = as.numeric(stringr::str_remove(document.exp.token.col, "text"))) %>%
    # Drop document internal document column
    dplyr::select(-document.exp.token.col) %>%
    # Sort result by document_id to align with original data frame's order.
    dplyr::arrange(document_id)

  if(keep_cols) {
    # If we need to keep original columns, then bring them back by joining the result data frame with original data frame.
    result <- df %>%  dplyr::left_join(result, by=c("document_id", "document_id"))
    if(drop) { # drop the text column
      result <- result %>% dplyr::select(-orig_input_col)
    }
  } else if(!drop) { # Bring back the text column by by joining the result data frame with original data frame and drop unwanted columns.
    result <- df %>% dplyr::left_join(result, by=c("document_id", "document_id")) %>%
      select(document_id, !!count_col, !!token_col, orig_input_col);
  }
  if(!with_id) { # Drop the document_id column
    result <- result %>% dplyr::select(-document_id)
  }
  # if stopwords_lang is provided, remove the stopwords for the language.
  if(!is.null(stopwords_lang)) {
    stop_words <- exploratory::get_stopwords(lang = stopwords_lang)
    result <- result %>% dplyr::filter(!!as.name(token_col) %nin% stop_words)
    if(stringr::str_to_lower(stopwords_lang) == "japanese") { # for Japanese exclude one letter Hiragana
      result <- result %>% dplyr::filter(!stringr::str_detect(!!as.name(token_col), "^[\\\u3040-\\\u309f]$") )
    }
  }

  result
}

#' Tokenize text and unnest
#' @param df Data frame
#' @param input Set a column of which you want to split the text or tokenize.
#' @param token Select the unit of token from "characters", "words", "sentences", "lines", "paragraphs", and "regex".
#' @param keep_cols Whether existing columns should be kept or not
#' @param drop Whether input column should be removed.
#' @param with_id Whether output should contain original document id and sentence id in each document.
#' @param output Set a column name for the new column to store the tokenized values.
#' @param to_lower Whether output should be lower cased.
#' @return Data frame with tokenized column
#' @export
do_tokenize <- function(df, input, token = "words", keep_cols = FALSE,  drop = TRUE, with_id = TRUE, output = token, stopwords_lang = NULL, ...){
  validate_empty_data(df)

  loadNamespace("tidytext")
  loadNamespace("stringr")

  orig_input_col <- col_name(substitute(input))
  input_col <- avoid_conflict(colnames(df), "input")
  # since unnest_tokens_ does not handle column with space well, rename column name temporarily.
  # the following does not work with error that says := is unknown. do it base-R way.
  # df <- dplyr::rename(!!rlang::sym(input_col) := !!rlang::sym(orig_input_col))
  colnames(df)[colnames(df) == orig_input_col] <- input_col

  output_col <- avoid_conflict(colnames(df), col_name(substitute(output)))
  # This is to prevent encoding error
  df[[input_col]] <- stringr::str_conv(df[[input_col]], "utf-8")

  if(!keep_cols){
    # keep only a column to tokenize
    df <- df[input_col]
  }

  if(with_id){
    # always put document_id to know what document the tokens are from
    doc_id <- avoid_conflict(colnames(df), "document_id")
    # this is SE version of dplyr::mutate(df, doc_id = row_number())
    df <- dplyr::mutate_(df, .dots=setNames(list(~row_number()),doc_id))
  }

  if(token %in% c("words", "characters") && with_id){
    # get sentence_id too in this case for each token

    loadNamespace("dplyr")

    # split into sentences
    func <- get("unnest_tokens_", asNamespace("tidytext"))

    # use unnest_token only with document_id and input_col,
    # so that it won't duplicate other columns,
    # which will be joined later
    tokenize_df <- df[,c(doc_id, input_col)]
    sentences <- tidytext::unnest_tokens_(tokenize_df, output_col, input_col, token="sentences", drop=TRUE, ...)

    # as.symbol is used for colum names with backticks
    grouped <- dplyr::group_by(sentences, !!!rlang::syms(doc_id))

    # put sentence_id
    sentence_id <- avoid_conflict(colnames(df), "sentence_id")
    tokenize_df <- dplyr::mutate_(grouped, .dots=setNames(list(~row_number()), sentence_id))

    # split into tokens
    tokenize_df <- dplyr::ungroup(tokenize_df)
    tokenized <- tidytext::unnest_tokens_(tokenize_df, output_col, output_col, token=token, drop=TRUE, ...)

    if(drop){
      df[[input_col]] <- NULL
    }

    ret <- dplyr::right_join(df, tokenized, by=doc_id)
  } else {
    ret <- tidytext::unnest_tokens_(df, col_name(substitute(output)), input_col, token=token, drop=drop, ...)
  }

  # set the column name to original.
  # the following does not work with error that says := is unknown. do it base-R way.
  # ret <- ret %>% dplyr::rename(!!rlang::sym(orig_input_col) := !!rlang::sym(input_col))
  colnames(ret)[colnames(ret) == input_col] <- orig_input_col
  # if stopwords_lang is provided, remove the stopwords for the language.
  if(!is.null(stopwords_lang)) {
    stop_words <- exploratory::get_stopwords(lang = stopwords_lang)
    ret <- ret %>% dplyr::filter(!!as.name(output_col) %nin% stop_words)
  }

  ret
}

#' Get idf for terms
calc_idf <- function(group, term, smooth_idf = FALSE){
  loadNamespace("Matrix")
  loadNamespace("text2vec")
  if(length(group)!=length(term)){
    stop("length of document and terms have to be the same")
  }
  doc_fact <- as.factor(group)
  term_fact <- as.factor(term)
  sparseMat <- Matrix::sparseMatrix(i = as.numeric(doc_fact), j = as.numeric(term_fact))

  m_tfidf <- text2vec::TfIdf$new(smooth_idf = smooth_idf, norm = "none")
  result_tfidf <- m_tfidf$fit_transform(sparseMat)
  idf <- result_tfidf@x[term_fact]
  df <- Matrix::colSums(sparseMat)[term_fact]

  data.frame(.df=df, .idf=idf)
}

#' Calculate term frequency
#' @param df Data frame
#' @param group Column to be considered as a group id
#' @param term Column to be considered as term
#' @param weight Type of weight calculation.
#' "ratio" is default and it's count/(total number of terms in the group).
#' This can be "raw", "binary" and "log_scale"
#' "raw" is the count of the term in the group.
#' "binary" is logic if the term is in the group or not.
#' "log_scale" is logic if the term is in the group or not.
#' @param term Column to be considered as term
#' @return Data frame with group, term and .tf column
calc_tf <- function(df, group, term, ...){
  group_col <- col_name(substitute(group))
  term_col <- col_name(substitute(term))
  calc_tf_(df, group_col, term_col, ...)
}

#' @rdname calc_tf
calc_tf_ <- function(df, group_col, term_col, weight="ratio"){
  loadNamespace("dplyr")
  loadNamespace("tidyr")

  cnames <- avoid_conflict(c(group_col, term_col), c("count_per_doc", "tf"))

  calc_weight <- function(raw){
    if(weight=="raw"){
      val <- raw
    } else if (weight=="binary"){
      val <- as.logical(raw)
    } else if (weight=="log_scale"){
      val <- 1+log(raw)
    }
    else{
      stop(paste0(weight, " is not recognized as weight argument"))
    }
    val
  }

  ret <- (df[,colnames(df) == group_col | colnames(df)==term_col] %>%
            dplyr::group_by(!!!rlang::syms(c(group_col, term_col))) %>% # convert the column name to symbol for colum names with backticks
            dplyr::summarise(!!rlang::sym(cnames[[1]]) := n()) %>%
            dplyr::mutate(!!rlang::sym(cnames[[2]]) := calc_weight(!!rlang::sym(cnames[[1]]))) %>%
            dplyr::ungroup()
  )
}

#' Calculate tfidf, which shows how much particular the token is in a group.
#' @param df Data frame which has columns of groups and their terms
#' @param group Column of group names
#' @param term Column of terms
#' @param [DEPRECATED] idf_log_scale
#' Function to scale IDF. 'log' function is always applied to IDF. Setting other functions is ignored
#' @export
do_tfidf <- function(df, group, term, idf_log_scale = log, tf_weight="raw", norm="l2"){
  validate_empty_data(df)

  loadNamespace("tidytext")
  loadNamespace("dplyr")

  if(!(norm %in% c("l1", "l2") | norm == FALSE)){
    stop("norm argument must be l1, l2 or FALSE")
  }

  if(!missing(idf_log_scale)){
    warnings("Argument idf_log_scale is deprecated. Log is always applied to IDF.")
  }

  group_col <- col_name(substitute(group))
  term_col <- col_name(substitute(term))

  # remove NA from group and term column to avoid error
  df <- tidyr::drop_na(df, !!rlang::sym(group_col), !!rlang::sym(term_col))

  cnames <- avoid_conflict(c(group_col, term_col), c("count_of_docs", "tfidf", "tf"))

  count_tbl <- calc_tf_(df, group_col, term_col, weight=tf_weight)
  tfidf <- calc_idf(count_tbl[[group_col]], count_tbl[[term_col]], smooth_idf = FALSE)
  count_tbl[[cnames[[1]]]] <- tfidf$.df
  count_tbl[[cnames[[2]]]] <- tfidf$.idf * count_tbl[[cnames[[3]]]]
  count_tbl[[cnames[[3]]]] <- NULL

  if(norm == "l2"){
    val <- lazyeval::interp(~x/sqrt(sum(x^2)), x=as.symbol(cnames[[2]]))
    count_tbl <- (count_tbl %>%
      dplyr::group_by(!!!rlang::syms(group_col)) %>% # convert the column name to symbol for colum names with backticks
      dplyr::mutate_(.dots=setNames(list(val), cnames[[2]])) %>%
      dplyr::ungroup())
  } else if(norm == "l1"){
    val <- lazyeval::interp(~x/sum(x), x=as.symbol(cnames[[2]]))
    count_tbl <- (count_tbl %>%
      dplyr::group_by(!!!rlang::syms(group_col)) %>% # convert the column name to symbol for colum names with backticks
      dplyr::mutate_(.dots=setNames(list(val), cnames[[2]])) %>%
      dplyr::ungroup())
  }
  # if NULL, no normalization

  count_tbl
}

#' Stem word so that words which are the same kind of word become the same charactor
#' @param language This can be "porter" or a kind of languages which use alphabet like "english" or "french".
#' @export
stem_word <- function(...){
  loadNamespace("quanteda")
  quanteda::char_wordstem(...)
}

#' Generate ngram in groups.
#' @param df Data frame which has tokens.
#' @param token Column name of token data.
#' @param n How many tokens should be together as new tokens. This should be numeric vector.
#' @export
do_ngram <- function(df, token, sentence, document, maxn=2, sep="_"){
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("stringr")
  loadNamespace("lazyeval")
  token_col <- col_name(substitute(token))
  sentence_col <- col_name(substitute(sentence))
  document_col <- col_name(substitute(document))

  # this is executed for ngrams not to be connected over sentences
  grouped <- df %>%
    dplyr::group_by(!!!rlang::syms(c(document_col, sentence_col))) # convert the column name to symbol for colum names with backticks
  prev_cname <- token_col
  # create ngram columns in this iteration
  for(n in seq(maxn)[-1]){
    # column name is gram number
    cname <- n

    # Use following non-standard evaluation formulas to use token_col, prev_cname and cname variables

    # lead the token to n-1 position
    # if n is 3 and a token is in 5th token in a group, it goes to 3rd row in the group
    lead_fml <- lazyeval::interp(~dplyr::lead(x, y), x=as.symbol(token_col), y=n-1)
    # connect the lead token to the ngram created previously
    # if n is 3 and the token is 5th token in a group, the token is connected with 3rd and 4th token in the group
    str_c_fml <- lazyeval::interp(~stringr::str_c(x, y, sep=z), x=as.symbol(prev_cname), y=as.symbol(cname), z=sep)

    # execute the formulas
    grouped <- (grouped %>%
              dplyr::mutate_(.dots=setNames(list(lead_fml), cname)) %>%
              dplyr::mutate_(.dots=setNames(list(str_c_fml), cname))
              )

    # preserve the cname to be used in next iteration
    prev_cname <- cname
  }
  ret <- dplyr::ungroup(grouped)

  # this change original token column name to be 1 (mono-gram)
  colnames(ret)[colnames(ret) == token_col] <- 1
  kv_cnames <- avoid_conflict(c(document_col, sentence_col), c("gram", "token"))
  # gather columns that have token (1 and newly created columns)
  ret <- tidyr::gather_(ret, kv_cnames[[1]], kv_cnames[[2]], c("1", colnames(ret)[(ncol(ret) - maxn + 2):ncol(ret)]), na.rm = TRUE, convert = TRUE)
  # sort the result
  ret <- dplyr::arrange(ret, !!!rlang::syms(c(document_col, sentence_col, kv_cnames[[1]])))
  ret
}

#' Calculate sentiment
#' @export
get_sentiment <- function(text){
  loadNamespace("sentimentr")
  if(is.character(text)) {
    sentimentr::sentiment_by(text)$ave_sentiment
  } else { # sentiment_by fails if the text is not character so convert it to character first
    sentimentr::sentiment_by(as.character(text))$ave_sentiment
  }
}

#' Wrapper function for readr::parse_character
#' @param text to parse
#' @export
parse_character <- function(text, ...){
  # After updating readr version from 1.1.1 to to 1.3.1,
  # readr::parse_character now fails for non-characters.
  # So if the input is not character (e.g. numeric, Date, etc),
  # use base as.character to avoid the error raised from readr::parse_character.
  if(!is.character(text)) {
    as.character(text)
  } else {
    readr::parse_character(text, ...)
  }
}

#' Wrapper function for readr::parse_number
#' @param text to parse
#' @export
parse_number <- function(text, ...){
  # readr::parse_number used to allow already numeric input, by doing nothing,
  # but after updating readr version from 1.1.1 to to 1.3.1, it only allows character input.
  # if numeric, return as is for backward compatibility.
  if(is.numeric(text)) {
    text
  } else if (!is.character(text)) {
    # non character data raises Error in parse_vector(x, col_number(), na = na, locale = locale, trim_ws = trim_ws) : is.character(x) is not TRUE
    # so explicitly convert it to character before calling readr::parse_number
    as.numeric(readr::parse_number(as.character(text), ...))
  } else {
    # For some reason, output from parse_number returns FALSE for
    # is.vector(), which becomes a problem when it is fed to ranger
    # as the target variable. To work it around, we apply as.numeric().
    as.numeric(readr::parse_number(text, ...))
  }
}

#' Wrapper function for readr::parse_logical
#' @param text to parse
#' @export
parse_logical <- function(text, ...){
  # After updating readr version from 1.1.1 to to 1.3.1, it only allows character input.
  # So if logical, return as is for backward compatibility.
  if(is.logical(text)) {
    text
  } else if (!is.character(text)){
    # non character data raises Error in parse_vector(x, col_number(), na = na, locale = locale, trim_ws = trim_ws) : is.character(x) is not TRUE
    # so explicitly convert it to character before calling readr::parse_number
    readr::parse_logical(as.character(text), ...)
  } else {
    readr::parse_logical(text, ...)
  }
}

#'Function to extract text inside the characters like bracket.
#'@export
str_extract_inside <- function(column, begin = "(", end = ")") {
  # Ref https://stackoverflow.com/questions/3926451/how-to-match-but-not-capture-part-of-a-regex
  # Below logic creates a Regular Expression that uses lookbehind and lookahead to extract string
  # between them.
  # Also, regarding the "*?" (? after asterisk) in the regular expression, it's necessary to handle the case
  # like below.
  #> stringr::str_extract("aaa[xxx][ggg]","(?<=\\[).*(?=\\])")
  #[1] "xxx][ggg"
  #> stringr::str_extract("aaa[xxx][ggg]","(?<=\\[).*?(?=\\])")
  #[1] "xxx"
  # As you can see in the above example, with *? it can extract xxx but without it, it ends up with "xxx][ggg"
  if(stringr::str_length(begin) > 1) {
    stop("The begin argument must be one character.")
  }
  if(stringr::str_length(end) > 1) {
    stop("The end argument must be one character.")
  }
  if(grepl("[A-Za-z]", begin)) {
    stop("The begin argument must be symbol such as (, {, [.")
  }
  if(grepl("[A-Za-z]", end)) {
    stop("The end argument must be symbol such as ), }, ].")
  }
  exp = stringr::str_c("(?<=\\", begin, ").*?(?=\\", end, ")")
  stringr::str_extract(column, exp)
}

#'Function to extract logical value from the specified column.
#'If true_value is provided, use it to decide TRUE or FALSE.
#'If true_value is not provided, "true", "yes", "1", and 1 are treated as TRUE.
#'@export
str_logical <- function(column, true_value = NULL) {
   # if true_value is explicitly provided, honor it
   if(!is.null(true_value)) {
     stringr::str_to_lower(stringr::str_trim(column)) == stringr::str_to_lower(true_value)
   } else if (is.numeric(column)) { # for numeric columns (integer, double, numeric, etc), use as.logical
     as.logical(column)
   } else { # default handling.
      # if value is "true" or "yes" or "1", return TRUE
      target <- stringr::str_to_lower(stringr::str_trim(column))
      ifelse (target %in% c("true", "yes", "1"),
              TRUE,
              # if value is "false" or "no" or "0", return FALSE.
              # All the other cases are NA
              ifelse(target %in%  c("false", "no", "0"), FALSE, NA))
   }
}
