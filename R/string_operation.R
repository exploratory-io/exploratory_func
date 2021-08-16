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
#' @param hiragana_word_length_to_assume_stopword Assume it as a stopword if the token is a Hirgana word whose the legnth is fewer than this or equal to this.
#' @return Logical vector if the token is in stop words or not.
#' @export
is_stopword <- function(token, lang = "english", include = c(), exclude = c(), hiragana_word_length_to_assume_stopword = 0, ...){
  if(hiragana_word_length_to_assume_stopword > 0) { # for Japanese, assume the token is stopword if it's one letter
    result <- token %in% get_stopwords(lang, include = include, exclude = exclude, ...) | stringr::str_detect(token, stringr::str_c("^[\\\u3040-\\\u309f]{1,", hiragana_word_length_to_assume_stopword, "}$"))
  } else {
    result <- token %in% get_stopwords(lang, include = include, exclude = exclude, ...)
  }
  result
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
    "english_smart")){
    # these data are created from data-raw/create_internal_data.R
    get(paste0("stopwords_", lang))
  } else if(lang == "japanese") {
    stopwords_japanese_minimum
  } else if(lang %in% c( # tm only supports these language for stopwords
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
    "swedish")) {
    loadNamespace("tm")
    tm::stopwords(kind = lang)
  } else { # set empty vector as a fallback.
    c()
  }
  # if is_twitter argument is true, append exploratory_stopwords which contains stopwords for twitter
  if(is_twitter) {
    stopwords <- append(stopwords, exploratory_stopwords)
  }
  # if lang is not in below special cases, append stopwords from tidystopwords package because tidystopwords provides
  # wide range of stopwords such as http and https which are not inclued in tm package.
  if (lang %nin% c(
    "japanese", # Avoid this for Japanese too. We are going for minimum set of stopwords for Japanese for now.
    "english_snowball",
    "english_onix",
    "english_smart")){
    # tidystopwords required language name with Title Case so make sure it's title case.
    stopwords <- append(stopwords, tidystopwords::generate_stoplist(stringr::str_to_title(lang)))
  }
  ret <- c(stopwords[!stopwords %in% exclude], include)
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
#' @param remove_punct Whether it should remove punctuations.
#' @param remove_numbers Whether it should remove numbers.
#' @param remove_hyphens Whether it should remove hyphens.
#' @param remove_separators Whether it should remove separators.
#' @param remove_symbols Whether it should remove symbols.
#' @param remove_twitter Whether it should remove remove Twitter characters @ and #.
#' @param remove_url Whether it should remove URL starts with http(s).
#' @param stopwords_lang Language for the stopwords that need to be excluded from the result.
#' @param hiragana_word_length_to_remove Length of a Hiragana word that needs to be excluded from the result.
#' @param summary_level Either "row" or "all". If this is "all", it ignores document and summarizes the result by token.
#' @param sort_by Supported options are "count", "name", and "doc"
#' @param ngrams - by default it's 1. Pass 2 for bigram, 3 for trigram.
#' @return Data frame with tokenized column.
#' @export
do_tokenize_icu <- function(df, text_col, token = "word", keep_cols = FALSE,
                                 drop = TRUE, with_id = TRUE, output = token,
                                 remove_punct = TRUE, remove_numbers = TRUE,
                                 remove_hyphens = FALSE, remove_separators = TRUE,
                                 remove_symbols = TRUE, remove_twitter = TRUE,
                                 remove_url = TRUE, stopwords_lang = NULL,
                                 hiragana_word_length_to_remove = 2,
                                 summary_level = "row", sort_by = "", ngrams = 1L, ...){

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
    quanteda::tokens(what = token, remove_punct = remove_punct, remove_numbers = remove_numbers,
                     remove_symbols = remove_symbols, remove_twitter = remove_twitter,
                     remove_hyphens = remove_hyphens, remove_separators = remove_separators,
                     remove_url = remove_url) %>%
    quanteda::tokens_wordstem()


  # when stopwords Language is set, use the stopwords to filter out the result.
  if(!is.null(stopwords_lang)) {
    stopwords_to_remove <- exploratory::get_stopwords(lang = stopwords_lang)
    tokens <- tokens %>% quanteda::tokens_remove(stopwords_to_remove, valuetype = "fixed")
  }
  # Remove Japanese Hiragana word whose length is less than hiragana_word_length_to_remove
  if(hiragana_word_length_to_remove > 0) {
    tokens <- tokens %>% quanteda::tokens_remove(stringr::str_c("^[\\\u3040-\\\u309f]{1,", hiragana_word_length_to_remove, "}$"), valuetype = "regex")
  }
  if(ngrams > 1) { # if ngrams is greater than 1, generate ngrams.
    tokens <- quanteda::tokens_ngrams(tokens, n = 1:ngrams)
  }
  # convert tokens to dfm object
  dfm <- tokens %>% quanteda::dfm()

  # Now convert result dfm to a data frame
  resultTemp <- quanteda::convert(dfm, to = "data.frame")
  # The first column name returned by quanteda::convert is always "doc_id" so rename it to avoid it conflicts with other tokens
  docCol <- resultTemp[c(1)]
  # Exclude the "doc_id" column
  result <- resultTemp[c(-1)]
  # Then bring the column back as internal doc_id column
  result$document.exp.token.col <- docCol$doc_id

  # Convert the data frame from "wide" to "long" format by tidyr::gather.
  result <- result %>% tidyr::gather(key = !!token_col, value = !!count_col, which(sapply(., is.numeric)), na.rm = TRUE, convert = TRUE) %>%
    # Exclude unused tokens for the document.
    dplyr::filter(!!as.name(count_col) > 0) %>%
    # The internal doc_id column value looks like "text100". Remove text part to make it numeric.
    dplyr::mutate(document_id = as.numeric(stringr::str_remove(document.exp.token.col, "text"))) %>%
    # Drop internal doc_id column
    dplyr::select(-document.exp.token.col) %>%
    # Sort result by document_id to align with original data frame's order.
    dplyr::arrange(document_id)

  if(keep_cols) {
    # If we need to keep original columns, then bring them back by joining the result data frame with original data frame.
    result <- df %>%  dplyr::left_join(result, by=c("document_id" = "document_id"))
    if(drop) { # drop the text column
      result <- result %>% dplyr::select(-orig_input_col)
    }
  } else if(!drop) { # Bring back the text column by by joining the result data frame with original data frame and drop unwanted columns.
    result <- df %>% dplyr::left_join(result, by=c("document_id"= "document_id")) %>%
      select(document_id, !!count_col, !!token_col, orig_input_col);
  }
  if(!with_id) { # Drop the document_id column
    result <- result %>% dplyr::select(-document_id)
  }
  # result column order should be document_id, <token_col>, <count_col> ...
  if(!with_id) {
    result <- result %>% select(!!rlang::sym(token_col), !!rlang::sym(count_col), dplyr::everything())
  } else {
    result <- result %>% select(document_id, !!rlang::sym(token_col), !!rlang::sym(count_col), dplyr::everything())
  }
  # if the summary_level is "all", summarize it by token.
  if(summary_level == "all") {
    result <- result %>% dplyr::group_by(!!rlang::sym(token_col)) %>% dplyr::summarise(!!rlang::sym(count_col) := sum(!!rlang::sym(count_col), na.rm = TRUE))
  }
  # Sort handling. if count is specified, sort by count descending.
  if(sort_by == "count") {
    result <- result %>% arrange(desc(!!rlang::sym(count_col)))
  } else if (sort_by == "token"){ #if token is specified, sort by token alphabetically.
    result <- result %>% arrange(!!rlang::sym(token_col))
  } else if (sort_by == "doc" && with_id) { # if document_id exists and sort by option is doc, sort by document_id.
    result <- result %>% arrange(document_id)
  }
  result
}

#' Tokenize text and unnest
#' @param df Data frame
#' @param text Set a column of which you want to split the text or tokenize.
#' @param token Select the unit of token from "words", or "sentences".
#' @param keep_cols Whether existing columns should be kept or not
#' @param drop Whether input column should be removed.
#' @param with_sentence_id Whether output should contain sentence id in each document.
#' @param output Set a column name for the new column to store the tokenized values.
#' @param stopwords_lang Language for the stopwords that need to be excluded from the result.
#' @param remove_punct Whether it should remove punctuations.
#' @param remove_numbers Whether it should remove numbers.
#' @param remove_url Whether it should remove URLs.
#' @param remove_twitter Whether it should remove Twitter social tags.
#' @param stopwords Additional stopwords.
#' @param stopwords_to_remove Words to be removed from the set of stopwords.
#' @param hiragana_word_length_to_remove Length of a Hiragana word that needs to be excluded from the result.
#' @param compound_tokens Sequence of words that should be treated as one word.
#' @return Data frame with tokenized column
#' @export
do_tokenize <- function(df, text, token = "words", keep_cols = FALSE,
                        drop = TRUE, with_sentence_id = TRUE,
                        output = "token", output_case = "lower",
                        remove_punct = TRUE, remove_numbers = TRUE,
                        tokenize_tweets = FALSE,
                        remove_url = TRUE, remove_twitter = TRUE,
                        stopwords_lang = NULL, stopwords = c(), stopwords_to_remove = c(),
                        hiragana_word_length_to_remove = 2,
                        compound_tokens = NULL, ...) {
  text_col <- tidyselect::vars_pull(names(df), !! rlang::enquo(text))
  if (!keep_cols) {
    # Keep only the column to tokenize
    df <- df[text_col]
  }
  # Replace NAs with empty string. quanteda::tokens() cannot handle NA, but can handle empty string.
  df[[text_col]] <- ifelse(is.na(df[[text_col]]), "", df[[text_col]])
  # Preserve df and text_col at this point before modifying them while tokenizing into sentences,
  # for the purpose of adding them back to the output.
  df_orig <- df
  text_col_orig <- text_col

  # As pre-processing, split text into sentences, if sentence_id is needed or the final output should be sentences.
  if (with_sentence_id || token == "sentences") {
    text_v <- df[[text_col]]
    sentences_list <- tokenizers::tokenize_sentences(text_v)
    # Create a data.frame, where one row represents one sentence. document_id is the document the sentence belongs (row number in the original df.)
    # .sentence is the text of the sentence.
    # To avoid expensive unnest_longer call, we use base R functions like unlist() instead here.
    # as.integer() is there to convert the matrix unlist() returns there into an integer vector.
    # unname() is there to unname the named vector unlist returns.
    res <- tibble::tibble(document_id = as.integer(unlist(mapply(function(tokens,index) {rep(index,length(tokens))},
                                                                 sentences_list, seq_along(sentences_list)))),
                          .sentence = unname(unlist(sentences_list)))
    df <- res
    text_col <- ".sentence"
    # Replace NAs with empty string again. As a result of this tokenization, NAs can be introduced.
    df[[text_col]] <- ifelse(is.na(df[[text_col]]), "", df[[text_col]])
  }

  if (token == "words") {
    text_v <- df[[text_col]]
    tokens <- tokenize_with_postprocess(text_v,
                                        remove_punct = remove_punct, remove_numbers = remove_numbers,
                                        tokenize_tweets = tokenize_tweets,
                                        remove_url = remove_url, remove_twitter = remove_twitter,
                                        stopwords_lang = stopwords_lang, stopwords = stopwords, stopwords_to_remove = stopwords_to_remove,
                                        hiragana_word_length_to_remove = hiragana_word_length_to_remove,
                                        compound_tokens = compound_tokens)
    tokens_list <- as.list(tokens)
    if (with_sentence_id) { # To avoid expensive unnest_longer call in the default behavior, use base R functions like unlist here.
      res <- tibble::tibble(document_id = as.integer(unlist(mapply(function(tokens,index){rep(index,length(tokens))}, tokens_list, df$document_id))),
                            sentence_id = as.integer(unlist(mapply(function(tokens,index){rep(index,length(tokens))}, tokens_list, seq_along(tokens_list)))))
      res <- res %>% dplyr::mutate(!!rlang::sym(output) := unname(unlist(tokens_list)))
    }
    else { # with_sentence_id is FALSE
      res <- tibble::tibble(document_id = as.integer(unlist(mapply(function(tokens,index){rep(index,length(tokens))}, tokens_list, seq_along(tokens_list)))))
      res <- res %>% dplyr::mutate(!!rlang::sym(output) := unname(unlist(tokens_list)))
    }
    if (output_case == "title") {
      res <- res %>% dplyr::mutate(!!rlang::sym(output) := stringr::str_to_title(!!rlang::sym(output)))
    }
    else if (output_case == "upper") {
      res <- res %>% dplyr::mutate(!!rlang::sym(output) := stringr::str_to_upper(!!rlang::sym(output)))
    }
  }
  else { # This means token == "sentences"
    # Just rename the output column from the pre-processing.
    res <- res %>% dplyr::rename(!!rlang::sym(output) := !!rlang::sym(text_col))
  }

  # Filter out rows with NA token. This happens if a sentence's tokens were all removed.
  res <- res %>% dplyr::filter(!is.na(!!rlang::sym(output)))

  # Put back other columns if necessary.
  if (!drop || keep_cols) {
    if (drop) {
      df_orig <- df_orig %>% select(-!!rlang::sym(text_col_orig))
    }
    df_orig <- df_orig %>% dplyr::mutate(document_id=seq(n())) # If document_id is in the input, it is overwritten.
    res <- res %>% dplyr::left_join(df_orig, by="document_id")
  }
  res
}

#' Input:
#' group - vector of documents
#' term - vector terms with the same length as group.
#' tf - vector of tf with the same length as group. It can be after weight function is applied,
#'      in which case, we assume that group-term combination in the input is unique, and summing up at the sparseMatrix would not happen.
#' count_per_doc - vector of word count per doc. It is tf before weight function is applied.
#' Output: data.frame of document frequency and tf-idf. The number of rows are the same as the input vectors.
calc_tfidf <- function(group, term, tf, count_per_doc, smooth_idf = FALSE){
  loadNamespace("Matrix")
  loadNamespace("text2vec")
  if(length(group)!=length(term)){
    stop("length of document and terms have to be the same")
  }
  doc_fact <- as.factor(group)
  term_fact <- as.factor(term)
  sparse_mat <- Matrix::sparseMatrix(i = as.numeric(doc_fact), j = as.numeric(term_fact), x = tf) # With weight function applied. To calculate tfidf.
  sparse_mat_for_count <- Matrix::sparseMatrix(i = as.numeric(doc_fact), j = as.numeric(term_fact), x = count_per_doc) # Without weight function applied. To calculate df.

  m_tfidf <- text2vec::TfIdf$new(smooth_idf = smooth_idf, norm = "none")
  result_tfidf <- m_tfidf$fit_transform(sparse_mat)

  tfidf <- purrr::flatten_dbl(purrr::map2(as.integer(doc_fact), as.integer(term_fact), function(x,y){result_tfidf[x,y]}))
  df <- Matrix::colSums(sparse_mat_for_count)[term_fact]

  data.frame(.df=df, .tfidf=tfidf)
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

#' @param group_col - Document ID column
#' @param term_col - Term column
#' Output: data.frame with the following column.
#' - document_id
#' - token - Term
#' - count_per_doc - Count of term occurrence per document
#' - tf - count_per_doc with weight function applied.
#' @rdname calc_tf
calc_tf_ <- function(df, group_col, term_col, weight="ratio", count_col = NULL){
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

  if (!is.null(count_col)) {
    ret <- df[,colnames(df) == group_col | colnames(df)==term_col | colnames(df)==count_col] %>%
      dplyr::group_by(!!!rlang::syms(c(group_col, term_col))) %>% # convert the column name to symbol for column names with backticks
      dplyr::summarise(!!rlang::sym(cnames[[1]]) := sum(!!rlang::sym(count_col), na.rm = TRUE))
  } else {
    ret <- df[,colnames(df) == group_col | colnames(df)==term_col] %>%
      dplyr::group_by(!!!rlang::syms(c(group_col, term_col))) %>% # convert the column name to symbol for column names with backticks
      dplyr::summarise(!!rlang::sym(cnames[[1]]) := n())
  }
  ret %>% dplyr::mutate(!!rlang::sym(cnames[[2]]) := calc_weight(!!rlang::sym(cnames[[1]]))) %>%
  dplyr::ungroup()

}

#' Calculate tfidf, which shows how much particular the token is in a group.
#' @param df Data frame which has columns of groups and their terms
#' @param group Column of group names
#' @param term Column of terms
#' @param [DEPRECATED] idf_log_scale
#' Function to scale IDF. 'log' function is always applied to IDF. Setting other functions is ignored
#' @export
do_tfidf <- function(df, group, term, idf_log_scale = log, tf_weight="raw", norm="l2", count = NULL){
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
  count_col <- NULL
  if (!is.null(count)) {
    count_col <- col_name(substitute(count))
  }

  # remove NA from group and term column to avoid error
  df <- tidyr::drop_na(df, !!rlang::sym(group_col), !!rlang::sym(term_col))

  cnames <- avoid_conflict(c(group_col, term_col), c("count_of_docs", "tfidf", "tf", "count_per_doc"))

  count_tbl <- calc_tf_(df, group_col, term_col, weight=tf_weight, count_col = count_col)
  tfidf <- calc_tfidf(count_tbl[[group_col]], count_tbl[[term_col]], count_tbl[[cnames[[3]]]], count_tbl[[cnames[[4]]]], smooth_idf = FALSE)
  count_tbl[[cnames[[1]]]] <- tfidf$.df
  count_tbl[[cnames[[2]]]] <- tfidf$.tfidf
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
  grouped <- NULL
  if (sentence_col != document_col) {
    grouped <- df %>%
      dplyr::group_by(!!!rlang::syms(c(document_col, sentence_col))) # convert the column name to symbol for column names with backticks
  } else {
    # If document_col and sentence_col are identical, just group the data frame by the document_col.
    # NOTE: If you group_by the data frame with identical columns, group_by throws an error.
    grouped <- df %>%
      dplyr::group_by(!!rlang::sym(document_col)) # convert the column name to symbol for column names with backticks
  }
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
    # Passing non-ascii character to readr::parse_number causes an error so remove non-ascii character before calling readr::parse_number.
    # ref: https://github.com/tidyverse/readr/issues/1111
    as.numeric(readr::parse_number(gsub('[^\x20-\x7E]', '', as.character(text)), ...))
  } else {
    # For some reason, output from parse_number returns FALSE for
    # is.vector(), which becomes a problem when it is fed to ranger
    # as the target variable. To work it around, we apply as.numeric().
    # Passing non-ascii character to readr::parse_number causes an error so remove non-ascii character before calling readr::parse_number.
    # ref: https://github.com/tidyverse/readr/issues/1111
    as.numeric(readr::parse_number(gsub('[^\x20-\x7E]', '', text), ...))
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
str_extract_inside <- function(column, begin = "(", end = ")", all = FALSE, include_special_chars = TRUE) {
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
  exp <- stringr::str_c("\\", begin, "[^\\" , begin, "\\", end, "]*\\", end)
  if(!include_special_chars) {
    exp <- stringr::str_c("(?<=\\", begin,  ").*?(?=\\", end,  ")");
  }
  if(all) {
    stringr::str_extract_all(column, exp)
  } else {
    stringr::str_extract(column, exp)
  }
}

#'Function to remove text inside the characters like bracket.
#'@export
str_remove_inside <- function(column, begin = "(", end = ")", all = FALSE, include_special_chars = TRUE){
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
  exp <- stringr::str_c("\\", begin, "[^\\" , begin, "\\", end, "]*\\", end)
  if(!include_special_chars) {
    exp <- stringr::str_c("(?<=\\", begin,  ").*?(?=\\", end,  ")");
  }
  if(all) {
    stringr::str_remove_all(column, exp)
  } else {
    stringr::str_remove(column, exp)
  }
}

#'Function to replace text inside the characters like bracket.
#'@export
str_replace_inside <- function(column, begin = "(", end = ")", rep = "", all = FALSE, include_special_chars = TRUE){
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
  exp <- stringr::str_c("\\", begin, "[^\\" , begin, "\\", end, "]*\\", end)
  if(!include_special_chars) {
    exp <- stringr::str_c("(?<=\\", begin,  ").*?(?=\\", end,  ")");
  }
  if(all) {
    stringr::str_replace_all(column, exp, rep)
  } else {
    stringr::str_replace(column, exp, rep)
  }
}

#' Wrapper function for stringr::str_remove.
#' When remove_extra_space argument is TRUE, it applies str_squish on top of the stringr::str_remove result.
str_remove <- function(string, pattern, remove_extra_space = FALSE) {
  res <- stringr::str_remove(string, pattern)
  if (remove_extra_space) {
    res <- stringr::str_squish(res)
  }
  res
}

#' Wrapper function for stringr::str_remove_all.
#' When remove_extra_space argument is TRUE, it applies str_squish on top of the stringr::str_remove_all result.
str_remove_all <- function(string, pattern, remove_extra_space = FALSE) {
  res <- stringr::str_remove_all(string, pattern)
  if (remove_extra_space) {
    res <- stringr::str_squish(res)
  }
  res
}

#'Function to remove URL from the text
#'@export
str_remove_url <- function(text, position = "any"){
  reg <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  if(position == "start") {
    reg <- stringr::str_c("^", reg, sep = "")
  } else if (position == "end") {
    reg <- stringr::str_c(reg, "$", sep = "")
  }
  stringr::str_replace_all(text, stringr::regex(reg, ignore_case = TRUE), "")
}
#'Function to replace URL from the text
#'@export
str_replace_url <- function(text, rep = ""){
  stringr::str_replace_all(text, stringr::regex("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", ignore_case = TRUE), rep)
}
#'Function to extract URL from the text
#'@export
str_extract_url <- function(text, position = "any"){
  reg <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  if(position == "start") {
    reg <- stringr::str_c("^", reg, sep = "")
  } else if (position == "end") {
    reg <- stringr::str_c(reg, "$", sep = "")
  }
  stringr::str_extract_all(text, stringr::regex(reg, ignore_case = TRUE))
}

#'Function to remove word from text.
#'@export
str_remove_word <- function(string, start = 1L, end = start, sep = fixed(" ")) {
  str_replace_word(string, start, end, sep, rep = "")
}

#'Function to remove word from text.
#'@export
str_replace_word <- function(string, start = 1L, end = start, sep = fixed(" "), rep = "") {
  sep_ <- sep
  # Below is the list of predefined separators passed from Exploratory Desktop in a regular expression format.
  # Changed it back to the original separator.
  if(sep == "\\s*\\,\\s*") {
    sep_ <- ", "
  } else if (sep == "\\s+") {
    sep_ <- " "
  } else if (sep == "\\s*\\;\\s*") {
    sep_ <- ";"
  } else if (sep == "\\s*\\:\\s*") {
    sep_ <- ":"
  } else if (sep == "\\s*\\/\\s*") {
    sep_ <- "/"
  } else if (sep ==  "\\s*\\-\\s*") {
    sep_ <- "-"
  } else if (sep == "\\s*\\_\\s*") {
    sep_ <- "_"
  } else if (sep == "\\s*\\.\\s*") {
    sep_ <- "."
  } else if (sep == "\\s*\\@\\s*") {
    sep_ <- "@"
  }
  if(end == 1) {
    ret <- stringr::word(string, start = start, end = end, sep = sep)
    if(rep != "") {
      rep <- stringr::str_c(rep, sep_, sep="")
    }
    # Placing characters between \\Q and \\E makes the regular expression engine treat them literally rather than as regular expressions.
    stringr::str_replace(string, stringr::str_c("^", "\\Q", ret, "\\E", sep, ""), rep)
  } else if (end == -1){
    ret <- stringr::word(string, start = start, end = end, sep = sep)
    if(rep != "") {
      rep <- stringr::str_c(sep_, rep, sep="")
    }
    # Placing characters between \\Q and \\E makes the regular expression engine treat them literally rather than as regular expressions.
    stringr::str_replace(string, stringr::str_c(sep, "\\Q", ret, "\\E$"), rep)
  } else {
    ## TODO: Implement other cases
    string
  }
}

get_emoji_regex <- function() {
  regexp = "\\p{EMOJI}|\\p{EMOJI_PRESENTATION}|\\p{EMOJI_MODIFIER_BASE}|\\p{EMOJI_MODIFIER}\\p{EMOJI_COMPONENT}";
  # it seems above condition does not cover all emojis.
  # So we will manually add below emojis. (ref: https://github.com/gagolews/stringi/issues/279)
  regexp = stringr::str_c(regexp, "|\U0001f970|\U0001f975|\U0001f976|\U0001f973|\U0001f974|\U0001f97a|\U0001f9b8|\U0001f9b9|\U0001f9b5|\U0001f9b6|\U0001f9b0|\U0001f9b1|\U0001f9b2", sep = "")
  regexp = stringr::str_c(regexp, "|\U0001f9b3|\U0001f9b4|\U0001f9b7|\U0001f97d|\U0001f97c|\U0001f97e|\U0001f97f|\U0001f99d|\U0001f999|\U0001f99b|\U0001f998|\U0001f9a1|\U0001f9a2", sep = "")
  regexp = stringr::str_c(regexp, "|\U0001f99a|\U0001f99c|\U0001f99e|\U0001f99f|\U0001f9a0|\U0001f96d|\U0001f96c|\U0001f96f|\U0001f9c2|\U0001f96e|\U0001f9c1|\U0001f9ed|\U0001f9f1", sep = "")
  regexp = stringr::str_c(regexp, "|\U0001f6f9|\U0001f9f3|\U0001f9e8|\U0001f9e7|\U0001f94e|\U0001f94f|\U0001f94d|\U0001f9ff|\U0001f9e9|\U0001f9f8|\U0001f9f5|\U0001f9f6|\U0001f9ee", sep = "")
  regexp = stringr::str_c(regexp, "|\U0001f9fe|\U0001f9f0|\U0001f9f2|\U0001f9ea|\U0001f9eb|\U0001f9ec|\U0001f9f4|\U0001f9f7|\U0001f9f9|\U0001f9fa|\U0001f9fb|\U0001f9fc|\U0001f9fd", sep = "")
  regexp = stringr::str_c(regexp, "|\U0001f9ef|\u267e",  sep = "")
  # Handle zero-width joiner (\u200d) prefixing another emoji. https://en.wikipedia.org/wiki/Zero-width_joiner
  # Also handle variation selector (\ufe0e, \ufe0f) suffixing emoji. https://en.wikipedia.org/wiki/Variation_Selectors_(Unicode_block)
  regexp = stringr::str_c("\u200d?(", regexp, ")(\ufe0e|\ufe0f)?",  sep = "")
  regexp
}

#'Function to remove emoji from a list of characters.
str_remove_emoji <- function(column, position = "any"){
  regexp <- get_emoji_regex()
  if (position == "start") {
    regexp = stringr::str_c("^(", regexp, ")+", sep = "")
  } else if (position == "end") {
    regexp = stringr::str_c("(", regexp, ")+$", sep = "")
  }
  stringi::stri_replace_all(column, regex = regexp, "")
}

#'Function to remove range of text.
#'This uses exploratory::str_replace_range under the hood and pass empty string as replacement to remove it.
#'
#'export
str_remove_range <- function(column, start, end = -1){
  # Pass empty string "", to remove the range of text.
  str_replace_range(column, start = start, end = end, replacement = "")
}

#'Function to replace range of text.
#'
#'It uses stringi::stri_sub_replace under the hood, but it does additional parameter validation.
#'For validation details, see comment below.
#'
#'export
str_replace_range <- function(column, start, end = -1, replacement = ""){
  # To make the behavior consistent with extract case (i.e. str_sub),
  # if both start and end are negative, end should be greater than or equal to start.
  # For example, stringi::stri_sub("Aaron Bergman", -3, -4) returns "" since it doesn't match.
  # However, stringi::stri_sub_replace("Aaron Bergman", -3, -4, replacement = "A") returns "Aaron BergAman" where it should return "Aaron Bergman"
  # So to make the behavior of the stringi::stri_sub_replace as same as the stringi::stri_sub, if the below condition met, just return the value as is.
  if (!is.null(start) && !is.null(end) && start < 0 && end < 0 && start > end) {
    column
  } else {
    stringi::stri_sub_replace(column, from = start, to = end, replacement = replacement)
  }
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

#' Function to detect pattern from a string.
#' It's a wrapper function for stringr::str_detect and the wrapper function has ignore_case handling.
#' @param string Input vector. Either a character vector, or something coercible to one.
#' @param pattern Pattern to look for.
#' @param negate If TRUE, return non-matching elements.
#' @param ignore_case If TRUE, detect the pattern with case insensitive way.
#' @export
str_detect <- function(string, pattern, negate = FALSE, ignore_case = FALSE) {
  if (pattern != "" && ignore_case) { # if pattern is not empty string and case insensitive is specified, use regex to handle the case insensitive match.
    stringr::str_detect(string, stringr::regex(pattern, ignore_case = TRUE), negate = negate)
  } else { # if the pattern is empty string stringr::regexp throws warning, so simply use stringr::str_detect as is.
    stringr::str_detect(string, pattern, negate)
  }
}
