#' Function for Text Analysis Analytics View
#' @export
exp_textanal <- function(df, text, token = "word", keep_cols = FALSE,
                                 drop = TRUE, with_id = TRUE, output = token,
                                 remove_punct = TRUE, remove_numbers = TRUE,
                                 remove_hyphens = FALSE, remove_separators = TRUE,
                                 remove_symbols = TRUE, remove_twitter = TRUE,
                                 remove_url = TRUE, stopwords_lang = NULL,
                                 hiragana_word_length_to_remove = 2,
                                 summary_level = "row", sort_by = "", ngrams = 1L, ...){

  # Always put document_id to know what document the tokens are from
  text_col <- tidyselect::vars_pull(names(df), !! rlang::enquo(text))
  doc_id <- avoid_conflict(colnames(df), "document_id")
  each_func <- function(df) {
    # This is SE version of dplyr::mutate(df, doc_id = row_number())
    df <- dplyr::mutate_(df, .dots=setNames(list(~row_number()),doc_id))
    textData <- df %>% dplyr::select(!!rlang::sym(text_col)) %>% dplyr::rename("text" = !!rlang::sym(text_col))
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
    fcm <- quanteda::fcm(tokens, context = "window", tri = TRUE)

    feats <- names(quanteda::topfeatures(fcm, 30))
    fcm_selected <- fcm_select(fcm, pattern = feats)

    model <- list()
    model$dfm <- dfm
    model$fcm <- fcm
    model$fcm_selected <- fcm_selected
    class(model) <- 'textanal_exploratory'
    model
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

fcm_to_df <- function(fcm) {
  row_idx <- fcm@i
  col_idx_compressed <- fcm@p
  # fcm is in CSR (Compressed Sparse Row) format.
  # Uncompress column index.
  col_idx <- c()
  for (j in 1:(length(col_idx_compressed)-1)) {
    cur_idx <- col_idx_compressed[j]
    next_idx <- col_idx_compressed[j+1]
    rep_num <- next_idx - cur_idx
    col_idx <- c(col_idx, rep(j-1, rep_num))
  }
  col_feats <- fcm@Dimnames$features[col_idx+1]
  row_feats <- fcm@Dimnames$features[row_idx+1]
  value <- fcm@x
  res <- tibble::tibble(token.x=col_feats, token.y=row_feats, value=value)
}

#' extracts results from textanal_exploratory object as a dataframe
#' @export
#' @param type - Type of output.
tidy.textanal_exploratory <- function(x, type="word_count", ...) {
  if (type == "word_count") {
    feats <- quanteda::featfreq(x$dfm)
    res <- tibble(word=names(feats), count=feats)
  }
  else if (type == "word_pairs") {
    res <- fcm_to_df(x$fcm)
  }
  res
}
