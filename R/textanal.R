#' Function for Text Analysis Analytics View
#' @export
exp_textanal <- function(df, text,
                         remove_punct = TRUE, remove_numbers = TRUE,
                         stopwords_lang = NULL, stopwords = c(),
                         hiragana_word_length_to_remove = 2,
                         compound_tokens = NULL,
                         cooccurrence_context = "window", # "document" or "window"
                         cooccurrence_window = 5, # 5 is quanteda's default
                         max_nrow = 50000,
                         ...){

  # Always put document_id to know what document the tokens are from
  text_col <- tidyselect::vars_pull(names(df), !! rlang::enquo(text))
  doc_id <- avoid_conflict(colnames(df), "document_id")
  each_func <- function(df) {
    # sample the data for performance if data size is too large.
    sampled_nrow <- NULL
    if (!is.null(max_nrow) && nrow(df) > max_nrow) {
      # Record that sampling happened.
      sampled_nrow <- max_nrow
      df <- df %>% sample_rows(max_nrow)
    }

    # <Tokenizing code with quoanteda's default tokenizer.>
    #
    # This is SE version of dplyr::mutate(df, doc_id = row_number())
    # df <- dplyr::mutate_(df, .dots=setNames(list(~row_number()),doc_id))
    #
    # textData <- df %>% dplyr::select(!!rlang::sym(text_col)) %>% dplyr::rename("text" = !!rlang::sym(text_col))
    # # Create a corpus from the text column then tokenize.
    # tokens <- quanteda::corpus(textData) %>%
    #   quanteda::tokens(what = token, remove_punct = remove_punct, remove_numbers = remove_numbers,
    #                    remove_symbols = remove_symbols, remove_twitter = remove_twitter,
    #                    remove_hyphens = remove_hyphens, remove_separators = remove_separators,
    #                    remove_url = remove_url) %>%
    #   quanteda::tokens_wordstem()

    tokenized <- tokenizers::tokenize_words(df[[text_col]], lowercase = TRUE, stopwords = NULL, strip_punct = remove_punct, strip_numeric = remove_numbers, simplify = FALSE)
    names(tokenized) <- paste0("text", 1:length(tokenized)) # Add unique names so the list so that it can be passed to quanteda::tokens().
    tokens <- quanteda::tokens(tokenized)

    if (!is.null(compound_tokens)) { # This probably should be kept before removing stopwords not to break compoint tokens that includes stopwords.
      tokens <- tokens %>% quanteda::tokens_compound(pattern = quanteda::phrase(compound_tokens), concatenator = ' ')
    }

    # when stopwords Language is set, use the stopwords to filter out the result.
    if(!is.null(stopwords_lang)) {
      stopwords_to_remove <- exploratory::get_stopwords(lang = stopwords_lang, include = stopwords)
      tokens <- tokens %>% quanteda::tokens_remove(stopwords_to_remove, valuetype = "fixed")
    }
    # Remove Japanese Hiragana word whose length is less than hiragana_word_length_to_remove
    if(hiragana_word_length_to_remove > 0) {
      tokens <- tokens %>% quanteda::tokens_remove(stringr::str_c("^[\\\u3040-\\\u309f]{1,", hiragana_word_length_to_remove, "}$"), valuetype = "regex")
    }
    # convert tokens to dfm object
    dfm_res <- tokens %>% quanteda::dfm()
    fcm_res <- quanteda::fcm(tokens, context = cooccurrence_context, window = cooccurrence_window, tri = TRUE)

    feats <- names(quanteda::topfeatures(fcm_res, 50))
    fcm_selected <- quanteda::fcm_select(fcm_res, pattern = feats)
    dfm_tfidf_res <- quanteda::dfm_tfidf(dfm_res)

    # # Cluster documents with k-means.
    # tfidf_df <- dfm_to_df(dfm_tfidf_res)
    # tfidf_df <- tfidf_df %>% dplyr::rename(tfidf=value)
    # tfidf_reduced <- tfidf_df %>% do_svd(skv = c("document", "token", "tfidf"), n_component = 4) #TODO: Make n_component configurable
    # tfidf_reduced_wide <- tfidf_reduced %>% tidyr::spread(new.dimension, value)
    # clustered_df <- tfidf_reduced_wide %>% build_kmeans(`1`, `2`, `3`, `4`, centers=5) #TODO: Make centers configurable
    # cluster_res <- clustered_df$cluster # Clustering result

    # # Run tf-idf treating each cluster as a document.
    # dfm_clustered <- quanteda::dfm_group(dfm_res, cluster_res)
    # dfm_clustered_tfidf <- quanteda::dfm_tfidf(dfm_clustered)
    # clustered_tfidf <- dfm_to_df(dfm_clustered_tfidf)

    model <- list()
    model$dfm <- dfm_res
    model$fcm <- fcm_res
    model$fcm_selected <- fcm_selected
    model$dfm_tfidf <- dfm_tfidf_res
    # model$cluster <- clustered_df$cluster
    # model$dfm_cluster <- dfm_clustered
    # model$dfm_cluster_tfidf <- dfm_clustered_tfidf
    model$df <- df # Keep original df for showing it with clustering result.
    model$sampled_nrow <- sampled_nrow
    class(model) <- 'textanal_exploratory'
    model
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

dfm_to_df <- function(dfm) {
  row_idx <- dfm@i
  col_idx_compressed <- dfm@p
  # dfm is in CSR (Compressed Sparse Row) format.
  # Uncompress column index.
  col_idx <- c()
  for (j in 1:(length(col_idx_compressed)-1)) {
    cur_idx <- col_idx_compressed[j]
    next_idx <- col_idx_compressed[j+1]
    rep_num <- next_idx - cur_idx
    col_idx <- c(col_idx, rep(j-1, rep_num))
  }
  col_feats <- dfm@Dimnames$features[col_idx+1]
  # row_docs <- dfm@Dimnames$docs[row_idx+1] # This is id generated by quanteda. e.g. "text1".
  row_docs <- row_idx+1 # Use number id instead for now so that it is sorted better by subsequent process.
  value <- dfm@x
  res <- tibble::tibble(document=row_docs, token=col_feats, value=value)
  res
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
  res
}

#' extracts results from textanal_exploratory object as a dataframe
#' @export
#' @param type - Type of output.
tidy.textanal_exploratory <- function(x, type="word_count", ...) {
  if (type == "word_count") {
    feats <- quanteda::featfreq(x$dfm)
    res <- tibble(word=stringr::str_to_title(names(feats)), count=feats)
  }
  else if (type == "word_pairs") {
    res <- fcm_to_df(x$fcm) %>%
      dplyr::filter(token.x != token.y) %>%
      dplyr::mutate(token.x = stringr::str_to_title(token.x), token.y = stringr::str_to_title(token.y)) %>%
      dplyr::rename(word.1 = token.x, word.2 = token.y, count=value)
  }
  else if (type == "doc_cluster") {
    res <- x$df
    res <- res %>% dplyr::mutate(cluster = !!x$cluster)
  }
  else if (type == "doc_cluster_words") {
    res <- dfm_to_df(x$dfm_cluster_tfidf)
    res <- res %>% dplyr::group_by(document) %>%
      dplyr::slice_max(value, n=5) %>%
      dplyr::ungroup() %>%
      dplyr::rename(cluster = document)
    res
  }
  res
}
