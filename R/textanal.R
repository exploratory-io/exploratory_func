
# Mapping table for mapping cld3 result to the input language name for get_stopwords().
# Based on https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
# and list of supported stopword language values from our UI definition (exp_textanal.json).
lang_code_mapping <- c(
  en="english",
  ar="arabic",
  ca="catalan",
  cs="czech",
  zh="chinese",
  da="danish",
  nl="dutch",
  et="estonian",
  fi="finnish",
  fr="french",
  de="german",
  el="greek",
  he="hebrew",
  hi="hindi",
  hu="hungarian",
  id="indonesian",
  it="italian",
  ja="japanese",
  ko="korean",
  lv="latvian",
  nb="norwegian",
  nn="norwegian",
  no="norwegian",
  pl="polish",
  pt="portuguese",
  ro="romanian",
  ru="russian",
  sk="slovak",
  sl="slovenian",
  es="spanish",
  sv="swedish",
  ta="tamil",
  tr="turkish",
  uk="ukrainian",
  vi="vietnamese")

# Guess language of the text based on the first 10 rows of it.
# Used to give default for get_stopwords().
guess_lang_for_stopwords <- function(text) {
  text <- head(text, 10)
  lang_code <- get_mode(cld3::detect_language(text))
  if (is.na(lang_code)) { # cld3 could not guess language.
    return("english") # Default to English
  }
  lang_name <- lang_code_mapping[lang_code]
  if (is.na(lang_name)) { # The language cld3 returned is not supported by get_stopwords().
    return("english") # Default to English
  }
  names(lang_name) <- NULL # Strip name
  lang_name
}

#' Function for Text Analysis Analytics View
#' @export
exp_textanal <- function(df, text,
                         remove_punct = TRUE, remove_numbers = TRUE,
                         stopwords_lang = NULL, stopwords = c(),
                         hiragana_word_length_to_remove = 2,
                         compound_tokens = NULL,
                         cooccurrence_context = "window", # "document" or "window"
                         cooccurrence_window = 1, # 5 is the quanteda's default, but narrowing it for speed of default run. 
                         cooccurrence_network_num_words = 50,
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

    # <Tokenizing code with quoanteda's default tokenizer.> TODO: Remove it when there is no need to keep it as a reference.
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
    names(tokenized) <- paste0("text", 1:length(tokenized)) # Add unique names to the list so that it can be passed to quanteda::tokens().
    tokens <- quanteda::tokens(tokenized)
    # tokens <- tokens %>% quanteda::tokens_wordstem() # TODO: Revive stemming and expose as an option.

    if (!is.null(compound_tokens)) { # This probably should be kept before removing stopwords not to break compoint tokens that includes stopwords.
      tokens <- tokens %>% quanteda::tokens_compound(pattern = quanteda::phrase(compound_tokens), concatenator = ' ')
    }

    # when stopwords Language is set, use the stopwords to filter out the result.
    if(!is.null(stopwords_lang)) {
      if (stopwords_lang == "auto") {
        stopwords_lang <- guess_lang_for_stopwords(df[[text_col]])
      }
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

    feats_selected <- quanteda::topfeatures(dfm_res, cooccurrence_network_num_words)
    feat_names <- names(feats_selected)
    fcm_selected <- quanteda::fcm_select(fcm_res, pattern = feat_names)

    # # Document clustering code below is temporarily commented out.
    # dfm_tfidf_res <- quanteda::dfm_tfidf(dfm_res)

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
    # Co-occurrence network / document clustering related code below is temporarily commented out. TODO: Revive it.
    model$feats_selected <- feats_selected
    model$fcm_selected <- fcm_selected

    # model$dfm_tfidf <- dfm_tfidf_res
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

# Uncompresses compressed index in CSR (Compressed Sparse Row) format.
uncompress_csr_index <- function(idx_compressed) {
  tot_length <- idx_compressed[length(idx_compressed)]
  idx_uncompressed <- c(NA, tot_length) # Allocate space first to avoid repeated allocation.
  for (j in 1:(length(idx_compressed)-1)) {
    cur_idx <- idx_compressed[j]
    next_idx <- idx_compressed[j+1]
    rep_num <- next_idx - cur_idx
    if (rep_num > 0) {
      idx_uncompressed[(cur_idx+1):next_idx] <- rep(j-1, rep_num)
    }
  }
  idx_uncompressed
}

# Extracts data as a long-format data.frame from quanteda::dfm.
dfm_to_df <- function(dfm) {
  row_idx <- dfm@i
  col_idx_compressed <- dfm@p
  # dfm is in CSR (Compressed Sparse Row) format.
  # Uncompress column index.
  col_idx <- uncompress_csr_index(col_idx_compressed)
  col_feats <- dfm@Dimnames$features[col_idx+1]
  # row_docs <- dfm@Dimnames$docs[row_idx+1] # This is id generated by quanteda. e.g. "text1".
  row_docs <- row_idx+1 # Use number id instead for now so that it is sorted better by subsequent process.
  value <- dfm@x
  res <- tibble::tibble(document=row_docs, token=col_feats, value=value)
  res
}

# Extracts data as a long-format data.frame from quanteda::fcm.
fcm_to_df <- function(fcm) {
  row_idx <- fcm@i
  col_idx_compressed <- fcm@p
  # fcm is in CSR (Compressed Sparse Row) format.
  # Uncompress column index.
  col_idx <- uncompress_csr_index(col_idx_compressed)
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
    res <- res %>% dplyr::group_by(cluster) %>% mutate(document_id = row_number()) %>% ungroup()
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

# vertex_size_method - "equal_length" or "equal_freq"
get_cooccurrence_graph_data <- function(model_df, max_vertex_size = 25, vertex_size_method = "equal_length", max_edge_width=8, font_size_factor=1.0) {
  # Prepare edges data
  edges <- exploratory:::fcm_to_df(model_df$model[[1]]$fcm_selected) %>% rename(from=token.x,to=token.y) %>% filter(from!=to)
  edges <- edges %>% mutate(from = stringr::str_to_title(from), to = stringr::str_to_title(to))

  edges <- edges %>% mutate(width=log(value+1)) # +1 to avoid 0 width.
  edges <- edges %>% mutate(width=max_edge_width*width/max(width))

  # Set edge colors based on number of co-occurrence.
  c_scale <- grDevices::colorRamp(c("white","red"))
  edges <- edges %>% mutate(color=apply(c_scale((log(value)+1)/max(log(value)+1)), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255, alpha=0.8)))
  weights=log(1+edges$value)
  weights <- 5*weights/max(weights)
  edges <- edges %>% mutate(weights=weights)

  # Prepare vertices data
  feat_names <- names(model_df$model[[1]]$feats_selected)
  feat_names <- stringr::str_to_title(feat_names)
  feat_counts <- model_df$model[[1]]$feats_selected
  names(feat_counts) <- NULL
  if (vertex_size_method == "equal_length") {
    vertex_sizes <- as.numeric(cut(feat_counts,5)) # equal_length
  }
  else { # "equal_freq"
    vertex_sizes <- floor((dplyr::min_rank(feat_counts)-1)/(length(feat_counts)/5)) + 1
  }
  vertex_sizes = vertex_sizes/max(vertex_sizes) * max_vertex_size
  vertices <- tibble::tibble(name=feat_names, size=vertex_sizes)

  ret <- list(edges=edges, vertices=vertices)
  attr(ret, "font_size_factor") <- font_size_factor
  ret <- data.frame(model=I(list(ret))) # return as data.frame. TODO: handle group_by
  class(ret$model) <- c("list", "exp_coocurrence_graph")
  ret
}

