
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

tokenize_with_postprocess <- function(text, 
                                      remove_punct = TRUE, remove_numbers = TRUE,
                                      stopwords_lang = NULL, stopwords = c(), stopwords_to_remove = c(),
                                      hiragana_word_length_to_remove = 2,
                                      compound_tokens = NULL
                                      ) {
  tokenized <- tokenizers::tokenize_words(text, lowercase = TRUE, stopwords = NULL, strip_punct = remove_punct, strip_numeric = remove_numbers, simplify = FALSE)
  names(tokenized) <- paste0("text", 1:length(tokenized)) # Add unique names to the list so that it can be passed to quanteda::tokens().
  tokens <- quanteda::tokens(tokenized)
  # tokens <- tokens %>% quanteda::tokens_wordstem() # TODO: Revive stemming and expose as an option.

  if (!is.null(compound_tokens)) { # This probably should be kept before removing stopwords not to break compound tokens that includes stopwords.
    # Split compound_tokens into ones separated by space and ones that are not.
    with_space_idx <- str_detect(compound_tokens, ' ')
    compound_tokens_with_space <- compound_tokens[with_space_idx]
    compound_tokens_without_space <- compound_tokens[!with_space_idx]

    # Handle ones separated by spaces.
    if (length(compound_tokens_with_space) > 0) {
      tokens <- tokens %>% quanteda::tokens_compound(pattern = quanteda::phrase(compound_tokens_with_space), concatenator = ' ')
    }

    # Handle ones that are not separated by spaces.
    if (length(compound_tokens_without_space) > 0) {
      # Tokenize those words with the same options with the original tokinizing, to know where such word would have been splitted.
      compound_tokens_list <- tokenizers::tokenize_words(compound_tokens_without_space, lowercase = TRUE, stopwords = NULL, strip_punct = remove_punct, strip_numeric = remove_numbers, simplify = FALSE)
      # Create space-separated expression of the word, which can be used with quanteda::tokens_compound.
      compound_tokens_with_space_inserted <- purrr::flatten_chr(purrr::map(compound_tokens_list, function(x){stringr::str_c(x, collapse=' ')}))
      tokens <- tokens %>% quanteda::tokens_compound(pattern = quanteda::phrase(compound_tokens_with_space_inserted), concatenator = '')
    }
  }

  # when stopwords Language is set, use the stopwords to filter out the result.
  if(!is.null(stopwords_lang)) {
    if (stopwords_lang == "auto") {
      stopwords_lang <- guess_lang_for_stopwords(text)
    }
    stopwords <- stringr::str_to_lower(stopwords)
    stopwords_to_remove <- stringr::str_to_lower(stopwords_to_remove)
    stopwords_final <- exploratory::get_stopwords(lang = stopwords_lang, include = stopwords, exclude = stopwords_to_remove)
    tokens <- tokens %>% quanteda::tokens_remove(stopwords_final, valuetype = "fixed")
  }
  # Remove Japanese Hiragana word whose length is less than hiragana_word_length_to_remove
  if(hiragana_word_length_to_remove > 0) {
    tokens <- tokens %>% quanteda::tokens_remove(stringr::str_c("^[\\\u3040-\\\u309f]{1,", hiragana_word_length_to_remove, "}$"), valuetype = "regex")
  }
  tokens
}


#' Function for Text Analysis Analytics View
#' @export
exp_textanal <- function(df, text,
                         remove_punct = TRUE, remove_numbers = TRUE,
                         stopwords_lang = NULL, stopwords = c(), stopwords_to_remove = c(),
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
    # Filter out NAs before sampling. We keep empty string, since we will anyway have to work with the case where no token was found in a doc.
    df <- df %>% dplyr::filter(!is.na(!!rlang::sym(text_col)))

    # sample the data for performance if data size is too large.
    sampled_nrow <- NULL
    if (!is.null(max_nrow) && nrow(df) > max_nrow) {
      # Record that sampling happened.
      sampled_nrow <- max_nrow
      df <- df %>% sample_rows(max_nrow)
    }

    tokens <- tokenize_with_postprocess(df[[text_col]],
                                        remove_punct = remove_punct, remove_numbers = remove_numbers,
                                        stopwords_lang = stopwords_lang, stopwords = stopwords, stopwords_to_remove = stopwords_to_remove,
                                        hiragana_word_length_to_remove = hiragana_word_length_to_remove,
                                        compound_tokens = compound_tokens)
    # It is possible that character(0) is returned for document that did not have any tokens, but this still can be handled in subsequent process.

    # convert tokens to dfm object
    dfm_res <- tokens %>% quanteda::dfm()
    fcm_res <- quanteda::fcm(tokens, context = cooccurrence_context, window = cooccurrence_window, tri = TRUE)

    feats_selected <- quanteda::topfeatures(dfm_res, cooccurrence_network_num_words)
    feat_names <- names(feats_selected)
    fcm_selected <- quanteda::fcm_select(fcm_res, pattern = feat_names)

    model <- list()
    model$tokens <- tokens
    model$dfm <- dfm_res
    model$fcm <- fcm_res
    # Co-occurrence network / document clustering related code below is temporarily commented out. TODO: Revive it.
    model$feats_selected <- feats_selected
    model$fcm_selected <- fcm_selected

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
  res <- tibble::tibble(document=row_docs, token=col_feats, token_id=col_idx+1, value=value)
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
tidy.textanal_exploratory <- function(x, type="word_count", max_words=NULL, max_word_pairs=NULL, ...) {
  if (type == "words") {
    res <- tibble::tibble(document=seq(length(as.list(x$tokens))), lst=as.list(x$tokens))
    res <- res %>% tidyr::unnest_longer(lst, values_to = "word") %>% dplyr::mutate(word = stringr::str_to_title(word))
  }
  if (type == "word_count") {
    feats <- quanteda::featfreq(x$dfm)
    res <- tibble::tibble(word=stringr::str_to_title(names(feats)), count=feats)
    if (!is.null(max_words)) { # This means it is for bar chart.
      if (max_words < 100) {
        res <- res %>% dplyr::slice_max(count, n=max_words, with_ties=TRUE) %>% slice_max(count, n=100, with_ties=FALSE) # Set hard limit of 100 even with ties.
      }
      else {
        res <- res %>% dplyr::slice_max(count, n=max_words, with_ties=FALSE) # Set hard limit even with ties.
      }
    }
  }
  else if (type == "word_pairs") {
    res <- fcm_to_df(x$fcm) %>%
      dplyr::filter(token.x != token.y) %>%
      dplyr::mutate(token.x = stringr::str_to_title(token.x), token.y = stringr::str_to_title(token.y)) %>%
      dplyr::rename(word.1 = token.x, word.2 = token.y, count=value)
    if (!is.null(max_word_pairs)) { # This means it is for bar chart.
      if (max_word_pairs < 100) {
        res <- res %>% dplyr::slice_max(count, n=max_word_pairs, with_ties=TRUE) %>% slice_max(count, n=100, with_ties=FALSE) # Set hard limit of 100 even with ties.
      }
      else {
        res <- res %>% dplyr::slice_max(count, n=max_word_pairs, with_ties=FALSE) # Set hard limit even with ties.
      }
    }
  }
  res
}

# vertex_size_method - "equal_length" or "equal_freq"
get_cooccurrence_graph_data <- function(model_df, max_vertex_size = 20, vertex_size_method = "equal_length", max_edge_width=8, font_size_ratio=1.0, area_factor=50, vertex_opacity=0.6, cluster_method="louvain") {
  # Prepare edges data
  edges <- exploratory:::fcm_to_df(model_df$model[[1]]$fcm_selected) %>% rename(from=token.x,to=token.y) %>% filter(from!=to)
  edges <- edges %>% mutate(from = stringr::str_to_title(from), to = stringr::str_to_title(to))

  edges <- edges %>% mutate(width=log(value+1)) # +1 to avoid 0 width.
  edges <- edges %>% mutate(width=max_edge_width*width/max(width))

  # Set edge colors based on number of co-occurrence.
  c_scale <- grDevices::colorRamp(c("white","#4A90E2"))
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
  vertex_sizes <- vertex_sizes/max(vertex_sizes) * max_vertex_size
  vertices <- tibble::tibble(name=feat_names, size=vertex_sizes)

  if (cluster_method != "none") {
    g <- igraph::graph.data.frame(edges, directed=FALSE, vertices=vertices) # Temporary graph object just to calculate cluster membership.
    lc <- switch(cluster_method,
                 louvain = igraph::cluster_louvain(g, weights=edges$value),
                 leading_eigen = igraph::cluster_leading_eigen(g, weights=edges$value),
                 fast_greedy = igraph::cluster_fast_greedy(g, weights=edges$value),
                 spinglass = igraph::cluster_spinglass(g, weights=edges$value),
                 infomap = igraph::cluster_infomap(g, e.weights=edges$value),
                 edge_betweenness = igraph::cluster_edge_betweenness(g, weights=edges$value),
                 label_prop = igraph::cluster_label_prop(g, weights=edges$value),
                 walktrap = igraph::cluster_walktrap(g, weights=edges$value)
    )
    cluster <- as.numeric(igraph::membership(lc))
    vertices <- vertices %>% dplyr::mutate(cluster=!!cluster)
  }

  ret <- list(edges=edges, vertices=vertices)
  attr(ret, "font_size_factor") <- font_size_ratio
  attr(ret, "area_factor") <- area_factor
  attr(ret, "vertex_opacity") <- vertex_opacity
  ret <- data.frame(model=I(list(ret))) # return as data.frame. TODO: handle group_by
  class(ret$model) <- c("list", "exp_coocurrence_graph")
  ret
}

#' Function for Text Clustering Analytics View
#' @export
exp_text_cluster <- function(df, text,
                         remove_punct = TRUE, remove_numbers = TRUE,
                         stopwords_lang = NULL, stopwords = c(), stopwords_to_remove = c(),
                         hiragana_word_length_to_remove = 2,
                         compound_tokens = NULL,
                         tf_scheme = "logcount",
                         idf_scheme = "unary",
                         tfidf_base = 10,
                         idf_smoothing = 0,
                         idf_k = 0,
                         idf_threshold = 0,
                         svd_dim=5,
                         num_clusters=3,
                         mds_sample_size=200,
                         max_nrow = 50000,
                         seed = 1,
                         ...){

  # Always put document_id to know what document the tokens are from
  text_col <- tidyselect::vars_pull(names(df), !! rlang::enquo(text))
  doc_id <- avoid_conflict(colnames(df), "document_id")

  # Set seed just once.
  if(!is.null(seed)) {
    set.seed(seed)
  }

  each_func <- function(df) {
    # Filter out NAs before sampling. We keep empty string, since we will anyway have to work with the case where no token was found in a doc.
    df <- df %>% dplyr::filter(!is.na(!!rlang::sym(text_col)))

    # sample the data for performance if data size is too large.
    sampled_nrow <- NULL
    if (!is.null(max_nrow) && nrow(df) > max_nrow) {
      # Record that sampling happened.
      sampled_nrow <- max_nrow
      df <- df %>% sample_rows(max_nrow)
    }

    tokens <- tokenize_with_postprocess(df[[text_col]],
                                        remove_punct = remove_punct, remove_numbers = remove_numbers,
                                        stopwords_lang = stopwords_lang, stopwords = stopwords, stopwords_to_remove = stopwords_to_remove,
                                        hiragana_word_length_to_remove = hiragana_word_length_to_remove,
                                        compound_tokens = compound_tokens)

    # convert tokens to dfm object
    dfm_res <- tokens %>% quanteda::dfm()

    dfm_tfidf_res <- quanteda::dfm_tfidf(dfm_res,
                                         scheme_tf = tf_scheme,
                                         scheme_df = idf_scheme,
                                         base = tfidf_base,
                                         smoothing = idf_smoothing,
                                         k = idf_k,
                                         threshold = idf_threshold)

    # Cluster documents with k-means.
    tfidf_df <- dfm_to_df(dfm_tfidf_res)
    tfidf_df <- tfidf_df %>% dplyr::rename(tfidf=value)

    # Convert to sparse matrix
    tfidf_sparse_mat <- Matrix::sparseMatrix(
      i = as.integer(tfidf_df$document),
      j = as.integer(tfidf_df$token_id),
      x = as.numeric(tfidf_df$tfidf),
      dims = dim(dfm_tfidf_res)
      )
    # SVD by irlba. nu defaults to nv. TODO: Look into if reducing nv affects performance positively without negative impact on the result.
    svd_res <- irlba::irlba(tfidf_sparse_mat, nv=svd_dim)
    docs_reduced <- svd_res$u

    # Make it a data frame (a row represents a document)
    docs_reduced_df <- as.data.frame(docs_reduced)
    # Cluster documents. #TODO: Expose arguments for kmeans.
    model_df <- docs_reduced_df %>% build_kmeans.cols(everything(), centers=num_clusters, augment=FALSE, seed=NULL) # seed is NULL since we already set it.
    kmeans_res <- model_df$model[[1]]
    cluster_res <- kmeans_res$cluster # Clustering result


    docs_sample_index <- if (nrow(docs_reduced_df) > mds_sample_size) {
      sample(nrow(docs_reduced_df), size=mds_sample_size)
    }
    else {
      1:nrow(docs_reduced_df)
    }
    docs_reduced_sampled <- docs_reduced[docs_sample_index,]
    docs_dist_mat <- dist(docs_reduced_sampled)
    docs_coordinates <- cmdscale(docs_dist_mat)


    # Run tf-idf treating each cluster as a document.
    dfm_clustered <- quanteda::dfm_group(dfm_res, cluster_res)
    dfm_clustered_tfidf <- quanteda::dfm_tfidf(dfm_clustered)
    clustered_tfidf <- dfm_to_df(dfm_clustered_tfidf)

    model <- list()
    model$dfm <- dfm_res

    model$dfm_tfidf <- dfm_tfidf_res
    model$kmeans <- kmeans_res
    model$dfm_cluster <- dfm_clustered
    model$dfm_cluster_tfidf <- dfm_clustered_tfidf

    model$docs_coordinates <- docs_coordinates # MDS result for scatter plot
    model$docs_sample_index <- docs_sample_index
    model$df <- df # Keep original df for showing it with clustering result.
    model$sampled_nrow <- sampled_nrow
    class(model) <- 'text_cluster_exploratory'
    model
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' extracts results from textanal_exploratory object as a dataframe
#' @export
#' @param type - Type of output.
tidy.text_cluster_exploratory <- function(x, type="word_count", num_top_words=5, ...) {
  if (type == "clusters") {
    res <- tibble(cluster=seq(length(x$kmeans$size)), size=x$kmeans$size, withinss=x$kmeans$withinss)
  }
  else if (type == "doc_cluster") {
    res <- x$df
    res <- res %>% dplyr::mutate(cluster=!!x$kmeans$cluster)
  }
  else if (type == "doc_cluster_mds") {
    res <- x$df[x$docs_sample_index,]
    cluster_res_sampled <- x$kmeans$cluster[x$docs_sample_index]
    res <- res %>% dplyr::mutate(cluster = !!cluster_res_sampled)
    docs_coordinates_df <- as.data.frame(x$docs_coordinates)
    res <- res %>% dplyr::bind_cols(docs_coordinates_df)
  }
  else if (type == "doc_cluster_words") {
    res <- dfm_to_df(x$dfm_cluster_tfidf)
    res <- res %>% dplyr::group_by(document)

    # Set hard-limit of 100 words even with ties, unless the limit is explicitly set above 100.
    # If the limit is set above 100, ties above the limit is not shown.
    if (num_top_words < 100) {
      res <- res %>% dplyr::slice_max(value, n=num_top_words, with_ties=TRUE) %>% slice_max(value, n=100, with_ties=FALSE) # Set hard limit of 100 even with ties.
    }
    else {
      res <- res %>% dplyr::slice_max(value, n=num_top_words, with_ties=FALSE) # Set hard limit even with ties.
    }

    res <- res %>% dplyr::ungroup() %>%
      dplyr::rename(cluster = document)

    # Extract count of words in each cluster from dfm_cluster.
    res <- res %>% dplyr::nest_by(cluster) %>% dplyr::ungroup() %>%
      dplyr::mutate(data=purrr::map2(data, cluster, function(y, clstr) {
        y %>% dplyr::mutate(count=as.numeric(x$dfm_cluster[clstr,token_id]))
      })) %>% tidyr::unnest(data)
  }
  res
}

#' Function for Topic Model Analytics View
#' @export
exp_topic_model <- function(df, text,
                            remove_punct = TRUE, remove_numbers = TRUE,
                            stopwords_lang = NULL, stopwords = c(), stopwords_to_remove = c(),
                            hiragana_word_length_to_remove = 2,
                            compound_tokens = NULL,
                            num_topics = 3,
                            max_iter = 2000,
                            alpha = NULL,
                            beta = NULL,
                            mds_sample_size=200,
                            max_nrow = 50000,
                            seed = 1,
                            ...){

  # Always put document_id to know what document the tokens are from
  text_col <- tidyselect::vars_pull(names(df), !! rlang::enquo(text))
  doc_id <- avoid_conflict(colnames(df), "document_id")

  # Set seed just once.
  if(!is.null(seed)) {
    set.seed(seed)
  }

  each_func <- function(df) {
    # Filter out NAs before sampling. We keep empty string, since we will anyway have to work with the case where no token was found in a doc.
    df <- df %>% dplyr::filter(!is.na(!!rlang::sym(text_col)))

    # sample the data for performance if data size is too large.
    sampled_nrow <- NULL
    if (!is.null(max_nrow) && nrow(df) > max_nrow) {
      # Record that sampling happened.
      sampled_nrow <- max_nrow
      df <- df %>% sample_rows(max_nrow)
    }

    tokens <- tokenize_with_postprocess(df[[text_col]],
                                        remove_punct = remove_punct, remove_numbers = remove_numbers,
                                        stopwords_lang = stopwords_lang, stopwords = stopwords, stopwords_to_remove = stopwords_to_remove,
                                        hiragana_word_length_to_remove = hiragana_word_length_to_remove,
                                        compound_tokens = compound_tokens)

    # convert tokens to dfm object
    dfm_res <- tokens %>% quanteda::dfm()

    lda_model <- seededlda::textmodel_lda(dfm_res, k = num_topics, max_iter=max_iter, alpha=alpha, beta=beta)
    docs_topics <- lda_model$theta # theta is the documents-topics matrix.

    # MDS for scatter plot. Commented out for now.
    # docs_sample_index <- if (nrow(docs_topics) > mds_sample_size) {
    #   sample(nrow(docs_topics), size=mds_sample_size)
    # }
    # else {
    #   1:nrow(docs_topics)
    # }
    #
    # # Prepare data for MDS. We use sampled-down data.
    # docs_topics_sampled <- docs_topics[docs_sample_index,]
    # docs_dist_mat <- dist(docs_topics_sampled)
    # docs_coordinates <- cmdscale(docs_dist_mat)

    model <- list()
    model$model <- lda_model
    # model$docs_coordinates <- docs_coordinates # MDS result for scatter plot
    # model$docs_sample_index <- docs_sample_index
    model$df <- df # Keep original df for showing it with LDA result.
    model$sampled_nrow <- sampled_nrow
    class(model) <- 'textmodel_lda_exploratory'
    model
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' extracts results from textmodel_lda_exploratory object as a dataframe
#' @export
#' @param type - Type of output.
tidy.textmodel_lda_exploratory <- function(x, type = "doc_topics", num_top_words = 10, ...) {
  if (type == "word_topics") {
    terms_topics_df <- as.data.frame(t(x$model$phi)) # phi is the topics-terms matrix. This needs to be transposed to make it a terms-topics matrix.
    terms <- rownames(terms_topics_df)
    terms_topics_df <- terms_topics_df %>% dplyr::mutate(max_topic = summarize_row(across(starts_with("topic")), which.max), topic_max = summarize_row(across(starts_with("topic")), max))
    res <- tibble::tibble(word=terms) %>% dplyr::bind_cols(terms_topics_df)
  }
  else if (type == "topic_words") { # Similar to the above but this is pivotted and sampled. TODO: Organize.
    terms_topics_df <- as.data.frame(t(x$model$phi))
    words <- rownames(terms_topics_df)
    terms_topics_df <- terms_topics_df %>% dplyr::mutate(word = words)
    terms_topics_df <- terms_topics_df %>% tidyr::pivot_longer(names_to = 'topic', values_to = 'probability', matches('^topic[0-9]+$'))
    res <- terms_topics_df %>% dplyr::group_by(topic) %>% dplyr::slice_max(probability, n = num_top_words, with_ties = FALSE) %>% dplyr::ungroup()
  }
  else if (type == "doc_topics") {
    res <- x$df
    docs_topics_df <- as.data.frame(x$model$theta)
    docs_topics_df <- docs_topics_df %>% dplyr::mutate(max_topic = summarize_row(across(starts_with("topic")), which.max), topic_max = summarize_row(across(starts_with("topic")), max))
    res <- res %>% dplyr::bind_cols(docs_topics_df)
  }
  else if (type == "doc_topics_mds") {
    res <- x$df[x$docs_sample_index,]
    docs_topics_sampled <- x$model$theta[x$docs_sample_index,]
    docs_topics_df <- as.data.frame(docs_topics_sampled)
    docs_topics_df <- docs_topics_df %>% dplyr::mutate(max_topic = summarize_row(across(starts_with("topic")), which.max))
    res <- res %>% dplyr::bind_cols(docs_topics_df)
    docs_coordinates_df <- as.data.frame(x$docs_coordinates)
    res <- res %>% dplyr::bind_cols(docs_coordinates_df)
  }
  res
}
