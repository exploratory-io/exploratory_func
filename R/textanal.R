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

# which.max can return integer(0) if input vector consists of all NAs.
# This sometimes happens for the lda output from seededlda. Have not figured out exact condition it happens yet.
# This function is to work around such a case by returning 1, so that it does not break the rest of the processing.
which.max.safe <- function(x) {
  y <- which.max(x)
  if (length(y) == 0) 1 else y
}

# Helper function to parse comma-separated tokens from a character vector
parse_comma_separated_tokens <- function(word_column) {
  # Split by comma
  tokens_list <- stringr::str_split(word_column, pattern = ",")
  
  # Trim whitespace and filter empty strings
  tokens_list <- purrr::map(tokens_list, function(tokens) {
    trimmed <- stringr::str_trim(tokens)
    trimmed[trimmed != "" & !is.na(trimmed)]
  })
  
  tokens_list
}

tokenize_with_postprocess <- function(text,
                                      remove_punct = TRUE, remove_numbers = TRUE,
                                      remove_alphabets = FALSE,
                                      tokenize_tweets = FALSE,
                                      remove_url = TRUE, remove_twitter = TRUE,
                                      stopwords_lang = NULL, stopwords = c(), stopwords_to_remove = c(),
                                      hiragana_word_length_to_remove = 2,
                                      compound_tokens = NULL
                                      ) {
  # comment out the "tokenize_tweets" handling for now since it's no longer available with tokenizers package.
  #if (tokenize_tweets) {
  #  tryCatch({
  #    tokenized <- tokenizers::tokenize_tweets(text, lowercase = TRUE, stopwords = NULL,
  #                                             strip_punct = remove_punct, strip_url = remove_url, simplify = FALSE)
  #  }, error = function(e) {
      # tokenize_tweets can throw error about invalid UTF-8 characters.
      # Try to recover from it by fixing the input with stri_enc_toutf8.
  #    if (stringr::str_detect(e$message, "invalid UTF-8 byte sequence detected")) {
        # validate=TRUE replaces invalid characters with replacement character.
        # Since text is fed to guess_lang_for_stopwords() later, use <<- rather than <- here.
  #      text <<- stringi::stri_enc_toutf8(text, validate = TRUE)
  #      tokenized <<- tokenizers::tokenize_tweets(text, lowercase = TRUE, stopwords = NULL,
  #                                                strip_punct = remove_punct, strip_url = remove_url, simplify = FALSE)
  #    }
  #    else {
  #      stop(e)
  #    }
  #  })
  #}
  #else {
    if (remove_url) { # For tokenizers::tokenize_words, we remove urls ourselves beforehand.
      text <- str_remove_url(text)
    }
    tokenized <- tokenizers::tokenize_words(text, lowercase = TRUE, stopwords = NULL,
                                            strip_punct = remove_punct, simplify = FALSE)
  #}
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
      if (tokenize_tweets) {
        compound_tokens_list <- tokenizers::tokenize_tweets(compound_tokens_without_space, lowercase = TRUE, stopwords = NULL,
                                                            strip_punct = remove_punct, strip_url = remove_url, simplify = FALSE)
      }
      else {
        compound_tokens_list <- tokenizers::tokenize_words(compound_tokens_without_space, lowercase = TRUE, stopwords = NULL,
                                                           strip_punct = remove_punct, simplify = FALSE)
      }
      # Create space-separated expression of the word, which can be used with quanteda::tokens_compound.
      compound_tokens_with_space_inserted <- purrr::flatten_chr(purrr::map(compound_tokens_list, function(x){stringr::str_c(x, collapse=' ')}))
      tokens <- tokens %>% quanteda::tokens_compound(pattern = quanteda::phrase(compound_tokens_with_space_inserted), concatenator = '')
    }
  }

  # when stopwords Language is set, use the stopwords to filter out the result.
  if (!is.null(stopwords_lang)) {
    if (stopwords_lang == "auto") {
      stopwords_lang <- guess_lang_for_stopwords(text)
    }
    stopwords <- stringr::str_to_lower(stopwords)
    stopwords_to_remove <- stringr::str_to_lower(stopwords_to_remove)
    stopwords_final <- exploratory::get_stopwords(lang = stopwords_lang, include = stopwords, exclude = stopwords_to_remove)
    tokens <- tokens %>% quanteda::tokens_remove(stopwords_final, valuetype = "fixed")
  }
  # Remove Japanese Hiragana word whose length is less than hiragana_word_length_to_remove
  if (hiragana_word_length_to_remove > 0) {
    tokens <- tokens %>% quanteda::tokens_remove(stringr::str_c("^[\\\u3040-\\\u309f]{1,", hiragana_word_length_to_remove, "}$"), valuetype = "regex")
  }
  if (remove_numbers) {
    # Since tokenize_words(strip_numeric=TRUE) seems to look at only the last char of token and strip too much words, we do it ourselves here instead.
    if (remove_alphabets) { # Remove alphanumeric words.
      # \uff10-\uff19 are fullwidth numbers. https://en.wikipedia.org/wiki/Halfwidth_and_Fullwidth_Forms_(Unicode_block)
      tokens <- tokens %>% quanteda::tokens_remove("^[a-zA-Z0-9\uff10-\uff19]+$", valuetype = "regex")
    }
    else { # Remove only numeric words.
      tokens <- tokens %>% quanteda::tokens_remove("^[0-9\uff10-\uff19]+$", valuetype = "regex")
    }
  }
  else if (remove_alphabets) { # Remove only alphabet-only words.
    tokens <- tokens %>% quanteda::tokens_remove("^[a-zA-Z]+$", valuetype = "regex")
  }
  # Results from tokenizers::tokenize_tweets seems to include emojis unlike tokenizers::tokenize_words.
  # For now, strip all-emoji-tokens here since they can't be displayed on word cloud.
  if (tokenize_tweets) {
    emoji_regex <- get_emoji_regex()
    tokens <- tokens %>% quanteda::tokens_remove(paste0("^(", emoji_regex, ")+$"), valuetype = "regex")
  }
  if (tokenize_tweets && remove_twitter) {
    # Remove twitter social tags with the same regex as in tokenizers::tokenize_twitter.
    tokens <- tokens %>% quanteda::tokens_remove("^#[A-Za-z]+\\w*|^@\\w+", valuetype = "regex")
  }
  tokens
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
  if (length(row_idx) == 0) { # No co-occurrence is in the fcm. Return empty data frame.
    return(tibble::tibble(token.x=character(0), token.y=character(0), value=integer(0)))
  }
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
    tokens_list <- as.list(x$tokens)
    # Use actual document names from tokens if available, otherwise use sequential numbers
    doc_names <- quanteda::docnames(x$tokens)
    if (length(doc_names) == 0 || all(doc_names == "")) {
      doc_names <- seq(length(tokens_list))
    }
    res <- tibble::tibble(document=doc_names, lst=tokens_list)
    res <- res %>% tidyr::unnest_longer(lst, values_to = "word") %>% dplyr::mutate(word = stringr::str_to_title(word))
  }
  if (type == "word_count" || type == "category_word_count") {
    feats <- quanteda::featfreq(x$dfm)
    res <- tibble::tibble(word=names(feats), count=feats)
    if (!is.null(max_words)) { # This means it is for bar chart.
      if (max_words < 100) {
        res <- res %>% dplyr::slice_max(count, n=max_words, with_ties=TRUE) %>% slice_max(count, n=100, with_ties=FALSE) # Set hard limit of 100 even with ties.
      }
      else {
        res <- res %>% dplyr::slice_max(count, n=max_words, with_ties=FALSE) # Set hard limit even with ties.
      }
    }

    # If there is category_col, create data frame whose row represents a category-word combination, for bar chart with category color.
    if (type == "category_word_count" && !is.null(x$category_col)) {
      res2 <- dfm_to_df(x$dfm)
      if (!is.null(max_words)) { # filter with the top words.
        res2 <- res2 %>% filter(token %in% res$word)
      }
      # Join document info.
      res2 <- res2 %>% dplyr::left_join(x$df %>% dplyr::select(!!rlang::sym(x$category_col)) %>% dplyr::mutate(doc_id=row_number()), by=c(document="doc_id")) %>%
        dplyr::rename(word = token, count=value) # Align output column names with the case without category_col.
      res <- res2 %>% dplyr::group_by(!!rlang::sym(x$category_col), word) %>% dplyr::summarize(count = sum(count))
    }
    res <- res %>% dplyr::mutate(word=stringr::str_to_title(word)) # Make it title case for displaying.
  }
  else if (type == "word_pairs") {
    res <- fcm_to_df(x$fcm) %>%
      dplyr::filter(token.x != token.y) %>%
      dplyr::mutate(token.x = stringr::str_to_title(token.x), token.y = stringr::str_to_title(token.y)) %>%
      dplyr::rename(word.1 = token.x, word.2 = token.y, count=value)
    if (!is.null(max_word_pairs)) { # This means it is for bar chart.
      if (max_word_pairs < 100) {
        res <- res %>% dplyr::slice_max(count, n=max_word_pairs, with_ties=TRUE) %>% dplyr::slice_max(count, n=100, with_ties=FALSE) # Set hard limit of 100 even with ties.
      }
      else {
        res <- res %>% dplyr::slice_max(count, n=max_word_pairs, with_ties=FALSE) # Set hard limit even with ties.
      }
    }
  }
  res
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



#' extracts results from textmodel_lda_exploratory object as a dataframe
#' @export
#' @param type - Type of output.
#' @param num_top_words - Number of top words for each topic in the output of topic_words type.
#' @param word_topic_probability_threshold - The probability of the topic of the word required to be highlighted in the output of doc_topics_tagged type.
tidy.textmodel_lda_exploratory <- function(x, type = "doc_topics", num_top_words = 10, word_topic_probability_threshold = 0, ...) {
  if (type == "topics_summary") { # Count number of documents that "belongs to" each topic.
    res <- x$doc_df %>% dplyr::select(max_topic) %>% dplyr::group_by(max_topic) %>% dplyr::summarize(n=n())
    # In case some topic do not have any doc that "belongs to" it, we still want to show a row for the topic with n with 0 value.
    res <- res %>% tidyr::complete(max_topic = 1:x$model$k, fill = list(n=0)) %>% dplyr::rename(topic = max_topic)
  }
  else if (type == "word_topics") {
    res <- x$words_topics_df %>% dplyr::mutate(max_topic = summarize_row(across(starts_with("topic")), which.max.safe), topic_max = summarize_row(across(starts_with("topic")), max))
  }
  else if (type == "topic_words") { # Similar to the above but this is pivotted and sampled. TODO: Organize.
    terms_topics_df <- x$words_topics_df %>% tidyr::pivot_longer(names_to = 'topic', values_to = 'probability', matches('^topic[0-9]+$'))
    res <- terms_topics_df %>% dplyr::group_by(topic) %>% dplyr::slice_max(probability, n = num_top_words, with_ties = FALSE) %>% dplyr::ungroup()
  }
  else if (type == "doc_topics") {
    res <- x$doc_df
  }
  else if (type == "doc_topics_tagged") {
    words_to_tag_df <- x$doc_word_df %>% dplyr::mutate(max_topic = summarize_row(across(starts_with("topic")), which.max.safe),
                                                       .topic_max = summarize_row(across(starts_with("topic")), max),
                                                       .topic_sum = summarize_row(across(starts_with("topic")), sum),
                                                       max_topic_prob = .topic_max/.topic_sum)
    words_to_tag_df <- words_to_tag_df %>% dplyr::filter(max_topic_prob > word_topic_probability_threshold) # TODO: expose the threshold.
    tag_df <- words_to_tag_df %>% dplyr::nest_by(document) %>% dplyr::ungroup()
    res <- x$doc_df %>% dplyr::rename(text=!!x$text_col) %>% dplyr::mutate(doc_id=row_number()) %>% left_join(tag_df, by=c("doc_id"="document"))
    res <- res %>% dplyr::mutate(tagged_text=purrr::flatten_chr(purrr::map2(text, data, function(txt,dat) {
      if (!is.null(dat)) {
        # dat is a data frame of words to surround with tags. The words in dat are in the order of appearance in the text.
        # We will find and tag them one by one from the beginning of the text to the end of the text.
        txt_out <- ''
        txt_remaining <- txt
        for (i in 1:nrow(dat)) {
          if (stringr::str_detect(dat$word[i], '[a-zA-Z]')) { # For alphabet word, char before/after should not be alphabet, to avoid matches within other words.
            # \\Q, \\E are to match literally even if regex special characters like . or - are in the word.
            pre_regex <- '^(.*?[^a-zA-Z])(\\Q'
            post_regex <- '\\E)([^a-zA-Z].*)$'
          }
          else {
            pre_regex <- '^(.*?)(\\Q' # .*? is for the shortest match, since we want to match with the first appearance of the word.
            post_regex <- '\\E)(.*)$'
          }
          # dotall = TRUE is necessary to process entire multiline text.
          matches <- stringr::str_match(txt_remaining, stringr::regex(stringr::str_c(pre_regex, dat$word[i], post_regex), ignore_case = TRUE, dotall = TRUE))
          if (!is.na(matches[1])) { # There always should be a match, but if there is no match, we just move on to the next word without proceeding on the text.
            txt_out <- stringr::str_c(txt_out, matches[2], '<span topic="', dat$max_topic[i], '">', matches[3], '</span>')
            txt_remaining <- matches[4]
          }
        }
        txt_out <- stringr::str_c(txt_out, txt_remaining)
        txt_out
      }
      else {
        txt
      }
    })))
  }
  else if (type == "doc_word_category") {
    terms_topics_df <- x$words_topics_df %>% tidyr::pivot_longer(names_to = 'topic', values_to = 'probability', matches('^topic[0-9]+$'))
    top_words_df <- terms_topics_df %>% dplyr::group_by(topic) %>% dplyr::slice_max(probability, n = num_top_words, with_ties = FALSE) %>% dplyr::ungroup()
    top_words_df <- top_words_df %>% mutate(topic=parse_number(topic))
    doc_df <- x$doc_df %>% dplyr::mutate(doc_id=row_number()) %>% dplyr::select(doc_id, document_max_topic=max_topic, !!rlang::sym(x$category_col))
    doc_word_df <- x$doc_word_df %>% dplyr::select(document, word)
    res <- doc_word_df %>% dplyr::left_join(doc_df, by=c(document="doc_id")) %>% dplyr::right_join(top_words_df, by=c(document_max_topic="topic", word="word"))
  }
  # # Unused MDS code. Keeping it for now.
  # else if (type == "doc_topics_mds") {
  #   res <- x$df[x$docs_sample_index,]
  #   docs_topics_sampled <- x$model$theta[x$docs_sample_index,]
  #   docs_topics_df <- as.data.frame(docs_topics_sampled)
  #   docs_topics_df <- docs_topics_df %>% dplyr::mutate(max_topic = summarize_row(across(starts_with("topic")), which.max.safe))
  #   res <- res %>% dplyr::bind_cols(docs_topics_df)
  #   docs_coordinates_df <- as.data.frame(x$docs_coordinates)
  #   res <- res %>% dplyr::bind_cols(docs_coordinates_df)
  # }
  res
}



