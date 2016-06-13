#' Check if the token is in stopwords.
#' @param token Character to be checked if it's stopword.
#' @param lexicon Type of stopwords. One of "snowball", "onix" and "SMART".
#' @return Logical vector if the token is in stopwords or not.
#' @export
is_stopword <- function(token, lexicon="snowball"){
  token %in% get_stopwords(lexicon)
}

#' Check if the word is digits.
#' @param word Character to be checked if it's digits.
#' @return Logical vector if the word is digits or not.
#' @export
is_digit <- function(word){
  loadNamespace("stringr")
  stringr::str_detect(word, "^[[:digit:]]+$")
}

#' Check if the word is digits.
#' @param word Character to be checked if it's digits.
#' @return Logical vector if the word is digits or not.
#' @export
is_alphabet <- function(word, lexicon="snowball"){
  loadNamespace("stringr")
  stringr::str_detect(word, "^[[:alpha:]]+$")
}

#' Get vector of stopwords
#' @param lexicon Type of stopwords. One of "snowball", "onix" and "SMART".
#' @return vector of stop word.
#' @export
get_stopwords <- function(lexicon="snowball"){
  loadNamespace("tidytext")
  data("stop_words", package = "tidytext", envir = environment())
  type_check <- stop_words$lexicon %in% lexicon
  words <- stop_words$word[type_check]
  unique(words)
}

#' Get sentiments of words
#' @param words Vector of words to check sentiment.
#' @param lexicon Type of sentiment. One of "nrc" "bing" "AFINN".
#' @return Vector of sentiment.
#' @export
get_sentiment <- function(words, lexicon="bing"){
  loadNamespace("tidytext")
  loadNamespace("dplyr")
  data("sentiments", package = "tidytext", envir = environment())
  # chosen lexicon and sentiment in words
  check <- sentiments$lexicon == lexicon
  joined_df <- dplyr::left_join(data.frame(word=words, stringsAsFactors = FALSE), sentiments[check,], by="word")
  if(lexicon=="AFINN"){
    joined_df$score
  } else {
    joined_df$sentiment
  }
}

#' Tokenize text and unnest
#' @param df Data frame
#' @param input Set a column of which you want to split the text or tokenize.
#' @param output Set a column name for the new column to store the tokenized values.
#' @param token Select the unit of token from "characters", "words", "sentences", "lines", "paragraphs", and "regex".
#' @param drop Whether input column should be removed.
#' @param to_lower Whether output should be lower cased.
#' @param with_id Whether output should contain original document id and sentence id in each document.
#' @return Data frame with tokenized column
#' @export
do_tokenize <- function(df, input, output=.token, token="words", drop=TRUE, with_id=TRUE, ...){
  loadNamespace("tidytext")
  loadNamespace("stringr")

  input_col <- col_name(substitute(input))
  output_col <- col_name(substitute(output))
  df[[input_col]] <- stringr::str_conv(df[[input_col]], "utf-8")
  if(token=="words" && with_id){
    loadNamespace("dplyr")

    # split into sentences
    func <- get("unnest_tokens_", asNamespace("tidytext"))
    df <- dplyr::mutate(df, .document_id=row_number())
    tokenize_df <- df[,c(".document_id", input_col)]
    args <- list(tokenize_df, output_col, input_col, token="sentences", drop=TRUE, ...)
    sentences <- do.call(func, args)
    grouped <- dplyr::group_by(sentences, .document_id)

    # split into tokens
    tokenize_df <- dplyr::mutate(grouped, .sentence_id=row_number())
    tokenize_df <- dplyr::ungroup(tokenize_df)
    args <- list(tokenize_df, output_col, output_col, token="words", drop=TRUE, ...)
    tokenized <- do.call(func, args)

    if(drop){
      df[[input_col]] <- NULL
    }

    dplyr::right_join(df, tokenized, by=".document_id")
  } else {
    tidytext::unnest_tokens_(df, col_name(substitute(output)), col_name(substitute(input)),token=token, drop=drop, ...)
  }
}

#' Get idf for terms
calc_idf <- function(group, term, log_scale = log, smooth_idf = FALSE){
  loadNamespace("Matrix")
  loadNamespace("text2vec")
  if(length(group)!=length(term)){
    stop("length of document and terms have to be the same")
  }
  doc_fact <- as.factor(group)
  term_fact <- as.factor(term)
  sparseMat <- Matrix::sparseMatrix(i = as.numeric(doc_fact), j = as.numeric(term_fact))
  idf <- text2vec::get_idf(sparseMat, log_scale=log_scale, smooth_idf=smooth_idf)
  idf <- idf@x[term_fact]
  df <- Matrix::colSums(sparseMat)[term_fact]
  data.frame(.df=df, .idf=idf)
}

#' Calculate term frequency
#' @param df Data frame
#' @param group Column to be considered as a group id
#' @param term Column to be considered as term
#' @param weight Type of weight calculation.
#' "ratio" is default and it's count/(total number of terms in the group).
#' This can be "raw_frequency", "binary", "log_normalization" and "k_normalization"
#' "raw_frequency" is the count of the term in the group.
#' "binary" is logic if the term is in the group or not.
#' "log_normalization" is logic if the term is in the group or not.
#' @param term Column to be considered as term
#' @return Data frame with group, term and .tf column
calc_tf <- function(df, group, term, ...){
  group_col <- col_name(substitute(group))
  term_col <- col_name(substitute(term))
  calc_tf_(df, group_col, term_col, ...)
}

#' @rdname calc_tf
calc_tf_ <- function(df, group_col, term_col, weight="ratio", k=0.5){
  loadNamespace("dplyr")
  loadNamespace("tidyr")

  calc_weight <- function(raw){
    if(weight=="ratio"){
      val <- raw/sum(raw)
    } else if(weight=="raw_frequency"){
      val <- raw
    } else if (weight=="binary"){
      val <- as.logical(raw)
    } else if (weight=="log_normalization"){
      val <- 1+log(raw)
    } else if (weight=="k_normalization"){
      val <- k + (1-k)*raw/max(raw)
    }
    else{
      stop(paste0(weight, " is not recognized as weight argument"))
    }
    val
  }
  ret <- (df[,colnames(df) == group_col | colnames(df)==term_col] %>%
            dplyr::group_by_(group_col, term_col) %>%
            dplyr::summarise(.count_per_doc = n()) %>%
            dplyr::mutate(.tf = calc_weight(.count_per_doc)) %>%
            dplyr::ungroup()
  )
}

#' Calculate tfidf, which shows how much particular the token is in a group.
#' @param df Data frame which has columns of groups and their terms
#' @param group Column of group names
#' @param term Column of terms
#' @param idf_log_scale
#' Function to scale IDF. It might be worth trying log2 or log10.
#' log10 strongly suppress the increase of idf values and log2 does it more weakly.
#' @export
calc_tfidf <- function(df, group, term, idf_log_scale = log, tf_weight="ratio", tf_k=0.5){
  loadNamespace("tidytext")
  loadNamespace("dplyr")
  group_col <- col_name(substitute(group))
  term_col <- col_name(substitute(term))
  count_tbl <- calc_tf_(df, group_col, term_col, weight=tf_weight, k=tf_k)
  tfidf <- calc_idf(count_tbl[[group_col]], count_tbl[[term_col]], log_scale = idf_log_scale, smooth_idf = FALSE)
  count_tbl$.count_of_docs <- tfidf$.df
  count_tbl$.tfidf <- tfidf$.idf * count_tbl$.tf
  count_tbl$.tf <- NULL
  count_tbl
}

#' Stem word so that words which are the same kind of word become the same charactor
#' @param language This can be "porter" or a kind of languages which use alphabet like "english" or "french".
#' @export
stem_word <- function(...){
  loadNamespace("quanteda")
  quanteda::wordstem(...)
}

#' Generate ngram in groups.
#' @param df Data frame which has tokens.
#' @param token Column name of token data.
#' @param n How many tokens should be together as new tokens. This should be numeric vector.
#' @param skip How many tokens can be skipped when connecting them.
#' @export
do_ngram <- function(df, token, n=1:2, skip=0){
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("quanteda")
  token_col <- col_name(substitute(token))

  indices <- attr(df, "indices")
  quanteda::ngrams
  if(is.null(indices)){
    # not grouped case
    skipgrams <- quanteda::skipgrams(as.character(df[[token_col]]), n=n, skip=skip)
    ret <- data.frame(skipgram=skipgrams, stringsAsFactors = F)
    colnames(ret) <- token_col
    ret
  } else {
    # grouped case
    labels <- attr(df, "labels")
    labels[[token_col]] <- lapply(indices, function(index){
      quanteda::skipgrams(as.character(df[[token_col]][index+1]), n=n, skip=skip)
    })
    tidyr::unnest_(labels, token_col)
  }
}
