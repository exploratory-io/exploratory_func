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
#' @param input Input column name
#' @param output Output column name
#' @return Data frame with tokenized column
#' @export
do_tokenize <- function(df, input, output=.token, ...){
  loadNamespace("tidytext")
  tidytext::unnest_tokens_(df, col_name(substitute(output)), col_name(substitute(input)), ...)
}

#' Get idf for terms
calc_idf <- function(document, term, log_scale = log, smooth_idf = FALSE){
  loadNamespace("Matrix")
  loadNamespace("text2vec")
  if(length(document)!=length(term)){
    stop("length of document and terms have to be the same")
  }
  doc_fact <- as.factor(document)
  term_fact <- as.factor(term)
  sparseMat <- Matrix::sparseMatrix(i = as.numeric(doc_fact), j = as.numeric(term_fact))
  idf <- text2vec::get_idf(sparseMat, log_scale=log_scale, smooth_idf=smooth_idf)
  idf <- idf@x[term_fact]
  df <- Matrix::colSums(sparseMat)[term_fact]
  data.frame(.df=df, .idf=idf)
}

#' Calculate term frequency
#' @param tbl Data frame
#' @param document Column to be considered as a document id
#' @param term Column to be considered as term
#' @param weight Type of weight calculation.
#' "ratio" is default and it's count/(total number of terms in the document).
#' This can be "raw_frequency", "binary", "log_normalization" and "k_normalization"
#' "raw_frequency" is count of the term in the document.
#' "binary" is logic if the term is in the document or not.
#' "log_normalization" is logic if the term is in the document or not.
#' @param term Column to be considered as term
#' @return Data frame with document, term and .tf column
#' @export
calc_tf <- function(tbl, document, term, ...){
  document_col <- col_name(substitute(document))
  term_col <- col_name(substitute(term))
  calc_tf_(tbl, document_col, term_col, ...)
}

#' Caluculate tf for standard evaluation
calc_tf_ <- function(tbl, document_col, term_col, weight="ratio", k=0.5){
  loadNamespace("lazyeval")
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  culc_weight <- function(df){
    raw <- df$.tf
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

    output <- data.frame(.tf = val)
    output[[term_col]] <- df[[term_col]]
    output
  }
  count <- (
    tbl[,colnames(tbl) == document_col | colnames(tbl)==term_col] %>%
    dplyr::group_by_(document_col, term_col) %>%
    dplyr::summarise(.tf = n()) %>%
    dplyr::do(.tf = culc_weight(.)) %>%
    tidyr::unnest(.tf)
    )

  dplyr::ungroup(count)
}

#' Caluculate tfidf
calc_tfidf <- function(tbl, document, term, idf_log_scale = log, smooth_idf = FALSE, tf_weight="ratio", tf_k=0.5){
  loadNamespace("tidytext")
  loadNamespace("dplyr")
  document_col <- col_name(substitute(document))
  term_col <- col_name(substitute(term))
  count_tbl <- calc_tf_(tbl, document_col, term_col, weight=tf_weight, k=tf_k)
  mat <- tidytext::cast_sparse_(count_tbl, document_col, term_col, ".tf")
  tfidf <- calc_idf(count_tbl[[document_col]], count_tbl[[term_col]], log_scale = idf_log_scale, smooth_idf = smooth_idf)
  count_tbl$.df <- tfidf$.df
  count_tbl$.tfidf <- tfidf$.idf * count_tbl$.tf
  count_tbl
}
