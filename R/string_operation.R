#' Check if the token is in stopwords.
#' @param token Character to be checked if it's stopword.
#' @param lexicon Type of stopwords.
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
is_stopword <- function(token, lang = "english", include = c(), exclude = c()){
  token %in% get_stopwords(lang, include = include, exclude = exclude)
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
  stringr::str_detect(word, "^[[:alpha:]]+$")
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
#' @return vector of stop words.
#' @export
get_stopwords <- function(lang = "english", include = c(), exclude = c()){
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
do_tokenize <- function(df, input, output=token, token="words", drop=TRUE, with_id=TRUE, ...){
  loadNamespace("tidytext")
  loadNamespace("stringr")

  input_col <- col_name(substitute(input))
  output_col <- avoid_conflict(colnames(df), col_name(substitute(output)))
  # This is to prevent encoding error
  df[[input_col]] <- stringr::str_conv(df[[input_col]], "utf-8")
  if(token=="words" && with_id){
    loadNamespace("dplyr")

    # split into sentences
    func <- get("unnest_tokens_", asNamespace("tidytext"))
    doc_id <- avoid_conflict(colnames(df), "document_id")
    df <- dplyr::mutate_(df, .dots=setNames(list(~row_number()),doc_id))
    tokenize_df <- df[,c(doc_id, input_col)]
    sentences <- tidytext::unnest_tokens_(tokenize_df, output_col, input_col, token="sentences", drop=TRUE, ...)
    grouped <- dplyr::group_by_(sentences, .dots=list( as.symbol(doc_id))) # convert the column name to symbol for colum names with backticks

    sentence_id <- avoid_conflict(colnames(df), "sentence_id")

    # split into tokens
    tokenize_df <- dplyr::mutate_(grouped, .dots=setNames(list(~row_number()), sentence_id))
    tokenize_df <- dplyr::ungroup(tokenize_df)
    tokenized <- tidytext::unnest_tokens_(tokenize_df, output_col, output_col, token="words", drop=TRUE, ...)

    if(drop){
      df[[input_col]] <- NULL
    }

    dplyr::right_join(df, tokenized, by=doc_id)
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

  weight_fml <- as.formula(paste("~calc_weight(",cnames[[1]],")", sep=""))

  ret <- (df[,colnames(df) == group_col | colnames(df)==term_col] %>%
            dplyr::group_by_(.dots=list(as.symbol(group_col), as.symbol(term_col))) %>% # convert the column name to symbol for colum names with backticks
            dplyr::summarise_(.dots=setNames(list(~n()), cnames[[1]])) %>%
            dplyr::mutate_(.dots=setNames(list(weight_fml), cnames[[2]])) %>%
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
do_tfidf <- function(df, group, term, idf_log_scale = log, tf_weight="raw", norm="l2"){
  loadNamespace("tidytext")
  loadNamespace("dplyr")

  if(!(norm %in% c("l1", "l2") | norm == FALSE)){
    stop("norm argument must be l1, l2 or FALSE")
  }

  group_col <- col_name(substitute(group))
  term_col <- col_name(substitute(term))

  # remove NA from group and term column to avoid error
  df <- tidyr::drop_na_(df, c(group_col, term_col))

  cnames <- avoid_conflict(c(group_col, term_col), c("count_of_docs", "tfidf", "tf"))

  count_tbl <- calc_tf_(df, group_col, term_col, weight=tf_weight)
  tfidf <- calc_idf(count_tbl[[group_col]], count_tbl[[term_col]], log_scale = idf_log_scale, smooth_idf = FALSE)
  count_tbl[[cnames[[1]]]] <- tfidf$.df
  count_tbl[[cnames[[2]]]] <- tfidf$.idf * count_tbl[[cnames[[3]]]]
  count_tbl[[cnames[[3]]]] <- NULL

  if(norm == "l2"){
    val <- lazyeval::interp(~x/sqrt(sum(x^2)), x=as.symbol(cnames[[2]]))
    count_tbl <- (count_tbl %>%
      dplyr::group_by_(.dots=list(as.symbol(group_col))) %>% # convert the column name to symbol for colum names with backticks
      dplyr::mutate_(.dots=setNames(list(val), cnames[[2]])) %>%
      dplyr::ungroup())
  } else if(norm == "l1"){
    val <- lazyeval::interp(~x/sum(x), x=as.symbol(cnames[[2]]))
    count_tbl <- (count_tbl %>%
      dplyr::group_by_(.dots=list(as.symbol(group_col))) %>% # convert the column name to symbol for colum names with backticks
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
  quanteda::wordstem(...)
}

#' Generate ngram in groups.
#' @param df Data frame which has tokens.
#' @param token Column name of token data.
#' @param n How many tokens should be together as new tokens. This should be numeric vector.
#' @export
do_ngram <- function(df, token, sentence, document, maxn=2, sep="_"){
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("stringr")
  loadNamespace("lazyeval")
  token_col <- col_name(substitute(token))
  sentence_col <- col_name(substitute(sentence))
  document_col <- col_name(substitute(document))

  # this is executed for ngrams not to be connected over sentences
  grouped <- df %>%
    dplyr::group_by_(.dots=list(as.symbol(document_col), as.symbol(sentence_col))) # convert the column name to symbol for colum names with backticks
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
  ret <- dplyr::arrange_(ret, .dots = c(document_col, sentence_col, kv_cnames[[1]]))
  ret
}

#' Calculate sentiment
#' @export
get_sentiment <- function(text){
  loadNamespace("sentimentr")
  sentimentr::sentiment_by(text)$ave_sentiment
}
