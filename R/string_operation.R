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

