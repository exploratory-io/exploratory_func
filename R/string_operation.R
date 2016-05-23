#' @param type Type of stopwords. One of "snowball", "onix" and "SMART".
#' @param stem Stem function or "porter" if you want to stem the stopwords.
#' @return vector of stop word.
#' @export
get_stopwords <- function(type="snowball", stem=NULL){
  loadNamespace("tidytext")
  data("stop_words", package = "tidytext", envir = environment())
  type_check <- stop_words$lexicon %in% type
  words <- stop_words$word[type_check]
  if(!is.null(stem)){
    if(stem == "porter"){
      loadNamespace("quanteda")
      words <- quanteda::wordstem(words)
    } else {
      words <- stem(words)
    }
  }
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

