#### Generate sentiment data
library(exploratory)

data("sentiments", package = "tidytext", envir = environment())
data("stop_words", package = "tidytext", envir = environment())

# only this uses summarize because nrc has many sentiment types, so list is used for one to many relationships among words and sentiments
# the other two is one to one relationships, so named vector can be used
sentiment_nrc_df <- sentiments %>% dplyr::filter(lexicon == "nrc") %>% dplyr::group_by(word) %>% dplyr::summarize(sentiment=list(sentiment))
sentiment_nrc <- sentiment_nrc_df[["sentiment"]]
names(sentiment_nrc) <- sentiment_nrc_df[["word"]]

sentiment_bing_df <- sentiments %>% dplyr::filter(lexicon == "bing")
sentiment_bing <- sentiment_bing_df[["sentiment"]]
names(sentiment_bing) <- sentiment_bing_df[["word"]]

sentiment_AFINN_df <- sentiments %>% dplyr::filter(lexicon == "AFINN")
sentiment_AFINN <- sentiment_AFINN_df[["score"]]
names(sentiment_AFINN) <- sentiment_AFINN_df[["word"]]

exploratory_stopwords <- c("http", "https", "t.co", "amp")
default_stopwords = c(tm::stopwords("english"), exploratory_stopwords)
stopwords_english <- default_stopwords
res <- httr::GET("http://svn.sourceforge.jp/svnroot/slothlib/CSharp/Version1/SlothLib/NLP/Filter/StopWord/word/Japanese.txt")
stopwords_japanese <- httr::content(res) %>% stringr::str_split("\r\n")
# ja_stopwrods is a list whose length is 1
stopwords_japanese <- stopwords_japanese[[1]][!is_empty(stopwords_japanese[[1]])]
stopwords_english_smart <- readRDS("data-raw/stopwords_smart.rds")
stopwords_english_onix <- readRDS("data-raw/stopwords_onix.rds")
stopwords_english_snowball <- readRDS("data-raw/stopwords_snowball.rds")

if(all(!stopwords_japanese %in% c("あなた", "いくつ", "いろいろ", "おまえ"))){
  stop("stopwords_japanese seems strange")
}

devtools::use_data(
  sentiment_nrc,
  sentiment_bing,
  sentiment_AFINN,
  default_stopwords,
  exploratory_stopwords,
  stopwords_japanese,
  stopwords_english,
  stopwords_english_smart,
  stopwords_english_onix,
  stopwords_english_snowball,
  internal = TRUE,
  overwrite = TRUE)
