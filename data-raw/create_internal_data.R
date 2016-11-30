#### Generate sentiment data

data("sentiments", package = "tidytext", envir = environment())

# only this uses summarize because nrc has many sentiment types, so list is used for one to many relationships among words and sentiments
# the other two is one to one relationships, so named vector can be used
sentiment_nrc_df <- sentiments %>% filter(lexicon == "nrc") %>% group_by(word) %>% dplyr::summarize(sentiment=list(sentiment))
sentiment_nrc <- sentiment_nrc_df[["sentiment"]]
names(sentiment_nrc) <- sentiment_nrc_df[["word"]]

sentiment_bing_df <- sentiments %>% filter(lexicon == "bing")
sentiment_bing <- sentiment_bing_df[["sentiment"]]
names(sentiment_bing) <- sentiment_bing_df[["word"]]

sentiment_AFINN_df <- sentiments %>% filter(lexicon == "AFINN")
sentiment_AFINN <- sentiment_AFINN_df[["score"]]
names(sentiment_AFINN) <- sentiment_AFINN_df[["word"]]

devtools::use_data(sentiment_nrc, sentiment_bing, sentiment_AFINN, internal = TRUE)#
