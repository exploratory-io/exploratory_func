#### source() this file to generate internal stopwords .rda files.
#### Copied from create_internal_data.R which uses old devtools::use_data, which writes to R/sysdata.rda.
#### This script writes to data/*.rda, and it seems to override content of R/sysdata.rda.

library(dplyr)
library(exploratory)

exploratory_stopwords <- c("http", "https", "t.co", "amp")
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

usethis::use_data(
  exploratory_stopwords,
  stopwords_japanese,
  stopwords_english_smart,
  stopwords_english_onix,
  stopwords_english_snowball,
  overwrite = TRUE)
