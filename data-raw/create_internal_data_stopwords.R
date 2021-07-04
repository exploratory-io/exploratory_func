#### source() this file to generate internal stopwords .rda files.
#### Copied from create_internal_data.R which uses old devtools::use_data, which writes to R/sysdata.rda.
#### This script writes to data/*.rda, and it seems to override content of R/sysdata.rda.

library(dplyr)
library(exploratory)

exploratory_stopwords <- c("http", "https", "t.co", "amp")
# stopwords_japanese.txt is based on http://svn.sourceforge.jp/svnroot/slothlib/CSharp/Version1/SlothLib/NLP/Filter/StopWord/word/Japanese.txt.
# We removed many words that we thought shoulb be kept outside of stopwords from there.
stopwords_japanese <- read.table("data-raw/stopwords_japanese.txt")$V1
stopwords_english_smart <- readRDS("data-raw/stopwords_smart.rds")
stopwords_english_onix <- readRDS("data-raw/stopwords_onix.rds")
stopwords_english_snowball <- readRDS("data-raw/stopwords_snowball.rds")

usethis::use_data(
  exploratory_stopwords,
  stopwords_japanese,
  stopwords_english_smart,
  stopwords_english_onix,
  stopwords_english_snowball,
  overwrite = TRUE)
