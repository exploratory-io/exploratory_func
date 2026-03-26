# how to run this test:
# devtools::test(filter="textanal")
context("test text analysis function, exp_textanal")

twitter_df <- exploratory::read_delim_file("https://www.dropbox.com/s/w1fh7j8iq6g36ry/Twitter_No_Spectator_Olympics_Ja.csv?dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)

nps_raw <- exploratory::read_delim_file("https://www.dropbox.com/scl/fi/7iur1jvyldqoxpieish2r/nps_raw.csv?rlkey=y3cwyosrplx6awt8wvwzut1ly&dl=1", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

nps_cluster <- exploratory::read_delim_file("https://www.dropbox.com/scl/fi/c6saij8if1iq76yfo2v0d/NPS_cluster.csv?rlkey=3lajklwltbe5tnijot1iujr7b&dl=1", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

Word_Size_Cluster <- exploratory::read_delim_file("https://www.dropbox.com/scl/fi/pjtl1qe8bnabsu8fqhbl4/Word_Size_Cluster.csv?rlkey=smi99mx36tn7c6khkl4x9yoel&dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)

Survey_English_raw <- exploratory::read_delim_file("https://www.dropbox.com/scl/fi/u8utgfwpmyw9nf8h6qkhe/Survey_English_raw.csv?rlkey=or2qe03a0vidbx7rpxps62o3m&dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)



























