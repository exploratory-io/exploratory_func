# how to run this test:
# devtools::test(filter="textanal")
context("test text analysis function, exp_textanal")

twitter_df <- exploratory::read_delim_file("https://www.dropbox.com/s/w1fh7j8iq6g36ry/Twitter_No_Spectator_Olympics_Ja.csv?dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)

nps_raw <- exploratory::read_delim_file("https://www.dropbox.com/scl/fi/7iur1jvyldqoxpieish2r/nps_raw.csv?rlkey=y3cwyosrplx6awt8wvwzut1ly&dl=1", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

nps_cluster <- exploratory::read_delim_file("https://www.dropbox.com/scl/fi/c6saij8if1iq76yfo2v0d/NPS_cluster.csv?rlkey=3lajklwltbe5tnijot1iujr7b&dl=1", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

Word_Size_Cluster <- exploratory::read_delim_file("https://www.dropbox.com/scl/fi/pjtl1qe8bnabsu8fqhbl4/Word_Size_Cluster.csv?rlkey=smi99mx36tn7c6khkl4x9yoel&dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)

Survey_English_raw_Tmp <- tempfile(fileext = ".rds")
download.file("https://www.dropbox.com/scl/fi/tk33gojc1la86zp3nwrms/Survey_text_data_english.rds?rlkey=w4t7doqg9cvnxgz24qhmk4u52&dl=1", destfile = Survey_English_raw_Tmp)
Survey_English_raw <- readRDS(Survey_English_raw_Tmp)

test_that("exp_textanal with Japanese twitter data", {
  lang_res <- exploratory:::guess_lang_for_stopwords(twitter_df$text)

  model_df <- twitter_df %>% exp_textanal(text, category=source, stopwords_lang = "japanese") # Testing both lower and upper case for compound_token.
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  res <- model_df %>% tidy_rowwise(model, type="words")
  expect_equal(colnames(res), c("document", "word"))
  res <- model_df %>% tidy_rowwise(model, type="word_pairs")

  # Test network clustering.
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="louvain")
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="leading_eigen")
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="fast_greedy")
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="spinglass")
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="none")
  expect_true(is.null(graph_data_res$model[[1]]$vertices$cluster))

  # Test with full parameters specified.
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(max_vertex_size=20, vertex_size_method='equal_length', vertex_opacity=0.6, max_edge_width=8, min_edge_width=1, edge_color='#EC5D57', font_size_ratio=1.2, area_factor=50, cluster_method='louvain')
})

test_that("exp_textanal with no-co-occurrence", {
  df <- tibble::tibble(text=c("Hello", "Hi", "world"))
  model_df <- df %>% exp_textanal(`text`, stopwords_lang = "auto", remove_punct = TRUE, remove_numbers = TRUE, remove_alphabets = FALSE, tokenize_tweets = FALSE, remove_url = TRUE, hiragana_word_length_to_remove = 2, cooccurrence_context = "window")
  res <- model_df %>% tidy_rowwise(model, type="word_pairs", max_word_pairs=30)
  expect_equal(colnames(res), c("word.1", "word.2", "count"))
  expect_equal(nrow(res), 0)
})


test_that("exp_textanal", {
  df <- tibble::tibble(text=c(
    "Jack and Jill went up the hill",
    "To fetch a pail of water",
    NA,
    "",
    "Jack fell down and broke his crown",
    "And Jill came tumbling after"))

  lang_res <- exploratory:::guess_lang_for_stopwords(df$text)

  model_df <- df %>% exp_textanal(text, stopwords_lang = "english", compound_tokens=c("Jack and jill"), stopwords_to_remove=c("And")) # Testing both lower and upper case for compound_token.
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  expect_true("Jack And Jill" %in% res$word)
  expect_true("And" %in% res$word) # Test stopwords_to_remove.
  res <- model_df %>% tidy_rowwise(model, type="words")
  expect_equal(colnames(res), c("document", "word"))
  expect_equal(length(unique(res$document)), 4) # NA and empty string are skipped.
  res <- model_df %>% tidy_rowwise(model, type="word_pairs")

  # Test network clustering.
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="louvain")
  expect_equal(length(graph_data_res$model[[1]]$vertices$cluster), 14)
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="leading_eigen")
  expect_equal(length(graph_data_res$model[[1]]$vertices$cluster), 14)
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="fast_greedy")
  expect_equal(length(graph_data_res$model[[1]]$vertices$cluster), 14)
  # Commented out due to this error with this toy data - Error: At clustertool.cpp:286 : Cannot work with unconnected graph, Invalid value
  # graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="spinglass")
  # expect_equal(length(graph_data_res$model[[1]]$vertices$cluster), 14)
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="none")
  expect_true(is.null(graph_data_res$model[[1]]$vertices$cluster))

  # Test for plotting
  # edges <- exploratory:::fcm_to_df(model_df$model[[1]]$fcm_selected) %>% rename(from=token.x,to=token.y) %>% filter(from!=to)
  # g <- igraph::graph.data.frame(edges, directed=FALSE)
  # c_scale <- grDevices::colorRamp(c("white","red"))
  # E(g)$color <- apply(c_scale((log(edges$value)+1)/max(log(edges$value)+1)), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255, alpha=0.8) )
  # plot(g, edge.arrow.size=0.5, layout=layout_with_graphopt, vertex.label.family="HiraKakuProN-W3", vertex.label.color=rgb(0.4,0.4,0.4), vertex.label.cex=0.9, vertex.frame.color=NA)
})



test_that("exp_get_top5_sentences_for_cluster", {
  df <- exp_get_top5_sentences_for_cluster(nps_raw, nps_cluster, "よりよくするための提案")
  expect_equal(nrow(df), 30)
  expect_equal(ncol(df), 2)
  expect_equal(df$cluster, c("1","1","1","1","1","2","2","2","2","2","3","3","3","3","3","4","4","4","4","4","5","5","5","5","5","6","6","6","6","6"))
  expect_equal(df$`よりよくするための提案`[[3]], c("発表者を減らして1人あたりの時間がもう少し余裕があってもいいかと思います。"))
})

test_that("exp_get_top5_sentences_for_cluster", {
  model_df <- nps_raw %>% exp_textanal(`よりよくするための提案`, stopwords_lang = "auto", remove_punct = TRUE, remove_numbers = TRUE, remove_alphabets = FALSE, tokenize_tweets = FALSE, remove_url = TRUE, hiragana_word_length_to_remove = 2, cooccurrence_context = "window")
  cluster_keywords_df <- model_df %>%
    get_cooccurrence_graph_data(
      max_vertex_size=20,
      vertex_size_method='equal_length',
      vertex_opacity=0.6,
      max_edge_width=8,
      min_edge_width=1,
      edge_color='#4A90E2',
      font_size_ratio=1.2,
      area_factor=50,
      cluster_method='louvain') %>%
    (function(df) { df$model[[1]]$vertices }) %>%
    arrange(cluster, dplyr::desc(size))

  df <- exp_add_cluster_column(model_df$model[[1]]$df, cluster_keywords_df, "よりよくするための提案")

  expect_equal(nrow(df), 164)
  expect_equal(ncol(df), 5)
  expect_equal(df$`よりよくするための提案`[[3]], c("特にはないですが、もっとアップデート内容を聞きたかったです！ みなさんの発表も参考になりました。"))
})

test_that("exp_get_top5_sentences_for_cluster: English", {
  df <- exp_get_top5_sentences_for_cluster(Survey_English_raw, Word_Size_Cluster, "what would it be? (Multiple answers are acceptable)")
  expect_equal(nrow(df), 30)
  expect_equal(ncol(df), 2)
  expect_equal(df$cluster, c("1","1","1","1","1","2","2","2","2","2","3","3","3","3","3","4","4","4","4","4","5","5","5","5","5","6","6","6","6","6"))
  expect_equal(df$`what would it be? (Multiple answers are acceptable)`[[1]], c("I think it would be better to put some restrictions on the presentation time and the number of presentation slides so that presentation finish on time."))
})
