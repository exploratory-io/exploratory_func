# how to run this test:
# devtools::test(filter="textanal")
context("test text analysis function, exp_textanal")

twitter_df <- exploratory::read_delim_file("https://www.dropbox.com/s/w1fh7j8iq6g36ry/Twitter_No_Spectator_Olympics_Ja.csv?dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)

nps_raw <- exploratory::read_delim_file("https://www.dropbox.com/scl/fi/7iur1jvyldqoxpieish2r/nps_raw.csv?rlkey=y3cwyosrplx6awt8wvwzut1ly&dl=1", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

nps_cluster <- exploratory::read_delim_file("https://www.dropbox.com/scl/fi/c6saij8if1iq76yfo2v0d/NPS_cluster.csv?rlkey=3lajklwltbe5tnijot1iujr7b&dl=1", ",", quote = "\"", skip = 0 , col_names = TRUE , na = c("","NA") , locale=readr::locale(encoding = "UTF-8", decimal_mark = "."), trim_ws = FALSE , progress = FALSE) %>% exploratory::clean_data_frame()

Word_Size_Cluster <- exploratory::read_delim_file("https://www.dropbox.com/scl/fi/pjtl1qe8bnabsu8fqhbl4/Word_Size_Cluster.csv?rlkey=smi99mx36tn7c6khkl4x9yoel&dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)

Survey_English_raw <- exploratory::read_delim_file("https://www.dropbox.com/scl/fi/u8utgfwpmyw9nf8h6qkhe/Survey_English_raw.csv?rlkey=or2qe03a0vidbx7rpxps62o3m&dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)


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
  # Words are kept in lowercase (compound tokens are also lowercase)
  expect_true("jack and jill" %in% res$word)
  expect_true("and" %in% res$word) # Test stopwords_to_remove.
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

test_that("exp_textanal Pattern B2: word column without document_id", {
  # Pattern B2: Each row is a document, word column contains comma-separated tokens
  df <- tibble::tibble(
    reason = c(
      "端末にインストールしなくても、使えるので、導入障壁が低い。",
      "外出先で取引先と会議をすることになったが、社内にいるときと変わらないクオリティで使えた。",
      "評価をするための時間がなかったが、数分でセットアップを終えてすぐに使い始めることができた。"
    ),
    word = c(
      "端末,インストール,使える,導入,障壁,低い",
      "外出先,取引先,会議,社内,変わらない,クオリティ,使えた",
      "評価,時間,数分,セットアップ,終え,すぐに,使い始める"
    )
  )

  model_df <- df %>% exp_textanal(word = word, stopwords_lang = "japanese")
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  expect_true("端末" %in% res$word)
  expect_true("インストール" %in% res$word)
  expect_true("会議" %in% res$word)

  res <- model_df %>% tidy_rowwise(model, type="words")
  expect_equal(colnames(res), c("document", "word"))
  expect_equal(length(unique(res$document)), 3) # Three documents

  res <- model_df %>% tidy_rowwise(model, type="word_pairs")
  expect_equal(colnames(res), c("word.1", "word.2", "count"))
})

test_that("exp_textanal Pattern B2: word column with whitespace", {
  # Test whitespace handling in comma-separated values
  df <- tibble::tibble(
    word = c(
      "word1, word2, word3",
      "word4,  word5,  word6",
      "word7,word8,word9"
    )
  )

  model_df <- df %>% exp_textanal(word = word, stopwords_lang = "english")
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  # Words are kept in lowercase
  expect_true("word1" %in% res$word)
  expect_true("word2" %in% res$word)
  expect_true("word5" %in% res$word)
  expect_equal(length(unique(res$word)), 9) # All 9 words should be present
})

test_that("exp_textanal Pattern B2: word column with empty strings and NA", {
  # Test edge cases: empty strings and NA
  df <- tibble::tibble(
    word = c(
      "word1, word2",
      "",
      NA,
      "word3, word4"
    )
  )

  model_df <- df %>% exp_textanal(word = word, stopwords_lang = "english")
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  # Words are kept in lowercase
  expect_true("word1" %in% res$word)
  expect_true("word3" %in% res$word)

  res <- model_df %>% tidy_rowwise(model, type="words")
  # Empty string and NA rows should be filtered out
  expect_true(length(unique(res$document)) <= 2)
})

test_that("exp_textanal Pattern B1: word column with document_id", {
  # Pattern B1: word column with document_id provided
  df <- tibble::tibble(
    doc_id = c("A", "A", "B", "B", "C"),
    word = c(
      "端末,インストール,使える",
      "導入,障壁,低い",
      "外出先,取引先,会議",
      "社内,変わらない,クオリティ",
      "評価,時間,数分"
    ),
    category = c("cat1", "cat1", "cat2", "cat2", "cat1")
  )

  model_df <- df %>% exp_textanal(word = word, document_id = doc_id, category = category, stopwords_lang = "japanese")
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  expect_true("端末" %in% res$word)
  expect_true("導入" %in% res$word)
  expect_true("外出先" %in% res$word)

  res <- model_df %>% tidy_rowwise(model, type="words")
  expect_equal(colnames(res), c("document", "word"))
  # Documents A and B have multiple rows, so they should be grouped
  expect_true(length(unique(res$document)) <= 3)

  # Test category_word_count
  res <- model_df %>% tidy_rowwise(model, type="category_word_count")
  expect_true("category" %in% colnames(res))
})

test_that("exp_textanal Pattern B1: multiple rows per document_id", {
  # Test that multiple rows with same document_id are aggregated
  df <- tibble::tibble(
    doc_id = c("A", "A", "A"),
    word = c("word1, word2", "word3, word4", "word5")
  )

  model_df <- df %>% exp_textanal(word = word, document_id = doc_id, stopwords_lang = "english")
  res <- model_df %>% tidy_rowwise(model, type="words")

  # All words from document A should be present
  # Words are kept in lowercase
  doc_a_words <- res %>% dplyr::filter(document == "A") %>% dplyr::pull(word)
  expect_true("word1" %in% doc_a_words)
  expect_true("word2" %in% doc_a_words)
  expect_true("word3" %in% doc_a_words)
  expect_true("word4" %in% doc_a_words)
  expect_true("word5" %in% doc_a_words)
})

test_that("exp_textanal Pattern B: stopword removal", {
  # Test stopword removal for Pattern B
  df <- tibble::tibble(
    word = c("the, quick, brown, fox", "jumps, over, the, lazy, dog")
  )

  model_df <- df %>% exp_textanal(word = word, stopwords_lang = "english")
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  # Common English stopwords like "the" should be removed
  expect_false("the" %in% res$word)
})

test_that("exp_textanal Pattern B: validation errors", {
  # Test validation: both text and word provided
  df <- tibble::tibble(
    text = c("some text"),
    word = c("word1, word2")
  )

  expect_error(
    df %>% exp_textanal(text = text, word = word),
    "Cannot specify both 'text' and 'word' arguments"
  )

  # Test validation: neither text nor word provided
  df2 <- tibble::tibble(other_col = c("value"))
  expect_error(
    df2 %>% exp_textanal(),
    "Either 'text' or 'word' must be provided"
  )
})

test_that("exp_textanal Pattern B: backward compatibility with Pattern A", {
  # Ensure Pattern A (text column) still works as before
  df <- tibble::tibble(text=c(
    "Jack and Jill went up the hill",
    "To fetch a pail of water"
  ))

  model_df <- df %>% exp_textanal(text = text, stopwords_lang = "english")
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  # Words are kept in lowercase (tokenize_with_postprocess lowercases them)
  expect_true("jack" %in% res$word)
  expect_true("jill" %in% res$word)
  expect_true("hill" %in% res$word)
})
