# how to run this test:
# devtools::test(filter="textanal")
context("test text analysis function, exp_textanal")

twitter_df <- exploratory::read_delim_file("https://www.dropbox.com/s/w1fh7j8iq6g36ry/Twitter_No_Spectator_Olympics_Ja.csv?dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)

test_that("exp_textanal with Japanese twitter data", {
  lang_res <- exploratory:::guess_lang_for_stopwords(twitter_df$text)

  model_df <- twitter_df %>% exp_textanal(text, stopwords_lang = "japanese") # Testing both lower and upper case for compound_token.
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  res <- model_df %>% tidy_rowwise(model, type="word_count", category_col="source")
  res <- model_df %>% tidy_rowwise(model, type="words")
  expect_equal(colnames(res), c("document", "word"))
  res <- model_df %>% tidy_rowwise(model, type="word_pairs")

  # Test network clustering.
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="louvain")
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="leading_eigen")
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="fast_greedy")
  # Commented out due to this error with this toy data - Error: At clustertool.cpp:286 : Cannot work with unconnected graph, Invalid value
  # graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="spinglass")
  # expect_equal(length(graph_data_res$model[[1]]$vertices$cluster), 14)
  graph_data_res <- model_df %>% get_cooccurrence_graph_data(cluster_method="none")
  expect_true(is.null(graph_data_res$model[[1]]$vertices$cluster))
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
  expect_equal(length(unique(res$document)), 5) # NA should be filtered, but empty string should be kept.
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
