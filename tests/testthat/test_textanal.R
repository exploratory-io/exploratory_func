# how to run this test:
# devtools::test(filter="textanal")
context("test text analysis function, exp_textanal")

test_that("exp_textanal", {
  df <- tibble::tibble(text=c("すもももももももものうち", "隣の客はよく柿食う客だ。", "隣の客はよく柿食う客だ。", "赤巻紙青巻紙黄巻紙"))

  browser()
  lang_res <- guess_lang_for_stopwords(df$text)
  browser()

  model_df <- df %>% exp_textanal(text, compound_tokens=c("赤 巻紙"))
  browser()
  res <- model_df %>% tidy_rowwise(model, type="word_count")
  res <- model_df %>% tidy_rowwise(model, type="word_pairs")
  browser()

  # Test for plotting
  edges <- exploratory:::fcm_to_df(model_df$model[[1]]$fcm_selected) %>% rename(from=token.x,to=token.y) %>% filter(from!=to)
  g <- igraph::graph.data.frame(edges, directed=FALSE)
  c_scale <- grDevices::colorRamp(c("white","red"))
  E(g)$color <- apply(c_scale((log(edges$value)+1)/max(log(edges$value)+1)), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255, alpha=0.8) )
  plot(g, edge.arrow.size=0.5, layout=layout_with_graphopt, vertex.label.family="HiraKakuProN-W3", vertex.label.color=rgb(0.4,0.4,0.4), vertex.label.cex=0.9, vertex.frame.color=NA)
})
