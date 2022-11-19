# how to run this test:
# devtools::test(filter="textanal")
context("test topic model function, exp_topic_model")
twitter_df <- exploratory::read_delim_file("https://www.dropbox.com/s/w1fh7j8iq6g36ry/Twitter_No_Spectator_Olympics_Ja.csv?dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)

test_that("exp_topic_model with Japanese twitter data", {
  model_df <- twitter_df %>% exp_topic_model(text, category=source, stopwords_lang = "japanese", max_nrow = 5000)
  # For doc_topics_tagged tidier to work, column order of topics must be sorted.
  expect_equal(colnames(model_df$model[[1]]$doc_word_df), c("document","word","topic1","topic2","topic3"))
  expect_equal(colnames(model_df$model[[1]]$words_topics_df), c("word","topic1","topic2","topic3"))
  res <- model_df %>% tidy_rowwise(model, type="doc_topics_tagged", word_topic_probability_threshold=0.4)
  # Testing how the result is processed by the Analytics View. #TODO: When it's settled, we might want to move it from the Desktop to here.
  # Make sure that factor levels set on document id is sorted by top topic. 
  # The first level should be an doc ID whose top topic is topic 1.
  res2 <- res %>% dplyr::slice_max(topic_max, n=5000) %>% dplyr::mutate(ID = seq(n())) %>% dplyr::select(ID, tagged_text, matches('^topic[0-9]+')) %>% tidyr::pivot_longer(names_to='Topics', values_to='Proportion', matches('^topic[0-9]+$')) %>% dplyr::mutate(Topics=stringr::str_extract(Topics,'[0-9]+')) %>% dplyr::mutate(Topics=as.numeric(stringr::str_extract(Topics,'[0-9]+'))) %>% dplyr::mutate(ID=factor(ID), ID=forcats::fct_reorder2(ID, Topics, Proportion, .fun=function(x,y){max(y) - 10*x[which.max(y)]}, .desc=TRUE)) %>% dplyr::rename(Text=tagged_text)
  topic_1_top_doc_id <- levels(res2$ID)[[1]]
  expect_equal((res2 %>% dplyr::filter(ID==!!topic_1_top_doc_id) %>% dplyr::arrange(desc(Proportion)))$Topics[[1]], 1)

  res <- model_df %>% tidy_rowwise(model, type="topics_summary")
  expect_equal(colnames(res), c("topic", "n"))
  expect_equal(sum(res$n), 5000)
  # Ensure that topics are named according to the frequency to make sure topic name mapping on doc_df is correct.
  expect_equal((res %>% dplyr::arrange(desc(n)))$topic, c(1,2,3))
  doc_topics_res <- model_df %>% tidy_rowwise(model, type="doc_topics")
  # Ensure the max_topic values from model$doc_df make sense, after the topic name mapping. slice is there to avoid the case with ties,
  # in which case the result can differ because of the randomness.
  test_res <- (doc_topics_res %>% slice(1:70) %>% dplyr::mutate(max_topic_2 = summarize_row(across(starts_with("topic")), which.max.safe)) %>%
               mutate(test_res = max_topic==max_topic_2))$test_res
  expect_true(sum(test_res)/length(test_res) > 0.9) # Give room for randomness for tie
  expect_equal(colnames(doc_topics_res), c("created_at", "screen_name", "text", "source", "topic1", "topic2", "topic3", "max_topic", "topic_max"))
  expect_equal(nrow(doc_topics_res), 5000) # NA should be filtered, but empty string should be kept.

  doc_word_res <- model_df %>% tidy_rowwise(model, type="doc_word_category")
  # Ensure that the document max topic from model$doc_df joined with doc_word_df corresponds with the one from model$doc_df.
  expect_true(all((doc_word_res %>% group_by(document) %>% summarise(document_max_topic=first(document_max_topic)))$document_max_topic == doc_topics_res$max_topic))

  res <- model_df %>% tidy_rowwise(model, type="topic_words")
  expect_equal(colnames(res), c("word", "topic", "probability"))
  res <- model_df %>% tidy_rowwise(model, type="word_topics")
  expect_equal(colnames(res), c("word", "topic1", "topic2", "topic3", "max_topic", "topic_max"))
})

test_that("exp_topic_model", {
  df <- tibble::tibble(text=c(
    "Jack and Jill went up the hill",
    "To fetch a pail of water",
    NA,
    "",
    "Jack fell down and broke his crown",
    "And Jill came tumbling after"))

  model_df <- df %>% exp_topic_model(text, stopwords_lang = "english", compound_tokens=c("Jack and jill")) # Testing both lower and upper case for compound_token.
  res <- model_df %>% tidy_rowwise(model, type="topics_summary")
  expect_equal(colnames(res), c("topic", "n"))
  res <- model_df %>% tidy_rowwise(model, type="doc_topics")
  expect_equal(colnames(res), c("text", "topic1", "topic2", "topic3", "max_topic", "topic_max"))
  expect_equal(nrow(res), 5) # NA should be filtered, but empty string should be kept.
  res <- model_df %>% tidy_rowwise(model, type="topic_words")
  expect_equal(colnames(res), c("word", "topic", "probability"))
  res <- model_df %>% tidy_rowwise(model, type="word_topics")
  expect_equal(colnames(res), c("word", "topic1", "topic2", "topic3", "max_topic", "topic_max"))
})

test_that("tidy.textmodel_lda_exploratory to complete topic with no document belonging to it.", {
  # Create a dummy model_df. We haven't seen this situation from real data yet.
  model <- list(k=2)
  doc_df <- tibble::tibble(max_topic=c(1,1))
  x <- list(model=model, doc_df=doc_df)
  class(x) <- "textmodel_lda_exploratory"
  model_df <- tibble::tibble(model = list(x))
  res <- model_df %>% tidy_rowwise(model, type="topics_summary")
  expect_equal(res$topic, c(1,2))
  expect_equal(res$n, c(2,0))
})

test_that("exp_topic_model with pre-parsed data", {
  df <- tibble::tibble(word=c(
    "Jack", "and", "Jill", "went", "up", "the", "hill",
    "To", "fetch", "a", "pail", "of", "water",
    NA,
    "",
    "Jack", "fell", "down", "and", "broke", "his", "crown",
    "And", "Jill", "came", "tumbling", "after"),
    doc = c(rep("one",13), rep("two",1), rep("three",1), rep("four", 12)))
  model_df <- df %>% exp_topic_model(text=NULL, word=word, document_id=doc)
})

test_that("exp_topic_model with pre-parsed data with category", {
  df <- tibble::tibble(word=c(
    "Jack", "and", "Jill", "went", "up", "the", "hill",
    "To", "fetch", "a", "pail", "of", "water",
    NA,
    "",
    "Jack", "fell", "down", "and", "broke", "his", "crown",
    "And", "Jill", "came", "tumbling", "after"),
    doc = c(rep("one",13), rep("two",1), rep("three",1), rep("four", 12)),
    cat = c(rep("A",13), rep("A",1), rep("B",1), rep("B", 12))
  )
  model_df <- df %>% exp_topic_model(text=NULL, word=word, document_id=doc, category=cat)
  # For doc_topics_tagged tidier to work, column order of topics must be sorted.
  expect_equal(colnames(model_df$model[[1]]$doc_word_df), c("document","word","topic1","topic2","topic3"))
  expect_equal(colnames(model_df$model[[1]]$words_topics_df), c("word","topic1","topic2","topic3"))

  res <- model_df %>% tidy_rowwise(model, type="topics_summary")
  expect_equal(colnames(res), c("topic", "n"))
  expect_equal(sum(res$n), 3)
  # Ensure that topics are named according to the frequency to make sure topic name mapping on doc_df is correct.
  expect_equal((res %>% dplyr::arrange(desc(n)))$topic, c(1,2,3))
  doc_topics_res <- model_df %>% tidy_rowwise(model, type="doc_topics")
  # Ensure the max_topic values from model$doc_df make sense, after the topic name mapping. slice is there to avoid the case with ties,
  # in which case the result can differ because of the randomness.
  test_res <- (doc_topics_res %>% slice(1:70) %>% dplyr::mutate(max_topic_2 = summarize_row(across(starts_with("topic")), which.max.safe)) %>%
               mutate(test_res = max_topic==max_topic_2))$test_res
  expect_true(sum(test_res)/length(test_res) > 0.9) # Give room for randomness for tie
  expect_equal(colnames(doc_topics_res), c("doc", "tokens", "cat", "topic1", "topic2", "topic3", "max_topic", "topic_max"))
  expect_equal(nrow(doc_topics_res), 3) # NA should be filtered, but empty string should be kept.

  doc_word_res <- model_df %>% tidy_rowwise(model, type="doc_word_category")
  # Ensure that the document max topic from model$doc_df joined with doc_word_df corresponds with the one from model$doc_df.
  expect_true(all((doc_word_res %>% group_by(document) %>% summarise(document_max_topic=first(document_max_topic)))$document_max_topic == doc_topics_res$max_topic))

  res <- model_df %>% tidy_rowwise(model, type="topic_words")
  expect_equal(colnames(res), c("word", "topic", "probability"))
  res <- model_df %>% tidy_rowwise(model, type="word_topics")
  expect_equal(colnames(res), c("word", "topic1", "topic2", "topic3", "max_topic", "topic_max"))
})
