# how to run this test:
# devtools::test(filter="textanal")
context("test topic model function, exp_topic_model")
twitter_df <- exploratory::read_delim_file("https://www.dropbox.com/s/w1fh7j8iq6g36ry/Twitter_No_Spectator_Olympics_Ja.csv?dl=1", delim = ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE)

test_that("exp_topic_model with Japanese twitter data", {
  model_df <- twitter_df %>% exp_topic_model(text, category=source, stopwords_lang = "japanese")
  res <- model_df %>% tidy_rowwise(model, type="doc_topics_tagged", word_topic_probability_threshold=0.4)
  res <- model_df %>% tidy_rowwise(model, type="topics_summary")
  expect_equal(colnames(res), c("topic", "n"))
  expect_equal(sum(res$n), 5000)
  # Ensure that topics are named according to the frequency to make sure topic name mapping on doc_df is correct.
  expect_equal((res %>% dplyr::arrange(desc(n)))$topic, c(1,2,3))
  doc_topics_res <- model_df %>% tidy_rowwise(model, type="doc_topics")
  # Ensure the max_topic values from model$doc_df make sense, after the topic name mapping. slice is there to avoid the case with ties,
  # in which case the result can differ because of the randomness.
  expect_true(all((doc_topics_res %>% slice(1:70) %>% dplyr::mutate(max_topic_2 = summarize_row(across(starts_with("topic")), which.max.safe)) %>%
                   mutate(test_res = max_topic==max_topic_2))$test_res))
  expect_equal(colnames(doc_topics_res), c("created_at", "screen_name", "text", "source", "topic1", "topic2", "topic3", "max_topic", "topic_max"))
  expect_equal(nrow(doc_topics_res), 5000) # NA should be filtered, but empty string should be kept.

  doc_word_res <- model_df %>% tidy_rowwise(model, type="doc_word_category")
  # Ensure that the document max topic from model$doc_df joined with doc_word_df corresponds with the one from model$doc_df.
  expect_true(all((doc_word_res %>% group_by(document) %>% summarise(document_max_topic=first(document_max_topic)))$document_max_topic == doc_topics_res$max_topic))

  res <- model_df %>% tidy_rowwise(model, type="topic_words")
  expect_equal(colnames(res), c("word", "topic", "probability"))
  res <- model_df %>% tidy_rowwise(model, type="word_topics")
  # Ingnoring order since column order is expected to be messed up here because of topic name mapping.
  expect_true(all(colnames(res) %in% c("word", "topic1", "topic2", "topic3", "max_topic", "topic_max")))
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
  theta <- matrix(c(0.9, 0.9, 0.1, 0.1), 2, 2, dimnames=list(c('text1','text2'),c('topic1','topic2')))
  model <- list(theta=theta, k=2)
  x <- list(model=model)
  class(x) <- "textmodel_lda_exploratory"
  model_df <- tibble::tibble(model = list(x))
  res <- model_df %>% tidy_rowwise(model, type="topics_summary")
  expect_equal(res$topic, c(1,2))
  expect_equal(res$n, c(2,0))
})
