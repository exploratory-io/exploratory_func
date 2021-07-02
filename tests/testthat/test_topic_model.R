# how to run this test:
# devtools::test(filter="textanal")
context("test topic model function, exp_topic_model")

test_that("exp_topic_model", {
  df <- tibble::tibble(text=c(
    "Jack and Jill went up the hill",
    "To fetch a pail of water",
    NA,
    "",
    "Jack fell down and broke his crown",
    "And Jill came tumbling after"))

  model_df <- df %>% exp_topic_model(text, stopwords_lang = "english", compound_tokens=c("Jack and jill")) # Testing both lower and upper case for compound_token.
  res <- model_df %>% tidy_rowwise(model, type="doc_topics")
  expect_equal(colnames(res), c("text", "topic1", "topic2", "topic3", "max_topic", "topic_max"))
  expect_equal(nrow(res), 5) # NA should be filtered, but empty string should be kept.
  res <- model_df %>% tidy_rowwise(model, type="topic_words")
  expect_equal(colnames(res), c("word", "topic", "probability"))
  res <- model_df %>% tidy_rowwise(model, type="word_topics")
  expect_equal(colnames(res), c("word", "topic1", "topic2", "topic3", "max_topic", "topic_max"))
})
