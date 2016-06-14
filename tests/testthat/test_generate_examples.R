loadNamespace("knitr")

test_that("do_tokenize examples", {
  ex_df <- data.frame(
    index=c("First", "Second"),
    text=c("It was a great party.", "She has just left. She will be off tomorrow."), stringsAsFactors = TRUE)
  knitr::kable(ex_df, format="markdown")
})
