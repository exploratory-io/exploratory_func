#' @export
build_lda <- function(df, document, token, count = NULL,
                      n_topics,
                      method = "VEM",
                      iter = 1000,
                      burnin = 1000,
                      keep = 50,
                      seed = 0,
                      keep.source = FALSE){
  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tidyr")
  loadNamespace("text2vec")

  row_col <- col_name(substitute(document))
  col_col <- col_name(substitute(token))
  value_col <- col_name(substitute(value))

  grouped_column <- grouped_by(df)
  model_column <- avoid_conflict(grouped_column, "model")
  source_column <- avoid_conflict(grouped_column, "source.data")

  build_lda_each <- function(df){
    mat <- sparse_cast(df, row = row_col, col = col_col, count = TRUE) %>% slam::as.simple_triplet_matrix()
    lda <- topicmodels::LDA(mat, method = method, n_topics, control = list(seed = seed, iter = iter, keep = keep))
    model_set <- lda
  }

  rename_source <- function(df){
    colnames(df)[colnames(df) == row_col] <- "document"
    colnames(df)[colnames(df) == col_col] <- "term"
    df
  }

  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~build_lda_each(.), ~rename_source(.)), c(model_column, "source.data")))
  class(ret[["source.data"]]) <- c("list", ".source.data")
  ret
}
