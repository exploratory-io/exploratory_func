#' Create lda model
#' @param df Tokenized data frame
#' @param document A column regarded as document id.
#' @param token A column that has token.
#' @param count Count of tokens in the document. If NULL, counts of token in a document.
#' @param n_topics Number of topics
#' @param method "VEM" or "Gibbs". Algorithm of LDA.
#' @param iter Number of iteration.
#' @param burnin How many iterations should be omitted in Gibbs iteration.
#' @param keep.source If nested source data should be in the result.
#' @param group_cols Grouping columns.
#' @export
build_lda <- function(df, document, token, count = NULL,
                      n_topics,
                      method = "VEM",
                      iter = 2000,
                      burnin = 0,
                      seed = 0,
                      keep.source = TRUE,
                      group_cols = NULL){
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("topicmodels")
  loadNamespace("slam")

  row_col <- col_name(substitute(document))
  col_col <- col_name(substitute(token))
  value_col <- col_name(substitute(count))

  # deal with group columns by index because those names might be changed
  group_col_index <- colnames(df) %in% group_cols

  # change column names to avoid name conflict when tidy or glance are executed
  reserved_names <- c(
    "model",
    # for tidy matrix beta
    "topic", "term", "beta",
    # for tidy matrix gamma
    "document", "gamma",
    # for glance
    "iter", "terms", "alpha"
  )
  colnames(df)[group_col_index] <- avoid_conflict(
    reserved_names,
    colnames(df)[group_col_index],
    ".group"
  )

  # make column names unique
  colnames(df) <- make.unique(colnames(df), sep = "")

  if(!is.null(group_cols)){
    df <- dplyr::group_by_(df, .dots =  colnames(df)[group_col_index])
  } else {
    df <- dplyr::ungroup(df)
  }

  model_col <- "model"
  source_col <- "source.data"

  build_lda_each <- function(df){
    mat <- sparse_cast(df, row = row_col, col = col_col, val = value_col, count = TRUE) %>% slam::as.simple_triplet_matrix()
    lda <- if(method == "VEM"){
      topicmodels::LDA(mat, method = method, n_topics, control = list(seed = seed))
    } else {
      topicmodels::LDA(mat, method = method, n_topics, control = list(seed = seed,
                                                                      iter = iter,
                                                                      burnin = burnin))
    }
    lda
  }

  # the column names of source data should be modified for augment
  # because it tries to join source data and the result by joining "document" and "term" columns
  rename_source <- function(df){
    colnames(df)[colnames(df) == row_col] <- "document"
    colnames(df)[colnames(df) == col_col] <- "term"
    df
  }

  if(keep.source){
    ret <- df %>%
      dplyr::do_(.dots=setNames(list(~build_lda_each(.), ~rename_source(.)), c("model", "source.data")))
    class(ret[["source.data"]]) <- c("list", ".source.data")
  } else {
    ret <- df %>%
      dplyr::do_(.dots=setNames(list(~build_lda_each(.)), "model"))
  }
  class(ret[["model"]]) <- c("list", ".model", ".model.LDA")
  ret
}
