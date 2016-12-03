#' @export
build_lda <- function(df, subject, key, value = NULL,
                      n_topics,
                      topic_word_prior = 1 / n_topics,
                      doc_topic_prior = 1 / n_topics,
                      n_iter = 1000,
                      convergence_tol = -1,
                      check_convergence_every_n = 0,
                      min_df = 2,
                      max_df = NULL ,
                      seed = 0,
                      keep.source = FALSE){
  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tidyr")
  loadNamespace("text2vec")

  set.seed(seed)

  row_col <- col_name(substitute(subject))
  col_col <- col_name(substitute(key))
  value_col <- col_name(substitute(value))

  grouped_column <- grouped_by(df)
  model_column <- avoid_conflict(grouped_column, "model")
  source_column <- avoid_conflict(grouped_column, "source.data")

  build_lda_each <- function(df){
    mat <- sparse_cast(df, row_col, col_col, value_col, count = TRUE)
    documents_name <- rownames(mat)
    if(! (is.null(min_df) & is.null(max_df)) ){
      # get df
      token <- Matrix::colSums((mat>0))
      if (!is.null(min_df)){
        range <- token > min_df
        token <- token[range]
        mat <- mat[, range]
      }
      if (!is.null(max_df)){
        range <- token < max_df
        token <- token[range]
        mat <- mat[, range]
      }
      if(ncol(mat) == 0){
        stop("There is no term after filtering")
      }
    }
    lda <- text2vec::LatentDirichletAllocation$new(n_topics,
                                                   vocabulary = colnames(mat),
                                                   doc_topic_prior = doc_topic_prior,
                                                   topic_word_prior = topic_word_prior)
    lda$fit(mat,
            n_iter = n_iter,
            convergence_tol = convergence_tol,
            check_convergence_every_n = check_convergence_every_n)

    ret <- list(model = lda)

    class(ret) <- c("text2vec_LDA", "exp_model_wrapper", class(ret))
    ret
  }

  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~build_lda_each(.)), c(model_column)))
  ret
}

#' @export
build_topicmodel <- function(df, document, token, count = NULL,
                      n_topics,
                      method = "VEM",
                      topic_word_prior = 1 / n_topics,
                      doc_topic_prior = 1 / n_topics,
                      n_iter = 1000,
                      convergence_tol = -1,
                      check_convergence_every_n = 0,
                      min_df = 2,
                      max_df = NULL ,
                      seed = 0,
                      keep.source = FALSE){
  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tidyr")
  loadNamespace("text2vec")

  set.seed(seed)

  row_col <- col_name(substitute(document))
  col_col <- col_name(substitute(token))
  value_col <- col_name(substitute(value))

  grouped_column <- grouped_by(df)
  model_column <- avoid_conflict(grouped_column, "model")
  source_column <- avoid_conflict(grouped_column, "source.data")

  build_lda_each <- function(df){
    mat <- sparse_cast(df, row = row_col, col = col_col, count = TRUE) %>% slam::as.simple_triplet_matrix()
      # dplyr::count_(df, c(row_col, col_col)) %>% tidytext::cast_dtm_(term_col = col_col, document_col = row_col, value = colnames(.)[[3]])
    # if(! (is.null(min_df) & is.null(max_df)) ){
    #   # get df
    #   token <- Matrix::colSums((mat>0))
    #   if (!is.null(min_df)){
    #     range <- token > min_df
    #     token <- token[range]
    #     mat <- mat[, range]
    #   }
    #   if (!is.null(max_df)){
    #     range <- token < max_df
    #     token <- token[range]
    #     mat <- mat[, range]
    #   }
    #   if(ncol(mat) == 0){
    #     stop("There is no term after filtering")
    #   }
    # }
    lda <- topicmodels::LDA(mat, n_topics, control = list(seed = seed))
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
