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
                      keep.source = FALSE,
                      matrix = "model"){
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

  if (!matrix %in% c("model", "phi", "theta")) {
    stop("matrix has to be \"model\", \"phi\" or \"theta\"")
  }

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

    model_set <- lda
  }

  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~build_lda_each(.)), c(model_column)))
  ret
}

#' @export
build_topicmodel <- function(df, subject, key, value = NULL,
                      n_topics,
                      topic_word_prior = 1 / n_topics,
                      doc_topic_prior = 1 / n_topics,
                      n_iter = 1000,
                      convergence_tol = -1,
                      check_convergence_every_n = 0,
                      min_df = 2,
                      max_df = NULL ,
                      seed = 0,
                      keep.source = FALSE,
                      matrix = "model"){
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

  if (!matrix %in% c("model", "phi", "theta")) {
    stop("matrix has to be \"model\", \"phi\" or \"theta\"")
  }

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
    mat <-  slam::as.simple_triplet_matrix(mat)
    lda <- topicmodels::LDA(mat, 3)
    model_set <- lda
  }

  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~build_lda_each(.)), c(model_column)))
  browser()
  ret
}

#' @export
tidy.LDA <- function(lda, matrix = "phi", ...){
  tryCatch({
    # this is for LDA object from text2vec
    if (matrix == "phi") {
      model <- lda$get_fitted_LDA_model()

      phi_melt <- lda$get_word_vectors() %>%
        t %>%
        text2vec::normalize("l1") %>%
        reshape2::melt()
      # rename columns avoiding conflict among grouped columns
      c_names <- avoid_conflict(groupers, c("topic", "term", "value"))
      colnames(phi_melt) <- c_names
      # the order of column names is term, topic and value
      phi <- phi_melt[, c(2, 1, 3)]
      # convert the type of term column from factor to character
      phi[["term"]] <- as.character(phi[["term"]])
      ret <- phi
    } else if (matrix == "theta") {
      model <- lda$get_fitted_LDA_model()
      theta_mat = model$document_topic_distr %>%
        t %>%
        text2vec::normalize("l1")
      rownames(theta_mat) <- params$documents_name
      theta_melt <- theta_mat %>%
        reshape2::melt()
      # rename columns avoiding conflict among grouped columns
      c_names <- avoid_conflict(groupers, c("document", "topic", "value"))
      colnames(theta_melt) <- c_names
      theta <- theta_melt
      ret <- theta
    }
    ret
  }, error = function(e){
    # this is for LDA object from topicmodels
    tidytext::tidy(lda, matrix = matrix, ...)
  })

}
