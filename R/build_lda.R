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
    class(lda) <- c("text2vec_LDA", class(lda))

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
    lda <- topicmodels::LDA(mat, n_topics)
    model_set <- lda
  }

  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~build_lda_each(.)), c(model_column)))
  ret
}

#' @export
tidy.text2vec_LDA <- function(lda, matrix = "phi", ...){
    # this is a way to access private members
    # https://github.com/wch/R6/issues/41
    private_member <- environment(lda$transform)$private
    # this is for LDA object from text2vec
    if (matrix == "phi") {
      model <- lda$get_fitted_LDA_model()
      phi <- (lda$get_word_vectors() + private_member$topic_word_prior) %>%
        t %>%
        text2vec::normalize("l1") %>%
        reshape2::melt()
      c_names <- c("topic", "term", "value")
      colnames(phi) <- c_names
      # convert the type of term column from factor to character
      phi[["term"]] <- as.character(phi[["term"]])
      ret <- phi
      ret
    } else if (matrix == "theta") {
      model <- lda$get_fitted_LDA_model()

      theta_mat = (model$document_topic_distr + private_member$doc_topic_prior) %>%
        t %>%
        text2vec::normalize("l1")
      rownames(theta_mat) <- private_member$doc_ids
      theta_melt <- theta_mat %>%
        reshape2::melt()
      c_names <- c("document", "topic", "value")
      colnames(theta_melt) <- c_names
      theta <- theta_melt
      ret <- theta
    ret
  }
}
