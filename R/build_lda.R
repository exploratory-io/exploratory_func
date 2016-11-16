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

  if (!output %in% c("model", "phi", "theta")) {
    stop("output has to be \"model\", \"phi\" or \"theta\"")
  }

  build_lda_each <- function(df){
    mat <- sparse_cast(df, row_col, col_col, value_col)
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
    doc_topic_distr <- lda$fit_transform(mat,
                                         n_iter = n_iter,
                                         convergence_tol = convergence_tol,
                                         check_convergence_every_n = check_convergence_every_n)

    model_set <- list(
      model = lda,
      source = df,
      params = list(
        topic_word_prior = topic_word_prior,
        doc_topic_prior = doc_topic_prior,
        documents_name = documents_name
      )
    )
    class(model_set) <- c("LDA_modelset")
    model_set
  }
  # if(keep.source | augment){
  #   output <- df %>%
  #     dplyr::do_(.dots=setNames(list(~build_lda_each(.), ~(.)), c(model_column, source_column)))
  #   # Add a class for Exploratyry to recognize the type of .source.data
  #   class(output[[source_column]]) <- c("list", "source.data")
  # } else {
  #   output <- df %>%
  #     dplyr::do_(.dots=setNames(list(~build_lda_each(.)), model_column))
  # }
  # # Add a class for Exploratyry to recognize the type of .model
  # if(augment){
  #   output <- do.call("augment_lda", list(output, model_column, source_column))
  # } else {
  #   class(output[[model_column]]) <- c("list", "model", "model.lda")
  # }
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~build_lda_each(.)), c(model_column)))
  if(output != "model"){
    ret <- ret %>% tidy(model, matrix = output)
  }
  ret
}

#' @export
tidy.LDA_modelset <- function(lda_modelset, matrix = "beta", groupers){
  lda <- lda_modelset$model
  params <- lda_modelset$params
  if (matrix == "phi") {
    model <- lda$get_fitted_LDA_model()

    phi_melt <- (lda$get_word_vectors() + params$topic_word_prior) %>%
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
    theta_mat = (model$document_topic_distr + params$doc_topic_prior) %>%
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
}
