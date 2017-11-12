#' do PCA
#' @export
do_princomp <- function(df,
                        ...
                       ) {
  # this evaluates select arguments like starts_with
  selected_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  # remove grouped col or target col
  selected_cols <- setdiff(selected_cols, grouped_cols)

  if (any(selected_cols %in% grouped_cols)) {
    stop("grouping column is used as variable columns")
  }

  each_func <- function(df) {
    cleaned_df <- df %>% dplyr::select_(.dots=selected_cols) %>%
      drop_na(everything()) # TODO: take care of the case where values of a column are mostly NA
    fit <- princomp(cleaned_df, cor=TRUE) # TODO: make cor an option
    fit$df <- df # add original df to model so that we can bind_col it for output.
    class(fit) <- c("princomp_exploratory", class(fit))
    fit
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' extracts results from princomp as a dataframe
#' @export
tidy.princomp_exploratory <- function(x, type="sdevs", ...) { #TODO: add test
  if (type == "sdevs") {
    res <- rownames_to_column(as.data.frame(x$sdev)) %>%
      mutate(rowname = fct_inorder(rowname)) # fct_inorder is to make order on chart right, e.g. Comp.2 before Comp.10
  }
  else if (type == "loadings") {
    res <- rownames_to_column(as.data.frame(x$loadings[,])) %>% gather(key, value, starts_with("Comp."), na.rm = TRUE, convert = TRUE) %>%
      mutate(key = fct_inorder(key)) # fct_inorder is to make order on chart right, e.g. Comp.2 before Comp.10
  }
  else { # should be "biplot"
    res <- x$df
    scores_matrix <- x$scores[,1:2] # keep only Comp.1 and Comp.2 for biplot
    max_abs_score <- max(abs(scores_matrix))
    res <- res %>% dplyr::bind_cols(as.data.frame(scores_matrix))
    loadings_matrix <- x$loadings[,1:2] # keep only Comp.1 and Comp.2 for biplot
    max_abs_loading <- max(abs(loadings_matrix))
    scale_ratio <- max_abs_score/max_abs_loading
    # scale loading_matrix so that the scale of measures and data points matches in the scatter plot.
    loadings_matrix <- loadings_matrix * scale_ratio
    loadings_df <- rownames_to_column(as.data.frame(loadings_matrix), var="measure_name") #TODO: what if name conflicts?
    loadings_df <- loadings_df %>% rename(Measures=Comp.2) # use different column name for Comp.2 of measures.
    loadings_df0 <- loadings_df %>% mutate(Comp.1=0, Measures=0) # create df for origin of coordinates.
    loadings_df <- loadings_df0 %>% bind_rows(loadings_df)
    res <- res %>% dplyr::bind_rows(loadings_df)
    res
  }
  res
}
