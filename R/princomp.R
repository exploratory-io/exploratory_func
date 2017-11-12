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

  each_func <- function(df) { # TODO: right now, group_by case throws error.
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
  else { # should be "scores"
    res <- x$df
    res <- res %>% dplyr::bind_cols(as.data.frame(x$scores))
    loadings <- rownames_to_column(as.data.frame(x$loadings[,]))
    res <- res %>% dplyr::bind_rows(loadings)
    res
  }
  res
}
