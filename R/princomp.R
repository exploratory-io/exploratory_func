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

  df <- df %>% dplyr::select_(.dots=selected_cols) %>%
    drop_na(everything())

  each_func <- function(df) {
    fit <- princomp(df, cor=TRUE)
    class(fit) <- c("princomp_exploratory", class(fit))
    fit
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' extracts results from princomp as a dataframe
#' @export
tidy.princomp_exploratory <- function(x, type="sdevs", ...) { #TODO: add test
  if (type == "sdevs") {
    res <- rownames_to_column(as.data.frame(x$sdev))
  }
  else if (type == "loadings") {
    res <- rownames_to_column(as.data.frame(x$loadings[,]))
  }
  else { # should be "scores"
    res <- as.data.frame(x$scores)
  }
  res
}
