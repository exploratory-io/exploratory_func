exp_mca <- function(df, ..., max_nrow = NULL, allow_single_column = FALSE, seed = 1) {
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    install.packages("FactoMineR")
  }
  library(FactoMineR)

  all_cols <- colnames(df)
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  grouped_cols <- grouped_by(df)
  selected_cols <- setdiff(selected_cols, grouped_cols)

  if (any(selected_cols %in% grouped_cols)) {
    stop("Repeat-By column cannot be used as a variable column.")
  }

  df <- df %>%
    dplyr::select(-where(is.list),
                  -where(lubridate::is.difftime),
                  -where(lubridate::is.duration),
                  -where(lubridate::is.interval),
                  -where(lubridate::is.period))

  if(!is.null(seed)) { # Set seed before starting to call sample_n.
    set.seed(seed)
  }

  each_func <- function(df) {
    sampled_nrow <- NULL
    if (!is.null(max_nrow) && nrow(df) > max_nrow) {
      sampled_nrow <- max_nrow
      df <- df %>% sample_rows(max_nrow)
    }

    cleaned_df <- df[, colnames(df) %in% selected_cols, drop = FALSE]

    cols_copy <- colnames(cleaned_df)
    for (col in cols_copy) {
      unique_val <- unique(cleaned_df[[col]])
      if (length(unique_val) == 1) {
        cleaned_df <- cleaned_df[colnames(cleaned_df) != col]
      }
    }
    if (allow_single_column) {
      min_ncol <- 1
    } else {
      min_ncol <- 2
    }
    if (length(colnames(cleaned_df)) < min_ncol) {
      if (length(grouped_cols) < 1) {
        stop("There are not enough columns after removing the columns with only NA or a single value.")
      } else {
        return(NULL)
      }
    }

    cleaned_df <- cleaned_df %>% mutate_all(as.factor)
    fit <- FactoMineR::MCA(cleaned_df, graph = FALSE)
    fit$df <- df
    fit$grouped_cols <- grouped_cols
    fit$sampled_nrow <- sampled_nrow
    class(fit) <- c("mca_exploratory", class(fit))
    fit
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

tidy.mca_exploratory <- function(x, type="categories") {
  if (type == "categories") {
    res <- tibble::rownames_to_column(as.data.frame(x$var$coord), var="category_name")
    res <- res %>% select(category_name, `Dim 1`, `Dim 2`)
    res
  }
  else if (type == "variables") {
    res <- tibble::rownames_to_column(as.data.frame(x$var$eta2), var="variable_name")
    res <- res %>% select(variable_name, `Dim 1`, `Dim 2`)
    res
  }
}
