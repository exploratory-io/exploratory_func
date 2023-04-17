exp_mca <- function(df, ..., max_nrow = NULL, allow_single_column = FALSE, ncp = 2, seed = 1) {
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
    var_names_map <- colnames(cleaned_df)
    names(var_names_map) <- paste0("V", 1:length(var_names_map))

    # Prefix the category values with the column index to make sure they are unique across columns, to avoid unwanted name prefixing by MCA
    for (i in 1:length(cleaned_df)) {
      cleaned_df[i] <- paste0("V",i,":",cleaned_df[[i]])
    }
    cleaned_df <- cleaned_df %>% mutate_all(as.factor)
    fit <- FactoMineR::MCA(cleaned_df, ncp = ncp, graph = FALSE)
    fit$df <- df
    fit$var_names_map <- var_names_map
    fit$grouped_cols <- grouped_cols
    fit$sampled_nrow <- sampled_nrow
    class(fit) <- c("mca_exploratory", class(fit))
    fit
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

tidy.mca_exploratory <- function(x, type="categories") {
  if (type == "categories") {
    res <- tibble::rownames_to_column(as.data.frame(x$var$coord), var="category")
    res <- res %>% dplyr::select(category, `Dim 1`, `Dim 2`)
    res <- res %>% tidyr::separate(col = category, into = c("variable", "category"), sep = ":")
    res <- res %>% dplyr::mutate(variable = x$var_names_map[variable])
    res
  }
  else if (type == "variables") {
    res <- tibble::rownames_to_column(as.data.frame(x$var$eta2), var="variable")
    res <- res %>% dplyr::select(variable, `Dim 1`, `Dim 2`)
    res
  }
  else if (type == "data") {
    res <- as.data.frame(x$ind$coord)
    res <- x$df %>% bind_cols(res)
    res
  }
}
