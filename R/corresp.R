exp_mca <- function(df, ..., max_nrow = NULL, allow_single_column = FALSE, ncp = 5, quanti_sups = NULL, seed = 1) {
  all_cols <- colnames(df)
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  grouped_cols <- grouped_by(df)
  selected_cols <- setdiff(selected_cols, grouped_cols)
  quanti_sups <- setdiff(quanti_sups, grouped_cols)

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

    cleaned_df <- df[, colnames(df) %in% c(selected_cols, quanti_sups), drop = FALSE]

    for (col in selected_cols) {
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
    if (sum(selected_cols %in% colnames(cleaned_df)) < min_ncol) {
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
      if (colnames(cleaned_df)[i] %in% selected_cols) {
        cleaned_df[i] <- as.factor(paste0("V",i,":",cleaned_df[[i]]))
      }
    }
    quanti_sup_idx <- which(colnames(cleaned_df) %in% quanti_sups)
    if (length(quanti_sup_idx) == 0) {
      quanti_sup_idx <- NULL
    }
    fit <- FactoMineR::MCA(cleaned_df, ncp = ncp, graph = FALSE, quanti.sup=quanti_sup_idx)
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
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), starts_with("Dim "))
    res
  }
  else if (type == "variables") {
    res <- tibble::rownames_to_column(as.data.frame(x$var$eta2), var="variable")
    res <- res %>% dplyr::select(variable, `Dim 1`, `Dim 2`)
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), starts_with("Dim "))
    res
  }
  else if (type == "quanti_sup") {
    res <- tibble::rownames_to_column(as.data.frame(x$quanti.sup$coord), var="variable")
    res <- res %>% dplyr::select(variable, `Dim 1`, `Dim 2`)
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), starts_with("Dim "))
    res
  }
  else if (type == "contrib") {
    res <- tibble::rownames_to_column(as.data.frame(x$var$contrib), var="category")
    res <- res %>% dplyr::select(category, starts_with("Dim "))
    res <- res %>% tidyr::separate(col = category, into = c("variable", "category"), sep = ":")
    res <- res %>% dplyr::mutate(variable = x$var_names_map[variable])
    res <- res %>% tidyr::unite(Category, variable, category, sep=" - ")
    res <- res %>% tidyr::pivot_longer(cols=starts_with("Dim "), names_to="Dimension", values_to="Value")
    res <- res %>% dplyr::mutate(Dimension = stringr::str_replace(Dimension, "Dim ", "Dimension "))
    res
  }
  else if (type == "variance") {
    res <- as.data.frame(x$eig) %>% dplyr::mutate(dim=1:n())
    # Omit the tail demensions with almost 0 variances.
    res <- res %>% dplyr::filter(`percentage of variance` > 1e-15)
  }
  else if (type == "data") {
    res <- as.data.frame(x$ind$coord)
    res <- x$df %>% bind_cols(res)
    res <- res %>% dplyr::rename_with(~gsub("Dim ", "Dimension ", .), starts_with("Dim "))
    res
  }
}
