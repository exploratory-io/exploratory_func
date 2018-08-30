#' do PCA
#' @export
do_prcomp <- function(df, ...) { # TODO: write test
  # this evaluates select arguments like starts_with
  selected_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  # remove grouped col or target col
  selected_cols <- setdiff(selected_cols, grouped_cols)

  if (any(selected_cols %in% grouped_cols)) {
    stop("grouping column is used as variable columns")
  }

  each_func <- function(df) {
    filtered_df <- df %>% tidyr::drop_na_(selected_cols) # TODO: take care of the case where values of a column are mostly NA
    if (nrow(filtered_df) == 0) { # skip this group if no row is left.
      return(NULL)
    }
    # select_ was not able to handle space in target_col. let's do it in base R way.
    cleaned_df <- filtered_df[,colnames(filtered_df) %in% selected_cols, drop=FALSE]

    # remove columns with only one unique value
    cols_copy <- colnames(cleaned_df)
    for (col in cols_copy) {
      unique_val <- unique(cleaned_df[[col]])
      if (length(unique_val) == 1) {
        cleaned_df <- cleaned_df[colnames(cleaned_df) != col]
      }
    }
    if (col(cleaned_df) == 0) { # skip this group if no column is left.
      return(NULL)
    }

    fit <- prcomp(cleaned_df, scale=TRUE)
    fit$df <- filtered_df # add filtered df to model so that we can bind_col it for output. It needs to be the filtered one to match row number.
    fit$grouped_cols <- grouped_cols
    class(fit) <- c("prcomp_exploratory", class(fit))
    fit
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' extracts results from prcomp as a dataframe
#' @export
#' @param n_sample Sample number for biplot. Default 5000, which is the default of our scatter plot
tidy.prcomp_exploratory <- function(x, type="variances", n_sample=5000, pretty.name=FALSE, normalize_data=FALSE, ...) { #TODO: add test
  if (type == "variances") {
    res <- as.data.frame(x$sdev*x$sdev) # square it to make it variance
    colnames(res)[1] <- "variance"
    res <- tibble::rownames_to_column(res, var="component") %>% # square it to make it variance
      mutate(component = forcats::fct_inorder(component)) # fct_inorder is to make order on chart right, e.g. PC2 before PC10
    total_variance = sum(res$variance)
    res <- res %>% dplyr::mutate(cum_pct_variance = cumsum(variance), cum_pct_variance = cum_pct_variance/total_variance*100)
    res <- res %>% dplyr::mutate(pct_variance = variance/total_variance*100)
    if (pretty.name) {
      res <- res %>% dplyr::rename(`% Variance`=pct_variance, `Cummulated % Variance`=cum_pct_variance)
    }
  }
  else if (type == "loadings") {
    res <- tibble::rownames_to_column(as.data.frame(x$rotation[,]), var="measure")
    res <- res %>% tidyr::gather(component, value, dplyr::starts_with("PC"), na.rm = TRUE, convert = TRUE)
    res <- res %>% dplyr::mutate(component = forcats::fct_inorder(component)) # fct_inorder is to make order on chart right, e.g. PC2 before PC10
    res <- res %>% dplyr::mutate(value = value^2) # square it to make it squared cosine. the original value is cosine.
  }
  else if (type == "biplot") {
    # prepare loadings matrix
    loadings_matrix <- x$rotation[,1:2] # keep only PC1 and PC2 for biplot

    # prepare scores matrix
    scores_matrix <- x$x[,1:2] # keep only PC1 and PC2 for biplot

    # sum of number of loading rows times 2 (because it is line between 2 points) and number of score rows should fit in n_sample.
    score_n_sample <- n_sample - nrow(loadings_matrix)*2

    # table of observations. bind original data so that color can be used later.
    res <- x$df

    orig_cols <- colnames(res)
    for (orig_col in orig_cols) {
      if (!is.numeric(res[[orig_col]])) {
        if (!is.logical(res[[orig_col]])) {
          # make categorical columns into factor with NA level, so that legend will show NA.
          # if we leave them as real NA, legend for NA would not be shown on biplot chart,
          # since we supress it not to show NAs from the lines for measures.
          res[[orig_col]] <- forcats::fct_explicit_na(as.factor(res[[orig_col]]), na_level="(NA)")
        }
        else {
          # make logical columns into factor with NA level, so that legend will show NA.
          res[[orig_col]] <- forcats::fct_explicit_na(factor(res[[orig_col]], levels = c("TRUE","FALSE")), na_level="(NA)")
        }
      }
    }

    res <- res %>% dplyr::bind_cols(as.data.frame(scores_matrix))

    if (!is.null(x$kmeans)) { # add cluster column if with kmeans.
      res <- res %>% dplyr::mutate(cluster=factor(x$kmeans$cluster))
    }

    if (nrow(res) > score_n_sample) {
      res <- res %>% dplyr::sample_n(score_n_sample)
    }

    # calculate scale ratio for displaying loadings on the same chart as scores.
    max_abs_loading <- max(abs(loadings_matrix))
    max_abs_score <- max(abs(c(res$PC1, res$PC2)))
    scale_ratio <- max_abs_score/max_abs_loading

    res <- res %>% rename(Observations=PC2) # name to appear at legend for dots in scatter plot.
    # scale loading_matrix so that the scale of measures and data points matches in the scatter plot.
    loadings_matrix <- loadings_matrix * scale_ratio
    loadings_df <- tibble::rownames_to_column(as.data.frame(loadings_matrix), var="measure_name") #TODO: what if name conflicts?
    loadings_df <- loadings_df %>% rename(Measures=PC2) # use different column name for PC2 of measures.
    loadings_df0 <- loadings_df %>% mutate(PC1=0, Measures=0) # create df for origin of coordinates.
    loadings_df <- loadings_df0 %>% bind_rows(loadings_df)
    res <- res %>% dplyr::bind_rows(loadings_df)
    # fill group_by column so that Repeat By on chart works fine. loadings_df does not have values for the group_by column.
    res <- res %>% tidyr::fill(x$grouped_cols)
    res
  }
  else { # should be data or gathered_data
    res <- x$df
    if (!is.null(x$kmeans)) {
      res <- res %>% dplyr::mutate(cluster=factor(x$kmeans$cluster))
    }
    res <- res %>% dplyr::bind_cols(as.data.frame(x$x))
    column_names <- attr(x$rotation, "dimname")[[1]] 
    if (normalize_data) {
      res <- res %>% mutate_at(column_names, exploratory::normalize)
    }

    if (type == "gathered_data") { # for boxplot. this is only when with kmeans.
      res <- res %>% select(!!c(column_names,"cluster"))
      res <- res %>% mutate(row_id=seq(n)) # row_id for line representation.
      res <- res %>% gather(key="key",value="value",!!column_names)
    }
  }
  res
}

