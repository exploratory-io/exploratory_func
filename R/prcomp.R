#' do PCA
#' @export
do_prcomp <- function(df, ..., normalize_data=TRUE, max_nrow = NULL, seed = NULL) { # TODO: write test
  # this evaluates select arguments like starts_with
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  # remove grouped col or target col
  selected_cols <- setdiff(selected_cols, grouped_cols)

  if (any(selected_cols %in% grouped_cols)) {
    stop("Repeat-By column cannot be used as a variable column.")
  }

  if(!is.null(seed)) { # Set seed before starting to call sample_n.
    set.seed(seed)
  }

  each_func <- function(df) {
    filtered_df <- df %>% tidyr::drop_na(!!!rlang::syms(selected_cols)) # TODO: take care of the case where values of a column are mostly NA
    if (nrow(filtered_df) == 0) { # skip this group if no row is left.
      return(NULL)
    }
    # sample the data for quicker turn around on UI,
    # if data size is larger than specified max_nrow.
    sampled_nrow <- NULL
    if (!is.null(max_nrow) && nrow(filtered_df) > max_nrow) {
      # Record that sampling happened.
      sampled_nrow <- max_nrow
      filtered_df <- filtered_df %>% sample_rows(max_nrow)
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
    if (length(colnames(cleaned_df)) == 0) { # skip this group if no column is left.
      return(NULL)
    }
    # "scale." is an argument name. There is no such operator like ".=". 
    fit <- prcomp(cleaned_df, scale.=normalize_data)
    fit$df <- filtered_df # add filtered df to model so that we can bind_col it for output. It needs to be the filtered one to match row number.
    fit$grouped_cols <- grouped_cols
    fit$sampled_nrow <- sampled_nrow
    class(fit) <- c("prcomp_exploratory", class(fit))
    fit
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' extracts results from prcomp as a dataframe
#' @export
#' @param n_sample Sample number for biplot. Default 5000, which is the default of our scatter plot.
#'        we use it for gathered_data for parallel coordinates too. sampling is applied before gather.
tidy.prcomp_exploratory <- function(x, type="variances", n_sample=NULL, pretty.name=FALSE, normalize_data=FALSE, ...) { #TODO: add test
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

    if (is.null(n_sample)) { # set default of 5000 for biplot case.
      n_sample = 5000
    }
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
      # res <- res %>% dplyr::mutate(cluster=factor(x$kmeans$cluster)) # this caused error when input had column x.
      res$cluster <- factor(x$kmeans$cluster)
    }

    res <- res %>% sample_rows(score_n_sample)

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
      # res <- res %>% dplyr::mutate(cluster=factor(x$kmeans$cluster)) # this caused error when input had column x.
      res$cluster <- factor(x$kmeans$cluster)
    }
    res <- res %>% dplyr::bind_cols(as.data.frame(x$x))
    column_names <- attr(x$rotation, "dimname")[[1]] 
    if (normalize_data) {
      res <- res %>% mutate_at(column_names, exploratory::normalize)
    }

    if (!is.null(n_sample)) { # default is no sampling.
      # limit n_sample so that no more dots are created than the max that can be plotted on scatter plot, which is 5000.
      n_sample <- min(n_sample, floor(5000 / length(column_names)))
      res <- res %>% sample_rows(n_sample)
    }

    if (type == "gathered_data") { # for boxplot and parallel coordinates. this is only when with kmeans.
      # We used to drop columns other than cluster and ones used for clustering like this commented out line,
      # to keep only the data we use, but since we are showing Subject Column value
      # on parallel coordinates, we need to keep other columns, which would include Subject Column.
      # res <- res %>% dplyr::select(!!c(column_names,"cluster"))
      res <- res %>% dplyr::mutate(row_id=seq(n())) # row_id for line representation.
      res <- res %>% tidyr::gather(key="key",value="value",!!column_names)
    }
  }
  res
}
