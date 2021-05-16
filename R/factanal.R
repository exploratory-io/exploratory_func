#' do PCA
#' @export
exp_factanal <- function(df, ..., factors = 2, scores = "regression", rotate = "none", max_nrow = NULL, seed = NULL) { # TODO: write test
  # this evaluates select arguments like starts_with
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  # remove grouped col or target col
  selected_cols <- setdiff(selected_cols, grouped_cols)

  if (any(selected_cols %in% grouped_cols)) {
    stop("Repeat-By column cannot be used as a variable column.")
  }

  # list and difftime etc. causes error in tidy_rowwise(model, type="biplot").
  # For now, we are removing them upfront.
  df <- df %>% dplyr::select(-where(is.list),
                             -where(lubridate::is.difftime),
                             -where(lubridate::is.duration),
                             -where(lubridate::is.interval),
                             -where(lubridate::is.period))

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
    min_ncol <- 2
    if (length(colnames(cleaned_df)) < min_ncol) {
      if (length(grouped_cols) < 1) {
        # If without group_by, throw error to display message.
        stop("There are not enough columns after removing the columns with only NA or a single value.")
      }
      else {
        # skip this group if less than 2 column is left. (We can't handle single column for now.)
        return(NULL)
      }
    }
    # "scale." is an argument name. There is no such operator like ".=". 
    fit <- psych::fa(cleaned_df, nfactors=factors, scores=scores, rotate=rotate)
    fit$correlation <- cor(cleaned_df) # For creating scree plot later.
    fit$df <- filtered_df # add filtered df to model so that we can bind_col it for output. It needs to be the filtered one to match row number.
    fit$grouped_cols <- grouped_cols
    fit$sampled_nrow <- sampled_nrow
    class(fit) <- c("fa_exploratory", class(fit))
    fit
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

glance.fa_exploratory <- function(x, pretty.name = FALSE, ...) {
  res <- broom:::glance.factanal(x) %>% dplyr::select(-n)
  res <- res %>% dplyr::rename(`Factors`=n.factors, `Total Variance`=total.variance, `Chi-Square`=statistic, `P Value`=p.value, `Degree of Freedom`=df, `Method`=method, `Converged`=converged, `Number of Rows`=nobs)
  res
}

#' extracts results from prcomp as a dataframe
#' @export
#' @param n_sample Sample number for biplot. Default 5000, which is the default of our scatter plot.
#'        we use it for gathered_data for parallel coordinates too. sampling is applied before gather.
tidy.fa_exploratory <- function(x, type="loadings", n_sample=NULL, pretty.name=FALSE, ...) { #TODO: add test
  if (type == "screeplot") {
    eigen_res <- eigen(x$correlation, only.values = TRUE) # Cattell's scree plot is eigenvalues of correlation/covariance matrix.
    res <- tibble::tibble(factors=1:length(eigen_res$values), eigenvalue=eigen_res$values)
  }
  else if (type == "variances") {
    res <- as.tibble(t(x$Vaccounted)) %>% mutate(Factor=as.factor(1:n()), `% Variance`=100*`Proportion Var`, `Cummulated % Variance`=100*`Cumulative Var`)
  }
  else if (type == "loadings") {
    res <- broom:::tidy.factanal(x) # TODO: This just happens to work. Revisit.
    res <- res %>% tidyr::pivot_longer(cols=c(starts_with("MR"), "uniqueness"), names_to="factor", values_to="value")
    res <- res %>% dplyr::mutate(factor = case_when(factor=="uniqueness"~"Uniqueness", TRUE~stringr::str_replace(factor,"^MR","Factor ")))
    res <- res %>% dplyr::mutate(factor = forcats::fct_inorder(factor)) # fct_inorder is to make order on chart right, e.g. Factor 2 before Factor 10
  }
  else if (type == "biplot") {
    scores_df <- broom:::augment.factanal(x)
    scores_df <- scores_df %>% select(-.rownames) # augment.factanal seems to always return row names in .rownames column.
    loadings_df <- broom:::tidy.factanal(x)

    if (is.null(n_sample)) { # set default of 5000 for biplot case.
      n_sample = 5000
    }
    # sum of number of loading rows times 2 (because it is line between 2 points) and number of score rows should fit in n_sample.
    score_n_sample <- n_sample - nrow(loadings_df)*2

    # table of observations. bind original data so that color can be used later.
    res <- x$df

    # Adjust types of other columns that can be used for color or label.
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

    res <- res %>% dplyr::bind_cols(scores_df)

    res <- res %>% sample_rows(score_n_sample)

    # calculate scale ratio for displaying loadings on the same chart as scores.
    loadings_matrix <- as.matrix(loadings_df %>% dplyr::select(MR1, MR2))
    max_abs_loading <- max(abs(loadings_matrix))
    max_abs_score <- max(abs(c(res$MR1, res$MR2)))
    scale_ratio <- max_abs_score/max_abs_loading

    res <- res %>% dplyr::rename(Observations=MR2, `Factor 1`=MR1) # name to appear at legend for dots in scatter plot.
    # scale loading_matrix so that the scale of measures and data points matches in the scatter plot.
    loadings_df <- loadings_df %>% dplyr::mutate(MR1=MR1*scale_ratio, MR2=MR2*scale_ratio)
    loadings_df <- loadings_df %>% dplyr::rename(`Factor 1`=MR1, Measures=MR2, Uniqueness=uniqueness) # use different column name for PC2 of measures.
    loadings_df0 <- loadings_df %>% dplyr::mutate(`Factor 1`=0, Measures=0) # create df for origin of coordinates.
    loadings_df <- loadings_df0 %>% dplyr::bind_rows(loadings_df)
    res <- res %>% dplyr::bind_rows(loadings_df)
    # fill group_by column so that Repeat By on chart works fine. loadings_df does not have values for the group_by column.
    res <- res %>% tidyr::fill(x$grouped_cols)
    res
  }
  else { # should be data
    scores_df <- broom:::augment.factanal(x) # This happens to work. Revisit.
    scores_df <- scores_df %>% select(-.rownames) # augment.factanal seems to always return row names in .rownames column.
    scores_df <- scores_df %>% rename_with(function(x){stringr::str_replace(x,"^\\MR", "Factor ")}, starts_with("MR")) #TODO: Make string match condition stricter.

    # table of observations. bind original data so that color can be used later.
    res <- x$df
    res <- res %>% dplyr::bind_cols(scores_df)
  }
  res
}
