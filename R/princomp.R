
#' extracts results from princomp as a dataframe
#' @export
tidy.princomp_exploratory <- function(x, type="variances", ...) { #TODO: add test
  if (type == "variances") {
    res <- as.data.frame(x$sdev*x$sdev) # square it to make it variance
    colnames(res)[1] <- "variance"
    res <- rownames_to_column(res, var="component") %>% # square it to make it variance
      mutate(component = forcats::fct_inorder(component)) # fct_inorder is to make order on chart right, e.g. Comp.2 before Comp.10
    res <- res %>% dplyr::mutate(cum_pct_variance = cumsum(variance), cum_pct_variance = cum_pct_variance/max(cum_pct_variance)*100)
  }
  else if (type == "loadings") {
    res <- rownames_to_column(as.data.frame(x$loadings[,]), var="measure") %>% gather(component, value, starts_with("Comp."), na.rm = TRUE, convert = TRUE) %>%
      mutate(component = fct_inorder(component)) # fct_inorder is to make order on chart right, e.g. Comp.2 before Comp.10
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
    loadings_df <- loadings_df0 %>% dplyr::bind_rows(loadings_df)
    res <- res %>% dplyr::bind_rows(loadings_df)
    # fill group_by column so that Repeat By on chart works fine. loadings_df does not have values for the group_by column.
    res <- res %>% tidyr::fill(x$grouped_cols)
    res
  }
  res
}
