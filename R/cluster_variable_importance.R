# Shared per-variable "Characteristic Variables" computation for numeric-only clustering
# analytics types (K-Medoids, K-Means). tam#38160.
#
# Ranks each clustering variable by eta-squared (the proportion of that variable's total
# variance explained by cluster membership -- a one-way-ANOVA effect size), paired with the
# ANOVA F statistic and p value for the same one-way model (variable ~ cluster). Larger
# eta-squared means the variable differs more strongly across clusters, i.e. it is more
# "characteristic" of the clustering.
#
# K-Modes' sibling feature (`.kmodes_variable_importance` in kmodes.R, not shown here) uses a
# different statistic (Cramer's V from a chi-square test) because K-Modes clusters purely
# CATEGORICAL variables, where ANOVA/eta-squared does not apply. K-Medoids and K-Means both
# cluster purely NUMERIC variables (see exp_kmedoids()'s `if (!all(vapply(df[selected_cols],
# is.numeric, ...)))` guard and kmeans.json's `columnTypes: ["numeric"]`), so they share this
# ANOVA-based implementation.
#
# @param mat a numeric matrix, one column per clustering variable, one row per observation
#   used in the fit (already NA-filtered by the caller -- see `preprocess_factanal_data_before_sample()`).
# @param cluster_ids a vector (integer or factor), length == nrow(mat), giving each row's
#   cluster assignment.
# @return a tibble with columns: variable, eta_squared, test_statistic (F value), p_value.
cluster_variable_importance_anova <- function(mat, cluster_ids) {
  ids <- factor(cluster_ids)
  purrr::map_dfr(seq_len(ncol(mat)), function(index) {
    value <- mat[, index]
    grand_mean <- mean(value, na.rm = TRUE)
    between <- sum(tapply(value, ids, function(group) {
      length(group) * (mean(group, na.rm = TRUE) - grand_mean)^2
    }), na.rm = TRUE)
    total <- sum((value - grand_mean)^2, na.rm = TRUE)
    eta_squared <- if (total > 0) between / total else 0
    fit <- tryCatch(stats::aov(value ~ ids), error = function(e) NULL)
    fit_table <- if (is.null(fit)) NULL else summary(fit)[[1]]
    tibble::tibble(
      variable = colnames(mat)[[index]],
      eta_squared = eta_squared,
      test_statistic = if (is.null(fit_table)) NA_real_ else fit_table[['F value']][[1]],
      p_value = if (is.null(fit_table)) NA_real_ else fit_table[['Pr(>F)']][[1]]
    )
  })
}
