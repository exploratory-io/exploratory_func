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

# Rank of each clustering variable by "characteristic-ness", as an integer order column.
# tam#38491.
#
# The eta-squared ranking is what the "Characteristic Variables" bar chart shows, but the
# sibling charts drawn from OTHER tidiers (the per-observation boxplot's colour legend, the
# per-variable detail table) had no way to reach it: a chart preprocessor cannot call
# `tidy_rowwise(model, ...)` twice, so the ranking cannot be joined in on the tam side. It
# therefore has to travel WITH the frame that needs it, the same way #38492 shipped the
# Cluster Profile axis order as a `variable_order` column.
#
# Returned as a rank column rather than by reordering `variable` itself, so `variable` keeps
# its character class for every other consumer; the chart preprocessor turns the rank into
# the factor order with `forcats::fct_reorder()`.
#
# @param mat a numeric matrix, one column per clustering variable (same argument as
#   `cluster_variable_importance_anova()`).
# @param cluster_ids a vector giving each row's cluster assignment.
# @param variables optional character vector of variable names the caller needs an order for.
#   Names not present in `mat` (so not rankable by eta-squared) are appended after the ranked
#   ones in name order, so the returned map always covers every requested name and a
#   downstream `fct_reorder()` never sees an NA rank.
# @return a tibble with columns: variable, importance_order (1 = most characteristic).
cluster_variable_importance_order <- function(mat, cluster_ids, variables = NULL) {
  ranked <- cluster_variable_importance_anova(mat, cluster_ids) %>%
    dplyr::arrange(dplyr::desc(eta_squared), variable)
  names_in_order <- ranked$variable
  if (!is.null(variables)) {
    extra <- sort(setdiff(as.character(variables), names_in_order))
    names_in_order <- c(names_in_order, extra)
  }
  tibble::tibble(
    variable = names_in_order,
    importance_order = seq_along(names_in_order)
  )
}
