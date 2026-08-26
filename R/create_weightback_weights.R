#' Create a population-weighted weight column
#' @param data A data frame.
#' @param pop_dist Population distribution with one row per weight-variable key.
#' @param weight_vars Character names of categorical key columns.
#' @param population_pct_col Population proportion column name.
#' @param weight_col New output column name.
#' @param normalize_population Whether to normalize supplied proportions.
#' @export
create_weightback_weights <- function(data, pop_dist, weight_vars,
                                      population_pct_col = "population_pct",
                                      weight_col = "weightback_weight",
                                      normalize_population = TRUE) {
  if (!is.character(weight_vars) || !length(weight_vars) || anyNA(weight_vars) ||
      !all(weight_vars %in% names(data)) || !all(weight_vars %in% names(pop_dist))) {
    stop("weight_vars must name columns in data and pop_dist.", call. = FALSE)
  }
  if (!population_pct_col %in% names(pop_dist) || weight_col %in% names(data)) {
    stop("population_pct_col must exist and weight_col must be new.", call. = FALSE)
  }
  pop.dist <- dplyr::transmute(pop_dist, dplyr::across(dplyr::all_of(weight_vars)),
                               population_pct = .data[[population_pct_col]])
  if (!is.numeric(pop.dist$population_pct) || any(!is.finite(pop.dist$population_pct)) ||
      any(pop.dist$population_pct < 0) || anyDuplicated(pop.dist[weight_vars])) {
    stop("Population distribution must have unique keys and finite non-negative proportions.", call. = FALSE)
  }
  total <- sum(pop.dist$population_pct)
  if (!is.finite(total) || total <= 0) stop("Population proportions must have a positive total.", call. = FALSE)
  if (normalize_population) pop.dist$population_pct <- pop.dist$population_pct / total
  sample.dist <- dplyr::ungroup(data) %>%
    dplyr::count(dplyr::across(dplyr::all_of(weight_vars)), name = ".sample_n") %>%
    dplyr::mutate(.sample_pct = .sample_n / sum(.sample_n))
  if (nrow(dplyr::anti_join(sample.dist, pop.dist, by = weight_vars)) > 0) {
    stop("Every sample combination must be present in pop_dist.", call. = FALSE)
  }
  weights <- dplyr::left_join(sample.dist, pop.dist, by = weight_vars) %>%
    dplyr::mutate(.weight = population_pct / .sample_pct) %>%
    dplyr::select(dplyr::all_of(weight_vars), .weight)
  dplyr::left_join(dplyr::ungroup(data), weights, by = weight_vars) %>%
    dplyr::rename(!!weight_col := .weight)
}
