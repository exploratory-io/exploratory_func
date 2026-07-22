
# Simplified preprocess_regression_data_before_sample for factor analysis (exp_factanal) PCA (do_prcomp). TODO: Consider using it for k-means too.
preprocess_factanal_data_before_sample <- function(df, predictor_cols) {
  # Remove all-NA-or-Inf columns.
  # NOTE: This has to be done bofore filtering predictor numeric NAs. Otherwise, all the rows could be filtered out.
  cols <- predictor_cols
  for (col in predictor_cols) {
    if(all(is.na(df[[col]]) | is.infinite(df[[col]]))){
      # remove columns if they are all NA or Inf
      cols <- setdiff(cols, col)
      df[[col]] <- NULL # drop the column so that SMOTE will not see it. 
    }
  }
  if (length(cols) == 0) {
    stop("No column is left after removing columns with only NA or Inf values.")
  }

  # To avoid unused factor level that causes margins::marginal_effects() to fail, filtering operation has
  # to be done before factor level adjustments.
  # This is done before sampling so that we will end up with more valid rows in the end.
  for (col in cols) {
    df <- df %>% dplyr::filter(!is.na(!!rlang::sym(col)) & !is.infinite(!!rlang::sym(col)))
  }
  if (nrow(df) == 0) {
    stop("No row is left after removing rows with NA/Inf.")
  }
  attr(df, 'predictors') <- cols
  df
}

# Threshold configuration for the Factor Analysis report judgments (issue #37018).
# Kept as an internal function (not a mutable global) so helpers can pick it up by default.
factanal_report_config <- function() {
  list(
    loading_salient = 0.40, loading_strong = 0.60, loading_crossload_diff = 0.20,
    loading_near = 0.30, loading_near_diff = 0.25,
    communality_low = 0.40, communality_good = 0.60, communality_too_high = 0.95,
    kmo_poor = 0.50, kmo_min = 0.60, kmo_good = 0.70, kmo_great = 0.80,
    factor_corr_medium = 0.30, factor_corr_high = 0.50,
    p_value_threshold = 0.05
  )
}

# All judge_* helpers emit English-canonical labels and a language-neutral `status` token.
# The desktop/server client translates the label (VizUtil message tables) and composes tooltips
# from the token + params. No natural language for translation lives in R output. (issue #37018)
judge_kmo <- function(kmo, cfg = factanal_report_config()) {
  if (length(kmo) != 1L || is.na(kmo)) {
    return(list(label = "Not Available", status = "na", description = "KMO could not be computed."))
  }
  if (kmo >= cfg$kmo_great) list(label = "Very Suitable", status = "great", description = "The variables have a correlation structure well suited for factor analysis.")
  else if (kmo >= cfg$kmo_good) list(label = "Suitable", status = "good", description = "The variables have a correlation structure suited for factor analysis.")
  else if (kmo >= cfg$kmo_min) list(label = "Marginally Suitable", status = "min", description = "Factor analysis is possible, but interpret the results with care.")
  else if (kmo >= cfg$kmo_poor) list(label = "Somewhat Weak", status = "poor", description = "The correlation structure is somewhat weak for factor analysis.")
  else list(label = "Possibly Unsuitable", status = "below", description = "Consider reselecting the variables.")
}

judge_bartlett <- function(p_value, cfg = factanal_report_config()) {
  if (length(p_value) != 1L || is.na(p_value)) {
    return(list(label = "Not Available", status = "na", description = "Bartlett's test could not be computed."))
  }
  if (p_value < cfg$p_value_threshold) list(label = "Suitable", status = "suitable", description = "There is enough correlation among the variables for factor analysis.")
  else list(label = "Caution", status = "caution", description = "The correlation among the variables may be weak.")
}

judge_communality <- function(communality, cfg = factanal_report_config()) {
  if (length(communality) != 1L || is.na(communality)) {
    return(list(label = "Not Available", status = "na"))
  }
  # A communality > 1 (i.e. negative uniqueness) is a Heywood case: NOT "explained more than
  # 100%", but a sign that the estimation is unstable / the solution is improper. Flag it before
  # the "too high" check so it gets its own, more serious judgment. (issue #37018)
  if (communality > 1) return(list(label = "Possibly improper solution", status = "improper"))
  if (communality >= cfg$communality_too_high) list(label = "Too high (caution)", status = "too_high")
  else if (communality >= cfg$communality_good) list(label = "Well explained", status = "good")
  else if (communality >= cfg$communality_low) list(label = "Moderately explained", status = "moderate")
  else list(label = "Weakly explained", status = "weak")
}

# loadings: named numeric vector, names are "Factor 1", "Factor 2", ...
# Returns label (English), status token, and params for the client-composed tooltip.
judge_loading <- function(loadings, cfg = factanal_report_config()) {
  loading_names <- names(loadings)
  loadings <- as.numeric(loadings)
  if (is.null(loading_names)) {
    loading_names <- rep("", length(loadings))
  }
  names(loadings) <- loading_names
  valid <- is.finite(loadings)
  if (!any(valid)) {
    return(list(label = "Not Available", status = "na",
                primary_factor = "", secondary_factors = "", direction = "na"))
  }

  loadings <- loadings[valid]
  abs_loadings <- abs(loadings)
  ord <- order(abs_loadings, decreasing = TRUE)
  primary_idx <- ord[1]
  secondary_idx <- if (length(ord) >= 2L) ord[2] else NA_integer_
  primary_factor <- names(loadings)[primary_idx]
  secondary_factor <- if (is.na(secondary_idx)) "" else names(loadings)[secondary_idx]
  primary_loading <- loadings[[primary_idx]]
  secondary_abs <- if (is.na(secondary_idx)) 0 else abs(loadings[[secondary_idx]])
  primary_abs <- abs(primary_loading)
  diff <- primary_abs - secondary_abs
  salient <- names(loadings)[abs_loadings >= cfg$loading_salient]
  num_salient <- length(salient)
  direction <- if (primary_loading >= 0) "positive" else "negative"
  neg_suffix <- if (primary_loading < 0) " (negative)" else ""

  if (primary_abs < cfg$loading_salient) {
    return(list(label = "Hard to interpret", status = "low_loading",
                primary_factor = primary_factor, secondary_factors = "", direction = direction))
  }
  if (num_salient >= 2 && diff < cfg$loading_crossload_diff) {
    return(list(label = "Cross-loading / ambiguous", status = "ambiguous_crossload",
                primary_factor = primary_factor,
                secondary_factors = paste(salient, collapse = ","), direction = direction))
  }
  if (num_salient >= 2) {
    others <- setdiff(salient, primary_factor)
    return(list(label = paste0("Leans to ", primary_factor, " / cross-loading"), status = "crossload",
                primary_factor = primary_factor,
                secondary_factors = paste(others, collapse = ","), direction = direction))
  }
  if (secondary_abs >= cfg$loading_near && diff < cfg$loading_near_diff) {
    return(list(label = paste0("Leans to ", primary_factor, " / somewhat ambiguous"), status = "near_crossload",
                primary_factor = primary_factor,
                secondary_factors = secondary_factor, direction = direction))
  }
  if (primary_abs >= cfg$loading_strong) {
    return(list(label = paste0("Strongly related to ", primary_factor, neg_suffix), status = "strong",
                primary_factor = primary_factor, secondary_factors = "", direction = direction))
  }
  list(label = paste0("Moderately related to ", primary_factor, neg_suffix), status = "moderate",
       primary_factor = primary_factor, secondary_factors = "", direction = direction)
}

# Horn's parallel analysis: compares actual correlation-matrix eigenvalues against the
# quantile of eigenvalues from random normal data of the same shape. (issue #37018 spec 3.4)
#' @param cor_type Correlation type the whole analysis uses. For anything other than "pearson" both the
#'        actual and the random-data eigenvalues are computed with that same correlation, so the parallel
#'        analysis never silently falls back to Pearson. (issue #26623)
#' @param cor_matrix Correlation matrix already built for the analysis. Reused for the actual eigenvalues
#'        so the scree/parallel chart matches the matrix `fa()` was fitted on.
compute_parallel_analysis <- function(x, n_iter = 100, quantile_prob = 0.95,
                                      cor_type = "pearson", cor_matrix = NULL) {
  if (length(n_iter) != 1L || is.na(n_iter) || n_iter < 1 || n_iter != as.integer(n_iter)) {
    stop("n_iter must be a positive integer")
  }
  if (length(quantile_prob) != 1L || is.na(quantile_prob) ||
      quantile_prob < 0 || quantile_prob > 1) {
    stop("quantile_prob must be between 0 and 1")
  }
  x <- as.data.frame(x)
  n <- nrow(x)
  p <- ncol(x)
  if (!identical(cor_type, "pearson")) {
    # Polychoric/tetrachoric/mixed: the null distribution is built by shuffling each column
    # independently, which keeps every variable's category marginals but destroys the association,
    # and the SAME correlation method is applied to it. Drawing normal deviates instead would
    # compare a polychoric eigenvalue against a Pearson one.
    actual_eigen <- if (!is.null(cor_matrix)) {
      eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values
    } else {
      eigen(build_factor_correlation(x, cor_type)$correlation, symmetric = TRUE, only.values = TRUE)$values
    }
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
    set.seed(1234)
    on.exit({
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    random_eigen_mat <- replicate(n_iter, {
      shuffled <- as.data.frame(lapply(x, function(col) col[sample.int(n)]))
      res <- build_factor_correlation(shuffled, cor_type)
      eigen(res$correlation, symmetric = TRUE, only.values = TRUE)$values
    })
    random_threshold <- apply(random_eigen_mat, 1, stats::quantile, probs = quantile_prob)
    return(list(
      recommended_n = sum(actual_eigen > random_threshold),
      table = tibble::tibble(
        factor_number = seq_along(actual_eigen),
        actual_eigenvalue = actual_eigen,
        random_eigenvalue_threshold = random_threshold
      )
    ))
  }
  # Reuse the analysis's own matrix when it was handed one, so the scree/parallel chart and the
  # fit can never be based on two separately-computed Pearson matrices. (issue #26623)
  actual_eigen <- if (!is.null(cor_matrix)) {
    eigen(cor_matrix, only.values = TRUE)$values
  } else {
    eigen(cor(x, use = "pairwise.complete.obs"), only.values = TRUE)$values
  }
  # Snapshot the RNG so this null-distribution draw does not perturb the outer deterministic
  # stream that later groups' sampling relies on. Restore afterward.
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(1234)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  random_eigen_mat <- replicate(n_iter, {
    rd <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
    eigen(cor(rd), only.values = TRUE)$values
  })
  random_threshold <- apply(random_eigen_mat, 1, stats::quantile, probs = quantile_prob)
  list(
    recommended_n = sum(actual_eigen > random_threshold),
    table = tibble::tibble(
      factor_number = seq_along(actual_eigen),
      actual_eigenvalue = actual_eigen,
      random_eigenvalue_threshold = random_threshold
    )
  )
}

#' Function for Factor Analysis Analytics View
#' @export
#' @param cor_type Correlation used for the whole analysis. "auto" (default) picks Pearson /
#'        Tetrachoric / Polychoric / Mixed from the variable types and response distributions,
#'        "pearson" and "polychoric" force that correlation. The SAME matrix drives fa(), KMO,
#'        Bartlett, the eigenvalues, the parallel analysis and the factor scores. (issue #26623)
#' @param parallel_n_iter Iterations of the parallel analysis null distribution (default 100 for
#'        every correlation type). Polychoric-family correlations re-estimate the correlation on
#'        each iteration, so this is the dominant cost of a polychoric run.
exp_factanal <- function(df, ..., nfactors = 2, fm = "minres", scores = "regression", rotate = "none", max_nrow = NULL, seed = 1,
                         cor_type = "auto", parallel_n_iter = NULL) {
  # this evaluates select arguments like starts_with
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  if (length(selected_cols) < nfactors) {
    stop("EXP-ANA-5 :: [] :: You need to set the number of factors to be less than or equal to the number of variables.")
  }

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
    # sample the data for quicker turn around on UI,
    # if data size is larger than specified max_nrow.
    sampled_nrow <- NULL
    if (!is.null(max_nrow) && nrow(df) > max_nrow) {
      # Record that sampling happened.
      sampled_nrow <- max_nrow
      df <- df %>% sample_rows(max_nrow)
    }

    # As the name suggests, this preprocessing function was originally designed to be done
    # before sampling, but we found that for this factor analysis function, that makes the
    # process as a whole slower in the cases we tried. So, we are doing this after sampling.
    filtered_df <- preprocess_factanal_data_before_sample(df, selected_cols)
    selected_cols <- attr(filtered_df, 'predictors') # predictors are updated (removed) in preprocess_factanal_data_before_sample. Sync with it.

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
    # Decide which correlation the whole analysis runs on, and build that ONE matrix. Every
    # downstream computation below reads it, so the report can never mix Pearson and Polychoric
    # (e.g. a polychoric fa() next to a Pearson KMO or parallel analysis). (issue #26623)
    selection <- select_factor_correlation_type(cleaned_df)
    resolved <- resolve_factanal_correlation_type(cor_type, selection)
    # An unsupported variable combination (a nominal category, a constant column) is unsupported
    # whichever correlation was asked for -- picking Pearson manually must not smuggle a nominal
    # column in as arbitrary integer codes. So gate on the SELECTION, not the resolved type.
    if (identical(selection$selected_method, "unsupported") || identical(resolved$type, "unsupported")) {
      # EXP-ANA-35 carries the offending column names as its params, so the client can name them.
      # (EXP-ANA-6 is PCA's reserved-column-name error -- do not reuse it here.)
      unsupported_vars <- selection$variable_summary$variable[
        selection$variable_summary$detected_type %in% c("nominal", "invalid")]
      stop(paste0("EXP-ANA-35 :: ",
                  jsonlite::toJSON(paste(unsupported_vars, collapse = ", ")),
                  " :: ", selection$reason))
    }
    # Category-ordered numeric coding. For an all-numeric data frame this is a no-op, so the
    # Pearson path stays bit-for-bit what it was before this change.
    encoded_df <- encode_factanal_data(cleaned_df, selection)
    # The correlation family that was ASKED for. It differs from resolved$type only when the
    # estimation fails and we degrade to Pearson, and the diagnostics table needs the asked-for
    # one so the failure is reported instead of silently disappearing with the table.
    requested_family <- resolved$type

    if (identical(resolved$type, "pearson")) {
      fit <- psych::fa(encoded_df, nfactors = nfactors, fm = fm, scores = scores, rotate = rotate)
      cor_result <- list(correlation = cor(encoded_df), thresholds = NULL, type = "pearson",
                         smoothed = FALSE, warnings = character(), failed = FALSE)
      cor_mat <- cor_result$correlation
    } else {
      # Correlation-matrix based fit. n.obs is mandatory here: without it psych assumes 100 rows
      # and every sample-size dependent fit index would be wrong.
      cor_result <- build_factor_correlation(encoded_df, requested_family)
      cor_mat <- cor_result$correlation
      if (isTRUE(cor_result$failed)) {
        # build_factor_correlation already degraded to Pearson; keep the reported type honest.
        # requested_family stays as asked so the diagnostics table can still REPORT the failure,
        # and degraded_from lets the report say WHICH correlation failed instead of claiming the
        # variables were treated as continuous on purpose.
        resolved$type <- "pearson"
        resolved$degraded_from <- requested_family
        resolved$reason <- sprintf("%s could not be estimated, so Pearson correlation was used instead.",
                                   factanal_correlation_label(requested_family))
      }
      fit <- psych::fa(cor_mat, nfactors = nfactors, n.obs = nrow(encoded_df), fm = fm, rotate = rotate)
      # fa() on a correlation matrix never returns scores, so compute them from the same matrix,
      # honoring the "Type of Scores" property instead of hardcoding one method.
      fit$scores <- tryCatch({
        psych::factor.scores(as.matrix(encoded_df), fit, rho = cor_mat,
                             method = factanal_score_method(scores))$scores
      }, error = function(e) NULL)
      if (is.null(fit$scores)) {
        # Keep the shape so the score-consuming outputs (Data tab, biplot) degrade to NA columns
        # instead of aborting inside broom's augment().
        fit$scores <- matrix(NA_real_, nrow = nrow(encoded_df), ncol = nfactors)
      }
    }

    fit$correlation <- cor_mat # For creating scree plot later.
    fit$correlation_type <- resolved$type
    fit$correlation_is_auto <- isTRUE(resolved$auto)
    fit$correlation_reason <- resolved$reason
    fit$correlation_degraded_from <- if (is.null(resolved$degraded_from)) "" else resolved$degraded_from
    fit$correlation_selection <- selection
    fit$correlation_result <- cor_result[setdiff(names(cor_result), "correlation")]
    fit$nfactors_requested <- nfactors
    fit$df <- filtered_df # add filtered df to model so that we can bind_col it for output. It needs to be the filtered one to match row number.
    fit$grouped_cols <- grouped_cols
    fit$sampled_nrow <- sampled_nrow
    # Suitability / factor-count diagnostics for the redesigned report (issue #37018).
    # psych is already a hard dependency, so KMO/cortest.bartlett add nothing new. Each is
    # guarded so a singular/degenerate correlation matrix degrades to NA instead of aborting.
    fit$n_rows_used <- nrow(cleaned_df)
    fit$n_variables <- ncol(cleaned_df)
    # KMO and Bartlett take the correlation MATRIX (never the raw data): passing raw data would
    # make psych recompute a Pearson correlation internally and silently contradict a polychoric
    # fit. cortest.bartlett also needs n explicitly, or it assumes n = 100. (issue #26623)
    fit$kmo <- tryCatch(as.numeric(psych::KMO(cor_mat)$MSA), error = function(e) NA_real_)
    fit$bartlett <- tryCatch(psych::cortest.bartlett(cor_mat, n = nrow(encoded_df)), error = function(e) NULL)
    n_iter <- if (!is.null(parallel_n_iter)) parallel_n_iter else 100
    fit$parallel <- tryCatch(compute_parallel_analysis(encoded_df, n_iter = n_iter,
                                                       cor_type = resolved$type, cor_matrix = cor_mat),
                             error = function(e) NULL)
    # Keyed off the REQUESTED family, so a failed polychoric estimation still shows the table with
    # its "Estimation failed" row rather than hiding the only signal the user has.
    fit$cor_diagnostics <- if (identical(requested_family, "pearson")) NULL else {
      tryCatch(compute_polychoric_diagnostics(encoded_df, cor_result, selection), error = function(e) NULL)
    }
    class(fit) <- c("fa_exploratory", class(fit))
    fit
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

glance.fa_exploratory <- function(x, pretty.name = FALSE, ...) {
  # glance.factanal works on psych::fa due to compatibility kept to some degree. TODO: Extract and output more info from psych::fa
  res <- broom:::glance.factanal(x) %>% dplyr::select(-n, -converged, -method) # But converged is NULL.
  res <- res %>% dplyr::mutate(variance=total.variance*length(x$communality), ratio_variance=total.variance)
  res <- res %>% dplyr::select(`Factors`=n.factors, `Variance Explained (Ratio)`=ratio_variance, `Variance Explained`=variance, `Chi-Square`=statistic, `P Value`=p.value, `DF`=df, `Rows`=nobs)
  # Append fit-quality columns for the report's supplementary section (issue #37018). NA-safe:
  # older psych fits may not carry every field. Append-only keeps the client's name-indexed reads working.
  safe_num <- function(v) if (is.null(v) || length(v) == 0) NA_real_ else suppressWarnings(as.numeric(v)[1])
  rmsea_val <- if (!is.null(x$RMSEA)) safe_num(x$RMSEA[["RMSEA"]]) else NA_real_
  res <- res %>% dplyr::mutate(
    `Method` = x$fm,
    `Rotation` = if (!is.null(x$rotation)) x$rotation else NA_character_,
    `RMSR` = safe_num(x$rms),
    `RMSEA` = rmsea_val,
    `TLI` = safe_num(x$TLI),
    `BIC` = safe_num(x$BIC)
  )
  res
}

#' extracts results from psych::fa object as a dataframe
#' @export
#' @param n_sample Sample number for biplot. Default 5000, which is the default of our scatter plot.
#'        we use it for gathered_data for parallel coordinates too. sampling is applied before gather.
tidy.fa_exploratory <- function(x, type="loadings", n_sample=NULL, pretty.name=FALSE, ...) {
  # Mapping from factorization method and prefix of loading output column name, e.g. MR1, MR2...
  factor_loading_prefix_mapping <- c(minres="MR",
                                     ml="ML",
                                     pa="MR",
                                     ols="X",
                                     wls="WLS",
                                     gls="GLS",
                                     minchi="MC",
                                     minrank="MRFA",
                                     alpha="MR")
  factor_loading_prefix <- factor_loading_prefix_mapping[x$fm]
  names(factor_loading_prefix) <- NULL

  # Mapping from factorization method and prefix of score output column name, e.g. MR1, MR2...
  # Only difference from factor_loading_prefix is the ols case.
  factor_score_prefix_mapping <- c(minres="MR",
                                   ml="ML",
                                   pa="MR",
                                   ols="V",
                                   wls="WLS",
                                   gls="GLS",
                                   minchi="MC",
                                   minrank="MRFA",
                                   alpha="MR")
  factor_score_prefix <- factor_score_prefix_mapping[x$fm]
  names(factor_score_prefix) <- NULL

  # Mapping from factorization method and prefix of the first factor of inter-factor correlation result.
  # The prefix for the second factor of inter-factor correlation result is the same as factor_score_prefix.
  # Only difference from factor_loading_prefix is the ols case.
  factor_correlation_prefix_mapping <- c(minres="MR",
                                         ml="ML",
                                         pa="MR",
                                         ols="",
                                         wls="WLS",
                                         gls="GLS",
                                         minchi="MC",
                                         minrank="MRFA",
                                         alpha="MR")
  factor_correlation_prefix <- factor_correlation_prefix_mapping[x$fm]
  names(factor_correlation_prefix) <- NULL

  n_factor <- x$factors # Number of factors.

  if (type == "screeplot") {
    eigen_res <- eigen(x$correlation, only.values = TRUE) # Cattell's scree plot is eigenvalues of correlation/covariance matrix.
    res <- tibble::tibble(factor=1:length(eigen_res$values), eigenvalue=eigen_res$values)
  }
  else if (type == "variances") {
    res <- tibble::as_tibble(t(x$Vaccounted)) %>% dplyr::mutate(Factor=as.factor(1:n()), `% Variance`=100*`Proportion Var`, `Cummulated % Variance`=100*`Cumulative Var`)
  }
  else if (type == "loadings") {
    res <- broom:::tidy.factanal(x) # TODO: This just happens to work. Revisit.
    # Column order of tidy.factanal() is in the order of importance, and not necessarily same as the order of the IDs, like this.
    # variable    uniqueness     MR1     MR2     MR5      MR3      MR4
    # Here we are renaming the IDs of the factors so that IDs correspond with the order of importance.
    colnames(res) <-c("variable","uniqueness",stringr::str_c(factor_loading_prefix,1:n_factor))
    # Reorder columns so that the result of pivot_longer is in the order we want.
    res <- res %>% dplyr::relocate(variable, uniqueness, .after = everything())
    # With the way psych::fa code is, x$communalities can have different value, but x$communality should always have this relationship with x$uniqueness,
    # which is the source of uniqueness in the output from tidy.factanal.
    res <- res %>% dplyr::mutate(communality=1-uniqueness)
    res <- res %>% tidyr::pivot_longer(cols=c(starts_with(factor_loading_prefix), "communality", "uniqueness"), names_to="factor", values_to="value")
    res <- res %>% dplyr::mutate(factor = dplyr::case_when(factor=="communality"~"Communality", factor=="uniqueness"~"Uniqueness", TRUE~stringr::str_replace(factor,paste0("^", !!factor_loading_prefix),"Factor "))) # e.g. replaces "MR2" with "Factor 2"
    res <- res %>% dplyr::mutate(factor = forcats::fct_inorder(factor)) # fct_inorder is to make order on chart right, e.g. Factor 2 before Factor 10
    # Set factor level to the variable column based on the top factor and value within the variables with the same top factor.
    res <- res %>% dplyr::mutate(factor = forcats::fct_drop(factor), variable=forcats::fct_reorder2(variable, factor, value, .fun=function(x,y) {
      df=(tibble::tibble(factor=x,value=abs(y)) %>% dplyr::filter(factor %nin% c('Communality','Uniqueness')) %>% dplyr::arrange(factor)); max(df$value) - 10*which.max(df$value)
    }, .desc=TRUE))
  }
  else if (type == "biplot") {
    if (n_factor < 2) {
      # Biplot plots Factor 1 against Factor 2, so it needs at least 2 factors. With nfactors=1
      # there is no "Factor 2" to build factor_2_loading_col/factor_2_score_col from below.
      # Degrade gracefully -- mirroring the type=="correlation" branch's handling of an
      # unavailable x$Phi above -- instead of erroring on a missing column. Keep the same column
      # shape the client's `tidy_rowwise(model, type="biplot") %>% dplyr::rename(...)` expects, so
      # the rename does not itself fail on a missing column. (issue #30798)
      return(tibble::tibble(.factor_1 = numeric(0), .factor_2 = numeric(0), .factor_2_variable = numeric(0)))
    }
    factor_1_loading_col <- paste0(factor_loading_prefix, "1")
    factor_2_loading_col <- paste0(factor_loading_prefix, "2")
    factor_1_score_col <- paste0(factor_score_prefix, "1")
    factor_2_score_col <- paste0(factor_score_prefix, "2")
    scores_df <- broom:::augment.factanal(x)
    scores_df <- scores_df %>% select(-.rownames) # augment.factanal seems to always return row names in .rownames column.
    # Rename the IDs of the factors so that IDs correspond with the order of importance, which is the order in the augment.factanal output.
    colnames(scores_df) <- stringr::str_c(factor_score_prefix, 1:n_factor)
    loadings_df <- broom:::tidy.factanal(x)
    # Rename the IDs of the factors so that IDs correspond with the order of importance, which is the order in the tidy.factanal output.
    colnames(loadings_df) <-c("variable", "uniqueness", stringr::str_c(factor_loading_prefix, 1:n_factor))

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
          res[[orig_col]] <- forcats::fct_na_value_to_level(as.factor(res[[orig_col]]), level="(NA)")
        }
        else {
          # make logical columns into factor with NA level, so that legend will show NA.
          res[[orig_col]] <- forcats::fct_na_value_to_level(factor(res[[orig_col]], levels = c("TRUE","FALSE")), level="(NA)")
        }
      }
    }

    res <- res %>% dplyr::bind_cols(scores_df)
    res <- res %>% sample_rows(score_n_sample)

    # calculate scale ratio for displaying loadings on the same chart as scores.
    loadings_matrix <- as.matrix(loadings_df %>% dplyr::select(!!rlang::sym(factor_1_loading_col), !!rlang::sym(factor_2_loading_col)))
    max_abs_loading <- max(abs(loadings_matrix))
    max_abs_score <- max(abs(c(res[[factor_1_score_col]], res[[factor_2_score_col]])))
    scale_ratio <- max_abs_score/max_abs_loading

    res <- res %>% dplyr::rename(.factor_2=!!factor_2_score_col, .factor_1=!!factor_1_score_col) # name to appear at legend for dots in scatter plot.

    # loadings_df is for the variable lines in biplot. It will be later merged (bind_rows) with res.
    # It shares x-axis column .factor_1 with res, and has separate y-axis column .factor_2_variable.
    # scale loading_matrix so that the scale of measures and data points matches in the scatter plot.
    loadings_df <- loadings_df %>% dplyr::select(-uniqueness) # uniqueness column is not necessary for biplot. Remove it to avoid name conflict as much as possible.
    loadings_df <- loadings_df %>% dplyr::rename(.factor_1=!!factor_1_loading_col, .factor_2_variable=!!factor_2_loading_col, .variable=variable) # use different column name for PC2 of measures.
    loadings_df <- loadings_df %>% dplyr::mutate(.factor_1=.factor_1*scale_ratio, .factor_2_variable=.factor_2_variable*scale_ratio)
    loadings_df0 <- loadings_df %>% dplyr::mutate(.factor_1=0, .factor_2_variable=0) # Create rows for origin of coordinates.
    loadings_df <- loadings_df0 %>% dplyr::bind_rows(loadings_df)

    res <- res %>% dplyr::bind_rows(loadings_df)
    # fill group_by column so that Repeat By on chart works fine. loadings_df does not have values for the group_by column.
    res <- res %>% tidyr::fill(x$grouped_cols)
    res
  }
  else if (type == "correlation") {
    if (!is.null(x$Phi)) {
      # If x$Phi is available, show its content in the pivot table.
      # TODO: print.psych.fa.R seems to have logic to calculate it even when x$Phi is not there. Should we do the same here?
      # But it seems x$Phi is there for most of the oblique rotations. #25435
      res <- as.data.frame(x$Phi) %>% tibble::rownames_to_column("factor1") %>% tidyr::pivot_longer(cols=starts_with(factor_score_prefix), names_to="factor2", values_to="correlation")
      res <- res %>% dplyr::mutate(factor1=stringr::str_replace(factor1, paste0("^", factor_correlation_prefix), "Factor "),
                                   factor2=stringr::str_replace(factor2, paste0("^", factor_score_prefix), "Factor "))
      # fct_relevel is to make order on chart right, e.g. Factor 2 before Factor 10
      # Since the factors does not appear in order in x$Phi in some cases with Promax and minch, simply applying fct_inorder is not enough, and fct_relevel is required.
      res <- res %>% dplyr::mutate(factor1=forcats::fct_relevel(factor1, stringr::str_c("Factor ",1:n_distinct(factor1))),
                                   factor2=forcats::fct_relevel(factor2, stringr::str_c("Factor ",1:n_distinct(factor2))))
    }
    else {
      # Return an empty data frame.
      res <- tibble::tibble()
    }
  }
  else if (type == "loadings_wide") {
    # Wide judged loadings table (issue #37018). One row per variable: Factor 1..N loadings plus a
    # Judgement label and hidden token/param columns the client uses to compose per-cell tooltips.
    wide <- broom:::tidy.factanal(x)
    colnames(wide) <- c("variable", "uniqueness", stringr::str_c(factor_loading_prefix, 1:n_factor))
    wide <- wide %>% dplyr::select(-uniqueness)
    new_names <- stringr::str_c("Factor ", 1:n_factor)
    colnames(wide) <- c("variable", new_names)
    judgements <- purrr::map(seq_len(nrow(wide)), function(i) {
      loadings <- unlist(wide[i, new_names], use.names = FALSE)
      names(loadings) <- new_names
      judge_loading(loadings)
    })
    res <- wide %>% dplyr::mutate(
      Judgement = purrr::map_chr(judgements, "label"),
      judgement_status = purrr::map_chr(judgements, "status"),
      primary_factor = purrr::map_chr(judgements, "primary_factor"),
      secondary_factors = purrr::map_chr(judgements, "secondary_factors"),
      direction = purrr::map_chr(judgements, "direction")
    )
  }
  else if (type == "communalities") {
    comm <- x$communality
    res <- tibble::tibble(variable = names(comm), Communality = as.numeric(comm), Uniqueness = as.numeric(x$uniquenesses))
    judgements <- purrr::map(res$Communality, judge_communality)
    res <- res %>% dplyr::mutate(
      Judgement = purrr::map_chr(judgements, "label"),
      judgement_status = purrr::map_chr(judgements, "status")
    ) %>% dplyr::arrange(dplyr::desc(Communality))
  }
  else if (type == "communalities_long") {
    # Long form for the 100%-stacked communality/uniqueness bar chart. Normally the two components
    # sum to 1. In a Heywood case (communality > 1) uniqueness would go negative, which would break
    # the stack. Per spec: the bar is CLIPPED at 100% (via the chart's 0-100 value-axis range) but
    # the numeric label shows the ACTUAL communality (e.g. 101%), so communality is left UNCAPPED
    # here; uniqueness is clamped to 0 so it is never negative. (#37018)
    comm <- as.numeric(x$communality)
    # Order the bars by communality DESCENDING (highest communality at the TOP of the horizontal
    # bar chart). Ratios are on a 0-100 percentage scale. Component is Communality-first so it
    # stacks/colors/legends before Uniqueness.
    var_factor <- forcats::fct_reorder(names(x$communality), comm, .desc = TRUE)
    res <- tibble::tibble(
      variable = var_factor,
      Communality = comm * 100,
      Uniqueness = pmax(1 - comm, 0) * 100
    ) %>%
      tidyr::pivot_longer(cols = c("Communality", "Uniqueness"), names_to = "Component", values_to = "Ratio") %>%
      dplyr::mutate(Component = forcats::fct_relevel(as.factor(Component), "Communality", "Uniqueness"))
  }
  else if (type == "suitability") {
    scalar_num <- function(v) {
      if (is.null(v) || length(v) != 1L) NA_real_ else suppressWarnings(as.numeric(v))
    }
    kmo <- scalar_num(x$kmo)
    bart <- x$bartlett
    p <- if (is.null(bart)) NA_real_ else scalar_num(bart$p.value)
    kj <- judge_kmo(kmo)
    bj <- judge_bartlett(p)
    kmo_val <- if (is.na(kmo)) "N/A" else format(round(kmo, 2), nsmall = 2)
    bart_val <- if (is.na(p)) "N/A" else if (p < 0.001) "p < 0.001" else paste0("p = ", format(round(p, 3), nsmall = 3))
    rows_used <- if (length(x$n_rows_used) == 1L && !is.na(x$n_rows_used)) as.character(x$n_rows_used) else "N/A"
    variables_used <- if (length(x$n_variables) == 1L && !is.na(x$n_variables)) as.character(x$n_variables) else "N/A"
    res <- tibble::tibble(
      Metric = c("KMO", "Bartlett's Test of Sphericity", "Rows Used", "Variables Used"),
      Value = c(kmo_val, bart_val, rows_used, variables_used),
      Judgement = c(kj$label, bj$label, "", ""),
      Description = c(kj$description, bj$description,
                      "Number of rows used after removing missing values.",
                      "Number of variables used in the analysis."),
      status = c(kj$status, bj$status, "", "")
    )
  }
  else if (type == "analysis_method") {
    # Report header table: what the numbers below were actually computed with. (issue #26623)
    cor_type <- if (is.null(x$correlation_type)) "pearson" else x$correlation_type
    rotation <- if (!is.null(x$rotation)) x$rotation else "none"
    res <- tibble::tibble(
      # NOTE: "Target Variables" / "Data Rows" instead of the shorter "Variables" / "Rows":
      # the client's shared translation map already binds those two keys to different wordings
      # for other tables, and a direct-map key can only have one translation. (issue #26623)
      Item = c("Correlation", "Factor Extraction Method", "Rotation", "Target Variables", "Data Rows"),
      Value = c(
        factanal_correlation_label(cor_type),
        factanal_extraction_method_label(x$fm),
        factanal_rotation_label(rotation),
        if (length(x$n_variables) == 1L && !is.na(x$n_variables)) as.character(x$n_variables) else "N/A",
        if (length(x$n_rows_used) == 1L && !is.na(x$n_rows_used)) as.character(x$n_rows_used) else "N/A"
      ),
      # Hidden columns. The client reads them to bind the report's explanation text; they are not
      # part of the rendered Item/Value table.
      correlation_type = cor_type,
      correlation_is_auto = isTRUE(x$correlation_is_auto),
      # Non-empty when the requested correlation could not be estimated and the fit fell back to
      # Pearson, so the report can say so instead of inventing a rationale.
      degraded_from = if (is.null(x$correlation_degraded_from)) "" else x$correlation_degraded_from,
      # Whether suggesting Polychoric makes sense for this data at all.
      polychoric_available = factanal_polychoric_available(x$correlation_selection),
      # Language-neutral tokens for the selector's warnings; the client renders the localized text.
      warning_tokens = paste(factanal_selection_warning_tokens(x$correlation_selection), collapse = ","),
      # TRUE also when a polychoric estimation failed and the fit degraded to Pearson, so the
      # report still shows the diagnostics table that reports the failure.
      has_diagnostics = !is.null(x$cor_diagnostics) && nrow(x$cor_diagnostics) > 0,
      reason = if (is.null(x$correlation_reason)) "" else x$correlation_reason
    )
  }
  else if (type == "cor_diagnostics") {
    # Polychoric-family data suitability diagnostics. Empty tibble (with the same columns) when
    # the analysis ran on Pearson, so the client can hide the section. (issue #26623)
    res <- if (is.null(x$cor_diagnostics)) {
      tibble::tibble(Diagnostic = character(), Judgement = character(),
                     Description = character(), status = character())
    } else {
      x$cor_diagnostics
    }
  }
  else if (type == "factor_count") {
    eig <- eigen(x$correlation, only.values = TRUE)$values
    kaiser_n <- sum(eig > 1)
    par <- x$parallel
    parallel_rec <- if (is.null(par)) "Not available" else as.character(par$recommended_n)
    res <- tibble::tibble(
      Method = c("Kaiser Criterion", "Parallel Analysis", "Scree Plot"),
      `Recommended Number of Factors` = c(as.character(kaiser_n), parallel_rec, "Check the chart"),
      Description = c(
        "Number of factors with an eigenvalue greater than 1.",
        "Number of factors whose eigenvalue exceeds the random-data eigenvalue.",
        "Look for the point where the eigenvalue drop levels off (the elbow)."
      )
    )
  }
  else if (type == "parallel_screeplot") {
    eig <- eigen(x$correlation, only.values = TRUE)$values
    par <- x$parallel
    threshold <- if (is.null(par)) rep(NA_real_, length(eig)) else par$table$random_eigenvalue_threshold
    length(threshold) <- length(eig) # pad/truncate to align with eigenvalue count
    res <- tibble::tibble(Factor = 1:length(eig), Eigenvalue = eig, `Random Data Eigenvalue` = threshold)
  }
  else { # should be data
    scores_df <- broom:::augment.factanal(x) # This happens to work. Revisit.
    scores_df <- scores_df %>% select(-.rownames) # augment.factanal seems to always return row names in .rownames column.
    # Rename the IDs of the factors so that IDs correspond with the order of importance, which is the order in the augment.factanal output.
    colnames(scores_df) <-stringr::str_c(factor_score_prefix, 1:n_factor)
    scores_df <- scores_df %>% rename_with(function(x){stringr::str_replace(x,paste0("^", factor_score_prefix), "Factor ")}, starts_with(factor_score_prefix)) #TODO: Make string match condition stricter.

    # table of observations. bind original data so that color can be used later.
    res <- x$df
    res <- res %>% dplyr::bind_cols(scores_df)
  }
  res
}
