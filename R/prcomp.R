# All judge_*/classify_* helpers emit English-canonical labels and a language-neutral
# `status` token. The desktop/server client translates the labels (VizUtil message tables)
# and composes tooltips from token + params. No natural language for translation lives
# in R output. (issue #37019; contract established by #37018)
prcomp_report_config <- function() {
  list(
    loading_salient = 0.40,
    dominant_contribution = 0.40,
    dominant_ratio = 1.5,
    representation_high = 0.70,
    representation_mostly = 0.50,
    representation_partial = 0.30,
    cumulative_high = 0.80,
    cumulative_mid = 0.60,
    two_d_high = 0.70,
    two_d_mid = 0.50,
    scale_ratio_warning = 10,
    na_exclusion_warning = 0.20,
    next_gain_threshold = 0.20,
    related_min = 2,
    related_max = 5
  )
}

classify_pca_component_pattern <- function(loadings, contributions, cfg = prcomp_report_config()) {
  ordered_contribution <- sort(contributions, decreasing = TRUE)
  maximum_contribution <- ordered_contribution[1]
  second_contribution <- if (length(ordered_contribution) >= 2) ordered_contribution[2] else 0
  is_dominant <- maximum_contribution >= cfg$dominant_contribution &&
    (second_contribution == 0 || maximum_contribution / second_contribution >= cfg$dominant_ratio)
  positive_variables <- names(loadings)[loadings >= cfg$loading_salient]
  negative_variables <- names(loadings)[loadings <= -cfg$loading_salient]
  salient_count <- length(positive_variables) + length(negative_variables)
  same_sign_share <- if (salient_count == 0) 0 else max(length(positive_variables), length(negative_variables)) / salient_count
  top_three_sum <- sum(head(ordered_contribution, 3))
  dominant_variable <- names(which.max(contributions))
  base <- list(dominant_variable = dominant_variable,
               positive_variables = paste(positive_variables, collapse = ","),
               negative_variables = paste(negative_variables, collapse = ","))
  if (is_dominant) {
    c(list(status = "single_variable", label = "Single Variable"), base)
  } else if (length(positive_variables) >= 1 && length(negative_variables) >= 1) {
    c(list(status = "contrast", label = "Contrast"), base)
  } else if (salient_count >= 3 && same_sign_share >= 0.80) {
    c(list(status = "common_direction", label = "Common Direction"), base)
  } else if (top_three_sum < 0.50) {
    c(list(status = "diffuse", label = "Diffuse"), base)
  } else {
    c(list(status = "mixed", label = "Mixed"), base)
  }
}

select_pca_related_variables <- function(loadings, contributions, cfg = prcomp_report_config()) {
  candidate_index <- which(abs(loadings) >= cfg$loading_salient)
  if (length(candidate_index) < cfg$related_min) {
    candidate_index <- head(order(abs(loadings), decreasing = TRUE), cfg$related_min)
  }
  candidate_index <- candidate_index[order(abs(loadings[candidate_index]),
                                           contributions[candidate_index], decreasing = TRUE)]
  candidate_index <- head(candidate_index, cfg$related_max)
  selected <- loadings[candidate_index]
  labels <- paste0(ifelse(selected >= 0, "+", "-"), names(selected))
  list(variables = names(selected), loadings = unname(selected),
       display_text = paste(labels, collapse = ", "))
}

#' Build a prcomp-shaped fit from a correlation matrix. (issue #37294)
#'
#' prcomp() cannot take a correlation matrix, so a Polychoric / Tetrachoric / Mixed PCA is the
#' eigen-decomposition of that matrix. That yields eigenvalues (-> sdev), principal component
#' coefficients (-> rotation) and contribution ratios directly, but NO observation scores: a
#' correlation matrix carries no rows. Per the issue, the scores are APPROXIMATED as
#'
#'   scores <- scale(numeric_data) %*% eigenvectors
#'
#' i.e. the ordinal answers coded to numbers, standardized, then projected onto the components.
#' They are NOT the latent continuous values the polychoric model assumes -- the report says so.
#'
#' @param encoded_df numeric data frame (ordinal categories already coded 1..k)
#' @param cor_mat the resolved correlation matrix, variables x variables
#' @return an object of class "prcomp" (sdev / rotation / center / scale / x)
prcomp_build_categorical_fit <- function(encoded_df, cor_mat) {
  eg <- eigen(cor_mat, symmetric = TRUE)
  # A smoothed correlation matrix can carry tiny negative eigenvalues; sqrt() of those is NaN.
  values <- pmax(eg$values, 0)
  rotation <- eg$vectors
  rownames(rotation) <- colnames(cor_mat)
  colnames(rotation) <- paste0("PC", seq_len(ncol(rotation)))
  scaled <- scale(as.matrix(encoded_df))
  # A constant column would give scale 0 -> NaN. Columns with a single unique value are already
  # dropped upstream, but a degenerate group can still get here; keep those variables at 0.
  scaled[!is.finite(scaled)] <- 0
  scores <- scaled %*% rotation
  colnames(scores) <- colnames(rotation)
  fit <- list(
    sdev = sqrt(values),
    rotation = rotation,
    center = attr(scaled, "scaled:center"),
    scale = attr(scaled, "scaled:scale"),
    x = scores
  )
  class(fit) <- "prcomp"
  fit
}

#' Signed principal component loadings (主成分負荷量), variables x components. (issue #37294)
#'
#' Historically every report branch recomputed cor(cleaned_df, x$x) -- a FRESH PEARSON
#' cross-correlation. Under a Polychoric fit that would (a) error on a factor column and
#' (b) silently mix correlation types inside one report. Routing every branch through this one
#' helper keeps the whole report on the correlation the analysis actually ran on.
#'
#' For the Pearson path the two are equal by construction: with scale. = TRUE,
#' cor(cleaned_df, fit$x) == rotation %*% diag(sdev).
prcomp_signed_loadings <- function(x) {
  if (!is.null(x$signed_loadings)) {
    return(x$signed_loadings)
  }
  cleaned_df <- x$df[, names(x$input_diagnostics$variable_sd), drop = FALSE]
  cor(cleaned_df, x$x)
}

#' Principal component scores in the scale the user asked for. (issue #27224)
#'
#' `prcomp()` returns scores whose spread reflects each component's own variance (sdev), while
#' SPSS (and `psych::principal()`) reports scores standardized to standard deviation 1. Both
#' describe the SAME solution -- the components, loadings and explained variance are identical and
#' only the scale of the score numbers differs -- so this is an output-scale choice, not a
#' different analysis.
#'
#' MUST be called AFTER sign stabilization, so the chosen scale is applied to the signs the report
#' actually shows.
#'
#' @param fit a prcomp-shaped fit (needs `$x` and `$sdev`)
#' @param score_scale "preserve_variance" (prcomp's own scores) or "unit_variance" (SD = 1, SPSS-compatible)
#' @return the score matrix, same dim/dimnames as `fit$x`
get_prcomp_scores <- function(fit, score_scale = c("preserve_variance", "unit_variance")) {
  score_scale <- match.arg(score_scale)
  if (identical(score_scale, "preserve_variance")) {
    return(fit$x)
  }
  sdev <- fit$sdev[seq_len(ncol(fit$x))]
  # Dividing by a ~0 sdev turns rounding noise into a huge score, so refuse rather than emit
  # garbage. A degenerate component means a variable is (nearly) a linear combination of the
  # others, or there are fewer rows than variables. EXP-ANA-36 carries no params.
  if (any(!is.finite(sdev) | sdev <= sqrt(.Machine$double.eps))) {
    stop("EXP-ANA-36 :: [] :: Some principal components have zero or near-zero standard deviation and cannot be standardized.")
  }
  sweep(fit$x, MARGIN = 2, STATS = sdev, FUN = "/")
}

#' User-facing score matrix of a stored fit, with back-compat fallback. (issue #27224)
#'
#' `$scores` only exists on fits produced after #27224. Models saved before it (and k-means fits,
#' which never carry report data) fall back to `$x`, i.e. the previous behavior exactly.
get_stored_prcomp_scores <- function(x) {
  if (!is.null(x$scores)) x$scores else x$x
}

#' The score scale a stored fit was built with, with back-compat fallback. (issue #27224)
get_stored_prcomp_score_scale <- function(x) {
  if (!is.null(x$score_scale)) x$score_scale else "preserve_variance"
}

#' do PCA
#' allow_single_column - Do not throw error and go ahead with PCA even if only one column is left after preprocessing. For K-means.
#' retained_components - Number of principal components the report treats as retained. NULL = auto (use parallel analysis recommendation). Clamped to [1, number of components].
#' with_report_data - Whether to compute and attach the redesigned PCA report data (parallel analysis, Kaiser, retained/diagnostics) AND apply sign stabilization. Pure-PCA only; exp_kmeans passes FALSE so k-means fits are neither given report data nor sign-flipped. (issue #37019)
#' cor_type - Correlation the analysis runs on: "auto" (decide from the variable types and the shape of their distributions), "pearson", "polychoric", "tetrachoric" or "mixed". Anything other than Pearson eigen-decomposes that correlation matrix instead of calling prcomp() on the raw data, and the observation scores become APPROXIMATE (see prcomp_build_categorical_fit). Pure-PCA only -- honored when with_report_data is TRUE, so exp_kmeans is unaffected. (issue #37294)
#' score_scale - Scale of the principal component scores that are OUTPUT (data columns, biplot, observation map): "preserve_variance" (default; prcomp's own scores, whose spread reflects each component's variance) or "unit_variance" (each component's scores standardized to SD 1, matching SPSS / psych::principal). Display scale only -- loadings, coefficients, contributions, explained variance and every judgment are computed from the canonical prcomp scores and are IDENTICAL either way. Pure-PCA only: exp_kmeans passes with_report_data = FALSE and is always kept at "preserve_variance" so clustering input never changes. (issue #27224)
#' @export
do_prcomp <- function(df, ..., normalize_data=TRUE, max_nrow = NULL, allow_single_column = FALSE, seed = 1, na.rm = TRUE, retained_components = NULL, with_report_data = TRUE, cor_type = "auto", score_scale = c("preserve_variance", "unit_variance")) {
  score_scale <- match.arg(score_scale)
  all_cols <- colnames(df)
  # this evaluates select arguments like starts_with
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  # remove grouped col or target col
  selected_cols <- setdiff(selected_cols, grouped_cols)

  if (any(stringr::str_detect(all_cols, "^PC[0-9]+$"))) {
    stop("EXP-ANA-6 :: [] :: Columns with names such as PC1, PC2, ... cannot be in the input data frame. Please rename them.")
  }

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

  # Resolve the correlation ONCE, from the whole data, and reuse it for every group. Selecting per
  # group would let one facet run on Polychoric and another on Pearson while the report describes a
  # single method, and would make the groups' loadings incomparable. Mirrors exp_factanal. (#37294)
  overall_type <- NULL
  overall_reason <- NULL
  if (with_report_data && identical(tolower(trimws(as.character(cor_type))), "auto")) {
    overall_selection <- tryCatch({
      candidate_cols <- intersect(selected_cols, colnames(df))
      if (length(candidate_cols) >= 2) select_factor_correlation_type(as.data.frame(df)[, candidate_cols, drop = FALSE]) else NULL
    }, error = function(e) NULL)
    if (!is.null(overall_selection) && !identical(overall_selection$selected_method, "unsupported")) {
      overall_type <- overall_selection$selected_method
      overall_reason <- overall_selection$reason
    }
  }

  each_func <- function(df) {
    # Capture the variable columns actually present in this group's data (after the
    # list/difftime drop above) BEFORE preprocess_factanal_data_before_sample overwrites
    # selected_cols, so the report can compute which variables were excluded. (issue #37019)
    report_selected_cols <- intersect(selected_cols, colnames(df))

    # sample the data for quicker turn around on UI,
    # if data size is larger than specified max_nrow.
    sampled_nrow <- NULL
    if (!is.null(max_nrow) && nrow(df) > max_nrow) {
      # Record that sampling happened.
      sampled_nrow <- max_nrow
      df <- df %>% sample_rows(max_nrow)
    }

    # As the name suggests, this preprocessing function was originally designed to be done
    # before sampling, but we found that for this PCA function, that makes the
    # process as a whole slower in the cases we tried. So, we are doing this after sampling.
    if (na.rm) { # Do NA preprocessing under this if statement, so that it can be skipped if it is already done. For exp_kmeans.
      filtered_df <- preprocess_factanal_data_before_sample(df, selected_cols)
      selected_cols <- attr(filtered_df, 'predictors') # predictors are updated (removed) in preprocess_factanal_data_before_sample. Sync with it.
    }
    else {
      filtered_df <- df
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
    if (allow_single_column) { # This is when exp_kmeans calling this function wants to go ahead even with single column.
      min_ncol <- 1
    }
    else {
      min_ncol <- 2
    }
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
    # downstream computation reads it, so the report can never mix Pearson and Polychoric
    # (e.g. polychoric eigenvalues next to Pearson loadings). Pure-PCA only: exp_kmeans passes
    # with_report_data = FALSE and keeps the original raw-data prcomp() path untouched. (#37294)
    selection <- NULL
    resolved <- list(type = "pearson", auto = FALSE, reason = "", degraded_from = NULL)
    cor_result <- NULL
    encoded_df <- cleaned_df
    if (with_report_data) {
      selection <- select_factor_correlation_type(cleaned_df)
      resolved <- resolve_factanal_correlation_type(cor_type, selection)
      # Auto: keep the whole-analysis choice made above, so every group uses the same correlation.
      if (isTRUE(resolved$auto) && !is.null(overall_type) &&
          !identical(selection$selected_method, "unsupported")) {
        resolved$type <- overall_type
        resolved$reason <- overall_reason
      }
      # An unsupported variable combination (a nominal category, a constant column) is unsupported
      # whichever correlation was asked for -- picking Pearson manually must not smuggle a nominal
      # column in as arbitrary integer codes. So gate on the SELECTION, not the resolved type.
      if (identical(selection$selected_method, "unsupported") || identical(resolved$type, "unsupported")) {
        if (length(grouped_cols) > 0) {
          # With Repeat By, skip just this group -- mirroring the not-enough-columns guard above.
          return(NULL)
        }
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
    }

    requested_family <- resolved$type
    if (identical(resolved$type, "pearson")) {
      # "scale." is an argument name. There is no such operator like ".=".
      fit <- prcomp(encoded_df, scale.=normalize_data)
      cor_mat <- cor(encoded_df) # Calculate correlation for screeplot.
    }
    else {
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
        fit <- prcomp(encoded_df, scale.=TRUE)
        cor_mat <- cor(encoded_df)
      }
      else {
        fit <- prcomp_build_categorical_fit(encoded_df, cor_mat)
      }
    }
    fit$correlation <- cor_mat
    if (with_report_data) {
      # Non-Pearson correlations eigen-decompose cor_mat, so the scores are APPROXIMATE
      # (scale(numeric_data) %*% eigenvectors) rather than exact prcomp() scores; the report says
      # so. Set only for pure PCA -- exp_kmeans fits must not grow new fields. (issue #37294)
      fit$is_categorical_correlation <- !identical(resolved$type, "pearson")
    }
    fit$df <- filtered_df # add filtered df to model so that we can bind_col it for output. It needs to be the filtered one to match row number.
    fit$grouped_cols <- grouped_cols
    fit$sampled_nrow <- sampled_nrow

    # Fit-time PCA report data (issue #37019). PURE-PCA ONLY: exp_kmeans shares this
    # machinery but passes with_report_data=FALSE, so k-means fits get neither the report
    # data nor the sign flip below. Each piece is tryCatch-guarded (mirrors factanal.R) so a
    # degenerate input degrades gracefully instead of aborting the fit.
    if (with_report_data) {
      # Sign stabilization (#37019 spec 4-3): flip each PC so the variable with the largest
      # |correlation| loads positively. PCA signs are arbitrary; this makes interpretation
      # text stable across runs. Compute in a guard so a degenerate correlation leaves signs
      # untouched (all-1 multiplier is a no-op sweep).
      sign_multiplier <- tryCatch({
        variable_pc_correlations <- if (isTRUE(fit$is_categorical_correlation)) {
          # A correlation-matrix PCA has no exact scores to correlate against; the component
          # loading IS eigenvector * sqrt(eigenvalue), which is that same correlation.
          fit$rotation %*% diag(fit$sdev, nrow = length(fit$sdev))
        } else {
          cor(encoded_df, fit$x)
        }
        vapply(seq_len(ncol(variable_pc_correlations)), function(i) {
          col <- variable_pc_correlations[, i]
          strongest <- col[which.max(abs(col))]
          if (length(strongest) == 0 || is.na(strongest) || strongest >= 0) 1 else -1
        }, numeric(1))
      }, error = function(e) rep(1, ncol(fit$rotation)))
      fit$rotation <- sweep(fit$rotation, 2, sign_multiplier, "*")
      fit$x <- sweep(fit$x, 2, sign_multiplier, "*")

      if (isTRUE(fit$is_categorical_correlation)) {
        # Cache the signed loadings AFTER the sign sweep. Every report branch reads them through
        # prcomp_signed_loadings() so no branch silently recomputes a FRESH PEARSON
        # cor(cleaned_df, x$x) on top of a polychoric solution -- and so a factor column, which
        # cor() cannot take at all, never reaches cor(). (issue #37294)
        fit$signed_loadings <- tryCatch({
          loadings <- fit$rotation %*% diag(fit$sdev, nrow = length(fit$sdev))
          dimnames(loadings) <- list(rownames(fit$rotation), colnames(fit$rotation))
          loadings
        }, error = function(e) NULL)
      }

      fit$parallel <- tryCatch(
        compute_parallel_analysis(encoded_df, cor_type = resolved$type, cor_matrix = fit$correlation),
        error = function(e) NULL)
      fit$kaiser_components <- tryCatch(
        # A correlation matrix is standardized by construction, so under a categorical correlation
        # the Kaiser criterion applies whatever normalize_data says.
        if (normalize_data || isTRUE(fit$is_categorical_correlation)) {
          as.integer(sum(eigen(fit$correlation)$values >= 1))
        } else NA_integer_,
        error = function(e) NA_integer_)
      fit$recommended_components <- if (!is.null(fit$parallel)) fit$parallel$recommended_n else NA_integer_
      n_comp <- length(fit$sdev)
      fit$retained_components <- if (!is.null(retained_components)) {
        min(max(1L, as.integer(retained_components)), n_comp)
      } else {
        min(max(1L, ifelse(is.na(fit$recommended_components), 1L, fit$recommended_components)), n_comp)
      }
      fit$retained_is_auto <- is.null(retained_components)
      fit$normalize_data <- normalize_data
      # Method metadata. Same field names as exp_factanal so the client-side extraction of the
      # report's explanation text is the same shape for both analytics. (issue #37294)
      fit$correlation_type <- resolved$type
      fit$correlation_is_auto <- isTRUE(resolved$auto)
      fit$correlation_reason <- if (is.null(resolved$reason)) "" else resolved$reason
      fit$correlation_degraded_from <- if (is.null(resolved$degraded_from)) "" else resolved$degraded_from
      fit$correlation_selection <- selection
      fit$correlation_polychoric_available <- tryCatch(
        factanal_polychoric_available(selection, encoded_df), error = function(e) FALSE)
      fit$cor_diagnostics <- tryCatch({
        if (identical(requested_family, "pearson")) NULL
        else compute_polychoric_diagnostics(encoded_df, cor_result, selection)
      }, error = function(e) unavailable_polychoric_diagnostics())

      fit$input_diagnostics <- tryCatch({
        # sd() over the ENCODED frame: a factor column has no sd(), and these names are what the
        # report branches use to reconstruct the analyzed variables.
        variable_sd <- vapply(encoded_df, sd, numeric(1))
        original_row_count <- nrow(df)
        analyzed_row_count <- nrow(cleaned_df)
        excluded_row_count <- original_row_count - analyzed_row_count
        list(
          original_row_count = original_row_count,
          analyzed_row_count = analyzed_row_count,
          excluded_row_count = excluded_row_count,
          excluded_row_rate = excluded_row_count / max(1, original_row_count),
          excluded_variables = setdiff(report_selected_cols, colnames(cleaned_df)),
          variable_sd = variable_sd,
          scale_ratio = if (min(variable_sd) > 0) max(variable_sd) / min(variable_sd) else NA_real_
        )
      }, error = function(e) NULL)
    }

    # User-facing score matrix (issue #27224). fit$x stays the canonical prcomp score matrix that
    # every loading / contribution / representation computation reads; fit$scores is what the
    # OUTPUT (data columns, biplot, observation map) shows. Built here -- outside the
    # with_report_data block, but AFTER the sign stabilization inside it -- so the scale is applied
    # to the signs the report shows and so a fit built with with_report_data = FALSE still gets a
    # $scores field. k-means is pinned to preserve_variance: rescaling its PCA scores would change
    # the clustering input, and its dialog exposes no such option.
    effective_score_scale <- if (with_report_data) score_scale else "preserve_variance"
    fit$score_scale <- effective_score_scale
    fit$scores <- get_prcomp_scores(fit, score_scale = effective_score_scale)

    class(fit) <- c("prcomp_exploratory", class(fit))
    fit
  }

  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' extracts results from prcomp as a dataframe
#' @export
#' @param n_sample - Sample number for biplot. Default 5000, which is the default of our scatter plot.
#'        we use it for gathered_data for parallel coordinates too. sampling is applied before gather.
#' @param with_excluded_rows - For "summary" type, whether to show number of rows excluded at the preprocessing. For k-means analytics view.
tidy.prcomp_exploratory <- function(x, type="variances", n_sample=NULL, pretty.name=FALSE, normalize_data=FALSE, with_excluded_rows=FALSE, ...) {
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

    # prepare scores matrix. Observation coordinates use the USER-FACING scores (issue #27224), so
    # a "unit variance" (SPSS-compatible) analysis plots the same numbers the Data tab shows. The
    # loading vectors are rescaled to the observations below (scale_ratio), so the biplot stays
    # readable either way -- only the axis range differs.
    scores_matrix <- get_stored_prcomp_scores(x)[, 1:2, drop = FALSE] # keep only PC1 and PC2 for biplot

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
          res[[orig_col]] <- forcats::fct_na_value_to_level(as.factor(res[[orig_col]]), level="(NA)")
        }
        else {
          # make logical columns into factor with NA level, so that legend will show NA.
          res[[orig_col]] <- forcats::fct_na_value_to_level(factor(res[[orig_col]], levels = c("TRUE","FALSE")), level="(NA)")
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
    loadings_df <- loadings_df %>% dplyr::rename(Measures=PC2) # use different column name for PC2 of measures.
    loadings_df0 <- loadings_df %>% dplyr::mutate(PC1=0, Measures=0) # create df for origin of coordinates.
    loadings_df <- loadings_df0 %>% dplyr::bind_rows(loadings_df)
    res <- res %>% dplyr::bind_rows(loadings_df)
    # fill group_by column so that Repeat By on chart works fine. loadings_df does not have values for the group_by column.
    res <- res %>% tidyr::fill(x$grouped_cols)
  }
  else if (type == "summary") { # This is only for kmeans case. TODO: We might want to separate PCA code and k-means code.
    res <- broom::tidy(x$kmeans)
    if (!is.null(x$silhouette)) {
      # Per-cluster silhouette aggregates keyed by the same cluster labels as broom::tidy(kmeans).
      sil_summary <- x$silhouette %>%
        dplyr::mutate(.cluster_key = as.character(x$kmeans$cluster)) %>%
        dplyr::group_by(.cluster_key) %>%
        dplyr::summarise(
          # Guard the degenerate all-NA case so it yields NA (not NaN/Inf) consistently.
          avg_silhouette = if (all(is.na(silhouette_score))) NA_real_ else mean(silhouette_score, na.rm = TRUE),
          min_silhouette = if (all(is.na(silhouette_score))) NA_real_ else min(silhouette_score, na.rm = TRUE),
          pct_negative = if (all(is.na(silhouette_score))) NA_real_ else mean(silhouette_score < 0, na.rm = TRUE),
          .groups = "drop"
        )
      res <- res %>%
        dplyr::mutate(.cluster_key = as.character(cluster)) %>%
        dplyr::left_join(sil_summary, by = ".cluster_key") %>%
        dplyr::select(-.cluster_key)
    }
    if (with_excluded_rows) {
      res <- res %>% tibble::add_row(size=x$excluded_nrow)
    }
  }
  else if (type == "screeplot") {
    eigen_res <- eigen(x$correlation, only.values = TRUE) # Cattell's scree plot is eigenvalues of correlation/covariance matrix.
    res <- tibble::tibble(factor=1:length(eigen_res$values), eigenvalue=eigen_res$values)
  }
  else if (type == "analysis_conditions") {
    # PCA report: one row per analysis condition, composed from fit-time input diagnostics
    # (issue #37019). English-canonical Description sentences + language-neutral status tokens;
    # the client translates. Empty typed tibble for k-means / old saved models (no report data).
    cfg <- prcomp_report_config()
    if (is.null(x$input_diagnostics) && is.null(x$parallel)) {
      res <- tibble::tibble(Metric = character(0), Value = character(0),
                            Description = character(0), status = character(0))
    }
    else {
      d <- x$input_diagnostics
      # A categorical correlation is standardized by construction (the analysis runs on the
      # correlation matrix), so normalize_data is inert there -- report it as normalized rather
      # than letting a stale FALSE trigger a bogus SD-ratio warning. (issue #37294)
      normalized <- isTRUE(x$normalize_data) || isTRUE(x$is_categorical_correlation)
      variables_used <- length(d$variable_sd)
      excluded_names <- d$excluded_variables
      # #37268: show "None" (JA: なし) when nothing was excluded, not "-".
      excluded_display <- if (length(excluded_names) == 0) "None" else paste(excluded_names, collapse = ", ")
      excluded_pct <- d$excluded_row_rate * 100
      scale_ratio <- d$scale_ratio
      scale_display <- if (is.na(scale_ratio)) "-" else format(round(scale_ratio, 1), nsmall = 1)
      scale_status <- if (!normalized && is.finite(scale_ratio) && scale_ratio >= cfg$scale_ratio_warning) "scale_warning" else "ok"
      # #37268: rename Rows Used / Variables Used; drop redundant Rows vs Variables row;
      # refresh Description copy (and English-canonical strings for the client translator).
      # Score Scale (issue #27224). English-canonical value labels; the client translates.
      # Old saved models have no $score_scale -- they were built with prcomp's own scores.
      score_scale_display <- if (identical(get_stored_prcomp_score_scale(x), "unit_variance")) {
        "Unit Variance"
      } else {
        "Preserve Component Variance"
      }
      res <- tibble::tibble(
        Metric = c("Row Count", "Rows Excluded", "Number of Variables", "Excluded Variables",
                   "Normalization", "Score Scale", "SD Ratio (Max/Min)"),
        Value = c(
          as.character(d$analyzed_row_count),
          paste0(d$excluded_row_count, " (", format(round(excluded_pct, 1), nsmall = 1), "%)"),
          as.character(variables_used),
          excluded_display,
          if (normalized) "Yes" else "No",
          score_scale_display,
          scale_display
        ),
        Description = c(
          "Number of rows used in the analysis.",
          "Number and rate of rows removed because of missing values.",
          "Number of variables used in the analysis.",
          "Variables dropped before analysis because they were all missing or had only one unique value.",
          "Whether variables were standardized before analysis.",
          "How principal-component scores are scaled.",
          "Ratio of the maximum to the minimum standard deviation across all variables."
        ),
        status = c(
          if (d$analyzed_row_count <= variables_used) "few_rows" else "ok",
          if (d$excluded_row_rate >= cfg$na_exclusion_warning) "high_na_exclusion" else "ok",
          "ok",
          if (length(excluded_names) == 0) "na" else "ok",
          "ok",
          "ok",
          scale_status
        )
      )
    }
  }
  else if (type == "analysis_method") {
    # Report header table: which correlation the numbers below were actually computed with.
    # (issue #37294; mirrors exp_factanal's table of the same name so the client-side extraction
    # of the explanation text is one shared shape.)
    cor_type <- if (is.null(x$correlation_type)) "pearson" else x$correlation_type
    d <- x$input_diagnostics
    n_variables <- if (!is.null(d)) length(d$variable_sd) else NA_integer_
    n_rows_used <- if (!is.null(d)) d$analyzed_row_count else NA_integer_
    normalized <- isTRUE(x$normalize_data) || isTRUE(x$is_categorical_correlation)
    res <- tibble::tibble(
      # NOTE: "Target Variables" / "Data Rows" instead of the shorter "Variables" / "Rows":
      # the client's shared translation map already binds those two keys to different wordings
      # for other tables, and a direct-map key can only have one translation. (issue #26623)
      Item = c("Correlation", "Normalization", "Target Variables", "Data Rows"),
      Value = c(
        factanal_correlation_label(cor_type),
        if (normalized) "Yes" else "No",
        if (length(n_variables) == 1L && !is.na(n_variables)) as.character(n_variables) else "N/A",
        if (length(n_rows_used) == 1L && !is.na(n_rows_used)) as.character(n_rows_used) else "N/A"
      ),
      # Hidden columns. The client reads them to bind the report's explanation text; they are not
      # part of the rendered Item/Value table. The booleans are emitted as EXPLICIT "TRUE"/"FALSE"
      # STRINGS, not R logicals: a logical column can reach the client serialized as "1"/"0"
      # depending on the pivot pipeline, and the client's isTrue() only accepts "TRUE"/"true"/true.
      correlation_type = cor_type,
      correlation_is_auto = if (isTRUE(x$correlation_is_auto)) "TRUE" else "FALSE",
      # Non-empty when the requested correlation could not be estimated and the fit fell back to
      # Pearson, so the report can say so instead of inventing a rationale.
      degraded_from = if (is.null(x$correlation_degraded_from)) "" else x$correlation_degraded_from,
      # Whether suggesting Polychoric makes sense for this data at all.
      polychoric_available = if (isTRUE(x$correlation_polychoric_available)) "TRUE" else "FALSE",
      # Language-neutral tokens for the selector's warnings; the client renders the localized text.
      warning_tokens = paste(factanal_selection_warning_tokens(x$correlation_selection), collapse = ","),
      # "TRUE" also when a polychoric estimation failed and the fit degraded to Pearson, so the
      # report still shows the diagnostics table that reports the failure.
      has_diagnostics = if (!is.null(x$cor_diagnostics) && nrow(x$cor_diagnostics) > 0) "TRUE" else "FALSE",
      reason = if (is.null(x$correlation_reason)) "" else x$correlation_reason
    )
  }
  else if (type == "cor_diagnostics") {
    # Polychoric-family data suitability diagnostics. Empty tibble (with the same columns) when
    # the analysis ran on Pearson, so the client can hide the section. (issue #37294)
    res <- if (is.null(x$cor_diagnostics)) {
      tibble::tibble(Diagnostic = character(), Judgement = character(),
                     Description = character(), status = character())
    } else {
      x$cor_diagnostics
    }
  }
  else if (type == "parallel_screeplot") {
    # Horn's parallel analysis scree data: actual correlation-matrix eigenvalue vs the random-data
    # threshold, per component (issue #37019). Component is the integer factor number. Empty typed
    # tibble when parallel analysis is absent (k-means / old saved models).
    if (is.null(x$parallel)) {
      res <- tibble::tibble(Component = integer(0), Eigenvalue = numeric(0),
                            `Random Data Eigenvalue` = numeric(0))
    }
    else {
      tbl <- x$parallel$table
      res <- tibble::tibble(
        Component = as.integer(tbl$factor_number),
        Eigenvalue = tbl$actual_eigenvalue,
        `Random Data Eigenvalue` = tbl$random_eigenvalue_threshold
      )
    }
  }
  else if (type == "variances_judged") {
    # PCA report: per-component variance table with three retention judgments (issue #37019).
    # Empty typed tibble for k-means / old saved models (no report data).
    if (is.null(x$input_diagnostics) && is.null(x$parallel)) {
      res <- tibble::tibble(
        Component = character(0), Eigenvalue = numeric(0),
        `% Variance` = numeric(0), `Cummulated % Variance` = numeric(0),
        `Parallel Analysis` = character(0), `Kaiser Criterion` = character(0),
        Selected = character(0), parallel_status = character(0),
        kaiser_status = character(0), selected_status = character(0)
      )
    }
    else {
      # Eigenvalue / % Variance / Cummulated % Variance use x$sdev^2 -- the SAME basis as the
      # existing "variances" branch -- so these numbers match the Variance (%) tab exactly.
      # When normalize_data=TRUE, x$sdev^2 equals the correlation-matrix eigenvalues that parallel
      # analysis and Kaiser use, so all bases coincide. They diverge only when normalize_data=FALSE
      # (covariance-scaled sdev), and there Kaiser is "na" anyway; the Parallel Adopt columns read
      # actual vs random eigenvalues directly from x$parallel$table (correlation eigenvalues from
      # compute_parallel_analysis), NOT from this Eigenvalue column, so the Adopt judgment always
      # agrees with the parallel scree regardless of basis.
      eigenvalue <- x$sdev^2
      n_comp <- length(eigenvalue)
      total_variance <- sum(eigenvalue)
      pct_variance <- eigenvalue / total_variance * 100
      cum_pct_variance <- cumsum(pct_variance)
      component <- paste0("PC", seq_len(n_comp))
      normalized <- isTRUE(x$normalize_data)

      # Parallel Analysis: adopt when actual eigenvalue > random threshold, keyed by component
      # index (factor_number). NULL parallel -> Not Available / na.
      if (is.null(x$parallel)) {
        parallel_label <- rep("Not Available", n_comp)
        parallel_status <- rep("na", n_comp)
      }
      else {
        ptbl <- x$parallel$table
        in_range <- ptbl$factor_number <= n_comp
        actual <- rep(NA_real_, n_comp)
        threshold <- rep(NA_real_, n_comp)
        actual[ptbl$factor_number[in_range]] <- ptbl$actual_eigenvalue[in_range]
        threshold[ptbl$factor_number[in_range]] <- ptbl$random_eigenvalue_threshold[in_range]
        adopted <- !is.na(actual) & !is.na(threshold) & actual > threshold
        parallel_label <- ifelse(adopted, "Adopt", "Not Adopted")
        parallel_status <- ifelse(adopted, "adopted", "not_adopted")
      }

      # Kaiser Criterion: only meaningful when normalized (eigenvalue >= 1). Otherwise "-"/na.
      if (normalized) {
        kaiser_adopted <- eigenvalue >= 1
        kaiser_label <- ifelse(kaiser_adopted, "Adopt", "Not Adopted")
        kaiser_status <- ifelse(kaiser_adopted, "adopted", "not_adopted")
      }
      else {
        kaiser_label <- rep("-", n_comp)
        kaiser_status <- rep("na", n_comp)
      }

      retained <- if (!is.null(x$retained_components)) x$retained_components else 0L
      selected_adopted <- seq_len(n_comp) <= retained
      selected_label <- ifelse(selected_adopted, "Adopt", "Not Adopted")
      selected_status <- ifelse(selected_adopted, "adopted", "not_adopted")

      res <- tibble::tibble(
        Component = component,
        Eigenvalue = eigenvalue,
        `% Variance` = pct_variance,
        `Cummulated % Variance` = cum_pct_variance,
        `Parallel Analysis` = parallel_label,
        `Kaiser Criterion` = kaiser_label,
        Selected = selected_label,
        parallel_status = parallel_status,
        kaiser_status = kaiser_status,
        selected_status = selected_status
      )
    }
  }
  else if (type == "component_profiles") {
    # PCA report: ONE ROW PER RETAINED COMPONENT with pattern classification + related variables
    # (issue #37019). English-canonical Pattern label + language-neutral status token; the client
    # translates. Empty typed tibble for k-means / old saved models (no report data).
    if (is.null(x$input_diagnostics) && is.null(x$parallel)) {
      res <- tibble::tibble(
        Component = character(0), Eigenvalue = numeric(0),
        `% Variance` = numeric(0), `Cummulated % Variance` = numeric(0),
        `Related Variables` = character(0), Pattern = character(0),
        pattern_status = character(0), dominant_variable = character(0),
        positive_variables = character(0), negative_variables = character(0)
      )
    }
    else {
      cfg <- prcomp_report_config()
      # 主成分負荷量 = correlation between variable and score (signed), variables x components.
      # Via prcomp_signed_loadings() so a Polychoric fit is not re-correlated with Pearson. (#37294)
      signed_loadings <- prcomp_signed_loadings(x)
      # Eigenvalue / % Variance / Cummulated % Variance from x$sdev^2 -- SAME basis as "variances".
      eigenvalue <- x$sdev^2
      total_variance <- sum(eigenvalue)
      pct_variance <- eigenvalue / total_variance * 100
      cum_pct_variance <- cumsum(pct_variance)
      retained <- if (!is.null(x$retained_components)) x$retained_components else 0L
      rows <- lapply(seq_len(retained), function(i) {
        loadings_i <- signed_loadings[, i]
        # Contribution = each variable's share of this PC. rotation columns are unit vectors so
        # sum(rotation[,i]^2) == 1; normalize explicitly so the share always sums to 1.
        contributions_i <- x$rotation[, i]^2
        contributions_i <- contributions_i / sum(contributions_i)
        profile <- classify_pca_component_pattern(loadings = loadings_i, contributions = contributions_i, cfg = cfg)
        related <- select_pca_related_variables(loadings = loadings_i, contributions = contributions_i, cfg = cfg)
        tibble::tibble(
          Component = paste0("PC", i),
          Eigenvalue = eigenvalue[i],
          `% Variance` = pct_variance[i],
          `Cummulated % Variance` = cum_pct_variance[i],
          `Related Variables` = related$display_text,
          Pattern = profile$label,
          pattern_status = profile$status,
          dominant_variable = profile$dominant_variable,
          positive_variables = profile$positive_variables,
          negative_variables = profile$negative_variables
        )
      })
      res <- dplyr::bind_rows(rows)
    }
  }
  else if (type == "loadings_signed") {
    # PCA report: signed principal-component loadings (主成分負荷量 = cor(cleaned_df, fit$x)) in
    # long format for ALL components (issue #37019). Unlike the "loadings" branch (squared cosine),
    # values are signed correlations -- sign-stabilized rotation still yields negatives on
    # non-dominant variables. Empty typed tibble for k-means / old saved models.
    if (is.null(x$input_diagnostics) && is.null(x$parallel)) {
      res <- tibble::tibble(Variable = character(0), Component = character(0), Loading = numeric(0))
    }
    else {
      signed_loadings <- prcomp_signed_loadings(x)
      res <- tibble::as_tibble(signed_loadings, rownames = "Variable") %>%
        tidyr::gather(Component, Loading, dplyr::starts_with("PC"), convert = TRUE) %>%
        dplyr::mutate(Component = forcats::fct_inorder(Component)) # PC2 before PC10 on chart
    }
  }
  else if (type == "loadings_signed_wide") {
    # PCA report: signed loadings (主成分負荷量 = cor(cleaned_df, fit$x)) as a WIDE table -- one
    # row per variable, one column per component -- for the in-cell diverging bar table (issue
    # #37130). Each PC column header carries that component's % variance (sdev^2 / sum(sdev^2) * 100,
    # the SAME basis as the "variances_judged" / "component_profiles" branches, so the numbers match
    # the Variance (%) tab exactly), e.g. "PC1 (43.1%)". Row order = cor() rownames = original
    # variable order (no reorder). Empty typed tibble for k-means / old saved models.
    if (is.null(x$input_diagnostics) && is.null(x$parallel)) {
      res <- tibble::tibble(Variable = character(0))
    }
    else {
      signed_loadings <- prcomp_signed_loadings(x) # variables x components
      n_comp <- ncol(signed_loadings)
      eigenvalue <- x$sdev^2
      pct_variance <- eigenvalue / sum(eigenvalue) * 100
      pc_labels <- paste0("PC", seq_len(n_comp), " (",
                          format(round(pct_variance[seq_len(n_comp)], 1), nsmall = 1, trim = TRUE), "%)")
      res <- tibble::as_tibble(signed_loadings, rownames = "Variable")
      colnames(res) <- c("Variable", pc_labels)
    }
  }
  else if (type == "contributions") {
    # PCA report: variable contributions to each component in long format, for the stacked-bar
    # contribution chart (issue #37132). Two value columns:
    #   Contribution           - the variable's share of that component (rotation^2 per column
    #                            normalized to sum 100 (%)); each component sums to 100.
    #   `Variance Contribution` - that share scaled by the component's explained-variance ratio,
    #                            i.e. the variable's contribution to the TOTAL data variance (%):
    #                            height = pc_explained_variance_ratio * variable_contribution_to_pc.
    #                            Summing a component's segments gives that component's % variance;
    #                            summing every segment gives the cumulative variance explained.
    # Long format. Empty typed tibble for k-means.
    if (is.null(x$input_diagnostics) && is.null(x$parallel)) {
      res <- tibble::tibble(Variable = character(0), Component = character(0),
                            Contribution = numeric(0), `Variance Contribution` = numeric(0))
    }
    else {
      contribution_fraction <- x$rotation^2
      contribution_fraction <- sweep(contribution_fraction, 2, colSums(contribution_fraction), "/") # each column sums to 1
      pct_variance_ratio <- x$sdev^2 / sum(x$sdev^2) # explained-variance ratio per component (fraction)
      variance_contribution <- sweep(contribution_fraction, 2,
                                     pct_variance_ratio[seq_len(ncol(contribution_fraction))], "*") * 100 # % of total variance
      contribution <- contribution_fraction * 100 # each column sums to 100 (%)
      res_contribution <- tibble::as_tibble(contribution, rownames = "Variable") %>%
        tidyr::gather(Component, Contribution, dplyr::starts_with("PC"), convert = TRUE)
      res_variance <- tibble::as_tibble(variance_contribution, rownames = "Variable") %>%
        tidyr::gather(Component, `Variance Contribution`, dplyr::starts_with("PC"), convert = TRUE)
      res <- res_contribution %>%
        dplyr::left_join(res_variance, by = c("Variable", "Component")) %>%
        dplyr::mutate(Component = forcats::fct_inorder(Component)) # PC2 before PC10 on chart
    }
  }
  else if (type == "coefficients") {
    # PCA report: raw principal-component coefficients (主成分係数 = fit$rotation, the eigenvector
    # weights that construct each PC) in long format for ALL components (issue #37019). Unlike
    # "loadings_signed" (cor(cleaned_df, fit$x) correlations) or "contributions" (rotation^2 share),
    # these are the sign-stabilized rotation values themselves -- signed weights, negatives expected
    # on non-dominant variables. rotation is already sign-stabilized (A2) -- do NOT re-flip. Empty
    # typed tibble for k-means / old saved models.
    if (is.null(x$input_diagnostics) && is.null(x$parallel)) {
      res <- tibble::tibble(Variable = character(0), Component = character(0),
                            Coefficient = numeric(0), `Score Coefficient` = numeric(0))
    }
    else {
      # `Score Coefficient` = the weights that actually produce the OUTPUT scores (issue #27224).
      # With "preserve variance" that IS the rotation, so the two columns coincide; with
      # "unit variance" (SPSS-compatible) the score is rotation / sdev -- SPSS's "Component Score
      # Coefficient Matrix". The client only shows this column in the unit-variance case, where the
      # two differ. sdev is guaranteed non-degenerate here: get_prcomp_scores() refuses to build
      # unit-variance scores otherwise, so a stored "unit_variance" fit cannot divide by ~0.
      score_scale <- get_stored_prcomp_score_scale(x)
      score_coefficients <- if (identical(score_scale, "unit_variance")) {
        sweep(x$rotation, MARGIN = 2, STATS = x$sdev[seq_len(ncol(x$rotation))], FUN = "/")
      } else {
        x$rotation
      }
      res <- tibble::as_tibble(x$rotation, rownames = "Variable") %>%
        tidyr::gather(Component, Coefficient, dplyr::starts_with("PC"), convert = TRUE)
      score_res <- tibble::as_tibble(score_coefficients, rownames = "Variable") %>%
        tidyr::gather(Component, `Score Coefficient`, dplyr::starts_with("PC"), convert = TRUE)
      res <- res %>%
        dplyr::left_join(score_res, by = c("Variable", "Component")) %>%
        dplyr::mutate(Component = forcats::fct_inorder(Component)) # PC2 before PC10 on chart
    }
  }
  else if (type == "variable_map") {
    # PCA report: variable-vector rows for a 2D correlation-circle chart (issue #37019).
    # The tam side renders this like the biplot's VARIABLE vectors only (no observation points),
    # so we MIRROR the biplot branch's variable-loading columns: `measure_name` (label), `PC1`
    # (x axis) and `Measures` (biplot's name for the PC2 axis of measures), plus a zero-origin
    # pairing (two rows per variable: origin (0,0) then the endpoint) so the chart can draw a line
    # from the origin to each variable point. We ADD an explicit `PC2` column (same value as
    # `Measures`) so downstream code / tests can read PC2 directly, and `Representation 2D`
    # (= (cor_PC1^2 + cor_PC2^2) * 100), the variable's 2D representation quality in percent.
    # Coordinates are RAW correlations cor(variable, score) -- NOT pre-scaled; the tam chart
    # applies its own display scaling (unlike the biplot branch, which pre-scales loadings so
    # measures and observation points share one scatter plot). Empty typed tibble for k-means /
    # old saved models (no report data).
    if (is.null(x$input_diagnostics) && is.null(x$parallel)) {
      res <- tibble::tibble(
        measure_name = character(0), PC1 = numeric(0), PC2 = numeric(0),
        Measures = numeric(0), `Representation 2D` = numeric(0)
      )
    }
    else {
      # cor(variable, score) -- signed correlations, variables x components. Same basis the fit
      # uses for sign stabilization and the component_profiles / loadings_signed branches.
      signed_loadings <- prcomp_signed_loadings(x)
      n_comp <- ncol(signed_loadings)
      cor_pc1 <- signed_loadings[, 1]
      # Guard: needs >= 2 components. With only 1 component PC2 has no meaning -- use 0 (a point on
      # the PC1 axis) rather than NA so the origin->endpoint line still renders on the chart.
      cor_pc2 <- if (n_comp >= 2) signed_loadings[, 2] else rep(0, length(cor_pc1))
      representation_2d <- (cor_pc1^2 + cor_pc2^2) * 100
      endpoint <- tibble::tibble(
        measure_name = rownames(signed_loadings),
        PC1 = cor_pc1,
        PC2 = cor_pc2,
        Measures = cor_pc2, # mirror biplot: the PC2 axis for measures is named "Measures".
        `Representation 2D` = representation_2d
      )
      # Origin rows (0,0) pair with each endpoint so the chart draws a vector from the origin.
      # Representation 2D is a per-variable quality, meaningless at the origin -> NA there.
      origin <- endpoint %>%
        dplyr::mutate(PC1 = 0, PC2 = 0, Measures = 0, `Representation 2D` = NA_real_)
      res <- dplyr::bind_rows(origin, endpoint)
    }
  }
  else if (type == "representation") {
    # PCA report: per-variable CUMULATIVE representation table, WIDE (issue #37019). Each PC column
    # holds the cumulative representation quality (%) up to that component -- how much of the
    # variable's variance is captured by PC1..PCk. `Retained` reads the cumulative value at the
    # retained-component count; `Judgement`/`judgement_status` bucket that fraction. English-canonical
    # Judgement label + language-neutral status token; the client translates. Empty typed tibble for
    # k-means / old saved models. NOTE: the dynamic PC1..PCn columns exist only when there is data;
    # the empty case returns just the fixed columns (Variable, Retained, Judgement, judgement_status).
    if (is.null(x$input_diagnostics) && is.null(x$parallel)) {
      res <- tibble::tibble(
        Variable = character(0), Retained = numeric(0),
        Judgement = character(0), judgement_status = character(0)
      )
    }
    else {
      cfg <- prcomp_report_config()
      sq <- prcomp_signed_loadings(x)^2 # squared correlations, variables x components.
      # Cumulative across components per variable. apply(..., 1, cumsum) returns components x
      # variables, so transpose back to variables x components. cumsum of non-negative squared
      # correlations is monotone non-decreasing; clamp+scale (a monotone map) preserves that order.
      cumrep <- t(apply(sq, 1, cumsum))
      # `cumrep[] <-` keeps the matrix dims/dimnames; a bare `pmin(...) * 100` drops them to a vector.
      cumrep[] <- pmin(1, pmax(0, cumrep)) * 100
      n_comp <- ncol(cumrep)
      colnames(cumrep) <- paste0("PC", seq_len(n_comp))
      retained_idx <- if (!is.null(x$retained_components)) as.integer(x$retained_components) else n_comp
      retained_idx <- max(1L, min(retained_idx, n_comp))
      retained_val <- cumrep[, retained_idx]
      frac <- retained_val / 100
      judgement <- ifelse(frac >= cfg$representation_high, "High",
                   ifelse(frac >= cfg$representation_mostly, "Mostly Retained",
                   ifelse(frac >= cfg$representation_partial, "Partially Retained", "Low")))
      judgement_status <- ifelse(frac >= cfg$representation_high, "high",
                          ifelse(frac >= cfg$representation_mostly, "mostly",
                          ifelse(frac >= cfg$representation_partial, "partial", "low")))
      res <- tibble::tibble(Variable = rownames(sq)) %>%
        dplyr::bind_cols(tibble::as_tibble(cumrep)) %>%
        dplyr::mutate(
          Retained = retained_val,
          Judgement = judgement,
          judgement_status = judgement_status
        )
    }
  }
  else { # should be data or gathered_data
    res <- x$df
    if (!is.null(x$kmeans)) {
      # res <- res %>% dplyr::mutate(cluster=factor(x$kmeans$cluster)) # this caused error when input had column x.
      res$cluster <- factor(x$kmeans$cluster)
    }
    # PC score columns appended to the data use the USER-FACING scores (issue #27224):
    # prcomp's own scores by default, or SD-1 standardized scores when the analysis was run with
    # score_scale = "unit_variance". Falls back to x$x for models saved before #27224 and for
    # k-means fits (which never set $scores through a UI option).
    res <- res %>% dplyr::bind_cols(as.data.frame(get_stored_prcomp_scores(x)))
    column_names <- attr(x$rotation, "dimname")[[1]]
    if (normalize_data) {
      res <- res %>% dplyr::mutate(dplyr::across(dplyr::all_of(column_names), exploratory::normalize))
    }

    if (type == "data" && !is.null(x$silhouette)) {
      # Bind per-row silhouette (aligned positionally to x$df, same as the cluster column above).
      res <- res %>% dplyr::bind_cols(x$silhouette)
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
