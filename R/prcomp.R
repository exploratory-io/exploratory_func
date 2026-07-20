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

#' do PCA
#' allow_single_column - Do not throw error and go ahead with PCA even if only one column is left after preprocessing. For K-means.
#' retained_components - Number of principal components the report treats as retained. NULL = auto (use parallel analysis recommendation). Clamped to [1, number of components].
#' with_report_data - Whether to compute and attach the redesigned PCA report data (parallel analysis, Kaiser, retained/diagnostics) AND apply sign stabilization. Pure-PCA only; exp_kmeans passes FALSE so k-means fits are neither given report data nor sign-flipped. (issue #37019)
#' @export
do_prcomp <- function(df, ..., normalize_data=TRUE, max_nrow = NULL, allow_single_column = FALSE, seed = 1, na.rm = TRUE, retained_components = NULL, with_report_data = TRUE) {
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
    # "scale." is an argument name. There is no such operator like ".=". 
    fit <- prcomp(cleaned_df, scale.=normalize_data)
    fit$correlation <- cor(cleaned_df) # Calculate correlation for screeplot.
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
        variable_pc_correlations <- cor(cleaned_df, fit$x)
        vapply(seq_len(ncol(variable_pc_correlations)), function(i) {
          col <- variable_pc_correlations[, i]
          strongest <- col[which.max(abs(col))]
          if (length(strongest) == 0 || is.na(strongest) || strongest >= 0) 1 else -1
        }, numeric(1))
      }, error = function(e) rep(1, ncol(fit$rotation)))
      fit$rotation <- sweep(fit$rotation, 2, sign_multiplier, "*")
      fit$x <- sweep(fit$x, 2, sign_multiplier, "*")

      fit$parallel <- tryCatch(compute_parallel_analysis(cleaned_df), error = function(e) NULL)
      fit$kaiser_components <- tryCatch(
        if (normalize_data) as.integer(sum(eigen(fit$correlation)$values >= 1)) else NA_integer_,
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
      fit$input_diagnostics <- tryCatch({
        variable_sd <- vapply(cleaned_df, sd, numeric(1))
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
      normalized <- isTRUE(x$normalize_data)
      variables_used <- length(d$variable_sd)
      excluded_names <- d$excluded_variables
      excluded_display <- if (length(excluded_names) == 0) "-" else paste(excluded_names, collapse = ", ")
      excluded_pct <- d$excluded_row_rate * 100
      scale_ratio <- d$scale_ratio
      scale_display <- if (is.na(scale_ratio)) "-" else format(round(scale_ratio, 1), nsmall = 1)
      scale_status <- if (!normalized && is.finite(scale_ratio) && scale_ratio >= cfg$scale_ratio_warning) "scale_warning" else "ok"
      res <- tibble::tibble(
        Metric = c("Rows Used", "Rows Excluded", "Variables Used", "Excluded Variables",
                   "Normalization", "SD Ratio (Max/Min)", "Rows vs Variables"),
        Value = c(
          as.character(d$analyzed_row_count),
          paste0(d$excluded_row_count, " (", format(round(excluded_pct, 1), nsmall = 1), "%)"),
          as.character(variables_used),
          excluded_display,
          if (normalized) "Yes" else "No",
          scale_display,
          paste0(d$analyzed_row_count, " rows / ", variables_used, " variables")
        ),
        Description = c(
          "Number of rows used after removing missing values.",
          "Number and rate of rows removed because of missing values.",
          "Number of variables used in the analysis.",
          "Variables dropped before analysis because they had only NA or a single value.",
          "Whether variables were scaled to unit variance before analysis.",
          "Ratio of the largest to the smallest variable standard deviation.",
          "Number of rows compared with the number of variables."
        ),
        status = c(
          "ok",
          if (d$excluded_row_rate >= cfg$na_exclusion_warning) "high_na_exclusion" else "ok",
          "ok",
          if (length(excluded_names) == 0) "na" else "ok",
          "ok",
          scale_status,
          if (d$analyzed_row_count <= variables_used) "few_rows" else "ok"
        )
      )
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
      # Reconstruct the used numeric variables. variable_sd is named over exactly those columns
      # (== rownames(x$rotation)), the SAME reconstruction the fit uses for cor(cleaned_df, fit$x).
      cleaned_df <- x$df[, names(x$input_diagnostics$variable_sd), drop = FALSE]
      # 主成分負荷量 = correlation between variable and score (signed), variables x components.
      signed_loadings <- cor(cleaned_df, x$x)
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
      cleaned_df <- x$df[, names(x$input_diagnostics$variable_sd), drop = FALSE]
      signed_loadings <- cor(cleaned_df, x$x)
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
      cleaned_df <- x$df[, names(x$input_diagnostics$variable_sd), drop = FALSE]
      signed_loadings <- cor(cleaned_df, x$x) # variables x components
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
      res <- tibble::tibble(Variable = character(0), Component = character(0), Coefficient = numeric(0))
    }
    else {
      res <- tibble::as_tibble(x$rotation, rownames = "Variable") %>%
        tidyr::gather(Component, Coefficient, dplyr::starts_with("PC"), convert = TRUE) %>%
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
      cleaned_df <- x$df[, names(x$input_diagnostics$variable_sd), drop = FALSE]
      # cor(variable, score) -- signed correlations, variables x components. Same basis the fit
      # uses for sign stabilization and the component_profiles / loadings_signed branches.
      signed_loadings <- cor(cleaned_df, x$x)
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
      cleaned_df <- x$df[, names(x$input_diagnostics$variable_sd), drop = FALSE]
      sq <- cor(cleaned_df, x$x)^2 # squared correlations, variables x components.
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
    res <- res %>% dplyr::bind_cols(as.data.frame(x$x))
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
