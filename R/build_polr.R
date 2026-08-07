# Ordered Logistic Regression (a.k.a. Proportional Odds Model) for a 3+ level
# ordered-categorical target variable (e.g. an NPS-style Detractor / Passive /
# Promoter outcome). Modeled on build_glm()/build_lm.fast()'s calling
# convention (NSE target + predictor columns, optional weight column, optional
# train/test split) so that the property dialog on the tam side can reuse the
# same "columnSelect" / "columnsEditor" widgets already used for Logistic
# Regression.
#
# Deliberately NOT included in v1 (see design doc referenced from the tam PR
# for the full list): SMOTE / imbalanced-data handling, predictor
# normalization, outlier filtering, marginal effects, partial dependence, and
# permutation-based variable importance. These are all present on
# build_lm.fast() but add a large amount of machinery that is disproportionate
# to a first version of a brand-new analytics type; a 3+ level ordered target
# also makes several of them (e.g. SMOTE's binary-minority-class assumption)
# not directly applicable without their own design discussion.

#' Fit an Ordered Logistic Regression (proportional odds) model.
#'
#' @param df Data frame.
#' @param target Target (objective) column (NSE). Must end up as an ordered
#'   factor with 3 or more levels. A character or unordered factor column is
#'   automatically coerced into an ordered factor (character values are
#'   ordered alphabetically; an existing factor's level order is kept as-is)
#'   -- use a "Set Category Values and Order" step upstream to control the
#'   order explicitly.
#' @param ... Predictor columns (NSE, tidyselect semantics), same convention
#'   as build_lm.fast()/build_glm().
#' @param predictor_funs Named list of per-predictor derivation functions
#'   (same convention as build_lm.fast()/build_glm(), e.g. extracting a date
#'   part before modeling).
#' @param weight Optional case-weight column (NSE).
#' @param weight_fun Optional function applied to the weight column before
#'   modeling.
#' @param max_nrow If the (per-group) input has more rows than this, a random
#'   sample of this size is used to keep fitting time bounded.
#' @param group_cols Optional vector of column names to fit one model per
#'   group (used for Repeat By in Analytics View).
#' @param seed Random seed used for sampling / test-data split.
#' @param test_rate Fraction of rows held out as test data. 0 means no split.
#' @param test_split_type "random" or "ordered", same convention as
#'   build_lm.fast().
#' @param keep.source Whether to retain the (pre-split) source data in the
#'   source.data column.
#' @param max_pd_vars Maximum number of predictors to compute partial dependence
#'   for (the most important ones are kept), mirroring build_lm.fast().
#' @param pd_grid_resolution Number of grid points per predictor for partial
#'   dependence.
#' @param pd_sample_size Maximum number of rows sampled when computing partial
#'   dependence.
#' @export
build_polr <- function(df,
                        target,
                        ...,
                        predictor_funs = NULL,
                        weight = NULL,
                        weight_fun = NULL,
                        max_nrow = 50000,
                        group_cols = NULL,
                        seed = 1,
                        test_rate = 0,
                        test_split_type = "random",
                        max_pd_vars = 20,
                        pd_grid_resolution = 20,
                        pd_sample_size = 500,
                        keep.source = TRUE) {
  validate_empty_data(df)

  target_col <- tidyselect::vars_select(names(df), !!rlang::enquo(target))
  orig_selected_cols <- tidyselect::vars_select(names(df), !!!rlang::quos(...))

  if (length(orig_selected_cols) == 0) {
    stop("At least 1 Predictor Variable is required.")
  }

  # make character predictors factor sorted by frequency, mirroring build_glm()/build_lm.fast().
  for (col in orig_selected_cols) {
    if (is.character(df[[col]])) {
      df[[col]] <- forcats::fct_infreq(df[[col]])
    }
  }

  if (!is.null(predictor_funs)) {
    df <- df %>% mutate_predictors(orig_selected_cols, predictor_funs)
    selected_cols <- names(unlist(predictor_funs))
  } else {
    selected_cols <- orig_selected_cols
  }

  weight_col <- tidyselect::vars_select(names(df), !!rlang::enquo(weight))
  if (is.null(weight_col) || length(weight_col) == 0) {
    weight_col <- NULL
  }
  if (!is.null(weight_col) && !is.null(weight_fun)) {
    weight_funs <- list(weight_fun)
    names(weight_funs) <- weight_col
    df <- df %>% mutate_predictors(weight_col, weight_funs)
  }
  if (!is.null(weight_col) && min(df[[weight_col]], na.rm = TRUE) <= 0) {
    # Weights must be strictly positive or polr()'s optimizer fails at prediction time.
    stop("Weight column must be positive.")
  }
  if (!is.null(weight_col)) {
    df <- df %>%
      dplyr::mutate(!!rlang::sym(weight_col) := ifelse(is.na(!!rlang::sym(weight_col)), 1, !!rlang::sym(weight_col)))
  }

  # --- Coerce / validate the target as an ordered factor with 3+ levels. ---
  if (is.character(df[[target_col]])) {
    df[[target_col]] <- factor(df[[target_col]], ordered = TRUE)
  } else if (is.factor(df[[target_col]]) && !is.ordered(df[[target_col]])) {
    df[[target_col]] <- factor(df[[target_col]], levels = levels(df[[target_col]]), ordered = TRUE)
  } else if (!is.ordered(df[[target_col]])) {
    stop(paste0(
      "Column to predict (", target_col, ") for Ordered Logistic Regression must be a categorical (factor) column. ",
      "Use 'Set Category Values and Order' to convert a numeric column first, if needed."
    ))
  }
  n_target_levels <- length(levels(df[[target_col]]))
  if (n_target_levels < 3) {
    stop(paste0(
      "Column to predict (", target_col, ") for Ordered Logistic Regression must have 3 or more categories (it has ",
      n_target_levels, "). For a 2-category outcome, use Logistic Regression instead."
    ))
  }

  if (test_rate < 0 | 1 <= test_rate) {
    stop("test_rate must be between 0 (inclusive) and 1 (exclusive)")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # deal with group columns by index because those names might be changed. NOTE: this must run
  # BEFORE dplyr::group_by() below (which reads colnames(df)[group_col_index]) and BEFORE the
  # "grouping column reused as variable" check further down (which reads grouped_by(df), i.e.
  # AFTER group_by() actually ran) -- mirrors build_glm()'s proven ordering.
  group_col_index <- colnames(df) %in% group_cols
  reserved_names <- c(
    "model", "source.data", ".train_data", ".test_data", ".model_metadata", ".target_col",
    # for tidy
    "term", "estimate", "odds.ratio", "std.error", "statistic", "p.value", "conf.low", "conf.high", "coefficient_type",
    # for glance
    "logLik", "AIC", "BIC", "deviance", "null.deviance", "df.residual", "df.null", "edf", "nobs",
    "mcfadden.r.squared", "n_classes"
  )
  colnames(df)[group_col_index] <- avoid_conflict(reserved_names, colnames(df)[group_col_index], ".group")
  colnames(df) <- make.unique(colnames(df), sep = "")

  if (!is.null(group_cols)) {
    df <- dplyr::group_by(df, !!!rlang::syms(colnames(df)[group_col_index]))
  }

  # Filter out rows with NA in the target or any predictor -- polr() would silently drop these
  # anyway, but doing it up front keeps the training/test split counts consistent.
  for (col in c(target_col, selected_cols)) {
    df <- df %>% dplyr::filter(!is.na(!!rlang::sym(col)))
  }

  # check if grouping columns are (still, after the possible rename above) in use as the
  # target/predictor/weight columns.
  group_col_names <- grouped_by(df)
  grouped_var <- group_col_names[group_col_names %in% c(target_col, selected_cols, weight_col)]
  if (length(grouped_var) == 1) {
    stop(paste0(grouped_var, " is a grouping column. Please remove it from variables."))
  } else if (length(grouped_var) > 0) {
    stop(paste0(paste(grouped_var, collapse = ", "), " are grouping columns. Please remove them from variables."))
  }

  rhs <- paste0("`", selected_cols, "`", collapse = " + ")
  fml <- stats::as.formula(paste0("`", target_col, "` ~ ", rhs))

  each_func <- function(source_data) {
    if (!is.null(seed)) {
      set.seed(seed)
    }

    data <- source_data
    if (!is.null(max_nrow) && nrow(data) > max_nrow) {
      data <- data %>% sample_rows(max_nrow)
    }

    test_index <- sample_df_index(data, rate = test_rate, ordered = (test_split_type == "ordered"))
    train_data <- safe_slice(data, test_index, remove = TRUE)
    test_data <- if (test_rate > 0) safe_slice(data, test_index, remove = FALSE) else NULL

    # Drop unused predictor/target factor levels from the TRAINING data only. This mirrors
    # augment.glm_exploratory_0's newdata-side filtering: prediction on test_data later filters
    # out rows whose predictor levels were never seen during training (see augment.polr_exploratory_0).
    for (col in c(target_col, selected_cols)) {
      if (is.factor(train_data[[col]])) {
        train_data[[col]] <- forcats::fct_drop(train_data[[col]])
      }
    }
    if (length(levels(train_data[[target_col]])) < 3) {
      stop("Fewer than 3 categories remain in the training data after sampling/splitting. Try lowering the test data rate.")
    }

    # `fml`'s environment is build_polr()'s frame, not this each_func() call's frame.
    # MASS::polr() -> stats::model.frame.default() resolves the `weights =` expression
    # using `environment(formula)` (falling back to the calling frame only when the
    # formula has none), so with a weight column it looks for `train_data`/`weight_col`
    # in build_polr()'s frame and fails with "object 'train_data' not found" even though
    # both are clearly in scope here. Rebind the formula's environment to this call's
    # frame so `weights = train_data[[weight_col]]` resolves where it's actually defined.
    local_fml <- fml
    environment(local_fml) <- environment()

    model <- tryCatch({
      if (is.null(weight_col)) {
        MASS::polr(local_fml, data = train_data, Hess = TRUE, method = "logistic")
      } else {
        MASS::polr(local_fml, data = train_data, weights = train_data[[weight_col]], Hess = TRUE, method = "logistic")
      }
    }, error = function(e) {
      # Error message was changed across dplyr/MASS versions in ways that are hard to predict here,
      # so surface the raw message rather than trying to remap it, except for this one very common case.
      if (stringr::str_detect(e$message, "contrasts can be applied only to factors with 2 or more levels")) {
        stop("more than 1 unique values are expected for categorical columns assigned as predictors")
      }
      stop(e$message)
    })

    # Mark this as a multiclass model so the model-agnostic report helpers
    # (ml_report_basic_info() on the tam side, handle_partial_dependence() here)
    # take their multiclass branch, and record the target's ordered levels.
    model$classification_type <- "multi"
    model$orig_target_col <- target_col
    attr(model, "ylevels") <- levels(train_data[[target_col]])

    # The shared report helpers vif_to_dataframe() and handle_partial_dependence()
    # BOTH end with `x$terms_mapping[<name column>]`. lm/glm fit on sanitized
    # column names (c1_, c2_, ...) and use that map to restore the originals; we
    # fit on the real names, so without a map the lookup returns NULL and dplyr
    # SILENTLY DROPS the whole column -- the VIF chart loses its Variable column
    # and the partial-dependence chart loses x_name (so it cannot facet).
    # Provide an identity map covering both the bare name and the backtick-quoted
    # form, since terms(model) labels a name needing quoting as `name`.
    model$terms_mapping <- stats::setNames(
      c(selected_cols, selected_cols),
      c(selected_cols, paste0("`", selected_cols, "`"))
    )

    # --- Report diagnostics, mirroring build_lm.fast()/build_glm(). ---------
    # These must run BEFORE the terms environment is stripped below, since
    # model.matrix()/predict() on the fitted model still need it.

    # Multicollinearity (VIF). Needs 2+ terms; a perfect-collinearity failure is
    # captured as an error object so tidy(type='vif') can skip that group
    # instead of failing the whole model, mirroring calc_vif() for lm/glm.
    model$vif <- tryCatch(calc_vif_polr(model), error = function(e) e)

    # Permutation importance. Skipped for a single predictor (nothing to rank).
    model$imp_df <- if (length(selected_cols) > 1) {
      tryCatch(
        calc_permutation_importance_polr(model, target_col, selected_cols, train_data),
        error = function(e) e
      )
    } else {
      simpleError("Variable importance requires two or more variables.")
    }

    # Partial dependence, computed only for the most important predictors.
    imp_vars <- if (!is.null(model$imp_df) && !inherits(model$imp_df, "error")) {
      as.character((model$imp_df %>% dplyr::arrange(-importance))$variable)
    } else {
      as.character(selected_cols)
    }
    imp_vars <- imp_vars[seq_len(min(length(imp_vars), max_pd_vars))]
    model$imp_vars <- imp_vars
    model$partial_dependence <- if (length(imp_vars) > 0) {
      tryCatch(
        partial_dependence.polr_exploratory(
          model,
          target = target_col,
          vars = imp_vars,
          data = train_data,
          n = c(pd_grid_resolution, min(nrow(train_data), pd_sample_size))
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }

    # Strip environments to save rds size when cached, mirroring build_lm.fast()/build_glm().
    if (!is.null(model$terms)) {
      attr(model$terms, ".Environment") <- NULL
    }

    class(model) <- c("polr_exploratory_0", class(model))

    list(model = model, train_data = train_data, test_data = test_data)
  }

  ret <- df %>%
    tidyr::nest(source.data = -dplyr::group_cols()) %>%
    dplyr::mutate(.fit = purrr::map(source.data, each_func)) %>%
    dplyr::mutate(
      model = purrr::map(.fit, function(f) f$model),
      .train_data = purrr::map(.fit, function(f) f$train_data),
      .test_data = purrr::map(.fit, function(f) f$test_data),
      .target_col = target_col
    ) %>%
    dplyr::mutate(.model_metadata = purrr::map(source.data, function(sdf) {
      tryCatch(create_model_meta(sdf, fml), error = function(e) list())
    })) %>%
    dplyr::select(-.fit)

  if (!keep.source) {
    ret <- dplyr::select(ret, -source.data)
  } else {
    class(ret[["source.data"]]) <- c("list", ".source.data")
  }

  ret <- dplyr::rowwise(ret)
  class(ret$model) <- c("list", ".model", ".model.polr")
  ret
}

#' @rdname build_polr
#' @export
build_ordinal_regression <- build_polr

# Variance Inflation Factor for a MASS::polr() model.
#
# This is the same generalized-VIF computation as vif() in build_lm.R (itself
# derived from car::vif), adapted for polr's two structural differences:
#
#   1. vcov(polr) is [slope coefficients ..., zeta (category thresholds) ...].
#      The thresholds are polr's analogue of the intercept and must be dropped;
#      lm/glm drop a single leading "(Intercept)" row instead.
#   2. coef(polr) contains NO intercept, so build_lm's
#      `names(coefficients(mod)[1]) == "(Intercept)"` test is FALSE and its
#      "No intercept: vifs may not be sensible" branch would fire wrongly.
#      model.matrix(polr) DOES include an intercept column (assign == 0), so
#      the assign vector is filtered instead.
#
# Verified to agree with car::vif() on a polr fit to 4 decimal places.
calc_vif_polr <- function(model) {
  if (any(is.na(stats::coef(model)))) {
    # Perfect collinearity: report the offending variables the same way
    # calc_vif() does for lm/glm so the caller can surface a useful message.
    coef_vec <- stats::coef(model)
    na_coef_names <- names(coef_vec[is.na(coef_vec)])
    stop(paste0("Variables causing perfect collinearity : ", paste(na_coef_names, collapse = ", ")))
  }
  v <- stats::vcov(model)
  n_coef <- length(stats::coef(model))
  # Drop the zeta (threshold) rows/columns; keep only the slope coefficients.
  v <- v[seq_len(n_coef), seq_len(n_coef), drop = FALSE]

  mm <- stats::model.matrix(model)
  assign <- attr(mm, "assign")
  assign <- assign[assign != 0] # drop the intercept column

  terms_labels <- labels(stats::terms(model))
  n_terms <- length(terms_labels)
  if (n_terms < 2) {
    stop("model contains fewer than 2 terms")
  }

  R <- stats::cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n_terms, 3)
  rownames(result) <- terms_labels
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (term in seq_len(n_terms)) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- result[, 1]
  } else {
    result[, 3] <- result[, 1]^(1 / (2 * result[, 2]))
  }
  result
}

# Permutation importance for a MASS::polr() model.
#
# Mirrors calc_permutation_importance_rpart_multiclass(): predict() returns an
# n x n_category probability matrix, and the loss is the probability-error of the
# ground-truth category (a plain negative probability rather than a negative LOG
# probability, so a zero probability for the observed class cannot contribute an
# infinite penalty).
calc_permutation_importance_polr <- function(fit, target, vars, data) {
  if (!requireNamespace("mmpf", quietly = TRUE)) {
    return(simpleError("Package 'mmpf' is not available. Permutation importance cannot be calculated."))
  }
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    tryCatch({
      mmpf::permutationImportance(
        data, var, target, fit,
        nperm = 1, # 1 permutation for performance, matching the other models.
        predict.fun = function(object, newdata) {
          stats::predict(object, newdata = newdata, type = "probs")
        },
        loss.fun = function(x, y) {
          sum(-(x[match(y[[1]][row(x)], colnames(x)) == col(x)]), na.rm = TRUE)
        }
      )
    }, error = function(e) {
      stop(paste0(e$message, " (while calculating permutation importance for variable '", var, "')"),
           call. = FALSE)
    })
  })
  importances <- purrr::flatten_dbl(importances)
  # Negative importance can happen by chance with permutation importance; clamp to 0.
  importances_df <- tibble::tibble(variable = vars, importance = pmax(importances, 0))
  importances_df %>% dplyr::arrange(-importance)
}

# Partial dependence for a MASS::polr() model, in the same shape
# (a data.frame with "pd" class and vars/target/points attributes) that
# handle_partial_dependence() consumes.
#
# Mirrors partial_dependence.rpart(): the prediction is a per-category
# probability matrix, so attr(,"target") is set to the ordered category levels
# rather than the target column name -- handle_partial_dependence() uses that to
# take its multiclass branch.
partial_dependence.polr_exploratory <- function(fit, target, vars = colnames(data),
                                                n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)),
                                                interaction = FALSE, uniform = TRUE, data, ...) {
  if (!requireNamespace("mmpf", quietly = TRUE)) {
    return(NULL)
  }

  # mmpf::marginalPrediction cannot handle column names containing characters
  # like a comma (see the same note in build_lm.R). lm/glm avoid this by fitting
  # on sanitized names (c1_, c2_, ...); we fit on the real names, so instead
  # hand mmpf a SAFE-NAMED COPY of the data and rename back inside predict.fun
  # before the model sees it. Without this, a data frame with a column name like
  # the repo's multibyte + symbol stress-test name silently yields a 0-row
  # partial-dependence table and the chart renders empty.
  orig_names <- colnames(data)
  safe_names <- paste0(".pdcol", seq_along(orig_names))
  safe_data <- data
  colnames(safe_data) <- safe_names
  safe_of <- stats::setNames(safe_names, orig_names)
  safe_vars <- unname(safe_of[as.character(vars)])

  predict.fun <- function(object, newdata) {
    colnames(newdata) <- orig_names[match(colnames(newdata), safe_names)]
    stats::predict(object, newdata = newdata, type = "probs")
  }

  # Grid points based on quantiles so an outlier does not dominate the grid,
  # matching partial_dependence.lm_exploratory()/partial_dependence.rpart().
  points <- list()
  quantile_points <- list()
  for (i in seq_along(vars)) {
    cname <- as.character(vars)[[i]]
    sname <- safe_vars[[i]]
    if (is.numeric(data[[cname]])) {
      coldata <- data[[cname]]
      minv <- min(coldata, na.rm = TRUE)
      maxv <- max(coldata, na.rm = TRUE)
      grid <- minv + (0:20) / 20 * (maxv - minv)
      quantile_grid <- stats::quantile(coldata, probs = 1:24 / 25)
      quantile_points[[cname]] <- quantile_grid
      points[[sname]] <- sort(unique(c(grid, quantile_grid)))
    } else {
      points[[sname]] <- unique(data[[cname]])
    }
  }

  args <- list(
    "data" = safe_data,
    "vars" = safe_vars,
    "n" = n,
    "model" = fit,
    "points" = points,
    "predict.fun" = predict.fun,
    ...
  )

  if (length(safe_vars) > 1L && !interaction) {
    pd <- data.table::rbindlist(sapply(safe_vars, function(x) {
      args$vars <- x
      if ("points" %in% names(args)) {
        args$points <- args$points[x]
      }
      do.call(mmpf::marginalPrediction, args)
    }, simplify = FALSE), fill = TRUE)
    data.table::setcolorder(pd, c(safe_vars, colnames(pd)[!colnames(pd) %in% safe_vars]))
  } else {
    args$vars <- as.character(safe_vars)
    pd <- do.call(mmpf::marginalPrediction, args)
  }

  # Restore the original predictor column names before handing the frame on --
  # every downstream consumer (handle_partial_dependence's `vars` attribute,
  # the chart's x_name) works in terms of the user's column names.
  pd <- as.data.frame(pd)
  restored <- orig_names[match(colnames(pd), safe_names)]
  colnames(pd) <- ifelse(is.na(restored), colnames(pd), restored)

  attr(pd, "class") <- c("pd", "data.frame")
  attr(pd, "interaction") <- interaction == TRUE
  attr(pd, "target") <- attr(fit, "ylevels")
  attr(pd, "vars") <- vars
  attr(pd, "points") <- points
  attr(pd, "quantile_points") <- quantile_points
  pd
}

#' Coefficient / odds-ratio table for an Ordered Logistic Regression model.
#' @param x A model built by build_polr(), with class polr_exploratory_0.
#' @param type What to return: "coefficients" (default), "vif",
#'   "importance", or "partial_dependence". Mirrors tidy.glm_exploratory().
#' @param conf.int Whether to compute a (Wald, i.e. normal-approximation) confidence interval.
#' @param conf.level Confidence level for conf.int.
#' @param exponentiate Whether to add an odds.ratio column (exp(estimate)) for slope coefficients.
#' @param pretty.name Whether to rename columns to display-friendly names.
#' @export
tidy.polr_exploratory_0 <- function(x, type = "coefficients", conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, pretty.name = FALSE, ...) {
  if (inherits(x, "error")) {
    return(data.frame())
  }
  # Non-coefficient outputs mirror tidy.glm_exploratory()'s switch. They return
  # an EMPTY data.frame (not an error) when unavailable so a single failing
  # Repeat By group is skipped rather than failing the whole chart.
  if (identical(type, "vif")) {
    if (!is.null(x$vif) && !inherits(x$vif, "error")) {
      return(vif_to_dataframe(x))
    }
    return(data.frame())
  }
  if (identical(type, "partial_dependence")) {
    return(handle_partial_dependence(x))
  }
  if (identical(type, "importance") || identical(type, "permutation_importance")) {
    if (is.null(x$imp_df) || inherits(x$imp_df, "error")) {
      # Structured empty frame so callers can safely arrange(desc(importance)).
      return(data.frame(variable = character(), importance = numeric(), p.value = numeric()))
    }
    ret <- x$imp_df
    # Attach the smallest P value among the model terms belonging to each
    # variable (a categorical predictor contributes one term per level), so the
    # importance chart can color bars by significance the way glm's does.
    coef_df <- tidy.polr_exploratory_0(x, type = "coefficients", conf.int = FALSE, exponentiate = FALSE)
    slope_df <- coef_df %>% dplyr::filter(coefficient_type == "coefficient")
    # Prefix match on the raw string rather than a regex: a column name can
    # legitimately contain regex metacharacters (see the repo's multibyte +
    # symbol stress-test name), which would silently mis-match under str_detect.
    ret <- ret %>% dplyr::mutate(p.value = purrr::map_dbl(variable, function(var) {
      bare_terms <- sub("^`", "", as.character(slope_df$term))
      matched <- slope_df$p.value[startsWith(bare_terms, as.character(var))]
      if (length(matched) == 0 || all(is.na(matched))) NA_real_ else min(matched, na.rm = TRUE)
    }))
    if (identical(type, "permutation_importance")) {
      ret <- ret %>% dplyr::rename(term = variable)
    }
    return(ret)
  }

  smry <- summary(x)$coefficients
  n_coef <- length(stats::coef(x))
  n_zeta <- length(x$zeta)

  ret <- tibble::tibble(
    term = rownames(smry),
    estimate = as.numeric(smry[, 1]),
    std.error = as.numeric(smry[, 2]),
    statistic = as.numeric(smry[, 3]),
    # polr() intentionally omits p-values (the Wald statistic is not exactly t-distributed);
    # this is the normal-approximation p-value shown in MASS::polr's own help page example.
    p.value = 2 * stats::pnorm(abs(as.numeric(smry[, 3])), lower.tail = FALSE),
    coefficient_type = c(rep("coefficient", n_coef), rep("intercept", n_zeta))
  )

  if (conf.int) {
    z <- stats::qnorm(1 - (1 - conf.level) / 2)
    ret$conf.low <- ret$estimate - z * ret$std.error
    ret$conf.high <- ret$estimate + z * ret$std.error
  }

  if (exponentiate) {
    is_coef <- ret$coefficient_type == "coefficient"
    ret$odds.ratio <- ifelse(is_coef, exp(ret$estimate), NA_real_)
    if (conf.int) {
      ret$odds.ratio.conf.low <- ifelse(is_coef, exp(ret$conf.low), NA_real_)
      ret$odds.ratio.conf.high <- ifelse(is_coef, exp(ret$conf.high), NA_real_)
    }
  }

  # Reference (base) level for each categorical predictor's dummy term, e.g.
  # `g` = "gB"/"gC" both get base.level = "A" (the level dropped by treatment
  # contrasts, i.e. x$xlevels[[var]][1]) -- same helper and same semantics
  # build_glm() uses (xlevels_to_base_level_table), joined by term string so the
  # report's coefficient interpretation examples can say "compared to <base>".
  if (length(x$xlevels) > 0) {
    base_level_table <- xlevels_to_base_level_table(x$xlevels)
    ret <- ret %>% dplyr::left_join(base_level_table, by = "term")
  }

  if (pretty.name) {
    ret <- ret %>% dplyr::rename(
      Term = term,
      Coefficient = estimate,
      `Std. Error` = std.error,
      `z value` = statistic,
      `P Value` = p.value,
      Type = coefficient_type
    )
    if ("conf.low" %in% colnames(ret)) {
      ret <- ret %>% dplyr::rename(`Conf. Low` = conf.low, `Conf. High` = conf.high)
    }
    if ("odds.ratio" %in% colnames(ret)) {
      ret <- ret %>% dplyr::rename(`Odds Ratio` = odds.ratio)
    }
    if ("odds.ratio.conf.low" %in% colnames(ret)) {
      ret <- ret %>% dplyr::rename(`Odds Ratio Conf. Low` = odds.ratio.conf.low, `Odds Ratio Conf. High` = odds.ratio.conf.high)
    }
    if ("base.level" %in% colnames(ret)) {
      ret <- ret %>% dplyr::rename(`Base Level` = base.level)
    }
  }

  ret
}

#' Model fit summary (glance) for an Ordered Logistic Regression model.
#' @param x A model built by build_polr(), with class polr_exploratory_0.
#' @param pretty.name Whether to rename columns to display-friendly names.
#' @export
glance.polr_exploratory_0 <- function(x, pretty.name = FALSE, ...) {
  ll <- stats::logLik(x)
  ll_val <- as.numeric(ll)
  edf_val <- length(stats::coef(x)) + length(x$zeta)
  nobs_val <- nrow(x$model)
  aic_val <- -2 * ll_val + 2 * edf_val
  bic_val <- -2 * ll_val + log(nobs_val) * edf_val
  df_residual_val <- nobs_val - edf_val

  # MASS::polr() does not compute a null (intercept-only) model on its own, unlike glm();
  # refit one here so that McFadden's Pseudo R-Squared can be reported, mirroring what the
  # existing Logistic Regression report computes from glm()'s built-in null.deviance/df.null.
  null_fit <- tryCatch({
    resp <- x$model[[1]]
    wts <- stats::model.weights(x$model)
    if (is.null(wts)) {
      MASS::polr(resp ~ 1, Hess = FALSE)
    } else {
      MASS::polr(resp ~ 1, weights = wts, Hess = FALSE)
    }
  }, error = function(e) NULL)

  null_deviance_val <- NA_real_
  df_null_val <- NA_integer_
  mcfadden_r_squared_val <- NA_real_
  if (!is.null(null_fit)) {
    null_deviance_val <- null_fit$deviance
    null_edf_val <- length(stats::coef(null_fit)) + length(null_fit$zeta)
    df_null_val <- nobs_val - null_edf_val
    if (is.finite(null_deviance_val) && null_deviance_val != 0) {
      mcfadden_r_squared_val <- 1 - (x$deviance / null_deviance_val)
    }
  }

  ret <- tibble::tibble(
    n_classes = length(x$lev),
    nobs = nobs_val,
    edf = edf_val,
    logLik = ll_val,
    AIC = aic_val,
    BIC = bic_val,
    deviance = x$deviance,
    df.residual = df_residual_val,
    null.deviance = null_deviance_val,
    df.null = df_null_val,
    mcfadden.r.squared = mcfadden_r_squared_val
  )

  if (pretty.name) {
    ret <- ret %>% dplyr::rename(
      `Number of Categories` = n_classes,
      `Rows` = nobs,
      `Degree of Freedom` = edf,
      `Log Likelihood` = logLik,
      `Residual Deviance` = deviance,
      `Residual DF` = df.residual,
      `Null Deviance` = null.deviance,
      `Null Model DF` = df.null,
      `McFadden R-Squared` = mcfadden.r.squared
    )
  }

  ret
}

#' Row-level predictions (predicted class + per-class probability) for an
#' Ordered Logistic Regression model.
#' @param x A model built by build_polr(), with class polr_exploratory_0.
#' @param data Original data (used when newdata is not given).
#' @param newdata New data to predict on. Rows whose predictor factor levels
#'   were not seen during training are dropped, mirroring
#'   augment.glm_exploratory_0().
#' @export
augment.polr_exploratory_0 <- function(x, data = NULL, newdata = NULL, ...) {
  target_data <- newdata
  if (is.null(target_data)) {
    target_data <- data
  }
  if (is.null(target_data)) {
    target_data <- x$model
  }

  if (!is.null(x$xlevels) && length(x$xlevels) > 0) {
    for (i in seq_along(x$xlevels)) {
      col <- names(x$xlevels)[[i]]
      if (col %in% colnames(target_data)) {
        target_data <- target_data %>%
          dplyr::filter(!!rlang::sym(col) %in% !!x$xlevels[[i]])
      }
    }
  }

  probs <- tryCatch(
    stats::predict(x, newdata = target_data, type = "probs"),
    error = function(e) stats::predict(x, type = "probs")
  )
  predicted_class <- tryCatch(
    stats::predict(x, newdata = target_data, type = "class"),
    error = function(e) stats::predict(x, type = "class")
  )

  if (is.null(dim(probs))) {
    # A 1-row newdata collapses predict()'s result to a plain named vector; restore the matrix shape.
    probs <- matrix(probs, nrow = 1, dimnames = list(NULL, names(probs)))
  }

  prob_df <- as.data.frame(probs, stringsAsFactors = FALSE)
  colnames(prob_df) <- paste0("predicted_probability_", colnames(prob_df))

  dplyr::bind_cols(
    tibble::as_tibble(target_data),
    prob_df,
    tibble::tibble(.fitted = predicted_class)
  )
}

#' Prediction accuracy summary (training and/or test) for an Ordered Logistic
#' Regression model. Analogous in spirit to evaluate_binary_training_and_test()
#' for Logistic Regression, scoped down to a single overall accuracy metric
#' (multi-class ROC/AUC is out of scope for v1).
#'
#' @param df A model data frame returned by build_polr().
#' @param data "training", "test", or "training_and_test".
#' @param pretty.name Whether to rename columns to display-friendly names.
#' @export
evaluate_polr <- function(df, data = "training", pretty.name = FALSE) {
  if (!("model" %in% colnames(df))) {
    stop("model column is required. Run build_polr() first.")
  }

  data_types <- switch(data,
    training = c("Training"),
    test = c("Test"),
    training_and_test = c("Training", "Test"),
    stop('data argument has to be "training", "test", or "training_and_test".')
  )

  group_cols <- grouped_by(df)

  ret <- df %>%
    dplyr::ungroup() %>%
    dplyr::mutate(.res = purrr::pmap(list(model, .train_data, .test_data, .target_col), function(m, tr, te, tc) {
      evaluate_polr_one_model(m, tr, te, tc, data_types)
    })) %>%
    dplyr::select(!!!rlang::syms(group_cols), .res) %>%
    tidyr::unnest(.res)

  if (length(group_cols) > 0) {
    ret <- ret %>% dplyr::group_by(!!!rlang::syms(group_cols))
  }

  ret
}

#' Compute the training/test accuracy rows for a single fitted polr model.
#' Kept as a standalone, pure function (rather than a closure inside
#' evaluate_polr()) so it is directly unit-testable.
#' @param model A polr_exploratory_0 model object.
#' @param train_data The data frame the model was trained on.
#' @param test_data The held-out test data frame, or NULL if there is none.
#' @param target_col Name of the target/objective column in train_data/test_data.
#' @param data_types Character vector, any of "Training"/"Test", naming which rows to compute.
#' @export
evaluate_polr_one_model <- function(model, train_data, test_data, target_col, data_types = c("Training", "Test")) {
  data_sets <- list(Training = train_data, Test = test_data)
  purrr::map_dfr(data_types, function(dt) {
    eval_data <- data_sets[[dt]]
    if (is.null(eval_data) || nrow(eval_data) == 0) {
      return(tibble::tibble(`Data Type` = dt, Rows = 0L, `Accuracy Rate` = NA_real_, `Misclass. Rate` = NA_real_))
    }
    augmented <- augment.polr_exploratory_0(model, newdata = eval_data)
    actual <- eval_data[[target_col]]
    accuracy <- mean(as.character(augmented$.fitted) == as.character(actual), na.rm = TRUE)
    tibble::tibble(
      `Data Type` = dt,
      Rows = nrow(eval_data),
      `Accuracy Rate` = accuracy,
      `Misclass. Rate` = 1 - accuracy
    )
  })
}
