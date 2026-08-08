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
    # out rows whose predictor levels were never seen during training (see augment.clm_exploratory_0).
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

    # ordinal::clm() rather than MASS::polr(): clm is the only one of the two that can
    # test the proportional-odds assumption (ordinal::nominal_test()), which the report
    # spec calls its most important section. The two share a parameterization -- on the
    # same fit their coefficients AND thresholds agree to 4dp (pinned by a test) -- so
    # this swap does not change any number the report already showed, and the spec's
    # "always present the effect in the higher-category direction" requirement is
    # satisfied by clm's own convention with no sign flipping.
    model <- tryCatch({
      if (is.null(weight_col)) {
        ordinal::clm(local_fml, data = train_data, link = "logit")
      } else {
        ordinal::clm(local_fml, data = train_data, weights = train_data[[weight_col]], link = "logit")
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

    class(model) <- c("clm_exploratory_0", class(model))

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


# ordinal::clm()'s predict() returns a LIST with a `fit` element, where MASS::polr()
# returned the matrix/vector directly. Every call site went through predict(), so this
# unwraps once rather than repeating the check.
clm_predict <- function(object, newdata = NULL, type = "prob") {
  # If the RESPONSE column is present in newdata, clm's predict(type="prob") returns one
  # probability per row -- the probability of the OBSERVED class -- instead of the full
  # n x n_category matrix. Callers here always want the matrix, and mmpf hands us the raw
  # training frame (target column included), so drop it first. Symptoms of not doing this:
  # permutation importance died with "a matrix-like object is required as argument to
  # 'row'", and partial dependence silently produced a SINGLE "preds" series rather than
  # one per category.
  if (!is.null(newdata) && !is.null(object$orig_target_col) &&
      object$orig_target_col %in% colnames(newdata)) {
    newdata <- newdata[, setdiff(colnames(newdata), object$orig_target_col), drop = FALSE]
  }
  res <- if (is.null(newdata)) {
    stats::predict(object, type = type)
  } else {
    stats::predict(object, newdata = newdata, type = type)
  }
  if (is.list(res) && !is.data.frame(res) && "fit" %in% names(res)) res$fit else res
}


# Proportional-odds assumption test, per predictor.
#
# ordinal::nominal_test() is the obvious call, but it refits via update(), which
# re-evaluates the ORIGINAL `data =` argument by name in the formula's environment. Our
# fits are built inside each_func() on a local `train_data` and then have their terms
# environment stripped (to keep cached models small), so update() cannot find the data and
# every LRT silently comes back NA -- no error, just an empty-looking table.
#
# Refit explicitly from the model frame clm itself stored ($model) instead, which is
# self-contained. Verified to reproduce ordinal::nominal_test()'s LRT / Df / p-value
# exactly on a fit where update() DOES work.
clm_nominal_test <- function(x) {
  mf <- x$model
  if (is.null(mf) || !is.data.frame(mf) || ncol(mf) < 2) return(NULL)
  resp <- colnames(mf)[[1]]
  term_labels <- labels(stats::terms(x))
  if (length(term_labels) == 0) return(NULL)
  # terms() ALREADY backtick-quotes a label that needs quoting and leaves plain ones bare,
  # so the set is mixed. Strip first and re-quote uniformly -- double-quoting produced
  # ``name`` , which R parses as an empty identifier ("attempt to use zero-length variable
  # name"). Same trap as the empty-backtick R-codegen hang recorded in the repo's lessons.
  bare_labels <- gsub("^`|`$", "", term_labels)

  # Fit on SANITIZED column names. clm() builds its nominal-effects formula by pasting
  # strings together WITHOUT re-quoting, so a predictor whose name contains a space or
  # symbol produces unparseable code ("unexpected symbol") the moment it is passed as
  # `nominal =`. Backticking our own formula is not enough -- the breakage happens inside
  # clm. Renaming to .ntv1, .ntv2, ... sidesteps it entirely, and the caller-facing term
  # names are restored from bare_labels below.
  safe_labels <- paste0(".ntv", seq_along(bare_labels))
  safe_mf <- mf[, c(resp, bare_labels), drop = FALSE]
  colnames(safe_mf) <- c(".ntresp", safe_labels)

  quoted <- paste(safe_labels, collapse = " + ")
  base_fml <- stats::as.formula(paste0(".ntresp ~ ", quoted), env = environment())
  base <- tryCatch(ordinal::clm(base_fml, data = safe_mf, link = x$link), error = function(e) NULL)
  if (is.null(base)) return(NULL)
  base_ll <- as.numeric(stats::logLik(base))

  rows <- lapply(seq_along(safe_labels), function(i) {
    # Refit letting ONLY this predictor have its own effect per category boundary.
    alt <- tryCatch(
      ordinal::clm(base_fml,
                   nominal = stats::as.formula(paste0("~ ", safe_labels[[i]]), env = environment()),
                   data = safe_mf, link = x$link),
      error = function(e) NULL
    )
    if (is.null(alt)) return(NULL)
    lrt <- 2 * (as.numeric(stats::logLik(alt)) - base_ll)
    dfd <- alt$edf - base$edf
    if (!is.finite(lrt) || !is.finite(dfd) || dfd <= 0) return(NULL)
    tibble::tibble(term = bare_labels[[i]], statistic = lrt, df = dfd,
                   p.value = stats::pchisq(lrt, dfd, lower.tail = FALSE))
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(NULL)
  dplyr::bind_rows(rows)
}

# Variance Inflation Factor for an ordinal::clm() model.
# Report-top "分析対象" summary for a build_polr() model (tam#4453 spec, report
# structure follow-up, S14). Deliberately a DEDICATED function rather than an
# extension of the shared ml_report_basic_info() in tam's library.r -- that
# helper explicitly documents itself as "only the columns every model can
# supply" (it infers Categories from model$classification_type, a field
# clm_exploratory_0 objects don't set), and Category Order has no equivalent in
# any other model family. Mirrors dtree_report_basic_info()'s per-model-family
# pattern instead. Not exported (same convention as the other _report_ helpers
# in this file) -- tam preprocessors call it bare.
polr_report_basic_info <- function(df, test_mode = FALSE) {
  if (!is.data.frame(df) || !"model" %in% colnames(df)) return(data.frame())
  model <- df$model[[1]]
  if (is.null(model) || inherits(model, "error")) return(data.frame())

  target_col <- model$orig_target_col
  lvls <- levels(model$model[[1]])
  if (is.null(lvls)) lvls <- character(0)

  source_data <- if ("source.data" %in% colnames(df)) df$source.data[[1]] else NULL
  rows <- if (!is.null(source_data) && is.data.frame(source_data)) {
    nrow(source_data)
  } else if (!is.null(model$model)) {
    nrow(model$model)
  } else {
    NA_integer_
  }

  predictors <- tryCatch(labels(stats::terms(model)), error = function(e) character(0))
  bare_predictors <- gsub("^`|`$", "", predictors)
  bare_predictors <- bare_predictors[!is.na(bare_predictors) & nzchar(bare_predictors)]

  data.frame(
    `Target` = if (is.null(target_col)) NA_character_ else target_col,
    `Categories` = length(lvls),
    `Category Order` = if (length(lvls) == 0) NA_character_ else paste(lvls, collapse = " < "),
    `Predictors` = length(unique(bare_predictors)),
    `Rows` = rows,
    `Model` = "Ordered Logistic Regression",
    # Mirrors ml_report_basic_info()'s wording (#37513): the report already shows
    # Training AND Test rows throughout, so this answers "was data held out for
    # testing", not "what is being evaluated right now".
    `Evaluation` = if (isTRUE(test_mode)) "Test Data" else "Training",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# One-vs-rest probability rows for a build_polr() model, mirroring
# dtree_report_multiclass_probabilities() (tam#37499) so the report's predicted
# probability distribution chart can use the same One-vs-Rest layout: one line per
# category, split This Category / Other Categories (tam#4453 spec, report
# structure follow-up, §5-4). Unlike the rpart version this is not gated to a
# specific tree class -- any build_polr() model qualifies. Not exported (mirrors
# dtree_report_multiclass_probabilities()) -- tam preprocessors call it bare, the
# same way every other _report_ internal helper is called.
polr_report_multiclass_probabilities <- function(df) {
  if (!is.data.frame(df) || !"model" %in% colnames(df)) return(data.frame())

  model <- df$model[[1]]
  if (is.null(model) || !inherits(model, "clm_exploratory_0")) return(data.frame())

  train_data <- if (".train_data" %in% colnames(df)) df$.train_data[[1]] else NULL
  test_data <- if (".test_data" %in% colnames(df)) df$.test_data[[1]] else NULL
  target_col <- if (".target_col" %in% colnames(df)) df$.target_col[[1]] else model$orig_target_col
  if (is.null(target_col)) return(data.frame())

  levels_target <- levels(model$model[[1]])
  if (is.null(levels_target)) return(data.frame())

  make_rows <- function(data, is_test) {
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) return(data.frame())
    if (!target_col %in% colnames(data)) return(data.frame())

    probabilities <- tryCatch(
      clm_predict(model, newdata = data, type = "prob"),
      error = function(e) NULL
    )
    if (is.null(probabilities)) return(data.frame())

    probabilities <- as.data.frame(probabilities, check.names = FALSE)
    categories <- intersect(levels_target, colnames(probabilities))
    if (length(categories) == 0) categories <- colnames(probabilities)

    dplyr::bind_rows(lapply(categories, function(category) {
      actual <- as.character(data[[target_col]])
      is_positive <- actual == category
      data.frame(
        Category = category,
        `Predicted Probability` = probabilities[[category]],
        `Actual Positive` = is_positive,
        `Actual Group` = factor(
          ifelse(is_positive, "This Category", "Other Categories"),
          levels = c("This Category", "Other Categories")
        ),
        `Actual Category` = actual,
        is_test_data = is_test,
        baseline_precision = mean(is_positive, na.rm = TRUE),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }))
  }

  dplyr::bind_rows(make_rows(train_data, FALSE), make_rows(test_data, TRUE))
}

# Macro (unweighted per-category mean) Precision / Recall / Specificity / F1 for a
# multiclass label pair, using one-vs-rest confusion-matrix arithmetic per category.
# `recall` here is numerically identical to multiclass_balanced_accuracy() (both are
# macro recall by definition) -- kept as its own self-contained helper, rather than
# reusing evaluate_multi_()'s internals, so the Prediction Accuracy table (tam#4453
# spec, report structure follow-up) can report Precision/Recall/Specificity/F1
# together without depending on another function's private locals.
polr_macro_precision_recall_specificity <- function(actual, predicted) {
  actual <- as.character(actual)
  predicted <- as.character(predicted)
  valid <- !is.na(actual) & !is.na(predicted)
  actual <- actual[valid]
  predicted <- predicted[valid]
  if (length(actual) == 0) {
    return(list(precision = NA_real_, recall = NA_real_, specificity = NA_real_, f1 = NA_real_))
  }
  classes <- sort(unique(actual))
  per_class <- lapply(classes, function(k) {
    tp <- sum(actual == k & predicted == k)
    fn <- sum(actual == k & predicted != k)
    fp <- sum(actual != k & predicted == k)
    tn <- sum(actual != k & predicted != k)
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
    recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
    specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
    f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
      2 * precision * recall / (precision + recall)
    } else {
      0
    }
    c(precision = precision, recall = recall, specificity = specificity, f1 = f1)
  })
  m <- do.call(rbind, per_class)
  list(precision = mean(m[, "precision"], na.rm = TRUE),
       recall = mean(m[, "recall"], na.rm = TRUE),
       specificity = mean(m[, "specificity"], na.rm = TRUE),
       f1 = mean(m[, "f1"], na.rm = TRUE))
}

#
# This is the same generalized-VIF computation as vif() in build_lm.R (itself
# derived from car::vif), adapted for clm's structural differences:
#
#   1. vcov(clm) is [alpha (category thresholds) ..., beta (slopes) ...] --
#      thresholds FIRST, the opposite of MASS::polr, which put them last. The
#      thresholds are the ordinal analogue of the intercept and must be dropped.
#      Selected BY NAME (x$beta) rather than by position so a future ordering
#      change cannot silently compute VIF off the wrong submatrix.
#   2. coef(clm) contains the thresholds too, so build_lm's
#      `names(coefficients(mod)[1]) == "(Intercept)"` test would misfire.
#      model.matrix(clm) returns a LIST whose $X carries the design matrix, and
#      that matrix DOES include an intercept column (assign == 0).
#
# Verified to agree with car::vif() to 4 decimal places.
calc_vif_polr <- function(model) {
  # Slopes only -- clm's coef() also contains the thresholds.
  coef_names <- names(model$beta)
  if (length(coef_names) == 0 || any(is.na(coef_names))) {
    stop("model contains fewer than 2 terms")
  }

  mm <- stats::model.matrix(model)
  # clm's model.matrix() returns list(X = <design matrix>, ...).
  if (is.list(mm) && !is.matrix(mm) && "X" %in% names(mm)) {
    mm <- mm$X
  }
  mm_assign <- attr(mm, "assign")
  mm_colnames <- colnames(mm)
  all_term_labels <- labels(stats::terms(model))

  # Map each SURVIVING coefficient to its formula term index (assign's values,
  # 1-based, 0 = intercept). Matched by NAME, not position: vcov()/coef() are
  # already in coefficient order, which need not equal model.matrix's column
  # order once a term has been dropped.
  surv_term_idx <- mm_assign[match(coef_names, mm_colnames)]

  # A rank-deficient design: unlike lm/glm, whose coef() keeps a slot per
  # aliased term and fills it with NA (the case the block above used to
  # handle), ordinal::clm silently DROPS the aliased term from the fit --
  # coef() simply has no entry for it at all. So a term is "aliased" here
  # when NONE of its dummy columns survive into coef_names, not when a slot
  # is NA. Detected by diffing the full formula's term set (all_term_labels)
  # against the terms the surviving coefficients actually belong to.
  # Surfaced with the SAME message calc_vif() uses for lm/glm so the caller
  # (the report's Collinearity Error Message chart) handles both identically.
  dropped_labels <- all_term_labels[!(seq_along(all_term_labels) %in% unique(surv_term_idx))]

  # clm reports rank deficiency differently from polr: the aliased term STAYS in $beta
  # (so the "missing from coef()" check above never fires) and is flagged in $aliased
  # instead -- but vcov() omits it, so the slope submatrix selection below would fail with
  # "subscript out of bounds". Translate the flag into the same user-facing message.
  aliased_beta <- model$aliased$beta
  if (!is.null(aliased_beta) && any(aliased_beta)) {
    aliased_names <- names(aliased_beta)[aliased_beta]
    if (is.null(aliased_names)) aliased_names <- coef_names[aliased_beta]
    dropped_labels <- unique(c(dropped_labels, aliased_names))
    coef_names <- setdiff(coef_names, aliased_names)
  }

  if (length(dropped_labels) > 0) {
    stop(paste0("Variables causing perfect collinearity : ", paste(dropped_labels, collapse = ", ")))
  }

  v <- stats::vcov(model)
  # Keep ONLY the slope rows/columns, selected by name (clm puts the thresholds
  # FIRST, so any positional slice written for polr would take the wrong block).
  v <- v[coef_names, coef_names, drop = FALSE]

  term_ids <- sort(unique(surv_term_idx))
  n_terms <- length(term_ids)
  if (n_terms < 2) {
    stop("model contains fewer than 2 terms")
  }

  R <- stats::cov2cor(v)
  detR <- det(R)
  result <- matrix(0, n_terms, 3)
  rownames(result) <- all_term_labels[term_ids]
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (i in seq_len(n_terms)) {
    subs <- which(surv_term_idx == term_ids[i])
    result[i, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
    result[i, 2] <- length(subs)
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
          clm_predict(object, newdata = newdata, type = "prob")
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
    clm_predict(object, newdata = newdata, type = "prob")
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
#' @param x A model built by build_polr(), with class clm_exploratory_0.
#' @param type What to return: "coefficients" (default), "vif", "importance",
#'   "partial_dependence", or "nominal_test" (the proportional-odds assumption
#'   test, which is specific to an ordinal model). Mirrors tidy.glm_exploratory().
#' @param conf.int Whether to compute a (Wald, i.e. normal-approximation) confidence interval.
#' @param conf.level Confidence level for conf.int.
#' @param exponentiate Whether to add an odds.ratio column (exp(estimate)) for slope coefficients.
#' @param pretty.name Whether to rename columns to display-friendly names.
#' @export
tidy.clm_exploratory_0 <- function(x, type = "coefficients", conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, pretty.name = FALSE, ...) {
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
  if (identical(type, "nominal_test")) {
    # Proportional-odds (parallel lines) assumption test -- the report spec's most
    # important ordinal-specific section. ordinal::nominal_test() refits the model
    # allowing each predictor in turn to have its OWN effect per category boundary,
    # and likelihood-ratio-tests that against the proportional-odds fit. A small
    # p-value means that predictor's effect plausibly differs across boundaries, i.e.
    # the assumption is doubtful FOR THAT VARIABLE.
    ret <- clm_nominal_test(x)
    if (is.null(ret) || nrow(ret) == 0) {
      return(data.frame(term = character(), statistic = numeric(), df = numeric(), p.value = numeric()))
    }
    # Map internal term labels back to the user's column names, same as the other types.
    if (!is.null(x$terms_mapping)) {
      mapped <- x$terms_mapping[ret$term]
      ret$term <- ifelse(is.na(mapped), ret$term, mapped)
    }
    if (pretty.name) {
      ret <- ret %>% dplyr::rename(
        Variable = term,
        `Likelihood Ratio Statistic` = statistic,
        `Degree of Freedom` = df,
        `P Value` = p.value
      )
    }
    return(ret)
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
    coef_df <- tidy.clm_exploratory_0(x, type = "coefficients", conf.int = FALSE, exponentiate = FALSE)
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

  smry <- stats::coef(summary(x))
  # ordinal::clm() orders coef(summary()) as [thresholds (alpha) ..., slopes (beta) ...] --
  # the OPPOSITE of MASS::polr(), which put its zeta thresholds last. Classify by NAME
  # against x$alpha / x$beta rather than by position so this cannot silently mislabel a
  # row if the ordering ever changes again.
  threshold_names <- names(x$alpha)
  term_names <- rownames(smry)

  ret <- tibble::tibble(
    term = term_names,
    estimate = as.numeric(smry[, 1]),
    std.error = as.numeric(smry[, 2]),
    statistic = as.numeric(smry[, 3]),
    # clm() reports a Wald p-value directly (column 4); polr() did not, so this used to be
    # computed from the z statistic. Keep reading column 4 when it is there.
    p.value = if (ncol(smry) >= 4) as.numeric(smry[, 4]) else 2 * stats::pnorm(abs(as.numeric(smry[, 3])), lower.tail = FALSE),
    coefficient_type = ifelse(term_names %in% threshold_names, "intercept", "coefficient")
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
#' @param x A model built by build_polr(), with class clm_exploratory_0.
#' @param pretty.name Whether to rename columns to display-friendly names.
#' @export
glance.clm_exploratory_0 <- function(x, pretty.name = FALSE, ...) {
  ll <- stats::logLik(x)
  ll_val <- as.numeric(ll)
  # clm()'s own edf already counts thresholds + slopes (coef(x) includes BOTH for clm,
  # unlike polr where coef() was slopes only and zeta held the thresholds -- adding them
  # again here would double-count).
  edf_val <- x$edf
  nobs_val <- stats::nobs(x)
  aic_val <- -2 * ll_val + 2 * edf_val
  bic_val <- -2 * ll_val + log(nobs_val) * edf_val
  df_residual_val <- nobs_val - edf_val

  # ordinal::clm() does not expose a null (intercept-only) model on its own, unlike glm();
  # refit one here so that McFadden's Pseudo R-Squared can be reported, mirroring what the
  # existing Logistic Regression report computes from glm()'s built-in null.deviance/df.null.
  null_fit <- tryCatch({
    resp <- x$model[[1]]
    wts <- stats::model.weights(x$model)
    null_df <- data.frame(.resp = resp)
    if (is.null(wts)) {
      ordinal::clm(.resp ~ 1, data = null_df, link = "logit")
    } else {
      null_df$.wts <- wts
      ordinal::clm(.resp ~ 1, data = null_df, weights = .wts, link = "logit")
    }
  }, error = function(e) NULL)

  null_deviance_val <- NA_real_
  df_null_val <- NA_integer_
  mcfadden_r_squared_val <- NA_real_
  # clm() exposes neither $deviance nor $zeta (polr did) -- derive deviance from the
  # log-likelihood (-2*logLik, the same identity polr's own $deviance satisfies) and take
  # the parameter count from clm's own $edf. Reading the absent fields returned NULL, which
  # made the is.finite() guard below throw "missing value where TRUE/FALSE needed".
  model_deviance_val <- -2 * ll_val
  if (!is.null(null_fit)) {
    null_deviance_val <- -2 * as.numeric(stats::logLik(null_fit))
    df_null_val <- nobs_val - null_fit$edf
    if (is.finite(null_deviance_val) && null_deviance_val != 0) {
      mcfadden_r_squared_val <- 1 - (model_deviance_val / null_deviance_val)
    }
  }

  ret <- tibble::tibble(
    n_classes = length(x$y.levels),
    nobs = nobs_val,
    edf = edf_val,
    logLik = ll_val,
    AIC = aic_val,
    BIC = bic_val,
    deviance = model_deviance_val,
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
#' @param x A model built by build_polr(), with class clm_exploratory_0.
#' @param data Original data (used when newdata is not given).
#' @param newdata New data to predict on. Rows whose predictor factor levels
#'   were not seen during training are dropped, mirroring
#'   augment.glm_exploratory_0().
#' @export
augment.clm_exploratory_0 <- function(x, data = NULL, newdata = NULL, ...) {
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

  # clm()'s predict(type="prob") returns the probability of the OBSERVED class when the
  # response column is present in newdata; dropping it yields the full per-category matrix,
  # which is what this augment contract has always returned.
  probs <- tryCatch(
    clm_predict(x, newdata = target_data, type = "prob"),
    error = function(e) clm_predict(x, type = "prob")
  )
  predicted_class <- tryCatch(
    clm_predict(x, newdata = target_data, type = "class"),
    error = function(e) clm_predict(x, type = "class")
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
#' @param model A clm_exploratory_0 model object.
#' @param train_data The data frame the model was trained on.
#' @param test_data The held-out test data frame, or NULL if there is none.
#' @param target_col Name of the target/objective column in train_data/test_data.
#' @param data_types Character vector, any of "Training"/"Test", naming which rows to compute.
#' @export
evaluate_polr_one_model <- function(model, train_data, test_data, target_col, data_types = c("Training", "Test")) {
  # Same convention as build_lm.R's lm/glm evaluators: fold the model's own VIF
  # into every row of the summary table as a single `Max VIF` value, so the tam
  # report can read multicollinearity off the SAME table it reads Accuracy /
  # Misclass. Rate from, without a separate bar-chart-data parse. NA when VIF
  # could not be computed at all (a single predictor); a perfect-collinearity
  # error is surfaced by tidy(type='vif')'s own empty-frame branch, not here.
  max_vif <- if (!is.null(model$vif) && !inherits(model$vif, "error")) {
    max(vif_to_dataframe(model)$VIF, na.rm = TRUE)
  } else {
    NA_real_
  }

  data_sets <- list(Training = train_data, Test = test_data)
  purrr::map_dfr(data_types, function(dt) {
    eval_data <- data_sets[[dt]]
    if (is.null(eval_data) || nrow(eval_data) == 0) {
      return(tibble::tibble(
        `Data Type` = dt, Rows = 0L,
        `Accuracy Rate` = NA_real_, `Misclass. Rate` = NA_real_,
        `ROC AUC` = NA_real_, `PR AUC` = NA_real_, `Balanced Accuracy` = NA_real_,
        `F1 Score` = NA_real_, `Precision` = NA_real_, `Recall` = NA_real_,
        `Specificity` = NA_real_,
        `Mean Category Error` = NA_real_, `Ranked Probability Score` = NA_real_,
        `Weighted Kappa` = NA_real_, `Log Loss` = NA_real_,
        `Max VIF` = max_vif
      ))
    }
    augmented <- augment.clm_exploratory_0(model, newdata = eval_data)
    actual <- eval_data[[target_col]]
    accuracy <- mean(as.character(augmented$.fitted) == as.character(actual), na.rm = TRUE)

    # --- Ordinal-aware evaluation metrics (tam#4453 spec) ----------------------
    # All four below treat the target as ORDERED: they use each category's RANK
    # (position in the ordered factor's levels), not just equality, so predicting
    # an ADJACENT category is penalized less than predicting a distant one.
    # That is the whole reason an ordinal model is being used instead of a
    # multiclass one, and plain Accuracy cannot express it.
    lvls <- levels(model$model[[1]])
    if (is.null(lvls)) {
      lvls <- levels(factor(actual))
    }
    K <- length(lvls)
    actual_rank <- match(as.character(actual), lvls)
    predicted_rank <- match(as.character(augmented$.fitted), lvls)

    prob_cols <- paste0("predicted_probability_", lvls)
    prob_mat <- if (all(prob_cols %in% colnames(augmented))) {
      m <- as.matrix(augmented[, prob_cols, drop = FALSE])
      colnames(m) <- lvls # so multiclass_auc_by_class() can match columns to actual's category labels.
      m
    } else {
      NULL
    }

    # --- One-vs-rest classification metrics (tam#4453 spec, report structure
    # follow-up) -----------------------------------------------------------
    # The report's "予測精度" (Prediction Accuracy) table mirrors the same
    # ROC AUC / PR AUC / Balanced Accuracy / Accuracy / F1 / Precision / Recall /
    # Specificity shape used elsewhere for multiclass models, computed one-vs-rest
    # per category and macro-averaged (unweighted mean across categories) so a
    # small category counts the same as a large one. These are in ADDITION to the
    # ordinal-aware metrics above -- both sets answer different questions (is the
    # category right at all vs. how far off is a miss).
    predicted_label <- as.character(augmented$.fitted)
    actual_label <- as.character(actual)
    balanced_accuracy <- multiclass_balanced_accuracy(actual_label, predicted_label)
    auc_by_class <- if (!is.null(prob_mat)) {
      multiclass_auc_by_class(actual_label, prob_mat)
    } else {
      data.frame(class = character(), roc_auc = numeric(), pr_auc = numeric())
    }
    macro_roc_auc <- if (nrow(auc_by_class) > 0) mean(auc_by_class$roc_auc, na.rm = TRUE) else NA_real_
    macro_pr_auc <- if (nrow(auc_by_class) > 0) mean(auc_by_class$pr_auc, na.rm = TRUE) else NA_real_
    prs <- polr_macro_precision_recall_specificity(actual_label, predicted_label)

    # 平均カテゴリ誤差 -- mean(|predicted_rank - actual_rank|). 0 = perfect;
    # 1 = off by one category on average. Directly interpretable in "categories".
    mean_category_error <- mean(abs(predicted_rank - actual_rank), na.rm = TRUE)

    # Ranked Probability Score -- the ordinal analogue of the Brier score. For
    # each row, compare the CUMULATIVE predicted probability against the
    # cumulative indicator of the actual category at every category boundary,
    # square the difference, and normalize by K-1 so the value stays in 0..1
    # regardless of how many categories there are. Lower is better. Unlike
    # Accuracy it rewards a confident, ordinally-close probability distribution.
    rps <- if (!is.null(prob_mat) && K > 1) {
      cum_pred <- t(apply(prob_mat, 1, cumsum))
      cum_actual <- t(vapply(actual_rank, function(r) as.numeric(seq_len(K) >= r), numeric(K)))
      # Only the first K-1 boundaries carry information (the K-th cumulative is
      # always 1 on both sides).
      mean(rowSums((cum_pred[, seq_len(K - 1), drop = FALSE] -
                      cum_actual[, seq_len(K - 1), drop = FALSE])^2) / (K - 1), na.rm = TRUE)
    } else {
      NA_real_
    }

    # Weighted Kappa (quadratic weights, per the spec's "v1 は quadratic weight
    # を標準とする") -- agreement corrected for chance, with the penalty growing
    # with the SQUARE of the rank distance. 1 = perfect, 0 = chance level,
    # negative = worse than chance.
    weighted_kappa <- {
      ok <- !is.na(actual_rank) & !is.na(predicted_rank)
      if (sum(ok) == 0) {
        NA_real_
      } else {
        ar <- actual_rank[ok]
        pr <- predicted_rank[ok]
        obs <- matrix(0, K, K)
        for (i in seq_along(ar)) obs[ar[i], pr[i]] <- obs[ar[i], pr[i]] + 1
        obs <- obs / sum(obs)
        # Expected agreement under independence of the two marginals.
        expct <- outer(rowSums(obs), colSums(obs))
        w <- outer(seq_len(K), seq_len(K), function(i, j) ((i - j)^2) / ((K - 1)^2))
        denom <- sum(w * expct)
        if (!is.finite(denom) || denom == 0) NA_real_ else 1 - sum(w * obs) / denom
      }
    }

    # Log Loss -- negative mean log of the probability the model assigned to the
    # category that actually occurred. Clamped away from 0 so a confidently wrong
    # row contributes a large-but-finite penalty instead of Inf.
    log_loss <- if (!is.null(prob_mat)) {
      p_actual <- prob_mat[cbind(seq_len(nrow(prob_mat)), actual_rank)]
      -mean(log(pmax(p_actual, .Machine$double.eps)), na.rm = TRUE)
    } else {
      NA_real_
    }

    tibble::tibble(
      `Data Type` = dt,
      Rows = nrow(eval_data),
      `Accuracy Rate` = accuracy,
      `Misclass. Rate` = 1 - accuracy,
      `ROC AUC` = macro_roc_auc,
      `PR AUC` = macro_pr_auc,
      `Balanced Accuracy` = balanced_accuracy,
      `F1 Score` = prs$f1,
      `Precision` = prs$precision,
      `Recall` = prs$recall,
      `Specificity` = prs$specificity,
      `Mean Category Error` = mean_category_error,
      `Ranked Probability Score` = rps,
      `Weighted Kappa` = weighted_kappa,
      `Log Loss` = log_loss,
      `Max VIF` = max_vif
    )
  })
}
