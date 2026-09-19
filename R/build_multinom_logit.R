# Multinomial Logistic Regression for a 3+ level UNORDERED categorical target
# (tam#37033). Estimates one set of slopes per non-reference category, so the
# drivers of e.g. "Promoter vs Passive" and "Detractor vs Passive" can differ --
# which Ordered Logistic Regression (build_polr()) constrains to one slope.
#
# Same calling convention as build_polr() (NSE target + predictors, optional
# weight, sampling, train/test split, Repeat By via group_cols) so the tam side
# reuses the same property widgets, and reuses build_polr()'s model-agnostic
# helpers (permutation importance / partial dependence via `prob_fun`, macro
# precision/recall, newdata level filtering).
#
# Not to be confused with build_multinom() (build_multinom.R), the legacy
# step-based "Build Model" wrapper, which is kept as-is for saved steps.

# Maximum number of target categories. Coefficients grow as (K-1) x predictors,
# and beyond this the report is unreadable (tam#37033 design P6).
MULTINOM_LOGIT_MAX_CATEGORIES <- 20

#' Fit a Multinomial Logistic Regression model.
#'
#' @param df Data frame.
#' @param target Target (objective) column (NSE). Must be categorical (character,
#'   factor or logical) with 3 to 20 categories. A numeric target is rejected;
#'   the tam side converts one to categories before calling this function.
#' @param ... Predictor columns (NSE, tidyselect semantics).
#' @param predictor_funs Named list of per-predictor derivation functions.
#' @param weight Optional case-weight column (NSE).
#' @param weight_fun Optional function applied to the weight column.
#' @param reference_category Category every other category is compared with.
#'   NULL or "" means the most frequent category (ties: first level).
#' @param max_nrow Per-group row limit; a random sample of this size is used above it.
#' @param group_cols Optional grouping columns (Repeat By).
#' @param seed Random seed used for sampling / test-data split.
#' @param test_rate Fraction of rows held out as test data. 0 means no split.
#' @param test_split_type "random" or "ordered".
#' @param max_pd_vars Maximum number of predictors to compute partial dependence for.
#' @param pd_grid_resolution Number of grid points per predictor for partial dependence.
#' @param pd_sample_size Maximum number of rows sampled for partial dependence.
#' @param maxit Maximum number of optimizer iterations passed to nnet::multinom().
#' @param keep.source Whether to retain the source data in the source.data column.
#' @export
build_multinom_logit <- function(df,
                                 target,
                                 ...,
                                 predictor_funs = NULL,
                                 weight = NULL,
                                 weight_fun = NULL,
                                 reference_category = NULL,
                                 max_nrow = 50000,
                                 group_cols = NULL,
                                 seed = 1,
                                 test_rate = 0,
                                 test_split_type = "random",
                                 max_pd_vars = 20,
                                 pd_grid_resolution = 20,
                                 pd_sample_size = 500,
                                 maxit = 200,
                                 keep.source = TRUE) {
  validate_empty_data(df)

  target_col <- unname(tidyselect::vars_select(names(df), !!rlang::enquo(target)))
  orig_selected_cols <- unname(tidyselect::vars_select(names(df), !!!rlang::quos(...)))

  if (length(orig_selected_cols) == 0) {
    stop("At least 1 Predictor Variable is required.")
  }

  # Character predictors -> factor sorted by frequency; an ORDERED factor predictor
  # -> unordered, so it gets treatment contrasts (one readable term per level)
  # rather than polynomial ones. Same rule as build_polr() (#37862).
  for (col in orig_selected_cols) {
    if (is.character(df[[col]])) {
      df[[col]] <- forcats::fct_infreq(df[[col]])
    } else if (is.factor(df[[col]]) && is.ordered(df[[col]])) {
      df[[col]] <- factor(df[[col]], levels = levels(df[[col]]), ordered = FALSE)
    }
  }

  if (!is.null(predictor_funs)) {
    df <- df %>% mutate_predictors(orig_selected_cols, predictor_funs)
    selected_cols <- names(unlist(predictor_funs))
  } else {
    selected_cols <- orig_selected_cols
  }

  weight_col <- unname(tidyselect::vars_select(names(df), !!rlang::enquo(weight)))
  if (is.null(weight_col) || length(weight_col) == 0) {
    weight_col <- NULL
  }
  if (!is.null(weight_col) && !is.null(weight_fun)) {
    weight_funs <- list(weight_fun)
    names(weight_funs) <- weight_col
    df <- df %>% mutate_predictors(weight_col, weight_funs)
  }
  if (!is.null(weight_col) && min(df[[weight_col]], na.rm = TRUE) <= 0) {
    stop("Weight column must be positive.")
  }
  if (!is.null(weight_col)) {
    df <- df %>%
      dplyr::mutate(!!rlang::sym(weight_col) := ifelse(is.na(!!rlang::sym(weight_col)), 1, !!rlang::sym(weight_col)))
  }

  # --- Coerce / validate the target as an UNORDERED factor. ---
  target_values <- df[[target_col]]
  if (is.numeric(target_values) || inherits(target_values, c("Date", "POSIXct"))) {
    stop(paste0(
      "Column to predict (", target_col, ") for Multinomial Logistic Regression must be a categorical column. ",
      "Convert the numeric column into categories first."
    ))
  }
  if (is.logical(target_values)) {
    target_values <- factor(as.character(target_values), levels = c("FALSE", "TRUE"))
  } else if (is.character(target_values)) {
    target_values <- factor(target_values)
  } else if (is.factor(target_values)) {
    target_values <- factor(target_values, levels = levels(target_values), ordered = FALSE)
  }
  df[[target_col]] <- target_values

  if (test_rate < 0 | 1 <= test_rate) {
    stop("test_rate must be between 0 (inclusive) and 1 (exclusive)")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Group-column renaming must run BEFORE group_by() -- same ordering as build_polr().
  group_col_index <- colnames(df) %in% group_cols
  reserved_names <- c(
    "model", "source.data", ".train_data", ".test_data", ".model_metadata", ".target_col",
    # for tidy
    "category", "term", "estimate", "odds.ratio", "std.error", "statistic", "p.value", "conf.low", "conf.high", "coefficient_type",
    # for glance
    "logLik", "AIC", "BIC", "deviance", "null.deviance", "df.residual", "df.null", "edf", "nobs",
    "mcfadden.r.squared", "n_classes", "converged"
  )
  colnames(df)[group_col_index] <- avoid_conflict(reserved_names, colnames(df)[group_col_index], ".group")
  colnames(df) <- make.unique(colnames(df), sep = "")

  if (!is.null(group_cols)) {
    df <- dplyr::group_by(df, !!!rlang::syms(colnames(df)[group_col_index]))
  }

  for (col in c(target_col, selected_cols)) {
    df <- df %>% dplyr::filter(!is.na(!!rlang::sym(col)))
  }

  group_col_names <- grouped_by(df)
  grouped_var <- group_col_names[group_col_names %in% c(target_col, selected_cols, weight_col)]
  if (length(grouped_var) == 1) {
    stop(paste0(grouped_var, " is a grouping column. Please remove it from variables."))
  } else if (length(grouped_var) > 0) {
    stop(paste0(paste(grouped_var, collapse = ", "), " are grouping columns. Please remove them from variables."))
  }

  # Category count and reference category are decided on the WHOLE data (after NA
  # removal) so every Repeat By group compares against the same reference.
  df[[target_col]] <- forcats::fct_drop(df[[target_col]])
  target_levels <- levels(df[[target_col]])
  n_target_levels <- length(target_levels)
  if (n_target_levels < 3) {
    stop(paste0(
      "Column to predict (", target_col, ") for Multinomial Logistic Regression must have 3 or more categories (it has ",
      n_target_levels, "). For a 2-category outcome, use Logistic Regression instead."
    ))
  }
  if (n_target_levels > MULTINOM_LOGIT_MAX_CATEGORIES) {
    stop(paste0(
      "Column to predict (", target_col, ") for Multinomial Logistic Regression can have at most ",
      MULTINOM_LOGIT_MAX_CATEGORIES, " categories (it has ", n_target_levels, "). ",
      "Group the categories first, or use binning for a numeric column."
    ))
  }
  reference_category <- resolve_multinom_reference_category(df[[target_col]], reference_category, target_col)
  df[[target_col]] <- stats::relevel(df[[target_col]], ref = reference_category)

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

    for (col in c(target_col, selected_cols)) {
      if (is.factor(train_data[[col]])) {
        train_data[[col]] <- forcats::fct_drop(train_data[[col]])
      }
    }
    if (length(levels(train_data[[target_col]])) < 3) {
      stop("Fewer than 3 categories remain in the training data after sampling/splitting. Try lowering the test data rate.")
    }
    if (!(reference_category %in% levels(train_data[[target_col]]))) {
      stop(paste0("Reference category (", reference_category, ") does not appear in the training data of this group."))
    }
    if (!is.null(test_data) && nrow(test_data) > 0) {
      test_only_categories <- setdiff(
        unique(as.character(test_data[[target_col]])),
        levels(train_data[[target_col]])
      )
      if (length(test_only_categories) > 0) {
        stop(paste0(
          "Target categories (", paste(test_only_categories, collapse = ", "),
          ") appear only in the test data and are absent from the training data. ",
          "Adjust the test split so every test category is represented in training data."
        ))
      }
    }

    # Rebind the formula's environment so `weights =` resolves in this frame (see build_polr()).
    local_fml <- fml
    environment(local_fml) <- environment()

    # nnet's default MaxNWts (1000) is exceeded easily by dummy-coded predictors x
    # categories; size it from the actual design.
    n_design_cols <- ncol(stats::model.matrix(stats::delete.response(stats::terms(local_fml)), data = train_data))
    max_nwts <- max(1000, (n_design_cols + 1) * length(levels(train_data[[target_col]])) + 10)

    model <- tryCatch({
      if (is.null(weight_col)) {
        nnet::multinom(local_fml, data = train_data, Hess = TRUE, model = TRUE, trace = FALSE,
                       maxit = maxit, MaxNWts = max_nwts)
      } else {
        nnet::multinom(local_fml, data = train_data, weights = train_data[[weight_col]], Hess = TRUE,
                       model = TRUE, trace = FALSE, maxit = maxit, MaxNWts = max_nwts)
      }
    }, error = function(e) {
      if (stringr::str_detect(e$message, "contrasts can be applied only to factors with 2 or more levels")) {
        stop("more than 1 unique values are expected for categorical columns assigned as predictors")
      }
      stop(e$message)
    })

    model$classification_type <- "multi"
    model$orig_target_col <- target_col
    model$reference_category <- reference_category
    attr(model, "ylevels") <- model$lev
    if (!is.null(predictor_funs)) {
      model$orig_predictor_cols <- orig_selected_cols
      model$predictor_funs <- predictor_funs
    }
    # Identity map for vif_to_dataframe()/handle_partial_dependence() (see build_polr()).
    model$terms_mapping <- stats::setNames(
      c(selected_cols, selected_cols),
      c(selected_cols, paste0("`", selected_cols, "`"))
    )

    model$vif <- tryCatch(calc_vif_multinom_logit(model), error = function(e) e)

    prob_fun <- function(object, newdata) multinom_logit_predict(object, newdata = newdata, type = "probs")
    model$imp_df <- if (length(selected_cols) > 1) {
      tryCatch(
        calc_permutation_importance_polr(model, target_col, selected_cols, train_data, prob_fun = prob_fun),
        error = function(e) e
      )
    } else {
      simpleError("Variable importance requires two or more variables.")
    }

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
          n = c(pd_grid_resolution, min(nrow(train_data), pd_sample_size)),
          prob_fun = prob_fun
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }

    if (!is.null(model$terms)) {
      attr(model$terms, ".Environment") <- NULL
    }

    class(model) <- c("multinom_logit_exploratory", class(model))

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
  class(ret$model) <- c("list", ".model", ".model.multinom_logit")
  ret
}

# Resolve the reference category: NULL/"" -> most frequent level (ties -> first
# level, since which.max() returns the first maximum); otherwise it must be one
# of the levels, compared as text.
resolve_multinom_reference_category <- function(target_values, reference_category = NULL, target_col = "") {
  lvls <- levels(target_values)
  if (is.null(reference_category) || length(reference_category) == 0 ||
      is.na(reference_category[[1]]) || !nzchar(trimws(as.character(reference_category[[1]])))) {
    counts <- table(target_values)
    return(names(counts)[which.max(counts)])
  }
  ref <- as.character(reference_category[[1]])
  if (!(ref %in% lvls)) {
    stop(paste0(
      "Reference category (", ref, ") is not a value of the column to predict (", target_col, "). ",
      "Choose one of: ", paste(lvls, collapse = ", ")
    ))
  }
  ref
}

# predict() for nnet::multinom that always returns the n x K probability matrix
# (a 1-row newdata collapses to a vector) and drops the response column first.
multinom_logit_predict <- function(object, newdata = NULL, type = "probs") {
  if (!is.null(newdata) && !is.null(object$orig_target_col) &&
      object$orig_target_col %in% colnames(newdata)) {
    newdata <- newdata[, setdiff(colnames(newdata), object$orig_target_col), drop = FALSE]
  }
  res <- if (is.null(newdata)) {
    stats::predict(object, type = type)
  } else {
    stats::predict(object, newdata = newdata, type = type)
  }
  if (identical(type, "probs") && is.null(dim(res))) {
    res <- matrix(res, nrow = 1, dimnames = list(NULL, names(res)))
  }
  res
}

# Covariance of the coefficients from the stored Hessian, or NULL when the
# Hessian is singular (quasi-separation). Never throws.
multinom_logit_vcov <- function(x) {
  h <- x$Hessian
  if (is.null(h) || !is.matrix(h) || any(!is.finite(h))) return(NULL)
  v <- tryCatch(solve(h), error = function(e) NULL)
  if (is.null(v) || any(!is.finite(diag(v))) || any(diag(v) < 0)) return(NULL)
  v
}

# Generalized VIF (Fox & Monette) computed from the correlation of the predictor
# design matrix. Multicollinearity is a property of the predictors, not of any one
# category comparison, so this is category-independent. Returns the same shapes
# as car::vif() so vif_to_dataframe() can consume it.
calc_vif_multinom_logit <- function(model) {
  mf <- model$model
  tt <- stats::terms(model)
  mm <- stats::model.matrix(tt, data = mf)
  mm_assign <- attr(mm, "assign")
  keep <- mm_assign != 0
  mm <- mm[, keep, drop = FALSE]
  mm_assign <- mm_assign[keep]
  term_labels <- labels(tt)

  qr_mm <- qr(mm)
  if (qr_mm$rank < ncol(mm)) {
    aliased <- unique(mm_assign[qr_mm$pivot[(qr_mm$rank + 1):ncol(mm)]])
    stop(paste0("Variables causing perfect collinearity : ",
                paste(gsub("`", "", term_labels[aliased], fixed = TRUE), collapse = ", ")))
  }
  term_ids <- sort(unique(mm_assign))
  if (length(term_ids) < 2) {
    stop("model contains fewer than 2 terms")
  }
  sds <- apply(mm, 2, stats::sd)
  if (any(!is.finite(sds) | sds == 0)) {
    constant <- unique(mm_assign[!is.finite(sds) | sds == 0])
    stop(paste0("Variables causing perfect collinearity : ",
                paste(gsub("`", "", term_labels[constant], fixed = TRUE), collapse = ", ")))
  }
  R <- stats::cor(mm)
  detR <- det(R)
  result <- matrix(0, length(term_ids), 3)
  rownames(result) <- term_labels[term_ids]
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
  for (i in seq_along(term_ids)) {
    subs <- which(mm_assign == term_ids[i])
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

#' Coefficient / odds-ratio table for a Multinomial Logistic Regression model.
#' @param x A model built by build_multinom_logit().
#' @param type "coefficients" (default), "vif", "importance", "permutation_importance" or "partial_dependence".
#' @param conf.int Whether to compute a Wald confidence interval.
#' @param conf.level Confidence level for conf.int.
#' @param exponentiate Whether to add odds.ratio columns for slope coefficients.
#' @param pretty.name Whether to rename columns to display-friendly names.
#' @export
tidy.multinom_logit_exploratory <- function(x, type = "coefficients", conf.int = TRUE, conf.level = 0.95,
                                            exponentiate = TRUE, pretty.name = FALSE, ...) {
  if (inherits(x, "error")) {
    return(data.frame())
  }
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
      return(data.frame(variable = character(), importance = numeric(), p.value = numeric()))
    }
    ret <- x$imp_df
    # Smallest P value among the variable's terms across ALL category comparisons.
    coef_df <- tidy.multinom_logit_exploratory(x, type = "coefficients", conf.int = FALSE, exponentiate = FALSE)
    slope_df <- coef_df %>% dplyr::filter(coefficient_type == "coefficient")
    # R backtick-quotes a term whenever the name is not syntactic -- including for
    # non-ASCII punctuation such as a Japanese comma -- so compare with the quoting
    # removed instead of guessing R's rule (tam#37033).
    unquoted_terms <- gsub("`", "", as.character(slope_df$raw_term), fixed = TRUE)
    ret <- ret %>% dplyr::mutate(p.value = purrr::map_dbl(variable, function(var) {
      factor_terms <- if (!is.null(x$xlevels) && var %in% names(x$xlevels)) {
        paste0(var, x$xlevels[[var]][-1])
      } else if (is.data.frame(x$model) && is.logical(x$model[[var]])) {
        # A logical predictor has no xlevels entry; its single term is "<var>TRUE".
        paste0(var, "TRUE")
      } else {
        character()
      }
      matched <- slope_df$p.value[unquoted_terms %in% c(var, factor_terms)]
      if (length(matched) == 0 || all(is.na(matched))) NA_real_ else min(matched, na.rm = TRUE)
    }))
    if (identical(type, "permutation_importance")) {
      ret <- ret %>% dplyr::rename(term = variable)
    }
    return(ret)
  }

  coefs <- stats::coef(x)
  if (is.null(dim(coefs))) {
    # A 2-level target collapses coef() to a vector; build_multinom_logit() forbids that, but be safe.
    coefs <- matrix(coefs, nrow = 1, dimnames = list(x$lev[[2]], names(coefs)))
  }
  categories <- rownames(coefs)
  term_names <- colnames(coefs)
  v <- multinom_logit_vcov(x)
  se_of <- function(category, term) {
    if (is.null(v)) return(NA_real_)
    key <- paste0(category, ":", term)
    if (!(key %in% rownames(v))) return(NA_real_)
    sqrt(v[key, key])
  }

  ret <- purrr::map_dfr(categories, function(category) {
    est <- as.numeric(coefs[category, ])
    se <- vapply(term_names, function(t) se_of(category, t), numeric(1))
    tibble::tibble(
      category = category,
      term = term_names,
      estimate = est,
      std.error = unname(se),
      statistic = est / unname(se),
      p.value = 2 * stats::pnorm(abs(est / unname(se)), lower.tail = FALSE),
      coefficient_type = ifelse(term_names == "(Intercept)", "intercept", "coefficient")
    )
  })
  # Keep the level order (reference excluded) so every Repeat By group and chart
  # lists the comparisons in the same order.
  ret$category <- factor(ret$category, levels = setdiff(x$lev, x$reference_category))
  ret$raw_term <- ret$term

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

  # R backtick-quotes a term whenever the column name is not syntactic -- ASCII
  # symbols AND non-ASCII punctuation such as a Japanese comma -- so match terms
  # with the quoting removed rather than re-deriving R's rule (the shared
  # xlevels_to_base_level_table()/prettify_polr_factor_terms() only quote for ASCII
  # symbols, which left "`地域、区分`東" unformatted with no base level, tam#37033).
  # The report shows column names, so the quoting is dropped; raw_term keeps it.
  ret$term <- gsub("`", "", ret$term, fixed = TRUE)
  ret$base.level <- NA_character_
  for (var in names(x$xlevels)) {
    lvls <- x$xlevels[[var]]
    if (length(lvls) < 2) next
    for (lvl in lvls[-1]) {
      is_term <- ret$term == paste0(var, lvl)
      ret$base.level[is_term] <- lvls[[1]]
      ret$term[is_term] <- paste0(var, ": ", lvl)
    }
  }
  if (all(is.na(ret$base.level))) {
    # Same contract as before: the column exists only when a categorical predictor does.
    ret$base.level <- NULL
  }
  ret$reference_category <- x$reference_category

  if (pretty.name) {
    ret <- ret %>% dplyr::select(-raw_term) %>% dplyr::rename(
      Category = category,
      Term = term,
      Coefficient = estimate,
      `Std. Error` = std.error,
      `z value` = statistic,
      `P Value` = p.value,
      Type = coefficient_type,
      `Reference Category` = reference_category
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

#' Model fit summary for a Multinomial Logistic Regression model.
#' @param x A model built by build_multinom_logit().
#' @param pretty.name Whether to rename columns to display-friendly names.
#' @export
glance.multinom_logit_exploratory <- function(x, pretty.name = FALSE, ...) {
  model_deviance_val <- x$deviance
  ll_val <- -model_deviance_val / 2
  edf_val <- x$edf
  nobs_val <- if (is.data.frame(x$model)) nrow(x$model) else length(x$weights)
  aic_val <- model_deviance_val + 2 * edf_val
  bic_val <- model_deviance_val + log(nobs_val) * edf_val

  null_fit <- tryCatch({
    resp <- x$model[[1]]
    wts <- stats::model.weights(x$model)
    null_df <- data.frame(.resp = resp)
    if (is.null(wts)) {
      nnet::multinom(.resp ~ 1, data = null_df, trace = FALSE)
    } else {
      null_df$.wts <- wts
      nnet::multinom(.resp ~ 1, data = null_df, weights = .wts, trace = FALSE)
    }
  }, error = function(e) NULL)

  null_deviance_val <- NA_real_
  df_null_val <- NA_real_
  mcfadden_r_squared_val <- NA_real_
  if (!is.null(null_fit)) {
    null_deviance_val <- null_fit$deviance
    df_null_val <- nobs_val - null_fit$edf
    if (is.finite(null_deviance_val) && null_deviance_val != 0) {
      mcfadden_r_squared_val <- 1 - (model_deviance_val / null_deviance_val)
    }
  }

  ret <- tibble::tibble(
    n_classes = length(x$lev),
    nobs = nobs_val,
    edf = edf_val,
    logLik = ll_val,
    AIC = aic_val,
    BIC = bic_val,
    deviance = model_deviance_val,
    df.residual = nobs_val - edf_val,
    null.deviance = null_deviance_val,
    df.null = df_null_val,
    mcfadden.r.squared = mcfadden_r_squared_val,
    converged = identical(as.integer(x$convergence), 0L)
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
      `McFadden R-Squared` = mcfadden.r.squared,
      `Converged` = converged
    )
  }
  ret
}

#' Row-level predictions (predicted class + per-class probability).
#' @param x A model built by build_multinom_logit().
#' @param data Original data (used when newdata is not given).
#' @param newdata New data to predict on. Rows with unseen predictor levels are dropped.
#' @param apply_predictor_funs Whether to apply the stored predictor functions to data/newdata.
#' @export
augment.multinom_logit_exploratory <- function(x, data = NULL, newdata = NULL,
                                               apply_predictor_funs = TRUE, ...) {
  has_explicit_data <- !is.null(newdata) || !is.null(data)
  target_data <- newdata
  if (is.null(target_data)) {
    target_data <- data
  }
  if (is.null(target_data)) {
    target_data <- x$model
  }
  if (has_explicit_data && isTRUE(apply_predictor_funs) &&
      !is.null(x$predictor_funs) && !is.null(x$orig_predictor_cols)) {
    target_data <- target_data %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
  }
  if (has_explicit_data) {
    target_data <- filter_clm_newdata(x, target_data)
  }

  target_levels <- x$lev
  if (nrow(target_data) == 0) {
    probs <- matrix(numeric(0), nrow = 0, ncol = length(target_levels), dimnames = list(NULL, target_levels))
    predicted_class <- factor(character(0), levels = target_levels)
  } else {
    probs <- multinom_logit_predict(x, newdata = target_data, type = "probs")
    predicted_class <- factor(colnames(probs)[max.col(probs, ties.method = "first")], levels = target_levels)
  }

  prob_df <- as.data.frame(probs, stringsAsFactors = FALSE, check.names = FALSE)
  colnames(prob_df) <- paste0("predicted_probability_", colnames(prob_df))
  # The model frame stores the weight column as "(weights)"; keep it out of the output.
  out <- tibble::as_tibble(target_data, .name_repair = "minimal")
  out <- out[, colnames(out) != "(weights)", drop = FALSE]

  dplyr::bind_cols(out, prob_df, tibble::tibble(.fitted = predicted_class))
}

#' Prediction accuracy summary (training and/or test) for a Multinomial Logistic Regression model.
#' @param df A model data frame returned by build_multinom_logit().
#' @param data "training", "test", or "training_and_test".
#' @param pretty.name Unused; kept for call compatibility with the other evaluators.
#' @export
evaluate_multinom_logit <- function(df, data = "training", pretty.name = FALSE) {
  if (!("model" %in% colnames(df))) {
    stop("model column is required. Run build_multinom_logit() first.")
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
      evaluate_multinom_logit_one_model(m, tr, te, tc, data_types)
    })) %>%
    dplyr::select(!!!rlang::syms(group_cols), .res) %>%
    tidyr::unnest(.res)

  if (length(group_cols) > 0) {
    ret <- ret %>% dplyr::group_by(!!!rlang::syms(group_cols))
  }
  ret
}

#' Training/test accuracy rows for a single fitted multinomial model (pure, unit-testable).
#' @param model A multinom_logit_exploratory model.
#' @param train_data Training data frame.
#' @param test_data Test data frame or NULL.
#' @param target_col Target column name.
#' @param data_types Any of "Training"/"Test".
#' @export
evaluate_multinom_logit_one_model <- function(model, train_data, test_data, target_col, data_types = c("Training", "Test")) {
  max_vif <- if (!is.null(model$vif) && !inherits(model$vif, "error")) {
    max(vif_to_dataframe(model)$VIF, na.rm = TRUE)
  } else {
    NA_real_
  }
  empty_row <- function(dt) {
    tibble::tibble(
      `Data Type` = dt, Rows = 0L,
      `Accuracy Rate` = NA_real_, `Misclass. Rate` = NA_real_,
      `ROC AUC` = NA_real_, `PR AUC` = NA_real_, `Balanced Accuracy` = NA_real_,
      `F1 Score` = NA_real_, `Precision` = NA_real_, `Recall` = NA_real_,
      `Specificity` = NA_real_, `Log Loss` = NA_real_,
      `Max VIF` = max_vif
    )
  }

  data_sets <- list(Training = train_data, Test = test_data)
  purrr::map_dfr(data_types, function(dt) {
    eval_data <- data_sets[[dt]]
    if (!is.null(eval_data)) {
      eval_data <- filter_clm_newdata(model, eval_data)
    }
    if (is.null(eval_data) || nrow(eval_data) == 0) {
      return(empty_row(dt))
    }
    augmented <- augment.multinom_logit_exploratory(model, newdata = eval_data, apply_predictor_funs = FALSE)
    actual_label <- as.character(eval_data[[target_col]])
    predicted_label <- as.character(augmented$.fitted)
    accuracy <- mean(predicted_label == actual_label, na.rm = TRUE)

    lvls <- model$lev
    prob_cols <- paste0("predicted_probability_", lvls)
    prob_mat <- if (all(prob_cols %in% colnames(augmented))) {
      m <- as.matrix(augmented[, prob_cols, drop = FALSE])
      colnames(m) <- lvls
      m
    } else {
      NULL
    }
    auc_by_class <- if (!is.null(prob_mat)) {
      multiclass_auc_by_class(actual_label, prob_mat)
    } else {
      data.frame(class = character(), roc_auc = numeric(), pr_auc = numeric())
    }
    prs <- polr_macro_precision_recall_specificity(actual_label, predicted_label)
    log_loss <- if (!is.null(prob_mat)) {
      actual_idx <- match(actual_label, lvls)
      p_actual <- prob_mat[cbind(seq_len(nrow(prob_mat)), actual_idx)]
      -mean(log(pmax(p_actual, .Machine$double.eps)), na.rm = TRUE)
    } else {
      NA_real_
    }

    tibble::tibble(
      `Data Type` = dt,
      Rows = nrow(eval_data),
      `Accuracy Rate` = accuracy,
      `Misclass. Rate` = 1 - accuracy,
      `ROC AUC` = if (nrow(auc_by_class) > 0) mean(auc_by_class$roc_auc, na.rm = TRUE) else NA_real_,
      `PR AUC` = if (nrow(auc_by_class) > 0) mean(auc_by_class$pr_auc, na.rm = TRUE) else NA_real_,
      `Balanced Accuracy` = multiclass_balanced_accuracy(actual_label, predicted_label),
      `F1 Score` = prs$f1,
      `Precision` = prs$precision,
      `Recall` = prs$recall,
      `Specificity` = prs$specificity,
      `Log Loss` = log_loss,
      `Max VIF` = max_vif
    )
  })
}

# Report-top "Data and Model Information" row for a build_multinom_logit() model.
# Not exported -- tam preprocessors call it with exploratory:::.
multinom_logit_report_basic_info <- function(df, test_mode = FALSE) {
  if (!is.data.frame(df) || !"model" %in% colnames(df)) return(data.frame())
  model <- df$model[[1]]
  if (is.null(model) || inherits(model, "error")) return(data.frame())

  lvls <- model$lev
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
    `Target` = if (is.null(model$orig_target_col)) NA_character_ else model$orig_target_col,
    `Categories` = length(lvls),
    `Reference Category` = if (is.null(model$reference_category)) NA_character_ else model$reference_category,
    `Predictors` = length(unique(bare_predictors)),
    `Rows` = rows,
    `Model` = "Multinomial Logistic Regression",
    `Evaluation` = if (isTRUE(test_mode)) "Test Data" else "Training",
    `Converged` = identical(as.integer(model$convergence), 0L),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# One-vs-rest probability rows (mirrors polr_report_multiclass_probabilities()).
multinom_logit_report_multiclass_probabilities <- function(df) {
  if (!is.data.frame(df) || !"model" %in% colnames(df)) return(data.frame())
  model <- df$model[[1]]
  if (is.null(model) || !inherits(model, "multinom_logit_exploratory")) return(data.frame())

  train_data <- if (".train_data" %in% colnames(df)) df$.train_data[[1]] else NULL
  test_data <- if (".test_data" %in% colnames(df)) df$.test_data[[1]] else NULL
  target_col <- if (".target_col" %in% colnames(df)) df$.target_col[[1]] else model$orig_target_col
  if (is.null(target_col)) return(data.frame())
  levels_target <- model$lev

  make_rows <- function(data, is_test) {
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) return(data.frame())
    if (!target_col %in% colnames(data)) return(data.frame())
    data <- filter_clm_newdata(model, data)
    if (nrow(data) == 0) return(data.frame())
    probabilities <- tryCatch(multinom_logit_predict(model, newdata = data, type = "probs"), error = function(e) NULL)
    if (is.null(probabilities)) return(data.frame())
    probabilities <- as.data.frame(probabilities, check.names = FALSE)
    categories <- intersect(levels_target, colnames(probabilities))
    dplyr::bind_rows(lapply(categories, function(category) {
      actual <- as.character(data[[target_col]])
      is_positive <- actual == category
      data.frame(
        Category = category,
        `Predicted Probability` = probabilities[[category]],
        `Actual Positive` = is_positive,
        `Actual Group` = factor(ifelse(is_positive, "This Category", "Other Categories"),
                                levels = c("This Category", "Other Categories")),
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
