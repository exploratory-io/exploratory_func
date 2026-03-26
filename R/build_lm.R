# Calculates permutation importance for binomial (including logistic) regression.
calc_permutation_importance_binomial <- function(fit, target, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(data, var, target, fit, nperm = 1, # By default, it creates 100 permuted data sets. We do just 1 for performance.
                                predict.fun = function(object,newdata){predict(object,type = "response",newdata=newdata)},
                                # Use minus log likelyhood as loss function, since it is what logistic regression tried to optimise. 
                                loss.fun = function(x,y){-sum(log(1- abs(x - y)),na.rm = TRUE)})
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble::tibble(variable=vars, importance=pmax(importances, 0))
  importances_df
}

# Calculates permutation importance for linear regression.
calc_permutation_importance_linear <- function(fit, target, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(data, var, target, fit, nperm = 1, # By default, it creates 100 permuted data sets. We do just 1 for performance.
                                # predict.fun can be the default function, which is predict(object, newdata=newdata).
                                # For some reason, default loss.fun, which is mean((x - y)^2) returns NA, even with na.rm=TRUE. Rewrote it with sum() to avoid the issue.
                                loss.fun = function(x,y){sum((x - y)^2, na.rm = TRUE)/length(x)})
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble::tibble(variable=vars, importance=pmax(importances, 0))
  importances_df
}

# Calculates permutation importance for GLM Gaussian regression.
calc_permutation_importance_gaussian <- function(fit, target, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(data, var, target, fit, nperm = 1, # By default, it creates 100 permuted data sets. We do just 1 for performance.
                                # type needs to be "response" so that x in loss.fun can be used to calculate sum of squares with any choice of link function.
                                predict.fun = function(object,newdata){predict(object,type = "response",newdata=newdata)},
                                # For some reason, default loss.fun, which is mean((x - y)^2) returns NA, even with na.rm=TRUE. Rewrote it with sum() to avoid the issue.
                                loss.fun = function(x,y){sum((x - y)^2, na.rm = TRUE)/length(x)})
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble::tibble(variable=vars, importance=pmax(importances, 0))
  importances_df
}

# Calculates permutation importance for logistic regression.
calc_permutation_importance_poisson <- function(fit, target, vars, data) {
  var_list <- as.list(vars)
  importances <- purrr::map(var_list, function(var) {
    mmpf::permutationImportance(data, var, target, fit, nperm = 1, # By default, it creates 100 permuted data sets. We do just 1 for performance.
                                # type needs to be "link" since x in loss.fun expects linear predictor.
                                predict.fun = function(object,newdata){predict(object,type = "link",newdata=newdata)},
                                # Use minus log likelyhood as loss function.
                                # Reference: https://en.wikipedia.org/wiki/Poisson_regression#Maximum_likelihood-based_parameter_estimation
                                #            https://stats.stackexchange.com/questions/70054/how-is-it-possible-that-poisson-glm-accepts-non-integer-numbers
                                loss.fun = function(x,y){-sum(y*x-exp(x), na.rm = TRUE)})
  })
  importances <- purrr::flatten_dbl(importances)
  importances_df <- tibble::tibble(variable=vars, importance=pmax(importances, 0))
  importances_df
}

# TODO: Make this function model-agnostic and consolidate. There are similar code for lm/glm, ranger, rpart, and xgboost.
# Builds partial_dependency object for lm/glm with same structure (a data.frame with attributes.) as edarf::partial_dependence.
partial_dependence.lm_exploratory <- function(fit, target, vars = colnames(data),
  n = c(min(nrow(unique(data[, vars, drop = FALSE])), 25L), nrow(data)), # Keeping same default of 25 as edarf::partial_dependence, although we usually overwrite from callers.
  interaction = FALSE, uniform = TRUE, data, ...) {

  predict.fun <- function(object, newdata) {
    predict(object, newdata = newdata, type = "response")
  }

  aggregate.fun <- function(x) {
    c("preds" = mean(x))
  }

  # Generate grid points based on quantile, so that FIRM calculated based on it would make good sense even when there are some outliers.
  points <- list()
  quantile_points <- list()
  for (cname in vars) {
    if (is.numeric(data[[cname]])) {
      coldata <- data[[cname]]
      minv <- min(coldata, na.rm=TRUE)
      maxv <- max(coldata, na.rm=TRUE)
      grid <- minv + (0:20)/20 * (maxv - minv)
      quantile_grid <- quantile(coldata, probs=1:24/25)
      quantile_points[[cname]] <- quantile_grid
      points[[cname]] <- sort(unique(c(grid, quantile_grid)))
    }
    else {
      points[[cname]] <- unique(data[[cname]])
    }
  }

  args = list(
    "data" = data,
    "vars" = vars,
    "n" = n,
    "model" = fit,
    "points" = points,
    "predict.fun" = predict.fun,
    "aggregate.fun" = aggregate.fun,
    ...
  )

  if (length(vars) > 1L & !interaction) { # More than one variables are there. Iterate calling mmpf::marginalPrediction.
    pd = rbindlist(sapply(vars, function(x) {
      args$vars = x
      if ("points" %in% names(args))
        args$points = args$points[x]
      mp = do.call(mmpf::marginalPrediction, args)
      names(mp)[ncol(mp)] = target
      mp
    }, simplify = FALSE), fill = TRUE)
    data.table::setcolorder(pd, c(vars, colnames(pd)[!colnames(pd) %in% vars]))
  } else {
    # Remove value label from vars to avoid unexpected column name in the result.
    args$vars = as.character(vars)
    pd = do.call(mmpf::marginalPrediction, args)
    names(pd)[ncol(pd)] = target
  }

  attr(pd, "class") = c("pd", "data.frame")
  attr(pd, "interaction") = interaction == TRUE
  attr(pd, "target") = target
  attr(pd, "vars") = vars
  attr(pd, "points") = points
  attr(pd, "quantile_points") = quantile_points
  pd
}


# Calculate average marginal effects from model with margins package.
calc_average_marginal_effects <- function(model, data=NULL, with_confint=FALSE) {
  if (with_confint) {
    if (!is.null(data)) {
      m <- margins::margins(model, data=data)
    }
    else {
      m <- margins::margins(model)
    }
    ret <- as.data.frame(summary(m))
    ret <- ret %>% dplyr::rename(term=factor, ame=AME, ame_low=lower, ame_high=upper) %>%
      dplyr::select(term, ame, ame_low, ame_high) #TODO: look into SE, z, p too.
    ret
  }
  else {
    # Fast versin that only calls margins::margins().
    # margins::margins() does a lot more than margins::marginal_effects(),
    # and takes about 10 times more time.
    if (!is.null(data)) {
      me <- margins::marginal_effects(model, data=data)
    }
    else {
      me <- margins::marginal_effects(model)
    }
    # For some reason, str_replace garbles column names generated from Date column with Japanese name. Using gsub instead to avoid the issue.
    # term <- stringr::str_replace(names(me), "^dydx_", "")
    term <- gsub("^dydx_", "", names(me))
    ame <- purrr::flatten_dbl(purrr::map(me, function(x){mean(x, na.rm=TRUE)}))
    ret <- data.frame(term=term, ame=ame)
    ret
  }
}

# VIF calculation definition from car::vif.
# Copied to avoid having to import many dependency packages.
vif <- function(mod, ...) {
    if (any(is.na(coef(mod)))) 
        stop ("there are aliased coefficients in the model")
    v <- vcov(mod)
    assign <- attr(model.matrix(mod), "assign")
    if (names(coefficients(mod)[1]) == "(Intercept)") {
        v <- v[-1, -1]
        assign <- assign[-1]
    }
    else warning("No intercept: vifs may not be sensible.")
    terms <- labels(terms(mod))
    n.terms <- length(terms)
    if (n.terms < 2) stop("model contains fewer than 2 terms")
    R <- cov2cor(v)
    detR <- det(R)
    result <- matrix(0, n.terms, 3)
    rownames(result) <- terms
    colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
    for (term in 1:n.terms) {
        subs <- which(assign == term)
        result[term, 1] <- det(as.matrix(R[subs, subs])) *
            det(as.matrix(R[-subs, -subs])) / detR
        result[term, 2] <- length(subs)
    }
    if (all(result[, 2] == 1)) result <- result[, 1]
    else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
    result
}

# Calculate VIF and throw user friendly message in case of perfect collinearity.
calc_vif <- function(model, terms_mapping) {
  tryCatch({
    vif(model)
  }, error = function(e){
    # in case of perfect multicollinearity, vif throws error with message "there are aliased coefficients in the model".
    # Check if it is the case. If coef() includes NA, corresponding variable is causing perfect multicollinearity.
    coef_vec <- coef(model)
    na_coef_vec <- coef_vec[is.na(coef_vec)]
    if (length(na_coef_vec) > 0) {
      na_coef_names <- names(na_coef_vec)
      na_coef_names <- map_terms_to_orig(na_coef_names, terms_mapping) # Map column names in the message back to original.
      message <- paste(na_coef_names, collapse = ", ")
      message <- paste0("Variables causing perfect collinearity : ", message)
      e$message <- message
    }
    stop(e)
  })
}

# augment function just to filter out unknown categories in predictors to avoid error.
augment.lm_exploratory_0 <- function(x, data = NULL, newdata = NULL, ...) {
  if (!is.null(newdata) && length(x$xlevels) > 0) {
    for (i in 1:length(x$xlevels)) {
      newdata <- newdata %>% dplyr::filter(!!rlang::sym(names(x$xlevels)[[i]]) %in% !!x$xlevels[[i]])
    }
  }
  if (is.null(data)) { # Giving data argument when it is NULL causes error from augment.glm. Do the same for lm just in case.
    ret <- broom:::augment.lm(x, newdata = newdata, se=TRUE, ...)
    if (!is.null(ret$.rownames)) { # Clean up .rownames column augment.glm adds for some reason. Do the same for lm just in case.
      ret$.rownames <- NULL
    }
  }
  else {
    ret <- broom:::augment.lm(x, data = data, newdata = newdata, se=TRUE, ...)
  }
  ret
}

tidy.lm_exploratory_0 <- function(x, ...) {
  # tidy.lm raises error when class has more than "lm". Working it around here.
  orig_class <- class(x)
  class(x) <- "lm"
  ret <- broom:::tidy.lm(x, ...)
  class(x) <- orig_class
  ret
}


# Map terms back to the original variable names.
# If term is for categorical variable, e.g. c1_UA,
# it will be mapped to name like "Carrier: UA".
map_terms_to_orig <- function(terms, mapping) {
  # Extract name part and value part of the coefficient name separately.
  var_names <- stringr::str_extract(terms, "^c[0-9]+_")
  var_values <- stringr::str_remove(terms, "^c[0-9]+_")
  var_names_orig <- mapping[var_names] # Map variable names back to the original.
  # is.na(var_names) is for "(Intercept)".
  # var_values == "" is for numerical variables.
  # Categorical variables are expected to have var_values.
  ret <- dplyr::if_else(is.na(var_names), terms, dplyr::if_else(var_values == "", var_names_orig, paste0(var_names_orig, ": ", var_values)))
  ret
}

#' Common preprocessing of regression data to be done BEFORE sampling.
#' Only common operations to be done, for example, in Summary View too.
#' @export
preprocess_regression_data_before_sample <- function(df, target_col, predictor_cols, filter_predictor_numeric_na=TRUE) {
  # ranger and rpart works with NA or Inf in the target, but we decided it would build rather meaningless or biased model.
  # For example, rpart binary classification just replaces NAs with FALSE, which would change the meaning of data inadvertently.
  # lm will filter out NAs anyway internally, and errors out if the remaining rows are with single value in any predictor column.
  # Also, filter Inf/-Inf too to avoid error at lm.
  # So, we always filter out NA/Inf from target variable.
  # NOTE: This has to be done before removing of all-NA predictor columns, since this operation might make new all-NA predictor columns.
  df <- df %>%
    dplyr::filter(!is.na(!!rlang::sym(target_col)) & !is.infinite(!!rlang::sym(target_col))) # this form does not handle group_by. so moved into each_func from outside.

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
    stop("No predictor column is left after removing columns with only NA or Inf values.")
  }

  # To avoid unused factor level that causes margins::marginal_effects() to fail, filtering operation has
  # to be done before factor level adjustments.
  # This is done before sampling so that we will end up with more valid rows in the end.
  for(col in cols){
    if(is.numeric(df[[col]]) || lubridate::is.Date(df[[col]]) || lubridate::is.POSIXct(df[[col]])) {
      # For numeric cols, filter NA rows, because lm will anyway do this internally, and errors out
      # if the remaining rows are with single value in any predictor column.
      # Filter Inf/-Inf too to avoid error at lm.
      # Do the same for Date/POSIXct, because we will create numeric columns from them.

      # For rpart, filter NAs for numeric columns to avoid instability. It seems that the resulting tree from rpart sometimes becomes
      # simplistic (e.g. only one split in the tree), especially in Exploratory for some reason, if we let rpart handle the handling of NAs,
      # even though it is supposed to just filter out rows with NAs, which is same as what we are doing here.
      if (filter_predictor_numeric_na) {
        df <- df %>% dplyr::filter(!is.na(!!rlang::sym(col)) & !is.infinite(!!rlang::sym(col)))
      }
      else {
        # For ranger, removing numeric NA is not necessary.
        # But even for ranger, filter Inf/-Inf to avoid following error from ranger.
        # Error in seq.default(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = length.out) : 'from' must be a finite number
        # TODO: In exp_rpart and calc_feature_imp, we have logic to remember and restore NA rows, but they are probably not made use of
        # if we filter NA rows here.
        df <- df %>% dplyr::filter(!is.na(!!rlang::sym(col)))
      }
    }
  }
  if (nrow(df) == 0) {
    stop("No row is left after removing NA/Inf from numeric, Date, or POSIXct columns.")
  }
  attr(df, 'predictors') <- cols
  df
}

#' Common preprocessing of regression data to be done AFTER sampling.
#' Only common operations to be done, for example, in Summary View too.
#' @export
#' @param df
#' @param target_col
#' @param predictor_cols
#' @param predictor_n
#' @param name_map
#' @param other_level - Level for "Other" group. Default is "Other".
preprocess_regression_data_after_sample <- function(df, target_col, predictor_cols,
                                                    predictor_n = 12, # so that at least months can fit in it.
                                                    name_map = NULL, 
                                                    other_level = "Other") {
  c_cols <- predictor_cols
  for(col in predictor_cols){
    if(lubridate::is.Date(df[[col]]) || lubridate::is.POSIXct(df[[col]])) {
      c_cols <- setdiff(c_cols, col)

      absolute_time_col <- avoid_conflict(colnames(df), paste0(col, "_abs_time"))
      wday_col <- avoid_conflict(colnames(df), paste0(col, "_w_"))
      day_col <- avoid_conflict(colnames(df), paste0(col, "_day_of_month"))
      yday_col <- avoid_conflict(colnames(df), paste0(col, "_day_of_year"))
      month_col <- avoid_conflict(colnames(df), paste0(col, "_m_"))
      year_col <- avoid_conflict(colnames(df), paste0(col, "_year"))
      new_name <- c(absolute_time_col, wday_col, day_col, yday_col, month_col, year_col)
      names(new_name) <- paste(
        names(name_map)[name_map == col],
        c(
          "_abs_time",
          "_w_",
          "_day_of_month",
          "_day_of_year",
          "_m_",
          "_year"
        ), sep="")

      name_map <- c(name_map, new_name)

      c_cols <- c(c_cols, absolute_time_col, wday_col, day_col, yday_col, month_col, year_col)
      df[[absolute_time_col]] <- as.numeric(df[[col]])
      # turn it into unordered factor since if it is ordered factor,
      # lm/glm takes polynomial terms (Linear, Quadratic, Cubic, and so on) and use them as variables,
      # which we do not want for this function.
      # Reference: https://hlplab.wordpress.com/2008/01/28/the-mysterious-l-q-and-c/
      df[[wday_col]] <- factor(lubridate::wday(df[[col]], label=TRUE), ordered=FALSE)
      # lubridate::day returns integer.
      # Convert integer to numeric. mmpf::marginalPrediction we use for partial dependence throws assertion error, if the data is integer and specified grid points are not integer.
      df[[day_col]] <- as.numeric(lubridate::day(df[[col]]))
      df[[yday_col]] <- lubridate::yday(df[[col]])
      # turn it into unordered factor for the same reason as wday.
      df[[month_col]] <- factor(lubridate::month(df[[col]], label=TRUE), ordered=FALSE)
      df[[year_col]] <- lubridate::year(df[[col]])
      if(lubridate::is.POSIXct(df[[col]])) {
        hour_col <- avoid_conflict(colnames(df), paste0(col, "_hour"))
        new_name <- c(hour_col)
        names(new_name) <- paste(
          names(name_map)[name_map == col],
          c(
            "_hour"
          ), sep="")
        name_map <- c(name_map, new_name)

        c_cols <- c(c_cols, hour_col)
        df[[hour_col]] <- factor(lubridate::hour(df[[col]])) # treat hour as category
      }
      df[[col]] <- NULL # drop original Date/POSIXct column to pass SMOTE later.
    } else if(is.factor(df[[col]])) {
      df[[col]] <- forcats::fct_drop(df[[col]]) # margins::marginal_effects() fails if unused factor level exists. Drop them to avoid it.
      # 1. if the data is factor, respect the levels and keep first 10 levels, and make others "Others" level.
      # 2. if the data is ordered factor, turn it into unordered. For ordered factor,
      #    lm/glm takes polynomial terms (Linear, Quadratic, Cubic, and so on) and use them as variables,
      #    which we do not want for this function.
      if (length(levels(df[[col]])) > predictor_n + 1) { # +1 is for 'Others' group.
        df[[col]] <- forcats::fct_other(factor(df[[col]], ordered=FALSE), keep=levels(df[[col]])[1:predictor_n], other_level=other_level)
      }
      else {
        df[[col]] <- factor(df[[col]], ordered=FALSE)
      }
      # turn NA into (Missing) factor level. Without this, lm or glm drops rows internally.
      if (any(is.na(df[[col]]))) {
        df[[col]] <- forcats::fct_na_value_to_level(df[[col]], level = "(Missing)")
      }
    } else if(is.logical(df[[col]])) {
      # 1. convert data to factor if predictors are logical. (as.factor() on logical always puts FALSE as the first level, which is what we want for predictor.)
      # 2. turn NA into (Missing) factor level so that lm will not drop all the rows.
      # For ranger, we need to convert logical to factor too since na.roughfix only works for numeric or factor.
      df[[col]] <- as.factor(df[[col]])
      if (any(is.na(df[[col]]))) {
        df[[col]] <- forcats::fct_na_value_to_level(df[[col]], level = "(Missing)")
      }
    } else if(!is.numeric(df[[col]])) {
      # 1. convert data to factor if predictors are not numeric or logical
      #    and limit the number of levels in factor by fct_lump.
      #    we use ties.method to handle the case where there are many unique values. (without it, they all survive fct_lump.)
      #    TODO: see if ties.method would make sense for calc_feature_imp.
      # 2. turn NA into (Missing) factor level so that lm will not drop all the rows.
      df[[col]] <- forcats::fct_lump(forcats::fct_infreq(as.factor(df[[col]])), n=predictor_n, ties.method="first", other_level=other_level)
      if (any(is.na(df[[col]]))) {
        df[[col]] <- forcats::fct_na_value_to_level(df[[col]], level = "(Missing)")
      }
    } else if(is.integer(df[[col]])) {
      # Convert integer to numeric. mmpf::marginalPrediction we use for partial dependence throws assertion error, if the data is integer and specified grid points are not integer.
      # To avoid something like that, we just convert integer to numeric before building predictive models.
      df[[col]] <- as.numeric(df[[col]])
    }
  }
  # If target is factor, turn it into unordered factor if it is not already.
  if (is.factor(df[[target_col]])) {
    df[[target_col]] <- factor(df[[target_col]], ordered=FALSE)
  }
  # remove columns with only one unique value
  cols_copy <- c_cols
  for (col in cols_copy) {
    unique_val <- unique(df[[col]])
    if (length(unique_val[!is.na(unique_val)]) == 1) {
      c_cols <- setdiff(c_cols, col)
      df[[col]] <- NULL # drop the column so that SMOTE will not see it. 
    }
  }
  if (length(c_cols) == 0) {
    # Previous version of message - stop("The selected predictor variables are invalid since they have only one unique values.")
    stop("Invalid Predictors: Only one unique value.") # Message is made short so that it fits well in the Summary table.
  }
  attr(df, 'predictors') <- c_cols # Pass up list of updated predictor column names.
  attr(df, 'name_map') <- name_map # Pass up list of updated column name map. Just for legacy Date/POSIXct column handling. 
  df
}

remove_outliers_for_regression_data <- function(df, target_col, predictor_cols,
                                                target_outlier_filter_type,
                                                target_outlier_filter_threshold,
                                                predictor_outlier_filter_type,
                                                predictor_outlier_filter_threshold) {
  if (!is.null(target_outlier_filter_type) || !is.null(predictor_outlier_filter_type)) {
    df$.is.outlier <- FALSE #TODO: handle possibility of name conflict.
    if (!is.null(target_outlier_filter_type)) {
      is_outlier <- function(x) {
        res <- detect_outlier(x, type=target_outlier_filter_type, threshold=target_outlier_filter_threshold) %in% c("Lower", "Upper")
        res
      }
      if (is.numeric(df[[target_col]])) {
        df$.is.outlier <- df$.is.outlier | is_outlier(df[[target_col]])
      }
    }

    if (!is.null(predictor_outlier_filter_type)) {
      is_outlier <- function(x) {
        res <- detect_outlier(x, type=predictor_outlier_filter_type, threshold=predictor_outlier_filter_threshold) %in% c("Lower", "Upper")
        res
      }
      for (col in predictor_cols) {
        if (is.numeric(df[[col]])) {
          df$.is.outlier <- df$.is.outlier | is_outlier(df[[col]])
        }
      }
    }
    df <- df %>% dplyr::filter(!.is.outlier)
    df$.is.outlier <- NULL # Removing the temporary column.
  }
  df
}


#' special version of glance.lm function to use with build_lm.fast.
#' @export
glance.lm_exploratory <- function(x, pretty.name = FALSE, ...) { #TODO: add test
  if ("error" %in% class(x)) {
    ret <- data.frame(Note = x$message)
    return(ret)
  }
  ret <- broom:::glance.lm(x)

  # Add Max VIF if VIF is available.
  if (!is.null(x$vif) && "error" %nin% class(x$vif)) {
    vif_df <- vif_to_dataframe(x)
    if (nrow(vif_df) > 0 ) {
      max_vif <- max(vif_df$VIF, na.rm=TRUE)
      ret <- ret %>% dplyr::mutate(`Max VIF`=!!max_vif)
    }
  }

  # Adjust the subtle difference between sigma (Residual Standard Error) and RMSE.
  # In RMSE, division is done by observation size, while it is by residual degree of freedom in sigma.
  # https://www.rdocumentation.org/packages/sjstats/versions/0.17.4/topics/cv
  # https://stat.ethz.ch/pipermail/r-help/2012-April/308935.html
  rmse_val <- sqrt(ret$sigma^2 * x$df.residual / nrow(x$model))
  sample_size <- nrow(x$model)
  ret <- ret %>% dplyr::mutate(rmse=!!rmse_val, n=!!sample_size)
  # Drop sigma in favor of rmse.
  ret <- ret %>% dplyr::select(r.squared, adj.r.squared, rmse, statistic, p.value, n, everything(), -sigma)

  if(pretty.name) {
    ret <- ret %>% dplyr::rename(`R Squared`=r.squared, `Adj R Squared`=adj.r.squared, `RMSE`=rmse, `F Value`=statistic, `P Value`=p.value, `DF`=df, `Log Likelihood`=logLik, `Residual Deviance`=deviance, `Residual DF`=df.residual, `Rows`=n)
    # Place the Max VIF column after the Residual Deviance.
    # ret <- ret %>% dplyr::relocate(`Residual DF`, .after=`Residual Deviance`)

    # Note column might not exist. Rename if it is there.
    colnames(ret)[colnames(ret) == "note"] <- "Note"
  }
  if (!is.null(ret$nobs)) { # glance.glm's newly added nobs seems to be same as Number of Rows. Suppressing it for now.
    ret <- ret %>% dplyr::select(-nobs)
  }
  ret
}

#' special version of glance.lm function to use with build_lm.fast.
#' @export
glance.glm_exploratory <- function(x, pretty.name = FALSE, binary_classification_threshold = 0.5, ...) { #TODO: add test
  if ("error" %in% class(x)) {
    ret <- data.frame(Note = x$message)
    return(ret)
  }
  ret <- broom:::glance.glm(x)

  # calculate model p-value since glm does not provide it as is.
  # https://stats.stackexchange.com/questions/129958/glm-in-r-which-pvalue-represents-the-goodness-of-fit-of-entire-model
  f0 <- x$formula # copy formula as a basis for null model.
  attr(f0,".Environment")<-environment() # Since we removed .Environment attribute for size reduction, add it back so that the following process works.
  lazyeval::f_rhs(f0) <- 1 # create null model formula.
  x0 <- glm(f0, x$model, family = x$family) # build null model. Use x$model rather than x$data since x$model seems to be the data after glm handled missingness.
  pvalue <- with(anova(x0,x),pchisq(Deviance,Df,lower.tail=FALSE)[2]) 
  if(pretty.name) {
    ret <- ret %>% dplyr::mutate(`P Value`=!!pvalue, `Rows`=!!length(x$y))
  }
  else {
    ret <- ret %>% dplyr::mutate(p.value=!!pvalue, n=!!length(x$y))
  }

  # For GLM (Negative Binomial)
  if("negbin" %in% class(x)) {
    if(pretty.name) {
      ret <- ret %>% dplyr::mutate(`Theta`=!!(x$theta), `SE Theta`=!!(x$SE.theta))
    }
    else {
      ret <- ret %>% dplyr::mutate(theta=!!(x$theta), SE.theta=!!(x$SE.theta))
    }
  }
  
  if (x$family$family %in% c('gaussian')) { # only for gaussian, which is same as linear regression (if link function is identity). 
    root_mean_square_error <- rmse(x$fitted.values, x$y)
    rsq <- r_squared(x$y, x$fitted.values)
    n_observations <- nrow(x$model)
    df_residual <- x$df.residual
    adj_rsq <- adjusted_r_squared(rsq, n_observations, df_residual)
    ret$rmse <- root_mean_square_error
    ret$r.squared <- rsq
    ret$adj.r.squared <- adj_rsq
  }
  if (x$family$family %in% c('binomial', 'quasibinomial')) { # only for logistic regression.
    # Calculate F Score, Accuracy Rate, Misclassification Rate, Precision, Recall, Number of Rows 
    threshold_value <- if (is.numeric(binary_classification_threshold)) {
      binary_classification_threshold
    } else {
      get_optimized_score(x$y, x$fitted.value, threshold = binary_classification_threshold)$threshold
    }
    if (is.null(threshold_value)) { # Could not get optimized value. To avoid error, it has to have some numeric value.
      threshold_value <- 0.5
    }
    predicted <- ifelse(x$fitted.value > threshold_value, 1, 0) #TODO make threshold adjustable
    ret2 <- evaluate_classification(x$y, predicted, 1, pretty.name = pretty.name)
    ret2 <- ret2[, 2:6]
    ret <- ret %>% bind_cols(ret2)

    # calculate AUC
    ret$auc <- auroc(x$fitted.value, x$y)
    # Show number of rows for positive case and negative case, especially so that result of SMOTE is visible.
    ret$positives <- sum(x$y == 1, na.rm = TRUE)
    ret$negatives <- sum(x$y != 1, na.rm = TRUE)
  }

  # Add Max VIF if VIF is available.
  if (!is.null(x$vif) && "error" %nin% class(x$vif)) {
    vif_df <- vif_to_dataframe(x)
    if (nrow(vif_df) > 0 ) {
      max_vif <- max(vif_df$VIF, na.rm=TRUE)
      ret <- ret %>% dplyr::mutate(`Max VIF`=!!max_vif)
    }
  }

  if(pretty.name) {
    if (x$family$family %in% c('binomial', 'quasibinomial')) { # for binomial regressions.
      colnames(ret)[colnames(ret) == "null.deviance"] <- "Null Deviance"
      colnames(ret)[colnames(ret) == "df.null"] <- "Null Model DF"
      colnames(ret)[colnames(ret) == "logLik"] <- "Log Likelihood"
      colnames(ret)[colnames(ret) == "deviance"] <- "Residual Deviance"
      colnames(ret)[colnames(ret) == "df.residual"] <- "Residual DF"
      colnames(ret)[colnames(ret) == "auc"] <- "AUC"
      
      ret <- ret %>% dplyr::select(AUC, `F1 Score`, `Accuracy Rate`, `Misclass. Rate`, `Precision`, `Recall`, `P Value`, `Rows`, positives, negatives,  `Log Likelihood`, `AIC`, `BIC`, `Residual Deviance`, `Residual DF`, `Null Deviance`, `Null Model DF`, everything())

      if (!is.null(x$orig_levels)) { 
        pos_label <- x$orig_levels[2]
        neg_label <- x$orig_levels[1]
      }
      else {
        # This should be only numeric case.
        # In case of 0 and 1, this is making sense.
        # But it seems the input can be numbers between 0 and 1 like 0.5 too.
        # TODO: Look into how to handle such case.
        pos_label <- "TRUE"
        neg_label <- "FALSE"
      }
      colnames(ret)[colnames(ret) == "positives"] <- paste0("Rows for ", pos_label)
      colnames(ret)[colnames(ret) == "negatives"] <- paste0("Rows for ", neg_label)
    }
    else { # for other numeric regressions.
      colnames(ret)[colnames(ret) == "null.deviance"] <- "Null Deviance"
      colnames(ret)[colnames(ret) == "df.null"] <- "Null Model DF"
      colnames(ret)[colnames(ret) == "logLik"] <- "Log Likelihood"
      colnames(ret)[colnames(ret) == "deviance"] <- "Residual Deviance"
      colnames(ret)[colnames(ret) == "df.residual"] <- "Residual DF"
      colnames(ret)[colnames(ret) == "rmse"] <- "RMSE"
      colnames(ret)[colnames(ret) == "r.squared"] <- "R Squared"
      colnames(ret)[colnames(ret) == "adj.r.squared"] <- "Adj R Squared"

      ret <- ret %>% dplyr::select(matches("^R Squared$"), matches("^Adj R Squared$"), matches("^RMSE$"), `P Value`, `Rows`, `Log Likelihood`, `AIC`, `BIC`, `Residual Deviance`, `Residual DF`, `Null Deviance`, `Null Model DF`, everything())
    }
  }
  if (!is.null(ret$nobs)) { # glance.glm's newly added nobs seems to be same as Number of Rows. Suppressing it for now.
    ret <- ret %>% dplyr::select(-nobs)
  }

  ret
}

# Creates a data frame that maps term name to its base level.
xlevels_to_base_level_table <- function(xlevels) {
  term <- purrr::flatten_chr(purrr::map(names(xlevels), function(vname) {
    # Quote variable name with backtick if it includes special characters or space.
    # Special characters to detect besides space. Note that period and underscore should *not* be included here. : ~!@#$%^&*()+={}|:;'<>,/?"[]-\
    # Using grepl() as opposed to str_detect() because str_detect seems to return wrong decision when vname ends with SJIS damemoji.
    # perl=TRUE is required here, since it seems this regex does not detect space, tilde, etc. without perl=TRUE for some reason.
    paste0(if_else(grepl("[ ~!@#$%^&*()+={}|:;'<>,/?\"\\[\\]\\-\\\\]", vname, perl=TRUE), paste0('`',vname,'`'),vname),xlevels[[vname]])
  }))
  base_level <- purrr::flatten_chr(purrr::map(xlevels, function(v){rep(v[[1]],length(v))}))
  ret <- data.frame(term=term, base.level=base_level)
  ret
}

# Takes lm/glm model with vif (variance inflation factor) and returns data frame with extracted info.
vif_to_dataframe <- function(x) {
  ret <- NULL
  if (is.matrix(x$vif)) {
    # It is a matrix when there are categorical variables.
    ret <- x$vif %>% as.data.frame() %>% tibble::rownames_to_column(var="term") %>% rename(VIF=GVIF)
  }
  else {
    # It is a vector when there is no categorical variable.
    ret <- data.frame(term=names(x$vif), VIF=x$vif)
  }
  # Eliminate negative VIF values that sometimes happen, most likely because of numeric error.
  ret <- ret %>% dplyr::mutate(VIF = pmax(VIF, 0))
  # Map variable names back to the original.
  # as.character is to be safe by converting from factor. With factor, reverse mapping result will be messed up.
  ret$term <- x$terms_mapping[as.character(ret$term)]
  ret
}

# From name of variable, returns possible names of terms returned from lm/glm.
var_to_possible_terms_lm <- function(var, x) {
  if (is.factor(x$model[[var]])) {
    # Possibly, the variable name in the term name is quoted with backtic.
    c(paste0(var, levels(x$model[[var]])),
      paste0('`', var, '`', levels(x$model[[var]])))
  }
  else {
    # Possibly, the term name is quoted with backtic.
    c(var, paste0('`', var, '`'))
  }
}

# From name of variable, returns possible names of terms returned from coxph.
var_to_possible_terms_coxph <- function(var, x) {
  if (!is.null(x$xlevels[[var]])) {
    # Possibly, the variable name in the term name is quoted with backtic.
    paste0(var, x$xlevels[[var]])
  }
  else {
    # Possibly, the term name is quoted with backtic.
    var
  }
}

# Returns P-value for the variable. For categorical, the smallest value is returned.
# For the color of relative importance bar chart.
get_var_min_pvalue <- function(var, coef_df, x) {
  if ("coxph" %in% class(x)) {
    terms <- var_to_possible_terms_coxph(as.character(var), x)
  }
  else { # We assume it is lm or glm here.
    terms <- var_to_possible_terms_lm(as.character(var), x)
  }
  # na.rm is necessary to handle the case where part of categorical variables are dropped due to perfect collinearity.
  min(coef_df$p.value[coef_df$term %in% terms], na.rm=TRUE)
}

#' special version of tidy.lm function to use with build_lm.fast.
#' @export
tidy.lm_exploratory <- function(x, type = "coefficients", pretty.name = FALSE, ...) { #TODO: add test
  if ("error" %in% class(x)) {
    ret <- data.frame()
    return(ret)
  }
  switch(type,
    coefficients = {
      # Since broom:::tidy.lm raises :Error: No tidy method for objects of class lm_exploratory",
      # always use broom:::tidy.glm which does not have this problem, and seems to return the same result,
      # even for lm.
      ret <- broom:::tidy.glm(x)
      ret <- ret %>% mutate(conf.low=estimate-1.96*std.error, conf.high=estimate+1.96*std.error)
      base_level_table <- xlevels_to_base_level_table(x$xlevels)
      # Convert term from factor to character to remove warning at left_join.
      ret <- ret %>% dplyr::mutate(term=as.character(term)) %>% dplyr::left_join(base_level_table, by="term")
      if (any(is.na(x$coefficients))) {
        # since broom skips coefficients with NA value, which means removed by lm because of multi-collinearity,
        # put it back to show them.
        # reference: https://stats.stackexchange.com/questions/25804/why-would-r-return-na-as-a-lm-coefficient
        removed_coef_df <- data.frame(term=names(x$coefficients[is.na(x$coefficients)]), note="Dropped most likely due to perfect multicollinearity.")
        ret <- ret %>% dplyr::bind_rows(removed_coef_df)
        if (pretty.name) {
          ret <- ret %>% rename(Note=note)
        }
      }
      # Map coefficient names back to original.
      ret$term <- map_terms_to_orig(ret$term, x$terms_mapping)
      if (pretty.name) {
        ret <- ret %>% rename(Term=term, Coefficient=estimate, `Std Error`=std.error,
                              `t Value`=statistic, `P Value`=p.value,
                              `Conf Low`=conf.low,
                              `Conf High`=conf.high,
                              `Base Level`=base.level)
      }
      ret
    },
    vif = {
      if (!is.null(x$vif) && "error" %nin% class(x$vif)) {
        ret <- vif_to_dataframe(x)
      }
      else {
        ret <- data.frame() # Skip output for this group. TODO: Report error in some way.
      }
      ret
    },
    partial_dependence = {
      handle_partial_dependence(x)
    },
    importance = {
      if (is.null(x$imp_df) || "error" %in% class(x$imp_df)) {
        # Permutation importance is not supported for the family and link function, or skipped because there is only one variable.
        # Return empty data.frame to avoid error.
        ret <- data.frame()
        return(ret)
      }
      ret <- x$imp_df
      # Add p.value column.
      # Since broom:::tidy.lm raises :Error: No tidy method for objects of class lm_exploratory",
      # always use broom:::tidy.glm which does not have this problem, and seems to return the same result,
      # even for lm.
      coef_df <- broom:::tidy.glm(x)
      ret <- ret %>% dplyr::mutate(p.value=purrr::map(variable, function(var) {
        get_var_min_pvalue(var, coef_df, x)
      })) %>% dplyr::mutate(p.value=as.numeric(p.value)) # Make list into numeric vector.
      # Map variable names back to the original.
      # as.character is to be safe by converting from factor. With factor, reverse mapping result will be messed up.
      ret$variable <- x$terms_mapping[as.character(ret$variable)]
      ret
    },
    # This is copy of the code for "importance" with adjustment for output column name just for backward compatibility for pre-6.5.
    # Remove at appropriate timing.
    permutation_importance = {
      if (is.null(x$imp_df) || "error" %in% class(x$imp_df)) {
        # Permutation importance is not supported for the family and link function, or skipped because there is only one variable.
        # Return empty data.frame to avoid error.
        ret <- data.frame()
        return(ret)
      }
      ret <- x$imp_df
      # Add p.value column.
      # Since broom:::tidy.lm raises :Error: No tidy method for objects of class lm_exploratory",
      # always use broom:::tidy.glm which does not have this problem, and seems to return the same result,
      # even for lm.
      coef_df <- broom:::tidy.glm(x)
      ret <- ret %>% dplyr::mutate(p.value=purrr::map(variable, function(var) {
        get_var_min_pvalue(var, coef_df, x)
      })) %>% dplyr::mutate(p.value=as.numeric(p.value)) # Make list into numeric vector.
      # Map variable names back to the original.
      # as.character is to be safe by converting from factor. With factor, reverse mapping result will be messed up.
      ret$variable <- x$terms_mapping[as.character(ret$variable)]
      ret <- ret %>% dplyr::rename(term=variable)
      ret
    }
  )
}

#' Special version of tidy.glm function to use with build_lm.fast.
#' In case of error, returns empty data frame, or data frame with Note column.
#' @export
tidy.glm_exploratory <- function(x, type = "coefficients", pretty.name = FALSE, variable_metric = NULL, converged_only = FALSE, ...) { #TODO: add test
  if ("error" %in% class(x)) {
    ret <- data.frame()
    return(ret)
  }
  # Skip if model did not converge. We are using this to skip coefficient scatter plot with very large coefficients,
  # or if odds ratio is used, Inf or 0 odds ratios.
  if (converged_only && !x$converged) {
    ret <- data.frame()
    return(ret)
  }
  switch(type,
    coefficients = {
      ret <- broom:::tidy.glm(x)
      ret <- ret %>% mutate(conf.low=estimate-1.96*std.error, conf.high=estimate+1.96*std.error)
      if (x$family$family == "binomial" && x$family$link == "logit") { # odds ratio is only for logistic regression
        ret <- ret %>% mutate(odds_ratio=exp(estimate))
        if (!is.null(variable_metric) && variable_metric == "odds_ratio") { # For Analytics View, overwrite conf.low/conf.high with those of odds ratio.
          ret <- ret %>% mutate(conf.low=exp(conf.low), conf.high=exp(conf.high))
          ret <- ret %>% select(term, odds_ratio, conf.low, conf.high, everything()) # Bring odds ratio upfront together with its confidence interval.
        }
        else {
          ret <- ret %>% select(term, estimate, conf.low, conf.high, everything()) # Bring coefficient upfront together with its confidence interval.
        }
      }
      if (!is.null(x$marginal_effects)) {
        # Convert term from factor to character to remove warning at left_join.
        ret <- ret %>% dplyr::mutate(term=as.character(term)) %>% dplyr::left_join(x$marginal_effects, by="term")
        # Adjust order of columns
        if (!is.null(ret$ame)) {
          ret <- ret %>% relocate(ame, .after=term)
          if (!is.null(ret$ame_low)) {
            ret <- ret %>% relocate(ame_high, ame_low, .after=ame)
          }
        }
      }
      base_level_table <- xlevels_to_base_level_table(x$xlevels)
      # Convert term from factor to character to remove warning at left_join.
      ret <- ret %>% dplyr::mutate(term=as.character(term)) %>% dplyr::left_join(base_level_table, by="term")
      if (any(is.na(x$coefficients))) {
        # since broom skips coefficients with NA value, which means removed by lm because of multi-collinearity,
        # put it back to show them.
        # reference: https://stats.stackexchange.com/questions/25804/why-would-r-return-na-as-a-lm-coefficient
        removed_coef_df <- data.frame(term=names(x$coefficients[is.na(x$coefficients)]), note="Dropped most likely due to perfect multicollinearity.")
        ret <- ret %>% dplyr::bind_rows(removed_coef_df)
        if (pretty.name) {
          ret <- ret %>% rename(Note=note)
        }
      }
      # Map coefficient names back to original.
      ret$term <- map_terms_to_orig(ret$term, x$terms_mapping)
      if (pretty.name) {
        # Rename to pretty names
        ret <- ret %>% rename(Term=term, Coefficient=estimate, `Std Error`=std.error,
                              `P Value`=p.value, `Conf Low`=conf.low, `Conf High`=conf.high,
                              `Base Level`=base.level)

        # Rename statistic to t or z value depending on the family.
        family <- if (!is.null(x$family) && !is.null(x$family$family) && !is.na(x$family$family)) x$family$family else ""
        if (family == "gaussian" || family == "quasi" || family == "quasibinomial" || family == "quasipoisson") {
          ret <- ret %>% rename(`t Value`=statistic)
        }
        else {
          ret <- ret %>% rename(`z Value`=statistic)
        }

        if (!is.null(ret$ame)) {
          ret <- ret %>% rename(`Average Marginal Effect`=ame)
        }
        if (!is.null(ret$ame_low)) {
          ret <- ret %>% rename(`AME Low`=ame_low,`AME High`=ame_high)
        }
        if (x$family$family == "binomial" && x$family$link == "logit") { # odds ratio is only for logistic regression
          ret <- ret %>% rename(`Odds Ratio`=odds_ratio)
        }
      }
      ret
    },
    conf_mat = {
      target_col <- as.character(lazyeval::f_lhs(x$formula)) # get target column name
      actual_val = x$model[[target_col]]

      predicted = x$fitted.value > 0.5 # TODO: make threshold adjustable. Note: This part of code seems to be unused. Check and remove.
      # convert predicted to original set of values. should be either logical, numeric, or factor.
      predicted <- if (is.logical(actual_val)) {
        predicted
      } else if (is.numeric(actual_val)) {
        as.numeric(predicted)
      } else if (is.factor(actual_val)){
        # create a factor vector with the same levels as actual_val
        # predicted is logical, so should +1 to make it index
        factor(levels(actual_val)[as.numeric(predicted) + 1], levels(actual_val))
      }

      ret <- data.frame(
        actual_value = actual_val,
        predicted_value = predicted
      ) %>%
        dplyr::filter(!is.na(predicted_value))

      # get count if it's classification
      ret <- ret %>%
        dplyr::group_by(actual_value, predicted_value) %>%
        dplyr::summarize(count = n()) %>%
        dplyr::ungroup()
      ret
    },
    vif = {
      if (!is.null(x$vif) && "error" %nin% class(x$vif)) {
        ret <- vif_to_dataframe(x)
      }
      else {
        ret <- data.frame() # Skip output for this group. TODO: Report error in some way.
      }
      ret
    },
    partial_dependence = {
      handle_partial_dependence(x)
    },
    importance = {
      if (is.null(x$imp_df) || "error" %in% class(x$imp_df)) {
        # Permutation importance is not supported for the family and link function, or skipped because there is only one variable.
        # Return empty data.frame to avoid error.
        ret <- data.frame()
        return(ret)
      }
      ret <- x$imp_df
      # Add p.value column.
      coef_df <- broom:::tidy.glm(x)
      ret <- ret %>% dplyr::mutate(p.value=purrr::map(variable, function(var) {
        get_var_min_pvalue(var, coef_df, x)
      })) %>% dplyr::mutate(p.value=as.numeric(p.value)) # Make list into numeric vector.
      # Map variable names back to the original.
      # as.character is to be safe by converting from factor. With factor, reverse mapping result will be messed up.
      ret$variable <- x$terms_mapping[as.character(ret$variable)]
      ret
    },
    # This is copy of the code for "importance" with adjustment for output column name just for backward compatibility for pre-6.5.
    # Remove at appropriate timing.
    permutation_importance = {
      if (is.null(x$imp_df) || "error" %in% class(x$imp_df)) {
        # Permutation importance is not supported for the family and link function, or skipped because there is only one variable.
        # Return empty data.frame to avoid error.
        ret <- data.frame()
        return(ret)
      }
      ret <- x$imp_df
      # Add p.value column.
      coef_df <- broom:::tidy.glm(x)
      ret <- ret %>% dplyr::mutate(p.value=purrr::map(variable, function(var) {
        get_var_min_pvalue(var, coef_df, x)
      })) %>% dplyr::mutate(p.value=as.numeric(p.value)) # Make list into numeric vector.
      # Map variable names back to the original.
      # as.character is to be safe by converting from factor. With factor, reverse mapping result will be messed up.
      ret$variable <- x$terms_mapping[as.character(ret$variable)]
      ret <- ret %>% dplyr::rename(term=variable)
      ret
    }
  )
}


#' @export
augment.lm_exploratory <- function(x, data = NULL, newdata = NULL, data_type = "training", ...) {
  if ("error" %in% class(x)) {
    ret <- data.frame()
    return(ret)
  }

  if(!is.null(newdata)) {
    # Replay the mutations on predictors.
    if(!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    predictor_variables <- all.vars(x$terms)[-1]
    predictor_variables_orig <- x$terms_mapping[predictor_variables]

    # Rename columns via predictor_variables_orig, which is a named vector.
    # TODO: What if names of the other columns conflicts with our temporary name, c1_, c2_...?
    cleaned_data <- newdata %>% dplyr::rename(predictor_variables_orig)

    # Align factor levels including Others and (Missing) to the model. TODO: factor level order can be different from the model training data. Is this ok?
    cleaned_data <- align_predictor_factor_levels(cleaned_data, x$xlevels, predictor_variables)

    na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
    if (length(na_row_numbers) > 0) {
      cleaned_data <- cleaned_data[-na_row_numbers,]
    }
    ret <- broom:::augment.lm(x, data = NULL, newdata = cleaned_data, se = TRUE, ...)
    # TODO: Restore removed rows.
  } else if (!is.null(data)) {
    data <- data %>% dplyr::relocate(!!rlang::sym(x$target_col), .after = last_col()) # Bring the target column to the last so that it is next to the predicted value in the output.
    ret <- switch(data_type,
      training = { # Call broom:::augment.lm as is
        broom:::augment.lm(x, data = data, newdata = newdata, se = TRUE, ...)
      },
      test = {
        # Augment data with already predicted result in the model.
        data$.fitted <- restore_na(x$prediction_test$fit, x$prediction_test$unknown_category_rows_index)
        data$.se.fit <- restore_na(x$prediction_test$se.fit, x$prediction_test$unknown_category_rows_index)
        data
      })
  }
  else {
    ret <- broom:::augment.lm(x, se = TRUE, ...)
  }
  # Rename columns back to the original names.
  names(ret) <- coalesce(x$terms_mapping[names(ret)], names(ret))
  ret
}

#' @export
augment.glm_exploratory <- function(x, data = NULL, newdata = NULL, data_type = "training", binary_classification_threshold = 0.5, ...) {
  if ("error" %in% class(x)) {
    ret <- data.frame()
    return(ret)
  }
  if(!is.null(newdata)) {
    # Replay the mutations on predictors.
    if(!is.null(x$predictor_funs)) {
      newdata <- newdata %>% mutate_predictors(x$orig_predictor_cols, x$predictor_funs)
    }

    predictor_variables <- all.vars(x$terms)[-1]
    predictor_variables_orig <- x$terms_mapping[predictor_variables]

    # Rename columns via predictor_variables_orig, which is a named vector.
    # TODO: What if names of the other columns conflicts with our temporary name, c1_, c2_...?
    cleaned_data <- newdata %>% dplyr::rename(predictor_variables_orig)

    # Align factor levels including Others and (Missing) to the model. TODO: factor level order can be different from the model training data. Is this ok?
    cleaned_data <- align_predictor_factor_levels(cleaned_data, x$xlevels, predictor_variables)

    na_row_numbers <- ranger.find_na(predictor_variables, cleaned_data)
    if (length(na_row_numbers) > 0) {
      cleaned_data <- cleaned_data[-na_row_numbers,]
    }

    ret <- tryCatch({
      broom:::augment.glm(x, data = NULL, newdata = cleaned_data, se = TRUE, ...)
    }, error = function(e){
      # se=TRUE throws error that looks like "singular matrix 'a' in solve",
      # in some subset of cases of perfect collinearity.
      # Try recovering from it by running with se=FALSE.
      broom:::augment.glm(x, data = NULL, newdata = cleaned_data, se = FALSE, ...)
    })
  } else if (!is.null(data)) {
    # Bring the target column to the last so that it is next to the predicted value in the output.
    # Note that source.data of lm/glm model df has mapped column names, unlike that of ranger.
    data <- data %>% dplyr::relocate(!!rlang::sym(x$target_col), .after = last_col())
    ret <- switch(data_type,
      training = {
        # If SMOTE was applied, use stored predictions on original training data
        if (!is.null(x$smote_applied) && x$smote_applied && !is.null(x$prediction_training)) {
          data$.fitted <- x$prediction_training$fit
          data$.se.fit <- x$prediction_training$se.fit
          data
        } else {
          tryCatch({
            broom:::augment.glm(x, data = data, newdata = newdata, se = TRUE, ...)
          }, error = function(e){
            broom:::augment.glm(x, data = data, newdata = newdata, se = FALSE, ...)
          })
        }
      },
      test = {
        # Augment data with already predicted result in the model.
        data$.fitted <- restore_na(x$prediction_test$fit, x$prediction_test$unknown_category_rows_index)
        data$.se.fit <- restore_na(x$prediction_test$se.fit, x$prediction_test$unknown_category_rows_index)
        data
      })
  }
  else {
    ret <- tryCatch({
      broom:::augment.glm(x, se = TRUE, ...)
    }, error = function(e){
      broom:::augment.glm(x, se = FALSE, ...)
    })
  }

  ret <- add_response(ret, x, "predicted_response") # Add response.
  if (!is.null(ret$.fitted)) {
    # Bring predicted_response column as the first of the prediction result related additional columns, so that it comes next to the actual values.
    ret <- ret %>% dplyr::relocate(any_of(c("predicted_response")), .before=.fitted)
  }

  if (x$family$family == "binomial") { # Add predicted label in case of binomial (including logistic regression).
    ret$predicted_label <- ret$predicted_response > binary_classification_threshold
  }
  # Rename columns back to the original names.
  names(ret) <- coalesce(x$terms_mapping[names(ret)], names(ret))
  ret
}

# For some reason, find_data called from inside margins::marginal_effects() fails in Exploratory.
# Explicitly declaring find_data for our glm_exploratory class works it around.
#' @export
find_data.glm_exploratory <- function(model, env = parent.frame(), ...) {
  model$data
}




# Calculate log likelihood and residual deviance from actual and predicted values for GLM families
calc_glm_test_metrics <- function(actual, predicted, m) {
  # The smallest positive floating-point number that can be represented on your computer.
  # This is used to avoid log(0) and other numerical issues.
  eps <- .Machine$double.eps
  predicted <- pmax(predicted, eps)
  actual <- pmax(actual, eps)
  family <- if (!is.null(m$family) && !is.null(m$family$family)) m$family$family else ""

  # family can be "Negative Binomial(456719.3)". In that case, we treat it as "negativebinomial".
  family <- if (grepl("Negative Binomial", family)) "negativebinomial" else family

  # Support both 'negativebinomial' and 'negbin' as family names for negative binomial models
  log_likelihood <- NA
  residual_deviance <- NA
  if (family == "poisson") {
    # Poisson log likelihood and deviance
    # logLik: sum(y * log(mu) - mu - log(y!))
    # Deviance: 2 * sum(y * log(y/mu) - (y - mu)), with 0*log(0) defined as 0
    # Ensure predicted values are strictly positive to avoid log(0) or log of negative numbers
    predicted <- pmax(predicted, .Machine$double.eps)
    log_likelihood <- sum(actual * log(predicted) - predicted - lfactorial(actual), na.rm = TRUE)
    residual_deviance <- 2 * sum(
      actual * log(ifelse(actual == 0, 1, actual / predicted)) - (actual - predicted),
      na.rm = TRUE
    )
  } else if (family == "negativebinomial" || family == "negbin") {
    # Negative binomial log likelihood and deviance
    # logLik: sum(lgamma(y + theta) - lgamma(theta) - lgamma(y + 1) +
    #             theta * log(theta / (theta + mu)) + y * log(mu / (theta + mu)))
    # Deviance: 2 * sum(y * log(y/mu) - (y + theta) * log((y + theta)/(mu + theta) ) + theta * log(mu / theta))
    theta <- if (!is.null(m$theta)) m$theta else stop("theta (dispersion) not found in model for negative binomial")
    log_likelihood <- sum(
      lgamma(actual + theta) - lgamma(theta) - lgamma(actual + 1) +
      theta * log(theta / (theta + predicted)) +
      actual * log(predicted / (theta + predicted)),
      na.rm = TRUE
    )
    residual_deviance <- 2 * sum(
      ifelse(actual == 0, 0, actual * log(actual / predicted)) -
      (actual + theta) * log((actual + theta) / (predicted + theta)) +
      theta * log((predicted + theta) / theta),
      na.rm = TRUE
    )
  } else if (family == "Gamma") {
    # Gamma log likelihood and deviance
    # logLik: sum(shape * (log(shape) + log(y) - log(mu)) - shape * (y/mu) - lgamma(shape))
    # Deviance: 2 * sum((y - mu)/mu - log(y/mu))
    shape <- tryCatch(1 / summary(m)$dispersion, error = function(e) NULL)
    if (is.null(shape)) stop("shape parameter not found for Gamma model")
    log_likelihood <- sum(
      shape * (log(shape) + log(actual) - log(predicted)) -
      shape * (actual / predicted) - lgamma(shape),
      na.rm = TRUE
    )
    residual_deviance <- 2 * sum(
      (actual - predicted) / predicted - log(actual / predicted),
      na.rm = TRUE
    )
  } else if (family == "inverse.gaussian") {
    # Inverse Gaussian log likelihood and deviance
    # logLik: sum(-0.5 * log(2 * pi * dispersion * y^3) - ((y - mu)^2) / (2 * dispersion * mu^2 * y))
    # Deviance: sum(((y - mu)^2) / (mu^2 * y))
    dispersion <- tryCatch(summary(m)$dispersion, error = function(e) NULL)
    if (is.null(dispersion)) stop("dispersion parameter not found for inverse gaussian model")
    log_likelihood <- sum(
      -0.5 * log(2 * pi * dispersion * actual^3) -
      ((actual - predicted)^2) / (2 * dispersion * predicted^2 * actual),
      na.rm = TRUE
    )
    residual_deviance <- sum(((actual - predicted)^2) / (predicted^2 * actual), na.rm = TRUE)
  }
  list(log_likelihood = log_likelihood, residual_deviance = residual_deviance)
}
