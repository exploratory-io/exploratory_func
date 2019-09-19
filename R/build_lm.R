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


#' lm wrapper with do
#' @return deta frame which has lm model
#' @param data Data frame to be used as data
#' @param formula Formula for lm
#' @param ... Parameters to be passed to lm function
#' @param keep.source Whether source should be kept in source.data column
#' @param augment Whether the result should be augmented immediately
#' @param group_cols A vector with columns names to be used as group columns
#' @param test_rate Ratio of test data
#' @param seed Random seed to control test data sampling
#' @export
build_lm <- function(data, formula, ..., keep.source = TRUE, augment = FALSE, group_cols = NULL, test_rate = 0.0, seed = 0){
  validate_empty_data(data)

  # make variables factor sorted by the frequency
  fml_vars <- all.vars(formula)
  for(var in fml_vars) {
    if(is.character(data[[var]])){
      data[[var]] <- forcats::fct_infreq(data[[var]])
    }
  }

  if(!is.null(seed)){
    set.seed(seed)
  }

  # deal with group columns by index because those names might be changed
  group_col_index <- colnames(data) %in% group_cols

  # change column names to avoid name conflict when tidy or glance are executed
  reserved_names <- c(
    "model", ".test_index", "data", ".model_metadata",
    # for tidy
    "term", "estimate", "std.error", "statistic", "p.value",
    # for glance
    "r.squared", "adj.r.squared", "sigma", "statistic", "p.value",
    "df", "logLik", "AIC", "BIC", "deviance", "df.residual"
  )


  if(test_rate < 0 | 1 < test_rate){
    stop("test_rate must be between 0 and 1")
  } else if (test_rate == 1){
    stop("test_rate must be less than 1")
  }

  colnames(data)[group_col_index] <- avoid_conflict(
    reserved_names,
    colnames(data)[group_col_index],
    ".group"
  )

  # make column names unique
  colnames(data) <- make.unique(colnames(data), sep = "")

  if(!is.null(group_cols)){
    data <- dplyr::group_by(data, !!!rlang::syms(colnames(data)[group_col_index]))
  } else if (!dplyr::is.grouped_df(data)) {
    # grouping is necessary for tidyr::nest to work so putting one value columns
    data <- data %>%
      dplyr::mutate(source.data = 1) %>%
      dplyr::group_by(source.data)
  }

  group_col_names <- grouped_by(data)

  # check if grouping columns are in the formula
  grouped_var <- group_col_names[group_col_names %in% fml_vars]
  if (length(grouped_var) == 1) {
    stop(paste0(grouped_var, " is a grouping column. Please remove it from variables."))
  } else if (length(grouped_var) > 0) {
    stop(paste0(paste(grouped_var, collapse = ", "), " are grouping columns. Please remove them from variables."))
  }

  model_col <- "model"
  source_col <- "source.data"

  caller <- match.call()
  # this expands dots arguemtns to character
  arg_char <- expand_args(caller, exclude = c("data", "keep.source", "augment", "group_cols", "test_rate", "seed"))

  ret <- tryCatch({
    ret <- data %>%
      tidyr::nest(.key = "source.data") %>%
      # create test index
      dplyr::mutate(.test_index = purrr::map(source.data, function(df){
        sample_df_index(df, rate = test_rate)
      })) %>%
      # slice training data
      dplyr::mutate(model = purrr::map2(source.data, .test_index, function(df, index){
        data <- safe_slice(df, index, remove = TRUE)

        # execute lm with parsed arguments
        eval(parse(text = paste0("stats::lm(data = data, ", arg_char, ")")))
      })) %>%
      dplyr::mutate(.model_metadata = purrr::map(source.data, function(df){
        if(!is.null(formula)){
          create_model_meta(df, formula)
        } else {
          list()
        }
      }))
    if(!keep.source & !augment){
      ret <- dplyr::select(ret, -source.data)
    } else {
      class(ret[[source_col]]) <- c("list", ".source.data")
    }
    ret <- dplyr::rowwise(ret)
    ret
  }, error = function(e){
    # error message was changed when upgrading dplyr to 0.7.1
    # so use stringr::str_detect to make these robust
    if(stringr::str_detect(e$message, "contrasts can be applied only to factors with 2 or more levels")){
      stop("more than 1 unique values are expected for categorical columns assigned as predictors")
    }
    if(stringr::str_detect(e$message, "0 \\(non\\-NA\\) cases")){
      stop("no data after removing NA")
    }

    stop(e$message)
  })
  if(augment){
    if(test_rate == 0){
      ret <- prediction(ret, data = "training")
    } else {
      ret <- prediction(ret, data = "test")
    }
  } else {
    class(ret[[model_col]]) <- c("list", ".model", ".model.lm")
  }
  ret
}

#' builds lm model quickly for analytics view.
#' @param relimp - Whether to enable relative importance by relaimpo.
#' @param relimp_type - Passed down to boot.relimp, but "lmg" seems to be the recommended option, but is very slow. default is "first".
#' @param relimp_bootstrap_runs - Number of bootstrap iterations. Default 20.
#' @param relimp_bootstrap_type - Type of bootstrapping, passed down to boot package from inside relaimpo package.
#'                                Can be "basic", "perc", "bca", or "norm".
#' @param relimp_conf_level - Confidence level for confidence interval of relative importance values. Default is 0.95.
#' @param relimp_relative - When TRUE, relative importance values add up to 1. When FALSE they add up to R-Squared.
#' @param seed - Random seed to control data sampling, SMOTE, and bootstrapping for confidence interval of relative importance.
#' @param test_rate Ratio of test data
#' @export
build_lm.fast <- function(df,
                    target,
                    ...,
                    model_type = "lm",
                    family = NULL,
                    link = NULL,
                    max_nrow = 50000,
                    predictor_n = 12, # so that at least months can fit in it.
                    normalize_predictors = FALSE,
                    target_outlier_filter_type = NULL,
                    target_outlier_filter_threshold = NULL,
                    predictor_outlier_filter_type = NULL,
                    predictor_outlier_filter_threshold = NULL,
                    smote = FALSE,
                    smote_target_minority_perc = 40,
                    smote_max_synth_perc = 200,
                    smote_k = 5,
                    relimp = FALSE,
                    relimp_type = "first",
                    relimp_bootstrap_runs = 20,
                    relimp_bootstrap_type = "perc",
                    relimp_conf_level = 0.95,
                    relimp_relative = TRUE,
                    with_marginal_effects = FALSE,
                    with_marginal_effects_confint = FALSE,
                    variable_metric = NULL,
                    seed = 1,
                    test_rate = 0.0,
                    test_split_type = "random" # "random" or "ordered"
                    ){
  target_col <- dplyr::select_var(names(df), !! rlang::enquo(target))
  selected_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  if (!is.null(variable_metric)  && variable_metric == "ame") { # Special argument for integration with Analytics View.
    with_marginal_effects <- TRUE
  }

  if (model_type  == "glm" && is.null(family)) {
    family = "binomial" # default for glm is logistic regression.
  }

  if(test_rate < 0 | 1 < test_rate){
    stop("test_rate must be between 0 and 1")
  } else if (test_rate == 1){
    stop("test_rate must be less than 1")
  }

  # drop unrelated columns so that SMOTE later does not have to deal with them.
  # select_ was not able to handle space in target_col. let's do it in base R way.
  df <- df[,colnames(df) %in% c(grouped_cols, selected_cols, target_col), drop=FALSE]

  # remove grouped col or target col
  selected_cols <- setdiff(selected_cols, c(grouped_cols, target_col))

  if (any(c(target_col, selected_cols) %in% grouped_cols)) {
    stop("grouping column is used as variable columns")
  }

  if (predictor_n < 2) {
    stop("Max # of categories for explanatory vars must be at least 2.")
  }

  if(!is.null(seed)){
    set.seed(seed)
  }

  orig_levels <- NULL # For recording original factor levels for binary classification, to show data size for each class in Summary View.
  if (!is.null(model_type) && model_type == "glm" && family %in% c("binomial", "quasibinomial")) {
    # binomial case.
    unique_val <- unique(df[[target_col]])
    if (length(unique_val[!is.na(unique_val)]) != 2) {
      stop(paste0("Column to predict (", target_col, ") with Binomial Regression must have 2 unique values."))
    }
    if (!is.numeric(df[[target_col]]) && !is.factor(df[[target_col]]) && !is.logical(df[[target_col]])) {
      # make other types factor so that it passes stats::glm call.
      df[[target_col]] <- factor(df[[target_col]])
    }
    # record original factor levels.
    if (is.factor(df[[target_col]])) {
      orig_levels <- levels(df[[target_col]])
      # Keep only the ones that actually are used.
      # One with larger index seems to be treated as TRUE (i.e. 1 in model$y) by glm.
      orig_levels <- orig_levels[orig_levels %in% unique_val]
    }
    else if (is.logical(df[[target_col]])) {
      orig_levels <- c("FALSE","TRUE")
    }
  }
  else { # this means the model is lm or glm with family other than binomial
    if (!is.numeric(df[[target_col]])) {
      # TODO: message should handle other than lm too.
      stop(paste0("Column to predict (", target_col, ") with Linear Regression must be numeric"))
    }
  }

  # cols will be filtered to remove invalid columns
  cols <- selected_cols

  for (col in selected_cols) {
    if(all(is.na(df[[col]]))){
      # remove columns if they are all NA
      cols <- setdiff(cols, col)
      df[[col]] <- NULL # drop the column so that SMOTE will not see it. 
    }
  }

  # Replace spaces with dots in column names. margins::marginal_effects() fails without it.
  clean_df <- df
  if (model_type == "lm") {
    # For lm, we can skip column names cleaning, since we do not use marginal_effects().
  }
  else {
    # Cleaning of column names for marginal_effects(). Space is not handled well. Replace them with '.'.
    # Also, cleaning of column names for relaimpo. - is not handled well. Replace them with _.
    # For some reason, str_replace garbles some column names in Japanese. Using gsub instead to avoid the issue.
    # names(clean_df) <- stringr::str_replace_all(names(df), ' ', '.') %>% stringr::str_replace_all('-', '_')
    names(clean_df) <- gsub('\\-', '_', gsub(' ', '.', names(df)))
  }
  # this mapping will be used to restore column names
  name_map <- colnames(clean_df)
  names(name_map) <- colnames(df)

  # clean_names changes column names
  # without chaning grouping column name
  # information in the data frame
  # and it causes an error,
  # so the value of grouping columns
  # should be still the names of grouping columns
  name_map[grouped_cols] <- grouped_cols
  colnames(clean_df) <- name_map

  clean_target_col <- name_map[target_col]
  clean_cols <- name_map[cols]

  each_func <- function(df) {
    tryCatch({
      df_test <- NULL # declare variable for test data
      df <- df %>%
        # dplyr::filter(!is.na(!!target_col))  TODO: this was not filtering, and replaced it with the next line. check other similar places.
        # for numeric cols, filter NA rows, because lm will anyway do this internally, and errors out
        # if the remaining rows are with single value in any predictor column.
        # filter Inf/-Inf too to avoid error at lm.
        dplyr::filter(!is.na(df[[clean_target_col]]) & !is.infinite(df[[clean_target_col]])) # this form does not handle group_by. so moved into each_func from outside.

      # Sample the data because randomForest takes long time if data size is too large.
      # If we are to do SMOTE, do not down sample here and let exp_balance handle it so that we do not sample out precious minority data.
      sampled_nrow <- NULL
      if (!smote) {
        if (!is.null(max_nrow) && nrow(df) > max_nrow) {
          # Record that sampling happened.
          sampled_nrow <- max_nrow
          df <- df %>% sample_rows(max_nrow)
        }
      }

      c_cols <- clean_cols
      # To avoid unused factor level that causes margins::marginal_effects() to fail, filtering operation has
      # to be done before factor level adjustments. Because of that, the for statement below has to
      # be separate from the for statement after that, and done first.
      for(col in clean_cols){
        if(is.numeric(df[[col]]) || lubridate::is.Date(df[[col]]) || lubridate::is.POSIXct(df[[col]])) {
          # For numeric cols, filter NA rows, because lm will anyway do this internally, and errors out
          # if the remaining rows are with single value in any predictor column.
          # Filter Inf/-Inf too to avoid error at lm.
          # Do the same for Date/POSIXct, because we will create numeric columns from them.
          df <- df %>% dplyr::filter(!is.na(df[[col]]) & !is.infinite(df[[col]]))
        }
      }
      for(col in clean_cols){
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
          df[[day_col]] <- lubridate::day(df[[col]])
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
          if (length(levels(df[[col]])) >= predictor_n + 2) {
            df[[col]] <- forcats::fct_other(factor(df[[col]], ordered=FALSE), keep=levels(df[[col]])[1:predictor_n])
          }
          else {
            df[[col]] <- factor(df[[col]], ordered=FALSE)
          }
          # turn NA into (Missing) factor level. Without this, lm or glm drops rows internally.
          df[[col]] <- forcats::fct_explicit_na(df[[col]])
        } else if(is.logical(df[[col]])) {
          # 1. convert data to factor if predictors are logical. (as.factor() on logical always puts FALSE as the first level, which is what we want for predictor.)
          # 2. turn NA into (Missing) factor level so that lm will not drop all the rows.
          df[[col]] <- forcats::fct_explicit_na(as.factor(df[[col]]))
        } else if(!is.numeric(df[[col]])) {
          # 1. convert data to factor if predictors are not numeric or logical
          #    and limit the number of levels in factor by fct_lump.
          #    we use ties.method to handle the case where there are many unique values. (without it, they all survive fct_lump.)
          #    TODO: see if ties.method would make sense for calc_feature_imp.
          # 2. turn NA into (Missing) factor level so that lm will not drop all the rows.
          df[[col]] <- forcats::fct_explicit_na(forcats::fct_lump(forcats::fct_infreq(as.factor(df[[col]])), n=predictor_n, ties.method="first"))
        }
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

      if (!is.null(target_outlier_filter_type)) {
        is_outlier <- function(x) {
          res <- detect_outlier(x, type=target_outlier_filter_type, threshold=target_outlier_filter_threshold) %in% c("lower", "upper")
          res
        }
        df$.is.outlier <- FALSE #TODO: handle possibility of name conflict.
        if (is.numeric(df[[clean_target_col]])) {
          df$.is.outlier <- df$.is.outlier & is_outlier(df[[col]])
        }
        df <- df %>% dplyr::filter(!.is.outlier)
        df$.is.outlier <- NULL
      }

      if (!is.null(predictor_outlier_filter_type)) {
        is_outlier <- function(x) {
          res <- detect_outlier(x, type=predictor_outlier_filter_type, threshold=predictor_outlier_filter_threshold) %in% c("lower", "upper")
          res
        }
        df$.is.outlier <- FALSE #TODO: handle possibility of name conflict.
        for (col in c_cols) {
          if (is.numeric(df[[col]])) {
            df$.is.outlier <- df$.is.outlier & is_outlier(df[[col]])
          }
        }
        df <- df %>% dplyr::filter(!.is.outlier)
        df$.is.outlier <- NULL
      }

      # Normalize numeric predictors so that resulting coefficients are comparable among them,
      # after all column changes for Date/POSIXct, filtering, dropping columns above are done.
      if (normalize_predictors) {
        for (col in c_cols) {
          if (is.numeric(df[[col]])) {
            df[[col]] <- normalize(df[[col]])
          }
        }
      }

      # build formula for lm
      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      # TODO: This clean_target_col is actually not a cleaned column name since we want lm to show real name. Clean up our variable name.
      fml <- as.formula(paste0("`", clean_target_col, "` ~ ", rhs))
      if (model_type == "glm") {
        if (smote) {
          if (with_marginal_effects) {
            # Keep the version of data before SMOTE,
            # since we want to know average marginal effect on a data that has
            # close distribution to the original data.
            df_before_smote <- df
          }
          df <- df %>% exp_balance(clean_target_col, target_size = max_nrow, target_minority_perc = smote_target_minority_perc, max_synth_perc = smote_max_synth_perc, k = smote_k)
          df <- df %>% dplyr::select(-synthesized) # Remove synthesized column added by exp_balance(). TODO: Handle it better. We might want to show it in resulting data.
          for(col in names(df)){
            if(is.factor(df[[col]])) {
              # margins::marginal_effects() fails if unused factor level exists. Drop them to avoid it.
              # In case of SMOTE, this has to be done after that. TODO: Do this just once in any case.
              df[[col]] <- forcats::fct_drop(df[[col]])
              if (with_marginal_effects) {
                df_before_smote <- df_before_smote %>% dplyr::filter(!!rlang::sym(col) %in% levels(df[[col]]))
                df_before_smote[[col]] <- forcats::fct_drop(df_before_smote[[col]])
              }
            }
          }
          if (with_marginal_effects) {
            # Sample df_before_smote for speed.
            # We do not remove imbalance here to keep close distribution to the original data.
            df_before_smote <- df_before_smote %>% sample_rows(max_nrow)
          }
        }

        # split training and test data
        source_data <- df
        test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
        df <- safe_slice(source_data, test_index, remove = TRUE)
        if (test_rate > 0) {
          # Test mode. Make prediction with test data here, rather than repeating it in Analytics View preprocessors.
          df_test <- safe_slice(source_data, test_index, remove = FALSE)
          # Remove rows with categorical values which does not appear in training data and unknown to the model.
          # Record where it was in unknown_category_rows_index, and keep it with model, so that prediction that
          # matches with original data can be generated later.
          unknown_category_rows_index_vector <- get_unknown_category_rows_index_vector(df_test, df)
          df_test <- df_test[!unknown_category_rows_index_vector, , drop = FALSE] # 2nd arg must be empty.
          unknown_category_rows_index <- get_row_numbers_from_index_vector(unknown_category_rows_index_vector)
        }

        # when family is negativebinomial, use MASS::glm.nb
        if (is.null(link) && family != "negativebinomial") {
          model <- stats::glm(fml, data = df, family = family) 
        }
        else {
          if (family == "gaussian") {
            family_arg <- stats::gaussian(link=link)
          }
          else if (family == "binomial") {
            family_arg <- stats::binomial(link=link)
          }
          else if (family == "Gamma") {
            family_arg <- stats::Gamma(link=link)
          }
          else if (family == "inverse.gaussian") {
            family_arg <- stats::inverse.gaussian(link=link)
          }
          else if (family == "poisson") {
            family_arg <- stats::poisson(link=link)
          }
          else if (family == "quasi") {
            family_arg <- stats::quasi(link=link)
          }
          else if (family == "quasibinomial") {
            family_arg <- stats::quasibinomial(link=link)
          }
          else if (family == "quasipoisson") {
            family_arg <- stats::quasipoisson(link=link)
          }
          else if (family == "negativebinomial") {
            family_arg <- family
          }
          else { # default gaussian
            family_arg <- stats::gaussian(link=link)
          }

          if (family_arg == "negativebinomial"){
            # In MASS::glm.nb function, the link arg must be one of log, sqrt or identity.
            # So if the other is used, the link arg should be set to "log" which is the default value.
            if (is.null(link) || (!link %in% c("log", "sqrt", "identity"))){
              link <- "log"
            }

            # The argument link in MASS::glm.nb is evaluated by substitution with delay,
            # so the variable specified in the argument is interpreted as the link argument as it is.
            # For example, if you execute like MASS::glm.nb(fmt, data = df, link = link), the following error will occur
            # link "link" not available for poisson family; available links are 'log', 'identity', 'sqrt'
            # Therefore, we used eval to pass the string (log etc.) specified in the argument to link as it is.
            model <- eval(parse(text=paste0("MASS::glm.nb(fml, data=df, link=", link, ")")))

            # A model by MASS::glm.nb has not a formula attribute.
            model$formula <- fml
          } else {
            model <- stats::glm(fml, data = df, family = family_arg)
          }
        }
      }
      else {
        # split training and test data
        source_data <- df
        test_index <- sample_df_index(source_data, rate = test_rate, ordered = (test_split_type == "ordered"))
        df <- safe_slice(source_data, test_index, remove = TRUE)
        if (test_rate > 0) {
          df_test <- safe_slice(source_data, test_index, remove = FALSE)
          unknown_category_rows_index_vector <- get_unknown_category_rows_index_vector(df_test, df)
          df_test <- df_test[!unknown_category_rows_index_vector, , drop = FALSE] # 2nd arg must be empty.
          unknown_category_rows_index <- get_row_numbers_from_index_vector(unknown_category_rows_index_vector)
        }

        model <- stats::lm(fml, data = df) 
        if (relimp && length(c_cols) > 1) { # relimp seems to work only when there are multiple predictors, which makes sense since it is "relative".
          tryCatch({
            # Calculate relative importance.
            model$relative_importance <- relaimpo::booteval.relimp(relaimpo::boot.relimp(model, type = relimp_type,
                                                                                      b = relimp_bootstrap_runs,
                                                                                      rela = relimp_relative,
                                                                                      rank = FALSE,
                                                                                      diff = FALSE),
                                                                bty = relimp_bootstrap_type, level = relimp_conf_level)
          }, error = function(e){
            # This can fail when columns are not linearly independent. Record error and keep going.
            model$relative_importance <<- e
          })
        }
      }

      tryCatch({
        model$vif <- car::vif(model)
      }, error = function(e){
        # in case of perfect multicollinearity, vif throws error with message "there are aliased coefficients in the model".
        # Check it it is the case. If coef() includes NA, corresponding variable is causing perfect multicollinearity.
        coef_vec <- coef(model)
        na_coef_vec <- coef_vec[is.na(coef_vec)]
        if (length(na_coef_vec) > 0) {
          na_coef_names <- names(na_coef_vec)
          message <- paste(na_coef_names, collapse = ", ")
          message <- paste0("Variables causing perfect collinearity : ", message)
          e$message <- message
        }
        model$vif <<- e
      })

      if (test_rate > 0) {
        # Note: Do not pass df_test like data=df_test. This for some reason ends up predict returning training data prediction.
        model$prediction_test <- predict(model, df_test, se.fit = TRUE)
        model$prediction_test$unknown_category_rows_index <- unknown_category_rows_index
      }
      # these attributes are used in tidy of randomForest TODO: is this good for lm too?
      model$terms_mapping <- names(name_map)
      names(model$terms_mapping) <- name_map
      model$orig_levels <- orig_levels

      # For displaying if sampling happened or not.
      model$sampled_nrow <- sampled_nrow

      # add special lm_exploratory class for adding extra info at glance().
      if (model_type == "glm") {
        class(model) <- c("glm_exploratory", class(model))
        if (with_marginal_effects) { # For now, we have tested marginal_effects for logistic regression only. It seems to fail for probit for example.
          if (smote) {
            model$marginal_effects <- calc_average_marginal_effects(model, data=df_before_smote, with_confint=with_marginal_effects_confint) # This has to be done after glm_exploratory class name is set.
          }
          else {
            model$marginal_effects <- calc_average_marginal_effects(model, with_confint=with_marginal_effects_confint) # This has to be done after glm_exploratory class name is set.
          }
        }
      }
      else {
        class(model) <- c("lm_exploratory", class(model))
      }
      list(model = model, test_index = test_index, source_data = source_data)

    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # ignore the error if
        # it is caused by subset of
        # grouped data frame
        # to show result of
        # data frames that succeed
        NULL
      } else {
        stop(e)
      }
    })
  }

  model_and_data_col <- "model_and_data"
  ret <- do_on_each_group(clean_df, each_func, name = model_and_data_col, with_unnest = FALSE)
  if (length(grouped_cols) > 0) {
    ret <- ret %>% tidyr::nest(-grouped_cols)
  } else {
    ret <- ret %>% tidyr::nest()
  }
  ret %>% dplyr::mutate(model = purrr::map(data, function(df){
            df[[model_and_data_col]][[1]]$model
          })) %>%
          dplyr::mutate(.test_index = purrr::map(data, function(df){
            df[[model_and_data_col]][[1]]$test_index
          })) %>%
          dplyr::mutate(source.data = purrr::map(data, function(df){
            data <- df[[model_and_data_col]][[1]]$source_data
            if (length(grouped_cols) > 0 && !is.null(data)) {
              data %>% dplyr::select(-grouped_cols)
            } else {
              data
            }
          })) %>%
          dplyr::select(-data) %>%
          dplyr::rowwise()
}

#' special version of glance.lm function to use with build_lm.fast.
#' @export
glance.lm_exploratory <- function(x, pretty.name = FALSE, ...) { #TODO: add test
  ret <- broom:::glance.lm(x)

  for(var in names(x$xlevels)) { # extract base levels info on factor/character columns from lm model
    if(pretty.name) {
      ret[paste0("Base Level of ", var)] <- x$xlevels[[var]][[1]]
    }
    else {
      ret[paste0(var, "_base")] <- x$xlevels[[var]][[1]]
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

  # We are checking if it is of error class, since in build_lm.fast, if calculation of relative importance fails, we set the returned error
  # so that we can report the error in Summary table (return from tidy()).
  if (!is.null(x$relative_importance) && "error" %in% class(x$relative_importance)) {
    note <- x$relative_importance$message
    if (note == "covg must be \n a positive definite covariance matrix \n or a data matrix / data frame with linearly independent columns.") {
      note <- "Calculation of variable importance failed most likely due to perfect multicollinearity."
    }
    ret <- ret %>% dplyr::mutate(note=note)
  }

  if(pretty.name) {
    ret <- ret %>% dplyr::rename(`R Squared`=r.squared, `Adj R Squared`=adj.r.squared, `RMSE`=rmse, `F Ratio`=statistic, `P Value`=p.value, `Degree of Freedom`=df, `Log Likelihood`=logLik, Deviance=deviance, `Residual DF`=df.residual, `Number of Rows`=n)
    # Note column might not exist. Rename if it is there.
    colnames(ret)[colnames(ret) == "note"] <- "Note"
  }
  ret
}

#' special version of glance.lm function to use with build_lm.fast.
#' @export
glance.glm_exploratory <- function(x, pretty.name = FALSE, binary_classification_threshold = 0.5, ...) { #TODO: add test
  ret <- broom:::glance.glm(x)

  # calculate model p-value since glm does not provide it as is.
  # https://stats.stackexchange.com/questions/129958/glm-in-r-which-pvalue-represents-the-goodness-of-fit-of-entire-model
  f0 <- x$formula # copy formula as a basis for null model.
  lazyeval::f_rhs(f0) <- 1 # create null model formula.
  x0 <- glm(f0, x$model, family = x$family) # build null model. Use x$model rather than x$data since x$model seems to be the data after glm handled missingness.
  pvalue <- with(anova(x0,x),pchisq(Deviance,Df,lower.tail=FALSE)[2]) 
  if(pretty.name) {
    ret <- ret %>% dplyr::mutate(`P Value`=!!pvalue, `Number of Rows`=!!length(x$y))
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
  
  if (x$family$family %in% c('binomial', 'quasibinomial')) { # only for logistic regression.
    # Calculate F Score, Accuracy Rate, Misclassification Rate, Precision, Recall, Number of Rows 
    threshold_value <- if (is.numeric(binary_classification_threshold)) {
      binary_classification_threshold
    } else {
      get_optimized_score(x$y, x$fitted.value, threshold = binary_classification_threshold)$threshold
    }
    predicted <- ifelse(x$fitted.value > threshold_value, 1, 0) #TODO make threshold adjustable
    ret2 <- evaluate_classification(x$y, predicted, 1, pretty.name = pretty.name)
    ret2 <- ret2[, 2:6]
    ret <- ret %>% bind_cols(ret2)

    # calculate AUC from ROC
    roc_df <- data.frame(actual = x$y, predicted_probability = x$fitted.value)
    roc <- roc_df %>% do_roc_(actual_val_col = "actual", pred_prob_col = "predicted_probability")
    # use numeric index so that it won't be disturbed by name change
    # 2 should be false positive rate (x axis) and 1 should be true positive rate (yaxis)
    # calculate the area under the plots
    auc <- sum((roc[[2]] - dplyr::lag(roc[[2]])) * roc[[1]], na.rm = TRUE)
    ret$auc <- auc
    # Show number of rows for positive case and negative case, especially so that result of SMOTE is visible.
    ret$positives <- sum(x$y == 1, na.rm = TRUE)
    ret$negatives <- sum(x$y != 1, na.rm = TRUE)
  }

  for(var in names(x$xlevels)) { # extract base levels info on factor/character columns from lm model
    if(pretty.name) {
      ret[paste0("Base Level of ", var)] <- x$xlevels[[var]][[1]]
    }
    else {
      ret[paste0(var, "_base")] <- x$xlevels[[var]][[1]]
    }
  }

  if(pretty.name) {
    if (x$family$family %in% c('binomial', 'quasibinomial')) { # for binomial regressions.
      ret <- ret %>% dplyr::rename(`Null Deviance`=null.deviance, `DF for Null Model`=df.null, `Log Likelihood`=logLik, Deviance=deviance, `Residual DF`=df.residual, `AUC`=auc) %>%
        dplyr::select(`F Score`, `Accuracy Rate`, `Misclassification Rate`, `Precision`, `Recall`, `AUC`,`P Value`, `Number of Rows`, positives, negatives,  `Log Likelihood`, `AIC`, `BIC`, `Deviance`, `Null Deviance`, `DF for Null Model`, everything())
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
      colnames(ret)[colnames(ret) == "positives"] <- paste0("Number of Rows for ", pos_label)
      colnames(ret)[colnames(ret) == "negatives"] <- paste0("Number of Rows for ", neg_label)
    }
    else { # for other numeric regressions.
      ret <- ret %>% dplyr::rename(`Null Deviance`=null.deviance, `DF for Null Model`=df.null, `Log Likelihood`=logLik, Deviance=deviance, `Residual DF`=df.residual) %>%
        dplyr::select(`P Value`, `Number of Rows`, `Log Likelihood`, `AIC`, `BIC`, `Deviance`, `Null Deviance`, `DF for Null Model`, everything())
    }
  }

  ret
}

# Creates a data frame that maps term name to its base level.
xlevels_to_base_level_table <- function(xlevels) {
  term <- purrr::flatten_chr(purrr::map(names(xlevels), function(vname) {
    # Quote variable name with backtick if it includes special characters or space.
    # Special characters to detect besides space. Note that period and underscore should *not* be included here. : ~!@#$%^&*()+={}|:;'<>,/?"[]-\
    paste0(if_else(stringr::str_detect(vname,"[ ~!@#$%^&*()+={}|:;'<>,/?\"\\[\\]\\-\\\\]"),paste0('`',vname,'`'),vname),xlevels[[vname]])
  }))
  base_level <- purrr::flatten_chr(purrr::map(xlevels, function(v){rep(v[[1]],length(v))}))
  ret <- data.frame(term=term, base.level=base_level)
  ret
}

vif_to_dataframe <- function(x) {
  ret <- NULL
  if (is.matrix(x$vif)) {
    ret <- x$vif %>% as.data.frame() %>%  rownames_to_column(var="term") %>% rename(VIF=GVIF)
  }
  else {
    ret <- data.frame(term=names(x$vif), VIF=x$vif)
  }
  ret
}

#' special version of tidy.lm function to use with build_lm.fast.
#' @export
tidy.lm_exploratory <- function(x, type = "coefficients", pretty.name = FALSE, ...) { #TODO: add test
  switch(type,
    coefficients = {
      ret <- broom:::tidy.lm(x) # it seems that tidy.lm takes care of glm too
      ret <- ret %>% mutate(conf.high=estimate+1.96*std.error, conf.low=estimate-1.96*std.error)
      base_level_table <- xlevels_to_base_level_table(x$xlevels)
      # Convert term from factor to character to remove warning at left_join.
      ret <- ret %>% dplyr::mutate(term=as.character(term)) %>% dplyr::left_join(base_level_table, by="term")
      if (any(is.na(x$coefficients))) {
        # since broom skips coefficients with NA value, which means removed by lm because of multi-collinearity,
        # put it back to show them.
        # reference: https://stats.stackexchange.com/questions/25804/why-would-r-return-na-as-a-lm-coefficient
        removed_coef_df <- data.frame(term=names(x$coefficients[is.na(x$coefficients)]), note="Dropped most likely due to perfect multicollinearity.")
        ret <- ret %>% bind_rows(removed_coef_df)
        if (pretty.name) {
          ret <- ret %>% rename(Note=note)
        }
      }
      if (pretty.name) {
        ret <- ret %>% rename(Term=term, Coefficient=estimate, `Std Error`=std.error,
                              `t Ratio`=statistic, `P Value`=p.value,
                              `Conf Low`=conf.low,
                              `Conf High`=conf.high,
                              `Base Level`=base.level)
      }
      ret
    },
    relative_importance = {
      # We are checking if it is of error class, since in build_lm.fast, if calculation of relative importance fails, we set the returned error
      # so that we can report the error in Summary table (return from tidy()).
      if (!is.null(x$relative_importance) && "error" %nin% class(x$relative_importance)) {
        # Add columns for relative importance. NA for the first row is for the row for intercept.
        term <- x$relative_importance$namen[2:length(x$relative_importance$namen)] # Skip first element, which is the target variable name.
        importance <- attr(x$relative_importance, x$relative_importance$type)
        importance.high <- attr(x$relative_importance, paste0(x$relative_importance$type, ".upper"))[1,] # Following naming convention of other columns.
        importance.low <- attr(x$relative_importance, paste0(x$relative_importance$type, ".lower"))[1,] # Following naming convention of other columns.
        ret <- data.frame(term = term, importance = importance, importance.high = importance.high, importance.low = importance.low)
        # Reorder factor by the value of relative importance (lmg).
        ret <- ret %>% dplyr::mutate(term = forcats::fct_reorder(term, importance, .fun = sum, .desc = TRUE))
        if (pretty.name) {
          ret <- ret %>% rename(`Variable` = term,
                                `Relative Importance` = importance,
                                `Relative Importance High` = importance.high,
                                `Relative Importance Low` = importance.low)
        }
        ret
      }
      else {
        ret <- data.frame() # Skip output for this group.
        ret
      }
    },
    vif = {
      if (!is.null(x$vif) && "error" %nin% class(x$vif)) {
        ret <- vif_to_dataframe(x)
      }
      else {
        ret <- data.frame() # Skip output for this group. TODO: Report error in some way.
      }
      ret
    }
  )
}

#' special version of tidy.glm function to use with build_lm.fast.
#' @export
tidy.glm_exploratory <- function(x, type = "coefficients", pretty.name = FALSE, variable_metric = NULL, ...) { #TODO: add test
  switch(type,
    coefficients = {
      ret <- broom:::tidy.lm(x) # it seems that tidy.lm takes care of glm too
      ret <- ret %>% mutate(conf.high=estimate+1.96*std.error, conf.low=estimate-1.96*std.error)
      if (x$family$family == "binomial") { # odds ratio is only for logistic regression
        ret <- ret %>% mutate(odds_ratio=exp(estimate))
        if (!is.null(variable_metric) && variable_metric == "odds_ratio") { # For Analytics View, overwrite conf.low/conf.high with those of odds ratio.
          ret <- ret %>% mutate(conf.low=exp(conf.low), conf.high=exp(conf.high))
        }
      }
      if (!is.null(x$marginal_effects)) {
        # Convert term from factor to character to remove warning at left_join.
        ret <- ret %>% dplyr::mutate(term=as.character(term)) %>% dplyr::left_join(x$marginal_effects, by="term")
      }
      base_level_table <- xlevels_to_base_level_table(x$xlevels)
      # Convert term from factor to character to remove warning at left_join.
      ret <- ret %>% dplyr::mutate(term=as.character(term)) %>% dplyr::left_join(base_level_table, by="term")
      if (any(is.na(x$coefficients))) {
        # since broom skips coefficients with NA value, which means removed by lm because of multi-collinearity,
        # put it back to show them.
        # reference: https://stats.stackexchange.com/questions/25804/why-would-r-return-na-as-a-lm-coefficient
        removed_coef_df <- data.frame(term=names(x$coefficients[is.na(x$coefficients)]), note="Dropped most likely due to perfect multicollinearity.")
        ret <- ret %>% bind_rows(removed_coef_df)
        if (pretty.name) {
          ret <- ret %>% rename(Note=note)
        }
      }
      if (pretty.name) {
        ret <- ret %>% rename(Term=term, Coefficient=estimate, `Std Error`=std.error,
                              `t Ratio`=statistic, `P Value`=p.value, `Conf Low`=conf.low, `Conf High`=conf.high,
                              `Base Level`=base.level)
        if (!is.null(ret$ame)) {
          ret <- ret %>% rename(`Average Marginal Effect`=ame)
        }
        if (!is.null(ret$ame_low)) {
          ret <- ret %>% rename(`AME Low`=ame_low,`AME High`=ame_high)
        }
        if (x$family$family == "binomial") { # odds ratio is only for logistic regression
          ret <- ret %>% rename(`Odds Ratio`=odds_ratio)
        }
      }
      ret
    },
    conf_mat = {
      target_col <- as.character(lazyeval::f_lhs(x$formula)) # get target column name
      actual_val = x$model[[target_col]]

      predicted = x$fitted.value > 0.5 # TODO: make threshold adjustable
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
    }
  )
}

#' @export
augment.lm_exploratory <- function(x, data = NULL, newdata = NULL, data_type = "training", ...) {
  if(!is.null(newdata)) { # Call broom:::augment.lm as is
    broom:::augment.lm(x, data = data, newdata = newdata, ...)
  } else if (!is.null(data)) {
    switch(data_type,
      training = { # Call broom:::augment.lm as is
        broom:::augment.lm(x, data = data, newdata = newdata, ...)
      },
      test = {
        # Minic broom:::augment.lm behavior of replacing spaces in column names. Without this, after bind_row in prediction(), such columns will end up in 2 separate columns.
        # For some reason, str_replace garbles some column names in Japanese. Using gsub instead to avoid the issue.
        # names(data) <- stringr::str_replace_all(names(data), ' ', '.')
        names(data) <- gsub(' ', '.', names(data))
        # Augment data with already predicted result in the model.
        data$.fitted <- restore_na(x$prediction_test$fit, x$prediction_test$unknown_category_rows_index)
        data$.se.fit <- restore_na(x$prediction_test$se.fit, x$prediction_test$unknown_category_rows_index)
        data
      })
  }
  else {
    broom:::augment.lm(x, ...)
  }
}

#' @export
augment.glm_exploratory <- function(x, data = NULL, newdata = NULL, data_type = "training", ...) {
  if(!is.null(newdata)) {
    # Calling broom:::augment.glm fails with 'NextMethod' called from an anonymous function
    # It seems augment.glm is only calling NextMethod, which is falling back to broom:::augment.lm.
    # So, we are just directly calling augment.lm here.
    broom:::augment.lm(x, data = data, newdata = newdata, ...)
  } else if (!is.null(data)) {
    switch(data_type,
      training = { # Call broom:::augment.lm as is
        broom:::augment.lm(x, data = data, newdata = newdata, ...)
      },
      test = {
        # Augment data with already predicted result in the model.
        data$.fitted <- restore_na(x$prediction_test$fit, x$prediction_test$unknown_category_rows_index)
        data$.se.fit <- restore_na(x$prediction_test$se.fit, x$prediction_test$unknown_category_rows_index)
        data
      })
  }
  else {
    broom:::augment.lm(x, ...)
  }
}

# For some reason, find_data called from inside margins::marginal_effects() fails in Exploratory.
# Explicitly declaring find_data for our glm_exploratory class works it around.
#' @export
find_data.glm_exploratory <- function(model, env = parent.frame(), ...) {
  model$data
}

# Generates Summary table for Analytics View. It can handle Test Mode.
# This is written for linear regression analytics view and GLM analytics views that makes numeric prediction.
evaluate_lm_training_and_test <- function(df, pretty.name = FALSE){
  # Get the summary row for traninng data. Info is retrieved from model by glance()
  training_ret <- df %>% broom::glance(model, pretty.name = pretty.name)
  ret <- training_ret

  grouped_col <- colnames(df)[!colnames(df) %in% c("model", ".test_index", "source.data")]

  # Consider it test mode if any of the element of .test_index column has non-zero length, and generate summary row for test data.
  # Unlike training data, this involves calculating metrics by ourselves from test prediction result.
  if (purrr::some(df$.test_index, function(x){length(x)!=0})) {
    ret$is_test_data <- FALSE # Set is_test_data FALSE for training data. Add is_test_data column only when there are test data too.
    each_func <- function(df){
      # With the way this is called, df becomes list rather than data.frame.
      # Make it data.frame again so that prediction() can be applied on it.
      if (!is.data.frame(df)) {
        df <- tibble::tribble(~model, ~.test_index, ~source.data,
                              df$model, df$.test_index, df$source.data)
      }

      tryCatch({
        test_pred_ret <- prediction(df, data = "test")
        ## get Model Object
        m <- df %>% filter(!is.null(model)) %>% `[[`(1, "model", 1)
        actual_val_col <- all.vars(df$model[[1]]$terms)[[1]]
        # Emulate the way lm replaces the column names in the output.
        # For some reason, str_replace garbles some column names in Japanese. Using gsub instead to avoid the issue.
        # actual_val_col_clean <- stringr::str_replace_all(actual_val_col, ' ', '.')
        actual_val_col_clean <- gsub(' ', '.', actual_val_col)

        actual <- test_pred_ret[[actual_val_col_clean]]
        predicted <- test_pred_ret$predicted_value
        root_mean_square_error <- rmse(actual, predicted)
        test_n <- sum(!is.na(predicted)) # Sample size for test.

        # To calculate R Squared for test data, use same null model basis as training,
        # so that the results are comparable.
        null_model_mean <- mean(df$model[[1]]$model[[actual_val_col]], na.rm=TRUE)

        rsq <- r_squared(actual, predicted, null_model_mean)

        # Calculate Adjusted R Sauared
        # https://en.wikipedia.org/wiki/Coefficient_of_determination
        n_observations <- nrow(df$model[[1]]$model)
        df_residual <- df$model[[1]]$df.residual
        adj_rsq <- 1 - (1 - rsq) * (n_observations - 1) / df_residual

        test_ret <- data.frame(
                          r.squared = rsq,
                          adj.r.squared = adj_rsq,
                          rmse = root_mean_square_error,
                          n = test_n
                          )
        if(pretty.name) {
          test_ret <- test_ret %>% dplyr::rename(`R Squared`=r.squared, `Adj R Squared`=adj.r.squared, `RMSE`=rmse, `Number of Rows`=n)
        }
        test_ret$is_test_data <- TRUE
        test_ret
      }, error = function(e){
        data.frame()
      })
    }

    # df is already grouped rowwise, but to get group column value on the output, we need to group it explicitly with the group column.
    if (length(grouped_col) > 0) {
      df <- df %>% dplyr::group_by(!!!rlang::syms(grouped_col))
    }

    test_ret <- do_on_each_group(df, each_func, with_unnest = TRUE)
    ret <- ret %>% dplyr::bind_rows(test_ret)
  }

  # Reorder columns. Bring group_by column first, and then is_test_data column, if it exists.
  if (!is.null(ret$is_test_data)) {
    if (length(grouped_col) > 0) {
      ret <- ret %>% dplyr::select(!!!rlang::syms(grouped_col), is_test_data, everything())
    }
    else {
      ret <- ret %>% dplyr::select(is_test_data, everything())
    }
  }
  else {
    if (length(grouped_col) > 0) {
      ret <- ret %>% dplyr::select(!!!rlang::syms(grouped_col), everything())
    }
  }

  if (length(grouped_col) > 0){
    ret <- ret %>% dplyr::arrange(!!!rlang::syms(grouped_col))
  }

  # Prettify is_test_data column. Do this after the above select calls, since it looks at is_test_data column.
  if (!is.null(ret$is_test_data) && pretty.name) {
    ret <- ret %>% dplyr::select(is_test_data, everything()) %>%
      dplyr::mutate(is_test_data = dplyr::if_else(is_test_data, "Test", "Training")) %>%
      dplyr::rename(`Data Type` = is_test_data)
  }

  ret
}
