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
    data <- dplyr::group_by_(data, .dots =  colnames(data)[group_col_index])
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
#' @export
build_lm.fast <- function(df,
                    target,
                    ...,
                    family = NULL,
                    max_nrow = 200000,
                    predictor_n = 12, # so that at least months can fit in it.
                    seed = 0
                    ){
  # TODO: add test
  # TODO: cleanup code only aplicable to randomForest. this func was started from copy of calc_feature_imp, and still adjusting for lm. 

  # this seems to be the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  target_col <- dplyr::select_var(names(df), !! rlang::enquo(target))
  # this evaluates select arguments like starts_with
  selected_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))

  grouped_cols <- grouped_by(df)

  # remove grouped col or target col
  selected_cols <- setdiff(selected_cols, c(grouped_cols, target_col))

  if (any(c(target_col, selected_cols) %in% grouped_cols)) {
    stop("grouping column is used as variable columns")
  }

  if(!is.null(seed)){
    set.seed(seed)
  }

  # cols will be filtered to remove invalid columns
  cols <- selected_cols

  # randomForest fails if columns are not clean. TODO is this needed?
  #clean_df <- janitor::clean_names(df)
  clean_df <- df # turn off clean_names for lm
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
      df <- df %>%
        # dplyr::filter(!is.na(!!target_col))  TODO: this was not filtering, and replaced it with the next line. check other similar places.
        # for numeric cols, filter NA rows, because lm will anyway do this internally, and errors out
        # if the remaining rows are with single value in any predictor column.
        # filter Inf/-Inf too to avoid error at lm.
        dplyr::filter(!is.na(df[[target_col]]) & !is.infinite(df[[target_col]])) # this form does not handle group_by. so moved into each_func from outside.

      # sample the data because randomForest takes long time
      # if data size is too large
      if (nrow(df) > max_nrow) {
        df <- df %>%
          dplyr::sample_n(max_nrow)
      }

      c_cols <- clean_cols
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
          # turn it into character since if it is factor, the name of term is broken
          df[[wday_col]] <- as.character(lubridate::wday(df[[col]], label=TRUE))
          df[[day_col]] <- lubridate::day(df[[col]])
          df[[yday_col]] <- lubridate::yday(df[[col]])
          # turn it into character since if it is factor, the name of term is broken
          df[[month_col]] <- as.character(lubridate::month(df[[col]], label=TRUE))
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
        } else if(!is.numeric(df[[col]])) {
          # convert data to factor if predictors are not numeric or logical
          # and limit the number of levels in factor by fct_lump.
          # we use ties.method to handle the case where there are many unique values. (without it, they all survive fct_lump.)
          # TODO: see if ties.method would make sense for calc_feature_imp.
          # also, turn NA into (Missing) factor level so that lm will not drop all the rows.
          df[[col]] <- forcats::fct_explicit_na(forcats::fct_lump(as.factor(df[[col]]), n=predictor_n, ties.method="first"))
        } else {
          # for numeric cols, filter NA rows, because lm will anyway do this internally, and errors out
          # if the remaining rows are with single value in any predictor column.
          # filter Inf/-Inf too to avoid error at lm.
          df <- df %>% dplyr::filter(!is.na(df[[col]]) & !is.infinite(df[[col]]))
        }
      }

      # remove columns with only one unique value
      cols_copy <- c_cols
      for (col in cols_copy) {
        unique_val <- unique(df[[col]])
        if (length(unique_val[!is.na(unique_val)]) == 1) {
          c_cols <- setdiff(c_cols, col)
        }
      }

      # build formula for lm
      rhs <- paste0("`", c_cols, "`", collapse = " + ")
      # TODO: This clean_target_col is actually not a cleaned column name since we want lm to show real name. Clean up our variable name.
      fml <- as.formula(paste0("`", clean_target_col, "` ~ ", rhs))
      if (is.null(family)) {
        rf <- stats::lm(fml, data = df) 
      }
      else {
        rf <- stats::glm(fml, data = df, family = family) 
      }
      # these attributes are used in tidy of randomForest TODO: is this good for lm too?
      rf$terms_mapping <- names(name_map)
      names(rf$terms_mapping) <- name_map
      # add special lm_exploratory class for adding extra info at glance().
      class(rf) <- c("lm_exploratory", class(rf))
      rf
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

  do_on_each_group(clean_df, each_func, name = "model", with_unnest = FALSE)
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
  if(pretty.name) {
    ret <- ret %>% dplyr::rename(`R Squared`=r.squared, `Adj R Squared`=adj.r.squared, `Root Mean Square Error`=sigma, `F Ratio`=statistic, `P Value`=p.value, DF=df, `Log Likelihood`=logLik, Deviance=deviance, `Residual DF`=df.residual)
  }
  ret
}
