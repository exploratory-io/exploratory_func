#' glm wrappwer for logistic regression
#' @export
build_lr <- function(df, ...) {
  validate_empty_data(df)
  data <- df
  dots <- list(...)
  # formula is from "formula =" argument or first argument
  formula <- dots$formula
  if(is.null(formula)){
    formula <- dots[[1]]
  }
  response_var <- all.vars(formula)[[1]]

  if(is.character(data[[response_var]])) {
    data[[response_var]] <- as.factor(data[[response_var]])
  }
  if(is.factor(data[[response_var]])){
    if(length(levels(data[[response_var]])) != 2){
      stop("outcome column has to have 2 unique values")
    }
  }
  do.call("build_glm", list(data, ..., family = "binomial"))
}

#' glm wrapper with do
#' @return deta frame which has glm model
#' @param data Data frame to be used as data
#' @param formula Formula for glm
#' @param ... Parameters to be passed to glm function
#' @param keep.source Whether source should be kept in source.data column
#' @param augment Whether the result should be augmented immediately
#' @param group_cols A vector with columns names to be used as group columns
#' @param test_rate Ratio of test data
#' @param seed Random seed to control test data sampling
#' @export
build_glm <- function(data, formula, ..., keep.source = TRUE, augment = FALSE, group_cols = NULL, test_rate = 0, seed = 1){
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
    "null.deviance", "df.null", "logLik", "AIC", "BIC", "deviance",
    "df.residual"
  )

  if(test_rate < 0 | test_rate > 1){
    stop("test_rate has to be between 0 and 1")
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
  }

  group_col_names <- grouped_by(data)

  model_col <- "model"
  source_col <- "source.data"

  caller <- match.call()

  # check if grouping columns are in the formula
  grouped_var <- group_col_names[group_col_names %in% fml_vars]
  if (length(grouped_var) == 1) {
    stop(paste0(grouped_var, " is a grouping column. Please remove it from variables."))
  } else if (length(grouped_var) > 0) {
    stop(paste0(paste(grouped_var, collapse = ", "), " are grouping columns. Please remove them from variables."))
  }

  # define glm function by the family option
  exclude_arg_names <- c("data", "keep.source", "augment", "group_cols", "test_rate", "seed")

  # check family argument.
  # family argument is 1: family object or 2: character or facater
  # the condition of using MASS::glm.nb is below.
  # 1. family argument is not family object. Because when it is family object, it should be MASS::negative.binomial
  #    and use stats::glm(..., family = family)
  # 2. family argument has negative.binomial characters.
  has_any_nb_string <- match.call()$family %>% as.character() %>%
                     stringr::str_detect("negative\\.binomial") %>%
                     any() %>% dplyr::if_else(is.na(.), FALSE, .)
  is_not_family_obj <- class(list(...)$family) != "family"
  if(has_any_nb_string && is_not_family_obj) {
    arg_char <- expand_args(caller, exclude = c(exclude_arg_names, "family", "offset"))
    glm_func_name = "MASS::glm.nb"

  } else {
    arg_char <- expand_args(caller, exclude = exclude_arg_names)
    glm_func_name = "stats::glm"
  }

  ret <- tryCatch({
    ret <- data %>%
      tidyr::nest(source.data=-dplyr::group_cols()) %>%
      # create test index
      dplyr::mutate(.test_index = purrr::map(source.data, function(df){
        sample_df_index(df, rate = test_rate)
      })) %>%
      # slice training data
      dplyr::mutate(model = purrr::map2(source.data, .test_index, function(df, index){
        data <- safe_slice(df, index, remove = TRUE)
        # execute glm with parsed arguments
        eval(parse(text = paste0(glm_func_name, "(data = data, ", arg_char, ")")))
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
    # so use stringr::str_detect to make this robust
    if(stringr::str_detect(e$message, "contrasts can be applied only to factors with 2 or more levels")){
      stop("more than 1 unique values are expected for categorical columns assigned as predictors")
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
    class(ret[[model_col]]) <- c("list", ".model", ".model.glm")
  }
  ret
}
