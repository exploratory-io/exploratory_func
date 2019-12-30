#' @rdname build_model_
#' @export
build_model <- function(data, model_func, seed = 1, test_rate = 0, group_cols = c(), reserved_colnames = c(), ...) {
  .dots <- lazyeval::dots_capture(...)
  build_model_(
    data = data,
    model_func = model_func,
    seed = seed,
    test_rate = test_rate,
    group_cols = group_cols,
    reserved_colnames = reserved_colnames,
    .dots = .dots)
}

#' Generic function for model functions
#' @param data Data frame for model function
#' @param model_func Function to create a model object
#' @param test_seed Random seed for train test split
#' @param test_rate Rate for test data
#' @param group_cols Column names for grouping
#' @param reserved_colnames Column names that should be avoided for information extraction like tidy, glance later
#' @param .dots Additional parameters to work around error of non standard evaluation.
#' @param ... Parameters for model_func
#' @export
build_model_ <- function(data, model_func, seed = 1, test_rate = 0, group_cols = c(), reserved_colnames = c(), .dots, ...) {
  validate_empty_data(data)

  if(!is.null(seed)){
    set.seed(seed)
  }

  # deal with group columns by index because those names might be changed
  group_col_index <- colnames(data) %in% group_cols

  # change column names to avoid name conflict when tidy or glance are executed
  reserved_names <- c(
    "model", ".test_index", "data", ".model_metadata",
    reserved_colnames
  )

  if(test_rate < 0 | test_rate > 1){
    stop("test_rate has to be between 0 and 1")
  }

  # use this for preprocess
  # and keep original data unchanged for later use
  # like getting column names from formula
  processed <- data

  # avoid name conflict of grouping columns
  colnames(processed)[group_col_index] <- avoid_conflict(
    reserved_names,
    colnames(processed)[group_col_index],
    ".group"
  )

  # make column names unique
  colnames(processed) <- make.unique(colnames(processed), sep = "")

  if(!is.null(group_cols)){
    processed <- dplyr::group_by(processed, !!!rlang::syms(colnames(processed)[group_col_index]))
  }

  group_col_names <- grouped_by(processed)

  # integrate all additional parameters
  dots <- lazyeval::all_dots(.dots, ...)

  # check if variables in grouped_col_names are not used
  formula <- dots$formula
  if(!is.null(formula)){
    # use terms function with data frame
    # to expand dot in formula with column names of the data
    # so that factorization can work on those columns too
    without_group <- data[, !colnames(data) %in% group_col_names]
    vars <- all.vars(terms(lazyeval::lazy_eval(dots$formula), data = without_group))
    if(any(vars %in% group_col_names)){
      grouped <- vars[vars %in% group_col_names]
      message <- paste("grouped column is used (", paste0(grouped, collapse = ", "), ")", sep = "")
      stop(message)
    } else {
      for (var in vars) {
        if(is.character(processed[[var]])){
          # make variables factor sorted by the frequency
          processed[[var]] <- forcats::fct_infreq(processed[[var]])
        }
      }
    }
  }
  model_col <- "model"
  source_col <- "source.data"

  ret <- tryCatch({
    ret <- processed %>%
      tidyr::nest(source.data=-dplyr::group_cols()) %>%
      # create test index
      dplyr::mutate(.test_index = purrr::map(source.data, function(df){
        sample_df_index(df, rate = test_rate)
      })) %>%
      # slice training data
      dplyr::mutate(model = purrr::map2(source.data, .test_index, function(df, index){
        tmp_df <- safe_slice(df, index, remove = TRUE)
        # execute model_func with parsed arguments
        eval_arg <- dots
        eval_arg[["data"]] <- lazyeval::lazy(tmp_df)
        .call <- lazyeval::make_call(quote(model_func), eval_arg)
        lazyeval::lazy_eval(.call, data = environment())
      })) %>%
      # .model_metadata is a list column to store data
      # to preserve information about the model which is
      # used later.
      # For example, column type validation of new data before prediction.
      dplyr::mutate(.model_metadata = purrr::map2(source.data, model, function(df, model){
        if(!is.null(formula)){
          create_model_meta(df, formula$expr)
        } else {
          list()
        }
      }))
    class(ret[[source_col]]) <- c("list", ".source.data")
    ret <- dplyr::rowwise(ret)
    ret
  }, error = function(e){
    stop(e$message)
  })
  ret
}

