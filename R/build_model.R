#' Generic function for model functions
#' @param data Data frame for model function
#' @param model_func Function to create a model object
#' @param test_seed Random seed for train test split
#' @param test_rate Rate for test data
#' @param group_cols Column names for grouping
#' @param reserved_colnames Column names that should be avoided for information extraction like tidy, glance later
#' @param ... Parameters for model_func
#' @export
build_model <- function(data, model_func, test_seed = 1, test_rate = 0, group_cols = c(), reserved_colnames = c(), ...) {

  if(!is.null(test_seed)){
    set.seed(test_seed)
  }

  # deal with group columns by index because those names might be changed
  group_col_index <- colnames(data) %in% group_cols

  # change column names to avoid name conflict when tidy or glance are executed
  reserved_names <- c(
    "model", ".test_index", "data",
    reserved_colnames
  )

  if(test_rate < 0 | test_rate > 1){
    stop("test_rate has to be between 0 and 1")
  }

  # avoid name conflict of grouping columns
  colnames(data)[group_col_index] <- avoid_conflict(
    reserved_names,
    colnames(data)[group_col_index],
    ".group"
  )

  # make column names unique
  colnames(data) <- make.unique(colnames(data), sep = "")

  if(!is.null(group_cols)){
    data <- dplyr::group_by_(data, .dots = colnames(data)[group_col_index])
  } else if (!dplyr::is.grouped_df(data)){
    # need to be grouped to nest
    data <- data %>%
      dplyr::mutate(.test_index = 1) %>%
      dplyr::group_by(.test_index)
  }

  group_col_names <- grouped_by(data)

  # check if variables in grouped_col_names are not used
  dots <- list(...)
  if(!is.null(dots)){
    fml <- dots$formula
    if(!is.null(fml)){
      vars <- all.vars(fml)
      if(any(vars %in% group_col_names)){
        grouped <- vars[vars %in% group_col_names]
        message <- paste("grouped column is used (", paste0(grouped, collapse = ", "), ")", sep = "")
        stop(message)
      } else {
        for (var in vars) {
          if(is.character(data[[var]])){
            # make variables factor sorted by the frequency
            data[[var]] <- forcats::fct_infreq(data[[var]])
          }
        }
      }
    }
  }

  model_col <- "model"
  source_col <- "source.data"

  caller <- match.call()

  # somehow, formula in caller becomes "..1" sometimes, so update it by list(...)
  if ("formula" %in% names(caller)) {
    caller$formula <- list(...)[["formula"]]
  }

  # this expands dots arguemtns to character
  arg_char <- expand_args(caller, exclude = c(
    "data",
    "model_func",
    "test_seed",
    "test_rate",
    "group_cols",
    "reserved_colnames"
    ))

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
        # execute model_func with parsed arguments
        eval(parse(text = paste0("model_func(data = data, ", arg_char, ")")))
      }))
    class(ret[[source_col]]) <- c("list", ".source.data")
    ret <- dplyr::rowwise(ret)
    ret
  }, error = function(e){
    stop(e$message)
  })
  ret
}
