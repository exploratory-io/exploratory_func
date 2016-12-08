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
build_glm <- function(data, formula, ..., keep.source = TRUE, augment = FALSE, group_cols = NULL, test_rate = 0, seed = 0){

  if(!is.null(seed)){
    set.seed(seed)
  }

  # deal with group columns by index because those names might be changed
  group_col_index <- colnames(data) %in% group_cols

  # change column names to avoid name conflict when tidy or glance are executed
  reserved_names <- c(
    "model", ".test_index", "data",
    # for tidy
    "term", "estimate", "std.error", "statistic", "p.value",
    # for glance
    "null.deviance",
    "df.null", "logLik", "AIC", "BIC", "deviance", "df.residual"
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
    data <- dplyr::group_by_(data, .dots =  colnames(data)[group_col_index])
  } else {
    data <- data %>%
      dplyr::mutate(.test_index = 1) %>%
      dplyr::group_by(.test_index)
  }

  model_col <- "model"
  source_col <- "source.data"

  caller <- match.call()
  # this expands dots arguemtns to character
  arg_char <- expand_args(caller, exclude = c("data", "keep.source", "augment", "group_cols", "test_rate", "seed"))

  ret <- tryCatch({
    if(keep.source || augment){
      ret <- data %>%
        tidyr::nest() %>%
        # create test index
        dplyr::mutate(.test_index = purrr::map(data, function(df){
          sample_df_index(df, rate = test_rate)
        })) %>%
        # slice training data
        # use source.data as column name to keep it
        dplyr::mutate(source.data = purrr::map2(data, .test_index, function(df, index){
          safe_slice(df, index, remove = TRUE)
        })) %>%
        # execute glm
        dplyr::mutate(model = purrr::map(source.data, function(data){
          eval(parse(text = paste0("stats::glm(data = data, ", arg_char, ")")))
        })) %>%
        dplyr::select(-data) %>%
        dplyr::rowwise()
      class(ret[[source_col]]) <- c("list", ".source.data")
      ret
    } else {
      ret <- data %>%
        tidyr::nest() %>%
        # create test index
        dplyr::mutate(.test_index = purrr::map(data, function(df){
          sample_df_index(df, rate = test_rate)
        })) %>%
        # slice training data
        # use source.data as column name to keep it
        dplyr::mutate(data = purrr::map2(data, .test_index, function(df, index){
          safe_slice(df, index, remove = TRUE)
        })) %>%
        # execute glm
        dplyr::mutate(model = purrr::map(data, function(data){
          eval(parse(text = paste0("stats::glm(data = data, ", arg_char, ")")))
        })) %>%
        dplyr::select(-data) %>%
        dplyr::rowwise()
      ret
    }
  }, error = function(e){
    if(e$message == "contrasts can be applied only to factors with 2 or more levels"){
      stop("more than 2 unique values are needed for categorical predictor columns")
    }
    stop(e$message)
  })
  if(augment){
    # do.call is used because augment tries to regard "model_col" and "source_col"
    # as column names as non standard evaluation
    ret <- do.call(broom::augment, list(ret, model_col, source_col))
  } else {
    class(ret[[model_col]]) <- c("list", ".model", ".model.glm")
  }
  ret
}
