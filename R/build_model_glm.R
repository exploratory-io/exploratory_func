#' glm wrapper with do
#' @param data Data frame to evaluate in glm
#' @param formula Variables to use for glm function
#' @param ... Arguments to pass to glm
#' @param output Format of output to use. One of "augment", "tidy", "glance", "anova" and "model"
#' @param cache_key Cache id for modelset.
#' If you set "tam.modelset_cache" to a directory path, this will create cache
#' and reuse if you indicate same cache_key
#' @return deta frame which has glm model
#' @export
build_model_glm <- function(data, formula, ..., output = "model", cache_key = NULL){
  # get argument named list (-1 means removing this function name)
  cl <- as.list(match.call())[-1]
  # keep just ... argument
  nodots <- as.list(match.call(expand.dots = FALSE))[-1]
  cl[names(nodots)] <- NULL
  # put only formula
  cl$formula <- formula
  # this is used for cache index in each group
  group_index <- 1
  modelset_classname <- "glm_modelset"
  build_glm_each <- function(df){
    cl$data <- df
    ret <- NULL
    if(!is.null(cache_key)){
      ret <- getModelSetFromRDS(modelset_classname, cache_key, group_index)
      group_index <- group_index + 1
    }
    if(is.null(ret)){
      model <- do.call(glm, cl)
      ret <- list(
        model = model,
        source = df
      )
      class(ret) <- c(modelset_classname, "modelset")
      saveModelSetToRDS(ret, modelset_classname, cache_key, group_index)
    }
    ret
  }

  ret <- tryCatch({
      data %>% dplyr::do_(.dots = setNames(list(~build_glm_each(.)), "model"))
    }, error = function(e){
    if(e$message == "contrasts can be applied only to factors with 2 or more levels"){
      stop("more than 2 unique values are needed for categorical predictor columns")
    }
    stop(e$message)
  })

  ret <- if(output == "augment"){
    ret <- augment(ret, "model")
  } else if (output == "tidy") {
    ret <- tidy(ret, "model")
  } else if (output == "glance") {
    glance(ret, "model")
  } else if (output == "model") {
    class(ret[["model"]]) <- c("list", ".model")
    ret
  } else {
    tidy(ret, "model", matrix = output)
  }
  ret
}

#' @export
tidy.glm_modelset <- function(modelset, matrix = "", groupers = c() ){
  if(matrix == "anova"){
    model <- anova(modelset$model)
    tidy(model)
  } else {
    tidy.modelset(modelset)
  }
}
