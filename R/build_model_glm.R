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
build_model_glm <- function(data, formula, ..., output = "model"){

  grouped_column <- grouped_by(data)

  # get argument named list (-1 means removing this function name)
  cl <- as.list(match.call())[-1]
  # keep just ... argument
  nodots <- as.list(match.call(expand.dots = FALSE))[-1]
  cl[names(nodots)] <- NULL
  # put only formula
  cl$formula <- formula
  modelset_classname <- "glm_modelset"
  build_glm_each <- function(df){
    cl$data <- df
    model <- do.call(glm, cl)
    ret <- list(
      model = model,
      source = df
    )
    class(ret) <- c(modelset_classname, "modelset")
    ret
  }

  columnname <- avoid_conflict(grouped_column, "model")

  ret <- tryCatch({
      data %>% dplyr::do_(.dots = setNames(list(~build_glm_each(.)), columnname))
    }, error = function(e){
    if(e$message == "contrasts can be applied only to factors with 2 or more levels"){
      stop("more than 2 unique values are needed for categorical predictor columns")
    }
    stop(e$message)
  })

  # do.call is used to use a variable in non standard evaluation
  ret <- if(output == "augment"){
    do.call(augment, list(ret, columnname))
  } else if (output == "tidy") {
    do.call(tidy, list(ret, columnname))
  } else if (output == "glance") {
    do.call(glance, list(ret, columnname))
  } else if (output == columnname) {
    class(ret[["model"]]) <- c("list", "modelset")
    ret
  } else {
    do.call(tidy, list(ret, columnname, matrix = output))
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
