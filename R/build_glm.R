#' glm wrapper with do
#' @return deta frame which has glm model
#' @export
build_glm <- function(data, formula, ..., output = "model"){
  # get argument named list (-1 means removing this function name)
  cl <- as.list(match.call())[-1]
  # keep just ... argument
  nodots <- as.list(match.call(expand.dots = FALSE))[-1]
  cl[names(nodots)] <- NULL
  # put only formula
  cl$formula <- formula
  build_glm_each <- function(df){
    cl$data <- df
    model <- do.call(glm, cl)
    ret <- list(
      model = model,
      source = df
    )
    class(ret) <- c("glm_modelset", "modelset")
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
