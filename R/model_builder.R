#'
#'

#' Create do wrapper function with source data
#' @return do wrapper function
build_data <- function(funcname) {
  ret <- function(df, ..., keep.source = FALSE, augment=FALSE){
    loadNamespace("dplyr")
    grouped_column <- grouped_by(df)
    model_column <- avoid_conflict(grouped_column, "model")
    source_column <- avoid_conflict(grouped_column, "source.data")
    if (keep.source | augment) {
      output <- df  %>%  dplyr::do_(.dots=setNames(list(~do.call(funcname, list(data = ., ...)), ~(.)), c(model_column, source_column)))
      # Add a class for Exploratyry to recognize the type of .source.data
      class(output[[source_column]]) <- c("list", ".source.data")
    } else {
      output <- df  %>%  dplyr::do_(.dots=setNames(list(~do.call(funcname, list(data = ., ...))), model_column))
    }

    if(augment){
      # use do.call for non standard evaluation
      augment_func <- get("augment", asNamespace("broom"))
      output <- do.call(augment_func, list(output, model_column, data=source_column))
    } else {
      # Add a class for Exploratyry to recognize the type of .model
      class(output[[model_column]]) <- c("list", ".model", paste0(".model.", funcname))
      output
    }
    output
  }
  ret
}

#' lm wrapper with do
#' @return deta frame which has lm model
#' @export
build_lm <- build_data("lm")

#' glm wrapper with do
#' @return deta frame which has glm model
#' @export
build_glm <- build_data("glm")

#' t.test wrapper with do
#' @return deta frame which has t.test model
#' @export
build_t.test <- build_data("t.test")

#' var.test wrapper with do
#' @return deta frame which has var.test model
#' @export
build_var.test <- build_data("var.test")

#' kmeans wrapper with do with key-value columns as input
#' @param centers Set an integer number to decide how many clusters (groups) to build.
#' @param keep.source It will make .source.data column to preserve source data.
#' @param seed This is random seed. You can change the result if you change this number.
#' @return deta frame which has kmeans model
#' @export
build_kmeans.kv <- function(df,
                            subject,
                            key,
                            value,
                            centers=3,
                            iter.max = 10,
                            nstart = 1,
                            algorithm = "Hartigan-Wong",
                            trace = FALSE,
                            keep.source = FALSE,
                            seed=0,
                            augment=FALSE,
                            fun.aggregate=mean,
                            fill=0){
  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tidyr")
  loadNamespace("broom")
  set.seed(seed)

  row_col <- col_name(substitute(subject))
  col_col <- col_name(substitute(key))
  value_col <- col_name(substitute(value))

  build_kmeans_each <- function(df){
    mat <- simple_cast(df, row_col, col_col, value_col, fun.aggregate = fun.aggregate, fill=fill)
    kmeans_ret <- kmeans(mat, centers = centers, iter.max = 10, nstart = nstart, algorithm = algorithm, trace = trace)
    if(augment){
      if(length(kmeans_ret$cluster) == nrow(df)){
        broom::augment(kmeans_ret, df)
      } else {
        broom::augment(kmeans_ret, setNames(data.frame(rownames(mat),stringsAsFactors = F), row_col))
      }
    } else {
      kmeans_ret
    }
  }
  grouped_column <- grouped_by(df)
  model_column <- avoid_conflict(grouped_column, "model")
  source_column <- avoid_conflict(grouped_column, "source.data")

  if(keep.source & !augment){
    output <- (
      df
      %>%  dplyr::do_(.dots=setNames(list(~build_kmeans_each(.), ~(.)), c(model_column, source_column)))
    )
    # Add a class for Exploratyry to recognize the type of .source.data
    class(output[[source_column]]) <- c("list", ".source.data")
  } else {
    output <- (
      df
      %>%  dplyr::do_(.dots=setNames(list(~build_kmeans_each(.)), model_column))
    )
  }
  # Add a class for Exploratyry to recognize the type of .model
  if(augment){
    output <- tidyr::unnest_(output, model_column)
  } else {
    class(output[[model_column]]) <- c("list", ".model", ".model.kmeans")
  }
  output
}

#' kmeans wrapper with do with variable columns as input
#' @export
build_kmeans.variables <- function(df, ...,
                            centers=3,
                            iter.max = 10,
                            nstart = 1,
                            algorithm = "Hartigan-Wong",
                            trace = FALSE,
                            keep.source = FALSE,
                            seed=0,
                            augment=FALSE){
  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tidyr")
  loadNamespace("broom")
  set.seed(seed)

  select_dots <- lazyeval::lazy_dots(...)
  grouped_column <- grouped_by(df)
  model_column <- avoid_conflict(grouped_column, "model")
  source_column <- avoid_conflict(grouped_column, "source.data")
  selected_column <- setdiff(colnames(dplyr::select_(df, .dots=select_dots)), grouped_column)

  omit_df <- na.omit(df[,selected_column])
  omit_row <- attr(omit_df, "na.action")
  if(!is.null(omit_row)){
    df <- df[setdiff(seq(nrow(df)), omit_row), ]
  }

  build_kmeans_each <- function(df){
    mat <- dplyr::select_(df, .dots=select_dots) %>% as.matrix()
    kmeans_ret <- kmeans(mat, centers = centers, iter.max = 10, nstart = nstart, algorithm = algorithm, trace = trace)
    if(augment){
      broom::augment(kmeans_ret, df)
    } else {
      kmeans_ret
    }
  }

  if(keep.source & !augment){
    output <- (
      df
      %>%  dplyr::do_(.dots=setNames(list(~build_kmeans_each(.), ~(.)), c(model_column, source_column)))
    )
    # Add a class for Exploratyry to recognize the type of .source.data
    class(output[[source_column]]) <- c("list", ".source.data")
  } else {
    output <- (
      df
      %>%  dplyr::do_(.dots=setNames(list(~build_kmeans_each(.)), model_column))
    )
  }
  if(augment){
    output <- tidyr::unnest_(output, model_column)
  } else {
    # Add a class for Exploratyry to recognize the type of .model
    class(output[[model_column]]) <- c("list", ".model", ".model.kmeans")
  }
  output
}
