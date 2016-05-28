#'
#'

#' Create do wrapper function with source data
#' @return do wrapper function
do_data <- function(funcname) {
  ret <- function(df, ..., keep.source = FALSE){
    loadNamespace("dplyr")
    if (keep.source) {
      output <- df  %>%  dplyr::do(.model = do.call(funcname, list(data = ., ...)), .source.data = (.))
      # Add a class for Exploratyry to recognize the type of .source.data
      class(output$.source.data) <- c("list", ".source.data")
    } else {
      output <- df  %>%  dplyr::do(.model= do.call(funcname, list(data = ., ...)))
    }
    # Add a class for Exploratyry to recognize the type of .model
    class(output$.model) <- c("list", ".model", paste0(".model.", funcname))
    output
  }
  ret
}

#' lm wrapper with do
#' @export
do_lm <- do_data("lm")

#' glm wrapper with do
#' @export
do_glm <- do_data("glm")

#' kmeans wrapper with do
#' @export
do_kmeans <- function(df, ..., centers=3, keep.source = FALSE, seed=0){
  loadNamespace("dplyr")
  set.seed(seed)
  labels <- attr(df, "labels")
  if(is.null(labels)){
    grouped_cname = NULL
  } else {
    grouped_cname = colnames(labels)
  }
  tryCatch({
    selected_df <- dplyr::select(df, ...)
    # check if dots are all about column selection
    if(!all(colnames(selected_df) %in% colnames(df))){
      # columns which were not in original data frame
      no_column <- colnames(selected_df)[!colnames(selected_df) %in% colnames(df)]
      stop(paste(no_column, "is undefined in the data frame and argument", collapse = " "))
    }
  }, error=function(e){
    if(e$message=="undefined columns selected"){
      # in case dplyr::select emits error
      stop("There is invalid column name or argument")
    } else {
      stop(e$message)
    }
  })
  selected_cnames <- colnames(selected_df)
  selected_cnames <- selected_cnames[!selected_cnames %in% grouped_cname]
  # expression to find NA row (ex. is.na(df[[\"vec1\"]] ) | is.na(df[[\"vec2\"]] ))
  exp = paste(paste("is.na(df[[\"",selected_cnames, collapse="\"]] ) | ", sep=""), "\"]])", sep="")
  na_row = eval(parse(text=exp))
  if(all(na_row)){
    # All rows has at least one NA. If we filter them out, no row is left.
    stop("No data left after filtering rows that have NA")
  }

  if(keep.source){
    output <- (
      df[!na_row,]
      %>%  dplyr::do(.model= kmeans(.[,selected_cnames], centers), .source.data=(.))
    )
    # Add a class for Exploratyry to recognize the type of .source.data
    class(output$.source.data) <- c("list", ".source.data")
  } else {
    output <- (
      df[!na_row,]
      %>%  dplyr::do(.model= kmeans(.[,selected_cnames], centers))
    )
  }
  # Add a class for Exploratyry to recognize the type of .model
  class(output$.model) <- c("list", ".model", ".model.kmeans")
  output
}

# Compress dimension
compress_dimension <- function(tbl, group, dimension, value, type="group", fill=0, fun.aggregate=mean){
  loadNamespace("reshape2")
  group_col <- col_name(substitute(group))
  dimension_col <- col_name(substitute(dimension))
  value_col <- col_name(substitute(value))
  fml <- as.formula(paste(group_col, dimension_col, sep="~"))
  matrix <- reshape2::acast(tbl, fml, value.var=value_col, fill=fill, fun.aggregate=fun.aggregate)
  model <- prcomp(matrix)
  if(type=="group"){
    mat <- model$x
    colnames(mat) <- NULL
    result <- reshape2::melt(mat)
    colnames(result) <- c("group", "component", "value")
  } else if (type=="dimension") {
    mat <- model$rotation
    colnames(mat) <- NULL
    result <- reshape2::melt(mat)
    colnames(result) <- c("dimension", "component", "value")
  }
  sdev <- rep(model$sdev, each=nrow(result)/length(model$sdev))
  result$stdev <- sdev
  result
}
