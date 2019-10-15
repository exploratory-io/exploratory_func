#'
#'

#' integrated build_kmeans
#' @export
build_kmeans <- function(df, ..., skv = NULL, fun.aggregate=mean, fill=0, seed=0){
  validate_empty_data(df)

  if (!is.null(skv)) {
    #.kv pattern
    if (!(length(skv) %in% c(2, 3))) {
      stop("length of skv has to be 2 or 3")
    }
    value <- if(length(skv) == 2) NULL else skv[[3]]
    build_kmeans.kv_(df, skv[[1]], skv[[2]], value, fun.aggregate = fun.aggregate, fill = fill, seed=seed, ...)
  } else {
    #.cols pattern
    build_kmeans.cols(df, seed=seed, ...)
  }
}

#' kmeans wrapper with do with key-value columns as input
#' @param centers Set an integer number to decide how many clusters (groups) to build.
#' @param keep.source It will make .source.data column to preserve source data.
#' @param seed This is random seed. You can change the result if you change this number.
#' @return deta frame which has kmeans model
#' @export
build_kmeans.kv <- function(df, subject, key, value = NULL, ...){

  row_col <- col_name(substitute(subject))
  col_col <- col_name(substitute(key))
  value_col <- if(is.null(substitute(value))) NULL else col_name(substitute(value))

  build_kmeans.kv_(df, row_col, col_col, value_col, ...)
}

#' SE version of build_kmeans.kv
#' @export
build_kmeans.kv_ <- function(df,
                             subject_col,
                             key_col,
                             value_col = NULL,
                             centers=3,
                             iter.max = 10,
                             nstart = 1,
                             algorithm = "Hartigan-Wong",
                             trace = FALSE,
                             normalize_data = TRUE,
                             keep.source = TRUE,
                             seed = 0,
                             augment=TRUE,
                             fun.aggregate=mean,
                             fill=0){
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tidyr")
  loadNamespace("broom")

  if (!is.null(seed)) {
    set.seed(seed)
  }

  row_col <- subject_col
  col_col <- key_col

  grouped_column <- grouped_by(df)
  model_column <- avoid_conflict(grouped_column, "model")
  source_column <- avoid_conflict(grouped_column, "source.data")

  df <- tidyr::drop_na(df, !!rlang::sym(subject_col), !!rlang::sym(key_col), !!rlang::sym(value_col))

  if(row_col %in% grouped_column){
    stop(paste0(row_col, " is a grouping column. ungroup() may be necessary before this operation."))
  }

  build_kmeans_each <- function(df){
    # remove grouping column
    # to avoid it appears as duplicated column
    # after unnesting the result
    df <- df[, !colnames(df) %in% grouped_column]
    mat <- simple_cast(
      df,
      row_col,
      col_col,
      value_col,
      fun.aggregate = fun.aggregate,
      fill=fill,
      na.rm = TRUE
    )
    if (normalize_data) { # Normalize data if specified so.
      mat <- scale(mat)
      # Column with zero variance will be filled with NaN because of division by 0.
      # Replace them with 0.
      mat[is.nan(mat)] <- 0
    }
    rownames(mat) <- NULL # this prevents warning about discarding row names of the matrix
    kmeans_ret <- tryCatch({
      kmeans(mat, centers = centers, iter.max = 10, nstart = nstart, algorithm = algorithm, trace = trace)},
      error = function(e){
        if(e$message == "cannot take a sample larger than the population when 'replace = FALSE'"){
          # falls into here when group is 2 and centers > 2
          stop("Centers should be less than unique subjects.")
        }
        # number of unique values among subjects should be more than centers
        if(e$message == "more cluster centers than distinct data points."){
          stop("Centers should be less than distinct data points.")
        }
        if(e$message == "number of cluster centres must lie between 1 and nrow(x)"){
          stop("Centers should be less than unique subjects.")
        }
        if(e$message == "NA/NaN/Inf in foreign function call (arg 1)"){
          stop("There is NA in the data.")
        }
        stop(e$message)
      }
    )
    if(augment){
      cluster_column <- avoid_conflict(grouped_column, "cluster")
      row_fact <- as.factor(df[[row_col]])
      # modify the result into factor from integer
      df[[cluster_column]] <- factor(kmeans_ret$cluster[row_fact], levels = seq(centers))
      df
    } else {
      # add an attribute to be referred from augment_kmeans
      attr(kmeans_ret, "subject_colname") <- row_col
      kmeans_ret
    }
  }

  if(keep.source & !augment){
    output <- df %>%
      dplyr::do_(.dots=setNames(list(~build_kmeans_each(.), ~(.)), c(model_column, source_column)))
    # Add a class for Exploratyry to recognize the type of .source.data
    class(output[[source_column]]) <- c("list", ".source.data")
  } else {
    output <- df %>%
      dplyr::do_(.dots=setNames(list(~build_kmeans_each(.)), model_column))
  }
  # Add a class for Exploratyry to recognize the type of .model
  if(augment){
    output <- output %>%
      dplyr::ungroup() %>%
      unnest_with_drop(!!rlang::sym(model_column))
  } else {
    class(output[[model_column]]) <- c("list", ".model", ".model.kmeans")
  }
  output
}

#' kmeans wrapper with do with variable columns as input
#' @export
build_kmeans.cols <- function(df, ...,
                            centers=3,
                            iter.max = 10,
                            nstart = 1,
                            algorithm = "Hartigan-Wong",
                            trace = FALSE,
                            normalize_data = TRUE,
                            keep.source = TRUE,
                            seed = 0,
                            augment=TRUE
                            ){
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tidyr")
  loadNamespace("broom")
  if (!is.null(seed)) {
    set.seed(seed)
  }
  select_dots <- lazyeval::lazy_dots(...)

  grouped_column <- grouped_by(df)
  model_column <- avoid_conflict(grouped_column, "model")
  source_column <- avoid_conflict(grouped_column, "source.data")
  # this gets a vector of column names which are selected by dots argument
  selected_column <- evaluate_select(df, .dots=select_dots, grouped_column)

  omit_df <- df[,selected_column] %>%
    as_numeric_matrix_(selected_column) %>%
    na.omit()
  omit_row <- attr(omit_df, "na.action")
  if(!is.null(omit_row)){
    df <- df[setdiff(seq(nrow(df)), omit_row), ]
  }

  build_kmeans_each <- function(df){
    # remove grouping column
    # to avoid it appears as duplicated column
    # after unnesting the result
    df <- df[, !colnames(df) %in% grouped_column]
    kmeans_ret <- tryCatch({
      if(nrow(df) == 0 | ncol(df) == 0){
        stop("No data after removing NA")
      }
      mat <- as_numeric_matrix_(df, columns = selected_column)
      if (normalize_data) { # Normalize data if specified so.
        mat <- scale(mat)
        # Column with zero variance will be filled with NaN because of division by 0.
        # Replace them with 0.
        mat[is.nan(mat)] <- 0
      }
      kmeans(mat, centers = centers, iter.max = 10, nstart = nstart, algorithm = algorithm, trace = trace)
    }, error = function(e) {
      if(e$message == "invalid first argument"){
        stop("Created matrix is invalid")
      }
      if(e$message == "cannot take a sample larger than the population when 'replace = FALSE'"){
        # this matrix falls into here when centers = 3 and mat is
        #        1 2 3 4 5
        # group1 1 5 1 5 1
        # group2 5 1 5 1 5
        stop("Centers should be less than rows.")
      }
      # number of unique values among subjects should be more than centers
      if(e$message == "more cluster centers than distinct data points."){
        stop("Centers should be less than distinct data points.")
      }
      if(e$message == "number of cluster centres must lie between 1 and nrow(x)"){
        stop("Centers should be less than rows.")
      }
      if(e$message == "NA/NaN/Inf in foreign function call (arg 1)"){
        stop("There is NA in the data.")
      }
      stop(e$message)
    })
    if(augment){
      ret <- broom::augment(kmeans_ret, df)
      colnames(ret)[[ncol(ret)]] <- avoid_conflict(grouped_column, "cluster")
      ret
    } else {
      kmeans_ret
    }
  }

  if(keep.source & !augment){
    output <- df %>%
        dplyr::do_(.dots=setNames(list(~build_kmeans_each(.), ~(.)), c(model_column, source_column)))
    # Add a class for Exploratyry to recognize the type of .source.data
    class(output[[source_column]]) <- c("list", ".source.data")
  } else {
    output <- df %>%
      dplyr::do_(.dots=setNames(list(~build_kmeans_each(.)), model_column))
  }
  if(augment){
    output <- output %>%
      dplyr::ungroup() %>%
      unnest_with_drop(!!rlang::sym(model_column))
  } else {
    # Add a class for Exploratyry to recognize the type of .model
    class(output[[model_column]]) <- c("list", ".model", ".model.kmeans")
  }
  output
}
