#'
#'

#' integrated do_cor
#' @export
do_cor <- function(df, ..., skv = NULL, fun.aggregate=mean, fill=0){
  if (!is.null(skv)) {
    #.kv pattern
    if (!(length(skv) %in% c(2, 3))) {
      stop("length of skv has to be 2 or 3")
    }
    value <- if(length(skv) == 2) NULL else skv[[3]]
    do_cor.kv_(df, skv[[1]], skv[[2]], value, fun.aggregate = fun.aggregate, fill = fill, ...)
  } else {
    #.cols pattern
    do_cor.cols(df, ...)
  }
}

#'
#' Calculate correlation among groups and output the correlation of each pair
#' @param df data frame in tidy format
#' @param group A column you want to calculate the correlations for.
#' @param dimension A column you want to use as a dimension to calculate the correlations.
#' @param value A column for the values you want to use to calculate the correlations.
#' @param use Operation type for dealing with missing values. This can be one of "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
#' @param method Method of calculation. This can be one of "pearson", "kendall", or "spearman".
#' @param fun.aggregate  Set an aggregate function when there are multiple entries for the key column per each category.
#' @return correlations between pairs of groups
#' @export
do_cor.kv <- function(df,
                   subject,
                   key,
                   value = NULL,
                   ...)
{
  loadNamespace("reshape2")
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("lazyeval")

  row <- col_name(substitute(key))
  col <- col_name(substitute(subject))
  val <- if(is.null(substitute(value))) NULL else col_name(substitute(value))

  do_cor.kv_(df, col, row, val, ...)
}

#' SE version of do_cor.kv
#' @export
do_cor.kv_ <- function(df,
                      subject_col,
                      key_col,
                      value_col = NULL,
                      use="pairwise.complete.obs",
                      method="pearson",
                      distinct = FALSE,
                      diag = FALSE,
                      fill = 0,
                      fun.aggregate=mean)
{
  loadNamespace("reshape2")
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("lazyeval")

  row <- key_col
  col <- subject_col
  val <- value_col

  grouped_col <- grouped_by(df)
  if(col %in% grouped_col){
    stop(paste0(col, " is a grouping column. ungroup() may be necessary before this operation."))
  }
  # column names are "{subject}.x", "{subject}.y", "value"
  output_cols <- avoid_conflict(grouped_col,
                           c(stringr::str_c(col, c(".x", ".y")),
                             "value")
  )


  do_cor_each <- function(df){
    mat <- simple_cast(df, row, col, val, fun.aggregate=fun.aggregate, fill=fill)
    cor_mat <- cor(mat, use = use, method = method)
    if(distinct){
      ret <- upper_gather(cor_mat, diag=diag, cnames=output_cols)
    } else {
      ret <- mat_to_df(cor_mat, cnames=output_cols, diag=diag)
    }
  }
  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(grouped_col, "tmp")
  df %>%
    dplyr::do_(.dots=setNames(list(~do_cor_each(.)), tmp_col)) %>%
    tidyr::unnest_(tmp_col)
}

#'
#' Calculate correlation among columns and output the correlation of each pair
#' @param df data frame in tidy format
#' @param ... Arguments to select columns to calculate correlation.
#' @param use Operation type for dealing with missing values. This can be one of "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
#' @param method Method of calculation. This can be one of "pearson", "kendall", or "spearman".
#' @return correlations between pairs of columns
#' @export
do_cor.cols <- function(df, ..., use="pairwise.complete.obs", method="pearson", distinct=FALSE, diag=FALSE){
  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tibble")
  # select columns using dplyr::select logic
  select_dots <- lazyeval::lazy_dots(...)
  grouped_col <- grouped_by(df)
  output_cols <- avoid_conflict(grouped_col, c("pair.name.1", "pair.name.2", "value"))
  # check if the df's grouped
  do_cor_each <- function(df){
    mat <- dplyr::select_(df, .dots = select_dots) %>%  as.matrix()
    # sort the column name so that the output of pair.name.1 and pair.name.2 will be sorted
    # it's better to be sorted so that heatmap in exploratory can be triangle if distinct is TRUE
    mat <- mat[,sort(colnames(mat))]

    cor_mat <- cor(mat, use = use, method = method)
    if(distinct){
      ret <- upper_gather(cor_mat, diag=diag, cnames=output_cols)
    } else {
      ret <- mat_to_df(cor_mat, cnames=output_cols,diag=diag)
    }
  }

  (df %>%  dplyr::do_(.dots=setNames(list(~do_cor_each(.)), output_cols[[1]])) %>%  tidyr::unnest_(output_cols[[1]]))
}

#' Calculate svd from tidy format. This can be used to calculate coordinations by reducing dimensionality.
#' @param df Data frame which has group and dimension
#' @param group Column to be regarded as groups
#' @param dimension Column to be regarded as original dimensions
#' @param value Column to be regarded as values
#' @param type "group" to see the coordinations in reduced dimension.
#' "dimension" to see the direction of new axes from original ones.
#' "variance" to see how much the data is distributed in the direction of new axes.
#' @param fill Value to fill where value doesn't exist.
#' @param fun.aggregate Value to fill where value doesn't exist.
#' @param n_component Number of dimensions to return.
#' @return Tidy format of data frame.
#' @export
do_svd.kv <- function(df,
                      subject,
                      key,
                      value = NULL,
                      type="group",
                      fill=0,
                      fun.aggregate=mean,
                      n_component=3,
                      centering=TRUE,
                      output ="long"){
  loadNamespace("dplyr")
  loadNamespace("tibble")
  loadNamespace("tidyr")
  subject_col <- col_name(substitute(subject))
  dimension_col <- col_name(substitute(key))
  value_col <- col_name(substitute(value))

  grouped_col <- grouped_by(df)

  if(subject_col %in% grouped_col){
    stop(paste0(subject_col, " is a grouping column. ungroup() may be necessary before this operation."))
  }


  axis_prefix <- "axis"
  value_cname <- avoid_conflict(colnames(df), "value")

  # this is executed on each group
  do_svd_each <- function(df){
    matrix <-simple_cast(df, subject_col, dimension_col, value_col, fun.aggregate = fun.aggregate, fill=fill)
    if(any(is.na(matrix))){
      stop("NA is not supported as value.")
    }
    if(centering){
      # move the origin to center of data
      matrix <- sweep(matrix, 2, colMeans(matrix), "-")
    }
    if(type=="group"){
      result <- svd(matrix, nu=n_component, nv=0)
      mat <- result$u

      if (output=="wide") {
        ret <- as.data.frame(mat)
        colnames(ret) <- avoid_conflict(c(grouped_col, subject_col), paste("axis", seq(ncol(mat)), sep=""))
        rnames <- same_type(rownames(matrix), df[[subject_col]])
        df <- setNames(data.frame(rnames, stringsAsFactors = FALSE), subject_col)
        ret <- cbind(df, ret)
      } else if (output=="long") {
        cnames <- avoid_conflict(grouped_col, c(subject_col, "new.dimension", value_cname))
        rownames(mat) <- rownames(matrix)
        ret <- mat_to_df(mat, cnames)
      } else {
        stop(paste(output, "is not supported as output"))
      }
    } else if (type=="dimension") {

      result <- svd(matrix, nv=n_component, nu=0)
      mat <- result$v
      rownames(mat) <- colnames(matrix)

      if (output=="wide") {
        ret <- as.data.frame(mat)
        colnames(ret) <- avoid_conflict(c(grouped_col, dimension_col), paste("axis", seq(ncol(mat)), sep=""))
        rnames <- same_type(rownames(mat), df[[subject_col]])
        df <- setNames(data.frame(rnames, stringsAsFactors = FALSE), dimension_col)
        ret <- cbind(df, ret)
      } else if (output=="long") {
        cnames <- avoid_conflict(grouped_col, c(dimension_col, "new.dimension", value_cname))
        ret <- mat_to_df(mat, cnames)
      } else {
        stop(paste(output, "is not supported as output"))
      }
    } else if (type=="variance"){
      variance <- svd(matrix, nu=0, nv=0)$d
      component <- seq(min(length(variance), n_component))
      if (output=="wide") {
        mat <- matrix(variance[component], ncol=length(component))
        ret <- as.data.frame(mat)
        colnames(ret) <- avoid_conflict(c(subject_col), paste("axis", seq(ncol(mat)), sep=""))
      } else if (output=="long") {
        ret <- data.frame(component = component, svd.value = variance[component])
        colnames(ret) <- avoid_conflict(subject_col, c("new.dimension", value_cname))
      } else {
        stop(paste(output, "is not supported as output"))
      }
    } else {
      stop(paste(type, "is not supported as type argument."))
    }
    ret
  }

  (df %>%  dplyr::do_(.dots=setNames(list(~do_svd_each(.)), value_cname)) %>%  tidyr::unnest_(value_cname))
}

#' Non standard evaluation version for do_cmdscale_
#' @return Tidy format of data frame.
#' @export
do_cmdscale <- function(df,
                         pair_name1,
                         pair_name2,
                         value,
                         ...){
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  pair1_col <- col_name(substitute(pair_name1))
  pair2_col <- col_name(substitute(pair_name2))
  value_col <- col_name(substitute(value))
  do_cmdscale_(df, pair1_col, pair2_col, value_col, ...)
}

#' Map dist result to k dimensions
#' @param df Data frame which has group and dimension
#' @return Tidy format of data frame.
#' @export
do_cmdscale_ <- function(df,
                         pair1_col,
                         pair2_col,
                         value_col,
                         k=2,
                         fun.aggregate=mean,
                         fill=0){
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  grouped_col <- grouped_by(df)

  name_col <- avoid_conflict(grouped_col, "name")

  # this is executed on each group
  do_cmdscale_each <- function(df){
    mat <- simple_cast(df, pair1_col, pair2_col, value_col, fun.aggregate = fun.aggregate, fill=fill)
    cnames <- colnames(mat)
    rnames <- rownames(mat)
    if(any(cnames != rnames)){
      diffcol <- setdiff(rnames, cnames)
      diffrow <- setdiff(cnames, rnames)
      if(!(length(diffcol)==1 & length(diffrow)==1)){
        stop(paste("Can't create dist matrix from ", pair1_col, " and ", pair2_col), collapse=" ")
      } else {
        # Create diagonal elements to be recognized as dist matrix
        mat <- cbind(matrix(0, nrow=nrow(mat), ncol=1, dimnames = list(NULL, diffcol)), mat)
        mat <- rbind(mat, matrix(0, nrow=1, ncol=ncol(mat), dimnames = list(diffrow, NULL)))
      }
    }
    points <- cmdscale(as.dist(t(mat)), eig=FALSE, k=k)
    result_df <- as.data.frame(points)

    df <- setNames(data.frame(rownames(points), stringsAsFactors=FALSE), name_col)
    ret <- cbind(df, result_df)
    ret
  }

  # Calculation is executed in each group.
  # Storing the result in this name_col and
  # unnesting the result.
  # name_col is not conflicting with grouping columns
  # thanks to avoid_conflict that is used before,
  # so this won't overwrite grouping columns.
  df %>%
    dplyr::do_(.dots=setNames(list(~do_cmdscale_each(.)), name_col)) %>%
    tidyr::unnest_(name_col)
}
