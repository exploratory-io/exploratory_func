#'
#'

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
                   value,
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

  row <- col_name(substitute(key))
  col <- col_name(substitute(subject))
  val <- col_name(substitute(value))

  grouped_col <- grouped_by(df)
  output_cols <- avoid_conflict(grouped_col, c("pair.name.1", "pair.name.2", "cor.value"))

  do_cor_each <- function(df){
    mat <- simple_cast(df, row, col, val, fun.aggregate=fun.aggregate, fill=fill)
    cor_mat <- cor(mat, use = use, method = method)
    if(distinct){
      ret <- upper_gather(cor_mat, diag=diag, cnames=output_cols)
    } else {
      ret <- mat_to_df(cor_mat, cnames=output_cols, diag=diag)
    }
  }

  (df %>%  dplyr::do_(.dots=setNames(list(~do_cor_each(.)), output_cols[[1]])) %>%  tidyr::unnest_(output_cols[[1]]))
}

#'
#' Calculate correlation among columns and output the correlation of each pair
#' @param df data frame in tidy format
#' @param ... Arguments to select columns to calculate correlation.
#' @param use Operation type for dealing with missing values. This can be one of "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
#' @param method Method of calculation. This can be one of "pearson", "kendall", or "spearman".
#' @return correlations between pairs of columns
#' @export
do_cor.variables <- function(df, ..., use="pairwise.complete.obs", method="pearson", distinct=FALSE, diag=FALSE){
  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tibble")
  # select columns using dplyr::select logic
  select_dots <- lazyeval::lazy_dots(...)
  grouped_col <- grouped_by(df)
  output_cols <- avoid_conflict(grouped_col, c("pair.name.1", "pair.name.2", "cor.value"))
  # check if the df's grouped
  do_cor_each <- function(df){
    mat <- dplyr::select_(df, .dots = select_dots) %>%  as.matrix()
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
                   value,
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
  axis_prefix <- "axis"
  value_cname <- avoid_conflict(colnames(df), "svd.value")

  # this is executed on each group
  do_svd_each <- function(df){
    matrix <-simple_cast(df, subject_col, dimension_col, value_col, fun.aggregate = fun.aggregate, fill=fill)
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
