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
calc_cor <- function(df,
                     group,
                     dimension,
                     value,
                     use="pairwise.complete.obs",
                     method="pearson",
                     distinct = FALSE,
                     diag = FALSE,
                     fun.aggregate=mean)
{
  loadNamespace("reshape2")
  loadNamespace("dplyr")
  row <- col_name(substitute(dimension))
  col <- col_name(substitute(group))
  val <- col_name(substitute(value))
  mat <- simple_cast(df, row, col, val, fun.aggregate=fun.aggregate, fill=NA_real_)

  cor_mat <- cor(mat, use = use, method = method)

  if(distinct){
    gathered <- upper_gather(cor_mat, diag=diag)
  }else{
    gathered <-  reshape2::melt(cor_mat)
    if(!diag){
      gathered <- dplyr::filter(gathered, Var1!=Var2)
    }
  }
  colnames(gathered) <- c("pair.name.1", "pair.name.2", "cor.value")
  if(is.factor(gathered[,1])){
    # if names are facters, it should be converted to character
    gathered[,1] <- as.character(gathered[,1])
    gathered[,2] <- as.character(gathered[,2])
  }
  dplyr::arrange(gathered, pair.name.1)
}

#'
#' Calculate correlation among columns and output the correlation of each pair
#' @param df data frame in tidy format
#' @param ... Arguments to select columns to calculate correlation.
#' @param use Operation type for dealing with missing values. This can be one of "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
#' @param method Method of calculation. This can be one of "pearson", "kendall", or "spearman".
#' @return correlations between pairs of columns
#' @export
calc_cor_var <- function(df, ..., use="pairwise.complete.obs", method="pearson"){
  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tibble")
  # select columns using dplyr::select logic
  selected_df <- dplyr::select_(df, .dots = lazyeval::lazy_dots(...))
  # check if the df's grouped
  indices <- attr(selected_df, "indices")
  labels <- attr(selected_df, "labels")
  if(is.null(indices)){
    # not grouped case
    indices <- list(seq(nrow(df))-1)
    calc_df <- selected_df
    label_df <- NULL
  } else {
    # grouped case: split data column and label column
    calc_df <- selected_df[,!colnames(selected_df) %in% colnames(labels)]
    label_df <- selected_df[,colnames(selected_df) %in% colnames(labels)]
  }
  df_list <- lapply(indices, function(index){
    input_df <- calc_df[index+1, ]
    # "pairwise.complete.obs" is to ignore NA. Range is the area of df columns to calculate
    cor_result <- as.data.frame(cor(input_df, use = use, method=method))
    # put label to recognize row names
    rownames(cor_result) <- colnames(input_df)
    df_list <- lapply(colnames(input_df), function(name){
      # get values for each column
      val <- cor_result[,name]
      data.frame(pair.name.1 = rep(name, length(val)), pair.name.2 = colnames(input_df), cor.value=val, stringsAsFactors = FALSE)
    })
    output_df <- do.call(rbind, df_list)
    if(is.null(label_df)){
      # not grouped case
      output_df
    } else {
      # grouped case: create group label column
      cols <- lapply(colnames(label_df), function(colname){
        df <- data.frame(unlist(replicate(nrow(output_df), label_df[index[[1]]+1, colname])), stringsAsFactors = FALSE)
        colnames(df) <- colname
        df
      })
      label <- do.call(rbind, replicate(nrow(output_df), label_df[1,]))
      group_df <- do.call(cbind, cols)

      cbind(group_df, output_df)
    }
  })
  result <- do.call(rbind, df_list)
  tibble::repair_names(result, sep="_")
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
do_svd <- function(df,
                   group,
                   dimension,
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
  group_col <- col_name(substitute(group))
  dimension_col <- col_name(substitute(dimension))
  value_col <- col_name(substitute(value))

  grouped_col <- grouped_by(df)
  axis_prefix <- "axis"
  value_cname <- avoid_conflict(colnames(df), "svd.value")

  # this is executed on each group
  do_svd_each <- function(df){
    matrix <-simple_cast(df, group_col, dimension_col, value_col, fun.aggregate = fun.aggregate, fill=fill)
    if(centering){
      # move the origin to center of data
      matrix <- sweep(matrix, 2, colMeans(matrix), "-")
    }
    if(type=="group"){
      result <- svd(matrix, nu=n_component, nv=0)
      mat <- result$u

      if (output=="wide") {
        ret <- as.data.frame(mat)
        colnames(ret) <- avoid_conflict(c(grouped_col, group_col), paste("axis", seq(ncol(mat)), sep=""))
        df <- setNames(data.frame(a=as.character(rownames(matrix)), stringsAsFactors = FALSE), group_col)
        ret <- cbind(df, ret)
      } else if (output=="long") {
        cnames <- avoid_conflict(grouped_col, c("group", "component", value_cname))
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
        colnames(ret) <- avoid_conflict(c(dimension_col, group_col), paste("axis", seq(ncol(mat)), sep=""))
        df <- setNames(data.frame(a=as.character(rownames(mat)), stringsAsFactors = FALSE), dimension_col)
        ret <- cbind(df, ret)
      } else if (output=="long") {
        cnames <- avoid_conflict(grouped_col, c("dimension", "component", value_cname))
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
        colnames(ret) <- avoid_conflict(c(group_col), paste("axis", seq(ncol(mat)), sep=""))
      } else if (output=="long") {
        ret <- data.frame(component = component, svd.value = variance[component])
        colnames(ret) <- avoid_conflict(group_col, c("component", value_cname))
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
