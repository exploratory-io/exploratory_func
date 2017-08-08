#'
#'

#' scale wrapper that returns a vector as a result
#' @export
normalize <- function(...) {
  # scale returns a matrix even if the input is a vector
  # it should be converted to a numeric vector by as.numeric
  ret <- scale(...)
  as.numeric(ret)
}

#' integrated do_cor
#' @export
do_cor <- function(df, ..., skv = NULL, fun.aggregate=mean, fill=0){
  validate_empty_data(df)

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
#' @param time_unit Unit of time to aggregate key_col if key_col is Date or POSIXct. NULL doesn't aggregate.
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
                      time_unit = NULL,
                      use="pairwise.complete.obs",
                      method="pearson",
                      distinct = FALSE,
                      diag = FALSE,
                      fill = 0,
                      fun.aggregate=mean
                      )
{
  validate_empty_data(df)
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
    mat <- simple_cast(
      df,
      row,
      col,
      val,
      fun.aggregate=fun.aggregate,
      fill=fill,
      time_unit = time_unit,
      na.rm = TRUE
    )
    cor_mat <- cor(mat, use = use, method = method)
    if(distinct){
      ret <- upper_gather(
        cor_mat,
        diag=diag,
        cnames=output_cols,
        na.rm = FALSE,
        zero.rm = FALSE
      )
    } else {
      ret <- mat_to_df(cor_mat, cnames=output_cols, diag=diag, na.rm = FALSE)
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
    dplyr::ungroup() %>%
    unnest_with_drop_(tmp_col)
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
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tibble")
  # select columns using dplyr::select logic
  select_dots <- lazyeval::lazy_dots(...)
  grouped_col <- grouped_by(df)
  output_cols <- avoid_conflict(grouped_col, c("pair.name.x", "pair.name.y", "value"))
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

  df %>%
    dplyr::do_(.dots=setNames(list(~do_cor_each(.)), output_cols[[1]])) %>%
    dplyr::ungroup() %>%
    unnest_with_drop_(output_cols[[1]])
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
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("tidyr")
  grouped_col <- grouped_by(df)

  # remove NA because cmdscale doesn't accept NA.
  # NA happens if a data has NA in a value and
  # all distances with others become NA too, so
  # it should be totally removed from the data
  df <- df %>%
    dplyr::filter(!is.na(!!as.symbol(value_col)))

  # if pair1_col and pair2_col are with the same name
  # like aaa.x and aaa.y (output of skv of do_dist),
  # it (in this case, "aaa") should be used as name column
  # because the results are coordinates of them
  # but pair.name.x and pair.name.y is from do_dist.cols and it isn't
  # a valid name for name column because the output is no longer pair
  # , so in that case, it will be just "name"
  name <- stringr::str_replace(pair1_col, "\\.[x|y]$", "")
  name_col <- if(name != "pair.name" && name == stringr::str_replace(pair2_col, "\\.[x|y]$", "")){
    name
  } else {
    avoid_conflict(grouped_col, "name")
  }

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

    if (ncol(mat) <= k) {
      # this causes an error in cmdscale, so should be validated
      stop("Number of unique points should be more than the number of dimensions")
    }

    points <- cmdscale(as.dist(t(mat)), eig=FALSE, k=k)

    result_df <- as.data.frame(points)

    # these column names should be consistent with the result of do_svd
    colnames(result_df) <- paste("axis", seq(ncol(result_df)), sep = "")
    df <- setNames(data.frame(rownames(points), stringsAsFactors=FALSE), name_col)
    ret <- cbind(df, result_df)
    ret
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(grouped_col, "tmp")
  df %>%
    dplyr::do_(.dots=setNames(list(~do_cmdscale_each(.)), tmp_col)) %>%
    dplyr::ungroup() %>%
    unnest_with_drop_(tmp_col)
}
