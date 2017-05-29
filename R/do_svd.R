#' integrated do_svd
#' @export
do_svd <- function(df, ..., skv = NULL, fun.aggregate=mean, fill=0){
  if (!is.null(skv)) {
    #.kv pattern
    if (!length(skv) %in% c(2, 3)) {
      stop("length of skv has to be 2 or 3")
    }
    value <- if(length(skv) == 2)  NULL else skv[[3]]
    do_svd.kv_(df, skv[[1]], skv[[2]], value, fun.aggregate = fun.aggregate, fill = fill, ...)
  } else {
    #.cols pattern
    do_svd.cols(df, ...)
  }
}

#' Non Standard Evaluation version of do_svd
#' Calculate svd of each pair of groups
#' @export
do_svd.kv <- function(df, subject, key, value = NULL, ...){
  subject_col <- col_name(substitute(subject))
  key_col <- col_name(substitute(key))
  if(!is.null(substitute(value))){
    value_col <- col_name(substitute(value))
  } else {
    value_col <- NULL
  }

  do_svd.kv_(df, subject_col, key_col, value_col, ...)
}


#' Calculate svd from tidy format. This can be used to calculate coordinations by reducing dimensionality.
#' @param df Data frame which has group and dimension
#' @param subject_col Column to be regarded as groups
#' @param key_col Column to be regarded as original dimensions
#' @param value_col Column to be regarded as values
#' @param type "group" to see the coordinations in reduced dimension.
#' "dimension" to see the direction of new axes from original ones.
#' "variance" to see how much the data is distributed in the direction of new axes.
#' @param fill Value to fill where value doesn't exist.
#' @param fun.aggregate Value to fill where value doesn't exist.
#' @param n_component Number of dimensions to return.
#' @return Tidy format of data frame.
#' @export
do_svd.kv_ <- function(df,
                      subject_col,
                      key_col,
                      value_col = NULL,
                      type="group",
                      fill=0,
                      fun.aggregate=mean,
                      n_component=3,
                      centering=TRUE,
                      output ="long"){
  loadNamespace("dplyr")
  loadNamespace("tibble")
  loadNamespace("tidyr")
  dimension_col <- key_col

  grouped_col <- grouped_by(df)

  if(subject_col %in% grouped_col){
    stop(paste0(subject_col, " is a grouping column. ungroup() may be necessary before this operation."))
  }


  axis_prefix <- "axis"
  value_cname <- avoid_conflict(colnames(df), "value")

  # this is executed on each group
  do_svd_each <- function(df){

    df <- df[!is.na(df[[value_col]]), ]

    matrix <- simple_cast(df, subject_col, dimension_col, value_col, fun.aggregate = fun.aggregate, fill=fill)

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

  (df %>%  dplyr::do_(.dots=setNames(list(~do_svd_each(.)), value_cname)) %>%  unnest_with_drop_(value_cname))
}

#' Calculate distance of each pair of groups.
#' @param df data frame in tidy format
#' @param group A column you want to calculate the correlations for.
#' @param dimension A column you want to use as a dimension to calculate the correlations.
#' @param value A column for the values you want to use to calculate the correlations.
#' @param distinct The returned pair should be duplicated in swapped order or not.
#' TRUE makes it easy to filter group names.
#' @param diag If similarity between itself should be returned or not.
#' @param method Type of calculation. https://cran.r-project.org/web/packages/proxy/vignettes/overview.pdf
#' @export
do_svd.cols <- function(df,
                         ...,
                        type="group",
                        fill=0,
                        fun.aggregate=mean,
                        n_component=3,
                        centering=TRUE,
                        output ="long"){
  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("reshape2")
  loadNamespace("stats")
  loadNamespace("lazyeval")

  grouped_col <- grouped_by(df)
  label_col <- col_name(substitute(label))

  select_dots <- lazyeval::lazy_dots(...)

  value_colname <- avoid_conflict(grouped_col, "value")

  # this is executed on each group
  do_svd_each <- function(df){
    # create matrix by selected columns
    matrix <- df %>%
      dplyr::select_(.dots=select_dots) %>%
      as.matrix() %>%
      na.omit()

    if(centering){
      # move the origin to center of data
      matrix <- sweep(matrix, 2, colMeans(matrix), "-")
    }
    if(type=="group"){
      result <- svd(matrix, nu=n_component, nv=0)
      mat <- result$u

      if (output=="wide") {
        # get row indice that had NA
        na_indice <- na.action(matrix)

        # fill rows with NA where there was any NA value
        mat <- fill_mat_NA(setdiff(seq(nrow(df)),na_indice), mat = mat, max_index = nrow(df))

        ret <- as.data.frame(mat)
        colnames(ret) <- avoid_conflict(c(colnames(df), grouped_col), paste0("axis", seq(ncol(ret))))
        ret <- cbind(df, ret)
      } else if (output=="long") {
        cnames <- avoid_conflict(grouped_col, c("row", "new.dimension", value_colname))
        #rownames(mat) <- seq(nrow(mat))
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
        colnames(ret) <- avoid_conflict(c(grouped_col), paste("axis", seq(ncol(mat)), sep=""))
        rnames <- colnames(matrix)
        df <- setNames(data.frame(rnames, stringsAsFactors = FALSE), "colname")
        ret <- cbind(df, ret)
      } else if (output=="long") {
        cnames <- avoid_conflict(grouped_col, c("colname", "new.dimension", value_colname))
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
        colnames(ret) <- avoid_conflict(grouped_col, paste("axis", seq(ncol(mat)), sep=""))
      } else if (output=="long") {
        ret <- data.frame(component = component, svd.value = variance[component])
        colnames(ret) <- avoid_conflict(grouped_col, c("new.dimension", value_colname))
      } else {
        stop(paste(output, "is not supported as output"))
      }
    } else {
      stop(paste(type, "is not supported as type argument."))
    }

    ret
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(grouped_col, "tmp")
  ret <- df %>%
    dplyr::do_(.dots=setNames(list(~do_svd_each(.)), tmp_col)) %>%
    unnest_with_drop_(tmp_col)

  ret
}
