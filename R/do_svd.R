#' integrated do_svd
#' @export
do_svd <- function(df, ..., skv = NULL, fun.aggregate=mean, fill=0){
  validate_empty_data(df)
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

#' Non Standard Evaluation version of do_svd.kv_
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
#' @param subject_col A column name to be regarded as groups
#' @param key_col A column name to be regarded as original dimensions
#' @param value_col A column name to be regarded as values
#' @param type "group" to see the coordinations of groups in reduced dimension.
#' "dimension" to see the direction of new axes in original dimension.
#' "variance" to see how much the data is distributed in the direction of new axes.
#' @param fill Value to fill where value doesn't exist.
#' @param fun.aggregate Function to aggregate values in duplicated pairs of subject and key.
#' @param n_component Number of dimensions to return.
#' @param centering Move the origin to center of data (Mean of each column)
#' Taken from http://genomicsclass.github.io/book/pages/pca_svd.html
#' @param output "long" or "wide".
#' "long" is a format with 3 columns which represents rows, columns and values
#' "wide" is a format which spreads the long information into matrix
#' @return Tidy format of svd result in a data frame.
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
  validate_empty_data(df)

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

    # value_col might be NULL
    # when values in matrix
    # should be count of key and value pairs,
    # so it should be checked
    if (!is.null(value_col)) {
      # remove NA from the value column
      df <- df[!is.na(df[[value_col]]), ]
    }

    matrix <- simple_cast(df, subject_col, dimension_col, value_col, fun.aggregate = fun.aggregate, fill=fill)

    # this might happen if fill argument is NA or fun.aggregate returns NA
    if(any(is.na(matrix))){
      stop("NA is not supported as value")
    }

    if(centering){
      # Move the origin to center of data
      # (Mean of each column)
      #
      # Explanation from http://genomicsclass.github.io/book/pages/pca_svd.html
      # "The second argument specifies we want to operate on the columns
      # (1 would be used for rows), and the third and fourth arguments
      # specify that we want to subtract the column means."
      matrix <- sweep(matrix, 2, colMeans(matrix), "-")
    }
    if(type=="group"){
      # This caliculates coordinations of groups (rows of matrix)
      # in reduced dimension
      # and create columns with the information

      # u matrix in svd can be regarded as coordinations of groups in reduced dimension
      result <- svd(matrix, nu=n_component, nv=0)
      mat <- result$u

      if (output=="wide") {
        # This returns a data frame
        # whose columns are coordinations
        # in new dimension.

        ret <- as.data.frame(mat)
        colnames(ret) <- avoid_conflict(c(grouped_col, subject_col), paste("axis", seq(ncol(mat)), sep=""))
        # subject column might be non-character type,
        # but rownames are always character,
        # so its type should be reverted to the same type
        # with subject column
        rnames <- same_type(rownames(matrix), df[[subject_col]])
        # create a column for subject values
        df <- setNames(data.frame(rnames, stringsAsFactors = FALSE), subject_col)
        ret <- cbind(df, ret)
      } else if (output=="long") {
        # this returns molten format data frame of matrix
        # with subjects, new dimentions and values
        cnames <- avoid_conflict(grouped_col, c(subject_col, "new.dimension", value_cname))
        rownames(mat) <- rownames(matrix)
        ret <- mat_to_df(mat, cnames)
      } else {
        stop(paste(output, "is not supported as output"))
      }
    } else if (type=="dimension") {
      # This caliculates direction of
      # base vectors in new dimension
      # in original dimension,
      # which is v in svd.

      # v matrix in svd can be regarded as the direction of new axes in original dimension.
      result <- svd(matrix, nv=n_component, nu=0)
      mat <- result$v

      rownames(mat) <- colnames(matrix)

      if (output=="wide") {
        # a column is a base vector of
        # a new axis of reduced dimension
        # in the original dimension
        ret <- as.data.frame(mat)
        colnames(ret) <- avoid_conflict(c(grouped_col, dimension_col), paste("axis", seq(ncol(mat)), sep=""))
        rnames <- same_type(rownames(mat), df[[dimension_col]])
        df <- setNames(data.frame(rnames, stringsAsFactors = FALSE), dimension_col)
        ret <- cbind(df, ret)
      } else if (output=="long") {
        # this returns molten format data frame of matrix
        # with key, new dimentions and values
        cnames <- avoid_conflict(grouped_col, c(dimension_col, "new.dimension", value_cname))
        ret <- mat_to_df(mat, cnames)
      } else {
        stop(paste(output, "is not supported as output"))
      }
    } else if (type=="variance"){
      # This returns d in svd which can be
      # regarded as how much the data is
      # distributed in the direction of new axes.
      variance <- svd(matrix, nu=0, nv=0)$d
      component <- seq(min(length(variance), n_component))
      if (output=="wide") {
        # this return one row data frame with d of svd
        mat <- matrix(variance[component], ncol=length(component))
        ret <- as.data.frame(mat)
        colnames(ret) <- avoid_conflict(c(grouped_col), paste("axis", seq(ncol(mat)), sep=""))
      } else if (output=="long") {
        # this return a data frame with a column with d of svd
        ret <- data.frame(component = component, svd.value = variance[component])
        colnames(ret) <- avoid_conflict(grouped_col, c("new.dimension", value_cname))
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

#' Calculate distance of each pair of groups.
#' @param df data frame in tidy format
#' @param ... Arguments to select columns to calculate svd.
#' @param type "group" to see the coordinations of groups in reduced dimension.
#' "dimension" to see the direction of new axes in original dimension.
#' "variance" to see how much the data is distributed in the direction of new axes.
#' @param fill Value to fill where value doesn't exist.
#' @param fun.aggregate Function to aggregate values in duplicated pairs of subject and key.
#' @param n_component Number of dimensions to return.
#' @param centering Move the origin to center of data (Mean of each column)
#' Taken from http://genomicsclass.github.io/book/pages/pca_svd.html
#' @param output "long" or "wide".
#' "long" is a format with 3 columns which represents rows, columns and values
#' "wide" is a format which spreads the long information into matrix
#' @return Tidy format of svd result in a data frame.
#' @export
do_svd.cols <- function(df,
                         ...,
                        type="group",
                        fill=0,
                        fun.aggregate=mean,
                        n_component=3,
                        centering=TRUE,
                        output ="long"){
  validate_empty_data(df)

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
      na.omit() # this removes rows which have any NA

    if(centering){
      # Move the origin to center of data
      # (Mean of each column)
      #
      # Explanation from http://genomicsclass.github.io/book/pages/pca_svd.html
      # "The second argument specifies we want to operate on the columns
      # (1 would be used for rows), and the third and fourth arguments
      # specify that we want to subtract the column means."
      matrix <- sweep(matrix, 2, colMeans(matrix), "-")
    }
    if(type=="group"){
      # This caliculates coordinations of groups (rows of matrix)
      # in reduced dimension
      # and create columns with the information

      # u matrix in svd can be regarded as coordinations of groups in reduced dimension
      result <- svd(matrix, nu=n_component, nv=0)
      mat <- result$u

      if (output=="wide") {
        # This returns a data frame
        # whose columns are coordinations
        # in new dimension
        # while keeping columns
        # from the original data frame.

        # get row indice that had NA
        na_indice <- na.action(matrix)

        # Rows which have any NA value are removed from matrix to calculate svd
        # but the result will be binded to original data frame
        # so rows with all NA should be inserted to the removed rows
        mat <- fill_mat_NA(setdiff(seq(nrow(df)), na_indice), mat = mat, max_index = nrow(df))

        ret <- as.data.frame(mat)
        colnames(ret) <- avoid_conflict(c(colnames(df), grouped_col), paste0("axis", seq(ncol(ret))))

        # append u matrix to the original data frame
        ret <- cbind(df, ret)
      } else if (output=="long") {
        # this returns molten format data frame of matrix
        # with subjects, new dimentions and values
        cnames <- avoid_conflict(grouped_col, c("row", "new.dimension", value_colname))
        ret <- mat_to_df(mat, cnames)
      } else {
        stop(paste(output, "is not supported as output"))
      }
    } else if (type=="dimension") {
      # This caliculates direction of
      # base vectors in new dimension
      # in original dimension,
      # which is v in svd.

      # v matrix in svd can be regarded as the direction of new axes in original dimension.
      result <- svd(matrix, nv=n_component, nu=0)
      mat <- result$v
      rownames(mat) <- colnames(matrix)

      if (output=="wide") {
        # This returns a data frame
        # whose columns are base vectors of
        # new axes of reduced dimension
        # in the original dimension
        ret <- as.data.frame(mat)
        colnames(ret) <- avoid_conflict(c(grouped_col), paste("axis", seq(ncol(mat)), sep=""))
        rnames <- colnames(matrix)
        df <- setNames(data.frame(rnames, stringsAsFactors = FALSE), "colname")
        ret <- cbind(df, ret)
      } else if (output=="long") {
        # this returns molten format data frame
        # of v matrix
        # with key, new dimentions and values
        cnames <- avoid_conflict(grouped_col, c("colname", "new.dimension", value_colname))
        ret <- mat_to_df(mat, cnames)
      } else {
        stop(paste(output, "is not supported as output"))
      }
    } else if (type=="variance"){
      # This returns d in svd which can be
      # regarded as how much the data is
      # distributed in the direction of new axes.
      variance <- svd(matrix, nu=0, nv=0)$d
      component <- seq(min(length(variance), n_component))
      if (output=="wide") {
        # this return one row data frame with d of svd
        mat <- matrix(variance[component], ncol=length(component))
        ret <- as.data.frame(mat)
        colnames(ret) <- avoid_conflict(grouped_col, paste("axis", seq(ncol(mat)), sep=""))
      } else if (output=="long") {
        # this return a data frame with a column with d of svd
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
