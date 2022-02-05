#'
#'

#' scale wrapper that returns a vector as a result
#' @export
normalize <- function(x, center = TRUE, scale = TRUE) {
  if (scale && (center || (x[1] == 0)) &&
      min(x, na.rm = TRUE) == max(x, na.rm = TRUE)) {
    # If input is constant and scaling is on, and centering is on or values in the first place were 0,
    # return vector of zeros.
    # scale() does not behave well here, like returning NaN or numbers very close to 0.
    # We handle this case ourselves. 
    rep(0, length(x))
  }
  else {
    ret <- scale(x, center = center, scale = scale)
    # scale returns a matrix even if the input is a vector.
    # Convert from a matrix to a numeric vector.
    as.numeric(ret)
  }
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
                      fun.aggregate = mean,
                      return_type = "data.frame"
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
                                c(paste0(col, c(".x", ".y")), # We use paste0 since str_c garbles multibyte column names here for some reason.
                                  "correlation", "p_value", "statistic"))

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
    if (dim(mat)[[1]] < 2) {
      # Correlation require 2 or more rows.
      if (length(grouped_col) > 0) {
        # Skip this group in this case. TODO: Report error.
        return(NULL)
      }
      else {
        # This is the only group. Throw error.
        stop("More than 1 aggregated measures per category are required to calculate correlations.")
      }
    }
    ret <- do_cor_internal(mat, use, method, diag, output_cols, na.rm=FALSE) # TODO: Why was na.rm explicitly set to TRUE for do_cor.kv_ but not for do_cor.cols?

    if (return_type == "data.frame") {
      ret # Return correlation data frame as is.
    }
    else {
      # Return cor_exploratory model, which is a set of correlation data frame and the original data.
      # We use the original data for scatter matrix on Analytics View.
      ret <- list(cor = ret, data = df)
      class(ret) <- c("cor_exploratory", class(ret))
      ret
    }
  }
  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  if (return_type == "data.frame") {
    tmp_col <- avoid_conflict(grouped_col, "tmp")
    df %>%
      dplyr::do_(.dots=setNames(list(~do_cor_each(.)), tmp_col)) %>%
      dplyr::ungroup() %>%
      unnest_with_drop(!!rlang::sym(tmp_col))
  }
  else {
    do_on_each_group(df, do_cor_each, name = "model", with_unnest = FALSE)
  }
}

#'
#' Calculate correlation among columns and output the correlation of each pair
#' @param df data frame in tidy format
#' @param ... Arguments to select columns to calculate correlation.
#' @param use Operation type for dealing with missing values. This can be one of "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs"
#' @param method Method of calculation. This can be one of "pearson", "kendall", or "spearman".
#' @return correlations between pairs of columns
#' @export
do_cor.cols <- function(df, ..., use = "pairwise.complete.obs", method = "pearson",
                        distinct = FALSE, diag = FALSE, variable_order = "correlation",
                        return_type = "data.frame") {
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("lazyeval")
  loadNamespace("tibble")
  # select columns using dplyr::select logic
  # using the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  select_dots <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  grouped_col <- grouped_by(df)
  output_cols <- avoid_conflict(grouped_col, c("pair.name.x", "pair.name.y", "correlation", "p_value", "statistic"))
  # check if the df's grouped
  do_cor_each <- function(df){
    if (nrow(df) < 2) {
      # Correlation require 2 or more rows.
      if (length(grouped_col) > 0) {
        # Skip this group in this case. TODO: Report error.
        return(NULL)
      }
      else {
        # This is the only group. Throw error.
        stop("More than 1 row are required to calculate correlations.")
      }
    }
    mat <- dplyr::select(df, !!!select_dots) %>%
      # Convert logical to numeric explicitly, since implicit conversion by as.matrix does not happen if all the columns are logical.
      dplyr::mutate(across(where(is.logical), as.numeric)) %>%
      as.matrix()

    ret <- do_cor_internal(mat, use, method, diag, output_cols, na.rm=TRUE)

    if (variable_order == "correlation") {
      # Set factor levels to pair.name.x and pair.name.y based on the mean of correlations with other columns.
      cor0 <- ret %>% filter(pair.name.x != pair.name.y)
      cor0 <- cor0 %>% group_by(pair.name.x) %>% summarize(mean_cor=mean(correlation, na.rm=TRUE)) %>% arrange(desc(mean_cor))
      ret <- ret %>% mutate(pair.name.x = forcats::fct_relevel(pair.name.x, cor0$pair.name.x), pair.name.y = forcats::fct_relevel(pair.name.y, cor0$pair.name.x))
    }
    else { # "input" case. Honor the specified variable order.
      ret <- ret %>% mutate(pair.name.x = forcats::fct_relevel(pair.name.x, !!select_dots), pair.name.y = forcats::fct_relevel(pair.name.y, !!select_dots))
    }

    if (distinct) {
      ret <- ret %>% filter(as.integer(pair.name.x) <= as.integer(pair.name.y))
    }

    if (return_type == "data.frame") {
      ret # Return correlation data frame as is.
    }
    else {
      # Return cor_exploratory model, which is a set of correlation data frame and the original data.
      # We use the original data for scatter matrix on Analytics View.
      ret <- list(cor = ret, data = df)
      class(ret) <- c("cor_exploratory", class(ret))
      ret
    }
  }

  if (return_type == "data.frame") {
    df %>%
      dplyr::do_(.dots=setNames(list(~do_cor_each(.)), output_cols[[1]])) %>%
      dplyr::ungroup() %>%
      unnest_with_drop(!!rlang::sym(output_cols[[1]]))
  }
  else {
    do_on_each_group(df, do_cor_each, name = "model", with_unnest = FALSE)
  }
}


do_cor_internal <- function(mat, use, method, diag, output_cols, na.rm) {
  # sort the column name so that the output of pair.name.1 and pair.name.2 will be sorted
  # it's better to be sorted so that heatmap in exploratory can be triangle if distinct is TRUE.
  # We use stringr::str_sort() as opposed to base sort() so that the result is consistent on Windows too.
  sorted_colnames <- stringr::str_sort(colnames(mat))
  mat <- mat[,sorted_colnames]

  cor_mat <- cor(mat, use = use, method = method)
  ret <- mat_to_df(cor_mat,
                   cnames=output_cols[1:3],
                   diag=diag,
                   na.rm=na.rm,
                   zero.rm=FALSE)

  # Create a matrix of P-values for Analytics View case.
  dim <- length(sorted_colnames)
  pvalue_mat <- matrix(NA, dim, dim)
  tvalue_mat <- matrix(NA, dim, dim)
  for (i in 2:dim) {
    for (j in 1:(i-1)) {
      tryCatch({
        cor_test_res <- cor.test(mat[,i], mat[,j], method = method)
        pvalue_mat[i, j] <- cor_test_res$p.value
        tvalue_mat[i, j] <- cor_test_res$statistic
      }, error = function(e) {
        if (e$message == "not enough finite observations") {
          # This is the error cor.test returns when there is not enough non-NA data.
          # Rather than stopping, set NA as the result, and we will handle it as a not-significant case on the UI.
          pvalue_mat[i, j] <- NA
          tvalue_mat[i, j] <- NA
        }
        else {
          stop(e)
        }
      })
      pvalue_mat[j, i] <- pvalue_mat[i, j]
      tvalue_mat[j, i] <- tvalue_mat[i, j]
    }
  }
  for (i in 1:dim) { # For i=j case, P value should be always 0 and t statistic should be Inf.
    pvalue_mat[i, i] <- 0
    tvalue_mat[i, i] <- Inf
  }
  colnames(pvalue_mat) <- sorted_colnames
  rownames(pvalue_mat) <- sorted_colnames
  colnames(tvalue_mat) <- sorted_colnames
  rownames(tvalue_mat) <- sorted_colnames
  p_value_ret <- mat_to_df(pvalue_mat, cnames=output_cols[c(1,2,4)], diag=diag, zero.rm=FALSE)
  t_value_ret <- mat_to_df(tvalue_mat, cnames=output_cols[c(1,2,5)], diag=diag, zero.rm=FALSE)
  ret <- ret %>% dplyr::left_join(p_value_ret, by=output_cols[1:2]) # Join by pair.name.x and pair.name.y.
  ret <- ret %>% dplyr::left_join(t_value_ret, by=output_cols[1:2]) # Join by pair.name.x and pair.name.y.
  ret
}

#' @export
tidy.cor_exploratory <- function(x, type = "cor", ...) { #TODO: add test
  if (type == "cor") {
    x$cor
  }
  else {
    x$data
  }
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
    if (all(df[[value_col]]==0)) {
      # cmdscale() returns broken dataframe with only a column
      # for the names of points and no columns for coordinate values,
      # which would break processing after that.
      # NAs are prefiltered at this point already.
      # Inf can be handled within cmdscale().
      stop("All distances are 0. Multidimensional scaling cannot be calculated.")
    }
    mat <- simple_cast(
      df,
      pair1_col,
      pair2_col,
      value_col,
      fun.aggregate = fun.aggregate,
      fill=fill,
      na.rm = TRUE
    )
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
    unnest_with_drop(!!rlang::sym(tmp_col))
}
