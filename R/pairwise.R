


#' Calculate distance of each pair of groups
#' @param df data frame in tidy format
#' @param group A column you want to calculate the correlations for.
#' @param dimension A column you want to use as a dimension to calculate the correlations.
#' @param value A column for the values you want to use to calculate the correlations.
#' @param distinct The returned pair should be duplicated in swapped order or not.
#' TRUE makes it easy to filter group names.
#' @param diag If similarity between itself should be returned or not.
#' @param method Type of calculation. https://cran.r-project.org/web/packages/proxy/vignettes/overview.pdf
#' @param p P parameter for "minkowski" method.
#' @param normalize Whether to normalize values for each key. 
#' @param cmdscale_k Number of dimention to map the result.
#' @param time_unit Unit of time to aggregate key_col if key_col is Date or POSIXct#' @param time_unit Unit of time to aggregate key_col if key_col is Date or POSIXct. NULL doesn't aggregate.
#' @export
do_dist.kv_ <- function(df,
                        subject_col,
                        key_col,
                        value_col = NULL,
                        fill=0,
                        fun.aggregate=mean,
                        distinct=FALSE,
                        diag=FALSE,
                        method="euclidean",
                        p=2,
                        normalize=FALSE,
                        cmdscale_k = NULL,
                        time_unit = NULL){
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("reshape2")
  loadNamespace("stats")

  grouped_column <- grouped_by(df)

  if(subject_col %in% grouped_column){
    stop(paste0(subject_col, " is a grouping column. ungroup() may be necessary before this operation."))
  }

  # column names are "{subject}.x", "{subject}.y", "value"
  cnames <- avoid_conflict(grouped_column,
                           c(paste0(subject_col, c(".x", ".y")), # We use paste0 since str_c garbles multibyte column names here for some reason.
                             "value")
                           )

  # this is executed on each group
  calc_dist_each <- function(df){
    # t is used because dist calculates distances of rows
    # but simple_cast is designed for subject to columns,
    # so the matrix is transposed
    mat <- df %>%
      simple_cast(
        key_col,
        subject_col,
        value_col,
        fill=fill,
        fun.aggregate=fun.aggregate,
        time_unit = time_unit,
        na.rm = TRUE
      )

    mat <- t(mat)

    if (normalize) {
      # normalize each key.
      # where normalization should take place is debatable.
      # we may want to do it outside of group_by to have uniform definition of distance across groups.
      # on the other hand, a good definition of distance for a group might not work well for another group,
      # in which case normalization per group might be better...
      mat <- scale(mat)
    }

    # Dist is actually an atomic vector of upper half so upper and diag arguments don't matter
    dist <- stats::dist(mat, method=method, diag=FALSE, p=p)
    if(distinct){
      if(diag){
        diag <- 0
      }else{
        diag <- NULL
      }
      ret <- upper_gather(
        as.vector(dist),
        rownames(mat),
        diag=diag,
        cnames=cnames,
        na.rm = FALSE,
        zero.rm = FALSE
      )
    }else{
      ret <- dist %>%  as.matrix() %>%  mat_to_df(cnames, na.rm = FALSE, zero.rm = FALSE)
      if(!diag){
        ret<- ret[ret[,1] != ret[,2],]
      }
    }
    rownames(ret) <- NULL
    if (!is.null(cmdscale_k)) {
      ret <- do_cmdscale_(ret, cnames[[1]], cnames[[2]], cnames[[3]], k = cmdscale_k)
      # the label for each point should be the subject
      # so the column name should be the same
      colnames(ret)[[1]] <- subject_col
    }

    ret
  }
  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(grouped_column, "tmp")
  df %>%
    dplyr::do_(.dots=setNames(list(~calc_dist_each(.)), tmp_col)) %>%
    dplyr::ungroup() %>%
    unnest_with_drop(!!rlang::sym(tmp_col))
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
#' @param p The power of the Minkowski distance.
#' @export
do_dist.cols <- function(df,
                         ...,
                         label=NULL,
                         fill=0,
                         fun.aggregate=mean,
                         distinct=FALSE,
                         diag=FALSE,
                         method="euclidean",
                         p=2,
                         cmdscale_k = NULL){
  validate_empty_data(df)

  loadNamespace("dplyr")
  loadNamespace("tidyr")
  loadNamespace("reshape2")
  loadNamespace("stats")
  loadNamespace("lazyeval")

  grouped_column <- grouped_by(df)
  label_col <- col_name(substitute(label))

  # using the new way of NSE column selection evaluation
  # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
  select_dots <- tidyselect::vars_select(names(df), !!! rlang::quos(...))

  cnames <- avoid_conflict(grouped_column, c("pair.name.x", "pair.name.y", "value"))

  # this is executed on each group
  calc_dist_each <- function(df){
    mat <- df %>%  dplyr::select(!!!select_dots) %>%  as.matrix()

    # sort the column name so that the output of pair.name.1 and pair.name.2 will be sorted
    # it's better to be sorted so that heatmap in exploratory can be triangle if distinct is TRUE
    # We use stringr::str_sort() as opposed to base sort() so that the result is consistent on Windows too.
    sortedNames <- stringr::str_sort(colnames(mat))
    mat <- t(mat)
    mat <- mat[sortedNames, ]

    # Dist is actually an atomic vector of upper half so upper and diag arguments don't matter
    dist <- stats::dist(mat, method=method, diag=FALSE, p=p)
    if(distinct){
      if(diag){
        diag <- 0
      }else{
        diag <- NULL
      }
      ret <- upper_gather(
        as.vector(dist),
        rownames(mat),
        diag=diag,
        cnames=cnames,
        na.rm = FALSE,
        zero.rm = FALSE
      )
    }else{
      ret <- dist %>%  as.matrix() %>%  mat_to_df(
        cnames,
        na.rm = FALSE,
        zero.rm = FALSE
      )
      if(!diag){
        ret <- ret[ret[,1] != ret[,2],]
      }
    }
    rownames(ret) <- NULL
    if (!is.null(cmdscale_k)) {
      ret <- do_cmdscale_(ret, cnames[[1]], cnames[[2]], cnames[[3]], k = cmdscale_k)
    }
    ret
  }
  df %>%
    dplyr::do_(.dots=setNames(list(~calc_dist_each(.)), cnames[[1]])) %>%
    dplyr::ungroup() %>%
    unnest_with_drop(!!rlang::sym(cnames[[1]]))
}
