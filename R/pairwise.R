#' Calculate similarity of each pair of groups.
#' @param df data frame in tidy format
#' @param subject A column you want to calculate the correlations for.
#' @param key A column you want to use as a dimension to calculate the correlations.
#' @param value A column for the values you want to use to calculate the correlations.
#' @param distinct The returned pair should be duplicated in swapped order or not.
#' TRUE makes it easy to filter group names.
#' @param diag If similarity between itself should be returned or not.
#' @param method Type of calculation. https://cran.r-project.org/web/packages/proxy/vignettes/overview.pdf
#' @param fun.aggregate Set an aggregate function when there are multiple entries for the key column per each category.
#' @export
do_cosine_sim.kv <- function(df, subject, key, value = NULL, distinct=FALSE, diag=FALSE, fun.aggregate=mean){
  validate_empty_data(df)

  loadNamespace("qlcMatrix")
  loadNamespace("tidytext")
  loadNamespace("Matrix")
  loadNamespace("stringr")
  subject_col <- col_name(substitute(subject))
  key_col <- col_name(substitute(key))
  value_col <- if(is.null(substitute(value))) NULL else col_name(substitute(value))

  grouped_column <- grouped_by(df)

  if(subject_col %in% grouped_column){
    stop(paste0(subject_col, " is a grouping column. ungroup() may be necessary before this operation."))
  }

  # column names are "{subject}.x", "{subject}.y", "value"
  cnames <- avoid_conflict(grouped_column,
                            c(stringr::str_c(subject_col, c(".x", ".y")),
                            "value")
                           )

  # this is executed on each group
  calc_doc_sim_each <- function(df){
    mat <- sparse_cast(df, key_col, subject_col, val = value_col, fun.aggregate = fun.aggregate, count = TRUE)
    sim <- qlcMatrix::cosSparse(mat)
    if(distinct){
      if(!diag){
        diag <- NULL
      }
      df <- upper_gather(sim, rownames(mat), diag=diag, cnames=cnames)
    }else{
      loadNamespace("reshape2")
      loadNamespace("dplyr")
      df <- sim %>%  as.matrix() %>%  mat_to_df(cnames, na.rm=FALSE)
      if(!diag){
        df <- df[df[,1] != df[,2],]
      }
    }
    df
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(grouped_column, "tmp")
  df %>%
    dplyr::do_(.dots=setNames(list(~calc_doc_sim_each(.)), tmp_col)) %>%
    dplyr::ungroup() %>%
    unnest_with_drop_(tmp_col)

}

#' integrated do_dist
#' @export
do_dist <- function(df, ..., skv = NULL, fun.aggregate=mean, fill=0){
  validate_empty_data(df)

  if (!is.null(skv)) {
    #.kv pattern
    if (!length(skv) %in% c(2, 3)) {
      stop("length of skv has to be 2 or 3")
    }
    value <- if(length(skv) == 2)  NULL else skv[[3]]
    do_dist.kv_(df, skv[[1]], skv[[2]], value, fun.aggregate = fun.aggregate, fill = fill, ...)
  } else {
    #.cols pattern
    do_dist.cols(df, ...)
  }
}

#' Non Standard Evaluation version of do_dist
#' Calculate distance of each pair of groups
#' @export
do_dist.kv <- function(df, subject, key, value = NULL, ...){
  subject_col <- col_name(substitute(subject))
  key_col <- col_name(substitute(key))
  if(!is.null(substitute(value))){
    value_col <- col_name(substitute(value))
  } else {
    value_col <- NULL
  }

  do_dist.kv_(df, subject_col, key_col, value_col, ...)
}

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
                           c(stringr::str_c(subject_col, c(".x", ".y")),
                             "value")
                           )

  # this is executed on each group
  calc_dist_each <- function(df){
    # t is used because dist calculates distances of rows
    # but simple_cast is designed for subject to columns,
    # so the matrix is transposed
    mat <- df %>%
      simple_cast(key_col, subject_col, value_col, fill=fill, fun.aggregate=fun.aggregate, time_unit = time_unit) %>%
      t()

    # Dist is actually an atomic vector of upper half so upper and diag arguments don't matter
    dist <- stats::dist(mat, method=method, diag=FALSE, p=p)
    if(distinct){
      if(diag){
        diag <- 0
      }else{
        diag <- NULL
      }
      ret <- upper_gather(as.vector(dist), rownames(mat), diag=diag, cnames=cnames)
    }else{
      ret <- dist %>%  as.matrix() %>%  mat_to_df(cnames)
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
    unnest_with_drop_(tmp_col)
}

#' A symmetric version of KL-divergence
#' This is often used with topic model to calculate distances between topics
#' Ref: https://github.com/cpsievert/LDAvis/blob/master/R/createJSON.R
#' @export
do_kl_dist.kv_ <- function(df,
                           subject_col,
                           key_col,
                           value_col = NULL,
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
  loadNamespace("proxy")

  grouped_column <- grouped_by(df)

  if(subject_col %in% grouped_column){
    stop(paste0(subject_col, " is a grouping column. ungroup() may be necessary before this operation."))
  }

  # column names are "{subject}.x", "{subject}.y", "value"
  cnames <- avoid_conflict(grouped_column,
                           c(stringr::str_c(subject_col, c(".x", ".y")),
                             "value")
  )

  # this is executed on each group
  calc_dist_each <- function(df){
    mat <- df %>%  simple_cast(subject_col, key_col, value_col, fill=fill, fun.aggregate=fun.aggregate)
    # Dist is actually an atomic vector of upper half so upper and diag arguments don't matter
    jensenShannon <- function(x, y) {
      m <- 0.5*(x + y)
      0.5*sum(x*log(x/m)) + 0.5*sum(y*log(y/m))
    }
    dist <- proxy::dist(x = mat, method = jensenShannon)
    if(distinct){
      if(diag){
        diag <- 0
      }else{
        diag <- NULL
      }
      ret <- upper_gather(as.vector(dist), rownames(mat), diag=diag, cnames=cnames)
    }else{
      ret <- dist %>%  as.matrix() %>%  mat_to_df(cnames)
      if(!diag){
        ret<- ret[ret[,1] != ret[,2],]
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
    unnest_with_drop_(cnames[[1]])
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

  select_dots <- lazyeval::lazy_dots(...)

  cnames <- avoid_conflict(grouped_column, c("pair.name.x", "pair.name.y", "value"))

  # this is executed on each group
  calc_dist_each <- function(df){
    mat <- df %>%  dplyr::select_(.dots=select_dots) %>%  as.matrix()

    # sort the column name so that the output of pair.name.1 and pair.name.2 will be sorted
    # it's better to be sorted so that heatmap in exploratory can be triangle if distinct is TRUE
    sortedNames <- sort(colnames(mat))
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
      ret <- upper_gather(as.vector(dist), rownames(mat), diag=diag, cnames=cnames)
    }else{
      ret <- dist %>%  as.matrix() %>%  mat_to_df(cnames)
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
    unnest_with_drop_(cnames[[1]])
}
