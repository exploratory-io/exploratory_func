#' wrapper for t.test, which compares means
#' @export
do_t.test <- function(df, value, key=NULL, ...){
  value_col <- col_name(substitute(value))
  with_key <- !is.null(substitute(key))

  if(with_key){
    # make a formula for two sample t-test
    # value_col is values and key_col is labels in which grouop the values are in
    key_col <- col_name(substitute(key))
    # this creates a formula like `val`~`group key`
    fml <- as.formula(paste(paste("`", value_col, "`", sep=""), paste("`", key_col, "`", sep=""), sep="~"))
  }
  # otherwise, one sample t-test from values in value_col is executed

  grouped_col <- grouped_by(df)

  model_col <- avoid_conflict(grouped_col, "model")

  # this is executed on each group
  do_t.test_each <- function(df, ...){
    if(with_key){
      # use formula (`value_col`~`key_col`) for two sample t-test
      model <- tryCatch({
        t.test(data=df, fml, ...)
      }, error = function(e){
        if(e$message == "grouping factor must have exactly 2 levels"){
          stop("Group Column has to have 2 unique values")
        }
        stop(e$message)
      })
    } else {
      # use df[[value_col]] for one sample t-test (comparison with indicated normal distribution)
      model <- t.test(df[[value_col]], ...)
    }
    ret <- broom::tidy(model)

    # convert from factor to character
    ret[["method"]] <- as.character(ret[["method"]])
    ret[["alternative"]] <- as.character(ret[["alternative"]])

    # change column names
    col_names <- avoid_conflict(grouped_col, vapply(colnames(ret), function(name){
      switch (name,
              estimate = "effect_size",
              estimate1 = "mean1",
              estimate2 = "mean2",
              statistic = "t.value",
              parameter = "digrees_of_freedom",
              name
      )
    }, FUN.VALUE = ""))

    colnames(ret) <- col_names
    ret
  }

  df %>% dplyr::do_(.dots=setNames(list(~do_t.test_each(df = ., ...)), model_col)) %>% tidyr::unnest_(model_col)
}

#' wrapper for var.test, which compares variances
#' @export
do_var.test <- function(df, value, key, ...){
  value_col <- col_name(substitute(value))
  key_col <- col_name(substitute(key))
  fml <- as.formula(paste(paste("`", value_col, "`", sep=""), paste("`", key_col, "`", sep=""), sep="~"))

  grouped_col <- grouped_by(df)

  model_col <- avoid_conflict(grouped_col, "model")

  # this is executed on each group
  do_var.test_each <- function(df, ...){
    model <- tryCatch({
      var.test(data=df, fml, ...)
    }, error = function(e){
      if(e$message == "grouping factor must have exactly 2 levels"){
        stop("Group Column has to have 2 unique values")
      }
      stop(e$message)
    })
    ret <- broom::tidy(model)

    ret[["method"]] <- as.character(ret[["method"]])
    ret[["alternative"]] <- as.character(ret[["alternative"]])

    # change column names
    col_names <- avoid_conflict(grouped_col, vapply(colnames(ret), function(name){
      switch (name,
              estimate = "variance_ratio",
              num.df = "numerator_degrees_of_freedom",
              denom.df = "denominator_degrees_of_freedom",
              statistic = "f.value",
              name
      )
    }, FUN.VALUE = ""))

    colnames(ret) <- col_names
    ret
  }

  df %>% dplyr::do_(.dots=setNames(list(~do_var.test_each(df = ., ...)), model_col)) %>% tidyr::unnest_(model_col)
}

#' Non standard evaluation version of do_chisq.test_
#' @export
do_chisq.test <- function(df, ...,
                          skv = NULL,
                          fill = 0,
                          fun.aggregate = mean,
                          correct = TRUE,
                          p = NULL,
                          rescale.p = FALSE,
                          simulate.p.value = FALSE,
                          B = 2000){
  cols <- if(is.null(skv)){
    # if skv is not indicated, selected area is regarded as matrix
    select_dots <- lazyeval::lazy_dots(...)
    ret <- evaluate_select(df, select_dots, excluded = grouped_by(df))
  }else{
    # if skv is indicated, cols won't be used
    NULL
  }
  do_chisq.test_(df,
                 selected_cols = cols,
                 skv = skv,
                 correct = correct,
                 p = p,
                 rescale.p = rescale.p,
                 simulate.p.value = simulate.p.value,
                 B = B)
}

#' chisq.test wrapper
#' @export
do_chisq.test_ <- function(df,
                           selected_cols = c(),
                           skv = NULL,
                           fill = 0,
                           fun.aggregate = mean,
                           correct = TRUE,
                           p = NULL,
                           rescale.p = FALSE,
                           simulate.p.value = FALSE,
                           B = 2000){

  chisq.test_each <- function(df, ...) {
    x <- NULL
    y <- NULL
    if (is.null(skv)){
      # if skv is not indicated, selected area is regarded as matrix
      x <- df[, selected_cols] %>% as.matrix()
    } else {
      if(length(skv) == 1){
        x <- df[[skv[[1]]]]
      } else if (length(skv) == 2) {
        x <- df[[skv[[1]]]]
        y <- df[[skv[[2]]]]
      } else if (length(skv) == 3){
        # casted matrix
        if(!is.numeric(df[[skv[[3]]]])){
          stop("value column must be numeric")
        }
        x <- simple_cast(df, skv[[1]], skv[[2]], skv[[3]], fill = fill, fun.aggregate = fun.aggregate)
      } else {
        stop("length of skv must be between 1 to 3")
      }
    }
    if (is.null(p)){
      # default of p from chisq.test
      p <- rep(1/length(x), length(x))
    }
    chisq.test(x = x, y = y,
               correct = correct,
               p = p,
               rescale.p = rescale.p,
               simulate.p.value = simulate.p.value,
               B = B) %>%
      broom::glance()
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(colnames(df), "tmp")
  df %>%
    dplyr::do_(.dots = setNames(list(~chisq.test_each(.)), tmp_col)) %>%
    tidyr::unnest_(tmp_col)
}
