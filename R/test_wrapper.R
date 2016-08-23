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

    # otherwise, one sample t-test from values in value_col is executed
  }

  grouped_col <- grouped_by(df)

  model_col <- avoid_conflict(grouped_col, "model")

  # this is executed on each group
  do_t.test_each <- function(df, ...){
    if(with_key){
      model <- t.test(data=df, fml, ...)
    } else {
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
    model <- var.test(data=df, fml, ...)
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

