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
      # make key column factor
      # so that which key
      # the result columns (estimate1 and estimate2)
      # are from can be clear
      df[[key_col]] <- as.factor(df[[key_col]])
      mean_col_names <- paste("mean_", levels(df[[key_col]]), sep = "")

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

    # "two.sided" should be "two sided" because "two.sided" is confused as a url by Exploratory Desktop
    # this can be "greater" or "lower" but they are okay.
    ret[["alternative"]] <- ifelse(ret[["alternative"]] == "two.sided", "two sided", ret[["alternative"]])

    # change column names
    col_names <- avoid_conflict(grouped_col, vapply(colnames(ret), function(name){
      switch (name,
              estimate = "effect_size",
              estimate1 = mean_col_names[[1]],
              estimate2 = mean_col_names[[2]],
              statistic = "t.value",
              parameter = "degrees_of_freedom",
              name
      )
    }, FUN.VALUE = ""))

    colnames(ret) <- col_names
    ret
  }

  df %>%
    dplyr::do_(.dots=setNames(list(~do_t.test_each(df = ., ...)), model_col)) %>%
    dplyr::ungroup() %>%
    unnest_with_drop_(model_col)
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

    # "two.sided" should be "two sided" because "two.sided" is confused as a url by Exploratory Desktop
    # this can be "greater" or "lower" but they are okay.
    ret[["alternative"]] <- ifelse(ret[["alternative"]] == "two.sided", "two sided", ret[["alternative"]])

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

  df %>%
    dplyr::do_(.dots=setNames(list(~do_var.test_each(df = ., ...)), model_col)) %>%
    dplyr::ungroup() %>%
    unnest_with_drop_(model_col)
}

#' Non standard evaluation version of do_chisq.test_
#' @export
do_chisq.test <- function(df, ...,
                          correct = TRUE,
                          p = NULL,
                          rescale.p = TRUE,
                          simulate.p.value = FALSE,
                          B = 2000){
  select_dots <- lazyeval::lazy_dots(...)
  cols <- evaluate_select(df, select_dots, excluded = grouped_by(df))
  # p should be able to be NSE column name or numeric vector
  # , so evaluated lazily
  lazy_p <- lazyeval::lazy(p)
  p <- lazyeval::lazy_eval(lazy_p, data = df)

  do_chisq.test_(df,
                 selected_cols = cols,
                 correct = correct,
                 p = p,
                 rescale.p = rescale.p,
                 simulate.p.value = simulate.p.value,
                 B = B)
}

#' chisq.test wrapper
#' @param df Data frame to be tested.
#' @param selected_cols Names of columns of categories.
#' @param correct Whether continuity correction will be applied for 2 by 2 tables.
#' @param p This works when one column is selected. A column to be considered as probability to be tested.
#' If NULL, it's considered as uniform distribution.
#' @param rescale.p If TRUE, p is rescaled to sum to 1. If FALSE and p doesn't sum to 1, it causes an error.
#' @param simulate.p.value Whether p value should be calculated by Monte Carlo simulation.
#' @param B This works only when simulate.p.value is TRUE. The number of replicates for Monte Carlo test.
#' @export
do_chisq.test_ <- function(df,
                           selected_cols = c(),
                           correct = TRUE,
                           p = NULL,
                           rescale.p = TRUE,
                           simulate.p.value = FALSE,
                           B = 2000){

  chisq.test_each <- function(df, ...) {
    x <- df[, selected_cols] %>% as.matrix()
    if (is.null(p)){
      # default of p from chisq.test
      p <- rep(1/length(x), length(x))
    }
    chisq.test(x = x,
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
    dplyr::ungroup() %>%
    unnest_with_drop_(tmp_col)
}

#' @export
exp_chisq <- function(df, var1, var2, value = NULL, ...) {
  var1_col <- col_name(substitute(var1))
  var2_col <- col_name(substitute(var2))
  value_col <- col_name(substitute(value))
  grouped_col <- grouped_by(df)

  formula = as.formula(paste0('`', var1_col, '`~`', var2_col, '`'))
  pivotted_df <- pivot_(df, formula, value_col = value_col, fun.aggregate = sum, fill = 0)

  chisq.test_each <- function(df) {
    if (!is.null(grouped_col)) {
      df <- df %>% select(-!!rlang::sym(grouped_col))
    }
    df <- df %>% column_to_rownames(var=var1_col)
    x <- df %>% as.matrix()
    model <- chisq.test(x = x, ...)
    # add variable name info to the model
    model$var1 <- var1_col
    model$var2 <- var2_col
    class(model) <- c("chisq_exploratory", class(model))
    model
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(colnames(pivotted_df), "model")
  ret <- pivotted_df %>%
    dplyr::do_(.dots = setNames(list(~chisq.test_each(.)), tmp_col))
  ret
}

#' @export
tidy.chisq_exploratory <- function(x, type = "observed") {
  if (type == "observed") {
    ret <- as.data.frame(x$observed)
    ret <- ret %>% rownames_to_column(var = x$var1)
  }
  if (type == "residuals") {
    ret <- as.data.frame(x$residuals)
    ret <- ret %>% rownames_to_column(var = x$var1)
  }
  ret
}
