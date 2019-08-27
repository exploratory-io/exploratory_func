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
              `num df` = "numerator_degrees_of_freedom",
              `denom df` = "denominator_degrees_of_freedom",
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

#' Chi-Square test wrapper for Analytics View
#' @export
exp_chisq <- function(df, var1, var2, value = NULL, func1 = NULL, func2 = NULL, fun.aggregate = sum, correct = FALSE, sig.level = 0.05, w = NULL, power = NULL, beta = NULL, ...) {
  if (!is.null(power) && !is.null(beta) && (power + beta != 1.0)) {
    stop("Specify only one of Power or Probability of Type 2 Error, or they must add up to 1.0.")
  }
  if (is.null(power) && !is.null(beta)) {
    power <- 1.0 - beta
  }
  # We are turning off Yates's correction by default because...
  # 1. It seems that it is commonly discussed that it is overly conservative and not necessary.
  #    https://en.wikipedia.org/wiki/Yates%27s_correction_for_continuity
  #    https://aue.repo.nii.ac.jp/?action=repository_uri&item_id=785&file_id=15&file_no=1
  # 2. With Yates's correction residuals do not add up to chi-square value, which makes contributions not adding up to 100%.
  var1_col <- col_name(substitute(var1))
  var2_col <- col_name(substitute(var2))
  value_col <- col_name(substitute(value))
  grouped_col <- grouped_by(df)

  if (!is.null(func1)) {
    if (lubridate::is.Date(df[[var1_col]]) || lubridate::is.POSIXct(df[[var1_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var1_col) := extract_from_date(!!rlang::sym(var1_col), type=!!func1))
    }
    else if (is.numeric(df[[var1_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var1_col) := extract_from_numeric(!!rlang::sym(var1_col), type=!!func1))
    }
  }
  if (!is.null(func2)) {
    if (lubridate::is.Date(df[[var2_col]]) || lubridate::is.POSIXct(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_date(!!rlang::sym(var2_col), type=!!func2))
    }
    else if (is.numeric(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_numeric(!!rlang::sym(var2_col), type=!!func2))
    }
  }
  
  if (n_distinct(df[[var1_col]]) < 2) {
    stop(paste0("Variable Column (", var1_col, ") has to have 2 or more kinds of values."))
  }
  if (n_distinct(df[[var2_col]]) < 2) {
    stop(paste0("Variable Column (", var2_col, ") has to have 2 or more kinds of values."))
  }

  var1_levels <- NULL
  var2_levels <- NULL
  # preserve class of var1 and var2 so that we can cast them back to original class later.
  var1_class <- class(df[[var1_col]])
  var2_class <- class(df[[var2_col]])
  if (is.factor(df[[var1_col]])) {
    var1_levels <- levels(df[[var1_col]])
  }
  if (is.factor(df[[var2_col]])) {
    var2_levels <- levels(df[[var2_col]])
  }

  formula = as.formula(paste0('`', var1_col, '`~`', var2_col, '`'))

  chisq.test_each <- function(df) {
    df <- df %>% dplyr::group_by(!!rlang::sym(var1_col), !!rlang::sym(var2_col))
    if (is.null(value_col)) {
      df <- df %>% dplyr::summarize(.temp_value_col=n())
    }
    else {
      #TODO: handle name conflict with .temp_value_col and group cols.
      if (identical(sum, fun.aggregate)) {
        df <- df %>% dplyr::summarize(.temp_value_col=fun.aggregate(!!rlang::sym(value_col), na.rm=TRUE))
      }
      else {
        # Possible fun.aggregate are, length, n_distinct, false_count (count for TRUE is done by sum),
        # na_count, non_na_count. They can/should be run without na.rm=TRUE.
        df <- df %>% dplyr::summarize(.temp_value_col=fun.aggregate(!!rlang::sym(value_col)))
      }
    }
    #TODO: spread creates column named "<NA>". For consistency on UI, we want "(NA)".
    df <- df %>% ungroup() %>% spread(key = !!rlang::sym(var2_col), value = .temp_value_col, fill=0)
    df <- df %>% mutate(!!rlang::sym(var1_col):=forcats::fct_explicit_na(as.factor(!!rlang::sym(var1_col)), na_level = "(NA)"))

    df <- df %>% tibble::column_to_rownames(var=var1_col)
    x <- df %>% as.matrix()
    model <- chisq.test(x = x, correct = correct, ...)
    # Calculate Cohen's w from actual data.
    cohens_w <- calculate_cohens_w(model$statistic, sum(x))
    if (is.null(w)) {
      # If w is not specified, use the one calculated from data.
      cohens_w_to_detect <- cohens_w
    }
    else {
      cohens_w_to_detect <- w
    }
    # add variable name info to the model
    model$var1 <- var1_col
    model$var2 <- var2_col
    model$var1_class <- var1_class
    model$var2_class <- var2_class
    model$var1_levels <- var1_levels
    model$var2_levels <- var2_levels
    model$sig.level <- sig.level
    model$cohens_w <- cohens_w
    model$cohens_w_to_detect <- cohens_w_to_detect
    model$power <- power
    class(model) <- c("chisq_exploratory", class(model))
    model
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(colnames(df), "model") #TODO: Conflict should be an issue only with group_by columns.
  ret <- df %>%
    dplyr::do_(.dots = setNames(list(~chisq.test_each(.)), tmp_col))
  ret
}

#' @export
tidy.chisq_exploratory <- function(x, type = "observed") {
  if (type == "observed") {
    ret <- as.data.frame(x$observed)
    ret <- ret %>% tibble::rownames_to_column(var = x$var1)
  }
  if (type == "residuals") {
    obs_df <- as.data.frame(x$observed)
    obs_df <- obs_df %>% tibble::rownames_to_column(var = x$var1)
    obs_df <- obs_df %>% tidyr::gather(!!rlang::sym(x$var2), "observed", -!!rlang::sym(x$var1))

    expected_df <- as.data.frame(x$expected)
    expected_df <- expected_df %>% tibble::rownames_to_column(var = x$var1)
    expected_df <- expected_df %>% tidyr::gather(!!rlang::sym(x$var2), "expected", -!!rlang::sym(x$var1))

    resid_df <- as.data.frame(x$residuals)
    resid_df <- resid_df %>% tibble::rownames_to_column(var = x$var1)
    resid_df <- resid_df %>% tidyr::gather(!!rlang::sym(x$var2), "residual", -!!rlang::sym(x$var1))

    resid_raw_df <- as.data.frame(x$observed - x$expected) # x$residual is standardized, but here, take raw difference between observed and expected. 
    resid_raw_df <- resid_raw_df %>% tibble::rownames_to_column(var = x$var1)
    resid_raw_df <- resid_raw_df %>% tidyr::gather(!!rlang::sym(x$var2), "residual_raw", -!!rlang::sym(x$var1))

    resid_ratio_df <- as.data.frame((x$observed - x$expected)/x$expected) # x$residual is standardized, but here, take raw difference between observed and expected. 
    resid_ratio_df <- resid_ratio_df %>% tibble::rownames_to_column(var = x$var1)
    resid_ratio_df <- resid_ratio_df %>% tidyr::gather(!!rlang::sym(x$var2), "residual_ratio", -!!rlang::sym(x$var1))

    ret <- obs_df %>% left_join(expected_df, by=c(x$var1, x$var2)) # join expected column 
    ret <- ret %>% left_join(resid_df, by=c(x$var1, x$var2)) # join expected column
    ret <- ret %>% left_join(resid_raw_df, by=c(x$var1, x$var2)) # join residual_raw column
    ret <- ret %>% left_join(resid_ratio_df, by=c(x$var1, x$var2)) # join residual_ratio column
    if (is.nan(x$statistic) || x$statistic <= 0) {
      ret <- ret %>% mutate(contrib = 0) # avoid division by 0
    }
    else {
      ret <- ret %>% mutate(contrib = 100*residual^2/(!!(x$statistic))) # add percent contribution too.
    }

    if (!is.null(x$var1_levels)) {
      ret[[x$var1]] <- factor(ret[[x$var1]], levels=x$var1_levels)
    }
    if (!is.null(x$var2_levels)) {
      ret[[x$var2]] <- factor(ret[[x$var2]], levels=x$var2_levels)
    }
    # if original class of var1, var2 was logical, return them as logical.
    if ("logical" %in% x$var1_class) {
      ret[[x$var1]] <- as.logical(ret[[x$var1]])
    }
    else if ("integer" %in% x$var1_class) {
      ret[[x$var1]] <- as.integer(ret[[x$var1]])
    }
    else if ("numeric" %in% x$var1_class) {
      ret[[x$var1]] <- as.numeric(ret[[x$var1]])
    }

    if ("logical" %in% x$var2_class) {
      ret[[x$var2]] <- as.logical(ret[[x$var2]])
    }
    else if ("integer" %in% x$var2_class) {
      ret[[x$var2]] <- as.integer(ret[[x$var2]])
    }
    else if ("numeric" %in% x$var2_class) {
      ret[[x$var2]] <- as.numeric(ret[[x$var2]])
    }
  }
  ret
}

#' @export
glance.chisq_exploratory <- function(x) {
  # ret <- x %>% broom:::glance.htest() # for some reason this does not work. just do it like following.
  ret <- data.frame(statistic=x$statistic, parameter=x$parameter, p.value=x$p.value)
  N <- sum(x$observed) # Total number of observations (rows).
  note <- NULL
  if (is.null(x$power)) {
    # If power is not specified in the arguments, estimate current power.
    # x$parameter is degree of freedom.
    if (!is.na(x$cohens_w_to_detect)) { # It is possible that x$cohens_w_to_detect is NA. Avoid calling pwr.chisq.test not to hit an error.
      # pwr functions returns value for the missing arg, which in this case is power.
      tryCatch({ # pwr function can return error from equation resolver. Catch it rather than stopping the whole thing.
        power_res <- pwr::pwr.chisq.test(df = x$parameter, N = N, w = x$cohens_w_to_detect, sig.level = x$sig.level)
        power_val <- power_res$power
      }, error = function(e) {
        note <<- e$message
        power_val <<- NA_real_
      })
    }
    else {
      note <- "Could not calculate Cohhen's w." 
      power_val <- NA_real_
    }
    ret <- ret %>% dplyr::mutate(w=!!(x$cohens_w), power=!!power_val, beta=1.0-!!power_val, n=!!N)
    ret <- ret %>% rename(`Chi-Square`=statistic,
                          `Degree of Freedom`=parameter,
                          `P Value`=p.value,
                          `Effect Size (Cohen's w)`=w,
                          `Power`=power,
                          `Probability of Type 2 Error`=beta,
                          `Number of Rows`=n)
  }
  else {
    # If required power is specified in the arguments, estimate required sample size. 
    # pwr functions returns value for the missing arg, which in this case is sample size (N). 
    tryCatch({ # pwr function can return error from equation resolver. Catch it rather than stopping the whole thing.
      power_res <- pwr::pwr.chisq.test(df = x$parameter, w = x$cohens_w_to_detect, sig.level = x$sig.level, power = x$power)
      required_sample_size <- power_res$N
    }, error = function(e) {
      note <<- e$message
      required_sample_size <<- NA_real_
    })
    ret <- ret %>% dplyr::mutate(w=!!(x$cohens_w), power=!!(x$power), beta=1.0-!!(x$power), n=!!N, required_n=!!required_sample_size)
    ret <- ret %>% rename(`Chi-Square`=statistic,
                          `Degree of Freedom`=parameter,
                          `P Value`=p.value,
                          `Effect Size (Cohen's w)`=w,
                          `Target Power`=power,
                          `Target Probability of Type 2 Error`=beta,
                          `Current Sample Size`=n,
                          `Required Sample Size`=required_n)
  }
  if (!is.null(note)) { # Add Note column, if there was an error from pwr function.
    ret <- ret %>% dplyr::mutate(Note=!!note)
  }
  ret
}


#' t-test wrapper for Analytics View
#' @export
#' @param conf.level - Level of confidence for confidence interval. Passed to t.test as part of ...
#' @param sig.level - Significance level for power analysis.
exp_ttest <- function(df, var1, var2, func2 = NULL, sig.level = 0.05, d = NULL, common_sd = NULL, diff_to_detect = NULL, power = NULL, beta = NULL, ...) {
  if (!is.null(power) && !is.null(beta) && (power + beta != 1.0)) {
    stop("Specify only one of Power or Probability of Type 2 Error, or they must add up to 1.0.")
  }
  if (is.null(power) && !is.null(beta)) {
    power <- 1.0 - beta
  }
  var1_col <- col_name(substitute(var1))
  var2_col <- col_name(substitute(var2))
  grouped_cols <- grouped_by(df)

  if (!is.null(func2)) {
    if (lubridate::is.Date(df[[var2_col]]) || lubridate::is.POSIXct(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_date(!!rlang::sym(var2_col), type=!!func2))
    }
    else if (is.numeric(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_numeric(!!rlang::sym(var2_col), type=!!func2))
    }
  }
  
  n_distinct_res <- n_distinct(df[[var2_col]]) # save n_distinct result to avoid repeating the relatively expensive call.
  if (n_distinct_res != 2) {
    if (n_distinct_res == 3 && any(is.na(df[[var2_col]]))) { # automatically filter NA to make number of category 2, if it is the 3rd category.
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var2_col)))
    }
    else {
      stop(paste0("Variable Column (", var2_col, ") has to have 2 kinds of values."))
    }
  }

  formula = as.formula(paste0('`', var1_col, '`~`', var2_col, '`'))

  ttest_each <- function(df) {
    if(length(grouped_cols) > 0) {
      n_distinct_res_each <- n_distinct(df[[var2_col]]) # check n_distinct again within group.
      if (n_distinct_res_each != 2) {
        return(NULL)
      }
    }
    # Calculate Cohen's d from data.
    cohens_d <- calculate_cohens_d(df[[var1_col]], df[[var2_col]])
    # Get size of Cohen's d to detect for power analysis.
    # If neither d nor diff_to_detect is specified, use the one calculated from data.
    if (is.null(d)) {
      if (is.null(diff_to_detect)) {
        # If neither d nor diff_to_detect is specified, calculate Cohen's d from data.
        cohens_d_to_detect <- cohens_d
      }
      else { # diff_to_detect is specified.
        if (is.null(common_sd)) {
          # If common SD is not specified, estimate from data, and use it to calculate Cohen's d
          cohens_d_to_detect <- diff_to_detect/calculate_common_sd(df[[var1_col]], df[[var2_col]])
        }
        else {
          cohens_d_to_detect <- diff_to_detect/common_sd
        }
      }
    }
    else {
      cohens_d_to_detect <- d
    }
    tryCatch({
      model <- t.test(formula, data = df, ...)
      class(model) <- c("ttest_exploratory", class(model))
      model$var1 <- var1_col
      model$var2 <- var2_col
      model$data <- df
      model$sig.level <- sig.level
      model$cohens_d <- cohens_d # model$d seems to be already used for something.
      model$cohens_d_to_detect <- cohens_d_to_detect
      model$power <- power
      model
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # Ignore the error if it is caused by subset of grouped data frame to show result of data frames that succeed.
        # For example, error can happen if one of the groups does not have both values (e.g. both TRUE and FALSE) of var2.
        NULL
      } else {
        stop(e)
      }
    })
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(colnames(df), "model")
  ret <- df %>%
    dplyr::do_(.dots = setNames(list(~ttest_each(.)), tmp_col))
  ret
}

#' @export
glance.ttest_exploratory <- function(x) {
  ret <- broom:::glance.htest(x) # for t-test. the returned content is same as tidy.
  ret
}

#' @export
tidy.ttest_exploratory <- function(x, type="model", conf_level=0.95) {
  if (type == "model") {
    note <- NULL
    ret <- broom:::tidy.htest(x)
    if (is.null(ret$estimate)) { # estimate is empty when var.equal = TRUE.
                                 # Looks like an issue from broom. Working it around.
      ret <- ret %>% dplyr::mutate(estimate = estimate1 - estimate2)
    }

    # Get sample sizes for the 2 groups (n1, n2).
    data_summary <- x$data %>% dplyr::group_by(!!rlang::sym(x$var2)) %>%
      dplyr::summarize(n_rows=length(!!rlang::sym(x$var1)))
    n1 <- data_summary$n_rows[[1]]
    n2 <- data_summary$n_rows[[2]]
    if (is.null(x$power)) {
      # If power is not specified in the arguments, estimate current power.
      # TODO: pwr functions does not seem to have argument for equal variance. Is it ok? 
      tryCatch({ # pwr function can return error from equation resolver. Catch it rather than stopping the whole thing.
        if (x$method == "Paired t-test") {
          # If paired, we should be able to assume n1 == n2.
          power_res <- pwr::pwr.t.test(n = n1, d = x$cohens_d_to_detect, sig.level = x$sig.level, type = "two.sample", alternative = x$alternative)
        }
        else {
          power_res <- pwr::pwr.t2n.test(n1 = n1, n2= n2, d = x$cohens_d_to_detect, sig.level = x$sig.level, alternative = x$alternative)
        }
        power_val <- power_res$power
      }, error = function(e) {
        note <- e$message
        power_val <<- NA_real_
      })

      ret <- ret %>% dplyr::select(statistic, p.value, parameter, estimate, conf.high, conf.low) %>%
        dplyr::mutate(d=!!(x$cohens_d), power=!!power_val, beta=1.0-!!power_val) %>%
        dplyr::rename(`t Ratio`=statistic,
                      `P Value`=p.value,
                      `Degree of Freedom`=parameter,
                      Difference=estimate,
                      `Conf High`=conf.high,
                      `Conf Low`=conf.low,
                      `Effect Size (Cohen's d)`=d,
                      `Power`=power,
                      `Probability of Type 2 Error`=beta)
    }
    else {
      # If required power is specified in the arguments, estimate required sample size. 
      # TODO: pwr functions does not seem to have argument for equal variance. Is it ok? 
      tryCatch({ # pwr function can return error from equation resolver. Catch it rather than stopping the whole thing.
        power_res <- pwr::pwr.t.test(d = x$cohens_d_to_detect, sig.level = x$sig.level, power = x$power, alternative = x$alternative)
        required_sample_size <- power_res$n
      }, error = function(e) {
        note <<- e$message
        required_sample_size <<- NA_real_
      })
      ret <- ret %>% dplyr::select(statistic, p.value, parameter, estimate, conf.high, conf.low) %>%
        dplyr::mutate(d=!!(x$cohens_d), power=!!(x$power), beta=1.0-!!(x$power)) %>%
        dplyr::mutate(current_sample_size=min(!!n1,!!n2), required_sample_size=required_sample_size) %>%
        dplyr::rename(`t Ratio`=statistic,
                      `P Value`=p.value,
                      `Degree of Freedom`=parameter,
                      Difference=estimate,
                      `Conf High`=conf.high,
                      `Conf Low`=conf.low,
                      `Effect Size (Cohen's d)`=d,
                      `Target Power`=power,
                      `Target Probability of Type 2 Error`=beta,
                      `Current Sample Size (Each Group)`=current_sample_size,
                      `Required Sample Size (Each Group)`=required_sample_size)
    }
    if (!is.null(note)) { # Add Note column, if there was an error from pwr function.
      ret <- ret %>% dplyr::mutate(Note=!!note)
    }
  }
  else if (type == "data_summary") { #TODO consolidate with code in tidy.anova_exploratory
    conf_threshold = 1 - (1 - conf_level)/2
    ret <- x$data %>% dplyr::group_by(!!rlang::sym(x$var2)) %>%
      dplyr::summarize(`Number of Rows`=length(!!rlang::sym(x$var1)),
                       Mean=mean(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Std Deviation`=sd(!!rlang::sym(x$var1), na.rm=TRUE),
                       # std error definition: https://www.rdocumentation.org/packages/plotrix/versions/3.7/topics/std.error
                       `Std Error of Mean`=sd(!!rlang::sym(x$var1), na.rm=TRUE)/sqrt(sum(!is.na(!!rlang::sym(x$var1)))),
                       # Note: Use qt (t distribution) instead of qnorm (normal distribution) here.
                       # For more detail take a look at 10.5.1 A slight mistake in the formula of "Learning Statistics with R" 
                       `Conf High` = Mean + `Std Error of Mean` * qt(p=!!conf_threshold, df=`Number of Rows`-1),
                       `Conf Low` = Mean - `Std Error of Mean` * qt(p=!!conf_threshold, df=`Number of Rows`-1),
                       `Minimum`=min(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Maximum`=max(!!rlang::sym(x$var1), na.rm=TRUE)) %>%
      dplyr::select(!!rlang::sym(x$var2),
                    `Number of Rows`,
                    Mean,
                    `Conf Low`,
                    `Conf High`,
                    `Std Error of Mean`,
                    `Std Deviation`,
                    `Minimum`,
                    `Maximum`)
  }
  else { # type == "data"
    ret <- x$data
  }
  ret
}

#' Wrapper for Wilcoxon rank sum test and signed-rank test for Analytics View
#' @export
#' @param conf.int - Whether to calculate estimate and confidence interval. Default FALSE. Passed to wilcox.test as part of ...
#' @param conf.level - Level of confidence for confidence interval. Passed to wilcox.test as part of ...
exp_wilcox <- function(df, var1, var2, func2 = NULL, ...) {
  var1_col <- col_name(substitute(var1))
  var2_col <- col_name(substitute(var2))
  grouped_cols <- grouped_by(df)

  if (!is.null(func2)) {
    if (lubridate::is.Date(df[[var2_col]]) || lubridate::is.POSIXct(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_date(!!rlang::sym(var2_col), type=!!func2))
    }
    else if (is.numeric(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_numeric(!!rlang::sym(var2_col), type=!!func2))
    }
  }
  
  n_distinct_res <- n_distinct(df[[var2_col]]) # save n_distinct result to avoid repeating the relatively expensive call.
  if (n_distinct_res != 2) {
    if (n_distinct_res == 3 && any(is.na(df[[var2_col]]))) { # automatically filter NA to make number of category 2, if it is the 3rd category.
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var2_col)))
    }
    else {
      stop(paste0("Variable Column (", var2_col, ") has to have 2 kinds of values."))
    }
  }

  formula = as.formula(paste0('`', var1_col, '`~`', var2_col, '`'))

  each_func <- function(df) {
    tryCatch({
      model <- wilcox.test(formula, data = df, ...)
      class(model) <- c("wilcox_exploratory", class(model))
      model$var1 <- var1_col
      model$var2 <- var2_col
      model$data <- df
      model
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # Ignore the error if it is caused by subset of grouped data frame to show result of data frames that succeed.
        # For example, error can happen if one of the groups does not have both values (e.g. both TRUE and FALSE) of var2.
        NULL
      } else {
        stop(e)
      }
    })
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(colnames(df), "model")
  ret <- df %>%
    dplyr::do_(.dots = setNames(list(~each_func(.)), tmp_col))
  ret
}

#' @export
tidy.wilcox_exploratory <- function(x, type="model", conf_level=0.95) {
  if (type == "model") {
    note <- NULL
    ret <- broom:::tidy.htest(x)
    if (!is.null(x$estimate)) { # Result is with estimate and confidence interval
      ret <- ret %>% dplyr::select(statistic, p.value, estimate, conf.high, conf.low, method)
    }
    else {
      ret <- ret %>% dplyr::select(statistic, p.value, method)
    }

    # Switch the name of statistic based on the type of performed test.
    if (stringr::str_detect(ret$method[[1]], "signed rank test")) {
      ret <- ret %>% dplyr::rename(`W Statistic`=statistic)
    }
    else if (stringr::str_detect(ret$method[[1]], "rank sum test")) {
      ret <- ret %>% dplyr::rename(`U Statistic`=statistic)
    }

    if (!is.null(x$estimate)) { # Result is with estimate and confidence interval
      ret <- ret %>% dplyr::rename(`P Value`=p.value,
                     Difference=estimate,
                     `Conf High`=conf.high,
                     `Conf Low`=conf.low,
                     `Method`=method)
    }
    else {
      ret <- ret %>% dplyr::rename(`P Value`=p.value,
                     `Method`=method)
    }

    if (!is.null(note)) { # Code to add Note column if there was an error. Not used for this particular function yet.
      ret <- ret %>% dplyr::mutate(Note=!!note)
    }
  }
  else if (type == "data_summary") { #TODO consolidate with code in tidy.anova_exploratory
    conf_threshold = 1 - (1 - conf_level)/2
    ret <- x$data %>% dplyr::group_by(!!rlang::sym(x$var2)) %>%
      dplyr::summarize(`Number of Rows`=length(!!rlang::sym(x$var1)),
                       Mean=mean(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Std Deviation`=sd(!!rlang::sym(x$var1), na.rm=TRUE),
                       # std error definition: https://www.rdocumentation.org/packages/plotrix/versions/3.7/topics/std.error
                       `Std Error of Mean`=sd(!!rlang::sym(x$var1), na.rm=TRUE)/sqrt(sum(!is.na(!!rlang::sym(x$var1)))),
                       # Note: Use qt (t distribution) instead of qnorm (normal distribution) here.
                       # For more detail take a look at 10.5.1 A slight mistake in the formula of "Learning Statistics with R" 
                       `Conf High` = Mean + `Std Error of Mean` * qt(p=conf_level, df=`Number of Rows`-1),
                       `Conf Low` = Mean - `Std Error of Mean` * qt(p=conf_level, df=`Number of Rows`-1),
                       `Minimum`=min(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Maximum`=max(!!rlang::sym(x$var1), na.rm=TRUE)) %>%
      dplyr::select(!!rlang::sym(x$var2),
                    `Number of Rows`,
                    Mean,
                    `Conf Low`,
                    `Conf High`,
                    `Std Error of Mean`,
                    `Std Deviation`,
                    `Minimum`,
                    `Maximum`)
  }
  else { # type == "data"
    ret <- x$data
  }
  ret
}

#' ANOVA wrapper for Analytics View
#' @export
exp_anova <- function(df, var1, var2, func2 = NULL, sig.level = 0.05, f = NULL, power = NULL, beta = NULL, ...) {
  if (!is.null(power) && !is.null(beta) && (power + beta != 1.0)) {
    stop("Specify only one of Power or Probability of Type 2 Error, or they must add up to 1.0.")
  }
  if (is.null(power) && !is.null(beta)) {
    power <- 1.0 - beta
  }
  var1_col <- col_name(substitute(var1))
  var2_col <- col_name(substitute(var2))
  grouped_cols <- grouped_by(df)

  if (!is.null(func2)) {
    if (lubridate::is.Date(df[[var2_col]]) || lubridate::is.POSIXct(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_date(!!rlang::sym(var2_col), type=!!func2))
    }
    else if (is.numeric(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_numeric(!!rlang::sym(var2_col), type=!!func2))
    }
  }
  
  if (n_distinct(df[[var2_col]]) < 2) {
    stop(paste0("Variable Column (", var2_col, ") has to have 2 or more kinds of values."))
  }

  formula = as.formula(paste0('`', var1_col, '`~`', var2_col, '`'))

  anova_each <- function(df) {
    if(length(grouped_cols) > 0) {
      # Check n_distinct again within group.
      # Group with NA and another category does not seem to work well with aov. Eliminating such case too. TODO: We could replace NA with an explicit level.
      n_distinct_res_each <- n_distinct(df[[var2_col]], na.rm=TRUE)
      if (n_distinct_res_each < 2) {
        return(NULL)
      }
    }
    tryCatch({
      model <- aov(formula, data = df, ...)
      # calculate Cohen's f from actual data
      model$cohens_f <- calculate_cohens_f(df[[var1_col]], df[[var2_col]])
      if (is.null(f)) {
        model$cohens_f_to_detect <- model$cohens_f
      }
      else {
        model$cohens_f_to_detect <- f
      }
      class(model) <- c("anova_exploratory", class(model))
      model$var1 <- var1_col
      model$var2 <- var2_col
      model$data <- df
      model$sig.level <- sig.level
      model$power <- power
      model
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # Ignore the error if it is caused by subset of grouped data frame to show result of data frames that succeed.
        # For example, error can happen if one of the groups has only one unique value in its set of var2.
        NULL
      } else {
        stop(e)
      }
    })
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(colnames(df), "model")
  ret <- df %>%
    dplyr::do_(.dots = setNames(list(~anova_each(.)), tmp_col))
  ret
}

#' @export
glance.anova_exploratory <- function(x) {
  ret <- broom:::tidy.aov(x) %>% slice(1:1) # there is no glance.aov. take first row of tidy.aov.
  ret$term[[1]] <- names(x$xlevels)[[1]]
  ret
}

#' @export
tidy.anova_exploratory <- function(x, type="model", conf_level=0.95) {
  if (type == "model") {
    note <- NULL
    ret <- broom:::tidy.aov(x)
    ret$term[[1]] <- names(x$xlevels)[[1]]
    # Get number of groups (k) , and the minimum sample size amoung those groups (min_n_rows).
    data_summary <- x$data %>% dplyr::group_by(!!rlang::sym(x$var2)) %>%
      dplyr::summarize(n_rows=length(!!rlang::sym(x$var1))) %>%
      dplyr::summarize(min_n_rows=min(n_rows), k=n())
    k <- data_summary$k
    # Using minimum group sample size as the sample size for power calculation.
    # Reference: https://www.theanalysisfactor.com/when-unequal-sample-sizes-are-and-are-not-a-problem-in-anova/
    min_n_rows <- data_summary$min_n_rows

    if (is.null(x$power)) {
      # If power is not specified in the arguments, estimate current power.
      tryCatch({ # pwr function can return error from equation resolver. Catch it rather than stopping the whole thing.
        power_res <- pwr::pwr.anova.test(k = k, n= min_n_rows, f = x$cohens_f_to_detect, sig.level = x$sig.level)
        power_val <- power_res$power
      }, error = function(e) {
        note <<- e$message
        power_val <<- NA_real_
      })
      ret <- ret %>% dplyr::select(term, statistic, p.value, df, sumsq, meansq) %>%
        dplyr::mutate(f=c(!!(x$cohens_f), NA_real_), power=c(!!power_val, NA_real_), beta=c(1.0-!!power_val, NA_real_)) %>%
        dplyr::rename(Term=term,
                      `F Ratio`=statistic,
                      `P Value`=p.value,
                      `Degree of Freedom`=df,
                      `Sum of Squares`=sumsq,
                      `Mean Square`=meansq,
                      `Effect Size (Cohen's f)`=f,
                      `Power`=power,
                      `Probability of Type 2 Error`=beta)
    }
    else {
      # If required power is specified in the arguments, estimate required sample size. 
      tryCatch({ # pwr function can return error from equation resolver. Catch it rather than stopping the whole thing.
        power_res <- pwr::pwr.anova.test(k = k, f = x$cohens_f_to_detect, sig.level = x$sig.level, power = x$power)
        required_sample_size <- power_res$n
      }, error = function(e) {
        note <<- e$message
        required_sample_size <<- NA_real_
      })
      ret <- ret %>% dplyr::select(term, statistic, p.value, df, sumsq, meansq) %>%
        dplyr::mutate(f=c(!!(x$cohens_f), NA_real_), power=c(!!(x$power), NA_real_), beta=c(1.0-!!(x$power), NA_real_)) %>%
        dplyr::mutate(current_sample_size=!!min_n_rows, required_sample_size=c(!!required_sample_size, NA_real_)) %>%
        dplyr::rename(Term=term,
                      `F Ratio`=statistic,
                      `P Value`=p.value,
                      `Degree of Freedom`=df,
                      `Sum of Squares`=sumsq,
                      `Mean Square`=meansq,
                      `Effect Size (Cohen's f)`=f,
                      `Target Power`=power,
                      `Target Probability of Type 2 Error`=beta,
                      `Current Sample Size (Each Group)`=current_sample_size,
                      `Required Sample Size (Each Group)`=required_sample_size)
    }
    if (!is.null(note)) { # Add Note column, if there was an error from pwr function.
      ret <- ret %>% dplyr::mutate(Note=!!note)
    }
  }
  else if (type == "data_summary") { #TODO consolidate with code in tidy.ttest_exploratory
    conf_threshold = 1 - (1 - conf_level)/2
    ret <- x$data %>% dplyr::group_by(!!rlang::sym(x$var2)) %>%
      dplyr::summarize(`Number of Rows`=length(!!rlang::sym(x$var1)),
                       Mean=mean(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Std Deviation`=sd(!!rlang::sym(x$var1), na.rm=TRUE),
                       # std error definition: https://www.rdocumentation.org/packages/plotrix/versions/3.7/topics/std.error
                       `Std Error of Mean`=sd(!!rlang::sym(x$var1), na.rm=TRUE)/sqrt(sum(!is.na(!!rlang::sym(x$var1)))),
                       # Note: Use qt (t distribution) instead of qnorm (normal distribution) here.
                       # For more detail take a look at 10.5.1 A slight mistake in the formula of "Learning Statistics with R" 
                       `Conf High` = Mean + `Std Error of Mean` * qt(p=!!conf_threshold, df=`Number of Rows`-1),
                       `Conf Low` = Mean - `Std Error of Mean` * qt(p=!!conf_threshold, df=`Number of Rows`-1),
                       `Minimum`=min(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Maximum`=max(!!rlang::sym(x$var1), na.rm=TRUE)) %>%
      dplyr::select(!!rlang::sym(x$var2),
                    `Number of Rows`,
                    Mean,
                    `Conf Low`,
                    `Conf High`,
                    `Std Error of Mean`,
                    `Std Deviation`,
                    `Minimum`,
                    `Maximum`)
  }
  else { # type == "data"
    ret <- x$data
  }
  ret
}

#' Kruskal-Wallis wrapper for Analytics View
#' @export
exp_kruskal <- function(df, var1, var2, func2 = NULL, ...) {
  var1_col <- col_name(substitute(var1))
  var2_col <- col_name(substitute(var2))
  grouped_cols <- grouped_by(df)

  if (!is.null(func2)) {
    if (lubridate::is.Date(df[[var2_col]]) || lubridate::is.POSIXct(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_date(!!rlang::sym(var2_col), type=!!func2))
    }
    else if (is.numeric(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_numeric(!!rlang::sym(var2_col), type=!!func2))
    }
  }
  
  if (n_distinct(df[[var2_col]]) < 2) {
    stop(paste0("Variable Column (", var2_col, ") has to have 2 or more kinds of values."))
  }

  formula = as.formula(paste0('`', var1_col, '`~`', var2_col, '`'))

  each_func <- function(df) {
    tryCatch({
      model <- kruskal.test(formula, data = df, ...)
      class(model) <- c("kruskal_exploratory", class(model))
      model$var1 <- var1_col
      model$var2 <- var2_col
      model$data <- df
      model
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # Ignore the error if it is caused by subset of grouped data frame to show result of data frames that succeed.
        # For example, error can happen if one of the groups has only one unique value in its set of var2.
        NULL
      } else {
        stop(e)
      }
    })
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(colnames(df), "model")
  ret <- df %>%
    dplyr::do_(.dots = setNames(list(~each_func(.)), tmp_col))
  ret
}

tidy.kruskal_exploratory <- function(x, type="model", conf_level=0.95) {
  if (type == "model") {
    note <- NULL
    ret <- broom:::tidy.htest(x)
    ret <- ret %>% dplyr::select(statistic, p.value, method)
    ret <- ret %>% dplyr::rename(`H Statistic` = statistic,
                                 `P Value`=p.value,
                                 `Method`=method)
    if (!is.null(note)) { # Add Note column, if there was an error from pwr function.
      ret <- ret %>% dplyr::mutate(Note=!!note)
    }
  }
  else if (type == "data_summary") { #TODO consolidate with code in tidy.ttest_exploratory
    conf_threshold = 1 - (1 - conf_level)/2
    ret <- x$data %>% dplyr::group_by(!!rlang::sym(x$var2)) %>%
      dplyr::summarize(`Number of Rows`=length(!!rlang::sym(x$var1)),
                       Mean=mean(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Std Deviation`=sd(!!rlang::sym(x$var1), na.rm=TRUE),
                       # std error definition: https://www.rdocumentation.org/packages/plotrix/versions/3.7/topics/std.error
                       `Std Error of Mean`=sd(!!rlang::sym(x$var1), na.rm=TRUE)/sqrt(sum(!is.na(!!rlang::sym(x$var1)))),
                       # Note: Use qt (t distribution) instead of qnorm (normal distribution) here.
                       # For more detail take a look at 10.5.1 A slight mistake in the formula of "Learning Statistics with R" 
                       `Conf High` = Mean + `Std Error of Mean` * qt(p=!!conf_threshold, df=`Number of Rows`-1),
                       `Conf Low` = Mean - `Std Error of Mean` * qt(p=!!conf_threshold, df=`Number of Rows`-1),
                       `Minimum`=min(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Maximum`=max(!!rlang::sym(x$var1), na.rm=TRUE)) %>%
      dplyr::select(!!rlang::sym(x$var2),
                    `Number of Rows`,
                    Mean,
                    `Conf Low`,
                    `Conf High`,
                    `Std Error of Mean`,
                    `Std Deviation`,
                    `Minimum`,
                    `Maximum`)
  }
  else { # type == "data"
    ret <- x$data
  }
  ret
}

# qqline function that does not draw line and instead return intercept and slope
qqline_data <- function (y, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7, ...) 
{
  stopifnot(length(probs) == 2, is.function(distribution))
  y <- quantile(y, probs, names = FALSE, type = qtype, na.rm = TRUE)
  x <- distribution(probs)
  if (datax) {
    slope <- diff(x)/diff(y)
    int <- x[1L] - slope * y[1L]
  }
  else {
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]
  }
  
  list(int, slope) # intercept and slope
}


#' @param n_sample - Downsample to this size before shapiro test. Note that this is not applied for qq data. 
#' @param n_sample_qq - Downsample qq-plot data down to this number.
#'                   This is to make sure qq-line part of the data would not be sampled out in qq scatter plot.
#'                   Default 4500 is to make room for qqline rows. (default sample size by scatter plot data query is 5000)
#' @export
exp_normality<- function(df, ...,
                         n_sample = 50,
                         n_sample_qq = 4500,
                         seed = 1
                         ) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  selected_cols <- dplyr::select_vars(names(df), !!! rlang::quos(...))
  
  shapiro_each <- function(df) {
    df.qq <- data.frame()
    df.qqline <- data.frame()
    df.model <- data.frame()
    for (col in selected_cols) {
      if (n_distinct(df[[col]], na.rm=TRUE) <= 1) {
        # skip if the column has only 1 unique value or only NAs, to avoid error.
        # TODO: show what happened in the summary table.
      }
      else {
        # set plot.it to FALSE to disable plotting (avoid launching another window)
        res <- stats::qqnorm(df[[col]], plot.it=FALSE)
        df.qq <- dplyr::bind_rows(df.qq, data.frame(x=res$x, y=res$y, col=col))

        # bind reference line data too.
        ref_res <- qqline_data(df[[col]])
        min_x <- min(res$x, na.rm=TRUE)
        max_x <- max(res$x, na.rm=TRUE)
        ref_min_y <- ref_res[[1]] + ref_res[[2]] * min_x
        ref_max_y <- ref_res[[1]] + ref_res[[2]] * max_x
        df.qqline <- dplyr::bind_rows(df.qqline, data.frame(x=c(min_x, max_x), refline_y=c(ref_min_y,ref_max_y), col=col))

        if (n_sample > 5000) {
          n_sample <- 5000 # shapiro.test takes only up to max of 5000 samples. 
        }

        if (length(df[[col]]) > n_sample) {
          col_to_test <- sample(df[[col]], n_sample)
          # If sampled, check if the column has only 1 unique value or only NAs again, to avoid error.
          if (n_distinct(col_to_test, na.rm=TRUE) <= 1) {
            next
          }
          sample_size <- n_sample
        }
        else {
          col_to_test <- df[[col]]
          sample_size <- length(col_to_test)
        }
        res <- shapiro.test(col_to_test) %>% tidy() %>%
          dplyr::mutate(col=!!col, sample_size=!!sample_size) %>%
          dplyr::select(col, everything())
        df.model <- dplyr::bind_rows(df.model, res)
      }
    }

    model <- list()
    model$qq <- df.qq
    model$sampled_qq <- sample_rows(df.qq, n_sample_qq)
    model$qqline <- df.qqline
    model$model_summary <- df.model
    class(model) <- c("shapiro_exploratory", class(model))
    model
  }

  # Calculation is executed in each group.
  # Storing the result in this tmp_col and
  # unnesting the result.
  # If the original data frame is grouped by "tmp",
  # overwriting it should be avoided,
  # so avoid_conflict is used here.
  tmp_col <- avoid_conflict(colnames(df), "model")
  ret <- df %>%
    dplyr::do_(.dots = setNames(list(~shapiro_each(.)), tmp_col))
  ret
}

#' @export
tidy.shapiro_exploratory <- function(x, type = "model", signif_level=0.05) {
  if (type == "qq" || type == "histogram") {
    if (type == "qq") {
      sampled_qq_df <- x$sampled_qq
    }
    else {
      sampled_qq_df <- x$qq
    }

    # table with TRUE/FALSE result on normality of each column.
    normal_df <- x$model_summary %>%
      dplyr::mutate(normal = p.value > !!signif_level) %>%
      dplyr::select(col, normal)

    ret <- dplyr::bind_rows(sampled_qq_df, x$qqline)
    # join normality result so that we can show histogram with colors based on it.
    ret <- ret %>% dplyr::left_join(normal_df, by="col")
    ret
  }
  else {
    ret <- x$model_summary
    ret <- ret %>% dplyr::mutate(normal = p.value > !!signif_level)
    ret <- ret %>% dplyr::select(-method)
    ret <- ret %>% dplyr::rename(`Column`=col, `Statistic`=statistic, `P Value`=p.value, `Normal Distribution`=normal, `Sample Size`=sample_size)
    ret
  }
}
