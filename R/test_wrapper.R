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
exp_chisq <- function(df, var1, var2, value = NULL, func1 = NULL, func2 = NULL, fun.aggregate = sum, correct = FALSE, ...) {
  # We are turning off Yates's correction by default because...
  # 1. It seems that it is commonly discussed that it is overly conservative and not necessary.
  #    https://en.wikipedia.org/wiki/Yates%27s_correction_for_continuity
  #    https://aue.repo.nii.ac.jp/?action=repository_uri&item_id=785&file_id=15&file_no=1
  # 2. With Yates's correction residuals do not add up to chi-square value, which makes contributions not adding up to 100%.
  var1_col <- col_name(substitute(var1))
  var2_col <- col_name(substitute(var2))
  value_col <- col_name(substitute(value))
  grouped_col <- grouped_by(df)

  if (!is.null(func1) && (is.Date(df[[var1_col]]) || is.POSIXct(df[[var1_col]]))) {
    df <- df %>% mutate(!!rlang::sym(var1_col) := extract_from_date(!!rlang::sym(var1_col), type=func1))
  }
  if (!is.null(func2) && (is.Date(df[[var2_col]]) || is.POSIXct(df[[var2_col]]))) {
    df <- df %>% mutate(!!rlang::sym(var2_col) := extract_from_date(!!rlang::sym(var2_col), type=func2))
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
  # pivot_ does pivot for each group.
  pivotted_df <- pivot_(df, formula, value_col = value_col, fun.aggregate = fun.aggregate, fill = 0)

  chisq.test_each <- function(df) {
    if (length(grouped_col) > 0) {
      df <- df %>% select(-!!rlang::sym(grouped_col))
    }
    df <- df %>% tibble::column_to_rownames(var=var1_col)
    x <- df %>% as.matrix()
    model <- chisq.test(x = x, correct = correct, ...)
    # add variable name info to the model
    model$var1 <- var1_col
    model$var2 <- var2_col
    model$var1_class <- var1_class
    model$var2_class <- var2_class
    model$var1_levels <- var1_levels
    model$var2_levels <- var2_levels
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
      ret <- ret %>% mutate(contrib = 100*residual^2/x$statistic) # add percent contribution too.
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
  ret <- ret %>% rename(`Chi-Square`=statistic, `Degree of Freedom`=parameter, `P Value`=p.value)
  ret
}

#' t-test wrapper for Analytics View
#' @export
exp_ttest <- function(df, var1, var2, func2 = NULL, ...) {
  var1_col <- col_name(substitute(var1))
  var2_col <- col_name(substitute(var2))
  grouped_cols <- grouped_by(df)

  if (!is.null(func2) && (is.Date(df[[var2_col]]) || is.POSIXct(df[[var2_col]]))) {
    df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_date(!!rlang::sym(var2_col), type=func2))
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
    tryCatch({
      model <- t.test(formula, data = df, ...)
      class(model) <- c("ttest_exploratory", class(model))
      model$var1 <- var1_col
      model$var2 <- var2_col
      model$data <- df
      model
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # Ignore the error if
        # it is caused by subset of
        # grouped data frame
        # to show result of
        # data frames that succeed.
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
    ret <- broom:::tidy.htest(x)
    ret <- ret %>% dplyr::select(statistic, p.value, parameter, estimate, conf.high, conf.low) %>%
      dplyr::rename(`t Ratio`=statistic,
                    `P Value`=p.value,
                    `Degree of Freedom`=parameter,
                    Difference=estimate,
                    `Conf High`=conf.high,
                    `Conf Low`=conf.low)
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
exp_anova <- function(df, var1, var2, func2 = NULL, ...) {
  var1_col <- col_name(substitute(var1))
  var2_col <- col_name(substitute(var2))
  grouped_cols <- grouped_by(df)

  if (!is.null(func2) && (is.Date(df[[var2_col]]) || is.POSIXct(df[[var2_col]]))) {
    df <- df %>% mutate(!!rlang::sym(var2_col) := extract_from_date(!!rlang::sym(var2_col), type=func2))
  }
  
  if (n_distinct(df[[var2_col]]) < 2) {
    stop(paste0("Variable Column (", var2_col, ") has to have 2 or more kinds of values."))
  }

  formula = as.formula(paste0('`', var1_col, '`~`', var2_col, '`'))

  anova_each <- function(df) {
    tryCatch({
      model <- aov(formula, data = df, ...)
      class(model) <- c("anova_exploratory", class(model))
      model$var1 <- var1_col
      model$var2 <- var2_col
      model$data <- df
      model
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # Ignore the error if
        # it is caused by subset of
        # grouped data frame
        # to show result of
        # data frames that succeed.
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
  ret
}

#' @export
tidy.anova_exploratory <- function(x, type="model", conf_level=0.95) {
  if (type == "model") {
    ret <- broom:::tidy.aov(x)
    ret <- ret %>% dplyr::select(term, statistic, p.value, df, sumsq, meansq) %>%
      dplyr::rename(Term=term,
                    `F Ratio`=statistic,
                    `P Value`=p.value,
                    `Degree of Freedom`=df,
                    `Sum of Squares`=sumsq,
                    `Mean Square`=meansq)
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
                       `Conf High` = Mean + `Std Error of Mean` * qt(p=conf_threshold, df=`Number of Rows`-1),
                       `Conf Low` = Mean - `Std Error of Mean` * qt(p=conf_threshold, df=`Number of Rows`-1),
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
exp_normality<- function(df, ..., n_sample = 50, n_sample_qq = 4500) {
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
          sample_size <- n_sample
        }
        else {
          col_to_test <- df[[col]]
          sample_size <- length(col_to_test)
        }
        res <- shapiro.test(col_to_test) %>% tidy() %>%
          dplyr::mutate(col=col, sample_size=sample_size) %>%
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
      dplyr::mutate(normal = p.value > signif_level) %>%
      dplyr::select(col, normal)

    ret <- dplyr::bind_rows(sampled_qq_df, x$qqline)
    # join normality result so that we can show histogram with colors based on it.
    ret <- ret %>% dplyr::left_join(normal_df, by="col")
    ret
  }
  else {
    ret <- x$model_summary
    ret <- ret %>% dplyr::mutate(normal = p.value > signif_level)
    ret <- ret %>% dplyr::select(-method)
    ret <- ret %>% dplyr::rename(`Column`=col, `Statistic`=statistic, `P Value`=p.value, `Normal Distribution`=normal, `Sample Size`=sample_size)
    ret
  }
}
