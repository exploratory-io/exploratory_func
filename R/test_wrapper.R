
# Generates data for t distribution probability density with critical section and statistic
# to depict a result of a t-test.
generate_ttest_density_data <- function(t, df, sig_level = 0.05, alternative = "two.sided") {
  l <- max(5, abs(t)*1.1) # limit of x for the data we generate here.

  x <- seq(from=-l,to=l,by=l/500 )
  ret <- tibble::tibble(x=x, y=dt(x, df=df))

  ret2 <- tibble::tibble(x=t, y=dt(x, df=df), statistic=TRUE)
  ret <- bind_rows(ret, ret2)

  if (alternative == "two.sided") {
    tt <- qt(1-sig_level/2, df=df) # Threshold t for critical section.
    ret <- ret %>% mutate(critical=(x>=tt|x<=-tt))
  }
  else if (alternative == "greater") {
    tt <- qt(1-sig_level, df=df) # Threshold t for critical section.
    ret <- ret %>% mutate(critical=(x>=tt))
  }
  else { # alternative == "less"
    tt <- qt(sig_level, df=df) # Threshold t for critical section.
    ret <- ret %>% mutate(critical=(x<=tt))
  }
  ret <- ret %>% mutate(df=df)
  ret
}

# Generates data for t distribution probability density to depict the power analysis.
generate_ttest_density_data_for_power <- function(d, n1, n2, t, df, sig_level = 0.05, alternative = "two.sided", paired = TRUE) {
  if (!paired) {
    ncp <- d * (1/sqrt(1/n1 + 1/n2))
  }
  else {
    ncp <- d * sqrt(n1) # Paired case. Assuming n1 == n2.
  }

  # Cover 3.5 sd of the both distributions.
  if (alternative == "less") {
    start <- -ncp-3.5
    end <- 3.5
  }
  else {
    start <- -3.5
    end <- ncp+3.5
  }
  x <- seq(from=start,to=end,by=(end-start)/500 )
  ret <- tibble::tibble(x=x, y=dt(x, df=df), ncp=0, type="Null")

  if (alternative == "two.sided") {
    tt <- qt(1-sig_level/2, df=df) # Threshold t for critical section.
    ret <- ret %>% mutate(critical=(x>=tt|x<=-tt))
    ret2 <- tibble::tibble(x=x, y=dt(x, df=df, ncp=ncp), ncp=ncp, type="Alternative")
    ret2 <- ret2 %>% mutate(critical=(x<=tt))
  }
  else if (alternative == "greater") {
    tt <- qt(1-sig_level, df=df) # Threshold t for critical section.
    ret <- ret %>% mutate(critical=(x>=tt))
    ret2 <- tibble::tibble(x=x, y=dt(x, df=df, ncp=ncp), ncp=ncp,type="Alternative")
    ret2 <- ret2 %>% mutate(critical=(x<=tt))
  }
  else { # alternative == "less"
    tt <- qt(sig_level, df=df) # Threshold t for critical section.
    ret <- ret %>% mutate(critical=(x<=tt))
    ret2 <- tibble::tibble(x=x, y=dt(x, df=df, ncp=-ncp), ncp=-ncp, type="Alternative")
    ret2 <- ret2 %>% mutate(critical=(x>=tt))
  }
  ret <- ret %>% mutate(df=df)

  ret3 <- tibble::tibble(x=t, y=dt(x, df=df), type="Null", statistic=TRUE)
  ret <- bind_rows(ret, ret2, ret3)
  ret <- ret %>% dplyr::mutate(type=factor(type, levels=c("Null", "Alternative")))
  ret
}

# Generates data for chi-square distribution probability density with critical section and statistic
# to depict a result of a chi-square test.
generate_chisq_density_data <- function(stat, df, sig_level = 0.05) {
  tx <- qchisq(1-sig_level, df=df) # The chisq value that corresponds to the significance level.
  l <- max(df*3, stat*1.1, tx*1.1) # Making sure stat and tx are in the displayed range.

  x <- seq(from=0, to=l, by=l/1000 )
  ret <- tibble::tibble(x=x, y=dchisq(x, df=df))

  ret2 <- tibble::tibble(x=stat, y=dchisq(x, df=df), statistic=TRUE)
  ret <- bind_rows(ret, ret2)

  ret <- ret %>% mutate(critical=x>=tx)
  ret <- ret %>% mutate(df=df)
  ret
}

# Generates data for probability density chart for chi-square test.
# df - Degree of freedom
# w - Cohen's w
# N - Sample size
# crit - Critical chi-square value, for the vertical reference line.
generate_chisq_density_data_for_power <- function(df, w, N, crit) {
  # Plot up to 95 percentile.
  ncp <- N*w^2
  # In the chart data, cover 0 to 95 percentile of the non-centeral chi-square distribution.
  l <- qchisq(0.95, df=df, ncp=ncp)
  x <- seq(from=0, to=l, by=l/1000 )

  ret <- tibble::tibble(x=x,
                        y=dchisq(x, df=df),
                        type="Null"
  )
  # Prepare and bind one-row data for the vertical reference line for the critical value.
  ret0 <- tibble::tibble(x=crit, y=dchisq(crit, df=df), type="Null", statistic=TRUE)
  ret <- ret %>% dplyr::bind_rows(ret0)
  ret <- ret %>% dplyr::mutate(critical=(x>=crit), df=1, ncp=0)
  ret2 <- tibble::tibble(x=x,
                        y=dchisq(x, df=df, ncp=N*w^2),
                        type="Alternative"
  )
  ret2 <- ret2 %>% dplyr::mutate(critical=(x<=crit), df=1, ncp=ncp)
  ret <- ret %>% dplyr::bind_rows(ret2)
  ret <- ret %>% dplyr::mutate(type=factor(type, levels=c("Null", "Alternative")))
  ret
}

# Generates data for F distribution probability density with critical section and statistic
# to depict a result of a F test like one-way ANOVA.
generate_ftest_density_data <- function(stat, df1, df2, sig_level = 0.05) {
  tx <- qf(1-sig_level, df1=df1, df2=df2) # The chisq value that corresponds to the significance level.
  l <- max(df1/df2*3, stat*1.1, tx*1.1) # Making sure stat and tx are in the displayed range.

  x <- seq(from=0,to=l,by=l/1000 )
  ret <- tibble::tibble(x=x, y=df(x, df1=df1, df2=df2))

  ret2 <- tibble::tibble(x=stat, y=df(x, df1=df1, df2=df2), statistic=TRUE)
  ret <- bind_rows(ret, ret2)

  ret <- ret %>% mutate(critical=x>=tx)
  ret <- ret %>% mutate(df1=df1, df2=df2)
  ret
}

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
    unnest_with_drop(!!rlang::sym(model_col))
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
              den.df = "denominator_degrees_of_freedom",
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
    unnest_with_drop(!!rlang::sym(model_col))
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
    unnest_with_drop(!!rlang::sym(tmp_col))
}

#' Chi-Square test wrapper for Analytics View
#' @param test_sig_level - Significance level for the t-test ifself.
#' @param sig.level - Significance level for power analysis.
#' @export
exp_chisq <- function(df, var1, var2, value = NULL, func1 = NULL, func2 = NULL, fun.aggregate = sum, correct = FALSE,
                      test_sig_level = 0.05, sig.level = 0.05, w = NULL, power = NULL, beta = NULL, ...) {
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
  var1_col <- tidyselect::vars_select(names(df), !! rlang::enquo(var1))
  var2_col <- tidyselect::vars_select(names(df), !! rlang::enquo(var2))
  value_col <- tidyselect::vars_select(names(df), !! rlang::enquo(value))
  grouped_cols <- grouped_by(df)

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
  
  # Check each category column has multiple classes. 
  # Currently, we filter out NA categories later. So, counting non-NA categories only.
  if (n_distinct(df[[var1_col]], na.rm=TRUE) < 2) {
    stop(paste0("The explanatory variable needs to have 2 or more unique values."))
  }
  if (n_distinct(df[[var2_col]], na.rm=TRUE) < 2) {
    stop(paste0("The target variable needs to have 2 or more unique values."))
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
    tryCatch({
      # If there is only one class of category in this class, skip it.
      # This is effectively for multiple group case, since for single group case, it is already checked before this loop.
      if (n_distinct(df[[var1_col]], na.rm=TRUE) < 2) {
        stop(paste0("The explanatory variable needs to have 2 or more unique values."))
      }
      if (n_distinct(df[[var2_col]], na.rm=TRUE) < 2) {
        stop(paste0("The target variable needs to have 2 or more unique values."))
      }
      # TODO: For now, we are filtering out NA categories, but we should include them and display them cleanly.
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var1_col)) & !is.na(!!rlang::sym(var2_col)))
      df <- df %>% dplyr::group_by(!!rlang::sym(var1_col), !!rlang::sym(var2_col))
      if (is.null(value_col) || length(value_col) == 0) { # It seems that if value_col is not specified value_col can be named character(0), which can be detected by length(value_col)=0.
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
      # TODO: spread creates column named "<NA>". For consistency on UI, we want "(NA)".
      # Note that this issue is currently avoided by filtering out rows with NA categories in the first place. 
      df <- df %>% dplyr::ungroup() %>% tidyr::spread(key = !!rlang::sym(var2_col), value = .temp_value_col, fill=0)
      # na_leves is set to "(NA)" for consistency on UI.
      # Note that this issue is currently avoided by filtering out rows with NA categories in the first place. 
      df <- df %>% dplyr::mutate(!!rlang::sym(var1_col):=forcats::fct_explicit_na(as.factor(!!rlang::sym(var1_col)), na_level = "(NA)"))

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
      model$test_sig_level <- test_sig_level
      model$sig.level <- sig.level
      model$cohens_w <- cohens_w
      model$cohens_w_to_detect <- cohens_w_to_detect
      model$power <- power
      class(model) <- c("chisq_exploratory", class(model))
      model
    }, error = function(e){
      if (length(grouped_cols) > 0) {
        # In repeat-by case, we report group-specific error in the Summary table,
        # so that analysis on other groups can go on.
        class(e) <- c("chisq_exploratory", class(e))
        e
      } else {
        stop(e)
      }
    })
  }
  do_on_each_group(df, chisq.test_each, name = "model", with_unnest = FALSE)
}

#' @export
exp_chisq_ab_aggregated <- function(df, a_b_identifier, conversion_rate, count, correct = FALSE, sig.level = 0.05) {
  a_b_identifier_col <- col_name(substitute(a_b_identifier))
  conversion_rate_col <- col_name(substitute(conversion_rate))
  count_col <- col_name(substitute(count))
  # Keep only necessary columns before pivot_longer to avoid unnecessary copies.
  df <- df %>% dplyr::select(!!rlang::sym(a_b_identifier_col), !!rlang::sym(conversion_rate_col), !!rlang::sym(count_col))
  df <- df %>% dplyr::mutate(`TRUE`=(!!rlang::sym(count_col))*(!!rlang::sym(conversion_rate_col)), `FALSE`=(!!rlang::sym(count_col))*((1-!!rlang::sym(conversion_rate_col)))) %>% dplyr::select(-!!rlang::sym(count_col), -!!rlang::sym(conversion_rate_col))
  df <- df %>% tidyr::pivot_longer(c(`TRUE`,`FALSE`), names_to="converted", values_to="n")
  res <- exp_chisq(df, !!rlang::sym(a_b_identifier_col), converted, value = n, func1 = NULL, func2 = NULL, fun.aggregate = sum, correct = FALSE,
                      test_sig_level = sig.level, sig.level = sig.level, w = NULL, power = NULL, beta = NULL)
  res
}

#' @export
tidy.chisq_exploratory <- function(x, type = "observed") {
  if ("error" %in% class(x)) {
    ret <- tibble::tibble()
    return(ret)
  }

  if (type == "observed") {
    ret <- as.data.frame(x$observed)
    ret <- ret %>% tibble::rownames_to_column(var = x$var1)
  }
  else if (type == "residuals") {
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
  else { # type == "prob_dist"
    ret <- generate_chisq_density_data(x$statistic, x$parameter, sig_level=x$test_sig_level)
    ret
  }
  ret
}

#' @export
glance.chisq_exploratory <- function(x) {
  if ("error" %in% class(x)) {
    ret <- tibble::tibble(Note = x$message)
    return(ret)
  }
  # ret <- x %>% broom:::glance.htest() # for some reason this does not work. just do it like following.
  ret <- data.frame(statistic=x$statistic, parameter=x$parameter, p.value=x$p.value)
  N <- sum(x$observed) # Total number of observations (rows).
  k <- ncol(x$observed)
  r <- nrow(x$observed)
  V <- sqrt(x$statistic/N/min(k-1, r-1)) # Cramer's V - https://en.wikipedia.org/wiki/Cram%C3%A9r%27s_V
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
    ret <- ret %>% dplyr::mutate(v=!!V, w=!!(x$cohens_w), power=!!power_val, beta=1.0-!!power_val, n=!!N)
    ret <- ret %>% dplyr::select(statistic, p.value, parameter, everything()) # Reorder to unify order with t-test.
    ret <- ret %>% dplyr::rename(`Chi-Square`=statistic,
                                 `P Value`=p.value,
                                 `Degree of Freedom`=parameter,
                                 `Association Coef. (Cramer's V)`=v,
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
    ret <- ret %>% dplyr::mutate(v=!!V, w=!!(x$cohens_w), power=!!(x$power), beta=1.0-!!(x$power), n=!!N, required_n=!!required_sample_size)
    ret <- ret %>% dplyr::select(statistic, p.value, parameter, everything()) # Reorder to unify order with t-test.
    ret <- ret %>% dplyr::rename(`Chi-Square`=statistic,
                                 `P Value`=p.value,
                                 `Degree of Freedom`=parameter,
                                 `Association Coef. (Cramer's V)`=v,
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

# Calculate standard error of difference between means for Welch's t-test.
calculate_welch_stderr <- function(N1, N2, s1, s2) {
  ret <- sqrt(s1^2/N1 + s2^2/N2)
  ret
}

# Calculate confidence interval of difference between means for Welch's t-test.
calculate_welch_confint <- function(N1, N2, X1, X2, s1, s2, conf.level, alternative = "two.sided") {
  alpha <- 1-conf.level
  dof <- calculate_welch_dof(N1, N2, s1, s2)
  if (alternative == "two.sided") {
    q <- qt(1-alpha/2, dof)
    ci <- c(-q, q)
  } else if (alternative == "greater") {
    q <- qt(alpha, dof)
    ci <- c(q, Inf)
  } else {
    q <- qt(1-alpha, dof)
    ci <- c(-Inf, q)
  }
  stderr <- calculate_welch_stderr(N1, N2, s1, s2)
  res <- X1 - X2 + stderr*ci
  res
}

# Calculate t statistic for Welch's t-test.
calculate_welch_t <- function(N1, N2, X1, X2, s1, s2) {
  ret <- (X1 - X2)/sqrt(s1^2/N1 + s2^2/N2)
  ret
}

# Calculate degree of freedom for Welch's t-test.
calculate_welch_dof <- function(N1, N2, s1, s2) {
  ret <- (s1^2/N1 + s2^2/N2)^2/((s1^4/((N1^2)*(N1-1))) + (s2^4/((N2^2)*(N2-1))))
  ret
}

# Calculate p value for Welch's t-test.
calculate_welch_p <- function(N1, N2, X1, X2, s1, s2, alternative = "two.sided") {
  t <- calculate_welch_t(N1, N2, X1, X2, s1, s2)
  dof <- calculate_welch_dof(N1, N2, s1, s2)
  p <- pt(t,dof)
  if (alternative == "two.sided") {
    if (p < 0.5) {
      res <- 2*p
    } else {
      res <- 2*(1-p)
    }
  } else if (alternative == "greater") {
    res <- 1-p
  } else {
    res <- p
  }
  res
}

# Calculate pooled standard error for Student's t-test.
calculate_pooled_stderr <- function(N1, N2, s1, s2) {
  res <- sqrt(((N1-1)*s1^2 + (N2-1)*s2^2)/(N1 + N2 - 2))
  res
}

# Calculate confidence interval of difference between means for Student's t-test.
calculate_student_confint <- function(N1, N2, X1, X2, s1, s2, conf.level, alternative = "two.sided") {
  alpha <- 1-conf.level
  dof <- calculate_student_dof(N1, N2)
  if (alternative == "two.sided") {
    q <- qt(1-alpha/2, dof)
    ci <- c(-q, q)
  } else if (alternative == "greater") {
    q <- qt(alpha, dof)
    ci <- c(q, Inf)
  } else {
    q <- qt(1-alpha, dof)
    ci <- c(-Inf, q)
  }
  stderr <- calculate_pooled_stderr(N1, N2, s1, s2)*sqrt(1/N1 + 1/N2)
  res <- X1 - X2 + stderr*ci
  res
}

# Calculate t statistic for Student's t-test.
calculate_student_t <- function(N1, N2, X1, X2, s1, s2) {
  ret <- (X1 - X2)/(calculate_pooled_stderr(N1, N2, s1, s2)*sqrt(1/N1 + 1/N2))
  ret
}

# Calculate degree of freedom for Student's t-test.
calculate_student_dof <- function(N1, N2) {
  ret <- N1 + N2 - 2
  ret
}

# Calculate p value for Student's t-test.
calculate_student_p <- function(N1, N2, X1, X2, s1, s2, alternative = "two.sided") {
  t <- calculate_student_t(N1, N2, X1, X2, s1, s2)
  dof <- calculate_student_dof(N1, N2)
  p <- pt(t,dof)
  if (alternative == "two.sided") {
    if (p < 0.5) {
      res <- 2*p
    } else {
      res <- 2*(1-p)
    }
  } else if (alternative == "greater") {
    res <- 1-p
  } else {
    res <- p
  }
  res
}

# A function that gives the same output as stats::t.test, but takes aggregated data as the input.
# N1, N2 - Sample sizes
# X1, X2 - Means
# s1, s2 - Standard Deviations
t.test.aggregated <- function(N1, N2, X1, X2, s1, s2, conf.level=0.95, mu=0, alternative = "two.sided", paired = FALSE, var.equal = FALSE) {
  if (!var.equal) {
    method="Welch Two Sample t-test"
    statistic <- calculate_welch_t(N1, N2, X1, X2, s1, s2)
    parameter <- calculate_welch_dof(N1, N2, s1, s2)
    p.value <- calculate_welch_p(N1, N2, X1, X2, s1, s2, alternative = alternative)
    conf.int <- calculate_welch_confint(N1, N2, X1, X2, s1, s2, conf.level, alternative = alternative)
    estimate <- c(X1, X2)
    stderr <- calculate_welch_stderr(N1, N2, s1, s2)
  }
  else {
    method="Two Sample t-test"
    statistic <- calculate_student_t(N1, N2, X1, X2, s1, s2)
    parameter <- calculate_student_dof(N1, N2)
    p.value <- calculate_student_p(N1, N2, X1, X2, s1, s2, alternative = alternative)
    conf.int <- calculate_student_confint(N1, N2, X1, X2, s1, s2, conf.level, alternative = alternative)
    estimate <- c(X1, X2)
    stderr <- calculate_pooled_stderr(N1, N2, s1, s2)*sqrt(1/N1 + 1/N2)
  }
  null.value <- mu
  names(statistic) <- "t"
  names(parameter) <- "df"
  names(mu) <- "difference in means"
  attr(conf.int, "conf.level") <- conf.level
  res <- list(
    statistic=statistic,
    parameter=parameter,
    p.value=p.value,
    conf.int=conf.int,
    estimate=estimate,
    stderr=stderr,
    null.value=null.value,
    method=method,
    alternative=alternative
  )
  class(res) <- c('ttest_exploratory', 'htest')
  res
}

#' t-test wrapper for Analytics View. Almost the same as exp_ttest, but takes already aggregated data.
#' @export
#' @param category - Column of the categories.
#' @param n - Column of the sample sizes.
#' @param category_mean - Column of the means of the caterories.
#' @param category_sd - Column of the standard deviations of the caterories.
#' @param conf.level - Level of confidence for confidence interval. Passed to t.test as part of ...
#' @param test_sig_level - Significance level for the t-test ifself.
#' @param sig.level - Significance level for power analysis.
#' @param d - Cohen's d to detect in power analysis.
#' @param common_sd - Used for calculation of Cohen's d.
#' @param diff_to_detect - Used for calculation of Cohen's d.
exp_ttest_aggregated <- function(df, category, n, category_mean, category_sd, test_sig_level = 0.05,
                                 sig.level = 0.05, d = NULL, common_sd = NULL, diff_to_detect = NULL, power = NULL, beta = NULL,
                                 ...) {
  if (!is.null(power) && !is.null(beta) && (power + beta != 1.0)) {
    stop("Specify only one of Power or Probability of Type 2 Error, or they must add up to 1.0.")
  }
  if (is.null(power) && !is.null(beta)) {
    power <- 1.0 - beta
  }
  var2_col <- col_name(substitute(category))
  n_col <- col_name(substitute(n))
  mean_col <- col_name(substitute(category_mean))
  sd_col <- col_name(substitute(category_sd))
  grouped_cols <- grouped_by(df)

  # For logical explanatory variable, make it a factor and adjust label order so that
  # the calculated difference is TRUE case - FALSE case, which intuitively makes better sense.
  if (is.logical(df[[var2_col]])) {
    df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := factor(!!rlang::sym(var2_col), levels=c("TRUE", "FALSE")))
    df <- df %>% dplyr::arrange(!!rlang::sym(var2_col))
  }

  n_distinct_res <- n_distinct(df[[var2_col]]) # save n_distinct result to avoid repeating the relatively expensive call.
  if (n_distinct_res != 2) {
    if (n_distinct_res == 3 && any(is.na(df[[var2_col]]))) { # automatically filter NA to make number of category 2, if it is the 3rd category.
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var2_col)))
    }
    else {
      stop("The explanatory variable needs to have 2 unique values.")
    }
  }

  ttest_each <- function(df) {
    tryCatch({
      if (nrow(df) != 2) {
        stop("Number of rows of the aggregated input data must be 2.")
      }
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(n_col)) & !is.na(!!rlang::sym(mean_col)) & !is.na(!!rlang::sym(sd_col))) # Remove NA from the target column.
      if (nrow(df) != 2) {
        stop("There is NA in the aggregated input data.")
      }

      if(length(grouped_cols) > 0) {
        n_distinct_res_each <- n_distinct(df[[var2_col]]) # check n_distinct again within group after handling outlier.
        if (n_distinct_res_each != 2) {
          stop("The explanatory variable needs to have 2 unique values.")
        }
      }
      # It seems that each group has to have at least 2 rows to avoid "not enough 'x' observations" error.
      # Check it here, rather than handling it later.
      min_n <- min(df[[n_col]], na.rm=TRUE)
      if (min_n <= 1) {
        e <- simpleError("Not enough data.")
        class(e) <- c("ttest_exploratory", class(e))
        e$v1 <- df[[var2_col]][1]
        e$n1 <- df[[n_col]][1]
        e$v2 <- df[[var2_col]][2]
        e$n2 <- df[[n_col]][2]
        return(e)
      }
      # Calculate Cohen's d from data.
      cohens_d <- calculate_cohens_d_aggregated(df[[n_col]][1], df[[n_col]][2], df[[mean_col]][1], df[[mean_col]][2], df[[sd_col]][1], df[[sd_col]][2])
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
            cohens_d_to_detect <- diff_to_detect/calculate_common_sd_aggregated(df[[n_col]][1], df[[n_col]][2], df[[sd_col]][1], df[[sd_col]][2])
          }
          else {
            cohens_d_to_detect <- diff_to_detect/common_sd
          }
        }
      }
      else {
        cohens_d_to_detect <- d
      }

      model <- t.test.aggregated(df[[n_col]][1], df[[n_col]][2], df[[mean_col]][1], df[[mean_col]][2], df[[sd_col]][1], df[[sd_col]][2], ...)
      class(model) <- c("ttest_exploratory", class(model))
      model$var2 <- var2_col
      model$data <- df
      model$test_sig_level <- test_sig_level
      model$sig.level <- sig.level
      model$cohens_d <- cohens_d # model$d seems to be already used for something.
      model$cohens_d_to_detect <- cohens_d_to_detect
      model$power <- power
      model$v1 <- df[[var2_col]][1]
      model$v2 <- df[[var2_col]][2]
      model$n1 <- df[[n_col]][1]
      model$n2 <- df[[n_col]][2]
      model$base.level <- df[[var2_col]][2] # The 2nd row is always the base in case of aggregated.
      model$s1 <- df[[sd_col]][1]
      model$s2 <- df[[sd_col]][2]
      model$m1 <- df[[mean_col]][1]
      model$m2 <- df[[mean_col]][2]
      model$data_type <- "aggregated"
      model
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # In repeat-by case, we report group-specific error in the Summary table,
        # so that analysis on other groups can go on.
        class(e) <- c("ttest_exploratory", class(e))
        e
      } else {
        stop(e)
      }
    })
  }
  do_on_each_group(df, ttest_each, name = "model", with_unnest = FALSE)
}

#' t-test wrapper for Analytics View
#' @export
#' @param conf.level - Level of confidence for confidence interval. Passed to t.test as part of ...
#' @param test_sig_level - Significance level for the t-test ifself.
#' @param sig.level - Significance level for power analysis.
#' @param d - Cohen's d to detect in power analysis.
#' @param common_sd - Used for calculation of Cohen's d.
#' @param diff_to_detect - Used for calculation of Cohen's d.
exp_ttest <- function(df, var1, var2, func2 = NULL, test_sig_level = 0.05,
                      sig.level = 0.05, d = NULL, common_sd = NULL, diff_to_detect = NULL, power = NULL, beta = NULL,
                      outlier_filter_type = NULL, outlier_filter_threshold = NULL,
                      ...) {
  if (!is.null(power) && !is.null(beta) && (power + beta != 1.0)) {
    stop("Specify only one of Power or Probability of Type 2 Error, or they must add up to 1.0.")
  }
  if (is.null(power) && !is.null(beta)) {
    power <- 1.0 - beta
  }
  var1_col <- col_name(substitute(var1))
  var2_col <- col_name(substitute(var2))
  grouped_cols <- grouped_by(df)

  # Apply func2 to var2.
  if (!is.null(func2)) {
    if (lubridate::is.Date(df[[var2_col]]) || lubridate::is.POSIXct(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_date(!!rlang::sym(var2_col), type=!!func2))
    }
    else if (is.numeric(df[[var2_col]])) {
      df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := extract_from_numeric(!!rlang::sym(var2_col), type=!!func2))
    }
  }

  # For logical explanatory variable, make it a factor and adjust label order so that
  # the calculated difference is TRUE case - FALSE case, which intuitively makes better sense.
  var2_logical <- is.logical(df[[var2_col]])
  if (var2_logical) {
    df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := factor(!!rlang::sym(var2_col), levels=c("TRUE", "FALSE")))
  }
  
  n_distinct_res <- n_distinct(df[[var2_col]]) # save n_distinct result to avoid repeating the relatively expensive call.
  if (n_distinct_res != 2) {
    if (n_distinct_res == 3 && any(is.na(df[[var2_col]]))) { # automatically filter NA to make number of category 2, if it is the 3rd category.
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var2_col)))
    }
    else {
      stop("The explanatory variable needs to have 2 unique values.")
    }
  }

  formula = as.formula(paste0('`', var1_col, '`~`', var2_col, '`'))

  ttest_each <- function(df) {
    tryCatch({
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var1_col))) # Remove NA from the target column.
      if (nrow(df) == 0) {
        stop("There is no data left after removing NA.")
      }
      if (!is.null(outlier_filter_type)) {
        is_outlier <- function(x) {
          res <- detect_outlier(x, type=outlier_filter_type, threshold=outlier_filter_threshold) %in% c("Lower", "Upper")
          res
        }
        df$.is.outlier <- FALSE #TODO: handle possibility of name conflict.
        df$.is.outlier <- df$.is.outlier | is_outlier(df[[var1_col]])
        df <- df %>% dplyr::filter(!.is.outlier)
        df$.is.outlier <- NULL
      }

      if(length(grouped_cols) > 0) {
        n_distinct_res_each <- n_distinct(df[[var2_col]]) # check n_distinct again within group after handling outlier.
        if (n_distinct_res_each != 2) {
          stop("The explanatory variable needs to have 2 unique values.")
        }
      }
      # It seems that each group has to have at least 2 rows to avoid "not enough 'x' observations" error.
      # Check it here, rather than handling it later.
      count_df <- df %>% group_by(!!rlang::sym(var2_col)) %>% summarize(n=n())
      min_n <- min(count_df$n, na.rm=TRUE)
      if (min_n <= 1) {
        e <- simpleError("Not enough data.")
        class(e) <- c("ttest_exploratory", class(e))
        e$v1 <- count_df[[1]][1] 
        e$n1 <- count_df$n[1]
        e$v2 <- count_df[[1]][2] 
        e$n2 <- count_df$n[2]
        return(e)
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

      # Adjust the factor levels since t.test consideres the 2nd level to be the base, which is not what we want.
      # We keep the original df as is, since we want to keep the original factor order for display purpose.
      if (var2_logical) {
        df_test <- df
      }
      else if (is.numeric(df[[var2_col]])) {
        # If numeric, we want the smaller number to be the base, e.g. in case of 0, 1, 0 should be the base.
        df_test <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := forcats::fct_rev(as.factor(!!rlang::sym(var2_col))))
      }
      else if (is.factor(df[[var2_col]])) {
        # For factor, drop unused levels and revert the factor levels since we want the first actually used level to be the base, just like linear regression.
        df_test <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := forcats::fct_rev(forcats::fct_drop(!!rlang::sym(var2_col))))
      }
      else {
        # For character and others, majority should become the base.
        df_test <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := forcats::fct_rev(forcats::fct_infreq(as.factor(!!rlang::sym(var2_col)))))
      }
      base.level <- levels(df_test[[var2_col]])[2]
      model <- t.test(formula, data = df_test, ...)
      class(model) <- c("ttest_exploratory", class(model))
      model$var1 <- var1_col
      model$var2 <- var2_col
      model$data <- df
      model$test_sig_level <- test_sig_level
      model$sig.level <- sig.level
      model$cohens_d <- cohens_d # model$d seems to be already used for something.
      model$cohens_d_to_detect <- cohens_d_to_detect
      model$power <- power
      model$v1 <- count_df[[1]][1] 
      model$n1 <- count_df$n[1]
      model$v2 <- count_df[[1]][2] 
      model$n2 <- count_df$n[2]
      model$base.level <- base.level
      model$data_type <- "raw"
      model
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # In repeat-by case, we report group-specific error in the Summary table,
        # so that analysis on other groups can go on.
        class(e) <- c("ttest_exploratory", class(e))
        e
      } else {
        stop(e)
      }
    })
  }
  do_on_each_group(df, ttest_each, name = "model", with_unnest = FALSE)
}

#' @export
glance.ttest_exploratory <- function(x) {
  if ("error" %in% class(x)) {
    ret <- tibble::tibble(Note = x$message)
    return(ret)
  }
  ret <- broom:::glance.htest(x) # for t-test. the returned content is same as tidy.
  ret
}

#' @export
tidy.ttest_exploratory <- function(x, type="model", conf_level=0.95) {
  if (type == "model") {
    if ("error" %in% class(x)) {
      if (!is.null(x$v1) && !is.null(x$v2) && !is.null(x$n1) && !is.null(x$n2)) {
        ret <- tibble::tibble(`Number of Rows`=x$n1+x$n2, n1=x$n1, n2=x$n2, Note = x$message)
        ret <- ret %>% dplyr::rename(!!rlang::sym(paste0("Number of Rows for ", x$v1)):=n1)
        ret <- ret %>% dplyr::rename(!!rlang::sym(paste0("Number of Rows for ", x$v2)):=n2)
      }
      else {
        ret <- tibble::tibble(Note = x$message)
      }
      return(ret)
    }

    note <- NULL
    ret <- broom:::tidy.htest(x)
    if (is.null(ret$estimate)) { # estimate is empty when var.equal = TRUE.
                                 # Looks like an issue from broom. Working it around.
      ret <- ret %>% dplyr::mutate(estimate = estimate1 - estimate2)
    }

    # Get sample sizes for the 2 groups (n1, n2).
    n1 <- x$n1 # number of 1st class
    n2 <- x$n2 # number of 2nd class
    v1 <- x$v1 # value for 1st class
    v2 <- x$v2 # value for 2nd class
    # t.test seems to consider the 2nd category based on alphabetical/numerical/factor sort as the base category.
    # Since group_by/summarize also sorts the group based on alphabetical/numerical/factor order, we can assume that the v2 is the base category.
    ret <- ret %>% dplyr::mutate(base.level = !!x$base.level)
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

      ret <- ret %>% dplyr::select(statistic, p.value, parameter, estimate, conf.high, conf.low, base.level) %>%
        dplyr::mutate(d=!!(x$cohens_d), power=!!power_val, beta=1.0-!!power_val) %>%
        dplyr::rename(`t Value`=statistic,
                      `P Value`=p.value,
                      `Degree of Freedom`=parameter,
                      Difference=estimate,
                      `Conf High`=conf.high,
                      `Conf Low`=conf.low,
                      `Base Level`=base.level,
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
      ret <- ret %>% dplyr::select(statistic, p.value, parameter, estimate, conf.high, conf.low, base.level) %>%
        dplyr::mutate(d=!!(x$cohens_d), power=!!(x$power), beta=1.0-!!(x$power)) %>%
        dplyr::mutate(current_sample_size=min(!!n1,!!n2), required_sample_size=required_sample_size) %>%
        dplyr::rename(`t Value`=statistic,
                      `P Value`=p.value,
                      `Degree of Freedom`=parameter,
                      Difference=estimate,
                      `Conf High`=conf.high,
                      `Conf Low`=conf.low,
                      `Base Level`=base.level,
                      `Effect Size (Cohen's d)`=d,
                      `Target Power`=power,
                      `Target Probability of Type 2 Error`=beta,
                      `Current Sample Size (Each Group)`=current_sample_size,
                      `Required Sample Size (Each Group)`=required_sample_size)
    }
    ret <- ret %>% dplyr::mutate(`Number of Rows`=!!(n1+n2))
    ret <- ret %>% dplyr::mutate(!!rlang::sym(paste0("Number of Rows for ", v1)):=!!n1)
    ret <- ret %>% dplyr::mutate(!!rlang::sym(paste0("Number of Rows for ", v2)):=!!n2)
    if (!is.null(note)) { # Add Note column, if there was an error from pwr function.
      ret <- ret %>% dplyr::mutate(Note=!!note)
    }
  }
  else if (type == "data_summary") { #TODO consolidate with code in tidy.anova_exploratory
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    conf_threshold = 1 - (1 - conf_level)/2
    if (x$data_type == "raw") {
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
    else { # x$data_type == "aggregated"
      stderr <- c(x$s1/sqrt(x$n1), x$s2/sqrt(x$n2))
      ci_radius <- stderr * qt(p=conf_threshold, df=c(x$n1, x$n2)-1)
      ret <- tibble::tibble(
                      group_ = c(x$v1, x$v2),
                      `Number of Rows` = c(x$n1, x$n2),
                      Mean = c(x$m1, x$m2),
                      `Conf Low` = c(x$m1, x$m2) - ci_radius,
                      `Conf High` = c(x$m1, x$m2) + ci_radius,
                      `Std Error of Mean` = stderr,
                      `Std Deviation` = c(x$s1, x$s2)
      )
      ret <- ret %>% dplyr::rename(!!rlang::sym(x$var2):=group_)
    }
  }
  else if (type == "prob_dist") {
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    ret <- generate_ttest_density_data(x$statistic, x$parameter, sig_level=x$test_sig_level, alternative=x$alternative)
    ret
  }
  else { # type == "data"
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
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

  # For logical explanatory variable, make it a factor and adjust label order so that
  # the calculated difference is TRUE case - FALSE case, which intuitively makes better sense.
  var2_logical <- is.logical(df[[var2_col]])
  if (var2_logical) {
    df <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := factor(!!rlang::sym(var2_col), levels=c("TRUE", "FALSE")))
  }
  
  n_distinct_res <- n_distinct(df[[var2_col]]) # save n_distinct result to avoid repeating the relatively expensive call.
  if (n_distinct_res != 2) {
    if (n_distinct_res == 3 && any(is.na(df[[var2_col]]))) { # automatically filter NA to make number of category 2, if it is the 3rd category.
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var2_col)))
    }
    else {
      stop("The explanatory variable needs to have 2 unique values.")
    }
  }

  formula = as.formula(paste0('`', var1_col, '`~`', var2_col, '`'))

  each_func <- function(df) {
    tryCatch({
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var1_col))) # Remove NA from the target column.
      if (nrow(df) == 0) {
        stop("There is no data left after removing NA.")
      }
      if(length(grouped_cols) > 0) {
        n_distinct_res_each <- n_distinct(df[[var2_col]]) # check n_distinct again within group after handling outlier.
        if (n_distinct_res_each != 2) {
          stop("The explanatory variable needs to have 2 unique values.")
        }
      }
      # Adjust the factor levels since t.test consideres the 2nd level to be the base, which is not what we want.
      # We keep the original df as is, since we want to keep the original factor order for display purpose.
      if (var2_logical) {
        df_test <- df
      }
      else if (is.numeric(df[[var2_col]])) {
        # If numeric, we want the smaller number to be the base, e.g. in case of 0, 1, 0 should be the base.
        df_test <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := forcats::fct_rev(as.factor(!!rlang::sym(var2_col))))
      }
      else if (is.factor(df[[var2_col]])) {
        # For factor, drop unused levels and revert the factor levels since we want the first actually used level to be the base, just like linear regression.
        df_test <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := forcats::fct_rev(forcats::fct_drop(!!rlang::sym(var2_col))))
      }
      else {
        # For character and others, majority should become the base.
        df_test <- df %>% dplyr::mutate(!!rlang::sym(var2_col) := forcats::fct_rev(forcats::fct_infreq(as.factor(!!rlang::sym(var2_col)))))
      }
      base.level <- levels(df_test[[var2_col]])[2]
      model <- wilcox.test(formula, data = df_test, ...)
      class(model) <- c("wilcox_exploratory", class(model))
      model$var1 <- var1_col
      model$var2 <- var2_col
      model$base.level <- base.level
      model$data <- df
      model
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # In repeat-by case, we report group-specific error in the Summary table,
        # so that analysis on other groups can go on.
        class(e) <- c("wilcox_exploratory", class(e))
        e
      } else {
        stop(e)
      }
    })
  }
  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

#' @export
tidy.wilcox_exploratory <- function(x, type="model", conf_level=0.95) {
  if (type == "model") {
    if ("error" %in% class(x)) {
      ret <- tibble::tibble(Note = x$message)
      return(ret)
    }

    note <- NULL
    ret <- broom:::tidy.htest(x)
    if (!is.null(x$estimate)) { # Result is with estimate and confidence interval
      ret <- ret %>% dplyr::select(statistic, p.value, estimate, conf.high, conf.low, method)
    }
    else {
      ret <- ret %>% dplyr::select(statistic, p.value, method)
    }

    # Get sample sizes for the 2 groups (n1, n2).
    data_summary <- x$data %>% dplyr::group_by(!!rlang::sym(x$var2)) %>%
      dplyr::summarize(n_rows=length(!!rlang::sym(x$var1)))
    n1 <- data_summary$n_rows[[1]] # number of 1st class
    n2 <- data_summary$n_rows[[2]] # number of 2nd class
    v1 <- data_summary[[x$var2]][[1]] # value for 1st class
    v2 <- data_summary[[x$var2]][[2]] # value for 2nd class

    # Switch the name of statistic based on the type of performed test.
    if (stringr::str_detect(ret$method[[1]], "signed rank")) {
      ret <- ret %>% dplyr::rename(`W Statistic`=statistic)
    }
    else if (stringr::str_detect(ret$method[[1]], "rank sum")) { # Intentionally matching with just "rank sum" to match "Wilcoxon rank sum exact test" too.
      ret <- ret %>% dplyr::rename(`U Statistic`=statistic)
    }

    if (!is.null(x$estimate)) { # Result is with estimate and confidence interval
      # wilcox.test, just like t.test, seems to consider the 2nd category based on alphabetical/numerical/factor sort as the base category.
      # Since group_by/summarize also sorts the group based on alphabetical/numerical/factor order, we can assume that the v2 is the base category.
      ret <- ret %>% dplyr::mutate(base.level = !!x$base.level)
      ret <- ret %>% dplyr::relocate(base.level, .after = conf.low)
      ret <- ret %>% dplyr::rename(`P Value`=p.value,
                     Difference=estimate,
                     `Conf High`=conf.high,
                     `Conf Low`=conf.low,
                     `Base Level`=base.level,
                     `Method`=method)
    }
    else {
      ret <- ret %>% dplyr::mutate(base.level = !!x$base.level)
      ret <- ret %>% dplyr::relocate(base.level, .after = p.value)
      ret <- ret %>% dplyr::rename(`P Value`=p.value,
                     `Base Level`=base.level,
                     `Method`=method)
    }

    ret <- ret %>% dplyr::mutate(`Number of Rows`=!!(n1+n2))
    ret <- ret %>% dplyr::mutate(!!rlang::sym(paste0("Number of Rows for ", v1)):=!!n1)
    ret <- ret %>% dplyr::mutate(!!rlang::sym(paste0("Number of Rows for ", v2)):=!!n2)
    if (!is.null(note)) { # Code to add Note column if there was an error. Not used for this particular function yet.
      ret <- ret %>% dplyr::mutate(Note=!!note)
    }
  }
  else if (type == "data_summary") { #TODO consolidate with code in tidy.anova_exploratory
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
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
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    ret <- x$data
  }
  ret
}

#' ANOVA wrapper for Analytics View
#' @export
#' @param var1 - The column for dependent variable
#' @param var2 - The column for the categorical independent variable(s).
#'               If it is a single column, it can be specified with unquoted column name.
#'               If there are 2 columns (2-way ANOVA case), it is a character vector with 2 elements of character.
#' @param test_sig_level - Significance level for the t-test ifself.
#' @param sig.level - Significance level for power analysis.
exp_anova <- function(df, var1, var2, covariates = NULL, func2 = NULL, covariate_funs = NULL, test_sig_level = 0.05,
                      sig.level = 0.05, f = NULL, power = NULL, beta = NULL,
                      outlier_filter_type = NULL, outlier_filter_threshold = NULL,
                      with_interaction = FALSE,
                      ...) {
  if (!is.null(power) && !is.null(beta) && (power + beta != 1.0)) {
    stop("Specify only one of Power or Probability of Type 2 Error, or they must add up to 1.0.")
  }
  if (is.null(power) && !is.null(beta)) {
    power <- 1.0 - beta
  }
  var1_col <- col_name(substitute(var1))
  var2_ <- substitute(var2)
  if (class(var2_) == "name") { # For backward compatibility with pre-6.13 when we only had one-way ANOVA.
    var2_col <- col_name(var2_)
  }
  else {
    var2_col <- var2
  }
  grouped_cols <- grouped_by(df)

  # Note that applying preprocessing function is taken care of by the analytics view framework now, and this code applying func2
  # is not currently used.
  if (!is.null(func2)) {
    for (i in 1:length(func2)) {
      if (lubridate::is.Date(df[[var2_col[i]]]) || lubridate::is.POSIXct(df[[var2_col[i]]])) {
        df <- df %>% dplyr::mutate(!!rlang::sym(var2_col[i]) := extract_from_date(!!rlang::sym(var2_col[i]), type=!!func2[i]))
      }
      else if (is.numeric(df[[var2_col[i]]])) {
        df <- df %>% dplyr::mutate(!!rlang::sym(var2_col[i]) := extract_from_numeric(!!rlang::sym(var2_col[i]), type=!!func2[i]))
      }
    }
  }
  
  for (i in 1:length(var2_col)) {
    if (n_distinct(df[[var2_col[i]]]) < 2) {
      stop(paste0("The explanatory variable needs to have 2 or more unique values."))
    }
  }

  # Apply preprocessing functions to the covariates.
  if (!is.null(covariates) && !is.null(covariate_funs)) {
    df <- df %>% mutate_predictors(covariates, covariate_funs)
    # Expand the set of modified column names after the mutate_predictors call.
    covariates <- names(unlist(covariate_funs))
  }

  # For ANCOVA or one-way ANOVA, parepare common var2 order to display un-adjusted/adjusted means everywhere.
  if (!is.null(covariates) || length(var2_col) == 1) {
    common_var2_order <- (df %>% ungroup() %>% group_by(!!rlang::sym(var2_col)) %>% summarize(mean=mean(!!rlang::sym(var1_col), na.rm=TRUE)) %>% arrange(desc(mean)))[[var2_col]]
  }

  anova_each <- function(df) {
    tryCatch({
      # Keep only the relevant columns.
      df <- df %>% dplyr::select(c(var1_col, var2_col, covariates))

      # Replace column names with names like c1_, c2_...
      clean_df <- df
      names(clean_df) <- paste0("c",1:length(colnames(clean_df)), "_")
      # Forward mapping of column names.
      name_map <- colnames(clean_df)
      names(name_map) <- colnames(df)
      # Reverse mapping of variable names.
      terms_mapping <- names(name_map)
      names(terms_mapping) <- name_map
      var1_col <- name_map[var1_col]
      var2_col <- name_map[var2_col]
      if (!is.null(covariates)) {
        covariates <- name_map[covariates]
      }
      df <- clean_df
      if (is.null(covariates)) { # 2-way/1-way ANOVA case
        if (with_interaction) {
          collapse_str <- "`*`"
        }
        else {
          collapse_str <- "`+`"
        }
        formula <- as.formula(paste0('`', var1_col, '`~`', paste(var2_col, collapse=collapse_str), '`'))
      }
      else { # ANCOVA case
        if (!with_interaction) {
          formula <- as.formula(paste0('`', var1_col, '`~`', var2_col, '`+`', paste(covariates, collapse="`+`"), '`'))
        }
        else { # Calculating interaction only with the first covariate for simplicity for now.
          formula <- as.formula(paste0('`', var1_col, '`~`', var2_col, '`*`', covariates[1], '`'))
        }
      }

      # For ANCOVA/2-way ANOVA case. Prepare contrasts setting to match SPSS.
      # http://www.statscanbefun.com/rblog/2015/8/27/ensuring-r-generates-the-same-anova-f-values-as-spss
      if (!is.null(covariates) || length(var2_col) > 1) {
        contrasts_list <- as.list(rep("contr.helmert", length(var2_col)))
        names(contrasts_list) <- var2_col
      }
      else {
        contrasts_list <- list()
      }

      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var1_col))) # Remove NA from the target column.
      if (nrow(df) == 0) {
        stop("There is no data left after removing NA.")
      }
      if (!is.null(outlier_filter_type)) { #TODO: duplicated code with exp_ttest.
        is_outlier <- function(x) {
          res <- detect_outlier(x, type=outlier_filter_type, threshold=outlier_filter_threshold) %in% c("Lower", "Upper")
          res
        }
        df$.is.outlier <- FALSE #TODO: handle possibility of name conflict.
        df$.is.outlier <- df$.is.outlier | is_outlier(df[[var1_col]])
        df <- df %>% dplyr::filter(!.is.outlier)
        df$.is.outlier <- NULL
      }

      if(length(grouped_cols) > 0) {
        # Check n_distinct again within group after handling outliers.
        # Group with NA and another category does not seem to work well with aov. Eliminating such case too. TODO: We could replace NA with an explicit level.
        for (i in 1:length(var2_col)) {
          n_distinct_res_each <- n_distinct(df[[var2_col[i]]], na.rm=TRUE)
          if (n_distinct_res_each < 2) {
            stop(paste0("The explanatory variable needs to have 2 or more unique values."))
          }
        }
      }
      # It seems that the 2nd row of broom:::tidy.aov(x) is missed, if no group has more than 1 row. Check it here, rather than handling it later.
      count_df <- df %>% group_by(!!!rlang::syms(as.character(var2_col))) %>% summarize(n=n()) %>% ungroup() %>% summarize(max_n=max(n),tot_n=sum(n))
      if (count_df$max_n <= 1) {
        e <- simpleError("At least one group needs to have 2 or more rows.")
        class(e) <- c("anova_exploratory", class(e))
        e$n <- count_df$tot_n
        return(e)
      }
      if (!is.null(covariates) || length(var2_col) > 1) {
        # For ANCOVA/2-way ANOVA, use lm() rather than aov(), since we need F statistic and P value as a lm in our summary later.
        model <- lm(formula, data = df, contrasts = contrasts_list, ...)
        # Calculate type 3 some of square and attach to the model.
        tryCatch({
          ss3 <- broom::tidy(car::Anova(model, type="III"))
        }, error = function(e) { # This can fail depending on the data.
          # With 2-way ANOVA with interaction, car::Anova(x, type="III") fails with "there are aliased coefficients in the model" when there are empty cells.
          if (with_interaction && stringr::str_detect(e$message, "there are aliased coefficients in the model")) {
            stop('EXP-ANA-9 :: [] :: Most likely there is a combination of categories with no rows. Try exclusing interaction.')
          }
          else {
            stop(e)
          }
        })
        model$ss3 <- ss3
      }
      else {
        model <- aov(formula, data = df)
      }
      # calculate Cohen's f from actual data #TODO: Support 2-way case. Also, is this valid for ANCOVA?
      if (length(var2_col) == 1) {
        model$cohens_f <- calculate_cohens_f(df[[var1_col]], df[[var2_col]])
      }
      if (is.null(f)) {
        model$cohens_f_to_detect <- model$cohens_f
      }
      else {
        model$cohens_f_to_detect <- f
      }
      class(model) <- c("anova_exploratory", class(model))
      model$var1 <- var1_col
      model$var2 <- var2_col
      if (!is.null(covariates)) {
        model$covariates <- covariates
      }
      if (!is.null(covariates) || length(var2_col) == 1) {
        model$common_var2_order <- as.character(common_var2_order) # as.character is to strip names.
      }
      model$terms_mapping <- terms_mapping
      model$data <- df
      model$test_sig_level <- test_sig_level
      model$sig.level <- sig.level
      model$power <- power
      model$with_interaction <- with_interaction
      model
    }, error = function(e){
      if(length(grouped_cols) > 0 && !str_detect(e$message, 'EXP-ANA')) {
        # In repeat-by case, we report group-specific error in the Summary table,
        # so that analysis on other groups can go on.
        # Also, since translation mechanism for EXP-ANA-xxx message is not there on the route with Note column,
        # fail over to just throwing error for those errors.
        class(e) <- c("anova_exploratory", class(e))
        e
      } else {
        stop(e)
      }
    })
  }
  do_on_each_group(df, anova_each, name = "model", with_unnest = FALSE)
}

#' @export
glance.anova_exploratory <- function(x) {
  if ("error" %in% class(x)) {
    ret <- tibble::tibble(Note = x$message)
    return(ret)
  }
  ret <- broom:::tidy.aov(x) %>% slice(1:1) # there is no glance.aov. take first row of tidy.aov.
  # Term value from tidy.aov() can be garbled on Windows with multibyte column name. Overwrite with not-garled value.
  if (!is.null(ret$term) && length(ret$term) > 0 && !is.null(x$xlevels) && length(x$xlevels) > 0) {
    ret$term[[1]] <- names(x$xlevels)[[1]]
  }
  ret
}

#' @export
tidy.anova_exploratory <- function(x, type="model", conf_level=0.95, pairs_adjust="none", levene_test_center="median", shapiro_seed=1, sort_factor_levels=FALSE) {
  if (type == "model") {
    if ("error" %in% class(x)) {
      if (is.null(x$message) || x$message == "") {
        # It seems there are some cases where x$message is an empty string. Get the error message with as.character().
        message <- as.character(x)
      }
      else {
        message <- x$message
      }
      # x$n can match something like x$nxyz. Check if it is numeric to make sure it is the number of rows we set.
      if (!is.null(x$n) && is.numeric(x$n)) {
        ret <- tibble::tibble(`Number of Rows`=x$n, Note = message)
      }
      else {
        ret <- tibble::tibble(Note = message)
      }
      return(ret)
    }
    note <- NULL

    one_way_anova <- is.null(x$covariates) && length(x$var2) == 1 # Power analysis is for one-way ANOVA case only.
    if (one_way_anova) { # one-way ANOVA case
      ret <- broom:::tidy.aov(x)
    } else { # ANCOVA/2-way ANOVA case
      ret <- x$ss3
    }
    if (one_way_anova) {
      # Get number of groups (k) , and the minimum sample size among those groups (min_n_rows).
      data_summary <- x$data %>% dplyr::group_by(!!rlang::sym(x$var2)) %>%
        dplyr::summarize(n_rows=length(!!rlang::sym(x$var1))) %>%
        dplyr::summarize(min_n_rows=min(n_rows), tot_n_rows=sum(n_rows), k=n())
      k <- data_summary$k
      # Using minimum group sample size as the sample size for power calculation.
      # Reference: https://www.theanalysisfactor.com/when-unequal-sample-sizes-are-and-are-not-a-problem-in-anova/
      min_n_rows <- data_summary$min_n_rows
      tot_n_rows <- data_summary$tot_n_rows
    }

    if (is.null(x$power)) {
      ret <- ret %>% dplyr::select(any_of(c("term", "sumsq", "df", "meansq", "statistic", "p.value")))
      if (one_way_anova) { # Power analysis is only for ANOVA case
        # If power is not specified in the arguments, estimate current power.
        tryCatch({ # pwr function can return error from equation resolver. Catch it rather than stopping the whole thing.
          power_res <- pwr::pwr.anova.test(k = k, n= min_n_rows, f = x$cohens_f_to_detect, sig.level = x$sig.level)
          power_val <- power_res$power
        }, error = function(e) {
          note <<- e$message
          power_val <<- NA_real_
        })
        ret <- ret %>% dplyr::mutate(f=c(!!(x$cohens_f), rep(NA, n()-1)), power=c(!!power_val, rep(NA, n()-1)), beta=c(1.0-!!power_val, rep(NA, n()-1)), n=c(!!tot_n_rows, rep(NA, n()-1)))
      }
      # Map the variable names in the term column back to the original.
      terms_mapping <- x$terms_mapping
      # Add mapping for interaction term
      terms_mapping <- c(terms_mapping,c(`c2_:c3_`=paste0(terms_mapping["c2_"], " * ", terms_mapping["c3_"])))
      orig_term <- terms_mapping[ret$term]
      orig_term[is.na(orig_term)] <- ret$term[is.na(orig_term)] # Fill the element that did not have a matching mapping. (Should be "Residual")
      ret$term <- orig_term
      if (one_way_anova) { # One-way ANOVA case
        ret <- ret %>% dplyr::add_row(sumsq = sum(ret$sumsq), df = sum(ret$df))
        ret <- ret %>% dplyr::mutate(ssr = sumsq/sumsq[3])
        ret <- ret %>% dplyr::relocate(ssr, .after = sumsq)
        ret <- ret %>% dplyr::mutate(term = c("Between Groups", "Within Groups", "Total"))
        ret <- ret %>% dplyr::rename(`Type of Variance`="term")
      }
      else { # ANCOVA/2-way ANOVA case
        total <- sum((x$data[[x$var1]]-mean(x$data[[x$var1]]))^2, na.rm=TRUE) # SS with subtracting mean.
        # total <- sum((broom:::tidy.aov(x))$sumsq) # Total SS could be calculated from summing up the type 1 SS, but tidy.aov does not work on x which is generated with lm() rather than aov().
        total0 <- sum(x$data[[x$var1]]^2, na.rm=TRUE) # SS without subtracting mean.
        total_df <- sum(ret$df)
        lm_summary <- broom:::glance.lm(x)
        model_sumsq <- total - (ret %>% filter(term=="Residuals"))$sumsq
        ret <- ret %>% dplyr::add_row(term="(Corrected Model)", sumsq = model_sumsq,
                                      statistic = lm_summary$statistic,
                                      p.value = lm_summary$p.value,
                                      df = lm_summary$df, .before = 1)
        ret <- ret %>% dplyr::mutate(meansq = sumsq/df)
        ret <- ret %>% dplyr::relocate(meansq, .after = df)
        ret <- ret %>% dplyr::add_row(term="(Total)", sumsq = total0, df = total_df)
        ret <- ret %>% dplyr::add_row(term="(Corrected Total)", sumsq = total, df = total_df-1)
        ret <- ret %>% dplyr::mutate(term = if_else(term=="Residuals", "(Residuals)", term))
        ret <- ret %>% dplyr::mutate(ssr = sumsq/!!total)
        ret <- ret %>% dplyr::relocate(ssr, .after = sumsq)
        ret <- ret %>% dplyr::rename(`Variable`="term")
      }
      ret <- ret %>% dplyr::rename(any_of(c(`F Value`="statistic",
                                            `P Value`="p.value",
                                            `Sum of Squares`="sumsq",
                                            `Degree of Freedom`="df",
                                            `Mean Square`="meansq",
                                            `SS Ratio`="ssr",
                                            `Effect Size (Cohen's f)`="f",
                                            `Power`="power",
                                            `Probability of Type 2 Error`="beta",
                                            `Number of Rows`="n")))
    }
    else { # Since we do not support power analysis for ANCOVA or 2-way ANOVA, this is only for one-way ANOVA case.
      # If required power is specified in the arguments, estimate required sample size. 
      tryCatch({ # pwr function can return error from equation resolver. Catch it rather than stopping the whole thing.
        power_res <- pwr::pwr.anova.test(k = k, f = x$cohens_f_to_detect, sig.level = x$sig.level, power = x$power)
        required_sample_size <- power_res$n
      }, error = function(e) {
        note <<- e$message
        required_sample_size <<- NA_real_
      })
      ret <- ret %>% dplyr::select(term, sumsq, df, meansq, statistic, p.value) %>%
        dplyr::mutate(f=c(!!(x$cohens_f), NA), power=c(!!(x$power), NA), beta=c(1.0-!!(x$power), NA)) %>%
        dplyr::mutate(current_sample_size=c(!!min_n_rows, NA), required_sample_size=c(!!required_sample_size, NA), n=c(!!tot_n_rows, NA))
      ret <- ret %>% dplyr::add_row(sumsq = sum(ret$sumsq), df = sum(ret$df))
      ret <- ret %>% dplyr::mutate(ssr = sumsq/sumsq[3])
      ret <- ret %>% dplyr::relocate(ssr, .after = sumsq)
      ret <- ret %>% dplyr::mutate(term = c("Between Groups", "Within Groups", "Total"))
      ret <- ret %>% dplyr::rename(`Type of Variance`=term,
                                   `F Value`=statistic,
                                   `P Value`=p.value,
                                   `Degree of Freedom`=df,
                                   `Sum of Squares`=sumsq,
                                   `SS Ratio`=ssr,
                                   `Mean Square`=meansq,
                                   `Effect Size (Cohen's f)`=f,
                                   `Target Power`=power,
                                   `Target Probability of Type 2 Error`=beta,
                                   `Current Sample Size (Each Group)`=current_sample_size,
                                   `Required Sample Size (Each Group)`=required_sample_size,
                                   `Number of Rows`=n)
    }
    if (!is.null(note)) { # Add Note column, if there was an error from pwr function.
      ret <- ret %>% dplyr::mutate(Note=!!note)
    }
  }
  else if (type == "emmeans") {
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    if (!is.null(x$covariates)) { # ANCOVA case
      if (!x$with_interaction) {
        formula <- as.formula(paste0('~`', x$var2, '`|`', paste(x$covariates, collapse='`+`'), '`'))
      } else {
        formula <- as.formula(paste0('~`', x$var2, '`|`', x$covariates[1], '`+', x$var2, ':', x$covariates[1]))
      }
    } else { # 2-way ANOVA case. The separator (*, :, or +) should not matter.
      formula <- as.formula(paste0('~`', paste(x$var2, collapse='`*`'), '`'))
    }
    ret <- emmeans::emmeans(x, formula)
    ret <- tibble::as.tibble(ret)
    if (!is.null(x$covariates)) { # ANCOVA case
      # For ANCOVA, join regular mean. [[1]] is necessary to remove name from x$var2.
      mean_df <- x$data %>% dplyr::group_by(!!rlang::sym(x$var2)) %>% dplyr::summarize(mean=mean(!!rlang::sym(x$var1), na.rm=TRUE))
      ret <- ret %>% dplyr::left_join(mean_df, by = x$var2[[1]])
    }
    # Set the common order to display means and emmeans.
    if (sort_factor_levels && !is.null(x$common_var2_order)) {
      ret <- ret %>% dplyr::mutate(!!rlang::sym(x$var2[[1]]):=forcats::fct_relevel(!!rlang::sym(x$var2[[1]]), x$common_var2_order))
    }
    # Map the column names back to the original.
    orig_terms <- x$terms_mapping[colnames(ret)]
    orig_terms[is.na(orig_terms)] <- colnames(ret)[is.na(orig_terms)] # Fill the column names that did not have a matching mapping.
    colnames(ret) <- orig_terms
    # Output example:
    # A tibble: 2 × 7
    # am       wt emmean    SE    df lower.CL upper.CL
    # <fct> <dbl>  <dbl> <dbl> <dbl>    <dbl>    <dbl>
    # 0      3.22   20.1 0.833    29     18.4     21.8
    # 1      3.22   20.1 1.07     29     17.9     22.3
    ret <- ret %>% dplyr::rename(any_of(c(`Adjusted Mean`="emmean",
                                          `Standard Error`="SE",
                                          `Degree of Freedom`="df",
                                          `Conf Low`="lower.CL",
                                          `Conf High`="upper.CL",
                                          `Mean`="mean")))
  }
  else if (type == "pairs") { # For ANCOVA or 2-way ANOVA
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    if (!is.null(x$covariates)) { # ANCOVA case
      if (!x$with_interaction) {
        formula <- as.formula(paste0('~`', x$var2, '`|`', paste(x$covariates, collapse='`+`'), '`'))
      } else {
        formula <- as.formula(paste0('~`', x$var2, '`|`', x$covariates[1], '`+', x$var2, ':', x$covariates[1]))
      }
    } else { # 1-way/2-way ANOVA case. The separator (*, :, or +) should not matter.
      formula <- as.formula(paste0('~`', paste(x$var2, collapse='`*`'), '`'))
    }
    emm_fit <- emmeans::emmeans(x, formula)
    pw_comp <- emmeans::contrast(emm_fit, "pairwise", adjust=pairs_adjust)
    ret <- tibble::as.tibble(pw_comp)
    ret <- ret %>% dplyr::mutate(contrast=stringr::str_replace_all(as.character(contrast), "(c2_|c3_)", ""))
    # Get confidence interval.
    emm_ci <- confint(pw_comp, level=0.95)
    ret <- ret %>% dplyr::mutate(conf.low=!!emm_ci$lower.CL, conf.high=!!emm_ci$upper.CL)
    ret <- ret %>% dplyr::relocate(conf.high, conf.low, .after=estimate)
    # Map the column names back to the original.
    orig_terms <- x$terms_mapping[colnames(ret)]
    orig_terms[is.na(orig_terms)] <- colnames(ret)[is.na(orig_terms)] # Fill the column names that did not have a matching mapping.
    colnames(ret) <- orig_terms
    # Example output:
    # A tibble: 1 × 7
    # contrast    `w t` estimate    SE    df t.ratio p.value
    # <fct>       <dbl>    <dbl> <dbl> <dbl>   <dbl>   <dbl>
    # c2_0 - c2_1  1.12     1.38  1.38    28    1.00   0.325
    ret <- ret %>% dplyr::rename(any_of(c(Pair="contrast",
                                          `Conf High`="conf.high",
                                          `Conf Low`="conf.low",
                                          `Standard Error`="SE",
                                          `Degree of Freedom`="df",
                                          `t Value`="t.ratio",
                                          `P Value`="p.value")))
    if (!is.null(x$covariates)) { # ANCOVA case
      ret <- ret %>% dplyr::rename(any_of(c(`Adjusted Difference`="estimate")))
    } else { # 2-way ANOVA case. For 2-way ANOVA, there is no adjustment here.
      ret <- ret %>% dplyr::rename(any_of(c(`Difference`="estimate")))
    }

    # The version that uses multcomp. It had an issue with column names with spaces.
    # ret <- eval(parse(text=paste0('multcomp::glht(x, linfct = multcomp::mcp(`', x$var2, '`="Tukey"))')))
    # ret <- broom::tidy(ret)
    # Output example:
    # A tibble: 1 × 7
    # term  contrast null.value estimate std.error statistic adj.p.value
    # <chr> <chr>         <dbl>    <dbl>     <dbl>     <dbl>       <dbl>
    # am    1 - 0             0  -0.0236      1.55   -0.0153       0.988
  }
  else if (type == "levene") {
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    # Levene's test of equality of error variances
    if (levene_test_center == "mean") {
      levene_test_center_fun <- mean
    }
    else {
      levene_test_center_fun <- median
    }

    if (!is.null(x$covariates)) { # ANCOVA case
      ret <- broom::tidy(car::leveneTest(x$residuals, x$data[[x$var2]], center=levene_test_center_fun))
    }
    else { # 2-way or 1-way ANOVA case
      formula <- as.formula(paste0('`', x$var1, '`~`', paste(x$var2, collapse='`*`'), '`'))
      ret <- broom::tidy(car::leveneTest(formula, data=x$data, center=levene_test_center_fun))
    }
    # Example output:
    # A tibble: 1 × 4
    # statistic p.value    df df.residual
    #  <dbl>   <dbl> <int>       <int>
    # 0.0607   0.807     1          30
    ret <- ret %>% dplyr::rename(any_of(c(`F Value`="statistic",
                                          `P Value`="p.value",
                                          `Degree of Freedom`="df",
                                          `Residual Degree of Freedom`="df.residual")))
  }
  else if (type == "shapiro") {
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    # Shapiro-Wilk test for residual normality
    if (length(x$residuals) > 5000) {
      if (!is.null(shapiro_seed)) {
        set.seed(shapiro_seed)
      }
      resid <- sample(x$residuals, 5000)
    }
    else {
      resid <- x$residuals
    }
    ret <- broom::tidy(shapiro.test(resid))
    ret$n = length(resid) # Add sample size info
    # Example output:
    # A tibble: 1 × 4
    # statistic p.value method                          n
    # <dbl>     <dbl>   <chr>                       <int>
    # 0.933     0.0483  Shapiro-Wilk normality test    32
    ret <- ret %>% dplyr::rename(any_of(c(`W Statistic`="statistic",
                                          `P Value`="p.value",
                                          `Method`="method",
                                          `Number of Rows`="n")))

  }
  else if (type == "data_summary") { #TODO consolidate with code in tidy.ttest_exploratory
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    conf_threshold = 1 - (1 - conf_level)/2
    ret <- x$data %>%
      dplyr::group_by(!!!rlang::syms(as.character(x$var2))) %>%
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
      dplyr::select(!!!rlang::syms(as.character(x$var2)),
                    `Number of Rows`,
                    Mean,
                    `Conf Low`,
                    `Conf High`,
                    `Std Error of Mean`,
                    `Std Deviation`,
                    `Minimum`,
                    `Maximum`)
    # Map the column names back to the original.
    orig_terms <- x$terms_mapping[colnames(ret)]
    orig_terms[is.na(orig_terms)] <- colnames(ret)[is.na(orig_terms)] # Fill the column names that did not have a matching mapping.
    colnames(ret) <- orig_terms
  }
  else if (type == "prob_dist") {
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    if (!is.null(x$covariates) || length(x$var2) > 1) { # ANCOVA or 2-way ANOVA case
      ret0 <- x$ss3
      # filter rows to extract the degree of freedoms (df1, df2) for the F-test.
      # df1 is from the categorical independent variable row, and df2 is from the residuals row.
      ret0 <- ret0 %>% filter(term %in% c(x$var2[1],"Residuals"))
      ret <- generate_ftest_density_data(ret0$statistic[[1]], df1=ret0$df[[1]], df2=ret0$df[[2]], sig_level=x$test_sig_level)
    } else { # one-way ANOVA case
      ret0 <- broom:::tidy.aov(x)
      ret <- generate_ftest_density_data(ret0$statistic[[1]], df1=ret0$df[[1]], df2=ret0$df[[2]], sig_level=x$test_sig_level)
    }
    ret
  }
  else { # type == "data"
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    ret <- x$data
    if (sort_factor_levels && !is.null(x$common_var2_order)) { # ANCOVA/one-way ANOVA and for the Means error bar.
      # Set the common order to display means and emmeans.
      ret <- ret %>% dplyr::mutate(!!rlang::sym(x$var2[[1]]):=forcats::fct_relevel(!!rlang::sym(x$var2[[1]]), x$common_var2_order))
    }
    # Map the column names back to the original.
    orig_terms <- x$terms_mapping[colnames(ret)]
    orig_terms[is.na(orig_terms)] <- colnames(ret)[is.na(orig_terms)] # Fill the column names that did not have a matching mapping.
    colnames(ret) <- orig_terms
  }
  ret
}

#' Kruskal-Wallis wrapper for Analytics View
#' @export
exp_kruskal <- function(df, var1, var2, func2 = NULL, test_sig_level = 0.05, ...) {
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
    stop(paste0("The explanatory variable needs to have 2 or more unique values."))
  }

  formula = as.formula(paste0('`', var1_col, '`~`', var2_col, '`'))

  each_func <- function(df) {
    tryCatch({
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(var1_col))) # Remove NA from the target column.
      if (nrow(df) == 0) {
        stop("There is no data left after removing NA.")
      }
      if(length(grouped_cols) > 0) {
        # Check n_distinct again within group after handling outliers.
        if (n_distinct(df[[var2_col]]) < 2) {
          stop(paste0("The explanatory variable needs to have 2 or more unique values."))
        }
      }
      model <- kruskal.test(formula, data = df, ...)
      N <- nrow(df)
      epsilon_squared <- calculate_epsilon_squared(model, N)
      class(model) <- c("kruskal_exploratory", class(model))
      model$var1 <- var1_col
      model$var2 <- var2_col
      model$data <- df
      model$epsilon_squared <- epsilon_squared
      model$test_sig_level <- test_sig_level
      model
    }, error = function(e){
      if(length(grouped_cols) > 0) {
        # In repeat-by case, we report group-specific error in the Summary table,
        # so that analysis on other groups can go on.
        class(e) <- c("kruskal_exploratory", class(e))
        e
      } else {
        stop(e)
      }
    })
  }
  do_on_each_group(df, each_func, name = "model", with_unnest = FALSE)
}

tidy.kruskal_exploratory <- function(x, type="model", conf_level=0.95) {
  if (type == "model") {
    if ("error" %in% class(x)) {
      ret <- tibble::tibble(Note = x$message)
      return(ret)
    }
    tot_n_rows <- nrow(x$data)
    note <- NULL
    ret <- broom:::tidy.htest(x)
    ret <- ret %>% dplyr::select(statistic, p.value) # Removed method since it is always "Kruskal-Wallis rank sum test" here.
    ret <- ret %>% dplyr::mutate(epsilon_squared=!!x$epsilon_squared, n=!!tot_n_rows)
    ret <- ret %>% dplyr::rename(`H Statistic` = statistic,
                                 `P Value`=p.value,
                                 `Effect Size (Epsilon Squared)`=epsilon_squared,
                                 `Number of Rows`=n)
    if (!is.null(note)) { # Add Note column, if there was an error from pwr function.
      ret <- ret %>% dplyr::mutate(Note=!!note)
    }
  }
  else if (type == "data_summary") { #TODO consolidate with code in tidy.ttest_exploratory
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
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
  else if (type == "prob_dist") {
    ret <- generate_chisq_density_data(x$statistic, x$parameter, sig_level=x$test_sig_level)
    ret
  }
  else { # type == "data"
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
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
  selected_cols <- tidyselect::vars_select(names(df), !!! rlang::quos(...))
  
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

  do_on_each_group(df, shapiro_each, name = "model", with_unnest = FALSE)
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
    ret <- ret %>% dplyr::rename(`Column`=col, `W Statistic`=statistic, `P Value`=p.value, `Normal Distribution`=normal, `Sample Size`=sample_size)
    ret
  }
}

#' dummy - Data frame. Since it is just ignored, it is named dummy here.
#' @export
exp_chisq_power <- function(dummy, rows=2, cols=2, w=0.1, sig.level=0.05, beta=0.2, n_start=10, n_end=1000, n_step=10) {
  power <- 1.0 - beta
  n = seq(n_start, n_end, by=n_step)

  chisq_power_each <- function(dummy) {
    df = (rows-1)*(cols-1) # Degree of freedom

    # Sample size vs power calculation
    n_to_power_res <- pwr::pwr.chisq.test(df=df, N=n, w=w, sig.level=sig.level)
    n_to_power <- tibble::tibble(n=n, power = n_to_power_res$power)

    # Required sample size calculation
    required_n <- (pwr::pwr.chisq.test(df=df, N=NULL, w=w, sig.level=sig.level, power=power))$N

    crit <- qchisq(1-sig.level, df=df) # The chisq value that corresponds to the significance level.
    density <- generate_chisq_density_data_for_power(df=df, w=w, N=required_n, crit)

    model <- list(n_to_power=n_to_power,
                  df=df,
                  w=w,
                  sig.level=sig.level,
                  beta=beta,
                  power=power,
                  required_n=required_n,
                  density=density)
    class(model) <- c("chisq_power_exploratory")
    model
  }
  do_on_each_group(dummy, chisq_power_each, name = "model", with_unnest = FALSE)
}

#' Chi-Square test power analysis function specialized for AB test. Rows and cols are fixed to 2, and Cohen's w is calculated from the conversion rate difference to detect, etc.
#' @export
exp_chisq_power_for_ab_test <- function(dummy, a_ratio=0.5, conversion_rate=0.1, diff=0.01, sig.level=0.05, beta=0.2, n_start=10, n_end=50000, n_step=10) {
  w <- calculate_cohens_w_for_ab_test(a_ratio, conversion_rate, diff)
  res <- exp_chisq_power(dummy, rows=2, cols=2, w=w, sig.level=sig.level, beta=beta, n_start=n_start, n_end=n_end, n_step=n_step)
  res
}

#' @export
tidy.chisq_power_exploratory <- function(x, type="summary") {
  if (type == "summary") {
    ret <- tibble::tibble(sig.level=x$sig.level, beta=x$beta, power=x$power, w=x$w, df=x$df, n=x$required_n)
    ret <- ret %>% dplyr::rename(any_of(c(`Probability of Type 1 Error`="sig.level",
                                          `Probability of Type 2 Error`="beta",
                                          `Power`="power",
                                          `Effect Size (Cohen's w)`="w",
                                          `Degree of Freedom`="df",
                                          `Required Sample Size`="n")))
  }
  else if (type == "n_to_power") {
    ret <- x$n_to_power
  }
  else if (type == "density") {
    ret <- x$density
  }
  ret
}

#' dummy - Data frame. Since it is just ignored, it is named dummy here.
#' @export
exp_ttest_power <- function(dummy, a_ratio=0.5, d=0.2, sig.level=0.05, beta=0.2, alternative="two.sided", paired=FALSE, n_start=10, n_end=1000, n_step=10) {
  power <- 1.0 - beta
  # Adjust n_start and n_end so that they are not too small. The smaller of s_start*a_ratio and n_start*(1-a_ratio) should be greater than 2.
  n_start <- max(n_start, 2/min(a_ratio,1-a_ratio))
  n_end <- max(n_end, n_start)
  n = seq(n_start, n_end, by=n_step)
  n1 = a_ratio*n
  n2 = (1-a_ratio)*n
  if (alternative == "less") {
    d_signed <- -d
  }
  else {
    d_signed <- d
  }

  ttest_power_each <- function(dummy) {
    # Sample size vs power calculation
    if (!paired) {
      n_to_power_res <- pwr::pwr.t2n.test(n1=n1, n2=n2, d=d_signed, sig.level=sig.level, alternative = alternative)
    }
    else {
      n_to_power_res <- pwr::pwr.t.test(n=n, d=d_signed, sig.level=sig.level, type="paired", alternative = alternative)
    }
    n_to_power <- tibble::tibble(n=n, power = n_to_power_res$power)

    # Required sample size calculation
    if (!paired) {
      required_n <- (pwr::pwr.t2nr.test(r=a_ratio, d=d_signed, sig.level=sig.level, power=power, alternative = alternative))$n
    }
    else {
      required_n <- (pwr::pwr.t.test(d=d_signed, sig.level=sig.level, power=power, type="paired", alternative = alternative))$n
    }


    if (!paired) {
      df <- required_n - 2 # Assuming Student's independent samples t-test sinde Welch's requires standard deviations as extra inputs.
    }
    else {
      df <- required_n - 1
    }
    if (alternative == "greater") {
      crit <- qt(1-sig.level, df=df) # The t statistic that corresponds to the significance level.
    }
    else if (alternative == "less") {
      crit <- qt(sig.level, df=df)
    }
    else { # "two.sided" case
      crit <- qt(1-sig.level/2, df=df)
    }

    if (!paired) {
      n1 = a_ratio*required_n
      n2 = (1-a_ratio)*required_n
    }
    else {
      n1 = required_n
      n2 = required_n
    }
    density <- generate_ttest_density_data_for_power(d=d, n1=n1, n2=n2, t=crit, df=df, sig_level = sig.level, alternative = alternative, paired = paired)

    model <- list(n_to_power=n_to_power,
                  df=df,
                  d=d,
                  sig.level=sig.level,
                  beta=beta,
                  power=power,
                  required_n=required_n,
                  density=density)
    class(model) <- c("ttest_power_exploratory")
    model
  }
  do_on_each_group(dummy, ttest_power_each, name = "model", with_unnest = FALSE)
}

#' @export
tidy.ttest_power_exploratory <- function(x, type="summary") {
  if (type == "summary") {
    ret <- tibble::tibble(sig.level=x$sig.level, beta=x$beta, power=x$power, d=x$d, df=x$df, n=x$required_n)
    ret <- ret %>% dplyr::rename(any_of(c(`Probability of Type 1 Error`="sig.level",
                                          `Probability of Type 2 Error`="beta",
                                          `Power`="power",
                                          `Effect Size (Cohen's d)`="d",
                                          `Degree of Freedom`="df",
                                          `Required Sample Size`="n")))
  }
  else if (type == "n_to_power") {
    ret <- x$n_to_power
  }
  else if (type == "density") {
    ret <- x$density
  }
  ret
}
