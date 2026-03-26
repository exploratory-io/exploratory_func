
# Generates data for t distribution probability density with critical section and statistic
# to depict a result of a t-test.
generate_ttest_density_data <- function(t, p.value, df, sig_level = 0.05, alternative = "two.sided") {
  l <- max(5, abs(t)*1.1) # limit of x for the data we generate here.

  x <- seq(from=-l,to=l,by=l/500 )
  ret <- tibble::tibble(x=x, y=dt(x, df=df))

  ret2 <- tibble::tibble(x=t, y=dt(x, df=df), statistic=TRUE, p.value=p.value[1])
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
generate_chisq_density_data <- function(stat, p.value, df, sig_level = 0.05) {
  tx <- qchisq(1-sig_level, df=df) # The chisq value that corresponds to the significance level.
  l <- max(df*3, stat*1.1, tx*1.1) # Making sure stat and tx are in the displayed range.

  x <- seq(from=0, to=l, by=l/1000 )
  ret <- tibble::tibble(x=x, y=dchisq(x, df=df))

  ret2 <- tibble::tibble(x=stat, y=dchisq(x, df=df), statistic=TRUE, p.value=p.value[1])
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
generate_ftest_density_data <- function(stat, p.value, df1, df2, sig_level = 0.05) {
  tx <- qf(1-sig_level, df1=df1, df2=df2) # The chisq value that corresponds to the significance level.
  l <- max(df1/df2*3, stat*1.1, tx*1.1) # Making sure stat and tx are in the displayed range.

  x <- seq(from=0,to=l,by=l/1000 )
  ret <- tibble::tibble(x=x, y=df(x, df1=df1, df2=df2))
  ret2 <- tibble::tibble(x=stat, y=df(x, df1=df1, df2=df2), statistic=TRUE, p.value=p.value[1])
  ret <- bind_rows(ret, ret2)
  ret <- ret %>% mutate(critical=x>=tx)
  ret <- ret %>% mutate(df1=df1, df2=df2)
  ret
}

# Generated data of a normal distribution with critical section and statistic for visualization.
# We currently use it for non-exact Wilcoxon test.
generate_norm_density_data <- function(z, p.value, mu, sigma, sig_level = 0.05, alternative = "two.sided") {
  r <- max(5*sigma, abs(z-mu)*1.1) # radius of x for the data we generate here.

  x <- seq(from=mu-r,to=mu+r,by=r/250)
  ret <- tibble::tibble(x=x, y=dnorm(x, mean=mu, sd=sigma))

  ret2 <- tibble::tibble(x=z, y=dnorm(z, mean=mu, sd=sigma), statistic=TRUE, p.value=p.value[1])
  ret <- bind_rows(ret, ret2)

  if (alternative == "two.sided") {
    tz_greater <- qnorm(1-sig_level/2, mean=mu, sd=sigma) # Threshold z for critical section.
    tz_less <- qnorm(sig_level/2, mean=mu, sd=sigma) # Threshold z for critical section.
    ret <- ret %>% mutate(critical=(x>=tz_greater|x<=tz_less))
  }
  else if (alternative == "greater") {
    tz <- qnorm(1-sig_level, mean=mu, sd=sigma) # Threshold z for critical section.
    ret <- ret %>% mutate(critical=(x>=tz))
  }
  else { # alternative == "less"
    tz <- qnorm(sig_level, mean=mu, sd=sigma) # Threshold z for critical section.
    ret <- ret %>% mutate(critical=(x<=tz))
  }
  ret <- ret %>% mutate(mean=mu, sd=sigma)
  ret
}

# Generates data for probability density chart for exact Wilcoxon test.
generate_wilcox_density_data <- function(stat, p.value, n1, n2, sig_level = 0.05, alternative = "two.sided") {
  from <- min(stat, qwilcox(sig_level/4, m=n1, n=n2)) # Start of the x axis range.
  to <- max(stat, qwilcox(1-sig_level/4, m=n1, n=n2)) # End of the x axis range.
  # Give some space around the data range.
  l <- to - from
  from <- from - l/10
  to <- to + l/10
  x <- seq(from=floor(from),to=ceiling(to)) # x has to be integer.

  ret <- tibble::tibble(x=x, y=dwilcox(x, m=n1, n=n2))

  # We need to take the mean of the density at the two closest integer values since non-integer density values are zero.
  ret2 <- tibble::tibble(x=stat, y=mean(dwilcox(c(floor(stat), ceiling(stat)), m=n1, n=n2)), statistic=TRUE, p.value=p.value[1])
  ret <- bind_rows(ret, ret2)

  if (alternative == "two.sided") {
    tx_greater <- qwilcox(1-sig_level/2, m=n1, n=n2) # Threshold x for critical section.
    tx_less <- qwilcox(sig_level/2, m=n1, n=n2) # Threshold x for critical section.
    ret <- ret %>% mutate(critical=(x>=tx_greater|x<=tx_less))
  }
  else if (alternative == "greater") {
    tx <- qwilcox(1-sig_level, m=n1, n=n2) # Threshold x for critical section.
    ret <- ret %>% mutate(critical=(x>=tx))
  }
  else { # alternative == "less"
    tx <- qwilcox(sig_level, m=n1, n=n2) # Threshold x for critical section.
    ret <- ret %>% mutate(critical=(x<=tx))
  }
  ret <- ret %>% mutate(n1=n1, n2=n2)
  ret
}

# Generates data for probability density chart for exact sign rank test.
generate_signrank_density_data <- function(stat, p.value, n, sig_level = 0.05, alternative = "two.sided") {
  from <- min(stat, qsignrank(sig_level/4, n=n)) # Start of the x axis range.
  to <- max(stat, qsignrank(1-sig_level/4, n=n)) # End of the x axis range.
  # Give some space around the data range.
  l <- to - from
  from <- from - l/10
  to <- to + l/10
  x <- seq(from=floor(from),to=ceiling(to)) # x has to be integer.
  
  ret <- tibble::tibble(x=x, y=dsignrank(x, n=n))

  # We need to take the mean of the density at the two closest integer values since non-integer density values are zero.
  ret2 <- tibble::tibble(x=stat, y=mean(dsignrank(c(floor(stat), ceiling(stat)), n=n)), statistic=TRUE, p.value=p.value[1])
  ret <- bind_rows(ret, ret2)

  if (alternative == "two.sided") {
    tx_greater <- qsignrank(1-sig_level/2, n=n) # Threshold x for critical section.
    tx_less <- qsignrank(sig_level/2, n=n) # Threshold x for critical section.
    ret <- ret %>% mutate(critical=(x>=tx_greater|x<=tx_less))
  }
  else if (alternative == "greater") {
    tx <- qsignrank(1-sig_level, n=n) # Threshold x for critical section.
    ret <- ret %>% mutate(critical=(x>=tx))
  }
  else { # alternative == "less"
    tx <- qsignrank(sig_level, n=n) # Threshold x for critical section.
    ret <- ret %>% mutate(critical=(x<=tx))
  }
  ret <- ret %>% mutate(n=n)

  # Smooth it out for visualization with LOESS. (dsignrank is not smooth unlike dwilcox.)
  span_value <- 0.5  # Larger span values result in more smoothing
  loess_model <- loess(y ~ x, data = ret, span = span_value)
  ret$y <- predict(loess_model)
  ret
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
    stop("Specify only one of Power or Type 2 Error, or they must add up to 1.0.")
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
      df <- df %>% dplyr::mutate(!!rlang::sym(var1_col):=forcats::fct_na_value_to_level(as.factor(!!rlang::sym(var1_col)), level = "(NA)"))

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
    ret <- generate_chisq_density_data(x$statistic, p.value=x$p.value, x$parameter, sig_level=x$test_sig_level)
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
  phi <- sqrt(x$statistic/N) 
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
    ret <- ret %>% dplyr::mutate(phi=!!phi, v=!!V, w=!!(x$cohens_w), power=!!power_val, beta=1.0-!!power_val, n=!!N)
    ret <- ret %>% dplyr::select(statistic, p.value, parameter, everything()) # Reorder to unify order with t-test.
    ret <- ret %>% dplyr::rename(`Chi-Square`=statistic,
                                 `P Value`=p.value,
                                 `DF`=parameter,
                                 `Phi`=phi,
                                 `Cramer's V`=v,
                                 `Cohen's W`=w,
                                 `Power`=power,
                                 `Type 2 Error`=beta,
                                 `Rows`=n)
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
    ret <- ret %>% dplyr::mutate(phi=!!phi, v=!!V, w=!!(x$cohens_w), power=!!(x$power), beta=1.0-!!(x$power), n=!!N, required_n=!!required_sample_size)
    ret <- ret %>% dplyr::select(statistic, p.value, parameter, everything()) # Reorder to unify order with t-test.
    ret <- ret %>% dplyr::rename(`Chi-Square`=statistic,
                                 `P Value`=p.value,
                                 `DF`=parameter,
                                 `Phi`=phi,
                                 `Cramer's V`=v,
                                 `Cohen's W`=w,
                                 `Target Power`=power,
                                 `Target Type 2 Error`=beta,
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
                      outlier_filter_type = NULL, outlier_filter_threshold = NULL, paired = FALSE,
                      ...) {
  if (!is.null(power) && !is.null(beta) && (power + beta != 1.0)) {
    stop("Specify only one of Power or Type 2 Error, or they must add up to 1.0.")
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
  # Use a different syntax for paired in R4.4.
  # https://bugs.r-project.org/show_bug.cgi?id=14359
  if (paired) {
    # For the paired t-test, we actually use the 2-vector interface
    # instead of the formula interface. See ttest_each for details. 
    # Please keep the follwing for the reference.
    formula = as.formula(paste0('Pair(`', var1_col, '`, `', var2_col, '`) ~ 1'))
  } else {
    formula = as.formula(paste0('`', var1_col, '`~`', var2_col, '`'))
  }

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

      if (paired) {
        # Split the target value column (var1_col) into two vectores 
        # by the category of the explanatory variable (var2_col).
        
        # First, we should check that the var2_col has only two categories.
        if (n_distinct(df[[var2_col]]) != 2) {
          stop("The explanatory variable needs to have 2 unique values.")
        }

        # Split the target value column (var1_col) into two vectores 
        # by the category of the explanatory variable (var2_col).
        var1_before <- df_test[[var1_col]][df_test[[var2_col]] == levels(df_test[[var2_col]])[1]]
        var1_after <- df_test[[var1_col]][df_test[[var2_col]] == levels(df_test[[var2_col]])[2]]

        # Check that the number of observations in both groups are the same.
        if (length(var1_before) != length(var1_after)) {
          stop("Paired t-test requires equal number of observations in both groups")
        }

        # Call t.test with the two vectors.
        model <- t.test(var1_before, var1_after, paired = TRUE, ...)
      } else {
        model <- t.test(formula, data = df_test, ...)
      }

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
        ret <- tibble::tibble(`Rows`=x$n1+x$n2, n1=x$n1, n2=x$n2, Note = x$message)
        ret <- ret %>% dplyr::rename(!!rlang::sym(paste0("Rows (", x$v1, ")")):=n1)
        ret <- ret %>% dplyr::rename(!!rlang::sym(paste0("Rows (", x$v2, ")")):=n2)
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

    # Calculate stderr.  Difference (estimate) / t-value (statistic)
    ret <- ret %>% dplyr::mutate(stderr = estimate / statistic)

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

      ret <- ret %>% dplyr::select(estimate, stderr, statistic, p.value, parameter, conf.low, conf.high, base.level) %>%
        dplyr::mutate(d=!!(x$cohens_d), power=!!power_val, beta=1.0-!!power_val) %>%
        dplyr::rename(`t Value`=statistic,
                      `P Value`=p.value,
                      `DF`=parameter,
                      Difference=estimate,
                      `Std Error`=stderr,
                      `Conf High`=conf.high,
                      `Conf Low`=conf.low,
                      `Base Level`=base.level,
                      `Cohen's D`=d,
                      `Power`=power,
                      `Type 2 Error`=beta)
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
      ret <- ret %>% dplyr::select(statistic, p.value, parameter, estimate, conf.low, conf.high, base.level) %>%
        dplyr::mutate(d=!!(x$cohens_d), power=!!(x$power), beta=1.0-!!(x$power)) %>%
        dplyr::mutate(current_sample_size=min(!!n1,!!n2), required_sample_size=required_sample_size) %>%
        dplyr::rename(`t Value`=statistic,
                      `P Value`=p.value,
                      `DF`=parameter,
                      Difference=estimate,
                      `Conf High`=conf.high,
                      `Conf Low`=conf.low,
                      `Base Level`=base.level,
                      `Cohen's D`=d,
                      `Target Power`=power,
                      `Target Type 2 Error`=beta,
                      `Current Sample Size (Each Group)`=current_sample_size,
                      `Required Sample Size (Each Group)`=required_sample_size)
    }
    ret <- ret %>% dplyr::mutate(`Rows`=!!(n1+n2))
    ret <- ret %>% dplyr::mutate(!!rlang::sym(paste0("Rows (", v1, ")")):=!!n1)
    ret <- ret %>% dplyr::mutate(!!rlang::sym(paste0("Rows (", v2, ")")):=!!n2)
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
        dplyr::summarize(`Rows`=length(!!rlang::sym(x$var1)),
                         Mean=mean(!!rlang::sym(x$var1), na.rm=TRUE),
                         `Std Deviation`=sd(!!rlang::sym(x$var1), na.rm=TRUE),
                         # std error definition: https://www.rdocumentation.org/packages/plotrix/versions/3.7/topics/std.error
                         `Std Error of Mean`=sd(!!rlang::sym(x$var1), na.rm=TRUE)/sqrt(sum(!is.na(!!rlang::sym(x$var1)))),
                         # Note: Use qt (t distribution) instead of qnorm (normal distribution) here.
                         # For more detail take a look at 10.5.1 A slight mistake in the formula of "Learning Statistics with R" 
                         `Conf High` = Mean + `Std Error of Mean` * qt(p=!!conf_threshold, df=`Rows`-1),
                         `Conf Low` = Mean - `Std Error of Mean` * qt(p=!!conf_threshold, df=`Rows`-1),
                         `Minimum`=min(!!rlang::sym(x$var1), na.rm=TRUE),
                         `Maximum`=max(!!rlang::sym(x$var1), na.rm=TRUE)) %>%
        dplyr::select(!!rlang::sym(x$var2),
                      `Rows`,
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
                      `Rows` = c(x$n1, x$n2),
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
    ret <- generate_ttest_density_data(x$statistic, p.value=x$p.value, x$parameter, sig_level=x$test_sig_level, alternative=x$alternative)
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

# Mean of the reference normal distribution (non-exact cases) for Wilcoxon test.
wilcox_norm_dist_mean <- function(alternative, paired, statistic, n1, n2) {
  if (!paired) {
    if (alternative == "two.sided") {
      if (statistic - n1*n2/2 >= 0) {
        correction <- 0.5
      }
      else {
        correction <- -0.5
      }
    }
    else if (alternative == "greater") {
      correction <- 0.5
    }
    else if (alternative == "less") {
      correction <- -0.5
    }
    else {
      stop("Invalid alternative value.")
    }
    return(correction + n1*n2/2) 
  }
  else {
    if (alternative == "two.sided") {
      if (statistic - n1*(n1 + 1)/4 >= 0) {
        correction <- 0.5
      }
      else {
        correction <- -0.5
      }
    }
    else if (alternative == "greater") {
      correction <- 0.5
    }
    else if (alternative == "less") {
      correction <- -0.5
    }
    else {
      stop("Invalid alternative value.")
    }
    return(correction + n1*(n1 + 1)/4) 
  }
}

# Standard deviation of the reference normal distribution (non-exact cases) for Wilcoxon test.
wilcox_norm_dist_sd <- function(alternative, paired, statistic, n1, n2, tie_counts) {
  if (!paired) {
    sqrt(n1*n2/12*(n1 + n2 + 1 - sum(tie_counts^3 - tie_counts)/((n1 + n2)*(n1 + n2 -1))))
  }
  else {
    sqrt(n1*(n1 + 1)*(2*n1 + 1)/6 - sum(tie_counts^3 - tie_counts)/12)/2
  }
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
      ret <- ret %>% dplyr::select(statistic, p.value, estimate, conf.low, conf.high, method)
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
      ret <- ret %>% dplyr::rename(`W Value`=statistic)
    }
    else if (stringr::str_detect(ret$method[[1]], "rank sum")) { # Intentionally matching with just "rank sum" to match "Wilcoxon rank sum exact test" too.
      ret <- ret %>% dplyr::rename(`U Value`=statistic)
    }

    if (!is.null(x$estimate)) { # Result is with estimate and confidence interval
      # wilcox.test, just like t.test, seems to consider the 2nd category based on alphabetical/numerical/factor sort as the base category.
      # Since group_by/summarize also sorts the group based on alphabetical/numerical/factor order, we can assume that the v2 is the base category.
      ret <- ret %>% dplyr::mutate(base.level = !!x$base.level)
      ret <- ret %>% dplyr::relocate(base.level, .after = conf.high)
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

    ret <- ret %>% dplyr::mutate(`Rows`=!!(n1+n2))
    ret <- ret %>% dplyr::mutate(!!rlang::sym(paste0("Rows (", v1, ")")):=!!n1)
    ret <- ret %>% dplyr::mutate(!!rlang::sym(paste0("Rows (", v2, ")")):=!!n2)
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
      dplyr::summarize(`Rows`=length(!!rlang::sym(x$var1)),
                       Mean=mean(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Std Deviation`=sd(!!rlang::sym(x$var1), na.rm=TRUE),
                       # std error definition: https://www.rdocumentation.org/packages/plotrix/versions/3.7/topics/std.error
                       `Std Error of Mean`=sd(!!rlang::sym(x$var1), na.rm=TRUE)/sqrt(sum(!is.na(!!rlang::sym(x$var1)))),
                       # Note: Use qt (t distribution) instead of qnorm (normal distribution) here.
                       # For more detail take a look at 10.5.1 A slight mistake in the formula of "Learning Statistics with R" 
                       `Conf High` = Mean + `Std Error of Mean` * qt(p=conf_threshold, df=`Rows`-1),
                       `Conf Low` = Mean - `Std Error of Mean` * qt(p=conf_threshold, df=`Rows`-1),
                       `Minimum`=min(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Maximum`=max(!!rlang::sym(x$var1), na.rm=TRUE)) %>%
      dplyr::select(!!rlang::sym(x$var2),
                    `Rows`,
                    Mean,
                    `Conf Low`,
                    `Conf High`,
                    `Std Error of Mean`,
                    `Std Deviation`,
                    `Minimum`,
                    `Maximum`)
  }
  else if (type == "prob_dist") {
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    tie_counts <- table(x$data[[x$var1]])
    if (x$n1 < 50 && x$n2 < 50 && max(tie_counts) <= 1) { # Use exact method as wilcox.test does.
      if (x$paired) {
        ret <- generate_signrank_density_data(x$statistic, p.value=x$p.value, x$n1, sig_level=x$test_sig_level, alternative=x$alternative)
      }
      else {
        ret <- generate_wilcox_density_data(x$statistic, p.value=x$p.value, x$n1, x$n2, sig_level=x$test_sig_level, alternative=x$alternative)
      }
    }
    else {
      mu <- wilcox_norm_dist_mean(x$alternative, x$paired, x$statistic, x$n1, x$n2)
      sigma <- wilcox_norm_dist_sd(x$alternative, x$paired, x$statistic, x$n1, x$n2, tie_counts)
      ret <- generate_norm_density_data(x$statistic, p.value=x$p.value, mu, sigma, sig_level=x$test_sig_level, alternative=x$alternative)
    }
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


# Function to perform the transformation
# column_list: e.g. list("col1", new_col=c("col2", "col3", "col4"))
gather_repeated_measures <- function(df, column_list, value_col_name) {
  new_col_name <- names(column_list)[1]
  cols_to_gather <- column_list[[1]]
  
  if (length(column_list) > 1) {
    cols_to_keep <- column_list[[1]]
    cols_to_gather <- column_list[[2]]
    new_col_name <- names(column_list[2])
  }
  
  # filter out rows that have NA in the columns to gather
  df <- df %>% filter(across(all_of(cols_to_gather), ~ !is.na(.)))

  df_transformed <- df %>%
    dplyr::select(if (length(column_list) > 1) all_of(cols_to_keep), all_of(cols_to_gather)) %>%
    tidyr::pivot_longer(
      cols = all_of(cols_to_gather),
      names_to = new_col_name,
      values_to = value_col_name
    )
  
  return(df_transformed)
}

# Returns the column names in the long-format data returned by gather_repeated_measures is applied.
get_gather_repeated_measures_colnames <- function(column_list) {
  if (length(column_list) > 1) { # This means 2-way repeated-measures ANOVA.
    cols_to_keep <- column_list[[1]]
    new_col_name <- names(column_list[2])
  } else { # This means 1-way repeated-measures ANOVA.
    cols_to_keep <- NULL
    new_col_name <- names(column_list[1])
  }
  
  resulting_colnames <- c(cols_to_keep, new_col_name)
  return(resulting_colnames)
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

# Returns a data frame for pairwise contrast. This is a common utility function for "pairs" and "pairs_per_variable" type of the tidier.
get_pairwise_contrast_df <- function(x, formula, pairs_adjust) {
  if (is.null(x$lm.model)) {
    emm_fit <- emmeans::emmeans(x, formula)
  } else {
    # If lm.model is provided, it is One-way ANOVA. 
    # Use the lm model for post-hoc test.
    if (x$var.equal) {
      emm_fit <- emmeans::emmeans(x$lm.model, formula)
    } else {
      # For unequal variances case, use sandwich::vcovHC() 
      # to get heteroscedasticity-consistent standard errors.
      emm_fit <- emmeans::emmeans(x$lm.model, formula, vcov = sandwich::vcovHC)
    }
  }

  if (length(levels(emm_fit)) >=2 && length(levels(emm_fit)$c3_) >= 2) { # 2-way ANOVA (repeated measures or regular)
    c2_levels <- levels(emm_fit)$c2_
    c3_levels <- levels(emm_fit)$c3_
    # Map the levels to numbers, so that we can stably parse the output contrast column.
    levels(emm_fit)$c2_ <- 1:length(levels(emm_fit)$c2_)
    levels(emm_fit)$c3_ <- 1:length(levels(emm_fit)$c3_)
    pw_comp <- emmeans::contrast(emm_fit, "pairwise", adjust=pairs_adjust, enhance.levels=FALSE)
    ret <- tibble::as.tibble(pw_comp)
    # Get confidence interval.
    emm_ci <- confint(pw_comp, level=0.95)
    # Bind confint to the base table immediately before arrange().
    ret <- ret %>% dplyr::mutate(conf.low=!!emm_ci$lower.CL, conf.high=!!emm_ci$upper.CL)
    ret <- ret %>% dplyr::relocate(conf.low, conf.high, .after=estimate)

    ret <- ret %>% tidyr::separate(contrast, into = c("pair1", "pair2"), sep = " - ", extra = "merge")
    ret <- ret %>% tidyr::separate(pair1, into = c("pair1_1", "pair1_2"), sep = " ", extra = "merge")
    ret <- ret %>% tidyr::separate(pair2, into = c("pair2_1", "pair2_2"), sep = " ", extra = "merge")
    ret <- ret %>% mutate(pair1_1=factor(c2_levels[as.integer(pair1_1)], levels=c2_levels))
    ret <- ret %>% mutate(pair1_2=factor(c3_levels[as.integer(pair1_2)], levels=c3_levels))
    ret <- ret %>% mutate(pair2_1=factor(c2_levels[as.integer(pair2_1)], levels=c2_levels))
    ret <- ret %>% mutate(pair2_2=factor(c3_levels[as.integer(pair2_2)], levels=c3_levels))
    ret <- ret %>% arrange(pair1_1, pair1_2, pair2_1, pair2_2)
    # Concat pair1_1 and pair1_2 with ":" and name it as "Group 1".
    # Concat pair2_1 and pair2_2 with ":" and name it as "Group 2".
    ret <- ret %>% dplyr::mutate(`Group 1`=stringr::str_c(pair1_1, " : ", pair1_2))
    ret <- ret %>% dplyr::mutate(`Group 2`=stringr::str_c(pair2_1, " : ", pair2_2)) 
    ret <- ret %>% dplyr::select(-pair1_1, -pair1_2, -pair2_1, -pair2_2)
    ret <- ret %>% dplyr::select(`Group 1`, `Group 2`, everything())

  }
  else { # Only 1 variable. Either c2_ or c3_.
    if (!is.null(levels(emm_fit)$c2_)) {# c2_ case
      var_levels <- levels(emm_fit)$c2_
      levels(emm_fit)$c2_ <- 1:length(levels(emm_fit)$c2_)
    }
    else { # c3_ case
      var_levels <- levels(emm_fit)$c3_
      levels(emm_fit)$c3_ <- 1:length(levels(emm_fit)$c3_)
    }
    pw_comp <- emmeans::contrast(emm_fit, "pairwise", adjust=pairs_adjust, enhance.levels=FALSE)
    ret <- tibble::as.tibble(pw_comp)
    # Get confidence interval.
    emm_ci <- confint(pw_comp, level=0.95)
    # Bind confint to the base table immediately before arrange().
    ret <- ret %>% dplyr::mutate(conf.low=!!emm_ci$lower.CL, conf.high=!!emm_ci$upper.CL)
    ret <- ret %>% dplyr::relocate(conf.low, conf.high, .after=estimate)

    ret <- ret %>% tidyr::separate(contrast, into = c("var1", "var2"), sep = " - ", extra = "merge")
    if (length(levels(emm_fit)) >=2) { # This means ANCOVA case. Strip the covariate value after the independent variable. e.g. "2 1.97101449275362"
      ret <- ret %>% mutate(var1=stringr::str_remove(var1, " .*"))
      ret <- ret %>% mutate(var2=stringr::str_remove(var2, " .*"))
    }
    ret <- ret %>% mutate(var1=var_levels[as.integer(var1)])
    ret <- ret %>% mutate(var2=var_levels[as.integer(var2)])
    ret <- ret %>% arrange(var1, var2)
  }


  # Map the column names back to the original.
  orig_terms <- x$terms_mapping[colnames(ret)]
  orig_terms[is.na(orig_terms)] <- colnames(ret)[is.na(orig_terms)] # Fill the column names that did not have a matching mapping.
  colnames(ret) <- orig_terms
  # Example output:
  # A tibble: 1 × 7
  # contrast    `w t` estimate    SE    df t.ratio p.value
  # <fct>       <dbl>    <dbl> <dbl> <dbl>   <dbl>   <dbl>
  # c2_0 - c2_1  1.12     1.38  1.38    28    1.00   0.325
  ret <- ret %>% dplyr::rename(any_of(c(
                                        `Pair 1 Var 1`="pair1_1",
                                        `Pair 1 Var 2`="pair1_2",
                                        `Pair 2 Var 1`="pair2_1",
                                        `Pair 2 Var 2`="pair2_2",
                                        `Group 1`="var1",
                                        `Group 2`="var2",
                                        `Conf High`="conf.high",
                                        `Conf Low`="conf.low",
                                        `Standard Error`="SE",
                                        `DF`="df",
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

  method <- switch(pairs_adjust,
    "none" = "Pairwise T-Test with No Adjustment",
    "tukey" = "Tukey's HSD Test",
    "bonferroni" = "Pairwise T-Test with Bonferroni Correction",
    "sheffe" = "Sheffe's Method",
    "sidak" = "Pairwise T-Test with Sidak Correction",
    "dunnett" = "Dunnett's Test",
    "holm" = "Pairwise T-Test with Holm Correction",
    "hochberg" = "Pairwise T-Test with Hochberg Correction"
  )
  ret <- ret %>% dplyr::mutate(`Method`=!!method)
  ret
}

#' @export
tidy.anova_exploratory <- function(x, type="model", conf_level=0.95, pairs_adjust="none", levene_test_center="median", shapiro_seed=1, sort_factor_levels=FALSE) {
  if (type %in% c("model", "between", "within")) {
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
        ret <- tibble::tibble(`Rows`=x$n, Note = message)
      }
      else {
        ret <- tibble::tibble(Note = message)
      }
      return(ret)
    }
    note <- NULL
    # Power analysis is for one-way ANOVA case only.
    one_way_anova_without_repeated_measures <- is.null(x$covariates) && (length(x$var2) == 1) && !x$with_repeated_measures
    if (one_way_anova_without_repeated_measures) { # one-way ANOVA case
      # broom::tidy doesn't support oneway.test model.
      # Use the broom::tidy like output saved in x$model.tidy.
      ret <- x$model.tidy
    } else if (x$with_repeated_measures) { # For repeated measures ANOVA, we need to extract results from Anova.mlm object from car package.
      x_summary <- summary(x$Anova)
      x_matrix <- matrix(as.numeric(x_summary$univariate.tests), ncol=ncol(x_summary$univariate.tests))
      colnames(x_matrix) <- colnames(x_summary$univariate.tests)
      ret <- as.data.frame(x_matrix)
      ret$term <- rownames(x_summary$univariate.tests)
      # Add info on p-value adjustment for departure from sphericity.
      ret2 <- as.data.frame(x_summary$pval.adjustments)
      ret2$term <- rownames(ret2)
      ret <- ret %>% dplyr::rename(p.value='Pr(>F)')
      total <- sum((x$dataframe[[x$var1]]-mean(x$dataframe[[x$var1]]))^2, na.rm=TRUE) # SS with subtracting mean.
      ret <- ret %>% dplyr::mutate(`Eta Squared`=`Sum Sq`/!!total)
      ret <- ret %>% dplyr::mutate(`Partial Eta Squared`=`Sum Sq`/(`Sum Sq`+`Error SS`))
      # Remove pointless negative eta squared.
      ret <- ret %>% dplyr::mutate(`Cohen's F`=ifelse(`Eta Squared`<1, sqrt(`Eta Squared`/(1-`Eta Squared`)), NA_real_))
      ret <- ret %>% dplyr::mutate(`Omega Squared`=(`Sum Sq`-`num Df`*(`Error SS`/`den Df`))/(total+(`Error SS`/`den Df`)))
      ret_err <- ret %>% dplyr::select(term, `Sum Sq`="Error SS", df="den Df")
      ret <- ret %>% select(-`Error SS`, -`den Df`, df="num Df")
      ret_gg <- ret2 %>% dplyr::select(term, eps="GG eps", p.value="Pr(>F[GG])") %>% dplyr::mutate(correction="Greenhouse-Geisser")
      ret_hf <- ret2 %>% dplyr::select(term, eps="HF eps", p.value="Pr(>F[HF])") %>% dplyr::mutate(correction="Huynh-Feldt")
      ret_gg <- ret_gg %>% dplyr::mutate(`Type of Variance`="Between Subjects")
      ret_hf <- ret_hf %>% dplyr::mutate(`Type of Variance`="Between Subjects")
      ret <- ret %>% dplyr::mutate(`Type of Variance`="Between Subjects")
      ret_err <- ret_err %>% dplyr::mutate(`Type of Variance`="Within Subjects")
      ret <- dplyr::bind_rows(ret, ret_err)
      # Rename to make Total/Corrected Total row addition code consistent with other cases.
      ret <- ret %>% dplyr::rename(sumsq=`Sum Sq`)

      if (any(!is.na(ret_gg$eps))) ret <- dplyr::bind_rows(ret, ret_gg)
      if (any(!is.na(ret_hf$eps))) ret <- dplyr::bind_rows(ret, ret_hf)
    } else { # ANCOVA/2-way ANOVA case
      ret <- x$ss3
    }
    if (one_way_anova_without_repeated_measures) {
      # Get number of groups (k) , and the minimum sample size among those groups (min_n_rows).
      data_summary <- x$dataframe %>% dplyr::group_by(!!rlang::sym(x$var2)) %>%
        dplyr::summarize(n_rows=length(!!rlang::sym(x$var1))) %>%
        dplyr::summarize(min_n_rows=min(n_rows), tot_n_rows=sum(n_rows), k=n())
      k <- data_summary$k
      # Using minimum group sample size as the sample size for power calculation.
      # Reference: https://www.theanalysisfactor.com/when-unequal-sample-sizes-are-and-are-not-a-problem-in-anova/
      min_n_rows <- data_summary$min_n_rows
      tot_n_rows <- data_summary$tot_n_rows
    }

    if (x$with_repeated_measures) {
      # Map ANOVA table rows to SPSS style with the following rules.
      if (length(x$var2) == 1) { # one-way case
        # Within Subjects,c2_ => Within Subjects,Error(c2_)
        ret <- ret %>% mutate(term=ifelse(`Type of Variance`=="Within Subjects"&term=="c2_","Error(c2_)",term))
        # Within Subjects,(Intercept) => Between Subjects,(Error)
        ret <- ret %>% mutate(term = ifelse(`Type of Variance` == "Within Subjects" & term == "(Intercept)", "(Error)", term), `Type of Variance` = ifelse(`Type of Variance` == "Within Subjects" & term == "(Error)", "Between Subjects", `Type of Variance`))
        # Between Subjects,c2_ => Within Subjects,c2_
        ret <- ret %>% mutate(`Type of Variance` = ifelse(`Type of Variance` == "Between Subjects" & term == "c2_", "Within Subjects", `Type of Variance`))
      }
      else { # 2-way case
        # Within Subjects,c2_:c3_ => remove
        # Within Subjects,c3_ => remove
        ret <- ret %>% filter(!(`Type of Variance`=="Within Subjects" & term=="c2_:c3_" | `Type of Variance`=="Within Subjects" & term=="c2_"))
        # Within Subjects,c3_ => Within Subjects,Error(c3_)
        ret <- ret %>% mutate(term=ifelse(`Type of Variance`=="Within Subjects"&term=="c3_","Error(c3_)",term))
        # Within Subjects,(Intercept) => Between Subjects,(Error)
        ret <- ret %>% mutate(term = ifelse(`Type of Variance` == "Within Subjects" & term == "(Intercept)", "(Error)", term), `Type of Variance` = ifelse(`Type of Variance` == "Within Subjects" & term == "(Error)", "Between Subjects", `Type of Variance`))
        # Between Subjects,c3_ => Within Subjects,c3_
        ret <- ret %>% mutate(`Type of Variance` = ifelse(`Type of Variance` == "Between Subjects" & term == "c3_", "Within Subjects", `Type of Variance`))
        # Between Subjects,c2_:c3_ => Within Subjects,c2_:c3_
        ret <- ret %>% mutate(`Type of Variance` = ifelse(`Type of Variance` == "Between Subjects" & term == "c2_:c3_", "Within Subjects", `Type of Variance`))
      }
      # Sort by the type of variance and the order of appearance of the terms.
      ret <- ret %>% arrange(`Type of Variance`, match(term, unique(term)))

      # Map the variable names in the term column back to the original.
      terms_mapping <- x$terms_mapping
      # Add mapping for interaction term and error term.
      terms_mapping <- c(terms_mapping,c(`c2_:c3_`=paste0(terms_mapping["c2_"], " * ", terms_mapping["c3_"])))
      terms_mapping <- c(terms_mapping,c(`Error(c3_)`=paste0("Error(", terms_mapping["c3_"], ")")))
      terms_mapping <- c(terms_mapping,c(`Error(c2_)`=paste0("Error(", terms_mapping["c2_"], ")")))
      orig_term <- terms_mapping[ret$term]
      orig_term[is.na(orig_term)] <- ret$term[is.na(orig_term)] # Fill the element that did not have a matching mapping. (Should be "Residual")
      ret$term <- orig_term
      ret <- ret %>% dplyr::mutate(`Mean Square`=sumsq/df)
      ret <- ret %>% dplyr::mutate(ssr=sumsq/!!total)

      # Relocate term column to the first column.
      ret <- ret %>% dplyr::relocate(`Type of Variance`, term, .before = 1)
      ret <- ret %>% dplyr::relocate(`Mean Square`, .after=df)
      ret <- ret %>% dplyr::relocate(ssr, .after=sumsq)
      if (!is.null(ret$correction)) {
        ret <- ret %>% dplyr::relocate(eps, .before = p.value)
        ret <- ret %>% dplyr::relocate(correction, .after = term)
        ret <- ret %>% dplyr::rename(`Correction`="correction",
                                     `Epsilon`="eps"
        )
      }
      ret <- ret %>% dplyr::rename(`Variable`="term", `P Value`="p.value",
                                   `Sum of Squares`="sumsq",
                                   `SS Ratio`="ssr",
                                   `DF` = "df",
                                   `F Value` = "F value"
      )
      if (type == "between") {
        ret <- ret %>% dplyr::filter(`Type of Variance` == "Between Subjects") %>% dplyr::select(-`Type of Variance`)
      }
      else if (type == "within") {
        ret <- ret %>% dplyr::filter(`Type of Variance` == "Within Subjects") %>% dplyr::select(-`Type of Variance`)
      }
      # Because of the filtering above, it is possible that the Correction/Epsilon column is all NA. If so, remove it.
      if (!is.null(ret$Correction) && all(is.na(ret$Correction))) {
        ret <- ret %>% dplyr::select(-`Correction`, -`Epsilon`)
      }
      ret
    }
    else if (is.null(x$power)) {
      ret <- ret %>% dplyr::select(any_of(c("term", "sumsq", "df", "meansq", "statistic", "p.value")))
      if (one_way_anova_without_repeated_measures) { # Power analysis is only for ANOVA case
        # If power is not specified in the arguments, estimate current power.
        tryCatch({ # pwr function can return error from equation resolver. Catch it rather than stopping the whole thing.
          power_res <- pwr::pwr.anova.test(k = k, n= min_n_rows, f = x$cohens_f_to_detect, sig.level = x$sig.level)
          power_val <- power_res$power
        }, error = function(e) {
          note <<- e$message
          power_val <<- NA_real_
        })
        ret <- ret %>% dplyr::mutate(power=c(!!power_val, rep(NA, n()-1)), beta=c(1.0-!!power_val, rep(NA, n()-1)), n=c(!!tot_n_rows, rep(NA, n()-1)))
      }
      # Map the variable names in the term column back to the original.
      terms_mapping <- x$terms_mapping
      # Add mapping for interaction term
      terms_mapping <- c(terms_mapping,c(`c2_:c3_`=paste0(terms_mapping["c2_"], " * ", terms_mapping["c3_"])))
      orig_term <- terms_mapping[ret$term]
      orig_term[is.na(orig_term)] <- ret$term[is.na(orig_term)] # Fill the element that did not have a matching mapping. (Should be "Residual")
      ret$term <- orig_term
      if (one_way_anova_without_repeated_measures) { # One-way ANOVA case
        total <- sum(ret$sumsq)
        total_df <- sum(ret$df)
        error_sumsq <- (ret %>% filter(term=="Residuals"))$sumsq
        error_meansq <- (ret %>% filter(term=="Residuals"))$meansq
        ret <- ret %>% dplyr::mutate(`Eta Squared`=sumsq/!!total)
        ret <- ret %>% dplyr::mutate(`Partial Eta Squared`=sumsq/(sumsq+error_sumsq))
        ret <- ret %>% dplyr::mutate(`Cohen's F`=ifelse(`Eta Squared`<1, sqrt(`Eta Squared`/(1-`Eta Squared`)), NA_real_))
        ret <- ret %>% dplyr::mutate(`Omega Squared`=(sumsq-df*error_meansq)/(total+error_meansq))
        # Set NA to the above effect sizes if the term is "Residuals" or "(Corrected Model)".
        ret <- ret %>% dplyr::mutate(`Eta Squared`=ifelse(term %in% c("Residuals"), NA_real_, `Eta Squared`))
        ret <- ret %>% dplyr::mutate(`Partial Eta Squared`=ifelse(term %in% c("Residuals"), NA_real_, `Partial Eta Squared`))
        ret <- ret %>% dplyr::mutate(`Cohen's F`=ifelse(term %in% c("Residuals"), NA_real_, `Cohen's F`))
        ret <- ret %>% dplyr::mutate(`Omega Squared`=ifelse(term %in% c("Residuals"), NA_real_, `Omega Squared`))

        ret <- ret %>% dplyr::add_row(sumsq = total, df = total_df)
        ret <- ret %>% dplyr::mutate(ssr = sumsq/total)
        ret <- ret %>% dplyr::relocate(ssr, .after = sumsq)
        # x$terms_mapping[[2]] is the column name of the explanatory variable.
        ret <- ret %>% dplyr::mutate(term = c(x$terms_mapping[[2]], "(Residuals)", "(Total)"))
        ret <- ret %>% dplyr::rename(`Type of Variance`="term")
      }
      else { # ANCOVA/2-way ANOVA case
        total <- sum((x$dataframe[[x$var1]]-mean(x$dataframe[[x$var1]]))^2, na.rm=TRUE) # SS with subtracting mean.
        # total <- sum((broom:::tidy.aov(x))$sumsq) # Total SS could be calculated from summing up the type 1 SS, but tidy.aov does not work on x which is generated with lm() rather than aov().
        total0 <- sum(x$dataframe[[x$var1]]^2, na.rm=TRUE) # SS without subtracting mean.
        total_df <- sum(ret$df)
        lm_summary <- broom:::glance.lm(x)
        error_sumsq <- (ret %>% filter(term=="Residuals"))$sumsq
        model_sumsq <- total - error_sumsq
        # Exclude "(Corrected Model)" row for now.
        # ret <- ret %>% dplyr::add_row(term="(Corrected Model)", sumsq = model_sumsq,
        #                               statistic = lm_summary$statistic,
        #                               p.value = lm_summary$p.value,
        #                               df = lm_summary$df, .before = 1)
        ret <- ret %>% dplyr::mutate(meansq = sumsq/df)
        error_meansq <- (ret %>% filter(term=="Residuals"))$meansq
        ret <- ret %>% dplyr::relocate(meansq, .after = df)

        ret <- ret %>% dplyr::mutate(`Eta Squared`=sumsq/!!total)
        ret <- ret %>% dplyr::mutate(`Partial Eta Squared`=sumsq/(sumsq+error_sumsq))
        # Remove pointless negative eta squared.
        ret <- ret %>% dplyr::mutate(`Cohen's F`=ifelse(`Eta Squared`<1, sqrt(`Eta Squared`/(1-`Eta Squared`)), NA_real_))
        ret <- ret %>% dplyr::mutate(`Omega Squared`=(sumsq-df*error_meansq)/(total+error_meansq))
        # Set NA to the above effect sizes if the term is "Residuals" or "(Corrected Model)".
        ret <- ret %>% dplyr::mutate(`Eta Squared`=ifelse(term %in% c("Residuals", "(Corrected Model)"), NA_real_, `Eta Squared`))
        ret <- ret %>% dplyr::mutate(`Partial Eta Squared`=ifelse(term %in% c("Residuals", "(Corrected Model)"), NA_real_, `Partial Eta Squared`))
        ret <- ret %>% dplyr::mutate(`Cohen's F`=ifelse(term %in% c("Residuals", "(Corrected Model)"), NA_real_, `Cohen's F`))
        ret <- ret %>% dplyr::mutate(`Omega Squared`=ifelse(term %in% c("Residuals", "(Corrected Model)"), NA_real_, `Omega Squared`))
        ret <- ret %>% dplyr::add_row(term="(Total)", sumsq = total, df = total_df-1)
        ret <- ret %>% dplyr::mutate(term = if_else(term=="Residuals", "(Residuals)", term))
        ret <- ret %>% dplyr::mutate(ssr = sumsq/!!total)
        ret <- ret %>% dplyr::relocate(ssr, .after = sumsq)
        # Remove "(Intercept)" from the result.
        ret <- ret %>% dplyr::filter(term!="(Intercept)")
        ret <- ret %>% dplyr::rename(`Variable`="term")
      }
      ret <- ret %>% dplyr::rename(any_of(c(`F Value`="statistic",
                                            `P Value`="p.value",
                                            `Sum of Squares`="sumsq",
                                            `DF`="df",
                                            `Mean Square`="meansq",
                                            `SS Ratio`="ssr",
                                            `Effect Size (Cohen's f)`="f",
                                            `Power`="power",
                                            `Type 2 Error`="beta",
                                            `Rows`="n")))
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
      # x$terms_mapping[[2]] is the column name of the explanatory variable.
      ret <- ret %>% dplyr::mutate(term = c(x$terms_mapping[[2]], "(Residuals)", "(Total)"))
      ret <- ret %>% dplyr::rename(`Type of Variance`=term,
                                   `F Value`=statistic,
                                   `P Value`=p.value,
                                   `DF`=df,
                                   `Sum of Squares`=sumsq,
                                   `SS Ratio`=ssr,
                                   `Mean Square`=meansq,
                                   `Effect Size (Cohen's f)`=f,
                                   `Target Power`=power,
                                   `Target Type 2 Error`=beta,
                                   `Current Sample Size (Each Group)`=current_sample_size,
                                   `Required Sample Size (Each Group)`=required_sample_size,
                                   `Rows`=n)
    }
    if (!is.null(note)) { # Add Note column, if there was an error from pwr function.
      ret <- ret %>% dplyr::mutate(Note=!!note)
    }
  }
  else if (type == "multivariate") {
    mvt <- summary(x$Anova)$multivariate.tests
    ret <- NULL
    for (var_name in names(mvt)) {
      var_mvt <- mvt[[var_name]]
      eigs <- Re(eigen(qr.coef(qr(var_mvt$SSPE), var_mvt$SSPH), symmetric = FALSE)$values)
      car_ns <- getNamespace("car")
      # Iterate over results from different test methods.
      for (func_name in c("Pillai", "Wilks", "HL", "Roy")) {
        var_mvt_res <- get(func_name, envir=car_ns)(eigs, var_mvt$df, var_mvt$df.residual)
        names(var_mvt_res) <- c("test stat", "approx F", "num Df", "den Df")
        var_mvt_res_df <- as.data.frame(as.list(var_mvt_res))
        var_mvt_res_df <- var_mvt_res_df %>% dplyr::mutate(term=var_name, method=func_name)
        if (is.null(ret)) {
          ret <- var_mvt_res_df
        }
        else {
          ret <- dplyr::bind_rows(ret, var_mvt_res_df)
        }
      }
    }
    ret <- ret %>% mutate(p.value=pf(approx.F, num.Df, den.Df, lower.tail = FALSE))
    # Map the variable names in the term column back to the original.
    terms_mapping <- x$terms_mapping
    # Add mapping for interaction term
    terms_mapping <- c(terms_mapping,c(`c2_:c3_`=paste0(terms_mapping["c2_"], " * ", terms_mapping["c3_"])))
    orig_term <- terms_mapping[ret$term]
    orig_term[is.na(orig_term)] <- ret$term[is.na(orig_term)] # Fill the element that did not have a matching mapping. (Should be "Residual")
    ret$term <- orig_term
    ret <- ret %>% dplyr::relocate(term, method, .before = 1)
    ret <- ret %>% dplyr::rename(`Variable`="term", `Method`="method", `Test Statistic`="test.stat", `Approximate F Value`="approx.F", `Hypothesis DF`="num.Df", `Error DF`="den.Df", `P Value`="p.value")
  }
  else if (type == "sphericity") {
    summary_x <- summary(x$Anova)
    s_matrix <- as.matrix(summary_x$sphericity.tests)
    class(s_matrix) <- "matrix" # Necessary to make it work with as.data.frame.
    ret <- as.data.frame(s_matrix)
    ret ["term"] <- rownames(ret)
    # Map the variable names in the term column back to the original.
    terms_mapping <- x$terms_mapping
    # Add mapping for interaction term
    terms_mapping <- c(terms_mapping,c(`c2_:c3_`=paste0(terms_mapping["c2_"], " * ", terms_mapping["c3_"])))
    orig_term <- terms_mapping[ret$term]
    orig_term[is.na(orig_term)] <- ret$term[is.na(orig_term)] # Fill the element that did not have a matching mapping. (Should be "Residual")
    ret$term <- orig_term
    # Relocate term column to the first column.
    ret <- ret %>% dplyr::relocate(term, .before = 1)
    ret <- ret %>% dplyr::rename(`Variable`="term", `W Value`="Test statistic", `P Value`="p-value")
    ret <- ret %>% dplyr::mutate(`Method`="Mauchly's Sphericity Test")
    ret <- ret %>% dplyr::mutate(`Result`=ifelse(`P Value` < x$test_sig_level, "Assumption is not valid.", "Assumption is valid."))
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
    # emmeans seems to work even with afex_aov objects, as well as car::Anova objects or aov objects.
    ret <- emmeans::emmeans(x, formula)
    ret <- tibble::as.tibble(ret)
    if (!is.null(x$covariates)) { # ANCOVA case
      conf_threshold = 1 - (1 - conf_level)/2

      # For ANCOVA, join regular mean. [[1]] is necessary to remove name from x$var2.
      mean_df <- x$dataframe %>% 
        dplyr::group_by(!!rlang::sym(x$var2)) %>% 
        dplyr::summarize(`Rows`=length(!!rlang::sym(x$var1)),
          Mean=mean(!!rlang::sym(x$var1), na.rm=TRUE),
          `Std Deviation`=sd(!!rlang::sym(x$var1), na.rm=TRUE),
          # std error definition: https://www.rdocumentation.org/packages/plotrix/versions/3.7/topics/std.error
          `Std Error`=sd(!!rlang::sym(x$var1), na.rm=TRUE)/sqrt(sum(!is.na(!!rlang::sym(x$var1)))),
          # Note: Use qt (t distribution) instead of qnorm (normal distribution) here.
          # For more detail take a look at 10.5.1 A slight mistake in the formula of "Learning Statistics with R" 
          `Conf Low` = Mean - `Std Error` * qt(p=!!conf_threshold, df=`Rows`-1),
          `Conf High` = Mean + `Std Error` * qt(p=!!conf_threshold, df=`Rows`-1),
          `Minimum`=min(!!rlang::sym(x$var1), na.rm=TRUE),
          `Maximum`=max(!!rlang::sym(x$var1), na.rm=TRUE))

      ret <- ret %>% dplyr::rename(any_of(c(`Mean (Adj)`="emmean",
                                            `Std Error (Adj)`="SE",
                                            `DF`="df",
                                            `Conf Low (Adj)`="lower.CL",
                                            `Conf High (Adj)`="upper.CL",
                                            `Mean`="mean")))

      ret <- ret %>% 
        dplyr::left_join(mean_df, by = x$var2[[1]]) %>%
        dplyr::relocate(any_of(c("Mean (Adj)", 
                                 "Std Error (Adj)",
                                 "Conf Low (Adj)", 
                                 "Conf High (Adj)",
                                 "DF")), .after=`Conf High`)

    }
    # Set the common order to display means and emmeans.
    if (sort_factor_levels && !is.null(x$common_var2_order)) {
      # as.character() is needed to avoid error when the var2 column is logical.
      ret <- ret %>% dplyr::mutate(!!rlang::sym(x$var2[[1]]):=forcats::fct_relevel(as.character(!!rlang::sym(x$var2[[1]])), x$common_var2_order))
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
                                          `DF`="df",
                                          `Conf Low`="lower.CL",
                                          `Conf High`="upper.CL",
                                          `Mean`="mean")))
  }
  else if (type == "pairs") {
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
    ret <- get_pairwise_contrast_df(x, formula, pairs_adjust)
  }
  else if (type == "pairs_per_variable") { # For only 2-way ANOVA
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    formula1 <- as.formula(paste0('~`', x$var2[1], '`'))
    formula2 <- as.formula(paste0('~`', x$var2[2], '`'))
    # Cast "Group 1" and "Group 2" columns to character to match the data type for bind_rows.
    ret <- get_pairwise_contrast_df(x, formula1, pairs_adjust) %>% 
      dplyr::mutate(`Variable`=(x$terms_mapping[x$var2[1]]), `Group 1`=as.character(`Group 1`), `Group 2`=as.character(`Group 2`)) %>% 
      dplyr::select(`Variable`, everything())
    ret2 <- get_pairwise_contrast_df(x, formula2, pairs_adjust) %>% 
      dplyr::mutate(`Variable`=(x$terms_mapping[x$var2[2]]), `Group 1`=as.character(`Group 1`), `Group 2`=as.character(`Group 2`)) %>% 
      dplyr::select(`Variable`, everything())
    ret <- ret %>% dplyr::bind_rows(ret2)
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
      # x$model[[x$var2]] is the data used to build the model.
      ret <- broom::tidy(car::leveneTest(x$residuals, x$model[[x$var2]], center=levene_test_center_fun))
    }
    else { # 2-way or 1-way ANOVA case
      formula <- as.formula(paste0('`', x$var1, '`~`', paste(x$var2, collapse='`*`'), '`'))
      ret <- broom::tidy(car::leveneTest(formula, data=x$dataframe, center=levene_test_center_fun))
    }
    # Example output:
    # A tibble: 1 × 4
    # statistic p.value    df df.residual
    #  <dbl>   <dbl> <int>       <int>
    # 0.0607   0.807     1          30
    ret <- ret %>% dplyr::rename(any_of(c(`F Value`="statistic",
                                          `P Value`="p.value",
                                          `DF`="df",
                                          `Residual DF`="df.residual")))
    if (levene_test_center == "mean") {
      ret <- ret %>% dplyr::mutate(`Method`="Levene's Test")
    }
    else { # Levene's test with median as the center is called Brown-Forsythe test. https://search.r-project.org/CRAN/refmans/misty/html/test.levene.html
      ret <- ret %>% dplyr::mutate(`Method`="Brown-Forsythe Test")
    }
    ret <- ret %>% dplyr::mutate(`Result`=ifelse(`P Value` < x$test_sig_level, "Homogeneity assumption is not valid.", "Homogeneity assumption is valid."))
  }
  else if (type == "shapiro") {
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    # Shapiro-Wilk test for residual normality
    if (x$with_repeated_measures) { # For afex::aov_car return, x$residuals is not available.
      resid <- residuals(x)
    }
    else {
      resid <- x$residuals
    }
    if (length(resid) > 5000) {
      if (!is.null(shapiro_seed)) {
        set.seed(shapiro_seed)
      }
      resid <- sample(resid, 5000)
    }
    ret <- broom::tidy(shapiro.test(resid))
    ret$n = length(resid) # Add sample size info
    # Example output:
    # A tibble: 1 × 4
    # statistic p.value method                          n
    # <dbl>     <dbl>   <chr>                       <int>
    # 0.933     0.0483  Shapiro-Wilk normality test    32
    ret <- ret %>% dplyr::rename(any_of(c(`W Value`="statistic",
                                          `P Value`="p.value",
                                          `Method`="method",
                                          `Rows`="n")))
    ret <- ret %>% dplyr::mutate(`Method`="Shapiro-Wilk Normality Test") # Just making it in Title Case.
    ret <- ret %>% dplyr::mutate(`Result`=ifelse(`P Value` < x$test_sig_level, "Normality assumption is not valid.", "Normality assumption is valid."))

  }
  else if (type == "data_summary") { #TODO consolidate with code in tidy.ttest_exploratory
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    conf_threshold = 1 - (1 - conf_level)/2
    ret <- x$dataframe %>%
      dplyr::group_by(!!!rlang::syms(as.character(x$var2))) %>%
      dplyr::summarize(`Rows`=length(!!rlang::sym(x$var1)),
                       Mean=mean(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Std Deviation`=sd(!!rlang::sym(x$var1), na.rm=TRUE),
                       # std error definition: https://www.rdocumentation.org/packages/plotrix/versions/3.7/topics/std.error
                       `Std Error`=sd(!!rlang::sym(x$var1), na.rm=TRUE)/sqrt(sum(!is.na(!!rlang::sym(x$var1)))),
                       # Note: Use qt (t distribution) instead of qnorm (normal distribution) here.
                       # For more detail take a look at 10.5.1 A slight mistake in the formula of "Learning Statistics with R" 
                       `Conf High` = Mean + `Std Error` * qt(p=!!conf_threshold, df=`Rows`-1),
                       `Conf Low` = Mean - `Std Error` * qt(p=!!conf_threshold, df=`Rows`-1),
                       `Minimum`=min(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Maximum`=max(!!rlang::sym(x$var1), na.rm=TRUE)) %>%
      dplyr::select(!!!rlang::syms(as.character(x$var2)),
                    `Rows`,
                    Mean,
                    `Std Error`,
                    `Conf Low`,
                    `Conf High`,
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
    if (x$with_repeated_measures) { # Repeated measures ANOVA case - x is afex_aov class.
      # Get summary for P-Value.
      x_summary <- summary(x$Anova)
      x_matrix <- matrix(as.numeric(x_summary$univariate.tests), ncol=ncol(x_summary$univariate.tests))
      colnames(x_matrix) <- colnames(x_summary$univariate.tests)
      ret0 <- as.data.frame(x_matrix)
      ret <- generate_ftest_density_data(x$anova_table$F[1], p.value=ret0$p.value, df1=x$anova_table$`num Df`[1], df2=x$anova_table$`den Df`[1], sig_level=x$test_sig_level)
    } else if (!is.null(x$covariates) || length(x$var2) > 1) { # ANCOVA or 2-way ANOVA case
      ret0 <- x$ss3
      # filter rows to extract the degree of freedoms (df1, df2) for the F-test.
      # df1 is from the categorical independent variable row, and df2 is from the residuals row.
      ret0 <- ret0 %>% filter(term %in% c(x$var2[1],"Residuals"))
      ret <- generate_ftest_density_data(ret0$statistic[[1]], p.value=ret0$p.value, df1=ret0$df[[1]], df2=ret0$df[[2]], sig_level=x$test_sig_level)
    } else { # one-way ANOVA case
      # broom::tidy() doesn't support oneway.test model.
      # Pass the metrics to generate_ftest_density_data directly.
      ret <- generate_ftest_density_data(
        x$statistic, 
        p.value=x$p.value, 
        df1=x$parameter[1], 
        df2=x$parameter[2], 
        sig_level=x$test_sig_level
      )
    }
    ret
  }
  else { # type == "data"
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    ret <- x$dataframe
    if (x$with_repeated_measures) {
      # Remove the subject_id column.
      ret <- ret %>% dplyr::select(-subject_id)
    }
    if (sort_factor_levels && !is.null(x$common_var2_order)) { # ANCOVA/one-way ANOVA and for the Means error bar.
      # Set the common order to display means and emmeans.
      # as.character() is needed to avoid error when the var2 column is logical.
      ret <- ret %>% dplyr::mutate(!!rlang::sym(x$var2[[1]]):=forcats::fct_relevel(as.character(!!rlang::sym(x$var2[[1]])), x$common_var2_order))
    }
    # Map the column names back to the original.
    orig_terms <- x$terms_mapping[colnames(ret)]
    orig_terms[is.na(orig_terms)] <- colnames(ret)[is.na(orig_terms)] # Fill the column names that did not have a matching mapping.
    colnames(ret) <- orig_terms
  }
  ret
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
    ret <- ret %>% dplyr::mutate(df=!!x$parameter, effsize=!!x$effsize, epsilon_squared=!!x$epsilon_squared, n=!!tot_n_rows)
    ret <- ret %>% dplyr::rename(`H Value` = statistic,
                                 `P Value`=p.value,
                                 `DF`=df,
                                 `Epsilon Squared`=epsilon_squared,
                                 `Eta Squared`=effsize,
                                 `Rows`=n)
    if (!is.null(note)) { # Add Note column, if there was an error from pwr function.
      ret <- ret %>% dplyr::mutate(Note=!!note)
    }
  }
  else if (type == "pairs") {
    ret <- tibble::as_tibble(x$dunn.test)
    ret <- ret %>% dplyr::select(-chi2, -P)
    ret <- ret %>% dplyr::select(`Group 1`, `Group 2`, everything())
    ret <- ret %>% dplyr::rename(`Z Value`=Z,
                                 `P Value`=P.adjusted)
    method <- switch(x$pairs_adjust,
      "none" = "Dunn's Test with No Adjustment",
      "bonferroni" = "Dunn's Test with Bonferroni Correction",
      "sidak" = "Dunn's Test with Sidak Correction",
      "holm" = "Dunn's Test with Holm Correction",
      "hochberg" = "Dunn's Test with Hochberg Correction"
    )
    ret <- ret %>% dplyr::mutate(`Method`=!!method)
  }
  else if (type == "data_summary") { #TODO consolidate with code in tidy.ttest_exploratory
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    conf_threshold = 1 - (1 - conf_level)/2
    ret <- x$data %>% dplyr::group_by(!!rlang::sym(x$var2)) %>%
      dplyr::summarize(`Rows`=length(!!rlang::sym(x$var1)),
                       Mean=mean(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Std Deviation`=sd(!!rlang::sym(x$var1), na.rm=TRUE),
                       # std error definition: https://www.rdocumentation.org/packages/plotrix/versions/3.7/topics/std.error
                       `Std Error of Mean`=sd(!!rlang::sym(x$var1), na.rm=TRUE)/sqrt(sum(!is.na(!!rlang::sym(x$var1)))),
                       # Note: Use qt (t distribution) instead of qnorm (normal distribution) here.
                       # For more detail take a look at 10.5.1 A slight mistake in the formula of "Learning Statistics with R" 
                       `Conf High` = Mean + `Std Error of Mean` * qt(p=!!conf_threshold, df=`Rows`-1),
                       `Conf Low` = Mean - `Std Error of Mean` * qt(p=!!conf_threshold, df=`Rows`-1),
                       `Minimum`=min(!!rlang::sym(x$var1), na.rm=TRUE),
                       `Maximum`=max(!!rlang::sym(x$var1), na.rm=TRUE)) %>%
      dplyr::select(!!rlang::sym(x$var2),
                    `Rows`,
                    Mean,
                    `Conf Low`,
                    `Conf High`,
                    `Std Error of Mean`,
                    `Std Deviation`,
                    `Minimum`,
                    `Maximum`)
  }
  else if (type == "prob_dist") {
    if ("error" %in% class(x)) {
      ret <- tibble::tibble()
      return(ret)
    }
    ret <- generate_chisq_density_data(x$statistic, p.value=x$p.value, x$parameter, sig_level=x$test_sig_level)
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
    ret <- ret %>% dplyr::mutate(normal=ifelse(p.value > !!signif_level, "Normality assumption is valid.", "Normality assumption is not valid."))
    ret <- ret %>% dplyr::select(-method)
    ret <- ret %>% dplyr::rename(`Column`=col, `W Value`=statistic, `P Value`=p.value, `Result`=normal, `Sample Size`=sample_size)
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


#' @export
tidy.chisq_power_exploratory <- function(x, type="summary") {
  if (type == "summary") {
    ret <- tibble::tibble(sig.level=x$sig.level, beta=x$beta, power=x$power, w=x$w, df=x$df, n=x$required_n)
    ret <- ret %>% dplyr::rename(any_of(c(`Type 1 Error`="sig.level",
                                          `Type 2 Error`="beta",
                                          `Power`="power",
                                          `Cohen's W`="w",
                                          `DF`="df",
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


#' @export
tidy.ttest_power_exploratory <- function(x, type="summary") {
  if (type == "summary") {
    ret <- tibble::tibble(sig.level=x$sig.level, beta=x$beta, power=x$power, d=x$d, df=x$df, n=x$required_n)
    ret <- ret %>% dplyr::rename(any_of(c(`Type 1 Error`="sig.level",
                                          `Type 2 Error`="beta",
                                          `Power`="power",
                                          `Cohen's D`="d",
                                          `DF`="df",
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
