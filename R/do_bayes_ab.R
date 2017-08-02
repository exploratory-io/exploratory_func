#' Run bayesTest from bayesAB package
#' @param df Data frame to run bayes ab test
#' @param a_b_identifier A column with 2 unique values to distinguish groups
#' @param total_count Column of the total count
#' @param conversion_rate Column of the rate of success
#' @param prior_mean Mean of prior beta distribution
#' @param prior_sd Standard deviation of prior beta distribution
#' The default value with 0.5 prior_mean is uniform distribution
#' 0.288675 is the sd of [0,1] uniform distribution sqrt(1/3 -1/2 + 1/4)
#' @param type Type of output
#' * model - Returns a data frame with bayesTest model.
#' * summary - Output summary of the result of the test.
#' * prior - Output coordinates of prior density chart.
#' * posteriors - Output coordinates of posterior density chart of the success rate.
#' * improvement - Output coordinate of histogram of lift, which is the ratio of performance improvement of A over B. The formula is (A - B) / B.
#' @param seed Random seed for bayes test to estimate probability density.
#' @export
do_bayes_ab <- function(df, a_b_identifier, total_count, conversion_rate, prior_mean = NULL, prior_sd = NULL, type = "model", seed = 0, ...){
  set.seed(seed)

  # when type is prior, no need to evaluate other parameters
  if (type != "prior") {
    # this seems to be the new way of NSE column selection evaluation
    # ref: https://github.com/tidyverse/tidyr/blob/3b0f946d507f53afb86ea625149bbee3a00c83f6/R/spread.R
    a_b_identifier_col <- dplyr::select_var(names(df), !! rlang::enquo(a_b_identifier))
    total_count_col <- dplyr::select_var(names(df), !! rlang::enquo(total_count))
    conversion_rate_col <- dplyr::select_var(names(df), !! rlang::enquo(conversion_rate))

    # make a_b identifier column to factor if they are numeric or character
    if (is.character(df[[a_b_identifier_col]]) || is.numeric(df[[a_b_identifier_col]])) {
      df[[a_b_identifier_col]] <- forcats::fct_inorder(as.character(df[[a_b_identifier_col]]))
    }

    # convert a_b_identifier_col from factor to logical
    fct_lev <- NULL
    if (is.factor(df[[a_b_identifier_col]])) {
      fct_lev <- levels(df[[a_b_identifier_col]])
      if (length(levels(df[[a_b_identifier_col]])) != 2) {
        stop("A/B must be 2 unique identifiers")
      }
      # first factor is group A, so TRUE and FALSE should be swapped
      df[[a_b_identifier_col]] <- !as.logical(as.integer(df[[a_b_identifier_col]]) - 1)
    }

    if(any(df[[conversion_rate_col]] < 0 | df[[conversion_rate_col]] > 1)) {
      stop("Conversion rate must be between 0 and 1")
    }
  }

  grouped_col <- grouped_by(df)

  # this will be executed to each group
  each_func <- function(df, ...){
    group_prior_mean <- if(is.null(prior_mean)){
      mean(df[[conversion_rate_col]], na.rm = TRUE)
    } else {
      prior_mean
    }

    group_prior_sd <- if(is.null(prior_sd)){
      sd(df[[conversion_rate_col]], na.rm = TRUE)
    } else {
      prior_sd
    }

    if(group_prior_mean <= 0 || 1 <= group_prior_mean) {
      stop("Average of CR must be between 0 and 1")
    }

    # calculate alpha and beta
    # https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
    group_prior_var <- group_prior_sd^2
    alpha <- ((1 - group_prior_mean) / group_prior_var - 1 / group_prior_mean) * group_prior_mean ^ 2
    beta <- alpha * (1 / group_prior_mean - 1)

    # validate alpha and beta
    # when they are invalid, sd is too large
    if (!(!is.na(alpha) && !is.na(beta) && alpha > 0 && beta > 0)){
      stop("SD of CR is too large")
    }

    if (type == "prior") {
      # this returns coordinates of density chart of prior
      return(data.frame(
        conversion_rate_pct = seq(0, 1, 0.001) * 100,
        probability_density = dbeta(seq(0, 1, 0.001), shape1 = alpha, shape2 = beta)
      ))
    }
    # get a, b subset data
    data_a <- df[df[[a_b_identifier_col]], ]
    data_b <- df[!df[[a_b_identifier_col]], ]

    # calculate sum of total count
    data_a_total <- sum(data_a[[total_count_col]], na.rm = TRUE)
    data_b_total <- sum(data_b[[total_count_col]], na.rm = TRUE)
    # calculate sum of success count
    data_a_conv_total <- sum(round(data_a[[total_count_col]] * data_a[[conversion_rate_col]]), na.rm = TRUE)
    data_b_conv_total <- sum(round(data_b[[total_count_col]] * data_b[[conversion_rate_col]]), na.rm = TRUE)

    # expand the data to TRUE and FALSE raw data
    bin_a <- c(rep(TRUE, data_a_conv_total), rep(FALSE, data_a_total - data_a_conv_total))
    bin_b <- c(rep(TRUE, data_b_conv_total), rep(FALSE, data_b_total - data_b_conv_total))

    bayes_model <- bayesAB::bayesTest(
      bin_a,
      bin_b,
      priors = c(alpha = alpha, beta = beta),
      n_samples = 1e5,
      distribution = 'bernoulli'
    )

    # save factor levels if the AB identifier is 2 levels factor
    if(!is.null(fct_lev)){
      bayes_model$ab_identifier <- fct_lev
    } else {
      c(TRUE, FALSE)
    }

    bayes_model
  }

  ret <- do_on_each_group(df, each_func, name = "model", with_unnest = FALSE, params = substitute(list(...)))

  if(type == "model"){
    ret
  } else if (type == "prior") {
    # expand nested data frame of prior distribution
    ret %>%
      dplyr::ungroup() %>%
      unnest_with_drop(model)
  } else {
    tidy(ret, model, type = type, ...)
  }
}

#' Estimate alpha and beta for prior beta distribution
#' @param df Data frame
#' @param rate A column that has success rate
#' @export
calc_beta_prior <- function(df, rate, ...){
  rate_col <- col_name(substitute(rate))
  grouped_col <- grouped_by(df)

  # this will be executed to each group
  each_func <- function(df, ...) {
    rate <- df[[rate_col]]
    m <- mean(rate, na.rm = TRUE)
    v <- var(rate, na.rm = TRUE)
    s <- sd(rate, na.rm = TRUE)

    # https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
    alpha <- ((1 - m) / v - 1 / m) * m ^ 2
    beta <- alpha * (1 / m - 1)
    data.frame(
      alpha = alpha,
      beta = beta,
      average = m,
      variance = v,
      sd = s
      )
  }

  do_on_each_group(df, each_func, params = substitute(list(...)))
}

#' S3 tidy method for bayesTest object
#' @param percentLift Lift threshold to calculate the probability of success
#' @param credInt Ratio for credible interval
#' @param type Type of output
#' This can be "summary", "prior", "posteriors" and "improvement"
#' @export
tidy.bayesTest <- function(x, percentLift = 0, credInt = 0.9, type = "summary", pretty.name = FALSE, ...) {
  if (type == "summary"){
    each_len <- c(length(x$inputs$A_data), length(x$inputs$B_data))
    each_success <- c(sum(x$inputs$A_data), sum(x$inputs$B_data))
    each_mean <- each_success / each_len
    # estimation of (A - B) / B
    lift <- (x$posteriors$Probability$A_probs -x$posteriors$Probability$B_probs)/ x$posteriors$Probability$B_probs
    # get density chart of lift
    d <- density(lift)
    # get the peak of density chart
    expected_lift <- d$x[which.max(d$y)]
    s <- summary(
      x,
      percentLift = rep(percentLift, length(x$posteriors)),
      credInt = rep(credInt, length(x$posteriors))
    )

    ab_identifier <- if(!is.null(x$ab_identifier) && length(x$ab_identifier) == 2){
      x$ab_identifier
    } else {
      c(TRUE, FALSE)
    }

    ret <- data.frame(
      group = c("A", "B"),
      ab_identifier = ab_identifier,
      total_population = each_len,
      converted = each_success,
      conversion_rate = each_mean,
      chance_of_being_better = c(s$probability[[1]], 1-s$probability[[1]]) ,
      expected_improvement_rate = c(expected_lift, NA_real_),
      credible_interval_low = c(s$interval$Probability[[1]], NA_real_),
      credible_interval_high = c(s$interval$Probability[[2]], NA_real_),
      expected_loss_rate = c(s$posteriorExpectedLoss$Probability, NA_real_),
      stringsAsFactors = FALSE
    )

    if (pretty.name) {
      map <- c(
        group = "Group",
        ab_identifier = "AB Identifier",
        total_population = "Total Population",
        converted = "Converted",
        conversion_rate = "Conversion Rate",
        chance_of_being_better = "Chance of Being Better",
        expected_improvement_rate = "Expected Improvement Rate",
        credible_interval_low = "Credible Interval Low",
        credible_interval_high = "Credible Interval High",
        expected_loss_rate = "Expected Loss Rate"
      )
      colnames(ret) <- map[colnames(ret)]
    }

    ret

  } else if (type == "posteriors") {
    s <- summary(x)
    level <- if(s$probability[[1]] > 0.5) {
      c("A", "B")
    } else {
      c("B", "A")
    }
    probability_a = x$posteriors$Probability$A_probs
    probability_b = x$posteriors$Probability$B_probs

    beta_a <- density(probability_a, n = 2048)
    # rate must be in 0 ~ 1,
    # so the data outside will be removed
    indice_a <- beta_a$x > 0 & beta_a$x < 1

    beta_b <- density(probability_b, n = 2048)
    indice_b <- beta_b$x > 0 & beta_b$x < 1

    ab_identifier <- if(!is.null(x$ab_identifier) && length(x$ab_identifier) == 2){
      x$ab_identifier
    } else {
      c("A", "B")
    }

    a_data <- data.frame(
      ab_identifier = rep("A", length(indice_a)),
      conversion_rate_pct = beta_a$x[indice_a] * 100,
      probability_density = beta_a$y[indice_a],
      stringsAsFactors = FALSE
    )

    b_data <- data.frame(
      ab_identifier = rep("B", length(indice_a)),
      conversion_rate_pct = beta_b$x[indice_b] * 100,
      probability_density = beta_b$y[indice_b],
      stringsAsFactors = FALSE
    )
    dplyr::bind_rows(a_data, b_data) %>%
      mutate(ab_identifier = factor(ab_identifier, levels = level))
  } else if (type == "improvement") {
    # estimation of (A - B) / B
    lift <- (x$posteriors$Probability$A_probs -x$posteriors$Probability$B_probs)/ x$posteriors$Probability$B_probs

    lift_hist <- hist(lift, breaks = 50, plot = FALSE)

    x$inputs$n_samples

    # get ratio chart of lift
    data.frame(
      improvement_rate_pct = lift_hist$mids * 100,
      probability_pct = lift_hist$counts/x$inputs$n_samples * 100, # devide by x$inputs$n_samples to make total of bars 1
      chance_of_being_better = factor(ifelse(lift_hist$mids >= 0, "A is better", "A is not better"), levels = c("A is better", "A is not better")),
      stringsAsFactors = FALSE
    )
  } else if (type == "prior") {
    # get prior distribution
    alpha <- x$inputs$priors[["alpha"]]
    beta <- x$inputs$priors[["beta"]]
    data.frame(
      conversion_rate_pct = seq(0, 1, 0.001) * 100,
      probability_density = dbeta(seq(0, 1, 0.001), shape1 = alpha, shape2 = beta)
    )
  } else {
    stop(paste0(type, " is not defined as type"))
  }
}

#' S3 glance method for bayesTest object
#' @param percentLift Lift threshold to calculate the probability of success
#' @param credInt Ratio for credible interval
#' @export
glance.bayesTest <- function(x, percentLift = 0, credInt = 0.9, ...) {
  # Get the peak of estimated density chart of (A - B) / B
  lift <- (x$posteriors$Probability$A_probs - x$posteriors$Probability$B_probs) / x$posteriors$Probability$B_probs
  d <- density(lift)
  expected_lift <- d$x[which.max(d$y)]

  s <- summary(x,
               percentLift = rep(percentLift, length(x$posteriors)),
               credInt = rep(credInt, length(x$posteriors))
  )

  data.frame(
    chance_of_a_is_better_than_b = s$probability[[1]] ,
    expected_lift = expected_lift,
    credible_interval_low = s$interval$Probability[[1]],
    credible_interval_high = s$interval$Probability[[2]],
    expected_loss = s$posteriorExpectedLoss$Probability
  )
}
