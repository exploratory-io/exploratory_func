#' Run bayesTest from bayesAB package
#' @param df Data frame to run bayes ab test
#' @param group A logical column to distinguish groups.
#' TRUE is for the A group which is the target to be tested.
#' @param total_count Column of the total count
#' @param success_count Column of the count of success
#' @param alpha Alpha parameter for prior beta distribution
#' @param beta Beta parameter for prior beta distribution
#' @param type Type of output
#' * model - Returns a data frame with bayesTest model.
#' * summary - Output summary of the result of the test.
#' * prior - Output coordinates of prior density chart.
#' * posteriors - Output coordinates of posterior density chart of the success rate.
#' * lift - Output probability density chart of lift, which is the ratio of performance improvement of A over B. The formula is (A - B) / B.
#' @param seed Random seed for bayes test to estimate probability density.
#' @export
do_bayes_ab <- function(df, group, total_count, success_count, alpha = 1, beta = 1, type = "model", seed = 0, ...){
  set.seed(seed)

  if (type == "prior") {
    # this returns coordinates of density chart of prior
    return(data.frame(
      estimate = seq(0, 1, 0.01),
      density = dbeta(seq(0, 1, 0.01), shape1 = alpha, shape2 = beta)
    ))
  }

  group_col <- col_name(substitute(group))
  total_count_col <- col_name(substitute(total_count))
  success_count_col <- col_name(substitute(success_count))

  grouped_col <- grouped_by(df)

  # this will be executed to each group
  each_func <- function(df, ...){
    data_a <- df[df[[group_col]], ]
    data_b <- df[!df[[group_col]], ]

    data_a_total <- sum(data_a[[total_count_col]], na.rm = TRUE)
    data_b_total <- sum(data_b[[total_count_col]], na.rm = TRUE)

    data_a_conv_total <- sum(data_a[[success_count_col]], na.rm = TRUE)
    data_b_conv_total <- sum(data_b[[success_count_col]], na.rm = TRUE)

    bin_a <- c(rep(TRUE, data_a_conv_total), rep(FALSE, data_a_total - data_a_conv_total))
    bin_b <- c(rep(TRUE, data_b_conv_total), rep(FALSE, data_b_total - data_b_conv_total))

    bayes_model <- bayesAB::bayesTest(
      bin_a,
      bin_b,
      priors = c(alpha = alpha, beta = beta),
      n_samples = 1e5,
      distribution = 'bernoulli'
    )
    bayes_model
  }

  ret <- do_on_each_group(df, each_func, name = "model", with_unnest = FALSE, params = substitute(list(...)))

  if(type == "model"){
    ret
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
#' This can be "summary", "prior", "posteriors" and "lift"
#' @export
tidy.bayesTest <- function(x, percentLift = 0, credInt = 0.9, type = "lift", ...) {
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
    s <- summary(x,
                 percentLift = rep(percentLift, length(x$posteriors)),
                 credInt = rep(credInt, length(x$posteriors))
    )
    data.frame(
      variation = c("A variation", "B default"),
      size = each_len,
      success = each_success,
      rate = each_mean,
      chance_of_being_better = c(s$probability[[1]], 1-s$probability[[1]]) ,
      expected_lift = c(expected_lift, NA_real_),
      credible_interval_low = c(s$interval$Probability[[1]], NA_real_),
      credible_interval_high = c(s$interval$Probability[[2]], NA_real_),
      expected_loss = c(s$posteriorExpectedLoss$Probability, NA_real_),
      stringsAsFactors = FALSE
    )
  } else if (type == "posteriors") {
    probability_a = x$posteriors$Probability$A_probs
    probability_b = x$posteriors$Probability$B_probs

    beta_a <- density(probability_a)
    # rate must be in 0 ~ 1,
    # so the data outside will be removed
    indice_a <- beta_a$x > 0 & beta_a$x < 1

    beta_b <- density(probability_b)
    indice_b <- beta_b$x > 0 & beta_b$x < 1

    a_data <- data.frame(
      a_or_b = rep("A", sum(indice_a)),
      estimate = beta_a$x[indice_a],
      density = beta_a$y[indice_a],
      stringsAsFactors = FALSE
    )

    b_data <- data.frame(
      a_or_b = rep("B",sum(indice_b)),
      estimate = beta_b$x[indice_b],
      density = beta_b$y[indice_b],
      stringsAsFactors = FALSE
    )
    dplyr::bind_rows(a_data, b_data)
  } else if (type == "lift") {
    # estimation of (A - B) / B
    lift <- (x$posteriors$Probability$A_probs -x$posteriors$Probability$B_probs)/ x$posteriors$Probability$B_probs
    # get density chart of lift
    d <- density(lift)
    data.frame(
      estimate = d$x,
      density = d$y,
      is_positive = d$x > 0,
      stringsAsFactors = FALSE
    )
  } else if (type == "prior") {
    # get prior distribution
    alpha <- x$inputs$priors[["alpha"]]
    beta <- x$inputs$priors[["beta"]]
    data.frame(
      estimate = seq(0, 1, 0.01),
      density = dbeta(seq(0, 1, 0.01), shape1 = alpha, shape2 = beta)
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
