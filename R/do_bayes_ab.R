#' @export
do_bayes_ab <- function(df, group, count, conversion, alpha = 1, beta = 1, type = "summary", ...){

  if (type == "prior") {
    return(data.frame(
      x = seq(0, 1, 0.01),
      density = dbeta(seq(0, 1, 0.01), shape1 = 10, shape2 = 30)
    )
    )
  }

  group_col <- col_name(substitute(group))
  count_col <- col_name(substitute(count))
  conversion_col <- col_name(substitute(conversion))

  data_a <- df[df[[group_col]], ]
  data_b <- df[!df[[group_col]], ]

  data_a_total <- sum(data_a[[count_col]], na.rm = TRUE)
  data_b_total <- sum(data_b[[count_col]], na.rm = TRUE)

  data_a_conv_total <- sum(data_a[[conversion_col]], na.rm = TRUE)
  data_b_conv_total <- sum(data_b[[conversion_col]], na.rm = TRUE)

  bin_a <- c(rep(TRUE, data_a_conv_total), rep(FALSE, data_a_total - data_a_conv_total))
  bin_b <- c(rep(TRUE, data_b_conv_total), rep(FALSE, data_b_total - data_b_conv_total))

  # https://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
  # alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  # beta <- alpha * (1 / mu - 1)

  bayes_model <- bayesAB::bayesTest(
    bin_a,
    bin_b,
    priors = c(alpha = alpha, beta = beta),
    n_samples = 1e5,
    distribution = 'bernoulli'
  )
  if (type == "summary") {
    broom::glance(bayes_model, ...)
  } else if (type == "lift") {
    lift <- (bayes_model$posteriors$Probability$A_probs -bayes_model$posteriors$Probability$B_probs)/ bayes_model$posteriors$Probability$B_probs
    d <- density(lift)
    data.frame(
      x = d$x,
      density = d$y,
      better_group = ifelse(d$x > 0, "a", "b"),
      stringsAsFactors = FALSE
    )
  }  else if (type == "posteriors") {
    probability_a = bayes_model$posteriors$Probability$A_probs
    probability_b = bayes_model$posteriors$Probability$B_probs

    beta_a <- density(probability_a)
    indice_a <- beta_a$x > 0 & beta_a$x < 1

    beta_b <- density(probability_b)
    indice_b <- beta_b$x > 0 & beta_b$x < 1

    a_data <- data.frame(
      data = rep("A", sum(indice_a)),
      x = beta_a$x[indice_a],
      density = beta_a$y[indice_a],
      stringsAsFactors = FALSE
    )

    b_data <- data.frame(
      data = rep("B",sum(indice_b)),
      x = beta_b$x[indice_b],
      density = beta_b$y[indice_b],
      stringsAsFactors = FALSE
    )
    dplyr::bind_rows(a_data, b_data)
  }
}

#' @export
calc_beta_prior <- function(df, rate, ...){
  rate_col <- col_name(substitute(rate))

  rate <- df[[rate_col]]
  mu <- mean(rate, na.rm = TRUE)
  mvar <- var(rate, na.rm = TRUE)
  alpha <- ((1 - mu) / mvar - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  data.frame(alpha = alpha, beta = beta)
}

tidy.bayesTest <- function(x, ...) {
  each_len <- c(length(x$inputs$A_data), length(x$inputs$B_data))
  each_mean <- c(mean(x$inputs$A_data), mean(x$inputs$B_data))
}

glance.bayesTest <- function(x, percentLift = 0, credInt = 0.9, ...) {
  lift <- (x$posteriors$Probability$A_probs -x$posteriors$Probability$B_probs)/ x$posteriors$Probability$B_probs
  d <- density(lift)
  expected_lift <- d$x[which.max(d$y)]

  s <- summary(x,
               percentLift = rep(percentLift, length(x$posteriors)),
               credInt = rep(credInt, length(x$posteriors))
  )

  #pos_percent <- sum(
  #  x$posteriors$Probability$A_probs > x$posteriors$Probability$B_probs
  #  ) / length(x$posteriors$Probability$A_probs)
  data.frame(
    chance_of_a_is_better_than_b = s$probability[[1]] ,
    expected_lift = expected_lift,
    credible_interval_low = s$interval$Probability[[1]],
    credible_interval_high = s$interval$Probability[[2]],
    expected_loss = s$posteriorExpectedLoss$Probability
  )
}
