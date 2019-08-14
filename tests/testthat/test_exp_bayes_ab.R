context("test exp_bayes_ab")

test_that("test exp_bayes_ab test with summary output", {
  set.seed(0)
  data_a <- data.frame(
    access_count = round(runif(50) * 100 + 100)
  ) %>%
    dplyr::mutate(click = 0.2 + 0.8 * runif(nrow(.)), `gro up` = "a")

  data_a$click[[5]] <- NA # inject NA for test

  data_b <- data.frame(
    access_count = round(runif(20) * 20 + 20)
  ) %>%
    dplyr::mutate(click = 0.3 + 0.7 * runif(nrow(.)), `gro up` = "b")

  full_data <- dplyr::bind_rows(data_a, data_b) %>%
    mutate(grouping = rbinom(n(), 1, 0.3)) %>%
    group_by(grouping)
  full_data <- full_data %>%
    dplyr::mutate(converted_true=as.integer(access_count*click), converted_false=as.integer(access_count*(1-click))) %>%
    tidyr::gather(key="conver ted", value="count col", converted_true, converted_false) %>%
    dplyr::mutate(`conver ted` = `conver ted`=="converted_true")
  full_data_logical_ab <- full_data %>% dplyr::mutate(`gro up` = `gro up` == "a")

  # revert A/B - character case
  ret1 <- exp_bayes_ab(full_data, `conver ted`, `gro up`, `count col`, prior_mean=0.1, prior_sd=0.01, type = "summary")
  ret2 <- exp_bayes_ab(full_data, `conver ted`, `gro up`, `count col`, prior_mean=0.1, prior_sd=0.01, revert_ab = T, type = "summary")
  expect_equal(ret1$ab_identifier, c("a", "b", "a", "b"))
  expect_equal(ret2$ab_identifier, c("b", "a", "b", "a"))

  # revert A/B - logical case
  ret1 <- exp_bayes_ab(full_data_logical_ab, `conver ted`, `gro up`, `count col`, prior_mean=0.1, prior_sd=0.01, type = "summary")
  ret2 <- exp_bayes_ab(full_data_logical_ab, `conver ted`, `gro up`, `count col`, prior_mean=0.1, prior_sd=0.01, revert_ab = T, type = "summary")
  expect_equal(ret1$ab_identifier, c(TRUE,FALSE,TRUE,FALSE))
  expect_equal(ret2$ab_identifier, c("FALSE","TRUE","FALSE","TRUE"))

  #full_data <- full_data %>% rename(`access count`=access_count, `cli ck`=click)
  ret <- exp_bayes_ab(full_data, `conver ted`, `gro up`, `count col`, prior_mean=0.1, prior_sd=0.01, type = "summary")
  expect_equal(nrow(ret), 4)

  # without prior
  ret <- exp_bayes_ab(full_data, `conver ted`, `gro up`, `count col`, type = "summary")
  expect_equal(nrow(ret), 4)

  # without count with NULL
  ret <- exp_bayes_ab(full_data, `conver ted`, `gro up`, NULL, type = "summary")
  expect_equal(nrow(ret), 4)

  # without count
  ret <- exp_bayes_ab(full_data, `conver ted`, `gro up`, , type = "summary")
  expect_equal(nrow(ret), 4)
})

#TODO: do the same set of tests as test_do_bayes_ab.R

