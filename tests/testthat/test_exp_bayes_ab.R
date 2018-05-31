context("test exp_bayes_ab")

test_that("test exp_bayes_ab test with summary output", {
  set.seed(0)
  data_a <- data.frame(
    access_count = round(runif(50) * 100 + 100)
  ) %>%
    dplyr::mutate(click = 0.2 + 0.8 * runif(nrow(.)), group = "a")
  data_b <- data.frame(
    access_count = round(runif(20) * 20 + 20)
  ) %>%
    dplyr::mutate(click = 0.3 + 0.7 * runif(nrow(.)), group = "b")

  full_data <- dplyr::bind_rows(data_a, data_b) %>%
    mutate(grouping = rbinom(n(), 1, 0.3)) %>%
    group_by(grouping)
  full_data <- full_data %>%
    dplyr::mutate(converted_true=as.integer(access_count*click), converted_false=as.integer(access_count*(1-click))) %>%
    tidyr::gather(key="converted", value="count", converted_true, converted_false) %>%
    dplyr::mutate(converted = converted=="converted_true")

  #full_data <- full_data %>% rename(`access count`=access_count, `cli ck`=click)
  ret <- exp_bayes_ab(full_data, group, converted, count, prior_mean=0.1, prior_sd=0.01, type = "summary")
  expect_equal(nrow(ret), 4)
})

#TODO: do the same set of tests as test_do_bayes_ab.R

