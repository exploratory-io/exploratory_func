context("test do_bayes_ab")

test_that("test do_bayes_ab test with summary output", {
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
  ret <- do_bayes_ab(full_data, group, access_count, click, type = "summary")
  expect_equal(nrow(ret), 4)
})

test_that("test do_bayes_ab test with lift output", {
  set.seed(0)
  data_a <- data.frame(
    access_count = round(runif(50) * 100 + 100)
  ) %>%
    dplyr::mutate(click = 0.2 + 0.8 * runif(nrow(.)), data_b = 100)
  data_b <- data.frame(
    access_count = round(runif(20) * 20 + 20)
  ) %>%
    dplyr::mutate(click = 0.3 + 0.7 * runif(nrow(.)), data_b = 1)

  full_data <- dplyr::bind_rows(data_a, data_b) %>%
    mutate(group = rbinom(n(), 1, 0.3)) %>%
    group_by(group)
  ret <- do_bayes_ab(full_data, data_b, access_count, click, type = "improvement")
  # this is 2 groups, so this should be 1 * 2
  expect_equal(sum(ret$probability_pct), 200)
})

test_that("test do_bayes_ab test with ", {
  set.seed(0)
  data_a <- data.frame(
    access_count = round(runif(50) * 100 + 100)
  ) %>%
    dplyr::mutate(click = 0.2 + 0.8 * runif(nrow(.)), data_b = "a")
  data_b <- data.frame(
    access_count = round(runif(20) * 20 + 20)
  ) %>%
    dplyr::mutate(click = 0.3 + 0.7 * runif(nrow(.)), data_b = "b")

  full_data <- dplyr::bind_rows(data_a, data_b) %>%
    mutate(data_b = as.factor(data_b)) %>%
    mutate(group = rbinom(n(), 1, 0.3)) %>%
    group_by(group)
  ret <- do_bayes_ab(full_data, data_b, access_count, click)
  expect_true(is.list(ret$model))
})

test_that("test do_bayes_ab test with ", {
  set.seed(0)
  data_a <- data.frame(
    access_count = round(runif(50) * 100 + 100)
  ) %>%
    dplyr::mutate(click_rate = 0.2 + 0.8 * runif(nrow(.)), data_b = 1)
  data_b <- data.frame(
    access_count = round(runif(20) * 20 + 20)
  ) %>%
    dplyr::mutate(click_rate = 0.3 + 0.7 * runif(nrow(.)), data_b = 2)

  full_data <- dplyr::bind_rows(data_a, data_b) %>%
    mutate(group = rbinom(n(), 1, 0.3)) %>%
    group_by(group)
  ret <- do_bayes_ab(full_data, data_b, access_count, click, prior_mean = 0.5, prior_sd = 0.2, type = "prior")
  expect_true(!all(ret$probability_density == 1))
})

test_that("test calc_beta_prior", {
  set.seed(0)
  data_a <- data.frame(
    access_count = round(runif(50) * 100 + 100)
  ) %>%
    dplyr::mutate(click = 0.2 + 0.8 * runif(nrow(.)), data_b = TRUE)
  data_b <- data.frame(
    access_count = round(runif(20) * 20 + 20)
  ) %>%
    dplyr::mutate(click = 0.3 + 0.7 * runif(nrow(.)), data_b = FALSE)

  full_data <- dplyr::bind_rows(data_a, data_b) %>%
    dplyr::mutate(click_rate = click / access_count, group = rbinom(n(), 1, 0.3)) %>%
    group_by(group)
  ret <- calc_beta_prior(full_data, click_rate)
  expect_equal(nrow(ret), 2)
})
