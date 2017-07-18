context("test do_bayes_ab")

test_that("test do_bayes_ab test with count data", {
  set.seed(0)
  data_a <- data.frame(
    access_count = round(runif(50) * 100 + 100)
  ) %>%
    dplyr::mutate(click = round(0.2 + 0.8 * runif(nrow(.)) * access_count), data_b = TRUE)
  data_b <- data.frame(
    access_count = round(runif(20) * 20 + 20)
  ) %>%
    dplyr::mutate(click = round(0.3 + 0.7 * runif(nrow(.)) * access_count), data_b = FALSE)

  full_data <- dplyr::bind_rows(data_a, data_b) %>%
    mutate(group = rbinom(n(), 1, 0.3)) %>%
    group_by(group)
  ret <- do_bayes_ab(full_data, data_b, access_count, click, type = "summary")
  expect_equal(nrow(ret), 4)
})

test_that("test do_bayes_ab test with count data", {
  set.seed(0)
  data_a <- data.frame(
    access_count = round(runif(50) * 100 + 100)
  ) %>%
    dplyr::mutate(click = round(0.2 + 0.8 * runif(nrow(.)) * access_count), data_b = TRUE)
  data_b <- data.frame(
    access_count = round(runif(20) * 20 + 20)
  ) %>%
    dplyr::mutate(click = round(0.3 + 0.7 * runif(nrow(.)) * access_count), data_b = FALSE)

  full_data <- dplyr::bind_rows(data_a, data_b) %>%
    mutate(group = rbinom(n(), 1, 0.3)) %>%
    group_by(group)
  ret <- do_bayes_ab(full_data, data_b, access_count, click, type = "prior")
  expect_equal(nrow(ret), 4)
})


test_that("test do_bayes_ab test with count data", {
  set.seed(0)
  data_a <- data.frame(
    access_count = round(runif(50) * 100 + 100)
  ) %>%
    dplyr::mutate(click = round(0.2 + 0.8 * runif(nrow(.)) * access_count), data_b = TRUE)
  data_b <- data.frame(
    access_count = round(runif(20) * 20 + 20)
  ) %>%
    dplyr::mutate(click = round(0.3 + 0.7 * runif(nrow(.)) * access_count), data_b = FALSE)

  full_data <- dplyr::bind_rows(data_a, data_b) %>%
    mutate(group = rbinom(n(), 1, 0.3)) %>%
    group_by(group)
  ret <- do_bayes_ab(full_data, data_b, access_count, click)
  expect_true(is.list(ret$model))
})

test_that("test do_bayes_ab test with count data", {
  set.seed(0)
  data_a <- data.frame(
    access_count = round(runif(50) * 100 + 100)
  ) %>%
    dplyr::mutate(click = round(0.2 + 0.8 * runif(nrow(.)) * access_count), data_b = TRUE)
  data_b <- data.frame(
    access_count = round(runif(20) * 20 + 20)
  ) %>%
    dplyr::mutate(click = round(0.3 + 0.7 * runif(nrow(.)) * access_count), data_b = FALSE)

  full_data <- dplyr::bind_rows(data_a, data_b) %>%
    dplyr::mutate(click_rate = click / access_count, group = rbinom(n(), 1, 0.3)) %>%
    group_by(group)
  ret <- calc_beta_prior(full_data, click_rate, param1, "param2")
  expect_equal(nrow(ret), 2)
})
