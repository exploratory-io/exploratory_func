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

  full_data <- dplyr::bind_rows(data_a, data_b)
  ret <- do_bayes_ab(full_data, data_b, access_count, click)
  browser()
})
