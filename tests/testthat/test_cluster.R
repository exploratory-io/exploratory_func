context("test cluster")

test_that("test cluster", {
  set.seed(1)
  test_data <- data.frame(
    runif(12) + rep(seq(3), 4),
    runif(12) + rep(seq(3), 4) + 10,
    runif(12) + rep(seq(3), 4) - 10
  )
  colnames(test_data) <- c("num 1", "num-2", "num_3")

  test_data[["num 1"]][[4]] <- NA

  test_data[["num_3"]][[10]] <- NA

  ret <- test_data %>%
    dplyr::mutate(cluster = cluster(`num 1`, `num-2`, num_3, n_cluster = 3))
  expect_true(is.factor(ret$cluster))
  expect_equal(length(levels(ret$cluster)), 3)
  expect_true(is.na(ret$cluster[[4]]))
  expect_true(is.na(ret$cluster[[10]]))
})

test_that("test cluster with one column", {
  set.seed(1)
  test_data <- data.frame(
    runif(12) + rep(seq(3), 4)
  )
  colnames(test_data) <- c("num 1")

  test_data[["num 1"]][[4]] <- NA

  ret <- test_data %>%
    dplyr::mutate(cluster = cluster(`num 1`, n_cluster = 3))
  expect_true(is.factor(ret$cluster))
  expect_equal(length(levels(ret$cluster)), 3)
  expect_true(is.na(ret$cluster[[4]]))
})

test_that("test cluster with more n_cluster than data", {
  set.seed(1)
  test_data <- data.frame(
    runif(12) + rep(seq(3), 4)
  )
  colnames(test_data) <- c("num 1")

  test_data[["num 1"]][[4]] <- NA

  expect_error(
    {
      ret <- test_data %>%
        dplyr::mutate(cluster = cluster(`num 1`, n_cluster = 100))
    }, "n_cluster must be smaller than the number of valid data"
  )
})

test_that("test cluster with one row", {
  set.seed(1)
  test_data <- data.frame(
    runif(1),
    runif(1),
    runif(1)
  )
  colnames(test_data) <- c("num 1", "num 2", "num 3")

  expect_error(
    {
      ret <- test_data %>%
        dplyr::mutate(cluster = cluster(`num 1`, `num 2`, `num 3`, n_cluster = 1))
    }, "n_cluster must be smaller than the number of valid data"
  )
})
