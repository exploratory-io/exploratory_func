context("test do_svd")

test_that("test do_svd skv with NA", {
  test_df <- data.frame(
    row = rep(paste("row", 1:4), 3),
    col = rep(paste("col", 1:3), each = 4),
    value = seq(12)
  )
  test_df <- test_df %>% rename(`ro w`=row, `co l`=col, `val ue`=value)

  test_df$value[[3]] <- NA_real_

  ret <- do_svd(test_df, skv = c("ro w", "co l", "val ue"))

  expect_equal(colnames(ret), c("ro w", "new.dimension", "value.new"))
})


test_that("test do_svd cols with NA long", {
  test_df <- data.frame(
    axis1 = rep(paste("row", 1:4), 3),
    col = rep(paste("col", 1:3), each = 4),
    value = seq(12)
  )

  test_df$value[[3]] <- NA_real_

  test_df <- pivot(test_df, row_cols=c("axis1"), col_cols=c("col"), value = value)

  ret <- do_svd(test_df, dplyr::starts_with("col"), output = "long")

  expect_equal(nrow(ret), 6)
  expect_true(all(ret$row %in% seq(3)))
})

test_that("test do_svd cols with NA", {
  test_df <- data.frame(
    axis1 = rep(paste("row", 1:4), 3),
    col = rep(paste("col", 1:3), each = 4),
    value = seq(12)
  )

  test_df$value[[3]] <- NA_real_

  test_df <- pivot(test_df, row_cols=c("axis1"), col_cols=c("col"), value = value)

  ret <- do_svd(test_df, dplyr::starts_with("col"), output = "wide")

  expect_equal(nrow(ret), 4)
  expect_equal(colnames(ret), c("axis1", "axis1.new", "axis2"))
})

test_that("test do_svd cols dimension with NA long", {
  test_df <- data.frame(
    axis1 = rep(paste("row", 1:4), 3),
    col = rep(paste("col", 1:3), each = 4),
    value = seq(12)
  )

  test_df$value[[3]] <- NA_real_

  test_df <- pivot(test_df, row_cols=c("axis1"), col_cols=c("col"), value = value)

  ret <- do_svd(test_df, dplyr::starts_with("col"), output = "long", type = "dimension")

  expect_equal(nrow(ret), 6)
  expect_true(all(ret$colname %in% colnames(test_df)))
})

test_that("test do_svd cols dimension with NA wide", {
  test_df <- data.frame(
    axis1 = rep(paste("row", 1:4), 3),
    col = rep(paste("col", 1:3), each = 4),
    value = seq(12)
  )

  test_df$value[[3]] <- NA_real_

  test_df <- pivot(test_df, row_cols=c("axis1"), col_cols=c("col"), value = value)

  ret <- do_svd(test_df, dplyr::starts_with("col"), output = "wide", type = "dimension")

  expect_equal(nrow(ret), 3)
  expect_true(all(ret$colname %in% colnames(test_df)))
})

test_that("test do_svd cols variance with NA long", {
  test_df <- data.frame(
    axis1 = rep(paste("row", 1:4), 3),
    col = rep(paste("col", 1:3), each = 4),
    value = seq(12)
  )

  test_df$value[[3]] <- NA_real_

  test_df <- pivot(test_df, row_cols=c("axis1"), col_cols=c("col"), value = value)

  ret <- do_svd(test_df, dplyr::starts_with("col"), output = "long", type = "variance")

  expect_equal(nrow(ret), 2)
  expect_equal(colnames(ret), c("new.dimension", "value"))
})

test_that("test do_svd cols variance with NA wide", {
  test_df <- data.frame(
    axis1 = rep(paste("row", 1:4), 3),
    col = rep(paste("col", 1:3), each = 4),
    value = seq(12)
  )

  test_df$value[[3]] <- NA_real_

  test_df <- pivot(test_df, row_cols=c("axis1"), col_cols=c("col"), value = value)

  ret <- do_svd(test_df, dplyr::starts_with("col"), output = "wide", type = "variance")

  expect_equal(nrow(ret), 1)
  expect_equal(colnames(ret), c("axis1", "axis2"))
})
