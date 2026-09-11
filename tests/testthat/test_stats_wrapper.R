context("tests for wrappers of stats package")

spread_test_df <- data.frame(var1 = c(1, 3, 2, NA), var2 = c(1, 3, 2, 10))
test_that("do_dist with NA results", {
  data <- data.frame(
    x = rep(letters[1:3], 3),
    y = rep(letters[1:3], each=3)
  )
  ret <- do_dist(data, skv = c("x", "y"), distinct = TRUE)
  expect_true(nrow(ret) > 0)
})

test_that("do_cor with NA results", {
  data <- data.frame(
    x = rep(letters[1:3], 3),
    y = rep(letters[1:3], each=3)
  )
  ret <- do_cor(data, skv = c("x", "y"))
  expect_true(nrow(ret) > 0)
})

test_that("do_cor with NA results", {
  data <- data.frame(
    x = rep(letters[1:3], 3),
    y = rep(letters[1:3], each=3)
  )
  ret <- do_cor(data, skv = c("x", "y"), distinct = TRUE)
  expect_true(nrow(ret) > 0)
})

test_that("do_cor with date aggregation", {
  set.seed(0)
  rownum <- 60
  test_df <- data.frame(
    rows = rep(c("a", "b", "c"), each = rownum / 3),
    dt = rep(as.Date("2014-01-01"), rownum) + lubridate::days(rep(seq(rownum / 3), 3)),
    val = runif(rownum)
  )

  mat <- test_df %>%
    dplyr::mutate(week_round = lubridate::floor_date(dt, unit = "weeks")) %>%
    dplyr::select(-dt) %>%
    dplyr::group_by(rows, week_round) %>%
    dplyr::summarize(mean_val = mean(val)) %>%
    tidyr::spread(rows, mean_val) %>%
    dplyr::select(-week_round) %>%
    as.matrix()

  cor_ret <- cor(mat, use = "pairwise.complete.obs")

  melt_ret <- reshape2::melt(cor_ret)

  ret <- test_df %>%
    do_cor(skv = c("rows", "dt", "val"), time_unit = "weeks", diag = TRUE)

  for(i in seq(nrow(cor_ret))){
    for(j in seq(ncol(cor_ret))){
      mat_answer <- cor_ret[i, j]
      df_answer <- ret[ret[[1]] == letters[[i]] & ret[[2]] == letters[[j]], 3][[1]]
      expect_equal(mat_answer, df_answer)
    }
  }
})

test_that("do_cor with zero correlations", {
  # Steps to produce the output
  df <- data.frame(x=c(1,1,0,0),y=c(1,0,1,0),z=c(T,T,F,F))
  model_df <- df %>% do_cor(`x`, `y`, `z`, method = "pearson", distinct = FALSE, diag = TRUE, return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type='cor')
  expect_equal(nrow(res), 9) # Make sure rows for all 9 combinations are there even though some have 0 correlation values.
})

test_that("add_prediction with do_cor should throw error", {
  # Steps to produce the output
  df <- data.frame(x=c(1,1,0,0),y=c(1,0,1,0),z=c(T,T,F,F))
  model_df <- df %>% do_cor(`x`, `y`, `z`, method = "pearson", distinct = FALSE, diag = TRUE, return_type = "model")
  expect_error({ret <- df %>% add_prediction(model_df=model_df)}, "EXP\\-ANA\\-4 :: \\[\\] :: This is not a prediction model.")
})

test_that("do_cor with Spearman method", {
  # Steps to produce the output
  df <- data.frame(x=c(1,1,0,0),y=c(1,0,1,0),z=c(T,T,F,F))
  model_df <- df %>% do_cor(`x`, `y`, `z`, method = "spearman", distinct = FALSE, diag = TRUE, return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type='cor')
  expect_equal(nrow(res), 9) # Make sure rows for all 9 combinations are there even though some have 0 correlation values.
})

test_that("do_cor with variable order based on the input order", {
  # Steps to produce the output
  df <- data.frame(x=c(1,1,0,0),y=c(1,0,1,0),z=c(T,T,F,F))
  model_df <- df %>% do_cor(`x`, `y`, `z`, method = "pearson", distinct = FALSE, diag = TRUE, variable_order = "input", return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type='cor')
  # The output variable name order should be same as the input.
  expect_equal(as.character(res$pair.name.x), c(rep("x", 3), rep("y", 3), rep("z", 3)))
  expect_equal(as.character(res$pair.name.y), rep(c("x", "y", "z"), 3))
  expect_equal(nrow(res), 9) # Make sure rows for all 9 combinations are there even though some have 0 correlation values.
})

test_that("do_cor with variable order based on clustering", {
  # Two groups of two. Within a group the correlation is about 0.7; across groups it is about 0.
  # The mean-correlation order cannot see the groups, because a variable's mean says how strong its
  # bonds are and not who they are with: it produces b1, a2, a1, b2, splitting group b to opposite
  # ends of the heatmap. Clustering keeps each group contiguous.
  set.seed(1)
  n <- 200
  ga <- rnorm(n)
  gb <- rnorm(n)
  df <- data.frame(a1 = ga + rnorm(n, sd = 0.5),
                   b1 = gb + rnorm(n, sd = 0.4),
                   a2 = ga + rnorm(n, sd = 0.6),
                   b2 = gb + rnorm(n, sd = 0.8))

  model_df <- df %>% do_cor(`a1`, `b1`, `a2`, `b2`, method = "pearson", distinct = FALSE, diag = TRUE,
                            variable_order = "cluster", return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type = 'cor')
  clustered <- levels(res$pair.name.x)
  expect_equal(levels(res$pair.name.y), clustered) # Both axes carry the same order.
  # Each group occupies adjacent positions, whichever end each group lands on.
  expect_equal(abs(diff(match(c("a1", "a2"), clustered))), 1)
  expect_equal(abs(diff(match(c("b1", "b2"), clustered))), 1)
  expect_equal(nrow(res), 16)

  # The order this replaces, on the same data, interleaves the two groups.
  mean_order_df <- df %>% do_cor(`a1`, `b1`, `a2`, `b2`, method = "pearson", distinct = FALSE, diag = TRUE,
                                 variable_order = "correlation", return_type = "model")
  mean_order <- levels((mean_order_df %>% tidy_rowwise(model, type = 'cor'))$pair.name.x)
  expect_equal(mean_order, c("b1", "a2", "a1", "b2"))
})

test_that("do_cor clustering order with fewer than 3 variables", {
  # hclust needs 2 or more objects. With 2 variables there is nothing to arrange, and the analysis
  # has to come back with the full pair set rather than an error.
  df <- data.frame(x = c(1, 1, 0, 0), y = c(1, 0, 1, 0))
  model_df <- df %>% do_cor(`x`, `y`, method = "pearson", distinct = FALSE, diag = TRUE,
                            variable_order = "cluster", return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type = 'cor')
  expect_equal(sort(levels(res$pair.name.x)), c("x", "y"))
  expect_equal(nrow(res), 4)
})

test_that("do_cor clustering order with a constant column", {
  # A column that never varies correlates with nothing, so its distances are all NA. It must not
  # take the clustering -- and with it the whole analysis -- down with it.
  df <- data.frame(x = c(1, 2, 3, 4), y = c(1, 2, 3, 5), z = c(4, 3, 2, 1), const = c(1, 1, 1, 1))
  model_df <- suppressWarnings(
    df %>% do_cor(`x`, `y`, `z`, `const`, method = "pearson", distinct = FALSE, diag = TRUE,
                  variable_order = "cluster", return_type = "model"))
  res <- suppressWarnings(model_df %>% tidy_rowwise(model, type = 'cor'))
  # x and y move together and z moves against them, so the two blocks stay apart.
  clustered <- levels(res$pair.name.x)
  expect_equal(abs(diff(match(c("x", "y"), clustered))), 1)
  expect_true(all(c("x", "y", "z") %in% clustered))
})

test_that("do_cor with only lower triangle", {
  # Steps to produce the output
  df <- data.frame(x=c(1,1,0,0),y=c(1,0,1,0),z=c(T,T,F,F))
  model_df <- df %>% do_cor(`x`, `y`, `z`, method = "pearson", distinct = TRUE, diag = TRUE, return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type='cor')
  expect_equal(nrow(res), 6) # Lower triangle elements with diagonal elements.
})

test_that("do_cor with only logical columns", {
  # Steps to produce the output
  df <- data.frame(x=c(T,T,F,F),y=c(T,F,T,F),z=c(T,T,F,F))
  model_df <- df %>% do_cor(`x`, `y`, `z`, method = "pearson", distinct = TRUE, diag = TRUE, return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type='cor')
  expect_equal(nrow(res), 6) # Lower triangle elements with diagonal elements.
})

test_that("do_cor should skip group with only one row.", {
  df <- data.frame(x=c(1,1,0,0),y=c(1,0,1,0),z=c(T,T,T,F))
  model_df <- df %>% group_by(z) %>% do_cor(`x`, `y`, method = "pearson", distinct = FALSE, diag = TRUE, return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type='cor')
  # verify that the group of z==F is skipped.
  expect_equal(nrow(res %>% filter(z==F)), 0)
})

test_that("do_cor with polychoric method", {
  skip_if_not_installed("polycor")

  # Polychoric correlation is for ordinal variables. The latent variables behind x and y
  # are correlated at 0.7, while z is independent of them.
  set.seed(123)
  n <- 200
  cut5 <- function(z) as.integer(cut(z, breaks = c(-Inf, -0.84, -0.25, 0.25, 0.84, Inf)))
  z1 <- rnorm(n); z2 <- 0.7 * z1 + sqrt(1 - 0.49) * rnorm(n); z3 <- rnorm(n)
  df <- data.frame(x = cut5(z1), y = cut5(z2), z = cut5(z3))
  model_df <- df %>% do_cor(`x`, `y`, `z`, method = "polychoric", distinct = FALSE, diag = TRUE, return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type = 'cor')
  expect_equal(nrow(res), 9) # All 9 combinations.
  xy <- res %>% filter(pair.name.x == "x", pair.name.y == "y")
  expect_true(xy$correlation > 0.5 && xy$correlation < 0.9) # Recovers the true latent 0.7, not inflated.
  expect_true(xy$p_value < 0.05) # Statistically significant.
  expect_true(is.finite(xy$statistic)) # z value is populated.
  xz <- res %>% filter(pair.name.x == "x", pair.name.y == "z")
  expect_true(abs(xz$correlation) < 0.3) # z is independent of x: near-zero polychoric correlation.
  expect_true(xz$p_value > 0.05) # The independent pair is not statistically significant.
  diag_row <- res %>% filter(pair.name.x == "x", pair.name.y == "x")
  expect_equal(diag_row$correlation, 1) # Diagonal correlation is 1.
  expect_equal(diag_row$p_value, 0) # Diagonal P value is 0.
})

test_that("do_cor automatic method selects the specified correlation family", {
  ordinal <- ordered(rep(1:5, each = 20))
  expect_identical(resolve_correlation_method(data.frame(x = 1:100, y = 101:200), "auto"), "pearson")
  expect_identical(resolve_correlation_method(data.frame(x = ordinal, y = rev(ordinal)), "auto"), "polychoric")
  expect_identical(resolve_correlation_method(data.frame(x = 1:100, y = ordinal), "auto"), "mixed")
})

test_that("do_cor supports automatic and mixed correlations with factor inputs", {
  skip_if_not_installed("polycor")

  set.seed(456)
  n <- 100
  ordinal <- ordered(cut(rnorm(n), breaks = c(-Inf, -0.84, -0.25, 0.25, 0.84, Inf)))
  df <- data.frame(continuous = rnorm(n), ordinal = ordinal)

  auto <- df %>% do_cor(continuous, ordinal, method = "auto", distinct = FALSE, diag = TRUE, return_type = "model") %>% tidy_rowwise(model, type = "cor")
  mixed <- df %>% do_cor(continuous, ordinal, method = "mixed", distinct = FALSE, diag = TRUE, return_type = "model") %>% tidy_rowwise(model, type = "cor")

  expect_equal(nrow(auto), 4)
  expect_equal(nrow(mixed), 4)
  expect_true(is.finite(auto$correlation[auto$pair.name.x != auto$pair.name.y][1]))
  expect_true(is.finite(mixed$correlation[mixed$pair.name.x != mixed$pair.name.y][1]))
})

test_that("do_cor with polychoric method handles a constant column without error", {
  skip_if_not_installed("polycor")

  set.seed(123)
  n <- 100
  cut5 <- function(z) as.integer(cut(z, breaks = c(-Inf, -0.84, -0.25, 0.25, 0.84, Inf)))
  z1 <- rnorm(n); z2 <- 0.7 * z1 + sqrt(1 - 0.49) * rnorm(n)
  df <- data.frame(x = cut5(z1), y = cut5(z2), w = rep(3L, n)) # w is constant.
  # hetcor warns for the non-estimable pairs involving the constant column; that is expected.
  model_df <- suppressWarnings(df %>% do_cor(`x`, `y`, `w`, method = "polychoric", distinct = FALSE, diag = TRUE, return_type = "model"))
  res <- model_df %>% tidy_rowwise(model, type = 'cor')
  xy <- res %>% filter(pair.name.x == "x", pair.name.y == "y")
  expect_equal(nrow(xy), 1) # The estimable pair is still computed.
  expect_true(xy$correlation > 0.5)
  # The constant-column pair has NA correlation and is dropped by na.rm in mat_to_df.
  expect_equal(nrow(res %>% filter(pair.name.x == "x", pair.name.y == "w")), 0)
})

test_that("do_cor with polychoric method handles complex column names", {
  skip_if_not_installed("polycor")

  set.seed(123)
  n <- 100
  cut5 <- function(z) as.integer(cut(z, breaks = c(-Inf, -0.84, -0.25, 0.25, 0.84, Inf)))
  z1 <- rnorm(n); z2 <- 0.7 * z1 + sqrt(1 - 0.49) * rnorm(n)
  sname <- "航空 会社 !\"#$%&'()*+, -./:;<=>?@[]^_'{|}~ 表"
  df <- data.frame(a = cut5(z1), b = cut5(z2), check.names = FALSE)
  names(df) <- c(sname, "plain")
  model_df <- df %>% do_cor(tidyselect::everything(), method = "polychoric", distinct = FALSE, diag = TRUE, return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type = 'cor')
  expect_true(sname %in% as.character(res$pair.name.x)) # Complex name survives the round trip.
  pair <- res %>% filter(as.character(pair.name.x) == sname, pair.name.y == "plain")
  expect_equal(nrow(pair), 1)
  expect_true(is.finite(pair$correlation))
})

test_that("do_cor with polychoric method accepts use values hetcor does not support", {
  skip_if_not_installed("polycor")

  # hetcor only accepts "complete.obs" and "pairwise.complete.obs", but the public
  # do_cor API (and the other methods via cor()/cor.test()) also accept "everything",
  # "all.obs", and "na.or.complete". Those must be mapped, not passed through as an error.
  set.seed(123)
  n <- 100
  cut5 <- function(z) as.integer(cut(z, breaks = c(-Inf, -0.84, -0.25, 0.25, 0.84, Inf)))
  z1 <- rnorm(n); z2 <- 0.7 * z1 + sqrt(1 - 0.49) * rnorm(n)
  df <- data.frame(x = cut5(z1), y = cut5(z2))
  for (u in c("everything", "all.obs", "na.or.complete", "complete.obs")) {
    model_df <- df %>% do_cor(`x`, `y`, method = "polychoric", use = u, distinct = FALSE, diag = TRUE, return_type = "model")
    res <- model_df %>% tidy_rowwise(model, type = 'cor')
    xy <- res %>% filter(pair.name.x == "x", pair.name.y == "y")
    expect_equal(nrow(xy), 1, info = u) # The pair is computed regardless of the use value.
    expect_true(xy$correlation > 0.5, info = u)
  }
})

test_that("do_cor with polychoric method for grouped (repeat-by) data", {
  skip_if_not_installed("polycor")

  # Repeat By on Analytics View maps to group_by(). Each group must get its own
  # polychoric correlation. Group A has a positive relationship, group B a negative one.
  set.seed(123)
  n <- 100
  cut5 <- function(z) as.integer(cut(z, breaks = c(-Inf, -0.84, -0.25, 0.25, 0.84, Inf)))
  mk <- function(rho) {
    z1 <- rnorm(n); z2 <- rho * z1 + sqrt(1 - rho^2) * rnorm(n)
    data.frame(x = cut5(z1), y = cut5(z2))
  }
  df <- dplyr::bind_rows(cbind(mk(0.7), grp = "A"), cbind(mk(-0.7), grp = "B"))
  model_df <- df %>% group_by(grp) %>% do_cor(`x`, `y`, method = "polychoric", distinct = FALSE, diag = TRUE, return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type = 'cor')
  expect_setequal(unique(as.character(res$grp)), c("A", "B")) # Both groups produced results.
  a_xy <- res %>% filter(grp == "A", pair.name.x == "x", pair.name.y == "y")
  b_xy <- res %>% filter(grp == "B", pair.name.x == "x", pair.name.y == "y")
  expect_true(a_xy$correlation > 0.4) # Positive correlation in group A.
  expect_true(b_xy$correlation < -0.4) # Negative correlation in group B, computed independently.
})

test_that("do_cor with polychoric method handles NA values via pairwise complete obs", {
  skip_if_not_installed("polycor")

  # Survey data routinely has missing responses (NA). The default use="pairwise.complete.obs"
  # must drop NAs pairwise rather than error, so every pair is still estimated.
  set.seed(123)
  n <- 200
  cut5 <- function(z) as.integer(cut(z, breaks = c(-Inf, -0.84, -0.25, 0.25, 0.84, Inf)))
  z1 <- rnorm(n); z2 <- 0.7 * z1 + sqrt(1 - 0.49) * rnorm(n); z3 <- rnorm(n)
  x <- cut5(z1); y <- cut5(z2); z <- cut5(z3)
  x[1:20] <- NA; y[15:30] <- NA; z[40:60] <- NA # Missing responses at different rows per column.
  df <- data.frame(x = x, y = y, z = z)
  model_df <- df %>% do_cor(`x`, `y`, `z`, method = "polychoric", distinct = FALSE, diag = TRUE, return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type = 'cor')
  expect_equal(nrow(res), 9) # Every pair is still estimated from the pairwise-complete rows.
  xy <- res %>% filter(pair.name.x == "x", pair.name.y == "y")
  expect_true(is.finite(xy$correlation)) # NAs did not break the estimate.
  expect_true(xy$correlation > 0.4) # The x-y relationship is still recovered despite the NAs.
})

test_that("test do_svd.kv with fill", {
  test_df <- data.frame(
    rand=runif(20, min = 0, max=10),
    axis2=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
    col=rep(seq(5),4))
  loadNamespace("dplyr")
  result <- (
    test_df  %>%
      do_svd.kv(axis2, col, n_component=3, fill = 1)
  )
  expect_equal(colnames(result), c("axis2","new.dimension", "value"))
  expect_true(any(result[[1]]=="group1"))
  expect_true(any(result[[2]]==1))

})

test_that("normalize", {
  test_vec <- c(seq(10), NA, 10 - seq(10))
  ans <- scale(test_vec) %>% as.numeric()
  ret <- normalize(ans)
  expect_equal(ans, ret)
})

test_that("normalize with constant data", {
  test_vec <- rep(0, 10) # zero constant input
  ret <- normalize(test_vec, center=TRUE, scale=TRUE)
  expect_equal(ret, rep(0, 10))
  ret <- normalize(test_vec, center=TRUE, scale=FALSE)
  expect_equal(ret, rep(0, 10))
  ret <- normalize(test_vec, center=FALSE, scale=TRUE)
  expect_equal(ret, rep(0, 10))
  ret <- normalize(test_vec, center=FALSE, scale=FALSE)
  expect_equal(ret, rep(0, 10))

  test_vec <- rep(1, 10) # non-zero constant input
  ret <- normalize(test_vec, center=TRUE, scale=TRUE)
  expect_equal(ret, rep(0, 10))
  ret <- normalize(test_vec, center=TRUE, scale=FALSE)
  expect_equal(ret, rep(0, 10))
  ret <- normalize(test_vec, center=FALSE, scale=TRUE)
  ans <- scale(test_vec, center=FALSE, scale=TRUE) %>% as.numeric()
  expect_equal(ans, ret)
  ret <- normalize(test_vec, center=FALSE, scale=FALSE)
  expect_equal(ret, rep(1, 10))
})

test_that("do_cor with NA values", {
  loadNamespace("reshape2")
  nrow <- 10
  ncol <- 20
  vec <- rnorm(nrow * ncol)
  mat <- matrix(vec, nrow = nrow)
  melt_mat <- reshape2::melt(mat)
  colnames(melt_mat)[[2]] <- "Var 2"

  ret <- do_cor(melt_mat, skv = c("Var 2", "Var1", "value"), diag = TRUE)

  cor_ret <- cor(mat, use = "pairwise.complete.obs")
  melt_ret <- reshape2::melt(cor_ret)

  for(i in seq(ncol)){
    for(j in seq(ncol)){
      mat_answer <- cor_ret[i, j]
      df_answer <- ret[ret[[1]] == i & ret[[2]] == j, 3][[1]]
      expect_equal(mat_answer, df_answer)
    }
  }
})

test_that("do_cor with NA values with model output", {
  loadNamespace("reshape2")
  nrow <- 10
  ncol <- 20
  vec <- rnorm(nrow * ncol)
  mat <- matrix(vec, nrow = nrow)
  melt_mat <- reshape2::melt(mat)
  colnames(melt_mat)[[2]] <- "Var 2"

  ret <- do_cor(melt_mat, skv = c("Var 2", "Var1", "value"), diag = TRUE, return_type = "model")
  ret <- ret %>% tidy_rowwise(model, type = "cor")

  cor_ret <- cor(mat, use = "pairwise.complete.obs")
  melt_ret <- reshape2::melt(cor_ret)

  for(i in seq(ncol)){
    for(j in seq(ncol)){
      mat_answer <- cor_ret[i, j]
      df_answer <- ret[ret[[1]] == i & ret[[2]] == j, 3][[1]]
      expect_equal(mat_answer, df_answer)
    }
  }
})

tidy_test_df <- data.frame(
  cat=rep(c("cat1", "cat2"), 20),
  dim = sort(rep(paste0("dim", seq(4)), 5)),
  val=seq(20),
  dim_na=c(paste0("dim", seq(10)), paste0("dim", seq(10)+3)))

# test data for group_by.
tidy_group_test_df <- dplyr::bind_rows(tidy_test_df, tidy_test_df) %>% dplyr::mutate(grp = c(rep("A",40), rep("B",40)))

test_that("test do_cor.cols", {
  result <- spread_test_df %>%
    do_cor.cols(dplyr::starts_with("var"))
  expect_equal(result[["correlation"]], rep(1, 2))
  expect_equal(result[["p_value"]], c(0, 0))
})

test_that("test do_cor.cols with model output", {
  result <- spread_test_df %>%
    do_cor.cols(dplyr::starts_with("var"), return_type = "model")
  expect_equal(colnames(result), "model")
  result_cor <- result %>% tidy_rowwise(model, type = "cor")
  expect_equal(result_cor[["correlation"]], rep(1, 2))
  expect_equal(result_cor[["p_value"]], rep(0, 2))
  result_data <- result %>% tidy_rowwise(model, type = "data")
  expect_equal(colnames(result_data), c("var1", "var2"))
})

test_that("test do_cor.cols for grouped df", {
  loadNamespace("dplyr")
  group1 <- cbind(spread_test_df, data.frame(group=rep("group1", 4)))
  group2 <- cbind(spread_test_df, data.frame(group=rep("group2", 4)))
  group2$var2 <- -group2$var2
  test_df <- rbind(group1, group2)
  result <- (
    test_df
    %>%  dplyr::group_by(group)
    %>%  do_cor.cols(dplyr::starts_with("var")))
  expect_equal(dim(result), c(4, 6))
})

test_that("test do_cor.cols for grouped df with model output", {
  loadNamespace("dplyr")
  group1 <- cbind(spread_test_df, data.frame(group=rep("group1", 4)))
  group2 <- cbind(spread_test_df, data.frame(group=rep("group2", 4)))
  group2$var2 <- -group2$var2
  test_df <- rbind(group1, group2)
  result <- (
    test_df
    %>%  dplyr::group_by(group)
    %>%  do_cor.cols(dplyr::starts_with("var"), return_type = "model"))

  result_cor <- result %>% tidy_rowwise(model)
  expect_equal(dim(result_cor), c(4, 6))
  result_data <- result %>% tidy_rowwise(model, type = "data")
  expect_equal(colnames(result_data), c("group", "var1", "var2"))
})

test_that("test do_cor.kv for duplicated pair", {
  result <- tidy_test_df %>%  do_cor.kv(cat, dim, val)
  expect_equal(ncol(result), 5)
  expect_equal(result[["cat.x"]], c("cat1", "cat2"))
  expect_equal(result[["cat.y"]], c("cat2", "cat1"))
  expect_equal(result[["correlation"]], replicate(2, 1))
  expect_equal(result[["p_value"]], c(0, 0))
})

test_that("test do_cor.kv with model output", {
  result <- tidy_test_df %>%  do_cor.kv(cat, dim, val, return_type = "model")
  result_cor <- result %>% tidy_rowwise(model, type = "cor")
  expect_equal(ncol(result_cor), 5)
  expect_equal(as.character(result_cor[["cat.x"]]), c("cat1", "cat2"))
  expect_equal(as.character(result_cor[["cat.y"]]), c("cat2", "cat1"))
  expect_equal(result_cor[["correlation"]], replicate(2, 1))
  expect_equal(result_cor[["p_value"]], c(0, 0))
  result_data <- result %>% tidy_rowwise(model, type = "data")
  expect_equal(colnames(result_data), c("cat", "dim", "val", "dim_na"))
})

test_that("test do_cor.kv with group_by with model output", {
  result <- tidy_group_test_df %>% group_by(grp) %>% do_cor.kv(cat, dim, val, return_type = "model")
  result_cor <- result %>% tidy_rowwise(model, type = "cor")
  expect_equal(ncol(result_cor), 6)
  expect_equal(result_cor[["grp"]], c("A", "A", "B", "B"))
  expect_equal(as.character(result_cor[["cat.x"]]), c("cat1", "cat2", "cat1", "cat2"))
  expect_equal(as.character(result_cor[["cat.y"]]), c("cat2", "cat1", "cat2", "cat1"))
  expect_equal(result_cor[["correlation"]], replicate(4, 1))
  expect_equal(result_cor[["p_value"]], c(0, 0, 0, 0))
  result_data <- result %>% tidy_rowwise(model, type = "data")
  expect_equal(colnames(result_data), c("grp", "cat", "dim", "val", "dim_na")) # TODO: group column comes as the last column, but it might be easier to understand if it comes first.
})

test_that("test do_cor.kv for grouped data frame as subject error", {
  data <- data.frame(group=rep(c(1,2,3), each=6),
                     row = rep(c(1, 1, 2, 2, 3,3), 3),
                     col = rep(c(1,2), 9),
                     val = rep(0, 18))
  expect_error({
    ret <- data %>%
      dplyr::group_by(group) %>%
      do_cor.kv(group, col, val)
  }, "group is a grouping column\\. ungroup\\(\\) may be necessary before this operation\\.")
})

test_that("test do_cor.kv for empty value", {
  expect_error({
    result <- tidy_test_df %>%  do_cor.kv(cat, dim_na, val)
  }, NA)
})

test_that("test do_cor without val", {
  loadNamespace("dplyr")

  test_df <- data.frame(
    subject = paste0("subject", rep(4-seq(3), each=3)),
    key = paste0("key", c(rep(3-seq(2), 4), 1)))

  result <- (
    test_df %>%
      do_cor( skv = c("subject", "key") )
  )

  expect_equal(result[[3]][1:2], c(1, -1))
})

test_that("do_svd.kv with NA value", {
  data <- data.frame(row = c(1, 1, 2, 2, 3,3),
                     col = rep(c(1,2), 3),
                     val = seq(6)) %>%
    dplyr::slice(-3)
  # this slice creates missing value by removing a row

  colnames(data)[1] <- "ro w"

  # expect no error
  do_svd.kv(data, `ro w`, col, val, fill=0)
  expect_error({
    do_svd.kv(data, `ro w`, col, val, fill=NA)
  }, "NA is not supported as value")
})

test_that("test do_svd.kv output wide", {
  test_df <- data.frame(
    rand=runif(20, min = 0, max=10),
    axis2=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
    col=rep(seq(5),4))
  loadNamespace("dplyr")
  result <- test_df  %>%
      do_svd.kv(axis2, col, rand, n_component=3, output="wide")
  expect_equal(colnames(result), c("axis2","axis1", "axis2.new", "axis3"))
  expect_true(any(result[,1]=="group1"))
})

test_that("test do_svd.kv", {
  if(requireNamespace("broom")){
    test_df <- data.frame(
      rand=runif(20, min = 0, max=10),
      axis2=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  do_svd.kv(axis2, col, rand, n_component=3))
    expect_equal(colnames(result), c("axis2","new.dimension", "value"))
    expect_true(any(result[[1]]=="group1"))
    expect_true(any(result[[2]]==1))
  }
})

test_that("test do_svd.kv without value", {
  if(requireNamespace("broom")){
    test_df <- data.frame(
      axis2=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    loadNamespace("dplyr")
    result <- test_df  %>%
      do_svd.kv(axis2, col, n_component=3)
    expect_equal(colnames(result), c("axis2","new.dimension", "value"))
    expect_true(any(result[[1]]=="group1"))
    expect_true(any(result[[2]]==1))
  }
})

test_that("test do_svd.kv", {
  test_df <- data.frame(
    rand=runif(20, min = 0, max=10),
    axis2=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
    col=rep(seq(5),4))
  loadNamespace("dplyr")
  result <- (
    test_df  %>%
      do_svd.kv(axis2, col, n_component=3)
    )
  expect_equal(colnames(result), c("axis2","new.dimension", "value"))
  expect_true(any(result[[1]]=="group1"))
  expect_true(any(result[[2]]==1))

})

test_that("test do_svd.kv with group_by, output=wide", {
  if(requireNamespace("broom")){
    test_df <- data.frame(
      rand=runif(20, min = 0, max=10),
      group=c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)),
      axis1=paste("group", c(rep(1,10), rep(2, 10)), sep=""),
      col=rep(seq(5),4), stringsAsFactors = FALSE)
    loadNamespace("dplyr")
    result <- test_df %>%
      dplyr::group_by(axis1) %>%
      do_svd.kv(group, col, rand, output="wide", n_component = 1)
    expect_equal(colnames(result), c("axis1","group","axis1.new"))
    expect_true(any(result[[1]]=="group2"))
    expect_equal(result[[2]], c(1, 2, 3, 4))
  }
})

test_that("test do_svd.kv with group_by output=long", {
  if(requireNamespace("broom")){
    test_df <- data.frame(
      vec1=seq(20),
      vec2=20-seq(20),
      rand=runif(20, min = 0, max=10),
      na=as.vector(replicate(5,c(NA,5))),
      group=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      group2=paste("group",c(rep(1,10), rep(2, 10)), sep=""),
      col=rep(seq(5),4), stringsAsFactors = FALSE)
    loadNamespace("dplyr")
    result <- (
      test_df
      %>%  dplyr::group_by(group2)
      %>%  do_svd.kv(group, col, rand, n_component=1))
    expect_true(!is.unsorted(result[,1]))
    expect_equal(colnames(result), c("group2","group","new.dimension", "value"))
    expect_true(any(result[,1]=="group2"))
    expect_true(any(result[,3]==1))
  }
})


test_that("test do_svd of dimension, output=wide", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    test_df <- data.frame(
      vec1=seq(20),
      vec2=20-seq(20),
      rand=runif(20, min = 0, max=10),
      na=as.vector(replicate(5,c(NA,5))),
      group=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    result <- (
      test_df
      %>%  do_svd.kv(group, col, rand, type="dimension", output="wide"))
    expect_true(!is.unsorted(result[,1]))
    expect_equal(colnames(result), c("col","axis1", "axis2", "axis3"))
    expect_true(any(result[[1]]=="1"))
  }
})

test_that("test do_svd of dimension output long", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    test_df <- data.frame(
      vec1=seq(20),
      vec2=20-seq(20),
      rand=runif(20, min = 0, max=10),
      na=as.vector(replicate(5,c(NA,5))),
      group=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    result <- (
      test_df
      %>%  do_svd.kv(group, col, rand, type="dimension"))
    expect_equal(colnames(result), c("col","new.dimension", "value"))
    expect_true(any(result[[1]]==1))
    expect_true(any(result[[2]]==1))
  }
})

test_that("test do_svd of variance output=wide", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    test_df <- data.frame(
      vec1=seq(20),
      vec2=20-seq(20),
      rand=runif(20, min = 0, max=10),
      na=as.vector(replicate(5,c(NA,5))),
      group=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    result <- (
      test_df
      %>%  do_svd.kv(group, col, rand, type="variance", n_component=2, output="wide"))
    expect_equal(colnames(result),c("axis1", "axis2"))
    expect_equal(nrow(result),1)
  }
})

test_that("test do_svd of variance output", {
  if(requireNamespace("broom")){
    loadNamespace("dplyr")
    test_df <- data.frame(
      vec1=seq(20),
      vec2=20-seq(20),
      rand=runif(20, min = 0, max=10),
      na=as.vector(replicate(5,c(NA,5))),
      group=paste("group",c(rep(1,5), rep(2, 5), rep(3, 5), rep(4, 5)), sep=""),
      col=rep(seq(5),4))
    result <- (
      test_df
      %>%  do_svd.kv(group, col, rand, type="variance", n_component=2))
    expect_equal(colnames(result), c("new.dimension", "value"))
    expect_equal(nrow(result),2)
  }
})

test_that("test do_svd.kv for grouped data frame as subject error", {
  data <- data.frame(group=rep(c(1,2,3), each=6),
                     row = rep(c(1, 1, 2, 2, 3,3), 3),
                     col = rep(c(1,2), 9),
                     val = rep(0, 18))

  expect_error({
    ret <- data %>%
      dplyr::group_by(group) %>%
      do_svd.kv(group, col, val)
  }, "group is a grouping column\\. ungroup\\(\\) may be necessary before this operation\\.")
})

test_that("test do_cmdscale", {
  loadNamespace("reshape2")
  mat <- matrix(c(1,2,3,3,4,5,5,6,6,8,1,2), nrow=4)
  rownames(mat) <- paste("row", seq(nrow(mat)))
  distance <- dist(mat)
  mds_result <- cmdscale(distance, eig=TRUE)
  points <- mds_result$points
  eig <- mds_result$eig

  test_df <- reshape2::melt(mat)
  # test column name with space
  colnames(test_df)[1] <- "Var 1"

  df_tt <- do_dist.kv(test_df, `Var 1`, Var2, value, distinct=TRUE ,diag=TRUE)
  df_tf <- do_dist.kv(test_df, `Var 1`, Var2, value, distinct=TRUE ,diag=FALSE)
  df_ft <- do_dist.kv(test_df, `Var 1`, Var2, value, distinct=FALSE ,diag=TRUE)
  df_ff <- do_dist.kv(test_df, `Var 1`, Var2, value, distinct=FALSE ,diag=FALSE)
  ret_tt <- do_cmdscale(df_tt, `Var 1.x`, `Var 1.y`, value)
  ret_tf <- do_cmdscale(df_tf, `Var 1.x`, `Var 1.y`, value)
  ret_ft <- do_cmdscale(df_ft, `Var 1.x`, `Var 1.y`, value)
  ret_ff <- do_cmdscale(df_ff, `Var 1.x`, `Var 1.y`, value)
  expect_equal(c(ret_tt[[2]], ret_tf[[2]], ret_ft[[2]], ret_ff[[2]]), setNames(rep(points[,1], 4), NULL))

  half_df <- distance %>% as.vector()  %>%  upper_gather(attr(distance, "Labels"), diag=TRUE)
  result_half <- do_cmdscale(half_df, Var1, Var2, value)

})

test_that("do_cmdscale undefined column name error", {
  data <- data.frame(var1 = c(1, 3, 2, NA), var2 = c(1, 3, 2, 10))
  expect_error({
    do_cmdscale(data, var1, var2, var3)
  }, "object 'var3' not found")
})

test_that("do_cmdscale all 0 distances error", {
  data <- data.frame(var1 = c(1, 2, 3, 4), var2 = c(4, 1, 2, 3), val = c(0,0,0,0))
  expect_error({
    do_cmdscale(data, var1, var2, val)
  }, "All distances are 0. Multidimensional scaling cannot be calculated.")
})

test_that("do_cor max_nrow caps the observations, per group and reproducibly", {
  set.seed(11)
  data <- data.frame(a = rnorm(500), b = rnorm(500), grp = rep(c("x", "y"), each = 250))

  full <- data %>% do_cor(a, b)
  capped <- data %>% do_cor(a, b, max_nrow = 100)

  # Independent oracle: the cap is a plain row sample, so the result must equal
  # running the correlation on that sample -- drawn here with the same seed, but
  # correlated with stats::cor rather than with do_cor's own internals.
  set.seed(1)
  sampled <- data %>% dplyr::select(a, b) %>% sample_rows(100)
  expected <- stats::cor(sampled$a, sampled$b, use = "pairwise.complete.obs")
  actual <- (capped %>% dplyr::filter(pair.name.x == "a", pair.name.y == "b"))$correlation
  expect_equal(actual, expected)
  expect_false(isTRUE(all.equal(
    actual,
    (full %>% dplyr::filter(pair.name.x == "a", pair.name.y == "b"))$correlation
  )))

  # NULL is what the Sample Data checkbox sends when it is off: every row, i.e.
  # identical to not passing the argument at all.
  expect_equal(data %>% do_cor(a, b, max_nrow = NULL), full)
  # A cap above the row count changes nothing.
  expect_equal(data %>% do_cor(a, b, max_nrow = 10000), full)
  # Same seed, same sample.
  expect_equal(data %>% do_cor(a, b, max_nrow = 100), capped)

  # The cap is per group.
  grouped <- data %>% dplyr::group_by(grp) %>% do_cor(a, b, max_nrow = 100)
  set.seed(1)
  expected_grouped <- data %>% dplyr::group_by(grp) %>% dplyr::select(a, b) %>% sample_rows(100)
  expect_equal(
    (grouped %>% dplyr::filter(pair.name.x == "a", pair.name.y == "b") %>% dplyr::arrange(grp))$correlation,
    (expected_grouped %>% dplyr::group_by(grp) %>%
      dplyr::summarize(r = stats::cor(a, b, use = "pairwise.complete.obs")) %>%
      dplyr::arrange(grp))$r
  )
})

test_that("do_cor max_nrow caps the cast matrix, not the long input", {
  # 100 keys, 5 long rows each. Each key's mean is exact only if the whole key
  # survives -- sampling the LONG input would change the aggregated values, not
  # just how many of them there are.
  set.seed(12)
  key_mean_x <- rnorm(100)
  key_mean_y <- key_mean_x * 0.7 + rnorm(100, sd = 0.5)
  offsets <- c(-2, -1, 0, 1, 2) # sum to 0, so each key's mean is exact
  long <- data.frame(
    key = rep(1:100, each = 10),
    subj = rep(rep(c("x", "y"), each = 5), 100),
    val = as.vector(sapply(1:100, function(k) {
      c(key_mean_x[k] + offsets, key_mean_y[k] + offsets)
    }))
  )

  capped <- long %>% do_cor(skv = c("subj", "key", "val"), max_nrow = 50)

  # Independent oracle: aggregate first (the means the cast produces), then keep
  # the 50 keys the cap draws, then correlate with stats::cor.
  set.seed(1)
  kept <- sort(sample.int(100, 50))
  expected <- stats::cor(key_mean_x[kept], key_mean_y[kept], use = "pairwise.complete.obs")
  actual <- (capped %>% dplyr::filter(subj.x == "x", subj.y == "y"))$correlation
  expect_equal(actual, expected)

  # Had the long rows been sampled instead, the surviving keys' means would be
  # noisy and the correlation would not match the exact-mean oracle above.
  full <- long %>% do_cor(skv = c("subj", "key", "val"))
  expect_equal(
    (full %>% dplyr::filter(subj.x == "x", subj.y == "y"))$correlation,
    stats::cor(key_mean_x, key_mean_y, use = "pairwise.complete.obs")
  )
  expect_equal(long %>% do_cor(skv = c("subj", "key", "val"), max_nrow = NULL), full)
  expect_equal(long %>% do_cor(skv = c("subj", "key", "val"), max_nrow = 1000), full)
})

test_that("do_cor does not reseed when no rows are sampled", {
  data <- data.frame(a = seq_len(20), b = seq_len(20) + 1)

  set.seed(123)
  before <- .Random.seed
  invisible(data %>% do_cor(a, b, max_nrow = NULL))
  expect_identical(.Random.seed, before)

  set.seed(123)
  before <- .Random.seed
  invisible(data %>% do_cor(a, b, max_nrow = 100))
  expect_identical(.Random.seed, before)

  long <- data.frame(
    subj = rep(c("x", "y"), each = 20),
    key = rep(seq_len(20), 2),
    val = seq_len(40)
  )
  set.seed(123)
  before <- .Random.seed
  invisible(long %>% do_cor(skv = c("subj", "key", "val"), max_nrow = NULL))
  expect_identical(.Random.seed, before)
})

# tam#37638 -- the Correlation report's "Analysis Conditions and Data" table.
# Before this existed, tidy.cor_exploratory's catch-all `else` returned the raw source data for
# any unrecognized type, so the report rendered the whole input data frame as its summary table.
test_that("do_cor analysis_conditions returns the report's 6-row conditions table", {
  set.seed(1)
  df <- data.frame(a = rnorm(20), b = rnorm(20), c = rnorm(20))
  model_df <- df %>% do_cor(`a`, `b`, `c`, method = "pearson", return_type = "model")
  res <- model_df %>% tidy_rowwise(model, type = "analysis_conditions")

  expect_equal(res$Metric, c("Number of Variables", "Variable Names", "Excluded Variables",
                             "Row Count", "Rows Removed", "Correlation"))
  expect_equal(res$Value[[1]], "3")
  expect_equal(res$Value[[2]], "a, b, c")
  expect_equal(res$Value[[3]], "None")
  expect_equal(res$Value[[4]], "20")
  expect_equal(res$Value[[5]], "0 (0.0%)")
  expect_equal(res$Value[[6]], "Pearson Correlation")
  # Hidden columns the report binds its explanation text from. Strings, never R logicals.
  expect_equal(unique(res$correlation_type), "pearson")
  expect_equal(unique(res$correlation_is_auto), "FALSE")
  expect_equal(unique(res$reason), "Pearson was specified in the settings.")
})

test_that("do_cor analysis_conditions reports the auto-resolved correlation and excluded variables", {
  set.seed(2)
  n <- 30
  mk <- function() factor(sample(1:5, n, TRUE), levels = 1:5, ordered = TRUE)
  df <- data.frame(q1 = mk(), q2 = mk(), flat = factor(rep(3, n), levels = 1:5, ordered = TRUE))
  model_df <- suppressWarnings(df %>% do_cor(`q1`, `q2`, `flat`, method = "auto", return_type = "model"))
  res <- model_df %>% tidy_rowwise(model, type = "analysis_conditions")

  # `flat` never varies, so it cannot correlate with anything.
  expect_equal(res$Value[[3]], "flat")
  expect_equal(res$Value[[6]], "Polychoric Correlation")
  expect_equal(unique(res$correlation_is_auto), "TRUE")
  expect_equal(unique(res$reason), "All variables are Factor or Logical.")
})

test_that("do_cor analysis_conditions counts rows the way the analysis actually did", {
  df <- data.frame(a = c(1, 2, 3, NA), b = c(4, 5, NA, NA), c = c(1, 3, 2, NA))
  # pairwise.complete.obs (the default): only an ALL-missing row is unused here
  # (rows 1-2 are complete, row 3 has two values, row 4 is all NA).
  pairwise <- df %>% do_cor(`a`, `b`, `c`, method = "pearson", return_type = "model") %>%
    tidy_rowwise(model, type = "analysis_conditions")
  expect_equal(pairwise$Value[[4]], "3")
  expect_equal(pairwise$Value[[5]], "1 (25.0%)")
  # complete.obs: any missing value drops the whole row.
  complete <- df %>% do_cor(`a`, `b`, `c`, method = "pearson", use = "complete.obs", return_type = "model") %>%
    tidy_rowwise(model, type = "analysis_conditions")
  expect_equal(complete$Value[[4]], "2")
  expect_equal(complete$Value[[5]], "2 (50.0%)")

  # A row with a single observed value contributes to no pairwise coefficient.
  one_obs <- data.frame(
    a = c(1, 4, NA, 7),
    b = c(2, NA, 5, 8),
    c = c(3, NA, 6, 9)
  )
  pairwise_one <- one_obs %>% do_cor(`a`, `b`, `c`, method = "pearson", return_type = "model") %>%
    tidy_rowwise(model, type = "analysis_conditions")
  expect_equal(pairwise_one$Value[[4]], "3")
  expect_equal(pairwise_one$Value[[5]], "1 (25.0%)")
  # na.or.complete is listwise, same as complete.obs.
  na_or_complete <- one_obs %>% do_cor(`a`, `b`, `c`, method = "pearson", use = "na.or.complete",
                                      return_type = "model") %>%
    tidy_rowwise(model, type = "analysis_conditions")
  expect_equal(na_or_complete$Value[[4]], "2")
  expect_equal(na_or_complete$Value[[5]], "2 (50.0%)")
})

test_that("do_cor analysis_conditions follows the use polychoric actually runs, not the one asked for", {
  # do_cor_internal() hands hetcor "complete.obs" only when that was asked for, and
  # "pairwise.complete.obs" for every other mode. So a polychoric fit requested with
  # use = "everything" still runs pairwise, and the row count has to say so. Without that
  # remap the count would fall through to "every row was used".
  mk <- function(v) factor(v, levels = 1:3, ordered = TRUE)
  df <- data.frame(
    a = mk(c(1, 2, NA, 3, 1, 2)),
    b = mk(c(2, NA, 3, 1, 2, 3)),
    c = mk(c(3, NA, 1, 2, 3, 1))
  )
  # Row 2 has a single observed value, so it forms no pair.
  res <- suppressWarnings(
    df %>% do_cor(`a`, `b`, `c`, method = "polychoric", use = "everything", return_type = "model")
  ) %>% tidy_rowwise(model, type = "analysis_conditions")

  expect_equal(res$Value[[6]], "Polychoric Correlation")
  expect_equal(res$Value[[4]], "5")
  expect_equal(res$Value[[5]], paste0("1 (", format(round(1 / 6 * 100, 1), nsmall = 1), "%)"))
})

test_that("do_cor analysis_conditions returns an empty same-shape table for a model saved before it existed", {
  df <- data.frame(a = c(1, 2, 3, 4), b = c(2, 4, 5, 9))
  model_df <- df %>% do_cor(`a`, `b`, method = "pearson", return_type = "model")
  # Simulate a model persisted before analysis_conditions was captured at fit time.
  model_df$model[[1]]$analysis_conditions <- NULL
  res <- model_df %>% tidy_rowwise(model, type = "analysis_conditions")

  expect_equal(nrow(res), 0)
  expect_true(all(c("Metric", "Value", "Description",
                    "correlation_type", "correlation_is_auto", "reason") %in% colnames(res)))
})

test_that("tidy.cor_exploratory errors on an unsupported type instead of returning the source data", {
  df <- data.frame(a = c(1, 2, 3, 4), b = c(2, 4, 5, 9))
  model_df <- df %>% do_cor(`a`, `b`, method = "pearson", return_type = "model")
  # The scatter matrix's own type must keep working.
  expect_equal(nrow(model_df %>% tidy_rowwise(model, type = "data.frame")), 4)
  expect_error(model_df %>% tidy_rowwise(model, type = "no_such_type"),
               "Unsupported tidy type for a correlation model")
})
