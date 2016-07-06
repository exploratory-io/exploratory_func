context("check util functions")

test_that("test upper_gather", {
  mat <- matrix(seq(20),nrow=5, ncol=4)
  colnames(mat) <- paste("col", seq(4))
  result <- upper_gather(mat)
  expect_equal( typeof(result[[2]]), "character")
  expect_equal(result[[1]], sort(result[[1]]))
  expect_equal(result[result[[1]]==3 & result[[2]]=="col 4", 3][[1]], 18)
  expect_equal(result[result[[1]]==2 & result[[2]]=="col 3", 3][[1]], 12)
  expect_equal(nrow(result), 6)
})

test_that("test upper_gather with vector", {
  mat <- seq(6)
  names <- paste("entity", seq(4))
  result <- upper_gather(mat,names)
  expect_equal(result$Var1, sort(result$Var1))
  expect_equal(result[result[,1]=="entity 1" & result[,2]=="entity 3",3], 2)
  expect_equal(result[result[,1]=="entity 1" & result[,2]=="entity 4",3], 3)
  expect_equal(result[result[,1]=="entity 2" & result[,2]=="entity 3",3], 4)
  expect_equal(result[result[,1]=="entity 2" & result[,2]=="entity 4",3], 5)
  expect_equal(result[result[,1]=="entity 3" & result[,2]=="entity 4",3], 6)
  expect_equal(nrow(result), 6)
})

test_that("test upper_gather with vector diag true", {
  mat <- seq(6)
  names <- paste("entity", seq(4))
  result <- upper_gather(mat,names, diag=1)
  expect_equal(nrow(result), 10)
})

test_that("test group_exclude", {
  test_df <- data.frame(
    col1=rep(paste("col1", seq(2)), 5),
    col2=rep(paste("col2", seq(2)), each=5),
    col3=paste("col3", seq(10))
    )
  test_df$list <- as.list(paste("list", seq(10)))
  ret <- group_exclude(test_df, col1, col2)
  expect_equal(colnames(attr(ret, "label")), "col3")
})

test_that("sparse_cast", {
  test_df <- data.frame(
    row = rep(paste("row", 6-seq(5)), each=4),
    col = rep(paste("col", seq(4)), 5),
    val = rep(c(NA,1,0,0), 5)
  )
  mat <- sparse_cast(test_df, "row", "col", "val")

  expect_equal(dim(mat), c(5, 4))
  expect_equal(dimnames(mat), list(paste("row", seq(5)), paste("col", seq(4))))

  mat <- sparse_cast(test_df, "row", "col")
  expect_equal(dim(mat), c(5, 4))
})

test_that("test group_exclude one col", {
  test_df <- data.frame(
    col1=rep(paste("col1", seq(2)), 5)
  )
  ret <- group_exclude(test_df, col1)
  expect_equal(attr(ret, "label"), NULL)
})

test_that("test avoid_conflict", {
  origin <- c("name1", "name1.new", "name2")
  new <- c("name1", "name2")
  ret <- avoid_conflict(origin, new)
  expect_equal(ret, c("name1.new.new", "name2.new"))
})

test_that("test grouped_by", {
  loadNamespace("dplyr")
  test_df <- data.frame(
    col1=rep(paste("col1", seq(2)), 5),
    col2=rep(paste("col2", seq(2)), each=5),
    col3=paste("col3", seq(10))
  )
  df <- dplyr::group_by(test_df, col1, col2)
  ret <- grouped_by(df)
  expect_equal(ret, c("col1", "col2"))
})

test_that("test mat_to_df", {
  nc <- 4
  nr <- 5
  mat <- matrix(seq(nc*nr), ncol=nc, nrow=nr)
  colnames(mat) <- paste("cname", seq(nc))
  rownames(mat) <- paste("rname", seq(nr))
  ret <- mat_to_df(mat, c("aa", "bb", "value"))
  expect_true(is.character(ret[,1]))
  expect_true(is.character(ret[,2]))
  expect_true(!is.unsorted(ret[,1]))
})

test_that("test %nin%", {
  ret <- c(1,3,NA,2) %nin% c(3, NA)
  expect_equal(ret, c(T,F,F,T))
})
