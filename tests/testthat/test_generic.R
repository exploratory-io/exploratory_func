context("test generic functions")

test_that("test clean_data_frame",{
  # create df with dupicated columns names and data frame type column 
  df <- data.frame(a = 1:5, a = 2:6)
  colnames(df)<-c("a", "a")
  df$b <- data.frame(c = 3:7, d = 4:8)

  result <- clean_data_frame(df)
  # data frame column should be flattened out and the result data frame should have 4 columns.
  expect_equal(length(colnames(result)), 4)
  expect_equal(colnames(result), c("a", "a.1", "b.c", "b.d"))
})
