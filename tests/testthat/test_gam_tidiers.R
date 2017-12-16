# how to run this test:
# devtools::test(filter="gam_tidiers")

context("test tidiers for gam")

test_that("test glance", {
  gam_model <- mgcv::gam(data=mtcars, cyl~mpg)
  class(gam_model) <- c("gam_exploratory",class(gam_model))
  res <- broom::glance(gam_model)
  expect_equal(class(res), "data.frame")
  expect_equal(class(res$r_squared), "numeric")
  expect_equal(class(res$deviance_explained), "numeric")
  expect_equal(class(res$gcv), "numeric")
  expect_equal(class(res$scale_est), "numeric")
})
