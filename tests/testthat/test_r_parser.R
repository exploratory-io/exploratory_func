context("test R parser")
test_that("get_refs_in_script", {
  refs <- get_refs_in_script("select(cyl)")
  expect_equal(refs, NULL)
  refs <- get_refs_in_script("mutate(cyl2 = cyl)")
  expect_equal(refs, NULL)
  refs <- get_refs_in_script('mutate(cyl2 = !!external[["cyl"]])')
  expect_equal(refs, c('external'))
  refs <- get_refs_in_script("mutate(cyl2 = \nexternal$cyl)")
  expect_equal(refs, c('external'))
  refs <- get_refs_in_script("mutate(cyl2 = \u5916\u90e8$cyl)") # "external" in Japanese.
  expect_equal(refs, c('\u5916\u90e8'))
  refs <- get_refs_in_script("mutate(cyl2 = `\u5916\u90e8`$cyl)") # With backticks.
  expect_equal(refs, c('\u5916\u90e8'))
})
