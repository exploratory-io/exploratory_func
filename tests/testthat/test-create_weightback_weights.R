test_that("create_weightback_weights calculates sample-to-population weights", {
  data <- tibble::tibble(gender = c("Male", "Male", "Female"))
  pop <- tibble::tibble(gender = c("Male", "Female"), population_pct = c(.4, .6))
  result <- create_weightback_weights(data, pop, "gender")
  expect_equal(result$weightback_weight, c(.6, .6, 1.8))
})

test_that("create_weightback_weights rejects absent and duplicate population keys", {
  data <- tibble::tibble(gender = c("Male", "Female"))
  expect_error(create_weightback_weights(data, tibble::tibble(gender = "Male", population_pct = 1), "gender"), "Every sample")
  expect_error(create_weightback_weights(data, tibble::tibble(gender = c("Male", "Male"), population_pct = c(.5, .5)), "gender"), "unique keys")
})
