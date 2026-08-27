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

test_that("create_weightback_weights rejects population combinations without sample coverage", {
  data <- tibble::tibble(gender = c("Male", "Male"))
  pop <- tibble::tibble(gender = c("Male", "Female"), population_pct = c(.5, .5))
  expect_error(
    create_weightback_weights(data, pop, "gender"),
    "Every sample and population combination"
  )
})

test_that("create_weightback_weights supports unnormalized population values", {
  data <- tibble::tibble(gender = c("Male", "Male", "Female"))
  pop <- tibble::tibble(gender = c("Male", "Female"), population_pct = c(2, 1))
  result <- create_weightback_weights(data, pop, "gender", normalize_population = FALSE)
  expect_equal(result$weightback_weight, c(3, 3, 3))
})

test_that("create_weightback_weights rejects invalid proportions and output names", {
  data <- tibble::tibble(gender = c("Male", "Female"))
  bad <- tibble::tibble(gender = c("Male", "Female"), population_pct = c(NA_real_, 1))
  expect_error(create_weightback_weights(data, bad, "gender"), "finite")
  expect_error(create_weightback_weights(data, tibble::tibble(gender = c("Male", "Female"), population_pct = c(0, 0)), "gender"), "positive total")
  expect_error(create_weightback_weights(data, tibble::tibble(gender = c("Male", "Female"), population_pct = c(.5, .5)), "gender", weight_col = NA_character_), "non-empty")
})

test_that("create_weightback_weights preserves an existing internal-name column", {
  data <- tibble::tibble(gender = c("Male", "Female"), .weightback_weight_internal = c(10, 20))
  pop <- tibble::tibble(gender = c("Male", "Female"), population_pct = c(.5, .5))
  result <- create_weightback_weights(data, pop, "gender")
  expect_equal(result$.weightback_weight_internal, c(10, 20))
  expect_equal(result$weightback_weight, c(1, 1))
})
