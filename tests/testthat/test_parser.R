context("test parsers")

test_that("test parser_pdf from online", {
  urls <- c("http://www.orimi.com/pdf-test.pdf", "http://vita.had.co.nz/papers/tidy-data.pdf")
  result <- parse_pdf(urls)
  expect_equal(length(urls), length(result))
  expect_true(grepl("Congratulations, your computer is equipped with a PDF",result[[1]]))
  expect_true(grepl("2.3. Tidy data",result[[2]]))
})

test_that("test parser_pdf from local", {

  paths <- c("~/Downloads/pdf-test.pdf", "~/Downloads/（別添２-２）付帯海学パンフレット.pdf", "~/Downloads/fs-sample.pdf")
  result <- parse_pdf(paths)
  expect_equal(length(paths), length(result))
  expect_true(grepl("Congratulations, your computer is equipped with a PDF",result[[1]]))
  expect_true(grepl("このパンフレットは、学研災付帯海外留学保険（海外旅行保険）の概要をご説明したものです",result[[2]]))
})
