# Original test is from evaluation package
# https://github.com/hadley/evaluate/blob/master/tests/test-replay.R

context("test replay function")

test_that("test replay",{
	# replay() should work when print() returns visible NULLs
	print.FOO_BAR <- function(x, ...) NULL
	library(evaluate)
	library(stringr)
	ret <- evaluate::evaluate('structure(1, class = "FOO_BAR")')
	# this replay call returns 4 lines.
	a<- capture.output(exploratory::replay(ret))
	expect_equal(length(a), 4)
})
