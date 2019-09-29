library(testthat)

context("Gives sample of from the elements of x of the specified size")

test_that("Sampling works", {
  
  x <- 1:10
  y <- c(NaN, 1, 6, 9, NaN, 7, 5, 8, NaN, 2)
  
  expect_equal(length(resample(x[x > 8])), 2)
  expect_equal(length(resample(x[x > 9])), 1)
  expect_equal(length(resample(x[x > 10])), 0)
  expect_equal(length(resample(y[y > 8])), 1)
  expect_equal(length(resample(y[y > 9])), 0)
  expect_equal(length(resample(y[y > 10])), 0)
  
})
