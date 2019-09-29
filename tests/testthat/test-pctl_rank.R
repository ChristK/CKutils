library(testthat)

context("Calculates the percentile rank of a numeric vector")

test_that("Calcul of percentile rank works", {
  
  x <- c(2,5,1,3,4,6)
  y <- c("a", 2, 6, "b")
  
  expect_equal(pctl_rank(x, ties.method="min"), c(0.2, 0.8, 0.0, 0.4, 0.6, 1.0))
  expect_error(pctl_rank(y, ties.method="min"), "is.numeric(x) is not TRUE", fixed = TRUE)
})
