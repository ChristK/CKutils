library(testthat)

context("estimates the beta parameters from a mean and a variance given")

test_that("Beta estimation works", {
  x <- estim_beta_params(0, 2)
  y <- estim_beta_params(1, 5)
  z <- estim_beta_params(0.5, 3)
  
  expect_error(estim_beta_params(5, 2), "between(mu, 0, 1) is not TRUE", fixed = TRUE )
  expect_equal(c(x$shape1, x$shape2), c(NaN, NaN))
  expect_equal(c(y$shape1, y$shape2), c(NaN, NaN))
  expect_equal(c(z$shape1, z$shape2), c(5.0005e-05, 5.0005e-05))
})
