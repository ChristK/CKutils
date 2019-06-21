library(testthat)

context("returning the matching names of an argument data.table`")

test_that("Names matching works", {
  dt <- data.table(id = c("city", "year", "birth", "idp"), b = c("age", "year", "bp", "name"))
  z <- list("id", "year", "b")
  v <- list("year", "a", "bloodPressure")
  w <- list(1, 3, 5)
  x <- match_colnames_pattern(dt, z)
  
  expect_equal(x, c("id", "b"))
  expect_error(match_colnames_pattern(dt, w), "Input patterns must be of type character")
  expect_equal(match_colnames_pattern(dt, v), character(0))
  
})
