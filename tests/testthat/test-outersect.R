library(testthat)

context("Calculates the symmetrical set difference of subsets")



test_that("Symmetrical subsetting works", {
  
  y <- "city"
  x <- "age"
  z <- c("age", "city")
  w <- c("city", "age")
  v <- 2
  u <- c("city", 2)
  
  expect_equal( outersect(x, y), z )
  expect_equal( outersect(y, x), w )
  expect_equal( outersect(y, v), u )
  expect_failure( expect_output(outersect(v, y), u), "produced no output")
})