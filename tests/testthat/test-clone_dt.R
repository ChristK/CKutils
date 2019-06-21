library(testthat)

context("cloning a data.table and binding the clones to the initial")

test_that("cloning works", {
  
  x <- c(1, 5, 3)
  dt <- data.table(x)
  dtc <- clone_dt(dt, 2, idcol = F)
  dtcid <- clone_dt(dt, 2, idcol = T)
  
  
  expect_equal(dtc$.id, NULL)
  expect_equal(dtc$x, c(1, 5, 3, 1, 5, 3))
  expect_equal(c(dtcid$x, dtcid$.id), c(c(1, 5, 3, 1, 5, 3), c(1, 1, 1, 2, 2, 2)))
  
  
})
