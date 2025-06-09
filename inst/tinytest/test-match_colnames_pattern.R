# Test returning the matching names of an argument data.table

library(data.table)
library(CKutils)

dt <- data.table(id = c("city", "year", "birth", "idp"), b = c("age", "year", "bp", "name"))
z <- list("id", "year", "b")
v <- list("year", "a", "bloodPressure")
w <- list(1, 3, 5)
x <- match_colnames_pattern(dt, z)

# Test successful pattern matching
expect_equal(x, c("id", "b"), info = "Correct column names matched")

# Test error for non-character patterns
expect_error(match_colnames_pattern(dt, w), pattern = "Input patterns must be of type character",
             info = "Error for non-character patterns")

# Test empty result for non-matching patterns
expect_equal(match_colnames_pattern(dt, v), character(0), info = "Empty result for non-matching patterns")
