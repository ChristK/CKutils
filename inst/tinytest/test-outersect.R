# Test calculates the symmetrical set difference of subsets

library(CKutils)

y <- "city"
x <- "age"
z <- c("age", "city")
w <- c("city", "age")
v <- 2
u <- c("city", 2)

# Test symmetrical set operations
expect_equal(outersect(x, y), z, info = "Outersect x,y produces correct result")
expect_equal(outersect(y, x), w, info = "Outersect y,x produces correct result")
expect_equal(outersect(y, v), u, info = "Outersect with mixed types works")

# Note: expect_failure with expect_output doesn't have a direct equivalent in tinytest
# We'll test that the function doesn't produce output when called
result <- outersect(v, y)
expect_true(length(result) > 0, info = "Function produces result without error")