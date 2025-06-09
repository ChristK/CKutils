# Test gives sample from the elements of x of the specified size

library(CKutils)

x <- 1:10
y <- c(NaN, 1, 6, 9, NaN, 7, 5, 8, NaN, 2)

# Test resampling with different subset sizes
expect_equal(length(resample(x[x > 8])), 2, info = "Resample length correct for x > 8")
expect_equal(length(resample(x[x > 9])), 1, info = "Resample length correct for x > 9")
expect_equal(length(resample(x[x > 10])), 0, info = "Resample length correct for empty set")

# Test resampling with NaN values
expect_equal(length(resample(y[y > 8])), 1, info = "Resample handles NaN values correctly")
expect_equal(length(resample(y[y > 9])), 0, info = "Resample handles empty NaN subset")
expect_equal(length(resample(y[y > 10])), 0, info = "Resample handles empty subset with NaN")
