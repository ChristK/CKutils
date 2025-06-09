# Test calculates the percentile rank of a numeric vector

library(CKutils)

x <- c(2,5,1,3,4,6)
y <- c("a", 2, 6, "b")

# Test percentile rank calculation
expect_equal(pctl_rank(x, ties.method="min"), c(0.2, 0.8, 0.0, 0.4, 0.6, 1.0),
             info = "Percentile rank calculated correctly")

# Test error for non-numeric input
expect_error(pctl_rank(y, ties.method="min"), pattern = "is\\.numeric\\(x\\) is not TRUE",
             info = "Error for non-numeric input")
