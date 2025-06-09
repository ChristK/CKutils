# Test estimates the beta parameters from a mean and a variance given

library(CKutils)

x <- estim_beta_params(0, 2)
y <- estim_beta_params(1, 5)
z <- estim_beta_params(0.5, 3)

# Test error for invalid mu value
expect_error(estim_beta_params(5, 2), pattern = "between\\(mu, 0, 1\\) is not TRUE", 
             info = "Error thrown for mu outside [0,1]")

# Test NaN results for boundary cases
expect_equal(c(x$shape1, x$shape2), c(NaN, NaN), info = "NaN for mu=0")
expect_equal(c(y$shape1, y$shape2), c(NaN, NaN), info = "NaN for mu=1")

# Test valid beta parameter estimation
expect_equal(c(z$shape1, z$shape2), c(5.0005e-05, 5.0005e-05), info = "Valid beta parameters for mu=0.5")
