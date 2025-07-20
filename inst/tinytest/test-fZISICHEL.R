# Test ZISICHEL distribution functions
# Using tinytest framework

if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
  exit_file("gamlss.dist not available - skipping SICHEL validation tests")
}

# Load required libraries
suppressMessages(library(gamlss.dist))

# Set tolerance for floating point comparisons
tolerance <- sqrt(.Machine$double.eps)

# Test data generators
generate_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  list(
    x = sample(0:15, n, replace = TRUE),
    q = sample(0:12, n, replace = TRUE),
    p = runif(n, 0.001, 0.999),
    mu = runif(n, 0.5, 5),
    sigma = runif(n, 0.1, 2),  # Avoid very large sigma values that cause numerical issues
    nu = runif(n, -2, 2),      # Moderate nu range to avoid extreme Bessel function values
    tau = runif(n, 0.01, 0.99)
  )
}

generate_edge_case_data <- function(seed = 456) {
  set.seed(seed)
  n <- 50
  list(
    x = sample(0:8, n, replace = TRUE),
    q = sample(0:6, n, replace = TRUE),
    p = runif(n, 0.001, 0.999),
    mu = runif(n, 0.5, 3),
    sigma = runif(n, 0.1, 1),
    nu = runif(n, -1, 1),
    tau = runif(n, 0.05, 0.95)
  )
}

# Generate test data
data <- generate_test_data()
edge_data <- generate_edge_case_data()

small_data <- generate_test_data(n = 20, seed = 789)

# =============================================================================
# ZERO-INFLATED SICHEL TESTS
# =============================================================================

# Test 16: ZI Quantile basic correctness (small subset)
qtl_zi_ck <- fqZISICHEL(small_data$p, small_data$mu, small_data$sigma, small_data$nu, small_data$tau)
qtl_zi_gamlss <- qZISICHEL(small_data$p, small_data$mu, small_data$sigma, small_data$nu, 
                           small_data$tau, max.value = 10000)
expect_equal(qtl_zi_ck, qtl_zi_gamlss, tolerance = 0,
             info = "ZI Quantile values should match gamlss.dist::qZISICHEL")

# Test 17: ZI CDF basic correctness
cdf_zi_ck <- fpZISICHEL(data$q, data$mu, data$sigma, data$nu, data$tau)
cdf_zi_gamlss <- pZISICHEL(data$q, data$mu, data$sigma, data$nu, data$tau)
expect_equal(cdf_zi_ck, cdf_zi_gamlss, tolerance = tolerance,
             info = "ZI CDF values should match gamlss.dist::pZISICHEL")

# Test 18: ZI CDF lower.tail = FALSE
cdf_zi_upper_ck <- fpZISICHEL(data$q, data$mu, data$sigma, data$nu, data$tau, lower_tail = FALSE)
cdf_zi_upper_gamlss <- pZISICHEL(data$q, data$mu, data$sigma, data$nu, data$tau, lower.tail = FALSE)
expect_equal(cdf_zi_upper_ck, cdf_zi_upper_gamlss, tolerance = tolerance,
             info = "ZI upper tail CDF should match gamlss.dist::pZISICHEL")

# Test 19: ZI CDF log scale
cdf_zi_log_ck <- fpZISICHEL(data$q, data$mu, data$sigma, data$nu, data$tau, log_p = TRUE)
cdf_zi_log_gamlss <- pZISICHEL(data$q, data$mu, data$sigma, data$nu, data$tau, log.p = TRUE)
expect_equal(cdf_zi_log_ck, cdf_zi_log_gamlss, tolerance = tolerance,
             info = "ZI log CDF should match gamlss.dist::pZISICHEL")

# =============================================================================
# SPECIAL CASES AND ERROR HANDLING
# =============================================================================


# Test 22: Error handling for invalid tau in ZI functions
expect_error(fqZISICHEL(c(0.5), mu = 1, sigma = 1, nu = 0, tau = -0.1),
             info = "Should error on negative tau")

expect_error(fqZISICHEL(c(0.5), mu = 1, sigma = 1, nu = 0, tau = 1.1),
             info = "Should error on tau > 1")


# =============================================================================
# RANDOM GENERATION TESTS
# =============================================================================

# Test 27: Zero-inflated random generation
set.seed(789)
r_zi_ck <- frZISICHEL(100, mu = 2, sigma = 1, nu = -0.5, tau = 0.3)
expect_true(length(r_zi_ck) == 100, info = "Should generate correct number of ZI values")
zero_prop <- sum(r_zi_ck == 0) / length(r_zi_ck)
expect_true(zero_prop > 0.2, info = "ZI random generation should produce excess zeros")


# Final success message
# cat("All ZISICHEL distribution tests passed successfully!\n")

