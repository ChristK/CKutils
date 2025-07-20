# Test ZINBI distribution functions
# Using tinytest framework

if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
    exit_file("gamlss.dist not available - skipping NBI validation tests")
}

# Load required libraries
suppressMessages(library(gamlss.dist))

# Set tolerance for floating point comparisons
tolerance <- sqrt(.Machine$double.eps)

# Test data generators
generate_test_data <- function(n = 100, seed = 123) {
    set.seed(seed)
    list(
        x = sample(0:20, n, replace = TRUE),
        q = sample(0:15, n, replace = TRUE),
        p = runif(n, 0.001, 0.999),
        mu = runif(n, 0.5, 5),
        sigma = runif(n, 0.1, 2),
        nu = runif(n, 0.01, 0.99)
    )
}

generate_edge_case_data <- function(seed = 456) {
    set.seed(seed)
    n <- 50
    list(
        x = sample(0:10, n, replace = TRUE),
        q = sample(0:8, n, replace = TRUE),
        p = runif(n, 0.001, 0.999),
        mu = runif(n, 0.5, 3),
        sigma = runif(n, 0.1, 1),
        nu = runif(n, 0.01, 0.99)
    )
}

# Generate test data
data <- generate_test_data()
edge_data <- generate_edge_case_data()

# =============================================================================
# ZERO-INFLATED NBI TESTS
# =============================================================================

# Test 19: ZINBI PDF basic correctness
pdf_zinbi_ck <- fdZINBI(data$x, data$mu, data$sigma, data$nu)
pdf_zinbi_gamlss <- dZINBI(data$x, data$mu, data$sigma, data$nu)
expect_equal(
    pdf_zinbi_ck,
    pdf_zinbi_gamlss,
    tolerance = tolerance,
    info = "ZINBI PDF should match gamlss.dist dZINBI"
)

# Test 20: ZINBI CDF basic correctness
cdf_zinbi_ck <- fpZINBI(data$q, data$mu, data$sigma, data$nu)
cdf_zinbi_gamlss <- pZINBI(data$q, data$mu, data$sigma, data$nu)
expect_equal(
    cdf_zinbi_ck,
    cdf_zinbi_gamlss,
    tolerance = tolerance,
    info = "ZINBI CDF should match gamlss.dist pZINBI"
)

# Test 21: ZINBI quantile basic correctness
q_zinbi_ck <- fqZINBI(data$p, data$mu, data$sigma, data$nu)
q_zinbi_gamlss <- qZINBI(data$p, data$mu, data$sigma, data$nu)
expect_equal(
    q_zinbi_ck,
    q_zinbi_gamlss,
    tolerance = tolerance,
    info = "ZINBI quantile should match gamlss.dist qZINBI"
)

# Test 22: ZINBI random generation
set.seed(123)
r_zinbi_ck <- frZINBI(100, mu = 2, sigma = 1, nu = 0.1)
expect_equal(
    length(r_zinbi_ck),
    100,
    info = "ZINBI random generation should return correct length"
)
expect_true(
    all(r_zinbi_ck >= 0),
    info = "ZINBI random variates should be non-negative"
)

# Test 23: ZINBI parameter validation - nu out of bounds
expect_error(
    fdZINBI(c(0, 1, 2), mu = 1, sigma = 1, nu = 0),
    info = "fdZINBI should error on nu = 0"
)
expect_error(
    fdZINBI(c(0, 1, 2), mu = 1, sigma = 1, nu = 1),
    info = "fdZINBI should error on nu = 1"
)