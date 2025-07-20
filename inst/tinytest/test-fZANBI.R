# Test ZANBI distribution functions
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

# NOTE: gamlss.dist has some vectorization bugs. I.e. 
# all.equal(
#     gamlss.dist::dZANBI(18, 1, 0.5, 0.5),
#     gamlss.dist::dZANBI(17:18, c(2, 1), c(1, 0.5), c(0.5, 0.5))[2]
# ) # "Mean relative difference: 0.1666667"

# To avoid the bug I will test individual calls instead of vectorized ones.

# =============================================================================
# ZERO-ALTERED NBI TESTS
# =============================================================================

# Test 24: ZANBI PDF basic correctness
# Use vectorized fdZANBI vs sapply for gamlss.dist to avoid vectorization bugs
pdf_zanbi_ck <- fdZANBI(data$x, data$mu, data$sigma, data$nu)
pdf_zanbi_gamlss <- sapply(seq_len(length(data$x)), function(i) {
    dZANBI(data$x[i], data$mu[i], data$sigma[i], data$nu[i])
})
expect_equal(
    pdf_zanbi_ck,
    pdf_zanbi_gamlss,
    tolerance = tolerance,
    info = "ZANBI PDF should match gamlss.dist dZANBI (vectorized vs sapply)"
)

# Test 25: ZANBI CDF basic correctness
# Use vectorized fpZANBI vs sapply for gamlss.dist to avoid vectorization bugs
cdf_zanbi_ck <- fpZANBI(data$q, data$mu, data$sigma, data$nu)
cdf_zanbi_gamlss <- sapply(seq_len(length(data$x)), function(i) {
    pZANBI(data$q[i], data$mu[i], data$sigma[i], data$nu[i])
})
expect_equal(
    cdf_zanbi_ck,
    cdf_zanbi_gamlss,
    tolerance = tolerance,
    info = "ZANBI CDF should match gamlss.dist pZANBI (vectorized vs sapply)"
)

# Test 26: ZANBI quantile basic correctness
# Use vectorized fqZANBI vs sapply for gamlss.dist to be consistent
q_zanbi_ck <- fqZANBI(data$p, data$mu, data$sigma, data$nu)
q_zanbi_gamlss <- sapply(seq_len(length(data$x)), function(i) {
    qZANBI(data$p[i], data$mu[i], data$sigma[i], data$nu[i])
})
expect_equal(
    q_zanbi_ck,
    q_zanbi_gamlss,
    tolerance = tolerance,
    info = "ZANBI quantile should match gamlss.dist qZANBI (vectorized vs sapply)"
)

# Test 27: ZANBI random generation
set.seed(123)
r_zanbi_ck <- frZANBI(100, mu = 2, sigma = 1, nu = 0.1)
expect_equal(
    length(r_zanbi_ck),
    100,
    info = "ZANBI random generation should return correct length"
)
expect_true(
    all(r_zanbi_ck >= 0),
    info = "ZANBI random variates should be non-negative"
)

# Test 28: ZANBI parameter validation - nu out of bounds
expect_error(
    fdZANBI(c(0, 1, 2), mu = 1, sigma = 1, nu = 0),
    info = "fdZANBI should error on nu = 0"
)
expect_error(
    fdZANBI(c(0, 1, 2), mu = 1, sigma = 1, nu = 1),
    info = "fdZANBI should error on nu = 1"
)

