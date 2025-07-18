# Test BNB distribution functions
# Using tinytest framework

# Skip tests if gamlss.dist is not available
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
  exit_file("gamlss.dist not available - skipping BNB validation tests")
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
    nu = runif(n, 0.1, 3)
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
    nu = runif(n, 0.1, 2)
  )
}

# Generate test data
data <- generate_test_data()
edge_data <- generate_edge_case_data()

# =============================================================================
# PDF TESTS
# =============================================================================

# Test 1: PDF basic correctness
pdf_ck <- fdBNB(data$x, data$mu, data$sigma, data$nu)
pdf_ref <- gamlss.dist::dBNB(data$x, data$mu, data$sigma, data$nu)

expect_equal(
  pdf_ck,
  pdf_ref,
  tolerance = tolerance,
  info = "PDF: Basic correctness test against gamlss.dist reference"
)

# Test 2: PDF log scale
pdf_ck_log <- fdBNB(data$x, data$mu, data$sigma, data$nu, log = TRUE)
pdf_ref_log <- gamlss.dist::dBNB(data$x, data$mu, data$sigma, data$nu, log = TRUE)

expect_equal(
  pdf_ck_log,
  pdf_ref_log,
  tolerance = tolerance,
  info = "PDF: Log scale correctness test"
)

# Test 3: PDF edge cases
pdf_edge_ck <- fdBNB(edge_data$x, edge_data$mu, edge_data$sigma, edge_data$nu)
pdf_edge_ref <- gamlss.dist::dBNB(edge_data$x, edge_data$mu, edge_data$sigma, edge_data$nu)

expect_equal(
  pdf_edge_ck,
  pdf_edge_ref,
  tolerance = tolerance,
  info = "PDF: Edge cases test"
)

# =============================================================================
# CDF TESTS
# =============================================================================

# Test 4: CDF basic correctness
cdf_ck <- fpBNB(data$q, data$mu, data$sigma, data$nu)
cdf_ref <- gamlss.dist::pBNB(data$q, data$mu, data$sigma, data$nu)

expect_equal(
  cdf_ck,
  cdf_ref,
  tolerance = tolerance,
  info = "CDF: Basic correctness test against gamlss.dist reference"
)

# Test 5: CDF lower.tail = FALSE
cdf_ck_upper <- fpBNB(data$q, data$mu, data$sigma, data$nu, lower_tail = FALSE)
cdf_ref_upper <- gamlss.dist::pBNB(data$q, data$mu, data$sigma, data$nu, lower.tail = FALSE)

expect_equal(
  cdf_ck_upper,
  cdf_ref_upper,
  tolerance = tolerance,
  info = "CDF: upper tail test"
)

# Test 6: CDF log.p = TRUE
cdf_ck_logp <- fpBNB(data$q, data$mu, data$sigma, data$nu, log_p = TRUE)
cdf_ref_logp <- gamlss.dist::pBNB(data$q, data$mu, data$sigma, data$nu, log.p = TRUE)

expect_equal(
  cdf_ck_logp,
  cdf_ref_logp,
  tolerance = tolerance,
  info = "CDF: log.p test"
)

# =============================================================================
# QUANTILE TESTS
# =============================================================================

# Test 7: Quantile basic correctness (subset for speed)
test_indices <- sample(length(data$p), 20)
p_test <- data$p[test_indices]
mu_test <- data$mu[test_indices]
sigma_test <- data$sigma[test_indices]
nu_test <- data$nu[test_indices]

qtl_ck <- fqBNB(p_test, mu_test, sigma_test, nu_test)
qtl_ref <- gamlss.dist::qBNB(p_test, mu_test, sigma_test, nu_test)

expect_equal(
  qtl_ck,
  qtl_ref,
  info = "Quantile: Basic correctness test"
)

# Test 8: Quantile lower.tail = FALSE
qtl_ck_upper <- fqBNB(p_test, mu_test, sigma_test, nu_test, lower_tail = FALSE)
qtl_ref_upper <- gamlss.dist::qBNB(p_test, mu_test, sigma_test, nu_test, lower.tail = FALSE)

expect_equal(
  qtl_ck_upper,
  qtl_ref_upper,
  info = "Quantile: upper tail test"
)

# =============================================================================
# ZERO INFLATED TESTS
# =============================================================================

# Generate tau parameter
tau_test <- runif(20, 0.01, 0.3)

# Test 9: ZIBNB quantile
qtl_zi_ck <- fqZIBNB(p_test, mu_test, sigma_test, nu_test, tau_test)
qtl_zi_ref <- gamlss.dist::qZIBNB(p_test, mu_test, sigma_test, nu_test, tau_test)

expect_equal(
  qtl_zi_ck,
  qtl_zi_ref,
  info = "ZIBNB: Quantile test"
)

# Test 10: ZABNB quantile
qtl_za_ck <- fqZABNB(p_test, mu_test, sigma_test, nu_test, tau_test)
qtl_za_ref <- gamlss.dist::qZABNB(p_test, mu_test, sigma_test, nu_test, tau_test)

expect_equal(
  qtl_za_ck,
  qtl_za_ref,
  info = "ZABNB: Quantile test"
)

# =============================================================================
# PARAMETER RECYCLING TESTS
# =============================================================================

# Test 11: Basic recycling - single parameter with multiple values
x_vec <- c(0, 1, 2, 3)
mu_single <- 2
sigma_single <- 0.5
nu_single <- 1

pdf_recycled <- fdBNB(x_vec, mu_single, sigma_single, nu_single)
pdf_expected <- fdBNB(x_vec, rep(mu_single, 4), rep(sigma_single, 4), rep(nu_single, 4))

expect_equal(
  pdf_recycled,
  pdf_expected,
  info = "PDF: Basic parameter recycling test"
)

# Test 12: Mixed length recycling - different parameter vector lengths
x_vec <- c(0, 1, 2, 3, 4, 5)
mu_vec <- c(1, 2, 3)        # length 3
sigma_vec <- c(0.1, 0.2)    # length 2
nu_single <- 0.5            # length 1

pdf_recycled <- fdBNB(x_vec, mu_vec, sigma_vec, nu_single)

# Manual recycling to expected length (6)
mu_expected <- rep(mu_vec, length.out = 6)        # [1,2,3,1,2,3]
sigma_expected <- rep(sigma_vec, length.out = 6)  # [0.1,0.2,0.1,0.2,0.1,0.2]
nu_expected <- rep(nu_single, length.out = 6)     # [0.5,0.5,0.5,0.5,0.5,0.5]

pdf_expected <- fdBNB(x_vec, mu_expected, sigma_expected, nu_expected)

expect_equal(
  pdf_recycled,
  pdf_expected,
  info = "PDF: Mixed length parameter recycling test"
)

# Test 13: CDF parameter recycling
q_vec <- c(0, 1, 2, 3, 4, 5)
mu_vec <- c(1, 2)           # length 2
sigma_single <- 0.5         # length 1
nu_vec <- c(1, 1.5, 2)      # length 3

cdf_recycled <- fpBNB(q_vec, mu_vec, sigma_single, nu_vec)
cdf_expected <- fpBNB(q_vec, rep(mu_vec, length.out = 6), 
                      rep(sigma_single, 6), rep(nu_vec, length.out = 6))

expect_equal(
  cdf_recycled,
  cdf_expected,
  info = "CDF: Parameter recycling test"
)

# Test 14: Quantile parameter recycling with 5 parameters (ZIBNB)
p_vec <- c(0.1, 0.3, 0.5, 0.7)
mu_vec <- c(1, 2)
sigma_single <- 0.5
nu_vec <- c(1, 1.5)
tau_single <- 0.1

qtl_recycled <- fqZIBNB(p_vec, mu_vec, sigma_single, nu_vec, tau_single)
qtl_expected <- fqZIBNB(p_vec, rep(mu_vec, length.out = 4), 
                        rep(sigma_single, 4), rep(nu_vec, length.out = 4),
                        rep(tau_single, 4))

expect_equal(
  qtl_recycled,
  qtl_expected,
  info = "ZIBNB: Parameter recycling test"
)

# =============================================================================
# PERFORMANCE AND EDGE CASE TESTS
# =============================================================================

# Test 15: Large scale recycling
n_large <- 1000
x_large <- sample(0:10, n_large, replace = TRUE)
mu_large <- runif(100, 0.5, 3)  # Different length to test recycling
sigma_large <- 0.5
nu_large <- 1.0

pdf_large <- fdBNB(x_large, mu_large, sigma_large, nu_large)

expect_true(
  length(pdf_large) == n_large && all(is.finite(pdf_large)) && all(pdf_large >= 0),
  info = "Large scale parameter recycling test"
)

# Test 16: Edge case parameter recycling
x_edge <- c(0, 1, 2, 3)
mu_edge <- c(0.1, 5.0)      # Extreme mu values
sigma_edge <- c(0.01, 3.0)  # Extreme sigma values  
nu_edge <- c(0.1, 5.0)      # Extreme nu values

pdf_edge_recycled <- fdBNB(x_edge, mu_edge, sigma_edge, nu_edge)

expect_true(
  all(is.finite(pdf_edge_recycled)) && all(pdf_edge_recycled >= 0),
  info = "Edge case parameter recycling: PDF should handle extreme parameter values"
)

# Test 17: Verify recycling matches gamlss.dist behavior when available
if (requireNamespace("gamlss.dist", quietly = TRUE)) {
  # Test that our recycling matches R's built-in recycling used by gamlss.dist
  x_test <- c(0, 1, 2)
  mu_test <- c(1, 2)
  sigma_test <- 0.5
  nu_test <- c(0.5, 1.5, 2.5)
  
  # Both should handle recycling the same way
  pdf_ck_recycled <- fdBNB(x_test, mu_test, sigma_test, nu_test)
  pdf_ref_recycled <- gamlss.dist::dBNB(x_test, mu_test, sigma_test, nu_test)
  
  expect_equal(
    pdf_ck_recycled,
    pdf_ref_recycled,
    info = "Recycling compatibility with gamlss.dist reference"
  )
}

# cat("All BNB distribution tests completed successfully!\n")
