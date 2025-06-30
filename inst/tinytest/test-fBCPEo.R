# Test BCPEo distribution functions against gamlss.dist reference
# Using tinytest framework

# Skip tests if gamlss.dist is not available
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
  exit_file("gamlss.dist not available - skipping BCPEo validation tests")
}

# Load required libraries
library(gamlss.dist)
library(CKutils)

# Test data generators
generate_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  list(
    x = rgamma(n, 2, 1),
    q = rgamma(n, 2, 1),
    p = runif(n, 0.001, 0.999),
    mu = runif(n, 0.5, 5),
    sigma = runif(n, 0.1, 2),
    nu = runif(n, -2, 2),
    tau = runif(n, 0.5, 5)
  )
}

generate_edge_case_data <- function(seed = 456) {
  set.seed(seed)
  n <- 50
  list(
    x = rgamma(n, 2, 1),
    q = rgamma(n, 2, 1),
    p = runif(n, 0.001, 0.999),
    mu = runif(n, 0.5, 5),
    sigma = runif(n, 0.1, 2),
    nu = rep(1e-12, n),  # nu close to 0
    tau = runif(n, 0.5, 5)
  )
}

# Generate test data
data <- generate_test_data()
edge_data <- generate_edge_case_data()

# =============================================================================
# PDF TESTS
# =============================================================================

# Test 1: PDF basic correctness
pdf_ck <- fdBCPEo(data$x, data$mu, data$sigma, data$nu, data$tau)
pdf_ref <- dBCPEo(data$x, data$mu, data$sigma, data$nu, data$tau)

expect_equal(
  pdf_ck, pdf_ref,
  info = "PDF: Basic correctness test"
)

# Test 2: PDF with log = TRUE
pdf_ck_log <- fdBCPEo(data$x, data$mu, data$sigma, data$nu, data$tau, log_ = TRUE)
pdf_ref_log <- dBCPEo(data$x, data$mu, data$sigma, data$nu, data$tau, log = TRUE)

expect_equal(
  pdf_ck_log,
  pdf_ref_log,
  info = "PDF: log=TRUE correctness test"
)

# Test 3: PDF edge case (nu ≈ 0)
pdf_ck_edge <- fdBCPEo(edge_data$x, edge_data$mu, edge_data$sigma, edge_data$nu, edge_data$tau)
pdf_ref_edge <- dBCPEo(edge_data$x, edge_data$mu, edge_data$sigma, edge_data$nu, edge_data$tau)

expect_equal(
  pdf_ck_edge,
  pdf_ref_edge,
  info = "PDF: nu≈0 edge case test"
)

# =============================================================================
# CDF TESTS  
# =============================================================================

# Test 4: CDF basic correctness
cdf_ck <- fpBCPEo(data$q, data$mu, data$sigma, data$nu, data$tau)
cdf_ref <- pBCPEo(data$q, data$mu, data$sigma, data$nu, data$tau)

expect_equal(
  cdf_ck,
  cdf_ref,
  info = "CDF: Basic correctness test"
)

# Test 5: CDF with lower_tail = FALSE
cdf_ck_upper <- fpBCPEo(data$q, data$mu, data$sigma, data$nu, data$tau, lower_tail = FALSE)
cdf_ref_upper <- pBCPEo(data$q, data$mu, data$sigma, data$nu, data$tau, lower.tail = FALSE)

expect_equal(
  cdf_ck_upper,
  cdf_ref_upper,
  info = "CDF: lower_tail=FALSE correctness test"
)

# Test 6: CDF with log_p = TRUE
cdf_ck_log <- fpBCPEo(data$q, data$mu, data$sigma, data$nu, data$tau, log_p = TRUE)
cdf_ref_log <- pBCPEo(data$q, data$mu, data$sigma, data$nu, data$tau, log.p = TRUE)

expect_equal(
  cdf_ck_log,
  cdf_ref_log,
  info = "CDF: log_p=TRUE correctness test"
)

# Test 7: CDF with both lower_tail = FALSE and log_p = TRUE
cdf_ck_both <- fpBCPEo(data$q, data$mu, data$sigma, data$nu, data$tau, 
                       lower_tail = FALSE, log_p = TRUE)
cdf_ref_both <- pBCPEo(data$q, data$mu, data$sigma, data$nu, data$tau, 
                       lower.tail = FALSE, log.p = TRUE)

expect_equal(
  cdf_ck_both,
  cdf_ref_both,
  info = "CDF: lower_tail=FALSE + log_p=TRUE correctness test"
)


# Test 8: CDF edge case (nu ≈ 0)
cdf_ck_edge <- fpBCPEo(edge_data$q, edge_data$mu, edge_data$sigma, edge_data$nu, edge_data$tau)
cdf_ref_edge <- pBCPEo(edge_data$q, edge_data$mu, edge_data$sigma, edge_data$nu, edge_data$tau)

expect_equal(
  cdf_ck_edge,
  cdf_ref_edge,
  info = "CDF: nu≈0 edge case test"
)

# =============================================================================
# QUANTILE TESTS
# =============================================================================

# Test 9: Quantile basic correctness
quantile_ck <- fqBCPEo(data$p, data$mu, data$sigma, data$nu, data$tau)
quantile_ref <- qBCPEo(data$p, data$mu, data$sigma, data$nu, data$tau)

expect_equal(
  quantile_ck,
  quantile_ref,
  info = "Quantile: Basic correctness test"
)


# Test 10: Quantile with lower_tail = FALSE
quantile_ck_upper <- fqBCPEo(data$p, data$mu, data$sigma, data$nu, data$tau, lower_tail = FALSE)
quantile_ref_upper <- qBCPEo(data$p, data$mu, data$sigma, data$nu, data$tau, lower.tail = FALSE)

expect_equal(
  quantile_ck_upper,
  quantile_ref_upper,
  info = "Quantile: lower_tail=FALSE correctness test"
)


# Test 11: Quantile with log_p = TRUE
p_log <- log(data$p)
quantile_ck_log <- fqBCPEo(p_log, data$mu, data$sigma, data$nu, data$tau, log_p = TRUE)
quantile_ref_log <- qBCPEo(p_log, data$mu, data$sigma, data$nu, data$tau, log.p = TRUE)

expect_equal(
  quantile_ck_log,
  quantile_ref_log,
  info = "Quantile: log_p=TRUE correctness test"
)

# Test 12: Quantile with both lower_tail = FALSE and log_p = TRUE
quantile_ck_both <- fqBCPEo(p_log, data$mu, data$sigma, data$nu, data$tau, 
                            lower_tail = FALSE, log_p = TRUE)
quantile_ref_both <- qBCPEo(p_log, data$mu, data$sigma, data$nu, data$tau, 
                            lower.tail = FALSE, log.p = TRUE)
expect_equal(
  quantile_ck_both,
  quantile_ref_both,
  info = "Quantile: lower_tail=FALSE + log_p=TRUE correctness test"
)

# Test 13: Quantile edge case (nu ≈ 0)
quantile_ck_edge <- fqBCPEo(edge_data$p, edge_data$mu, edge_data$sigma, edge_data$nu, edge_data$tau)
quantile_ref_edge <- qBCPEo(edge_data$p, edge_data$mu, edge_data$sigma, edge_data$nu, edge_data$tau)

expect_equal(
  quantile_ck_edge,
  quantile_ref_edge,
  info = "Quantile: nu≈0 edge case test"
)


# =============================================================================
# CONSISTENCY TESTS
# =============================================================================

# Test 14: PDF-CDF consistency (d/dx P(X ≤ x) ≈ f(x))
# Use numerical differentiation to check
dx <- 1e-6
x_test <- data$x[1:10]  # Test subset for speed
mu_test <- data$mu[1:10]
sigma_test <- data$sigma[1:10]
nu_test <- data$nu[1:10]
tau_test <- data$tau[1:10]

cdf_lower <- fpBCPEo(x_test - dx, mu_test, sigma_test, nu_test, tau_test)
cdf_upper <- fpBCPEo(x_test + dx, mu_test, sigma_test, nu_test, tau_test)
pdf_test <- fdBCPEo(x_test, mu_test, sigma_test, nu_test, tau_test)

pdf_numerical <- (cdf_upper - cdf_lower) / (2 * dx)

expect_equal(
  pdf_numerical,
  pdf_test,
  info = "PDF-CDF consistency test"
)


# Test 15: CDF-Quantile consistency (Q(P(x)) ≈ x)
x_test <- data$x[1:20]  # Test subset
mu_test <- data$mu[1:20]
sigma_test <- data$sigma[1:20]
nu_test <- data$nu[1:20]
tau_test <- data$tau[1:20]

p_test <- fpBCPEo(x_test, mu_test, sigma_test, nu_test, tau_test)
x_recovered <- fqBCPEo(p_test, mu_test, sigma_test, nu_test, tau_test)

# For CDF-Quantile consistency, we need to exclude edge cases where p=0 or p=1
# because these return the distribution bounds (0 or Inf) rather than the original x
valid_idx <- which(p_test > 0 & p_test < 1)
if (length(valid_idx) > 0) {
    expect_equal(
      x_recovered[valid_idx], 
      x_test[valid_idx],
      info = "CDF-Quantile consistency test for valid p values"
    )
}

# Test edge cases separately - they should return correct bounds
edge_idx <- which(p_test == 0 | p_test == 1)
if (length(edge_idx) > 0) {
  p0_idx <- which(p_test == 0)
  p1_idx <- which(p_test == 1)
  
  if (length(p0_idx) > 0) {
    expect_true(
      all(x_recovered[p0_idx] == 0),
      info = "CDF-Quantile edge case: p=0 should give q=0"
    )
  }
  
  if (length(p1_idx) > 0) {
    expect_true(
      all(is.infinite(x_recovered[p1_idx]) & x_recovered[p1_idx] > 0),
      info = "CDF-Quantile edge case: p=1 should give q=Inf"
    )
  }
}

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

# Test 16: Error handling for invalid inputs
expect_error(
  fdBCPEo(c(-1, 1), c(1, 1), c(1, 1), c(1, 1), c(1, 1)),
  "x must be >=0"
)

expect_error(
  fdBCPEo(c(1, 1), c(-1, 1), c(1, 1), c(1, 1), c(1, 1)),
  "mu must be positive"
)

expect_error(
  fdBCPEo(c(1, 1), c(1, 1), c(-1, 1), c(1, 1), c(1, 1)),
  "sigma must be positive"
)

expect_error(
  fdBCPEo(c(1, 1), c(1, 1), c(1, 1), c(1, 1), c(-1, 1)),
  "tau must be positive"
)

expect_error(
  fqBCPEo(c(-0.1, 0.5), c(1, 1), c(1, 1), c(1, 1), c(1, 1)),
  "p must be between 0 and 1"
)

expect_error(
  fqBCPEo(c(1.1, 0.5), c(1, 1), c(1, 1), c(1, 1), c(1, 1)),
  "p must be between 0 and 1"
)

# Test 17: Vector length mismatch
expect_error(
  fdBCPEo(c(1, 2), c(1), c(1, 1), c(1, 1), c(1, 1)),
  "Distribution parameters must be of same length"
)
