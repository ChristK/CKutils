# Test BCPEo distribution functions against gamlss.dist reference
# Using tinytest framework

# Skip tests if gamlss.dist is not available
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
  exit_file("gamlss.dist not available - skipping BCPEo validation tests")
}

# Load required libraries
suppressMessages(library(gamlss.dist))

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
# ADDITIONAL EDGE CASES
# =============================================================================

# Test 13b: Negative nu values should match gamlss.dist
neg_data <- list(
  x = c(0.2, 1.0, 2.5, 5.0),
  q = c(0.1, 0.5, 1.0, 3.0),
  p = c(0.05, 0.5, 0.8, 0.95),
  mu = c(1.2, 2.0, 1.5, 3.0),
  sigma = c(0.3, 0.6, 0.4, 0.8),
  nu = c(-0.5, -1.2, -0.2, -0.7),
  tau = c(1.5, 3.0, 5.0, 2.0)
)

pdf_ck_neg <- fdBCPEo(neg_data$x, neg_data$mu, neg_data$sigma, neg_data$nu, neg_data$tau)
pdf_ref_neg <- dBCPEo(neg_data$x, neg_data$mu, neg_data$sigma, neg_data$nu, neg_data$tau)

expect_equal(
  pdf_ck_neg,
  pdf_ref_neg,
  info = "PDF: negative nu values test"
)

cdf_ck_neg <- fpBCPEo(neg_data$q, neg_data$mu, neg_data$sigma, neg_data$nu, neg_data$tau)
cdf_ref_neg <- pBCPEo(neg_data$q, neg_data$mu, neg_data$sigma, neg_data$nu, neg_data$tau)

expect_equal(
  cdf_ck_neg,
  cdf_ref_neg,
  info = "CDF: negative nu values test"
)

quantile_ck_neg <- fqBCPEo(neg_data$p, neg_data$mu, neg_data$sigma, neg_data$nu, neg_data$tau)
quantile_ref_neg <- qBCPEo(neg_data$p, neg_data$mu, neg_data$sigma, neg_data$nu, neg_data$tau)

expect_equal(
  quantile_ck_neg,
  quantile_ref_neg,
  info = "Quantile: negative nu values test"
)

# Test 13c: log/log_p consistency for safe values
log_params <- list(
  x = c(0.5, 1.5, 2.5),
  q = c(1.5, 2.0, 2.5),
  p = c(0.2, 0.5, 0.8),
  mu = 2.0,
  sigma = 0.4,
  nu = 0.7,
  tau = 3.0
)

pdf_plain <- fdBCPEo(log_params$x, log_params$mu, log_params$sigma, log_params$nu, log_params$tau, log_ = FALSE)
pdf_log <- fdBCPEo(log_params$x, log_params$mu, log_params$sigma, log_params$nu, log_params$tau, log_ = TRUE)

expect_equal(
  log(pdf_plain),
  pdf_log,
  tolerance = 1e-8,
  info = "PDF: log_ should match log of density"
)

cdf_plain <- fpBCPEo(log_params$q, log_params$mu, log_params$sigma, log_params$nu, log_params$tau, log_p = FALSE)
cdf_log <- fpBCPEo(log_params$q, log_params$mu, log_params$sigma, log_params$nu, log_params$tau, log_p = TRUE)

expect_equal(
  log(cdf_plain),
  cdf_log,
  tolerance = 1e-8,
  info = "CDF: log_p should match log of probability"
)

quantile_plain <- fqBCPEo(log_params$p, log_params$mu, log_params$sigma, log_params$nu, log_params$tau, log_p = FALSE)
quantile_log <- fqBCPEo(log(log_params$p), log_params$mu, log_params$sigma, log_params$nu, log_params$tau, log_p = TRUE)

expect_equal(
  quantile_log,
  quantile_plain,
  tolerance = 1e-8,
  info = "Quantile: log_p should match exp(p) path"
)

cdf_upper_plain <- fpBCPEo(log_params$q, log_params$mu, log_params$sigma, log_params$nu, log_params$tau,
                           lower_tail = FALSE, log_p = FALSE)
cdf_upper_log <- fpBCPEo(log_params$q, log_params$mu, log_params$sigma, log_params$nu, log_params$tau,
                         lower_tail = FALSE, log_p = TRUE)

expect_equal(
  log(cdf_upper_plain),
  cdf_upper_log,
  tolerance = 1e-8,
  info = "CDF: upper tail log_p should match log of upper tail"
)

# Test 13d: Quantile bounds for p=0/1 and log_p
bound_mu <- 2.0
bound_sigma <- 0.5
bound_nu <- 0.8
bound_tau <- 3.0

quantile_bounds <- fqBCPEo(c(0, 1), bound_mu, bound_sigma, bound_nu, bound_tau)

expect_true(
  quantile_bounds[1] == 0,
  info = "Quantile: p=0 should return 0"
)

expect_true(
  is.infinite(quantile_bounds[2]) && quantile_bounds[2] > 0,
  info = "Quantile: p=1 should return Inf"
)

quantile_bounds_log <- fqBCPEo(c(-Inf, 0), bound_mu, bound_sigma, bound_nu, bound_tau, log_p = TRUE)

expect_true(
  quantile_bounds_log[1] == 0,
  info = "Quantile: log_p p=-Inf should return 0"
)

expect_true(
  is.infinite(quantile_bounds_log[2]) && quantile_bounds_log[2] > 0,
  info = "Quantile: log_p p=0 should return Inf"
)

quantile_bounds_upper_log <- fqBCPEo(c(-Inf, 0), bound_mu, bound_sigma, bound_nu, bound_tau,
                                     lower_tail = FALSE, log_p = TRUE)

expect_true(
  is.infinite(quantile_bounds_upper_log[1]) && quantile_bounds_upper_log[1] > 0,
  info = "Quantile: upper tail log_p p=-Inf should return Inf"
)

expect_true(
  quantile_bounds_upper_log[2] == 0,
  info = "Quantile: upper tail log_p p=0 should return 0"
)

# Test 13e: CDF monotonicity
q_grid <- seq(0.1, 5, length.out = 50)
cdf_grid <- fpBCPEo(q_grid, 2.0, 0.4, 0.7, 3.0)

expect_true(
  all(diff(cdf_grid) >= -1e-12),
  info = "CDF: should be non-decreasing in q"
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

# Test 17: Parameter recycling functionality (replaces old vector length mismatch test)
# =============================================================================
# PARAMETER RECYCLING TESTS
# =============================================================================

# Test 17a: Basic recycling - single parameter with multiple values
x_vec <- c(1, 2, 3, 4)
mu_single <- 2
sigma_single <- 0.5
nu_single <- 1
tau_single <- 2

pdf_recycled <- fdBCPEo(x_vec, mu_single, sigma_single, nu_single, tau_single)
pdf_expected <- fdBCPEo(x_vec, rep(mu_single, 4), rep(sigma_single, 4), 
                        rep(nu_single, 4), rep(tau_single, 4))

expect_equal(
  pdf_recycled,
  pdf_expected,
  info = "PDF: Basic parameter recycling test"
)

# Test 17b: Mixed length recycling - different parameter vector lengths
x_vec <- c(1, 2, 3, 4, 5, 6)
mu_vec <- c(1, 2, 3)        # length 3
sigma_vec <- c(0.1, 0.2)    # length 2
nu_single <- 0.5            # length 1
tau_vec <- c(2, 3, 4, 5)    # length 4

pdf_recycled <- fdBCPEo(x_vec, mu_vec, sigma_vec, nu_single, tau_vec)

# Manual recycling to expected length (6)
mu_expected <- rep(mu_vec, length.out = 6)        # [1,2,3,1,2,3]
sigma_expected <- rep(sigma_vec, length.out = 6)  # [0.1,0.2,0.1,0.2,0.1,0.2]
nu_expected <- rep(nu_single, length.out = 6)     # [0.5,0.5,0.5,0.5,0.5,0.5]
tau_expected <- rep(tau_vec, length.out = 6)      # [2,3,4,5,2,3]

pdf_expected <- fdBCPEo(x_vec, mu_expected, sigma_expected, nu_expected, tau_expected)

expect_equal(
  pdf_recycled,
  pdf_expected,
  info = "PDF: Mixed length parameter recycling test"
)

# Test 17c: CDF recycling test
q_vec <- c(1, 2, 3, 4)
mu_vec <- c(1, 2)
sigma_single <- 0.3
nu_vec <- c(0, 1, 0.5)
tau_single <- 2.5

cdf_recycled <- fpBCPEo(q_vec, mu_vec, sigma_single, nu_vec, tau_single)

# Expected recycling to length 4
mu_expected <- rep(mu_vec, length.out = 4)        # [1,2,1,2]
sigma_expected <- rep(sigma_single, length.out = 4) # [0.3,0.3,0.3,0.3]
nu_expected <- rep(nu_vec, length.out = 4)        # [0,1,0.5,0]
tau_expected <- rep(tau_single, length.out = 4)   # [2.5,2.5,2.5,2.5]

cdf_expected <- fpBCPEo(q_vec, mu_expected, sigma_expected, nu_expected, tau_expected)

expect_equal(
  cdf_recycled,
  cdf_expected,
  info = "CDF: Parameter recycling test"
)

# Test 17d: Quantile recycling test with options
p_vec <- c(0.1, 0.3, 0.5, 0.7, 0.9)
mu_vec <- c(1, 2)
sigma_vec <- c(0.2, 0.4, 0.6)
nu_single <- 1.5
tau_vec <- c(2, 3)

quantile_recycled <- fqBCPEo(p_vec, mu_vec, sigma_vec, nu_single, tau_vec, 
                             lower_tail = FALSE, log_p = FALSE)

# Expected recycling to length 5
mu_expected <- rep(mu_vec, length.out = 5)        # [1,2,1,2,1]
sigma_expected <- rep(sigma_vec, length.out = 5)  # [0.2,0.4,0.6,0.2,0.4]
nu_expected <- rep(nu_single, length.out = 5)     # [1.5,1.5,1.5,1.5,1.5]
tau_expected <- rep(tau_vec, length.out = 5)      # [2,3,2,3,2]

quantile_expected <- fqBCPEo(p_vec, mu_expected, sigma_expected, nu_expected, tau_expected,
                             lower_tail = FALSE, log_p = FALSE)

expect_equal(
  quantile_recycled,
  quantile_expected,
  info = "Quantile: Parameter recycling with options test"
)

# Test 17e: Recycling consistency across all functions
# Test that all three functions (PDF, CDF, Quantile) handle recycling consistently
set.seed(789)
x_test <- rgamma(6, 2, 1)
mu_vec <- c(1, 2, 3)
sigma_vec <- c(0.1, 0.2)
nu_single <- 0.8
tau_vec <- c(2, 3, 4, 5)

# All functions should use the same recycling logic
pdf_result <- fdBCPEo(x_test, mu_vec, sigma_vec, nu_single, tau_vec)
cdf_result <- fpBCPEo(x_test, mu_vec, sigma_vec, nu_single, tau_vec)

# Compute probabilities and then quantiles
p_from_cdf <- fpBCPEo(x_test, mu_vec, sigma_vec, nu_single, tau_vec)
# Filter out extreme values for quantile consistency
valid_p <- p_from_cdf[p_from_cdf > 0.001 & p_from_cdf < 0.999]
if (length(valid_p) > 0) {
  x_recovered <- fqBCPEo(valid_p, rep(mu_vec, length.out = length(valid_p)), 
                         rep(sigma_vec, length.out = length(valid_p)),
                         rep(nu_single, length.out = length(valid_p)),
                         rep(tau_vec, length.out = length(valid_p)))
  
  # The recovered x should be close to original (within recycled parameter context)
  expect_true(
    all(is.finite(x_recovered)) && all(x_recovered > 0),
    info = "Recycling consistency: quantiles should be finite and positive"
  )
}

expect_true(
  all(is.finite(pdf_result)) && all(pdf_result >= 0),
  info = "Recycling consistency: PDF values should be finite and non-negative"
)

expect_true(
  all(is.finite(cdf_result)) && all(cdf_result >= 0) && all(cdf_result <= 1),
  info = "Recycling consistency: CDF values should be valid probabilities"
)

# Test 17f: Edge case - all parameters same length (should work without recycling)
n <- 5
x_vec <- rgamma(n, 2, 1)
mu_vec <- runif(n, 1, 3)
sigma_vec <- runif(n, 0.1, 0.5)
nu_vec <- runif(n, -1, 1)
tau_vec <- runif(n, 1, 4)

# These should work exactly as before (no recycling needed)
pdf_same_length <- fdBCPEo(x_vec, mu_vec, sigma_vec, nu_vec, tau_vec)
cdf_same_length <- fpBCPEo(x_vec, mu_vec, sigma_vec, nu_vec, tau_vec)

expect_true(
  all(is.finite(pdf_same_length)) && all(pdf_same_length >= 0),
  info = "Equal length parameters: PDF should work without recycling"
)

expect_true(
  all(is.finite(cdf_same_length)) && all(cdf_same_length >= 0) && all(cdf_same_length <= 1),
  info = "Equal length parameters: CDF should work without recycling"
)

# Test 17g: Extreme recycling - length 1 to length 100
n_large <- 100
x_large <- rgamma(n_large, 2, 1)
mu_single <- 2.5
sigma_single <- 0.3
nu_single <- 1.2
tau_single <- 3.0

pdf_large_recycled <- fdBCPEo(x_large, mu_single, sigma_single, nu_single, tau_single)

# Should be equivalent to manually expanded parameters
pdf_large_expected <- fdBCPEo(x_large, rep(mu_single, n_large), rep(sigma_single, n_large),
                              rep(nu_single, n_large), rep(tau_single, n_large))

expect_equal(
  pdf_large_recycled,
  pdf_large_expected,
  info = "Large scale parameter recycling test"
)

# Test 17h: Recycling with special parameter values
# Test recycling works correctly with edge case parameter values
x_edge <- c(1, 2, 3, 4)
mu_vec <- c(0.5, 2)      # Small and normal mu
sigma_vec <- c(0.01, 1)  # Very small and normal sigma  
nu_vec <- c(-1, 0, 1)    # Negative, zero, and positive nu
tau_vec <- c(0.5, 10)    # Small and large tau

pdf_edge_recycled <- fdBCPEo(x_edge, mu_vec, sigma_vec, nu_vec, tau_vec)

expect_true(
  all(is.finite(pdf_edge_recycled)) && all(pdf_edge_recycled >= 0),
  info = "Edge case parameter recycling: PDF should handle extreme parameter values"
)

cdf_edge_recycled <- fpBCPEo(x_edge, mu_vec, sigma_vec, nu_vec, tau_vec)

expect_true(
  all(is.finite(cdf_edge_recycled)) && all(cdf_edge_recycled >= 0) && all(cdf_edge_recycled <= 1),
  info = "Edge case parameter recycling: CDF should handle extreme parameter values"
)

# Test 17i: Verify recycling matches gamlss.dist behavior when available
if (requireNamespace("gamlss.dist", quietly = TRUE)) {
  # Test that our recycling matches R's built-in recycling used by gamlss.dist
  x_test <- c(1, 2, 3)
  mu_test <- c(1, 2)
  sigma_test <- 0.5
  nu_test <- c(0.5, 1.5, -0.5)
  tau_test <- c(2, 3)
  
  # Both should handle recycling the same way
  pdf_ck_recycled <- fdBCPEo(x_test, mu_test, sigma_test, nu_test, tau_test)
  pdf_ref_recycled <- suppressWarnings(gamlss.dist::dBCPEo(x_test, mu_test, sigma_test, nu_test, tau_test))
  
  expect_equal(
    pdf_ck_recycled,
    pdf_ref_recycled,
    info = "Recycling compatibility with gamlss.dist reference"
  )
}
