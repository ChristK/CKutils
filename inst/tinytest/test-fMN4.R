# Test MN4 distribution functions against gamlss.dist reference
# Using tinytest framework
#
# NOTE: The gamlss.dist::qMN4 function has a bug where it doesn't implement
# the lower.tail and log.p parameters correctly. It completely ignores them.
# Our CKutils::fqMN4 function correctly implements these parameters.
# Therefore, some tests need to work around this by manually transforming
# the probabilities to match the expected behavior.

# Skip tests if gamlss.dist is not available
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
  exit_file("gamlss.dist not available - skipping MN4 validation tests")
}

# Load required libraries
suppressMessages(library(gamlss.dist))

# Test data generators
generate_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  list(
    x = sample(1:4, n, replace = TRUE),
    q = sample(1:4, n, replace = TRUE),
    p = runif(n, 0.001, 0.999),
    mu = runif(n, 0.1, 3),
    sigma = runif(n, 0.1, 3),
    nu = runif(n, 0.1, 3)
  )
}

generate_edge_case_data <- function(seed = 456) {
  set.seed(seed)
  n <- 20
  list(
    x = rep(1:4, each = 5),
    q = rep(1:4, each = 5),
    p = c(0.001, 0.25, 0.5, 0.75, 0.999),
    mu = c(rep(0.1, 5), rep(1, 5), rep(2, 5), rep(5, 5)),
    sigma = c(rep(0.1, 5), rep(1, 5), rep(2, 5), rep(5, 5)),
    nu = c(rep(0.1, 5), rep(1, 5), rep(2, 5), rep(5, 5))
  )
}

# Generate test data
data <- generate_test_data()
edge_data <- generate_edge_case_data()

# =============================================================================
# PDF TESTS
# =============================================================================

# Test 1: PDF basic correctness
pdf_ck <- fdMN4(data$x, data$mu, data$sigma, data$nu)
pdf_ref <- dMN4(data$x, data$mu, data$sigma, data$nu)

expect_equal(
  pdf_ck, pdf_ref,
  info = "PDF: Basic correctness test"
)

# Test 2: PDF with log = TRUE
pdf_ck_log <- fdMN4(data$x, data$mu, data$sigma, data$nu, log_ = TRUE)
pdf_ref_log <- dMN4(data$x, data$mu, data$sigma, data$nu, log = TRUE)

expect_equal(
  pdf_ck_log,
  pdf_ref_log,
  info = "PDF: log=TRUE correctness test"
)

# Test 3: PDF edge cases
pdf_ck_edge <- fdMN4(edge_data$x, edge_data$mu, edge_data$sigma, edge_data$nu)
pdf_ref_edge <- dMN4(edge_data$x, edge_data$mu, edge_data$sigma, edge_data$nu)

expect_equal(
  pdf_ck_edge,
  pdf_ref_edge,
  info = "PDF: Edge cases test"
)

# Test 4: PDF with invalid x values
expect_equal(
  fdMN4(c(0, 5, -1), mu = 1, sigma = 1, nu = 1),
  c(0, 0, 0),
  info = "PDF: Invalid x values return 0"
)

# Test 5: PDF with invalid x values and log = TRUE
expect_equal(
  fdMN4(c(0, 5, -1), mu = 1, sigma = 1, nu = 1, log_ = TRUE),
  c(-Inf, -Inf, -Inf),
  info = "PDF: Invalid x values return -Inf when log=TRUE"
)

# =============================================================================
# CDF TESTS  
# =============================================================================

# Test 6: CDF basic correctness
cdf_ck <- fpMN4(data$q, data$mu, data$sigma, data$nu)
cdf_ref <- pMN4(data$q, data$mu, data$sigma, data$nu)

expect_equal(
  cdf_ck,
  cdf_ref,
  info = "CDF: Basic correctness test"
)

# Test 7: CDF with lower_tail = FALSE
cdf_ck_upper <- fpMN4(data$q, data$mu, data$sigma, data$nu, lower_tail = FALSE)
cdf_ref_upper <- pMN4(data$q, data$mu, data$sigma, data$nu, lower.tail = FALSE)

expect_equal(
  cdf_ck_upper,
  cdf_ref_upper,
  info = "CDF: lower_tail=FALSE correctness test"
)

# Test 8: CDF with log_p = TRUE
cdf_ck_log <- fpMN4(data$q, data$mu, data$sigma, data$nu, log_p = TRUE)
cdf_ref_log <- pMN4(data$q, data$mu, data$sigma, data$nu, log.p = TRUE)

expect_equal(
  cdf_ck_log,
  cdf_ref_log,
  info = "CDF: log_p=TRUE correctness test"
)

# Test 9: CDF with both lower_tail = FALSE and log_p = TRUE
cdf_ck_both <- fpMN4(data$q, data$mu, data$sigma, data$nu, 
                     lower_tail = FALSE, log_p = TRUE)
cdf_ref_both <- pMN4(data$q, data$mu, data$sigma, data$nu, 
                     lower.tail = FALSE, log.p = TRUE)

expect_equal(
  cdf_ck_both,
  cdf_ref_both,
  info = "CDF: lower_tail=FALSE + log_p=TRUE correctness test"
)

# Test 10: CDF edge cases
cdf_ck_edge <- fpMN4(edge_data$q, edge_data$mu, edge_data$sigma, edge_data$nu)
cdf_ref_edge <- pMN4(edge_data$q, edge_data$mu, edge_data$sigma, edge_data$nu)

expect_equal(
  cdf_ck_edge,
  cdf_ref_edge,
  info = "CDF: Edge cases test"
)

# Test 11: CDF boundary conditions
expect_equal(
  fpMN4(c(0, 1, 2, 3, 4, 5), mu = 1, sigma = 1, nu = 1),
  c(0, 0.25, 0.5, 0.75, 1, 1),
  info = "CDF: Boundary conditions test"
)

# =============================================================================
# QUANTILE TESTS
# =============================================================================

# Test 12: Quantile basic correctness
quantile_ck <- fqMN4(data$p, data$mu, data$sigma, data$nu)
quantile_ref <- qMN4(data$p, data$mu, data$sigma, data$nu)

expect_equal(
  quantile_ck,
  quantile_ref,
  info = "Quantile: Basic correctness test"
)

# Test 13: Quantile with lower_tail = FALSE
# Note: gamlss.dist qMN4 doesn't implement lower.tail parameter correctly
# so we need to transform the probabilities manually for comparison
quantile_ck_upper <- fqMN4(data$p, data$mu, data$sigma, data$nu, lower_tail = FALSE)
quantile_ref_upper <- qMN4(1 - data$p, data$mu, data$sigma, data$nu, lower.tail = TRUE)

expect_equal(
  quantile_ck_upper,
  quantile_ref_upper,
  info = "Quantile: lower_tail=FALSE correctness test"
)

# Test 14: Quantile with log_p = TRUE
# Note: gamlss.dist qMN4 doesn't implement log.p parameter correctly
# so we need to transform the probabilities manually for comparison
p_log <- log(data$p)
quantile_ck_log <- fqMN4(p_log, data$mu, data$sigma, data$nu, log_p = TRUE)
quantile_ref_log <- qMN4(data$p, data$mu, data$sigma, data$nu, lower.tail = TRUE)

expect_equal(
  quantile_ck_log,
  quantile_ref_log,
  info = "Quantile: log_p=TRUE correctness test"
)

# Test 15: Quantile with both lower_tail = FALSE and log_p = TRUE
quantile_ck_both <- fqMN4(p_log, data$mu, data$sigma, data$nu, 
                          lower_tail = FALSE, log_p = TRUE)
# Note: Transform probabilities for comparison because of bug in the current version of gamlss.dist
quantile_ref_both <- qMN4(1 - data$p, data$mu, data$sigma, data$nu, 
                          lower.tail = FALSE, log.p = FALSE)

expect_equal(
  quantile_ck_both,
  quantile_ref_both,
  info = "Quantile: lower_tail=FALSE + log_p=TRUE correctness test"
)

# Test 16: Quantile edge cases
quantile_ck_edge <- fqMN4(edge_data$p, edge_data$mu, edge_data$sigma, edge_data$nu)
quantile_ref_edge <- qMN4(edge_data$p, edge_data$mu, edge_data$sigma, edge_data$nu)

expect_equal(
  quantile_ck_edge,
  quantile_ref_edge,
  info = "Quantile: Edge cases test"
)

# Test 17: Quantile boundary conditions
expect_equal(
  fqMN4(c(0, 0.25, 0.5, 0.75, 1), mu = 1, sigma = 1, nu = 1),
  c(1, 2, 3, 4, 4),
  info = "Quantile: Boundary conditions test"
)

# =============================================================================
# RANDOM GENERATION TESTS
# =============================================================================

# Test 18: Random generation basic functionality
set.seed(123)
r_ck <- frMN4(1000, mu = 1, sigma = 1, nu = 1)
set.seed(123)
r_ref <- rMN4(1000, mu = 1, sigma = 1, nu = 1)

# Both should have similar distributions (not exactly equal due to different RNG)
expect_true(
  all(r_ck %in% 1:4),
  info = "Random generation: All values in valid range"
)

expect_true(
  abs(mean(r_ck) - mean(r_ref)) < 0.1,
  info = "Random generation: Similar means"
)

# Test 19: Random generation with different parameters
set.seed(456)
r_ck2 <- frMN4(1000, mu = 2, sigma = 0.5, nu = 1)

expect_true(
  all(r_ck2 %in% 1:4),
  info = "Random generation: Different parameters - valid range"
)

# Test 20: Random generation error for invalid n
expect_error(
  frMN4(0, mu = 1, sigma = 1, nu = 1),
  info = "Random generation: Error for n=0"
)

expect_error(
  frMN4(-1, mu = 1, sigma = 1, nu = 1),
  info = "Random generation: Error for negative n"
)

# =============================================================================
# RECYCLING TESTS
# =============================================================================

# Test 21: Parameter recycling in PDF
pdf_recycle <- fdMN4(c(1, 2, 3, 4), mu = 1, sigma = 1, nu = 1)
pdf_no_recycle <- fdMN4(c(1, 2, 3, 4), mu = c(1, 1, 1, 1), 
                        sigma = c(1, 1, 1, 1), nu = c(1, 1, 1, 1))

expect_equal(
  pdf_recycle,
  pdf_no_recycle,
  info = "PDF: Parameter recycling works correctly"
)

# Test 22: Parameter recycling in CDF
cdf_recycle <- fpMN4(c(1, 2, 3, 4), mu = 1, sigma = 1, nu = 1)
cdf_no_recycle <- fpMN4(c(1, 2, 3, 4), mu = c(1, 1, 1, 1), 
                        sigma = c(1, 1, 1, 1), nu = c(1, 1, 1, 1))

expect_equal(
  cdf_recycle,
  cdf_no_recycle,
  info = "CDF: Parameter recycling works correctly"
)

# Test 23: Parameter recycling in quantile function
quantile_recycle <- fqMN4(c(0.1, 0.3, 0.6, 0.9), mu = 1, sigma = 1, nu = 1)
quantile_no_recycle <- fqMN4(c(0.1, 0.3, 0.6, 0.9), mu = c(1, 1, 1, 1), 
                             sigma = c(1, 1, 1, 1), nu = c(1, 1, 1, 1))

expect_equal(
  quantile_recycle,
  quantile_no_recycle,
  info = "Quantile: Parameter recycling works correctly"
)

# Test 24: Parameter recycling in random generation
set.seed(789)
r_recycle <- frMN4(4, mu = 1, sigma = 1, nu = 1)
set.seed(789)
r_no_recycle <- frMN4(4, mu = c(1, 1, 1, 1), sigma = c(1, 1, 1, 1), nu = c(1, 1, 1, 1))

expect_equal(
  r_recycle,
  r_no_recycle,
  info = "Random generation: Parameter recycling works correctly"
)

# Test 25: Complex recycling pattern
pdf_complex <- fdMN4(c(1, 2, 3, 4, 1, 2), mu = c(1, 2), sigma = c(0.5, 1, 1.5), nu = 1)
# Expected: mu recycled to (1, 2, 1, 2, 1, 2), sigma to (0.5, 1, 1.5, 0.5, 1, 1.5), nu to (1, 1, 1, 1, 1, 1)
pdf_expanded <- fdMN4(c(1, 2, 3, 4, 1, 2), 
                      mu = c(1, 2, 1, 2, 1, 2), 
                      sigma = c(0.5, 1, 1.5, 0.5, 1, 1.5), 
                      nu = c(1, 1, 1, 1, 1, 1))

expect_equal(
  pdf_complex,
  pdf_expanded,
  info = "PDF: Complex recycling pattern works correctly"
)

# =============================================================================
# CONSISTENCY TESTS
# =============================================================================

# Test 26: PDF sums to 1
mu_test <- 2
sigma_test <- 1.5
nu_test <- 0.8
pdf_sum <- sum(fdMN4(1:4, mu = mu_test, sigma = sigma_test, nu = nu_test))

expect_equal(
  pdf_sum,
  1,
  tolerance = 1e-10,
  info = "PDF: Probabilities sum to 1"
)

# Test 27: CDF is non-decreasing
cdf_test <- fpMN4(1:4, mu = 1, sigma = 1, nu = 1)
expect_true(
  all(diff(cdf_test) >= 0),
  info = "CDF: Non-decreasing property"
)

# Test 28: CDF boundary values
expect_equal(
  fpMN4(c(0, 4), mu = 1, sigma = 1, nu = 1),
  c(0, 1),
  info = "CDF: Boundary values correct"
)

# Test 29: Quantile-CDF consistency
p_test <- c(0.1, 0.3, 0.6, 0.9)
q_test <- fqMN4(p_test, mu = 1, sigma = 1, nu = 1)
cdf_at_q <- fpMN4(q_test, mu = 1, sigma = 1, nu = 1)

# For discrete distributions, P(X <= Q(p)) >= p
expect_true(
  all(cdf_at_q >= p_test),
  info = "Quantile-CDF: Consistency property"
)

# Test 30: Log probability consistency
pdf_log <- fdMN4(c(1, 2, 3, 4), mu = 1, sigma = 1, nu = 1, log_ = TRUE)
pdf_normal <- log(fdMN4(c(1, 2, 3, 4), mu = 1, sigma = 1, nu = 1))

expect_equal(
  pdf_log,
  pdf_normal,
  info = "PDF: log_ parameter consistency"
)

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

# Test 31: Invalid parameters in PDF
expect_true(
  is.nan(suppressWarnings(fdMN4(1, mu = 0, sigma = 1, nu = 1))),
  info = "PDF: Invalid mu returns NaN"
)

expect_true(
  is.nan(suppressWarnings(fdMN4(1, mu = 1, sigma = 0, nu = 1))),
  info = "PDF: Invalid sigma returns NaN"
)

expect_true(
  is.nan(suppressWarnings(fdMN4(1, mu = 1, sigma = 1, nu = 0))),
  info = "PDF: Invalid nu returns NaN"
)

# Test 32: Invalid parameters in CDF
expect_true(
  is.nan(suppressWarnings(fpMN4(1, mu = 0, sigma = 1, nu = 1))),
  info = "CDF: Invalid mu returns NaN"
)

expect_true(
  is.nan(suppressWarnings(fpMN4(1, mu = 1, sigma = 0, nu = 1))),
  info = "CDF: Invalid sigma returns NaN"
)

expect_true(
  is.nan(suppressWarnings(fpMN4(1, mu = 1, sigma = 1, nu = 0))),
  info = "CDF: Invalid nu returns NaN"
)

# Test 33: Invalid parameters in quantile function
expect_true(
  is.na(suppressWarnings(fqMN4(0.5, mu = 0, sigma = 1, nu = 1))),
  info = "Quantile: Invalid mu returns NA"
)

expect_true(
  is.na(suppressWarnings(fqMN4(0.5, mu = 1, sigma = 0, nu = 1))),
  info = "Quantile: Invalid sigma returns NA"
)

expect_true(
  is.na(suppressWarnings(fqMN4(0.5, mu = 1, sigma = 1, nu = 0))),
  info = "Quantile: Invalid nu returns NA"
)

# Test 34: Invalid probabilities in quantile function
expect_true(
  is.na(suppressWarnings(fqMN4(-0.1, mu = 1, sigma = 1, nu = 1))),
  info = "Quantile: Invalid probability < 0 returns NA"
)

expect_true(
  is.na(suppressWarnings(fqMN4(1.1, mu = 1, sigma = 1, nu = 1))),
  info = "Quantile: Invalid probability > 1 returns NA"
)

# =============================================================================
# PERFORMANCE TESTS (optional, for development)
# =============================================================================

if (FALSE) {
  # Test 35: Performance comparison (optional)
  library(microbenchmark)
  
  n <- 10000
  x <- sample(1:4, n, replace = TRUE)
  mu <- runif(n, 0.1, 3)
  sigma <- runif(n, 0.1, 3)
  nu <- runif(n, 0.1, 3)
  
  mb <- microbenchmark(
    CKutils = fdMN4(x, mu, sigma, nu),
    gamlss = dMN4(x, mu, sigma, nu),
    times = 100
  )
  
  print(mb)
  
  # CKutils should be faster due to SIMD optimization
  expect_true(
    median(mb$time[mb$expr == "CKutils"]) < median(mb$time[mb$expr == "gamlss"]),
    info = "Performance: CKutils faster than gamlss.dist"
  )
}

# Print summary
# cat("All MN4 distribution tests completed successfully!\n")
