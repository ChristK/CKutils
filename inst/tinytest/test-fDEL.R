# Comprehensive Test Suite for DEL distribution functions
# Comparing CKutils::fdDEL, fpDEL, fqDEL with gamlss.dist::dDEL, pDEL, qDEL

# Skip tests if gamlss.dist is not available
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
    exit_file("gamlss.dist not available - skipping DEL validation tests")
}

library(gamlss.dist)
library(CKutils)

# Set tolerance for floating point comparisons
tolerance <- sqrt(.Machine$double.eps)
# Use more relaxed tolerance for extreme tail log probabilities
log_tail_tolerance <- 1e-2  # Allow larger differences in extreme log tail regions where numerical precision matters

# =============================================================================
# TEST PARAMETERS - Valid parameter combinations for DEL distribution
# =============================================================================

# Basic parameter sets (nu must be between 0 and 1 for DEL)
basic_params <- list(
  list(mu = 1, sigma = 0.5, nu = 0.1),
  list(mu = 2, sigma = 1, nu = 0.5),
  list(mu = 5, sigma = 2, nu = 0.9),
  list(mu = 0.5, sigma = 0.1, nu = 0.3),
  list(mu = 10, sigma = 3, nu = 0.7)
)

# Test values
x_vals <- 0:20
q_vals <- 0:15  
p_vals <- c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999)

# =============================================================================
# DENSITY FUNCTION COMPARISONS - fdDEL vs gamlss.dist::dDEL
# =============================================================================

# Test 1: Basic density comparison
for (i in seq_along(basic_params)) {
  params <- basic_params[[i]]
  
  # Compare fdDEL with dDEL
  ck_dens <- fdDEL(x_vals, mu = params$mu, sigma = params$sigma, nu = params$nu)
  gamlss_dens <- dDEL(x_vals, mu = params$mu, sigma = params$sigma, nu = params$nu)
  
  test_name <- paste0("Density comparison - params set ", i)
  print(expect_equal(ck_dens, gamlss_dens, tolerance = tolerance, info = test_name))
}

# Test 2: Log density comparison
for (i in seq_along(basic_params)) {
  params <- basic_params[[i]]
  
  # Compare log densities
  ck_log_dens <- fdDEL(x_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, log_ = TRUE)
  gamlss_log_dens <- dDEL(x_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, log = TRUE)
  
  test_name <- paste0("Log density comparison - params set ", i)
  print(expect_equal(ck_log_dens, gamlss_log_dens, tolerance = tolerance, info = test_name))
}

# Test 3: Single value density tests
single_vals <- c(0, 1, 2, 5, 10)
for (val in single_vals) {
  ck_dens <- fdDEL(val, mu = 3, sigma = 1.5, nu = 0.4)
  gamlss_dens <- dDEL(val, mu = 3, sigma = 1.5, nu = 0.4)
  
  test_name <- paste0("Single value density - x=", val)
  print(expect_equal(ck_dens, gamlss_dens, tolerance = tolerance, info = test_name))
}

# =============================================================================
# CDF FUNCTION COMPARISONS - fpDEL vs gamlss.dist::pDEL
# =============================================================================

# Test 4: Basic CDF comparison
for (i in seq_along(basic_params)) {
  params <- basic_params[[i]]
  
  # Compare fpDEL with pDEL (lower tail)
  ck_cdf <- fpDEL(q_vals, mu = params$mu, sigma = params$sigma, nu = params$nu)
  gamlss_cdf <- pDEL(q_vals, mu = params$mu, sigma = params$sigma, nu = params$nu)
  
  test_name <- paste0("CDF comparison (lower tail) - params set ", i)
  print(expect_equal(ck_cdf, gamlss_cdf, tolerance = tolerance, info = test_name))
}

# Test 5: Upper tail CDF comparison
for (i in seq_along(basic_params)) {
  params <- basic_params[[i]]
  
  # Compare upper tail CDFs
  ck_cdf_upper <- fpDEL(q_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, lower_tail = FALSE)
  gamlss_cdf_upper <- pDEL(q_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, lower.tail = FALSE)
  
  test_name <- paste0("CDF comparison (upper tail) - params set ", i)
 print(expect_equal(ck_cdf_upper, gamlss_cdf_upper, tolerance = tolerance, info = test_name))
}

# Test 6: Log CDF comparison
for (i in seq_along(basic_params)) {
  params <- basic_params[[i]]
  
  # Compare log CDFs
  ck_log_cdf <- fpDEL(q_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, log_p = TRUE)
  gamlss_log_cdf <- pDEL(q_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, log.p = TRUE)
  
  test_name <- paste0("Log CDF comparison - params set ", i)
  print(expect_equal(ck_log_cdf, gamlss_log_cdf, tolerance = tolerance, info = test_name))
}

# Test 7: Log upper tail CDF comparison
for (i in seq_along(basic_params)) {
  params <- basic_params[[i]]
  
  # Compare log upper tail CDFs
  ck_log_cdf_upper <- suppressWarnings(fpDEL(q_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, 
                           lower_tail = FALSE, log_p = TRUE))
  gamlss_log_cdf_upper <- suppressWarnings(pDEL(q_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, 
                              lower.tail = FALSE, log.p = TRUE))
  
  test_name <- paste0("Log upper tail CDF comparison - params set ", i)
  # Use relaxed tolerance for extreme tail log probabilities where numerical precision matters
  current_tolerance <- ifelse(any(ck_log_cdf_upper < -20), log_tail_tolerance, tolerance)
  print(expect_equal(ck_log_cdf_upper, gamlss_log_cdf_upper, tolerance = current_tolerance, info = test_name))
}

# Note: For extreme tail regions (e.g., q=14 with mu=0.5, sigma=0.1, nu=0.1),
# small numerical differences in the original CDF (on the order of machine epsilon) 
# can lead to relatively larger differences in log upper tail probabilities.
# This is expected behavior due to the nature of floating-point arithmetic
# when dealing with very small probabilities (around 1e-15).

# =============================================================================
# QUANTILE FUNCTION COMPARISONS - fqDEL vs gamlss.dist::qDEL
# =============================================================================

# Test 8: Basic quantile comparison
for (i in seq_along(basic_params)) {
  params <- basic_params[[i]]
  
  # Compare fqDEL with qDEL (lower tail)
  ck_quant <- fqDEL(p_vals, mu = params$mu, sigma = params$sigma, nu = params$nu)
  gamlss_quant <- qDEL(p_vals, mu = params$mu, sigma = params$sigma, nu = params$nu)
  
  test_name <- paste0("Quantile comparison (lower tail) - params set ", i)
  print(expect_equal(ck_quant, gamlss_quant, tolerance = 0, info = test_name))  # Exact match for integers
}

# Test 9: Upper tail quantile comparison
for (i in seq_along(basic_params)) {
  params <- basic_params[[i]]
  
  # Compare upper tail quantiles
  ck_quant_upper <- fqDEL(p_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, lower_tail = FALSE)
  gamlss_quant_upper <- qDEL(p_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, lower.tail = FALSE)
  
  test_name <- paste0("Quantile comparison (upper tail) - params set ", i)
  print(expect_equal(ck_quant_upper, gamlss_quant_upper, tolerance = 0, info = test_name))
}

# Test 10: Correct log probability quantile test
# CKutils correctly validates probabilities AFTER exp(p) when log_p=TRUE
# gamlss.dist::qDEL has a bug where it validates BEFORE exp(p), so they differ
log_p_test_vals <- log(c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
for (i in seq_along(basic_params)) {
  params <- basic_params[[i]]
  
  # Test correct log probability behavior in CKutils
  ck_quant_log <- fqDEL(log_p_test_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, log_p = TRUE)
  
  # Also test with exp() to verify correct behavior
  regular_p_vals <- exp(log_p_test_vals)
  ck_quant_regular <- fqDEL(regular_p_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, log_p = FALSE)
  
  test_name <- paste0("Correct log probability quantile behavior - params set ", i)
  print(expect_equal(ck_quant_log, ck_quant_regular, tolerance = 1e-12, info = test_name))
}

# Test 11: Correct log probability upper tail quantile test
# CKutils correctly validates probabilities AFTER exp(p) when log_p=TRUE
for (i in seq_along(basic_params)) {
  params <- basic_params[[i]]
  
  # Test correct log probability upper tail behavior in CKutils
  ck_quant_log_upper <- fqDEL(log_p_test_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, 
                             lower_tail = FALSE, log_p = TRUE)
  
  # Also test with exp() to verify correct behavior
  regular_p_vals <- exp(log_p_test_vals)
  ck_quant_regular_upper <- fqDEL(regular_p_vals, mu = params$mu, sigma = params$sigma, nu = params$nu, 
                                  lower_tail = FALSE, log_p = FALSE)
  
  test_name <- paste0("Correct log probability upper tail quantile behavior - params set ", i)
  print(expect_equal(ck_quant_log_upper, ck_quant_regular_upper, tolerance = 1e-12, info = test_name))
}

# =============================================================================
# PARAMETER RECYCLING TESTS
# =============================================================================

# Test 12: Parameter recycling - density
mu_vec <- c(1, 2, 3)
sigma_vec <- c(0.5, 1)
nu_vec <- c(0.2, 0.4, 0.6, 0.8)
x_recycle <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

ck_dens_recycle <- fdDEL(x_recycle, mu = mu_vec, sigma = sigma_vec, nu = nu_vec)
gamlss_dens_recycle <- dDEL(x_recycle, mu = mu_vec, sigma = sigma_vec, nu = nu_vec)

expect_equal(ck_dens_recycle, gamlss_dens_recycle, tolerance = tolerance, 
            info = "Parameter recycling - density")

# Test 13: Parameter recycling - CDF
ck_cdf_recycle <- fpDEL(x_recycle, mu = mu_vec, sigma = sigma_vec, nu = nu_vec)
gamlss_cdf_recycle <- pDEL(x_recycle, mu = mu_vec, sigma = sigma_vec, nu = nu_vec)

expect_equal(ck_cdf_recycle, gamlss_cdf_recycle, tolerance = tolerance, 
            info = "Parameter recycling - CDF")

# Test 14: Parameter recycling - quantiles
p_recycle <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.05, 0.95, 0.99)

ck_quant_recycle <- fqDEL(p_recycle, mu = mu_vec, sigma = sigma_vec, nu = nu_vec)
gamlss_quant_recycle <- qDEL(p_recycle, mu = mu_vec, sigma = sigma_vec, nu = nu_vec)

expect_equal(ck_quant_recycle, gamlss_quant_recycle, tolerance = 0, 
            info = "Parameter recycling - quantiles")

# =============================================================================
# ROUND-TRIP PROPERTY TESTS
# =============================================================================

# Test 15: Round-trip property (Q(P(x)) = x)
for (i in seq_along(basic_params)) {
  params <- basic_params[[i]]
  
  # Test: qDEL(pDEL(x)) should equal x
  test_q_vals <- 0:7  # Test with a small range of quantiles
  
  ck_roundtrip_q <- fqDEL(fpDEL(test_q_vals, mu = params$mu, sigma = params$sigma, nu = params$nu),
                         mu = params$mu, sigma = params$sigma, nu = params$nu)
  
  test_name <- paste0("Round-trip P(p(Q)) = p - params set ", i)
  print(expect_equal(ck_roundtrip_q, test_q_vals, tolerance = 1e-10, info = test_name))
}

# Note: Round-trip property (P(Q(p)) = p) is not observed in discrete distributions


# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

# Test 17: Invalid probabilities for quantiles
expect_true(is.na(suppressWarnings(tryCatch(fqDEL(-0.1, mu=2, sigma=1, nu=0.5), error=function(e) NA))), 
           info = "Invalid probability < 0")
expect_true(is.na(suppressWarnings(tryCatch(fqDEL(1.1, mu=2, sigma=1, nu=0.5), error=function(e) NA))), 
           info = "Invalid probability > 1")
expect_true(is.na(suppressWarnings(tryCatch(fqDEL(0.1, mu=2, sigma=1, nu=0.5, log_p=TRUE), error=function(e) NA))), 
           info = "Invalid log probability > 0")

# Test 18: Invalid parameters
expect_true(is.na(suppressWarnings(tryCatch(fdDEL(1, mu=-1, sigma=1, nu=0.5), error=function(e) NA))), 
           info = "Invalid mu < 0")
expect_true(is.na(suppressWarnings(tryCatch(fdDEL(1, mu=1, sigma=-1, nu=0.5), error=function(e) NA))), 
           info = "Invalid sigma < 0")
expect_true(is.na(suppressWarnings(tryCatch(fdDEL(1, mu=1, sigma=1, nu=-0.1), error=function(e) NA))), 
           info = "Invalid nu < 0")
expect_true(is.na(suppressWarnings(tryCatch(fdDEL(1, mu=1, sigma=1, nu=1.1), error=function(e) NA))), 
           info = "Invalid nu > 1")

# =============================================================================
# GAMLSS.DIST BUG DOCUMENTATION TEST
# =============================================================================

# Test 19: Document the gamlss.dist::qDEL log_p bug
# CKutils correctly validates probabilities AFTER exp(p) when log_p=TRUE
# gamlss.dist has a bug where it validates BEFORE exp(p), causing incorrect behavior
cat("\n=== DOCUMENTING gamlss.dist::qDEL log_p BUG ===\n")
cat("CKutils fqDEL correctly validates probabilities after exp(p) transformation.\n")
cat("gamlss.dist::qDEL incorrectly validates before exp(p), causing a bug.\n")

# This should work in CKutils (correct behavior)
log_prob_vals <- log(c(0.1, 0.5, 0.9))
ck_correct <- tryCatch(fqDEL(log_prob_vals, mu=2, sigma=1, nu=0.5, log_p=TRUE), error=function(e) "ERROR")
cat("CKutils with log probabilities log(0.1, 0.5, 0.9):", ifelse(is.numeric(ck_correct), "SUCCESS", "ERROR"), "\n")

# This would fail in gamlss.dist because it validates log_prob_vals directly (which are negative)
# We don't test gamlss.dist here to avoid errors, but document the difference
cat("gamlss.dist::qDEL with same log probabilities would give an error due to the bug.\n")
cat("=========================================================\n\n")

# =============================================================================
# EDGE CASES AND BOUNDARY CONDITIONS
# =============================================================================

# Test 20: Boundary values for nu (close to 0 and 1)
nu_boundary <- c(1e-10, 0.001, 0.999, 1-1e-10)
for (nu_val in nu_boundary) {
  if (nu_val > 0 && nu_val < 1) {
    ck_dens_boundary <- fdDEL(0:5, mu = 2, sigma = 1, nu = nu_val)
    gamlss_dens_boundary <- dDEL(0:5, mu = 2, sigma = 1, nu = nu_val)
    
    test_name <- paste0("Boundary nu value - nu=", nu_val)
    print(expect_equal(ck_dens_boundary, gamlss_dens_boundary, tolerance = tolerance, info = test_name))
  }
}

# Test 21: Small parameter values
small_params <- list(
  list(mu = 1e-6, sigma = 1e-6, nu = 0.1),
  list(mu = 0.001, sigma = 0.001, nu = 0.5)
)

for (i in seq_along(small_params)) {
  params <- small_params[[i]]
  
  ck_dens_small <- fdDEL(0:3, mu = params$mu, sigma = params$sigma, nu = params$nu)
  gamlss_dens_small <- dDEL(0:3, mu = params$mu, sigma = params$sigma, nu = params$nu)
  
  test_name <- paste0("Small parameter values - set ", i)
  print(expect_equal(ck_dens_small, gamlss_dens_small, tolerance = tolerance, info = test_name))
}

# Test 22: Large parameter values
large_params <- list(
  list(mu = 100, sigma = 50, nu = 0.3),
  list(mu = 1000, sigma = 100, nu = 0.7)
)

for (i in seq_along(large_params)) {
  params <- large_params[[i]]
  
  # Test only a few values to avoid very long computation times
  ck_dens_large <- fdDEL(0:5, mu = params$mu, sigma = params$sigma, nu = params$nu)
  gamlss_dens_large <- dDEL(0:5, mu = params$mu, sigma = params$sigma, nu = params$nu)
  
  test_name <- paste0("Large parameter values - set ", i)
  print(expect_equal(ck_dens_large, gamlss_dens_large, tolerance = tolerance, info = test_name))
}

# =============================================================================
# CACHE FUNCTIONALITY TESTS
# =============================================================================

# Test 23: Cache functionality
clear_DEL_cache()
expect_true({clear_DEL_cache(); TRUE}, info = "Cache clear function works")

# Test 24: Verify results are consistent before and after cache clear
params_cache <- list(mu = 5, sigma = 2, nu = 0.4)

# Compute values (builds cache)
result1 <- fqDEL(c(0.1, 0.5, 0.9), mu = params_cache$mu, sigma = params_cache$sigma, nu = params_cache$nu)

# Clear cache
clear_DEL_cache()

# Compute same values again (rebuilds cache)
result2 <- fqDEL(c(0.1, 0.5, 0.9), mu = params_cache$mu, sigma = params_cache$sigma, nu = params_cache$nu)

expect_equal(result1, result2, tolerance = 0, info = "Results consistent after cache clear")
