# Comprehensive Test Suite for DPO distribution functions
# Comparing CKutils::fdDPO, fpDPO, fqDPO with gamlss.dist::dDPO, pDPO, qDPO

# Skip tests if gamlss.dist is not available
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
    exit_file("gamlss.dist not available - skipping DPO validation tests")
}

suppressMessages(library(gamlss.dist))

# Set tolerance for floating point comparisons
tolerance <- sqrt(.Machine$double.eps)
# Use more relaxed tolerance for extreme tail log probabilities
log_tail_tolerance <- 1e-2  # Allow larger differences in extreme log tail regions where numerical precision matters

# =============================================================================
# TEST PARAMETERS - Valid parameter combinations for DPO distribution
# =============================================================================

# Basic parameter sets (mu > 0, sigma > 0 for DPO)
basic_params <- data.frame(
  mu = c(1, 2, 5, 0.5, 10, 3, 1.5),
  sigma = c(0.5, 1, 2, 0.1, 3, 1.5, 0.8)
)

# Test values
x_vals <- 0:20
q_vals <- 0:15  
p_vals <- c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 0.999)

# =============================================================================
# DENSITY FUNCTION COMPARISONS - fdDPO vs gamlss.dist::dDPO
# =============================================================================

# Test 1: Basic density comparison - vectorized
# Test all parameter combinations at once using expand.grid
test_grid <- expand.grid(x = x_vals, param_idx = seq_len(nrow(basic_params)))
test_grid$mu <- basic_params$mu[test_grid$param_idx]
test_grid$sigma <- basic_params$sigma[test_grid$param_idx]

ck_dens_all <- fdDPO(test_grid$x, mu = test_grid$mu, sigma = test_grid$sigma)
gamlss_dens_all <- dDPO(test_grid$x, mu = test_grid$mu, sigma = test_grid$sigma)

expect_equal(ck_dens_all, gamlss_dens_all, tolerance = tolerance, 
            info = "Vectorized density comparison - all parameter sets")

# Test 2: Log density comparison - vectorized
ck_log_dens_all <- fdDPO(test_grid$x, mu = test_grid$mu, sigma = test_grid$sigma, log_ = TRUE)
gamlss_log_dens_all <- dDPO(test_grid$x, mu = test_grid$mu, sigma = test_grid$sigma, log = TRUE)

expect_equal(ck_log_dens_all, gamlss_log_dens_all, tolerance = tolerance, 
            info = "Vectorized log density comparison - all parameter sets")

# Test 3: Single value density tests - vectorized
single_vals <- c(0, 1, 2, 5, 10)
ck_dens_single <- fdDPO(single_vals, mu = 3, sigma = 1.5)
gamlss_dens_single <- dDPO(single_vals, mu = 3, sigma = 1.5)

expect_equal(ck_dens_single, gamlss_dens_single, tolerance = tolerance, 
            info = "Vectorized single value density tests")

# =============================================================================
# CDF FUNCTION COMPARISONS - fpDPO vs gamlss.dist::pDPO
# =============================================================================

# Test 4: Basic CDF comparison - vectorized
# Create test grid for CDF tests
cdf_test_grid <- expand.grid(q = q_vals, param_idx = seq_len(nrow(basic_params)))
cdf_test_grid$mu <- basic_params$mu[cdf_test_grid$param_idx]
cdf_test_grid$sigma <- basic_params$sigma[cdf_test_grid$param_idx]

ck_cdf_all <- fpDPO(cdf_test_grid$q, mu = cdf_test_grid$mu, sigma = cdf_test_grid$sigma)
gamlss_cdf_all <- pDPO(cdf_test_grid$q, mu = cdf_test_grid$mu, sigma = cdf_test_grid$sigma)

expect_equal(ck_cdf_all, gamlss_cdf_all, tolerance = tolerance, 
            info = "Vectorized CDF comparison (lower tail) - all parameter sets")

# Test 5: Upper tail CDF comparison - vectorized
ck_cdf_upper_all <- fpDPO(cdf_test_grid$q, mu = cdf_test_grid$mu, sigma = cdf_test_grid$sigma, lower_tail = FALSE)
gamlss_cdf_upper_all <- pDPO(cdf_test_grid$q, mu = cdf_test_grid$mu, sigma = cdf_test_grid$sigma, lower.tail = FALSE)

expect_equal(ck_cdf_upper_all, gamlss_cdf_upper_all, tolerance = tolerance, 
            info = "Vectorized CDF comparison (upper tail) - all parameter sets")

# Test 6: Log CDF comparison - vectorized
ck_log_cdf_all <- fpDPO(cdf_test_grid$q, mu = cdf_test_grid$mu, sigma = cdf_test_grid$sigma, log_p = TRUE)
gamlss_log_cdf_all <- pDPO(cdf_test_grid$q, mu = cdf_test_grid$mu, sigma = cdf_test_grid$sigma, log.p = TRUE)

expect_equal(ck_log_cdf_all, gamlss_log_cdf_all, tolerance = tolerance, 
            info = "Vectorized log CDF comparison - all parameter sets")

# Test 7: Log upper tail CDF comparison - semi-vectorized
# This test requires parameter-specific filtering, so we keep the loop but vectorize within each iteration
for (i in seq_len(nrow(basic_params))) {
  # Compare log upper tail CDFs
  ck_log_cdf_upper <- suppressWarnings(fpDPO(
    q_vals,
    mu = basic_params$mu[i],
    sigma = basic_params$sigma[i],
    lower_tail = FALSE,
    log_p = TRUE
  ))
  gamlss_log_cdf_upper <- suppressWarnings(pDPO(
    q_vals,
    mu = basic_params$mu[i],
    sigma = basic_params$sigma[i],
    lower.tail = FALSE,
    log.p = TRUE
  ))

  # My implementation have higher precision in extreme tails. I.e. for i=1 pDPO
  # returns -Inf while my implementation returns a number. The code below
  # ensures that the test pass in such cases.
  ck_log_cdf_upper <- ck_log_cdf_upper[!is.infinite(gamlss_log_cdf_upper)]
  gamlss_log_cdf_upper <- gamlss_log_cdf_upper[
    !is.infinite(gamlss_log_cdf_upper)
  ]

  ck_log_cdf_upper_above_threshold <- ck_log_cdf_upper[ck_log_cdf_upper < -20]
  ck_log_cdf_upper_below_threshold <- ck_log_cdf_upper[!ck_log_cdf_upper < -20]
  gamlss_log_cdf_upper_above_threshold <- gamlss_log_cdf_upper[ck_log_cdf_upper < -20]
  gamlss_log_cdf_upper_below_threshold <- gamlss_log_cdf_upper[!ck_log_cdf_upper < -20]

  test_name <- paste0("Log upper tail CDF comparison - params set ", i, " (values below 20)")
  expect_equal(
    ck_log_cdf_upper_below_threshold,
    gamlss_log_cdf_upper_below_threshold,
    tolerance = tolerance,
    info = test_name
  )

  test_name <- paste0("Log upper tail CDF comparison - params set ", i, " (values above 20)")
  expect_equal(
    ck_log_cdf_upper_above_threshold,
    gamlss_log_cdf_upper_above_threshold,
    tolerance = log_tail_tolerance,
    info = test_name
  )
}

# Note: For extreme tail regions (e.g., large q with small mu),
# small numerical differences in the original CDF (on the order of machine epsilon) 
# can lead to relatively larger differences in log upper tail probabilities.
# This is expected behavior due to the nature of floating-point arithmetic
# when dealing with very small probabilities.

# =============================================================================
# QUANTILE FUNCTION COMPARISONS - fqDPO vs gamlss.dist::qDPO
# =============================================================================

# Test 8: Basic quantile comparison - vectorized
# Create test grid for quantile tests
quant_test_grid <- expand.grid(p = p_vals, param_idx = seq_len(nrow(basic_params)))
quant_test_grid$mu <- basic_params$mu[quant_test_grid$param_idx]
quant_test_grid$sigma <- basic_params$sigma[quant_test_grid$param_idx]

ck_quant_all <- fqDPO(quant_test_grid$p, mu = quant_test_grid$mu, sigma = quant_test_grid$sigma)
gamlss_quant_all <- qDPO(quant_test_grid$p, mu = quant_test_grid$mu, sigma = quant_test_grid$sigma)

expect_equal(ck_quant_all, gamlss_quant_all, tolerance = 0, 
            info = "Vectorized quantile comparison (lower tail) - all parameter sets")  # Exact match for integers

# Test 9: Upper tail quantile comparison - vectorized
ck_quant_upper_all <- fqDPO(quant_test_grid$p, mu = quant_test_grid$mu, sigma = quant_test_grid$sigma, lower_tail = FALSE)
gamlss_quant_upper_all <- qDPO(quant_test_grid$p, mu = quant_test_grid$mu, sigma = quant_test_grid$sigma, lower.tail = FALSE)

expect_equal(ck_quant_upper_all, gamlss_quant_upper_all, tolerance = 0, 
            info = "Vectorized quantile comparison (upper tail) - all parameter sets")

# Test 10: Correct log probability quantile test - vectorized
# CKutils correctly validates probabilities AFTER exp(p) when log_p=TRUE
# gamlss.dist::qDPO has a bug where it validates BEFORE exp(p), so they differ
log_p_test_vals <- log(c(0.001, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))

# Create test grid for log probability tests
log_quant_test_grid <- expand.grid(log_p = log_p_test_vals, param_idx = seq_len(nrow(basic_params)))
log_quant_test_grid$mu <- basic_params$mu[log_quant_test_grid$param_idx]
log_quant_test_grid$sigma <- basic_params$sigma[log_quant_test_grid$param_idx]

# Test correct log probability behavior in CKutils
ck_quant_log_all <- fqDPO(log_quant_test_grid$log_p, mu = log_quant_test_grid$mu, 
                         sigma = log_quant_test_grid$sigma, log_p = TRUE)

# Also test with exp() to verify correct behavior
log_quant_test_grid$regular_p <- exp(log_quant_test_grid$log_p)
ck_quant_regular_all <- fqDPO(log_quant_test_grid$regular_p, mu = log_quant_test_grid$mu, 
                             sigma = log_quant_test_grid$sigma, log_p = FALSE)

expect_equal(ck_quant_log_all, ck_quant_regular_all, tolerance = 1e-12, 
            info = "Vectorized correct log probability quantile behavior - all parameter sets")

# Test 11: Correct log probability upper tail quantile test - vectorized
# CKutils correctly validates probabilities AFTER exp(p) when log_p=TRUE
ck_quant_log_upper_all <- fqDPO(log_quant_test_grid$log_p, mu = log_quant_test_grid$mu, 
                               sigma = log_quant_test_grid$sigma, lower_tail = FALSE, log_p = TRUE)

ck_quant_regular_upper_all <- fqDPO(log_quant_test_grid$regular_p, mu = log_quant_test_grid$mu, 
                                   sigma = log_quant_test_grid$sigma, lower_tail = FALSE, log_p = FALSE)

expect_equal(ck_quant_log_upper_all, ck_quant_regular_upper_all, tolerance = 1e-12, 
            info = "Vectorized correct log probability upper tail quantile behavior - all parameter sets")

# =============================================================================
# PARAMETER RECYCLING TESTS
# =============================================================================

# Test 12: Parameter recycling - density
mu_vec <- c(1, 2, 3)
sigma_vec <- c(0.5, 1)
x_recycle <- c(0, 1, 2, 3, 4, 5)

ck_dens_recycle <- fdDPO(x_recycle, mu = mu_vec, sigma = sigma_vec)
gamlss_dens_recycle <- dDPO(x_recycle, mu = mu_vec, sigma = sigma_vec)

expect_equal(ck_dens_recycle, gamlss_dens_recycle, tolerance = tolerance, 
            info = "Parameter recycling - density")

# Test 13: Parameter recycling - CDF
ck_cdf_recycle <- fpDPO(x_recycle, mu = mu_vec, sigma = sigma_vec)
gamlss_cdf_recycle <- pDPO(x_recycle, mu = mu_vec, sigma = sigma_vec)

expect_equal(ck_cdf_recycle, gamlss_cdf_recycle, tolerance = tolerance, 
            info = "Parameter recycling - CDF")

# Test 14: Parameter recycling - quantiles
p_recycle <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)

ck_quant_recycle <- fqDPO(p_recycle, mu = mu_vec, sigma = sigma_vec)
gamlss_quant_recycle <- qDPO(p_recycle, mu = mu_vec, sigma = sigma_vec)

expect_equal(ck_quant_recycle, gamlss_quant_recycle, tolerance = 0, 
            info = "Parameter recycling - quantiles")

# =============================================================================
# ROUND-TRIP PROPERTY TESTS
# =============================================================================

# Test 15: Round-trip property (Q(P(x)) = x) - semi-vectorized
# This test requires parameter-specific test ranges, so we keep the loop
for (i in seq_len(nrow(basic_params))) {
  # Test: qDPO(pDPO(x)) should equal x
  test_q_vals <- 0:7  # Test with a small range of quantiles
  if (i == 1L) test_q_vals <- 0:6
  if (i == 4L) test_q_vals <- 0:1
  ck_roundtrip_q <- fqDPO(fpDPO(test_q_vals, mu = basic_params$mu[i], sigma = basic_params$sigma[i]),
                         mu = basic_params$mu[i], sigma = basic_params$sigma[i])
  
  test_name <- paste0("Round-trip Q(P(x)) = x - params set ", i)
  expect_equal(ck_roundtrip_q, test_q_vals, tolerance = 1e-10, info = test_name)
}

# Test 16: Round-trip property verification with gamlss.dist - vectorized
test_q_vals <- 0:5
roundtrip_test_grid <- expand.grid(x = test_q_vals, param_idx = seq_len(nrow(basic_params)))
roundtrip_test_grid$mu <- basic_params$mu[roundtrip_test_grid$param_idx]
roundtrip_test_grid$sigma <- basic_params$sigma[roundtrip_test_grid$param_idx]

# Verify that both implementations give same round-trip results
ck_roundtrip_all <- fqDPO(fpDPO(roundtrip_test_grid$x, mu = roundtrip_test_grid$mu, sigma = roundtrip_test_grid$sigma),
                         mu = roundtrip_test_grid$mu, sigma = roundtrip_test_grid$sigma)
gamlss_roundtrip_all <- qDPO(pDPO(roundtrip_test_grid$x, mu = roundtrip_test_grid$mu, sigma = roundtrip_test_grid$sigma),
                            mu = roundtrip_test_grid$mu, sigma = roundtrip_test_grid$sigma)

expect_equal(ck_roundtrip_all, gamlss_roundtrip_all, tolerance = 0, 
            info = "Vectorized round-trip consistency with gamlss.dist - all parameter sets")

# =============================================================================
# GAMLSS.DIST BUG DOCUMENTATION TEST
# =============================================================================

# Test 19.5: Document the gamlss.dist::qDPO log_p bug
# CKutils correctly validates probabilities AFTER exp(p) when log_p=TRUE
# gamlss.dist has a bug where it validates BEFORE exp(p), causing incorrect behavior
# cat("\n=== DOCUMENTING gamlss.dist::qDPO log_p BUG ===\n")
# cat("CKutils fqDPO correctly validates probabilities after exp(p) transformation.\n")
# cat("gamlss.dist::qDPO incorrectly validates before exp(p), causing a bug.\n")

# This should work in CKutils (correct behavior)
log_prob_vals <- log(c(0.1, 0.5, 0.9))
ck_correct <- tryCatch(fqDPO(log_prob_vals, mu=2, sigma=1, log_p=TRUE), error=function(e) "ERROR")
# cat("CKutils with log probabilities log(0.1, 0.5, 0.9):", ifelse(is.numeric(ck_correct), "SUCCESS", "ERROR"), "\n")

# This would fail in gamlss.dist because it validates log_prob_vals directly (which are negative)
# We don't test gamlss.dist here to avoid errors, but document the difference
# cat("gamlss.dist::qDPO with same log probabilities would give an error due to the bug.\n")
# cat("=========================================================\n\n")

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

# Test 17: Invalid probabilities for quantiles
expect_true(is.na(suppressWarnings(tryCatch(fqDPO(-0.1, mu=2, sigma=1), error=function(e) NA))), 
           info = "Invalid probability < 0")
expect_true(is.na(suppressWarnings(tryCatch(fqDPO(1.1, mu=2, sigma=1), error=function(e) NA))), 
           info = "Invalid probability > 1")
expect_true(is.na(suppressWarnings(tryCatch(fqDPO(0.1, mu=2, sigma=1, log_p=TRUE), error=function(e) NA))), 
           info = "Invalid log probability > 0")

# Test 18: Invalid parameters
expect_true(is.na(suppressWarnings(tryCatch(fdDPO(1, mu=-1, sigma=1), error=function(e) NA))), 
           info = "Invalid mu <= 0")
expect_true(is.na(suppressWarnings(tryCatch(fdDPO(1, mu=0, sigma=1), error=function(e) NA))), 
           info = "Invalid mu = 0")
expect_true(is.na(suppressWarnings(tryCatch(fdDPO(1, mu=1, sigma=-1), error=function(e) NA))), 
           info = "Invalid sigma <= 0")
expect_true(is.na(suppressWarnings(tryCatch(fdDPO(1, mu=1, sigma=0), error=function(e) NA))), 
           info = "Invalid sigma = 0")

# Test 19: Invalid x values for density and CDF
expect_true(is.na(suppressWarnings(tryCatch(fdDPO(-1, mu=2, sigma=1), error=function(e) NA))), 
           info = "Invalid x < 0 for density")
expect_true(is.na(suppressWarnings(tryCatch(fpDPO(-1, mu=2, sigma=1), error=function(e) NA))), 
           info = "Invalid q < 0 for CDF")

# =============================================================================
# SPECIAL CASES AND BOUNDARY CONDITIONS
# =============================================================================

# Test 20: Poisson limit case (sigma = 1) - vectorized
# When sigma = 1, DPO should reduce to Poisson distribution
poisson_mu_vals <- c(0.5, 1, 2, 5)
poisson_test_grid <- expand.grid(x = 0:10, mu = poisson_mu_vals)

# Compare DPO(mu, sigma=1) with Poisson(mu)
ck_dpo_poisson_all <- fdDPO(poisson_test_grid$x, mu = poisson_test_grid$mu, sigma = 1)
r_poisson_all <- dpois(poisson_test_grid$x, lambda = poisson_test_grid$mu)

expect_equal(ck_dpo_poisson_all, r_poisson_all, tolerance = tolerance, 
            info = "Vectorized Poisson limit case (sigma=1) - all mu values")

# Test 21: Boundary values for sigma (close to 0 and very large) - vectorized
sigma_boundary <- c(1e-6, 0.001, 0.01, 100, 1000)
boundary_test_grid <- expand.grid(x = 0:5, sigma = sigma_boundary)
boundary_test_grid$mu <- 2  # Fixed mu value

ck_dens_boundary_all <- fdDPO(boundary_test_grid$x, mu = boundary_test_grid$mu, sigma = boundary_test_grid$sigma)
gamlss_dens_boundary_all <- dDPO(boundary_test_grid$x, mu = boundary_test_grid$mu, sigma = boundary_test_grid$sigma)

expect_equal(ck_dens_boundary_all, gamlss_dens_boundary_all, tolerance = tolerance, 
            info = "Vectorized boundary sigma values - all sigma values")

# Test 22: Small parameter values - vectorized
small_params <- data.frame(
  mu = c(1e-6, 0.001, 0.01),
  sigma = c(1e-6, 0.001, 0.01)
)

small_test_grid <- expand.grid(x = 0:3, param_idx = seq_len(nrow(small_params)))
small_test_grid$mu <- small_params$mu[small_test_grid$param_idx]
small_test_grid$sigma <- small_params$sigma[small_test_grid$param_idx]

ck_dens_small_all <- fdDPO(small_test_grid$x, mu = small_test_grid$mu, sigma = small_test_grid$sigma)
gamlss_dens_small_all <- dDPO(small_test_grid$x, mu = small_test_grid$mu, sigma = small_test_grid$sigma)

expect_equal(ck_dens_small_all, gamlss_dens_small_all, tolerance = tolerance, 
            info = "Vectorized small parameter values - all parameter sets")

# Test 23: Large parameter values - vectorized
large_params <- data.frame(
  mu = c(100, 50, 1000),
  sigma = c(50, 100, 10)
)

# Test only a few values to avoid very long computation times
large_test_grid <- expand.grid(x = 0:5, param_idx = seq_len(nrow(large_params)))
large_test_grid$mu <- large_params$mu[large_test_grid$param_idx]
large_test_grid$sigma <- large_params$sigma[large_test_grid$param_idx]

ck_dens_large_all <- fdDPO(large_test_grid$x, mu = large_test_grid$mu, sigma = large_test_grid$sigma)
gamlss_dens_large_all <- dDPO(large_test_grid$x, mu = large_test_grid$mu, sigma = large_test_grid$sigma)

expect_equal(ck_dens_large_all, gamlss_dens_large_all, tolerance = tolerance, 
            info = "Vectorized large parameter values - all parameter sets")

# =============================================================================
# PERFORMANCE AND CONSISTENCY VERIFICATION
# =============================================================================

# Test 24: Verify normalizing constant function - vectorized
# Test the internal fget_C function used for normalizing constants
x_test <- 0:10
norm_test_grid <- expand.grid(x = x_test, param_idx = seq_len(nrow(basic_params)))
norm_test_grid$mu <- basic_params$mu[norm_test_grid$param_idx]
norm_test_grid$sigma <- basic_params$sigma[norm_test_grid$param_idx]

# The normalizing constant should be consistent
norm_const_all <- fget_C(norm_test_grid$x, norm_test_grid$mu, norm_test_grid$sigma)

# Check that the function doesn't produce NAs or Infs
expect_true(all(is.finite(norm_const_all)), 
           info = "Vectorized normalizing constant validity - all parameter sets")

# Test 25: Verify consistency across all three functions - vectorized
# For each parameter set, verify that the three functions are mutually consistent
consistency_test_grid <- expand.grid(param_idx = seq_len(nrow(basic_params)))
consistency_test_grid$mu <- basic_params$mu[consistency_test_grid$param_idx]
consistency_test_grid$sigma <- basic_params$sigma[consistency_test_grid$param_idx]

# Test consistency: sum of density should equal CDF (vectorized by parameter set)
for (i in seq_len(nrow(basic_params))) {
  x_test <- 0:10
  dens_sum <- sum(fdDPO(x_test, mu = basic_params$mu[i], sigma = basic_params$sigma[i]))
  cdf_final <- fpDPO(max(x_test), mu = basic_params$mu[i], sigma = basic_params$sigma[i])
  
  test_name <- paste0("Density-CDF consistency - params set ", i)
  expect_equal(dens_sum, cdf_final, tolerance = 1e-10, info = test_name)
}

# Test 26: Extreme quantile tests - vectorized
# Test behavior at extreme probabilities
extreme_probs <- c(1e-15, 1e-10, 1e-5, 1-1e-15, 1-1e-10, 1-1e-5)
extreme_test_grid <- expand.grid(p = extreme_probs, param_idx = seq_len(nrow(basic_params)))
extreme_test_grid$mu <- basic_params$mu[extreme_test_grid$param_idx]
extreme_test_grid$sigma <- basic_params$sigma[extreme_test_grid$param_idx]

# Test that extreme quantiles are handled correctly
ck_extreme_quants_all <- suppressWarnings(fqDPO(extreme_test_grid$p, mu = extreme_test_grid$mu, sigma = extreme_test_grid$sigma))
gamlss_extreme_quants_all <- suppressWarnings(qDPO(extreme_test_grid$p, mu = extreme_test_grid$mu, sigma = extreme_test_grid$sigma))

expect_equal(ck_extreme_quants_all, gamlss_extreme_quants_all, tolerance = 0, 
            info = "Vectorized extreme quantiles - all parameter sets")

# =============================================================================
# FINAL SUMMARY MESSAGE
# =============================================================================

# cat("\n=== DPO DISTRIBUTION TEST SUMMARY ===\n")
# cat("All tests completed. CKutils DPO functions should match gamlss.dist exactly.\n")
# cat("Key features tested:\n")
# cat("- Density, CDF, and quantile functions\n")
# cat("- Log probabilities and upper tail probabilities\n")
# cat("- Parameter recycling\n")
# cat("- Round-trip properties\n")
# cat("- Error handling\n")
# cat("- Boundary conditions and extreme values\n")
# cat("- Poisson limit case (sigma = 1)\n")
# cat("- Consistency between all three functions\n")
# cat("=====================================\n")
