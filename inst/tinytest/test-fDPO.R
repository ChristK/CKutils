# Comprehensive Test Suite for DPO distribution functions
# Comparing CKutils::fdDPO, fpDPO, fqDPO with gamlss.dist::dDPO, pDPO, qDPO

# Skip tests if gamlss.dist is not available
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
    exit_file("gamlss.dist not available - skipping DPO validation tests")
}

suppressMessages(library(gamlss.dist))

# Set tolerance for floating point comparisons
tolerance <- sqrt(.Machine$double.eps)

# gamlss.dist 6.1-11 (CRAN 2026-09-10) evaluates d{FAM}() WRONGLY when the
# parameters vary along the vectors (NAs or wrong values -- warnings such as
# "number of items to replace is not a multiple of replacement length" from its
# own code). Its constant-parameter path is right (checked against the
# normalisation and element-by-element calls), so where a test grid mixes
# parameter sets the reference is evaluated one parameter set at a time.
ref_per_set <- function(fun, x, set, ..., extra = list()) {
  pars <- list(...)
  out <- numeric(length(x))
  for (s in unique(set)) {
    i <- which(set == s)
    out[i] <- do.call(fun, c(list(x[i]), lapply(pars, function(p) p[i][1L]), extra))
  }
  out
}
# An implementation-independent DPO density: the normalising constant from the
# unnormalised terms summed over y = 0..ymax in log space (log-sum-exp).
# gamlss.dist's dDPO() stops that sum at max(3 * x, 500), so for a large sigma
# or a large mu it misses mass, and its densities are off: by 2.4% at mu = 2,
# sigma = 1000, by a factor of ~5e7 at mu = 1000, sigma = 10, x = 0.
dDPO_lse <- function(x, mu, sigma, ymax = 20000) {
  lterm <- function(y) {
    ylogy <- ifelse(y == 0, 0, y * log(y))
    -0.5 * log(sigma) - mu / sigma - lgamma(y + 1) + ylogy - y +
      (y * log(mu)) / sigma + y / sigma - ylogy / sigma
  }
  lt <- lterm(0:ymax)
  m <- max(lt)
  exp(lterm(x) - (m + log(sum(exp(lt - m)))))
}
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
gamlss_dens_all <- ref_per_set(dDPO, test_grid$x, test_grid$param_idx, mu = test_grid$mu, sigma = test_grid$sigma)

expect_equal(ck_dens_all, gamlss_dens_all, tolerance = tolerance, 
            info = "Vectorized density comparison - all parameter sets")

# Test 2: Log density comparison - vectorized
ck_log_dens_all <- fdDPO(test_grid$x, mu = test_grid$mu, sigma = test_grid$sigma, log_ = TRUE)
gamlss_log_dens_all <- ref_per_set(dDPO, test_grid$x, test_grid$param_idx, mu = test_grid$mu, sigma = test_grid$sigma,
                                   extra = list(log = TRUE))

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

# gamlss.dist's qDPO() returns Inf for every p + 1e-9 >= 1 although the quantile is finite, so
# its round trip is not a reference there: the property itself is checked, and gamlss.dist is
# compared where it returns a finite value.
# (set 4, mu = 0.5, sigma = 0.1: F(3) == F(4) == F(5) in double precision, the pmf there is below 1e-16, so only
# x <= 3 can round-trip; the loop test further up uses 0:1 for that set)
rt_ok <- !(roundtrip_test_grid$param_idx == 4L & roundtrip_test_grid$x > 3L)
expect_equal(ck_roundtrip_all[rt_ok], as.numeric(roundtrip_test_grid$x)[rt_ok], tolerance = 0,
            info = "Vectorized round-trip q(p(x)) == x - all parameter sets")
expect_equal(ck_roundtrip_all[is.finite(gamlss_roundtrip_all)],
             gamlss_roundtrip_all[is.finite(gamlss_roundtrip_all)], tolerance = 0,
            info = "Vectorized round-trip consistency with gamlss.dist where it is finite")

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
# reference: dDPO_lse(), as gamlss.dist truncates the normalising sum at sigma = 100, 1000
gamlss_dens_boundary_all <- ref_per_set(dDPO_lse, boundary_test_grid$x, boundary_test_grid$sigma,
                                        mu = boundary_test_grid$mu, sigma = boundary_test_grid$sigma)

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
gamlss_dens_small_all <- ref_per_set(dDPO, small_test_grid$x, small_test_grid$param_idx,
                                     mu = small_test_grid$mu, sigma = small_test_grid$sigma)

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
# reference: dDPO_lse(), as gamlss.dist truncates the normalising sum for all three sets
gamlss_dens_large_all <- ref_per_set(dDPO_lse, large_test_grid$x, large_test_grid$param_idx,
                                     mu = large_test_grid$mu, sigma = large_test_grid$sigma)

expect_equal(ck_dens_large_all, gamlss_dens_large_all, tolerance = tolerance, 
            info = "Vectorized large parameter values - all parameter sets")

# Test 23b: The normalising constant covers the mass at a large mu (0.1.34): the
# sum stopped at max(3x, 500), so P(0) at mu = 5000 came out Inf, and with it
# fpDPO/fqDPO for any q (the right value underflows to 0)
expect_identical(fpDPO(0L, mu = 5000, sigma = 2), 0,
                 info = "fpDPO: P(0) at mu = 5000 underflows to 0 (was Inf)")
expect_equal(sum(fdDPO(0:7000, mu = 5000, sigma = 2)), 1, tolerance = 1e-9,
             info = "fdDPO: densities at mu = 5000 sum to 1")
expect_equal(fdDPO(c(0, 5, 1000), mu = 1000, sigma = 10, log_ = TRUE),
             log(dDPO_lse(c(0, 5, 1000), mu = 1000, sigma = 10)),
             tolerance = 1e-10,
             info = "fdDPO: left tail at mu = 1000, sigma = 10 (was ~5e7 times too large)")

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

# gamlss.dist returns Inf for p + 1e-9 >= 1 (a rule copied into CKutils before 0.1.34, which made
# every quantile in [1 - 1e-9, 1) Inf although it is finite): compare it where it is finite,
# and check the rest against the exact quantile from the normalised density (log-sum-exp).
expect_equal(ck_extreme_quants_all[is.finite(gamlss_extreme_quants_all)],
             gamlss_extreme_quants_all[is.finite(gamlss_extreme_quants_all)], tolerance = 0,
            info = "Vectorized extreme quantiles - all parameter sets (gamlss.dist finite)")
q_exact <- function(p, mu, sigma, ymax = 3000) which(cumsum(dDPO_lse(0:ymax, mu, sigma, ymax)) >= p)[1] - 1
tail_rows <- which(extreme_test_grid$p == 1 - 1e-10)
expect_equal(ck_extreme_quants_all[tail_rows],
             vapply(tail_rows, function(r) q_exact(extreme_test_grid$p[r], extreme_test_grid$mu[r], extreme_test_grid$sigma[r]), 0),
             tolerance = 0, info = "Quantiles at p = 1 - 1e-10 are finite and exact")
expect_true(all(is.finite(ck_extreme_quants_all[extreme_test_grid$p == 1 - 1e-15])),
            info = "Quantiles at p = 1 - 1e-15 are finite")

# =============================================================================
# NON-FINITE PARAMETERS AND THE int RANGE
# =============================================================================

# Test 27: an infinite mu or sigma has no normalising constant: NaN (NA for
# fqDPO), with the usual warning, at once. The constant's loop used to run its
# 2^31 iterations first, ~33 s for each constant: fdDPO and fpDPO(0, ...) ~33 s,
# fqDPO ~100 s (its search asks for three densities). The calls now take
# microseconds, so the 1 s bound leaves a slow machine room to spare.
elapsed <- system.time(v <- suppressWarnings(fdDPO(1, Inf, 2)))[["elapsed"]]
expect_true(is.na(v) && elapsed < 1,
            info = "fdDPO: mu = Inf gives NaN at once (was ~33 s)")
elapsed <- system.time(v <- suppressWarnings(fdDPO(1, 2, Inf)))[["elapsed"]]
expect_true(is.na(v) && elapsed < 1,
            info = "fdDPO: sigma = Inf gives NaN at once (was ~33 s)")
elapsed <- system.time(v <- suppressWarnings(fpDPO(0L, Inf, 2)))[["elapsed"]]
expect_true(is.nan(v) && elapsed < 1,
            info = "fpDPO: mu = Inf gives NaN at once (was ~33 s)")
elapsed <- system.time(v <- suppressWarnings(fqDPO(0.5, Inf, 2)))[["elapsed"]]
expect_true(is.na(v) && !is.nan(v) && elapsed < 1,
            info = "fqDPO: mu = Inf gives NA at once (was ~100 s)")

# Test 27b: fget_C() takes the largest x as a double, skipping NA, and clamps the
# range it derives from it: 3 * .Machine$integer.max overflows an int. This is a
# guard only: on x86-64 the overflow wraps to a harmless value, so it cannot fail
# there without a sanitizer (UBSan). The constant does not depend on x.
expect_identical(fget_C(c(1L, NA, .Machine$integer.max), 5, 2),
                 rep(fget_C(1L, 5, 2), 3),
                 info = "fget_C: an NA and the largest int in x do not overflow")

# =============================================================================
# THE CDF STOPS WHEN ITS SUM HAS SETTLED; THE UPPER TAIL IS SUMMED
# =============================================================================

# Test 28: fpDPO() added the densities from 0 to q however large q was -- 0.5 s at
# q = 1e7, for a sum that is complete by q = 220 at mu = 90, sigma = 2 -- and took
# the upper tail as 1 - F, which is 0, negative or wrong by orders of magnitude once
# F has rounded to 1. It now stops where the sum has settled (past the largest term,
# at a term too small to change it: the same double as the full sum), starts at the
# first density that does not underflow, and sums the upper tail from q + 1.

# P(X > q), or with lower = TRUE P(X <= q), from the log-sum-exp density. The upper
# tail is summed from its small end, so that it keeps its relative accuracy.
dDPO_lse_tail <- function(q, mu, sigma, lower = FALSE, ymax = 5000) {
  d <- dDPO_lse(0:ymax, mu, sigma, ymax)
  if (lower) cumsum(d)[q + 1] else rev(cumsum(rev(d)))[q + 2]
}

# 28a: a q far beyond the mass gives the settled sum, to the bit
expect_identical(fpDPO(1e7, 90, 2), fpDPO(1000, 90, 2),
                 info = "fpDPO: q = 1e7 gives the same double as q = 1000 (the sum has settled)")
if (at_home()) {
  elapsed <- system.time(fpDPO(1e7, 90, 2))[["elapsed"]]
  expect_true(elapsed < 0.1,
              info = "fpDPO: q = 1e7 stops where the sum settles (was O(q): 0.5 s)")
}

# 28b: the upper tail is summed, not 1 - F. Before: -6.7e-16 and 4.4e-16 for tails of
# 8.7e-46 and 2.0e-23, and NaN with log_p; 4.4e-16 at q = 1e7, where the tail underflows to 0.
# The references are the log-sum-exp tails (dDPO_lse_tail): P(X > 80 | mu = 10, sigma = 2) =
# 1.993625e-23 and P(X > 90 | mu = 1.041833, sigma = 3.157198) = 8.654893e-46. Values this
# small are compared as ratios to the reference (target 1) and on the log scale:
# expect_equal() (all.equal) compares ABSOLUTELY when the target is below the tolerance,
# and 4.4e-16 "equals" 2.0e-23 to 1e-6.
ref_a <- dDPO_lse_tail(90, 1.041833, 3.157198)
ref_b <- dDPO_lse_tail(80, 10, 2)
expect_true(fpDPO(90, 1.041833, 3.157198, lower_tail = FALSE) > 0,
            info = "fpDPO: the upper tail at q = 90, mu = 1.041833, sigma = 3.157198 is positive (was -6.7e-16)")
expect_equal(fpDPO(90, 1.041833, 3.157198, lower_tail = FALSE) / ref_a, 1, tolerance = 1e-6,
             info = "fpDPO: that upper tail is 8.7e-46 (ratio to the log-sum-exp tail)")
expect_equal(fpDPO(90, 1.041833, 3.157198, lower_tail = FALSE, log_p = TRUE), log(ref_a), tolerance = 1e-10,
             info = "fpDPO: log_p of that upper tail is finite (was NaN)")
expect_equal(fpDPO(80, 10, 2, lower_tail = FALSE) / ref_b, 1, tolerance = 1e-6,
             info = "fpDPO: the upper tail at q = 80, mu = 10, sigma = 2 is 2.0e-23 (ratio to the log-sum-exp tail; was 4.4e-16)")
expect_equal(fpDPO(80, 10, 2, lower_tail = FALSE, log_p = TRUE), log(ref_b), tolerance = 1e-10,
             info = "fpDPO: log_p is the log of the summed upper tail")
expect_identical(fpDPO(1e7, 90, 2, lower_tail = FALSE), 0,
                 info = "fpDPO: the upper tail far out is 0 (was 4.4e-16)")
expect_identical(fpDPO(1e7, 90, 2, lower_tail = FALSE, log_p = TRUE), -Inf,
                 info = "fpDPO: its log is -Inf")

# 28c: over a grid of (mu, sigma, q) -- bimodal parameter sets included (sigma large
# against mu, as 16.27 and 7.53, or 57 and 20) -- the upper tail (on the log scale,
# where it does not underflow) and the lower tail match the log-sum-exp reference,
# and the two add up to 1
tail_grid <- expand.grid(q = c(0L, 3L, 10L, 30L, 60L, 90L, 200L),
                         mu = c(0.5, 2, 16.27, 57), sigma = c(1.15, 3, 7.53, 20, 126))
tail_set <- as.integer(interaction(tail_grid$mu, tail_grid$sigma, drop = TRUE))
tail_upper_ref <- ref_per_set(dDPO_lse_tail, tail_grid$q, tail_set,
                              mu = tail_grid$mu, sigma = tail_grid$sigma)
tail_lower_ref <- ref_per_set(dDPO_lse_tail, tail_grid$q, tail_set,
                              mu = tail_grid$mu, sigma = tail_grid$sigma, extra = list(lower = TRUE))
tail_upper <- fpDPO(tail_grid$q, tail_grid$mu, tail_grid$sigma, lower_tail = FALSE)
tail_lower <- fpDPO(tail_grid$q, tail_grid$mu, tail_grid$sigma)
keep_up <- tail_upper_ref > 1e-290
expect_equal(log(tail_upper[keep_up]), log(tail_upper_ref[keep_up]), tolerance = 1e-10,
             info = "fpDPO: upper tails (log scale) over a grid of parameters")
expect_equal(tail_lower, tail_lower_ref, tolerance = 1e-13,
             info = "fpDPO: lower tails over a grid of parameters")
expect_equal(tail_lower + tail_upper, rep(1, nrow(tail_grid)), tolerance = 1e-13,
             info = "fpDPO: the two tails add up to 1")

# 28d: guard: at model-like parameters the sum is the one it has always been. The values
# at q = 0 and 90 are what the IMPACTncd models use (bit-identical to the full sum).
expect_equal(fpDPO(0:200, 16.27, 7.53), cumsum(dDPO_lse(0:200, 16.27, 7.53)), tolerance = 1e-13,
             info = "fpDPO: 0:200 at mu = 16.27, sigma = 7.53 matches the log-sum-exp sums")
expect_equal(fpDPO(c(0, 90), 16.27, 7.53), cumsum(dDPO_lse(0:90, 16.27, 7.53))[c(1, 91)], tolerance = 1e-13,
             info = "fpDPO: q = 0 and 90 at mu = 16.27, sigma = 7.53")
expect_equal(fpDPO(0:400, 57, 20), cumsum(dDPO_lse(0:400, 57, 20)), tolerance = 1e-13,
             info = "fpDPO: 0:400 at mu = 57, sigma = 20 (a second mode at 0) matches the log-sum-exp sums")

# 28e: at a large mu the sum starts at the first density that does not underflow (q > 64):
# 0 below it, and the value of the sum from 0 above it
expect_identical(fpDPO(100L, 5000, 2), 0,
                 info = "fpDPO: q below the first non-zero density is exactly 0")
expect_equal(fpDPO(5100L, 5000, 2), sum(fdDPO(0:5100, 5000, 2)), tolerance = 1e-12,
             info = "fpDPO: mu = 5000, q = 5100 is the sum of the densities from 0")
expect_equal(fpDPO(5100L, 5000, 2, lower_tail = FALSE), sum(fdDPO(5101:8000, 5000, 2)), tolerance = 1e-10,
             info = "fpDPO: mu = 5000, q = 5100, upper tail")

# 28f: a term of 0 is not a settled sum. At mu / sigma near 743, p(0) is at the underflow limit
# (4.9e-324) and the pmf has a second mode there: the dip after it underflows to exactly 0 (for
# mu = 29720, sigma = 40: 4.9e-324, six zeros, then tiny terms that rise to the main mass around
# 29720), and a sum that stopped at the first zero returned 4.9e-324 for every q. The values are
# those of the log-sum-exp density. (Whether the dip underflows depends on the platform's exp()
# and lgamma() to an ulp of the exponent; the values are right either way.)
dip_q <- c(20000L, 29720L, 33000L, 40000L)
dip_lower_ref <- dDPO_lse_tail(dip_q, 29720, 40, lower = TRUE, ymax = 60000)
dip_upper_ref <- dDPO_lse_tail(dip_q, 29720, 40, ymax = 60000)
expect_equal(fpDPO(dip_q, 29720, 40) / dip_lower_ref, rep(1, length(dip_q)), tolerance = 1e-10,
             info = "fpDPO: p(0) at the underflow limit, a dip of exact zeros after it (ratio to the log-sum-exp CDF)")
expect_equal(log(fpDPO(dip_q, 29720, 40, lower_tail = FALSE)), log(dip_upper_ref), tolerance = 1e-9,
             info = "fpDPO: the same, upper tail (log scale)")

# =============================================================================
# THE QUANTILE SEARCH DOES NOT READ AN UNDERFLOWED ZERO AS A STALL
# =============================================================================

# Test 29: fqDPO() adds the densities from 0 until the sum reaches p, and gives up (NA) at a
# falling term that cannot change the sum. An exact 0 cannot change any sum, but for a sigma
# large against mu the pmf is not unimodal: it has a second, shallow mode at 0, and where
# mu / sigma is near 743 p(0) sits at the underflow limit (4.9e-324) with the dip after it
# exactly 0, ahead of the main mass near mu. The search read that zero as a stall and
# answered NA for quantiles near mu: fqDPO(c(.1, .5, .9), 17934.73, 24.14) was NA NA NA. A sum
# still below DBL_MIN has not settled, so the search walks on through the zeros (Test 28f is
# the same trap in fpDPO()). The references are the quantiles of the log-sum-exp density
# (q_exact, Test 26): p - F(q - 1) and F(q) - p are at least 4e-6 in every case, far above the
# 1e-15 accuracy of the sums. (Whether a dip underflows depends on the platform's exp() and
# lgamma() to an ulp of the exponent; the answers are right either way.)
p29 <- c(0.1, 0.5, 0.9)
band29 <- list(list(mu = 17934.734753921883, sigma = 24.138224901457072, ymax = 40000),  # one zero after p(0)
               list(mu = 74250, sigma = 100, ymax = 130000))                             # eleven or more
for (b in band29) {
  lab29 <- sprintf("mu = %.10g, sigma = %.10g", b$mu, b$sigma)
  q29 <- fqDPO(p29, b$mu, b$sigma)
  expect_true(all(is.finite(q29)),
              info = paste("fqDPO: p(0) at the underflow limit, a dip of exact zeros after it: finite quantiles (was NA),", lab29))
  expect_true(all(fpDPO(q29 - 1, b$mu, b$sigma) < p29 & p29 <= fpDPO(q29, b$mu, b$sigma)),
              info = paste("fqDPO: the quantile of the package's own CDF, F(q - 1) < p <= F(q),", lab29))
  expect_identical(q29, vapply(p29, q_exact, 0, mu = b$mu, sigma = b$sigma, ymax = b$ymax),
                   info = paste("fqDPO: the quantiles of the log-sum-exp density,", lab29))
}
# p far into the left tail: every one of these searches crosses the dip while its sum is subnormal,
# and for the first two (below DBL_MIN) the quantile itself is reached before the sum is normal
ptiny29 <- c(1e-320, 1e-308, 1e-300, 1e-100, 1e-10)
qtiny29 <- fqDPO(ptiny29, 17934.734753921883, 24.138224901457072)
expect_true(all(is.finite(qtiny29)) && !is.unsorted(qtiny29),
            info = "fqDPO: p from 1e-320 to 1e-10 at the underflow limit: finite and non-decreasing in p (was NA)")
expect_true(all(fpDPO(qtiny29 - 1, 17934.734753921883, 24.138224901457072) < ptiny29 &
                  ptiny29 <= fpDPO(qtiny29, 17934.734753921883, 24.138224901457072)),
            info = "fqDPO: the same quantiles bracket p on the package's own CDF")
# guard: an ordinary pair (a second mode at 0, nowhere near the underflow limit) is the log-sum-exp quantile, as before
expect_identical(fqDPO(p29, 57, 20), vapply(p29, q_exact, 0, mu = 57, sigma = 20),
                 info = "fqDPO: mu = 57, sigma = 20 (a second mode at 0, no underflow) is the log-sum-exp quantile")
# guard: once the sum is normal, a stall still ends the search. This pair's summed CDF stops about 3e-13 short of 1
# (the accuracy of the normalising constant), so p = 1 - 1e-15 lies above the largest sum there is: the search
# walks the dip, the mass, and then gives up at once (NA) instead of walking on to the int cap (minutes).
if (at_home()) {
  elapsed <- system.time(v29 <- suppressWarnings(fqDPO(1 - 1e-15, 17832, 24)))[["elapsed"]]
  expect_true(elapsed < 5,
              info = "fqDPO: a p above the largest sum ends at once, by the stall (not by a walk to the int cap)")
}

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
