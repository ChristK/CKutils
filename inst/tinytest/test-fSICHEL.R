# Test SICHEL distribution functions
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

# =============================================================================
# PDF TESTS
# =============================================================================

# Test 1: PDF basic correctness
pdf_ck <- fdSICHEL(data$x, data$mu, data$sigma, data$nu)
pdf_gamlss <- dSICHEL(data$x, data$mu, data$sigma, data$nu)
expect_equal(pdf_ck, pdf_gamlss, tolerance = tolerance,
             info = "PDF values should match gamlss.dist::dSICHEL")

# Test 2: PDF log scale
pdf_log_ck <- fdSICHEL(data$x, data$mu, data$sigma, data$nu, log_p = TRUE)
pdf_log_gamlss <- dSICHEL(data$x, data$mu, data$sigma, data$nu, log = TRUE)
expect_equal(pdf_log_ck, pdf_log_gamlss, tolerance = tolerance,
             info = "Log PDF values should match gamlss.dist::dSICHEL")

# Test 3: PDF with edge case data
pdf_edge_ck <- fdSICHEL(edge_data$x, edge_data$mu, edge_data$sigma, edge_data$nu)
pdf_edge_gamlss <- dSICHEL(edge_data$x, edge_data$mu, edge_data$sigma, edge_data$nu)
expect_equal(pdf_edge_ck, pdf_edge_gamlss, tolerance = tolerance,
             info = "PDF edge cases should match gamlss.dist::dSICHEL")

# Test 4: PDF parameter recycling
x_short <- c(0, 1, 2)
mu_long <- c(1, 2, 3, 4, 5)
pdf_recycled <- fdSICHEL(x_short, mu_long, sigma = 1, nu = -0.5)
expect_true(length(pdf_recycled) == max(length(x_short), length(mu_long)),
            info = "PDF should handle parameter recycling correctly")

# Test 5: PDF non-negativity
expect_true(all(pdf_ck >= 0), info = "PDF values should be non-negative")
expect_true(all(is.finite(pdf_ck[is.finite(pdf_ck)])), 
            info = "PDF values should be finite when parameters are valid")

# =============================================================================
# CDF TESTS
# =============================================================================

# Test 6: CDF basic correctness
cdf_ck <- fpSICHEL(data$q, data$mu, data$sigma, data$nu)
cdf_gamlss <- pSICHEL(data$q, data$mu, data$sigma, data$nu)
expect_equal(cdf_ck, cdf_gamlss, tolerance = tolerance,
             info = "CDF values should match gamlss.dist::pSICHEL")

# Test 7: CDF lower.tail = FALSE
cdf_upper_ck <- fpSICHEL(data$q, data$mu, data$sigma, data$nu, lower_tail = FALSE)
cdf_upper_gamlss <- pSICHEL(data$q, data$mu, data$sigma, data$nu, lower.tail = FALSE)
expect_equal(cdf_upper_ck, cdf_upper_gamlss, tolerance = tolerance,
             info = "Upper tail CDF should match gamlss.dist::pSICHEL")

# Test 8: CDF log scale
cdf_log_ck <- fpSICHEL(data$q, data$mu, data$sigma, data$nu, log_p = TRUE)
cdf_log_gamlss <- pSICHEL(data$q, data$mu, data$sigma, data$nu, log.p = TRUE)
expect_equal(cdf_log_ck, cdf_log_gamlss, tolerance = tolerance,
             info = "Log CDF should match gamlss.dist::pSICHEL")

# Test 9: CDF monotonicity
q_sorted <- sort(sample(0:10, 20, replace = TRUE))
cdf_sorted <- fpSICHEL(q_sorted, mu = 2, sigma = 1, nu = -0.5)
expect_true(all(diff(cdf_sorted) >= 0), 
            info = "CDF should be non-decreasing")

# Test 10: CDF bounds
expect_true(all(cdf_ck >= 0 & cdf_ck <= 1), 
            info = "CDF values should be between 0 and 1")

# =============================================================================
# QUANTILE TESTS
# =============================================================================

# Test 11: Quantile basic correctness (using smaller data subset for performance)
small_data <- generate_test_data(n = 20, seed = 789)
qtl_ck <- fqSICHEL(small_data$p, small_data$mu, small_data$sigma, small_data$nu)
qtl_gamlss <- qSICHEL(small_data$p, small_data$mu, small_data$sigma, small_data$nu, max.value = 10000)
expect_equal(qtl_ck, qtl_gamlss, tolerance = 0,
             info = "Quantile values should match gamlss.dist::qSICHEL")

# Test 12: Quantile lower.tail = FALSE (small subset)
qtl_upper_ck <- fqSICHEL(small_data$p, small_data$mu, small_data$sigma, small_data$nu, 
                         lower_tail = FALSE)
qtl_upper_gamlss <- qSICHEL(small_data$p, small_data$mu, small_data$sigma, small_data$nu, 
                            lower.tail = FALSE, max.value = 10000)
expect_equal(qtl_upper_ck, qtl_upper_gamlss, tolerance = 0,
             info = "Upper tail quantiles should match gamlss.dist::qSICHEL")

# Test 13: Quantile log scale (small subset)
p_log <- log(small_data$p)
qtl_log_ck <- fqSICHEL(p_log, small_data$mu, small_data$sigma, small_data$nu, log_p = TRUE)

# as usual gamlss.dist implementation has a bug with log scale probabilities
qtl_log_gamlss <- qSICHEL(small_data$p, small_data$mu, small_data$sigma, small_data$nu, 
                          log.p = FALSE, max.value = 10000)
expect_equal(qtl_log_ck, qtl_log_gamlss, tolerance = 0,
             info = "Log scale quantiles should match gamlss.dist::qSICHEL")

# Test 14: Quantile consistency with CDF
mu_test <- 2; sigma_test <- 1; nu_test <- -0.5
p_test <- fpSICHEL(0:10, mu_test, sigma_test, nu_test)
q_from_p <- fqSICHEL(p_test, mu_test, sigma_test, nu_test)
p_from_q <- fpSICHEL(q_from_p, mu_test, sigma_test, nu_test)
expect_equal(p_from_q, p_test, tolerance = 1e-6,
             info = "Quantile and CDF should be inverse functions")

# Test 15: Quantile non-decreasing property
p_sorted <- sort(runif(10, 0.1, 0.9))
q_sorted <- fqSICHEL(p_sorted, mu = 3, sigma = 1.5, nu = 0)
expect_true(all(diff(q_sorted) >= 0), 
            info = "Quantiles should be non-decreasing")

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

# Test 20: Error handling for invalid parameters
expect_error(fdSICHEL(c(1, 2), mu = c(-1, 2), sigma = c(1, 1), nu = c(0, 0)),
             info = "Should error on negative mu")

expect_error(fdSICHEL(c(1, 2), mu = c(1, 2), sigma = c(-1, 1), nu = c(0, 0)),
             info = "Should error on negative sigma")

expect_error(fdSICHEL(c(-1, 2), mu = c(1, 2), sigma = c(1, 1), nu = c(0, 0)),
             info = "Should error on negative x")

# Test 21: Error handling for invalid probabilities in quantile functions
expect_error(fqSICHEL(c(-0.1, 0.5), mu = 1, sigma = 1, nu = 0),
             info = "Should error on probability < 0")

expect_error(fqSICHEL(c(0.5, 1.1), mu = 1, sigma = 1, nu = 0),
             info = "Should error on probability > 1")

# Test 22: Error handling for invalid tau in ZI functions
expect_error(fqZISICHEL(c(0.5), mu = 1, sigma = 1, nu = 0, tau = -0.1),
             info = "Should error on negative tau")

expect_error(fqZISICHEL(c(0.5), mu = 1, sigma = 1, nu = 0, tau = 1.1),
             info = "Should error on tau > 1")

# Test 23: Boundary value behavior
# Test CDF at 0
cdf_at_0 <- fpSICHEL(0, mu = 2, sigma = 1, nu = -0.5)
pdf_at_0 <- fdSICHEL(0, mu = 2, sigma = 1, nu = -0.5)
expect_equal(cdf_at_0, pdf_at_0, tolerance = tolerance,
             info = "CDF at 0 should equal PDF at 0")

# Test 24: Large sigma with positive nu (should use NBI approximation)
pdf_large_sigma <- fdSICHEL(c(0, 1, 2), mu = 1, sigma = 15000, nu = 0.5)
expect_true(all(is.finite(pdf_large_sigma)), 
            info = "Should handle large sigma with positive nu using NBI approximation")

# =============================================================================
# RANDOM GENERATION TESTS
# =============================================================================

# Test 25: Random generation basic functionality
set.seed(123)
r_ck <- frSICHEL(100, mu = 2, sigma = 1, nu = -0.5)
expect_true(length(r_ck) == 100, info = "Should generate correct number of values")
expect_true(all(r_ck >= 0), info = "Random values should be non-negative integers")
expect_true(all(r_ck == floor(r_ck)), info = "Random values should be integers")

# Test 26: Random generation with different parameters
set.seed(456)
r_ck_params <- frSICHEL(50, mu = c(1, 3), sigma = c(0.5, 1.5), nu = c(-1, 1))
expect_true(length(r_ck_params) == 50, info = "Should handle parameter recycling in random generation")

# Test 27: Zero-inflated random generation
set.seed(789)
r_zi_ck <- frZISICHEL(100, mu = 2, sigma = 1, nu = -0.5, tau = 0.3)
expect_true(length(r_zi_ck) == 100, info = "Should generate correct number of ZI values")
zero_prop <- sum(r_zi_ck == 0) / length(r_zi_ck)
expect_true(zero_prop > 0.2, info = "ZI random generation should produce excess zeros")

# =============================================================================
# PERFORMANCE AND NUMERICAL STABILITY
# =============================================================================

# Test 28: Performance with moderate-sized vectors
n_perf <- 1000
set.seed(111)
perf_data <- generate_test_data(n = n_perf, seed = 111)

# These should run without excessive delay
start_time <- Sys.time()
pdf_perf <- fdSICHEL(perf_data$x, perf_data$mu, perf_data$sigma, perf_data$nu)
end_time <- Sys.time()
expect_true(difftime(end_time, start_time, units = "secs") < 5,
            info = "PDF computation should be reasonably fast")

# Test 29: Numerical stability with extreme parameter values
extreme_nu <- c(-3, 3)
extreme_sigma <- c(0.01, 5)  # Avoid extremely large sigma
pdf_extreme <- fdSICHEL(c(0, 1, 2), mu = 1, sigma = extreme_sigma, nu = extreme_nu)
expect_true(sum(is.finite(pdf_extreme)) >= length(pdf_extreme) * 0.8,
            info = "Should maintain numerical stability with reasonable extreme parameters")

# =============================================================================
# CONSISTENCY CHECKS
# =============================================================================

# Test 30: PDF sums approximately to 1 for reasonable truncation
x_range <- 0:20
mu_test <- 3; sigma_test <- 1; nu_test <- -0.5
pdf_range <- fdSICHEL(x_range, mu_test, sigma_test, nu_test)
pdf_sum <- sum(pdf_range)
expect_true(pdf_sum > 0.95 && pdf_sum <= 1.001,
            info = "PDF should sum approximately to 1 over reasonable range")

# Test 31: Mean consistency check
# For SICHEL distribution, mean should be approximately mu
set.seed(222)
r_mean_test <- frSICHEL(5000, mu = 4, sigma = 1, nu = -0.5)
sample_mean <- mean(r_mean_test)
expect_true(abs(sample_mean - 4) < 0.3,
            info = "Sample mean should be close to theoretical mean (mu)")

# Test 32: Zero-inflation consistency
# ZI distribution should have more zeros than regular distribution
set.seed(333)
r_regular <- frSICHEL(2000, mu = 3, sigma = 1, nu = -0.5)
r_zi <- frZISICHEL(2000, mu = 3, sigma = 1, nu = -0.5, tau = 0.2)

zero_prop_regular <- sum(r_regular == 0) / length(r_regular)
zero_prop_zi <- sum(r_zi == 0) / length(r_zi)

expect_true(zero_prop_zi > zero_prop_regular,
            info = "Zero-inflated distribution should have more zeros than regular distribution")

# =============================================================================
# EDGE CASES
# =============================================================================

# Test 33: Single value inputs
single_pdf <- fdSICHEL(1, mu = 2, sigma = 1, nu = -0.5)
expect_true(length(single_pdf) == 1 && is.finite(single_pdf),
            info = "Should handle single value inputs correctly")

single_cdf <- fpSICHEL(1, mu = 2, sigma = 1, nu = -0.5)
expect_true(length(single_cdf) == 1 && single_cdf >= 0 && single_cdf <= 1,
            info = "Should handle single value CDF inputs correctly")

# Test 34: Very small and large quantiles
very_small_p <- c(1e-6, 1e-5)
very_large_p <- c(0.999, 0.9999)

qtl_small <- fqSICHEL(very_small_p, mu = 2, sigma = 1, nu = -0.5)
qtl_large <- fqSICHEL(very_large_p, mu = 2, sigma = 1, nu = -0.5)

expect_true(all(qtl_small >= 0), info = "Small probability quantiles should be non-negative")
expect_true(all(qtl_large >= qtl_small[1]), info = "Large probability quantiles should be larger")

# =============================================================================
# THE BOUND BEHIND THE EARLY NA OF THE QUANTILE SEARCH
# =============================================================================

# Test 35: fqSICHEL() reports a quantile beyond the int range at once when the internal
# bound sichel_cdf_below() proves F(2^31 - 2) < p. The scan it saves takes some 40 s, so the
# bound is tested here at N = 2000, against the package's own CDF, on a log grid of mu around
# the boundary mu* (F(N; mu*) = p; F falls as mu rises). It must never claim F(N) < p where
# fpSICHEL(N) >= p, which would turn a finite quantile into NA; and it must be able to fire,
# or that check would pass for a bound that is always FALSE.
N_bound <- 2000
for (sn in list(c(0.01, -30), c(1, -0.5), c(1, 12), c(100, -0.5), c(100, 0.7), c(1e8, -0.5))) {
  for (p_bound in c(1e-10, 0.5, 0.999, 1 - 1e-8)) {
    sg_bound <- sn[1]
    nu_bound <- sn[2]
    info_bound <- sprintf("N = %d, sigma = %g, nu = %g, p = %g", N_bound, sg_bound, nu_bound, p_bound)
    # the boundary: the root of F(N; mu) - p in log(mu)
    mu_star <- exp(uniroot(function(t) fpSICHEL(N_bound, exp(t), sg_bound, nu_bound) - p_bound,
                           interval = log(c(1e-6, 1e17)), tol = 1e-12)$root)
    # 61 points from mu*/1000 to 1000 mu*, and three just below mu*
    mu_grid <- mu_star * c(10^seq(-3, 3, by = 0.1), 1 - c(1e-3, 1e-6, 1e-9))
    F_grid <- fpSICHEL(N_bound, mu_grid, sg_bound, nu_bound)
    expect_true(all(is.finite(F_grid)), info = paste("fpSICHEL is finite:", info_bound))
    expect_false(any(CKutils:::.sichel_cdf_below(N_bound, p_bound, mu_grid, sg_bound, nu_bound) &
                       F_grid >= p_bound),
                 info = paste("no false NA:", info_bound))
    # It fires at 10 mu*. The bound keeps a margin of 1e-6 p below p, which for sigma = 1e8 at
    # p = 1 - 1e-8 is 100 times 1 - p: that tail has to fall by that much first (it fires from
    # about 1e4 mu*), so 1e6 mu* is used there.
    far_bound <- if (sg_bound == 1e8 && p_bound > 0.9999) 1e6 else 10
    expect_true(CKutils:::.sichel_cdf_below(N_bound, p_bound, far_bound * mu_star, sg_bound, nu_bound),
                info = paste("fires far above the boundary:", info_bound))
  }
}

# Final success message
# cat("All SICHEL distribution tests passed successfully!\n")

# =============================================================================
# A large q: O(1) memory, and the CDF sum stops once it has settled
# =============================================================================
# fcdfSICHEL_scalar used to store the whole Bessel-ratio recursion in two
# std::vector<double>s of q + 1 elements (16 bytes per unit of q) and to add all
# q + 1 terms: fpSICHEL(5e7, mu = 2, ...) took ~0.8 s and ~800 MB for a sum
# that settles after a few hundred terms. It now holds one step of the
# recursion and returns once a term past the mode leaves the sum unchanged. The
# value is the same double either way, so the CDF at 5e7 is the CDF at 1e4,
# where the sum has long settled, bit for bit. The time bound is a timing
# expectation, so it runs only when at_home(): CRAN never runs it.
el <- system.time(v <- fpSICHEL(5e7, mu = 2, sigma = 1, nu = -0.5))[["elapsed"]]
expect_identical(v, fpSICHEL(1e4, mu = 2, sigma = 1, nu = -0.5),
                 info = "fpSICHEL at q = 5e7 is the CDF at q = 1e4, bit for bit")
expect_equal(v, 1, tolerance = 1e-12,
             info = "fpSICHEL at q = 5e7 is 1 (the mass is all below q)")
if (at_home()) {
  expect_true(el < 0.2,
              info = "fpSICHEL(5e7, mu = 2) in under 0.2 s (the sum stops once settled)")
}

# The CDF sum is not cut short where the recursion has broken down. For a very
# negative nu and a small mu the ratios of the recursion turn negative before j
# reaches -nu, so the density is NaN from about x = 10 on, and the CDF is NaN at
# every q beyond that: both are the one recursion, and they always were. The sum
# has settled by j = 8-11, a step or two before the break-down, so a settled-exit
# taken there would return ~1 where the full sum is NaN. (These are outside the
# range of any model; the expectation is that the CDF is NaN exactly when the
# density is NaN somewhere in 0..q, not a claim that NaN is the right answer.)
for (pr in list(c(0.02, 0.5, -20), c(0.1, 0.5, -15), c(0.05, 0.1, -40))) {
  d_nr <- suppressWarnings(fdSICHEL(0:200, pr[1], pr[2], pr[3]))
  p_nr <- suppressWarnings(fpSICHEL(200, pr[1], pr[2], pr[3]))
  expect_identical(is.nan(p_nr), anyNA(d_nr),
                   info = paste0("fpSICHEL is NaN exactly when fdSICHEL(0:q) is, mu = ", pr[1],
                                 ", sigma = ", pr[2], ", nu = ", pr[3]))
}

