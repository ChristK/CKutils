# Test NBI distribution functions
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
# PDF TESTS
# =============================================================================

# Test 1: PDF basic correctness
pdf_ck <- fdNBI(data$x, data$mu, data$sigma)
pdf_gamlss <- dNBI(data$x, data$mu, data$sigma)
expect_equal(pdf_ck, pdf_gamlss, tolerance = tolerance, 
             info = "NBI PDF should match gamlss.dist dNBI")

# Test 2: PDF with log = TRUE
pdf_ck_log <- fdNBI(data$x, data$mu, data$sigma, log = TRUE)
pdf_gamlss_log <- dNBI(data$x, data$mu, data$sigma, log = TRUE)
expect_equal(pdf_ck_log, pdf_gamlss_log, tolerance = tolerance,
             info = "NBI PDF with log should match gamlss.dist dNBI")

# Test 3: PDF edge cases - very small sigma (Poisson approximation)
small_sigma <- rep(1e-6, 50)
pdf_ck_small <- fdNBI(data$x[1:50], data$mu[1:50], small_sigma)
pdf_gamlss_small <- dNBI(data$x[1:50], data$mu[1:50], small_sigma)
expect_equal(pdf_ck_small, pdf_gamlss_small, tolerance = tolerance,
             info = "NBI PDF with small sigma should match gamlss.dist dNBI")

# Test 4: PDF parameter validation
expect_error(fdNBI(c(-1, 0, 1), mu = 1, sigma = 1), 
             info = "fdNBI should error on negative x")
expect_error(fdNBI(c(0, 1, 2), mu = 0, sigma = 1), 
             info = "fdNBI should error on non-positive mu")
expect_error(fdNBI(c(0, 1, 2), mu = 1, sigma = 0), 
             info = "fdNBI should error on non-positive sigma")

# Test 5: PDF recycling
pdf_recycle <- fdNBI(c(0, 1, 2), mu = c(1, 2), sigma = 0.5)
expect_equal(length(pdf_recycle), 3, info = "PDF recycling should work")


# Skip tests i# Test 5: PDF parameter validation
expect_error(fdNBI(c(-1, 0, 1), mu = 1, sigma = 1), 
             info = "fdNBI should error on negative x")
expect_error(fdNBI(c(0, 1, 2), mu = 0, sigma = 1), 
             info = "fdNBI should error on non-positive mu")
expect_error(fdNBI(c(0, 1, 2), mu = 1, sigma = 0), 
             info = "fdNBI should error on non-positive sigma")

# Test 5: PDF recycling
pdf_recycle <- fdNBI(c(0, 1, 2), mu = c(1, 2), sigma = 0.5)
norec <- fdNBI(c(0, 1, 2), mu = c(1, 2, 1), sigma = c(0.5, 0.5, 0.5))
expect_equal(pdf_recycle, norec, info = "PDF recycling should work")


# =============================================================================
# CDF TESTS
# =============================================================================

# Test 6: CDF basic correctness
cdf_ck <- fpNBI(data$q, data$mu, data$sigma)
cdf_gamlss <- pNBI(data$q, data$mu, data$sigma)
expect_equal(cdf_ck, cdf_gamlss, tolerance = tolerance,
             info = "NBI CDF should match gamlss.dist pNBI")

# Test 7: CDF with log.p = TRUE
cdf_ck_log <- fpNBI(data$q, data$mu, data$sigma, log_p = TRUE)
cdf_gamlss_log <- pNBI(data$q, data$mu, data$sigma, log.p = TRUE)
expect_equal(cdf_ck_log, cdf_gamlss_log, tolerance = tolerance,
             info = "NBI CDF with log.p should match gamlss.dist pNBI")

# Test 8: CDF with lower.tail = FALSE
cdf_ck_upper <- fpNBI(data$q, data$mu, data$sigma, lower_tail = FALSE)
cdf_gamlss_upper <- pNBI(data$q, data$mu, data$sigma, lower.tail = FALSE)
expect_equal(cdf_ck_upper, cdf_gamlss_upper, tolerance = tolerance,
             info = "NBI CDF with lower.tail=FALSE should match gamlss.dist pNBI")

# Test 9: CDF edge cases - very small sigma
cdf_ck_small <- fpNBI(data$q[1:50], data$mu[1:50], small_sigma)
# Below sigma = 1e-4 the NBI is evaluated as its Poisson limit, by design in
# both packages; gamlss.dist 6.1-11 gets that branch wrong (off by up to 1,
# vectorised and scalar alike), so the reference is the limit itself.
cdf_gamlss_small <- ppois(data$q[1:50], data$mu[1:50])
expect_equal(cdf_ck_small, cdf_gamlss_small, tolerance = tolerance,
             info = "NBI CDF with small sigma should match gamlss.dist pNBI")

# Test 10: CDF parameter validation
expect_error(fpNBI(c(-1, 0, 1), mu = 1, sigma = 1), 
             info = "fpNBI should error on negative q")
expect_error(fpNBI(c(0, 1, 2), mu = 0, sigma = 1), 
             info = "fpNBI should error on non-positive mu")
expect_error(fpNBI(c(0, 1, 2), mu = 1, sigma = 0), 
             info = "fpNBI should error on non-positive sigma")

# =============================================================================
# QUANTILE TESTS
# =============================================================================

# Test 11: Quantile basic correctness
q_ck <- fqNBI(data$p, data$mu, data$sigma)
q_gamlss <- qNBI(data$p, data$mu, data$sigma)
expect_equal(q_ck, q_gamlss, tolerance = tolerance,
             info = "NBI quantile should match gamlss.dist qNBI")

# Test 12: Quantile with log.p = TRUE
q_ck_log <- fqNBI(log(data$p), data$mu, data$sigma, log_p = TRUE)
q_gamlss_log <- qNBI(data$p, data$mu, data$sigma, log.p = FALSE) # avoid gamlss.dist bug in log.p
expect_equal(q_ck_log, q_gamlss_log, tolerance = tolerance,
             info = "NBI quantile with log.p should match gamlss.dist qNBI")

# Test 13: Quantile with lower.tail = FALSE
q_ck_upper <- fqNBI(data$p, data$mu, data$sigma, lower_tail = FALSE)
q_gamlss_upper <- qNBI(data$p, data$mu, data$sigma, lower.tail = FALSE)
expect_equal(q_ck_upper, q_gamlss_upper, tolerance = tolerance,
             info = "NBI quantile with lower.tail=FALSE should match gamlss.dist qNBI")

# Test 14: Quantile edge cases - very small sigma
q_ck_small <- fqNBI(data$p[1:50], data$mu[1:50], small_sigma)
q_gamlss_small <- qNBI(data$p[1:50], data$mu[1:50], small_sigma)
expect_equal(q_ck_small, q_gamlss_small, tolerance = tolerance,
             info = "NBI quantile with small sigma should match gamlss.dist qNBI")

# Test 15: Quantile parameter validation
# Test p outside [0,1]
expect_error(fqNBI(c(-0.1, 0.5, 1.1), mu = 1, sigma = 1), 
  info = "fqNBI should error on p outside [0,1]")

# Test non-positive mu
expect_error(fqNBI(c(0.1, 0.5, 0.9), mu = 0, sigma = 1),
   info = "fqNBI should error on non-positive mu")

# Test non-positive sigma
expect_error(fqNBI(c(0.1, 0.5, 0.9), mu = 1, sigma = 0),
 info = "fqNBI should error on non-positive sigma")

# =============================================================================
# RANDOM GENERATION TESTS
# =============================================================================

# Test 16: Random generation basic functionality
set.seed(123)
r_ck <- frNBI(100, mu = 2, sigma = 1)
expect_equal(length(r_ck), 100, info = "Random generation should return correct length")
expect_true(all(r_ck >= 0), info = "Random variates should be non-negative")

# Test 17: Random generation with vector parameters
set.seed(123)
r_ck_vec <- frNBI(10, mu = c(1, 2), sigma = c(0.5, 1.5))
expect_equal(length(r_ck_vec), 10, info = "Random generation with vectors should work")

# Test 18: Random generation parameter validation
expect_error(frNBI(10, mu = 0, sigma = 1), 
             info = "frNBI should error on non-positive mu")
expect_error(frNBI(10, mu = 1, sigma = 0), 
             info = "frNBI should error on non-positive sigma")
expect_error(frNBI(0, mu = 1, sigma = 1), 
             info = "frNBI should error on non-positive n")


# =============================================================================
# CONSISTENCY TESTS
# =============================================================================

# Test 29: PDF sums to 1 (approximately, for discrete distributions)
# Test with small range to avoid computational issues
x_test <- 0:20
pdf_sum <- sum(fdNBI(x_test, mu = 2, sigma = 1))
expect_true(abs(pdf_sum - 1) < 0.01, info = "PDF should approximately sum to 1")

# Test 30: CDF is monotonic
q_test <- 0:10
cdf_test <- fpNBI(q_test, mu = 2, sigma = 1)
expect_true(all(diff(cdf_test) >= 0), info = "CDF should be monotonic")

# Test 31: Quantile is left-inverse of CDF
p_test <- fpNBI(0:10, mu = 2, sigma = 1)
q_from_p <- fqNBI(p_test, mu = 2, sigma = 1)
p_from_q <- fpNBI(q_from_p, mu = 2, sigma = 1)
expect_equal(p_from_q, p_test,
             info = "Quantile should be left-inverse of CDF")

# Test 32: Random variates follow expected distribution
set.seed(123)
r_test <- frNBI(10000, mu = 2, sigma = 1)
empirical_mean <- mean(r_test)
theoretical_mean <- 2  # mu = 2
expect_true(abs(empirical_mean - theoretical_mean) < 0.1,
            info = "Random variates should have correct mean")

# Test 33: Zero-inflated has more zeros than regular NBI
set.seed(123)
r_nbi <- frNBI(1000, mu = 2, sigma = 1)
r_zinbi <- frZINBI(1000, mu = 2, sigma = 1, nu = 0.3)
prop_zeros_nbi <- mean(r_nbi == 0)
prop_zeros_zinbi <- mean(r_zinbi == 0)
expect_true(prop_zeros_zinbi > prop_zeros_nbi,
            info = "Zero-inflated should have more zeros than regular NBI")

# =============================================================================
# PERFORMANCE TESTS (basic timing)
# =============================================================================

# Test 34: Basic timing test (should complete reasonably quickly)
start_time <- Sys.time()
large_pdf <- fdNBI(rep(1:5, 2000), mu = rep(c(1, 2), 5000), sigma = rep(0.5, 10000))
end_time <- Sys.time()
elapsed <- as.numeric(end_time - start_time)
expect_true(elapsed < 5, info = "Large PDF calculation should complete within 5 seconds")

# cat("All NBI distribution tests passed!\n")

# =============================================================================
# A QUANTILE AN INT CANNOT HOLD IS NA, WITH A WARNING
#
# R's qnbinom_mu / qpois return a double: Inf for p = 1 (also log_p with p = 0, and
# lower_tail = FALSE with p = 0), Inf or NaN for mu = Inf, and above INT_MAX for a
# huge mu. Putting that in an int is undefined behaviour: it gave NA only by
# accident on x86-64 (INT_MIN), silently, and probably INT_MAX on AArch64. It is now
# NA on purpose, with one warning per call.
# =============================================================================

# p = 1: the quantile is infinite
expect_warning(res_p1 <- fqNBI(1, mu = 5, sigma = 0.5), "NAs produced",
               info = "fqNBI(p = 1) warns")
expect_true(is.na(res_p1), info = "fqNBI(p = 1) is NA")

# a finite quantile above INT_MAX (the true value at mu = 1e10, sigma = 1 is 6931471805)
expect_warning(res_big <- fqNBI(0.5, mu = 1e10, sigma = 1), "NAs produced",
               info = "fqNBI warns for a quantile above INT_MAX")
expect_true(is.na(res_big), info = "fqNBI above INT_MAX is NA")

# the other ways to reach an infinite quantile
expect_warning(res_logp0 <- fqNBI(0, mu = 5, sigma = 0.5, log_p = TRUE), "NAs produced",
               info = "fqNBI(log p = 0) warns")
expect_true(is.na(res_logp0), info = "fqNBI(log p = 0) is NA")
expect_warning(res_up0 <- fqNBI(0, mu = 5, sigma = 0.5, lower_tail = FALSE), "NAs produced",
               info = "fqNBI(upper tail, p = 0) warns")
expect_true(is.na(res_up0), info = "fqNBI(upper tail, p = 0) is NA")
expect_warning(res_muinf <- fqNBI(0.5, mu = Inf, sigma = 1), "NAs produced",
               info = "fqNBI(mu = Inf) warns")
expect_true(is.na(res_muinf), info = "fqNBI(mu = Inf) is NA")

# the Poisson branch (sigma < 1e-4) is guarded the same way
expect_warning(res_pois1 <- fqNBI(1, mu = 5, sigma = 1e-6), "NAs produced",
               info = "fqNBI Poisson branch, p = 1, warns")
expect_true(is.na(res_pois1), info = "fqNBI Poisson branch, p = 1, is NA")
expect_warning(res_poisbig <- fqNBI(0.5, mu = 1e10, sigma = 1e-6), "NAs produced",
               info = "fqNBI Poisson branch, quantile above INT_MAX, warns")
expect_true(is.na(res_poisbig), info = "fqNBI Poisson branch, quantile above INT_MAX, is NA")

# one warning per call; the other elements are untouched, and an NA input stays NA
n_warn <- 0L
res_mixed <- withCallingHandlers(
  fqNBI(c(0.5, 1, NA, 1, 0.9), mu = 5, sigma = 0.5),
  warning = function(w) {
    n_warn <<- n_warn + 1L
    invokeRestart("muffleWarning")
  })
expect_identical(res_mixed, c(4L, NA, NA, NA, 11L),
                 info = "fqNBI: only the unrepresentable and NA elements are NA")
expect_equal(n_warn, 1L, info = "fqNBI warns once per call, not once per element")

# The largest quantile returned is INT_MAX - 1 (CK_MAX_COUNT in recycling_helpers.h):
# a quantile of exactly INT_MAX is NA as well. sigma < 1e-4 is Poisson, so mu is
# chosen to put qpois(0.5, mu) on each side of that bound.
mu_ok <- 2147483645.7
mu_edge <- 2147483646.7
if (qpois(0.5, mu_ok) == 2147483646 && qpois(0.5, mu_edge) == 2147483647) {
  expect_silent(res_ok <- fqNBI(0.5, mu = mu_ok, sigma = 1e-6))
  expect_identical(res_ok, 2147483646L, info = "fqNBI returns INT_MAX - 1")
  expect_warning(res_edge <- fqNBI(0.5, mu = mu_edge, sigma = 1e-6), "NAs produced",
                 info = "fqNBI warns for a quantile of exactly INT_MAX")
  expect_true(is.na(res_edge), info = "fqNBI: a quantile of exactly INT_MAX is NA")
}

# Guards that hold with or without the fix: ordinary quantiles are unchanged and
# silent, and an NA/NaN input is NA without the new warning
expect_silent(res_ord <- fqNBI(c(.1, .5, .9), 5, .5))
expect_identical(res_ord, as.integer(qnbinom(c(.1, .5, .9), size = 2, mu = 5)),
                 info = "fqNBI ordinary quantiles are unchanged")
expect_silent(res_na <- fqNBI(c(NA, NaN), 5, .5))
expect_identical(res_na, c(NA_integer_, NA_integer_), info = "fqNBI NA/NaN p is NA, silently")
