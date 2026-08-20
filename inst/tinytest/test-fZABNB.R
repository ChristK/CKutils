# Test ZABNB (Zero Adjusted / hurdle Beta Negative Binomial) distribution
# functions. Using tinytest framework.

# Skip tests if gamlss.dist is not available
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
  exit_file("gamlss.dist not available - skipping ZABNB validation tests")
}

# Load required libraries
suppressMessages(library(gamlss.dist))

# Set tolerance for floating point comparisons
tolerance <- sqrt(.Machine$double.eps)

# NOTE: gamlss.dist's *BNB functions have element-recycling bugs when several
# parameter vectors are supplied together (the same class of bug documented in
# test-fZANBI.R). Every reference value below is therefore computed one element
# at a time and compared against our vectorised call.
ref_scalar <- function(FUN, first, ...) {
  args <- list(...)
  vapply(seq_along(first), function(i) {
    do.call(FUN, c(list(first[i]), lapply(args, `[`, i)))
  }, numeric(1))
}

# Test data generators
generate_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  list(
    x = sample(0:20, n, replace = TRUE),
    q = sample(0:15, n, replace = TRUE),
    p = runif(n, 0.001, 0.999),
    mu = runif(n, 0.5, 5),
    sigma = runif(n, 0.1, 2),
    nu = runif(n, 0.1, 3),
    tau = runif(n, 0.01, 0.6)
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
    nu = runif(n, 0.1, 2),
    tau = runif(n, 0.05, 0.5)
  )
}

# Generate test data
data <- generate_test_data()
edge_data <- generate_edge_case_data()

# =============================================================================
# DENSITY TESTS
# =============================================================================

# Test 1: ZABNB PDF basic correctness
expect_equal(
  fdZABNB(data$x, data$mu, data$sigma, data$nu, data$tau),
  ref_scalar(gamlss.dist::dZABNB, data$x, data$mu, data$sigma, data$nu, data$tau),
  tolerance = tolerance,
  info = "ZABNB PDF should match gamlss.dist dZABNB (vectorised vs elementwise)"
)

# Test 2: ZABNB PDF on the log scale
expect_equal(
  fdZABNB(data$x, data$mu, data$sigma, data$nu, data$tau, log = TRUE),
  log(ref_scalar(gamlss.dist::dZABNB, data$x, data$mu, data$sigma, data$nu, data$tau)),
  tolerance = tolerance,
  info = "ZABNB log-PDF should match log(dZABNB)"
)

# Test 3: the hurdle means P(Y = 0) is exactly tau
expect_identical(
  fdZABNB(rep(0, 10), data$mu[1:10], data$sigma[1:10], data$nu[1:10], data$tau[1:10]),
  data$tau[1:10],
  info = "ZABNB: P(Y = 0) must be exactly tau (hurdle, not zero inflation)"
)

# Test 4: edge-case parameter block
expect_equal(
  fdZABNB(edge_data$x, edge_data$mu, edge_data$sigma, edge_data$nu, edge_data$tau),
  ref_scalar(gamlss.dist::dZABNB, edge_data$x, edge_data$mu, edge_data$sigma,
             edge_data$nu, edge_data$tau),
  tolerance = tolerance,
  info = "ZABNB PDF should match gamlss.dist on the edge-case parameter block"
)

# =============================================================================
# CDF TESTS
# =============================================================================

# Test 5: ZABNB CDF basic correctness
expect_equal(
  fpZABNB(data$q, data$mu, data$sigma, data$nu, data$tau),
  ref_scalar(gamlss.dist::pZABNB, data$q, data$mu, data$sigma, data$nu, data$tau),
  tolerance = tolerance,
  info = "ZABNB CDF should match gamlss.dist pZABNB (vectorised vs elementwise)"
)

# Test 6: upper tail
expect_equal(
  fpZABNB(data$q, data$mu, data$sigma, data$nu, data$tau, lower_tail = FALSE),
  1 - ref_scalar(gamlss.dist::pZABNB, data$q, data$mu, data$sigma, data$nu, data$tau),
  tolerance = tolerance,
  info = "ZABNB CDF with lower_tail = FALSE should be the survival function"
)

# Test 7: log scale
expect_equal(
  fpZABNB(data$q, data$mu, data$sigma, data$nu, data$tau, log_p = TRUE),
  log(ref_scalar(gamlss.dist::pZABNB, data$q, data$mu, data$sigma, data$nu, data$tau)),
  tolerance = tolerance,
  info = "ZABNB log-CDF should match log(pZABNB)"
)

# Test 8: F(0) is exactly tau
expect_identical(
  fpZABNB(rep(0, 10), data$mu[1:10], data$sigma[1:10], data$nu[1:10], data$tau[1:10]),
  data$tau[1:10],
  info = "ZABNB: F(0) must be exactly tau"
)

# =============================================================================
# DENSITY / CDF / QUANTILE INTERNAL CONSISTENCY
# =============================================================================

# Test 9: cumsum of the pmf must reproduce the cdf
for (i in 1:5) {
  dens <- fdZABNB(0:40, data$mu[i], data$sigma[i], data$nu[i], data$tau[i])
  expect_equal(
    cumsum(dens),
    fpZABNB(0:40, data$mu[i], data$sigma[i], data$nu[i], data$tau[i]),
    tolerance = 1e-10,
    info = paste0("ZABNB: cumsum(pmf) should equal the cdf (parameter set ", i, ")")
  )
}

# Test 10: total probability. BNB has a polynomial tail, so the pmf summed over
# a finite range leaves a real residual; the identity that must hold to machine
# precision is sum(pmf over 0:K) + P(X > K) == 1.
for (i in 1:5) {
  K <- 2000L
  expect_equal(
    sum(fdZABNB(0:K, data$mu[i], data$sigma[i], data$nu[i], data$tau[i])) +
      fpZABNB(K, data$mu[i], data$sigma[i], data$nu[i], data$tau[i], lower_tail = FALSE),
    1,
    tolerance = 1e-12,
    info = paste0("ZABNB: sum(pmf) + survival should be 1 (parameter set ", i, ")")
  )
}

# Test 11: quantile inverts the cdf, F(Q(p)) >= p > F(Q(p) - 1)
for (i in 1:10) {
  qq <- fqZABNB(data$p[i], data$mu[i], data$sigma[i], data$nu[i], data$tau[i])
  if (is.finite(qq)) {
    expect_true(
      fpZABNB(qq, data$mu[i], data$sigma[i], data$nu[i], data$tau[i]) >= data$p[i] - 1e-8,
      info = paste0("ZABNB: F(Q(p)) >= p (parameter set ", i, ")")
    )
    if (qq > 0) {
      expect_true(
        fpZABNB(qq - 1, data$mu[i], data$sigma[i], data$nu[i], data$tau[i]) < data$p[i] + 1e-8,
        info = paste0("ZABNB: F(Q(p) - 1) < p (parameter set ", i, ")")
      )
    }
  }
}

# =============================================================================
# QUANTILE TESTS
# =============================================================================

test_indices <- sample(length(data$p), 20)
p_test <- data$p[test_indices]
mu_test <- data$mu[test_indices]
sigma_test <- data$sigma[test_indices]
nu_test <- data$nu[test_indices]
tau_test <- runif(20, 0.01, 0.3)

# Test 12: ZABNB quantile
qtl_za_ck <- fqZABNB(p_test, mu_test, sigma_test, nu_test, tau_test)
qtl_za_ref <- gamlss.dist::qZABNB(p_test, mu_test, sigma_test, nu_test, tau_test)

expect_equal(
  qtl_za_ck,
  qtl_za_ref,
  info = "ZABNB: Quantile test"
)

# Test 13: upper-tail quantile
expect_equal(
  fqZABNB(p_test, mu_test, sigma_test, nu_test, tau_test, lower_tail = FALSE),
  gamlss.dist::qZABNB(p_test, mu_test, sigma_test, nu_test, tau_test,
                      lower.tail = FALSE),
  info = "ZABNB: Quantile test with lower_tail = FALSE"
)

# =============================================================================
# PARAMETER RECYCLING TESTS
# =============================================================================

p_vec <- c(0.1, 0.3, 0.5, 0.7)
x_vec <- 0:3
mu_vec <- c(1, 2)
sigma_single <- 0.5
nu_vec <- c(1, 1.5)
tau_single <- 0.1

# Test 14: Density parameter recycling with 5 arguments
expect_equal(
  fdZABNB(x_vec, mu_vec, sigma_single, nu_vec, tau_single),
  fdZABNB(x_vec, rep(mu_vec, length.out = 4), rep(sigma_single, 4),
          rep(nu_vec, length.out = 4), rep(tau_single, 4)),
  info = "ZABNB: Density parameter recycling test"
)

# Test 15: CDF parameter recycling with 5 arguments
expect_equal(
  fpZABNB(x_vec, mu_vec, sigma_single, nu_vec, tau_single),
  fpZABNB(x_vec, rep(mu_vec, length.out = 4), rep(sigma_single, 4),
          rep(nu_vec, length.out = 4), rep(tau_single, 4)),
  info = "ZABNB: CDF parameter recycling test"
)

# Test 16: Quantile parameter recycling with 5 parameters (ZABNB)
qtl_recycled <- fqZABNB(p_vec, mu_vec, sigma_single, nu_vec, tau_single)
qtl_expected <- fqZABNB(p_vec, rep(mu_vec, length.out = 4),
                        rep(sigma_single, 4), rep(nu_vec, length.out = 4),
                        rep(tau_single, 4))

expect_equal(
  qtl_recycled,
  qtl_expected,
  info = "ZABNB: Parameter recycling test"
)

# Test 17: zero-length inputs recycle to a zero-length result
expect_equal(length(fdZABNB(numeric(0), 1, 1, 1, 0.1)), 0L,
             info = "fdZABNB zero-length x recycles to length 0")
expect_equal(length(fpZABNB(numeric(0), 1, 1, 1, 0.1)), 0L,
             info = "fpZABNB zero-length q recycles to length 0")

# =============================================================================
# RANDOM GENERATION
# =============================================================================

# Test 18: frZABNB draws the right number of non-negative variates and the
# proportion of zeros tracks tau (the hurdle probability).
set.seed(123)
r_za <- frZABNB(2000, mu = 5, sigma = 2, nu = 1.5, tau = 0.25)
expect_equal(length(r_za), 2000L, info = "frZABNB returns the requested length")
expect_true(all(r_za >= 0), info = "frZABNB variates are non-negative")
expect_true(abs(mean(r_za == 0) - 0.25) < 0.05,
            info = "frZABNB proportion of zeros should be close to tau")

# =============================================================================
# NA / NaN HANDLING
# =============================================================================

expect_true(is.na(fdZABNB(NA_real_, 1, 1, 1, 0.1)), info = "fdZABNB NA x -> NA")
expect_true(is.na(fdZABNB(NaN, 1, 1, 1, 0.1)), info = "fdZABNB NaN x -> NA")
expect_true(is.na(fpZABNB(NA_real_, 1, 1, 1, 0.1)), info = "fpZABNB NA q -> NA")
expect_true(is.na(fpZABNB(NaN, 1, 1, 1, 0.1)), info = "fpZABNB NaN q -> NA")
expect_true(is.na(fqZABNB(NA_real_, 1, 1, 1, 0.1)), info = "fqZABNB NA p -> NA")
expect_true(is.na(fdZABNB(1, NA_real_, 1, 1, 0.1)), info = "fdZABNB NA mu -> NA")
expect_true(is.na(fpZABNB(1, 1, 1, 1, NA_real_)), info = "fpZABNB NA tau -> NA")

# =============================================================================
# PARAMETER VALIDATION
# =============================================================================

expect_error(fdZABNB(-1, 1, 1, 1, 0.1), "x must be >=0",
             info = "fdZABNB rejects x < 0")
expect_error(fpZABNB(-1, 1, 1, 1, 0.1), "q must be >=0",
             info = "fpZABNB rejects q < 0")
expect_error(fdZABNB(1, 0, 1, 1, 0.1), "mu must be greater than 0",
             info = "fdZABNB rejects mu = 0")
expect_error(fdZABNB(1, 1, 0, 1, 0.1), "sigma must be greater than 0",
             info = "fdZABNB rejects sigma = 0")
expect_error(fdZABNB(1, 1, 1, 0, 0.1), "nu must be greater than 0",
             info = "fdZABNB rejects nu = 0")
expect_error(fdZABNB(1, 1, 1, 1, 0), "tau must be >0 and <1",
             info = "fdZABNB rejects tau = 0")
expect_error(fdZABNB(1, 1, 1, 1, 1), "tau must be >0 and <1",
             info = "fdZABNB rejects tau = 1")
expect_error(fpZABNB(1, -1, 1, 1, 0.1), "mu must be greater than 0",
             info = "fpZABNB rejects mu < 0")
expect_error(fpZABNB(1, 1, 1, 1, 1.5), "tau must be >0 and <1",
             info = "fpZABNB rejects tau > 1")

# =============================================================================
# NUMERICAL STABILITY
# =============================================================================

# A tiny mu drives f_BNB(0) -> 1; the naive log(1 - exp(log f0)) of the
# gamlss.dist formula underflows to -Inf there, the log(-expm1(.)) form does not.
d_small <- fdZABNB(1:3, mu = 1e-8, sigma = 1, nu = 1, tau = 0.1)
expect_true(all(is.finite(d_small)) && all(d_small >= 0 & d_small <= 1),
            info = "fdZABNB stays finite and in [0, 1] at mu = 1e-8")

# A tiny tau must not lose the (1 - tau) factor
expect_true(is.finite(fdZABNB(1, 2, 1, 1, 1e-15)),
            info = "fdZABNB stays finite at tau = 1e-15")
expect_true(all(is.finite(fpZABNB(0:5, 2, 1, 1, 1e-15))),
            info = "fpZABNB stays finite at tau = 1e-15")

# x / q beyond the range of int cannot be converted without undefined
# behaviour (x86 saturates to INT_MIN and silently reports a CDF of 0 where the
# truth is ~1; AArch64 saturates to INT_MAX and would loop about two billion
# times). Both must report NA instead. gamlss.dist::pZABNB simply hangs here.
expect_true(is.na(fdZABNB(2^31, 2, 1, 1, 0.1)),
            info = "fdZABNB x above INT_MAX -> NA")
expect_true(is.na(fpZABNB(2^31, 2, 1, 1, 0.1)),
            info = "fpZABNB q above INT_MAX -> NA")
expect_true(is.na(fdZABNB(1e10, 2, 1, 1, 0.1)),
            info = "fdZABNB huge x -> NA")
expect_true(is.na(fpZABNB(Inf, 2, 1, 1, 0.1)),
            info = "fpZABNB infinite q -> NA")
# INT_MAX itself is excluded from both, because the underlying BNB scalars
# overflow their own int arithmetic exactly there: fpBNB_scalar accumulates with
# `for (int i = 0; i <= q; i++)` so the loop never terminates, and fdBNB_scalar
# evaluates lgammafn(x + 1) so the density collapses to 0 (true value ~2.2e-27).
expect_true(is.na(fpZABNB(.Machine$integer.max, 2, 1, 1, 0.1)),
            info = "fpZABNB q == INT_MAX -> NA (the 0..q loop would not terminate)")
expect_true(is.na(fdZABNB(.Machine$integer.max, 2, 1, 1, 0.1)),
            info = "fdZABNB x == INT_MAX -> NA (lgammafn(x + 1) overflows there)")
# Just below the boundary both are still computed, and match gamlss.dist
expect_equal(fdZABNB(.Machine$integer.max - 1, 2, 1, 1, 0.1),
             gamlss.dist::dZABNB(.Machine$integer.max - 1, 2, 1, 1, 0.1),
             tolerance = tolerance,
             info = "fdZABNB x == INT_MAX - 1 still matches gamlss.dist")

# The CDF must be monotone non-decreasing and bounded by [0, 1]
cdf_mono <- fpZABNB(0:50, mu = 3, sigma = 1.5, nu = 0.5, tau = 0.2)
expect_false(is.unsorted(cdf_mono), info = "fpZABNB is monotone non-decreasing")
expect_true(all(cdf_mono >= 0 & cdf_mono <= 1), info = "fpZABNB stays in [0, 1]")

# cat("All ZABNB distribution tests completed successfully!\n")
