# Test ZISICHEL distribution functions
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

small_data <- generate_test_data(n = 20, seed = 789)

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


# Test 22: Error handling for invalid tau in ZI functions
expect_error(fqZISICHEL(c(0.5), mu = 1, sigma = 1, nu = 0, tau = -0.1),
             info = "Should error on negative tau")

expect_error(fqZISICHEL(c(0.5), mu = 1, sigma = 1, nu = 0, tau = 1.1),
             info = "Should error on tau > 1")


# =============================================================================
# RANDOM GENERATION TESTS
# =============================================================================

# Test 27: Zero-inflated random generation
set.seed(789)
r_zi_ck <- frZISICHEL(100, mu = 2, sigma = 1, nu = -0.5, tau = 0.3)
expect_true(length(r_zi_ck) == 100, info = "Should generate correct number of ZI values")
zero_prop <- sum(r_zi_ck == 0) / length(r_zi_ck)
expect_true(zero_prop > 0.2, info = "ZI random generation should produce excess zeros")


# Final success message
# cat("All ZISICHEL distribution tests passed successfully!\n")



# =============================================================================
# Density (added in 0.1.30, completing ZISICHEL's d/p/q/r), and a regression
# guard on the fdSICHEL_scalar extraction that fdZISICHEL is built on.
# =============================================================================
if (requireNamespace("gamlss.dist", quietly = TRUE)) {
  elt_zs <- function(FUN, x, ...) {
    a <- list(...)
    vapply(seq_along(x), function(i)
      do.call(FUN, c(list(x[i]), lapply(a, `[`, i))), numeric(1))
  }
  set.seed(2026)
  n_zs   <- 80L
  x_zs   <- sample(0:15, n_zs, TRUE)
  mu_zs  <- runif(n_zs, 0.5, 5);  sg_zs  <- runif(n_zs, 0.1, 2)
  nu_zs  <- runif(n_zs, -1.5, 1.5); tau_zs <- runif(n_zs, 0.01, 0.8)
  tol_zs <- sqrt(.Machine$double.eps)
  # gamlss.dist 6.1-11's dZISICHEL is wrong (off by up to 1; its pZISICHEL and
  # dSICHEL are right). The density reference is the zero-inflation identity
  # on gamlss.dist's dSICHEL: P(0) = tau + (1 - tau) f(0), P(y) = (1 - tau) f(y).
  zis_ref <- ifelse(x_zs == 0, tau_zs, 0) +
    (1 - tau_zs) * elt_zs(gamlss.dist::dSICHEL, x_zs, mu_zs, sg_zs, nu_zs)

  expect_equal(fdZISICHEL(x_zs, mu_zs, sg_zs, nu_zs, tau_zs),
               zis_ref,
               tolerance = tol_zs, info = "fdZISICHEL matches gamlss.dist dZISICHEL")
  expect_equal(fdZISICHEL(x_zs, mu_zs, sg_zs, nu_zs, tau_zs, log = TRUE),
               log(zis_ref),
               tolerance = tol_zs, info = "fdZISICHEL log scale matches")

  # fdSICHEL now delegates to the extracted fdSICHEL_scalar; it must be unchanged
  expect_equal(fdSICHEL(x_zs, mu_zs, sg_zs, nu_zs),
               elt_zs(gamlss.dist::dSICHEL, x_zs, mu_zs, sg_zs, nu_zs),
               tolerance = tol_zs,
               info = "fdSICHEL unchanged by the fdSICHEL_scalar extraction")
  # fpZISICHEL now delegates to fpZISICHEL_scalar; likewise unchanged
  expect_equal(fpZISICHEL(x_zs, mu_zs, sg_zs, nu_zs, tau_zs),
               elt_zs(gamlss.dist::pZISICHEL, x_zs, mu_zs, sg_zs, nu_zs, tau_zs),
               tolerance = tol_zs,
               info = "fpZISICHEL unchanged by the fpZISICHEL_scalar extraction")

  for (i in 1:5)
    expect_equal(cumsum(fdZISICHEL(0:40, mu_zs[i], sg_zs[i], nu_zs[i], tau_zs[i])),
                 fpZISICHEL(0:40, mu_zs[i], sg_zs[i], nu_zs[i], tau_zs[i]),
                 tolerance = 1e-10,
                 info = paste0("ZISICHEL cumsum(pmf) == cdf, set ", i))

  z_zs <- fdZISICHEL(rep(0, 10), mu_zs[1:10], sg_zs[1:10], nu_zs[1:10], tau_zs[1:10])
  expect_true(all(z_zs > tau_zs[1:10]),
              info = "ZISICHEL P(0) > tau (zero inflation)")

  expect_equal(fdZISICHEL(0:3, c(1, 2), 1, -0.5, 0.1),
               fdZISICHEL(0:3, rep(c(1, 2), 2), rep(1, 4), rep(-0.5, 4), rep(0.1, 4)),
               info = "fdZISICHEL parameter recycling")
  expect_true(is.na(suppressWarnings(fdZISICHEL(NA_real_, 1, 1, -0.5, 0.1))),
              info = "fdZISICHEL NA x -> NA")
  expect_true(is.na(suppressWarnings(fdZISICHEL(4e9, 1, 1, -0.5, 0.1))),
              info = "fdZISICHEL unrepresentable x -> NA")
  expect_error(fdZISICHEL(-1, 1, 1, -0.5, 0.1), "x must be >=0",
               info = "fdZISICHEL rejects x < 0")
  expect_error(fdZISICHEL(1, 1, 1, -0.5, 1), "tau must be between 0 and 1",
               info = "fdZISICHEL rejects tau = 1")
  expect_equal(length(fdZISICHEL(numeric(0), 1, 1, -0.5, 0.1)), 0L,
               info = "fdZISICHEL zero-length x recycles to length 0")
}

# =============================================================================
# A large q: O(1) memory, and the CDF sum stops once it has settled
# =============================================================================
# fpZISICHEL delegates to fcdfSICHEL_scalar, which used to store the whole
# Bessel-ratio recursion in two std::vector<double>s of q + 1 elements (16 bytes
# per unit of q) and to add all q + 1 terms: fpZISICHEL(5e7, mu = 2, ...) took
# ~0.8 s and ~800 MB for a sum that settles after a few hundred terms. It now
# holds one step of the recursion and returns once a term past the mode leaves
# the sum unchanged. The value is the same double either way, so the CDF at 5e7
# is the CDF at 1e4, where the sum has long settled, bit for bit. The time bound
# is a timing expectation, so it runs only when at_home(): CRAN never runs it.
el <- system.time(v <- fpZISICHEL(5e7, mu = 2, sigma = 1, nu = -0.5, tau = 0.1))[["elapsed"]]
expect_identical(v, fpZISICHEL(1e4, mu = 2, sigma = 1, nu = -0.5, tau = 0.1),
                 info = "fpZISICHEL at q = 5e7 is the CDF at q = 1e4, bit for bit")
expect_equal(v, 1, tolerance = 1e-12,
             info = "fpZISICHEL at q = 5e7 is 1 (the mass is all below q)")
if (at_home()) {
  expect_true(el < 0.2,
              info = "fpZISICHEL(5e7, mu = 2) in under 0.2 s (the sum stops once settled)")
}
