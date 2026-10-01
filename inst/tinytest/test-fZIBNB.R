# Test ZIBNB distribution functions
# Using tinytest framework

# Skip tests if gamlss.dist is not available
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
  exit_file("gamlss.dist not available - skipping ZIBNB validation tests")
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
    nu = runif(n, 0.1, 3)
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
    nu = runif(n, 0.1, 2)
  )
}

# Generate test data
data <- generate_test_data()
edge_data <- generate_edge_case_data()

# Test 7: Quantile basic correctness (subset for speed)
test_indices <- sample(length(data$p), 20)
p_test <- data$p[test_indices]
mu_test <- data$mu[test_indices]
sigma_test <- data$sigma[test_indices]
nu_test <- data$nu[test_indices]


# =============================================================================
# ZERO INFLATED TESTS
# =============================================================================

# Generate tau parameter
tau_test <- runif(20, 0.01, 0.3)

# Test 9: ZIBNB quantile
qtl_zi_ck <- fqZIBNB(p_test, mu_test, sigma_test, nu_test, tau_test)
# gamlss.dist 6.1-11's qZIBNB is not the inverse of its own pZIBNB (wrong
# on ~40% of draws); pZIBNB is right. The quantile reference is the definition,
# min{y : F(y) >= p}, on gamlss.dist's CDF.
qtl_zi_ref <- vapply(seq_along(p_test), function(i) {
  y <- 0
  while (gamlss.dist::pZIBNB(y, mu_test[i], sigma_test[i], nu_test[i], tau_test[i]) < p_test[i] - 1e-12 && y < 1e6) y <- y + 1
  y
}, 0)

expect_equal(
  qtl_zi_ck,
  qtl_zi_ref,
  info = "ZIBNB: Quantile test"
)



# =============================================================================
# PARAMETER RECYCLING TESTS
# =============================================================================




# Test 14: Quantile parameter recycling with 5 parameters (ZIBNB)
p_vec <- c(0.1, 0.3, 0.5, 0.7)
mu_vec <- c(1, 2)
sigma_single <- 0.5
nu_vec <- c(1, 1.5)
tau_single <- 0.1

qtl_recycled <- fqZIBNB(p_vec, mu_vec, sigma_single, nu_vec, tau_single)
qtl_expected <- fqZIBNB(p_vec, rep(mu_vec, length.out = 4), 
                        rep(sigma_single, 4), rep(nu_vec, length.out = 4),
                        rep(tau_single, 4))

expect_equal(
  qtl_recycled,
  qtl_expected,
  info = "ZIBNB: Parameter recycling test"
)

# =============================================================================
# PERFORMANCE AND EDGE CASE TESTS
# =============================================================================



# cat("All ZIBNB distribution tests completed successfully!\n")


# =============================================================================
# Density and CDF (added in 0.1.30, completing ZIBNB's d/p/q/r)
#
# gamlss.dist's *BNB functions mis-recycle several parameter vectors supplied
# together, so reference values are computed one element at a time.
# =============================================================================
if (requireNamespace("gamlss.dist", quietly = TRUE)) {
  elt <- function(FUN, x, ...) {
    a <- list(...)
    vapply(seq_along(x), function(i)
      do.call(FUN, c(list(x[i]), lapply(a, `[`, i))), numeric(1))
  }
  set.seed(99)
  n_zi  <- 100L
  x_zi  <- sample(0:20, n_zi, TRUE)
  mu_zi <- runif(n_zi, 0.5, 5); sg_zi <- runif(n_zi, 0.1, 2)
  nu_zi <- runif(n_zi, 0.1, 3); tau_zi <- runif(n_zi, 0.01, 0.8)
  tol_zi <- sqrt(.Machine$double.eps)

  expect_equal(fdZIBNB(x_zi, mu_zi, sg_zi, nu_zi, tau_zi),
               elt(gamlss.dist::dZIBNB, x_zi, mu_zi, sg_zi, nu_zi, tau_zi),
               tolerance = tol_zi, info = "fdZIBNB matches gamlss.dist dZIBNB")
  expect_equal(fdZIBNB(x_zi, mu_zi, sg_zi, nu_zi, tau_zi, log = TRUE),
               log(elt(gamlss.dist::dZIBNB, x_zi, mu_zi, sg_zi, nu_zi, tau_zi)),
               tolerance = tol_zi, info = "fdZIBNB log scale matches")
  expect_equal(fpZIBNB(x_zi, mu_zi, sg_zi, nu_zi, tau_zi),
               elt(gamlss.dist::pZIBNB, x_zi, mu_zi, sg_zi, nu_zi, tau_zi),
               tolerance = tol_zi, info = "fpZIBNB matches gamlss.dist pZIBNB")
  expect_equal(fpZIBNB(x_zi, mu_zi, sg_zi, nu_zi, tau_zi, lower_tail = FALSE),
               1 - elt(gamlss.dist::pZIBNB, x_zi, mu_zi, sg_zi, nu_zi, tau_zi),
               tolerance = tol_zi, info = "fpZIBNB upper tail matches")

  # cumsum of the pmf must reproduce the cdf
  for (i in 1:5)
    expect_equal(cumsum(fdZIBNB(0:50, mu_zi[i], sg_zi[i], nu_zi[i], tau_zi[i])),
                 fpZIBNB(0:50, mu_zi[i], sg_zi[i], nu_zi[i], tau_zi[i]),
                 tolerance = 1e-10,
                 info = paste0("ZIBNB cumsum(pmf) == cdf, set ", i))

  # Inflation, not a hurdle: the zero mass EXCEEDS tau. This is the property
  # that distinguishes ZIBNB from ZABNB, where P(0) is exactly tau.
  z_zi <- fdZIBNB(rep(0, 10), mu_zi[1:10], sg_zi[1:10], nu_zi[1:10], tau_zi[1:10])
  expect_true(all(z_zi > tau_zi[1:10]),
              info = "ZIBNB P(0) > tau (zero inflation adds to the BNB zero mass)")
  expect_identical(fdZABNB(rep(0, 10), mu_zi[1:10], sg_zi[1:10], nu_zi[1:10], tau_zi[1:10]),
                   tau_zi[1:10],
                   info = "ZABNB P(0) == tau exactly (the hurdle contrast)")

  # Recycling, NA and validation
  expect_equal(fdZIBNB(0:3, c(1, 2), 0.5, c(1, 1.5), 0.1),
               fdZIBNB(0:3, rep(c(1, 2), 2), rep(0.5, 4), rep(c(1, 1.5), 2), rep(0.1, 4)),
               info = "fdZIBNB parameter recycling")
  expect_equal(fpZIBNB(0:3, c(1, 2), 0.5, c(1, 1.5), 0.1),
               fpZIBNB(0:3, rep(c(1, 2), 2), rep(0.5, 4), rep(c(1, 1.5), 2), rep(0.1, 4)),
               info = "fpZIBNB parameter recycling")
  expect_true(is.na(fdZIBNB(NA_real_, 1, 1, 1, 0.1)), info = "fdZIBNB NA x -> NA")
  expect_true(is.na(fpZIBNB(NA_real_, 1, 1, 1, 0.1)), info = "fpZIBNB NA q -> NA")
  expect_true(is.na(fdZIBNB(4e9, 1, 1, 1, 0.1)), info = "fdZIBNB unrepresentable x -> NA")
  expect_true(is.na(fpZIBNB(4e9, 1, 1, 1, 0.1)), info = "fpZIBNB unrepresentable q -> NA")
  expect_error(fdZIBNB(-1, 1, 1, 1, 0.1), "x must be >=0", info = "fdZIBNB rejects x < 0")
  expect_error(fpZIBNB(-1, 1, 1, 1, 0.1), "q must be >=0", info = "fpZIBNB rejects q < 0")
  expect_error(fdZIBNB(1, 1, 1, 1, 0), "tau must be >0 and <1", info = "fdZIBNB rejects tau = 0")
  expect_error(fdZIBNB(1, 0, 1, 1, 0.1), "mu must be greater than 0", info = "fdZIBNB rejects mu = 0")
  expect_equal(length(fdZIBNB(numeric(0), 1, 1, 1, 0.1)), 0L,
               info = "fdZIBNB zero-length x recycles to length 0")
}
