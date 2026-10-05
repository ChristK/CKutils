# Test ZANBI distribution functions
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

# NOTE: gamlss.dist has some vectorization bugs. I.e. 
# all.equal(
#     gamlss.dist::dZANBI(18, 1, 0.5, 0.5),
#     gamlss.dist::dZANBI(17:18, c(2, 1), c(1, 0.5), c(0.5, 0.5))[2]
# ) # "Mean relative difference: 0.1666667"

# To avoid the bug I will test individual calls instead of vectorized ones.

# =============================================================================
# ZERO-ALTERED NBI TESTS
# =============================================================================

# Test 24: ZANBI PDF basic correctness
# Use vectorized fdZANBI vs sapply for gamlss.dist to avoid vectorization bugs
pdf_zanbi_ck <- fdZANBI(data$x, data$mu, data$sigma, data$nu)
pdf_zanbi_gamlss <- sapply(seq_len(length(data$x)), function(i) {
    dZANBI(data$x[i], data$mu[i], data$sigma[i], data$nu[i])
})
expect_equal(
    pdf_zanbi_ck,
    pdf_zanbi_gamlss,
    tolerance = tolerance,
    info = "ZANBI PDF should match gamlss.dist dZANBI (vectorized vs sapply)"
)

# Test 25: ZANBI CDF basic correctness
# Use vectorized fpZANBI vs sapply for gamlss.dist to avoid vectorization bugs
cdf_zanbi_ck <- fpZANBI(data$q, data$mu, data$sigma, data$nu)
cdf_zanbi_gamlss <- sapply(seq_len(length(data$x)), function(i) {
    pZANBI(data$q[i], data$mu[i], data$sigma[i], data$nu[i])
})
expect_equal(
    cdf_zanbi_ck,
    cdf_zanbi_gamlss,
    tolerance = tolerance,
    info = "ZANBI CDF should match gamlss.dist pZANBI (vectorized vs sapply)"
)

# Test 26: ZANBI quantile basic correctness
# Use vectorized fqZANBI vs sapply for gamlss.dist to be consistent
q_zanbi_ck <- fqZANBI(data$p, data$mu, data$sigma, data$nu)
q_zanbi_gamlss <- sapply(seq_len(length(data$x)), function(i) {
    qZANBI(data$p[i], data$mu[i], data$sigma[i], data$nu[i])
})
expect_equal(
    q_zanbi_ck,
    q_zanbi_gamlss,
    tolerance = tolerance,
    info = "ZANBI quantile should match gamlss.dist qZANBI (vectorized vs sapply)"
)

# Test 27: ZANBI random generation
set.seed(123)
r_zanbi_ck <- frZANBI(100, mu = 2, sigma = 1, nu = 0.1)
expect_equal(
    length(r_zanbi_ck),
    100,
    info = "ZANBI random generation should return correct length"
)
expect_true(
    all(r_zanbi_ck >= 0),
    info = "ZANBI random variates should be non-negative"
)

# Test 28: ZANBI parameter validation - nu out of bounds
expect_error(
    fdZANBI(c(0, 1, 2), mu = 1, sigma = 1, nu = 0),
    info = "fdZANBI should error on nu = 0"
)
expect_error(
    fdZANBI(c(0, 1, 2), mu = 1, sigma = 1, nu = 1),
    info = "fdZANBI should error on nu = 1"
)

# =============================================================================
# A QUANTILE AN INT CANNOT HOLD IS NA, WITH A WARNING (see test-fNBI.R)
#
# fqZANBI inverts through fqNBI_scalar, which returned the double from R's quantile
# function through an implicit int conversion: undefined behaviour for Inf (mu = Inf),
# or for a value above INT_MAX, and only an accidental, silent NA on x86-64.
# =============================================================================
expect_warning(res_muinf <- fqZANBI(0.5, mu = Inf, sigma = 1, nu = 0.3), "NAs produced",
               info = "fqZANBI(mu = Inf) warns")
expect_true(is.na(res_muinf), info = "fqZANBI(mu = Inf) is NA")

# a finite quantile above INT_MAX (the NBI part is about 3.4e9 at mu = 1e10, sigma = 1)
expect_warning(res_big <- fqZANBI(0.5, mu = 1e10, sigma = 1, nu = 0.3), "NAs produced",
               info = "fqZANBI warns for a quantile above INT_MAX")
expect_true(is.na(res_big), info = "fqZANBI above INT_MAX is NA")

# One warning per call, and the other elements are untouched
n_warn <- 0L
res_vec <- withCallingHandlers(
  fqZANBI(c(0.5, 0.5, 0.9), mu = c(5, Inf, 5), sigma = 0.5, nu = 0.3),
  warning = function(w) {
    n_warn <<- n_warn + 1L
    invokeRestart("muffleWarning")
  })
expect_identical(is.na(res_vec), c(FALSE, TRUE, FALSE),
                 info = "fqZANBI: only the mu = Inf element is NA")
expect_equal(n_warn, 1L, info = "fqZANBI warns once per call")

# Guards that hold with or without the fix. p = 1 is NOT an infinite quantile for
# the zero-altered quantile (it inverts at a probability 1e-10 short of 1, a finite
# value), and it must stay that way: IMPACTncd's C++ relies on the finite value.
expect_silent(res_p1 <- fqZANBI(1, 5, .5, .3))
expect_identical(res_p1, 78L, info = "fqZANBI(p = 1) keeps its finite value, silently")
expect_silent(res_na <- fqZANBI(NaN, 5, .5, .3))
expect_true(is.na(res_na), info = "fqZANBI NaN p is NA, silently")

# =============================================================================
# UPPER TAIL: (1 - nu) * S_NBI(q) / (1 - F_NBI(0)), NOT 1 - F
#
# fpZANBI computed lower_tail = FALSE as 1 - F. F rounds to 1 once the tail is
# below about 1e-16, so the upper tail came out as 0, or wrong by orders of
# magnitude, where it is 1e-20 or smaller. And at a tiny mu F_NBI(0) rounds to 1,
# and the lower tail's (F_NBI(q) - F_NBI(0)) / (1 - F_NBI(0)) was 0/0 = NaN.
#
# The references are closed forms on base R's negative binomial
# (NBI(mu, sigma) = NB(size = 1 / sigma, mu)), compared as a RATIO to the
# reference: expect_equal() is relative only while the reference exceeds the
# tolerance. Below that all.equal() takes an absolute difference, and a result
# of 0 would pass against a reference of 1e-28.
# =============================================================================
ref_up <- 0.7 * pnbinom(200, size = 2, mu = 5, lower.tail = FALSE) /
  (1 - dnbinom(0, size = 2, mu = 5))
expect_true(ref_up > 1e-300 && ref_up < 1e-20,
            info = "the reference upper tail is far below the resolution of 1 - F")
expect_equal(fpZANBI(200, 5, .5, .3, lower_tail = FALSE) / ref_up, 1, tolerance = 1e-12,
             info = "fpZANBI upper tail at q = 200 is 1.9e-28, not 0")
expect_equal(fpZANBI(200, 5, .5, .3, lower_tail = FALSE, log_p = TRUE), log(ref_up),
             tolerance = 1e-12, info = "fpZANBI log upper tail at q = 200")

# the Poisson branch (sigma < 1e-4) takes its tail from ppois
ref_up_pois <- 0.7 * ppois(200, 5, lower.tail = FALSE) / (1 - dpois(0, 5))
expect_equal(fpZANBI(200, 5, 1e-5, .3, lower_tail = FALSE) / ref_up_pois, 1, tolerance = 1e-12,
             info = "fpZANBI upper tail, Poisson branch")
expect_equal(fpZANBI(200, 5, 1e-5, .3, lower_tail = FALSE, log_p = TRUE), log(ref_up_pois),
             tolerance = 1e-12, info = "fpZANBI log upper tail, Poisson branch")

# a grid across the NBI regimes: the log tail never underflows, so it is compared
# everywhere; the plain tail where the reference is representable
g <- expand.grid(q = c(1L, 2L, 5L, 20L, 80L, 200L), mu = c(0.5, 5, 50),
                 sigma = c(0.05, 0.5, 2), nu = c(0.05, 0.5, 0.9))
g_size <- 1 / g$sigma
g_f0 <- dnbinom(0, size = g_size, mu = g$mu)
g_ref <- (1 - g$nu) * pnbinom(g$q, size = g_size, mu = g$mu, lower.tail = FALSE) / (1 - g_f0)
g_ref_log <- log1p(-g$nu) +
  pnbinom(g$q, size = g_size, mu = g$mu, lower.tail = FALSE, log.p = TRUE) - log(1 - g_f0)
expect_equal(fpZANBI(g$q, g$mu, g$sigma, g$nu, lower_tail = FALSE, log_p = TRUE), g_ref_log,
             tolerance = 1e-12, info = "fpZANBI log upper tail on a grid")
g_ok <- g_ref > 1e-300
expect_true(sum(g_ok) > 100, info = "the grid has representable plain upper tails")
expect_equal(fpZANBI(g$q, g$mu, g$sigma, g$nu, lower_tail = FALSE)[g_ok] / g_ref[g_ok],
             rep(1, sum(g_ok)), tolerance = 1e-12, info = "fpZANBI upper tail on a grid")

# A tiny mu. With sigma = 1 the zero-truncated NBI is geometric, so for q >= 1
# P(X > q) = (1 - nu) * (mu / (1 + mu))^q exactly: a reference that needs no
# subtraction at any mu.
p_tiny <- fpZANBI(1, 1e-17, 1, 0.5)
expect_false(is.nan(p_tiny), info = "fpZANBI at a tiny mu is not NaN")
expect_true(is.finite(p_tiny) && p_tiny >= 0 && p_tiny <= 1,
            info = "fpZANBI at a tiny mu is a probability")
expect_equal(fpZANBI(0:6, 1e-17, 1, 0.5), c(0.5, rep(1, 6)), tolerance = 1e-12,
             info = "fpZANBI at a tiny mu: nu at 0, then the whole mass (the lower tail is 1)")
gm <- expand.grid(q = 1:3, mu = c(1e-17, 1e-12, 1e-6))
gm_ref <- 0.5 * (gm$mu / (1 + gm$mu))^gm$q
expect_equal(fpZANBI(gm$q, gm$mu, 1, 0.5, lower_tail = FALSE) / gm_ref, rep(1, nrow(gm)),
             tolerance = 1e-12, info = "fpZANBI upper tail at a tiny mu (was 0 or noise)")
expect_equal(fpZANBI(gm$q, gm$mu, 1, 0.5, lower_tail = FALSE, log_p = TRUE), log(gm_ref),
             tolerance = 1e-12, info = "fpZANBI log upper tail at a tiny mu")
# the lower tail where 1 - F_NBI(0) < 1e-3, away from the NaN: it is 1 - P(X > q)
# there. The old ratio of two differences was off by 4e-14 at this point (and by
# 5e-10 at mu = 1e-8, q = 1)
expect_equal(fpZANBI(2L, 1e-5, 1, 0.5), 1 - 0.5 * (1e-5 / (1 + 1e-5))^2, tolerance = 1e-14,
             info = "fpZANBI lower tail where 1 - F_NBI(0) < 1e-3")

# q = 0: P(X > 0) = 1 - nu, in log space log1p(-nu) (log(1 - nu) is 0 for a tiny nu)
expect_equal(fpZANBI(0, 2, 1, 1e-20, lower_tail = FALSE, log_p = TRUE) / -1e-20, 1,
             tolerance = 1e-12, info = "fpZANBI log upper tail at q = 0 keeps a tiny nu")

# Guards that hold with or without the fix: where both tails are well conditioned
# they sum to 1, and the upper tail at q = 0 is 1 - nu
lo <- fpZANBI(data$q, data$mu, data$sigma, data$nu)
up <- fpZANBI(data$q, data$mu, data$sigma, data$nu, lower_tail = FALSE)
expect_equal(lo + up, rep(1, length(lo)), tolerance = 1e-12,
             info = "fpZANBI: the two tails sum to 1")
expect_equal(fpZANBI(0, 2, 1, .3, lower_tail = FALSE), 0.7, tolerance = 1e-14,
             info = "fpZANBI upper tail at q = 0 is 1 - nu")
