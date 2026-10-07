# Test ZINBI distribution functions
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
# ZERO-INFLATED NBI TESTS
# =============================================================================

# Test 19: ZINBI PDF basic correctness
pdf_zinbi_ck <- fdZINBI(data$x, data$mu, data$sigma, data$nu)
# gamlss.dist 6.1-11's dZINBI is wrong when the parameters vary along the
# vectors (rel. diff 1.86 on this data); its scalar calls are right. The
# reference is the zero-inflation identity on base R's negative binomial
# (NBI(mu, sigma) = NB(size = 1/sigma, mu)), which fdZINBI meets to 1e-16.
pdf_zinbi_gamlss <- ifelse(data$x == 0, data$nu, 0) +
  (1 - data$nu) * dnbinom(data$x, size = 1 / data$sigma, mu = data$mu)
expect_equal(
    pdf_zinbi_ck,
    pdf_zinbi_gamlss,
    tolerance = tolerance,
    info = "ZINBI PDF should match gamlss.dist dZINBI"
)

# Test 20: ZINBI CDF basic correctness
cdf_zinbi_ck <- fpZINBI(data$q, data$mu, data$sigma, data$nu)
cdf_zinbi_gamlss <- pZINBI(data$q, data$mu, data$sigma, data$nu)
expect_equal(
    cdf_zinbi_ck,
    cdf_zinbi_gamlss,
    tolerance = tolerance,
    info = "ZINBI CDF should match gamlss.dist pZINBI"
)

# Test 21: ZINBI quantile basic correctness
q_zinbi_ck <- fqZINBI(data$p, data$mu, data$sigma, data$nu)
q_zinbi_gamlss <- qZINBI(data$p, data$mu, data$sigma, data$nu)
expect_equal(
    q_zinbi_ck,
    q_zinbi_gamlss,
    tolerance = tolerance,
    info = "ZINBI quantile should match gamlss.dist qZINBI"
)

# Test 22: ZINBI random generation
set.seed(123)
r_zinbi_ck <- frZINBI(100, mu = 2, sigma = 1, nu = 0.1)
expect_equal(
    length(r_zinbi_ck),
    100,
    info = "ZINBI random generation should return correct length"
)
expect_true(
    all(r_zinbi_ck >= 0),
    info = "ZINBI random variates should be non-negative"
)

# Test 23: ZINBI parameter validation - nu out of bounds
expect_error(
    fdZINBI(c(0, 1, 2), mu = 1, sigma = 1, nu = 0),
    info = "fdZINBI should error on nu = 0"
)
expect_error(
    fdZINBI(c(0, 1, 2), mu = 1, sigma = 1, nu = 1),
    info = "fdZINBI should error on nu = 1"
)

# =============================================================================
# A QUANTILE AN INT CANNOT HOLD IS NA, WITH A WARNING (see test-fNBI.R)
#
# fqZINBI inverts through fqNBI_scalar, which returned the double from R's quantile
# function through an implicit int conversion: undefined behaviour for Inf (mu = Inf),
# or for a value above INT_MAX, and only an accidental, silent NA on x86-64.
# =============================================================================
expect_warning(res_muinf <- fqZINBI(0.5, mu = Inf, sigma = 1, nu = 0.3), "NAs produced",
               info = "fqZINBI(mu = Inf) warns")
expect_true(is.na(res_muinf), info = "fqZINBI(mu = Inf) is NA")

# a finite quantile above INT_MAX (the NBI part is about 3.4e9 at mu = 1e10, sigma = 1)
expect_warning(res_big <- fqZINBI(0.5, mu = 1e10, sigma = 1, nu = 0.3), "NAs produced",
               info = "fqZINBI warns for a quantile above INT_MAX")
expect_true(is.na(res_big), info = "fqZINBI above INT_MAX is NA")

# One warning per call, and the other elements are untouched
n_warn <- 0L
res_vec <- withCallingHandlers(
  fqZINBI(c(0.5, 0.5, 0.9), mu = c(5, Inf, 5), sigma = 0.5, nu = 0.3),
  warning = function(w) {
    n_warn <<- n_warn + 1L
    invokeRestart("muffleWarning")
  })
expect_identical(is.na(res_vec), c(FALSE, TRUE, FALSE),
                 info = "fqZINBI: only the mu = Inf element is NA")
expect_equal(n_warn, 1L, info = "fqZINBI warns once per call")

# Guards that hold with or without the fix. p = 1 is NOT an infinite quantile for
# the zero-inflated quantile (it inverts at 1 - CK_P_SLACK / (1 - nu), a finite
# value), and it must stay that way: callers rely on the finite value. It was 77
# while the transform subtracted 1e-10 (the NBI quantile at 1 - 1e-10, a cap); with
# only the rounding slack taken off it is the NBI quantile a few ulp short of 1 (110
# when this was written). The value is not pinned: finite, and not below what it was.
res_p1 <- NA_integer_   # expect_silent() assigns nothing when the call warns: then this fails, instead of an error
expect_silent(res_p1 <- fqZINBI(1, 5, .5, .3))
expect_true(!is.na(res_p1) && res_p1 >= 77L,
            info = "fqZINBI(p = 1) keeps a finite value, not below the old 77, silently")
expect_silent(res_na <- fqZINBI(NaN, 5, .5, .3))
expect_true(is.na(res_na), info = "fqZINBI NaN p is NA, silently")

# =============================================================================
# UPPER TAIL: (1 - nu) * S_NBI(q), NOT 1 - F
#
# fpZINBI computed lower_tail = FALSE as 1 - F. F rounds to 1 once the tail is
# below about 1e-16, so the upper tail came out as 0, or wrong by orders of
# magnitude, where it is 1e-20 or smaller.
#
# The references are closed forms on base R's negative binomial
# (NBI(mu, sigma) = NB(size = 1 / sigma, mu)), compared as a RATIO to the
# reference: expect_equal() is relative only while the reference exceeds the
# tolerance. Below that all.equal() takes an absolute difference, and a result
# of 0 would pass against a reference of 1e-28.
# =============================================================================
ref_up <- 0.7 * pnbinom(200, size = 2, mu = 5, lower.tail = FALSE)
expect_true(ref_up > 1e-300 && ref_up < 1e-20,
            info = "the reference upper tail is far below the resolution of 1 - F")
expect_equal(fpZINBI(200, 5, .5, .3, lower_tail = FALSE) / ref_up, 1, tolerance = 1e-12,
             info = "fpZINBI upper tail at q = 200 is 1.7e-28, not 0")
expect_equal(fpZINBI(200, 5, .5, .3, lower_tail = FALSE, log_p = TRUE), log(ref_up),
             tolerance = 1e-12, info = "fpZINBI log upper tail at q = 200")

# the Poisson branch (sigma < 1e-4) takes its tail from ppois
ref_up_pois <- 0.7 * ppois(200, 5, lower.tail = FALSE)
expect_equal(fpZINBI(200, 5, 1e-5, .3, lower_tail = FALSE) / ref_up_pois, 1, tolerance = 1e-12,
             info = "fpZINBI upper tail, Poisson branch")
expect_equal(fpZINBI(200, 5, 1e-5, .3, lower_tail = FALSE, log_p = TRUE), log(ref_up_pois),
             tolerance = 1e-12, info = "fpZINBI log upper tail, Poisson branch")

# a grid across the NBI regimes: the log tail never underflows, so it is compared
# everywhere; the plain tail where the reference is representable
g <- expand.grid(q = c(0L, 1L, 2L, 5L, 20L, 80L, 200L), mu = c(0.5, 5, 50),
                 sigma = c(0.05, 0.5, 2), nu = c(0.05, 0.5, 0.9))
g_size <- 1 / g$sigma
g_ref <- (1 - g$nu) * pnbinom(g$q, size = g_size, mu = g$mu, lower.tail = FALSE)
g_ref_log <- log1p(-g$nu) +
  pnbinom(g$q, size = g_size, mu = g$mu, lower.tail = FALSE, log.p = TRUE)
expect_equal(fpZINBI(g$q, g$mu, g$sigma, g$nu, lower_tail = FALSE, log_p = TRUE), g_ref_log,
             tolerance = 1e-12, info = "fpZINBI log upper tail on a grid")
g_ok <- g_ref > 1e-300
expect_true(sum(g_ok) > 100, info = "the grid has representable plain upper tails")
expect_equal(fpZINBI(g$q, g$mu, g$sigma, g$nu, lower_tail = FALSE)[g_ok] / g_ref[g_ok],
             rep(1, sum(g_ok)), tolerance = 1e-12, info = "fpZINBI upper tail on a grid")

# A tiny mu. With sigma = 1 the NBI is geometric, so for q >= 0
# P(X > q) = (1 - nu) * (mu / (1 + mu))^(q + 1) exactly: a reference that needs
# no subtraction at any mu.
gm <- expand.grid(q = 0:2, mu = c(1e-17, 1e-12, 1e-6))
gm_ref <- 0.5 * (gm$mu / (1 + gm$mu))^(gm$q + 1)
expect_equal(fpZINBI(gm$q, gm$mu, 1, 0.5, lower_tail = FALSE) / gm_ref, rep(1, nrow(gm)),
             tolerance = 1e-12, info = "fpZINBI upper tail at a tiny mu (was 0 or noise)")
expect_equal(fpZINBI(gm$q, gm$mu, 1, 0.5, lower_tail = FALSE, log_p = TRUE), log(gm_ref),
             tolerance = 1e-12, info = "fpZINBI log upper tail at a tiny mu")

# Guards that hold with or without the fix: where both tails are well conditioned
# they sum to 1
lo <- fpZINBI(data$q, data$mu, data$sigma, data$nu)
up <- fpZINBI(data$q, data$mu, data$sigma, data$nu, lower_tail = FALSE)
expect_equal(lo + up, rep(1, length(lo)), tolerance = 1e-12,
             info = "fpZINBI: the two tails sum to 1")
