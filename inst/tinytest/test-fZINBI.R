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
# the zero-inflated quantile (it inverts at 1 - nu-adjusted - 1e-10, a finite
# value), and it must stay that way: callers rely on the finite value.
expect_silent(res_p1 <- fqZINBI(1, 5, .5, .3))
expect_identical(res_p1, 77L, info = "fqZINBI(p = 1) keeps its finite value, silently")
expect_silent(res_na <- fqZINBI(NaN, 5, .5, .3))
expect_true(is.na(res_na), info = "fqZINBI NaN p is NA, silently")
