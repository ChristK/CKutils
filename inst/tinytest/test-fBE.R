# Tests for the beta distribution in gamlss.dist's BE parametrisation:
# fdBE, fpBE, fqBE (src/distr_BE.cpp, kernels in inst/include/distr_BE.h) and
# frBE (R/rng_distr.R).
#
# The reference is the R code the functions replace (IMPACTncdEngland2026's
# R/beta_dist.R, 2026-11-17): the shapes k = (1 - sigma^2) / sigma^2,
# shape1 = mu * k, shape2 = (1 - mu) * k, handed to stats' dbeta / pbeta / qbeta.
# The C++ must give exactly that, bit for bit, so a model that drew with the R
# code keeps its draws (section A); section B is the control that the comparison
# can tell the two orders of evaluation apart. gamlss.dist (Suggests) is compared
# within a tolerance (section C): it evaluates the shapes in another order.

# A function this build lacks is replaced by one that returns NA, so its checks
# fail rather than abort the file.
ck_fun <- function(nm) get0(nm, envir = asNamespace("CKutils"), inherits = FALSE,
                            ifnotfound = function(...) NA_real_)
fdBE_ <- ck_fun("fdBE"); fpBE_ <- ck_fun("fpBE"); fqBE_ <- ck_fun("fqBE"); frBE_ <- ck_fun("frBE")

# the reference: the R code, operator by operator
ref_shapes <- function(mu, sigma) { k <- (1 - sigma^2) / sigma^2; list(a = mu * k, b = (1 - mu) * k) }
ref_d <- function(x, mu, sigma, log = FALSE) { s <- ref_shapes(mu, sigma); dbeta(x, s$a, s$b, log = log) }
ref_p <- function(q, mu, sigma, lt = TRUE, lp = FALSE) {
  s <- ref_shapes(mu, sigma); pbeta(q, s$a, s$b, lower.tail = lt, log.p = lp)
}
ref_q <- function(p, mu, sigma, lt = TRUE, lp = FALSE) {
  s <- ref_shapes(mu, sigma); qbeta(p, s$a, s$b, lower.tail = lt, log.p = lp)
}

# =============================================================================
# A. Bit for bit the R reference, on the whole parameter space and both tails
#    and scales
# =============================================================================
set.seed(20261118)
n <- 2e5
mu <- runif(n, 1e-4, 1 - 1e-4)
sigma <- runif(n, 1e-3, 1 - 1e-3)
p <- runif(n)
x <- runif(n)
# R's qbeta() warns ("full precision may not have been achieved") where a shape
# is below ~1e-3 (sigma near 1 with mu near 0 or 1); the random draws above reach
# that corner, and the reference warns there exactly as fqBE does (checked at the
# end of this section). The comparisons muffle both sides' warnings.
for (lt in c(TRUE, FALSE)) {
  expect_identical(suppressWarnings(fqBE_(p, mu, sigma, lower_tail = lt)),
                   suppressWarnings(ref_q(p, mu, sigma, lt = lt)),
                   info = paste("fqBE is the R reference bit for bit, lower_tail =", lt))
  expect_identical(suppressWarnings(fqBE_(log(p), mu, sigma, lower_tail = lt, log_p = TRUE)),
                   suppressWarnings(ref_q(log(p), mu, sigma, lt = lt, lp = TRUE)),
                   info = paste("fqBE on the log scale is the R reference, lower_tail =", lt))
  expect_identical(fpBE_(x, mu, sigma, lower_tail = lt), ref_p(x, mu, sigma, lt = lt),
                   info = paste("fpBE is the R reference bit for bit, lower_tail =", lt))
  # pbeta(log.p = TRUE) notes an underflow of its series for very large shapes (both sides)
  expect_identical(suppressWarnings(fpBE_(x, mu, sigma, lower_tail = lt, log_p = TRUE)),
                   suppressWarnings(ref_p(x, mu, sigma, lt = lt, lp = TRUE)),
                   info = paste("fpBE on the log scale is the R reference, lower_tail =", lt))
}
expect_identical(fdBE_(x, mu, sigma), ref_d(x, mu, sigma), info = "fdBE is the R reference bit for bit")
expect_identical(fdBE_(x, mu, sigma, log = TRUE), ref_d(x, mu, sigma, log = TRUE),
                 info = "fdBE on the log scale is the R reference bit for bit")

# the parameter grid of a model table: mu and sigma on a fine grid, p at the
# draws' clamp (eps, 1 - eps) and in the tails
g <- expand.grid(mu = seq(0.005, 0.995, by = 0.005), sigma = seq(0.01, 0.99, by = 0.01))
pg <- c(.Machine$double.eps, 1e-10, 1e-6, 0.001, 0.05, 0.5, 0.95, 0.999, 1 - 1e-6, 1 - .Machine$double.eps)
for (pp in pg) {
  expect_identical(suppressWarnings(fqBE_(pp, g$mu, g$sigma)), suppressWarnings(ref_q(pp, g$mu, g$sigma)),
                   info = paste("fqBE is the R reference on the mu x sigma grid at p =", signif(pp, 3)))
}

# q and x outside [0, 1]: pbeta's / dbeta's own values, on every tail and scale
qo <- c(-1, -1e-300, 0, 1, 1 + 1e-15, 2, -Inf, Inf)
for (lt in c(TRUE, FALSE)) for (lp in c(FALSE, TRUE)) {
  expect_identical(fpBE_(qo, 0.3, 0.4, lower_tail = lt, log_p = lp), ref_p(qo, 0.3, 0.4, lt = lt, lp = lp),
                   info = paste("fpBE outside (0, 1) is pbeta's, lower_tail =", lt, "log_p =", lp))
}
expect_identical(fdBE_(qo, 0.3, 0.4), ref_d(qo, 0.3, 0.4), info = "fdBE outside (0, 1) is dbeta's")
expect_identical(fdBE_(qo, 0.3, 0.4, log = TRUE), ref_d(qo, 0.3, 0.4, log = TRUE),
                 info = "fdBE outside (0, 1) on the log scale is dbeta's (-Inf, not gamlss.dist's 0)")
# p at the ends of [0, 1]
expect_identical(fqBE_(c(0, 1), 0.3, 0.4), ref_q(c(0, 1), 0.3, 0.4), info = "fqBE at p = 0 and 1 is qbeta's")

# qbeta's own warnings come through: where the reference warns, fqBE warns, and
# where it is silent, so is fqBE (the "NaNs produced" check adds none)
warns <- function(expr) { w <- FALSE; withCallingHandlers(expr, warning = function(m) { w <<- TRUE; invokeRestart("muffleWarning") }); w }
cand <- expand.grid(mu = c(0.001, 0.01, 0.5, 0.99, 0.999), sigma = c(0.95, 0.99, 0.999), p = c(0.01, 0.3, 0.7, 0.99))
ref_w <- mapply(function(m, s, pp) warns(ref_q(pp, m, s)), cand$mu, cand$sigma, cand$p)
ck_w  <- mapply(function(m, s, pp) warns(fqBE_(pp, m, s)), cand$mu, cand$sigma, cand$p)
expect_true(any(ref_w), info = "control: some of these rows make R's qbeta warn")
expect_identical(ck_w, ref_w, info = "fqBE warns on exactly the rows where the R reference warns")
expect_false(warns(fqBE_(c(0.1, 0.5, 0.9), 0.6, 0.3)), info = "fqBE is silent on an ordinary row")

# =============================================================================
# B. The control: the comparison above can tell the two orders apart. Find a
#    (mu, sigma) whose shapes differ between the reference order and
#    gamlss.dist's, and check that fqBE follows the reference there.
# =============================================================================
gl_shapes <- function(mu, sigma) { a <- mu * (1 - sigma^2) / (sigma^2); list(a = a, b = a * (1 - mu) / mu) }
rs <- ref_shapes(mu, sigma); gs <- gl_shapes(mu, sigma)
qr_ <- suppressWarnings(qbeta(0.37, rs$a, rs$b)); qg_ <- suppressWarnings(qbeta(0.37, gs$a, gs$b))
w <- which(qr_ != qg_)
expect_true(length(w) > 0, info = "control: some (mu, sigma) give a different quantile under gamlss.dist's order")
if (length(w)) {
  i <- w[1]
  expect_identical(suppressWarnings(fqBE_(0.37, mu[i], sigma[i])), qr_[i],
                   info = "control: at such a row fqBE gives the reference order's quantile")
  expect_false(identical(suppressWarnings(fqBE_(0.37, mu[i], sigma[i])), qg_[i]),
               info = "control: ... and not gamlss.dist's (so section A would see the other order)")
}

# =============================================================================
# C. gamlss.dist's BE, within the tolerance of the order of evaluation
# =============================================================================
if (requireNamespace("gamlss.dist", quietly = TRUE)) {
  m <- 5e4
  mu2 <- runif(m, 0.01, 0.99); s2 <- runif(m, 0.02, 0.95)
  p2 <- runif(m, 1e-8, 1 - 1e-8)   # away from qBE's snapping at 1e-10
  x2 <- runif(m, 1e-6, 1 - 1e-6)   # inside the support (dBE / pBE clip at its ends)
  q_ck <- suppressWarnings(fqBE_(p2, mu2, s2)); q_gl <- suppressWarnings(gamlss.dist::qBE(p2, mu2, s2))
  expect_true(max(abs(q_ck - q_gl)) < 1e-12,
              info = "fqBE agrees with gamlss.dist::qBE within 1e-12 (lower tail, natural scale)")
  expect_true(max(abs(fpBE_(x2, mu2, s2) - gamlss.dist::pBE(x2, mu2, s2))) < 1e-12,
              info = "fpBE agrees with gamlss.dist::pBE within 1e-12")
  d_ck <- fdBE_(x2, mu2, s2); d_gl <- gamlss.dist::dBE(x2, mu2, s2)
  expect_true(max(abs(d_ck / d_gl - 1)[d_gl > 1e-300]) < 1e-9,
              info = "fdBE agrees with gamlss.dist::dBE to a relative 1e-9")
  # the documented differences
  expect_identical(fqBE_(0.2, 0.6, 0.3, lower_tail = FALSE), qbeta(0.2, ref_shapes(0.6, 0.3)$a, ref_shapes(0.6, 0.3)$b, lower.tail = FALSE),
                   info = "fqBE's upper tail is qbeta's")
  expect_true(abs(fqBE_(0.2, 0.6, 0.3, lower_tail = FALSE) - fqBE_(0.8, 0.6, 0.3)) < 1e-12,
              info = "fqBE's upper tail at p is the lower-tail quantile at 1 - p")
  expect_true(fqBE_(1e-12, 0.6, 0.3) > 0, info = "fqBE does not snap a p below 1e-10 to 0 (qBE does)")
}

# =============================================================================
# D. NA and NaN: NA, silently, and an NA parameter is not range-checked
# =============================================================================
expect_identical(fqBE_(0.5, NA, 0.3), NA_real_, info = "fqBE: NA mu gives NA")
expect_identical(fqBE_(0.5, 0.3, NA), NA_real_, info = "fqBE: NA sigma gives NA")
expect_identical(fqBE_(NA, 0.3, 0.3), NA_real_, info = "fqBE: NA p gives NA")
expect_identical(fqBE_(NaN, 0.3, 0.3), NA_real_, info = "fqBE: NaN p gives NA")
expect_identical(fpBE_(0.5, NaN, 0.3), NA_real_, info = "fpBE: NaN mu gives NA")
expect_identical(fdBE_(0.5, 0.3, NA), NA_real_, info = "fdBE: NA sigma gives NA")
expect_identical(fqBE_(c(0.1, 0.5), c(NA, 0.3), 0.3), c(NA, ref_q(0.5, 0.3, 0.3)),
                 info = "fqBE: an NA row does not disturb the others")
expect_silent(fqBE_(c(0.5, NA), c(0.3, NA), c(0.3, NA)))
expect_error(fqBE_(0.5, c(NA, 2), 0.3), pattern = "mu must be",
             info = "fqBE: an NA row is skipped, but a bad row after it is still checked")

# =============================================================================
# E. Parameter checks, after recycling
# =============================================================================
for (bad in c(0, 1, -0.1, 1.5, Inf)) {
  expect_error(fqBE_(0.5, bad, 0.3), pattern = "mu must be between 0 and 1", info = paste("fqBE stops on mu =", bad))
  expect_error(fpBE_(0.5, bad, 0.3), pattern = "mu must be between 0 and 1", info = paste("fpBE stops on mu =", bad))
  expect_error(fdBE_(0.5, bad, 0.3), pattern = "mu must be between 0 and 1", info = paste("fdBE stops on mu =", bad))
  expect_error(fqBE_(0.5, 0.3, bad), pattern = "sigma must be between 0 and 1", info = paste("fqBE stops on sigma =", bad))
}
expect_error(fqBE_(1.1, 0.3, 0.3), pattern = "p must be", info = "fqBE stops on p > 1")
expect_error(fqBE_(-0.1, 0.3, 0.3), pattern = "p must be", info = "fqBE stops on p < 0")
expect_error(fqBE_(0.1, 0.3, 0.3, log_p = TRUE), pattern = "log scale", info = "fqBE stops on log p > 0")
expect_error(fqBE_(c(0.5, 0.5), c(0.3, 1), 0.3), pattern = "mu must be", info = "fqBE checks after recycling")

# =============================================================================
# F. Recycling and types
# =============================================================================
expect_identical(fqBE_(c(0.1, 0.5, 0.9), 0.6, 0.3), ref_q(c(0.1, 0.5, 0.9), 0.6, 0.3),
                 info = "fqBE recycles a scalar parameter")
expect_identical(fqBE_(0.5, c(0.2, 0.6), c(0.1, 0.5)), ref_q(0.5, c(0.2, 0.6), c(0.1, 0.5)),
                 info = "fqBE recycles a scalar p")
expect_identical(length(fqBE_(numeric(0), 0.6, 0.3)), 0L, info = "fqBE: a zero-length argument gives a zero-length result")
expect_identical(fpBE_(0L, 0.6, 0.3), 0, info = "fpBE takes an integer q")
expect_true(is.double(fqBE_(0.5, 0.6, 0.3)), info = "fqBE returns doubles")

# =============================================================================
# G. Consistency: the CDF inverts the quantile; the density integrates to 1
# =============================================================================
pp <- seq(0.01, 0.99, by = 0.01)
expect_true(max(abs(fpBE_(fqBE_(pp, 0.6, 0.3), 0.6, 0.3) - pp)) < 1e-10, info = "fpBE(fqBE(p)) = p")
expect_true(abs(stats::integrate(function(z) fdBE_(z, 0.35, 0.4), 0, 1)$value - 1) < 1e-6,
            info = "fdBE integrates to 1")

# =============================================================================
# H. frBE: fqBE at dqrng's uniforms; its moments within the sampling error
# =============================================================================
if (requireNamespace("dqrng", quietly = TRUE)) {
  dqrng::dqset.seed(77)
  a <- frBE_(1000, mu = 0.6, sigma = 0.3)
  dqrng::dqset.seed(77)
  b <- fqBE_(dqrng::dqrunif(1000), 0.6, 0.3)
  expect_identical(a, b, info = "frBE is fqBE at dqrunif's uniforms")
  expect_true(all(a > 0 & a < 1), info = "frBE draws lie in (0, 1)")
  dqrng::dqset.seed(78)
  big <- frBE_(2e5, mu = 0.6, sigma = 0.3)
  v <- 0.3^2 * 0.6 * 0.4
  expect_true(abs(mean(big) - 0.6) < 5 * sqrt(v / 2e5), info = "frBE's mean is mu (within 5 standard errors)")
  expect_true(abs(var(big) - v) < 5 * v * sqrt(2 / 2e5), info = "frBE's variance is sigma^2 mu (1 - mu) (within ~5 s.e.)")
  expect_error(frBE_(0), pattern = "positive", info = "frBE stops on n <= 0")
}
