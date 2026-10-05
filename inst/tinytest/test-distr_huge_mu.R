# Quantiles, densities and CDFs at a large mu (and a small sigma), as of
# CKutils 0.1.34. Before:
# - the discrete quantile searches (BNB, SICHEL, DPO, DEL; ZIBNB, ZABNB and
#   ZISICHEL through them) stopped after 1e6 terms and returned 1e6 as if it
#   were the quantile (fqDPO returned 0, its densities being Inf);
# - SICHEL's unscaled Bessel K underflowed to 0 for a large mu or a small
#   sigma, and the difference of their logs was NaN;
# - BNB's log term and its CDF sum lost digits as the count grew: quantiles
#   beyond ~1e6 were off by units (by 4% in a heavy tail), the log density by
#   1e-5 at 1.5e9, the CDF by 3e-10 at 1e7.
# (DPO's normalising constant: test-fDPO.R.)

# q is the quantile at p when F(q) >= p > F(q - 1)
right_quantile <- function(q, pfun, p, ...) {
  isTRUE(pfun(q, ...) >= p) && isTRUE(pfun(q - 1, ...) < p)
}

# --- medians beyond 1e6 ---
q <- fqBNB(0.5, mu = 2e6, sigma = 0.5, nu = 1)
expect_true(q > 1e6 && right_quantile(q, fpBNB, 0.5, mu = 2e6, sigma = 0.5, nu = 1),
            info = "fqBNB: median beyond 1e6")
q <- fqZIBNB(0.5, mu = 3e6, sigma = 0.5, nu = 1, tau = 0.1)
expect_true(q > 1e6 && right_quantile(q, fpZIBNB, 0.5, mu = 3e6, sigma = 0.5, nu = 1, tau = 0.1),
            info = "fqZIBNB: median beyond 1e6")
q <- fqZABNB(0.5, mu = 3e6, sigma = 0.5, nu = 1, tau = 0.1)
expect_true(q > 1e6 && right_quantile(q, fpZABNB, 0.5, mu = 3e6, sigma = 0.5, nu = 1, tau = 0.1),
            info = "fqZABNB: median beyond 1e6")
q <- fqSICHEL(0.5, mu = 2e6, sigma = 1, nu = -0.5)
expect_true(q > 1e6 && right_quantile(q, fpSICHEL, 0.5, mu = 2e6, sigma = 1, nu = -0.5),
            info = "fqSICHEL: median beyond 1e6")
q <- fqZISICHEL(0.5, mu = 2e6, sigma = 1, nu = -0.5, tau = 0.1)
expect_true(q > 1e6 && right_quantile(q, fpZISICHEL, 0.5, mu = 2e6, sigma = 1, nu = -0.5, tau = 0.1),
            info = "fqZISICHEL: median beyond 1e6")
expect_identical(fqDPO(0.5, mu = c(1e4, 2e6), sigma = 2), c(1e4, 2e6),
                 info = "fqDPO (sigma != 1): median at mu = 1e4 and 2e6")

# --- BNB: exact quantiles, CDF and density at large values ---
# Up to 0.1.34 the BNB log term was lbeta(i+n, m+k) - lbeta(n, m) - lgamma(i+1) -
# lgamma(k) + lgamma(i+k): the last three parts are large and nearly cancel
# (lgamma(i+1) is 2e10 at i = 1e9), so the term lost digits as i grew, and the CDF
# was a plain double sum. The expected values are from a quad-precision sum of the
# mass function. Each quantile is the q with F(q) >= p > F(q - 1), and p is cleared
# on both sides by far more than the rounding of a correct double CDF:
#   fqBNB(0.5, 2e8, 0.7, 1.3):   p - F(q - 1) 1.7e7 ulp(p), F(q) - p 1.1e7 ulp(p)
#   fqBNB(0.95, 2e7, 0.3, 0.2):  1.9e7 and 6.2e6 ulp(p)
#   fqBNB(0.99, 1e7, 0.5, 0.7):  3.2e6 and 1.0e5 ulp(p)
#   fqBNB(0.5, 3e9, 0.5, 1):     1.1e6 and 6.6e5 ulp(p)
# Before: 83762994, 49238660 and 66003854 for the first three.
expect_identical(fqBNB(0.5, mu = 2e8, sigma = 0.7, nu = 1.3), 83762993,
                 info = "fqBNB: median at mu = 2e8 is exact")
expect_identical(fqBNB(0.95, mu = 2e7, sigma = 0.3, nu = 0.2), 49238657,
                 info = "fqBNB: 0.95 quantile at mu = 2e7, nu = 0.2 is exact")
expect_identical(fqBNB(0.99, mu = 1e7, sigma = 0.5, nu = 0.7), 66003856,
                 info = "fqBNB: 0.99 quantile at mu = 1e7, nu = 0.7 is exact")
# before: the error of the CDF was -2.7e-10
expect_true(abs(fpBNB(1e7, 2e6, 0.7, 1.3) - 0.97113512511612876) < 1e-13,
            info = "fpBNB: CDF at 1e7 is accurate")
# before: the error of the log density was 1.2e-5 at 1.5e9 and 1.5e-9 at 1e6
expect_true(abs(fdBNB(1.5e9, 3e9, 0.7, 1.3, log = TRUE) + 22.435871685526369) < 1e-12,
            info = "fdBNB: log density at 1.5e9 is accurate")
expect_true(abs(fdBNB(1e6, 2e6, 0.3, 0.2, log = TRUE) + 14.556222794213836) < 1e-12,
            info = "fdBNB: log density at 1e6 is accurate")
if (at_home()) {
  # the median at mu = 3e9: O(q) at ~4 ns a term, ~6 s. (Before: the same value,
  # in ~2 minutes at ~100 ns a term; this one guards the cost, not the value.)
  expect_identical(fqBNB(0.5, mu = 3e9, sigma = 0.5, nu = 1), 1559526299,
                   info = "fqBNB: median at mu = 3e9 is exact")
}

# --- SICHEL with a small sigma: finite, near its Poisson limit ---
d <- fdSICHEL(0:4, mu = 2, sigma = 0.001, nu = -0.5)
expect_true(all(is.finite(d)) && max(abs(d - dpois(0:4, 2))) < 1e-3,
            info = "fdSICHEL: sigma = 0.001 is finite and near Poisson(2)")

# --- NA, with a warning, where no int quantile exists ---
# p = 1: Inf, which was stored into the integer result (undefined behaviour:
# NA on x86-64, INT_MAX on AArch64)
expect_warning(r <- fqSICHEL(1, mu = 2, sigma = 1, nu = -0.5), pattern = "infinite")
expect_identical(r, NA_integer_, info = "fqSICHEL(p = 1): NA")
# a quantile beyond the int range, decided without scanning the range
expect_warning(r <- fqBNB(0.5, mu = 1e300, sigma = 0.5, nu = 1), pattern = "not found")
expect_true(is.na(r), info = "fqBNB: quantile beyond the int range is NA")
expect_warning(r <- fqDPO(0.5, mu = 3e9, sigma = 2))
expect_true(is.na(r), info = "fqDPO: quantile beyond the int range is NA")

# --- SICHEL: a quantile beyond the int range is reported at once ---
# fqSICHEL() and fqZISICHEL() used to scan all 2^31 - 2 terms before giving the NA: 27-40 s,
# which cannot be interrupted. A bound on the CDF now settles it first (the bound itself is
# tested in test-fSICHEL.R). The values are always checked, the time bounds only at_home().
elapsed <- system.time(expect_warning(
  r <- fqSICHEL(0.5, mu = 1e300, sigma = 1, nu = -0.5), pattern = "not found"))[["elapsed"]]
expect_true(is.na(r), info = "fqSICHEL: mu = 1e300 is NA")
if (at_home()) expect_true(elapsed < 5, info = "fqSICHEL: mu = 1e300 is NA at once")
elapsed <- system.time(expect_warning(
  r <- fqZISICHEL(0.5, mu = 1e300, sigma = 1, nu = -0.5, tau = 0.1), pattern = "not found"))[["elapsed"]]
expect_true(is.na(r), info = "fqZISICHEL: mu = 1e300 is NA")
if (at_home()) expect_true(elapsed < 5, info = "fqZISICHEL: mu = 1e300 is NA at once")
# left side: F(2^31 - 2) < p although the terms are still rising there (the sum stays 0)
elapsed <- system.time(expect_warning(
  r <- fqSICHEL(0.5, mu = 1e10, sigma = 1, nu = -0.5), pattern = "not found"))[["elapsed"]]
expect_true(is.na(r), info = "fqSICHEL: mu = 1e10 is NA")
if (at_home()) expect_true(elapsed < 5, info = "fqSICHEL: mu = 1e10 is NA at once")
# right side: a heavy tail, F(2^31 - 2) = 0.90 < p
elapsed <- system.time(expect_warning(
  r <- fqSICHEL(0.999, mu = 1e9, sigma = 10, nu = -0.5), pattern = "not found"))[["elapsed"]]
expect_true(is.na(r), info = "fqSICHEL: p = 0.999, mu = 1e9, sigma = 10 is NA")
if (at_home()) expect_true(elapsed < 5, info = "fqSICHEL: p = 0.999, mu = 1e9, sigma = 10 is NA at once")

# The bound must stay silent where the quantile is finite, with the Markov gate open
# (mu > (1 - p) (2^31 - 1): 107 for p = 1 - 5e-8, 43 for p = 1 - 2e-8). p lies 3.0e-11 above
# F(7577) and 6.2e-11 below F(7578) (checked against two references independent of the package: a
# Gamma-inverse-Gaussian closed form and a Poisson-GIG mixture integral), far more than the
# rounding of the CDF sum on another platform.
expect_identical(fqSICHEL(1 - 5e-8, mu = 300, sigma = 1, nu = -0.5), 7578L,
                 info = "fqSICHEL: finite quantile with the gate open")
# a heavy tail: the quantile is q = 82035, and the package's own CDF brackets p at q
q <- fqSICHEL(1 - 2e-8, mu = 90, sigma = 50, nu = -0.5)
expect_false(is.na(q), info = "fqSICHEL: heavy tail with the gate open is not NA")
expect_true(fpSICHEL(q - 1, 90, 50, -0.5) < 1 - 2e-8, info = "fqSICHEL: heavy tail, F(q - 1) < p")
expect_true(1 - 2e-8 <= fpSICHEL(q, 90, 50, -0.5), info = "fqSICHEL: heavy tail, p <= F(q)")
