# Quantiles near p = 1, as of CKutils 0.1.34: the DPO, BNB and SICHEL parts. Later commits
# append the DEL and zero-inflated / zero-altered (ZI/ZA) parts of the same fix.
#
# Before, fqDPO() returned Inf for every p + 1e-9 >= 1, a cutoff copied from
# gamlss.dist::qDPO, where it guards an R loop, although the quantile of such a p
# is finite (in the models it became NA_integer_). Every expectation below fails on
# 0.1.34 (Inf), except p = 1, which is Inf on both and guards the new `p >= 1` test.
# The round trips also guard the new search, which compares cdf >= p exactly and
# allows a tolerance only for a sum that has stopped growing.
#
# Reference values: from an independent reference, the DPO pmf summed in long double
# (upper tail first). Each integer pinned below has F(q) - p and p - F(q - 1) >= 4e-12,
# far above the ~1e-15 accuracy of the package's own sums. The other expectations are
# properties (round trip, p between two CDF values) and need no external numbers.

# ---- quantiles in [1 - 1e-9, 1) are finite (0.1.34: Inf) --------------------------------------------------------------
expect_identical(fqDPO(0.999999999, 16.2709, 7.52822), 118)
expect_identical(fqDPO(c(1 - 1e-9, 1 - 5e-10, 1 - 4e-11), 16.27, 7.53), c(118, 121, 130))
# deeper into the window the quantile is still finite (below ~1e-11 the CDF steps are under 1e-12, so double
# precision no longer pins the integer: only finiteness is asked)
expect_true(all(is.finite(fqDPO(c(1 - 1e-12, 1 - 1e-15), 16.27, 7.53))))
# the result is the quantile: F(q - 1) < p <= F(q) in the package's own CDF
p <- 1 - c(9e-10, 5e-10, 1e-10)
q <- fqDPO(p, 1.04, 2.36)
expect_true(all(is.finite(q)))
expect_true(all(fpDPO(q - 1, 1.04, 2.36) < p & p <= fpDPO(q, 1.04, 2.36)))
# the models store the quantile as an integer: no NA there
expect_false(anyNA(as.integer(fqDPO(1 - 5e-10, 16.27, 7.53))))
# the Poisson fast path (sigma = 1) reaches the window too
expect_equal(fqDPO(c(1 - 5e-10, 1 - 1e-12), 5, 1), qpois(c(1 - 5e-10, 1 - 1e-12), 5))
# p = 1 is exactly Inf
expect_identical(fqDPO(1, 5, 2), Inf)
# the same quantile on the lower tail, the upper tail and the log scale (0.1.34: Inf on all three)
expect_identical(fqDPO(1 - 1e-10, 16.2709, 7.52822), 127)
expect_identical(fqDPO(1e-10, 16.2709, 7.52822, lower_tail = FALSE), 127)
expect_identical(fqDPO(log1p(-1e-10), 16.2709, 7.52822, log_p = TRUE), 127)

# ---- round trip q(p(x)) == x, p from the package's own CDF; x with a CDF step > 1e-11 and F < 1 - 1e-10 --------------------
rt <- function(fq, fp, x, ...) {
  p <- fp(x, ...)
  keep <- p < 1 - 1e-10 & c(TRUE, diff(p) > 1e-11)
  expect_identical(as.numeric(fq(p[keep], ...)), as.numeric(x[keep]))
}
rt(fqDPO, fpDPO, 0:150, 16.27, 7.53)
rt(fqDPO, fpDPO, 0:90, 7.48826, 5.59393)

# =====================================================================================================================
# BNB and SICHEL: the same fix. fqBNB() returned Inf, and fqSICHEL() NA with a warning, for every p + 1e-9 >= 1 (the same
# gamlss.dist cutoff, copied from its R loop over pBNB / pSICHEL calls), although the quantile is finite. fqZABNB() draws
# through fqBNB(): it reached the window through its own transform of p ((p - tau) / (1 - tau) - 1e-10, then the zero mass
# folded in); fqZIBNB() and fqZISICHEL() subtracted 1e-7 and stayed clear of it for every p <= 1. The zero-inflated /
# zero-altered part at the end of this file removed those offsets: all three reach the window now.
# Every expectation of this part fails on 0.1.34 (Inf for BNB, NA and a warning for SICHEL), except those at p = 1 and above
# (Inf / NA on both: they guard the new `p >= 1` test). The round trips reach p up to 1 - 1e-10, inside the window, and also
# guard the exact search (`cdf >= p` compared exactly); the settle expectations (a p above the largest sum there is) fail
# when a stalled sum is always NA, as it was.
#
# Reference values: fqBNB from the pmf summed in quad precision (__float128), fqSICHEL from the Poisson-GIG mixture
# integrals; neither shares code with the package. Each integer pinned has p - F(q - 1) and F(q) - p >= 1.9e-12 (BNB) or
# 1.7e-12 (SICHEL), 1000 times the ~1e-15 accuracy of the package's own sums. The other expectations are properties (a bracket
# on the package's own CDF at a p >= 1e-13 from both neighbouring CDF values, finiteness, a round trip) and need no external numbers.

# ---- fqBNB: quantiles in [1 - 1e-9, 1) are finite (0.1.34: Inf) ------------------------------------------------------
# smok_cig_ex-like parameter sets (the ZABNB models draw through fqBNB), pinned at p whose neighbouring CDF values are far away
expect_identical(fqBNB(0.999999999, 10.6139, 0.08621, 0.0174699), 342)
expect_identical(fqBNB(c(1 - 1e-9, 1 - 5e-10, 1 - 2e-10), 10.6139, 0.08621, 0.0174699), c(342, 365, 398))
expect_identical(fqBNB(c(1 - 1e-9, 1 - 5e-10, 1 - 1e-10), 6.0573, 0.0490984, 0.0147), c(150, 157, 176))
expect_identical(fqBNB(c(1 - 1e-9, 1 - 5e-10), 3.9, 0.16, 0.4), c(315, 348))
# deeper into the window the CDF steps fall below 1e-12 and double precision no longer pins the integer (the last set, a plain
# BNB with a longer tail, has steps of ~6e-13 already at 1 - 1e-9, so only the first two p): the result is finite and is the
# quantile of the package's own CDF, F(q - 1) < p <= F(q), at p at least 1e-13 from both neighbouring CDF values; and finite
# at 1 - 1e-11 and 1 - 1e-12
for (s in list(list(c(10.6139, 0.08621, 0.0174699), c(9e-10, 5e-10, 1e-10, 5e-11)), list(c(6.0573, 0.0490984, 0.0147), c(9e-10, 5e-10, 1e-10, 5e-11)),
               list(c(3.9, 0.16, 0.4), c(9e-10, 5e-10, 1e-10, 5e-11)), list(c(2, 0.5, 1), c(9e-10, 5e-10)))) {
  par <- s[[1]]
  p <- 1 - s[[2]]
  q <- fqBNB(p, par[1], par[2], par[3])
  expect_true(all(is.finite(q)))
  expect_true(all(fpBNB(q - 1, par[1], par[2], par[3]) < p & p <= fpBNB(q, par[1], par[2], par[3])))
}
for (par in list(c(10.6139, 0.08621, 0.0174699), c(6.0573, 0.0490984, 0.0147), c(3.9, 0.16, 0.4))) {
  expect_true(all(is.finite(fqBNB(1 - c(1e-11, 1e-12), par[1], par[2], par[3]))))
}
# a p within a few roundings of 1 lies above the largest sum there is (the terms stop changing it ~3e-15 short of 1): the
# answer is the index where the sum stopped, not Inf and not NA (the settle step of the search; 0.1.34: Inf)
q <- suppressWarnings(fqBNB(c(1 - 1e-12, 1 - 1e-14, 1 - 1e-15, 1 - 2^-53), 10.6139, 0.08621, 0.0174699))
expect_true(all(is.finite(q)) && !is.unsorted(q))
# a heavy tail (terms ~ i^-4: they fall below the resolution of the sum while ~9e-13 of the mass is still beyond): the sum
# stops far short of p and the quantile is not found (NA with the warning), as for any p < 1 that no sum reaches
r <- suppressWarnings(fqBNB(1 - 1e-14, 2, 0.5, 1))
expect_true(is.na(r))
expect_warning(fqBNB(1 - 1e-14, 2, 0.5, 1), "NAs produced")
# the same quantile on the lower tail, the upper tail and the log scale (0.1.34: Inf on all three)
expect_identical(fqBNB(1 - 1e-10, 6.0573, 0.0490984, 0.0147), 176)
expect_identical(fqBNB(1e-10, 6.0573, 0.0490984, 0.0147, lower_tail = FALSE), 176)
expect_identical(fqBNB(log1p(-1e-10), 6.0573, 0.0490984, 0.0147, log_p = TRUE), 176)
# p = 1 is exactly Inf, and so is a p above it within the 1.0001 tolerance
expect_identical(fqBNB(1, 5, 0.5, 0.5), Inf)
expect_identical(fqBNB(1.00005, 5, 0.5, 0.5), Inf)
expect_identical(fqBNB(0, 5, 0.5, 0.5, lower_tail = FALSE), Inf)
# fqZABNB() inherits the finite answers wherever its transformed p reaches the window (0.1.34: Inf from 1 - 1e-10 on)
expect_true(all(is.finite(fqZABNB(c(1 - 1e-9, 1 - 1e-10, 1 - 1e-12), 6.0573, 0.0490984, 0.0147, 0.000398))))

# ---- fqSICHEL: quantiles in [1 - 1e-9, 1) are finite (0.1.34: NA with a warning) ---------------------------------------
expect_silent(fqSICHEL(0.999999999, 2.34544, 0.212277, -6.19696))
expect_identical(fqSICHEL(c(1 - 1e-9, 1 - 5e-10, 1 - 1e-10, 1 - 1e-11), 2.34544, 0.212277, -6.19696), c(33L, 34L, 37L, 42L))
expect_identical(fqSICHEL(c(1 - 1e-9, 1 - 5e-10, 1 - 1e-10, 1 - 1e-11), 2.24037, 0.108986, -9.79), c(22L, 23L, 24L, 27L))
expect_identical(fqSICHEL(c(1 - 1e-9, 1 - 5e-10, 1 - 2e-11), 2, 1, -0.5), c(73L, 76L, 89L))
expect_identical(fqSICHEL(c(1 - 1e-9, 1 - 5e-10, 1 - 2e-10), 1.5, 3, -1.5), c(352L, 373L, 400L))
# deeper still: finite, and the quantile of the package's own CDF, F(q - 1) < p <= F(q), at p at least 1e-13 from both
# neighbouring CDF values; finite at 1 - 1e-11 and 1 - 1e-12
for (par in list(c(2.34544, 0.212277, -6.19696), c(2.24037, 0.108986, -9.79), c(2, 1, -0.5), c(1.5, 3, -1.5))) {
  p <- 1 - c(9e-10, 5e-10, 1e-10, 5e-11, 3e-11)
  q <- fqSICHEL(p, par[1], par[2], par[3])
  expect_false(anyNA(q))
  expect_true(all(fpSICHEL(q - 1L, par[1], par[2], par[3]) < p & p <= fpSICHEL(q, par[1], par[2], par[3])))
  expect_false(anyNA(fqSICHEL(1 - c(1e-11, 1e-12), par[1], par[2], par[3])))
}
# the settle step: a p above the largest sum there is (the terms stop changing it ~2e-15 short of 1) gets the index where
# the sum stopped (0.1.34: NA with a warning)
expect_silent(fqSICHEL(c(1 - 1e-12, 1 - 1e-15, 1 - 2^-53), 2, 0.5, -6))
q <- suppressWarnings(fqSICHEL(c(1 - 1e-12, 1 - 1e-15, 1 - 2^-53), 2, 0.5, -6))
expect_true(!anyNA(q) && !is.unsorted(q))
# the same quantile on the upper tail and the log scale
expect_identical(fqSICHEL(1 - 1e-10, 2.34544, 0.212277, -6.19696), 37L)
expect_identical(fqSICHEL(1e-10, 2.34544, 0.212277, -6.19696, lower_tail = FALSE), 37L)
expect_identical(fqSICHEL(log1p(-1e-10), 2.34544, 0.212277, -6.19696, log_p = TRUE), 37L)
# for sigma > 1e4 and nu > 0 the NBI approximation answers (fqNBI), in the window too
expect_identical(fqSICHEL(1 - c(1e-9, 1e-10), 5, 2e4, 2), fqNBI(1 - c(1e-9, 1e-10), 5, 0.5))
expect_false(anyNA(fqSICHEL(1 - c(1e-9, 1e-10), 5, 2e4, 2)))
# p = 1 is NA with the warning (an integer cannot hold Inf), and so is a p above it within the 1.0001 tolerance
expect_warning(fqSICHEL(1, 5, 0.5, -0.5), "NAs produced")
expect_true(is.na(suppressWarnings(fqSICHEL(1, 5, 0.5, -0.5))))
expect_warning(fqSICHEL(1.00005, 5, 0.5, -0.5), "NAs produced")
expect_true(is.na(suppressWarnings(fqSICHEL(1.00005, 5, 0.5, -0.5))))

# ---- round trip q(p(x)) == x, p from the package's own CDF ---------------------------------------------------------------
rt(fqBNB, fpBNB, 0:400, 10.6139, 0.08621, 0.0174699)
rt(fqBNB, fpBNB, 0:300, 6.0573, 0.0490984, 0.0147)
rt(fqBNB, fpBNB, 0:600, 3.9, 0.16, 0.4)
rt(fqSICHEL, fpSICHEL, 0:60, 2.34544, 0.212277, -6.19696)
rt(fqSICHEL, fpSICHEL, 0:40, 2.24037, 0.108986, -9.79)
rt(fqSICHEL, fpSICHEL, 0:500, 1.5, 3, -1.5)

# =====================================================================================================================
# DEL: the same fix. fqDEL() returned Inf for every p + 1e-9 >= 1 (the gamlss.dist cutoff, as in DPO), although the quantile
# is finite, and fqDEL_search treated any stalled sum as "not found". Now only p >= 1 is Inf, and a stall settles within
# CK_P_FUZZ of p, as in fqDPO. Every expectation of this part fails on the base (Inf), except p = 1 (Inf on both, guarding
# the new `p >= 1` test).
#
# Reference values: the convolution F(q) = sum_k dpois(k, mu*nu) * pnbinom(q - k, size = 1/sigma, mu = mu*(1 - nu)) over the
# Poisson part's 40-sd window, summed as the upper tail (no 1 - F cancellation); shares no code with the package. Each
# integer pinned has p - F(q - 1) and F(q) - p >= 1e-12 (~1000 times the ~1e-15 accuracy of the package's sums). Where the
# margin is smaller (the sets with quantiles >= 4096, which run on CkDELAccurate, and p deeper in the window) the expectation
# is a bracket on the package's own fpDEL.

# ---- quantiles in [1 - 1e-9, 1) are finite (base: Inf) ---------------------------------------------------------------
expect_identical(fqDEL(1 - 1e-9, 2.03065, 2.30919, 0.830551), 25)
expect_identical(fqDEL(c(1 - 1e-9, 1 - 5e-10, 1 - 1e-10), 2.03065, 2.30919, 0.830551), c(25, 26, 28))
expect_identical(fqDEL(c(1 - 1e-9, 1 - 5e-10, 1 - 1e-10), 5, 0.5, 0.3), c(54, 55, 59))
expect_identical(fqDEL(c(1 - 1e-9, 1 - 5e-10, 1 - 1e-10), 1.2, 0.8, 0.5), c(20, 20, 22))
expect_identical(fqDEL(c(1 - 1e-9, 1 - 5e-10), 60, 0.9, 0.2), c(935, 965))
# deeper into the window: finite, and the quantile in the package's own CDF
p <- 1 - c(1e-10, 1e-12)
q <- fqDEL(p, 60, 0.9, 0.2)
expect_true(all(is.finite(q)))
expect_true(all(fpDEL(q - 1, 60, 0.9, 0.2) < p & p <= fpDEL(q, 60, 0.9, 0.2)))
# quantiles >= 4096: the search restarts on CkDELAccurate (mu = 2500, sigma = 1.2, nu = 0.1: 54514 at 1 - 1e-9)
p <- 1 - c(1e-9, 5e-10, 1e-10)
for (par in list(c(2500, 1.2, 0.1), c(1500, 3, 0.3))) {
  q <- fqDEL(p, par[1], par[2], par[3])
  expect_true(all(is.finite(q)) && all(q >= 4096))
  expect_true(all(fpDEL(q - 1, par[1], par[2], par[3]) < p & p <= fpDEL(q, par[1], par[2], par[3])))
}
expect_equal(fqDEL(1 - 1e-9, 2500, 1.2, 0.1) / 54514, 1, tolerance = 1e-4)
# p = 1 is exactly Inf
expect_identical(fqDEL(1, 5, 0.5, 0.3), Inf)
# the same quantile on the lower tail, the upper tail and the log scale (base: Inf on all three)
expect_identical(fqDEL(1 - 1e-10, 2.03065, 2.30919, 0.830551), 28)
expect_identical(fqDEL(1e-10, 2.03065, 2.30919, 0.830551, lower_tail = FALSE), 28)
expect_identical(fqDEL(log1p(-1e-10), 2.03065, 2.30919, 0.830551, log_p = TRUE), 28)
# the Poisson fast path (sigma < 1e-4) reaches the window too
expect_equal(fqDEL(c(1 - 5e-10, 1 - 1e-12), 5, 1e-5, 0.5), qpois(c(1 - 5e-10, 1 - 1e-12), 5))
# a p above the largest sum there is (the terms stop changing it ~2e-15 short of 1) settles, and is not NA
q <- fqDEL(c(1 - 1e-15, 1 - 2^-53), 2.03065, 2.30919, 0.830551)
expect_true(!anyNA(q) && all(is.finite(q)) && !is.unsorted(q))

# ---- round trip q(p(x)) == x, p from the package's own CDF ---------------------------------------------------------------
rt(fqDEL, fpDEL, 0:60, 2.03065, 2.30919, 0.830551)
rt(fqDEL, fpDEL, 0:300, 5, 0.5, 0.3)
rt(fqDEL, fpDEL, 0:1200, 60, 0.9, 0.2)

# =====================================================================================================================
# Zero-inflated and zero-altered quantiles (ZINBI, ZANBI, ZABNB, ZIBNB, ZISICHEL): no 1e-10 / 1e-7 offsets.
#
# These five functions undo the zero mass, (p - w) / (1 - w), and then subtracted an ABSOLUTE offset before the base quantile:
# 1e-10 (ZINBI, ZANBI, ZABNB) or 1e-7 (ZIBNB, ZISICHEL), copied from gamlss.dist. The rounding of p = w + (1 - w) F(x) is only
# ~1e-16, so the offsets were 1e6 to 1e9 times too wide. Every p in (F(x), F(x) + (1 - w) * offset] came out one quantile too low
# (fqZIBNB(0.564835214835, 5, 0.5, 1, 0.1) was 2: p lies 5e-8 above F(2), inside the 1e-7 offset; the definition, and
# gamlss.dist::qZIBNB, give 3), q(p(x)) == x broke wherever the CDF step was below the offset, and the upper end was capped (the
# quantile at p = 1 - 1e-12 stopped far short of the true one). They now take CK_P_SLACK = 2 * DBL_EPSILON (absolute, on the p
# scale) off p instead: the rounding of p, and no more.
#   * p = 1: ZINBI and ZANBI stay finite (IMPACTncd's C++ calls fqZANBI_scalar(1 - rn, ...) and relies on it); the double ones,
#     ZIBNB and ZABNB, are Inf; the integer ZISICHEL is NA with the warning, as fqSICHEL.
#   * A zero-altered variate above the mass at 0 is at least 1, and a p up to the mass plus the slack is the mass itself.
# Every expectation of this part fails on the build before it, except the guards (marked as such: they hold on both and pin what
# must NOT change, or catch a wrong slack in the other direction) and the checks of the test sets themselves.
#
# Reference values: the pmf of the base distribution summed in long double (NBI and BNB by their recurrences, SICHEL from the
# Bessel K of each order; no code shared with the package). Each integer pinned has p - F(q - 1) and F(q) - p >= 1e-12, 1000
# times the ~1e-15 accuracy of the package's own sums; fqZIBNB(0.564835214835, ...) has 5e-8. The other expectations are
# properties (a move of 1e-11 around a CDF value of the package, a round trip) and need no external numbers.

# ---- p a hair above and a hair below a CDF value: q(F(x) + 1e-11) == x + 1 and q(F(x) - 1e-11) == x, for x = 0 and 1 --------
# 1e-11 is far above the ~1e-16 rounding of p and far below the old offsets ((1 - w) * 1e-10 or * 1e-7 on the p scale), so the
# old code answered x to the first (the second is a guard: it catches a slack that moves the answer up). F is the package's own
# CDF, and every set has CDF steps >= 1e-4 around x (checked, a check of the sets that holds on the old build too), so no
# neighbouring CDF value can matter. The sets include the models' own parameters (alcohol, fruit, durations, smok_cig_ex) and
# large zero masses (w = 0.9, 0.6, 0.5), where the rounding of p is amplified by 1 / (1 - w) on the scale of the base quantile.
zz_call <- function(fn, x, par) as.numeric(do.call(fn, c(list(x), as.list(par))))
zi_sets <- list(
  ZINBI = list(c(5, .5, .3), c(2, 1, .9), c(20, .2, .05), c(11.1455, .219614, .00433)),
  ZANBI = list(c(5, .5, .3), c(2, 1, .9), c(20, .2, .05), c(1.41353, 2.88325, .424)),
  ZIBNB = list(c(5, .5, 1, .1), c(10.6139, .08621, .0174699, .3), c(2, .5, 1, .6)),
  ZABNB = list(c(5, .5, 1, .1), c(10.6139, .08621, .0174699, .3), c(6.0573, .0490984, .0147, .0004)),
  ZISICHEL = list(c(2, 1, -2, .1), c(2.34544, .212277, -6.19696, .2), c(1.5, 3, -1.5, .5), c(2.24037, .108986, -9.79, .0163)))
for (fam in names(zi_sets)) {
  fq_ <- match.fun(paste0("fq", fam)); fp_ <- match.fun(paste0("fp", fam))
  up <- dn <- numeric(0)
  for (par in zi_sets[[fam]]) {
    Fx <- zz_call(fp_, 0:2, par)
    expect_true(all(diff(c(0, Fx)) > 1e-4), info = paste(fam, "CDF steps around x = 0, 1 at", paste(par, collapse = ", ")))
    up <- c(up, zz_call(fq_, Fx[1:2] + 1e-11, par))
    dn <- c(dn, zz_call(fq_, Fx[1:2] - 1e-11, par))
  }
  n_set <- length(zi_sets[[fam]])
  expect_identical(up, rep(c(1, 2), n_set), info = paste(fam, "q(F(x) + 1e-11) == x + 1, x = 0, 1"))
  expect_identical(dn, rep(c(0, 1), n_set), info = paste(fam, "q(F(x) - 1e-11) == x, x = 0, 1 (guard)"))
}

# ---- round trip q(p(x)) == x, p from the package's own CDF (`rt` above) ----------------------------------------------------
# In the tails the CDF steps fall below the old offsets (on the transformed scale: 1e-10 for ZINBI, ZANBI and ZABNB, 1e-7 for
# ZIBNB and ZISICHEL) and the old code answered x - 1 there; `rt` keeps the x with a step above 1e-11. The ZINBI and ZANBI sets
# with nu = 0.9 and 0.999 and the ZABNB set with tau = 0.6 are guards (the old build passes them, its offset being far wider than
# the rounding): the rounding of p = w + (1 - w) F(x) is amplified by 1 / (1 - w) on the scale of F, which a slack that is
# relative to F, or none, does not cover.
rt(fqZINBI, fpZINBI, 0:250, 5, .5, .3)
rt(fqZINBI, fpZINBI, 0:250, 20, .2, .05)
rt(fqZINBI, fpZINBI, 0:100, 11.1455, .219614, .00433)
rt(fqZINBI, fpZINBI, 0:250, 2, 1, .9)          # guard
rt(fqZINBI, fpZINBI, 0:300, 2, 1, .999)        # guard
rt(fqZANBI, fpZANBI, 0:250, 5, .5, .3)
rt(fqZANBI, fpZANBI, 0:250, 20, .2, .05)
rt(fqZANBI, fpZANBI, 0:600, 1.41353, 2.88325, .424)
rt(fqZANBI, fpZANBI, 0:250, 2, 1, .9)          # guard
rt(fqZANBI, fpZANBI, 0:300, 2, 1, .999)        # guard
rt(fqZIBNB, fpZIBNB, 0:600, 5, .5, 1, .1)
rt(fqZIBNB, fpZIBNB, 0:400, 10.6139, .08621, .0174699, .3)
rt(fqZIBNB, fpZIBNB, 0:300, 2, .5, 1, .6)
rt(fqZABNB, fpZABNB, 0:400, 10.6139, .08621, .0174699, .3)
rt(fqZABNB, fpZABNB, 0:300, 6.0573, .0490984, .0147, .0004)
rt(fqZABNB, fpZABNB, 0:300, 2, .5, 1, .6)      # guard
rt(fqZISICHEL, fpZISICHEL, 0:150, 2, 1, -2, .1)
rt(fqZISICHEL, fpZISICHEL, 0:80, 2.34544, .212277, -6.19696, .2)
rt(fqZISICHEL, fpZISICHEL, 0:400, 1.5, 3, -1.5, .5)
rt(fqZISICHEL, fpZISICHEL, 0:60, 2.24037, .108986, -9.79, .0163)

# ---- zero-altered: p == the mass at 0 is 0, a hair above it is 1 ---------------------------------------------------------------
# The mass is nu (ZANBI) or tau (ZABNB). The old code answered 0 for every p up to the mass + (1 - w) * 1e-10.
expect_identical(fqZANBI(.3, 5, .5, .3), 0L)                     # guard
expect_identical(fqZANBI(.3 + 1e-11, 5, .5, .3), 1L)
expect_identical(fqZABNB(.1, 5, .5, 1, .1), 0)                   # guard
expect_identical(fqZABNB(.1 + 1e-11, 5, .5, 1, .1), 1)
# A p some ulp above the mass, past the slack (4.4e-16) but below what the base quantile resolves, is above it all the same: 1.
# With a small mu the transformed p rounds to F_base(0) (R's qnbinom also takes a relative fuzz of 8 eps; the BNB search
# compares the sum with that p), so the base quantile alone says 0 here; the zero-altered variate above the mass is at least 1.
expect_identical(fqZANBI(.3 + 1e-15, .1, .1, .3), 1L)
expect_identical(fqZABNB(.1 + 1e-15, .1, .1, 1, .1), 1)

# ---- fqZIBNB(0.564835214835, 5, 0.5, 1, 0.1) is 3, not 2 ----------------------------------------------------------------------
# p lies 5e-8 above F_ZIBNB(2) = 0.564835164835 (long-double reference; the package's CDF is within 7e-16 of it) and 8e-2 below
# F_ZIBNB(3), so the quantile is 3, as gamlss.dist::qZIBNB, which has no offset, says. The old 1e-7 offset made it 2.
expect_identical(fqZIBNB(0.564835214835, 5, 0.5, 1, 0.1), 3)
# the package's own CDF brackets p the same way (guard: it holds on the old build too)
expect_true(fpZIBNB(2, 5, 0.5, 1, 0.1) < 0.564835214835 && 0.564835214835 <= fpZIBNB(3, 5, 0.5, 1, 0.1))

# ---- p = 1 ------------------------------------------------------------------------------------------------------------------
# ZIBNB and ZABNB (double results) are Inf; ZISICHEL (integer) is NA with the warning, as fqSICHEL(1, ...). The old build gave a
# finite cap for all three. A p above 1, within the 1.0001 tolerance of ZIBNB and ZISICHEL, is Inf / NA too (guards).
expect_identical(fqZIBNB(1, 5, .5, 1, .1), Inf)
expect_identical(fqZABNB(1, 5, .5, 1, .1), Inf)
expect_identical(fqZIBNB(1.00005, 5, .5, 1, .1), Inf)               # guard
expect_identical(fqZIBNB(0, 5, .5, 1, .1, lower_tail = FALSE), Inf)
expect_identical(fqZABNB(0, 5, .5, 1, .1, log_p = TRUE), Inf)
expect_warning(fqZISICHEL(1, 2, 1, -2, .1), "NAs produced")
expect_true(is.na(suppressWarnings(fqZISICHEL(1, 2, 1, -2, .1))))
expect_warning(fqZISICHEL(1.00005, 2, 1, -2, .1), "NAs produced")   # guard
expect_true(is.na(suppressWarnings(fqZISICHEL(1.00005, 2, 1, -2, .1))))   # guard
# ZINBI and ZANBI keep a finite value at p = 1 (guards), also with a large zero mass, where the slack is amplified
expect_true(all(is.finite(fqZINBI(1, c(5, 2, 20), c(.5, 1, .2), c(.3, .9, .05)))))
expect_true(all(is.finite(fqZANBI(1, c(5, 2, 20), c(.5, 1, .2), c(.3, .9, .05)))))

# ---- the upper end is not capped -----------------------------------------------------------------------------------------------
# p = 1 - 1e-8 ... 1 - 1e-10: the old offsets held the transformed p to at most 1 - 1e-10 (ZINBI, ZANBI, ZABNB) or 1 - 1e-7
# (ZIBNB, ZISICHEL), so the quantile stopped short: fqZIBNB(1 - 1e-8, 148.473, 0.108158, 1.866, 0.501399) was 7953 for 9936.
# The integers are the long-double reference quantiles (margin >= 1e-12 each, see above).
cap <- list(
  list("ZINBI", c(1010.22, .374893, .419279), c(1e-8, 1e-9), c(8626, 9562)),
  list("ZINBI", c(11.1455, .219614, .00433), c(1e-9, 1e-10), c(87, 94)),
  list("ZANBI", c(1.41353, 2.88325, .424), 1e-10, 93),
  list("ZANBI", c(5, .5, .3), 1e-10, 76),
  list("ZIBNB", c(148.473, .108158, 1.866, .501399), 1e-8, 9936),
  list("ZIBNB", c(10.6139, .08621, .0174699, .3), c(1e-8, 1e-9), c(263, 330)),
  list("ZABNB", c(10.6139, .08621, .0174699, .0004), c(1e-8, 1e-9), c(274, 343)),
  list("ZABNB", c(6.0573, .0490984, .0147, .000398), c(1e-9, 1e-10), c(151, 177)),
  list("ZISICHEL", c(2.34544, .212277, -6.19696, .2), c(1e-9, 1e-10), c(33, 37)),
  list("ZISICHEL", c(2.24037, .108986, -9.79, .0163), c(1e-9, 1e-10), c(22, 24)))
for (cs in cap) {
  expect_identical(zz_call(match.fun(paste0("fq", cs[[1]])), 1 - cs[[3]], cs[[2]]), cs[[4]],
                   info = paste0("fq", cs[[1]], " at 1 - ", paste(cs[[3]], collapse = ", "), " for ", paste(cs[[2]], collapse = ", ")))
}
# further in (p = 1 - 1e-12, 1 - 1e-10): the sum is at the limit of double precision, so only that the quantile is far above the
# old cap (the references: 12337, 113, 16960, 644, 46; the old build: 10703, 95, 8138, 425, 25)
expect_true(fqZINBI(1 - 1e-12, 1010.22, .374893, .419279) > 12000)
expect_true(fqZANBI(1 - 1e-12, 1.41353, 2.88325, .424) > 100)
expect_true(fqZIBNB(1 - 1e-10, 148.473, .108158, 1.866, .501399) > 15000)
expect_true(fqZABNB(1 - 1e-12, 10.6139, .08621, .0174699, .0004) > 600)
expect_true(fqZISICHEL(1 - 1e-12, 2.34544, .212277, -6.19696, .2) > 40)
# the same quantile on the upper tail and the log scale
expect_identical(fqZINBI(1e-10, 11.1455, .219614, .00433, lower_tail = FALSE), 94L)
expect_identical(fqZINBI(log1p(-1e-10), 11.1455, .219614, .00433, log_p = TRUE), 94L)
expect_identical(fqZISICHEL(1e-10, 2.34544, .212277, -6.19696, .2, lower_tail = FALSE), 37L)
expect_identical(fqZISICHEL(log1p(-1e-10), 2.34544, .212277, -6.19696, .2, log_p = TRUE), 37L)
expect_identical(fqZABNB(1e-9, 10.6139, .08621, .0174699, .0004, lower_tail = FALSE), 343)
expect_identical(fqZIBNB(log1p(-1e-9), 10.6139, .08621, .0174699, .3, log_p = TRUE), 330)

# ---- the zero-altered second transform: a small mu (P_base(0) near 1) ----------------------------------------------------------
# The zero truncation maps the p-scale slack to (1 - P_base(0)) * slack on the scale of the base CDF, below the rounding of that
# sum when P_base(0) is near 1, so the slack is taken off on the base scale too. Without it: fqZANBI(1, ...) rounds to a
# probability of exactly 1 (NA, where p = 1 must stay finite), and fqZABNB's exact search fails q(p(1)) == 1.
res_p1 <- NA_integer_
expect_silent(res_p1 <- fqZANBI(1, c(.05, .1, .1), c(.1, 1, .1), c(.1, .1, .3)))
expect_true(!anyNA(res_p1) && all(res_p1 >= 1L), info = "fqZANBI(p = 1) is finite at a small mu, silently")
rt(fqZABNB, fpZABNB, 0:8, 0.0957895, 0.277371, 0.0231258, 0.0163009)   # P_BNB(0) = 0.98
rt(fqZABNB, fpZABNB, 0:8, 0.0827971, 0.11918, 0.0154564, 0.00383418)
rt(fqZABNB, fpZABNB, 0:8, 0.298812, 0.282458, 0.0131807, 0.168673)
