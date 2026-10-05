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
# through fqBNB(): it reaches the window through its own transform of p ((p - tau) / (1 - tau) - 1e-10, then the zero mass
# folded in); fqZIBNB() and fqZISICHEL() subtract 1e-7 and stay clear of it for every p <= 1 (a later commit changes these).
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
