# Quantiles near p = 1, as of CKutils 0.1.34: the DPO part. Later commits append the
# DEL, BNB, SICHEL and zero-inflated / zero-altered (ZI/ZA) parts of the same fix.
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
