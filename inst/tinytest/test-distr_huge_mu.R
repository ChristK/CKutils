# Quantiles, densities and CDFs at a large mu (and a small sigma), as of
# CKutils 0.1.34. Before:
# - the discrete quantile searches (BNB, SICHEL, DPO, DEL; ZIBNB, ZABNB and
#   ZISICHEL through them) stopped after 1e6 terms and returned 1e6 as if it
#   were the quantile (fqDPO returned 0, its densities being Inf);
# - SICHEL's unscaled Bessel K underflowed to 0 for a large mu or a small
#   sigma, and the difference of their logs was NaN.
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
