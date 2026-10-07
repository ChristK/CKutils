# Delaporte (DEL): the CDF and the quantile search are O(q), as of CKutils
# 0.1.34. Before, ftofydel2_scalar rebuilt the whole tofydel2 recurrence
# (a std::vector of y + 2 doubles) for every y, so fpDEL(q) and fqDEL were
# O(q^2) (about 40 s at a quantile of 1e5, an hour at 1e6), fpDEL(0:q) was
# O(q^3) and fdDEL(0:q) O(q^2). The fix carries the recurrence forward and
# returns the same doubles, so the values below are those before the change;
# only the time differs. The time bounds (5 s) leave a margin of >1000x on the
# fixed code; before the change each call took 12-45 s. They are timing
# expectations, so they run only when at_home(): CRAN never runs them.

# Reference: the CDF of DEL(mu, sigma, nu) = NB(size 1/sigma, mean mu (1-nu))
# + Poisson(mu nu), by direct convolution (R's dnbinom / ppois; sum() adds in
# long double)
ref_pDEL <- function(y, mu, sigma, nu) {
  k <- 0:y
  sum(dnbinom(k, size = 1 / sigma, mu = mu * (1 - nu)) * ppois(y - k, mu * nu))
}
ref_dDEL <- function(y, mu, sigma, nu) {
  vapply(y, function(yy) {
    k <- 0:yy
    sum(dnbinom(k, size = 1 / sigma, mu = mu * (1 - nu)) * dpois(yy - k, mu * nu))
  }, numeric(1))
}

# --- a median near 1e5 (before: ~40 s) ---
mu <- 1e5; sigma <- 0.01; nu <- 0.5
el <- system.time(q <- fqDEL(0.5, mu, sigma, nu))[["elapsed"]]
if (at_home()) {
  expect_true(el < 5, info = "fqDEL: a quantile near 1e5 in under 5 s (O(q) search)")
}
expect_true(ref_pDEL(q - 1, mu, sigma, nu) < 0.5 && ref_pDEL(q, mu, sigma, nu) >= 0.5,
            info = "fqDEL: the median near 1e5 brackets 0.5 on the convolution CDF")

# --- one CDF value at q near 1e5 (before: ~40 s) ---
el <- system.time(p <- fpDEL(q, mu, sigma, nu))[["elapsed"]]
if (at_home()) {
  expect_true(el < 5, info = "fpDEL: q near 1e5 in under 5 s (O(q))")
}
expect_equal(p, ref_pDEL(q, mu, sigma, nu), tolerance = 1e-7,
             info = "fpDEL at q near 1e5 matches the convolution CDF")

# --- vectors over 0:q (before: fpDEL(0:2000) ~12 s, O(q^3)) ---
y <- 0:2000
el <- system.time({
  d <- fdDEL(y, 1000, 0.5, 0.5)
  p <- fpDEL(y, 1000, 0.5, 0.5)
})[["elapsed"]]
if (at_home()) {
  expect_true(el < 5, info = "fdDEL(0:2000) and fpDEL(0:2000) in under 5 s")
}
# the CDF adds the densities left to right in double precision: the running
# sum of fdDEL, bit for bit (any change to the summation breaks this)
expect_identical(p, Reduce(`+`, d, accumulate = TRUE),
                 info = "fpDEL(0:q) is the running double sum of fdDEL(0:q)")

# --- the same doubles whether or not consecutive elements share the work ---
x <- c(0:40, 25:60, 3, 3, 70:50)
for (pr in list(c(2, 1, 0.5), c(90, 2.31, 0.830551), c(4, 5e-5, 0.3))) {
  expect_identical(fdDEL(x, pr[1], pr[2], pr[3]),
                   vapply(x, function(z) fdDEL(z, pr[1], pr[2], pr[3]), numeric(1)),
                   info = "fdDEL: vector == element by element")
  expect_identical(fpDEL(x, pr[1], pr[2], pr[3], FALSE),
                   vapply(x, function(z) fpDEL(z, pr[1], pr[2], pr[3], FALSE), numeric(1)),
                   info = "fpDEL (upper tail): vector == element by element")
}

# --- the quantile search sums the same doubles as the CDF ---
# The IMPACTncd models truncate at max_value through maxq = fpDEL(10, ...) and
# rely on fqDEL(fpDEL(q)) == q for q = 0..10. Parameters: rows of their veg
# table (mu 0.86..5.06, sigma 6.9e-6..20.9, nu 0.830551, one of them in the
# sigma < 1e-4 Poisson branch), sigma = 1e-4 exactly, nu near 0 and near 1.
for (pr in list(c(2.03065, 2.30919, 0.830551), c(5.06004, 20.9438, 0.830551),
                c(0.856042, 6.90562e-06, 0.830551), c(3.6, 1e-4, 0.830551),
                c(1, 0.1, 1e-6), c(1, 0.1, 1 - 1e-6))) {
  expect_identical(fqDEL(fpDEL(0:10, pr[1], pr[2], pr[3]), pr[1], pr[2], pr[3]), as.numeric(0:10),
                   info = sprintf("fqDEL(fpDEL(0:10)) == 0:10 at mu=%g sigma=%g nu=%g", pr[1], pr[2], pr[3]))
}

# --- the density against the convolution, nu near 0 and 1, large sigma ---
for (pr in list(c(2, 1, 0.5), c(20, 20.94, 1e-6), c(5, 0.001, 1 - 1e-6), c(90, 2.31, 0.830551))) {
  expect_equal(fdDEL(0:60, pr[1], pr[2], pr[3]), ref_dDEL(0:60, pr[1], pr[2], pr[3]), tolerance = 1e-9,
               info = sprintf("fdDEL vs convolution at mu=%g sigma=%g nu=%g", pr[1], pr[2], pr[3]))
}
# --- a NaN / NA sigma stays NA, with a warning, in the state-carrying vector path ---
# The vector fpDEL carries its running sum from one element to the next when
# (mu, sigma, nu) repeat. A NaN sigma must not inherit the state of the element
# before it (every comparison with NaN is false, so a guard like
# `sigma >= 1e-04` skips the rebuild): it returned finite garbage such as 0.0828
# in an earlier draft of the change. fpDEL(c(5L, 5L), 2, c(1, NaN), 0.5) is
# c(0.9576, NaN) with the "NaNs or NAs were produced" warning, before and after.
r <- suppressWarnings(fpDEL(c(5L, 5L, 5L), 2, c(1, NaN, 1), 0.5))
expect_true(is.na(r[2]) && !anyNA(r[c(1, 3)]) && identical(r[1], r[3]),
            info = "fpDEL: a NaN sigma between valid elements is NA and does not disturb its neighbours")
expect_warning(r <- fpDEL(c(5L, 7L), 2, c(NaN, NaN), 0.5), pattern = "NaNs or NAs",
               info = "fpDEL: NaN sigma in the first element warns")
expect_true(all(is.na(r)), info = "fpDEL: NaN sigma in the first element is NA")
r <- suppressWarnings(fpDEL(c(3L, 8L), 2, c(1, NA_real_), 0.5, lower_tail = FALSE))
expect_true(is.na(r[2]) && !is.na(r[1]), info = "fpDEL (upper tail): NA sigma after a valid element is NA")
r <- suppressWarnings(fpDEL(c(5L, 5L), 2, c(1e-5, NaN), 0.5))
expect_true(!is.na(r[1]) && is.na(r[2]), info = "fpDEL: NaN sigma after a Poisson-branch element is NA")

# =============================================================================
# DEL at large counts: accurate from 4096 recurrence steps on
# =============================================================================
# The recurrence evaluated log f(j) = logpy0 - lgamma(j + 1) + S_j in double
# precision, so its error grew with j: against the exact convolution the CDF
# was off by 1e-10 at q = 8e3, 3e-9 at 65535, 5.5e-9 at 94119 (mu = 1e5),
# -7.5e-8 at 941185 (mu = 1e6), -4.1e-6 at 9.4e6 and 8.6e-3 at 9.3e7; 11 of 23
# large-mu quantiles were wrong by 1 to 663857 units, two came back NA after 13 s
# (fqDEL(0.5, 1e8, 1, 0.8)), and fqDEL(0.5, 1e9, 0.3, 0.4) returned 412402586
# where the CDF is 1.4e-5. From index 4096 (CK_DEL_COMPENSATE_AFTER) on the
# density, the CDF and the quantile search use CkDELAccurate (distr_DEL.h),
# which carries the deviation of the term ratio from its Poisson value (or from
# 1) instead of the ratio, and the log density as a compensated sum:
# |F - F_ref| < 6e-13 for 4096 <= q <= 65535 and < 5e-12 up to 2.0e9. Below 4096
# nothing changes, bit for bit (the first block above, and the checks below that
# straddle 4096), and the old recurrence is inside 1e-10 there (<= 7e-11 for
# sigma 0.05-3, nu 0.1-0.9; it is not only for nearly pure Poisson parameters,
# 1 - nu <= 1e-5). The expected integers were derived from an independent
# reference, the same convolution in C++ with long double sums; for each,
# F(q) - p and p - F(q - 1) are both >= 1e-10 (>= 2e-11 for the first).

# Reference, in plain R: DEL(mu, sigma, nu) = Poisson(mu nu) + NB(size 1/sigma,
# mean mu (1 - nu)), so F(q) = sum_k dpois(k, mu nu) pnbinom(q - k, ...), with k
# over the Poisson part's 40-sd window (the rest is below 1e-300): O(sd), not
# O(q). R's sum() accumulates in long double. It agrees with the C++
# long double convolution to 1e-15 on every case below.
ref_pDEL_conv <- function(q, mu, sigma, nu) {
  lam <- mu * nu
  lo <- max(0, floor(lam - 40 * sqrt(lam) - 50)); hi <- min(q, ceiling(lam + 40 * sqrt(lam) + 50))
  if (hi < lo) return(0)   # q left of the window: F(q) < 1e-300
  k <- lo:hi
  sum(dpois(k, lam) * pnbinom(q - k, size = 1 / sigma, mu = mu * (1 - nu)))
}
ref_dDEL_conv <- function(x, mu, sigma, nu) {
  lam <- mu * nu
  lo <- max(0, floor(lam - 40 * sqrt(lam) - 50)); hi <- min(x, ceiling(lam + 40 * sqrt(lam) + 50))
  if (hi < lo) return(0)
  k <- lo:hi
  sum(dpois(k, lam) * dnbinom(x - k, size = 1 / sigma, mu = mu * (1 - nu)))
}

# --- the CDF: the error was -7.5e-8 here, 4e-9 at (65000, 69149, 3, 0.9), 6e-10 at (12000, 8571, 0.05, 0.9) ---
expect_true(abs(fpDEL(941185L, 1e6, 0.3, 0.4) - ref_pDEL_conv(941185, 1e6, 0.3, 0.4)) < 1e-10,
            info = "fpDEL(941185, 1e6, 0.3, 0.4) is within 1e-10 of the convolution")
for (cs in list(c(12000, 12000 / 1.4, 3, 0.9), c(12000, 12000 / 1.4, 0.05, 0.9), c(40000, 40000 / 0.94, 1, 0.8),
                c(40000, 40000 / 1.4, 0.05, 0.9), c(65000, 65000 / 0.94, 3, 0.9), c(150000, 1e5, 0.3, 0.4),
                c(300000, 2e5, 3, 0.9), c(300000, 3e5, 0.05, 0.1), c(2800000, 3e6, 1, 0.8))) {
  expect_true(abs(fpDEL(as.integer(cs[1]), cs[2], cs[3], cs[4]) - ref_pDEL_conv(cs[1], cs[2], cs[3], cs[4])) < 1e-10,
              info = sprintf("fpDEL(%g, %g, %g, %g) is within 1e-10 of the convolution", cs[1], cs[2], cs[3], cs[4]))
}
# across the threshold itself: both sides within 1e-10 of the convolution, and F still increasing
q <- c(2000L, 4094L, 4095L, 4096L, 4097L, 6000L)
expect_true(all(abs(fpDEL(q, 3000, 0.3, 0.4) - vapply(q, ref_pDEL_conv, 0, mu = 3000, sigma = 0.3, nu = 0.4)) < 1e-10),
            info = "fpDEL across 4096 (mu = 3000) is within 1e-10 of the convolution")
expect_true(all(diff(fpDEL(q, 3000, 0.3, 0.4)) > 0), info = "fpDEL increases across 4096")

# --- exact quantiles the old recurrence got wrong ---
# 0.999999 at mu = 1e5: 397766 before (242 too low). Both margins >= 2.2e-11.
q <- fqDEL(0.999999, 1e5, 0.3, 0.4)
expect_identical(q, 398008, info = "fqDEL(0.999999, 1e5, 0.3, 0.4) is 398008 (it was 397766)")
expect_true(ref_pDEL_conv(q - 1, 1e5, 0.3, 0.4) < 0.999999 && ref_pDEL_conv(q, 1e5, 0.3, 0.4) >= 0.999999,
            info = "fqDEL(0.999999, 1e5, ...) brackets p on the convolution CDF")
# 0.99 at mu = 1e6: 2013415 before (one too high). Margins 2.8e-9 and 4.0e-8; through the p transformations too.
expect_identical(fqDEL(0.99, 1e6, 0.3, 0.4), 2013414, info = "fqDEL(0.99, 1e6, 0.3, 0.4) is 2013414 (it was 2013415)")
expect_identical(fqDEL(log(0.99), 1e6, 0.3, 0.4, log_p = TRUE), 2013414, info = "fqDEL, log_p")
expect_identical(fqDEL(0.01, 1e6, 0.3, 0.4, lower_tail = FALSE), 2013414, info = "fqDEL, upper tail")
# quantiles between 4096 and 65535 in the far tail, which were one or two off (both margins >= 1.1e-10)
expect_identical(fqDEL(0.999999, 25000, 0.05, 0.1), 57445, info = "fqDEL(0.999999, 25000, 0.05, 0.1) is 57445 (it was 57446)")
expect_identical(fqDEL(0.999999, 12000, 0.3, 0.4), 47769, info = "fqDEL(0.999999, 12000, 0.3, 0.4) is 47769 (it was 47768)")
expect_identical(fqDEL(0.99999, 12000, 0.05, 0.1), 25535, info = "fqDEL(0.99999, 12000, 0.05, 0.1) is 25535 (it was 25534)")
expect_identical(fqDEL(1 - 1e-7, 5e4, 0.05, 0.9), 58186, info = "fqDEL(1 - 1e-7, 5e4, 0.05, 0.9) is 58186 (it was 58187)")

# --- the density beyond 4096 ---
for (cs in list(c(12000, 12000 / 1.4, 3, 0.9), c(20000, 20000 / 0.94, 0.3, 0.4), c(40000, 40000 / 0.94, 1, 0.8),
                c(65000, 65000 / 0.94, 3, 0.9), c(1000000, 1e6, 0.3, 0.4), c(300000, 2e5, 3, 0.9))) {
  d <- fdDEL(as.integer(cs[1]), cs[2], cs[3], cs[4])
  expect_true(abs(d / ref_dDEL_conv(cs[1], cs[2], cs[3], cs[4]) - 1) < 1e-10,
              info = sprintf("fdDEL(%g, %g, %g, %g) is within 1e-10 (relative) of the convolution", cs[1], cs[2], cs[3], cs[4]))
}
expect_equal(fdDEL(1000000L, 1e6, 0.3, 0.4, log_ = TRUE), log(ref_dDEL_conv(1e6, 1e6, 0.3, 0.4)), tolerance = 1e-11,
             info = "fdDEL log_ beyond 4096")
# in the far left tail the density is below 1e-300 but the log density is finite, between the Poisson
# log density (f <= Pois(x; lam) for x <= lam) and that plus log P(NB = 0)
ld <- fdDEL(100000L, 1e6, 0.3, 0.4, log_ = TRUE)
lp <- dpois(100000, 4e5, log = TRUE)
expect_true(is.finite(ld) && ld <= lp + 1e-9 && ld >= lp - log1p(1e6 * 0.3 * 0.6) / 0.3 - 1e-9,
            info = "fdDEL log_ in the far left tail is finite and between its bounds")
expect_identical(fdDEL(100000L, 1e6, 0.3, 0.4), 0, info = "fdDEL in the far left tail underflows to 0")

# --- the pieces agree with each other across 4096 ---
# a vector gives the same doubles as element by element, whatever the order of small and large counts
# (4095 and 4096 are the last old-recurrence index and the first accurate one); the first three parameter sets
# have their mass around 4096, the last far above it
xs <- c(5L, 4096L, 6L, 4097L, 4097L, 4095L, 4096L, 3L, 70000L, 120000L)
for (pr in list(c(3000, 0.3, 0.4), c(4500, 1, 0.8), c(2000, 3, 0.9), c(7e4, 0.3, 0.4))) {
  expect_identical(fdDEL(xs, pr[1], pr[2], pr[3]), vapply(xs, function(z) fdDEL(z, pr[1], pr[2], pr[3]), numeric(1)),
                   info = sprintf("fdDEL vector == element by element across 4096 at mu=%g sigma=%g nu=%g", pr[1], pr[2], pr[3]))
  expect_identical(fpDEL(xs, pr[1], pr[2], pr[3]), vapply(xs, function(z) fpDEL(z, pr[1], pr[2], pr[3]), numeric(1)),
                   info = sprintf("fpDEL vector == element by element across 4096 at mu=%g sigma=%g nu=%g", pr[1], pr[2], pr[3]))
  expect_identical(fpDEL(xs, pr[1], pr[2], pr[3], FALSE), vapply(xs, function(z) fpDEL(z, pr[1], pr[2], pr[3], FALSE), numeric(1)),
                   info = sprintf("fpDEL upper tail, vector == element by element across 4096 at mu=%g sigma=%g nu=%g", pr[1], pr[2], pr[3]))
}
# fqDEL(fpDEL(q)) == q on both sides of 4096 and far beyond (the search adds the CDF's terms in the CDF's order)
q <- c(2000L, 4095L, 4096L, 4097L, 5000L, 6500L)
expect_identical(fqDEL(fpDEL(q, 3000, 0.3, 0.4), 3000, 0.3, 0.4), as.numeric(q), info = "fqDEL(fpDEL(q)) == q across 4096")
q <- c(65536L, 70000L, 120000L, 398008L)
expect_identical(fqDEL(fpDEL(q, 1e5, 0.3, 0.4), 1e5, 0.3, 0.4), as.numeric(q), info = "fqDEL(fpDEL(q)) == q far beyond 4096")
# below 4096 the CDF is still the running double sum of the densities, bit for bit (as in the first block) ...
for (pr in list(c(3000, 0.3, 0.4), c(2000, 3, 0.9))) {
  expect_identical(fpDEL(4095L, pr[1], pr[2], pr[3]), Reduce(`+`, fdDEL(0:4095, pr[1], pr[2], pr[3])),
                   info = sprintf("fpDEL(4095) is the running double sum of fdDEL(0:4095) at mu=%g sigma=%g nu=%g", pr[1], pr[2], pr[3]))
}
# ... and beyond it too when the densities below 4096 are 0 (mu = 7e4: they underflow), since both then add the
# same accurate terms in the same order
expect_identical(fpDEL(70000L, 7e4, 0.3, 0.4), Reduce(`+`, fdDEL(0:70000, 7e4, 0.3, 0.4)),
                 info = "fpDEL(70000) is the running double sum of fdDEL(0:70000) at mu = 7e4")
# the Poisson branch (sigma < 1e-4) does not use the recurrence
expect_equal(fpDEL(100000L, 1e5, 5e-5, 0.5), ppois(100000, 1e5), tolerance = 1e-11, info = "fpDEL, Poisson branch, q = 1e5")

# --- odd parameters at counts from 4096 on stay finite where they were ---
# (a distribution concentrated at 0: the CDF is 1 and the density 0; a huge mu: both 0; NaN stays NaN)
for (qq in c(4095L, 4096L, 70000L)) {
  expect_equal(fpDEL(qq, c(1e-300, 1e-12, 1e-3), 0.5, 0.5), c(1, 1, 1), tolerance = 1e-12, info = sprintf("fpDEL(%d) for a mu near 0", qq))
  expect_identical(fdDEL(qq, c(1e-300, 1e-12, 1e-3), 0.5, 0.5), c(0, 0, 0), info = sprintf("fdDEL(%d) for a mu near 0", qq))
  expect_identical(fpDEL(qq, 1e300, 1, 0.5), 0, info = sprintf("fpDEL(%d) for a huge mu", qq))
  expect_identical(fdDEL(qq, 1e300, 1, 0.5), 0, info = sprintf("fdDEL(%d) for a huge mu", qq))
  expect_warning(fpDEL(qq, 2, NaN, 0.5), pattern = "NaNs or NAs", info = sprintf("fpDEL(%d) with a NaN sigma warns", qq))
  expect_true(is.na(suppressWarnings(fpDEL(qq, 2, NaN, 0.5))), info = sprintf("fpDEL(%d) with a NaN sigma is NA", qq))
}

# --- the larger cases (seconds each, so only at home): fqDEL at mu = 1e7 .. 1e9 ---
if (at_home()) {
  # before: 9411879 (31 too high); margins F - p = 1.1e-7, p - F(q - 1) = 1.8e-8
  expect_identical(fqDEL(0.5, 1e7, 0.3, 0.4), 9411848, info = "fqDEL(0.5, 1e7, 0.3, 0.4) is 9411848 (it was 9411879)")
  # before: NA after 12.6 s although the median exists; margins 1.1e-9 and 2.4e-8
  expect_identical(fqDEL(0.5, 1e8, 1, 0.8), 93862945, info = "fqDEL(0.5, 1e8, 1, 0.8) is 93862945 (it was a false NA)")
  expect_true(abs(fpDEL(93862945L, 1e8, 1, 0.8) - ref_pDEL_conv(93862945, 1e8, 1, 0.8)) < 1e-10,
              info = "fpDEL(93862945, 1e8, 1, 0.8) is within 1e-10 of the convolution")
  # before: 412402586, where the CDF is 1.4e-5; margins 1.05e-9 and 2.4e-10 (about 11 s)
  expect_identical(fqDEL(0.5, 1e9, 0.3, 0.4), 941184756, info = "fqDEL(0.5, 1e9, 0.3, 0.4) is 941184756 (it was 412402586)")
}
