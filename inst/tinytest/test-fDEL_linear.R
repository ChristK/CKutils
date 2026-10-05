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
