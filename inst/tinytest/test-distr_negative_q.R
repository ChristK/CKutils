# Every count-distribution CDF wrapper accepts q below the support (q < 0).
#
# IMPACTncd draws an exposure as qfun(minq + rank * (maxq - minq)) with
# minq = pfun(min_value) and maxq = pfun(max_value). With min_value = 0 that is
# minq = P(Y <= 0) = P(Y = 0), so every draw lies above the zero mass and no simulant
# is ever a 0 (0 zeros in 68,887 person-years of alcohol drawn with ZINBI). The fix is
# min_value = -1: minq = pfun(-1) = 0, as gamlss.dist's pZAGA(-1) is. The wrappers used
# to stop with "q must be >=0", which made that fix impossible.
#
# Below the support the CDF is 0, on the four (lower_tail, log_p) scales:
#   lower_tail = TRUE,  log_p = FALSE -> 0
#   lower_tail = TRUE,  log_p = TRUE  -> -Inf
#   lower_tail = FALSE, log_p = FALSE -> 1
#   lower_tail = FALSE, log_p = TRUE  -> 0
# for any negative q (-Inf, -1e10, -1, and the non-integers in (-1, 0) such as -0.5 or
# -1e-300), except that a NA / NaN parameter gives NA there (as pnbinom does, and as
# these functions do for q >= 0): an infinite parameter keeps the below-support value.
# Nothing else changes: NA / NaN q is NA, a bad parameter still stops with the same
# message (also when q is negative), and a non-negative q gives the same value as before
# (a non-integer q >= 0 is truncated, which equals floor there).
#
# fpMN4 (categorical on 1:4, and 0 below its support) takes a q that is a double too,
# so that -Inf and -1e10 are not coerced to NA: section 11. The continuous fpBCPE and
# fpBCPEo (support q > 0) give the same values for q <= 0 and no longer stop on a
# negative q: section 12. fpBCT and every d* and q* function are out of scope.

# Set tolerance for floating point comparisons
tolerance <- sqrt(.Machine$double.eps)

# =============================================================================
# THE TEN FUNCTIONS
# =============================================================================
# par, par2: two valid parameter sets (the second only to make a vector of parameters
# that differs along the vector). Small mu everywhere, so that P(Y = 0) is sizeable.
# bad: each validated parameter, the invalid values, and the exact message of the stop.
bad_par <- function(arg, values, msg) list(arg = arg, values = values, msg = msg)
msg_mu    <- "mu must be greater than 0"
msg_sigma <- "sigma must be greater than 0"
bad_pos   <- c(0, -1)           # "must be greater than 0"
bad_unit  <- c(0, 1, -0.5, 1.5) # "must be between 0 and 1" (open interval)

cnt <- list(
  NBI = list(
    fp = fpNBI, fq = fqNBI,
    par = list(mu = 1.2, sigma = 0.8),
    par2 = list(mu = 2.5, sigma = 0.4),
    bad = list(bad_par("mu", bad_pos, msg_mu), bad_par("sigma", bad_pos, msg_sigma))),
  BNB = list(
    fp = fpBNB, fq = fqBNB,
    par = list(mu = 0.8, sigma = 0.6, nu = 1.5),
    par2 = list(mu = 2, sigma = 0.9, nu = 2.5),
    bad = list(bad_par("mu", bad_pos, msg_mu), bad_par("sigma", bad_pos, msg_sigma),
               bad_par("nu", bad_pos, "nu must be greater than 0"))),
  SICHEL = list(
    fp = fpSICHEL, fq = fqSICHEL,
    par = list(mu = 0.8, sigma = 0.7, nu = -0.5),
    par2 = list(mu = 1.5, sigma = 0.5, nu = 1),
    bad = list(bad_par("mu", bad_pos, msg_mu), bad_par("sigma", bad_pos, msg_sigma))),
  DPO = list(
    fp = fpDPO, fq = fqDPO,
    par = list(mu = 0.8, sigma = 1.3),
    par2 = list(mu = 2, sigma = 0.7),
    bad = list(bad_par("mu", bad_pos, msg_mu), bad_par("sigma", bad_pos, msg_sigma))),
  DEL = list(
    fp = fpDEL, fq = fqDEL,
    par = list(mu = 0.8, sigma = 1.2, nu = 0.5),
    par2 = list(mu = 2, sigma = 0.8, nu = 0.7),
    bad = list(bad_par("mu", bad_pos, msg_mu), bad_par("sigma", bad_pos, msg_sigma),
               bad_par("nu", bad_unit, "nu must be between 0 and 1"))),
  ZINBI = list(
    fp = fpZINBI, fq = fqZINBI,
    par = list(mu = 1.2, sigma = 0.8, nu = 0.3),
    par2 = list(mu = 2.5, sigma = 0.4, nu = 0.1),
    bad = list(bad_par("mu", bad_pos, msg_mu), bad_par("sigma", bad_pos, msg_sigma),
               bad_par("nu", bad_unit, "nu must be between 0 and 1"))),
  ZANBI = list(
    fp = fpZANBI, fq = fqZANBI,
    par = list(mu = 1.2, sigma = 0.8, nu = 0.3),
    par2 = list(mu = 2.5, sigma = 0.4, nu = 0.1),
    bad = list(bad_par("mu", bad_pos, msg_mu), bad_par("sigma", bad_pos, msg_sigma),
               bad_par("nu", bad_unit, "nu must be between 0 and 1"))),
  ZIBNB = list(
    fp = fpZIBNB, fq = fqZIBNB,
    par = list(mu = 0.8, sigma = 0.6, nu = 1.5, tau = 0.3),
    par2 = list(mu = 2, sigma = 0.9, nu = 2.5, tau = 0.15),
    bad = list(bad_par("mu", bad_pos, msg_mu), bad_par("sigma", bad_pos, msg_sigma),
               bad_par("nu", bad_pos, "nu must be greater than 0"),
               bad_par("tau", bad_unit, "tau must be >0 and <1"))),
  ZABNB = list(
    fp = fpZABNB, fq = fqZABNB,
    par = list(mu = 0.8, sigma = 0.6, nu = 1.5, tau = 0.3),
    par2 = list(mu = 2, sigma = 0.9, nu = 2.5, tau = 0.15),
    bad = list(bad_par("mu", bad_pos, msg_mu), bad_par("sigma", bad_pos, msg_sigma),
               bad_par("nu", bad_pos, "nu must be greater than 0"),
               bad_par("tau", bad_unit, "tau must be >0 and <1"))),
  ZISICHEL = list(
    fp = fpZISICHEL, fq = fqZISICHEL,
    par = list(mu = 0.8, sigma = 0.7, nu = -0.5, tau = 0.3),
    par2 = list(mu = 1.5, sigma = 0.5, nu = 1, tau = 0.15),
    bad = list(bad_par("mu", bad_pos, msg_mu), bad_par("sigma", bad_pos, msg_sigma),
               bad_par("tau", bad_unit, "tau must be between 0 and 1")))
)

# The four (lower_tail, log_p) scales, and the CDF below the support on each
tails <- expand.grid(lower_tail = c(TRUE, FALSE), log_p = c(FALSE, TRUE))
below_support <- function(lower_tail, log_p) {
  if (lower_tail) {
    if (log_p) -Inf else 0
  } else {
    if (log_p) 0 else 1
  }
}

# The CDF wrapper of spec s at q; par defaults to the valid set of s
cdf <- function(s, q, par = s$par, lower_tail = TRUE, log_p = FALSE) {
  do.call(s$fp, c(list(q), par, list(lower_tail = lower_tail, log_p = log_p)))
}

# A vector of parameters that differs along the vector: sigma has 4 values, the others 2
# (both divide the 8 positions of the mixed vector below, so recycling is exact)
vpar <- function(s) {
  out <- lapply(seq_along(s$par), function(k) {
    a <- s$par[[k]]
    b <- s$par2[[k]]
    if (k == 2L) c(a, b, b, a) else c(a, b)
  })
  names(out) <- names(s$par)
  out
}
# the parameters of position i of a recycled vector of parameters
at <- function(par, i) lapply(par, function(v) v[(i - 1L) %% length(v) + 1L])

# =============================================================================
# 1. BELOW THE SUPPORT THE CDF IS 0, ON ALL FOUR SCALES
# =============================================================================
qneg <- c(-Inf, -1e10, -3, -1, -0.5, -1e-300)

for (nm in names(cnt)) {
  s <- cnt[[nm]]
  for (k in seq_len(nrow(tails))) {
    lt <- tails$lower_tail[k]
    lp <- tails$log_p[k]
    b <- below_support(lt, lp)
    lab <- sprintf("fp%s(q < 0), lower_tail = %s, log_p = %s", nm, lt, lp)
    # one q at a time
    expect_identical(
      vapply(qneg, function(x) cdf(s, x, lower_tail = lt, log_p = lp), 0),
      rep(b, length(qneg)), info = paste(lab, "scalar q"))
    # all of them in one vector
    expect_identical(cdf(s, qneg, lower_tail = lt, log_p = lp),
                     rep(b, length(qneg)), info = paste(lab, "vector q"))
    # a long vector, past any chunk or SIMD block of the wrappers
    expect_identical(cdf(s, rep_len(qneg, 101L), lower_tail = lt, log_p = lp),
                     rep(b, 101L), info = paste(lab, "long vector q"))
  }
}

# =============================================================================
# 2. INTEGER q
# =============================================================================
for (nm in names(cnt)) {
  s <- cnt[[nm]]
  for (k in seq_len(nrow(tails))) {
    lt <- tails$lower_tail[k]
    lp <- tails$log_p[k]
    b <- below_support(lt, lp)
    lab <- sprintf("fp%s(q < 0L), lower_tail = %s, log_p = %s", nm, lt, lp)
    expect_identical(
      vapply(c(-1L, -5L), function(x) cdf(s, x, lower_tail = lt, log_p = lp), 0),
      rep(b, 2L), info = paste(lab, "scalar q"))
    expect_identical(cdf(s, c(-5L, -1L, -1L, -5L, -5L), lower_tail = lt, log_p = lp),
                     rep(b, 5L), info = paste(lab, "vector q"))
  }
}

# =============================================================================
# 3. A MIXED VECTOR: negative q are below the support, the others are as before
# =============================================================================
# Vector parameters recycled over the 8 positions. The non-negative positions must be
# identical() to a call on those positions alone, with the matching parameters, and to a
# call on each of them alone (2.5 is truncated to 2 there, as before).
q8 <- c(-1, 0, 1, 2.5, -0.5, 5, -Inf, 7)
neg8 <- q8 < 0

for (nm in names(cnt)) {
  s <- cnt[[nm]]
  vp <- vpar(s)
  vp_pos <- lapply(vp, function(v) rep_len(v, length(q8))[!neg8])
  for (k in seq_len(nrow(tails))) {
    lt <- tails$lower_tail[k]
    lp <- tails$log_p[k]
    lab <- sprintf("fp%s mixed q, lower_tail = %s, log_p = %s", nm, lt, lp)
    expect_identical(cdf(s, q8, vp, lt, lp)[neg8],
                     rep(below_support(lt, lp), sum(neg8)),
                     info = paste(lab, "negative positions"))
    expect_identical(cdf(s, q8, vp, lt, lp)[!neg8],
                     cdf(s, q8[!neg8], vp_pos, lt, lp),
                     info = paste(lab, "non-negative positions, subset call"))
    expect_identical(cdf(s, q8, vp, lt, lp)[!neg8],
                     vapply(which(!neg8),
                            function(i) cdf(s, q8[i], at(vp, i), lt, lp), 0),
                     info = paste(lab, "non-negative positions, one at a time"))
  }
}

# =============================================================================
# 4. NA / NaN q STAY NA
# =============================================================================
# (fpDPO and fpDEL warn when an element is NA, as before: the warning is not the point)
for (nm in names(cnt)) {
  s <- cnt[[nm]]
  expect_true(is.na(suppressWarnings(cdf(s, NA_real_))),
              info = paste0("fp", nm, "(NA_real_)"))
  expect_true(is.na(suppressWarnings(cdf(s, NaN))),
              info = paste0("fp", nm, "(NaN)"))
  expect_true(is.na(suppressWarnings(cdf(s, NA_integer_))),
              info = paste0("fp", nm, "(NA_integer_)"))
  # NA among negative and non-negative q: NA only where q is NA
  qn <- c(-1, NA, -Inf, NaN, 1)
  for (k in seq_len(nrow(tails))) {
    lt <- tails$lower_tail[k]
    lp <- tails$log_p[k]
    lab <- sprintf("fp%s NA among q, lower_tail = %s, log_p = %s", nm, lt, lp)
    expect_identical(is.na(suppressWarnings(cdf(s, qn, lower_tail = lt, log_p = lp))),
                     c(FALSE, TRUE, FALSE, TRUE, FALSE), info = lab)
    expect_identical(suppressWarnings(cdf(s, qn, lower_tail = lt, log_p = lp))[c(1, 3)],
                     rep(below_support(lt, lp), 2L), info = lab)
  }
  # integer NA among negative integers
  expect_identical(is.na(suppressWarnings(cdf(s, c(-1L, NA_integer_, -5L)))),
                   c(FALSE, TRUE, FALSE), info = paste0("fp", nm, "(integer q with NA)"))
}

# =============================================================================
# 5. AN INVALID PARAMETER STILL STOPS, ALSO WHEN q IS NEGATIVE
# =============================================================================
# The messages differ between the functions: each is pinned exactly.
with_par <- function(s, arg, value) {
  p <- s$par
  p[[arg]] <- value
  p
}

for (nm in names(cnt)) {
  s <- cnt[[nm]]
  for (b in s$bad) {
    pat <- paste0("^", b$msg, "$")
    good <- s$par[[b$arg]]
    lab <- sprintf("fp%s, invalid %s", nm, b$arg)
    # every invalid value, q = -1 (the one the IMPACTncd fix uses)
    for (v in b$values) {
      expect_error(cdf(s, -1, with_par(s, b$arg, v)), pat,
                   info = sprintf("%s = %s, q = -1", lab, v))
      # the same error for a non-negative q: unchanged
      expect_error(cdf(s, 1, with_par(s, b$arg, v)), pat,
                   info = sprintf("%s = %s, q = 1", lab, v))
    }
    v1 <- b$values[1L]
    bp <- with_par(s, b$arg, v1)
    for (q in c(-Inf, -1e10, -0.5, -1e-300)) {
      expect_error(cdf(s, q, bp), pat, info = sprintf("%s, q = %s", lab, q))
    }
    expect_error(cdf(s, -1L, bp), pat, info = paste(lab, "q = -1L"))
    expect_error(cdf(s, -1, bp, lower_tail = FALSE, log_p = TRUE), pat,
                 info = paste(lab, "upper log tail"))
    # in a vector: the invalid value at the negative q, and at the non-negative one
    expect_error(cdf(s, c(-1, 2), with_par(s, b$arg, c(v1, good))), pat,
                 info = paste(lab, "at the negative q of c(-1, 2)"))
    expect_error(cdf(s, c(-1, 2), with_par(s, b$arg, c(good, v1))), pat,
                 info = paste(lab, "at the non-negative q of c(-1, 2)"))
    expect_error(cdf(s, c(-1, -Inf), with_par(s, b$arg, c(good, v1))), pat,
                 info = paste(lab, "at the second negative q of c(-1, -Inf)"))
  }
}

# =============================================================================
# 6. AGREEMENT WITH BASE R WHERE BASE R DEFINES THE SAME DISTRIBUTION
# =============================================================================
# NBI(mu, sigma) is the negative binomial with size = 1 / sigma and mean mu. pnbinom
# truncates a non-integer q >= 0 to floor(q), as the wrappers do, and gives 0 below 0.
qb <- c(-Inf, -2.5, -1, -0.5, 0, 0.5, 1, 2.5, 3, 12)
qb_pos <- qb[qb >= 0]
mu0 <- 1.2
sigma0 <- 0.8
nu0 <- 0.3
size0 <- 1 / sigma0
# the reference CDF p (lower tail, natural scale) on the requested scale
on_scale <- function(p, lower_tail, log_p) {
  if (!lower_tail) p <- 1 - p
  if (log_p) log(p) else p
}
refs <- list(
  NBI = list(
    ck = function(q, lt, lp) fpNBI(q, mu0, sigma0, lower_tail = lt, log_p = lp),
    ref = function(q, lt, lp) {
      pnbinom(q, size = size0, mu = mu0, lower.tail = lt, log.p = lp)
    },
    what = "pnbinom"),
  # ZINBI: the NBI with a point mass nu0 added at 0
  ZINBI = list(
    ck = function(q, lt, lp) fpZINBI(q, mu0, sigma0, nu0, lower_tail = lt, log_p = lp),
    ref = function(q, lt, lp) {
      on_scale(ifelse(q < 0, 0, nu0 + (1 - nu0) * pnbinom(q, size = size0, mu = mu0)),
               lt, lp)
    },
    what = "the NBI with a point mass at 0"),
  # ZANBI: the NBI truncated at 0, with the mass nu0 at 0
  ZANBI = list(
    ck = function(q, lt, lp) fpZANBI(q, mu0, sigma0, nu0, lower_tail = lt, log_p = lp),
    ref = function(q, lt, lp) {
      p0 <- dnbinom(0, size = size0, mu = mu0)
      on_scale(ifelse(q < 0, 0,
                      nu0 + (1 - nu0) * (pnbinom(q, size = size0, mu = mu0) - p0) / (1 - p0)),
               lt, lp)
    },
    what = "the NBI truncated at 0 with a point mass at 0")
)

for (nm in names(refs)) {
  r <- refs[[nm]]
  for (k in seq_len(nrow(tails))) {
    lt <- tails$lower_tail[k]
    lp <- tails$log_p[k]
    lab <- sprintf("fp%s vs %s, lower_tail = %s, log_p = %s", nm, r$what, lt, lp)
    # q >= 0 (unchanged by the change)
    expect_equal(r$ck(qb_pos, lt, lp), r$ref(qb_pos, lt, lp), tolerance = tolerance,
                 info = paste(lab, "q >= 0"))
    # the whole grid, below the support included
    expect_equal(r$ck(qb, lt, lp), r$ref(qb, lt, lp), tolerance = tolerance,
                 info = paste(lab, "all q"))
  }
}

# =============================================================================
# 7. THE BOUNDARY: F(-1e-300) = 0 < F(0), AND F IS MONOTONE ACROSS IT
# =============================================================================
qgrid <- c(-Inf, -1e10, -3, -1, -0.5, -1e-300, 0:6)

for (nm in names(cnt)) {
  s <- cnt[[nm]]
  lab <- paste0("fp", nm)
  expect_true(cdf(s, 0) > 0, info = paste(lab, "P(Y <= 0) > 0"))
  expect_identical(cdf(s, -1e-300), 0, info = paste(lab, "F(-1e-300) = 0"))
  expect_true(cdf(s, -1e-300) < cdf(s, 0), info = paste(lab, "F(-1e-300) < F(0)"))
  expect_identical(cdf(s, c(-1e-300, 0))[1L], 0,
                   info = paste(lab, "F(-1e-300) in a vector"))
  # non-decreasing over the whole grid, and the upper tail non-increasing
  expect_false(is.unsorted(cdf(s, qgrid)), info = paste(lab, "F non-decreasing"))
  expect_false(is.unsorted(-cdf(s, qgrid, lower_tail = FALSE)),
               info = paste(lab, "upper tail non-increasing"))
  # F(q) + upper tail = 1 below the support too
  expect_identical(cdf(s, qneg) + cdf(s, qneg, lower_tail = FALSE), rep(1, length(qneg)),
                   info = paste(lab, "lower + upper tail = 1 below the support"))
}

# =============================================================================
# 8. THE IMPACTncd USE CASE: min_value = -1 KEEPS THE ZEROS, min_value = 0 LOSES THEM
# =============================================================================
# N stratified ranks, maxq = F(80) as the model's max_value. The draw is
# fq(minq + rank * (maxq - minq)), clamped to [eps, 1 - eps]. With minq = F(-1) = 0 the
# draws below F(0) are the zeros, a share F(0) / maxq (to within the 1 / N of the ranks
# and the 1 / N of the count). With minq = F(0) every draw is above the zero mass.
N <- 10000L
u <- (seq_len(N) - 0.5) / N
eps <- 1e-8
draw <- function(s, minq, maxq) {
  p <- pmin(pmax(minq + u * (maxq - minq), eps), 1 - eps)
  do.call(s$fq, c(list(p), s$par))
}

for (nm in names(cnt)) {
  s <- cnt[[nm]]
  lab <- paste0("fq", nm)
  maxq <- cdf(s, 80)
  p0 <- cdf(s, 0)        # P(Y = 0)
  expect_true(p0 > 0.1 && p0 < maxq, info = paste(lab, "has a sizeable zero mass"))

  # min_value = 0: minq = P(Y = 0), the draws start above the zero mass, no zeros
  d0 <- draw(s, p0, maxq)
  expect_identical(sum(d0 == 0), 0L, info = paste(lab, "min_value = 0 draws no zeros"))
  expect_false(anyNA(d0), info = paste(lab, "min_value = 0 draws no NA"))

  # min_value = -1: minq = F(-1) = 0, the draws keep the zeros
  d1 <- draw(s, cdf(s, -1), maxq)
  expect_false(anyNA(d1), info = paste(lab, "min_value = -1 draws no NA"))
  expect_true(abs(mean(d1 == 0) - p0 / maxq) <= 2 / N,
              info = paste(lab, "min_value = -1: zero share is F(0) / maxq"))
  expect_true(min(d1) == 0 && max(d1) <= 80,
              info = paste(lab, "min_value = -1: draws within 0..max_value"))
}

# =============================================================================
# 9. q >= 0 IS UNCHANGED: TRUNCATION, AND NA ABOVE THE INTEGER RANGE
# =============================================================================
# A non-integer q >= 0 is truncated (floor there); a q above 2147483646, the largest
# integer the kernels take, is NA (+Inf too), also next to a negative q.
for (nm in names(cnt)) {
  s <- cnt[[nm]]
  lab <- paste0("fp", nm)
  expect_identical(cdf(s, c(0.999, 1.5, 2.999)), cdf(s, c(0, 1, 2)),
                   info = paste(lab, "truncates a non-integer q >= 0"))
  expect_true(all(is.na(suppressWarnings(cdf(s, c(2147483647, 1e10, Inf))))),
              info = paste(lab, "q above the integer range is NA"))
  expect_identical(is.na(suppressWarnings(cdf(s, c(-1, 1e10, 1, -Inf)))),
                   c(FALSE, TRUE, FALSE, FALSE),
                   info = paste(lab, "q above the integer range, next to negative q"))
}

# =============================================================================
# 10. A NA / NaN PARAMETER GIVES NA, ALSO WHEN q IS NEGATIVE; AN INFINITE ONE DOES NOT
# =============================================================================
# pnbinom(-1, size = 1, mu = NA) is NA, and so is fpNBI(0, mu = NA, sigma = 1): the value
# below the support must not hide a parameter that is NA. F = 0 below 0 holds for any
# parameter, an infinite one included, so that keeps the below-support value (where the
# parameter's own range lets it through: nu and tau in (0, 1) reject Inf, as in section 5).
# (fpSICHEL, fpDPO and fpDEL warn when an element is NA, as for q >= 0: not the point)
unit_arg <- function(s, arg) {
  any(vapply(s$bad, function(b) b$arg == arg && identical(b$values, bad_unit), NA))
}
free_arg <- function(s, arg) !any(vapply(s$bad, function(b) b$arg == arg, NA))

for (nm in names(cnt)) {
  s <- cnt[[nm]]
  for (arg in names(s$par)) {
    good <- s$par[[arg]]
    for (v in list(NA_real_, NaN)) {
      bp <- with_par(s, arg, v)
      for (k in seq_len(nrow(tails))) {
        lt <- tails$lower_tail[k]
        lp <- tails$log_p[k]
        lab <- sprintf("fp%s, %s = %s, lower_tail = %s, log_p = %s", nm, arg, format(v), lt, lp)
        # q = -1 (the value the IMPACTncd fix uses), and the same parameter with q = 1
        expect_true(is.na(suppressWarnings(cdf(s, -1, bp, lt, lp))),
                    info = paste(lab, "q = -1"))
        expect_true(is.na(suppressWarnings(cdf(s, 1, bp, lt, lp))),
                    info = paste(lab, "q = 1"))
        # the other negative q, in one vector: all NA
        expect_true(all(is.na(suppressWarnings(cdf(s, c(-Inf, -1e10, -0.5, -1e-300), bp, lt, lp)))),
                    info = paste(lab, "other negative q"))
        # NA only where the parameter is NA: a vector of parameters, recycled over q
        expect_identical(
          is.na(suppressWarnings(cdf(s, c(-1, -1, -Inf, -0.5), with_par(s, arg, c(v, good)), lt, lp))),
          c(TRUE, FALSE, TRUE, FALSE), info = paste(lab, "NA parameter at some positions only"))
        expect_identical(
          suppressWarnings(cdf(s, c(-1, -1), with_par(s, arg, c(v, good)), lt, lp))[2L],
          below_support(lt, lp), info = paste(lab, "the other position is below the support"))
      }
    }
    # an infinite parameter (-Inf only where the parameter has no range check)
    for (v in c(Inf, if (free_arg(s, arg)) -Inf)) {
      if (unit_arg(s, arg)) next
      ip <- with_par(s, arg, v)
      for (k in seq_len(nrow(tails))) {
        lt <- tails$lower_tail[k]
        lp <- tails$log_p[k]
        expect_identical(cdf(s, c(-1, -Inf, -0.5), ip, lt, lp),
                         rep(below_support(lt, lp), 3L),
                         info = sprintf("fp%s, %s = %s, lower_tail = %s, log_p = %s",
                                        nm, arg, v, lt, lp))
      }
    }
  }
}

# =============================================================================
# 11. fpMN4: A q BELOW ITS SUPPORT (1:4) IS NOT COERCED TO NA
# =============================================================================
# fpMN4 took an IntegerVector q: R coerced -Inf and -1e10 to NA (with a warning) and
# -0.5 to 0 before the function ran, the trap removed from fpBNB, fpDEL and fpDPO. It
# takes a double now, and maps q < 0 to the value below the support, as the other ten do.
# Everything else is as before: q >= 0 gives the values of 0be2c75 (constants below).
mn4 <- list(mu = 1, sigma = 2, nu = 0.5)
fp_mn4 <- function(q, par = mn4, lower_tail = TRUE, log_p = FALSE) {
  do.call(fpMN4, c(list(q), par, list(lower_tail = lower_tail, log_p = log_p)))
}
qneg_mn4 <- c(-Inf, -1e10, -1, -0.5, -1e-300)

for (k in seq_len(nrow(tails))) {
  lt <- tails$lower_tail[k]
  lp <- tails$log_p[k]
  b <- below_support(lt, lp)
  lab <- sprintf("fpMN4(q < 0), lower_tail = %s, log_p = %s", lt, lp)
  # no warning (the coercion used to give one), one q at a time and all in one vector
  for (x in qneg_mn4) {
    expect_silent(fp_mn4(x, lower_tail = lt, log_p = lp))
    expect_identical(fp_mn4(x, lower_tail = lt, log_p = lp), b, info = paste(lab, "q =", x))
  }
  expect_silent(fp_mn4(qneg_mn4, lower_tail = lt, log_p = lp))
  expect_identical(fp_mn4(qneg_mn4, lower_tail = lt, log_p = lp), rep(b, length(qneg_mn4)),
                   info = paste(lab, "vector q"))
  expect_identical(fp_mn4(rep_len(qneg_mn4, 101L), lower_tail = lt, log_p = lp),
                   rep(b, 101L), info = paste(lab, "long vector q"))
  # integer q
  expect_silent(fp_mn4(-1L, lower_tail = lt, log_p = lp))
  expect_identical(fp_mn4(-1L, lower_tail = lt, log_p = lp), b, info = paste(lab, "q = -1L"))
  expect_identical(fp_mn4(c(-5L, -1L, -1L), lower_tail = lt, log_p = lp), rep(b, 3L),
                   info = paste(lab, "integer vector q"))
}

# a mixed vector, with vector parameters: negative q are below the support, the others
# are as a call on those positions alone
q8m <- c(-1, 0, 1, 2.5, -0.5, 5, -Inf, 3)
neg8m <- q8m < 0
vmn4 <- list(mu = c(1, 2), sigma = c(2, 0.5, 1, 3), nu = 0.5)
vmn4_pos <- lapply(vmn4, function(v) rep_len(v, length(q8m))[!neg8m])
for (k in seq_len(nrow(tails))) {
  lt <- tails$lower_tail[k]
  lp <- tails$log_p[k]
  lab <- sprintf("fpMN4 mixed q, lower_tail = %s, log_p = %s", lt, lp)
  res <- fp_mn4(q8m, vmn4, lt, lp)
  expect_identical(res[neg8m], rep(below_support(lt, lp), sum(neg8m)),
                   info = paste(lab, "negative positions"))
  expect_identical(res[!neg8m], fp_mn4(q8m[!neg8m], vmn4_pos, lt, lp),
                   info = paste(lab, "non-negative positions"))
}

# NA / NaN q stay NA; a NA / NaN parameter at a negative q is NA, and an infinite one is
# not; an invalid parameter (mu <= 0) is NaN at a negative q as it is for q >= 1
# (fpMN4 does not stop on one, as the ten do)
expect_true(is.na(suppressWarnings(fp_mn4(NA_real_))), info = "fpMN4(NA_real_)")
expect_true(is.na(suppressWarnings(fp_mn4(NaN))), info = "fpMN4(NaN)")
expect_true(is.na(suppressWarnings(fp_mn4(NA_integer_))), info = "fpMN4(NA_integer_)")
expect_identical(is.na(suppressWarnings(fp_mn4(c(-1, NA, -Inf, NaN, 1)))),
                 c(FALSE, TRUE, FALSE, TRUE, FALSE), info = "fpMN4: NA among negative q")
for (arg in names(mn4)) {
  for (v in list(NA_real_, NaN)) {
    for (k in seq_len(nrow(tails))) {
      lt <- tails$lower_tail[k]
      lp <- tails$log_p[k]
      bp <- mn4
      bp[[arg]] <- v
      lab <- sprintf("fpMN4, %s = %s, lower_tail = %s, log_p = %s", arg, format(v), lt, lp)
      expect_true(all(is.na(suppressWarnings(fp_mn4(c(-1, -Inf, -1e10, -0.5), bp, lt, lp)))),
                  info = paste(lab, "negative q"))
    }
  }
  ip <- mn4
  ip[[arg]] <- Inf
  expect_identical(fp_mn4(c(-1, -Inf), ip), c(0, 0), info = paste("fpMN4,", arg, "= Inf"))
  zp <- mn4
  zp[[arg]] <- 0
  expect_true(all(is.nan(suppressWarnings(fp_mn4(c(-1, 1), zp)))),
              info = paste("fpMN4,", arg, "= 0 gives NaN at q = -1 and q = 1"))
}

# q >= 0 is as before: the doubles below were computed with the build of 0be2c75
# (q as an IntegerVector then; integer-valued and truncated non-integer q, q beyond the
# categories, and 2147483646, the largest count)
# --- q >= 0 against 0be2c75: begin
q0 <- c(0, 0.5, 1, 1.5, 2, 2.9, 3, 4, 5, 100, 2147483646)
mn4_lower <- c(0, 0, 0.22222222222222221, 0.22222222222222221, 0.66666666666666663,
               0.66666666666666663, 0.77777777777777779, 1, 1, 1, 1)
mn4_upper <- c(1, 1, 0.77777777777777779, 0.77777777777777779, 0.33333333333333337,
               0.33333333333333337, 0.22222222222222221, 0, 0, 0, 0)
expect_identical(fp_mn4(q0), mn4_lower, info = "fpMN4 q >= 0, lower tail")
expect_identical(fp_mn4(q0, lower_tail = FALSE), mn4_upper, info = "fpMN4 q >= 0, upper tail")
expect_identical(fp_mn4(q0, log_p = TRUE), log(mn4_lower), info = "fpMN4 q >= 0, log")
expect_identical(fp_mn4(q0, lower_tail = FALSE, log_p = TRUE), log(mn4_upper),
                 info = "fpMN4 q >= 0, upper tail, log")
expect_identical(fp_mn4(1:4), c(0.22222222222222221, 0.66666666666666663,
                                0.77777777777777779, 1), info = "fpMN4(1:4)")
expect_identical(fp_mn4(c(1L, 2L, 3L, 4L, 0L)),
                 c(0.22222222222222221, 0.66666666666666663, 0.77777777777777779, 1, 0),
                 info = "fpMN4(integer q)")
# NA, NA_integer_, and a count beyond the integer range are NA (with the wrapper's warning)
expect_identical(is.na(suppressWarnings(fp_mn4(c(1, NA, 3, NA_integer_, NaN, 2147483647)))),
                 c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE), info = "fpMN4: NA q")
expect_identical(suppressWarnings(fp_mn4(c(1, NA, 3))),
                 c(0.22222222222222221, NA, 0.77777777777777779), info = "fpMN4(c(1, NA, 3))")
expect_warning(fp_mn4(NA_real_), "NaNs were produced", info = "fpMN4(NA) warns as before")
# --- q >= 0 against 0be2c75: end

# =============================================================================
# 12. fpBCPE AND fpBCPEo: q <= 0 IS BELOW THE SUPPORT (q > 0), NOT AN ERROR
# =============================================================================
# fpBCPEo stopped with "q must be positive" for q < 0. Both now give the value below
# the support on the requested tail and scale for every q <= 0 (q = 0 included: the
# support is q > 0), with no warning; a NA / NaN parameter gives NA there, and an
# invalid parameter still stops with its message. (A function this build lacks gives
# NA, and a call that stops gives its message, so a check fails rather than aborts.)
fp_bcpe_fns <- list(
  fpBCPE  = get0("fpBCPE", envir = asNamespace("CKutils"), inherits = FALSE,
                 ifnotfound = function(...) NA_real_),
  fpBCPEo = fpBCPEo)
bcpe_par <- list(mu = 2, sigma = 0.4, nu = 0.7, tau = 1.5)
qneg_bcpe <- c(-Inf, -1e10, -1, -0.5, -1e-300, 0, -1L)
bcpe_val <- function(expr) tryCatch(expr, error = function(e) conditionMessage(e))
for (nm in names(fp_bcpe_fns)) {
  f <- fp_bcpe_fns[[nm]]
  for (k in seq_len(nrow(tails))) {
    lt <- tails$lower_tail[k]
    lp <- tails$log_p[k]
    b <- below_support(lt, lp)
    lab <- sprintf("%s(q <= 0), lower_tail = %s, log_p = %s", nm, lt, lp)
    expect_silent(bcpe_val(do.call(f, c(list(qneg_bcpe), bcpe_par, list(lower_tail = lt, log_p = lp)))))
    expect_identical(bcpe_val(do.call(f, c(list(qneg_bcpe), bcpe_par, list(lower_tail = lt, log_p = lp)))),
                     rep(b, length(qneg_bcpe)), info = lab)
    # with vector parameters (nu of both signs and 0) the value is the same
    expect_identical(bcpe_val(f(c(-1, 0, -Inf, -2), mu = c(1, 3), sigma = 0.2, nu = c(-1, 0, 2, 1e-12),
                                tau = c(0.5, 2, 8, 2), lower_tail = lt, log_p = lp)),
                     rep(b, 4L), info = paste(lab, "vector parameters"))
    # a NA / NaN parameter at a negative q is NA
    for (arg in names(bcpe_par)) {
      bp <- bcpe_par
      bp[[arg]] <- NA_real_
      expect_true(all(is.na(bcpe_val(do.call(f, c(list(c(-1, 0)), bp, list(lower_tail = lt, log_p = lp)))))),
                  info = paste(lab, arg, "= NA"))
    }
  }
  # an invalid parameter still stops, also at a negative q
  expect_error(f(-1, mu = -1, sigma = 1, nu = 1, tau = 2), "mu must be positive", info = nm)
  expect_error(f(-1, mu = 1, sigma = 0, nu = 1, tau = 2), "sigma must be positive", info = nm)
  expect_error(f(-1, mu = 1, sigma = 1, nu = 1, tau = Inf), "tau must be finite", info = nm)
  # the values for q > 0 are untouched by the q <= 0 positions of a vector
  expect_identical(bcpe_val(f(c(-1, 1.5, 0, 3), 2, 0.4, 0.7, 1.5))[c(2, 4)],
                   bcpe_val(f(c(1.5, 3), 2, 0.4, 0.7, 1.5)), info = paste(nm, "mixed vector"))
}
