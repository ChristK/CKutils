# Tests targeting otherwise-uncovered C++ branches in src/distr_<NAME>.cpp:
#   1. parameter-validation `stop()` branches in fd/fp/fq, exercised with
#      log_p = TRUE (and lower_tail = FALSE where relevant), and
#   2. the compiled fr<NAME> random generators, which at the R level are
#      shadowed by pure-R versions in R/rng_distr.R and are therefore only
#      reachable via .Call().
# These do NOT depend on gamlss.dist.

library(CKutils)
library(tinytest)

# A q below 0 is below the support, so the CDF is 0 on each scale; the count CDFs
# used to stop with "q must be >=0". Lower tail, upper tail, and their logs.
below_support <- function(f, ..., q = -1) {
  c(f(q, ...), f(q, ..., lower_tail = FALSE),
    f(q, ..., log_p = TRUE), f(q, ..., lower_tail = FALSE, log_p = TRUE))
}
expected_below <- c(0, 1, -Inf, 0)

# --------------------------------------------------------------------------
# 1. Validation / log_p / lower_tail error branches in fq / fp / fd
# --------------------------------------------------------------------------

# NOTE: the log_p = TRUE cases below pass log(0.5), not 0.5. Since check_prob()
# ranges p on the scale actually supplied, a natural-scale 0.5 is p > 1 on the log
# scale and would now be rejected before the parameter checks these lines target.
## ---- NBI (mu, sigma) ----
# fqNBI has a SEPARATE validation loop guarded by `if (log_p)`.
expect_error(fqNBI(log(0.5), mu = -1, sigma = 1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqNBI(log(0.5), mu =  1, sigma = 0, log_p = TRUE), "sigma must be greater than 0")
# fqNBI non-log_p branch (p out of range, and invalid params)
expect_error(fqNBI(-0.1, mu = 1, sigma = 1), "p must be >=0 and <=1")
expect_error(fqNBI(0.5, mu = -1, sigma = 1), "mu must be greater than 0")
expect_error(fqNBI(0.5, mu =  1, sigma = 0), "sigma must be greater than 0")
# fqNBI lower_tail = FALSE combined with log_p = TRUE (normal path)
expect_silent(fqNBI(log(0.5), mu = 2, sigma = 1, lower_tail = FALSE, log_p = TRUE))
# fdNBI / fpNBI validation (unconditional loops)
expect_error(fdNBI(0, mu = -1, sigma = 1, log_p = TRUE), "mu must be greater than 0")
expect_error(fdNBI(0, mu =  1, sigma = 0, log_p = TRUE), "sigma must be greater than 0")
expect_error(fdNBI(-1, mu = 1, sigma = 1), "x must be >=0")
expect_error(fpNBI(0, mu = -1, sigma = 1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpNBI(0, mu =  1, sigma = 0, log_p = TRUE), "sigma must be greater than 0")
expect_identical(below_support(fpNBI, mu = 1, sigma = 1), expected_below)
expect_error(fpNBI(-1, mu = -1, sigma = 1), "mu must be greater than 0")   # a negative q is validated like any other

## ---- ZINBI (mu, sigma, nu in (0,1)) ----
expect_error(fdZINBI(0, mu = -1, sigma = 1, nu = 0.1, TRUE), "mu must be greater than 0")
expect_error(fdZINBI(0, mu = 1, sigma = 0, nu = 0.1, TRUE), "sigma must be greater than 0")
expect_error(fdZINBI(0, mu = 1, sigma = 1, nu = 1.5, TRUE), "nu must be between 0 and 1")
expect_error(fdZINBI(-1, mu = 1, sigma = 1, nu = 0.1), "x must be >=0")
expect_error(fpZINBI(0, mu = -1, sigma = 1, nu = 0.1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpZINBI(0, mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpZINBI(0, mu = 1, sigma = 1, nu = 0, log_p = TRUE), "nu must be between 0 and 1")
expect_identical(below_support(fpZINBI, mu = 1, sigma = 1, nu = 0.1), expected_below)
expect_error(fpZINBI(-1, mu = -1, sigma = 1, nu = 0.1), "mu must be greater than 0")   # a negative q is validated like any other
expect_error(fqZINBI(log(0.5), mu = -1, sigma = 1, nu = 0.1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqZINBI(log(0.5), mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqZINBI(log(0.5), mu = 1, sigma = 1, nu = 2, log_p = TRUE), "nu must be between 0 and 1")
expect_error(fqZINBI(-0.1, mu = 1, sigma = 1, nu = 0.1), "p must be >=0 and <=1")

## ---- ZANBI (mu, sigma, nu in (0,1)) ----
expect_error(fdZANBI(0, mu = -1, sigma = 1, nu = 0.1, TRUE), "mu must be greater than 0")
expect_error(fdZANBI(0, mu = 1, sigma = 0, nu = 0.1, TRUE), "sigma must be greater than 0")
expect_error(fdZANBI(0, mu = 1, sigma = 1, nu = 1.5, TRUE), "nu must be between 0 and 1")
expect_error(fdZANBI(-1, mu = 1, sigma = 1, nu = 0.1), "x must be >=0")
expect_error(fpZANBI(0, mu = -1, sigma = 1, nu = 0.1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpZANBI(0, mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpZANBI(0, mu = 1, sigma = 1, nu = 0, log_p = TRUE), "nu must be between 0 and 1")
expect_identical(below_support(fpZANBI, mu = 1, sigma = 1, nu = 0.1), expected_below)
expect_error(fpZANBI(-1, mu = -1, sigma = 1, nu = 0.1), "mu must be greater than 0")   # a negative q is validated like any other
expect_error(fqZANBI(log(0.5), mu = -1, sigma = 1, nu = 0.1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqZANBI(log(0.5), mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqZANBI(log(0.5), mu = 1, sigma = 1, nu = 2, log_p = TRUE), "nu must be between 0 and 1")
expect_error(fqZANBI(-0.1, mu = 1, sigma = 1, nu = 0.1), "p must be >=0 and <=1")

## ---- BNB (mu, sigma, nu > 0) ----
expect_error(fdBNB(0, mu = -1, sigma = 1, nu = 1, TRUE), "mu must be greater than 0")
expect_error(fdBNB(0, mu = 1, sigma = 0, nu = 1, TRUE), "sigma must be greater than 0")
expect_error(fdBNB(0, mu = 1, sigma = 1, nu = 0, TRUE), "nu must be greater than 0")
expect_error(fdBNB(-1, mu = 1, sigma = 1, nu = 1), "x must be >=0")
expect_error(fpBNB(0, mu = -1, sigma = 1, nu = 1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpBNB(0, mu = 1, sigma = 0, nu = 1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpBNB(0, mu = 1, sigma = 1, nu = 0, log_p = TRUE), "nu must be greater than 0")
expect_identical(below_support(fpBNB, mu = 1, sigma = 1, nu = 1), expected_below)
expect_error(fpBNB(-1, mu = -1, sigma = 1, nu = 1), "mu must be greater than 0")   # a negative q is validated like any other
expect_error(fqBNB(log(0.5), mu = -1, sigma = 1, nu = 1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqBNB(log(0.5), mu = 1, sigma = 0, nu = 1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqBNB(log(0.5), mu = 1, sigma = 1, nu = 0, log_p = TRUE), "nu must be greater than 0")
expect_error(fqBNB(-0.1, mu = 1, sigma = 1, nu = 1), "p must be >=0 and <=1")

## ---- ZABNB (mu, sigma, nu > 0; tau in (0,1)) ----
expect_error(fdZABNB(0, mu = -1, sigma = 1, nu = 1, tau = 0.1, TRUE), "mu must be greater than 0")
expect_error(fdZABNB(0, mu = 1, sigma = 0, nu = 1, tau = 0.1, TRUE), "sigma must be greater than 0")
expect_error(fdZABNB(0, mu = 1, sigma = 1, nu = 0, tau = 0.1, TRUE), "nu must be greater than 0")
expect_error(fdZABNB(0, mu = 1, sigma = 1, nu = 1, tau = 0, TRUE), "tau must be >0 and <1")
expect_error(fdZABNB(0, mu = 1, sigma = 1, nu = 1, tau = 1, TRUE), "tau must be >0 and <1")
expect_error(fdZABNB(-1, mu = 1, sigma = 1, nu = 1, tau = 0.1), "x must be >=0")
expect_error(fpZABNB(0, mu = -1, sigma = 1, nu = 1, tau = 0.1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpZABNB(0, mu = 1, sigma = 0, nu = 1, tau = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpZABNB(0, mu = 1, sigma = 1, nu = 0, tau = 0.1, log_p = TRUE), "nu must be greater than 0")
expect_error(fpZABNB(0, mu = 1, sigma = 1, nu = 1, tau = 1.5, log_p = TRUE), "tau must be >0 and <1")
expect_identical(below_support(fpZABNB, mu = 1, sigma = 1, nu = 1, tau = 0.1), expected_below)
expect_error(fpZABNB(-1, mu = -1, sigma = 1, nu = 1, tau = 0.1), "mu must be greater than 0")   # a negative q is validated like any other
expect_error(fqZABNB(log(0.5), mu = -1, sigma = 1, nu = 1, tau = 0.1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqZABNB(log(0.5), mu = 1, sigma = 0, nu = 1, tau = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqZABNB(log(0.5), mu = 1, sigma = 1, nu = 0, tau = 0.1, log_p = TRUE), "nu must be greater than 0")
expect_error(fqZABNB(log(0.5), mu = 1, sigma = 1, nu = 1, tau = 0, log_p = TRUE), "tau must be >0 and <1")
expect_error(fqZABNB(-0.1, mu = 1, sigma = 1, nu = 1, tau = 0.1), "p must be >=0 and <=1")

## ---- ZIBNB (mu, sigma, nu > 0; tau in (0,1)) ----
expect_error(fqZIBNB(log(0.5), mu = -1, sigma = 1, nu = 1, tau = 0.1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqZIBNB(log(0.5), mu = 1, sigma = 1, nu = 1, tau = 0, log_p = TRUE), "tau must be >0 and <1")
expect_error(fqZIBNB(-0.1, mu = 1, sigma = 1, nu = 1, tau = 0.1), "p must be >=0 and <=1")

## ---- SICHEL (mu, sigma, nu); fd/fp/fq validate mu & sigma ----
expect_error(fdSICHEL(0, mu = -1, sigma = 1, nu = -0.5, log_p = TRUE), "mu must be greater than 0")
expect_error(fdSICHEL(0, mu = 1, sigma = 0, nu = -0.5, log_p = TRUE), "sigma must be greater than 0")
expect_error(fdSICHEL(-1, mu = 1, sigma = 1, nu = -0.5), "x must be >=0")
expect_error(fpSICHEL(0, mu = -1, sigma = 1, nu = -0.5, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpSICHEL(0, mu = 1, sigma = 0, nu = -0.5, log_p = TRUE), "sigma must be greater than 0")
expect_identical(below_support(fpSICHEL, mu = 1, sigma = 1, nu = -0.5), expected_below)
expect_error(fpSICHEL(-1, mu = -1, sigma = 1, nu = -0.5), "mu must be greater than 0")   # a negative q is validated like any other
# fqSICHEL has a SEPARATE `if (log_p)` validation loop.
expect_error(fqSICHEL(log(0.5), mu = -1, sigma = 1, nu = -0.5, log_p = TRUE), "mu must be greater than 0")
expect_error(fqSICHEL(log(0.5), mu = 1, sigma = 0, nu = -0.5, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqSICHEL(0.5, mu = -1, sigma = 1, nu = -0.5), "mu must be greater than 0")
expect_error(fqSICHEL(0.5, mu = 1, sigma = 0, nu = -0.5), "sigma must be greater than 0")

## ---- ZISICHEL (mu, sigma, nu, tau in (0,1)); fp/fq validate mu, sigma, tau ----
expect_error(fpZISICHEL(0, mu = -1, sigma = 1, nu = -0.5, tau = 0.1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpZISICHEL(0, mu = 1, sigma = 0, nu = -0.5, tau = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpZISICHEL(0, mu = 1, sigma = 1, nu = -0.5, tau = 2, log_p = TRUE), "tau must be between 0 and 1")
expect_identical(below_support(fpZISICHEL, mu = 1, sigma = 1, nu = -0.5, tau = 0.1), expected_below)
expect_error(fpZISICHEL(-1, mu = -1, sigma = 1, nu = -0.5, tau = 0.1), "mu must be greater than 0")   # a negative q is validated like any other
# fqZISICHEL has a SEPARATE `if (log_p)` validation loop.
expect_error(fqZISICHEL(log(0.5), mu = -1, sigma = 1, nu = -0.5, tau = 0.1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqZISICHEL(log(0.5), mu = 1, sigma = 0, nu = -0.5, tau = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqZISICHEL(log(0.5), mu = 1, sigma = 1, nu = -0.5, tau = 2, log_p = TRUE), "tau must be between 0 and 1")
expect_error(fqZISICHEL(0.5, mu = -1, sigma = 1, nu = -0.5, tau = 0.1), "mu must be greater than 0")
expect_error(fqZISICHEL(0.5, mu = 1, sigma = 0, nu = -0.5, tau = 0.1), "sigma must be greater than 0")
expect_error(fqZISICHEL(0.5, mu = 1, sigma = 1, nu = -0.5, tau = 2), "tau must be between 0 and 1")

## ---- DEL (mu, sigma, nu in (0,1)) ----
# fdDEL density-log arg is positional (log_); pass positionally as 5th arg.
expect_error(fdDEL(0L, mu = -1, sigma = 1, nu = 0.1, TRUE), "mu must be greater than 0")
expect_error(fdDEL(0L, mu = 1, sigma = 0, nu = 0.1, TRUE), "sigma must be greater than 0")
expect_error(fdDEL(0L, mu = 1, sigma = 1, nu = 1.5, TRUE), "nu must be between 0 and 1")
expect_error(fdDEL(-1L, mu = 1, sigma = 1, nu = 0.1), "x must be >=0")
# x is read as a double: before 0.1.36 R coerced -0.5 and -1e-300 to 0, which
# gave the density at 0, and -Inf to NA, with a coercion warning
for (xx in c(-0.5, -1e-300, -Inf)) {
  expect_error(fdDEL(xx, mu = 1, sigma = 1, nu = 0.1), "x must be >=0", info = paste("fdDEL x =", xx))
}
expect_identical(fdDEL(c(0, 2.7, NA), mu = 2, sigma = 1, nu = 0.5),
                 fdDEL(c(0L, 2L, NA_integer_), mu = 2, sigma = 1, nu = 0.5),
                 info = "fdDEL: a positive non-integer x is truncated, NA stays NA")
expect_error(fpDEL(0L, mu = -1, sigma = 1, nu = 0.1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpDEL(0L, mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpDEL(0L, mu = 1, sigma = 1, nu = 0, log_p = TRUE), "nu must be between 0 and 1")
expect_identical(below_support(fpDEL, mu = 1, sigma = 1, nu = 0.1, q = -1L), expected_below)
expect_error(fpDEL(-1L, mu = -1, sigma = 1, nu = 0.1), "mu must be greater than 0")   # a negative q is validated like any other
expect_error(fqDEL(log(0.5), mu = -1, sigma = 1, nu = 0.1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqDEL(log(0.5), mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqDEL(log(0.5), mu = 1, sigma = 1, nu = 2, log_p = TRUE), "nu must be between 0 and 1")
# fqDEL p-range validation after exp(log_p) transform
expect_error(fqDEL(2, mu = 1, sigma = 1, nu = 0.1), "p must be between 0 and 1")

## ---- DPO (mu, sigma) ----
expect_error(fdDPO(0L, mu = -1, sigma = 1, TRUE), "mu must be greater than 0")
expect_error(fdDPO(0L, mu = 1, sigma = 0, TRUE), "sigma must be greater than 0")
expect_error(fdDPO(-1L, mu = 1, sigma = 1), "x must be >=0")
# x is read as a double (see fdDEL above)
for (xx in c(-0.5, -1e-300, -Inf)) {
  expect_error(fdDPO(xx, mu = 1, sigma = 1.5), "x must be >=0", info = paste("fdDPO x =", xx))
}
expect_identical(fdDPO(c(0, 2.7, NA), mu = 2, sigma = 1.5),
                 fdDPO(c(0L, 2L, NA_integer_), mu = 2, sigma = 1.5),
                 info = "fdDPO: a positive non-integer x is truncated, NA stays NA")
expect_error(fpDPO(0L, mu = -1, sigma = 1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpDPO(0L, mu = 1, sigma = 0, log_p = TRUE), "sigma must be greater than 0")
expect_identical(below_support(fpDPO, mu = 1, sigma = 1, q = -1L), expected_below)
expect_error(fpDPO(-1L, mu = -1, sigma = 1), "mu must be greater than 0")   # a negative q is validated like any other
expect_error(fqDPO(log(0.5), mu = -1, sigma = 1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqDPO(log(0.5), mu = 1, sigma = 0, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqDPO(2, mu = 1, sigma = 1), "p must be between 0 and 1")

## ---- BCT (mu, sigma, nu, tau); validates mu, sigma, tau ----
expect_error(fdBCT(1, mu = -1, sigma = 1, nu = 1, tau = 5, TRUE), "mu must be positive")
expect_error(fdBCT(1, mu = 1, sigma = 0, nu = 1, tau = 5, TRUE), "sigma must be positive")
expect_error(fdBCT(1, mu = 1, sigma = 1, nu = 1, tau = 0, TRUE), "tau must be positive")
expect_error(fdBCT(-1, mu = 1, sigma = 1, nu = 1, tau = 5), "x must be >=0")
expect_error(fpBCT(1, mu = -1, sigma = 1, nu = 1, tau = 5, lower_tail = FALSE, log_p = TRUE), "mu must be positive")
expect_error(fpBCT(1, mu = 1, sigma = 0, nu = 1, tau = 5, log_p = TRUE), "sigma must be positive")
expect_error(fpBCT(1, mu = 1, sigma = 1, nu = 1, tau = 0, log_p = TRUE), "tau must be positive")
expect_error(fpBCT(-1, mu = 1, sigma = 1, nu = 1, tau = 5), "q must be >=0")
# For fqBCT the p-range check runs after the exp() transform, so use log(0.5)
# to keep p valid and reach the mu/sigma/tau checks.
expect_error(fqBCT(log(0.5), mu = -1, sigma = 1, nu = 1, tau = 5, log_p = TRUE), "mu must be positive")
expect_error(fqBCT(log(0.5), mu = 1, sigma = 0, nu = 1, tau = 5, log_p = TRUE), "sigma must be positive")
expect_error(fqBCT(log(0.5), mu = 1, sigma = 1, nu = 1, tau = 0, log_p = TRUE), "tau must be positive")
expect_error(fqBCT(2, mu = 1, sigma = 1, nu = 1, tau = 5), "p must be between 0 and 1")

## ---- BCPEo (mu, sigma, nu, tau); validates mu, sigma, tau ----
expect_error(fdBCPEo(1, mu = -1, sigma = 1, nu = 1, tau = 2, TRUE), "mu must be positive")
expect_error(fdBCPEo(1, mu = 1, sigma = 0, nu = 1, tau = 2, TRUE), "sigma must be positive")
expect_error(fdBCPEo(1, mu = 1, sigma = 1, nu = 1, tau = 0, TRUE), "tau must be positive")
# A negative x or q is below the support, not an error (density 0; F = 0, upper tail 1).
# (A call that stops gives its message, so the comparison fails rather than aborting.)
bcpe_val <- function(expr) tryCatch(expr, error = function(e) conditionMessage(e))
expect_identical(bcpe_val(fdBCPEo(-1, mu = 1, sigma = 1, nu = 1, tau = 2)), 0)
expect_identical(bcpe_val(fpBCPEo(-1, mu = 1, sigma = 1, nu = 1, tau = 2)), 0)
expect_identical(bcpe_val(fpBCPEo(-1, mu = 1, sigma = 1, nu = 1, tau = 2, lower_tail = FALSE)), 1)
expect_error(fpBCPEo(1, mu = -1, sigma = 1, nu = 1, tau = 2, lower_tail = FALSE, log_p = TRUE), "mu must be positive")
expect_error(fpBCPEo(1, mu = 1, sigma = 0, nu = 1, tau = 2, log_p = TRUE), "sigma must be positive")
expect_error(fpBCPEo(1, mu = 1, sigma = 1, nu = 1, tau = 0, log_p = TRUE), "tau must be positive")
expect_error(fqBCPEo(log(0.5), mu = -1, sigma = 1, nu = 1, tau = 2, log_p = TRUE), "mu must be positive")
expect_error(fqBCPEo(log(0.5), mu = 1, sigma = 0, nu = 1, tau = 2, log_p = TRUE), "sigma must be positive")
expect_error(fqBCPEo(log(0.5), mu = 1, sigma = 1, nu = 1, tau = 0, log_p = TRUE), "tau must be positive")
expect_error(fqBCPEo(2, mu = 1, sigma = 1, nu = 1, tau = 2), "p must be between 0 and 1")
expect_error(fqBCPEo(0.5, mu = 1, sigma = 1, nu = 1, tau = 2, log_p = TRUE), "log_p")
# Infinite parameters are errors (nu has no range, but must be finite).
expect_error(fdBCPEo(1, mu = Inf, sigma = 1, nu = 1, tau = 2), "mu must be finite")
expect_error(fpBCPEo(1, mu = 1, sigma = Inf, nu = 1, tau = 2), "sigma must be finite")
expect_error(fqBCPEo(0.5, mu = 1, sigma = 1, nu = -Inf, tau = 2), "nu must be finite")
expect_error(fdBCPEo(1, mu = 1, sigma = 1, nu = 1, tau = Inf), "tau must be finite")

## ---- BCPE: the same validation and messages as BCPEo ----
if (exists("fdBCPE", envir = asNamespace("CKutils"), inherits = FALSE)) {
  bcpe_msgs <- list(
    list(a = list(mu = -1, sigma = 1, nu = 1, tau = 2),  m = "mu must be positive"),
    list(a = list(mu = 0, sigma = 1, nu = 1, tau = 2),   m = "mu must be positive"),
    list(a = list(mu = 1, sigma = 0, nu = 1, tau = 2),   m = "sigma must be positive"),
    list(a = list(mu = 1, sigma = 1, nu = 1, tau = -2),  m = "tau must be positive"),
    list(a = list(mu = -Inf, sigma = 1, nu = 1, tau = 2), m = "mu must be positive"),
    list(a = list(mu = Inf, sigma = 1, nu = 1, tau = 2), m = "mu must be finite"),
    list(a = list(mu = 1, sigma = Inf, nu = 1, tau = 2), m = "sigma must be finite"),
    list(a = list(mu = 1, sigma = 1, nu = Inf, tau = 2), m = "nu must be finite"),
    list(a = list(mu = 1, sigma = 1, nu = 1, tau = Inf), m = "tau must be finite"))
  for (b in bcpe_msgs) {
    expect_error(do.call(fdBCPE, c(list(1), b$a)), b$m, info = paste("fdBCPE:", b$m))
    expect_error(do.call(fpBCPE, c(list(1), b$a, list(lower_tail = FALSE, log_p = TRUE))), b$m,
                 info = paste("fpBCPE:", b$m))
    expect_error(do.call(fqBCPE, c(list(0.5), b$a)), b$m, info = paste("fqBCPE:", b$m))
  }
  expect_error(fqBCPE(-0.1, 1, 1, 1, 2), "p must be between 0 and 1")
  expect_error(fqBCPE(1.1, 1, 1, 1, 2), "p must be between 0 and 1")
  expect_error(fqBCPE(0.1, 1, 1, 1, 2, log_p = TRUE), "log_p")
  # the bad element is found after recycling, wherever it is
  expect_error(fpBCPE(c(1, 2, 3), 1, c(1, 1, 0), 1, 2), "sigma must be positive")
  # a NA row is not checked: NA, silently, even next to an otherwise invalid value
  expect_identical(fdBCPE(c(1, NA), c(1, -1), 1, 1, 2), c(fdBCPE(1, 1, 1, 1, 2), NA))
  expect_silent(fqBCPE(0.5, c(1, NA), 1, c(1, Inf), 2))
} else {
  expect_true(FALSE, info = "fdBCPE is not in this build")
}

# --------------------------------------------------------------------------
# 2. NOTE: the compiled frNBI / frZINBI / frZANBI wrappers that used to be
#    exercised here through .Call("_CKutils_fr...") no longer exist. They were
#    dead code: R/rng_distr.R defines the same three names and, because R/ is
#    collated alphabetically, always overwrote them, so the .Call entry points
#    were the only way to reach them at all. The exported frNBI / frZINBI /
#    frZANBI are the R implementations, covered in test-rng_distr.R.
#
#    The frNBI_scalar / frZINBI_scalar / frZANBI_scalar inline kernels remain
#    in inst/include/ as part of the LinkingTo: CKutils surface, but nothing in
#    this package calls them any more, so they are no longer reachable from R
#    and cannot be covered from here.
# --------------------------------------------------------------------------


# =============================================================================
# check_prob: log_p = TRUE validates p on the LOG scale
#
# These wrappers used to range-check p against [0, 1] before applying the log_p
# back-transform, so every legitimate log(p) < 0 was rejected and log_p was
# effectively unusable. A quantile call on the log scale must now agree with the
# same call on the natural scale.
# =============================================================================
p_nat <- c(0.1, 0.25, 0.5, 0.75, 0.9)
p_log <- log(p_nat)
log_cases <- list(
  list(f = fqNBI,      args = list(2, 1)),
  list(f = fqBNB,      args = list(2, 1, 1)),
  list(f = fqZABNB,    args = list(2, 1, 1, 0.1)),
  list(f = fqZIBNB,    args = list(2, 1, 1, 0.1)),
  list(f = fqZANBI,    args = list(2, 1, 0.1)),
  list(f = fqZINBI,    args = list(2, 1, 0.1)),
  list(f = fqSICHEL,   args = list(1, 1, -0.5)),
  list(f = fqDPO,      args = list(2, 1)),
  list(f = fqDEL,      args = list(2, 1, 0.5)),
  list(f = fqMN4,      args = list(1, 1, 1)),
  list(f = fqBCT,      args = list(2, 1, 1, 5)),
  list(f = fqBCPEo,    args = list(2, 1, 1, 2)),
  list(f = fqZISICHEL, args = list(1, 1, -0.5, 0.1))
)
for (k in seq_along(log_cases)) {
  cs <- log_cases[[k]]
  expect_equal(
    as.numeric(do.call(cs$f, c(list(p_log), cs$args, list(log_p = TRUE)))),
    as.numeric(do.call(cs$f, c(list(p_nat), cs$args))),
    tolerance = 1e-8,
    info = paste0("quantile case ", k, ": log_p = TRUE matches the natural scale")
  )
}

# log(0) = -Inf is an admissible log-scale probability
expect_equal(fqBNB(-Inf, 2, 1, 1, log_p = TRUE), fqBNB(0, 2, 1, 1),
             info = "fqBNB accepts -Inf as log(0)")
expect_equal(fqZABNB(-Inf, 2, 1, 1, 0.1, log_p = TRUE), fqZABNB(0, 2, 1, 1, 0.1),
             info = "fqZABNB accepts -Inf as log(0)")

# A positive log-scale p means p > 1 and must still be rejected
expect_error(fqNBI(0.5, 2, 1, log_p = TRUE), "log_p",
             info = "fqNBI rejects a positive log-scale p")
expect_error(fqBNB(1.0, 2, 1, 1, log_p = TRUE), "log_p",
             info = "fqBNB rejects a positive log-scale p")
expect_error(fqZABNB(0.3, 2, 1, 1, 0.1, log_p = TRUE), "log_p",
             info = "fqZABNB rejects a positive log-scale p")
expect_error(fqSICHEL(2, 1, 1, -0.5, log_p = TRUE), "log_p",
             info = "fqSICHEL rejects a positive log-scale p")

# Natural-scale range checks are unchanged
expect_error(fqBNB(1.5, 2, 1, 1), "p must be >=0 and <=1",
             info = "fqBNB still rejects a natural-scale p > 1")
expect_error(fqZANBI(-0.1, 2, 1, 0.1), "p must be >=0 and <=1",
             info = "fqZANBI still rejects a natural-scale p < 0")


## ---- ZIBNB density/CDF and ZISICHEL density (added 0.1.30) ----
expect_error(fdZIBNB(0, mu = -1, sigma = 1, nu = 1, tau = 0.1, TRUE), "mu must be greater than 0")
expect_error(fdZIBNB(0, mu = 1, sigma = 0, nu = 1, tau = 0.1, TRUE), "sigma must be greater than 0")
expect_error(fdZIBNB(0, mu = 1, sigma = 1, nu = 0, tau = 0.1, TRUE), "nu must be greater than 0")
expect_error(fdZIBNB(0, mu = 1, sigma = 1, nu = 1, tau = 1.5, TRUE), "tau must be >0 and <1")
expect_error(fpZIBNB(0, mu = 1, sigma = 1, nu = 1, tau = 0, log_p = TRUE), "tau must be >0 and <1")
expect_error(fpZIBNB(0, mu = -1, sigma = 1, nu = 1, tau = 0.1, lower_tail = FALSE), "mu must be greater than 0")
expect_error(fdZISICHEL(0, mu = -1, sigma = 1, nu = -0.5, tau = 0.1), "mu must be greater than 0")
expect_error(fdZISICHEL(0, mu = 1, sigma = 0, nu = -0.5, tau = 0.1), "sigma must be greater than 0")
expect_error(fdZISICHEL(0, mu = 1, sigma = 1, nu = -0.5, tau = 0), "tau must be between 0 and 1")
