# Tests targeting otherwise-uncovered C++ branches in src/distr_<NAME>.cpp:
#   1. parameter-validation `stop()` branches in fd/fp/fq, exercised with
#      log_p = TRUE (and lower_tail = FALSE where relevant), and
#   2. the compiled fr<NAME> random generators, which at the R level are
#      shadowed by pure-R versions in R/rng_distr.R and are therefore only
#      reachable via .Call().
# These do NOT depend on gamlss.dist.

library(CKutils)
library(tinytest)

# --------------------------------------------------------------------------
# 1. Validation / log_p / lower_tail error branches in fq / fp / fd
# --------------------------------------------------------------------------

## ---- NBI (mu, sigma) ----
# fqNBI has a SEPARATE validation loop guarded by `if (log_p)`.
expect_error(fqNBI(0.5, mu = -1, sigma = 1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqNBI(0.5, mu =  1, sigma = 0, log_p = TRUE), "sigma must be greater than 0")
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
expect_error(fpNBI(-1, mu = 1, sigma = 1), "q must be >=0")

## ---- ZINBI (mu, sigma, nu in (0,1)) ----
expect_error(fdZINBI(0, mu = -1, sigma = 1, nu = 0.1, TRUE), "mu must be greater than 0")
expect_error(fdZINBI(0, mu = 1, sigma = 0, nu = 0.1, TRUE), "sigma must be greater than 0")
expect_error(fdZINBI(0, mu = 1, sigma = 1, nu = 1.5, TRUE), "nu must be between 0 and 1")
expect_error(fdZINBI(-1, mu = 1, sigma = 1, nu = 0.1), "x must be >=0")
expect_error(fpZINBI(0, mu = -1, sigma = 1, nu = 0.1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpZINBI(0, mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpZINBI(0, mu = 1, sigma = 1, nu = 0, log_p = TRUE), "nu must be between 0 and 1")
expect_error(fpZINBI(-1, mu = 1, sigma = 1, nu = 0.1), "q must be >=0")
expect_error(fqZINBI(0.5, mu = -1, sigma = 1, nu = 0.1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqZINBI(0.5, mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqZINBI(0.5, mu = 1, sigma = 1, nu = 2, log_p = TRUE), "nu must be between 0 and 1")
expect_error(fqZINBI(-0.1, mu = 1, sigma = 1, nu = 0.1), "p must be >=0 and <=1")

## ---- ZANBI (mu, sigma, nu in (0,1)) ----
expect_error(fdZANBI(0, mu = -1, sigma = 1, nu = 0.1, TRUE), "mu must be greater than 0")
expect_error(fdZANBI(0, mu = 1, sigma = 0, nu = 0.1, TRUE), "sigma must be greater than 0")
expect_error(fdZANBI(0, mu = 1, sigma = 1, nu = 1.5, TRUE), "nu must be between 0 and 1")
expect_error(fdZANBI(-1, mu = 1, sigma = 1, nu = 0.1), "x must be >=0")
expect_error(fpZANBI(0, mu = -1, sigma = 1, nu = 0.1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpZANBI(0, mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpZANBI(0, mu = 1, sigma = 1, nu = 0, log_p = TRUE), "nu must be between 0 and 1")
expect_error(fpZANBI(-1, mu = 1, sigma = 1, nu = 0.1), "q must be >=0")
expect_error(fqZANBI(0.5, mu = -1, sigma = 1, nu = 0.1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqZANBI(0.5, mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqZANBI(0.5, mu = 1, sigma = 1, nu = 2, log_p = TRUE), "nu must be between 0 and 1")
expect_error(fqZANBI(-0.1, mu = 1, sigma = 1, nu = 0.1), "p must be >=0 and <=1")

## ---- BNB (mu, sigma, nu > 0) ----
expect_error(fdBNB(0, mu = -1, sigma = 1, nu = 1, TRUE), "mu must be greater than 0")
expect_error(fdBNB(0, mu = 1, sigma = 0, nu = 1, TRUE), "sigma must be greater than 0")
expect_error(fdBNB(0, mu = 1, sigma = 1, nu = 0, TRUE), "nu must be greater than 0")
expect_error(fdBNB(-1, mu = 1, sigma = 1, nu = 1), "x must be >=0")
expect_error(fpBNB(0, mu = -1, sigma = 1, nu = 1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpBNB(0, mu = 1, sigma = 0, nu = 1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpBNB(0, mu = 1, sigma = 1, nu = 0, log_p = TRUE), "nu must be greater than 0")
expect_error(fpBNB(-1, mu = 1, sigma = 1, nu = 1), "q must be >=0")
expect_error(fqBNB(0.5, mu = -1, sigma = 1, nu = 1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqBNB(0.5, mu = 1, sigma = 0, nu = 1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqBNB(0.5, mu = 1, sigma = 1, nu = 0, log_p = TRUE), "nu must be greater than 0")
expect_error(fqBNB(-0.1, mu = 1, sigma = 1, nu = 1), "p must be >=0 and <=1")

## ---- SICHEL (mu, sigma, nu); fd/fp/fq validate mu & sigma ----
expect_error(fdSICHEL(0, mu = -1, sigma = 1, nu = -0.5, log_p = TRUE), "mu must be greater than 0")
expect_error(fdSICHEL(0, mu = 1, sigma = 0, nu = -0.5, log_p = TRUE), "sigma must be greater than 0")
expect_error(fdSICHEL(-1, mu = 1, sigma = 1, nu = -0.5), "x must be >=0")
expect_error(fpSICHEL(0, mu = -1, sigma = 1, nu = -0.5, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpSICHEL(0, mu = 1, sigma = 0, nu = -0.5, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpSICHEL(-1, mu = 1, sigma = 1, nu = -0.5), "q must be >=0")
# fqSICHEL has a SEPARATE `if (log_p)` validation loop.
expect_error(fqSICHEL(0.5, mu = -1, sigma = 1, nu = -0.5, log_p = TRUE), "mu must be greater than 0")
expect_error(fqSICHEL(0.5, mu = 1, sigma = 0, nu = -0.5, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqSICHEL(0.5, mu = -1, sigma = 1, nu = -0.5), "mu must be greater than 0")
expect_error(fqSICHEL(0.5, mu = 1, sigma = 0, nu = -0.5), "sigma must be greater than 0")

## ---- ZISICHEL (mu, sigma, nu, tau in (0,1)); fp/fq validate mu, sigma, tau ----
expect_error(fpZISICHEL(0, mu = -1, sigma = 1, nu = -0.5, tau = 0.1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpZISICHEL(0, mu = 1, sigma = 0, nu = -0.5, tau = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpZISICHEL(0, mu = 1, sigma = 1, nu = -0.5, tau = 2, log_p = TRUE), "tau must be between 0 and 1")
expect_error(fpZISICHEL(-1, mu = 1, sigma = 1, nu = -0.5, tau = 0.1), "q must be >=0")
# fqZISICHEL has a SEPARATE `if (log_p)` validation loop.
expect_error(fqZISICHEL(0.5, mu = -1, sigma = 1, nu = -0.5, tau = 0.1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqZISICHEL(0.5, mu = 1, sigma = 0, nu = -0.5, tau = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqZISICHEL(0.5, mu = 1, sigma = 1, nu = -0.5, tau = 2, log_p = TRUE), "tau must be between 0 and 1")
expect_error(fqZISICHEL(0.5, mu = -1, sigma = 1, nu = -0.5, tau = 0.1), "mu must be greater than 0")
expect_error(fqZISICHEL(0.5, mu = 1, sigma = 0, nu = -0.5, tau = 0.1), "sigma must be greater than 0")
expect_error(fqZISICHEL(0.5, mu = 1, sigma = 1, nu = -0.5, tau = 2), "tau must be between 0 and 1")

## ---- DEL (mu, sigma, nu in (0,1)) ----
# fdDEL density-log arg is positional (log_); pass positionally as 5th arg.
expect_error(fdDEL(0L, mu = -1, sigma = 1, nu = 0.1, TRUE), "mu must be greater than 0")
expect_error(fdDEL(0L, mu = 1, sigma = 0, nu = 0.1, TRUE), "sigma must be greater than 0")
expect_error(fdDEL(0L, mu = 1, sigma = 1, nu = 1.5, TRUE), "nu must be between 0 and 1")
expect_error(fdDEL(-1L, mu = 1, sigma = 1, nu = 0.1), "x must be >=0")
expect_error(fpDEL(0L, mu = -1, sigma = 1, nu = 0.1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpDEL(0L, mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpDEL(0L, mu = 1, sigma = 1, nu = 0, log_p = TRUE), "nu must be between 0 and 1")
expect_error(fpDEL(-1L, mu = 1, sigma = 1, nu = 0.1), "q must be >=0")
expect_error(fqDEL(0.5, mu = -1, sigma = 1, nu = 0.1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqDEL(0.5, mu = 1, sigma = 0, nu = 0.1, log_p = TRUE), "sigma must be greater than 0")
expect_error(fqDEL(0.5, mu = 1, sigma = 1, nu = 2, log_p = TRUE), "nu must be between 0 and 1")
# fqDEL p-range validation after exp(log_p) transform
expect_error(fqDEL(2, mu = 1, sigma = 1, nu = 0.1), "p must be between 0 and 1")

## ---- DPO (mu, sigma) ----
expect_error(fdDPO(0L, mu = -1, sigma = 1, TRUE), "mu must be greater than 0")
expect_error(fdDPO(0L, mu = 1, sigma = 0, TRUE), "sigma must be greater than 0")
expect_error(fdDPO(-1L, mu = 1, sigma = 1), "x must be >=0")
expect_error(fpDPO(0L, mu = -1, sigma = 1, lower_tail = FALSE, log_p = TRUE), "mu must be greater than 0")
expect_error(fpDPO(0L, mu = 1, sigma = 0, log_p = TRUE), "sigma must be greater than 0")
expect_error(fpDPO(-1L, mu = 1, sigma = 1), "q must be >=0")
expect_error(fqDPO(0.5, mu = -1, sigma = 1, log_p = TRUE), "mu must be greater than 0")
expect_error(fqDPO(0.5, mu = 1, sigma = 0, log_p = TRUE), "sigma must be greater than 0")
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
expect_error(fdBCPEo(-1, mu = 1, sigma = 1, nu = 1, tau = 2), "x must be >=0")
expect_error(fpBCPEo(1, mu = -1, sigma = 1, nu = 1, tau = 2, lower_tail = FALSE, log_p = TRUE), "mu must be positive")
expect_error(fpBCPEo(1, mu = 1, sigma = 0, nu = 1, tau = 2, log_p = TRUE), "sigma must be positive")
expect_error(fpBCPEo(1, mu = 1, sigma = 1, nu = 1, tau = 0, log_p = TRUE), "tau must be positive")
expect_error(fpBCPEo(-1, mu = 1, sigma = 1, nu = 1, tau = 2), "q must be positive")
expect_error(fqBCPEo(0.5, mu = -1, sigma = 1, nu = 1, tau = 2, log_p = TRUE), "mu must be positive")
expect_error(fqBCPEo(0.5, mu = 1, sigma = 0, nu = 1, tau = 2, log_p = TRUE), "sigma must be positive")
expect_error(fqBCPEo(0.5, mu = 1, sigma = 1, nu = 1, tau = 0, log_p = TRUE), "tau must be positive")
expect_error(fqBCPEo(2, mu = 1, sigma = 1, nu = 1, tau = 2), "p must be between 0 and 1")

# --------------------------------------------------------------------------
# 2. Compiled fr<NAME> random generators (reached only via .Call, since the
#    R-level names are shadowed by pure-R versions in R/rng_distr.R).
# --------------------------------------------------------------------------

## ---- frNBI(n, mu, sigma) ----
expect_error(.Call("_CKutils_frNBI", 0L, 2.0, 1.0), "n must be a positive integer")
expect_error(.Call("_CKutils_frNBI", 5L, -1.0, 1.0), "mu must be greater than 0")
expect_error(.Call("_CKutils_frNBI", 5L, 2.0, 0.0), "sigma must be greater than 0")
set.seed(42)
# Normal sigma -> negative-binomial branch of frNBI_scalar
x_nbi <- .Call("_CKutils_frNBI", 100L, 2.0, 1.0)
expect_equal(length(x_nbi), 100L)
expect_true(all(is.finite(x_nbi)))
expect_true(all(x_nbi >= 0))
expect_true(is.integer(x_nbi))
# Tiny sigma (< 1e-4) -> Poisson-approximation branch of frNBI_scalar
set.seed(42)
x_nbi_pois <- .Call("_CKutils_frNBI", 100L, 2.0, 1e-6)
expect_equal(length(x_nbi_pois), 100L)
expect_true(all(is.finite(x_nbi_pois)))
expect_true(all(x_nbi_pois >= 0))

## ---- frZINBI(n, mu, sigma, nu) ----
expect_error(.Call("_CKutils_frZINBI", 0L, 2.0, 1.0, 0.1), "n must be a positive integer")
expect_error(.Call("_CKutils_frZINBI", 5L, -1.0, 1.0, 0.1), "mu must be greater than 0")
expect_error(.Call("_CKutils_frZINBI", 5L, 2.0, 0.0, 0.1), "sigma must be greater than 0")
expect_error(.Call("_CKutils_frZINBI", 5L, 2.0, 1.0, 1.5), "nu must be between 0 and 1")
set.seed(7)
# Large nu so the u < nu (zero-inflation) branch of frZINBI_scalar is hit,
# and the else (frNBI_scalar) branch is also exercised.
x_zinbi <- .Call("_CKutils_frZINBI", 200L, 2.0, 1.0, 0.5)
expect_equal(length(x_zinbi), 200L)
expect_true(all(is.finite(x_zinbi)))
expect_true(all(x_zinbi >= 0))
expect_true(is.integer(x_zinbi))
expect_true(any(x_zinbi == 0))   # zero-inflation branch reached
expect_true(any(x_zinbi > 0))    # NBI branch reached

## ---- frZANBI(n, mu, sigma, nu) ----
expect_error(.Call("_CKutils_frZANBI", 0L, 2.0, 1.0, 0.1), "n must be a positive integer")
expect_error(.Call("_CKutils_frZANBI", 5L, -1.0, 1.0, 0.1), "mu must be greater than 0")
expect_error(.Call("_CKutils_frZANBI", 5L, 2.0, 0.0, 0.1), "sigma must be greater than 0")
expect_error(.Call("_CKutils_frZANBI", 5L, 2.0, 1.0, 0.0), "nu must be between 0 and 1")
set.seed(99)
# nu = 0.5 hits both the zero-altered (u < nu) branch and the truncated-NBI
# rejection branch (do { x = frNBI_scalar } while (x == 0)).
x_zanbi <- .Call("_CKutils_frZANBI", 200L, 2.0, 1.0, 0.5)
expect_equal(length(x_zanbi), 200L)
expect_true(all(is.finite(x_zanbi)))
expect_true(all(x_zanbi >= 0))
expect_true(is.integer(x_zanbi))
expect_true(any(x_zanbi == 0))   # zero-altered branch reached
expect_true(any(x_zanbi > 0))    # truncated-NBI branch reached
