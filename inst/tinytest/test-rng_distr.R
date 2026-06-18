# Test suite for the random-generation wrappers in R/rng_distr.R
# These functions (frXXX) draw high-quality uniforms via dqrng::dqrunif and
# apply the corresponding inverse-CDF (fqXXX) transformation.

# -----------------------------------------------------------------------------
# Continuous distributions: frBCPEo, frBCT
# -----------------------------------------------------------------------------
cont_funs <- list(
  frBCPEo = function(n, ...) frBCPEo(n, ...),
  frBCT   = function(n, ...) frBCT(n, ...)
)

for (nm in names(cont_funs)) {
  f <- cont_funs[[nm]]
  set.seed(42)
  x <- f(100, mu = 5, sigma = 0.1, nu = 1, tau = 2)
  expect_equal(length(x), 100L, info = paste(nm, "returns requested length"))
  expect_true(all(is.finite(x)), info = paste(nm, "produces finite values"))
  expect_true(is.numeric(x), info = paste(nm, "returns numeric"))
  # non-integer support for continuous distributions
  expect_true(any(x != round(x)), info = paste(nm, "is continuous"))
  # n is rounded up with ceiling()
  expect_equal(length(f(9.2)), 10L, info = paste(nm, "ceilings non-integer n"))
  # error on non-positive n
  expect_error(f(0), info = paste(nm, "errors on n = 0"))
  expect_error(f(-3), info = paste(nm, "errors on negative n"))
}

# parameter recycling for the continuous functions
set.seed(1)
expect_equal(length(frBCPEo(10, mu = c(1, 2), sigma = 0.3, nu = c(-0.5, 0.5), tau = 4)),
             10L, info = "frBCPEo recycles parameters")
set.seed(1)
expect_equal(length(frBCT(10, mu = c(1, 2), sigma = 0.3, nu = c(-0.5, 0.5), tau = 4)),
             10L, info = "frBCT recycles parameters")

# -----------------------------------------------------------------------------
# Count distributions: frBNB, frZIBNB, frZABNB, frDPO, frDEL,
#                       frNBI, frZINBI, frZANBI, frSICHEL, frZISICHEL
# -----------------------------------------------------------------------------
# Each entry: function plus a list of valid parameters to exercise the body.
count_calls <- list(
  list(nm = "frBNB",      fun = frBNB,      args = list(mu = 1, sigma = 1, nu = 1)),
  list(nm = "frZIBNB",    fun = frZIBNB,    args = list(mu = 1, sigma = 1, nu = 1, tau = 0.1)),
  list(nm = "frZABNB",    fun = frZABNB,    args = list(mu = 1, sigma = 1, nu = 1, tau = 0.1)),
  list(nm = "frDPO",      fun = frDPO,      args = list(mu = 1, sigma = 1)),
  list(nm = "frDEL",      fun = frDEL,      args = list(mu = 1, sigma = 1, nu = 0.5)),
  list(nm = "frNBI",      fun = frNBI,      args = list(mu = 2, sigma = 1)),
  list(nm = "frZINBI",    fun = frZINBI,    args = list(mu = 2, sigma = 1, nu = 0.1)),
  list(nm = "frZANBI",    fun = frZANBI,    args = list(mu = 2, sigma = 1, nu = 0.1)),
  list(nm = "frSICHEL",   fun = frSICHEL,   args = list(mu = 2, sigma = 1, nu = -0.5)),
  list(nm = "frZISICHEL", fun = frZISICHEL, args = list(mu = 2, sigma = 1, nu = -0.5, tau = 0.1))
)

for (cc in count_calls) {
  set.seed(123)
  x <- suppressWarnings(do.call(cc$fun, c(list(n = 200), cc$args)))
  expect_equal(length(x), 200L, info = paste(cc$nm, "returns requested length"))
  expect_true(all(is.finite(x)), info = paste(cc$nm, "produces finite values"))
  # count distributions return non-negative integers
  expect_true(all(x >= 0), info = paste(cc$nm, "is non-negative"))
  expect_true(all(x == round(x)), info = paste(cc$nm, "returns integer counts"))
  # ceiling of non-integer n
  expect_equal(length(suppressWarnings(do.call(cc$fun, c(list(n = 4.1), cc$args)))),
               5L, info = paste(cc$nm, "ceilings non-integer n"))
  # error on non-positive n (covers the any(n <= 0) stop branch)
  expect_error(do.call(cc$fun, c(list(n = 0), cc$args)),
               info = paste(cc$nm, "errors on n = 0"))
  expect_error(do.call(cc$fun, c(list(n = -2), cc$args)),
               info = paste(cc$nm, "errors on negative n"))
}

# -----------------------------------------------------------------------------
# Reproducibility: setting the dqrng seed yields identical draws
# -----------------------------------------------------------------------------
dqrng::dqset.seed(99)
a <- frDPO(50, mu = 3, sigma = 1)
dqrng::dqset.seed(99)
b <- frDPO(50, mu = 3, sigma = 1)
expect_equal(a, b, info = "frDPO is reproducible under a fixed dqrng seed")
