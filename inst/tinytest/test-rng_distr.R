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


# =============================================================================
# Header-only RNG scalars must agree with the exported R samplers
#
# frNBI_scalar / frZANBI_scalar / frZINBI_scalar are inline kernels in
# inst/include/ for LinkingTo: CKutils consumers. Before 0.1.30 they drew from
# R's own stream by rejection/direct sampling, so a C++ consumer got a DIFFERENT
# sample from the exported R frNBI()/frZANBI()/frZINBI() even under a fixed
# seed. They now invert the same CDF from a caller-supplied uniform, which is
# exactly what the R functions do with dqrng::dqrunif(), so the two must agree
# draw for draw. Reached through the internal .fr*_scalar_vec bridges.
# =============================================================================
if (requireNamespace("dqrng", quietly = TRUE)) {
  n_par <- 1000L
  parity_cases <- list(
    list(nm = "frNBI",   R = frNBI,   p = list(mu = 2, sigma = 1),
         C = function(u, p) CKutils:::.frNBI_scalar_vec(u, p$mu, p$sigma)),
    list(nm = "frNBI",   R = frNBI,   p = list(mu = 5, sigma = 1e-6),
         C = function(u, p) CKutils:::.frNBI_scalar_vec(u, p$mu, p$sigma)),
    list(nm = "frZANBI", R = frZANBI, p = list(mu = 2, sigma = 1, nu = 0.1),
         C = function(u, p) CKutils:::.frZANBI_scalar_vec(u, p$mu, p$sigma, p$nu)),
    list(nm = "frZANBI", R = frZANBI, p = list(mu = 0.3, sigma = 2, nu = 0.5),
         C = function(u, p) CKutils:::.frZANBI_scalar_vec(u, p$mu, p$sigma, p$nu)),
    list(nm = "frZINBI", R = frZINBI, p = list(mu = 2, sigma = 1, nu = 0.1),
         C = function(u, p) CKutils:::.frZINBI_scalar_vec(u, p$mu, p$sigma, p$nu)),
    list(nm = "frZINBI", R = frZINBI, p = list(mu = 0.3, sigma = 2, nu = 0.5),
         C = function(u, p) CKutils:::.frZINBI_scalar_vec(u, p$mu, p$sigma, p$nu))
  )
  for (k in seq_along(parity_cases)) {
    cs <- parity_cases[[k]]
    lbl <- paste0(cs$nm, " case ", k)
    dqrng::dqset.seed(4242)
    from_R <- do.call(cs$R, c(list(n_par), cs$p))
    dqrng::dqset.seed(4242)
    from_C <- cs$C(dqrng::dqrunif(n_par), cs$p)
    expect_identical(
      as.integer(from_C), as.integer(from_R),
      info = paste(lbl, "- scalar kernel matches the exported R sampler under one seed")
    )
  }

  # The old frZANBI_scalar rejection loop spun as mu -> 0 (nearly every draw
  # from the untruncated NBI is a zero); the inverse-CDF form cannot.
  el <- system.time(
    z <- CKutils:::.frZANBI_scalar_vec(dqrng::dqrunif(5000), 1e-8, 1, 0.1)
  )[["elapsed"]]
  expect_true(el < 10 && all(is.finite(z)),
              info = "frZANBI_scalar returns promptly at mu = 1e-8 (no rejection loop)")
}
