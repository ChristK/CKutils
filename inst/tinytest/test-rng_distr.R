# Test suite for the random-generation wrappers in R/rng_distr.R
# These functions (frXXX) draw high-quality uniforms via dqrng::dqrunif and
# apply the corresponding inverse-CDF (fqXXX) transformation.

# -----------------------------------------------------------------------------
# Continuous distributions: frBCPE, frBCPEo, frBCT, frZAGA
# -----------------------------------------------------------------------------
# A function this build lacks is replaced by one that returns NA, so its checks fail
# rather than abort the file.
ck_fun <- function(nm) get0(nm, envir = asNamespace("CKutils"), inherits = FALSE,
                            ifnotfound = function(...) NA_real_)
frBCPE_ <- ck_fun("frBCPE")
# Each entry: function plus a list of valid parameters to exercise the body
# (frZAGA has no tau, and needs 0 < nu < 1).
cont_calls <- list(
  list(nm = "frBCPE",  fun = frBCPE_, args = list(mu = 5, sigma = 0.1, nu = 1, tau = 2)),
  list(nm = "frBCPEo", fun = frBCPEo, args = list(mu = 5, sigma = 0.1, nu = 1, tau = 2)),
  list(nm = "frBCT",   fun = frBCT,   args = list(mu = 5, sigma = 0.1, nu = 1, tau = 2)),
  list(nm = "frZAGA",  fun = frZAGA,  args = list(mu = 5, sigma = 0.5, nu = 0.2))
)

for (cc in cont_calls) {
  nm <- cc$nm
  f <- function(n) do.call(cc$fun, c(list(n = n), cc$args))
  set.seed(42)
  x <- f(100)
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

# frZAGA is a double vector on [0, Inf) with exact zeros
dqrng::dqset.seed(7)
x <- frZAGA(2000, mu = 5, sigma = 0.5, nu = 0.2)
expect_true(is.double(x), info = "frZAGA returns doubles, not integers")
expect_true(all(x >= 0), info = "frZAGA is non-negative")
expect_true(any(x == 0) && any(x > 0), info = "frZAGA has exact zeros and positive values")

# frBCPE is a double vector on (0, Inf), and frBCPEo gives the same draws
dqrng::dqset.seed(7)
x <- frBCPE_(2000, mu = 25, sigma = 0.15, nu = c(-1, 0, 0.5), tau = c(1.5, 2, 4))
expect_true(is.double(x) && length(x) == 2000L, info = "frBCPE returns doubles")
expect_true(all(x > 0 & is.finite(x)), info = "frBCPE is positive and finite")
dqrng::dqset.seed(7)
expect_identical(frBCPEo(2000, mu = 25, sigma = 0.15, nu = c(-1, 0, 0.5), tau = c(1.5, 2, 4)), x,
                 info = "frBCPEo gives the same draws as frBCPE under one seed")

# parameter recycling for the continuous functions
set.seed(1)
expect_equal(length(frBCPE_(10, mu = c(1, 2), sigma = 0.3, nu = c(-0.5, 0.5), tau = 4)),
             10L, info = "frBCPE recycles parameters")
set.seed(1)
expect_equal(length(frBCPEo(10, mu = c(1, 2), sigma = 0.3, nu = c(-0.5, 0.5), tau = 4)),
             10L, info = "frBCPEo recycles parameters")
set.seed(1)
expect_equal(length(frBCT(10, mu = c(1, 2), sigma = 0.3, nu = c(-0.5, 0.5), tau = 4)),
             10L, info = "frBCT recycles parameters")
set.seed(1)
expect_equal(length(frZAGA(10, mu = c(1, 2), sigma = 0.3, nu = c(0.1, 0.5))),
             10L, info = "frZAGA recycles parameters")

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

dqrng::dqset.seed(99)
a <- frZAGA(50, mu = 3, sigma = 0.5, nu = 0.2)
dqrng::dqset.seed(99)
b <- frZAGA(50, mu = 3, sigma = 0.5, nu = 0.2)
expect_identical(a, b, info = "frZAGA is reproducible under a fixed dqrng seed")

dqrng::dqset.seed(99)
a <- frBCPE_(50, mu = 25, sigma = 0.15, nu = 0.5, tau = 2)
dqrng::dqset.seed(99)
b <- frBCPE_(50, mu = 25, sigma = 0.15, nu = 0.5, tau = 2)
expect_true(identical(a, b) && length(a) == 50L,
            info = "frBCPE is reproducible under a fixed dqrng seed")


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
# frZAGA_scalar and frBCPE_scalar (continuous, so their draws are doubles) were
# written to the same contract from the start.
# =============================================================================
if (requireNamespace("dqrng", quietly = TRUE)) {
  n_par <- 1000L
  frBCPE_bridge <- ck_fun(".frBCPE_scalar_vec")
  parity_cases <- list(
    # BCPE is continuous: doubles, compared bit for bit; the gamlss path, the stable
    # path (nu near 0) and the truncation regime (nu = 3, sigma = 1)
    list(nm = "frBCPE",  R = frBCPE_, p = list(mu = 25, sigma = 0.15, nu = 0.5, tau = 2),
         C = function(u, p) frBCPE_bridge(u, p$mu, p$sigma, p$nu, p$tau)),
    list(nm = "frBCPE",  R = frBCPE_, p = list(mu = 2, sigma = 0.3, nu = 1e-9, tau = 0.7),
         C = function(u, p) frBCPE_bridge(u, p$mu, p$sigma, p$nu, p$tau)),
    list(nm = "frBCPE",  R = frBCPE_, p = list(mu = 1, sigma = 1, nu = 3, tau = 5),
         C = function(u, p) frBCPE_bridge(u, p$mu, p$sigma, p$nu, p$tau)),
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
         C = function(u, p) CKutils:::.frZINBI_scalar_vec(u, p$mu, p$sigma, p$nu)),
    # ZAGA is continuous: the draws are doubles, compared bit for bit
    list(nm = "frZAGA",  R = frZAGA,  p = list(mu = 2, sigma = 1, nu = 0.1),
         C = function(u, p) CKutils:::.frZAGA_scalar_vec(u, p$mu, p$sigma, p$nu)),
    list(nm = "frZAGA",  R = frZAGA,  p = list(mu = 1e-3, sigma = 0.05, nu = 0.5),
         C = function(u, p) CKutils:::.frZAGA_scalar_vec(u, p$mu, p$sigma, p$nu))
  )
  for (k in seq_along(parity_cases)) {
    cs <- parity_cases[[k]]
    lbl <- paste0(cs$nm, " case ", k)
    dqrng::dqset.seed(4242)
    from_R <- do.call(cs$R, c(list(n_par), cs$p))
    dqrng::dqset.seed(4242)
    from_C <- cs$C(dqrng::dqrunif(n_par), cs$p)
    expect_identical(
      as.numeric(from_C), as.numeric(from_R),
      info = paste(lbl, "- scalar kernel matches the exported R sampler under one seed")
    )
  }

  # (the BCPE cases above compare like with like only where the bridge exists)
  u3 <- c(0.1, 0.5, 0.9)
  expect_identical(frBCPE_bridge(u3, 25, 0.15, 0.5, 2), fqBCPEo(u3, 25, 0.15, 0.5, 2),
                   info = ".frBCPE_scalar_vec is fqBCPE at the given uniforms")

  # The old frZANBI_scalar rejection loop spun as mu -> 0 (nearly every draw
  # from the untruncated NBI is a zero); the inverse-CDF form cannot.
  el <- system.time(
    z <- CKutils:::.frZANBI_scalar_vec(dqrng::dqrunif(5000), 1e-8, 1, 0.1)
  )[["elapsed"]]
  expect_true(el < 10 && all(is.finite(z)),
              info = "frZANBI_scalar returns promptly at mu = 1e-8 (no rejection loop)")
}
