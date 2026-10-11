# Test the ZAGA (zero-adjusted gamma) distribution functions:
# fdZAGA, fpZAGA, fqZAGA and frZAGA. Using the tinytest framework.
#
# ZAGA is the zero-adjusted gamma of gamlss.dist: P(Y = 0) = nu, and given Y > 0,
# Y ~ GA(mu, sigma), a gamma with shape a = 1 / sigma^2 and scale s = mu * sigma^2
# (mean mu, coefficient of variation sigma). E[Y] = (1 - nu) * mu and
# Var[Y] = (1 - nu) * mu^2 * (sigma^2 + nu).
#
# The references are closed forms built from stats::dgamma / pgamma / qgamma, and
# exact exponential (sigma = 1) and Erlang (sigma = 0.5) formulae, not the package's
# own code. Where gamlss.dist is right the file also compares against it.
#
# Departures from gamlss.dist (all deliberate, all asserted below). Every expectation
# that gamlss.dist cannot meet carries a tag in its info string: dep("...") for the
# departures, conv("...") for other CKutils conventions that the SPEC fixes, and
# num("...") for numerical accuracy that gamlss.dist's formulae do not reach.
#   dep("strict ranges")          mu > 0, sigma > 0, 0 < nu < 1, and mu and sigma finite
#                                 (gamlss.dist d/p/q accept nu in [0, 1], d/p accept
#                                 mu = 0 and sigma = 0, and none checks for Inf)
#   dep("q < 0 tails")            q < 0 with lower_tail = FALSE or log_p = TRUE is
#                                 1 / -Inf (gamlss.dist: 0 for both)
#   dep("log density x < 0")      the log density at x < 0 is -Inf (gamlss.dist: 0)
#   dep("density at Inf")         the density at x = Inf is 0 (gamlss.dist: NaN)
#   dep("direct upper tail")      the upper tail is computed directly, never as 1 - F
#                                 (gamlss.dist: 0 where the true value is 6.46e-66)
#   dep("p = 1")                  p = 1 gives Inf (gamlss.dist: an error)
#   dep("upper-tail quantile")    an upper-tail quantile keeps relative accuracy where
#                                 1 - p would round to 1
#   dep("log lower tail F > 1/2") the lower tail with log_p = TRUE keeps relative accuracy
#                                 as F -> 1: log F is log1p(-U) and Q is inverted from
#                                 -expm1(p) (gamlss.dist: log F = 0 where the true value
#                                 is -6.46e-66, and an error from qZAGA for p near 0)
#   conv("quantile >= 0")         a quantile is never negative: a negative value from
#                                 qgamma (shapes beyond ~1e49) is NaN
#   conv("NaN warning")           a NaN out of non-NaN inputs gives one "NaNs produced"
#                                 warning per call, as base R; NA/NaN inputs stay silent
#   conv("error messages")        the CKutils wording of every error ("mu must be greater
#                                 than 0", ...); gamlss.dist says "mu must be positive"
#   conv("NaN/NA")                a NaN/NA argument gives NA (gamlss.dist errors)
#   conv("zero-length")           a zero-length argument gives a zero-length result
#   conv("recycling")             all arguments recycle to the longest
#   conv("dqrng")                 frZAGA draws from dqrng
#   num("log1p(-nu)")             log(1 - nu) is 0 for a tiny nu
#   num("small-sigma density")    dZAGA cancels at sigma = 1e-3 (relative error 3e-9)
#   num("tiny-scale density")     dgamma underflows or is inaccurate for a tiny mu *
#                                 sigma^2: the natural density below 1e-280 is exp(log)
#   num("upper log quantile")     the upper-tail quantile on the log scale, for a tiny
#                                 nu (p just below log(1 - nu)), inverts from the log
#                                 of the gamma part's lower tail
#   num("zero-mass edge")         log(nu) is the zero mass on the log scale; F(0) = nu
#                                 is exact, also where sigma^2 underflows

# ----------------------------------------------------------------------------
# HELPERS
# ----------------------------------------------------------------------------

dep <- function(what) paste0("[departure: ", what, "] ")
conv <- function(what) paste0("[convention: ", what, "] ")
num <- function(what) paste0("[numerics: ", what, "] ")

# Largest elementwise error between `current` and `target`, relative by default.
# expect_equal() averages the relative error over the vector (and goes absolute below
# its tolerance), so a single wrong element, or a 0 against a true 1e-28, can pass.
# Any structural mismatch gives Inf: length, the NA pattern, the position and sign of
# an infinity, or (relative) a 0 against a non-zero.
max_err <- function(current, target, relative = TRUE) {
    current <- as.numeric(current)
    target <- as.numeric(target)
    if (length(current) != length(target)) return(Inf)
    if (!identical(is.na(current), is.na(target))) return(Inf)
    keep <- !is.na(target)
    current <- current[keep]
    target <- target[keep]
    inf <- is.infinite(current) | is.infinite(target)
    if (!identical(current[inf], target[inf])) return(Inf)
    current <- current[!inf]
    target <- target[!inf]
    if (length(target) == 0L) return(0)
    d <- abs(current - target)
    if (relative) {
        d <- d / abs(target)
        d[current == target] <- 0
    }
    max(d)
}

expect_close <- function(current, target, tol = 1e-12, info = "", relative = TRUE) {
    err <- max_err(current, target, relative)
    expect_true(isTRUE(err <= tol),
                info = sprintf("%s (max %s err. %.3g, tolerance %g)", info,
                               if (relative) "rel." else "abs.", err, tol))
}

# the message of the error `expr` raises, NA when it raises none
err_msg <- function(expr) {
    tryCatch({
        expr
        NA_character_
    }, error = function(e) conditionMessage(e))
}

# Closed-form references. Arguments are recycled to a common length first, so that
# ifelse() returns one value per element.
.recycle4 <- function(a, b, c, d) {
    n <- max(length(a), length(b), length(c), length(d))
    list(rep_len(a, n), rep_len(b, n), rep_len(c, n), rep_len(d, n))
}

ref_d <- function(x, mu, sigma, nu, log = FALSE) {
    r <- .recycle4(x, mu, sigma, nu)
    x <- r[[1]]; mu <- r[[2]]; sigma <- r[[3]]; nu <- r[[4]]
    a <- 1 / sigma^2
    s <- mu * sigma^2
    if (log) {
        ifelse(x < 0, -Inf,
               ifelse(x == 0, log(nu), log1p(-nu) + dgamma(x, a, scale = s, log = TRUE)))
    } else {
        ifelse(x < 0, 0, ifelse(x == 0, nu, (1 - nu) * dgamma(x, a, scale = s)))
    }
}

ref_p <- function(q, mu, sigma, nu, lower_tail = TRUE, log_p = FALSE) {
    r <- .recycle4(q, mu, sigma, nu)
    q <- r[[1]]; mu <- r[[2]]; sigma <- r[[3]]; nu <- r[[4]]
    a <- 1 / sigma^2
    s <- mu * sigma^2
    if (lower_tail) {
        f <- ifelse(q < 0, 0, pmin(nu + (1 - nu) * pgamma(q, a, scale = s), 1))
        if (log_p) log(f) else f
    } else if (log_p) {
        ifelse(q < 0, 0, log1p(-nu) + pgamma(q, a, scale = s, lower.tail = FALSE, log.p = TRUE))
    } else {
        ifelse(q < 0, 1, (1 - nu) * pgamma(q, a, scale = s, lower.tail = FALSE))
    }
}

ref_q <- function(p, mu, sigma, nu, lower_tail = TRUE, log_p = FALSE) {
    r <- .recycle4(p, mu, sigma, nu)
    p <- r[[1]]; mu <- r[[2]]; sigma <- r[[3]]; nu <- r[[4]]
    a <- 1 / sigma^2
    s <- mu * sigma^2
    # ifelse() evaluates both branches; the branch not taken may see an argument
    # outside (0, 1), which only produces NaN warnings
    suppressWarnings({
        if (lower_tail) {
            pl <- if (log_p) exp(p) else p
            ifelse(pl <= nu, 0,
                   ifelse(pl == 1, Inf, qgamma((pl - nu) / (1 - nu), a, scale = s)))
        } else if (log_p) {
            ifelse(p >= log1p(-nu), 0,
                   ifelse(p == -Inf, Inf,
                          qgamma(p - log1p(-nu), a, scale = s, lower.tail = FALSE, log.p = TRUE)))
        } else {
            ifelse(p >= 1 - nu, 0,
                   ifelse(p == 0, Inf, qgamma(p / (1 - nu), a, scale = s, lower.tail = FALSE)))
        }
    })
}

# ----------------------------------------------------------------------------
# INTERFACE: argument names, defaults, return type
# ----------------------------------------------------------------------------

expect_identical(names(formals(fdZAGA)), c("x", "mu", "sigma", "nu", "log"),
                 info = "fdZAGA arguments")
expect_identical(formals(fdZAGA)$log, FALSE, info = "fdZAGA log defaults to FALSE")
expect_identical(names(formals(fpZAGA)), c("q", "mu", "sigma", "nu", "lower_tail", "log_p"),
                 info = "fpZAGA arguments")
expect_identical(names(formals(fqZAGA)), c("p", "mu", "sigma", "nu", "lower_tail", "log_p"),
                 info = "fqZAGA arguments")
expect_identical(c(formals(fpZAGA)$lower_tail, formals(fpZAGA)$log_p), c(TRUE, FALSE),
                 info = "fpZAGA defaults: lower_tail = TRUE, log_p = FALSE")
expect_identical(c(formals(fqZAGA)$lower_tail, formals(fqZAGA)$log_p), c(TRUE, FALSE),
                 info = "fqZAGA defaults: lower_tail = TRUE, log_p = FALSE")
expect_identical(names(formals(frZAGA)), c("n", "mu", "sigma", "nu"), info = "frZAGA arguments")
expect_identical(unlist(formals(frZAGA)[c("mu", "sigma", "nu")]),
                 c(mu = 1, sigma = 1, nu = 0.1), info = "frZAGA defaults")

# ZAGA is continuous: every function returns a double, including the quantile, which
# is not an integer as it is for the count distributions. Integer input is accepted.
expect_true(is.double(fdZAGA(1, 2, 1, 0.1)), info = "fdZAGA returns a double")
expect_true(is.double(fpZAGA(1, 2, 1, 0.1)), info = "fpZAGA returns a double")
expect_true(is.double(fqZAGA(0.5, 2, 1, 0.1)), info = "fqZAGA returns a double")
expect_true(is.double(fqZAGA(0.01, 2, 1, 0.1)), info = "fqZAGA returns a double when the quantile is 0")
expect_true(is.double(frZAGA(3, 2, 1, 0.1)), info = "frZAGA returns a double")
expect_true(fqZAGA(0.5, 2, 1, 0.1) %% 1 != 0, info = "the quantile keeps its fractional part")
expect_identical(fdZAGA(1L, 2L, 1L, 0.1), fdZAGA(1, 2, 1, 0.1),
                 info = "integer x, mu and sigma give the same density as doubles")
expect_identical(fpZAGA(1L, 2L, 1L, 0.1), fpZAGA(1, 2, 1, 0.1),
                 info = "integer q, mu and sigma give the same CDF as doubles")

# ----------------------------------------------------------------------------
# HAND-PICKED POINTS, closed forms that do not use pgamma or qgamma
#
# sigma = 1 makes the gamma an exponential with mean mu (shape 1, scale mu):
#   f(x) = (1 - nu) / mu * exp(-x / mu),  P(Y > x) = (1 - nu) * exp(-x / mu),
#   F(x) = 1 - (1 - nu) * exp(-x / mu),   Q_upper(pu) = -mu * log(pu / (1 - nu)).
# sigma = 0.5 makes it an Erlang of shape 4 and scale mu / 4 (both exact in binary),
# with y = x / s:  f = (1 - nu) * y^3 * exp(-y) / (6 * s)
#                  P(Y > x) = (1 - nu) * exp(-y) * (1 + y + y^2 / 2 + y^3 / 6).
# ----------------------------------------------------------------------------

xe <- c(0, 0.5, 2, 7)                 # mu = 2, sigma = 1, nu = 0.1
expect_close(fdZAGA(xe, 2, 1, 0.1),
             c(0.1, 0.9 / 2 * exp(-0.25), 0.9 / 2 * exp(-1), 0.9 / 2 * exp(-3.5)),
             info = "exponential density")
expect_close(fdZAGA(xe, 2, 1, 0.1, log = TRUE),
             c(log(0.1), log(0.9) - log(2) - 0.25, log(0.9) - log(2) - 1, log(0.9) - log(2) - 3.5),
             info = "exponential log density")
expect_identical(fpZAGA(0, 2, 1, 0.1), 0.1, info = "F(0) is exactly nu")
expect_close(fpZAGA(0, 2, 1, 0.1, log_p = TRUE), log(0.1), info = "log F(0) = log(nu)")
expect_close(fpZAGA(0, 2, 1, 0.1, lower_tail = FALSE), 0.9, info = "P(Y > 0) = 1 - nu")
expect_close(fpZAGA(xe[-1], 2, 1, 0.1), 1 - 0.9 * exp(-xe[-1] / 2), info = "exponential CDF")
expect_close(fpZAGA(xe, 2, 1, 0.1, lower_tail = FALSE), 0.9 * exp(-xe / 2),
             info = "exponential upper tail")
expect_close(fpZAGA(xe, 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE), log(0.9) - xe / 2,
             info = "exponential log upper tail")
expect_close(fpZAGA(xe[-1], 2, 1, 0.1, log_p = TRUE), log(1 - 0.9 * exp(-xe[-1] / 2)),
             info = "exponential log CDF")
pe <- c(0.55, 0.7, 0.9, 0.99)         # 1 - p is exact for p >= 0.5
expect_close(fqZAGA(pe, 2, 1, 0.1), -2 * log((1 - pe) / 0.9), info = "exponential quantile")
pu_e <- c(0.05, 0.3, 0.6, 0.85)     # below 1 - nu = 0.9, the positive part
expect_close(fqZAGA(pu_e, 2, 1, 0.1, lower_tail = FALSE), -2 * log(pu_e / 0.9),
             info = "exponential upper-tail quantile")
expect_close(fqZAGA(log(pu_e), 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE),
             -2 * (log(pu_e) - log1p(-0.1)), info = "exponential upper-tail quantile, log_p")

xr <- c(0.3, 1.5, 4, 9)               # mu = 3, sigma = 0.5, nu = 0.25: s = 0.75
yr <- xr / 0.75
expect_close(fdZAGA(xr, 3, 0.5, 0.25), 0.75 * yr^3 * exp(-yr) / (6 * 0.75),
             info = "Erlang-4 density")
expect_close(fpZAGA(xr, 3, 0.5, 0.25, lower_tail = FALSE),
             0.75 * exp(-yr) * (1 + yr + yr^2 / 2 + yr^3 / 6), info = "Erlang-4 upper tail")
expect_close(fpZAGA(xr, 3, 0.5, 0.25, lower_tail = FALSE, log_p = TRUE),
             log(0.75) - yr + log(1 + yr + yr^2 / 2 + yr^3 / 6), info = "Erlang-4 log upper tail")

# E[Y] = (1 - nu) mu and Var[Y] = (1 - nu) mu^2 (sigma^2 + nu), from the density by
# quadrature on the Erlang-4 case (the density is smooth there)
m1 <- integrate(function(x) x * fdZAGA(x, 3, 0.5, 0.25), 0, Inf, rel.tol = 1e-10)$value
m2 <- integrate(function(x) x^2 * fdZAGA(x, 3, 0.5, 0.25), 0, Inf, rel.tol = 1e-10)$value
expect_equal(m1, 0.75 * 3, tolerance = 1e-8, info = "mean of ZAGA is (1 - nu) mu")
expect_equal(m2 - m1^2, 0.75 * 9 * (0.25 + 0.25), tolerance = 1e-8,
             info = "variance of ZAGA is (1 - nu) mu^2 (sigma^2 + nu)")

# ----------------------------------------------------------------------------
# RANDOM GRIDS against the closed forms built from stats::*gamma
#
# Tolerance 1e-12 relative, elementwise (see max_err()). mu and sigma are
# log-uniform over a wide range, nu is uniform on (0.01, 0.99).
# ----------------------------------------------------------------------------

set.seed(20260101)
ng <- 300
g_mu <- exp(runif(ng, log(0.05), log(50)))
g_sigma <- exp(runif(ng, log(0.15), log(4)))
g_nu <- runif(ng, 0.01, 0.99)
# x >= 0: 30 zeros, the rest spread over the body of each gamma (0.5% to 99.5%).
# Negative x is below the support and is tested with the departures further down.
g_x <- qgamma(runif(ng, 0.005, 0.995), 1 / g_sigma^2, scale = g_mu * g_sigma^2)
g_x[1:30] <- 0
g_p <- runif(ng, 0.001, 0.999)

expect_silent(d_new <- fdZAGA(g_x, g_mu, g_sigma, g_nu))
expect_close(d_new, ref_d(g_x, g_mu, g_sigma, g_nu), info = "density on the random grid")
expect_close(fdZAGA(g_x, g_mu, g_sigma, g_nu, log = TRUE),
             ref_d(g_x, g_mu, g_sigma, g_nu, log = TRUE),
             info = "log density on the random grid")
expect_silent(p_new <- fpZAGA(g_x, g_mu, g_sigma, g_nu))
expect_close(p_new, ref_p(g_x, g_mu, g_sigma, g_nu), info = "CDF on the random grid")
# log F is compared in absolute terms (1e-12): F is in [nu, 1], and where F is close to 1
# the relative error of log F is the rounding error of F divided by 1 - F, which any
# faithful implementation (fused multiply-add or not) cannot be held to
expect_close(fpZAGA(g_x, g_mu, g_sigma, g_nu, log_p = TRUE),
             ref_p(g_x, g_mu, g_sigma, g_nu, log_p = TRUE), relative = FALSE,
             info = "log CDF on the random grid")
# the upper tails: gamlss.dist forms 1 - F, which cannot reach 1e-12 where F is near 1
expect_close(fpZAGA(g_x, g_mu, g_sigma, g_nu, lower_tail = FALSE),
             ref_p(g_x, g_mu, g_sigma, g_nu, lower_tail = FALSE),
             info = paste0(dep("direct upper tail"), "upper tail on the random grid"))
expect_close(fpZAGA(g_x, g_mu, g_sigma, g_nu, lower_tail = FALSE, log_p = TRUE),
             ref_p(g_x, g_mu, g_sigma, g_nu, lower_tail = FALSE, log_p = TRUE),
             info = paste0(dep("direct upper tail"), "log upper tail on the random grid"))
expect_silent(q_new <- fqZAGA(g_p, g_mu, g_sigma, g_nu))
expect_close(q_new, ref_q(g_p, g_mu, g_sigma, g_nu), info = "quantile on the random grid")
expect_close(fqZAGA(log(g_p), g_mu, g_sigma, g_nu, log_p = TRUE),
             ref_q(log(g_p), g_mu, g_sigma, g_nu, log_p = TRUE),
             info = "log_p quantile on the random grid")
# (again 1 - p in gamlss.dist, which cannot reach 1e-12 for every p)
expect_close(fqZAGA(g_p, g_mu, g_sigma, g_nu, lower_tail = FALSE),
             ref_q(g_p, g_mu, g_sigma, g_nu, lower_tail = FALSE),
             info = paste0(dep("upper-tail quantile"), "upper-tail quantile on the random grid"))
expect_close(fqZAGA(log(g_p), g_mu, g_sigma, g_nu, lower_tail = FALSE, log_p = TRUE),
             ref_q(log(g_p), g_mu, g_sigma, g_nu, lower_tail = FALSE, log_p = TRUE),
             info = paste0(dep("upper-tail quantile"), "log_p upper-tail quantile on the random grid"))
# the grid reaches the zero region of the quantile and the positive part
expect_true(any(q_new == 0) && any(q_new > 0), info = "the p grid covers both parts of the quantile")

# properties of a distribution function on the grid
expect_true(all(p_new >= g_nu) && all(p_new <= 1), info = "nu <= F(q) <= 1 for q >= 0")
expect_identical(p_new[g_x == 0], g_nu[g_x == 0], info = "F(0) is exactly nu on the grid")
expect_close(fpZAGA(g_x, g_mu, g_sigma, g_nu) + fpZAGA(g_x, g_mu, g_sigma, g_nu, lower_tail = FALSE),
             rep(1, ng), tol = 1e-12, info = "the two tails sum to 1")
# the density at 0 is the mass nu (to a few ulp: a log-then-exp route is allowed)
expect_close(fdZAGA(c(0, 0), c(0.5, 3), c(0.7, 2), c(0.2, 0.6)), c(0.2, 0.6), tol = 1e-15,
             info = "the density at 0 is the mass nu")
expect_close(fdZAGA(c(0, 0), c(0.5, 3), c(0.7, 2), c(0.2, 0.6), log = TRUE), log(c(0.2, 0.6)),
             tol = 1e-15, info = "the log density at 0 is log(nu)")

# the density integrates to the CDF increment (shape > 1 here: no singularity at 0)
for (par in list(c(2, 0.6, 0.3), c(5, 0.9, 0.05), c(0.7, 0.45, 0.8))) {
    q_hi <- fqZAGA(0.97, par[1], par[2], par[3])
    area <- integrate(function(x) fdZAGA(x, par[1], par[2], par[3]), 0, q_hi, rel.tol = 1e-10)$value
    expect_equal(area, fpZAGA(q_hi, par[1], par[2], par[3]) - par[3], tolerance = 1e-8,
                 info = "the density integrates to F(q) - nu")
}

# the log density is the log of the density (moderate values, no underflow)
expect_close(fdZAGA(g_x, g_mu, g_sigma, g_nu, log = TRUE), log(d_new), tol = 1e-12,
             info = "log density is the log of the density")

# ----------------------------------------------------------------------------
# AGREEMENT WITH gamlss.dist where gamlss.dist is right
#
# Scalar calls through mapply(): gamlss.dist's vectorised code is not trusted when the
# parameters vary along the vectors. x, q >= 0 and moderate p only; the departures are
# tested separately below.
# ----------------------------------------------------------------------------

if (requireNamespace("gamlss.dist", quietly = TRUE)) local({
    set.seed(20260102)
    n <- 200
    mu <- exp(runif(n, log(0.1), log(20)))
    sigma <- exp(runif(n, log(0.2), log(2.5)))
    nu <- runif(n, 0.02, 0.9)
    x <- qgamma(runif(n, 0.01, 0.99), 1 / sigma^2, scale = mu * sigma^2)
    x[1:20] <- 0
    p <- runif(n, 0.001, 0.999)
    # a lower-tail probability whose upper tail 1 - p is well conditioned for gamlss.dist:
    # (1 - p - nu) / (1 - nu) lies in [0.1, 0.95]
    p_up <- (1 - nu) * runif(n, 0.05, 0.9)

    gl_d <- mapply(function(x, m, s, v) gamlss.dist::dZAGA(x, m, s, v), x, mu, sigma, nu)
    gl_dl <- mapply(function(x, m, s, v) gamlss.dist::dZAGA(x, m, s, v, log = TRUE), x, mu, sigma, nu)
    gl_p <- mapply(function(q, m, s, v) gamlss.dist::pZAGA(q, m, s, v), x, mu, sigma, nu)
    gl_pl <- mapply(function(q, m, s, v) gamlss.dist::pZAGA(q, m, s, v, log.p = TRUE), x, mu, sigma, nu)
    gl_q <- mapply(function(p, m, s, v) gamlss.dist::qZAGA(p, m, s, v), p, mu, sigma, nu)
    gl_ql <- mapply(function(p, m, s, v) gamlss.dist::qZAGA(p, m, s, v, log.p = TRUE),
                    log(p), mu, sigma, nu)

    # 1e-10 for the density: dZAGA writes the log density as a sum of large terms
    # (a log(x / s), x / s, lgamma(a)) and loses a few digits to cancellation; the
    # worst case on this grid is about 2e-13
    expect_close(fdZAGA(x, mu, sigma, nu), gl_d, tol = 1e-10, info = "density vs gamlss.dist::dZAGA")
    expect_close(fdZAGA(x, mu, sigma, nu, log = TRUE), gl_dl, tol = 1e-10,
                 info = "log density vs gamlss.dist::dZAGA")
    expect_close(fpZAGA(x, mu, sigma, nu), gl_p, info = "CDF vs gamlss.dist::pZAGA")
    expect_close(fpZAGA(x, mu, sigma, nu, log_p = TRUE), gl_pl, relative = FALSE,
                 info = "log CDF vs gamlss.dist::pZAGA")
    expect_close(fqZAGA(p, mu, sigma, nu), gl_q, info = "quantile vs gamlss.dist::qZAGA")
    expect_close(fqZAGA(log(p), mu, sigma, nu, log_p = TRUE), gl_ql,
                 info = "log_p quantile vs gamlss.dist::qZAGA")

    # the upper tail of gamlss.dist is 1 - F: right only where the tail is not small, so
    # q is taken where P(Y > q) >= 0.01 (1e-10 leaves room for the lost digits)
    ok <- fpZAGA(x, mu, sigma, nu, lower_tail = FALSE) > 0.01
    expect_true(sum(ok) > 50, info = "the grid has well-conditioned upper tails")
    gl_pu <- mapply(function(q, m, s, v) gamlss.dist::pZAGA(q, m, s, v, lower.tail = FALSE),
                    x[ok], mu[ok], sigma[ok], nu[ok])
    expect_close(fpZAGA(x[ok], mu[ok], sigma[ok], nu[ok], lower_tail = FALSE), gl_pu, tol = 1e-10,
                 info = "upper tail vs gamlss.dist::pZAGA where 1 - F is accurate")
    gl_qu <- mapply(function(p, m, s, v) gamlss.dist::qZAGA(p, m, s, v, lower.tail = FALSE),
                    p_up, mu, sigma, nu)
    expect_close(fqZAGA(p_up, mu, sigma, nu, lower_tail = FALSE), gl_qu, tol = 1e-10,
                 info = "upper-tail quantile vs gamlss.dist::qZAGA where 1 - p is accurate")
})

# ----------------------------------------------------------------------------
# DEPARTURES FROM gamlss.dist, asserted against the closed forms
# ----------------------------------------------------------------------------

# q < 0 is below the support. All four (lower_tail, log_p) combinations give
# 0 / -Inf / 1 / 0; gamlss.dist gives 0 for every one of them.
qneg <- c(-1, -1e-300, -1e10, -Inf)
expect_identical(fpZAGA(qneg, 2, 1, 0.1, lower_tail = TRUE, log_p = FALSE), rep(0, 4),
                 info = "F(q < 0) = 0")
expect_identical(fpZAGA(qneg, 2, 1, 0.1, lower_tail = TRUE, log_p = TRUE), rep(-Inf, 4),
                 info = paste0(dep("q < 0 tails"), "log F(q < 0) = -Inf"))
expect_identical(fpZAGA(qneg, 2, 1, 0.1, lower_tail = FALSE, log_p = FALSE), rep(1, 4),
                 info = paste0(dep("q < 0 tails"), "P(Y > q) = 1 for q < 0"))
expect_identical(fpZAGA(qneg, 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE), rep(0, 4),
                 info = paste0(dep("q < 0 tails"), "log P(Y > q) = 0 for q < 0"))
# the element-wise rule inside a mixed vector, with parameters that differ per element
expect_identical(fpZAGA(c(-1, 0, 1), c(1, 2, 3), 1, c(0.2, 0.3, 0.4), lower_tail = FALSE)[1], 1,
                 info = paste0(dep("q < 0 tails"), "q < 0 inside a mixed vector"))

# the same with random parameters and a spread of negative q
neg_q <- -c(1e-300, runif(ng - 2, 0, 10), Inf)
expect_identical(fpZAGA(neg_q, g_mu, g_sigma, g_nu), rep(0, ng), info = "F(q < 0) = 0, random parameters")
expect_identical(fpZAGA(neg_q, g_mu, g_sigma, g_nu, log_p = TRUE), rep(-Inf, ng),
                 info = paste0(dep("q < 0 tails"), "log F(q < 0) = -Inf, random parameters"))
expect_identical(fpZAGA(neg_q, g_mu, g_sigma, g_nu, lower_tail = FALSE), rep(1, ng),
                 info = paste0(dep("q < 0 tails"), "P(Y > q) = 1 for q < 0, random parameters"))
expect_identical(fpZAGA(neg_q, g_mu, g_sigma, g_nu, lower_tail = FALSE, log_p = TRUE), rep(0, ng),
                 info = paste0(dep("q < 0 tails"), "log P(Y > q) = 0 for q < 0, random parameters"))

# the log density at x < 0 is -Inf (the density itself is 0)
xneg <- c(-1, -1e-300, -Inf)
expect_identical(fdZAGA(xneg, 2, 1, 0.1), rep(0, 3), info = "f(x < 0) = 0")
expect_identical(fdZAGA(xneg, 2, 1, 0.1, log = TRUE), rep(-Inf, 3),
                 info = paste0(dep("log density x < 0"), "log f(x < 0) = -Inf"))
expect_identical(fdZAGA(neg_q, g_mu, g_sigma, g_nu), rep(0, ng), info = "f(x < 0) = 0, random parameters")
expect_identical(fdZAGA(neg_q, g_mu, g_sigma, g_nu, log = TRUE), rep(-Inf, ng),
                 info = paste0(dep("log density x < 0"), "log f(x < 0) = -Inf, random parameters"))

# x = +Inf: density 0 (log -Inf), F = 1 (upper 0, log upper -Inf), for every nu
nus <- c(0.001, 0.1, 0.3, 0.5, 0.7, 0.123456789, 0.999)
infs <- rep(Inf, length(nus))
expect_identical(fdZAGA(infs, 2, 1, nus), rep(0, length(nus)),
                 info = paste0(dep("density at Inf"), "f(Inf) = 0"))
expect_identical(fdZAGA(infs, 2, 1, nus, log = TRUE), rep(-Inf, length(nus)),
                 info = paste0(dep("density at Inf"), "log f(Inf) = -Inf"))
expect_identical(fpZAGA(infs, 2, 1, nus), rep(1, length(nus)), info = "F(Inf) is exactly 1")
expect_identical(fpZAGA(infs, 2, 1, nus, log_p = TRUE), rep(0, length(nus)), info = "log F(Inf) is exactly 0")
expect_identical(fpZAGA(infs, 2, 1, nus, lower_tail = FALSE), rep(0, length(nus)),
                 info = "P(Y > Inf) = 0")
expect_identical(fpZAGA(infs, 2, 1, nus, lower_tail = FALSE, log_p = TRUE), rep(-Inf, length(nus)),
                 info = "log P(Y > Inf) = -Inf")

# F never exceeds 1 (the SPEC clamps it; nu + (1 - nu) * G(q) cannot round above 1, so
# this guards the result, not the clamp)
expect_true(max(fpZAGA(c(1e3, 1e6, 1e12), 2, 1, c(0.3, 0.7, 0.9999))) <= 1, info = "F never exceeds 1")
expect_identical(fpZAGA(1e12, 2, 1, 0.3), 1, info = "F(1e12) is exactly 1")

# THE UPPER TAIL IS DIRECT, NEVER 1 - F. For mu = 2, sigma = 1, nu = 0.1 the gamma is
# exponential, P(Y > q) = 0.9 * exp(-q / 2): 6.46e-66 at q = 300, where F rounds to 1
# and 1 - F is 0. At q = 1e4 the plain tail underflows (0.9 * exp(-5000)) but the log
# tail is the finite -5000 + log(0.9).
up300 <- fpZAGA(300, 2, 1, 0.1, lower_tail = FALSE)
expect_true(up300 > 6.4e-66 && up300 < 6.5e-66,
            info = paste0(dep("direct upper tail"), "P(Y > 300) is 6.46e-66, not 0"))
expect_close(up300, 0.9 * exp(-150), info = paste0(dep("direct upper tail"), "P(Y > 300), exponential closed form"))
expect_close(fpZAGA(300, 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE), log(0.9) - 150,
             info = paste0(dep("direct upper tail"), "log P(Y > 300)"))
expect_identical(fpZAGA(1e4, 2, 1, 0.1, lower_tail = FALSE), 0,
                 info = "P(Y > 1e4) underflows to 0 (the true value is 3e-2172)")
expect_close(fpZAGA(1e4, 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE), log(0.9) - 5000,
             info = paste0(dep("direct upper tail"), "log P(Y > 1e4) is finite where the tail underflows"))
# the same on a non-exponential gamma (shape 4, scale 0.5), against pgamma, over a
# range of q and nu: the upper tail across the point where F rounds to 1
qq <- c(20, 50, 100, 150)
nn <- c(0.01, 0.5, 0.99)
fr <- expand.grid(q = qq, nu = nn)
expect_close(fpZAGA(fr$q, 2, 0.5, fr$nu, lower_tail = FALSE),
             (1 - fr$nu) * pgamma(fr$q, 4, scale = 0.5, lower.tail = FALSE),
             info = paste0(dep("direct upper tail"), "upper tail, shape 4, F rounded to 1"))
expect_close(fpZAGA(fr$q, 2, 0.5, fr$nu, lower_tail = FALSE, log_p = TRUE),
             log1p(-fr$nu) + pgamma(fr$q, 4, scale = 0.5, lower.tail = FALSE, log.p = TRUE),
             info = paste0(dep("direct upper tail"), "log upper tail, shape 4, F rounded to 1"))
# log1p(-nu), not log(1 - nu): for a tiny nu, 1 - nu rounds to 1
expect_close(fpZAGA(0, 1, 1, 1e-20, lower_tail = FALSE, log_p = TRUE), -1e-20,
             info = paste0(dep("direct upper tail"), num("log1p(-nu)"),
                           "log upper tail at q = 0 and nu = 1e-20 is log1p(-nu)"))
expect_close(fdZAGA(1e-30, 1, 1, 1e-20, log = TRUE), -1e-20 - 1e-30,
             info = paste0(num("log1p(-nu)"), "log density at nu = 1e-20 keeps the zero-adjustment term"))

# p = 1 is the infinite quantile (gamlss.dist stops with an error); so is an upper-tail
# p = 0 and the log scale's p = 0 (lower) and p = -Inf (upper)
expect_identical(fqZAGA(1, c(0.5, 2, 30), c(0.3, 1, 3), c(0.01, 0.1, 0.9)), rep(Inf, 3),
                 info = paste0(dep("p = 1"), "Q(1) = Inf"))
expect_identical(fqZAGA(0, 2, 1, 0.1, log_p = TRUE), Inf,
                 info = paste0(dep("p = 1"), "Q(log p = 0) = Inf"))
expect_identical(fqZAGA(0, c(0.5, 2, 30), c(0.3, 1, 3), c(0.01, 0.1, 0.9), lower_tail = FALSE), rep(Inf, 3),
                 info = paste0(dep("p = 1"), "upper-tail Q(0) = Inf"))
expect_identical(fqZAGA(-Inf, 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE), Inf,
                 info = paste0(dep("p = 1"), "upper-tail Q(log p = -Inf) = Inf"))
expect_silent(fqZAGA(1, 2, 1, 0.1), info = paste0(dep("p = 1"), "Q(1) raises neither an error nor a warning"))

# the zero region of the quantile: p <= nu is 0, p just above nu is positive
nz <- c(0.05, 0.3, 0.9)
expect_identical(fqZAGA(nz, 2, 1, nz), rep(0, 3), info = "Q(nu) is exactly 0")
expect_identical(fqZAGA(c(0, nz / 2, 1e-300), 2, 1, c(0.4, nz[1:3], 0.4)), rep(0, 5),
                 info = "Q(p) is 0 for p below nu, including p = 0 and 1e-300")
# p is within a factor 2 of nu here, so p - nu is exact (Sterbenz) and the reference
# is the same qgamma argument as the SPEC's, to the last bit
pj <- nz + c(1e-9, 1e-6, 1e-3) * (1 - nz)
expect_true(all(fqZAGA(pj, 2, 1, nz) > 0), info = "Q(p) > 0 for p just above nu")
expect_close(fqZAGA(pj, 2, 1, nz), qgamma((pj - nz) / (1 - nz), 1, scale = 2),
             info = "Q(p) for p just above nu")
expect_close(fqZAGA(pj, 2, 1, nz), -2 * log1p(-(pj - nz) / (1 - nz)),
             info = "Q(p) for p just above nu, exponential closed form")
# log scale: well below and well above log(nu)
expect_identical(fqZAGA(log(0.1) - 1e-6, 2, 1, 0.1, log_p = TRUE), 0, info = "Q(log p) is 0 just below log(nu)")
expect_true(fqZAGA(log(0.1 + 1e-6), 2, 1, 0.1, log_p = TRUE) > 0, info = "Q(log p) > 0 just above log(nu)")
# upper tail: the zero region is p >= 1 - nu (log_p: p >= log1p(-nu))
expect_identical(fqZAGA(c(1 - 0.1, 1), 2, 1, 0.1, lower_tail = FALSE), c(0, 0),
                 info = "upper-tail Q(1 - nu) = 0 and Q(1) = 0")
expect_identical(fqZAGA(c(log1p(-0.1), 0), 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE), c(0, 0),
                 info = "upper-tail Q(log(1 - nu)) = 0 and Q(log 1) = 0")
expect_true(fqZAGA((1 - 0.1) * (1 - 1e-6), 2, 1, 0.1, lower_tail = FALSE) > 0,
            info = "upper-tail Q just below 1 - nu is positive")

# AN UPPER-TAIL QUANTILE KEEPS RELATIVE ACCURACY. 1 - p rounds to 1 for p = 1e-300, so
# a lower-tail inversion would see p = 1 and fail; on the upper scale the quantile is
# qgamma(p / (1 - nu), lower = FALSE): for the exponential, -mu * log(p / (1 - nu)).
tiny <- c(1e-10, 1e-20, 1e-100, 1e-300)
expect_true({q_tiny <- fqZAGA(tiny, 2, 1, 0.1, lower_tail = FALSE); all(is.finite(q_tiny)) && all(q_tiny > 0)},
            info = paste0(dep("upper-tail quantile"), "upper-tail Q(1e-300) is finite"))
expect_close(q_tiny, -2 * log(tiny / 0.9),
             info = paste0(dep("upper-tail quantile"), "upper-tail Q, exponential closed form"))
expect_close(fqZAGA(1e-300, c(2, 7), c(1, 0.5), 0.1, lower_tail = FALSE),
             c(qgamma(1e-300 / 0.9, 1, scale = 2, lower.tail = FALSE),
               qgamma(1e-300 / 0.9, 4, scale = 7 / 4, lower.tail = FALSE)),
             info = paste0(dep("upper-tail quantile"), "upper-tail Q(1e-300) = qgamma(p / (1 - nu), lower = FALSE)"))
# on the log scale p can be far below what exp() can hold: log p = -1e5
expect_close(fqZAGA(c(-1e3, -1e5), 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE),
             -2 * (c(-1e3, -1e5) - log1p(-0.1)),
             info = paste0(dep("upper-tail quantile"), "log_p upper-tail Q where exp(p) underflows"))
# the quantile and the tail invert each other in the far tail: nothing is subtracted
# from 1 on either side, so the round trip is accurate to |log p| * eps (hence 1e-11)
expect_close(fpZAGA(q_tiny, 2, 1, 0.1, lower_tail = FALSE), tiny, tol = 1e-11,
             info = paste0(dep("upper-tail quantile"), "P(Y > Q_upper(p)) = p, far tail"))
xt <- c(5, 20, 100, 300, 1000)
expect_close(fqZAGA(fpZAGA(xt, 2, 1, 0.1, lower_tail = FALSE), 2, 1, 0.1, lower_tail = FALSE), xt,
             info = paste0(dep("direct upper tail"), "Q_upper(P(Y > x)) = x, exponential, far tail"))
expect_close(fqZAGA(fpZAGA(xt, 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE), 2, 1, 0.1,
                    lower_tail = FALSE, log_p = TRUE), xt,
             info = paste0(dep("direct upper tail"), "Q_upper(log P(Y > x)) = x, exponential, far tail"))
xg <- c(5, 20, 60, 150)
expect_close(fqZAGA(fpZAGA(xg, 3, 0.5, 0.3, lower_tail = FALSE), 3, 0.5, 0.3, lower_tail = FALSE), xg,
             info = paste0(dep("direct upper tail"), "Q_upper(P(Y > x)) = x, Erlang, far tail"))

# ----------------------------------------------------------------------------
# NaN / NA HANDLING
#
# An element with a NaN or NA in any argument is NA, and is skipped by the parameter
# checks; the other elements are computed as usual.
# ----------------------------------------------------------------------------

na_checks <- function(f, args, label, tag) {
    ref <- do.call(f, args)
    for (k in seq_along(args)) {
        for (bad in list(NA_real_, NaN)) {
            a <- lapply(args, rep_len, 3L)
            a[[k]][2] <- bad
            res <- do.call(f, a)
            expect_true(length(res) == 3L && is.na(res[2]),
                        info = paste0(tag, label, ": ", format(bad), " in argument ", k, " gives NA"))
            expect_close(res[c(1, 3)], ref[c(1, 3)],
                         info = paste0(tag, label, ": ", format(bad), " in argument ", k, ", other elements intact"))
        }
    }
}
na_checks(fdZAGA, list(c(0.5, 1, 3), 2, 0.8, 0.15), "fdZAGA", conv("NaN/NA"))
na_checks(fpZAGA, list(c(0.5, 1, 3), 2, 0.8, 0.15), "fpZAGA", conv("NaN/NA"))
na_checks(fqZAGA, list(c(0.3, 0.6, 0.9), 2, 0.8, 0.15), "fqZAGA", conv("NaN/NA"))

expect_true(is.na(fdZAGA(NA, 2, 1, 0.1)), info = paste0(conv("NaN/NA"), "logical NA x is NA"))
expect_true(is.na(fpZAGA(NA_integer_, 2, 1, 0.1)), info = paste0(conv("NaN/NA"), "integer NA q is NA"))
expect_true(is.na(fqZAGA(NA, 2, 1, 0.1)), info = paste0(conv("NaN/NA"), "logical NA p is NA"))
for (f in list(fdZAGA, fpZAGA, fqZAGA)) {
    expect_true(all(is.na(f(c(NA, NaN), 2, 1, 0.1))), info = paste0(conv("NaN/NA"), "NA and NaN first argument"))
}
expect_true(is.na(fpZAGA(NaN, 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE)), info = paste0(conv("NaN/NA"), "NaN q, log upper tail"))
expect_true(is.na(fqZAGA(NaN, 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE)), info = paste0(conv("NaN/NA"), "NaN p, log upper tail"))
expect_true(is.na(fdZAGA(NaN, 2, 1, 0.1, log = TRUE)), info = paste0(conv("NaN/NA"), "NaN x, log density"))

# An element with a NaN is skipped by the checks even when another of its arguments is
# invalid (the SPEC: skipped by the checks and NA); an invalid element elsewhere is
# still reported.
expect_identical(is.na(fdZAGA(c(NaN, 1), c(1, 1), c(-1, 1), 0.1)), c(TRUE, FALSE),
                 info = paste0(conv("NaN/NA"), "fdZAGA: NaN x with sigma < 0 is NA, no error"))
expect_identical(is.na(fpZAGA(c(1, 1), c(NaN, 1), 1, c(2, 0.1))), c(TRUE, FALSE),
                 info = paste0(conv("NaN/NA"), "fpZAGA: NaN mu with nu > 1 is NA, no error"))
expect_identical(is.na(fqZAGA(c(2, 0.5), c(1, 1), c(NA, 1), 0.1)), c(TRUE, FALSE),
                 info = paste0(conv("NaN/NA"), "fqZAGA: NA sigma with p > 1 is NA, no error"))
expect_identical(err_msg(fdZAGA(c(NaN, 1), c(-1, -1), 1, 0.1)), "mu must be greater than 0",
                 info = paste0(conv("NaN/NA"), conv("error messages"), "an invalid element next to a NaN one still errors"))

# ----------------------------------------------------------------------------
# ZERO-LENGTH INPUT, RECYCLING
# ----------------------------------------------------------------------------

expect_identical(fdZAGA(numeric(0), 2, 1, 0.1), numeric(0), info = paste0(conv("zero-length"), "fdZAGA"))
expect_identical(fpZAGA(numeric(0), 2, 1, 0.1), numeric(0), info = paste0(conv("zero-length"), "fpZAGA"))
expect_identical(fqZAGA(numeric(0), 2, 1, 0.1), numeric(0), info = paste0(conv("zero-length"), "fqZAGA"))
expect_identical(fpZAGA(numeric(0), 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE), numeric(0),
                 info = paste0(conv("zero-length"), "fpZAGA, log upper tail"))
expect_identical(fdZAGA(numeric(0), numeric(0), numeric(0), numeric(0)), numeric(0),
                 info = paste0(conv("zero-length"), "all four arguments empty"))
expect_identical(fdZAGA(1:3, numeric(0), 1, 0.1), numeric(0), info = paste0(conv("zero-length"), "fdZAGA, empty mu"))
expect_identical(fqZAGA(c(0.2, 0.5), 2, 1, numeric(0)), numeric(0), info = paste0(conv("zero-length"), "fqZAGA, empty nu"))
expect_identical(fpZAGA(1, 2, numeric(0), 0.1), numeric(0), info = paste0(conv("zero-length"), "fpZAGA, empty sigma"))

# unequal lengths recycle to the longest, exactly as the explicitly rep()-ed call
x6 <- c(0, 0.4, 1, 2.5, 6, 30)
m2 <- c(1, 3)
s3 <- c(0.5, 1, 2)
expect_identical(fdZAGA(x6, m2, s3, 0.2), fdZAGA(x6, rep(m2, 3), rep(s3, 2), 0.2),
                 info = paste0(conv("recycling"), "fdZAGA, lengths 6, 2, 3, 1"))
expect_identical(fdZAGA(x6, m2, s3, 0.2, log = TRUE), fdZAGA(x6, rep(m2, 3), rep(s3, 2), 0.2, log = TRUE),
                 info = paste0(conv("recycling"), "fdZAGA log, lengths 6, 2, 3, 1"))
expect_identical(fpZAGA(x6, m2, s3, 0.2), fpZAGA(x6, rep(m2, 3), rep(s3, 2), 0.2),
                 info = paste0(conv("recycling"), "fpZAGA, lengths 6, 2, 3, 1"))
expect_identical(fpZAGA(x6, m2, s3, c(0.1, 0.3), lower_tail = FALSE, log_p = TRUE),
                 fpZAGA(x6, rep(m2, 3), rep(s3, 2), rep(c(0.1, 0.3), 3), lower_tail = FALSE, log_p = TRUE),
                 info = paste0(conv("recycling"), "fpZAGA log upper tail, lengths 6, 2, 3, 2"))
p6 <- c(0.05, 0.3, 0.5, 0.7, 0.9, 0.99)
expect_identical(fqZAGA(p6, m2, s3, 0.2), fqZAGA(p6, rep(m2, 3), rep(s3, 2), 0.2),
                 info = paste0(conv("recycling"), "fqZAGA, lengths 6, 2, 3, 1"))
expect_identical(fqZAGA(p6, m2, s3, c(0.1, 0.3), lower_tail = FALSE),
                 fqZAGA(p6, rep(m2, 3), rep(s3, 2), rep(c(0.1, 0.3), 3), lower_tail = FALSE),
                 info = paste0(conv("recycling"), "fqZAGA upper tail, lengths 6, 2, 3, 2"))
# a scalar first argument against vector parameters gives one value per parameter set
# (gamlss.dist returns length(x) for d and p)
expect_identical(length(fdZAGA(2, c(1, 2, 3), 1, 0.1)), 3L,
                 info = paste0(conv("recycling"), "scalar x with three mu gives three densities"))
expect_close(fdZAGA(2, c(1, 2, 3), 1, 0.1), fdZAGA(rep(2, 3), c(1, 2, 3), 1, 0.1),
             info = paste0(conv("recycling"), "scalar x against vector mu"))
expect_close(fpZAGA(2, 2, c(0.5, 1, 2), c(0.1, 0.2, 0.3)), fpZAGA(rep(2, 3), 2, c(0.5, 1, 2), c(0.1, 0.2, 0.3)),
             info = paste0(conv("recycling"), "scalar q against vector sigma and nu"))
expect_identical(length(fqZAGA(0.6, 2, 1, c(0.1, 0.2, 0.3, 0.4))), 4L,
                 info = paste0(conv("recycling"), "scalar p with four nu gives four quantiles"))
expect_silent(fdZAGA(x6, m2, s3, 0.2), info = paste0(conv("recycling"), "recycling is silent"))

# ----------------------------------------------------------------------------
# VALIDATION: every error with its exact message
#
# The parameters are checked after recycling; the checks are strict: mu > 0, sigma > 0,
# 0 < nu < 1, and p in [0, 1] (log_p: p <= 0). x and q have no range.
# ----------------------------------------------------------------------------

bad_cases <- list(
    list(arg = "mu", value = 0, msg = "mu must be greater than 0"),
    list(arg = "mu", value = -1, msg = "mu must be greater than 0"),
    list(arg = "sigma", value = 0, msg = "sigma must be greater than 0"),
    list(arg = "sigma", value = -0.5, msg = "sigma must be greater than 0"),
    list(arg = "nu", value = 0, msg = "nu must be between 0 and 1"),
    list(arg = "nu", value = -0.1, msg = "nu must be between 0 and 1"),
    list(arg = "nu", value = 1, msg = "nu must be between 0 and 1"),
    list(arg = "nu", value = 1.5, msg = "nu must be between 0 and 1")
)
good <- list(mu = 2, sigma = 1, nu = 0.1)
for (fn in c("fdZAGA", "fpZAGA", "fqZAGA")) {
    f <- get(fn)
    first <- if (fn == "fqZAGA") 0.5 else 1
    for (cs in bad_cases) {
        a <- good
        a[[cs$arg]] <- cs$value
        expect_identical(err_msg(do.call(f, c(list(first), a))), cs$msg,
                         info = paste0(dep("strict ranges"), conv("error messages"), fn, ": ", cs$arg, " = ", cs$value))
        # the invalid element is found when it is not the first, after recycling
        a[[cs$arg]] <- c(good[[cs$arg]], cs$value)
        expect_identical(err_msg(do.call(f, c(list(first), a))), cs$msg,
                         info = paste0(dep("strict ranges"), conv("error messages"), fn, ": ", cs$arg, " = ", cs$value, " as the second element"))
    }
}
# x and q have no range: below the support is a value, not an error
expect_identical(err_msg(fdZAGA(-5, 2, 1, 0.1)), NA_character_, info = "fdZAGA(x < 0) does not error")
expect_identical(err_msg(fpZAGA(-5, 2, 1, 0.1)), NA_character_, info = "fpZAGA(q < 0) does not error")
# parameters next to the edge of their range are valid
expect_identical(err_msg(fdZAGA(1, 1e-300, 1, 0.1)), NA_character_, info = "mu = 1e-300 is valid")
expect_identical(err_msg(fdZAGA(1, 1e300, 1, 0.1)), NA_character_, info = "mu = 1e300 is valid")
expect_identical(err_msg(fdZAGA(1, 1, 1e-3, 0.1)), NA_character_, info = "sigma = 1e-3 is valid")
expect_identical(err_msg(fdZAGA(1, 1, 1, 1e-15)), NA_character_, info = "nu = 1e-15 is valid")
expect_identical(err_msg(fdZAGA(1, 1, 1, 1 - 1e-15)), NA_character_, info = "nu = 1 - 1e-15 is valid")

# p
pm <- "p must be >=0 and <=1"
pl <- "p must be <=0 when log_p = TRUE (p is on the log scale)"
em <- function(what) paste0(conv("error messages"), what)
expect_identical(err_msg(fqZAGA(-0.1, 2, 1, 0.1)), pm, info = em("fqZAGA: p < 0"))
expect_identical(err_msg(fqZAGA(1.1, 2, 1, 0.1)), pm, info = em("fqZAGA: p > 1"))
expect_identical(err_msg(fqZAGA(c(0.5, 1.5), 2, 1, 0.1)), pm, info = em("fqZAGA: p > 1 as the second element"))
expect_identical(err_msg(fqZAGA(-1e-300, 2, 1, 0.1)), pm, info = em("fqZAGA: p just below 0"))
expect_identical(err_msg(fqZAGA(-0.1, 2, 1, 0.1, lower_tail = FALSE)), pm, info = em("fqZAGA upper tail: p < 0"))
expect_identical(err_msg(fqZAGA(1.1, 2, 1, 0.1, lower_tail = FALSE)), pm, info = em("fqZAGA upper tail: p > 1"))
expect_identical(err_msg(fqZAGA(0.1, 2, 1, 0.1, log_p = TRUE)), pl, info = em("fqZAGA: log_p with p > 0"))
expect_identical(err_msg(fqZAGA(1.5, 2, 1, 0.1, log_p = TRUE)), pl, info = em("fqZAGA: log_p with p > 1"))
expect_identical(err_msg(fqZAGA(c(-1, 1e-300), 2, 1, 0.1, log_p = TRUE)), pl,
                 info = em("fqZAGA: log_p with p just above 0"))
expect_identical(err_msg(fqZAGA(0.1, 2, 1, 0.1, lower_tail = FALSE, log_p = TRUE)), pl,
                 info = em("fqZAGA upper tail: log_p with p > 0"))
# on the log scale p = -0.5 is valid (p < 0 is the point of the log scale); p = 0 and
# p = 1 are valid on the plain scale, and log p = 0 and -Inf on the log scale
expect_identical(err_msg(fqZAGA(-0.5, 2, 1, 0.1, log_p = TRUE)), NA_character_, info = "log p = -0.5 is valid")
expect_identical(err_msg(fqZAGA(c(0, 0.5), 2, 1, 0.1)), NA_character_, info = "p = 0 is valid")
expect_identical(err_msg(fqZAGA(1, 2, 1, 0.1)), NA_character_,
                 info = paste0(dep("p = 1"), "p = 1 is valid"))
expect_identical(err_msg(fqZAGA(c(-Inf, -1), 2, 1, 0.1, log_p = TRUE)), NA_character_,
                 info = "log p = -Inf is valid")
expect_identical(err_msg(fqZAGA(0, 2, 1, 0.1, log_p = TRUE)), NA_character_,
                 info = paste0(dep("p = 1"), "log p = 0 is valid"))

# frZAGA
expect_error(frZAGA(0, 2, 1, 0.1), "n must be a positive integer", info = "frZAGA(0) errors")
expect_error(frZAGA(-3, 2, 1, 0.1), "n must be a positive integer", info = "frZAGA(-3) errors")
expect_error(frZAGA(c(5, 0), 2, 1, 0.1), "n must be a positive integer", info = "frZAGA(c(5, 0)) errors")
expect_identical(err_msg(frZAGA(3, mu = -1)), "mu must be greater than 0", info = em("frZAGA: mu < 0"))
expect_identical(err_msg(frZAGA(3, sigma = 0)), "sigma must be greater than 0", info = em("frZAGA: sigma = 0"))
expect_identical(err_msg(frZAGA(3, nu = 1)), "nu must be between 0 and 1",
                 info = paste0(dep("strict ranges"), em("frZAGA: nu = 1")))
expect_identical(err_msg(frZAGA(3, nu = 0)), "nu must be between 0 and 1",
                 info = paste0(dep("strict ranges"), em("frZAGA: nu = 0")))
expect_true(all(is.na(frZAGA(3, mu = NaN))), info = paste0(conv("NaN/NA"), "frZAGA: a NaN parameter gives NA"))

# ----------------------------------------------------------------------------
# ROUND TRIPS
# ----------------------------------------------------------------------------

set.seed(20260103)
nr <- 400
r_mu <- exp(runif(nr, log(0.5), log(5)))
r_sigma <- exp(runif(nr, log(0.3), log(2)))
r_nu <- runif(nr, 0.05, 0.5)

# F(Q(p)) = p for p > nu. The quantile inverts G = (p - nu) / (1 - nu) and the CDF
# adds nu back, so what the subtraction loses is restored: the round trip is accurate to
# a few ulp even for p just above nu (p is taken from 2% to 99% of the way up to 1).
r_p <- r_nu + runif(nr, 0.02, 0.99) * (1 - r_nu)
expect_close(fpZAGA(fqZAGA(r_p, r_mu, r_sigma, r_nu), r_mu, r_sigma, r_nu), r_p,
             info = "F(Q(p)) = p for p > nu")
r_pu <- (1 - r_nu) * runif(nr, 0.01, 0.98)     # upper-tail p, in the positive part (0, 1 - nu)
expect_close(fpZAGA(fqZAGA(r_pu, r_mu, r_sigma, r_nu, lower_tail = FALSE), r_mu, r_sigma, r_nu, lower_tail = FALSE),
             r_pu, info = "P(Y > Q_upper(p)) = p for 0 < p < 1 - nu")

# Q(F(x)) = x for x > 0 needs p - nu = (1 - nu) * G(x) to keep its digits. The
# subtraction cancels when G(x) is small (F(x) just above nu) or nu is near 1: each lost
# digit is a lost digit of x. Here nu <= 0.5 and x sits at G(x) in [0.1, 0.95], so at most
# about one digit is lost (the measured worst case is about 7e-15), and 1e-12 holds. In the far
# upper tail F rounds to 1 and Q(F(x)) is Inf; the upper-tail round trips above cover it.
r_x <- qgamma(runif(nr, 0.1, 0.95), 1 / r_sigma^2, scale = r_mu * r_sigma^2)
expect_close(fqZAGA(fpZAGA(r_x, r_mu, r_sigma, r_nu), r_mu, r_sigma, r_nu), r_x,
             info = "Q(F(x)) = x for x > 0, G(x) in [0.1, 0.95]")
expect_close(fqZAGA(fpZAGA(r_x, r_mu, r_sigma, r_nu, log_p = TRUE), r_mu, r_sigma, r_nu, log_p = TRUE), r_x,
             tol = 1e-11, info = "Q(log F(x)) = x for x > 0, G(x) in [0.1, 0.95]")
expect_close(fqZAGA(fpZAGA(r_x, r_mu, r_sigma, r_nu, lower_tail = FALSE), r_mu, r_sigma, r_nu, lower_tail = FALSE),
             r_x, info = "Q_upper(P(Y > x)) = x for x > 0")
expect_close(fqZAGA(fpZAGA(r_x, r_mu, r_sigma, r_nu, lower_tail = FALSE, log_p = TRUE),
                    r_mu, r_sigma, r_nu, lower_tail = FALSE, log_p = TRUE),
             r_x, info = "Q_upper(log P(Y > x)) = x for x > 0")

# ----------------------------------------------------------------------------
# frZAGA: random generation
# ----------------------------------------------------------------------------

expect_identical(length(frZAGA(7, 2, 1, 0.1)), 7L, info = "frZAGA(7) has length 7")
expect_identical(length(frZAGA(2.3, 2, 1, 0.1)), 3L, info = "a non-integer n is rounded up")
expect_identical(length(frZAGA(1)), 1L, info = "frZAGA(1) works with the default parameters")
r_few <- frZAGA(50, 2, 1, 0.1)
expect_true(all(is.finite(r_few) & r_few >= 0), info = "draws are finite and non-negative")

# the expression is evaluated after the seed is set
with_seed <- function(seed, expr) {
    dqrng::dqset.seed(seed)
    expr
}
expect_identical(with_seed(4242, frZAGA(500, mu = 3, sigma = 0.8, nu = 0.25)),
                 with_seed(4242, frZAGA(500, mu = 3, sigma = 0.8, nu = 0.25)),
                 info = paste0(conv("dqrng"), "frZAGA is reproducible under dqrng::dqset.seed"))
expect_true(!identical(with_seed(4242, frZAGA(500, mu = 3, sigma = 0.8, nu = 0.25)),
                       with_seed(4243, frZAGA(500, mu = 3, sigma = 0.8, nu = 0.25))),
            info = paste0(conv("dqrng"), "a different dqrng seed gives different draws"))
# frZAGA is the quantile function applied to dqrng uniforms
expect_identical(with_seed(99, frZAGA(500, mu = 3, sigma = 0.8, nu = 0.25)),
                 with_seed(99, fqZAGA(dqrng::dqrunif(500), mu = 3, sigma = 0.8, nu = 0.25)),
                 info = paste0(conv("dqrng"), "frZAGA(n) is fqZAGA(dqrng::dqrunif(n))"))
expect_identical(with_seed(100, frZAGA(100, mu = c(1, 5), sigma = c(0.5, 2), nu = c(0.1, 0.4))),
                 with_seed(100, fqZAGA(dqrng::dqrunif(100), mu = c(1, 5), sigma = c(0.5, 2), nu = c(0.1, 0.4))),
                 info = paste0(conv("dqrng"), conv("recycling"), "frZAGA recycles vector parameters like fqZAGA"))

# Moments at n = 2e5, within 5 standard errors (the seed is fixed, so this is
# deterministic; the margin only absorbs a different but valid generator):
#   P(Y = 0) = nu, E[Y] = (1 - nu) mu, Var[Y] = (1 - nu) mu^2 (sigma^2 + nu),
# and, among the positive draws, the mean is mu.
nn <- 2e5
mu_r <- 2; sigma_r <- 0.8; nu_r <- 0.25
dqrng::dqset.seed(20260104)
rr <- frZAGA(nn, mu = mu_r, sigma = sigma_r, nu = nu_r)
expect_identical(length(rr), as.integer(nn), info = "frZAGA(2e5) has the right length")
se_zero <- sqrt(nu_r * (1 - nu_r) / nn)
expect_true(abs(mean(rr == 0) - nu_r) < 5 * se_zero,
            info = sprintf("zero share %.5f vs nu %.2f (5 se = %.5f)", mean(rr == 0), nu_r, 5 * se_zero))
mean_true <- (1 - nu_r) * mu_r
se_mean <- sd(rr) / sqrt(nn)
expect_true(abs(mean(rr) - mean_true) < 5 * se_mean,
            info = sprintf("mean %.5f vs %.5f (5 se = %.5f)", mean(rr), mean_true, 5 * se_mean))
var_true <- (1 - nu_r) * mu_r^2 * (sigma_r^2 + nu_r)
m4 <- mean((rr - mean(rr))^4)
se_var <- sqrt((m4 - var(rr)^2) / nn)
expect_true(abs(var(rr) - var_true) < 5 * se_var,
            info = sprintf("variance %.5f vs %.5f (5 se = %.5f)", var(rr), var_true, 5 * se_var))
rp <- rr[rr > 0]
se_pos <- sd(rp) / sqrt(length(rp))
expect_true(abs(mean(rp) - mu_r) < 5 * se_pos,
            info = sprintf("mean of the positive draws %.5f vs mu %.2f (5 se = %.5f)", mean(rp), mu_r, 5 * se_pos))
sd_pos_true <- mu_r * sigma_r
# the sd of a gamma sample has a relative standard error of about 0.3% here
expect_true(abs(sd(rp) / sd_pos_true - 1) < 0.02, info = "sd of the positive draws is mu * sigma, within 2%")

# ----------------------------------------------------------------------------
# THE IMPACTncd USE CASE: zero-adjusted draws from stratified ranks
#
# The model computes minq = pfun(min_value) and maxq = pfun(max_value) and draws
# fq(clamp(minq + u * (maxq - minq))) for the stratified ranks u = (i - 0.5) / N.
# With minq = P(Y <= 0) = nu (min_value 0) no draw can be a zero; with min_value below
# the support, minq = 0 and a share nu / maxq of the ranks lies in the zero mass.
# (fpZINBI refuses a negative q, so a zero-inflated count cannot use min_value -1: its
# minq is P(Y <= 0) = nu + (1 - nu) f_NBI(0) and no simulant is ever a non-drinker.)
# ----------------------------------------------------------------------------

mu_i <- 60; sigma_i <- 1; nu_i <- 0.2
N <- 1e4
eps <- 1e-8
u_i <- (seq_len(N) - 0.5) / N
maxq <- fpZAGA(250, mu_i, sigma_i, nu_i)
expect_true(maxq < 0.99 && abs(nu_i / maxq - nu_i) > 10 / N,
            info = "design: maxq < 1 so that nu / maxq differs from nu by far more than 2 / N")
draw_ranks <- function(minq) {
    fqZAGA(pmin(pmax(minq + u_i * (maxq - minq), eps), 1 - eps), mu_i, sigma_i, nu_i)
}

minq_neg <- fpZAGA(-1, mu_i, sigma_i, nu_i)
expect_identical(minq_neg, 0, info = "minq = F(-1) is exactly 0")
d_neg <- draw_ranks(minq_neg)
expect_true(abs(mean(d_neg == 0) - nu_i / maxq) <= 2 / N,
            info = sprintf("zero share %.5f within 2/N of nu / maxq = %.5f", mean(d_neg == 0), nu_i / maxq))
expect_true(all(d_neg >= 0) && all(d_neg <= 250) && all(is.finite(d_neg)),
            info = "draws lie in [0, 250]")
expect_true(!is.unsorted(d_neg), info = "draws are non-decreasing in the rank")

minq_zero <- fpZAGA(0, mu_i, sigma_i, nu_i)
expect_identical(minq_zero, nu_i, info = "minq = F(0) is exactly nu")
d_zero <- draw_ranks(minq_zero)
expect_identical(sum(d_zero == 0), 0L, info = "with minq = F(0) no draw is a zero")
expect_true(all(d_zero > 0) && all(d_zero <= 250), info = "with minq = F(0) all draws are in (0, 250]")

# ----------------------------------------------------------------------------
# PARAMETER REGIMES
#
# Each regime is checked on d, log d, F, log F, the upper tails and the quantiles
# against the closed forms. tol_dp is the tolerance for the density and the CDF, tol_q
# for the quantiles.
# ----------------------------------------------------------------------------

check_regime <- function(label, mu, sigma, nu, x, tol_dp = 1e-12, tol_q = 1e-12, dens_tag = "") {
    i <- function(what) paste0(label, ": ", what)
    expect_silent(res <- fdZAGA(x, mu, sigma, nu))
    expect_close(res, ref_d(x, mu, sigma, nu), tol = tol_dp, info = paste0(dens_tag, i("density")))
    expect_close(fdZAGA(x, mu, sigma, nu, log = TRUE), ref_d(x, mu, sigma, nu, log = TRUE),
                 tol = tol_dp, info = paste0(dens_tag, i("log density")))
    expect_close(fpZAGA(x, mu, sigma, nu), ref_p(x, mu, sigma, nu), tol = tol_dp, info = i("CDF"))
    # absolute, as for the grid: log F next to F = 1 is ill-conditioned
    expect_close(fpZAGA(x, mu, sigma, nu, log_p = TRUE), ref_p(x, mu, sigma, nu, log_p = TRUE),
                 tol = tol_dp, relative = FALSE, info = i("log CDF"))
    expect_close(fpZAGA(x, mu, sigma, nu, lower_tail = FALSE),
                 ref_p(x, mu, sigma, nu, lower_tail = FALSE), tol = tol_dp,
                 info = paste0(dep("direct upper tail"), i("upper tail")))
    expect_close(fpZAGA(x, mu, sigma, nu, lower_tail = FALSE, log_p = TRUE),
                 ref_p(x, mu, sigma, nu, lower_tail = FALSE, log_p = TRUE), tol = tol_dp,
                 info = paste0(dep("direct upper tail"), i("log upper tail")))
    # probabilities from 5% to 95% of the way across the positive part
    pq <- nu + (1 - nu) * c(0.05, 0.3, 0.5, 0.8, 0.95)
    expect_silent(res <- fqZAGA(pq, mu, sigma, nu))
    expect_close(res, ref_q(pq, mu, sigma, nu), tol = tol_q, info = i("quantile"))
    expect_close(fqZAGA(log(pq), mu, sigma, nu, log_p = TRUE), ref_q(log(pq), mu, sigma, nu, log_p = TRUE),
                 tol = tol_q, info = i("log_p quantile"))
    pu <- (1 - nu) * c(0.05, 0.3, 0.5, 0.8, 0.95)
    expect_close(fqZAGA(pu, mu, sigma, nu, lower_tail = FALSE),
                 ref_q(pu, mu, sigma, nu, lower_tail = FALSE), tol = tol_q,
                 info = paste0(dep("upper-tail quantile"), i("upper-tail quantile")))
    expect_close(fqZAGA(log(pu), mu, sigma, nu, lower_tail = FALSE, log_p = TRUE),
                 ref_q(log(pu), mu, sigma, nu, lower_tail = FALSE, log_p = TRUE), tol = tol_q,
                 info = paste0(dep("upper-tail quantile"), i("log_p upper-tail quantile")))
}

# sigma = 1e-3: shape 1e6, a near-point mass at mu with sd mu * 1e-3. A one-ulp
# difference in how the shape or scale is formed moves the density and the CDF by about
# shape * eps = 1e-10 (measured 1.5e-12 across the points below), so 1e-10 here; the
# quantile is insensitive (3e-16). (gamlss.dist writes the log density as a sum of terms
# of order 1e7 here and loses about 9 digits to cancellation.)
check_regime("sigma = 1e-3", mu = 3, sigma = 1e-3, nu = 0.2,
             x = c(0, 3 * (1 + 1e-3 * c(-5, -2, -1, 0, 1, 2, 5)), 3.1),
             tol_dp = 1e-10, dens_tag = num("small-sigma density"))
# sigma = 20: shape 0.0025, almost all of the gamma mass sits against 0 (the density
# at 0+ is infinite) and the quantiles are tiny, many underflowing to 0. A quantile at
# probability G is about G^(1/shape) = G^400, so a one-ulp difference in G moves it by
# 400 ulp: 1e-11.
check_regime("sigma = 20", mu = 2, sigma = 20, nu = 0.2,
             x = c(0, 1e-300, 1e-100, 1e-10, 0.01, 1, 2, 100, 1e4), tol_q = 1e-11)
check_regime("mu = 1e-8", mu = 1e-8, sigma = 0.7, nu = 0.3,
             x = 1e-8 * c(0, 0.001, 0.1, 0.5, 1, 1.5, 3, 10, 50))
check_regime("mu = 1e8", mu = 1e8, sigma = 0.7, nu = 0.3,
             x = 1e8 * c(0, 0.001, 0.1, 0.5, 1, 1.5, 3, 10, 50))
check_regime("nu = 1e-12", mu = 2, sigma = 1.3, nu = 1e-12, x = c(0, 0.1, 1, 5, 20))
check_regime("nu = 0.999", mu = 2, sigma = 1.3, nu = 0.999, x = c(0, 0.1, 1, 5, 20))

# ----------------------------------------------------------------------------
# THE LOWER TAIL ON THE LOG SCALE NEAR F = 1
#
# log(F) is exactly 0 once F rounds to 1, and loses relative accuracy before that. Where
# F > 1/2, fpZAGA(log_p = TRUE) returns log1p(-U), U the direct upper tail
# (1 - nu) * pgamma(q, upper), and fqZAGA(log_p = TRUE) inverts on the upper scale from
# pu = -expm1(p) for p > -log(2) (exp(p) rounds to 1 for p above about -5.6e-17, -2^-54). Below
# that, and for the natural-scale lower tail, nothing changed: the natural-scale lower
# p and q are identical() to gamlss.dist::pZAGA / qZAGA (checked at the end).
# ----------------------------------------------------------------------------

lg <- function(what) paste0(dep("log lower tail F > 1/2"), what)

# mu = 2, sigma = 1, nu = 0.1: an exponential, U = 0.9 * exp(-q / 2) = 6.46e-66 at
# q = 300, where log(F) is 0
U300 <- 0.9 * pgamma(300, 1, scale = 2, lower.tail = FALSE)
expect_true(U300 > 0 && U300 < 1e-16, info = "design: U(300) is positive and far below 1 - F's resolution")
expect_identical(log(0.1 + 0.9 * pgamma(300, 1, scale = 2)), 0,
                 info = "design: log(F) of the plain route is 0 at q = 300")
expect_close(fpZAGA(300, 2, 1, 0.1, log_p = TRUE), log1p(-U300), tol = 1e-12,
             info = lg("log F(300) is log1p(-U), -6.46e-66 (it was 0)"))
expect_close(fpZAGA(300, 2, 1, 0.1, log_p = TRUE), log1p(-0.9 * exp(-150)), tol = 1e-12,
             info = lg("log F(300) against the exponential closed form"))
expect_true(fpZAGA(300, 2, 1, 0.1, log_p = TRUE) < 0, info = lg("log F(300) is negative, not 0"))

# a q-grid where F > 1/2, against log1p(-U) from three gamma shapes; the gamma quantile
# grid reaches an upper tail of 1e-250
pu_grid <- c(0.3, 0.1, 1e-3, 1e-6, 1e-9, 1e-12, 1e-15, 1e-18, 1e-30, 1e-100, 1e-250)
for (par in list(c(2, 1, 0.1), c(3, 0.5, 0.25), c(1, 2, 0.6), c(0.4, 0.3, 0.02))) {
    mu_f <- par[1]; sigma_f <- par[2]; nu_f <- par[3]
    q_f <- qgamma(pu_grid, 1 / sigma_f^2, scale = mu_f * sigma_f^2, lower.tail = FALSE)
    U_f <- (1 - nu_f) * pgamma(q_f, 1 / sigma_f^2, scale = mu_f * sigma_f^2, lower.tail = FALSE)
    expect_true(all(U_f < 0.5), info = "design: F > 1/2 on the whole grid")
    expect_close(fpZAGA(q_f, mu_f, sigma_f, nu_f, log_p = TRUE), log1p(-U_f), tol = 1e-13,
                 info = lg(sprintf("log F vs log1p(-U), mu %g sigma %g nu %g", mu_f, sigma_f, nu_f)))
}
# the exponential closed form, with no gamma function in the reference
q_e <- c(1.5, 2, 5, 10, 20, 40, 80, 150, 300, 700)
expect_close(fpZAGA(q_e, 2, 1, 0.1, log_p = TRUE), log1p(-0.9 * exp(-q_e / 2)), tol = 1e-13,
             info = lg("log F vs the exponential closed form"))
expect_true(all(fpZAGA(q_e, 2, 1, 0.1, log_p = TRUE) < 0), info = lg("log F < 0 on the whole grid"))
expect_true(all(diff(fpZAGA(q_e, 2, 1, 0.1, log_p = TRUE)) > 0), info = lg("log F increases with q"))
# F(0) = nu: log(nu) for nu < 1/2 (the log(F) branch) and, through log1p(-(1 - nu)), for
# nu > 1/2
expect_close(fpZAGA(0, c(2, 2, 2), 1, c(0.1, 0.5, 0.9), log_p = TRUE), log(c(0.1, 0.5, 0.9)), tol = 1e-15,
             info = lg("log F(0) is log(nu) on both sides of nu = 1/2"))

# continuity at F = 1/2 (U = 1/2), where the two routes meet: x* = 2 * log(1.8)
x_half <- 2 * log(1.8)
d_h <- c(-1e-6, -1e-9, -1e-12, 0, 1e-12, 1e-9, 1e-6)
expect_close(fpZAGA(x_half + d_h, 2, 1, 0.1, log_p = TRUE), log1p(-0.9 * exp(-(x_half + d_h) / 2)),
             tol = 1e-13, info = lg("log F is continuous across F = 1/2"))

# F <= 1/2 keeps log(F): identical to the log of the natural-scale lower tail
q_lo <- c(0, 0.1, 0.5, 1, 1.1)    # F(1.1) = 0.481 for mu = 2, sigma = 1, nu = 0.1
expect_true(all(fpZAGA(q_lo, 2, 1, 0.1) < 0.499), info = "design: these q have F below 1/2")
expect_identical(fpZAGA(q_lo, 2, 1, 0.1, log_p = TRUE), log(fpZAGA(q_lo, 2, 1, 0.1)),
                 info = "log F for F <= 1/2 is log() of the natural-scale lower tail")
low <- fpZAGA(g_x, g_mu, g_sigma, g_nu) < 0.49
expect_true(sum(low) > 10, info = "design: the random grid has points with F below 1/2")
expect_identical(fpZAGA(g_x[low], g_mu[low], g_sigma[low], g_nu[low], log_p = TRUE),
                 log(fpZAGA(g_x[low], g_mu[low], g_sigma[low], g_nu[low])),
                 info = "log F on the random grid, F below 1/2, is log() of the lower tail")
# on the whole random grid log F is within rounding of log(F) and, where F > 1/2, of log1p(-U)
U_g <- fpZAGA(g_x, g_mu, g_sigma, g_nu, lower_tail = FALSE)
hi <- U_g < 0.49
expect_close(fpZAGA(g_x[hi], g_mu[hi], g_sigma[hi], g_nu[hi], log_p = TRUE), log1p(-U_g[hi]),
             tol = 1e-13, info = lg("log F on the random grid, F > 1/2, is log1p(-U)"))
# q < 0, q = Inf and NaN keep their values
expect_identical(fpZAGA(c(-1, -1e-300, Inf, NaN), 2, 1, 0.1, log_p = TRUE),
                 c(-Inf, -Inf, 0, NA_real_), info = "log F at q < 0, Inf and NaN is as before")

# ----------------------------------------------------------------------------
# THE LOWER-TAIL QUANTILE ON THE LOG SCALE NEAR p = 0
# ----------------------------------------------------------------------------

# log1p(-1e-20) = -1e-20: exp() of it is exactly 1 and the old route gave Inf
for (par in list(c(2, 1, 0.1), c(3, 0.5, 0.25), c(1, 2, 0.6), c(0.4, 0.3, 0.02), c(50, 0.7, 0.9))) {
    mu_f <- par[1]; sigma_f <- par[2]; nu_f <- par[3]
    q20 <- fqZAGA(log1p(-1e-20), mu_f, sigma_f, nu_f, log_p = TRUE)
    expect_true(is.finite(q20) && q20 > 0,
                info = lg(sprintf("Q(log1p(-1e-20)) is finite, mu %g sigma %g nu %g", mu_f, sigma_f, nu_f)))
    expect_close(q20, qgamma(1e-20 / (1 - nu_f), 1 / sigma_f^2, scale = mu_f * sigma_f^2, lower.tail = FALSE),
                 tol = 1e-12,
                 info = lg(sprintf("Q(log1p(-1e-20)) vs the gamma upper-tail quantile, nu %g", nu_f)))
}

# the exponential closed form: pu = -expm1(p), Q = -mu * log(pu / (1 - nu))
p_e <- c(-1e-300, -1e-100, -1e-20, -1e-17, -1e-16, -1e-12, -1e-8, -1e-4, -0.01, -0.2, -0.5, -0.69)
expect_true(all(p_e > -log(2)), info = "design: the grid lies in the upper-scale region")
q_e2 <- fqZAGA(p_e, 2, 1, 0.1, log_p = TRUE)
expect_true(all(is.finite(q_e2)), info = lg("Q is finite on the whole grid (it was Inf for p > -5.6e-17)"))
expect_close(q_e2, -2 * log(-expm1(p_e) / 0.9), tol = 1e-12,
             info = lg("Q(log p) against the exponential closed form"))
expect_true(all(diff(q_e2) < 0), info = lg("Q decreases as p moves away from 0"))
# the same quantile from the upper tail, which never formed 1 - p
expect_close(q_e2, fqZAGA(-expm1(p_e), 2, 1, 0.1, lower_tail = FALSE), tol = 1e-12,
             info = lg("Q(log p) agrees with the natural-scale upper-tail quantile at -expm1(p)"))
# the pair is consistent where it used to be 0 and Inf: F(Q(p)) = p. The relative error of
# the round trip is about Q / mu times the rounding of Q (Q reaches 1381 here), hence 1e-10
expect_close(fpZAGA(q_e2, 2, 1, 0.1, log_p = TRUE), p_e, tol = 1e-10,
             info = lg("log F(Q(log p)) = log p near 0"))

expect_true(is.finite(fqZAGA(-1e-300, 2, 1, 0.1, log_p = TRUE)),
            info = lg("Q(log p = -1e-300) is finite (it was Inf)"))
expect_close(fqZAGA(-1e-300, 2, 1, 0.1, log_p = TRUE), -2 * log(1e-300 / 0.9), tol = 1e-12,
             info = lg("Q(log p = -1e-300) = -mu * log(1e-300 / (1 - nu))"))
expect_identical(fqZAGA(0, c(2, 0.5, 9), c(1, 0.5, 3), c(0.1, 0.4, 0.95), log_p = TRUE), rep(Inf, 3),
                 info = paste0(dep("p = 1"), "Q(log p = 0) is Inf"))
expect_identical(fqZAGA(-0, 2, 1, 0.1, log_p = TRUE), Inf, info = paste0(dep("p = 1"), "Q(log p = -0) is Inf"))

# the zero mass is kept on the upper-scale route: nu = 0.9 puts log(nu) = -0.105 in the
# region p > -log(2)
expect_identical(fqZAGA(c(-0.2, log(0.9) - 1e-9), 2, 1, 0.9, log_p = TRUE), c(0, 0),
                 info = lg("quantiles below log(nu) are 0 for nu > 1/2"))
# just above log(nu): Q is about 18 * (p - log(nu)) (the mass 0.9 over the density 0.05 at
# 0+); the offset is 1e-4 so that the representation error of p (1e-16) is far below the
# tolerance
q_nu <- fqZAGA(log(0.9) + 1e-4, 2, 1, 0.9, log_p = TRUE)
expect_true(q_nu > 0 && q_nu < 1e-2, info = lg("just above log(nu) the quantile is small and positive"))
expect_close(q_nu, -2 * log(-expm1(log(0.9) + 1e-4) / 0.1), tol = 1e-9,
             info = lg("just above log(nu) the quantile follows the exponential closed form"))

# continuity across p = -log(2), where the two routes meet; Q' = 2 here
d_c <- c(-1e-6, -1e-9, -1e-12, 0, 1e-12, 1e-9, 1e-6)
p_c <- -log(2) + d_c
q_c <- fqZAGA(p_c, 2, 1, 0.1, log_p = TRUE)
expect_close(q_c, -2 * log(-expm1(p_c) / 0.9), tol = 1e-12,
             info = lg("Q(log p) matches the closed form on both sides of -log(2)"))
expect_true(all(abs(diff(q_c) - 2 * diff(p_c)) < 1e-9) && all(diff(q_c) > 0),
            info = lg("Q(log p) is continuous and monotone across p = -log(2)"))
# (p = -log(2) itself takes the exp() route, the next double above takes the upper scale)
p_hi <- -log(2) + 2^-53
expect_true(p_hi > -log(2), info = "design: p_hi is above -log(2)")
expect_close(fqZAGA(c(-log(2), p_hi), 2, 1, 0.1, log_p = TRUE), rep(2 * log(1.8), 2), tol = 1e-12,
             info = lg("Q at -log(2) and the double above it agree"))

# on the log scale, both routes against the natural-scale quantile where both are well
# conditioned (p from 0.05 to 0.95; nu = 0.17 is not on the grid, so no p sits on the
# zero mass, where exp(log(p)) could round either side of nu)
pn <- seq(0.05, 0.95, by = 0.05)
expect_close(fqZAGA(log(pn), 3, 0.7, 0.17, log_p = TRUE), fqZAGA(pn, 3, 0.7, 0.17), tol = 1e-12,
             info = "log_p quantile vs natural scale across the switch at p = 1/2")

# ----------------------------------------------------------------------------
# THE NATURAL-SCALE LOWER TAIL IS BIT-IDENTICAL TO gamlss.dist
#
# fpZAGA(q) and fqZAGA(p) (lower_tail = TRUE, log_p = FALSE) are the IMPACTncd alcohol
# model's drop-in for pZAGA / qZAGA, so they must reproduce its values exactly. Scalar
# calls through mapply(), as above. q covers q < 0, q = 0, the body, and a far upper
# tail where G rounds to 1; p covers p = 0, p = nu (the zero mass) and the body.
# ----------------------------------------------------------------------------

if (requireNamespace("gamlss.dist", quietly = TRUE)) local({
    set.seed(20260103)
    n <- 600
    mu <- exp(runif(n, log(0.05), log(40)))
    sigma <- exp(runif(n, log(0.15), log(4)))
    nu <- runif(n, 0.01, 0.99)
    q <- qgamma(runif(n, 0.0005, 0.9995), 1 / sigma^2, scale = mu * sigma^2)
    q[1:40] <- 0
    q[41:80] <- -runif(40, 0, 5)
    q[81:100] <- 1e6
    p <- runif(n, 0.0005, 0.9995)
    p[1:30] <- nu[1:30]
    p[31:60] <- nu[31:60] * runif(30)
    p[61] <- 0
    ok_q <- mapply(function(q, m, s, v) gamlss.dist::pZAGA(q, m, s, v), q, mu, sigma, nu)
    ok_p <- mapply(function(p, m, s, v) gamlss.dist::qZAGA(p, m, s, v), p, mu, sigma, nu)
    expect_identical(fpZAGA(q, mu, sigma, nu), ok_q,
                     info = "natural-scale lower-tail CDF is identical() to gamlss.dist::pZAGA")
    expect_identical(fqZAGA(p, mu, sigma, nu), ok_p,
                     info = "natural-scale lower-tail quantile is identical() to gamlss.dist::qZAGA")
    expect_true(any(ok_p == 0) && any(ok_p > 0) && any(ok_q == 1) && any(ok_q == 0),
                info = "the identical() grid reaches the zero mass, the body and both ends of the CDF")
})

# ----------------------------------------------------------------------------
# EDGES OF THE KERNELS, REFINED
#
# Six refinements, each asserted below:
#  1. the upper-tail quantile on the log scale, p just below log(1 - nu) for a tiny nu
#  2. the zero mass on the log scale (log(nu) for nu > 1/2) and F(0) = nu exactly
#  3. the natural-scale density where mu * sigma^2 is tiny
#  4. a quantile is never negative
#  5. an infinite mu or sigma is an error
#  6. a NaN out of non-NaN inputs warns once per call
# ----------------------------------------------------------------------------

# the warnings an expression raises (muffled), with its value
warns_of <- function(expr) {
    msgs <- character()
    val <- withCallingHandlers(expr, warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    list(value = val, warnings = msgs)
}

# 1. fqZAGA(lower_tail = FALSE, log_p = TRUE) for p just below log(1 - nu) ------------

uq <- function(p, mu = 1, sigma = 0.05, nu = 1e-300) {
    fqZAGA(p, mu, sigma, nu, lower_tail = FALSE, log_p = TRUE)
}
nm1 <- num("upper log quantile")

# The references are a __float128 Newton inversion of the ZAGA upper tail (the gamma
# part's lower tail is about |p|, here 1e-200 to 6e-281), not this package's own code.
expect_close(uq(c(-1e-200, -1e-280)), c(0.1343177408651402, 0.080304767327987525), tol = 1e-12,
             info = paste0(nm1, "mu = 1, sigma = 0.05, nu = 1e-300 against the quad reference"))
p_pair <- c(-7.10334430369e-281, -5.96614663917e-281)
expect_close(uq(p_pair), c(0.080230162657415241, 0.080192135758497915), tol = 1e-12,
             info = paste0(nm1, "the pair that was non-monotone, against the quad reference"))
expect_true(all(diff(uq(p_pair)) < 0),
            info = paste0(nm1, "a larger p (a larger upper tail) gives a smaller quantile"))
# p one double below log(1 - nu) = -1e-300: the reduced log tail is subnormal (2e-316)
q_sub <- uq(-1.0000000000000002e-300, mu = 1e100, sigma = 0.5)
expect_true(is.finite(q_sub) && q_sub > 0, info = paste0(nm1, "mu = 1e100: finite, it was NaN"))
expect_close(q_sub, 6.27879720421535121408e20, tol = 1e-10,
             info = paste0(nm1, "mu = 1e100, sigma = 0.5: the quad reference 6.2787972e20"))

# against the gamma part's own lower-tail inversion, with r = p - log1p(-nu), above and
# below the switch at r = -log(2), over tiny and large nu; the third reference is the
# exponential (sigma = 1, Q = -mu * r), which uses no gamma function
nu_u <- c(1e-300, 1e-200, 1e-100, 1e-20, 1e-8, 0.01, 0.4, 0.9)
e_u <- c(0.2, 1, 2, 4, 8, 12)
for (nu_f in nu_u) {
    sg <- 0.3
    p_f <- log1p(-nu_f) - 10^-e_u
    r_f <- p_f - log1p(-nu_f)
    expect_true(all(r_f < 0 & r_f > -log(2)), info = "design: r lies above -log(2)")
    expect_close(uq(p_f, 2, sg, nu_f),
                 qgamma(log(-expm1(r_f)), 1 / sg^2, scale = 2 * sg^2, log.p = TRUE), tol = 1e-13,
                 info = paste0(nm1, "above the switch, against the gamma lower-tail inversion, nu ", nu_f))
    p_d <- log1p(-nu_f) - c(0.7, 1, 5, 30, 300)
    r_d <- p_d - log1p(-nu_f)
    expect_close(uq(p_d, 2, sg, nu_f),
                 qgamma(r_d, 1 / sg^2, scale = 2 * sg^2, lower.tail = FALSE, log.p = TRUE), tol = 1e-13,
                 info = paste0(nm1, "below the switch, against the gamma upper-tail inversion, nu ", nu_f))
    p_x <- log1p(-nu_f) - c(10^-e_u, 0.5, log(2), 0.8, 2, 10, 50)
    expect_close(uq(p_x, 2, 1, nu_f), -2 * (p_x - log1p(-nu_f)), tol = 1e-12,
                 info = paste0(nm1, "against the exponential closed form across the switch, nu ", nu_f))
}
# the monotone sweep over the whole range of |p| from 1e-2 to 1e-299, nu = 1e-300
p_s <- -10^-seq(2, 299, by = 3)
q_s <- uq(p_s)
expect_true(all(is.finite(q_s)) && all(q_s > 0) && all(diff(q_s) < 0),
            info = paste0(nm1, "finite, positive and decreasing in p over p = -1e-2 ... -1e-299"))
expect_close(fpZAGA(uq(p_pair), 1, 0.05, 1e-300, lower_tail = FALSE, log_p = TRUE), p_pair, tol = 1e-9,
             info = paste0(nm1, "the round trip through the log upper tail"))

# 2. the zero mass on the log scale, and F(0) = nu exactly ----------------------------

nz <- num("zero-mass edge")
set.seed(20261011)
nu_z <- c(runif(2000, 0.5, 1), 1 - 10^-(1:15), 0.5 + 10^-(1:12))
for (par in list(c(2, 0.7), c(0.05, 3), c(300, 0.1))) {
    expect_identical(fqZAGA(log(nu_z), par[1], par[2], nu_z, log_p = TRUE), numeric(length(nu_z)),
                     info = paste0(nz, "fqZAGA(log(nu), log_p = TRUE) is exactly 0 for nu > 1/2, mu ", par[1]))
}
nu_0 <- c(1e-300, 1e-100, 1e-10, 0.1, 0.3, 0.5, 0.7, 0.9, 1 - 1e-10)
for (sg in c(0.5, 1, 1e-170, 1e-200, 1e200)) {   # the last three: sigma^2 underflows or overflows
    w0 <- warns_of(list(fpZAGA(0, 2, sg, nu_0),
                        fpZAGA(0, 2, sg, nu_0, log_p = TRUE),
                        fpZAGA(0, 2, sg, nu_0, lower_tail = FALSE),
                        fpZAGA(0, 2, sg, nu_0, lower_tail = FALSE, log_p = TRUE)))
    expect_identical(w0$value[[1]], nu_0, info = paste0(nz, "F(0) = nu exactly, sigma ", sg))
    expect_identical(w0$value[[2]], log(nu_0), info = paste0(nz, "log F(0) = log(nu) exactly, sigma ", sg))
    expect_identical(w0$value[[3]], 1 - nu_0, info = paste0(nz, "P(Y > 0) = 1 - nu exactly, sigma ", sg))
    expect_identical(w0$value[[4]], log1p(-nu_0), info = paste0(nz, "log P(Y > 0) = log1p(-nu) exactly, sigma ", sg))
    expect_identical(w0$warnings, character(), info = paste0(nz, "no NaN, so no warning at q = 0, sigma ", sg))
}
expect_identical(fpZAGA(0, c(1, 7), 1e-200, 0.3), c(0.3, 0.3),
                 info = paste0(nz, "fpZAGA(0, mu, 1e-200, 0.3) is nu"))

# 3. the natural-scale density at a tiny scale mu * sigma^2 -----------------------------

nt <- num("tiny-scale density")
d_a <- fdZAGA(7.45e-18, 1e-20, 1, 0.3)
expect_close(d_a, exp(fdZAGA(7.45e-18, 1e-20, 1, 0.3, log = TRUE)), tol = 1e-10,
             info = paste0(nt, "x = 7.45e-18, mu = 1e-20: exp of the log density"))
expect_close(d_a, 1.9756455113303452e-304, tol = 1e-12,
             info = paste0(nt, "x = 7.45e-18, mu = 1e-20: the quad reference (it was 75 % too high)"))
d_b <- fdZAGA(1.004e-250, 1e-250, 1e-4, 0.3)
expect_close(d_b, exp(fdZAGA(1.004e-250, 1e-250, 1e-4, 0.3, log = TRUE)), tol = 1e-10,
             info = paste0(nt, "x = 1.004e-250, mu = 1e-250, sigma = 1e-4: exp of the log density"))
expect_close(d_b, 8.5588052603915857e-94, tol = 1e-9,
             info = paste0(nt, "x = 1.004e-250, mu = 1e-250, sigma = 1e-4: the quad reference (it was 0)"))
# the exponential (sigma = 1) with a scale of 1e-20, x / mu from 680 to 700, across the
# 1e-280 switch: f = (1 - nu) / mu * exp(-x / mu), and the density decreases through it
mu_t <- 1e-20
z_t <- seq(680, 700, by = 0.5)
d_t <- fdZAGA(z_t * mu_t, mu_t, 1, 0.3)
expect_true(any(d_t > 1e-280) && any(d_t < 1e-280), info = "design: the sweep crosses 1e-280")
expect_close(d_t, exp(log1p(-0.3) - log(mu_t) - z_t), tol = 1e-10,
             info = paste0(nt, "exponential density across the 1e-280 switch"))
expect_true(all(diff(d_t) < 0), info = paste0(nt, "the density decreases through the switch"))
# above the switch nothing changed: the product (1 - nu) * dgamma
set.seed(20261012)
n_t <- 500
mu_h <- exp(runif(n_t, log(0.05), log(50)))
sg_h <- exp(runif(n_t, log(0.15), log(4)))
nu_h <- runif(n_t, 0.01, 0.99)
x_h <- qgamma(runif(n_t, 0.001, 0.999), 1 / sg_h^2, scale = mu_h * sg_h^2)
expect_identical(fdZAGA(x_h, mu_h, sg_h, nu_h),
                 (1 - nu_h) * dgamma(x_h, 1 / sg_h^2, scale = mu_h * sg_h^2),
                 info = paste0(nt, "above 1e-280 the density is (1 - nu) * dgamma, bit for bit"))

# 4. a quantile is never negative --------------------------------------------------------

n4 <- conv("quantile >= 0")
for (cs in list(list(p = 0.4, mu = 1e10, sg = 1e-40, nu = 0.17, lt = FALSE, lp = FALSE),
                list(p = log(0.6), mu = 1e10, sg = 1e-40, nu = 0.17, lt = TRUE, lp = TRUE),
                list(p = 0.4, mu = 3.9847972202982205e+51, sg = 2.687813101924436e-88, nu = 0.16892664670012891,
                     lt = FALSE, lp = FALSE),
                list(p = 9.9769265646432601e-55, mu = 3.9847972202982205e+51, sg = 2.687813101924436e-88,
                     nu = 0.16892664670012891, lt = FALSE, lp = FALSE))) {
    w4 <- warns_of(fqZAGA(cs$p, cs$mu, cs$sg, cs$nu, lower_tail = cs$lt, log_p = cs$lp))
    expect_true(is.nan(w4$value),
                info = paste0(n4, "a negative qgamma value is NaN: p ", format(cs$p), " sigma ", cs$sg))
    expect_identical(w4$warnings, "NaNs produced",
                     info = paste0(n4, "...with one warning: p ", format(cs$p), " sigma ", cs$sg))
}
# a quantile is NaN or non-negative on the whole sweep of huge shapes, on all four tails
sg4 <- 10^-seq(20, 100, by = 4)
p4 <- c(0.3, 0.6, 0.9)
all4 <- suppressWarnings(c(
    as.vector(outer(p4, sg4, function(p, s) fqZAGA(p, 1e10, s, 0.17))),
    as.vector(outer(p4, sg4, function(p, s) fqZAGA(p, 1e10, s, 0.17, lower_tail = FALSE))),
    as.vector(outer(log(p4), sg4, function(p, s) fqZAGA(p, 1e10, s, 0.17, log_p = TRUE))),
    as.vector(outer(log(p4), sg4, function(p, s) fqZAGA(p, 1e10, s, 0.17, lower_tail = FALSE, log_p = TRUE)))))
expect_true(all(is.nan(all4) | all4 >= 0), info = paste0(n4, "no negative quantile on any tail, sigma 1e-20 to 1e-100"))
expect_true(any(is.nan(all4)) && any(all4 > 0), info = "design: the sweep reaches both the NaN and the finite region")

# 5. an infinite mu or sigma is an error -------------------------------------------------

n5 <- dep("strict ranges")
for (nm5 in c("fdZAGA", "fpZAGA", "fqZAGA")) {
    f5 <- get(nm5)
    a5 <- if (nm5 == "fqZAGA") 0.5 else 1
    expect_identical(err_msg(f5(a5, Inf, 1, 0.1)), "mu must be finite",
                     info = paste0(n5, nm5, ": mu = Inf is an error"))
    expect_identical(err_msg(f5(a5, 2, Inf, 0.1)), "sigma must be finite",
                     info = paste0(n5, nm5, ": sigma = Inf is an error"))
    expect_identical(err_msg(f5(a5, c(2, 2), c(1, Inf), 0.1)), "sigma must be finite",
                     info = paste0(n5, nm5, ": one infinite sigma among finite ones is an error"))
    expect_identical(err_msg(f5(a5, -Inf, 1, 0.1)), "mu must be greater than 0",
                     info = paste0(n5, conv("error messages"), nm5, ": mu = -Inf keeps its message"))
    expect_identical(err_msg(f5(a5, 2, -Inf, 0.1)), "sigma must be greater than 0",
                     info = paste0(n5, conv("error messages"), nm5, ": sigma = -Inf keeps its message"))
    expect_true(is.na(f5(a5, Inf, NA, 0.1)),
                info = paste0(n5, conv("NaN/NA"), nm5, ": an infinite mu next to a NA sigma is NA, no error"))
}

# 6. one "NaNs produced" warning per call, for a NaN out of non-NaN inputs ----------------

n6 <- conv("NaN warning")
# sigma = 1e-200: sigma^2 underflows to 0, the shape is infinite and the gamma part is NaN
for (cs in list(list(f = fdZAGA, a = c(1, 2, 3)),
                list(f = fpZAGA, a = c(1, 2, 3)),
                list(f = fqZAGA, a = c(0.5, 0.6, 0.7)))) {
    w6 <- warns_of(cs$f(cs$a, 2, 1e-200, 0.1))
    expect_true(all(is.nan(w6$value)), info = paste0(n6, "design: the three elements are NaN"))
    expect_identical(w6$warnings, "NaNs produced", info = paste0(n6, "one warning for three NaN elements"))
}
w6 <- warns_of(fdZAGA(c(0, 1, -1), 2, 1e-200, 0.1))
expect_identical(w6$value[c(1, 3)], c(0.1, 0), info = paste0(n6, "x = 0 and x < 0 are not NaN at sigma = 1e-200"))
expect_identical(w6$warnings, "NaNs produced", info = paste0(n6, "the one NaN element gives the warning"))
w6 <- warns_of(fpZAGA(c(0, -1, Inf), 2, 1e-200, 0.1))
expect_identical(w6$value, c(0.1, 0, 1), info = paste0(n6, "q = 0, q < 0, q = Inf are not NaN at sigma = 1e-200"))
expect_identical(w6$warnings, character(), info = paste0(n6, "no NaN element, no warning"))
# NA/NaN inputs stay silent, also next to a NaN produced by the others (that one warns)
for (f in list(fdZAGA, fpZAGA, fqZAGA)) {
    expect_identical(warns_of(f(c(NA, NaN), 2, 1, 0.1))$warnings, character(),
                     info = paste0(n6, "NA/NaN in the first argument is silent"))
    expect_identical(warns_of(f(0.5, NA, 1e-200, 0.1))$warnings, character(),
                     info = paste0(n6, "NA mu next to an underflowing sigma is silent"))
    expect_identical(warns_of(f(0.5, 2, 1, 0.1))$warnings, character(),
                     info = paste0(n6, "an ordinary call is silent"))
}
expect_identical(warns_of(fqZAGA(c(NA, 0.5), 2, 1e-200, 0.1))$warnings, "NaNs produced",
                 info = paste0(n6, "a NaN element beside an NA one still warns, once"))
dqrng::dqset.seed(20261013)
expect_identical(warns_of(frZAGA(5, 2, 1e-200, 0.1))$warnings, "NaNs produced",
                 info = paste0(n6, "frZAGA, through fqZAGA, warns once"))
# no warning from the paths this file already exercises, at the edges of x, q and p
expect_identical(warns_of(list(fdZAGA(c(-1, 0, Inf), 2, 1, 0.1), fpZAGA(c(-1, 0, Inf), 2, 1, 0.1),
                               fqZAGA(c(0, 0.1, 1), 2, 1, 0.1)))$warnings, character(),
                 info = paste0(n6, "x, q, p at -1, 0, Inf / 0, nu, 1 are silent"))
