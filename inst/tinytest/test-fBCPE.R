# Test the BCPE (Box-Cox power exponential) distribution functions:
# fdBCPE, fpBCPE, fqBCPE and frBCPE (and the BCPEo names, the same functions). Using the
# tinytest framework.
#
# BCPE is the Box-Cox power exponential of gamlss.dist (Rigby & Stasinopoulos 2004): mu > 0 the
# median, sigma > 0, nu real, tau > 0; support y > 0. With z = ((y/mu)^nu - 1) / (nu sigma)
# (log(y/mu) / sigma for nu = 0), Z is T truncated to (-b, Inf) for nu > 0 and (-Inf, b) for
# nu < 0, b = 1 / (sigma |nu|), where T is the power exponential with unit variance:
#   f_T(t) = tau / (c 2^(1 + 1/tau) Gamma(1/tau)) exp(-0.5 |t/c|^tau),
#   c = exp(0.5 (-(2/tau) log 2 + lgamma(1/tau) - lgamma(3/tau))).
# D = F_T(b) is the kept mass: F(y) = P(lo < T <= z) / D and f(y) = y^(nu-1) / (mu^nu sigma) f_T(z) / D.
#
# The references are not the package's code:
#   * tau = 2 (T is N(0, 1)): pnorm / qnorm / dnorm on both tails and with log.p, which are
#     accurate in the tails (measured: pnorm <= 7e-16, qnorm <= 8e-16 relative; R's pgamma(., 1/2)
#     is 1.2e-13 off at s = 685 and its qgamma 2e-10 off near upper p = 1e-14), and, for the mass
#     between z and the truncation point when it is a small difference, Gauss-Legendre quadrature
#     of dnorm (an entire integrand);
#   * tau = 1 (T is Laplace, c = 1 / (2 sqrt 2)): exact exponentials, including the between-mass
#     0.5 exp(-sqrt(2) t1) (-expm1(-sqrt(2) h)) = 0.5 exp(-sqrt(2) b) expm1(sqrt(2) h), h = b - t1,
#     and closed-form quantiles;
#   * mpmath (80 digits) values hard-coded below (mp_ref), and the oracle table bcpe_reference.csv;
#   * integrate() of the density against F for other tau;
#   * gamlss.dist itself where the package is documented to return gamlss.dist's own values,
#     identical() (the "gamlss region").
#
# Tolerances. The accuracy contract is 1e-13 relative (1e-13 * max(1, |value|) absolute on the log
# density) on the stable path, with gamlss.dist's own error (~2e-13) allowed inside the gamlss
# region, so the base tolerance is 1e-13 (5e-13 inside the region). A point whose value is ill-conditioned in
# its inputs gets more, never less: K = 16 ulp times its condition number, from the reference's own
# intermediates (ell = log(y/mu), the elasticity e = d log(mass) / d ell, s = 0.5 |z/c|^tau; see
# tol_mass(), tol_logd(), tol_quant()). A double-precision implementation cannot do better than
# that: the input y fixes ell only to eps * (1 + |ell|), and s to a few ulp.
#
# Departures from gamlss.dist (all deliberate, all asserted below). Every expectation that
# gamlss.dist cannot meet carries a tag in its info string: dep("...") for the departures,
# conv("...") for the CKutils conventions, num("...") for numerical accuracy that
# gamlss.dist's formulae do not reach.
#   dep("log density x <= 0")  the log density at x <= 0 is -Inf (gamlss.dist: 0)
#   dep("density at Inf")      the density at x = Inf is 0, log -Inf (gamlss.dist: NaN for nu >= 1)
#   dep("q <= 0 tails")        q <= 0 gives 0 / -Inf / 1 / 0 on the requested tail and scale
#                              (gamlss.dist: 0 for every tail and scale)
#   dep("direct upper tail")   the upper tail is computed directly, never as 1 - F
#   dep("log scales")          log F and log U are accurate at both ends: log F = log1p(-U) near
#                              F = 1, quantiles invert -expm1(log p) (gamlss.dist: log(F), 1 - F)
#   dep("far lower tail")      F is accurate below 1e-16 (gamlss.dist's 0.5 * (1 + G * sign(t)) is
#                              0 there: pBCPE(7, 25, .15, 0, 2) = 0 for 1.07e-17)
#   dep("nu near 0")           no cancellation for a tiny nu (gamlss.dist: nu = 1e-12 puts qBCPE
#                              6e-5 off; at nu = 1e-17 pBCPE is exactly 0.5 for q/mu from about
#                              exp(-5.55) to exp(11.1), 0 below that and 1 above, and
#                              qBCPE(1e-300, 25, .15, 1e-17, 2) = Inf, the truth 0.0965)
#   dep("power-law regime")    near the truncation point (y -> 0 for nu > 0, y -> Inf for nu < 0)
#                              the tail is a power law and is accurate, also far below the double
#                              range on the log scale (gamlss.dist subtracts two T-tails)
#   dep("strict ranges")       mu, sigma, tau > 0 and finite, nu finite (gamlss.dist accepts
#                              mu = sigma = tau = 0 and Inf); p outside [0, 1] is an error
#                              (gamlss.dist: NaN with a warning)
#   dep("qgamma polish")       far-tail quantiles are polished by Newton steps on pgamma: R's
#                              qgamma(., lower.tail = FALSE) is up to ~5e-10 off (relative, in s)
#                              for tails of about 1e-14 .. 3.5e-12 (shape 1/tau != 1)
#   conv("NaN/NA")             an NA/NaN argument gives NA_real_ for that element, silently
#                              (gamlss.dist stops for NA in mu, sigma or tau, and qBCPE(.5, nu = NA)
#                              is 0)
#   conv("NaN warning")        a NaN out of non-NaN inputs gives one "NaNs produced" warning per
#                              call (the design goal is that valid inputs never give NaN)
#   conv("error messages")     the wording of every error
#   conv("zero-length")        a zero-length argument gives a zero-length result
#   conv("recycling")          all arguments recycle to the longest
#   conv("dqrng")              frBCPE draws from dqrng: fqBCPE(dqrng::dqrunif(n), ...)
#   conv("quantile >= 0")      a quantile is in [0, Inf], never negative, never NaN
#   conv("BCPEo")              fdBCPEo / fpBCPEo / fqBCPEo / frBCPEo are identical() to the BCPE ones
#   conv("density at 0")       the density at x = 0 is 0 by convention (the right limit is +Inf for
#                              0 < nu < 1, f_T(-b) / (mu sigma D) for nu = 1, 0 otherwise)
#   num("tau extremes")        tau from 0.05 to 100 (c far from 1)
#   num("sigma extremes")      sigma from 1e-3 to 10
#   num("exp band")            an exp() argument of gamlss.dist below 2^-26: identical() to the last bit
#
# The gamlss region (identical() to gamlss.dist) is where gamlss.dist's own arithmetic is accurate:
# G-d (density), G-p (lower-tail CDF) and G-q'' (lower-tail quantile), with the thresholds below.
# All three also need gamlss.dist's intermediates (y/mu, c, s at the truncation point, s at z, the
# qgamma value) in the normal double range, and a CDF that is not above 1.

# ----------------------------------------------------------------------------
# HELPERS
# ----------------------------------------------------------------------------

dep <- function(what) paste0("[departure: ", what, "] ")
conv <- function(what) paste0("[convention: ", what, "] ")
num <- function(what) paste0("[numerics: ", what, "] ")

eps <- .Machine$double.eps
LN2 <- log(2)
K_ULP <- 16      # ulp per unit of condition number

# The gamlss region (see the header comment of inst/include/distr_BCPE.h, "The gamlss path"). Keep
# these in step with the BCPE_GAMLSS_* constexprs there.
G_NU_MIN <- 2^-8     # G1 (d, p): |nu| * min(1, sigma) >= G_NU_MIN, or nu == 0
G_DP_TAU_MIN <- 1    # G-d, G-p: tau >= G_DP_TAU_MIN (the cusp of f_T at 0 for tau < 1)
G_FT_MIN <- 2^-8     # G-p: g_z >= 0 or g_FT(g_z) >= G_FT_MIN
G_Q_NUW_MIN <- 2^-8  # G-q'': nu == 0, or |nu| * min(1, g_w) >= G_Q_NUW_MIN (g_w <= 0 fails)
G_Q_PT_MIN <- 2^-8   # G-q'': min(g_pt, 1 - g_pt) >= G_Q_PT_MIN * max(1, sigma / g_w)  (BCPE_Q_M_MIN)
G_Q_SIGMA_BAND <- TRUE    # the max(1, sigma / g_w) factor of G-q''

# Largest elementwise error of `current` against `target`, relative by default; any structural
# mismatch (length, NA pattern, position and sign of an infinity, 0 against non-zero) is Inf.
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

expect_close <- function(current, target, tol = 1e-13, info = "", relative = TRUE) {
    err <- max_err(current, target, relative)
    expect_true(isTRUE(err <= tol),
                info = sprintf("%s (max %s err. %.3g, tolerance %g)", info,
                               if (relative) "rel." else "abs.", err, tol))
}

# Elementwise check with a tolerance per element: |current - target| <= tol * scale, scale =
# |target| (relative) or 1 (absolute) or a given vector. Structural mismatches fail. The info
# string reports the worst element (index, values, tolerance).
expect_within <- function(current, target, tol, info = "", relative = TRUE, scale = NULL) {
    current <- as.numeric(current)
    target <- as.numeric(target)
    n <- length(target)
    tol <- rep_len(as.numeric(tol), n)
    r <- rep(0, n)
    if (length(current) != n) {
        r <- Inf
    } else {
        na_c <- is.na(current); na_t <- is.na(target)
        r[na_c != na_t] <- Inf
        ok <- !na_c & !na_t
        inf <- ok & (is.infinite(current) | is.infinite(target))
        r[inf] <- ifelse(current[inf] == target[inf], 0, Inf)
        fin <- ok & !inf
        sc <- if (!is.null(scale)) rep_len(scale, n)[fin] else if (relative) abs(target[fin]) else 1
        d <- abs(current[fin] - target[fin])
        rr <- d / (tol[fin] * sc)
        rr[d == 0] <- 0
        r[fin] <- rr
    }
    worst <- which.max(r)
    msg <- if (length(worst) && is.finite(max(r)) && max(r) <= 1) "" else if (length(current) != n) " (length mismatch)" else
        sprintf(" worst [%d]: got %.17g, want %.17g, tol %.3g", worst, current[worst], target[worst], tol[worst])
    expect_true(isTRUE(max(r) <= 1), info = sprintf("%s (max err/tol %.3g)%s", info, max(r), msg))
}

# the message of the error `expr` raises, NA when it raises none
err_msg <- function(expr) {
    tryCatch({
        expr
        NA_character_
    }, error = function(e) conditionMessage(e))
}

# the warnings an expression raises (muffled), with its value
warns_of <- function(expr) {
    msgs <- character()
    val <- withCallingHandlers(expr, warning = function(w) {
        msgs <<- c(msgs, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    list(value = val, warnings = msgs)
}

# NA that is NA_real_, not NaN (an element with an NA/NaN argument is NA_real_)
is_na_real <- function(x) is.na(x) & !is.nan(x)

# non-decreasing, allowing a 1e-13 relative wobble at a path switch (gamlss path to stable path)
mono_ok <- function(x) {
    x <- x[is.finite(x)]
    length(x) < 2L || all(diff(x) >= -1e-13 * pmax(abs(x[-1]), abs(x[-length(x)])))
}

# gamlss.dist's own intermediates, as R computes them: c = exp(log.c), s(t) = 0.5 * (|t/c|)^tau and
# F.T(t) = 0.5 * (1 + pgamma(s(t), 1/tau) * sign(t)) of its f.T / F.T / q.T
DBL_MIN <- .Machine$double.xmin
DBL_MAX <- .Machine$double.xmax
g_cfun <- function(tau) exp(0.5 * (-(2 / tau) * log(2) + lgamma(1 / tau) - lgamma(3 / tau)))
g_sfun <- function(t, tau) 0.5 * (abs(t / g_cfun(tau)))^tau
g_FTfun <- function(t, tau) 0.5 * (1 + pgamma(g_sfun(t, tau), 1 / tau) * sign(t))

# gamlss.dist's quantile intermediates (R semantics): the T target g_pt, the qgamma value g_s,
# g_za, g_w = nu sigma g_za + 1, c, and s at the truncation point
gamlss_q_parts <- function(p, sigma, nu, tau) {
    g_c <- g_cfun(tau)
    g_b <- 1 / (sigma * abs(nu))
    g_FTb <- g_FTfun(g_b, tau)
    g_pt <- ifelse(nu < 0, p * g_FTb, ifelse(nu == 0, p, 1 - (1 - p) * g_FTb))
    g_s <- suppressWarnings(qgamma((2 * g_pt - 1) * sign(g_pt - 0.5), 1 / tau))
    g_za <- sign(g_pt - 0.5) * (2 * g_s)^(1 / tau) * g_c
    list(pt = g_pt, za = g_za, w = nu * sigma * g_za + 1, s = g_s, c = g_c, s_b = g_sfun(g_b, tau))
}

# The gamlss region, from gamlss.dist's own intermediates (R semantics): the thresholds G_* and
# the conditions the header applies to gamlss's intermediates. `which`: "d" (density, G-d),
# "p" (natural lower-tail CDF, G-p), "q" (natural lower-tail quantile, G-q'').
#   d, p: y/mu, c and s at the truncation point (nu != 0) in the normal double range;
#   p:    also s at z (or z = 0) in the normal range, and gamlss's quotient (FYy1 - FYy2) / FYy3 not
#         above 1 (it rounds to 1 + 2^-52 for z far above the truncation point: 31 of 1e6 domain rows);
#   q:    c and s at the truncation point (nu != 0) in the normal range, and the qgamma value in the
#         normal range (or g_pt = 0.5).
in_gamlss_region <- function(arg, mu, sigma, nu, tau, which) {
    n <- max(length(arg), length(mu), length(sigma), length(nu), length(tau))
    arg <- rep_len(arg, n); mu <- rep_len(mu, n); sigma <- rep_len(sigma, n)
    nu <- rep_len(nu, n); tau <- rep_len(tau, n)
    g_c <- g_cfun(tau)
    c_ok <- g_c >= DBL_MIN & g_c <= DBL_MAX
    if (which == "q") {
        p <- arg
        g <- gamlss_q_parts(p, sigma, nu, tau)
        m <- pmin(g$pt, 1 - g$pt)
        band <- G_Q_PT_MIN * (if (G_Q_SIGMA_BAND) pmax(1, sigma / g$w) else 1)
        ok <- (nu == 0 | (g$w > 0 & abs(nu) * pmin(1, g$w) >= G_Q_NUW_MIN)) & p > 0 & p < 1 & m >= band &
            c_ok & (nu == 0 | g$s_b >= DBL_MIN) & (g$pt == 0.5 | g$s >= DBL_MIN)
        return(ok & !is.na(ok))
    }
    ratio <- arg / mu
    g_b <- 1 / (sigma * abs(nu))
    G1 <- (nu == 0 | abs(nu) * pmin(1, sigma) >= G_NU_MIN) & tau >= G_DP_TAU_MIN &
        arg > 0 & is.finite(arg) & ratio >= DBL_MIN & ratio <= DBL_MAX & c_ok &
        (nu == 0 | suppressWarnings(g_sfun(g_b, tau)) >= DBL_MIN)
    if (which == "d") return(G1 & !is.na(G1))
    g_z <- suppressWarnings(ifelse(nu != 0, ((ratio)^nu - 1) / (nu * sigma), log(ratio) / sigma))
    FYy1 <- suppressWarnings(g_FTfun(g_z, tau))
    FYy2 <- suppressWarnings(ifelse(nu > 0, g_FTfun(-g_b, tau), 0))
    FYy3 <- suppressWarnings(g_FTfun(g_b, tau))
    ok <- G1 & (g_z == 0 | suppressWarnings(g_sfun(g_z, tau)) >= DBL_MIN) &
        (g_z >= 0 | FYy1 >= G_FT_MIN) & ifelse(nu > 0, FYy1 >= 2 * FYy2, TRUE) &
        (FYy1 - FYy2) / FYy3 <= 1
    ok & !is.na(ok)
}
# the relative error gamlss.dist's quantile carries inside its region: za from qgamma of 1 - 2m has a
# relative error ~eps/m (m = min(g_pt, 1 - g_pt)), which y inherits times sigma |za| / w; the
# rounding of w adds eps / |nu|
gamlss_q_err <- function(p, sigma, nu, tau) {
    n <- max(length(p), length(sigma), length(nu), length(tau))
    p <- rep_len(p, n); sigma <- rep_len(sigma, n); nu <- rep_len(nu, n); tau <- rep_len(tau, n)
    g <- gamlss_q_parts(p, sigma, nu, tau)
    m <- pmin(g$pt, 1 - g$pt)
    ifelse(nu == 0, eps * (1 + sigma * abs(g$za)) / m, eps * (sigma * abs(g$za) / (m * g$w) + 1 / abs(nu)))
}

# ----------------------------------------------------------------------------
# CLOSED-FORM REFERENCES, tau = 2 (normal) and tau = 1 (Laplace)
# ----------------------------------------------------------------------------

# Gauss-Legendre nodes and weights on [0, 1] (Newton on the Legendre recurrence)
gauleg01 <- function(n) {
    m <- (n + 1) %/% 2
    x <- w <- numeric(n)
    for (i in seq_len(m)) {
        z <- cos(pi * (i - 0.25) / (n + 0.5))
        for (it in 1:100) {
            p1 <- 1; p2 <- 0
            for (j in seq_len(n)) { p3 <- p2; p2 <- p1; p1 <- ((2 * j - 1) * z * p2 - (j - 1) * p3) / j }
            pp <- n * (z * p1 - p2) / (z * z - 1)
            z1 <- z
            z <- z1 - p1 / pp
            if (abs(z - z1) < 1e-16) break
        }
        p1 <- 1; p2 <- 0
        for (j in seq_len(n)) { p3 <- p2; p2 <- p1; p1 <- ((2 * j - 1) * z * p2 - (j - 1) * p3) / j }
        pp <- n * (z * p1 - p2) / (z * z - 1)
        x[i] <- -z; x[n + 1 - i] <- z
        w[i] <- w[n + 1 - i] <- 2 / ((1 - z * z) * pp * pp)
    }
    list(x = (x + 1) / 2, w = w / 2)
}
GL24 <- gauleg01(24)

log1mexp <- function(x) if (x <= LN2) log(-expm1(-x)) else log1p(-exp(-x))   # log(1 - e^-x), x > 0
logadd <- function(a, b) { m <- max(a, b); if (m == -Inf) -Inf else m + log1p(exp(min(a, b) - m)) }
# log(log1p(exp(X)))
llog1pexp <- function(X) {
    if (X < -30) X - exp(X) / 2 else if (X > 36) log(X + log1p(exp(-X))) else log(log1p(exp(X)))
}

# T for tau = 2: tail(t) = P(T > t), cen(t) = P(0 < T < t) (t >= 0), lfT = log f_T,
# s = 0.5 t^2, dlfT = |d log f_T / dt|
T2 <- list(
    tau = 2,
    tail = function(t) pnorm(t, lower.tail = FALSE),
    ltail = function(t) pnorm(t, lower.tail = FALSE, log.p = TRUE),
    # pnorm(t) - 0.5 cancels for a small t: the series of erf there
    cen = function(t) {
        if (t < 0.5) {
            k <- 0:20
            sum((-1)^k * t^(2 * k + 1) / (2^k * factorial(k) * (2 * k + 1))) / sqrt(2 * pi)
        } else pnorm(t) - 0.5
    },
    lfT = function(t) dnorm(t, log = TRUE),
    s = function(t) t * t / 2,
    dlfT = function(t) abs(t),
    # P(t1 < T < b), h = b - t1 given as log h: a difference of tails where it loses at most a
    # bit, else quadrature of dnorm(b - u) = dnorm(b) exp(b u - u^2 / 2) over u in [0, h]
    between = function(t1, b, lh) {
        lU1 <- pnorm(t1, lower.tail = FALSE, log.p = TRUE)
        lU2 <- pnorm(b, lower.tail = FALSE, log.p = TRUE)
        if (lU2 <= lU1 - LN2) {
            lg <- if (lU2 == -Inf) lU1 else lU1 + log1mexp(lU1 - lU2)
            nat <- pnorm(t1, lower.tail = FALSE) - pnorm(b, lower.tail = FALSE)
            if (nat < 1e-290) nat <- exp(lg)
            return(c(nat, lg))
        }
        c1 <- T2$cen(t1); c2 <- T2$cen(b)
        if (c1 <= c2 / 2) return(c(c2 - c1, log(c2 - c1)))
        h <- exp(lh)
        I <- sum(GL24$w * exp(b * h * GL24$x - (h * GL24$x)^2 / 2))
        lg <- dnorm(b, log = TRUE) + lh + log(I)
        c(exp(lg), lg)
    }
)
# T for tau = 1 (Laplace): s = sqrt(2) |t|, every mass an exponential
R2 <- sqrt(2)
T1 <- list(
    tau = 1,
    tail = function(t) 0.5 * exp(-R2 * t),
    ltail = function(t) -LN2 - R2 * t,
    cen = function(t) -0.5 * expm1(-R2 * t),
    lfT = function(t) -0.5 * LN2 - R2 * abs(t),
    s = function(t) R2 * abs(t),
    dlfT = function(t) R2,
    # = 0.5 exp(-sqrt2 t1) (-expm1(-sqrt2 h)) = 0.5 exp(-sqrt2 b) expm1(sqrt2 h), both exact; the
    # first where h is not small (b may be Inf), the second near the truncation point
    between = function(t1, b, lh) {
        h <- exp(lh)
        if (R2 * h > 1) {
            lg <- -LN2 - R2 * t1 + log(-expm1(-R2 * h))
            nat <- if (lg > -700) 0.5 * exp(-R2 * t1) * (-expm1(-R2 * h)) else exp(lg)
            return(c(nat, lg))
        }
        lx <- log(R2) + lh
        lem1 <- if (lx < -30) lx + exp(lx) / 2 else log(expm1(exp(lx)))
        lg <- -LN2 - R2 * b + lem1
        nat <- if (lg > -700 && lh > -700) 0.5 * exp(-R2 * b) * expm1(R2 * h) else exp(lg)
        c(nat, lg)
    }
)

# Box-Cox and the masses at one point, from the definition (sums, never a small difference of large
# masses). Returns ell, z, F, U, log F, log U, the log density, and the conditioning quantities.
cf_one <- function(y, mu, sigma, nu, tau) {
    T <- if (tau == 2) T2 else if (tau == 1) T1 else stop("closed forms exist for tau = 1 and 2 only")
    rr <- y / mu
    # y - mu is exact for 0.5 <= y/mu <= 2 (Sterbenz): ell keeps its relative accuracy near y = mu
    ell <- if (is.finite(rr) && rr >= 0.5 && rr <= 2) log1p((y - mu) / mu)
           else if (is.finite(rr) && rr >= .Machine$double.xmin) log(rr) else log(y) - log(mu)
    x <- nu * ell
    z <- (ell / sigma) * (if (x == 0) 1 else expm1(x) / x)
    az <- abs(z)
    if (nu == 0) {
        D <- 1; lD <- 0
        if (z <= 0) { ML <- T$tail(az); lML <- T$ltail(az); MU <- 0.5 + T$cen(az); lMU <- log(MU) }
        else { ML <- 0.5 + T$cen(az); lML <- log(ML); MU <- T$tail(az); lMU <- T$ltail(az) }
    } else {
        b <- 1 / (sigma * abs(nu))
        Lb <- T$tail(b)
        D <- 1 - Lb; lD <- log1p(-Lb)
        lh <- x + log(b)          # log of the gap h = w * b between z and the truncation point
        h <- exp(lh)
        if (nu > 0) {
            if (z >= 0) { ML <- T$cen(b) + T$cen(az); lML <- log(ML); MU <- T$tail(az); lMU <- T$ltail(az) }
            else {
                bm <- T$between(if (h < b / 2) b - h else az, b, lh)
                ML <- bm[1]; lML <- bm[2]; MU <- 0.5 + T$cen(az); lMU <- log(MU)
            }
        } else {
            if (z <= 0) { ML <- T$tail(az); lML <- T$ltail(az); MU <- T$cen(az) + T$cen(b); lMU <- log(MU) }
            else {
                ML <- 0.5 + T$cen(az); lML <- log(ML)
                bm <- T$between(if (h < b / 2) b - h else az, b, lh)
                MU <- bm[1]; lMU <- bm[2]
            }
        }
    }
    F <- ML / D; U <- MU / D
    lF <- if (F > 0.5) log1p(-U) else lML - lD
    lU <- if (U > 0.5) log1p(-F) else lMU - lD
    lfT <- T$lfT(z)
    lyf <- lfT - lD + x - log(sigma)          # log(y f(y)) = log f_T(z) - log D + log w - log sigma
    w <- exp(x)
    c(ell = ell, z = z, w = w, F = F, U = U, lF = lF, lU = lU, lf = lyf - log(y),
      eF = exp(lyf - lML + lD), eU = exp(lyf - lMU + lD), sz = T$s(az), dlfT = T$dlfT(az),
      dlogf = abs(nu - 1 - (if (z == 0) 0 else sign(z) * T$dlfT(az) * w / sigma)),
      lterms = abs(log(mu)) + abs(ell) + abs(x) + abs(log(sigma)) + abs(lfT) + abs(lD))
}
cf <- function(y, mu, sigma, nu, tau) {
    n <- max(length(y), length(mu), length(sigma), length(nu), length(tau))
    m <- mapply(cf_one, rep_len(y, n), rep_len(mu, n), rep_len(sigma, n), rep_len(nu, n), rep_len(tau, n))
    as.data.frame(t(m))
}

# Closed-form quantile (tau = 1 always; tau = 2 away from the truncation point, else NA): the
# smaller tail mass is the target.
cf_q_one <- function(p, mu, sigma, nu, tau, lower = TRUE, logp = FALSE) {
    T <- if (tau == 2) T2 else T1
    if (logp) {
        if (p <= -LN2) { sideL <- lower; lp <- p } else { sideL <- !lower; lp <- log(-expm1(p)) }
    } else {
        if (p <= 0.5) { sideL <- lower; lp <- log(p) } else { sideL <- !lower; lp <- log1p(-p) }
    }
    if (nu == 0) {
        lm <- lp
        z <- if (tau == 2) qnorm(lm, lower.tail = sideL, log.p = TRUE)
             else if (sideL) (lm + LN2) / R2 else -(lm + LN2) / R2
        return(mu * exp(sigma * z))
    }
    b <- 1 / (sigma * abs(nu))
    lm <- lp + log1p(-T$tail(b))
    lcb <- log(T$cen(b))
    z <- NA_real_; lw <- NA_real_
    between_side <- (nu > 0 && sideL) || (nu < 0 && !sideL)
    if (between_side && lm <= lcb) {
        # mass between z and the truncation point
        if (tau == 1) {
            if (lm > T1$ltail(b) + 40) {
                # the tail beyond b is below e^-40 of the target: a plain tail (also for b = Inf)
                az <- -(lm + LN2) / R2
                z <- if (nu > 0) -az else az
            } else {
                lh <- llog1pexp(lm + LN2 + R2 * b) - log(R2)
                lw <- lh - log(b)
            }
        } else {
            z <- if (nu > 0) qnorm(logadd(pnorm(-b, log.p = TRUE), lm), log.p = TRUE)
                 else qnorm(logadd(pnorm(b, lower.tail = FALSE, log.p = TRUE), lm), lower.tail = FALSE, log.p = TRUE)
            if (1 + nu * sigma * z < 0.5) return(NA_real_)    # too close to the truncation point
        }
    } else if (between_side) {
        # beyond the centre: P(0 < T < |z|) = m - P(0 < T < b), for tau = 1
        m <- exp(lm)
        if (tau == 1) {
            az <- -log1p(-2 * (m - T$cen(b))) / R2
            z <- if (nu > 0) az else -az
        } else {
            z <- if (nu > 0) qnorm(logadd(pnorm(-b, log.p = TRUE), lm), log.p = TRUE)
                 else qnorm(logadd(pnorm(b, lower.tail = FALSE, log.p = TRUE), lm), lower.tail = FALSE, log.p = TRUE)
        }
    } else {
        # the untruncated side: a plain T tail
        z <- if (tau == 2) qnorm(lm, lower.tail = sideL, log.p = TRUE)
             else if (sideL) (lm + LN2) / R2 else -(lm + LN2) / R2
    }
    if (is.na(lw)) lw <- log1p(nu * sigma * z)
    ell <- lw / nu
    if (abs(ell) < 700) mu * exp(ell) else exp(log(mu) + ell)
}
cf_q <- function(p, mu, sigma, nu, tau, lower = TRUE, logp = FALSE) {
    n <- max(length(p), length(mu), length(sigma), length(nu), length(tau))
    mapply(cf_q_one, rep_len(p, n), rep_len(mu, n), rep_len(sigma, n), rep_len(nu, n), rep_len(tau, n),
           MoreArgs = list(lower = lower, logp = logp))
}

# Tolerances (see the header). `e` the elasticity of the mass in ell, `R` a cf() frame.
tol_mass <- function(e, R, tau, base = 1e-13) base + K_ULP * eps * (e * (1 + abs(R$ell)) + (1 + tau) * R$sz)
# absolute tolerance of the log density
tol_logd <- function(R, tau, base = 1e-13) {
    base * pmax(1, abs(R$lf)) + K_ULP * eps * (R$lterms + (1 + tau) * R$sz + R$dlogf * (1 + abs(R$ell)))
}
# the abs. error of gamlss.dist's z = ((y/mu)^nu - 1) / (nu sigma), from the rounding of (y/mu)^nu
gz_err <- function(R, nu, sigma) ifelse(nu == 0, 0, eps * R$w / (abs(nu) * sigma))
# quantile: relative tolerance; `r` = (input-side mass) / (smaller mass) for a natural p given on the
# larger side, `lm` the log of the smaller mass and `e` its elasticity
tol_quant <- function(R, e, lm, r, tau, base = 1e-13) {
    base + K_ULP * eps * (1 + abs(R$ell) + (r + abs(lm)) / e + 1 / tau)
}

# ----------------------------------------------------------------------------
# INTERFACE: argument names, defaults, return type
# ----------------------------------------------------------------------------

expect_identical(names(formals(fdBCPE)), c("x", "mu", "sigma", "nu", "tau", "log_"), info = "fdBCPE arguments")
expect_identical(formals(fdBCPE)$log_, FALSE, info = "fdBCPE log_ defaults to FALSE")
expect_identical(names(formals(fpBCPE)), c("q", "mu", "sigma", "nu", "tau", "lower_tail", "log_p"),
                 info = "fpBCPE arguments")
expect_identical(names(formals(fqBCPE)), c("p", "mu", "sigma", "nu", "tau", "lower_tail", "log_p"),
                 info = "fqBCPE arguments")
expect_identical(c(formals(fpBCPE)$lower_tail, formals(fpBCPE)$log_p), c(TRUE, FALSE),
                 info = "fpBCPE defaults: lower_tail = TRUE, log_p = FALSE")
expect_identical(c(formals(fqBCPE)$lower_tail, formals(fqBCPE)$log_p), c(TRUE, FALSE),
                 info = "fqBCPE defaults: lower_tail = TRUE, log_p = FALSE")
expect_identical(names(formals(frBCPE)), c("n", "mu", "sigma", "nu", "tau"), info = "frBCPE arguments")
expect_identical(unlist(formals(frBCPE)[c("mu", "sigma", "nu", "tau")]),
                 c(mu = 5, sigma = 0.1, nu = 1, tau = 2), info = "frBCPE defaults (those of gamlss.dist::rBCPE)")
for (nm in c("fdBCPE", "fpBCPE", "fqBCPE", "frBCPE")) {
    expect_identical(formals(get(paste0(nm, "o"))), formals(get(nm)),
                     info = paste0(conv("BCPEo"), nm, "o has the arguments of ", nm))
}

expect_true(is.double(fdBCPE(20, 25, 0.15, 0.5, 2)), info = "fdBCPE returns a double")
expect_true(is.double(fpBCPE(20, 25, 0.15, 0.5, 2)), info = "fpBCPE returns a double")
expect_true(is.double(fqBCPE(0.5, 25, 0.15, 0.5, 2)), info = "fqBCPE returns a double")
expect_true(is.double(fqBCPE(0, 25, 0.15, 0.5, 2)), info = "fqBCPE returns a double at p = 0")
expect_true(is.double(frBCPE(3)), info = "frBCPE returns a double")
expect_true(fqBCPE(0.3, 25, 0.15, 0.5, 2) %% 1 != 0, info = "the quantile keeps its fractional part")
expect_identical(fdBCPE(20L, 25L, 0.15, 1L, 2L), fdBCPE(20, 25, 0.15, 1, 2),
                 info = "integer x, mu, nu and tau give the same density as doubles")
expect_identical(fpBCPE(20L, 25L, 0.15, 1L, 2L), fpBCPE(20, 25, 0.15, 1, 2),
                 info = "integer q, mu, nu and tau give the same CDF as doubles")
expect_identical(fqBCPE(0.3, 25L, 0.15, -1L, 2L), fqBCPE(0.3, 25, 0.15, -1, 2),
                 info = "integer mu, nu and tau give the same quantile as doubles")

# ----------------------------------------------------------------------------
# EDGES, on all four tail/scale combinations, for nu > 0, nu < 0, nu = 0 and several tau
# ----------------------------------------------------------------------------

edge_pars <- list(c(25, 0.15, 1.2, 2), c(25, 0.15, -0.7, 2), c(25, 0.15, 0, 2), c(3, 0.6, 2.5, 0.5),
                  c(1e-3, 0.05, -3, 8), c(40, 2, 0, 1), c(1e5, 0.01, 1e-300, 1.5), c(7, 0.3, -1e-12, 30))
qle0 <- c(0, -0, -1e-300, -1, -1e300, -Inf)
for (pr in edge_pars) {
    lab <- sprintf("mu %g sigma %g nu %g tau %g", pr[1], pr[2], pr[3], pr[4])
    f4 <- function(q, lt, lp) fpBCPE(q, pr[1], pr[2], pr[3], pr[4], lower_tail = lt, log_p = lp)
    expect_identical(f4(qle0, TRUE, FALSE), rep(0, 6), info = paste("F(q <= 0) = 0,", lab))
    expect_identical(f4(qle0, TRUE, TRUE), rep(-Inf, 6), info = paste0(dep("q <= 0 tails"), "log F(q <= 0) = -Inf, ", lab))
    expect_identical(f4(qle0, FALSE, FALSE), rep(1, 6), info = paste0(dep("q <= 0 tails"), "P(Y > q) = 1 for q <= 0, ", lab))
    expect_identical(f4(qle0, FALSE, TRUE), rep(0, 6), info = paste0(dep("q <= 0 tails"), "log P(Y > q) = 0 for q <= 0, ", lab))
    expect_identical(c(f4(Inf, TRUE, FALSE), f4(Inf, TRUE, TRUE), f4(Inf, FALSE, FALSE), f4(Inf, FALSE, TRUE)),
                     c(1, 0, 0, -Inf), info = paste("q = Inf: F = 1, log F = 0, U = 0, log U = -Inf,", lab))
    xle0 <- c(0, -1e-300, -2, -Inf)
    expect_identical(fdBCPE(xle0, pr[1], pr[2], pr[3], pr[4]), rep(0, 4), info = paste("f(x <= 0) = 0,", lab))
    expect_identical(fdBCPE(xle0, pr[1], pr[2], pr[3], pr[4], log_ = TRUE), rep(-Inf, 4),
                     info = paste0(dep("log density x <= 0"), "log f(x <= 0) = -Inf, ", lab))
    expect_identical(fdBCPE(Inf, pr[1], pr[2], pr[3], pr[4]), 0, info = paste0(dep("density at Inf"), "f(Inf) = 0, ", lab))
    expect_identical(fdBCPE(Inf, pr[1], pr[2], pr[3], pr[4], log_ = TRUE), -Inf,
                     info = paste0(dep("density at Inf"), "log f(Inf) = -Inf, ", lab))
    q4 <- function(p, lt, lp) fqBCPE(p, pr[1], pr[2], pr[3], pr[4], lower_tail = lt, log_p = lp)
    expect_identical(q4(c(0, 1), TRUE, FALSE), c(0, Inf), info = paste("Q(0) = 0, Q(1) = Inf,", lab))
    expect_identical(q4(c(-Inf, 0, -0), TRUE, TRUE), c(0, Inf, Inf), info = paste("log_p: Q(-Inf) = 0, Q(0) = Inf,", lab))
    expect_identical(q4(c(0, 1), FALSE, FALSE), c(Inf, 0), info = paste("upper tail: Q(0) = Inf, Q(1) = 0,", lab))
    expect_identical(q4(c(-Inf, 0, -0), FALSE, TRUE), c(Inf, 0, 0), info = paste("upper tail, log_p: Q(-Inf) = Inf, Q(0) = 0,", lab))
    expect_identical(warns_of(list(f4(c(qle0, Inf), TRUE, TRUE), q4(c(0, 1), FALSE, FALSE),
                                   fdBCPE(c(xle0, Inf), pr[1], pr[2], pr[3], pr[4], log_ = TRUE)))$warnings,
                     character(), info = paste0(conv("NaN warning"), "the edges are silent, ", lab))
}
# a mixed vector with parameters that differ per element keeps the element-wise rule
expect_identical(fpBCPE(c(-1, 0, 20, Inf), c(25, 20, 30, 25), 0.15, c(1, -1, 0, 2), c(2, 1.5, 3, 2), lower_tail = FALSE),
                 c(1, 1, fpBCPE(20, 30, 0.15, 0, 3, lower_tail = FALSE), 0),
                 info = paste0(dep("q <= 0 tails"), "q <= 0 and q = Inf inside a mixed vector"))

# the density at x = 0 is the convention 0; just above 0 it follows the power law y^(nu - 1)
R0 <- cf(c(1e-300, 1e-100, 1e-10), 25, 0.15, 0.5, 2)
expect_within(fdBCPE(c(1e-300, 1e-100, 1e-10), 25, 0.15, 0.5, 2), exp(R0$lf), tol_logd(R0, 2),
              info = paste0(conv("density at 0"), "f(1e-300) = 1.3e111 for nu = 0.5 (the right limit at 0 is +Inf)"))
expect_identical(fdBCPE(0, 25, 0.15, c(0.5, 1, 2, -1, 0), 2), rep(0, 5), info = paste0(conv("density at 0"), "f(0) = 0 for every nu"))

# ----------------------------------------------------------------------------
# VALIDATION: every error with its exact message (after recycling)
# ----------------------------------------------------------------------------

bad_cases <- list(
    list(arg = "mu", value = 0, msg = "mu must be positive"),
    list(arg = "mu", value = -1, msg = "mu must be positive"),
    list(arg = "mu", value = -Inf, msg = "mu must be positive"),
    list(arg = "mu", value = Inf, msg = "mu must be finite"),
    list(arg = "sigma", value = 0, msg = "sigma must be positive"),
    list(arg = "sigma", value = -0.5, msg = "sigma must be positive"),
    list(arg = "sigma", value = -Inf, msg = "sigma must be positive"),
    list(arg = "sigma", value = Inf, msg = "sigma must be finite"),
    list(arg = "nu", value = Inf, msg = "nu must be finite"),
    list(arg = "nu", value = -Inf, msg = "nu must be finite"),
    list(arg = "tau", value = 0, msg = "tau must be positive"),
    list(arg = "tau", value = -2, msg = "tau must be positive"),
    list(arg = "tau", value = -Inf, msg = "tau must be positive"),
    list(arg = "tau", value = Inf, msg = "tau must be finite")
)
good <- list(mu = 25, sigma = 0.15, nu = 0.5, tau = 2)
for (fn in c("fdBCPE", "fpBCPE", "fqBCPE", "fdBCPEo", "fpBCPEo", "fqBCPEo")) {
    f <- get(fn)
    first <- if (grepl("fq", fn)) 0.5 else 20
    for (cs in bad_cases) {
        a <- good
        a[[cs$arg]] <- cs$value
        expect_identical(err_msg(do.call(f, c(list(first), a))), cs$msg,
                         info = paste0(dep("strict ranges"), conv("error messages"), fn, ": ", cs$arg, " = ", cs$value))
        a[[cs$arg]] <- c(good[[cs$arg]], cs$value)       # the invalid element second, after recycling
        expect_identical(err_msg(do.call(f, c(list(c(first, first, first)), a))), cs$msg,
                         info = paste0(dep("strict ranges"), conv("error messages"), fn, ": ", cs$arg, " = ", cs$value,
                                       " as the second element"))
    }
}
# within one element, the positivity checks come before the finiteness checks
expect_identical(err_msg(fdBCPE(20, Inf, -1, 0.5, 2)), "sigma must be positive",
                 info = paste0(conv("error messages"), "mu = Inf with sigma < 0: the positivity check first"))
expect_identical(err_msg(fpBCPE(20, 25, Inf, 0.5, 0)), "tau must be positive",
                 info = paste0(conv("error messages"), "sigma = Inf with tau = 0: the positivity check first"))
# x and q have no range; nu has no range
expect_identical(err_msg(fdBCPE(c(-5, -Inf, Inf, 0), 25, 0.15, 0.5, 2)), NA_character_, info = "fdBCPE(x <= 0 or Inf) does not error")
expect_identical(err_msg(fpBCPE(c(-5, -Inf, Inf, 0), 25, 0.15, 0.5, 2, lower_tail = FALSE)), NA_character_,
                 info = "fpBCPE(q <= 0 or Inf) does not error")
expect_identical(err_msg(fpBCPE(20, 25, 0.15, c(-1e300, -10, 0, 1e-300, 10, 1e300), 2)), NA_character_, info = "nu has no range")
# parameters at the edge of their range are valid
expect_identical(err_msg(fdBCPE(1, c(1e-300, 1e300), c(1e-300, 1e300), 1, c(1e-300, 1e300))), NA_character_,
                 info = "mu, sigma, tau at 1e-300 and 1e300 are valid")

pm <- "p must be between 0 and 1"
pl <- "p must be <=0 when log_p = TRUE (p is on the log scale)"
em <- function(what) paste0(conv("error messages"), what)
for (fn in c("fqBCPE", "fqBCPEo")) {
    f <- get(fn)
    expect_identical(err_msg(f(-0.1, 25, 0.15, 0.5, 2)), pm, info = paste0(dep("strict ranges"), em(paste(fn, ": p < 0"))))
    expect_identical(err_msg(f(1.1, 25, 0.15, 0.5, 2)), pm, info = paste0(dep("strict ranges"), em(paste(fn, ": p > 1"))))
    expect_identical(err_msg(f(c(0.5, 1 + 2^-52), 25, 0.15, 0.5, 2)), pm,
                     info = paste0(dep("strict ranges"), em(paste(fn, ": p just above 1, second element"))))
    expect_identical(err_msg(f(-1e-300, 25, 0.15, 0.5, 2)), pm, info = paste0(dep("strict ranges"), em(paste(fn, ": p just below 0"))))
    expect_identical(err_msg(f(c(0.2, -0.1), 25, 0.15, 0.5, 2, lower_tail = FALSE)), pm,
                     info = paste0(dep("strict ranges"), em(paste(fn, "upper tail: p < 0"))))
    expect_identical(err_msg(f(0.1, 25, 0.15, 0.5, 2, log_p = TRUE)), pl, info = em(paste(fn, ": log_p with p > 0")))
    expect_identical(err_msg(f(c(-1, 1e-300), 25, 0.15, 0.5, 2, log_p = TRUE)), pl, info = em(paste(fn, ": log_p with p just above 0")))
    expect_identical(err_msg(f(0.1, 25, 0.15, 0.5, 2, lower_tail = FALSE, log_p = TRUE)), pl,
                     info = em(paste(fn, "upper tail: log_p with p > 0")))
    expect_identical(err_msg(f(c(0, 1, 0.5), 25, 0.15, 0.5, 2)), NA_character_, info = paste(fn, ": p = 0 and p = 1 are valid"))
    expect_identical(err_msg(f(c(-Inf, -1e5, -0.5, 0), 25, 0.15, 0.5, 2, log_p = TRUE)), NA_character_,
                     info = paste(fn, ": log p = -Inf, -1e5, -0.5, 0 are valid"))
}
expect_error(frBCPE(0), "n must be a positive integer", info = "frBCPE(0) errors")
expect_error(frBCPE(-3), "n must be a positive integer", info = "frBCPE(-3) errors")
expect_error(frBCPE(c(5, 0)), "n must be a positive integer", info = "frBCPE(c(5, 0)) errors")
expect_identical(err_msg(frBCPE(3, mu = -1)), "mu must be positive", info = em("frBCPE: mu < 0"))
expect_identical(err_msg(frBCPE(3, sigma = 0)), "sigma must be positive", info = em("frBCPE: sigma = 0"))
expect_identical(err_msg(frBCPE(3, tau = 0)), "tau must be positive", info = paste0(dep("strict ranges"), em("frBCPE: tau = 0")))
expect_identical(err_msg(frBCPE(3, nu = Inf)), "nu must be finite", info = paste0(dep("strict ranges"), em("frBCPE: nu = Inf")))

# ----------------------------------------------------------------------------
# NaN / NA HANDLING: an element with an NA/NaN argument is NA_real_, silently, and is skipped by
# the checks; the other elements are computed as usual
# ----------------------------------------------------------------------------

na_checks <- function(f, args, label) {
    ref <- do.call(f, args)
    for (k in seq_along(args)) {
        for (bad in list(NA_real_, NaN, NA)) {
            a <- lapply(args, rep_len, 3L)
            a[[k]][2] <- bad
            w <- warns_of(res <- do.call(f, a))
            expect_true(length(res) == 3L && is_na_real(res[2]),
                        info = paste0(conv("NaN/NA"), label, ": ", format(bad), " in argument ", k, " gives NA_real_"))
            expect_identical(res[c(1, 3)], ref[c(1, 3)],
                             info = paste0(conv("NaN/NA"), label, ": ", format(bad), " in argument ", k, ", other elements intact"))
            expect_identical(w$warnings, character(),
                             info = paste0(conv("NaN/NA"), conv("NaN warning"), label, ": ", format(bad), " in argument ", k, " is silent"))
        }
    }
}
na_checks(fdBCPE, list(c(18, 25, 33), 25, 0.15, 0.5, 2), "fdBCPE")
na_checks(function(...) fdBCPE(..., log_ = TRUE), list(c(18, 25, 33), 25, 0.15, -0.5, 1.5), "fdBCPE log")
na_checks(fpBCPE, list(c(18, 25, 33), 25, 0.15, 0.5, 2), "fpBCPE")
na_checks(function(...) fpBCPE(..., lower_tail = FALSE, log_p = TRUE), list(c(18, 25, 33), 25, 0.15, 0, 3), "fpBCPE log upper")
na_checks(fqBCPE, list(c(0.1, 0.5, 0.9), 25, 0.15, 0.5, 2), "fqBCPE")
na_checks(function(...) fqBCPE(..., lower_tail = FALSE, log_p = TRUE), list(log(c(0.1, 0.5, 0.9)), 25, 0.15, -1, 2.5),
          "fqBCPE log upper")
# an NA element is skipped by the checks even when another of its arguments is invalid; an invalid
# element elsewhere is still reported
expect_identical(is.na(fdBCPE(c(NaN, 20), 25, c(-1, 0.15), 0.5, 2)), c(TRUE, FALSE),
                 info = paste0(conv("NaN/NA"), "fdBCPE: NaN x with sigma < 0 is NA, no error"))
expect_identical(is.na(fpBCPE(c(20, 20), c(NA, 25), 0.15, 0.5, c(0, 2))), c(TRUE, FALSE),
                 info = paste0(conv("NaN/NA"), "fpBCPE: NA mu with tau = 0 is NA, no error"))
expect_identical(is.na(fqBCPE(c(2, 0.5), 25, c(NA, 0.15), 0.5, 2)), c(TRUE, FALSE),
                 info = paste0(conv("NaN/NA"), "fqBCPE: NA sigma with p > 1 is NA, no error"))
expect_identical(is.na(fqBCPE(c(NaN, 0.5), 25, 0.15, c(Inf, 0.5), 2)), c(TRUE, FALSE),
                 info = paste0(conv("NaN/NA"), "fqBCPE: NaN p with nu = Inf is NA, no error"))
expect_identical(err_msg(fdBCPE(c(NaN, 20), c(-1, -1), 0.15, 0.5, 2)), "mu must be positive",
                 info = paste0(conv("NaN/NA"), conv("error messages"), "an invalid element next to a NaN one still errors"))
expect_true(is_na_real(fdBCPE(NA, 25, 0.15, 0.5, 2)), info = paste0(conv("NaN/NA"), "logical NA x is NA_real_"))
expect_true(is_na_real(fpBCPE(NA_integer_, 25, 0.15, 0.5, 2)), info = paste0(conv("NaN/NA"), "integer NA q is NA_real_"))
expect_true(all(is_na_real(frBCPE(3, mu = NaN))), info = paste0(conv("NaN/NA"), "frBCPE: a NaN parameter gives NA"))

# ----------------------------------------------------------------------------
# ZERO-LENGTH INPUT, RECYCLING
# ----------------------------------------------------------------------------

expect_identical(fdBCPE(numeric(0), 25, 0.15, 0.5, 2), numeric(0), info = paste0(conv("zero-length"), "fdBCPE"))
expect_identical(fpBCPE(numeric(0), 25, 0.15, 0.5, 2), numeric(0), info = paste0(conv("zero-length"), "fpBCPE"))
expect_identical(fqBCPE(numeric(0), 25, 0.15, 0.5, 2), numeric(0), info = paste0(conv("zero-length"), "fqBCPE"))
expect_identical(fpBCPE(numeric(0), 25, 0.15, 0.5, 2, lower_tail = FALSE, log_p = TRUE), numeric(0),
                 info = paste0(conv("zero-length"), "fpBCPE, log upper tail"))
expect_identical(fdBCPE(1:3, numeric(0), 0.15, 0.5, 2), numeric(0), info = paste0(conv("zero-length"), "fdBCPE, empty mu"))
expect_identical(fpBCPE(1:3, 25, numeric(0), 0.5, 2), numeric(0), info = paste0(conv("zero-length"), "fpBCPE, empty sigma"))
expect_identical(fqBCPE(c(0.2, 0.5), 25, 0.15, numeric(0), 2), numeric(0), info = paste0(conv("zero-length"), "fqBCPE, empty nu"))
expect_identical(fdBCPE(1, 25, 0.15, 0.5, numeric(0), log_ = TRUE), numeric(0), info = paste0(conv("zero-length"), "fdBCPE, empty tau"))
expect_identical(fdBCPE(numeric(0), -1, 0.15, 0.5, 2), numeric(0), info = paste0(conv("zero-length"), "an empty call is not validated"))

x6 <- c(5, 12, 20, 25, 40, 90)
m2 <- c(20, 30)
s3 <- c(0.1, 0.2, 0.3)
n2 <- c(-0.8, 1.3)
t3 <- c(1.5, 2, 4)
expect_identical(fdBCPE(x6, m2, s3, n2, t3), fdBCPE(x6, rep(m2, 3), rep(s3, 2), rep(n2, 3), rep(t3, 2)),
                 info = paste0(conv("recycling"), "fdBCPE, lengths 6, 2, 3, 2, 3"))
expect_identical(fdBCPE(x6, m2, s3, n2, t3, log_ = TRUE), fdBCPE(x6, rep(m2, 3), rep(s3, 2), rep(n2, 3), rep(t3, 2), log_ = TRUE),
                 info = paste0(conv("recycling"), "fdBCPE log"))
for (lt in c(TRUE, FALSE)) for (lp in c(TRUE, FALSE)) {
    expect_identical(fpBCPE(x6, m2, s3, n2, t3, lt, lp), fpBCPE(x6, rep(m2, 3), rep(s3, 2), rep(n2, 3), rep(t3, 2), lt, lp),
                     info = paste0(conv("recycling"), "fpBCPE lower_tail ", lt, " log_p ", lp))
    p6 <- c(0.01, 0.2, 0.5, 0.7, 0.9, 0.999)
    if (lp) p6 <- log(p6)
    expect_identical(fqBCPE(p6, m2, s3, n2, t3, lt, lp), fqBCPE(p6, rep(m2, 3), rep(s3, 2), rep(n2, 3), rep(t3, 2), lt, lp),
                     info = paste0(conv("recycling"), "fqBCPE lower_tail ", lt, " log_p ", lp))
}
expect_identical(length(fdBCPE(20, c(15, 20, 25), 0.15, 0.5, 2)), 3L,
                 info = paste0(conv("recycling"), "a scalar x with three mu gives three densities"))
expect_identical(fpBCPE(20, 25, 0.15, c(-1, 0, 1, 2), 2), fpBCPE(rep(20, 4), 25, 0.15, c(-1, 0, 1, 2), 2),
                 info = paste0(conv("recycling"), "a scalar q against a vector nu"))
expect_identical(length(fqBCPE(0.6, 25, 0.15, 0.5, c(1, 2, 3, 4, 5))), 5L,
                 info = paste0(conv("recycling"), "a scalar p with five tau gives five quantiles"))
expect_silent(fdBCPE(x6, m2, s3, n2, t3), info = paste0(conv("recycling"), "recycling is silent"))

# ----------------------------------------------------------------------------
# RANDOM GRIDS AGAINST THE CLOSED FORMS (tau = 2 and tau = 1)
#
# mu log-uniform on [1e-6, 1e6], sigma on [1e-3, 10]; nu uniform on (-3, 3), large (3 to 10),
# tiny (|nu| = 10^-(2..300)) or 0. Each point is either in the body or a tail (z = +-10^(-4..zmax),
# out to masses of about 1e-300) or, for nu != 0, near the truncation point (w = (y/mu)^nu =
# 10^-(0.3..120): the power-law regime). The references use the double y actually tested.
# ----------------------------------------------------------------------------

make_grid <- function(n, tau, seed) {
    set.seed(seed)
    mu <- exp(runif(n, log(1e-6), log(1e6)))
    sigma <- exp(runif(n, log(1e-3), log(10)))
    kind <- sample(4, n, replace = TRUE, prob = c(0.55, 0.1, 0.2, 0.15))
    sgn <- sample(c(-1, 1), n, replace = TRUE)
    nu <- ifelse(kind == 1, runif(n, -3, 3), ifelse(kind == 2, sgn * runif(n, 3, 10),
                 ifelse(kind == 3, sgn * 10^-runif(n, 2, 300), 0)))
    zmax <- if (tau == 2) 37 else 480
    z <- sample(c(-1, 1), n, replace = TRUE) * 10^runif(n, -4, log10(zmax))
    regime <- nu != 0 & runif(n) < 0.3
    lw <- ifelse(regime, -runif(n, 0.3, 120) * log(10), suppressWarnings(log1p(nu * sigma * z)))
    ell <- ifelse(nu == 0, sigma * z, lw / nu)
    y <- mu * exp(ell)
    ok <- is.finite(ell) & is.finite(y) & y > 1e-300 & y < 1e300
    data.frame(y = y[ok], mu = mu[ok], sigma = sigma[ok], nu = nu[ok], tau = tau, regime = regime[ok])
}

check_grid <- function(G, label) {
    R <- cf(G$y, G$mu, G$sigma, G$nu, G$tau)
    keep <- is.finite(R$lF) & is.finite(R$lU) & R$lF < 0 & R$lU < 0 & is.finite(R$lf)
    G <- G[keep, ]; R <- R[keep, ]
    i <- function(what) paste0(label, ": ", what)
    expect_true(nrow(G) > 300 && sum(G$regime) > 40 && sum(G$nu == 0) > 20 && sum(R$F < 1e-100) > 10 &&
                    sum(R$U < 1e-100) > 10, info = i("design: the grid covers the body, both far tails and the regime"))
    regd <- in_gamlss_region(G$y, G$mu, G$sigma, G$nu, G$tau, "d")
    regp <- in_gamlss_region(G$y, G$mu, G$sigma, G$nu, G$tau, "p")
    # density
    tl <- tol_logd(R, G$tau, base = ifelse(regd, 5e-13, 1e-13)) +
        ifelse(regd, K_ULP * R$dlfT * gz_err(R, G$nu, G$sigma), 0)
    expect_silent(d_nat <- fdBCPE(G$y, G$mu, G$sigma, G$nu, G$tau))
    nat <- exp(R$lf) > 1e-290
    expect_within(d_nat[nat], exp(R$lf[nat]), tl[nat] + 4 * eps * abs(R$lf[nat]), info = i("density"))
    expect_within(fdBCPE(G$y, G$mu, G$sigma, G$nu, G$tau, log_ = TRUE), R$lf, tl, relative = FALSE, info = i("log density"))
    # CDF, both tails, both scales
    tF <- tol_mass(R$eF, R, G$tau, base = ifelse(regp, 5e-13, 1e-13)) +
        ifelse(regp & G$nu != 0, K_ULP * eps * R$eF / abs(G$nu), 0)
    tU <- tol_mass(R$eU, R, G$tau)
    tF_stable <- tol_mass(R$eF, R, G$tau)
    expect_silent(p_nat <- fpBCPE(G$y, G$mu, G$sigma, G$nu, G$tau))
    okF <- R$F > 1e-290; okU <- R$U > 1e-290
    expect_within(p_nat[okF], R$F[okF], tF[okF], info = i("CDF"))
    expect_within(fpBCPE(G$y, G$mu, G$sigma, G$nu, G$tau, lower_tail = FALSE)[okU], R$U[okU], tU[okU],
                  info = paste0(dep("direct upper tail"), i("upper tail")))
    # log F: absolute (plus 1e-13 |log F|) where F <= 1/2; relative, with the upper tail's tolerance,
    # where F > 1/2 (log F = log1p(-U) keeps the relative accuracy of U)
    hiF <- R$F > 0.5
    expect_within(fpBCPE(G$y, G$mu, G$sigma, G$nu, G$tau, log_p = TRUE), R$lF,
                  ifelse(hiF, tU, tF_stable + 1e-13 * abs(R$lF)), scale = ifelse(hiF, abs(R$lF), 1),
                  info = paste0(dep("log scales"), i("log CDF")))
    hiU <- R$U > 0.5
    expect_within(fpBCPE(G$y, G$mu, G$sigma, G$nu, G$tau, lower_tail = FALSE, log_p = TRUE), R$lU,
                  ifelse(hiU, tF_stable, tU + 1e-13 * abs(R$lU)), scale = ifelse(hiU, abs(R$lU), 1),
                  info = paste0(dep("log scales"), i("log upper tail")))
    # quantiles: the grid's y is the reference quantile of the closed-form masses (the rounding of
    # the probability is in the tolerance through the elasticity)
    smallF <- R$F <= R$U
    e_s <- ifelse(smallF, R$eF, R$eU)
    lm_s <- ifelse(smallF, R$lF, R$lU)
    ql <- R$F > 1e-290 & R$U > 1e-3
    regq <- in_gamlss_region(R$F, G$mu, G$sigma, G$nu, G$tau, "q")
    tq <- tol_quant(R, e_s, lm_s, ifelse(smallF, 1, R$F / R$U), G$tau, base = ifelse(regq, 5e-13, 1e-13)) +
        ifelse(regq, K_ULP * gamlss_q_err(R$F, G$sigma, G$nu, G$tau), 0)
    expect_silent(q_nat <- fqBCPE(R$F[ql], G$mu[ql], G$sigma[ql], G$nu[ql], G$tau[ql]))
    expect_within(q_nat, G$y[ql], tq[ql], info = i("quantile at p = F(y)"))
    qu <- R$U > 1e-290 & R$F > 1e-3
    tqu <- tol_quant(R, e_s, lm_s, ifelse(smallF, R$U / R$F, 1), G$tau)
    expect_within(fqBCPE(R$U[qu], G$mu[qu], G$sigma[qu], G$nu[qu], G$tau[qu], lower_tail = FALSE), G$y[qu], tqu[qu],
                  info = paste0(dep("direct upper tail"), i("upper-tail quantile at p = U(y)")))
    tql <- tol_quant(R, e_s, lm_s, 1, G$tau)
    expect_within(fqBCPE(R$lF, G$mu, G$sigma, G$nu, G$tau, log_p = TRUE), G$y, tql,
                  info = paste0(dep("log scales"), i("log_p quantile at log F(y)")))
    expect_within(fqBCPE(R$lU, G$mu, G$sigma, G$nu, G$tau, lower_tail = FALSE, log_p = TRUE), G$y, tql,
                  info = paste0(dep("log scales"), i("log_p upper-tail quantile at log U(y)")))
    invisible(list(G = G, R = R))
}
grid2 <- check_grid(make_grid(900, 2, 20261013), "tau = 2 grid")
grid1 <- check_grid(make_grid(900, 1, 20261014), "tau = 1 grid")

# ----------------------------------------------------------------------------
# CLOSED-FORM QUANTILES on p grids: p = 10^-(1..300), log p down to -1e5, both tails
# ----------------------------------------------------------------------------

qgrid_pars <- list(c(25, 0.15, 0, 2), c(25, 0.15, 1.5, 1), c(25, 0.15, -1.5, 1), c(20, 0.2, 0.8, 2),
                   c(30, 0.25, -0.6, 2), c(1e-4, 2, 3, 1), c(5e4, 0.02, -7, 1), c(3, 0.5, 1e-10, 2),
                   c(3, 0.5, -1e-200, 1))
pp_nat <- 10^-c(1:20, seq(25, 300, by = 5))
pp_log <- -10^seq(-0.15, 5, by = 0.25)
for (pr in qgrid_pars) {
    lab <- sprintf("mu %g sigma %g nu %g tau %g", pr[1], pr[2], pr[3], pr[4])
    for (lt in c(TRUE, FALSE)) for (lp in c(FALSE, TRUE)) {
        pg <- if (lp) pp_log else pp_nat
        want <- cf_q(pg, pr[1], pr[2], pr[3], pr[4], lower = lt, logp = lp)
        ok <- !is.na(want) & want > 1e-300 & want < 1e300
        R <- cf(want[ok], pr[1], pr[2], pr[3], pr[4])
        smallF <- R$F <= R$U
        tq <- tol_quant(R, ifelse(smallF, R$eF, R$eU), ifelse(smallF, R$lF, R$lU), 1, pr[4])
        if (lt && !lp) {      # inside G-q'' the value is gamlss.dist's, with its own error
            rq <- in_gamlss_region(pg[ok], pr[1], pr[2], pr[3], pr[4], "q")
            tq <- tq + ifelse(rq, 4e-13 + K_ULP * gamlss_q_err(pg[ok], pr[2], pr[3], pr[4]), 0)
        }
        expect_true(sum(ok) >= 3, info = paste("design: the closed-form quantile grid is not empty,", lab))
        got <- fqBCPE(pg[ok], pr[1], pr[2], pr[3], pr[4], lower_tail = lt, log_p = lp)
        tag <- paste0(if (!lt) dep("direct upper tail") else "", if (lp) dep("log scales") else "",
                      if (pr[3] == 0 && lt && !lp) dep("far lower tail") else "")
        expect_within(got, want[ok], tq, info = sprintf("%sclosed-form quantile, lower_tail %s log_p %s, %s", tag, lt, lp, lab))
    }
}

# ----------------------------------------------------------------------------
# HAND-PICKED mpmath VALUES (80 digits), with a tolerance per row from
# the point's conditioning (kind "rel": |got/value - 1| <= tol; "abs": |got - value| <= tol)
# ----------------------------------------------------------------------------

mp_ref <- read.csv(text = "label,fn,arg,mu,sigma,nu,tau,lower_tail,log,value,kind,tol
far_lower_p7,p,7,25,0.15,0,2,TRUE,FALSE,1.0653377621316583e-17,rel,1e-12
far_lower_p4,p,4,25,0.15,0,2,TRUE,FALSE,1.2578587371997947e-34,rel,2e-12
far_lower_q1e-20,q,9.9999999999999995e-21,25,0.15,0,2,TRUE,FALSE,6.2309250903486149,rel,2e-13
far_lower_q1e-14,q,1e-14,25,0.15,0,2,TRUE,FALSE,7.9349861689066383,rel,2e-13
far_lower_logq-1e4,q,-10000,25,0.15,0,2,TRUE,TRUE,1.5412296746762819e-08,rel,3e-13
far_lower_logp,p,0.001,25,0.15,0,2,TRUE,TRUE,-2283.9905023574752,abs,5e-10
nu1e-12_q,q,0.90000000000000002,25,0.15,1e-12,2,TRUE,FALSE,30.298813692915616,rel,2e-13
nu1e-17_q,q,0.90000000000000002,25,0.15,1e-17,2,TRUE,FALSE,30.298813692916173,rel,2e-13
nu1e-17_p,p,30,25,0.15,1e-17,2,TRUE,FALSE,0.88790790073236492,rel,2e-13
nu1e-9_p,p,30,25,0.15,1e-09,2,TRUE,FALSE,0.88790790075348303,rel,2e-13
nu-1e-6_p_up,p,40,25,0.15,-1e-06,2,FALSE,FALSE,0.00086409613848074409,rel,4e-13
nu1e-300_d,d,30,25,0.15,1e-300,2.5,NA,FALSE,0.04691451690107714,rel,3e-13
nu1e-12_q_tau1.5,q,0.02,25,0.15,1e-12,1.5,TRUE,FALSE,18.108684302873684,rel,2e-13
trunc_pos_p_tau2,p,0.001,25,0.15,1.5,2,TRUE,FALSE,2.3040522151894529e-11,rel,4e-13
trunc_pos_logp_tau2,p,1.0000000000000001e-30,25,0.15,1.5,2,TRUE,TRUE,-117.74846538236444,abs,2e-11
trunc_pos_logp_tau3,p,9.9999999999999998e-201,20,0.2,2,3,TRUE,TRUE,-930.74340442565494,abs,2e-10
trunc_pos_p_tau0.7,p,0.5,20,0.3,1,0.7,TRUE,FALSE,0.00062107192268934545,rel,2e-13
trunc_pos_q_tau2,q,9.9999999999999995e-21,25,0.15,1.5,2,TRUE,FALSE,5.7324355251667186e-10,rel,4e-13
trunc_pos_logq_tau2,q,-300,25,0.15,1.5,2,TRUE,TRUE,1.7091340696900109e-83,rel,2e-12
trunc_pos_upq_tau3,q,-1.0000000000000001e-30,20,0.2,2,3,FALSE,TRUE,1.2834007978848819e-13,rel,4e-13
trunc_pos_d_tau2,d,0.001,25,0.15,1.5,2,NA,TRUE,-17.180543731836416,abs,4e-12
trunc_pos_logU_tau2,p,0.001,25,0.15,1.5,2,FALSE,TRUE,-2.3040522152159964e-11,rel,4e-13
trunc_neg_U_tau2,p,100000,25,0.15,-1.5,2,FALSE,FALSE,3.6002131430954979e-10,rel,4e-13
trunc_neg_logU_tau2,p,1e+40,25,0.15,-1.5,2,FALSE,TRUE,-142.63061430267052,abs,3e-11
trunc_neg_logU_tau1.3,p,9.9999999999999992e+249,30,0.25,-0.8,1.3,FALSE,TRUE,-464.78983969241028,abs,9e-11
trunc_neg_logF_tau2,p,1e+40,25,0.15,-1.5,2,TRUE,TRUE,-1.1384429129866815e-62,rel,8e-13
trunc_neg_upq_tau2,q,1e-25,25,0.15,-1.5,2,FALSE,FALSE,2348952160662574.5,rel,5e-13
trunc_neg_uplogq_tau2,q,-500,25,0.15,-1.5,2,FALSE,TRUE,2.9446576226082514e+143,rel,3e-12
trunc_neg_lowlogq_tau2,q,-1e-25,25,0.15,-1.5,2,TRUE,TRUE,2348952160662574.5,rel,5e-13
trunc_neg_d_tau2,d,1e+40,25,0.15,-1.5,2,NA,TRUE,-234.32855291432421,abs,6e-11
between_pos_tau2,p,3,25,0.15,1.5,2,TRUE,FALSE,5.8294850893817961e-06,rel,3e-13
between_neg_tau2,p,400,25,0.15,-1.5,2,FALSE,FALSE,1.6656675476121538e-06,rel,3e-13
between_pos_tau4,p,1,20,0.2,1.2,4,TRUE,FALSE,4.2168400434044321e-16,rel,8e-13
far_upper_U_nu1,p,60,25,0.15,1,2,FALSE,FALSE,5.1296284710262909e-21,rel,2e-12
far_upper_logF_nu1,p,60,25,0.15,1,2,TRUE,TRUE,-5.1296284710262909e-21,rel,2e-12
far_upper_U_nu0_tau1.2,p,200,25,0.15,0,1.2,FALSE,FALSE,1.4853961139222039e-12,rel,6e-13
upper_q_tiny,q,1e-100,25,0.15,0.5,2.5,FALSE,FALSE,101.83620407554326,rel,2e-13
lower_logq_near0,q,-9.9999999999999995e-21,25,0.15,0.5,2.5,TRUE,TRUE,57.863390746616112,rel,2e-13
tau0.05_p,p,26,25,0.15,0.5,0.05,TRUE,FALSE,0.99898494813552063,rel,3e-13
tau0.05_U,p,40,25,0.15,0.5,0.05,FALSE,FALSE,8.0658723125556138e-05,rel,4e-13
tau0.05_q,q,0.999,25,0.15,0.5,0.05,TRUE,FALSE,26.01764846249252,rel,3e-13
tau100_p,p,29,25,0.15,0.5,100,TRUE,FALSE,0.79657177883393027,rel,2e-13
tau100_q,q,0.01,25,0.15,0.5,100,TRUE,FALSE,19.036845969876619,rel,2e-13
tau100_d,d,28,25,0.15,0.5,100,NA,FALSE,0.072756904330326805,rel,2e-13
sigma1e-3_p,p,25.010000000000002,25,0.001,0.7,2,TRUE,FALSE,0.65541290461622215,rel,2e-12
sigma10_p,p,1.0000000000000001e-05,25,10,0.05,2,TRUE,FALSE,0.12876632539604987,rel,2e-13
sigma10_U,p,1000000000000,25,10,0.05,2,FALSE,FALSE,9.0363817843156892e-07,rel,5e-13
sigma10_q,q,0.29999999999999999,25,10,-0.05,2,TRUE,FALSE,0.20311571991120317,rel,3e-13
mu1e-6_p,p,2.9999999999999999e-07,1e-06,0.4,-0.3,1.7,TRUE,FALSE,0.00043112685193133461,rel,3e-13
mu1e6_q,q,0.99999899999999997,1e+06,0.4,0.3,2.7,TRUE,FALSE,3528231.7070667502,rel,2e-13", stringsAsFactors = FALSE)

# evaluate the package at the rows of a reference table (fn, arg, mu, sigma, nu, tau, lower_tail, log)
eval_ref_rows <- function(d) {
    got <- numeric(nrow(d))
    lt <- as.logical(d$lower_tail); lt[is.na(lt)] <- TRUE
    lg <- as.logical(d$log)
    for (fn in c("d", "p", "q")) for (a in c(TRUE, FALSE)) for (b in c(TRUE, FALSE)) {
        k <- which(d$fn == fn & lt == a & lg == b)
        if (!length(k)) next
        got[k] <- switch(fn,
            d = fdBCPE(d$arg[k], d$mu[k], d$sigma[k], d$nu[k], d$tau[k], log_ = b),
            p = fpBCPE(d$arg[k], d$mu[k], d$sigma[k], d$nu[k], d$tau[k], lower_tail = a, log_p = b),
            q = fqBCPE(d$arg[k], d$mu[k], d$sigma[k], d$nu[k], d$tau[k], lower_tail = a, log_p = b))
    }
    got
}
mp_got <- eval_ref_rows(mp_ref)
mp_err <- ifelse(mp_ref$kind == "abs", abs(mp_got - mp_ref$value), abs(mp_got / mp_ref$value - 1)) / mp_ref$tol
mp_err[mp_got == mp_ref$value] <- 0
mp_err[is.na(mp_err)] <- Inf
mp_tag <- function(lab) {
    if (grepl("^far_lower", lab)) dep("far lower tail") else if (grepl("^nu", lab)) dep("nu near 0")
    else if (grepl("^trunc|^between", lab)) dep("power-law regime") else if (grepl("^far_upper|^upper_q", lab)) dep("direct upper tail")
    else if (grepl("^lower_logq", lab)) dep("log scales") else if (grepl("^tau", lab)) num("tau extremes")
    else if (grepl("^sigma", lab)) num("sigma extremes") else ""
}
for (k in seq_len(nrow(mp_ref))) {
    expect_true(mp_err[k] <= 1,
                info = sprintf("%smpmath %s: %s(%s; %g, %g, %g, %g, lower %s, log %s) = %.17g, want %.17g (err/tol %.3g)",
                               mp_tag(mp_ref$label[k]), mp_ref$label[k], mp_ref$fn[k], format(mp_ref$arg[k], digits = 17),
                               mp_ref$mu[k], mp_ref$sigma[k], mp_ref$nu[k], mp_ref$tau[k], mp_ref$lower_tail[k],
                               mp_ref$log[k], mp_got[k], mp_ref$value[k], mp_err[k]))
}

# ----------------------------------------------------------------------------
# DEPARTURES FROM gamlss.dist, against the references
# ----------------------------------------------------------------------------

# far lower tail, nu = 0, tau = 2: F = pnorm(log(y/mu)/sigma); gamlss.dist gives 0 below ~1e-16
y_fl <- c(10, 7, 5, 4, 2, 0.5, 1e-3)
want_fl <- pnorm(log(y_fl / 25) / 0.15)
R_fl <- cf(y_fl, 25, 0.15, 0, 2)
expect_within(fpBCPE(y_fl, 25, 0.15, 0, 2), want_fl, tol_mass(R_fl$eF, R_fl, 2),
              info = paste0(dep("far lower tail"), "F = pnorm(log(y/mu)/sigma), 5e-10 down to 1e-150 (and 0 below 1e-308)"))
expect_close(fpBCPE(y_fl, 25, 0.15, 0, 2, log_p = TRUE), pnorm(log(y_fl / 25) / 0.15, log.p = TRUE), tol = 1e-13,
             info = paste0(dep("far lower tail"), dep("log scales"), "log F = pnorm(log.p = TRUE)"))
p_fl <- 10^-c(10, 14, 16, 20, 50, 100, 200, 300)
expect_close(fqBCPE(p_fl, 25, 0.15, 0, 2), 25 * exp(0.15 * qnorm(p_fl)), tol = 2e-13,
             info = paste0(dep("far lower tail"), "Q = mu exp(sigma qnorm(p)) (gamlss.dist: 0 for p = 1e-20)"))
expect_true(all(fqBCPE(p_fl, 25, 0.15, 0, 2) > 0), info = paste0(dep("far lower tail"), "Q(1e-300) > 0"))

# nu near 0: z = (ell/sigma) * expm1(nu ell)/(nu ell) is log(y/mu)/sigma to the last bit for
# |nu ell| < eps; at nu = 1e-12 the first-order term nu ell^2 / (2 sigma) is still 1e-14 of z
y_nu <- c(5, 15, 24.9, 25.1, 30, 40, 80)
for (nu0 in c(1e-300, -1e-300, 1e-100, 1e-17, -1e-17)) {
    expect_within(fpBCPE(y_nu, 25, 0.15, nu0, 2), pnorm(log(y_nu / 25) / 0.15), 2e-13,
                  info = paste0(dep("nu near 0"), "F at nu = ", nu0, " is the nu = 0 CDF"))
    expect_within(fpBCPE(y_nu, 25, 0.15, nu0, 2, lower_tail = FALSE), pnorm(log(y_nu / 25) / 0.15, lower.tail = FALSE), 2e-13,
                  info = paste0(dep("nu near 0"), "U at nu = ", nu0, " is the nu = 0 upper tail"))
    expect_within(fqBCPE(c(1e-300, 1e-10, 0.1, 0.5, 0.9), 25, 0.15, nu0, 2), 25 * exp(0.15 * qnorm(c(1e-300, 1e-10, 0.1, 0.5, 0.9))), 2e-13,
                  info = paste0(dep("nu near 0"), "Q at nu = ", nu0, " is the nu = 0 quantile"))
    expect_within(fdBCPE(y_nu, 25, 0.15, nu0, 2), dnorm(log(y_nu / 25) / 0.15) / (0.15 * y_nu), 2e-13,
                  info = paste0(dep("nu near 0"), "f at nu = ", nu0, " is the nu = 0 density"))
}
for (nu0 in c(1e-12, -1e-9, 3e-6)) {
    Rn <- cf(y_nu, 25, 0.15, nu0, 2)
    expect_within(fpBCPE(y_nu, 25, 0.15, nu0, 2), Rn$F, tol_mass(Rn$eF, Rn, 2),
                  info = paste0(dep("nu near 0"), "F at nu = ", nu0, " against the closed form"))
    # (y = 80 has U = 5e-15: Q(F) there is ill-conditioned, so it is left out)
    expect_within(fqBCPE(Rn$F[-7], 25, 0.15, nu0, 2), y_nu[-7], 2e-13, info = paste0(dep("nu near 0"), "Q(F(y)) = y at nu = ", nu0))
    expect_within(fqBCPE(Rn$U[-1], 25, 0.15, nu0, 2, lower_tail = FALSE), y_nu[-1], 2e-13,
                  info = paste0(dep("nu near 0"), "Q_upper(U(y)) = y at nu = ", nu0))
}
# continuity in nu across 0 and across the G1 boundary |nu| * min(1, sigma) = G_NU_MIN: nu_in is
# the smallest double inside the region, nu_out the largest one outside (an ulp or two apart, so
# the true change of F is ~1e-16)
for (sg in c(0.15, 2)) {
    cand <- G_NU_MIN / min(1, sg) * (1 + (-6:6) * 2^-52)
    nu_in <- min(cand[cand * min(1, sg) >= G_NU_MIN]); nu_out <- max(cand[cand * min(1, sg) < G_NU_MIN])
    nus <- c(-nu_in, -nu_out, -1e-30, 0, 1e-30, nu_out, nu_in)
    for (yy in c(10, 25.5, 60)) {
        F7 <- fpBCPE(yy, 25, sg, nus, 2.5)
        expect_true(max(abs(diff(F7[3:5]))) <= 2e-15 * max(F7) && max(abs(F7[1] / F7[2] - 1), abs(F7[7] / F7[6] - 1)) <= 3e-13,
                    info = sprintf("F is continuous in nu across 0 and across the gamlss-region boundary (y %g, sigma %g)", yy, sg))
        Q7 <- fqBCPE(c(0.02, 0.5, 0.97), 25, sg, rep(nus, each = 3), 2.5)
        Q7 <- matrix(Q7, nrow = 3)
        expect_true(max(abs(Q7[, 1] / Q7[, 2] - 1), abs(Q7[, 7] / Q7[, 6] - 1), abs(Q7[, 3] / Q7[, 4] - 1)) <= 3e-13,
                    info = sprintf("Q is continuous in nu across 0 and the gamlss-region boundary (sigma %g)", sg))
    }
}

# THE UPPER TAIL IS DIRECT. nu = 1, tau = 2: z = (y/mu - 1)/sigma, U = pnorm(z, upper) / D
up_y <- c(40, 60, 100, 150)
up_D <- pnorm(1 / 0.15)
expect_within(fpBCPE(up_y, 25, 0.15, 1, 2, lower_tail = FALSE), pnorm((up_y / 25 - 1) / 0.15, lower.tail = FALSE) / up_D, 2e-13,
              info = paste0(dep("direct upper tail"), "U is pnorm(z, upper) / D where F rounds to 1"))
expect_within(fpBCPE(up_y, 25, 0.15, 1, 2, lower_tail = FALSE, log_p = TRUE),
              pnorm((up_y / 25 - 1) / 0.15, lower.tail = FALSE, log.p = TRUE) - pnorm(1 / 0.15, log.p = TRUE), 2e-13,
              info = paste0(dep("direct upper tail"), dep("log scales"), "log U"))
expect_within(fpBCPE(up_y, 25, 0.15, 1, 2, log_p = TRUE), log1p(-pnorm((up_y / 25 - 1) / 0.15, lower.tail = FALSE) / up_D), 2e-13,
              info = paste0(dep("log scales"), "log F = log1p(-U), -5e-21 at q = 60 where F rounds to 1 (gamlss.dist: 0)"))
expect_true(all(fpBCPE(up_y, 25, 0.15, 1, 2, log_p = TRUE) < 0), info = paste0(dep("log scales"), "log F < 0, never 0, for a finite q"))
# and the quantiles from those tails
pu_t <- 10^-c(5, 20, 50, 100, 200, 300)
expect_within(fqBCPE(pu_t, 25, 0.15, 1, 2, lower_tail = FALSE), 25 * (1 + 0.15 * qnorm(pu_t * up_D, lower.tail = FALSE)), 2e-13,
              info = paste0(dep("direct upper tail"), "upper-tail Q(1e-300) (gamlss.dist: from 1 - p = 1)"))
expect_within(fqBCPE(-pu_t, 25, 0.15, 1, 2, log_p = TRUE), 25 * (1 + 0.15 * qnorm(-expm1(-pu_t) * up_D, lower.tail = FALSE)), 2e-13,
              info = paste0(dep("log scales"), "Q(log p) for log p = -1e-300 inverts -expm1(log p) (gamlss.dist: Inf)"))

# THE POWER-LAW REGIME. tau = 1 and tau = 2 sweeps towards the truncation point: nu > 0 as y -> 0,
# nu < 0 as y -> Inf, w = 10^-1 ... 10^-1000 (the masses go far below the double range on the log
# scale; nu = +-5 keeps y = mu w^(1/nu) a double)
for (tt in c(1, 2)) for (nn in c(5, 1.5, -1.5, -5)) {
    lab <- sprintf("tau %g nu %g", tt, nn)
    kw <- if (abs(nn) == 5) c(1, 3, 10, 30, 100, 300, 600, 1000) else c(1, 3, 10, 30, 100, 300, 450)
    yw <- 25 * 10^(-kw / nn)
    Rw <- cf(yw, 25, 0.15, nn, tt)
    small <- if (nn > 0) "F" else "U"
    lsm <- if (nn > 0) Rw$lF else Rw$lU
    esm <- if (nn > 0) Rw$eF else Rw$eU
    tsm <- tol_mass(esm, Rw, tt)
    expect_true(all(is.finite(lsm)) && min(lsm) < (if (abs(nn) == 5) -2000 else -1000) && all(abs(esm / abs(nn) - 1) < 0.05 | kw < 3),
                info = paste("design: the sweep is a power law reaching log mass < -1000,", lab))
    expect_within(fpBCPE(yw, 25, 0.15, nn, tt, lower_tail = nn > 0, log_p = TRUE), lsm, tsm + 1e-13 * abs(lsm), relative = FALSE,
                  info = paste0(dep("power-law regime"), dep("log scales"), "log of the small tail, ", lab))
    nat <- exp(lsm) > 1e-290
    expect_within(fpBCPE(yw[nat], 25, 0.15, nn, tt, lower_tail = nn > 0), exp(lsm[nat]), tsm[nat] + 4 * eps * abs(lsm[nat]),
                  info = paste0(dep("power-law regime"), "the small tail, natural scale, ", lab))
    # the other tail is 1 - small: its log is log1p(-small), to the relative accuracy of small
    expect_within(fpBCPE(yw[nat], 25, 0.15, nn, tt, lower_tail = nn < 0, log_p = TRUE), log1p(-exp(lsm[nat])), tsm[nat],
                  info = paste0(dep("power-law regime"), dep("log scales"), "log of the large tail = log1p(-small), ", lab))
    expect_within(fdBCPE(yw, 25, 0.15, nn, tt, log_ = TRUE), Rw$lf, tol_logd(Rw, tt), relative = FALSE,
                  info = paste0(dep("power-law regime"), "log density, ", lab))
    tq <- tol_quant(Rw, esm, lsm, 1, tt)
    expect_within(fqBCPE(lsm, 25, 0.15, nn, tt, lower_tail = nn > 0, log_p = TRUE), yw, tq,
                  info = paste0(dep("power-law regime"), dep("log scales"), "quantile from the log small tail, ", lab))
    expect_within(fqBCPE(exp(lsm[nat]), 25, 0.15, nn, tt, lower_tail = nn > 0), yw[nat], tq[nat],
                  info = paste0(dep("power-law regime"), "quantile from the natural small tail, ", lab))
    expect_within(fqBCPE(log1p(-exp(lsm[nat])), 25, 0.15, nn, tt, lower_tail = nn < 0, log_p = TRUE), yw[nat], tq[nat],
                  info = paste0(dep("power-law regime"), dep("log scales"), "quantile from the log large tail near 0, ", lab))
}
# tau = 1: the quantile of a target mass far below the double range, in closed form
for (nn in c(5, -5)) {
    lpw <- -c(800, 1500, 3000)
    want <- cf_q(lpw, 25, 0.15, nn, 1, lower = nn > 0, logp = TRUE)
    expect_true(all(want > 1e-300 & want < 1e300), info = "design: the far quantiles are doubles")
    expect_within(fqBCPE(lpw, 25, 0.15, nn, 1, lower_tail = nn > 0, log_p = TRUE), want, 2e-13 + K_ULP * eps * abs(lpw / nn),
                  info = paste0(dep("power-law regime"), "Laplace quantile at log p down to -3000, nu ", nn))
}

# ----------------------------------------------------------------------------
# OTHER tau: the density integrates to the CDF (integrate(), rel.tol 1e-11)
# ----------------------------------------------------------------------------

# integrate() with a relative tolerance only (abs.tol = 0: the masses can be tiny); a roundoff
# message is not an error here, the comparison below judges the value
integ <- function(f, a, b) integrate(f, a, b, rel.tol = 1e-11, abs.tol = 0, subdivisions = 1000L, stop.on.error = FALSE)$value
# the mass above y, through x = y / t on (0, 1]: integrate() cannot follow a power-law tail to Inf
integ_up <- function(f, y) integ(function(t) f(y / t) * y / t^2, 0, 1)
int_pars <- list(c(25, 0.15, 0.6, 0.6), c(25, 0.15, -1.2, 3), c(20, 0.25, 1.8, 7), c(30, 0.1, 0, 1.3),
                 c(2, 0.6, 0.9, 0.3), c(40, 0.2, -0.4, 20), c(25, 0.15, 3e-9, 4))
for (pr in int_pars) {
    lab <- sprintf("mu %g sigma %g nu %g tau %g", pr[1], pr[2], pr[3], pr[4])
    qs <- fqBCPE(c(0.001, 0.2, 0.5, 0.8, 0.999), pr[1], pr[2], pr[3], pr[4])
    fd <- function(x) fdBCPE(x, pr[1], pr[2], pr[3], pr[4])
    Fq <- fpBCPE(qs, pr[1], pr[2], pr[3], pr[4])
    for (k in 1:4) {
        area <- integ(fd, qs[k], qs[k + 1])
        expect_equal(area / (Fq[k + 1] - Fq[k]), 1, tolerance = 1e-9,
                     info = sprintf("the density integrates to F(%.4g) - F(%.4g), %s", qs[k + 1], qs[k], lab))
    }
    # the far tails: the upper tail mass beyond the 0.999 point and the lower mass below the 0.001
    # point, against the direct tails (including the truncation side for nu != 0)
    up <- integ_up(fd, qs[5])
    expect_equal(up / fpBCPE(qs[5], pr[1], pr[2], pr[3], pr[4], lower_tail = FALSE), 1, tolerance = 1e-9,
                 info = paste("the density integrates to the upper tail,", lab))
    lo <- integ(fd, 0, qs[1])
    expect_equal(lo / Fq[1], 1, tolerance = 1e-9, info = paste("the density integrates to the lower tail,", lab))
    # the log density is the log of the density
    xs <- seq(qs[1], qs[5], length.out = 50)
    expect_close(fdBCPE(xs, pr[1], pr[2], pr[3], pr[4], log_ = TRUE), log(fd(xs)), tol = 1e-12, relative = FALSE,
                 info = paste("log density is the log of the density,", lab))
}
# deep in the truncation regime for tau = 3 and tau = 0.6 the mass below y is the integral of the
# density from 0 (the density is a power law y^(nu - 1) there)
for (pr in list(c(20, 0.2, 2, 3), c(20, 0.3, 1, 0.6), c(25, 0.15, -1.5, 3))) {
    fd <- function(x) fdBCPE(x, pr[1], pr[2], pr[3], pr[4])
    if (pr[3] > 0) {
        yy <- pr[1] * c(1e-2, 1e-4, 1e-8)
        area <- sapply(yy, function(u) integ(fd, 0, u))
        expect_equal(area / fpBCPE(yy, pr[1], pr[2], pr[3], pr[4]), rep(1, 3), tolerance = 1e-9,
                     info = sprintf("%sthe power-law lower tail integrates from 0, tau %g", dep("power-law regime"), pr[4]))
    } else {
        yy <- pr[1] * c(1e2, 1e4, 1e8)
        area <- sapply(yy, function(u) integ_up(fd, u))
        expect_equal(area / fpBCPE(yy, pr[1], pr[2], pr[3], pr[4], lower_tail = FALSE), rep(1, 3), tolerance = 1e-9,
                     info = sprintf("%sthe power-law upper tail integrates to Inf, tau %g", dep("power-law regime"), pr[4]))
    }
}

# ----------------------------------------------------------------------------
# THE GAMLSS REGION: identical() to gamlss.dist
#
# The natural-scale density (both log_), the natural lower-tail CDF and quantile are gamlss.dist's
# own values, bit for bit, wherever the region flags (computed here from gamlss.dist's own
# intermediates, see in_gamlss_region()) hold.
# ----------------------------------------------------------------------------

if (requireNamespace("gamlss.dist", quietly = TRUE)) local({
    check_parity <- function(mu, sigma, nu, tau, u, label) {
        # materialise every argument here, in this order: a promise such as runif(n, ...) would
        # otherwise be forced inside gamlss.dist::qBCPE, in the order that function happens to use
        # its arguments, and the rows would depend on gamlss.dist's internals
        force(mu); force(sigma); force(nu); force(tau); force(u)
        q <- suppressWarnings(gamlss.dist::qBCPE(u, mu, sigma, nu, tau))
        ok <- is.finite(q) & q > 0
        mu <- mu[ok]; sigma <- sigma[ok]; nu <- nu[ok]; tau <- tau[ok]; u <- u[ok]; q <- q[ok]
        # a spread of x: the quantile points, and points 3% either side of them
        x <- q * sample(c(0.97, 1, 1.03), length(q), replace = TRUE)
        rd <- in_gamlss_region(x, mu, sigma, nu, tau, "d")
        rp <- in_gamlss_region(x, mu, sigma, nu, tau, "p")
        rq <- in_gamlss_region(u, mu, sigma, nu, tau, "q")
        expect_true(sum(rd) > 100 && sum(rp) > 100 && sum(rq) > 100, info = paste("design: the region is populated,", label))
        expect_identical(fdBCPE(x[rd], mu[rd], sigma[rd], nu[rd], tau[rd]),
                         gamlss.dist::dBCPE(x[rd], mu[rd], sigma[rd], nu[rd], tau[rd]),
                         info = paste("density identical() to gamlss.dist::dBCPE in G1,", label))
        expect_identical(fdBCPE(x[rd], mu[rd], sigma[rd], nu[rd], tau[rd], log_ = TRUE),
                         gamlss.dist::dBCPE(x[rd], mu[rd], sigma[rd], nu[rd], tau[rd], log = TRUE),
                         info = paste("log density identical() to gamlss.dist::dBCPE(log = TRUE) in G1,", label))
        expect_identical(fpBCPE(x[rp], mu[rp], sigma[rp], nu[rp], tau[rp]),
                         gamlss.dist::pBCPE(x[rp], mu[rp], sigma[rp], nu[rp], tau[rp]),
                         info = paste("CDF identical() to gamlss.dist::pBCPE in G-p,", label))
        expect_identical(fqBCPE(u[rq], mu[rq], sigma[rq], nu[rq], tau[rq]),
                         gamlss.dist::qBCPE(u[rq], mu[rq], sigma[rq], nu[rq], tau[rq]),
                         info = paste("quantile identical() to gamlss.dist::qBCPE in G-q,", label))
        # the BCPEo names agree with gamlss.dist too
        expect_identical(fqBCPEo(u[rq], mu[rq], sigma[rq], nu[rq], tau[rq]),
                         gamlss.dist::qBCPEo(u[rq], mu[rq], sigma[rq], nu[rq], tau[rq]),
                         info = paste(conv("BCPEo"), "fqBCPEo identical() to gamlss.dist::qBCPEo in G-q,", label))
    }
    set.seed(20261015)
    n <- 3000
    # IMPACTncd-like rows (every argument is drawn before the call, in a fixed order)
    mu <- runif(n, 15, 40); sigma <- runif(n, 0.05, 0.3); nu <- runif(n, -1.5, 1.5); tau <- runif(n, 1.2, 4)
    u <- runif(n)
    check_parity(mu, sigma, nu, tau, u, "IMPACTncd-like rows")
    # the supported domain
    kind <- sample(3, n, replace = TRUE, prob = c(0.6, 0.3, 0.1))
    nu <- ifelse(kind == 1, runif(n, -3, 3), ifelse(kind == 2, runif(n, -10, 10), 0))
    mu <- exp(runif(n, log(1e-6), log(1e6))); sigma <- exp(runif(n, log(1e-3), log(10)))
    tau <- exp(runif(n, log(0.05), log(100))); u <- runif(n)
    check_parity(mu, sigma, nu, tau, u, "supported domain")

    # Coverage: on IMPACTncd-like rows at least 98% of fqBCPE(u) are identical() to qBCPE(u)
    nc <- 20000
    mu <- runif(nc, 15, 40); sigma <- runif(nc, 0.05, 0.3); nu <- runif(nc, -1.5, 1.5); tau <- runif(nc, 1.2, 4)
    u <- runif(nc)
    share <- mean(fqBCPE(u, mu, sigma, nu, tau) == gamlss.dist::qBCPE(u, mu, sigma, nu, tau))
    expect_true(share >= 0.98, info = sprintf("identical() share on IMPACTncd-like rows is %.4f (>= 0.98 expected; region share %.4f)",
                                             share, mean(in_gamlss_region(u, mu, sigma, nu, tau, "q"))))
})

# ----------------------------------------------------------------------------
# THE exp() BAND: rows where an exp() of gamlss.dist has an argument below 2^-26 in absolute value
#
# The gamlss path replays gamlss.dist's arithmetic, exp() included. R >= 4.6.0 evaluates exp() of a
# vector with Rexp(): 1 + x for |x| <= 2^-26 = 1.49e-8, libm's exp(x) otherwise; R <= 4.5 uses libm's
# exp throughout, which differs from 1 + x in the last bit for a few percent (|x| = 3e-9) to a third
# (|x| = 1e-8) of the band. The package follows the exp() of the R it is compiled against, so its
# gamlss-path values must be identical() to gamlss.dist's on every R version, here and on
# r-oldrel. Random draws almost never put an exp() argument in the band, so the rows are built to:
#   F1  nu = 0 quantile, mu * exp(sigma * za) with |sigma * za| in the band (u = 0.5 + x f_T(0) / sigma)
#   F2  tau within ~1e-7 of 2, so that c = exp(log.c) has |log.c| in the band: d, log d, p and q
#   F3  the peak density (nu = 0, sigma = f_T(0) / mu, y = mu (1 +- 1e-8)): exp(log f), |log f| in the band
# Every call has vectors of length >= 2 (gamlss.dist's byte-compiled code takes libm's exp for a
# length-1 argument, so the same element can differ in the last bit between a scalar and a vector
# call), and every argument is drawn before the call (so the rows do not depend on the order in which
# gamlss.dist forces its arguments). With the exp rule of the other R version (a package compiled
# with -DBCPE_R_EXP_TINY=0 on R >= 4.6, or 1 on R <= 4.5) these expectations fail.
# ----------------------------------------------------------------------------

if (requireNamespace("gamlss.dist", quietly = TRUE)) local({
    set.seed(20261018)
    nb <- 1500
    b_lo <- 8e-9; b_hi <- 0.97 * 2^-26
    in_band <- function(x) is.finite(x) & abs(x) >= 8e-9 & abs(x) <= 2^-26
    lc_of <- function(tau) 0.5 * (-(2 / tau) * log(2) + lgamma(1 / tau) - lgamma(3 / tau))
    fT0_of <- function(tau) tau / (g_cfun(tau) * 2^(1 + 1 / tau) * gamma(1 / tau))
    sgn <- function(n) sample(c(-1, 1), n, replace = TRUE)
    pick <- function(a, k) lapply(a, function(v) v[k])
    tg <- num("exp band")

    # F1: nu = 0 quantile
    tau <- sample(c(1.5, 2, 3, 4), nb, replace = TRUE); sigma <- runif(nb, 0.05, 0.3); mu <- runif(nb, 15, 40)
    nu <- rep(0, nb)
    u <- 0.5 + sgn(nb) * runif(nb, b_lo, b_hi) * fT0_of(tau) / sigma
    xa <- sigma * gamlss_q_parts(u, sigma, nu, tau)$za       # the argument of gamlss.dist's exp(sigma * za)
    A <- pick(list(u = u, mu = mu, sigma = sigma, nu = nu, tau = tau),
              in_band(xa) & in_gamlss_region(u, mu, sigma, nu, tau, "q"))
    expect_true(length(A$u) >= 1000, info = paste0(tg, sprintf("design: F1 has %d nu = 0 quantile rows in the band", length(A$u))))
    expect_identical(fqBCPE(A$u, A$mu, A$sigma, A$nu, A$tau), gamlss.dist::qBCPE(A$u, A$mu, A$sigma, A$nu, A$tau),
                     info = paste0(tg, "F1: nu = 0 quantile with |sigma * za| in [8e-9, 2^-26] is identical() to qBCPE"))

    # F2: tau within ~1e-7 of 2 (|log.c| = |slope| |tau - 2| in the band), nu != 0
    slope <- (lc_of(2 + 1e-5) - lc_of(2 - 1e-5)) / 2e-5
    tau <- 2 + sgn(nb) * runif(nb, b_lo, b_hi) / abs(slope)
    mu <- runif(nb, 15, 40); sigma <- runif(nb, 0.1, 0.3); nu <- sgn(nb) * runif(nb, 0.3, 1.5)
    u <- runif(nb)
    x <- gamlss.dist::qBCPE(u, mu, sigma, nu, tau)
    x <- x * sample(c(0.97, 1, 1.03), nb, replace = TRUE)
    band2 <- in_band(lc_of(tau))
    rd <- band2 & in_gamlss_region(x, mu, sigma, nu, tau, "d")
    rp <- band2 & in_gamlss_region(x, mu, sigma, nu, tau, "p")
    rq <- band2 & in_gamlss_region(u, mu, sigma, nu, tau, "q")
    expect_true(sum(rd) >= 1000 && sum(rp) >= 1000 && sum(rq) >= 1000,
                info = paste0(tg, sprintf("design: F2 has %d d, %d p and %d q rows with |log.c| in the band", sum(rd), sum(rp), sum(rq))))
    D <- pick(list(x = x, mu = mu, sigma = sigma, nu = nu, tau = tau), rd)
    expect_identical(fdBCPE(D$x, D$mu, D$sigma, D$nu, D$tau), gamlss.dist::dBCPE(D$x, D$mu, D$sigma, D$nu, D$tau),
                     info = paste0(tg, "F2: density with c = exp(log.c), |log.c| in the band, is identical() to dBCPE"))
    expect_identical(fdBCPE(D$x, D$mu, D$sigma, D$nu, D$tau, log_ = TRUE),
                     gamlss.dist::dBCPE(D$x, D$mu, D$sigma, D$nu, D$tau, log = TRUE),
                     info = paste0(tg, "F2: log density identical() to dBCPE(log = TRUE)"))
    P <- pick(list(x = x, mu = mu, sigma = sigma, nu = nu, tau = tau), rp)
    expect_identical(fpBCPE(P$x, P$mu, P$sigma, P$nu, P$tau), gamlss.dist::pBCPE(P$x, P$mu, P$sigma, P$nu, P$tau),
                     info = paste0(tg, "F2: CDF identical() to pBCPE"))
    Q <- pick(list(u = u, mu = mu, sigma = sigma, nu = nu, tau = tau), rq)
    expect_identical(fqBCPE(Q$u, Q$mu, Q$sigma, Q$nu, Q$tau), gamlss.dist::qBCPE(Q$u, Q$mu, Q$sigma, Q$nu, Q$tau),
                     info = paste0(tg, "F2: quantile identical() to qBCPE"))

    # F3: the peak density, exp(log f) with |log f| in the band
    tau <- sample(c(1.5, 2, 3, 4), nb, replace = TRUE); mu <- runif(nb, 0.5, 30); sigma <- fT0_of(tau) / mu
    nu <- rep(0, nb)
    x <- mu * (1 + sgn(nb) * runif(nb, b_lo, b_hi))
    ll <- gamlss.dist::dBCPE(x, mu, sigma, nu, tau, log = TRUE)       # the argument of exp() in dBCPE
    A <- pick(list(x = x, mu = mu, sigma = sigma, nu = nu, tau = tau), in_band(ll) & in_gamlss_region(x, mu, sigma, nu, tau, "d"))
    expect_true(length(A$x) >= 1000, info = paste0(tg, sprintf("design: F3 has %d peak-density rows in the band", length(A$x))))
    expect_identical(fdBCPE(A$x, A$mu, A$sigma, A$nu, A$tau), gamlss.dist::dBCPE(A$x, A$mu, A$sigma, A$nu, A$tau),
                     info = paste0(tg, "F3: density with |log f| in the band is identical() to dBCPE"))
})

# ----------------------------------------------------------------------------
# CONSISTENCY
# ----------------------------------------------------------------------------

set.seed(20261016)
nr <- 1500
c_mu <- exp(runif(nr, log(1e-3), log(1e3)))
c_sigma <- exp(runif(nr, log(0.01), log(3)))
c_nu <- sample(c(-1, 1), nr, replace = TRUE) * c(runif(nr / 2, 0, 4), 10^-runif(nr / 2, 1, 200))
c_tau <- exp(runif(nr, log(0.1), log(30)))
c_u <- c(runif(nr / 2, 0.001, 0.999), 10^-runif(nr / 2, 3, 250))
c_y <- fqBCPE(c_u, c_mu, c_sigma, c_nu, c_tau)
ok <- is.finite(c_y) & c_y > 1e-290 & c_y < 1e290
c_mu <- c_mu[ok]; c_sigma <- c_sigma[ok]; c_nu <- c_nu[ok]; c_tau <- c_tau[ok]; c_u <- c_u[ok]; c_y <- c_y[ok]
cF <- fpBCPE(c_y, c_mu, c_sigma, c_nu, c_tau)
cU <- fpBCPE(c_y, c_mu, c_sigma, c_nu, c_tau, lower_tail = FALSE)
clF <- fpBCPE(c_y, c_mu, c_sigma, c_nu, c_tau, log_p = TRUE)
clU <- fpBCPE(c_y, c_mu, c_sigma, c_nu, c_tau, lower_tail = FALSE, log_p = TRUE)
c_reg <- in_gamlss_region(c_y, c_mu, c_sigma, c_nu, c_tau, "p")
cd <- fdBCPE(c_y, c_mu, c_sigma, c_nu, c_tau)
cld <- fdBCPE(c_y, c_mu, c_sigma, c_nu, c_tau, log_ = TRUE)
c_ell <- log(c_y / c_mu)
eF_c <- exp(log(c_y) + cld - clF)          # elasticities d log F / d ell and d log U / d ell
eU_c <- exp(log(c_y) + cld - clU)
# inside the region F is gamlss.dist's: its z carries an absolute error eps w / (|nu| sigma), i.e.
# a relative error eps * e / |nu| in F (large near the cusp of f_T for tau < 1, which is why the
# region needs tau >= 1; the tau < 1 rows further down are checked without this allowance)
galw <- ifelse(c_reg & c_nu != 0, K_ULP * eps * eF_c / abs(c_nu), 0)
expect_true(all(cF >= 0 & cF <= 1 + 4 * eps & cU >= 0 & cU <= 1 & clF <= 0 & clU <= 0),
            info = "0 <= F <= 1 (gamlss.dist's own F may round a few ulp above 1), 0 <= U <= 1, log values <= 0")
expect_within(cF + cU, rep(1, length(cF)), ifelse(c_reg, 5e-13, 2e-13) + galw * cF,
              info = "F + U = 1 (to the accuracy contract: 1e-13 per tail, gamlss.dist's own error inside its region)")
# exp(log F) = F to max(1e-14, 4 eps |log F|) where both come from the stable path (R's
# natural-scale pgamma is itself up to ~|log M| eps off in the far tail, hence 4 eps rather than 2);
# inside the gamlss region F is gamlss.dist's own value (error up to ~1.5e-13)
nF <- cF > 1e-290
expect_within(exp(clF[nF]), cF[nF], ifelse(c_reg, 3e-13 + galw + 4 * eps * abs(clF), pmax(1e-14, 4 * eps * abs(clF)))[nF],
              info = "exp(log F) = F where F is a normal double")
nU <- cU > 1e-290
expect_within(exp(clU[nU]), cU[nU], pmax(1e-14, 4 * eps * abs(clU[nU])), info = "exp(log U) = U where U is a normal double")
dd <- cd > 1e-290
expect_within(exp(cld[dd]), cd[dd], 2e-13 + 4 * eps * abs(cld[dd]), info = "exp(log f) = f where f is a normal double")
# round trips (u in [0.001, 0.999], and the far lower tail). The tolerance scales with the
# elasticities e = y f(y) / F and y f(y) / U (from the package's own density, validated above):
# a relative error d of Q moves F by e * d, and a rounding of p moves Q by 1 / e
well <- c_u >= 0.001 & c_u <= 0.999
expect_within(cF[well], c_u[well], 1e-12 + galw[well] + K_ULP * eps * eF_c[well] * (1 + abs(c_ell[well])),
              info = "F(Q(p)) = p, p in [0.001, 0.999]")
expect_within(clF[!well], log(c_u[!well]), 3e-13 + 1e-13 * abs(log(c_u[!well])) + K_ULP * eps * eF_c[!well] * (1 + abs(c_ell[!well])),
              relative = FALSE, info = paste0(dep("far lower tail"), "log F(Q(p)) = log p, p down to 1e-250"))
expect_within(fqBCPE(clU, c_mu, c_sigma, c_nu, c_tau, lower_tail = FALSE, log_p = TRUE), c_y,
              3e-13 + K_ULP * eps * (1 + abs(c_ell) + (1 + abs(clU)) / eU_c), info = "Q_upper(log U(y)) = y")
expect_within(fqBCPE(clF, c_mu, c_sigma, c_nu, c_tau, log_p = TRUE), c_y,
              3e-13 + K_ULP * eps * (1 + abs(c_ell) + (1 + abs(clF)) / eF_c), info = "Q(log F(y)) = y")

# ----------------------------------------------------------------------------
# F IS A PROBABILITY: never above 1. gamlss.dist::pBCPE's quotient (FYy1 - FYy2) / FYy3 rounds to 1 + 2^-52 when sigma * nu is of
# order 1 and q is far above the truncation point (0.6 % of such random points); the gamlss path of fpBCPE falls back to the stable
# path there. The consistency block above allows F <= 1 + 4 eps (gamlss.dist's own F may round a few ulp above 1), so nothing else
# asserts F <= 1 exactly; a F > 1 makes fqBCPE(F) stop with "p must be between 0 and 1".
# ----------------------------------------------------------------------------
cdf1 <- read.csv(text = "q,mu,sigma,nu,tau
1119.1344803750424,29.331325204111636,2.5338849251011828,2.5808943549717411,1.1742451768368483
626.22026962381369,23.842107090167701,1.5043662134892524,4.7370987437694216,2.6899547958746552
82.561911507608016,15.001691288780421,2.2770506463854243,3.4174117285703622,3.7926766974851489
1095.6637719285175,26.807138463482261,4.4967091698270769,3.3347718593546487,3.7497183624655008
1669.1782796587236,31.843242039903998,3.3956976563923935,0.87342318302120192,3.1722238203510642
65.071879324901474,11.845889468677342,2.0690104474369546,3.8152085813155803,1.221647740341723
197.68136757436156,28.054490168578923,4.0473606554709889,2.5464575637387017,2.8931141346693039
1753.4490650517992,32.896332088857889,4.9226032041518755,1.9166748706236989,1.3776582824066281
340.99100704614023,11.601482790429145,3.2813157695986819,1.1943765373585533,1.8463321700692177
1065.376194862994,31.287099048495293,3.5624086057449875,1.6871330776410212,3.549769451841712
855.93310302949897,16.508290607016534,4.0303837596453667,1.6865260333472152,1.1149672158062458
125.79240074160411,17.923994779121131,3.5832733772177519,2.6211429640364075,2.9650430837646127", stringsAsFactors = FALSE)
cdf1_F <- fpBCPE(cdf1$q, cdf1$mu, cdf1$sigma, cdf1$nu, cdf1$tau)
expect_true(all(cdf1_F <= 1 & cdf1_F > 1 - 1e-13), info = "F <= 1 (and = 1 to 1e-13) where gamlss.dist's quotient is 1 + 2^-52")
expect_true(all(fpBCPE(cdf1$q, cdf1$mu, cdf1$sigma, cdf1$nu, cdf1$tau, log_p = TRUE) <= 0), info = "log F <= 0 at the same points")
expect_identical(err_msg(fqBCPE(cdf1_F, cdf1$mu, cdf1$sigma, cdf1$nu, cdf1$tau)), NA_character_,
                 info = "fqBCPE(fpBCPE(q)) does not stop at the same points")

# tau < 1 NEAR THE MEDIAN. f_T has a cusp at 0 and s = 0.5 |z/c|^tau is very sensitive to z there;
# gamlss.dist's z ((y/mu)^nu - 1) / (nu sigma) has an absolute error ~eps w / (|nu| sigma), which
# put its F 1.9e-4 and its log f 0.087 off at tau = 0.05. The gamlss region for d and p therefore
# needs tau >= 1, and the stable path, which computes z to ~eps relative from the exact y
# (ell = log1p((y - mu)/mu) for 0.5 <= y/mu <= 2), is accurate. mpmath values, y = 25 (1 + 1e-15,
# 1e-9, 1e-3, -1e-9), sigma 0.05; kz = |d log value / d log z| (all <= 1.6 here), tolerance
# 2e-13 + 16 eps kz.
mp_cusp <- read.csv(text = "fn,arg,nu,tau,lower_tail,log,value,kz
d,25.000000000000028,0.5,0.05,NA,TRUE,21.506077093452344,0.392
p,25.000000000000028,0.5,0.05,TRUE,FALSE,0.50009571906712391,0.000124
p,25.000000000000028,0.5,0.05,FALSE,FALSE,0.49990428093287609,0.000124
d,25.000000025000002,0.5,0.05,NA,TRUE,13.804220841201538,0.777
p,25.000000025000002,0.5,0.05,TRUE,FALSE,0.57853784250578144,0.0427
p,25.000000025000002,0.5,0.05,FALSE,FALSE,0.42146215749421856,0.0587
d,25.024999999999999,0.5,0.05,NA,TRUE,-1.663112156298193,1.55
p,25.024999999999999,0.5,0.05,TRUE,FALSE,0.99281743957215973,0.00477
p,25.024999999999999,0.5,0.05,FALSE,FALSE,0.0071825604278402668,0.66
d,24.999999975000001,0.5,0.05,NA,TRUE,13.804220952237385,0.777
p,24.999999975000001,0.5,0.05,TRUE,FALSE,0.42145798969420323,0.0587
p,24.999999975000001,0.5,0.05,FALSE,FALSE,0.57854201030579677,0.0427
d,25.000000000000028,-1,0.05,NA,TRUE,21.506083275129232,0.392
p,25.000000000000028,-1,0.05,TRUE,FALSE,0.5001029818331002,0.000124
p,25.000000000000028,-1,0.05,FALSE,FALSE,0.49989701816689974,0.000124
d,25.000000025000002,-1,0.05,NA,TRUE,13.804227021961211,0.777
p,25.000000025000002,-1,0.05,TRUE,FALSE,0.57854559015857854,0.0427
p,25.000000025000002,-1,0.05,FALSE,FALSE,0.42145440984142141,0.0587
d,25.024999999999999,-1,0.05,NA,TRUE,-1.6634430882425875,1.55
p,25.024999999999999,-1,0.05,TRUE,FALSE,0.99282419444728809,0.00478
p,25.024999999999999,-1,0.05,FALSE,FALSE,0.0071758055527119296,0.661
d,24.999999975000001,-1,0.05,NA,TRUE,13.804227134831493,0.777
p,24.999999975000001,-1,0.05,TRUE,FALSE,0.42146476632710306,0.0587
p,24.999999975000001,-1,0.05,FALSE,FALSE,0.57853523367289694,0.0427
d,25.000000000000028,0,0.05,NA,TRUE,21.506072922160556,0.392
p,25.000000000000028,0,0.05,TRUE,FALSE,0.50009780430939577,0.000124
p,25.000000000000028,0,0.05,FALSE,FALSE,0.49990219569060423,0.000124
d,25.000000025000002,0,0.05,NA,TRUE,13.804216669604012,0.777
p,25.000000025000002,0,0.05,TRUE,FALSE,0.5785396005375707,0.0427
p,25.000000025000002,0,0.05,FALSE,FALSE,0.4214603994624293,0.0587
d,25.024999999999999,0,0.05,NA,TRUE,-1.663228661690453,1.55
p,25.024999999999999,0,0.05,TRUE,FALSE,0.99281628502690167,0.00477
p,25.024999999999999,0,0.05,FALSE,FALSE,0.0071837149730982876,0.66
d,24.999999975000001,0,0.05,NA,TRUE,13.804216781251338,0.777
p,24.999999975000001,0,0.05,TRUE,FALSE,0.42146040295052573,0.0587
p,24.999999975000001,0,0.05,FALSE,FALSE,0.57853959704947433,0.0427
d,25.000000000000028,0.5,0.2,NA,TRUE,5.2859419644489796,0.00338
p,25.000000000000028,0.5,0.2,TRUE,FALSE,0.49998970968284201,1.12e-11
p,25.000000000000028,0.5,0.2,FALSE,FALSE,0.50001029031715793,1.12e-11
d,25.000000025000002,0.5,0.2,NA,TRUE,5.04207094077191,0.0522
p,25.000000025000002,0.5,0.2,TRUE,FALSE,0.49999375408210756,7.74e-06
p,25.000000025000002,0.5,0.2,FALSE,FALSE,0.50000624591789244,7.74e-06
d,25.024999999999999,0.5,0.2,NA,TRUE,1.1699063249379507,0.826
p,25.024999999999999,0.5,0.2,TRUE,FALSE,0.69850085437682397,0.115
p,25.024999999999999,0.5,0.2,FALSE,FALSE,0.30149914562317598,0.267
d,24.999999975000001,0.5,0.2,NA,TRUE,5.0420709491568196,0.0522
p,24.999999975000001,0.5,0.2,TRUE,FALSE,0.49998566527286392,7.74e-06
p,24.999999975000001,0.5,0.2,FALSE,FALSE,0.50001433472713608,7.74e-06
d,25.000000000000028,-1,0.2,NA,TRUE,5.2860627101205528,0.00338
p,25.000000000000028,-1,0.2,TRUE,FALSE,0.50007066805181932,1.12e-11
p,25.000000000000028,-1,0.2,FALSE,FALSE,0.49992933194818062,1.12e-11
d,25.000000025000002,-1,0.2,NA,TRUE,5.0421916849825976,0.0522
p,25.000000025000002,-1,0.2,TRUE,FALSE,0.50007471293945527,7.74e-06
p,25.000000025000002,-1,0.2,FALSE,FALSE,0.49992528706054479,7.74e-06
d,25.024999999999999,-1,0.2,NA,TRUE,1.1691473005023834,0.826
p,25.024999999999999,-1,0.2,TRUE,FALSE,0.69854539109943359,0.115
p,25.024999999999999,-1,0.2,FALSE,FALSE,0.30145460890056647,0.267
d,24.999999975000001,-1,0.2,NA,TRUE,5.0421916962892812,0.0522
p,24.999999975000001,-1,0.2,TRUE,FALSE,0.50006662315346384,7.74e-06
p,24.999999975000001,-1,0.2,FALSE,FALSE,0.4999333768465361,7.74e-06
d,25.000000000000028,0,0.2,NA,TRUE,5.2859213840151815,0.00338
p,25.000000000000028,0,0.2,TRUE,FALSE,0.50000000000563016,1.12e-11
p,25.000000000000028,0,0.2,FALSE,FALSE,0.49999999999436984,1.12e-11
d,25.000000025000002,0,0.2,NA,TRUE,5.0420503598511504,0.0522
p,25.000000025000002,0,0.2,TRUE,FALSE,0.50000404432166012,7.74e-06
p,25.000000025000002,0,0.2,FALSE,FALSE,0.49999595567833988,7.74e-06
d,25.024999999999999,0,0.2,NA,TRUE,1.1695925153212792,0.826
p,25.024999999999999,0,0.2,TRUE,FALSE,0.69848692877310892,0.115
p,25.024999999999999,0,0.2,FALSE,FALSE,0.30151307122689108,0.267
d,24.999999975000001,0,0.2,NA,TRUE,5.0420503692099841,0.0522
p,24.999999975000001,0,0.2,TRUE,FALSE,0.49999595567888594,7.74e-06
p,24.999999975000001,0,0.2,FALSE,FALSE,0.50000404432111412,7.74e-06
d,25.000000000000028,0.5,0.5,NA,TRUE,0.78430746877655466,2.5e-07
p,25.000000000000028,0.5,0.5,TRUE,FALSE,0.49999999555298319,1.25e-13
p,25.000000000000028,0.5,0.5,FALSE,FALSE,0.50000000444701687,1.25e-13
d,25.000000025000002,0.5,0.5,NA,TRUE,0.78383989786791097,0.000234
p,25.000000025000002,0.5,0.5,TRUE,FALSE,0.50000005030809314,1.09e-07
p,25.000000025000002,0.5,0.5,FALSE,FALSE,0.49999994969190681,1.09e-07
d,25.024999999999999,0.5,0.5,NA,TRUE,0.31579723692091738,0.234
p,25.024999999999999,0.5,0.5,TRUE,FALSE,0.5403315187146398,0.0635
p,25.024999999999999,0.5,0.5,FALSE,FALSE,0.4596684812853602,0.0746
d,24.999999975000001,0.5,0.5,NA,TRUE,0.78383989890105221,0.000234
p,24.999999975000001,0.5,0.5,TRUE,FALSE,0.49999994079775639,1.09e-07
p,24.999999975000001,0.5,0.5,FALSE,FALSE,0.50000005920224366,1.09e-07
d,25.000000000000028,-1,0.5,NA,TRUE,0.78431040698792198,2.5e-07
p,25.000000000000028,-1,0.5,TRUE,FALSE,0.50000147355499724,1.25e-13
p,25.000000000000028,-1,0.5,FALSE,FALSE,0.49999852644500276,1.25e-13
d,25.000000025000002,-1,0.5,NA,TRUE,0.78384283457945536,0.000234
p,25.000000025000002,-1,0.5,TRUE,FALSE,0.50000152831026812,1.09e-07
p,25.000000025000002,-1,0.5,FALSE,FALSE,0.49999847168973194,1.09e-07
d,25.024999999999999,-1,0.5,NA,TRUE,0.31447630088417988,0.234
p,25.024999999999999,-1,0.5,TRUE,FALSE,0.54030741716603481,0.0634
p,25.024999999999999,-1,0.5,FALSE,FALSE,0.45969258283396514,0.0746
d,24.999999975000001,-1,0.5,NA,TRUE,0.78384283861224568,0.000234
p,24.999999975000001,-1,0.5,TRUE,FALSE,0.50000141879960947,1.09e-07
p,24.999999975000001,-1,0.5,FALSE,FALSE,0.49999858120039048,1.09e-07
d,25.000000000000028,0,0.5,NA,TRUE,0.78430745988239592,2.5e-07
p,25.000000000000028,0,0.5,TRUE,FALSE,0.50000000000006228,1.25e-13
p,25.000000000000028,0,0.5,FALSE,FALSE,0.49999999999993772,1.25e-13
d,25.000000025000002,0,0.5,NA,TRUE,0.78383988847381125,0.000234
p,25.000000025000002,0,0.5,TRUE,FALSE,0.50000005475517173,1.09e-07
p,25.000000025000002,0,0.5,FALSE,FALSE,0.49999994524482821,1.09e-07
d,25.024999999999999,0,0.5,NA,TRUE,0.31535594878389944,0.234
p,25.024999999999999,0,0.5,TRUE,FALSE,0.54032295445727363,0.0635
p,25.024999999999999,0,0.5,FALSE,FALSE,0.45967704554272643,0.0746
d,24.999999975000001,0,0.5,NA,TRUE,0.78383989050683556,0.000234
p,24.999999975000001,0,0.5,TRUE,FALSE,0.49999994524483593,1.09e-07
p,24.999999975000001,0,0.5,FALSE,FALSE,0.50000005475516407,1.09e-07", stringsAsFactors = FALSE)
expect_true(!any(in_gamlss_region(as.numeric(mp_cusp$arg), 25, 0.05, mp_cusp$nu, mp_cusp$tau, "p")),
            info = "design: tau < 1 is outside the gamlss region for d and p")
cu_got <- numeric(nrow(mp_cusp))
for (k in seq_len(nrow(mp_cusp))) {
    r <- mp_cusp[k, ]
    cu_got[k] <- if (r$fn == "d") fdBCPE(r$arg, 25, 0.05, r$nu, r$tau, log_ = TRUE)
                 else fpBCPE(r$arg, 25, 0.05, r$nu, r$tau, lower_tail = as.logical(r$lower_tail))
}
cu_tol <- 2e-13 + K_ULP * eps * mp_cusp$kz
cu_scale <- ifelse(mp_cusp$fn == "d", pmax(1, abs(mp_cusp$value)), abs(mp_cusp$value))
for (tt in unique(mp_cusp$tau)) for (fn in c("d", "pL", "pU")) {
    k <- mp_cusp$tau == tt & (if (fn == "d") mp_cusp$fn == "d" else mp_cusp$fn == "p" & mp_cusp$lower_tail == (fn == "pL"))
    what <- c(d = "log density", pL = "F", pU = "U")[[fn]]
    expect_within(cu_got[k], mp_cusp$value[k], cu_tol[k], scale = cu_scale[k],
                  info = sprintf("tau %g < 1 near the median: %s against mpmath (stable path)", tt, what))
}
# and F agrees with the log-scale path there (both stable), on a finer sweep of offsets
cl_y <- 25 * (1 + c(1e-15, 1e-13, 1e-11, 1e-9, 1e-7, 1e-5, 1e-3, -1e-3, -1e-7, -1e-12))
for (pr in list(c(0.05, 0.5, 0.1), c(0.05, 0.5, 0.2), c(0.05, 0.5, 0.5), c(0.05, -1, 0.3), c(0.2, 0.08, 0.7), c(0.05, 0, 0.05))) {
    expect_within(fpBCPE(cl_y, 25, pr[1], pr[2], pr[3]), exp(fpBCPE(cl_y, 25, pr[1], pr[2], pr[3], log_p = TRUE)), 5e-13,
                  info = sprintf("tau %g < 1 near the median: F = exp(log F), sigma %g nu %g", pr[3], pr[1], pr[2]))
    expect_within(fpBCPE(cl_y, 25, pr[1], pr[2], pr[3]) + fpBCPE(cl_y, 25, pr[1], pr[2], pr[3], lower_tail = FALSE), rep(1, 10), 4e-13,
                  info = sprintf("tau %g < 1 near the median: F + U = 1, sigma %g nu %g", pr[3], pr[1], pr[2]))
}

# monotonicity, with a 1e-13 relative wobble allowance at a path switch: q sweeps that cross the
# gamlss-region boundaries (F = 2^-8, the truncation condition) and p sweeps across 2^-8, 1/2,
# 1 - 2^-8 and the w = 1/2 boundary
mono_pars <- list(c(25, 0.15, 0.5, 2), c(25, 0.3, 1.5, 1.4), c(25, 0.3, -1.5, 3.5), c(30, 0.05, 0.003, 2),
                  c(20, 0.2, 0, 0.7), c(25, 1.5, 2, 2), c(25, 0.15, -6, 1.2))
for (pr in mono_pars) {
    lab <- sprintf("mu %g sigma %g nu %g tau %g", pr[1], pr[2], pr[3], pr[4])
    qa <- fqBCPE(c(1e-12, 1 - 1e-12), pr[1], pr[2], pr[3], pr[4])
    qs <- exp(seq(log(qa[1]), log(qa[2]), length.out = 4000))
    for (lt in c(TRUE, FALSE)) {
        Fs <- fpBCPE(qs, pr[1], pr[2], pr[3], pr[4], lower_tail = lt)
        dF <- diff(Fs) * (if (lt) 1 else -1)
        expect_true(all(dF >= -1e-13 * pmax(Fs[-1], Fs[-length(Fs)])),
                    info = sprintf("F monotone in q (lower_tail %s), %s; worst step %.3g", lt, lab, min(dF / pmax(Fs[-1], 1e-300))))
        lFs <- fpBCPE(qs, pr[1], pr[2], pr[3], pr[4], lower_tail = lt, log_p = TRUE)
        dl <- diff(lFs) * (if (lt) 1 else -1)
        expect_true(all(dl >= -1e-13 * pmax(1, abs(lFs[-1]))), info = sprintf("log F monotone in q (lower_tail %s), %s", lt, lab))
    }
    ps <- sort(c(seq(1e-6, 1 - 1e-6, length.out = 3000), 2^-8 * (1 + (-50:50) * 1e-10), 1 - 2^-8 * (1 + (-50:50) * 1e-10),
                 0.5 + (-50:50) * 1e-14))
    for (lt in c(TRUE, FALSE)) {
        Qs <- fqBCPE(ps, pr[1], pr[2], pr[3], pr[4], lower_tail = lt)
        dQ <- diff(Qs) * (if (lt) 1 else -1)
        expect_true(all(dQ >= -1e-13 * pmax(Qs[-1], Qs[-length(Qs)])),
                    info = sprintf("Q monotone in p (lower_tail %s), %s; worst step %.3g", lt, lab, min(dQ / Qs[-1])))
    }
    lps <- sort(c(-10^seq(-15, 3, length.out = 2000), -LN2 + (-50:50) * 1e-14))
    Ql <- fqBCPE(lps, pr[1], pr[2], pr[3], pr[4], log_p = TRUE)
    expect_true(all(diff(Ql) >= -1e-13 * Ql[-1]), info = sprintf("Q monotone in log p (across -log 2), %s", lab))
}

# ----------------------------------------------------------------------------
# tau AND sigma EXTREMES, and the corners of the supported domain
# ----------------------------------------------------------------------------

# no NaN, no warning, quantiles in [0, Inf] and monotone, F in [0, 1], at the corners
corner <- expand.grid(mu = c(1e-6, 1e6), sigma = c(1e-3, 10), nu = c(-10, -1e-300, 0, 1e-300, 10), tau = c(0.05, 100))
pc <- c(0, 1e-300, 1e-100, 1e-10, 0.01, 0.3, 0.5, 0.7, 0.99, 1 - 1e-10, 1)
lpc <- c(-Inf, -1e5, -700, -40, -1, -LN2, -0.1, -1e-10, -1e-300, 0)
for (i in seq_len(nrow(corner))) {
    cr <- corner[i, ]
    lab <- sprintf("mu %g sigma %g nu %g tau %g", cr$mu, cr$sigma, cr$nu, cr$tau)
    w <- warns_of(list(
        q1 = fqBCPE(pc, cr$mu, cr$sigma, cr$nu, cr$tau), q2 = fqBCPE(pc, cr$mu, cr$sigma, cr$nu, cr$tau, lower_tail = FALSE),
        q3 = fqBCPE(lpc, cr$mu, cr$sigma, cr$nu, cr$tau, log_p = TRUE),
        q4 = fqBCPE(lpc, cr$mu, cr$sigma, cr$nu, cr$tau, lower_tail = FALSE, log_p = TRUE)))
    qq <- unlist(w$value)
    expect_true(!any(is.na(qq)) && all(qq >= 0) && length(w$warnings) == 0L,
                info = paste0(conv("quantile >= 0"), num("tau extremes"), "quantiles in [0, Inf], no NaN, no warning, ", lab))
    expect_true(mono_ok(w$value$q1) && mono_ok(rev(w$value$q2)) && mono_ok(w$value$q3) && mono_ok(rev(w$value$q4)),
                info = paste0(num("tau extremes"), "quantiles monotone in p at the corner, ", lab))
    ys <- unique(sort(c(w$value$q1, cr$mu * c(0.5, 1, 2))))
    ys <- ys[ys > 0 & is.finite(ys)]
    wp <- warns_of(list(F = fpBCPE(ys, cr$mu, cr$sigma, cr$nu, cr$tau), lF = fpBCPE(ys, cr$mu, cr$sigma, cr$nu, cr$tau, log_p = TRUE),
                        U = fpBCPE(ys, cr$mu, cr$sigma, cr$nu, cr$tau, lower_tail = FALSE),
                        ld = fdBCPE(ys, cr$mu, cr$sigma, cr$nu, cr$tau, log_ = TRUE)))
    expect_true(length(wp$warnings) == 0L && !any(is.na(unlist(wp$value))) && all(wp$value$F >= 0 & wp$value$F <= 1 + 4 * eps) &&
                    all(wp$value$lF <= 0) && mono_ok(wp$value$F),
                info = paste0(num("tau extremes"), "F in [0, 1], monotone, no NaN, no warning, ", lab))
}
# sigma = 1e-3 and 10, tau = 0.05 and 100: quantile and CDF invert each other in the body
for (pr in list(c(25, 1e-3, 0.7, 2), c(25, 10, 0.05, 2), c(25, 10, -0.05, 1.5), c(25, 0.15, 0.5, 0.05), c(25, 0.15, 0.5, 100),
                c(25, 0.15, -2, 0.05), c(25, 0.15, -2, 100))) {
    lab <- sprintf("mu %g sigma %g nu %g tau %g", pr[1], pr[2], pr[3], pr[4])
    pp <- c(0.001, 0.05, 0.3, 0.5, 0.7, 0.95, 0.999)
    qq <- fqBCPE(pp, pr[1], pr[2], pr[3], pr[4])
    # through the stable (log-scale and upper-tail) paths; the tolerance scales with the
    # elasticities (for tau = 0.05 the body of T sits in |z| < 1e-2 around a spike: a one-ulp
    # change of y near the median moves F by ~1e-5, so the round trip there is ill-conditioned)
    lF_q <- fpBCPE(qq, pr[1], pr[2], pr[3], pr[4], log_p = TRUE)
    lU_q <- fpBCPE(qq, pr[1], pr[2], pr[3], pr[4], lower_tail = FALSE, log_p = TRUE)
    lyf_q <- log(qq) + fdBCPE(qq, pr[1], pr[2], pr[3], pr[4], log_ = TRUE)
    l_q <- abs(log(qq / pr[1]))
    tg <- num(if (pr[4] %in% c(0.05, 100)) "tau extremes" else "sigma extremes")
    expect_within(lF_q, log(pp), 1e-12 + K_ULP * eps * exp(lyf_q - lF_q) * (1 + l_q), relative = FALSE,
                  info = paste0(tg, "log F(Q(p)) = log p, ", lab))
    expect_within(exp(lU_q), 1 - pp, 1e-12 + K_ULP * eps * exp(lyf_q - lU_q) * (1 + l_q),
                  info = paste0(tg, "U(Q(p)) = 1 - p, ", lab))
}

# ----------------------------------------------------------------------------
# frBCPE: random generation
# ----------------------------------------------------------------------------

expect_identical(length(frBCPE(7, 25, 0.15, 0.5, 2)), 7L, info = "frBCPE(7) has length 7")
expect_identical(length(frBCPE(2.3, 25, 0.15, 0.5, 2)), 3L, info = "a non-integer n is rounded up")
expect_identical(length(frBCPE(1)), 1L, info = "frBCPE(1) works with the default parameters")
r_few <- frBCPE(500, 25, 0.15, -0.7, 1.5)
expect_true(is.double(r_few) && all(is.finite(r_few) & r_few > 0), info = "draws are finite, positive doubles")
with_seed <- function(seed, expr) {
    dqrng::dqset.seed(seed)
    expr
}
expect_identical(with_seed(4242, frBCPE(500, 25, 0.15, -0.7, 1.5)), with_seed(4242, frBCPE(500, 25, 0.15, -0.7, 1.5)),
                 info = paste0(conv("dqrng"), "frBCPE is reproducible under dqrng::dqset.seed"))
expect_true(!identical(with_seed(4242, frBCPE(500, 25, 0.15, -0.7, 1.5)), with_seed(4243, frBCPE(500, 25, 0.15, -0.7, 1.5))),
            info = paste0(conv("dqrng"), "a different dqrng seed gives different draws"))
expect_identical(with_seed(99, frBCPE(500, 25, 0.15, -0.7, 1.5)), with_seed(99, fqBCPE(dqrng::dqrunif(500), 25, 0.15, -0.7, 1.5)),
                 info = paste0(conv("dqrng"), "frBCPE(n) is fqBCPE(dqrng::dqrunif(n))"))
expect_identical(with_seed(100, frBCPE(100, mu = c(20, 30), sigma = c(0.1, 0.2), nu = c(-1, 1), tau = c(1.5, 3))),
                 with_seed(100, fqBCPE(dqrng::dqrunif(100), mu = c(20, 30), sigma = c(0.1, 0.2), nu = c(-1, 1), tau = c(1.5, 3))),
                 info = paste0(conv("dqrng"), conv("recycling"), "frBCPE recycles vector parameters like fqBCPE"))
expect_identical(with_seed(101, frBCPE(50)), with_seed(101, fqBCPE(dqrng::dqrunif(50), 5, 0.1, 1, 2)),
                 info = paste0(conv("dqrng"), "frBCPE's defaults are mu 5, sigma 0.1, nu 1, tau 2"))
# the empirical CDF of 2e4 draws matches fpBCPE within 5 standard errors at five points
dqrng::dqset.seed(20261017)
rr <- frBCPE(2e4, 25, 0.2, -0.8, 1.6)
qk <- fqBCPE(c(0.02, 0.25, 0.5, 0.75, 0.98), 25, 0.2, -0.8, 1.6)
Fk <- fpBCPE(qk, 25, 0.2, -0.8, 1.6)
ek <- sapply(qk, function(q) mean(rr <= q))
expect_true(all(abs(ek - Fk) < 5 * sqrt(Fk * (1 - Fk) / 2e4)),
            info = sprintf("empirical CDF of frBCPE vs fpBCPE (max |diff| / se = %.2f)", max(abs(ek - Fk) / sqrt(Fk * (1 - Fk) / 2e4))))

# ----------------------------------------------------------------------------
# BCPEo: the same functions, identical() on a grid with edges, both tails and scales
# ----------------------------------------------------------------------------

o_x <- c(-1, 0, 1e-300, 0.01, 5, 20, 25, 33, 80, 1e6, 1e300, Inf, NA)
o_p <- c(0, 1e-300, 1e-10, 0.003, 0.2, 0.5, 0.8, 0.997, 1 - 1e-10, 1, NA)
o_lp <- c(-Inf, -1e5, -50, -1, -LN2, -0.1, -1e-20, 0, NaN)
for (pr in list(c(25, 0.15, 0.6, 2), c(25, 0.15, -1.5, 1), c(25, 0.15, 0, 3), c(7, 1.2, 4, 0.4), c(25, 0.15, 1e-13, 2.5))) {
    lab <- sprintf("mu %g sigma %g nu %g tau %g", pr[1], pr[2], pr[3], pr[4])
    for (lg in c(FALSE, TRUE)) {
        expect_identical(fdBCPEo(o_x, pr[1], pr[2], pr[3], pr[4], lg), fdBCPE(o_x, pr[1], pr[2], pr[3], pr[4], lg),
                         info = paste0(conv("BCPEo"), "fdBCPEo == fdBCPE, log_ ", lg, ", ", lab))
    }
    for (lt in c(TRUE, FALSE)) for (lp in c(FALSE, TRUE)) {
        expect_identical(fpBCPEo(o_x, pr[1], pr[2], pr[3], pr[4], lt, lp), fpBCPE(o_x, pr[1], pr[2], pr[3], pr[4], lt, lp),
                         info = paste0(conv("BCPEo"), "fpBCPEo == fpBCPE, lower_tail ", lt, " log_p ", lp, ", ", lab))
        pv <- if (lp) o_lp else o_p
        expect_identical(fqBCPEo(pv, pr[1], pr[2], pr[3], pr[4], lt, lp), fqBCPE(pv, pr[1], pr[2], pr[3], pr[4], lt, lp),
                         info = paste0(conv("BCPEo"), "fqBCPEo == fqBCPE, lower_tail ", lt, " log_p ", lp, ", ", lab))
    }
}
expect_identical(with_seed(7, frBCPEo(300, 25, 0.15, -0.7, 1.5)), with_seed(7, frBCPE(300, 25, 0.15, -0.7, 1.5)),
                 info = paste0(conv("BCPEo"), conv("dqrng"), "frBCPEo draws what frBCPE draws"))
expect_identical(err_msg(fpBCPEo(20, 25, 0.15, 0.5, 0)), "tau must be positive",
                 info = paste0(conv("BCPEo"), conv("error messages"), "fpBCPEo validates like fpBCPE"))

# ----------------------------------------------------------------------------
# THE IMPACTncd USE CASE: draws truncated to [min_value, max_value] from stratified ranks
#
# The model computes minq = pfun(min_value) and maxq = pfun(max_value) and draws
# fq(clamp(minq + u * (maxq - minq))) for the stratified ranks u = (i - 0.5) / N.
# ----------------------------------------------------------------------------

N <- 1e4
u_i <- (seq_len(N) - 0.5) / N
for (pr in list(c(27, 0.17, -0.6, 2.3, 15, 70), c(5.2, 0.25, 0.4, 1.6, 2, 12), c(120, 0.12, 1.1, 3.2, 70, 160))) {
    lab <- sprintf("mu %g sigma %g nu %g tau %g, [%g, %g]", pr[1], pr[2], pr[3], pr[4], pr[5], pr[6])
    minq <- fpBCPE(pr[5], pr[1], pr[2], pr[3], pr[4])
    maxq <- fpBCPE(pr[6], pr[1], pr[2], pr[3], pr[4])
    expect_true(minq > 0 && maxq < 1, info = paste("design: the truncation bounds are inside (0, 1),", lab))
    pd <- pmin(pmax(minq + u_i * (maxq - minq), 1e-12), 1 - 1e-12)
    d_i <- fqBCPE(pd, pr[1], pr[2], pr[3], pr[4])
    expect_true(all(is.finite(d_i)) && all(d_i >= pr[5] * (1 - 1e-12)) && all(d_i <= pr[6] * (1 + 1e-12)),
                info = paste("draws lie in [min_value, max_value],", lab))
    expect_true(!is.unsorted(d_i), info = paste("draws are non-decreasing in the rank,", lab))
    if (requireNamespace("gamlss.dist", quietly = TRUE)) {
        same <- mean(d_i == gamlss.dist::qBCPE(pd, pr[1], pr[2], pr[3], pr[4]))
        expect_true(same >= 0.98, info = sprintf("%.4f of the draws are identical() to gamlss.dist::qBCPE's, %s", same, lab))
    }
}
# min_value = 0 (or below the support): minq is exactly 0 and the draws start at the bottom
expect_identical(fpBCPE(c(0, -1), 27, 0.17, -0.6, 2.3), c(0, 0), info = "minq = F(0) = F(-1) = 0 exactly")
d0 <- fqBCPE(0 + u_i * (fpBCPE(70, 27, 0.17, -0.6, 2.3) - 0), 27, 0.17, -0.6, 2.3)
expect_true(all(d0 > 0 & d0 <= 70 * (1 + 1e-12)) && !is.unsorted(d0), info = "with minq = 0 the draws are in (0, max_value], sorted")

# ----------------------------------------------------------------------------
# THE ORACLE TABLE (bcpe_reference.csv, mpmath values to 50 digits): every row within its rtol
# (|got/value - 1| <= rtol for natural-scale values, quantiles and log-scale p (the relative criterion
# for log probabilities: max(1, |value|) would be vacuous for values near 0);
# |got - value| <= rtol * max(1, |value|) for the log density). A value of 0 or +-Inf, and every
# edge row (rtol 0), must be matched exactly.
# ----------------------------------------------------------------------------

csv <- "bcpe_reference.csv"
expect_true(file.exists(csv), info = "bcpe_reference.csv is next to this file")
if (file.exists(csv)) {
    orc <- read.csv(csv, stringsAsFactors = FALSE)
    expect_true(nrow(orc) > 100 && all(c("id", "fn", "arg", "mu", "sigma", "nu", "tau", "lower_tail", "log", "value",
                                         "rtol", "class") %in% names(orc)), info = "design: the oracle table has its columns")
    # the regimes added after the first table (each fails on a build that lacks the code it guards):
    #   tau_large_median        tau 10 .. 100 with y within 1e-15 .. 1e-1 of mu: s = 0.5 (|z|/c)^tau is below 1e-21 or
    #                           underflows, the lower gamma comes from t itself (G = 2 f_T(0) t), and gamlss's own s
    #                           underflows, so the gamlss path must not be taken (it gives F = 0.5, 1e-6 .. 4e-4 off)
    #   w_overflow              tau < 1 with w = (y/mu)^nu beyond the double range: z overflows while s and the log
    #                           mass (-1e17 .. -1e210) are finite
    #   qgamma_band             the Newton polish of R's qgamma, tails 1e-14 .. 3.5e-12 (shape 1/tau != 1)
    #   powerlaw_deep_tau_large tau 60 .. 100, deep tails on the truncation side (nu > 0 lower, nu < 0 upper), where
    #                           the quantile solves for the gap h by Newton in log h from an already converged start
    #                           (an iterate equal to the bracket end was discarded, the 60-iteration cap was hit, and
    #                           quantiles came out with relative errors up to 8e10)
    need <- c(tau_large_median = 70, w_overflow = 60, qgamma_band = 70, powerlaw_deep_tau_large = 70)
    expect_true(all(table(factor(orc$class, names(need))) >= need),
                info = "design: the oracle table has its tau_large_median, w_overflow, qgamma_band and powerlaw_deep_tau_large rows")
    pdl <- orc[orc$class == "powerlaw_deep_tau_large", ]
    expect_true(all(pdl$fn == "q") && all(pdl$tau >= 60) && any(pdl$nu > 0) && any(pdl$nu < 0) && any(pdl$lower_tail %in% TRUE) &&
                    any(pdl$lower_tail %in% FALSE) && any(pdl$log %in% TRUE) && any(pdl$log %in% FALSE),
                info = "design: powerlaw_deep_tau_large covers both nu signs, both tails and both scales")
    orc_tag <- function(cls) {
        if (cls == "qgamma_band") dep("qgamma polish")
        else if (cls %in% c("w_overflow", "powerlaw_deep_tau_large", "powerlaw_lower", "powerlaw_upper")) dep("power-law regime")
        else if (cls %in% c("tau_large_median", "tau_small_median")) num("tau extremes")
        else ""
    }
    orc$got <- eval_ref_rows(orc)
    lgs <- as.logical(orc$log) & orc$fn == "d"
    v <- orc$value
    e <- ifelse(lgs, abs(orc$got - v) / pmax(1, abs(v)), abs(orc$got / v - 1))
    e[orc$got == v] <- 0
    sub <- !lgs & v != 0 & abs(v) < 2.2250738585072014e-308      # subnormal natural values: one subnormal step
    e[sub] <- abs(orc$got[sub] - v[sub]) / (orc$rtol[sub] * abs(v[sub]) + 2 * 4.9406564584124654e-324) * orc$rtol[sub]
    e[is.na(e)] <- Inf
    orc$ratio <- ifelse(e == 0, 0, e / orc$rtol)
    orc$ratio[is.na(orc$ratio)] <- Inf
    for (g in split(orc, paste(orc$fn, orc$class))) {
        worst <- head(g[order(-g$ratio), ], 3)
        expect_true(all(g$ratio <= 1),
                    info = sprintf("%soracle %s / %s: %d rows, %d over rtol; worst: %s", orc_tag(g$class[1]), g$fn[1], g$class[1], nrow(g), sum(g$ratio > 1),
                                   paste(sprintf("id %s (%s, %g, %g, %g, %g, lt %s, log %s) got %.17g want %.17g err/rtol %.3g",
                                                 worst$id, format(worst$arg, digits = 17), worst$mu, worst$sigma, worst$nu, worst$tau,
                                                 worst$lower_tail, worst$log, worst$got, worst$value, worst$ratio), collapse = "; ")))
    }
}

# ----------------------------------------------------------------------------
# THE "NaNs produced" CONVENTION: one warning per call for a NaN out of non-NaN inputs; valid
# inputs (also far outside the supported domain) give no NaN at all, or NaN with that one warning
# ----------------------------------------------------------------------------

nan_ok <- function(w) {
    v <- unlist(w$value)
    (!any(is.nan(v)) && length(w$warnings) == 0L) || (any(is.nan(v)) && identical(w$warnings, "NaNs produced"))
}
ext <- expand.grid(mu = c(1e-300, 1, 1e300), sigma = c(1e-300, 1e-8, 1e8, 1e300), nu = c(-1e300, -1e-8, 1e-320, 1e300),
                   tau = c(1e-300, 1e-4, 1e4, 1e300))
for (i in seq_len(nrow(ext))) {
    ex <- ext[i, ]
    lab <- sprintf("mu %g sigma %g nu %g tau %g", ex$mu, ex$sigma, ex$nu, ex$tau)
    xs <- ex$mu * c(1e-10, 0.5, 1, 2, 1e10)
    ok4 <- c(nan_ok(warns_of(fdBCPE(xs, ex$mu, ex$sigma, ex$nu, ex$tau))),
             nan_ok(warns_of(fdBCPE(xs, ex$mu, ex$sigma, ex$nu, ex$tau, log_ = TRUE))),
             nan_ok(warns_of(fpBCPE(xs, ex$mu, ex$sigma, ex$nu, ex$tau))),
             nan_ok(warns_of(fpBCPE(xs, ex$mu, ex$sigma, ex$nu, ex$tau, FALSE, TRUE))))
    expect_true(all(ok4), info = paste0(conv("NaN warning"), "d/p far outside the supported domain: no NaN, or one warning per call, ", lab))
    wq <- warns_of(fqBCPE(c(1e-300, 0.1, 0.5, 0.9, 1 - 1e-12), ex$mu, ex$sigma, ex$nu, ex$tau))
    expect_true(nan_ok(wq) && all(is.nan(wq$value) | wq$value >= 0),
                info = paste0(conv("NaN warning"), conv("quantile >= 0"), "q far outside the supported domain, ", lab))
}
for (f in list(fdBCPE, fpBCPE, fqBCPE)) {
    expect_identical(warns_of(f(c(NA, NaN), 25, 0.15, 0.5, 2))$warnings, character(),
                     info = paste0(conv("NaN warning"), "NA/NaN in the first argument is silent"))
    expect_identical(warns_of(f(0.5, NA, 0.15, 0.5, 2))$warnings, character(),
                     info = paste0(conv("NaN warning"), "NA mu is silent"))
    expect_identical(warns_of(f(0.5, 25, 0.15, 0.5, 2))$warnings, character(), info = paste0(conv("NaN warning"), "an ordinary call is silent"))
}
