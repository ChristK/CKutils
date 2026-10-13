/* CKutils: an R package with some utility functions I use regularly
Copyright (C) 2025  Chris Kypridemos

CKutils is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see <http://www.gnu.org/licenses/>
or write to the Free Software Foundation, Inc., 51 Franklin Street,
Fifth Floor, Boston, MA 02110-1301  USA. */

#ifndef DISTR_BCPE_H
#define DISTR_BCPE_H

// Header-only scalar API for the Box-Cox Power Exponential (BCPE) distribution of
// gamlss.dist (Rigby and Stasinopoulos, 2004): mu > 0 (the median), sigma > 0,
// nu real, tau > 0; support y > 0. With z the Box-Cox transform of y,
//   z = ((y / mu)^nu - 1) / (nu * sigma)   (nu != 0),   log(y / mu) / sigma   (nu = 0),
// Z is a power exponential T with unit variance (shape tau; tau = 2 is the normal,
// tau = 1 the Laplace) truncated to the range of the transform: z > -1/(sigma*nu)
// for nu > 0, z < 1/(sigma*|nu|) for nu < 0. gamlss.dist's BCPEo is the same
// distribution (its d/p/q/r functions are identical to BCPE's; the two gamlss
// families differ only in the link for mu), so distr_BCPEo.h aliases these kernels.
//
// The *_scalar functions below are defined `inline` so that a downstream package
// can `LinkingTo: CKutils`, `#include <distr_BCPE.h>` and call them directly from
// its own C++ (e.g. in a hot per-row loop) without linking against CKutils.so. The
// vectorised, Rcpp-exported wrappers (declared at the bottom) live in
// src/distr_BCPE.cpp and call these same inline scalars. They use R's own gamma
// functions (R::lgammafn, R::lgamma1p, R::pgamma, R::qgamma) and ::R_pow, so they
// must be called from a thread that may call into R. Every helper is prefixed
// bcpe_ and every constant BCPE_, so this header can be included next to the
// other distr_*.h headers.
//
// BCPE is CONTINUOUS: x, q and the returned quantile are doubles. The count caller
// contract documented in recycling_helpers.h (0 <= x, q <= CK_MAX_COUNT) does not
// apply here, and nothing is converted to int.
//
// Like the other kernels these do no validation of the parameters; the caller
// owns mu > 0, sigma > 0, tau > 0, all four finite (the R-facing wrappers check
// them; nu has no range). Outside that contract a kernel returns NaN or a
// plausible but wrong number, and never hangs (every iteration is capped). The
// kernels do define the edges of x, q and p:
//   - NaN (NA) in any argument gives NA_REAL;
//   - density: x <= 0 (incl. -Inf) -> 0 (log: -Inf), x == +Inf -> 0 (log: -Inf);
//   - CDF: q <= 0 (incl. -Inf) is below the support (F = 0, upper tail 1) and
//     q == +Inf beyond it (F = 1, upper tail 0), each on the requested tail and
//     scale;
//   - quantile: p = 0 -> 0 and p = 1 -> Inf on the lower tail (log_p: -Inf -> 0,
//     0 -> Inf); on the upper tail p = 0 -> Inf, p = 1 -> 0 (log_p: -Inf -> Inf,
//     0 -> 0). A p outside [0, 1] (p > 0 on the log scale) gives NaN. A quantile
//     is in [0, Inf] and never negative; for valid inputs inside the supported
//     domain (below) no kernel returns NaN. It underflows to 0 or overflows to Inf
//     where the true value is outside the double range (one exception: see "Limits").
//
// Supported domain of the accuracy claims below: mu in [1e-6, 1e6], sigma in
// [1e-3, 10], nu in [-10, 10] (including |nu| down to 1e-300, and 0), tau in
// [0.05, 100]; x and q in (0, Inf); p in (0, 1) and log p down to -1e5. Outside
// it nothing crashes or hangs and no quantile is negative, but a contract-valid
// yet absurd input (e.g. tau = 5e-324, where the constants of T are meaningless)
// can give NaN; the exported wrappers then warn "NaNs produced".
//
// Two paths.
// 1. The gamlss path. For the natural scale -- the density (log_p FALSE and TRUE),
//    the lower-tail CDF and the lower-tail quantile -- every element inside the
//    "gamlss region" is computed by gamlss.dist's own arithmetic, operation for
//    operation (R_pow for every ^, R's left-to-right association, the same
//    lgamma/pgamma/qgamma calls), so it is identical() to dBCPE/pBCPE/qBCPE and a
//    model that switches from qBCPE() reproduces its draws. The region is where
//    that arithmetic is accurate (within ~40 eps times the condition number of the
//    point, i.e. ~2e-13 where it is well conditioned; see "Limits" for small sigma):
//      (G-d) density: nu == 0 or |nu| * min(1, sigma) >= 2^-8 (the forward Box-Cox
//            transform keeps its digits), and tau >= 1 (no cusp of f_T at 0 for
//            gamlss's z error to meet);
//      (G-p) CDF: G-d, and for z < 0 gamlss's lower T tail >= 2^-8, and for nu > 0
//            the truncation F_T(z) - F_T(-b) keeps at least half of F_T(z);
//      (G-q'') quantile: for nu != 0 the base w = 1 + nu*sigma*za of the power with
//            |nu| * min(1, w) >= 2^-8 (w = 1 for nu == 0), and gamlss's T target pt
//            with min(pt, 1 - pt) >= 2^-8 * max(1, sigma/w);
//    and in each, no intermediate (y/mu, c, the gamma argument s) outside the normal
//    double range, and no CDF above 1 (gamlss's quotient can round to 1 + 2^-52).
//    The thresholds are the named BCPE_GAMLSS_* constants below.
//    gamlss's exp() calls follow the exp() of the R this header is compiled against
//    (bcpe_g_exp): the results are identical() to gamlss.dist's except possibly in the
//    last bit where exp()'s argument is below 2^-26 in absolute value, where
//    gamlss.dist itself differs between R versions and with the number of rows in the
//    call.
// 2. The stable path, everywhere else: every upper tail, every log scale (and the
//    log density outside the region), and the natural-scale elements outside it.
//    Its relative error is a few ulp times the condition number of the point: the
//    Box-Cox transform from ell = log(y/mu) (log1p((y - mu)/mu) near y = mu) as
//    z = (ell/sigma) * expm1(nu*ell) / (nu*ell) (no division by a tiny nu*sigma;
//    this is also the nu = 0 formula);
//    T tails directly, 0.5 * pgamma(s, 1/tau, lower = FALSE), never
//    0.5 * (1 + G * sign(t)); the masses of Z as sums of such tails, never 1 - F;
//    next to the truncation point the "between" mass P(b - h < T < b) from the
//    accurately known gap h = b * (y/mu)^nu (a difference that loses at most a bit,
//    or Gauss-Legendre quadrature in v = log(b/t)), so the power-law tail of Y there
//    keeps its relative accuracy far below the double range on the log scale;
//    log F = log1p(-U) where F > 1/2 (and the converse); quantiles invert the
//    smaller tail mass, with qgamma on a probability <= 1/2 on its own side, polished
//    by Newton steps on pgamma, and solve for the gap h by safeguarded Newton where
//    1 + nu*sigma*z < 1/2.
//
// Bit-identity with gamlss.dist assumes that the compiler does not fuse a product
// and a sum into one FMA: R evaluates each operator separately. bcpe_rnd() forces
// the rounding of each product that gamlss adds to something: on every GNU-compatible
// compiler it is an empty asm statement through which the value passes in memory (a
// feature test such as __FMA__ is not enough: a bare -mavx512f lets GCC emit scalar
// FMA without defining it), and a volatile copy on other compilers. Measured cost at
// -O2 under R's default flags (no FMA): below 1 % for fdBCPE, fpBCPE and fqBCPE on
// 1e6 draws of mu 15-40, sigma 0.05-0.3, nu -1.5..1.5, tau 1.2-4. Every inexact product
// that gamlss adds to something goes through it, including the argument of an exp().
//
// Limits (measured against 50-digit references).
//  - Small sigma inside the gamlss region. gamlss's z = ((y/mu)^nu - 1) / (nu*sigma)
//    (log(y/mu)/sigma for nu == 0) carries an absolute error of about eps/(2*sigma)
//    from the rounding of y/mu, and the gamlss path keeps it, for parity. For
//    sigma <= ~0.01 the natural lower-tail CDF reaches ~5e-14 absolute (largest error
//    found: 5.2e-14 for sigma <= 0.003, 6e-15 for sigma <= 0.01, <= 5e-15 above) and
//    the log density ~1.5e-12 (the natural density ~1e-12 at |z| ~ 10), so
//    F + (upper tail) = 1 and exp(log F) = F then hold to ~1e-13 rather than a few
//    ulp for sigma <= 0.05; the stable path (upper tails, log scales) is unaffected.
//    These are eps times the condition number with respect to a relative perturbation
//    of y/mu (about f_T(z) / (sigma * F) for the CDF), not with respect to the exact
//    inputs.
//  - Quantile at a tiny tau and a huge |log p|. The T quantile z = c * (2 |log p|)^(1/tau)
//    overflows the double range when |log p| exceeds ~1.4e8 (tau 0.02), ~1e17 (tau
//    0.05), ~1e32 (tau 0.1) or ~2e93 (tau 0.3); the quantile is then returned as 0 or
//    Inf even if the Box-Cox power would bring a finite true value into range
//    (e.g. 8.5e-121 for log p = -1e100, tau = 0.1, nu = -8). The forward functions
//    survive the same overflow. Never reached for |log p| <= 1e5.

#include <Rcpp.h>
#include <Rmath.h>
#include <Rversion.h>
#include <cmath>
#include <cfloat>
#include <algorithm>

// ---------------------------------------------------------------------------
// Named thresholds. Each is exact in binary.
// ---------------------------------------------------------------------------

// (G1) The density and CDF need nu == 0 or |nu| * min(1, sigma) >= this (2^-8):
// gamlss's ((y/mu)^nu - 1) / (nu*sigma) carries an absolute error of about
// eps * w / (|nu| * sigma), w = (y/mu)^nu, which a small |nu| * sigma blows up.
constexpr double BCPE_GAMLSS_NU_MIN = 0.00390625;
// (G-d, G-p) ... and tau >= this. For tau < 1 f_T has a cusp at 0, and near the
// median gamlss's z (absolute error ~eps * w / (|nu| * sigma), and through log(y/mu)
// for nu == 0) meets it: gamlss's F is up to 1.9e-4 off at tau = 0.05, 1.3e-11 at
// tau = 0.2, 1.5e-13 at tau = 0.5, and its log density up to 0.087 at tau = 0.05.
constexpr double BCPE_GAMLSS_DP_TAU_MIN = 1.0;
// (G-p) For z < 0 the CDF needs gamlss's lower T tail 0.5 * (1 - G) >= this (2^-8),
// so that the difference 1 - G loses at most 7 bits.
constexpr double BCPE_GAMLSS_FT_MIN = 0.00390625;
// (G-q'') The quantile (which never forms the forward Box-Cox transform, so G1's
// sigma factor does not apply) needs, for nu != 0, |nu| * min(1, w) >= this (2^-8),
// w = nu*sigma*za + 1 the base of w^(1/nu) (w <= 0 fails): gamlss's y then carries
// a relative error ~eps * (1 + |nu*sigma*za|) / (|nu| * w).
constexpr double BCPE_GAMLSS_Q_NUW_MIN = 0.00390625;
// (G-q'') ... and its target pt with m = min(pt, 1 - pt) >= this * max(1, sigma/w)
// (2^-8; w = 1 for nu == 0): gamlss's za, from qgamma of 1 - 2m, carries a relative
// error ~eps/m, which y inherits times sigma*|za|/w. Measured: inside, gamlss's qBCPE
// is within 1e-13 of the true quantile (4.3e-14 on 11537 points, 9.7e-14 on a fresh
// boundary sample); on mu 15-40, sigma 0.05-0.3, nu -1.5..1.5, tau 1.2-4 about 99 % of
// the quantiles are inside the region.
constexpr double BCPE_GAMLSS_Q_M_MIN = 0.00390625;

// Stable path: the between mass P(t1 < T < b) is a difference of T tails while the
// subtracted tail is at most this fraction of the larger one (at most one bit lost).
constexpr double BCPE_BETWEEN_SUB = 0.5;
// Stable quantile: below this value of w = 1 + nu*sigma*z the back-transform
// mu * w^(1/nu) is not formed from z (w would carry only the absolute accuracy of
// z); the gap h = b * w is solved for instead.
constexpr double BCPE_QUANT_W_MIN = 0.5;
// Stable quantile: above this w the solve for h starts from the direct w (relative
// error ~eps/w); below it from the first-order h = target / f_T(b) (2^-20).
constexpr double BCPE_QUANT_W_START = 9.5367431640625e-07;
// Cap on the Newton iterations for h (each converges in ~3-5).
constexpr int BCPE_NEWTON_MAXIT = 60;
// A plain Newton step below this size (relative to max(1, |log h|)) that does not halve
// |f| marks a plateau of f in bcpe_q_powerlaw, and the solve stops there.
constexpr double BCPE_NEWTON_PLATEAU_STEP = 1e-8;
// Newton polish of R::qgamma: at most this many steps, stopping at a relative step
// below the tolerance (4 eps).
constexpr int BCPE_QPOLISH_MAXIT = 4;
constexpr double BCPE_QPOLISH_TOL = 8.8817841970012523e-16;
// A natural-scale mass below this is redone in log space for a log-scale result
// (2^-960, ~1e-289: comfortably above the subnormal range).
constexpr double BCPE_TINY = 1.0255520352341779e-289;
// Below this s (2^-70, ~8.5e-22) the lower gamma tail is G = s^a / Gamma(1 + a) to
// double precision (the next term is a s / (a + 1)), and is computed from t, not s
// (s may have underflowed); a gamma quantile below it is inverted the same way.
constexpr double BCPE_S_SERIES = 8.4703294725430034e-22;
// log(1/4) and log(3/4) (rounded to double).
constexpr double BCPE_LOG_QUARTER = -1.3862943611198906;
constexpr double BCPE_LOG_3_4 = -0.2876820724517809;
// log(3) (rounded to double).
constexpr double BCPE_LN3 = 1.0986122886681098;

// ---------------------------------------------------------------------------
// Small numeric helpers
// ---------------------------------------------------------------------------

// R's sign() for a non-NaN double.
inline double bcpe_sign(const double x) {
    return x > 0.0 ? 1.0 : (x == 0.0 ? 0.0 : -1.0);
}

// Returns x rounded to double, so a product passed through it cannot be fused with
// the following addition into one FMA (see the note at the top). Unconditional on every
// GNU-compatible compiler: no feature macro (__FMA__, __FMA4__, __AVX__) covers every
// flag that lets the compiler contract a * b + c.
inline double bcpe_rnd(double x) {
#if defined(__GNUC__) || defined(__clang__)
    __asm__ volatile("" : "+m"(x));
    return x;
#else
    volatile double v = x;
    return v;
#endif
}

// log(1 - exp(x)) for x <= 0, accurate at both ends (R's R_Log1_Exp).
inline double bcpe_log1mexp(const double x) {
    return x > -M_LN2 ? std::log(-std::expm1(x)) : std::log1p(-std::exp(x));
}

// log(exp(x) + exp(y)), with -Inf as an exact zero.
inline double bcpe_logadd(const double x, const double y) {
    if (x == R_NegInf) return y;
    if (y == R_NegInf) return x;
    return std::max(x, y) + std::log1p(std::exp(-std::fabs(x - y)));
}

// 0.5 * exp(arg), also where exp(arg) overflows although the half is finite (arg in
// (709.78, 710.47]): exp(arg - ln 2) only then, as that costs a second rounding of arg.
inline double bcpe_half_exp(const double arg) {
    const double ex = std::exp(arg);
    return (ex <= DBL_MAX) ? 0.5 * ex : std::exp(arg - M_LN2);
}

// mu * exp(e), without overflowing or underflowing early.
inline double bcpe_mu_exp(const double mu, const double e) {
    if (e > 700.0 || e < -700.0) return std::exp(std::log(mu) + e);
    return mu * std::exp(e);
}

// exp() as gamlss.dist gets it from the R this header is compiled against. R >= 4.6.0
// evaluates exp() of a vector with Rexp() in arithmetic.c, which returns 1 + x for
// fabs(x) <= sqrt(DBL_EPSILON) (= 2^-26, the boundary included) and libm's exp(x)
// otherwise (R 4.6.0 NEWS: "Experimentally, several math library functions ... are now
// ensured to give fully accurate results in a small neighbourhood of special values");
// R <= 4.5 uses libm's exp throughout, which differs from 1 + x by an ulp for some |x|
// in that band (so this is a property of the R version, not of the platform). The rule
// is therefore chosen at compile time from R_VERSION; define
// BCPE_R_EXP_TINY to 1 (R >= 4.6.0 rule) or 0 (libm) before including this header to
// override it. R's byte-compiled code on a length-1 argument uses libm's exp, so
// gamlss.dist's own qBCPE/dBCPE/pBCPE can differ in the last bit between a scalar call
// and the same element inside a vector; the gamlss path follows the vector form. It
// matters for c = exp(log.c) with tau within ~3.5e-8 of 2, mu * exp(sigma*za) at nu == 0
// near the median, and a density near 1.
#ifndef BCPE_R_EXP_TINY
# if defined(R_VERSION) && R_VERSION >= R_Version(4, 6, 0)
#  define BCPE_R_EXP_TINY 1
# else
#  define BCPE_R_EXP_TINY 0
# endif
#endif
inline double bcpe_g_exp(const double x) {
#if BCPE_R_EXP_TINY
    return std::fabs(x) <= 1.4901161193847656e-08 ? 1.0 + x : std::exp(x);
#else
    return std::exp(x);
#endif
}

// (G1): the Box-Cox transform is accurate enough in gamlss's arithmetic.
inline bool bcpe_in_G1(const double sigma, const double nu) {
    return nu == 0.0 || std::fabs(nu) * std::min(1.0, sigma) >= BCPE_GAMLSS_NU_MIN;
}

// ---------------------------------------------------------------------------
// The gamlss path: gamlss.dist's arithmetic, operation for operation
// ---------------------------------------------------------------------------

// gamlss's per-tau intermediates of f.T, F.T and q.T:
//   log.c <- 0.5 * (-(2/tau) * log(2) + lgamma(1/tau) - lgamma(3/tau)); c <- exp(log.c)
struct bcpe_g_T {
    double tau;   // tau
    double a;     // 1/tau
    double lg1;   // lgamma(1/tau)
    double logc;  // log.c
    double c;     // c
};

inline bcpe_g_T bcpe_g_T_make(const double tau) {
    bcpe_g_T g;
    g.tau = tau;
    g.a = 1.0 / tau;
    g.lg1 = R::lgammafn(g.a);
    const double lg3 = R::lgammafn(3.0 / tau);
    g.logc = 0.5 * ((bcpe_rnd(-(2.0 / tau) * M_LN2) + g.lg1) - lg3);
    g.c = bcpe_g_exp(g.logc);
    return g;
}

// gamlss's c in the normal range (it underflows for tau below ~0.0095).
inline bool bcpe_g_c_ok(const bcpe_g_T& g) {
    return g.c >= DBL_MIN && g.c <= DBL_MAX;
}

// s <- 0.5 * ((abs(t/c))^tau)
inline double bcpe_g_s(const double t, const bcpe_g_T& g) {
    return 0.5 * ::R_pow(std::fabs(t / g.c), g.tau);
}

// z <- ifelse(nu != 0, (((y/mu)^nu - 1)/(nu * sigma)), log(y/mu)/sigma), ratio = y/mu
inline double bcpe_g_z(const double ratio, const double sigma, const double nu) {
    if (nu != 0.0) return (::R_pow(ratio, nu) - 1.0) / (nu * sigma);
    return std::log(ratio) / sigma;
}

// gamlss's log density at x > 0 (ratio = x/mu, FTb = F.T(1/(sigma*abs(nu))) in
// gamlss's arithmetic):
//   log.lik <- log(tau) - log.c - (0.5 * (abs(t/c)^tau)) - (1 + (1/tau)) * log(2) - lgamma(1/tau)
//   logfZ   <- f.T(z, log = TRUE) - log(F.T(1/(sigma * abs(nu))))
//   logder  <- (nu - 1) * log(x) - nu * log(mu) - log(sigma)
inline double bcpe_g_logd(const double x, const double ratio, const double mu,
                          const double sigma, const double nu, const double FTb,
                          const bcpe_g_T& g) {
    const double z = bcpe_g_z(ratio, sigma, nu);
    const double loglik_T = (((std::log(g.tau) - g.logc) -
                              0.5 * ::R_pow(std::fabs(z / g.c), g.tau)) -
                             bcpe_rnd((1.0 + g.a) * M_LN2)) - g.lg1;
    const double logfZ = loglik_T - std::log(FTb);
    const double logder = (bcpe_rnd((nu - 1.0) * std::log(x)) -
                           bcpe_rnd(nu * std::log(mu))) - std::log(sigma);
    return logder + logfZ;
}

// ---------------------------------------------------------------------------
// The stable path
// ---------------------------------------------------------------------------

// Per-tau constants of T, accurate to a few ulp: a = 1/tau, log c, c, log f_T(0),
// lgamma(1 + a), and a rough median of Gamma(a) (only to choose which gamma tail
// to compute first).
struct bcpe_T {
    double tau;
    double a;
    double log_c;
    double c;
    bool c_ok;      // c in the normal range
    double lg1p_a;  // lgamma(1 + a)
    double log_fT0; // log f_T(0) = -log c - (1 + a) log 2 - lgamma(1 + a)
    double med;     // ~ median of Gamma(a)
};

inline bcpe_T bcpe_T_make(const double tau) {
    bcpe_T T;
    T.tau = tau;
    T.a = 1.0 / tau;
    T.lg1p_a = R::lgamma1p(T.a);
    if (tau == 2.0) {
        T.log_c = 0.0;                    // c = 1 (T is the standard normal)
    } else if (tau == 1.0) {
        T.log_c = -1.5 * M_LN2;           // c^2 = 1/8 (Laplace)
    } else if (T.a < 0.5) {
        // lgamma(a) - lgamma(3a) = log(3) + lgamma(1 + a) - lgamma(1 + 3a): for a
        // small each lgamma(.) is ~ -log(.), and this form avoids their cancellation.
        T.log_c = 0.5 * (((BCPE_LN3 + T.lg1p_a) - R::lgamma1p(3.0 * T.a)) - 2.0 * T.a * M_LN2);
    } else {
        T.log_c = 0.5 * ((R::lgammafn(T.a) - R::lgammafn(3.0 * T.a)) - 2.0 * T.a * M_LN2);
    }
    T.c = std::exp(T.log_c);
    T.c_ok = T.c >= DBL_MIN && T.c <= DBL_MAX;
    T.log_fT0 = (-T.log_c - (1.0 + T.a) * M_LN2) - T.lg1p_a;
    T.med = (T.a >= 1.0) ? T.a - 1.0 / 3.0 + 8.0 / (405.0 * T.a)
                         : std::exp((-M_LN2 + T.lg1p_a) / T.a);
    return T;
}

// s(t) = 0.5 * (t / c)^tau for t >= 0 (t may be +Inf).
inline double bcpe_s_of_t(const double t, const bcpe_T& T) {
    if (t == 0.0) return 0.0;
    if (T.c_ok) {
        const double r = t / T.c;
        if (r >= DBL_MIN && r <= DBL_MAX) {
            const double rp = std::pow(r, T.tau);
            if (rp <= DBL_MAX) return 0.5 * rp;
            // (t/c)^tau overflowed (large tau): 0.5 * (t/c)^tau may still be finite
        }
    }
    if (t == R_PosInf) return R_PosInf;
    return bcpe_half_exp(T.tau * (std::log(t) - T.log_c));
}

// |t| at the gamma value s: t = c * (2s)^(1/tau).
inline double bcpe_t_of_s(const double s, const bcpe_T& T) {
    if (ISNAN(s)) return s;
    if (s <= 0.0) return 0.0;
    if (s == R_PosInf) return R_PosInf;
    if (T.c_ok && s <= 0.25 * DBL_MAX) {
        const double t = T.c * std::pow(2.0 * s, T.a);
        if (t >= DBL_MIN && t <= DBL_MAX) return t;
    }
    return std::exp(T.log_c + T.a * (M_LN2 + std::log(s)));
}

// Both gamma tails at s = s(t), each with its relative accuracy: G = pgamma(s, a) and
// Gc = pgamma(s, a, lower = FALSE); the smaller is computed directly, the larger
// (>= 1/4) as 1 minus it. On the log scale (bcpe_lgpair) the same, in logs.
// For s below BCPE_S_SERIES (where s may also have underflowed: at a large tau,
// s = 0.5 (t/c)^tau is 0 for t/c below ~10^(-323/tau)) G comes from t itself:
// G = s^a / Gamma(1 + a) (1 + O(s)) = t 2^-a / (c Gamma(1 + a)) = 2 f_T(0) t.
struct bcpe_GG {
    double G;
    double Gc;
};

// log G(s(t)) for s(t) < BCPE_S_SERIES
inline double bcpe_logG_small(const double t, const bcpe_T& T) {
    return ((std::log(t) - T.log_c) - T.a * M_LN2) - T.lg1p_a;
}

inline bcpe_GG bcpe_gpair(const double s, const double t, const bcpe_T& T) {
    bcpe_GG r;
    if (ISNAN(s)) { r.G = s; r.Gc = s; return r; }
    if (t == 0.0) { r.G = 0.0; r.Gc = 1.0; return r; }
    if (s == R_PosInf) { r.G = 1.0; r.Gc = 0.0; return r; }
    if (s < BCPE_S_SERIES) {
        r.G = std::exp(bcpe_logG_small(t, T));
        r.Gc = 1.0 - r.G;
        return r;
    }
    if (s < T.med) {
        r.G = R::pgamma(s, T.a, 1.0, 1, 0);
        if (r.G <= 0.75) { r.Gc = 1.0 - r.G; return r; }
        r.Gc = R::pgamma(s, T.a, 1.0, 0, 0);
    } else {
        r.Gc = R::pgamma(s, T.a, 1.0, 0, 0);
        if (r.Gc <= 0.75) { r.G = 1.0 - r.Gc; return r; }
        r.G = R::pgamma(s, T.a, 1.0, 1, 0);
    }
    return r;
}

inline bcpe_GG bcpe_lgpair(const double s, const double t, const bcpe_T& T) {
    bcpe_GG r;
    if (ISNAN(s)) { r.G = s; r.Gc = s; return r; }
    if (t == 0.0) { r.G = R_NegInf; r.Gc = 0.0; return r; }
    if (s == R_PosInf) { r.G = 0.0; r.Gc = R_NegInf; return r; }
    if (s < BCPE_S_SERIES) {
        r.G = bcpe_logG_small(t, T);
        r.Gc = std::log1p(-std::exp(r.G));
        return r;
    }
    if (s < T.med) {
        r.G = R::pgamma(s, T.a, 1.0, 1, 1);
        if (r.G <= BCPE_LOG_3_4) { r.Gc = bcpe_log1mexp(r.G); return r; }
        r.Gc = R::pgamma(s, T.a, 1.0, 0, 1);
    } else {
        r.Gc = R::pgamma(s, T.a, 1.0, 0, 1);
        if (r.Gc <= BCPE_LOG_3_4) { r.G = bcpe_log1mexp(r.Gc); return r; }
        r.G = R::pgamma(s, T.a, 1.0, 1, 1);
    }
    return r;
}

// The Box-Cox transform without cancellation: ell = log(y/mu) (log y - log mu if
// the ratio leaves the normal range), x = nu * ell = log((y/mu)^nu), and
// z = (ell/sigma) * expm1(x)/x (ell/sigma at x == 0, which covers nu == 0 and a nu
// so small that x underflows), and s = s(|z|). Where z overflows, s comes from
// log|z| = log|ell| - log(sigma) + log(expm1(x)/x).
struct bcpe_bc {
    double ell;
    double x;
    double z;
    double s;
};

inline bcpe_bc bcpe_boxcox(const double y, const double mu, const double sigma,
                           const double nu, const bcpe_T& T) {
    bcpe_bc r;
    const double ratio = y / mu;
    if (ratio >= 0.5 && ratio <= 2.0) {
        // y - mu is exact here (Sterbenz), so ell keeps its relative accuracy near y = mu.
        r.ell = std::log1p((y - mu) / mu);
    } else if (ratio >= DBL_MIN && ratio <= DBL_MAX) {
        r.ell = std::log(ratio);
    } else {
        r.ell = std::log(y) - std::log(mu);
    }
    r.x = nu * r.ell;
    double em = 0.0;
    if (r.x == 0.0) {
        r.z = r.ell / sigma;
    } else {
        em = std::expm1(r.x);
        r.z = (r.ell / sigma) * (em / r.x);
    }
    if (std::fabs(r.z) <= DBL_MAX) {
        r.s = bcpe_s_of_t(std::fabs(r.z), T);
    } else if (ISNAN(r.z)) {
        r.s = r.z;
    } else {
        double logE1 = 0.0;
        if (r.x != 0.0) logE1 = (em <= DBL_MAX) ? std::log(em / r.x) : r.x - std::log(r.x);
        const double log_tz = std::log(std::fabs(r.ell)) - std::log(sigma) + logE1;
        r.s = bcpe_half_exp(T.tau * (log_tz - T.log_c));
    }
    return r;
}

// The truncation point b = 1/(sigma*|nu|) (nu != 0), its log and s(b).
struct bcpe_trunc {
    double b;
    double log_b;
    double s_b;
};

inline bcpe_trunc bcpe_trunc_make(const double sigma, const double nu, const bcpe_T& T) {
    bcpe_trunc tr;
    const double snu = sigma * std::fabs(nu);
    tr.b = 1.0 / snu;
    tr.log_b = (snu >= DBL_MIN && snu <= DBL_MAX) ? -std::log(snu)
                                                   : -(std::log(sigma) + std::log(std::fabs(nu)));
    tr.s_b = bcpe_s_of_t(tr.b, T);
    return tr;
}

// log of I = int_0^V exp(-s_b * expm1(-tau v) - v) dv, V = -log1p(-w) = log(b/t1),
// t1 = b (1 - w): with t = b e^-v, P(t1 < T < b) = b f_T(b) I. The integrand is
// entire in v (no branch point, unlike f_T in t), and in the cases that reach the
// quadrature its log varies by O(1) over [0, V]; it is still split into panels if
// V (1 + tau s_b) is large. For V (1 + tau s_b) < 1e-17 it is 1 to double
// precision, so I = V (log V = log w for w < 1e-17, where w may underflow).
inline double bcpe_logI(const double w, const double log_w, const double s_b,
                        const bcpe_T& T) {
    double V, log_V;
    if (w < 1e-17) {
        V = w;
        log_V = log_w;
    } else {
        V = -std::log1p(-w);
        log_V = std::log(V);
    }
    const double k = T.tau * s_b + 1.0;
    if (!(V * k >= 1e-17)) return log_V;
    // 20-point Gauss-Legendre rule on [-1, 1] (positive nodes; the rule is symmetric).
    // Function-local: a namespace-scope constexpr array has internal linkage, so each
    // translation unit would get its own copy for this inline function to read.
    static constexpr double gl_x[10] = {
        0.076526521133497338, 0.22778585114164507, 0.37370608871541955,
        0.51086700195082713,  0.63605368072651502, 0.7463319064601508,
        0.83911697182221878,  0.91223442825132595, 0.96397192727791381,
        0.99312859918509488};
    static constexpr double gl_w[10] = {
        0.15275338713072584,  0.14917298647260374,  0.14209610931838204,
        0.13168863844917664,  0.11819453196151841,  0.10193011981724044,
        0.083276741576704755, 0.062672048334109068, 0.040601429800386939,
        0.017614007139152118};
    double np = std::ceil(V * k / 4.0);
    const int panels = (np >= 64.0) ? 64 : ((np < 1.0) ? 1 : static_cast<int>(np));
    const double width = V / panels;
    const double half = 0.5 * width;
    double pmax = R_NegInf, sum = 0.0;
    for (int j = 0; j < panels; ++j) {
        const double mid = (j + 0.5) * width;
        for (int i = 0; i < 10; ++i) {
            for (int sgn = -1; sgn <= 1; sgn += 2) {
                const double v = mid + sgn * half * gl_x[i];
                const double psi = -s_b * std::expm1(-T.tau * v) - v;
                if (psi > pmax) {
                    sum = (pmax == R_NegInf) ? 0.0 : sum * std::exp(pmax - psi);
                    pmax = psi;
                }
                sum += gl_w[i] * std::exp(psi - pmax);
            }
        }
    }
    if (!(pmax > R_NegInf) || !(sum > 0.0)) return log_V;
    return std::log(half) + pmax + std::log(sum);
}

// The between mass P(t1 < T < b), 0 <= t1 = b (1 - w) < b, from the gamma tails at
// s1 = s(t1) (g1) and s_b (gb), with w = h/b in (0, 1] known accurately (log_w its
// log, finite where w underflows). A difference of upper tails while it loses at
// most a bit, else of lower tails, else the quadrature b f_T(b) I.
inline double bcpe_between(const bcpe_GG& g1, const bcpe_GG& gb, const bcpe_trunc& tr,
                           const double w, const double log_w, const bcpe_T& T) {
    if (gb.Gc <= BCPE_BETWEEN_SUB * g1.Gc) return 0.5 * (g1.Gc - gb.Gc);
    if (g1.G <= BCPE_BETWEEN_SUB * gb.G) return 0.5 * (gb.G - g1.G);
    return std::exp(tr.log_b + (T.log_fT0 - tr.s_b) + bcpe_logI(w, log_w, tr.s_b, T));
}

// The same in log space (lg1, lgb: bcpe_lgpair at s1 and s_b).
inline double bcpe_between_log(const bcpe_GG& lg1, const bcpe_GG& lgb, const bcpe_trunc& tr,
                               const double w, const double log_w, const bcpe_T& T) {
    if (lg1.Gc == R_NegInf) return R_NegInf;
    if (lgb.Gc - lg1.Gc <= -M_LN2) return -M_LN2 + lg1.Gc + bcpe_log1mexp(lgb.Gc - lg1.Gc);
    if (lg1.G - lgb.G <= -M_LN2) return -M_LN2 + lgb.G + bcpe_log1mexp(lg1.G - lgb.G);
    return tr.log_b + (T.log_fT0 - tr.s_b) + bcpe_logI(w, log_w, tr.s_b, T);
}

// Stable density (log = TRUE gives the log density):
//   log f(y) = nu*ell - log(y) - log(sigma) + log f_T(z) - log D,
// with log f_T(z) = log f_T(0) - s(z) and D = 1 - 0.5 * Gc(s_b) the kept mass
// (D >= 1/2, so the absolute accuracy of Gc(s_b) suffices). On the natural scale
// the factor 1/y is applied by division where exp() of the rest is in range.
inline double bcpe_d_stable(const double x, const double mu, const double sigma,
                            const double nu, const double tau, const bool log_p) {
    const bcpe_T T = bcpe_T_make(tau);
    const bcpe_bc bc = bcpe_boxcox(x, mu, sigma, nu, T);
    double logD = 0.0;
    if (nu != 0.0) {
        const bcpe_trunc tr = bcpe_trunc_make(sigma, nu, T);
        // from t as well as s_b: s_b underflows for a large tau and a small b, where
        // G(s_b) = 2 f_T(0) b is not negligible
        const bcpe_GG gb = bcpe_gpair(tr.s_b, tr.b, T);
        logD = std::log1p(-0.5 * gb.Gc);
    }
    const double e = ((bc.x - std::log(sigma)) + (T.log_fT0 - bc.s)) - logD;   // log(f * x)
    if (log_p) return e - std::log(x);
    if (e > -700.0 && e < 700.0) return std::exp(e) / x;
    return std::exp(e - std::log(x));
}

// Stable CDF on the requested tail and scale. With z the Box-Cox value, s_z = s(|z|),
// b the truncation point and L = 0.5 * Gc(s_b) the T mass beyond it (D = 1 - L), the
// lower and upper masses of Z (ML + MU = D) are, each a sum of nonnegative terms:
//   nu == 0: z < 0: ML = 0.5 Gc(s_z), MU = 0.5 + 0.5 G(s_z); z >= 0: the mirror;
//   nu  > 0: z >= 0: MU = 0.5 Gc(s_z), ML = 0.5 (G(s_b) + G(s_z));
//            z  < 0: ML = P(|z| < T < b) (between), MU = 0.5 + 0.5 G(s_z);
//   nu  < 0: z <= 0: ML = 0.5 Gc(s_z), MU = 0.5 (G(s_b) + G(s_z));
//            z  > 0: MU = P(z < T < b) (between), ML = 0.5 + 0.5 G(s_z).
// F = ML/D, U = MU/D. log F is log1p(-U) where F > 1/2, else log(ML) - log(D), with
// ML redone in log space where it is below BCPE_TINY (and the converse for U).
inline double bcpe_p_stable(const double q, const double mu, const double sigma,
                            const double nu, const double tau, const bool lower_tail,
                            const bool log_p) {
    const bcpe_T T = bcpe_T_make(tau);
    const bcpe_bc bc = bcpe_boxcox(q, mu, sigma, nu, T);
    const bcpe_GG gz = bcpe_gpair(bc.s, std::fabs(bc.z), T);
    // kind of the small-side formula, for the log-space redo: 0 tail, 1 sum, 2 between
    int kind_L, kind_U;
    double ML, MU, logD = 0.0;
    bcpe_trunc tr;
    bcpe_GG gb;
    tr.b = R_PosInf; tr.log_b = R_PosInf; tr.s_b = R_PosInf;
    gb.G = 1.0; gb.Gc = 0.0;
    if (nu == 0.0) {
        if (bc.z < 0.0) { ML = 0.5 * gz.Gc; MU = 0.5 + 0.5 * gz.G; kind_L = 0; kind_U = -1; }
        else            { MU = 0.5 * gz.Gc; ML = 0.5 + 0.5 * gz.G; kind_U = 0; kind_L = -1; }
    } else {
        tr = bcpe_trunc_make(sigma, nu, T);
        gb = bcpe_gpair(tr.s_b, tr.b, T);
        logD = std::log1p(-0.5 * gb.Gc);
        const bool trunc_side = (nu > 0.0) ? (bc.z < 0.0) : (bc.z > 0.0);
        if (trunc_side) {
            // z between the centre and the truncation point: w = (y/mu)^nu < 1
            const double Mb = bcpe_between(gz, gb, tr, std::exp(bc.x), bc.x, T);
            const double Mo = 0.5 + 0.5 * gz.G;
            if (nu > 0.0) { ML = Mb; MU = Mo; kind_L = 2; kind_U = -1; }
            else          { MU = Mb; ML = Mo; kind_U = 2; kind_L = -1; }
        } else {
            const double Mt = 0.5 * gz.Gc;              // the untruncated tail beyond z
            const double Ms = 0.5 * (gb.G + gz.G);      // from z through 0 to the truncation point
            if (nu > 0.0) { MU = Mt; ML = Ms; kind_U = 0; kind_L = 1; }
            else          { ML = Mt; MU = Ms; kind_L = 0; kind_U = 1; }
        }
    }
    const double D = 1.0 - 0.5 * gb.Gc;
    // (a NaN mass must stay NaN, so not std::min(1.0, ML / D), which returns 1.0 for a NaN)
    const double fr = ML / D;
    const double ur = MU / D;
    const double F = fr > 1.0 ? 1.0 : fr;
    const double U = ur > 1.0 ? 1.0 : ur;
    if (!log_p) return lower_tail ? F : U;
    const double big = lower_tail ? F : U;      // the requested side
    const double other = lower_tail ? U : F;
    if (big > 0.5) return std::log1p(-other);
    const double m = lower_tail ? ML : MU;
    if (m >= BCPE_TINY) return std::log(m) - logD;
    // The requested side is tiny: redo it in log space.
    const int kind = lower_tail ? kind_L : kind_U;
    const bcpe_GG lz = bcpe_lgpair(bc.s, std::fabs(bc.z), T);
    double lm;
    if (kind == 0) {
        lm = -M_LN2 + lz.Gc;
    } else {
        const bcpe_GG lb = bcpe_lgpair(tr.s_b, tr.b, T);
        if (kind == 1) lm = -M_LN2 + bcpe_logadd(lb.G, lz.G);
        else           lm = bcpe_between_log(lz, lb, tr, std::exp(bc.x), bc.x, T);
    }
    // (a log probability is <= 0; this only bites far outside the parameter contract,
    // e.g. tau = 1e-300, where the constants of T are themselves meaningless)
    const double r = lm - logD;
    return r > 0.0 ? 0.0 : r;
}

// Newton polish of a gamma quantile s on R::pgamma in log space: solve
// log P(s) = lp, P the lower (lower = true) or upper gamma tail, in log s, with
// d log P / d log s = +-s g(s) / P(s) (g the gamma density). R's qgamma is up to
// ~5e-10 relative off for upper tails in about [1e-14, 3.5e-12]; two or three steps
// bring s to a few ulp. Stops at a relative step below BCPE_QPOLISH_TOL, after
// BCPE_QPOLISH_MAXIT steps, or on a step that does not reduce the residual.
inline double bcpe_qgamma_polish(double s, const double lp, const bool lower, const bcpe_T& T) {
    if (!(s > 0.0) || s == R_PosInf) return s;
    double lP = R::pgamma(s, T.a, 1.0, lower ? 1 : 0, 1);
    double f = lP - lp;
    for (int it = 0; it < BCPE_QPOLISH_MAXIT; ++it) {
        if (f == 0.0 || ISNAN(f)) break;
        const double ldg = R::dgamma(s, T.a, 1.0, 1);
        // relative step in s: -f / (d log P / d log s)
        double rstep = f * std::exp(lP - ldg - std::log(s));
        if (lower) rstep = -rstep;
        if (ISNAN(rstep)) break;
        // A step below the tolerance is taken without re-evaluating (the usual case:
        // R's qgamma is within an ulp or two), so it costs one pgamma and one dgamma.
        if (std::fabs(rstep) < BCPE_QPOLISH_TOL) { s *= 1.0 + rstep; break; }
        if (rstep > 1.0) rstep = 1.0;               // at most doubling / halving per step
        if (rstep < -0.5) rstep = -0.5;
        const double s_new = s * (1.0 + rstep);
        if (!(s_new > 0.0) || s_new == R_PosInf) break;
        const double lP_new = R::pgamma(s_new, T.a, 1.0, lower ? 1 : 0, 1);
        const double f_new = lP_new - lp;
        if (!(std::fabs(f_new) < std::fabs(f))) break;
        s = s_new; lP = lP_new; f = f_new;
        if (std::fabs(rstep) < BCPE_QPOLISH_TOL) break;
    }
    return s;
}

// |t| at which the T mass on one side is exp(lq) (lq <= log(1/2) up to rounding):
// P(T > t) = 0.5 Gc(s(t)) for the far side; near the centre (mass above 1/4)
// G(s(t)) = 1 - 2m is inverted instead, so qgamma always gets a probability <= 1/2
// on its own tail. Each qgamma is polished (bcpe_qgamma_polish). A tiny s from the
// lower tail uses s^a = G Gamma(1 + a)(1 + O(s)).
inline double bcpe_qT_abs(const double lq, const bcpe_T& T) {
    if (lq == R_NegInf) return R_PosInf;
    if (lq <= BCPE_LOG_QUARTER) {
        const double l2m = lq + M_LN2;
        double s = (l2m > -700.0) ? R::qgamma(std::exp(l2m), T.a, 1.0, 0, 0)
                                  : R::qgamma(l2m, T.a, 1.0, 0, 1);
        if (!(s > 0.0 && s < R_PosInf)) {
            // R's qgamma returns NaN, 0 or +-Inf for log p below about -1e300 (at shape 1,
            // NaN). There s ~ -l2m: the start from log Gc(s) = (a - 1) log s - s -
            // lgamma(a) + O(1/s) is exact to double precision, and the polish checks it.
            const double s0 = -l2m;
            s = s0 + (T.a - 1.0) * std::log(s0) - (T.lg1p_a - std::log(T.a));
        }
        s = bcpe_qgamma_polish(s, l2m, false, T);
        return bcpe_t_of_s(s, T);
    }
    const double g = -std::expm1(lq + M_LN2);
    if (!(g > 0.0)) return ISNAN(g) ? g : 0.0;
    double s = R::qgamma(g, T.a, 1.0, 1, 0);
    if (s >= BCPE_S_SERIES) {
        s = bcpe_qgamma_polish(s, std::log(g), true, T);
        return bcpe_t_of_s(s, T);
    }
    // the inverse of bcpe_logG_small: t = c 2^a Gamma(1 + a) G
    return std::exp(T.log_c + T.a * M_LN2 + std::log(g) + T.lg1p_a);
}

// The quantile next to the truncation point: solve P(b - h < T < b) = exp(lm) for
// the gap h by Newton in log h, safeguarded by a bracket, and return log w =
// log(h/b). M(h) is increasing and convex, and h0 = target / f_T(b) lies at or to
// the right of the root; where the direct w (from z) is not tiny, it starts there.
inline double bcpe_q_powerlaw(const double lm, const double w_direct, const bcpe_trunc& tr,
                              const bcpe_GG& lgb, const bcpe_T& T) {
    if (lm == R_NegInf) return R_NegInf;
    double log_h;
    if (w_direct > BCPE_QUANT_W_START) log_h = std::log(w_direct) + tr.log_b;
    else log_h = lm - (T.log_fT0 - tr.s_b);
    // h0 = min(target / f_T(b), b): M is convex on [0, b] only, and the clamped start
    // stays at or right of the root.
    if (!(log_h <= tr.log_b)) log_h = tr.log_b;
    double lo = R_NegInf, hi = tr.log_b;
    // A plain Newton step of negligible size that leaves |f| unreduced means that f is a
    // plateau function of log h at this resolution: where the difference of two T tails
    // is used, t1 = b - h is a double with spacing ~eps*b, so log h is resolved only to
    // ~eps*b/h, and the tail changes by tau*s(t1)*eps per spacing. Newton then creeps by
    // the same tiny step to the iteration cap (seen at tau 30-100, about 1 solve in 1e5).
    // The iterate with the smaller residual is returned, which is the best that f resolves.
    bool plain_prev = false;
    double f_prev = 0.0, log_h_prev = log_h;
    for (int it = 0; it < BCPE_NEWTON_MAXIT; ++it) {
        const double log_w = log_h - tr.log_b;
        const double w = std::exp(log_w);
        const double t1 = tr.b * (-std::expm1(log_w));   // b - h
        const double s1 = bcpe_s_of_t(t1, T);
        const bcpe_GG lg1 = bcpe_lgpair(s1, t1, T);
        const double lM = bcpe_between_log(lg1, lgb, tr, w, log_w, T);
        const double f = lM - lm;
        if (ISNAN(f) || f == 0.0) break;
        if (plain_prev && std::fabs(f) >= 0.5 * std::fabs(f_prev)) {
            if (std::fabs(f) > std::fabs(f_prev)) log_h = log_h_prev;
            break;
        }
        if (f > 0.0) hi = log_h; else lo = log_h;
        // d log M / d log h = h f_T(t1) / M
        const double slope = std::exp(log_h + (T.log_fT0 - s1) - lM);
        double next = log_h - f / slope;
        // A converged step is taken before the bracket test: when it is below the
        // resolution of log_h, next == hi (f > 0) or next == lo (f < 0), and the
        // open-interval test below would throw the converged iterate away.
        if (std::fabs(next - log_h) <= 4.0 * DBL_EPSILON * std::max(1.0, std::fabs(log_h))) {
            log_h = next;
            break;
        }
        plain_prev = false;
        if (!(next > lo && next < hi)) {
            next = (lo == R_NegInf) ? hi - 1.0 : 0.5 * (lo + hi);
        } else if (std::fabs(next - log_h) <= BCPE_NEWTON_PLATEAU_STEP * std::max(1.0, std::fabs(log_h))) {
            plain_prev = true;
            f_prev = f;
            log_h_prev = log_h;
        }
        const double step = next - log_h;
        log_h = next;
        if (std::fabs(step) <= 4.0 * DBL_EPSILON * std::max(1.0, std::fabs(log_h))) break;
        if (hi - lo <= 4.0 * DBL_EPSILON * std::max(1.0, std::fabs(log_h))) break;
    }
    return log_h - tr.log_b;
}

// Stable quantile. The requested probability gives the log lower and upper tail
// probabilities lpl, lpu (each accurate where it is the smaller). The T masses below
// and above z are then qL = L + pl*D, qU = pu*D (nu > 0), qL = pl*D, qU = R + pu*D
// (nu < 0), or pl, pu (nu == 0), with L = R = 0.5 Gc(s_b), all in logs; the smaller
// is inverted. Back-transform y = mu exp(sigma z log1p(x)/x), x = nu sigma z, while
// w = 1 + x >= 1/2; below that, the gap h is solved for (bcpe_q_powerlaw) and
// y = mu exp(log(w)/nu).
inline double bcpe_q_stable(const double p, const double mu, const double sigma,
                            const double nu, const double tau, const bool lower_tail,
                            const bool log_p) {
    double lpl, lpu;
    if (!log_p) {
        if (lower_tail) { lpl = std::log(p); lpu = std::log1p(-p); }
        else            { lpu = std::log(p); lpl = std::log1p(-p); }
    } else {
        if (lower_tail) { lpl = p; lpu = bcpe_log1mexp(p); }
        else            { lpu = p; lpl = bcpe_log1mexp(p); }
    }
    const bcpe_T T = bcpe_T_make(tau);
    double lqL = lpl, lqU = lpu, logD = 0.0;
    bcpe_trunc tr;
    bcpe_GG lgb;
    tr.b = R_PosInf; tr.log_b = R_PosInf; tr.s_b = R_PosInf;
    lgb.G = 0.0; lgb.Gc = R_NegInf;
    if (nu != 0.0) {
        tr = bcpe_trunc_make(sigma, nu, T);
        lgb = bcpe_lgpair(tr.s_b, tr.b, T);
        const double lL = -M_LN2 + lgb.Gc;
        logD = bcpe_log1mexp(lL);
        if (nu > 0.0) { lqL = bcpe_logadd(lL, lpl + logD); lqU = lpu + logD; }
        else          { lqL = lpl + logD; lqU = bcpe_logadd(lL, lpu + logD); }
    }
    const bool upper = lqU < lqL;
    const double t = bcpe_qT_abs(upper ? lqU : lqL, T);
    const double z = upper ? t : -t;
    if (ISNAN(z)) return z;
    // z = -Inf / +Inf: the ends of the support of Y (for every nu)
    if (z == R_NegInf) return 0.0;
    if (z == R_PosInf) return R_PosInf;
    const double xx = nu * sigma * z;
    double e;
    if (xx == 0.0) {
        e = sigma * z;
    } else if (xx > 1.0) {
        e = std::log1p(xx) / nu;        // no cancellation here, and xx may be +Inf
    } else if (xx >= BCPE_QUANT_W_MIN - 1.0) {
        e = sigma * z * (std::log1p(xx) / xx);
    } else {
        // Next to the truncation point (nu > 0: the lower side; nu < 0: the upper).
        const double lm = ((nu > 0.0) ? lpl : lpu) + logD;
        e = bcpe_q_powerlaw(lm, 1.0 + xx, tr, lgb, T) / nu;
    }
    return bcpe_mu_exp(mu, e);
}

// ---------------------------------------------------------------------------
// The scalars
// ---------------------------------------------------------------------------

// BCPE density scalar function (log_p = TRUE gives the log density).
inline double fdBCPE_scalar(const double& x,
                            const double& mu,
                            const double& sigma,
                            const double& nu,
                            const double& tau,
                            const bool& log_p) {
    // Parameter validation (uncommented for performance)
    // if (mu <= 0.0 || sigma <= 0.0 || tau <= 0.0) stop("... must be positive");
    // if (!R_FINITE(mu) || !R_FINITE(sigma) || !R_FINITE(nu) || !R_FINITE(tau)) stop("... must be finite");

    if (ISNAN(x) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu) || ISNAN(tau)) return NA_REAL;

    // Below the support, and the (improper) limit at +Inf, carry no density.
    if (x <= 0.0 || x == R_PosInf) return log_p ? R_NegInf : 0.0;

    // (G-d): G1 and tau >= 1, with y/mu, c and s(b) in the normal range.
    const double ratio = x / mu;
    if (tau >= BCPE_GAMLSS_DP_TAU_MIN && bcpe_in_G1(sigma, nu) &&
        ratio >= DBL_MIN && ratio <= DBL_MAX) {
        const bcpe_g_T g = bcpe_g_T_make(tau);
        if (bcpe_g_c_ok(g)) {
            double FTb = 1.0;   // F.T(Inf) = 0.5 * (1 + 1 * 1) for nu == 0
            bool ok = true;
            if (nu != 0.0) {
                const double s_b = bcpe_g_s(1.0 / (sigma * std::fabs(nu)), g);
                ok = s_b >= DBL_MIN;
                FTb = 0.5 * (1.0 + R::pgamma(s_b, g.a, 1.0, 1, 0) * 1.0);
            }
            if (ok) {
                const double ll = bcpe_g_logd(x, ratio, mu, sigma, nu, FTb, g);
                return log_p ? ll : bcpe_g_exp(ll);
            }
        }
    }
    return bcpe_d_stable(x, mu, sigma, nu, tau, log_p);
}

// BCPE CDF scalar function, on the requested tail and scale.
inline double fpBCPE_scalar(const double& q,
                            const double& mu,
                            const double& sigma,
                            const double& nu,
                            const double& tau,
                            const bool& lower_tail,
                            const bool& log_p) {
    // Parameter validation (uncommented for performance): as fdBCPE_scalar.

    if (ISNAN(q) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu) || ISNAN(tau)) return NA_REAL;

    if (q <= 0.0) {
        // Below the support: F = 0, so the upper tail is 1.
        const double cdf = lower_tail ? 0.0 : 1.0;
        return log_p ? std::log(cdf) : cdf;
    }
    if (q == R_PosInf) {
        // Beyond the support: F = 1, so the upper tail is 0.
        const double cdf = lower_tail ? 1.0 : 0.0;
        return log_p ? std::log(cdf) : cdf;
    }

    // The natural-scale lower tail inside (G-p) is gamlss's arithmetic:
    //   FYy1 <- F.T(z); FYy2 <- ifelse(nu > 0, F.T(-1/(sigma*abs(nu))), 0)
    //   FYy3 <- F.T(1/(sigma*abs(nu))); FYy <- (FYy1 - FYy2)/FYy3
    // F.T(-b) and F.T(b) share s(b), so one pgamma serves both.
    if (lower_tail && !log_p && tau >= BCPE_GAMLSS_DP_TAU_MIN) {
        const double ratio = q / mu;
        if (bcpe_in_G1(sigma, nu) && ratio >= DBL_MIN && ratio <= DBL_MAX) {
            const bcpe_g_T g = bcpe_g_T_make(tau);
            if (bcpe_g_c_ok(g)) {
                const double z = bcpe_g_z(ratio, sigma, nu);
                const double s_z = bcpe_g_s(z, g);
                const double s_b = bcpe_g_s(1.0 / (sigma * std::fabs(nu)), g);
                if ((z == 0.0 || s_z >= DBL_MIN) && s_b >= DBL_MIN) {
                    const double FYy1 = 0.5 * (1.0 + R::pgamma(s_z, g.a, 1.0, 1, 0) * bcpe_sign(z));
                    const double Gb = R::pgamma(s_b, g.a, 1.0, 1, 0);
                    const double FYy2 = (nu > 0.0) ? 0.5 * (1.0 + Gb * -1.0) : 0.0;
                    const double FYy3 = 0.5 * (1.0 + Gb * 1.0);
                    if ((z >= 0.0 || FYy1 >= BCPE_GAMLSS_FT_MIN) &&
                        (nu <= 0.0 || FYy1 >= 2.0 * FYy2)) {
                        // gamlss's quotient can round to 1 + 2^-52 (z far above b for
                        // nu > 0); a probability above 1 is not returned (it would make
                        // fqBCPE(fpBCPE(...)) stop): the stable path gives F <= 1.
                        const double F = (FYy1 - FYy2) / FYy3;
                        if (F <= 1.0) return F;
                    }
                }
            }
        }
    }
    return bcpe_p_stable(q, mu, sigma, nu, tau, lower_tail, log_p);
}

// BCPE quantile scalar function: the y with P(Y <= y) = p (lower_tail) or
// P(Y > y) = p, p on the log scale for log_p.
inline double fqBCPE_scalar(const double& p,
                            const double& mu,
                            const double& sigma,
                            const double& nu,
                            const double& tau,
                            const bool& lower_tail,
                            const bool& log_p) {
    // Parameter validation (uncommented for performance): as fdBCPE_scalar, and
    // 0 <= p <= 1 (p <= 0 on the log scale).

    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu) || ISNAN(tau)) return NA_REAL;

    // The ends of the support, and a p outside [0, 1].
    if (log_p) {
        if (p > 0.0) return R_NaN;
        if (p == R_NegInf) return lower_tail ? 0.0 : R_PosInf;
        if (p == 0.0) return lower_tail ? R_PosInf : 0.0;
    } else {
        if (p < 0.0 || p > 1.0) return R_NaN;
        if (p == 0.0) return lower_tail ? 0.0 : R_PosInf;
        if (p == 1.0) return lower_tail ? R_PosInf : 0.0;
    }

    // The natural-scale lower tail inside (G-q'') is gamlss's arithmetic:
    //   pt = p * F.T(b) (nu < 0), p (nu == 0), 1 - (1 - p) * F.T(b) (nu > 0)
    //   s  <- qgamma((2*pt - 1) * sign(pt - 0.5), 1/tau); za <- sign(pt - 0.5) * ((2*s)^(1/tau)) * c
    //   y  <- mu * (nu*sigma*za + 1)^(1/nu)   (nu != 0),   mu * exp(sigma*za)   (nu == 0)
    if (lower_tail && !log_p) {
        const bcpe_g_T g = bcpe_g_T_make(tau);
        if (bcpe_g_c_ok(g)) {
            double pt = p;
            bool ok = true;
            if (nu != 0.0) {
                const double s_b = bcpe_g_s(1.0 / (sigma * std::fabs(nu)), g);
                ok = s_b >= DBL_MIN;
                const double FTb = 0.5 * (1.0 + R::pgamma(s_b, g.a, 1.0, 1, 0) * 1.0);
                pt = (nu < 0.0) ? p * FTb : 1.0 - bcpe_rnd((1.0 - p) * FTb);
            }
            const double m = std::min(pt, 1.0 - pt);
            // m >= 2^-8 * max(1, sigma/w) needs at least m >= 2^-8: test that before qgamma
            if (ok && m >= BCPE_GAMLSS_Q_M_MIN) {
                const double sg = bcpe_sign(pt - 0.5);
                const double s = R::qgamma((2.0 * pt - 1.0) * sg, g.a, 1.0, 1, 0);
                if (pt == 0.5 || s >= DBL_MIN) {
                    const double za = (sg * ::R_pow(2.0 * s, g.a)) * g.c;
                    if (nu == 0.0) {
                        if (m >= BCPE_GAMLSS_Q_M_MIN * std::max(1.0, sigma)) {
                            return mu * bcpe_g_exp(bcpe_rnd(sigma * za));
                        }
                    } else {
                        const double w = bcpe_rnd((nu * sigma) * za) + 1.0;
                        if (w > 0.0 && std::fabs(nu) * std::min(1.0, w) >= BCPE_GAMLSS_Q_NUW_MIN &&
                            m >= BCPE_GAMLSS_Q_M_MIN * std::max(1.0, sigma / w)) {
                            return mu * ::R_pow(w, 1.0 / nu);
                        }
                    }
                }
            }
        }
    }
    return bcpe_q_stable(p, mu, sigma, nu, tau, lower_tail, log_p);
}

// BCPE random generation scalar function: inverts the BCPE CDF at a uniform u in
// [0, 1). The uniform is supplied by the caller rather than drawn here, as for
// frZAGA_scalar: this kernel then agrees with the exported R frBCPE(), which is
// fqBCPE(dqrng::dqrunif(n), ...), value for value under one dqset.seed(), keeps the
// generator out of a hot loop, and imposes no dqrng dependency on a LinkingTo:
// CKutils consumer.
inline double frBCPE_scalar(const double& u,
                            const double& mu,
                            const double& sigma,
                            const double& nu,
                            const double& tau) {
    // Parameter validation (uncommented for performance): as fdBCPE_scalar, and
    // 0 <= u <= 1.
    return fqBCPE_scalar(u, mu, sigma, nu, tau, true, false);
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_BCPE.cpp)
Rcpp::NumericVector fdBCPE(const Rcpp::NumericVector& x,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& log_);

Rcpp::NumericVector fpBCPE(const Rcpp::NumericVector& q,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& lower_tail,
                           const bool& log_p);

Rcpp::NumericVector fqBCPE(const Rcpp::NumericVector& p,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& lower_tail,
                           const bool& log_p);

#endif // DISTR_BCPE_H
