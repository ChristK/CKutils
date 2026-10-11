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

#ifndef DISTR_ZAGA_H
#define DISTR_ZAGA_H

// Header-only scalar API for the Zero-Adjusted Gamma (ZAGA) distribution of
// gamlss.dist: P(Y = 0) = nu and, given Y > 0, Y ~ GA(mu, sigma), the gamma with
// mean mu and coefficient of variation sigma, i.e. shape a = 1 / sigma^2 and
// scale s = mu * sigma^2.
//
// The *_scalar functions below are defined `inline` so that a downstream
// package can `LinkingTo: CKutils`, `#include <distr_ZAGA.h>` and call them
// directly from its own C++ (e.g. in a hot per-row loop) without linking
// against CKutils.so. The vectorised, Rcpp-exported wrappers (declared at the
// bottom) live in src/distr_ZAGA.cpp and call these same inline scalars. They
// use R's own gamma functions (R::dgamma, R::pgamma, R::qgamma), so they must
// be called from a thread that may call into R.
//
// ZAGA is CONTINUOUS: x, q and the returned quantile are doubles. The count
// caller contract documented in recycling_helpers.h (0 <= x, q <= CK_MAX_COUNT)
// does not apply here, and nothing is converted to int.
//
// Like the other kernels these do no validation of mu, sigma and nu; the caller
// owns mu > 0, sigma > 0 and 0 < nu < 1, with mu and sigma finite (the R-facing
// wrappers check them). Outside that contract a kernel returns NaN or a plausible
// but wrong number (sigma = -1 acts as 1, since only sigma^2 is used), and never
// hangs. A scale mu * sigma^2 below the normal range (subnormal) loses accuracy,
// as it does in gamlss.dist. The kernels do define the edges of x, q and p:
//   - NaN (NA) in any argument gives NA_REAL;
//   - density: x < 0 -> 0 (log: -Inf), x == 0 -> nu, x == +Inf -> 0;
//   - CDF:     q < 0 is below the support (F = 0, upper tail 1), q == 0 gives
//     F = nu (handled before any gamma call, so it holds where sigma^2 underflows
//     to 0), and q == +Inf gives F = 1 (upper tail 0), each on the requested tail
//     and scale;
//   - quantile: the smallest x >= 0 with F(x) >= p. p <= nu is the zero mass and
//     gives 0 (on the log scale too: p <= log(nu)); p == 1 gives Inf. A quantile
//     is never negative: R::qgamma returns a negative value at shapes beyond
//     about 1e49 (sigma below about 1e-25), and that comes back as NaN.
// The upper tail of the CDF and of the quantile is worked on directly, never as
// 1 - F or 1 - p, so it keeps its relative accuracy where F rounds to 1. The
// lower tail on the log scale does the same once F > 1/2: log F is log1p(-U) from
// the direct upper tail U, and the quantile is inverted from pu = -expm1(log p),
// not from exp(log p), which rounds to 1. The upper-tail quantile on the log
// scale does so too, from the gamma part's lower tail log(-expm1(r)) where its
// upper tail exp(r) exceeds 1/2. The natural-scale lower tail is the plain
// nu + (1 - nu) * G and its inverse, unchanged.
// The natural-scale density is (1 - nu) * dgamma, but where dgamma underflows (a
// tiny scale mu * sigma^2) it is exp(log1p(-nu) + log dgamma), which stays
// accurate.

#include <Rcpp.h>
#include <cmath>

// ZAGA density scalar function
//   x == 0: nu
//   x  > 0: (1 - nu) * dgamma(x, shape = 1 / sigma^2, scale = mu * sigma^2)
inline double fdZAGA_scalar(const double& x,
                            const double& mu,
                            const double& sigma,
                            const double& nu,
                            const bool& log_p) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0 || nu >= 1.0) stop("nu must be between 0 and 1");

    if (ISNAN(x) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu)) return NA_REAL;

    // Below the support, and the (improper) limit at +Inf, carry no density.
    if (x < 0.0 || x == R_PosInf) return log_p ? R_NegInf : 0.0;

    // The point mass at zero.
    if (x == 0.0) return log_p ? std::log(nu) : nu;

    const double sigma2 = sigma * sigma;
    const double shape  = 1.0 / sigma2;
    const double scale  = mu * sigma2;

    // log1p(-nu) rather than log(1 - nu): for a tiny nu the literal difference
    // rounds to exactly 1 and its log to 0, losing the zero-adjustment term.
    if (log_p) return std::log1p(-nu) + R::dgamma(x, shape, scale, true);

    // R::dgamma on the natural scale underflows, or loses digits to subnormal
    // intermediates, when the density is tiny and the scale mu * sigma^2 is small
    // (at x = 7.45e-18, shape 1, scale 1e-20 it is 75 % too high; at x =
    // 1.004e-250, shape 1e8, scale 1e-258 it is 0 for a true 1.2e-93). Its
    // log form does not, so below 1e-280 the density is the exponential of the log
    // density; above it the plain product is exact.
    const double d = R::dgamma(x, shape, scale, false);
    if (d < 1e-280) {
        return std::exp(std::log1p(-nu) + R::dgamma(x, shape, scale, true));
    }
    return (1.0 - nu) * d;
}

// ZAGA CDF scalar function
//
// For q >= 0, with G the gamma(shape = 1 / sigma^2, scale = mu * sigma^2) CDF:
//   lower tail  F(q)     = nu + (1 - nu) * G(q)     (exactly nu at q == 0)
//   upper tail  P(Y > q) = (1 - nu) * (1 - G(q))
//
// The upper tail is computed directly, from pgamma with lower_tail = FALSE (in
// log space for log_p), and not as 1 - F. F rounds to 1 as soon as the tail
// drops below about 1e-16, so 1 - F is 0, or wrong by orders of magnitude, where
// the tail is far smaller: gamlss.dist::pZAGA(300, 2, 1, 0.1, lower.tail = FALSE)
// is 0 for a true 6.5e-66, and its log is -Inf. The lower tail is clamped to
// <= 1 as a defensive measure: nu + (1 - nu) * G was not seen above 1, but a
// result a ulp above 1 would leave [0, 1].
//
// q == 0 is the point mass: F(0) = nu exactly, so it is returned (or its log, or
// the upper tail 1 - nu) before any gamma call. That keeps it exact, and defined,
// where sigma^2 underflows to 0 and the gamma shape is infinite.
//
// The lower tail with log_p = TRUE has the same problem: log(F) is exactly 0 once
// F rounds to 1 (a true log F of -6.5e-66 at the point above) and loses relative
// accuracy before that. So where the direct upper tail U = (1 - nu) * (1 - G) is
// below 1/2 (F > 1/2) it returns log1p(-U); otherwise log(nu + (1 - nu) * G), as
// F is then not near 1. The natural-scale lower tail (log_p = FALSE) is untouched.
inline double fpZAGA_scalar(const double& q,
                            const double& mu,
                            const double& sigma,
                            const double& nu,
                            const bool& lower_tail,
                            const bool& log_p) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0 || nu >= 1.0) stop("nu must be between 0 and 1");

    if (ISNAN(q) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu)) return NA_REAL;

    if (q < 0.0) {
        // Below the support: F = 0, so the upper tail is 1.
        const double cdf = lower_tail ? 0.0 : 1.0;
        return log_p ? std::log(cdf) : cdf;
    }

    if (q == R_PosInf) {
        // Beyond the support: F = 1, so the upper tail is 0.
        const double cdf = lower_tail ? 1.0 : 0.0;
        return log_p ? std::log(cdf) : cdf;
    }

    if (q == 0.0) {
        // The point mass: F(0) = nu, upper tail 1 - nu. log1p(-nu) rather than
        // log(1 - nu), as in the density.
        if (lower_tail) return log_p ? std::log(nu) : nu;
        return log_p ? std::log1p(-nu) : 1.0 - nu;
    }

    const double sigma2 = sigma * sigma;
    const double shape  = 1.0 / sigma2;
    const double scale  = mu * sigma2;

    if (!lower_tail) {
        if (log_p) return std::log1p(-nu) + R::pgamma(q, shape, scale, false, true);
        return (1.0 - nu) * R::pgamma(q, shape, scale, false, false);
    }

    // log F for F > 1/2: log1p(-U) from the direct upper tail U, never log(F),
    // which is 0 once F rounds to 1. U < 1/2 is F > 1/2; the log of an F at or
    // below 1/2 is well conditioned, so that case falls through to log(F) below.
    if (log_p) {
        const double upper = (1.0 - nu) * R::pgamma(q, shape, scale, false, false);
        if (upper < 0.5) return std::log1p(-upper);
    }

    // F(q) = nu + (1-nu) * G(q); G(0) = 0 makes F(0) exactly nu.
    double cdf = nu + (1.0 - nu) * R::pgamma(q, shape, scale, true, false);
    if (cdf > 1.0) cdf = 1.0;

    return log_p ? std::log(cdf) : cdf;
}

// A quantile is never negative. R::qgamma returns a negative value, or -Inf, at
// shapes beyond about 1e49 (sigma below about 1e-25), which is outside the
// parameter contract; no negative number is a quantile of a distribution on
// [0, Inf), so it is NaN. A NaN passes through (NaN < 0 is false).
inline double fqZAGA_nonneg(const double& x) {
    return x < 0.0 ? R_NaN : x;
}

// Helper of fqZAGA_scalar: the quantile at an upper-tail probability
// pu = P(Y > x) on the natural scale. It is the one inverse of the upper tail,
// used for lower_tail = FALSE and, through pu = -expm1(log p), for the lower tail
// with log_p = TRUE where F > 1/2. shape and scale are those of the gamma part
// (1 / sigma^2 and mu * sigma^2). P(Y > 0) = 1 - nu, so any pu at or above it is
// met by x = 0; pu <= 0 is the upper end of the support.
inline double fqZAGA_upper_natural(const double& pu,
                                   const double& shape,
                                   const double& scale,
                                   const double& nu) {
    if (pu >= 1.0 - nu) return 0.0;
    if (pu <= 0.0) return R_PosInf;
    return fqZAGA_nonneg(R::qgamma(pu / (1.0 - nu), shape, scale, false, false));
}

// ZAGA quantile scalar function
//
// The smallest x >= 0 with F(x) >= p, on the lower-tail scale. Every p up to the
// zero mass nu gives 0; above it the quantile is that of the gamma part at
// (p - nu) / (1 - nu). The upper tail is inverted on its own scale (P(Y > x) =
// p), so a far-upper-tail quantile keeps its relative accuracy: it is not found
// from 1 - p, which rounds to 1 for a tiny p.
//
// The lower tail with log_p = TRUE has the same problem: exp(p) rounds to 1 for
// p > about -5.6e-17 (-2^-54, and gives Inf), and the tail 1 - exp(p) loses
// relative accuracy long before that. So for p > -log(2) (F > 1/2) it inverts on
// the upper scale, from pu = -expm1(p), with the same code as the natural-scale
// upper tail; for p <= -log(2) it keeps pl = exp(p) and the path below. There the
// zero mass is met first: for nu > 1/2, log(nu) lies above -log(2), and a p at or
// below it gives 0 (-expm1(p) can round a ulp below 1 - nu, and the quantile would
// be a tiny positive number). The natural-scale lower tail (log_p = FALSE) is
// untouched.
//
// The upper tail with log_p = TRUE is inverted from r = p - log1p(-nu), the log of
// the gamma part's upper tail. For r > -log(2) that tail exceeds 1/2, and qgamma
// on the log of the lower tail, log(-expm1(r)), is the well-conditioned form
// (R::qgamma(r, ..., FALSE, TRUE) near r = 0 is off by up to 18 %, or NaN or
// non-monotone, for a nu below about 1e-184); otherwise r goes to qgamma directly.
inline double fqZAGA_scalar(const double& p,
                            const double& mu,
                            const double& sigma,
                            const double& nu,
                            const bool& lower_tail,
                            const bool& log_p) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0 || nu >= 1.0) stop("nu must be between 0 and 1");
    // if (p < 0.0 || p > 1.0) stop("p must be >=0 and <=1");

    // NaN/NA guard: base R returns NA for a NaN p, and a NaN parameter would
    // otherwise reach R::qgamma.
    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu)) {
        return NA_REAL;
    }

    const double sigma2 = sigma * sigma;
    const double shape  = 1.0 / sigma2;
    const double scale  = mu * sigma2;

    if (lower_tail) {
        // F > 1/2 on the log scale: exp(p) would round to 1 and give Inf, and
        // 1 - exp(p) would lose the tail, so invert on the upper scale. p >= 0
        // gives pu <= 0, so Inf, as exp(p) >= 1 did.
        if (log_p && p > -M_LN2) {
            // The zero mass first (it can only be above -log(2) for nu > 1/2):
            // pu = -expm1(p) at p == log(nu) is 1 - nu up to rounding, which can
            // fall a ulp short of the test in the helper.
            if (p <= std::log(nu)) return 0.0;
            return fqZAGA_upper_natural(-std::expm1(p), shape, scale, nu);
        }
        const double pl = log_p ? std::exp(p) : p;
        // The zero mass: F(0) = nu >= pl.
        if (pl <= nu) return 0.0;
        // p == 1 is the upper end of the support.
        if (pl >= 1.0) return R_PosInf;
        return fqZAGA_nonneg(R::qgamma((pl - nu) / (1.0 - nu), shape, scale, true, false));
    }

    // Upper tail: p is P(Y > x). P(Y > 0) = 1 - nu, so any p at or above it is
    // met by x = 0.
    if (log_p) {
        if (p >= std::log1p(-nu)) return 0.0;
        if (p == R_NegInf) return R_PosInf;
        // r is the log of the gamma part's upper tail; above -log(2) that tail is
        // over 1/2 and the lower tail log(-expm1(r)) is the one to invert.
        const double r = p - std::log1p(-nu);
        if (r > -M_LN2) {
            return fqZAGA_nonneg(R::qgamma(std::log(-std::expm1(r)), shape, scale, true, true));
        }
        return fqZAGA_nonneg(R::qgamma(r, shape, scale, false, true));
    }
    return fqZAGA_upper_natural(p, shape, scale, nu);
}

// ZAGA random generation scalar function
// Inverts the ZAGA CDF at a uniform u in [0, 1).
//
// The uniform is supplied by the caller rather than drawn here, exactly as for
// frZINBI_scalar: this kernel then agrees with the exported R frZAGA(), which is
// fqZAGA(dqrng::dqrunif(n), ...), value for value under one dqset.seed(), keeps
// the generator out of a hot loop, and imposes no dqrng dependency on a
// LinkingTo: CKutils consumer.
inline double frZAGA_scalar(const double& u,
                            const double& mu,
                            const double& sigma,
                            const double& nu) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0 || nu >= 1.0) stop("nu must be between 0 and 1");
    // if (u < 0.0 || u > 1.0) stop("u must be a uniform in [0, 1]");

    return fqZAGA_scalar(u, mu, sigma, nu, true, false);
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_ZAGA.cpp)
Rcpp::NumericVector fdZAGA(const Rcpp::NumericVector& x,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const Rcpp::NumericVector& nu,
                          const bool& log);

Rcpp::NumericVector fpZAGA(const Rcpp::NumericVector& q,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const Rcpp::NumericVector& nu,
                          const bool& lower_tail,
                          const bool& log_p);

Rcpp::NumericVector fqZAGA(const Rcpp::NumericVector& p,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const Rcpp::NumericVector& nu,
                          const bool& lower_tail,
                          const bool& log_p);

#endif // DISTR_ZAGA_H
