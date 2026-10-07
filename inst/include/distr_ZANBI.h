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

#ifndef DISTR_ZANBI_H
#define DISTR_ZANBI_H

// Header-only scalar API for the Zero-Altered Negative Binomial type I (ZANBI)
// distribution.
//
// The *_scalar functions below are defined `inline` so that a downstream
// package can `LinkingTo: CKutils`, `#include <distr_ZANBI.h>` and call them
// directly from its own C++ (e.g. in a hot per-row loop) without linking
// against CKutils.so. The vectorised, Rcpp-exported wrappers (declared at the
// bottom) live in src/distr_ZANBI.cpp and call these same inline scalars.
//
// CALLER CONTRACT (see recycling_helpers.h for the full statement). The *_scalar
// kernels in this package do no bounds checking; the caller owns
//     0 <= x, q <= CK_MAX_COUNT   (INT_MAX - 1)
// The ZANBI kernels specifically are safe for any int, because the NBI kernels
// they delegate to are closed form. The contract still matters for the BNB,
// DPO, DEL and SICHEL kernels: fpDPO_scalar does not return at q == INT_MAX when
// its sum cannot settle, fdDPO_scalar's log density is wrong there, and the BNB,
// DEL and SICHEL ones are O(q) in time (see recycling_helpers.h).

#include <Rcpp.h>
#include <cmath>
#include "distr_NBI.h"   // ZANBI scalars are defined in terms of the NBI scalars

// SIMD-optimised ZANBI density scalar function
inline double fdZANBI_scalar(const int& x,
                             const double& mu,
                             const double& sigma,
                             const double& nu,
                             const bool& log_p) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0 || nu >= 1.0) stop("nu must be between 0 and 1");
    // if (x      < 0) stop("x must be >=0");

    double log_density;
    if (x == 0) {
        log_density = std::log(nu);
    } else {
        // For x > 0: P(X = x) = (1-nu) * f_NBI(x) / (1 - f_NBI(0))
        const double log_f0 = fdNBI_scalar(0, mu, sigma, true);
        const double log_fx = fdNBI_scalar(x, mu, sigma, true);
        // log1p(-nu) and log(-expm1(log_f0)) are the numerically stable forms of
        // gamlss.dist's log(1 - nu) and log(1 - f_NBI(0)). Identical in exact
        // arithmetic, but the literal differences cancel catastrophically: for a
        // tiny nu, 1 - nu rounds to exactly 1 and the log to 0, and for a small
        // mu (where f_NBI(0) -> 1) 1 - exp(log_f0) can round to exactly 0 and
        // send the density to -Inf. Matches fdZABNB_scalar.
        log_density = std::log1p(-nu) + log_fx - std::log(-std::expm1(log_f0));
    }

    return log_p ? log_density : std::exp(log_density);
}

// SIMD-optimised ZANBI CDF scalar function
//
// For q > 0, with S_NBI = 1 - F_NBI the NBI upper tail:
//   upper tail  P(X > q) = (1 - nu) * S_NBI(q) / (1 - F_NBI(0))
//   lower tail  F(q)     = nu + (1 - nu) * (F_NBI(q) - F_NBI(0)) / (1 - F_NBI(0))
//                        = 1 - P(X > q)
//
// The upper tail is computed directly, from R's pnbinom_mu / ppois with
// lower_tail = FALSE (in log space for log_p), not as 1 - F. F rounds to 1 once the
// tail drops below about 1e-16, so 1 - F was 0, or wrong by orders of magnitude,
// where the tail is far smaller: fpZANBI(200, 5, .5, .3, lower_tail = FALSE) was 0
// for a true 1.9e-28. 1 - F_NBI(0) is -expm1(log f0), as in fdZANBI_scalar: unlike
// the literal difference it stays accurate where f0 rounds to 1 (a tiny mu), where
// the lower tail's ratio of two differences was 0/0 (fpZANBI(1, 1e-17, 1, .5) was
// NaN).
//
// The lower tail keeps its original expression, bit for bit, while
// 1 - F_NBI(0) >= 1e-3: its error there is at most about eps / 1e-3 = 1e-13.
// Below that it is 1 - P(X > q), accurate to a few eps.
inline double fpZANBI_scalar(const int& q,
                             const double& mu,
                             const double& sigma,
                             const double& nu,
                             const bool& lower_tail,
                             const bool& log_p) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0 || nu >= 1.0) stop("nu must be between 0 and 1");
    // if (q      < 0) stop("q must be >=0");

    if (q < 0) {
        // Below the support: F = 0, so the upper tail is 1.
        const double cdf = lower_tail ? 0.0 : 1.0;
        return log_p ? std::log(cdf) : cdf;
    }

    if (!lower_tail) {
        // P(X > 0) = 1 - nu
        if (q == 0) return log_p ? std::log1p(-nu) : 1.0 - nu;

        // 1 - F_NBI(0), without the cancellation of 1 - exp(log f0)
        const double one_minus_f0 = -std::expm1(fdNBI_scalar(0, mu, sigma, true));
        if (log_p) {
            return std::log1p(-nu)
                 + fpNBI_scalar(q, mu, sigma, false, true)
                 - std::log(one_minus_f0);
        }
        return (1.0 - nu) * fpNBI_scalar(q, mu, sigma, false, false) / one_minus_f0;
    }

    double cdf;
    if (q == 0) {
        cdf = nu;
    } else {
        // F(q) = nu + (1-nu) * (F_NBI(q) - F_NBI(0)) / (1 - F_NBI(0))
        const double cdf0 = fpNBI_scalar(0, mu, sigma, true, false);
        if (1.0 - cdf0 >= 1e-3) {
            const double cdf1 = fpNBI_scalar(q, mu, sigma, true, false);
            cdf = nu + ((1.0 - nu) * (cdf1 - cdf0) / (1.0 - cdf0));
        } else {
            // 1 - cdf0 and cdf1 - cdf0 are differences of two numbers that are
            // nearly 1 here: use 1 - P(X > q) instead (a NaN cdf0 comes here too,
            // and stays NaN).
            const double one_minus_f0 = -std::expm1(fdNBI_scalar(0, mu, sigma, true));
            cdf = 1.0 - (1.0 - nu) * fpNBI_scalar(q, mu, sigma, false, false)
                                   / one_minus_f0;
        }
    }

    cdf = std::min(cdf, 1.0);   // rounding can leave the sum above 1
    return log_p ? std::log(cdf) : cdf;
}

// SIMD-optimised ZANBI quantile scalar function
inline int fqZANBI_scalar(const double& p,
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

    // NaN/NA guard: a NaN probability or parameter slips past every range check
    // (NaN comparisons are false) and would reach static_cast<int>(...) UB in
    // fqNBI_scalar. Base R returns NA for a NaN probability.
    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu)) {
        return NA_INTEGER;
    }

    double p_adj = p;
    if (log_p) p_adj = std::exp(p_adj);
    if (!lower_tail) p_adj = 1.0 - p_adj;

    // A p within the rounding slack above the mass nu at 0 is nu itself
    if (p_adj <= nu + CK_P_SLACK) {
        return 0;
    }

    // Adjust probability for zero-alteration: above the mass nu at 0 the variate
    // is the NBI one truncated at 0, so p = nu + (1 - nu) (F_NBI(x) - F_NBI(0)) /
    // (1 - F_NBI(0)). CK_P_SLACK (distr_search.h) is the rounding slack of that
    // sum. gamlss.dist::qZANBI subtracts 1e-10 here, 1e6 times the rounding,
    // which moved whole bands of p one quantile too low and broke q(p(x)) == x.
    // p = 1 stays finite, as callers rely on: it maps to 1 - CK_P_SLACK / (1 - nu),
    // below 1.
    const double p_new = (p_adj - nu - CK_P_SLACK) / (1.0 - nu);
    const double cdf0 = fpNBI_scalar(0, mu, sigma, true, false);
    // The zero-truncation maps the slack to (1 - cdf0) * slack on the base scale, below the rounding of this sum when cdf0 is near 1
    // (a small mu): it is taken off here as well. This also keeps p = 1 below 1 on that scale (a sum that rounds to 1 is NA).
    // A negative result (a tiny cdf0 and p_new) is clamped at 0; std::max keeps a NaN.
    const double p_new2 = std::max(cdf0 * (1.0 - p_new) + p_new - CK_P_SLACK, 0.0);

    // Above the mass nu at 0 the zero-altered variate is at least 1. NA (a
    // quantile an int cannot hold) is kept.
    const int q = fqNBI_scalar(p_new2, mu, sigma, true, false);
    return (q == NA_INTEGER || q >= 1) ? q : 1;
}

// SIMD-optimised ZANBI random generation scalar function
// Inverts the ZANBI CDF at a uniform u in [0, 1).//
// The uniform is supplied by the caller rather than drawn here. That is what
// makes this kernel agree with the exported R frZANBI() exactly: that function is
// fqZANBI(dqrng::dqrunif(n), ...), so the same uniform yields the same variate, and
// feeding this kernel dqrng uniforms reproduces frZANBI() value for value under one
// dqset.seed(). Keeping the RNG out of the kernel also lets a caller hoist the
// generator out of a hot loop, and imposes no dqrng dependency on a
// LinkingTo: CKutils consumer.
//
// NOTE: before CKutils 0.1.30 this took (mu, sigma, nu) and sampled by
// rejection from R's own stream -- a different sample from the exported
// frZANBI() even under a fixed seed, and a loop that spins unboundedly as
// mu -> 0 because almost every draw from the untruncated NBI is a zero.
// Nothing in the package called it.
inline int frZANBI_scalar(const double& u,
                          const double& mu,
                          const double& sigma,
                          const double& nu) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0 || nu >= 1.0) stop("nu must be between 0 and 1");
    // if (u < 0.0 || u > 1.0) stop("u must be a uniform in [0, 1]");

    return fqZANBI_scalar(u, mu, sigma, nu, true, false);
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_ZANBI.cpp)
Rcpp::NumericVector fdZANBI(const Rcpp::NumericVector& x,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const bool& log);

Rcpp::NumericVector fpZANBI(const Rcpp::NumericVector& q,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const bool& lower_tail,
                           const bool& log_p);

Rcpp::IntegerVector fqZANBI(const Rcpp::NumericVector& p,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const bool& lower_tail,
                           const bool& log_p);

#endif // DISTR_ZANBI_H
