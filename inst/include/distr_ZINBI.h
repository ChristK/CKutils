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

#ifndef DISTR_ZINBI_H
#define DISTR_ZINBI_H

// Header-only scalar API for the Zero-Inflated Negative Binomial type I (ZINBI)
// distribution.
//
// The *_scalar functions below are defined `inline` so that a downstream
// package can `LinkingTo: CKutils`, `#include <distr_ZINBI.h>` and call them
// directly from its own C++ (e.g. in a hot per-row loop) without linking
// against CKutils.so. The vectorised, Rcpp-exported wrappers (declared at the
// bottom) live in src/distr_ZINBI.cpp and call these same inline scalars.
//
// CALLER CONTRACT (see recycling_helpers.h for the full statement). The *_scalar
// kernels in this package do no bounds checking; the caller owns
//     0 <= x, q <= CK_MAX_COUNT   (INT_MAX - 1)
// The ZINBI kernels specifically are safe for any int, because the NBI kernels
// they delegate to are closed form. The contract still matters for the BNB,
// DPO, DEL and SICHEL kernels, some of which do not return at all when it is
// violated.

#include <Rcpp.h>
#include <cmath>
#include "distr_NBI.h"   // ZINBI scalars are defined in terms of the NBI scalars

// SIMD-optimised ZINBI density scalar function
inline double fdZINBI_scalar(const int& x,
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
        // P(X = 0) = nu + (1-nu) * f_NBI(0)
        const double log_f0 = fdNBI_scalar(0, mu, sigma, true);
        log_density = std::log(nu + (1.0 - nu) * std::exp(log_f0));
    } else {
        // P(X = x) = (1-nu) * f_NBI(x) for x > 0
        const double log_f = fdNBI_scalar(x, mu, sigma, true);
        // log1p(-nu) rather than log(1 - nu): for a tiny nu the literal
        // difference rounds to exactly 1 and its log to 0, losing the
        // zero-inflation term entirely. Matches fdZANBI_scalar / fdZABNB_scalar.
        log_density = std::log1p(-nu) + log_f;
    }

    return log_p ? log_density : std::exp(log_density);
}

// SIMD-optimised ZINBI CDF scalar function
//
// For q >= 0, with S_NBI = 1 - F_NBI the NBI upper tail:
//   lower tail  F(q)     = nu + (1 - nu) * F_NBI(q)
//   upper tail  P(X > q) = (1 - nu) * S_NBI(q)
//
// The upper tail is computed directly, from R's pnbinom_mu / ppois with
// lower_tail = FALSE (in log space for log_p), and not as 1 - F. F rounds to 1 as
// soon as the tail drops below about 1e-16, so 1 - F was 0, or wrong by orders of
// magnitude, where the tail is far smaller: fpZINBI(200, 5, .5, .3, lower_tail =
// FALSE) was 0 for a true 1.7e-28. The lower tail is unchanged.
inline double fpZINBI_scalar(const int& q,
                      const double& mu = 1.0,
                      const double& sigma = 1.0,
                      const double& nu = 0.1,
                      const bool& lower_tail = true,
                      const bool& log_p = false) {
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
        if (log_p) return std::log1p(-nu) + fpNBI_scalar(q, mu, sigma, false, true);
        return (1.0 - nu) * fpNBI_scalar(q, mu, sigma, false, false);
    }

    // F(q) = nu + (1-nu) * F_NBI(q)
    const double cdf_nbi = fpNBI_scalar(q, mu, sigma, true, false);
    const double cdf = nu + (1.0 - nu) * cdf_nbi;

    return log_p ? std::log(cdf) : cdf;
}

// SIMD-optimised ZINBI quantile scalar function
inline int fqZINBI_scalar(const double& p,
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

    // NaN/NA guard: a NaN p (or NaN parameter) would otherwise propagate through
    // the arithmetic below into fqNBI_scalar, where it can reach an out-of-range
    // float-to-int cast / runaway search. Base R returns NA for a NaN p.
    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu)) {
        return NA_INTEGER;
    }

    double p_adj = p;
    if (log_p) p_adj = exp(p_adj);
    if (!lower_tail) p_adj = 1.0 - p_adj;

    // Adjust probability for zero-inflation
    const double p_new = (p_adj - nu) / (1.0 - nu) - 1e-10;

    if (p_new <= 0.0) {
        return 0;
    }

    return fqNBI_scalar(p_new, mu, sigma, true, false);
}

// SIMD-optimised ZINBI random generation scalar function
// Inverts the ZINBI CDF at a uniform u in [0, 1).//
// The uniform is supplied by the caller rather than drawn here. That is what
// makes this kernel agree with the exported R frZINBI() exactly: that function is
// fqZINBI(dqrng::dqrunif(n), ...), so the same uniform yields the same variate, and
// feeding this kernel dqrng uniforms reproduces frZINBI() value for value under one
// dqset.seed(). Keeping the RNG out of the kernel also lets a caller hoist the
// generator out of a hot loop, and imposes no dqrng dependency on a
// LinkingTo: CKutils consumer.
//
// NOTE: before CKutils 0.1.30 this took (mu, sigma, nu) and drew from R's own
// stream, producing a different sample from the exported frZINBI() even under a
// fixed seed. Nothing in the package called it.
inline int frZINBI_scalar(const double& u,
                          const double& mu,
                          const double& sigma,
                          const double& nu) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0 || nu >= 1.0) stop("nu must be between 0 and 1");
    // if (u < 0.0 || u > 1.0) stop("u must be a uniform in [0, 1]");

    return fqZINBI_scalar(u, mu, sigma, nu, true, false);
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_ZINBI.cpp)
Rcpp::NumericVector fdZINBI(const Rcpp::NumericVector& x,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const bool& log);

Rcpp::NumericVector fpZINBI(const Rcpp::NumericVector& q,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const bool& lower_tail,
                           const bool& log_p);

Rcpp::IntegerVector fqZINBI(const Rcpp::NumericVector& p,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const bool& lower_tail,
                           const bool& log_p);

#endif // DISTR_ZINBI_H
