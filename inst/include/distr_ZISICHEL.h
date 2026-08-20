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

#ifndef DISTR_ZISICHEL_H
#define DISTR_ZISICHEL_H

// Header-only API for the Zero-Inflated Sichel (ZISICHEL) distribution.
//
// The *_scalar functions below are defined `inline` so that a downstream
// package can `LinkingTo: CKutils`, `#include <distr_ZISICHEL.h>` and call them
// directly from its own C++ (e.g. in a hot per-row loop) without linking
// against CKutils.so. They are thin: ZISICHEL is the Sichel distribution with
// its zero probability inflated, so they delegate to the SICHEL scalars in
// distr_SICHEL.h. The vectorised, Rcpp-exported wrappers (declared at the
// bottom) live in src/distr_ZISICHEL.cpp and call these same inline scalars.
//
// CALLER CONTRACT (see recycling_helpers.h for the full statement). The
// *_scalar kernels in this package do no bounds checking; the caller owns
//     0 <= x, q <= CK_MAX_COUNT   (INT_MAX - 1)
// The vectorised wrappers below already apply it via count_to_int(), but a
// package using LinkingTo: CKutils to call the scalars directly does not get
// it. fpSICHEL_scalar, which these wrappers delegate to, allocates O(y) memory
// and overflows its workspace size at y == INT_MAX.

#include <Rcpp.h>
#include <cmath>
#include "distr_SICHEL.h"   // ZISICHEL scalars delegate to the SICHEL scalars

// dZISICHEL ----
inline double fdZISICHEL_scalar(const int& x,
                                const double& mu = 1.0,
                                const double& sigma = 1.0,
                                const double& nu = -0.5,
                                const double& tau = 0.1,
                                const bool& log_p = false)
{
    // Parameter validation (commented out for performance; the vectorised
    // wrapper fdZISICHEL validates before entering the hot loop)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (tau   <= 0.0 || tau >= 1.0) stop("tau must be between 0 and 1");
    // if (x      < 0) stop("x must be >=0");

    if (x == 0) {
        // P(Y = 0) = tau + (1 - tau) * f(0), computed two ways because neither
        // is accurate over the whole range, and gamlss.dist uses only the first:
        //   * when the result is small (a tiny tau AND a tiny f(0)) the direct
        //     sum of two positives is exact, while the log1p form collapses --
        //     1 - (1-tau)(1-f(0)) rounds to exactly 1 and its log to 0.
        //   * when the result approaches 1 (f(0) -> 1, i.e. a small mu) the
        //     direct sum quantises: 1 - 1.1e-16 is the closest double below 1,
        //     so log() cannot resolve anything finer. The identity
        //     tau + (1-tau)f(0) == 1 - (1-tau)(1 - f(0)) keeps the resolution.
        // 0.5 is the natural switch: below it there is no cancellation to avoid.
        //
        // MEASURED benefit (claude_process/probe_zeroinfl_branch_benefit.R):
        //   a narrow window around mu ~ 1e-15, where log f_SICHEL(0) is
        //   -9.99e-16: the naive form returns -8.88e-16 against the correct
        //   -8.99e-16, a 1.2% relative error. Below mu ~ 1e-16 f_SICHEL(0)
        //   floors to 0 and both forms agree again. The absolute effect is
        //   negligible -- a density of 1-9e-16 either way -- so this is a
        //   correctness nicety, not a materially better answer.
        const double log_f0 = fdSICHEL_scalar(0, mu, sigma, nu, true);
        const double p_zero = tau + (1.0 - tau) * std::exp(log_f0);
        const double log_density = (p_zero < 0.5)
            ? std::log(p_zero)
            : std::log1p(-(1.0 - tau) * (-std::expm1(log_f0)));
        return log_p ? log_density : std::exp(log_density);
    }

    // For x > 0 the Sichel mass is simply scaled: P(X = x) = (1 - tau) f(x).
    // log1p(-tau) rather than log(1 - tau), which rounds to 0 for a tiny tau.
    const double log_density =
        std::log1p(-tau) + fdSICHEL_scalar(x, mu, sigma, nu, true);
    return log_p ? log_density : std::exp(log_density);
}

// pZISICHEL ----
inline double fpZISICHEL_scalar(const int& q,
                                const double& mu = 1.0,
                                const double& sigma = 1.0,
                                const double& nu = -0.5,
                                const double& tau = 0.1,
                                const bool& lower_tail = true,
                                const bool& log_p = false)
{
    // Parameter validation (commented out for performance; the vectorised
    // wrapper fpZISICHEL validates before entering the hot loop)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (tau   <= 0.0 || tau >= 1.0) stop("tau must be between 0 and 1");
    // if (q      < 0) stop("q must be >=0");

    double cdf;
    if (q < 0) {
        cdf = 0.0;
    } else {
        // Zero inflation shifts the whole CDF: F(q) = tau + (1 - tau) F(q).
        // No renormalisation, so no special case at q == 0.
        cdf = tau + (1.0 - tau) * fpSICHEL_scalar(q, mu, sigma, nu, true, false);
    }

    if (!lower_tail) cdf = 1.0 - cdf;
    if (log_p) cdf = std::log(cdf);

    return cdf;
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_ZISICHEL.cpp)
Rcpp::NumericVector fdZISICHEL(const Rcpp::NumericVector& x,
                              const Rcpp::NumericVector& mu,
                              const Rcpp::NumericVector& sigma,
                              const Rcpp::NumericVector& nu,
                              const Rcpp::NumericVector& tau,
                              const bool& log);

Rcpp::IntegerVector fqZISICHEL(Rcpp::NumericVector p,
                              const Rcpp::NumericVector& mu,
                              const Rcpp::NumericVector& sigma,
                              const Rcpp::NumericVector& nu,
                              const Rcpp::NumericVector& tau,
                              const bool& lower_tail,
                              const bool& log_p);

Rcpp::NumericVector fpZISICHEL(const Rcpp::NumericVector& q,
                              const Rcpp::NumericVector& mu,
                              const Rcpp::NumericVector& sigma,
                              const Rcpp::NumericVector& nu,
                              const Rcpp::NumericVector& tau,
                              const bool& lower_tail,
                              const bool& log_p);

#endif // DISTR_ZISICHEL_H
