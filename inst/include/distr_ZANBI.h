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
        log_density = std::log(1.0 - nu) + log_fx - std::log(1.0 - std::exp(log_f0));
    }

    return log_p ? log_density : std::exp(log_density);
}

// SIMD-optimised ZANBI CDF scalar function
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

    double cdf;
    if (q < 0) {
        cdf = 0.0;
    } else if (q == 0) {
        cdf = nu;
    } else {
        // F(q) = nu + (1-nu) * (F_NBI(q) - F_NBI(0)) / (1 - F_NBI(0))
        const double cdf0 = fpNBI_scalar(0, mu, sigma, true, false);
        const double cdf1 = fpNBI_scalar(q, mu, sigma, true, false);
        cdf = nu + ((1.0 - nu) * (cdf1 - cdf0) / (1.0 - cdf0));
    }

    if (!lower_tail) cdf = 1.0 - cdf;
    if (log_p) cdf = std::log(cdf);

    return cdf;
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

    if (p_adj <= nu) {
        return 0;
    }

    // Adjust probability for zero-alteration
    const double p_new = (p_adj - nu) / (1.0 - nu) - 1e-10;
    const double cdf0 = fpNBI_scalar(0, mu, sigma, true, false);
    const double p_new2 = cdf0 * (1.0 - p_new) + p_new;

    if (p_new2 <= 0.0) {
        return 0;
    }

    return fqNBI_scalar(p_new2, mu, sigma, true, false);
}

// SIMD-optimised ZANBI random generation scalar function
inline int frZANBI_scalar(const double& mu,
                          const double& sigma,
                          const double& nu) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0 || nu >= 1.0) stop("nu must be between 0 and 1");

    // Generate uniform random number
    const double u = R::runif(0.0, 1.0);

    if (u < nu) {
        return 0;  // Zero-altered part
    } else {
        // Generate from truncated NBI (excluding zero)
        int x;
        do {
            x = frNBI_scalar(mu, sigma);
        } while (x == 0);
        return x;
    }
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

Rcpp::IntegerVector frZANBI(const int& n,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu);

#endif // DISTR_ZANBI_H
