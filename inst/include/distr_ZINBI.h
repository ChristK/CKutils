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
        log_density = std::log(1.0 - nu) + log_f;
    }

    return log_p ? log_density : std::exp(log_density);
}

// SIMD-optimised ZINBI CDF scalar function
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

    double cdf;
    if (q < 0) {
        cdf = 0.0;
    } else {
        // F(q) = nu + (1-nu) * F_NBI(q)
        const double cdf_nbi = fpNBI_scalar(q, mu, sigma, true, false);
        cdf = nu + (1.0 - nu) * cdf_nbi;
    }

    if (!lower_tail) cdf = 1.0 - cdf;
    if (log_p) cdf = std::log(cdf);

    return cdf;
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
inline int frZINBI_scalar(const double& mu,
                   const double& sigma,
                   const double& nu) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0 || nu >= 1.0) stop("nu must be between 0 and 1");

    // Generate uniform random number
    const double u = R::runif(0.0, 1.0);

    if (u < nu) {
        return 0;  // Zero-inflated part
    } else {
        return frNBI_scalar(mu, sigma);  // Standard NBI part
    }
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

Rcpp::IntegerVector frZINBI(const int& n,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu);

#endif // DISTR_ZINBI_H
