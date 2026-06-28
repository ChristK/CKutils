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

#ifndef DISTR_NBI_H
#define DISTR_NBI_H

// Header-only scalar API for the Negative Binomial type I (NBI) distribution.
//
// The *_scalar functions below are defined `inline` so that a downstream
// package can `LinkingTo: CKutils`, `#include <distr_NBI.h>` and call them
// directly from its own C++ (e.g. in a hot per-row loop) without linking
// against CKutils.so. The vectorised, Rcpp-exported wrappers (declared at the
// bottom) live in src/distr_NBI.cpp and call these same inline scalars.

#include <Rcpp.h>   // brings in the R:: namespace math functions (dnbinom_mu, ...)
#include <cmath>

// SIMD-optimised NBI density scalar function
inline double fdNBI_scalar(const int& x,
                           const double& mu = 1.0,
                           const double& sigma = 1.0,
                           const bool& log_p = false) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (x      < 0.0) stop("x must be >=0");

    // For very small sigma values, use Poisson approximation
    if (sigma < 1e-4) {
        return R::dpois(x, mu, log_p);
    }

    // Standard NBI calculation using negative binomial
    const double size = 1.0 / sigma;
    return R::dnbinom_mu(x, size, mu, log_p);
}

// SIMD-optimised NBI CDF scalar function
inline double fpNBI_scalar(const int& q,
                           const double& mu = 1.0,
                           const double& sigma = 1.0,
                           const bool& lower_tail = true,
                           const bool& log_p = false) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (q      < 0) stop("q must be >=0");

    // For very small sigma values, use Poisson approximation
    if (sigma < 1e-4) {
        return R::ppois(q, mu, lower_tail, log_p);
    }

    // Standard NBI calculation using negative binomial
    const double size = 1.0 / sigma;
    return R::pnbinom_mu(q, size, mu, lower_tail, log_p);
}

// SIMD-optimised NBI quantile scalar function
inline int fqNBI_scalar(const double& p,
                        const double& mu = 1.0,
                        const double& sigma = 1.0,
                        const bool& lower_tail = true,
                        const bool& log_p = false) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0) stop("mu must be greater than 0");
    // if (sigma <= 0) stop("sigma must be greater than 0");
    // if (p < 0.0 || p > 1.0) stop("p must be >=0 and <=1");

    // NaN/NA guard: a NaN p (or NaN mu/sigma) slips past the disabled range
    // checks above and would reach R::qnbinom_mu/qpois(NaN,...), feeding an
    // out-of-range value to float-to-int conversion (UB). Return NA like base R.
    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma)) {
        return NA_INTEGER;
    }

    // For very small sigma values, use Poisson approximation
    if (sigma < 1e-4) {
        return R::qpois(p, mu, lower_tail, log_p);
    }

    // Standard NBI calculation using negative binomial
    const double size = 1.0 / sigma;
    return R::qnbinom_mu(p, size, mu, lower_tail, log_p);
}

// SIMD-optimised NBI random generation scalar function
inline int frNBI_scalar(const double& mu = 1.0,
                        const double& sigma = 1.0) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0) stop("mu must be greater than 0");
    // if (sigma <= 0) stop("sigma must be greater than 0");

    // For very small sigma values, use Poisson approximation
    if (sigma < 1e-4) {
        return R::rpois(mu);
    }

    // Standard NBI generation using negative binomial
    const double size = 1.0 / sigma;
    return R::rnbinom(size, size / (size + mu));
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_NBI.cpp)
Rcpp::NumericVector fdNBI(const Rcpp::NumericVector& x,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const bool& log_p);

Rcpp::NumericVector fpNBI(const Rcpp::NumericVector& q,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const bool& lower_tail,
                          const bool& log_p);

Rcpp::IntegerVector fqNBI(const Rcpp::NumericVector& p,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const bool& lower_tail,
                          const bool& log_p);

Rcpp::IntegerVector frNBI(const int& n,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma);

#endif // DISTR_NBI_H
