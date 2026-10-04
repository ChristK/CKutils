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

#ifndef DISTR_SICHEL_H
#define DISTR_SICHEL_H

// Header-only scalar API for the Sichel (SICHEL) distribution.
//
// The *_scalar functions (and their compute_* helpers) below are defined
// `inline` so that a downstream package can `LinkingTo: CKutils`,
// `#include <distr_SICHEL.h>` and call them directly from its own C++ (e.g.
// in a hot per-row loop) without linking against CKutils.so. The vectorised,
// Rcpp-exported wrappers (declared at the bottom) live in src/distr_SICHEL.cpp
// and call these same inline scalars.
//
// CALLER CONTRACT (unguarded on purpose -- see recycling_helpers.h for the full
// statement). These kernels do no bounds checking, so the caller must ensure
//     0 <= x, q <= CK_MAX_COUNT   (INT_MAX - 1)
// before calling them; count_to_int() in recycling_helpers.h does that test.
// The vectorised wrappers below already apply it, but a package using
// LinkingTo: CKutils to call the scalars directly does not get it. The
// allocating kernels are ftofySICHEL2_scalar, which sizes a std::vector<double>
// as y + 1, and fcdfSICHEL_scalar, which sizes two of them the same way; both
// therefore allocate O(y) memory -- tens of gigabytes for a large y -- and that
// size expression overflows to a negative int at y == INT_MAX. fdSICHEL_scalar
// and fpSICHEL_scalar inherit this through them.

#include <Rcpp.h>
#include <cmath>
#include <vector>
#include "distr_NBI.h"   // fdSICHEL_scalar falls back to the NBI limit

// Helper functions for SICHEL computations

// Compute cvec efficiently
// log K_nu(x), from the exponentially scaled Bessel K: R's bessel_k(x, nu, 2)
// is K_nu(x) * exp(x). The unscaled K (expo = 1) underflows to 0 once x is
// beyond ~700 -- alpha is ~2000 at mu = 2e6, and 1/sigma is 1000 at
// sigma = 0.001 -- so its log was -Inf and differences of such logs NaN.
inline double log_bessel_k(const double& x, const double& nu) {
    return std::log(R::bessel_k(x, nu, 2)) - x;
}

inline double compute_cvec(const double& sigma, const double& nu) {
    return exp(log_bessel_k(1.0/sigma, nu + 1.0) - log_bessel_k(1.0/sigma, nu));
}

// Compute alpha efficiently
inline double compute_alpha(const double& sigma, const double& mu, const double& cvec) {
    return sqrt(1.0 + 2.0 * sigma * mu / cvec) / sigma;
}

// Compute lbes efficiently
inline double compute_lbes(const double& alpha, const double& nu) {
    return log_bessel_k(alpha, nu + 1.0) - log_bessel_k(alpha, nu);
}

// Scalar helper function for tofySICHEL computation
inline double ftofySICHEL2_scalar(const int& y, const double& mu,
                          const double& sigma, const double& nu,
                          const double& lbes, const double& cvec) {
    if (y <= 0) return 0.0;

    const int iy = y + 1;  // This is the key: iy = y + 1
    std::vector<double> tofY(iy);
    const double alpha = compute_alpha(sigma, mu, cvec);

    tofY[0] = (mu / cvec) * pow(1.0 + 2.0 * sigma * mu / cvec, -0.5) * exp(lbes);

    double sumT = 0.0;
    for (int j = 1; j < iy; j++) {  // j < iy, not j < y
        tofY[j] = (cvec * sigma * (2.0 * (j + nu) / mu) + (1.0 / tofY[j-1])) *
                  pow(mu / (sigma * alpha * cvec), 2.0);
        sumT += log(tofY[j-1]);
    }

    return sumT;
}

// CDF helper function
// SICHEL density scalar.
//
// Extracted from the fdSICHEL() wrapper so the density is available header-only
// to a LinkingTo: CKutils consumer (previously only the CDF was), and so that
// fdZISICHEL_scalar can be defined in terms of it rather than duplicating the
// Bessel-function evaluation.
inline double fdSICHEL_scalar(const int& x, const double& mu,
                              const double& sigma, const double& nu,
                              const bool& log_p = false) {
    // Parameter validation (commented out for performance; the vectorised
    // wrapper validates before entering the hot loop)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (x      < 0) stop("x must be >=0");

    // Large sigma with positive nu: use the NBI limit, as gamlss.dist does.
    if (sigma > 10000.0 && nu > 0.0) {
        return fdNBI_scalar(x, mu, 1.0 / nu, log_p);
    }

    const double cvec   = compute_cvec(sigma, nu);
    const double alpha  = compute_alpha(sigma, mu, cvec);
    const double lbes   = compute_lbes(alpha, nu);
    const double sumlty = ftofySICHEL2_scalar(x, mu, sigma, nu, lbes, cvec);

    // x + 1.0 is deliberately computed in double: at x == INT_MAX an int
    // lgamma(x + 1) would wrap the argument to INT_MIN.
    const double logfy = -R::lgammafn(x + 1.0) - nu * std::log(sigma * alpha) +
                         sumlty + log_bessel_k(alpha, nu) -
                         log_bessel_k(1.0 / sigma, nu);

    return log_p ? logfy : std::exp(logfy);
}

inline double fcdfSICHEL_scalar(const int& y, const double& mu, const double& sigma, const double& nu) {
    if (y < 0) return 0.0;

    const int lyp1 = y + 1;
    const double cvec = compute_cvec(sigma, nu);
    const double alpha = compute_alpha(sigma, mu, cvec);
    const double lbes = compute_lbes(alpha, nu);

    std::vector<double> tynew(lyp1);
    std::vector<double> lpnew(lyp1);

    tynew[0] = (mu / cvec) * pow(1.0 + 2.0 * sigma * mu / cvec, -0.5) * exp(lbes);
    lpnew[0] = -nu * log(sigma * alpha) + log_bessel_k(alpha, nu) -
               log_bessel_k(1.0/sigma, nu);

    for (int j = 1; j < lyp1; j++) {
        tynew[j] = (cvec * sigma * (2.0 * (j + nu) / mu) + (1.0 / tynew[j-1])) *
                   pow(mu / (sigma * alpha * cvec), 2.0);
        lpnew[j] = lpnew[j-1] + log(tynew[j-1]) - log(j);
    }

    double sumT = 0.0;
    for (int j = 0; j < lyp1; j++) {
        sumT += exp(lpnew[j]);
    }

    return sumT;
}

// Scalar CDF function for internal use
inline double fpSICHEL_scalar(const int& q, const double& mu, const double& sigma, const double& nu,
                       const bool& lower_tail = true, const bool& log_p = false) {
    double cdf = fcdfSICHEL_scalar(q, mu, sigma, nu);

    if (!lower_tail) cdf = 1.0 - cdf;
    if (log_p) cdf = log(cdf);

    return cdf;
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_SICHEL.cpp)
Rcpp::NumericVector fdSICHEL(const Rcpp::NumericVector& x,
                            const Rcpp::NumericVector& mu,
                            const Rcpp::NumericVector& sigma,
                            const Rcpp::NumericVector& nu,
                            const bool& log_p);

Rcpp::NumericVector fpSICHEL(const Rcpp::NumericVector& q,
                            const Rcpp::NumericVector& mu,
                            const Rcpp::NumericVector& sigma,
                            const Rcpp::NumericVector& nu,
                            const bool& lower_tail,
                            const bool& log_p);

Rcpp::IntegerVector fqSICHEL(Rcpp::NumericVector p,
                            const Rcpp::NumericVector& mu,
                            const Rcpp::NumericVector& sigma,
                            const Rcpp::NumericVector& nu,
                            const bool& lower_tail,
                            const bool& log_p);

#endif // DISTR_SICHEL_H
