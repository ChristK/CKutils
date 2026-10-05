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
// LinkingTo: CKutils to call the scalars directly does not get it. The two
// kernels that run the Bessel-ratio recursion, ftofySICHEL2_scalar and
// fcdfSICHEL_scalar (and so fdSICHEL_scalar and fpSICHEL_scalar), carry only
// its previous step: they take O(1) memory, but they are O(x) / O(q) in TIME
// (about 15-30 s at 2^31 - 2), so a large-but-legal count is slow. The CDF
// stops as soon as its sum has settled, which is long before q when q lies far
// beyond the mean.
// Before 0.1.34 both stored the whole recursion in std::vector<double>
// workspaces of y + 1 elements (one for the density, two for the CDF): O(y)
// memory, tens of gigabytes for a large y, and a size that overflowed at
// y == INT_MAX.

#include <Rcpp.h>
#include <cmath>
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
//
// The sum of log(tofY[j]) over j = 0..y-1, where tofY is the ratio recursion
//     tofY[0] = (mu / cvec) (1 + 2 sigma mu / cvec)^(-1/2) exp(lbes)
//     tofY[j] = (2 cvec sigma (j + nu) / mu + 1 / tofY[j-1]) (mu / (sigma alpha cvec))^2
// Each step needs only the previous ratio, so the recursion is carried forward
// in one variable: O(1) memory, O(y) time. The updates and the additions run in
// the same order as when tofY was a std::vector<double> of y + 1 elements, so
// every value is the same double. The counter j never passes y, so y == INT_MAX
// does not overflow it.
inline double ftofySICHEL2_scalar(const int& y, const double& mu,
                          const double& sigma, const double& nu,
                          const double& lbes, const double& cvec) {
    if (y <= 0) return 0.0;

    const double alpha = compute_alpha(sigma, mu, cvec);

    double tofY = (mu / cvec) * pow(1.0 + 2.0 * sigma * mu / cvec, -0.5) * exp(lbes);  // tofY[0]

    double sumT = 0.0;
    int j = 0;
    while (j < y) {   // j = 1..y; tofY is tofY[j-1] until the update below
        ++j;
        const double tofY_next = (cvec * sigma * (2.0 * (j + nu) / mu) + (1.0 / tofY)) *
                                 pow(mu / (sigma * alpha * cvec), 2.0);   // tofY[j]
        sumT += log(tofY);   // log(tofY[j-1])
        tofY = tofY_next;
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

// SICHEL CDF scalar: the pmf added at 0..y, in that order, in double precision.
//
// The pmf comes from the recursion of ftofySICHEL2_scalar: ty is tynew[j] and
// lp is the log pmf lpnew[j], and each step needs only the previous pair, so
// the loop takes O(1) memory. (Before 0.1.34 it filled two std::vector<double>s
// of y + 1 elements and added them up in a second pass.) The updates and the
// additions run in the same order, so every sum is the same double. The
// counter j never passes y, so y == INT_MAX does not overflow it.
//
// It returns as soon as the sum has settled: at an index past the mode (the
// term is smaller than the one before), with cdf > 0, whose term leaves the
// sum unchanged (cdf + term == cdf). The pmf is unimodal -- a Poisson mixture
// over a unimodal mixing density is unimodal (Holgate 1970, "The modality of
// some compound Poisson distributions", Biometrika 57), and the generalised
// inverse Gaussian density the Sichel mixes over has a single mode -- so every
// later term is no larger than this one. Added to the same, unchanged sum, each
// is rounded away just as this one is, and adding all of them gives the same
// double as stopping here. fpSICHEL(5e7, mu = 2, ...) thus takes a few hundred
// steps rather than 5e7. A sum that has not settled by q (the mass lies beyond
// q, e.g. mu ~ q) takes all y + 1 terms.
//
// Two more conditions confine the exit to where that argument holds. It is
// about the pmf, but the terms are the recursion's, and they follow the pmf only
// where the recursion is stable. For j + nu < 0 its drive term 2 (j + nu) / mu
// is negative, and for a very negative nu and a small mu the ratios can turn
// negative there: the terms are noise that can rise again or be NaN, and a NaN
// reaches the full sum, which is then NaN where an earlier exit would return a
// finite value. So no exit is taken before j >= -nu, and from there on the
// ratios stay positive. For a few steps after that they can still alternate
// around 1 as the noise dies away, so the next term must fall as well:
// term[j+1] = term[j] * tynew[j] / (j + 1), i.e. tynew[j] < j + 1.
inline double fcdfSICHEL_scalar(const int& y, const double& mu, const double& sigma, const double& nu) {
    if (y < 0) return 0.0;

    const double cvec = compute_cvec(sigma, nu);
    const double alpha = compute_alpha(sigma, mu, cvec);
    const double lbes = compute_lbes(alpha, nu);

    double ty = (mu / cvec) * pow(1.0 + 2.0 * sigma * mu / cvec, -0.5) * exp(lbes);   // tynew[0]
    double lp = -nu * log(sigma * alpha) + log_bessel_k(alpha, nu) -
                log_bessel_k(1.0/sigma, nu);                                           // lpnew[0]

    double term = exp(lp);   // the pmf at 0
    double sumT = 0.0;
    sumT += term;

    int j = 0;
    while (j < y) {   // j = 1..y; ty and lp are tynew[j-1] and lpnew[j-1] until the updates below
        ++j;
        const double ty_next = (cvec * sigma * (2.0 * (j + nu) / mu) + (1.0 / ty)) *
                               pow(mu / (sigma * alpha * cvec), 2.0);                  // tynew[j]
        lp = lp + log(ty) - log(j);                                                    // lpnew[j]
        ty = ty_next;

        const double prev = term;
        term = exp(lp);
        // settled; ty is tynew[j] here
        if (sumT > 0.0 && term < prev && sumT + term == sumT &&
            j + nu >= 0.0 && ty < j + 1.0) return sumT;
        sumT += term;
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
