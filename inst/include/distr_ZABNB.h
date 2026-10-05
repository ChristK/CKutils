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

#ifndef DISTR_ZABNB_H
#define DISTR_ZABNB_H

// Header-only scalar API for the Zero Adjusted (Hurdle) Beta Negative Binomial
// (ZABNB) distribution.
//
// The *_scalar functions below are defined `inline` so that a downstream
// package can `LinkingTo: CKutils`, `#include <distr_ZABNB.h>` and call them
// directly from its own C++ (e.g. in a hot per-row loop) without linking
// against CKutils.so. The vectorised, Rcpp-exported wrappers (declared at the
// bottom) live in src/distr_ZABNB.cpp and call these same inline scalars.
//
// CALLER CONTRACT (unguarded on purpose -- see recycling_helpers.h for the full
// statement). These kernels do no bounds checking, so the caller must ensure
//     0 <= x, q <= CK_MAX_COUNT   (INT_MAX - 1)
// before calling them; count_to_int() in recycling_helpers.h does that test.
// The vectorised fdZABNB/fpZABNB wrappers below already apply it, but a package
// using LinkingTo: CKutils to call the scalars directly does not get it. The BNB
// kernels these delegate to no longer fail at INT_MAX itself (fpBNB_scalar
// counts with a 64-bit index, fdBNB_scalar works in double), so
// fpZABNB_scalar returns at q == INT_MAX and fdZABNB_scalar is right at
// x == INT_MAX. The cost remains: fpZABNB_scalar is O(q) in time (about 5 s at
// q == INT_MAX, less where the terms underflow, as it stops then; see
// distr_BNB.h).
//
// Parameterisation follows gamlss.dist::dZABNB/pZABNB/qZABNB (Rigby et al.
// 2019): tau is the hurdle probability, i.e. P(Y = 0) = tau exactly, and the
// positive part is the BNB(mu, sigma, nu) mass renormalised by 1 - f_BNB(0).
// This differs from ZIBNB, where the zero mass is tau + (1-tau) f_BNB(0).

#include <Rcpp.h>
#include <cmath>
#include "distr_BNB.h"   // ZABNB scalars are defined in terms of the BNB scalars

// dZABNB ----
inline double fdZABNB_scalar(const int& x,
                             const double& mu = 1.0,
                             const double& sigma = 1.0,
                             const double& nu = 1.0,
                             const double& tau = 0.1,
                             const bool& log_p = false)
{
    // Parameter validation (commented out for performance; the vectorised
    // wrapper fdZABNB validates before entering the hot loop)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0) stop("nu must be greater than 0");
    // if (tau   <= 0.0 || tau >= 1.0) stop("tau must be >0 and <1");
    // if (x      < 0) stop("x must be >=0");

    // P(Y = 0) = tau exactly (hurdle), so return tau itself rather than
    // round-tripping it through exp(log(tau)) as gamlss.dist does -- that
    // round trip is off by up to 1 ulp for no benefit.
    if (x == 0) return log_p ? std::log(tau) : tau;

    // For x > 0: P(X = x) = (1-tau) * f_BNB(x) / (1 - f_BNB(0))
    const double log_f0 = fdBNB_scalar(0, mu, sigma, nu, true);
    const double log_fx = fdBNB_scalar(x, mu, sigma, nu, true);
    // log1p(-tau) and log(-expm1(log_f0)) are the numerically stable forms of
    // gamlss.dist's log(1 - tau) and log(1 - f_BNB(0)): the literal differences
    // cancel catastrophically for a tiny tau, and for a small mu (where
    // f_BNB(0) -> 1, so 1 - exp(log_f0) can round to exactly 0 and send the
    // density to -Inf). Identical in exact arithmetic.
    const double log_density =
        std::log1p(-tau) + log_fx - std::log(-std::expm1(log_f0));

    return log_p ? log_density : std::exp(log_density);
}

// pZABNB ----
inline double fpZABNB_scalar(const int& q,
                             const double& mu = 1.0,
                             const double& sigma = 1.0,
                             const double& nu = 1.0,
                             const double& tau = 0.1,
                             const bool& lower_tail = true,
                             const bool& log_p = false)
{
    // Parameter validation (commented out for performance; the vectorised
    // wrapper fpZABNB validates before entering the hot loop)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0) stop("nu must be greater than 0");
    // if (tau   <= 0.0 || tau >= 1.0) stop("tau must be >0 and <1");
    // if (q      < 0) stop("q must be >=0");

    double cdf;
    if (q < 0) {
        cdf = 0.0;
    } else if (q == 0) {
        cdf = tau;
    } else {
        // F(q) = tau + (1-tau) * (F_BNB(q) - F_BNB(0)) / (1 - F_BNB(0))
        const double cdf0 = fpBNB_scalar(0, mu, sigma, nu, true, false);
        const double cdf1 = fpBNB_scalar(q, mu, sigma, nu, true, false);
        cdf = tau + ((1.0 - tau) * (cdf1 - cdf0) / (1.0 - cdf0));
    }

    if (!lower_tail) cdf = 1.0 - cdf;
    if (log_p) cdf = std::log(cdf);

    return cdf;
}

// qZABNB ----
inline double fqZABNB_scalar(const double& p,
                     const double& mu = 1.0,
                     const double& sigma = 1.0,
                     const double& nu = 1.0,
                     const double& tau = 0.1,
                     const bool& lower_tail = true,
                     const bool& log_p = false)
{
  // if (mu    <= 0) stop("mu must be greater than 0");
  // if (sigma <= 0) stop("sigma must be greater than 0");
  // if (nu    <= 0) stop("nu must be greater than 0");
  // if (tau <= 0.0 || tau >= 1.0) stop("tau must be >0 and <1");
  // if (p < 0.0 || p > 1.0) stop("p must be >=0 and <=1"); //I don't like this but it comes from original function


  // NaN/NA guard: the vector wrapper (fqZABNB) already maps NaN args to NA, but
  // guard here too so a NaN can never slip past the (commented-out) range checks
  // and reach fqBNB_scalar's search, which would return a wrong non-NA value.
  if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu) || ISNAN(tau)) {
    return NA_REAL;
  }

  double p_ = p;
  if (log_p) p_ = exp(p_);
  if (!lower_tail) p_ = 1.0 - p_;

  p_ = (p_ - tau)/(1.0 - tau) - (1e-010);
  double cdf0 = fpBNB_scalar(0, mu, sigma, nu, true, false);
  p_ = cdf0 * (1.0 - p_) + p_;
  if (p_ < 0.0) p_ = 0.0;

  return fqBNB_scalar(p_, mu, sigma, nu, true, false);
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_ZABNB.cpp)
Rcpp::NumericVector fdZABNB(const Rcpp::NumericVector& x,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& log);

Rcpp::NumericVector fpZABNB(const Rcpp::NumericVector& q,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& lower_tail,
                           const bool& log_p);

Rcpp::NumericVector fqZABNB(const Rcpp::NumericVector& p,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& lower_tail,
                           const bool& log_p);

#endif // DISTR_ZABNB_H
