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

#include <Rcpp.h>
#include <cmath>
#include "distr_BNB.h"   // ZABNB scalars are defined in terms of the BNB scalars

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
Rcpp::NumericVector fqZABNB(const Rcpp::NumericVector& p,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& lower_tail,
                           const bool& log_p);

#endif // DISTR_ZABNB_H
