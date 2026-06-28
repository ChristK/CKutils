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

#ifndef DISTR_MN4_H
#define DISTR_MN4_H

// Header-only scalar API for the 4-category multinomial (MN4) distribution.
//
// The *_scalar functions below are defined `inline` so that a downstream
// package can `LinkingTo: CKutils`, `#include <distr_MN4.h>` and call them
// directly from its own C++ (e.g. in a hot per-row loop) without linking
// against CKutils.so. The vectorised, Rcpp-exported wrappers (declared at the
// bottom) live in src/distr_MN4.cpp and call these same inline scalars.

#include <Rcpp.h>   // brings in the R:: namespace and Rcpp vector types
#include <cmath>

// Scalar helper for MN4 density
inline double fdMN4_scalar(const int& x,
                           const double& mu,
                           const double& sigma,
                           const double& nu,
                           const bool& log_) {
  if (mu <= 0.0) return R_NaN;
  if (sigma <= 0.0) return R_NaN;
  if (nu <= 0.0) return R_NaN;
  if (x < 1 || x > 4) return log_ ? R_NegInf : 0.0;

  double logfy = R_NegInf;
  const double normaliser = 1.0 + mu + sigma + nu;

  if (x == 1) logfy = std::log(mu);
  else if (x == 2) logfy = std::log(sigma);
  else if (x == 3) logfy = std::log(nu);
  else if (x == 4) logfy = 0.0;  // log(1) = 0

  logfy -= std::log(normaliser);

  return log_ ? logfy : std::exp(logfy);
}

// Scalar helper for MN4 distribution function
inline double fpMN4_scalar(const int& q,
                           const double& mu,
                           const double& sigma,
                           const double& nu,
                           const bool& lower_tail,
                           const bool& log_p) {
  if (mu <= 0.0) return R_NaN;
  if (sigma <= 0.0) return R_NaN;
  if (nu <= 0.0) return R_NaN;
  if (q < 1) return lower_tail ? (log_p ? R_NegInf : 0.0) : (log_p ? 0.0 : 1.0);
  if (q >= 4) return lower_tail ? (log_p ? 0.0 : 1.0) : (log_p ? R_NegInf : 0.0);

  const double normaliser = 1.0 + mu + sigma + nu;
  double cdf = 0.0;

  if (q >= 1) cdf = mu / normaliser;
  if (q >= 2) cdf = (mu + sigma) / normaliser;
  if (q >= 3) cdf = (mu + sigma + nu) / normaliser;
  if (q >= 4) cdf = 1.0;

  if (!lower_tail) cdf = 1.0 - cdf;

  return log_p ? std::log(cdf) : cdf;
}

// Scalar helper for MN4 quantile function
inline int fqMN4_scalar(const double& p,
                        const double& mu,
                        const double& sigma,
                        const double& nu,
                        const bool& lower_tail,
                        const bool& log_p) {
  if (mu <= 0.0) return NA_INTEGER;
  if (sigma <= 0.0) return NA_INTEGER;
  if (nu <= 0.0) return NA_INTEGER;

  double p_ = p;
  if (log_p) p_ = std::exp(p);
  if (!lower_tail) p_ = 1.0 - p_;

  if (p_ < 0.0 || p_ > 1.0) return NA_INTEGER;

  const double normaliser = 1.0 + mu + sigma + nu;
  int q = 1;

  if (p_ >= (mu / normaliser)) q = 2;
  if (p_ >= ((mu + sigma) / normaliser)) q = 3;
  if (p_ >= ((mu + sigma + nu) / normaliser)) q = 4;

  return q;
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_MN4.cpp)
Rcpp::NumericVector fdMN4(const Rcpp::IntegerVector& x,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const Rcpp::NumericVector& nu,
                          const bool& log_);

Rcpp::NumericVector fpMN4(const Rcpp::IntegerVector& q,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const Rcpp::NumericVector& nu,
                          const bool& lower_tail,
                          const bool& log_p);

Rcpp::IntegerVector fqMN4(const Rcpp::NumericVector& p,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const Rcpp::NumericVector& nu,
                          const bool& lower_tail,
                          const bool& log_p);

Rcpp::IntegerVector frMN4(const int& n,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const Rcpp::NumericVector& nu);

#endif // DISTR_MN4_H
