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

#ifndef DISTR_DEL_H
#define DISTR_DEL_H

// Header-only scalar API for the Delaporte (DEL) distribution.
//
// The *_scalar functions (and the helpers they call) below are defined
// `inline` so that a downstream package can `LinkingTo: CKutils`,
// `#include <distr_DEL.h>` and call them directly from its own C++ (e.g. in a
// hot per-row loop) without linking against CKutils.so. The vectorised,
// Rcpp-exported wrappers (declared at the bottom) live in src/distr_DEL.cpp and
// call these same inline scalars.

#include <Rcpp.h>   // brings in the R:: namespace math functions (dpois, dnbinom_mu, ...)
#include <cmath>
#include <vector>   // ftofydel2_scalar builds a std::vector workspace

// dPO (Poisson) density scalar helper
inline double fdPO_scalar(const int &x, const double &mu = 1.0, const double &sigma = 1.0, const bool &log_ = false)
{
  // if (x < 0) stop("x must be >=0");
  // if (mu <= 0.0) stop("mu must be greater than 0");
  // if (sigma <= 0.0) stop("sigma must be greater than 0");
  double fy;
  if (sigma > 1e-04)
  {
    fy = R::dnbinom_mu(x, 1.0 / sigma, mu, log_);
  }
  else
  {
    fy = R::dpois(x, mu, log_);
  }
  return fy;
}

// SIMD optimised ftofydel2 computation (helper used by fdDEL_scalar)
inline double ftofydel2_scalar(const int &y, const double &mu,
                       const double &sigma, const double &nu) {
    if (y <= 0) return 0.0;

    std::vector<double> tofY(y + 2);
    const double mu_nu = mu * nu;
    const double one_minus_nu = 1.0 - nu;
    const double mu_sigma_1_minus_nu = mu * sigma * one_minus_nu;
    const double sigma_1_minus_nu = sigma * one_minus_nu;

    // Initial value
    tofY[0] = mu_nu + mu * one_minus_nu / (1.0 + mu_sigma_1_minus_nu);

    double sumT = 0.0;
    const double inv_sigma_1_minus_nu = 1.0 / sigma_1_minus_nu;
    const double dum_const = 1.0 + 1.0 / mu_sigma_1_minus_nu;

    // Optimized loop with better cache access patterns
    for (int j = 1; j < y + 1; j++) {
        double term = (j + mu_nu + inv_sigma_1_minus_nu -
                      (mu_nu * j) / tofY[j - 1]) / dum_const;
        tofY[j] = term;
        sumT += log(tofY[j - 1]);
    }

    return sumT;
}

// Optimized scalar density function
inline double fdDEL_scalar(const int &x,
                      const double &mu,
                      const double &sigma,
                      const double &nu,
                      const bool &log_ = false)
{
  double logfy = 0.0;
  if (sigma < 1e-04) {
    logfy = R::dpois(x, mu, (int)log_);
  } else {
    const double one_minus_nu = 1.0 - nu;
    double logpy0 = -mu * nu - (1.0 / sigma) *
                    log(1.0 + mu * sigma * one_minus_nu);
    double S = ftofydel2_scalar(x, mu, sigma, nu);
    logfy = logpy0 - lgamma(x + 1) + S;
    if (!log_)
      logfy = exp(logfy);
  }
  return logfy;
}

// CDF helper function matching gamlss.dist algorithm exactly
inline double fpDEL_hlp_fn(const int &q,
                      const double &mu,
                      const double &sigma,
                      const double &nu)
{
  if (q < 0) return 0.0;

  // Match gamlss.dist algorithm exactly: sum(dDEL(0:q, ...))
  double ans = 0.0;

  // Use the same small sigma threshold as gamlss.dist
  const bool use_poisson = (sigma < 1e-04);

  if (use_poisson) {
    // Use Poisson distribution for small sigma (matching gamlss.dist)
    for (int i = 0; i <= q; i++) {
      ans += R::dpois(i, mu, false);
    }
  } else {
    // Sum density values exactly as gamlss.dist does
    for (int i = 0; i <= q; i++) {
      ans += fdDEL_scalar(i, mu, sigma, nu, false);
    }
  }

  return ans;
}

// Optimized scalar CDF function
inline double fpDEL_scalar(const int &q,
                      const double &mu,
                      const double &sigma,
                      const double &nu,
                      const bool &lower_tail = true,
                      const bool &log_p = false)
{
  double cdf = fpDEL_hlp_fn(q, mu, sigma, nu);
  if (!lower_tail)
    cdf = 1.0 - cdf;
  if (log_p)
    cdf = log(cdf);

  return cdf;
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_DEL.cpp)
Rcpp::NumericVector fdDEL(const Rcpp::IntegerVector &x,
                         const Rcpp::NumericVector &mu,
                         const Rcpp::NumericVector &sigma,
                         const Rcpp::NumericVector &nu,
                         const bool &log_);

Rcpp::NumericVector fpDEL(const Rcpp::IntegerVector &q,
                         const Rcpp::NumericVector &mu,
                         const Rcpp::NumericVector &sigma,
                         const Rcpp::NumericVector &nu,
                         const bool &lower_tail,
                         const bool &log_p);

Rcpp::NumericVector fqDEL(Rcpp::NumericVector p,
                         const Rcpp::NumericVector &mu,
                         const Rcpp::NumericVector &sigma,
                         const Rcpp::NumericVector &nu,
                         const bool &lower_tail,
                         const bool &log_p);

#endif // DISTR_DEL_H
