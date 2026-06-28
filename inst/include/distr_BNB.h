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

#ifndef DISTR_BNB_H
#define DISTR_BNB_H

// Header-only scalar API for the Beta Negative Binomial (BNB) distribution.
//
// The *_scalar functions below are defined `inline` so that a downstream
// package can `LinkingTo: CKutils`, `#include <distr_BNB.h>` and call them
// directly from its own C++ (e.g. in a hot per-row loop) without linking
// against CKutils.so. The vectorised, Rcpp-exported wrappers (declared at the
// bottom) live in src/distr_BNB.cpp and call these same inline scalars.

#include <Rcpp.h>   // brings in the R:: namespace math functions (lbeta, lgammafn, ...)
#include <cmath>

// SIMD-optimised Beta Negative Binomial density scalar function
inline double fdBNB_scalar(const int& x,
                      const double& mu = 1.0,
                      const double& sigma = 1.0,
                      const double& nu = 1.0,
                      const bool& log = false) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0) stop("nu must be greater than 0");
    // if (x      < 0.0) stop("x must be >=0");

    // Pre-compute commonly used values
    const double inv_sigma = 1.0 / sigma;
    const double inv_nu = 1.0 / nu;
    const double mu_nu_over_sigma = (mu * nu) * inv_sigma;

    const double m = inv_sigma + 1.0;
    const double n = mu_nu_over_sigma;
    const double k = inv_nu;

    // Use lgamma instead of lgammafn for better performance
    const double logL = R::lbeta(x + n, m + k) - R::lbeta(n, m) -
                        R::lgammafn(x + 1) - R::lgammafn(k) + R::lgammafn(x + k);

    return log ? logL : std::exp(logL);
}

// SIMD-optimised Beta Negative Binomial CDF scalar function
inline double fpBNB_scalar(const int& q,
                      const double& mu = 1.0,
                      const double& sigma = 1.0,
                      const double& nu = 1.0,
                      const bool& lower_tail = true,
                      const bool& log_p = false)
  {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0) stop("nu must be greater than 0");
    // if (q      < 0) stop("q must be >=0");

    double cdf = 0.0;

    // Cache parameters for repeated use
    const double inv_sigma = 1.0 / sigma;
    const double inv_nu = 1.0 / nu;
    const double mu_nu_over_sigma = (mu * nu) * inv_sigma;
    const double m = inv_sigma + 1.0;
    const double n = mu_nu_over_sigma;
    const double k = inv_nu;

    // Pre-compute common terms
    const double log_beta_n_m = R::lbeta(n, m);
    const double log_gamma_k = R::lgammafn(k);

    for(int i = 0; i <= q; i++)
    {
      const double log_prob = R::lbeta(i + n, m + k) - log_beta_n_m -
                             R::lgammafn(i + 1) - log_gamma_k + R::lgammafn(i + k);
      cdf += std::exp(log_prob);
    }

    if (!lower_tail) cdf = 1.0 - cdf;
    return log_p ? std::log(cdf) : cdf;
  }

// Optimized quantile search using incremental CDF computation
inline int fqBNB_search(const double& p, const double& mu, const double& sigma, const double& nu) {
    // NaN/NA guard: the vector wrapper (fqBNB) already maps NaN args to NA, but
    // guard here too so the search below can never see NaN. For a NaN p the
    // `cdf >= p` test is always false, so it would otherwise spin to max_iter
    // and return a wrong, non-NA value.
    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu)) {
        return NA_INTEGER;
    }
    // Pre-compute common terms for density calculation
    const double inv_sigma = 1.0 / sigma;
    const double inv_nu = 1.0 / nu;
    const double mu_nu_over_sigma = (mu * nu) * inv_sigma;
    const double m = inv_sigma + 1.0;
    const double n_param = mu_nu_over_sigma;
    const double k = inv_nu;

    const double log_beta_n_m = R::lbeta(n_param, m);
    const double log_gamma_k = R::lgammafn(k);

    double cdf = 0.0;
    const int max_iter = 1000000;

    for (int i = 0; i < max_iter; i++) {
        const double log_prob = R::lbeta(i + n_param, m + k) - log_beta_n_m -
                               R::lgammafn(i + 1) - log_gamma_k + R::lgammafn(i + k);
        cdf += std::exp(log_prob);

        if (cdf >= p) {
            return i;
        }
    }

    return max_iter;
}

// qBNB ----
// fast
inline double fqBNB_scalar(const double& p,
                    const double& mu = 1.0,
                    const double& sigma = 1.0,
                    const double& nu = 1.0,
                    const bool& lower_tail = true,
                    const bool& log_p = false)
{
  double p_ = p;
  if (log_p) p_ = std::exp(p_);
  if (!lower_tail) p_ = 1.0 - p_;

  if (p_ + 1e-09 >= 1.0) {
    return R_PosInf;
  }

  // Use optimized incremental search
  return fqBNB_search(p_, mu, sigma, nu);
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_BNB.cpp)
Rcpp::NumericVector fdBNB(const Rcpp::NumericVector& x,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const Rcpp::NumericVector& nu,
                          const bool& log);

Rcpp::NumericVector fpBNB(const Rcpp::IntegerVector& q,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const Rcpp::NumericVector& nu,
                          const bool& lower_tail,
                          const bool& log_p);

Rcpp::NumericVector fqBNB(const Rcpp::NumericVector& p,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const Rcpp::NumericVector& nu,
                          const bool& lower_tail,
                          const bool& log_p);

#endif // DISTR_BNB_H
