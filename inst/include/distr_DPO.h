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

#ifndef DISTR_DPO_H
#define DISTR_DPO_H

// Header-only scalar API for the DPO (Double Poisson) distribution.
//
// The *_scalar functions below are defined `inline` so that a downstream
// package can `LinkingTo: CKutils`, `#include <distr_DPO.h>` and call them
// directly from its own C++ (e.g. in a hot per-row loop) without linking
// against CKutils.so. The vectorised, Rcpp-exported wrappers (declared at the
// bottom) live in src/distr_DPO.cpp and call these same inline scalars.

#include <Rcpp.h>   // brings in the R:: namespace math functions (lgammafn, dpois, ppois, ...)
#include <cmath>

// Optimized scalar version for single computation with random parameters
inline double fdDPOgetC5_C_scalar(const double& mu, const double& sigma,
                              const int& lmu, const int& ly) {
  double   sumC, mus, lmus, lsig2, invs, ls;

  // Fast path for near-Poisson case
  if (std::abs(sigma - 1.0) < 1e-6) {
    return 1.0; // Normalizing constant for Poisson is 1
  }

  // Pre-compute constants outside loops
  mus = mu / sigma;
  lsig2 = -0.5 * log(sigma);
  lmus = log(mu) / sigma - 1;
  invs = 1 / sigma;
  ls = lsig2 - mus;

  sumC = 0.0;

  // Ultra-aggressive early termination using mathematical properties of DPO series
  double prev_term = 0.0;
  double max_term = 0.0;
  int max_term_pos = 0;

  // The DPO series has its mode approximately at mu/sigma for most practical cases
  // We can use this to terminate much more aggressively
  double expected_mode = mu / sigma;
  bool pathological = (mu > 800.0 && sigma > 8.0);

  for (int j = 0; j < ly; j++) {
    double j_log = (j == 0) ? 1.0 : log(static_cast<double>(j));
    double ylogofy = j * j_log;
    double lga = R::lgammafn(j + 1);
    double ym = j - ylogofy;

    double term = exp(ls - lga + ylogofy + j * lmus + invs * ym);
    sumC += term;

    // Track maximum term
    if (term > max_term) {
      max_term = term;
      max_term_pos = j;
    }

    // Ultra-aggressive termination based on mathematical insights:
    if (pathological) {
      // Conservative for pathological cases
      if (j > 100 && term < 1e-15 && term < prev_term * 1e-6) {
        break;
      }
    } else {
      // Conservative termination for normal cases - prioritize accuracy over extreme speed
      int min_iter = std::max(15, static_cast<int>(expected_mode * 0.7));

      if (j >= min_iter && j > max_term_pos + 8) {
        // Only terminate if we're well past the mode and terms are extremely small
        if (term < max_term * 1e-15) {
          break;
        }
        // Very conservative decreasing term check
        if (j > max_term_pos + 12 && term < prev_term * 0.01) {
          break;
        }
      }
    }
    prev_term = term;
  }

  return 1.0 / sumC;
}

// Cache structure for normalizing constants.
// Required by fdDPO_scalar; kept here (rather than in the .cpp) so the inline
// scalar API is self-contained and header-only-linkable. The cache is
// `inline static thread_local` (C++17): `inline` gives it a single definition
// across translation units, and `thread_local` gives each thread its own copy
// so a downstream consumer can call fdDPO_scalar/fpDPO_scalar from a parallel
// (e.g. OpenMP) hot loop without a data race, while still benefiting from
// per-thread memoization.
struct DPOCache {
  static const int CACHE_SIZE = 1024;
  struct CacheEntry {
    double mu, sigma;
    int ly;
    double result;
    bool valid;
  };

  inline static thread_local CacheEntry cache[CACHE_SIZE] = {};

  static double get_or_compute(double mu, double sigma, int ly) {
    // Simple hash for cache lookup
    int hash = (int)(mu * 1000 + sigma * 100000 + ly) % CACHE_SIZE;

    CacheEntry& entry = cache[hash];
    if (entry.valid &&
        std::abs(entry.mu - mu) < 1e-10 &&
        std::abs(entry.sigma - sigma) < 1e-10 &&
        entry.ly == ly) {
      return entry.result;
    }

    // Compute and cache
    double result = fdDPOgetC5_C_scalar(mu, sigma, 1, ly);
    entry.mu = mu;
    entry.sigma = sigma;
    entry.ly = ly;
    entry.result = result;
    entry.valid = true;

    return result;
  }
};

// Optimized scalar density function
inline double fdDPO_scalar(const int& x,
                      const double& mu,
                      const double& sigma,
                      const bool& log_ = false)
{
  if (x < 0) return log_ ? R_NegInf : 0.0;
  if (mu <= 0.0 || sigma <= 0.0) return log_ ? R_NegInf : 0.0;

  // Fast path for near-Poisson case
  if (std::abs(sigma - 1.0) < 1e-6) {
    return R::dpois(x, mu, log_);
  }

  // Use cache for normalizing constant
  int maxV = std::max(x * 3, 500);
  double theC = log(DPOCache::get_or_compute(mu, sigma, maxV + 1));

  double logofx = (x > 0) ? log(static_cast<double>(x)) : 1.0;

  double lh = -0.5 * log(sigma) - (mu/sigma) -
              R::lgammafn(x + 1) + x * logofx - x +
              (x * log(mu))/sigma + x/sigma -
              (x * logofx)/sigma + theC;

  return log_ ? lh : exp(lh);
}

// Helper function for optimised CDF computation
inline double fpDPO_scalar(const int& q,
                      const double& mu,
                      const double& sigma,
                      const bool& lower_tail = true,
                      const bool& log_p = false)
{
  if (q < 0) return (lower_tail) ? ((log_p) ? R_NegInf : 0.0) : ((log_p) ? 0.0 : 1.0);

  // Fast path for near-Poisson case
  if (std::abs(sigma - 1.0) < 1e-6) {
    return R::ppois(q, mu, lower_tail, log_p);
  }

  // Optimized CDF computation using cached normalizing constants
  double cdf = 0.0;

  // Sum densities from 0 to q using cached computations
  for (int i = 0; i <= q; i++) {
    cdf += fdDPO_scalar(i, mu, sigma, false);
  }

  if (!lower_tail) cdf = 1.0 - cdf;
  if (log_p) cdf = log(cdf);

  return cdf;
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_DPO.cpp)
Rcpp::NumericVector fget_C(const Rcpp::IntegerVector& x,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma);

Rcpp::NumericVector fdDPO(const Rcpp::IntegerVector& x,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const bool& log_);

Rcpp::NumericVector fpDPO(const Rcpp::IntegerVector& q,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const bool& lower_tail,
                          const bool& log_p);

Rcpp::NumericVector fqDPO(Rcpp::NumericVector p,
                          const Rcpp::NumericVector& mu,
                          const Rcpp::NumericVector& sigma,
                          const bool& lower_tail,
                          const bool& log_p,
                          const int& max_value);

#endif // DISTR_DPO_H
