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
//
// CALLER CONTRACT (unguarded on purpose -- see recycling_helpers.h for the full
// statement). These kernels do no bounds checking, so the caller must ensure
//     0 <= x, q <= CK_MAX_COUNT   (INT_MAX - 1)
// before calling them; count_to_int() in recycling_helpers.h does that test.
// The vectorised wrappers below already apply it, but a package using
// LinkingTo: CKutils to call the scalars directly does not get it. Here:
//   * fpDPO_scalar accumulates with `for (int i = 0; i <= q; i++)`, so
//     q == INT_MAX overflows i and the call NEVER RETURNS. It is also O(q) in
//     time, so a large-but-legal q is slow rather than wrong.
//   * fdDPO_scalar evaluates lgammafn(x + 1), so x == INT_MAX wraps the
//     argument to INT_MIN and returns a silently wrong density.

#include <Rcpp.h>   // brings in the R:: namespace math functions (lgammafn, dpois, ppois, ...)
#include <cmath>
#include <algorithm>

// Optimized scalar version for single computation with random parameters
// The normalising constant is the inverse of a sum of terms over y = 0, 1, ...
// whose mass lies around mu, with a standard deviation of about
// sqrt(mu * sigma) (taken as sqrt(mu * max(sigma, 1))). The sum has to cover
// that mass. Up to 0.1.34 its callers stopped it at max(3 * x, 500): for small
// x and mu beyond ~300 it missed the mass, so densities in the left tail came
// out many orders of magnitude too large, and once the terms in that window
// underflowed the constant was Inf (fpDPO(0, 5000, 2) = Inf, and with it every
// fpDPO/fqDPO for mu above ~5000 when sigma != 1).
inline double fdDPO_C_sd(const double& mu, const double& sigma) {
  return std::sqrt(mu * std::max(sigma, 1.0));
}

// A window that holds the bulk of the mass: up to 40 standard deviations past
// mu (at least 501, the old minimum). The sum below does not stop there -- a
// large sigma has a slowly decaying tail, and at mu = 2, sigma = 1000 the
// window would still miss 1.7e-6 of the mass -- but at convergence; the window
// serves as the cache key (it depends on mu and sigma only) and as the point
// after which a sum that found no mass at all gives up.
inline int fdDPO_C_window(const double& mu, const double& sigma) {
  const double w = std::ceil(mu + 40.0 * fdDPO_C_sd(mu, sigma)) + 100.0;
  return static_cast<int>(std::min(std::max(w, 501.0), 2147483646.0));
}

// The sum runs from 40 standard deviations below mu (or 0) until its terms,
// past the largest, are negligible -- not to a fixed `ly`, which is kept for
// the callers but no longer limits it -- so the result depends on mu and sigma
// only.
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

  // clamped before the cast: for mu beyond the int range the double is too
  // large for an int (an out-of-range conversion is undefined behaviour)
  const int j0 = static_cast<int>(std::min(
      std::max(0.0, std::floor(mu - 40.0 * fdDPO_C_sd(mu, sigma))), 2147483646.0));
  const int window = std::max(ly, fdDPO_C_window(mu, sigma));
  for (int j = j0; j < 2147483646; j++) {
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
    // Past the largest term, once the terms are negligible against it: this
    // ends every tail, also the slowly decaying ones of a large sigma that the
    // rules above leave running. And a sum that found no mass in the window
    // (all terms underflowing) gives up rather than run on.
    if ((j > max_term_pos && term < max_term * 1e-17) ||
        (j >= window && max_term == 0.0)) {
      break;
    }
    prev_term = term;
  }

  // No term summed, or all underflowed (the mass lies beyond the int range):
  // the constant is unknown, not 1 / 0 = Inf
  if (!(sumC > 0.0)) return R_NaN;
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

  // Use cache for normalizing constant. Its window depends on mu and sigma
  // only (see fdDPO_C_window), not on x, so every x of the same mu and sigma
  // finds it in the cache
  double theC = log(DPOCache::get_or_compute(mu, sigma, fdDPO_C_window(mu, sigma)));

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
