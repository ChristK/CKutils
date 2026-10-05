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
//   * fpDPO_scalar adds the densities (about 50 ns each) up to q, but stops once
//     the sum has settled (see the comment in the function). For a finite mu > 0
//     and sigma > 0 that is the width of the mass -- from the first density that
//     does not underflow to roughly 8 to 18 standard deviations (sqrt(mu * sigma))
//     above mu, more for a heavy tail -- and not O(q): a q of INT_MAX - 1 returns
//     at once. A sum that cannot settle still runs to q, with
//     `for (int i = ...; i <= q; i++)`: one whose terms are NaN (a non-finite mu
//     or sigma: the constant is NaN) or all 0 (mu <= 0 or sigma <= 0, which the
//     wrappers reject). That is O(q), and for q == INT_MAX i overflows and the
//     call NEVER RETURNS.
//   * fdDPO_scalar evaluates lgammafn(x + 1), so x == INT_MAX wraps the
//     argument to INT_MIN and returns a silently wrong density.

#include <Rcpp.h>   // brings in the R:: namespace math functions (lgammafn, dpois, ppois, ...)
#include <cfloat>
#include <cmath>
#include <algorithm>
#include "distr_search.h"   // ck_search_start(), CK_SEARCH_MAX

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
  // not `std::max(w, 501.0)`: for a NaN w (a NaN mu or sigma) that keeps the NaN, and
  // an out-of-range float-to-int conversion is undefined behaviour
  if (!(w > 501.0)) return 501;
  return static_cast<int>(std::min(w, 2147483646.0));
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

  // A non-finite mu or sigma has no constant (and the loop below would run its
  // 2^31 iterations, ~33 s, to find that out)
  if (!std::isfinite(mu) || !std::isfinite(sigma)) return R_NaN;

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
  bool converged = false;
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
        { converged = true; break; }
      }
    } else {
      // Conservative termination for normal cases - prioritize accuracy over extreme speed
      int min_iter = std::max(15, static_cast<int>(std::min(expected_mode * 0.7, 2147483646.0)));

      if (j >= min_iter && j > max_term_pos + 8) {
        // Only terminate if we're well past the mode and terms are extremely small
        if (term < max_term * 1e-15) {
          { converged = true; break; }
        }
        // Very conservative decreasing term check
        if (j > max_term_pos + 12 && term < prev_term * 0.01) {
          { converged = true; break; }
        }
      }
    }
    // Past the largest term, once the terms are negligible against it: this
    // ends every tail, also the slowly decaying ones of a large sigma that the
    // rules above leave running. And a sum that found no mass in the window
    // (all terms underflowing) gives up rather than run on.
    if ((j > max_term_pos && term < max_term * 1e-17) ||
        (j >= window && max_term == 0.0)) {
      { converged = true; break; }
    }
    prev_term = term;
  }

  // No term summed, or all underflowed (the mass lies beyond the int range):
  // the constant is unknown, not 1 / 0 = Inf
  // the loop reached the int cap without a stopping rule: the sum needs more terms
  // than an int holds, so the constant is unknown
  if (!converged || !(sumC > 0.0)) return R_NaN;
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
    const double key = mu * 1000 + sigma * 100000 + ly;
    // in range: the slot of a plain int cast; out of range or NaN: slot 0 (an
    // out-of-range double-to-int conversion is undefined behaviour). The slot only
    // spreads the entries: a hit compares mu, sigma and ly below, so a collision
    // costs a recomputation at most.
    const int hash = (key >= 0.0 && key < 2147483647.0) ? static_cast<int>(key) % CACHE_SIZE : 0;

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
//
// The CDF is the sum of the densities from 0 to q (the normalizing constant is
// cached), with two shortcuts that leave the sum unchanged, bit for bit, and a
// direct upper tail:
//  * The head. For q > 64 (a shorter sum is not worth the probe) the sum starts at
//    the first density that does not underflow (ck_search_start, distr_search.h):
//    at a large mu the ones before it are exactly 0, and adding 0 changes nothing.
//  * The settle. Past the largest term, with the terms falling, the sum stops at a
//    term that cannot change it (cdf + t == cdf). Rounding is monotone, so every
//    later term, no larger, leaves it unchanged too, and the full sum to q would
//    give the same double. The terms rise to a mode and fall from there, but for
//    a sigma large against mu (as mu = 16.27, sigma = 7.53) the pmf is not
//    unimodal: a second, shallow mode at 0 makes the terms first fall -- by at most
//    1.5 + log(v) / 2 nats to a valley at v < mu (12 nats at the int limit; 8.4
//    measured up to mu = 1e9, sigma = 1e8) -- and then rise. That dip is far from
//    the 37 nats (53 log 2) below the sum that a positive term needs to be
//    absorbed, so it cannot end the sum early. A term of 0 is absorbed by any sum,
//    though, and the dip can underflow to exactly 0: where mu / sigma is near 745,
//    p(0) is at the underflow limit (at most 5.5e-319 when the dip reaches 0, so
//    the sum is below 1.2e-309 then) and the main mass is still to come. So the
//    sum does not settle before it holds some mass (cdf >= DBL_MIN).
//  * The upper tail. 1 - cdf is 0 once the sum has reached 1, is negative if it
//    rounded above 1, and is wrong by orders of magnitude between: the tail of
//    fpDPO(80, 10, 2) is 2.0e-23 where 1 - cdf is 4.4e-16. So where 1 - cdf would
//    lose digits (cdf > 0.5) the tail is summed from q + 1, with the same settle
//    test; log_p takes the log of that sum.
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

  // The head: the sum starts at the first density that does not underflow, or at
  // q if that lies beyond q (every term of the sum is 0 then)
  int i0 = 0;
  if (q > 64) {
    const long long start = ck_search_start(
        [&](double x) { return fdDPO_scalar(static_cast<int>(x), mu, sigma, true); }, mu);
    i0 = static_cast<int>(std::min<long long>(start, q));
  }

  // Optimized CDF computation using cached normalizing constants
  double cdf = 0.0;
  double prev = -1.0;       // the previous term
  double max_term = 0.0;    // the largest term so far, and where it was
  int max_pos = i0;

  // Sum densities from i0 to q using cached computations, until the sum settles.
  // (The conditions of the stop are pure comparisons; cdf + t == cdf is first as it
  // is false for nearly every term, and then the others are not evaluated.)
  for (int i = i0; i <= q; i++) {
    const double t = fdDPO_scalar(i, mu, sigma, false);
    if (t > max_term) {
      max_term = t;
      max_pos = i;
    } else if (cdf + t == cdf && i > max_pos && t < prev && cdf >= DBL_MIN) {
      break;
    }
    cdf += t;
    prev = t;
  }

  double res = cdf;
  if (!lower_tail) {
    if (cdf > 0.5) {
      // The upper tail itself, from q + 1 to where it settles (or to the largest
      // count there is). The first term is added, unless it is 0: the terms past
      // q have underflowed, and the tail is 0. The terms may still be rising here
      // (q below the mode), and then no term can end the sum: it needs t < up_prev.
      double up = 0.0;
      double up_prev = R_PosInf;
      for (long long j = static_cast<long long>(q) + 1; j <= CK_SEARCH_MAX; j++) {
        const double t = fdDPO_scalar(static_cast<int>(j), mu, sigma, false);
        if (!std::isfinite(t)) return R_NaN;   // defensive: the constant is that of the finite sum above
        if (t < up_prev && up + t == up) break;
        up += t;
        up_prev = t;
      }
      res = up;
    } else {
      res = 1.0 - cdf;
    }
  }
  if (log_p) res = log(res);

  return res;
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
