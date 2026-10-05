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
//
// CALLER CONTRACT (unguarded on purpose -- see recycling_helpers.h for the full
// statement). These kernels do no bounds checking, so the caller must ensure
//     0 <= x, q <= CK_MAX_COUNT   (INT_MAX - 1)
// before calling them; count_to_int() in recycling_helpers.h does that test.
// The vectorised fdBNB/fpBNB wrappers below already apply it, but a package
// using LinkingTo: CKutils to call the scalars directly does not get it.
// fpBNB_scalar is O(q) in time (about 2.3 ns per term: q = 2^31 - 1024 takes
// about 5 s), but it stops once the terms have fallen below 2e-292 past the
// mode, as they do for a small sigma: the sum is complete by then.

#include <Rcpp.h>   // brings in the R:: namespace math functions (lbeta, lgammafn, ...)
#include <cmath>
#include "distr_search.h"

// The BNB terms term(i) = P(X = i), with n = mu nu / sigma, m = 1/sigma + 1, k = 1/nu:
//   log term(i) = lbeta(i+n, m+k) - lbeta(n, m) - log(i+k) - lbeta(i+1, k),
// as Gamma(i+k) / (Gamma(i+1) Gamma(k)) = 1 / ((i+k) B(i+1, k)). Up to 0.1.34
// the last part was -lgamma(i+1) - lgamma(k) + lgamma(i+k): that forms
// lgamma(i+1) ~ i log i (2e10 at i = 1e9) and rounds lgamma(k) and the argument
// i+k on its grid, so the term was off by ~1e-9 at i = 1e6 and ~1e-6 at 1e9,
// with a bias when k != 1 (the quantile 997 too low at mu = 3e9, nu = 1.3).
// R's lbeta keeps one small argument apart, so this form stays within ~1e-14
// (~1e-12 for k ~ 1000). Consecutive terms: ratio(i) = term(i+1) / term(i)
//   = (i+n)(i+k) / ((i+n+m+k)(i+1)),
// computed as 1 - a near 1 (no rounding of i+n or n+m+k accumulates through the
// products: drift ~1e-13 over 1e8 terms instead of ~1e-8).
struct ck_bnb_terms {
    double n, m, k, mk, one_minus_k, log_beta_n_m;
    ck_bnb_terms(const double& mu, const double& sigma, const double& nu) {
        const double inv_sigma = 1.0 / sigma;
        const double inv_nu = 1.0 / nu;
        n = (mu * nu) * inv_sigma;
        m = inv_sigma + 1.0;
        k = inv_nu;
        mk = m + k;
        one_minus_k = 1.0 - k;
        log_beta_n_m = R::lbeta(n, m);
    }
    inline double log_term(const double& i) const {
        return R::lbeta(i + n, mk) - log_beta_n_m - std::log(i + k) - R::lbeta(i + 1.0, k);
    }
    inline double ratio(const double& i) const {
        const double x = i + n;
        const double d = (x + mk) * (i + 1.0);
        const double a = (x * one_minus_k + mk * (i + 1.0)) / d;   // 1 - ratio
        return (a <= 0.5) ? 1.0 - a : (x * (i + k)) / d;
    }
};

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

    // double argument: no int arithmetic on x (x + 1 wrapped at INT_MAX up to 0.1.34)
    const double logL = ck_bnb_terms(mu, sigma, nu).log_term(static_cast<double>(x));
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

    const ck_bnb_terms T(mu, sigma, nu);
    auto log_term = [&](double i) { return T.log_term(i); };
    double cdf = 0.0;
    if (q >= 0) {
      // the terms before `start` underflow to 0 and add nothing (ck_search_start)
      const long long start = ck_search_start(log_term, mu);
      // the terms fall from this index on: ceil(r), r as in fqBNB_search (0 if r < 0)
      auto first_falling = [&]() {
        const double r = (T.n * (T.k - 1.0) - T.m - T.k) / (T.m + 1.0);
        return static_cast<long long>(std::min(std::max(0.0, std::ceil(r)),
                                               static_cast<double>(CK_SEARCH_MAX)));
      };
      ck_compensated_sum sum;
      double term = 0.0;
      int countdown = 0;
      // long long: q == INT_MAX ends (an int i overflowed and never returned)
      for (long long i = start; i <= static_cast<long long>(q); i++) {
        if (countdown == 0 || term < CK_TERM_TINY) {
          // Underflow exit. term(i + 1) / term(i) crosses 1 once, at r, so from the
          // mode on each term is at most the one before. Once that one (term, of
          // index i - 1) is below CK_TERM_TINY, the terms still to come add up to
          // less than 2^31 * CK_TERM_TINY ~ 4e-283. With the sum above 1e-250,
          // where ulp(sum) / 2 > 7e-267, they cannot change sum + c (the
          // compensation absorbs them): the sum is complete. Only this exit: a
          // general `cdf + term == cdf` would drop the mass of a power-law tail,
          // which the compensated sum is there to keep.
          if (term < CK_TERM_TINY && sum.value() > 1e-250 && i > first_falling()) break;
          term = std::exp(log_term(static_cast<double>(i)));
          countdown = CK_REANCHOR;
        } else {
          term *= T.ratio(static_cast<double>(i - 1));
        }
        --countdown;
        sum.add(term);
      }
      cdf = sum.value();
    }

    if (!lower_tail) cdf = 1.0 - cdf;
    return log_p ? std::log(cdf) : cdf;
  }

// How many terms a quantile search adds before it checks the closed-form bound
// on a quantile beyond the int range (see fqBNB_search).
constexpr long long CK_BNB_BOUND_AFTER = 65536;

// Quantile search: the smallest i with F(i) >= p, found by adding the terms one
// at a time; NA_INTEGER when there is none. Its limits, all in extreme regimes:
//   * O(q) in time, at ~4 ns per unit of the quantile: the median at mu = 3e9
//     (sigma = 0.5, nu = 1; 1.56e9) takes ~6 s, and ~9 s are needed to answer
//     NA when the quantile lies past the int range and the closed-form bound
//     below does not show it (p = 0.9 at the same parameters).
//   * With 1 - p < ~1e-5 and a heavy tail, the steps of the CDF fall below
//     ulp(p), which no sum in double precision resolves, so the answer may be
//     off by a few units or more: +34 at p = 1 - 1e-8, mu = 90, sigma = 10,
//     nu = 1 (169562403 for 169562369).
//   * sigma <~ 1e-4: lbeta(i+n, m+k) - lbeta(n, m) cancels when m = 1/sigma + 1
//     is large, which leaves the CDF off by ~1e-11: 7153450 for 7153453 at
//     p = 0.999999, mu = 1e4, sigma = 1e-4, nu = 100.
inline int fqBNB_search(const double& p, const double& mu, const double& sigma, const double& nu) {
    // NaN/NA guard: the vector wrapper (fqBNB) already maps NaN args to NA, but
    // guard here too so the search below can never see NaN. For a NaN p the
    // `cdf >= p` test is always false, so the search could never stop at p.
    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu)) {
        return NA_INTEGER;
    }
    const ck_bnb_terms T(mu, sigma, nu);
    auto log_term = [&](double i) { return T.log_term(i); };
    // A quantile beyond the int range is reported as not found, without scanning
    // the whole range, when this bound shows it. The terms rise while
    //   term(i + 1) / term(i) = (i + n)(i + k) / ((i + n + m + k)(i + 1)) > 1,
    // i.e. while i < r = (n (k - 1) - m - k) / (m + 1): the largest term is at
    // ceil(r), or at 0, so F(CK_SEARCH_MAX) <= (CK_SEARCH_MAX + 1) * term there
    // (with a margin of 1e-3 in the log for its rounding).
    auto beyond_int = [&]() {
        const double r = (T.n * (T.k - 1.0) - T.m - T.k) / (T.m + 1.0);
        const double mode = std::min(std::max(0.0, std::ceil(r)), static_cast<double>(CK_SEARCH_MAX));
        return p > 0.0 && log_term(mode) + std::log(CK_SEARCH_MAX + 1.0) < std::log(p) - 1e-3;
    };
    // The term at 0 is computed once: it is the first term of the scan, and it
    // tells whether the head of the distribution underflows (a large mu).
    const double first = std::exp(log_term(0.0));
    long long start = 0;
    long long check_at = -1;   // the index at which the scan checks the bound; -1: never
    if (p > 0.0) {
        if (first >= CK_TERM_TINY) {
            // The bound costs a log_term and two logs, which a search that ends
            // within a few terms (nearly all do) does not need: it is checked when
            // the scan reaches index CK_BNB_BOUND_AFTER. The outcome is the same as
            // if it were checked first. A scan that ends sooner either found
            // F(i) >= p, which the bound rules out (F(i) <= (i + 1) * the largest
            // term, and the bound puts (CK_SEARCH_MAX + 1) * the largest term below
            // p), or gave up (NA either way). Only a quantile beyond the int range
            // pays for the wait, up to ~0.3 ms.
            check_at = start + CK_BNB_BOUND_AFTER;
        } else {
            // The head underflows, or nearly (a large mu; a term below CK_TERM_TINY
            // is recomputed from its log at every step): the scan is long and slow,
            // so the bound is checked at once, and the scan starts at the first term
            // that does not underflow (ck_search_start, distr_search.h)
            if (beyond_int()) {
                return NA_INTEGER;
            }
            start = ck_search_start(log_term, mu);
        }
    }

    // No fixed cap: ck_search_gives_up() (distr_search.h) ends a search that
    // cannot reach p, and NA_INTEGER is returned rather than a number. Each
    // term is the previous one times ratio(), recomputed from its log every
    // CK_REANCHOR terms; the CDF is a compensated sum (distr_search.h). About
    // 3.9 ns per term (was ~100: three lgamma/lbeta per term).
    ck_compensated_sum cdf;
    double prev_term = -1.0;
    double term = 0.0;
    int countdown = 0;
    for (int i = static_cast<int>(start); i <= CK_SEARCH_MAX; i++) {
        if (i == check_at && beyond_int()) {
            return NA_INTEGER;
        }
        if (countdown == 0 || term < CK_TERM_TINY) {
            term = (i == 0) ? first : std::exp(log_term(static_cast<double>(i)));
            countdown = CK_REANCHOR;
        } else {
            term *= T.ratio(i - 1.0);
        }
        --countdown;
        if (ck_search_gives_up(term, prev_term, cdf.value())) {
            return NA_INTEGER;
        }
        cdf.add(term);
        if (cdf.value() >= p) {
            return i;
        }
        prev_term = term;
    }
    return NA_INTEGER;
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

  // Use optimized incremental search. It reports "not found" as NA_INTEGER,
  // which must become NA here: converted to double it would read -2147483648.
  const int q = fqBNB_search(p_, mu, sigma, nu);
  return (q == NA_INTEGER) ? NA_REAL : static_cast<double>(q);
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
