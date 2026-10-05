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
//
// CALLER CONTRACT (unguarded on purpose -- see recycling_helpers.h for the full
// statement). These kernels do no bounds checking, so the caller must ensure
//     0 <= x, q <= CK_MAX_COUNT   (INT_MAX - 1)
// before calling them; count_to_int() in recycling_helpers.h does that test.
// The vectorised wrappers below already apply it, but a package using
// LinkingTo: CKutils to call the scalars directly does not get it. Here the
// kernels terminate and use O(1) memory for any int, INT_MAX included, but
// they are O(x) / O(q) in TIME (one log, one division and, for the CDF, one
// exp and one lgamma per index), so a large-but-legal count is slow rather
// than wrong: about a minute at q = 2^31 - 2 for fpDEL_hlp_fn.
// Before 0.1.34, ftofydel2_scalar rebuilt its recurrence in a std::vector of
// y + 2 doubles on every call (O(y) memory, and y + 2 overflowed at the
// boundary), so fpDEL_hlp_fn and the quantile search were O(q^2) in time,
// fpDEL_hlp_fn's `for (int i = 0; i <= q; i++)` never returned at
// q == INT_MAX, and fdDEL_scalar's lgamma(x + 1) wrapped at x == INT_MAX.

#include <Rcpp.h>   // brings in the R:: namespace math functions (dpois, dnbinom_mu, ...)
#include <cmath>

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

// The recurrence of gamlss.dist's tofydel2, carried forward one index at a
// time. With t_j = (j + 1) f(j + 1) / f(j) and S_j = sum_{k < j} log(t_k),
//     log f(j) = logpy0 - lgamma(j + 1) + S_j,
//     logpy0   = -mu nu - (1 / sigma) log(1 + mu sigma (1 - nu)).
// The state (j, t_j, S_j) advances with the same expressions, in the same
// order, as the earlier std::vector loop did, so every value is the same
// double; but it takes O(1) memory and one step per index, where rebuilding
// t_0..t_{y-1} for every y made the CDF and the quantile search O(q^2).
// advance() moves j to j + 1: the caller must not advance past INT_MAX.
// (The Poisson branch, sigma < 1e-4, does not use it.)
struct CkDELRecurrence {
  int j;          // the index
  double t;       // t_j
  double S;       // S_j
  double logpy0;  // log f(0)
  double mu_nu, inv_sigma_1_minus_nu, dum_const;
  CkDELRecurrence(const double &mu, const double &sigma, const double &nu) {
    mu_nu = mu * nu;
    const double one_minus_nu = 1.0 - nu;
    const double mu_sigma_1_minus_nu = mu * sigma * one_minus_nu;
    const double sigma_1_minus_nu = sigma * one_minus_nu;
    t = mu_nu + mu * one_minus_nu / (1.0 + mu_sigma_1_minus_nu);   // t_0
    inv_sigma_1_minus_nu = 1.0 / sigma_1_minus_nu;
    dum_const = 1.0 + 1.0 / mu_sigma_1_minus_nu;
    logpy0 = -mu * nu - (1.0 / sigma) * log(1.0 + mu * sigma * one_minus_nu);
    S = 0.0;
    j = 0;
  }
  inline void advance() {
    S += log(t);
    ++j;
    t = (j + mu_nu + inv_sigma_1_minus_nu - (mu_nu * j) / t) / dum_const;
  }
  // log f(j); j + 1.0, not j + 1, so that j == INT_MAX does not overflow
  inline double log_density() const { return logpy0 - lgamma(j + 1.0) + S; }
};

// S_y = sum_{j < y} log(t_j) (helper used by fdDEL_scalar): O(y) time, O(1)
// memory
inline double ftofydel2_scalar(const int &y, const double &mu,
                       const double &sigma, const double &nu) {
    if (y <= 0) return 0.0;
    CkDELRecurrence r(mu, sigma, nu);
    while (r.j < y) r.advance();   // ends at j == y, so y == INT_MAX is safe
    return r.S;
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
    logfy = logpy0 - lgamma(x + 1.0) + S;   // x + 1.0: no overflow at INT_MAX
    if (!log_)
      logfy = exp(logfy);
  }
  return logfy;
}

// The running CDF F(q) = the densities at 0..q added in that order in double
// precision (gamlss.dist's pDEL is sum(dDEL(0:q, ...)), which R accumulates in
// long double, so the two agree to rounding, not bit for bit) of ONE parameter
// set, extendable to a larger q without starting again. fpDEL_hlp_fn is one
// pass of it; the vector fpDEL keeps one across consecutive elements with the
// same parameters. The recurrence is built for every sigma (the Poisson branch
// does not use it), so a NaN sigma gives NaN terms rather than stale state.
struct CkDELCdf {
  CkDELRecurrence r;
  double mu;
  bool poisson;   // sigma < 1e-04: the terms are Poisson(mu) densities
  double ans;     // the sum of the densities at 0..q
  int q;          // the last index added; -1 before the first
  CkDELCdf(const double &mu_, const double &sigma, const double &nu)
      : r(mu_, sigma, nu), mu(mu_), poisson(sigma < 1e-04), ans(0.0), q(-1) {}
  // F(to), for any 0 <= to <= INT_MAX. A `to` below q returns the sum at q.
  // The loop ends at index `to` before incrementing, so INT_MAX returns.
  inline double advance_to(const int &to) {
    while (q < to) {
      if (poisson) {
        ++q;
        ans += R::dpois(q, mu, false);
      } else {
        if (q >= 0) r.advance();   // r.j == q + 1
        ++q;
        ans += exp(r.log_density());
      }
    }
    return ans;
  }
};

// CDF helper: the sum of the densities at 0..q. One recurrence step per
// index: O(q) time, O(1) memory.
inline double fpDEL_hlp_fn(const int &q,
                      const double &mu,
                      const double &sigma,
                      const double &nu)
{
  if (q < 0) return 0.0;
  CkDELCdf cdf(mu, sigma, nu);
  return cdf.advance_to(q);
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
