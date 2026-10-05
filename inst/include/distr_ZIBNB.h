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

#ifndef DISTR_ZIBNB_H
#define DISTR_ZIBNB_H

// Header-only scalar API for the Zero Inflated Beta Negative Binomial (ZIBNB)
// distribution.
//
// The *_scalar functions below are defined `inline` so that a downstream
// package can `LinkingTo: CKutils`, `#include <distr_ZIBNB.h>` and call them
// directly from its own C++ (e.g. in a hot per-row loop) without linking
// against CKutils.so. The vectorised, Rcpp-exported wrappers (declared at the
// bottom) live in src/distr_ZIBNB.cpp and call these same inline scalars.
//
// CALLER CONTRACT (see recycling_helpers.h for the full statement). The *_scalar
// kernels in this package do no bounds checking; the caller owns
//     0 <= x, q <= CK_MAX_COUNT   (INT_MAX - 1)
// fqZIBNB_scalar itself is safe: the fqBNB_search it delegates to scans at most
// the int range (CK_SEARCH_MAX = INT_MAX - 1, NA beyond; ~9 s to scan all of it)
// and gives up earlier on a search that cannot reach p (distr_search.h). The
// contract matters for the DPO, DEL and SICHEL density and CDF kernels, some of
// which do not return at all when it is violated; the BNB ones return, even at
// INT_MAX (fpBNB_scalar is O(q) in time: about 5 s there, less where the terms
// underflow, as it stops then; see distr_BNB.h).

#include <Rcpp.h>
#include <cmath>
#include "distr_BNB.h"   // ZIBNB scalars are defined in terms of the BNB scalars

// dZIBNB ----
inline double fdZIBNB_scalar(const int& x,
                             const double& mu = 1.0,
                             const double& sigma = 1.0,
                             const double& nu = 1.0,
                             const double& tau = 0.1,
                             const bool& log_p = false)
{
    // Parameter validation (commented out for performance; the vectorised
    // wrapper fdZIBNB validates before entering the hot loop)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0) stop("nu must be greater than 0");
    // if (tau   <= 0.0 || tau >= 1.0) stop("tau must be >0 and <1");
    // if (x      < 0) stop("x must be >=0");

    if (x == 0) {
        // P(Y = 0) = tau + (1 - tau) * f(0), computed two ways because neither
        // is accurate over the whole range, and gamlss.dist uses only the first:
        //   * when the result is small (a tiny tau AND a tiny f(0)) the direct
        //     sum of two positives is exact, while the log1p form collapses --
        //     1 - (1-tau)(1-f(0)) rounds to exactly 1 and its log to 0.
        //   * when the result approaches 1 (f(0) -> 1, i.e. a small mu) the
        //     direct sum quantises: 1 - 1.1e-16 is the closest double below 1,
        //     so log() cannot resolve anything finer. The identity
        //     tau + (1-tau)f(0) == 1 - (1-tau)(1 - f(0)) keeps the resolution.
        // 0.5 is the natural switch: below it there is no cancellation to avoid.
        //
        // MEASURED benefit (claude_process/probe_zeroinfl_branch_benefit.R):
        //   none reachable here. The two forms diverge only once
        //   |log f_BNB(0)| falls below about 1.1e-16, but fdBNB_scalar floors
        //   log f_BNB(0) to exactly 0 at mu ~ 1e-14 -- its smallest non-zero
        //   value over mu = 1e-1..1e-20 is 4.97e-14 -- so the upstream floor
        //   always bites first. Kept for correctness and to match
        //   fdZISICHEL_scalar, not because it changes an answer here.
        const double log_f0 = fdBNB_scalar(0, mu, sigma, nu, true);
        const double p_zero = tau + (1.0 - tau) * std::exp(log_f0);
        const double log_density = (p_zero < 0.5)
            ? std::log(p_zero)
            : std::log1p(-(1.0 - tau) * (-std::expm1(log_f0)));
        return log_p ? log_density : std::exp(log_density);
    }

    // For x > 0 the BNB mass is simply scaled: P(X = x) = (1 - tau) f_BNB(x).
    // log1p(-tau) rather than log(1 - tau), which rounds to 0 for a tiny tau.
    const double log_density = std::log1p(-tau) + fdBNB_scalar(x, mu, sigma, nu, true);
    return log_p ? log_density : std::exp(log_density);
}

// pZIBNB ----
inline double fpZIBNB_scalar(const int& q,
                             const double& mu = 1.0,
                             const double& sigma = 1.0,
                             const double& nu = 1.0,
                             const double& tau = 0.1,
                             const bool& lower_tail = true,
                             const bool& log_p = false)
{
    // Parameter validation (commented out for performance; the vectorised
    // wrapper fpZIBNB validates before entering the hot loop)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0) stop("nu must be greater than 0");
    // if (tau   <= 0.0 || tau >= 1.0) stop("tau must be >0 and <1");
    // if (q      < 0) stop("q must be >=0");

    double cdf;
    if (q < 0) {
        cdf = 0.0;
    } else {
        // Zero inflation shifts the whole CDF: F(q) = tau + (1 - tau) F_BNB(q).
        // Unlike the hurdle (ZABNB) case there is no renormalisation, so this
        // needs no special case at q == 0.
        cdf = tau + (1.0 - tau) * fpBNB_scalar(q, mu, sigma, nu, true, false);
    }

    if (!lower_tail) cdf = 1.0 - cdf;
    if (log_p) cdf = std::log(cdf);

    return cdf;
}

// qZIBNB ----
inline double fqZIBNB_scalar(const double& p,
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
  // if (p < 0.0 || p > 1.0001) stop("p must be >=0 and <=1"); //I don't like this but it comes from original function

  // NaN/NA guard: a NaN p (or NaN parameter) slips past the range checks above
  // (every NaN comparison is false) and would otherwise propagate into
  // fqBNB_scalar's search, returning a wrong, non-NA value instead of NA.
  if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu) || ISNAN(tau)) {
    return NA_REAL;
  }

  double p_ = p;
  if (log_p) p_ = exp(p_);
  if (!lower_tail) p_ = 1.0 - p_;

  p_ = (p_ - tau)/(1.0 - tau) - (1e-07);
  if (p_ <= 0) p_ = 0.0;
  return fqBNB_scalar(p_, mu, sigma, nu, true, false);
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_ZIBNB.cpp)
Rcpp::NumericVector fdZIBNB(const Rcpp::NumericVector& x,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& log);

Rcpp::NumericVector fpZIBNB(const Rcpp::NumericVector& q,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& lower_tail,
                           const bool& log_p);

Rcpp::NumericVector fqZIBNB(const Rcpp::NumericVector& p,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& lower_tail,
                           const bool& log_p);

#endif // DISTR_ZIBNB_H
