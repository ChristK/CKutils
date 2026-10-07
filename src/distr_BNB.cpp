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

#include <Rcpp.h>
#include <math.h>
#include <Rmath.h>
#include "recycling_helpers.h"
#include "distr_BNB.h"   // canonical header-only scalar definitions
// [[Rcpp::plugins(cpp17)]]

// Enable vectorization hints for modern compilers
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif

using namespace Rcpp;


// BNB *_scalar definitions now live (inline) in inst/include/distr_BNB.h so
// that downstream LinkingTo: CKutils consumers can call them directly.


//' Beta Negative Binomial Distribution Density
//'
//' Probability density function for the Beta Negative Binomial (BNB) distribution
//' with parameters mu (mean), sigma (dispersion), and nu (shape).
//'
//' @param x vector of (non-negative integer) quantiles. A non-integer is truncated to an integer; a count above 2147483646 gives \code{NA}.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of positive shape parameters.
//' @param log logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The probability mass function of the BNB distribution is:
//' \deqn{f(y|\mu,\sigma,\nu) = \frac{\Gamma(y+1/\nu)\mathrm{B}(y+(\mu\nu)/\sigma, 1/\sigma+1/\nu+1)}{\Gamma(y+1)\Gamma(1/\nu)\mathrm{B}((\mu\nu)/\sigma, 1/\sigma+1)}}
//' for \eqn{y = 0, 1, 2, \ldots}, \eqn{\mu > 0}, \eqn{\sigma > 0}, and \eqn{\nu > 0}.
//'
//' @return A numeric vector of density values.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, 
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Single values
//' fdBNB(c(0,1,2,3), mu=2, sigma=1, nu=1)
//' 
//' # Vector inputs with recycling
//' fdBNB(0:5, mu=c(1,2), sigma=0.5, nu=c(1,1.5,2))
//'
//' @export
// [[Rcpp::export]]
NumericVector fdBNB(const NumericVector& x,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& log = false)
  {
  // Recycle vectors to common length
  auto recycled = recycle_vectors(x, mu, sigma, nu);
  const int n = recycled.n;
  
  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    // NaN/NA x -> handled in the compute loop below (NA_REAL). Skip validation
    // here so a NaN x does not trip the `< 0` check (NaN comparisons are false
    // anyway) before we can map it to NA.
    if (ISNAN(recycled.vec1[i])) continue;
    if (recycled.vec1[i] < 0.0) stop("x must be >=0");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0) stop("nu must be greater than 0");
  }

 NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // NA/NaN or a count too large to convert to int -> NA. See count_to_int()
    // in recycling_helpers.h: the unguarded cast is out-of-range float-to-int
    // undefined behaviour and it is not benign on either x86-64 or AArch64.
    int x_i;
    if (!count_to_int(recycled.vec1[i], x_i)) {
      out[i] = NA_REAL;
      continue;
    }
    out[i] = fdBNB_scalar(x_i, recycled.vec2[i],
                          recycled.vec3[i], recycled.vec4[i], log);
  }

  return out;
}

//' Beta Negative Binomial Distribution Function
//'
//' Cumulative distribution function for the Beta Negative Binomial (BNB) distribution
//' with parameters mu (mean), sigma (dispersion), and nu (shape).
//'
//' @param q vector of (non-negative integer) quantiles. A non-integer is truncated to an integer; a count above 2147483646 gives \code{NA}.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of positive shape parameters.
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The cumulative distribution function is computed by summing the probability mass
//' function from 0 to q. Each term follows from the one before by its ratio
//' (recomputed from its logarithm every 1024 terms), and the sum is
//' error-compensated. The cost is O(q), about 2.3 ms per million terms when the
//' tail is long (\code{fpBNB(1e8, 2, 1, 1)} takes 0.23 s). Past the mode the sum stops
//' once the terms underflow, so a short-tailed distribution does not pay for a
//' large q (\code{fpBNB(5e7, 1, 1e-3, 1)} takes under a millisecond). The
//' result never exceeds 1, so an upper tail is never negative.
//'
//' @return A numeric vector of cumulative probabilities.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, 
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Single values
//' fpBNB(c(0,1,2,3), mu=2, sigma=1, nu=1)
//' 
//' # Vector inputs with recycling
//' fpBNB(0:5, mu=c(1,2), sigma=0.5, nu=c(1,1.5,2))
//'
//' @export
// [[Rcpp::export]]
NumericVector fpBNB(const IntegerVector& q,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu,
                      const bool& lower_tail = true,
                      const bool& log_p = false)
  {
  // Recycle vectors to common length (handles IntegerVector->NumericVector conversion automatically)
  auto recycled = recycle_vectors(q, mu, sigma, nu);
  const int n = recycled.n;
  
  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    // NaN/NA q -> handled in the compute loop below (NA_REAL). Skip validation
    // here so a NaN q does not trip the `< 0` check (NaN comparisons are false
    // anyway) before we can map it to NA.
    if (ISNAN(recycled.vec1[i])) continue;
    if (recycled.vec1[i] < 0) stop("q must be >=0");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0) stop("nu must be greater than 0");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // NA/NaN or a count too large to convert to int -> NA. See count_to_int()
    // in recycling_helpers.h: the unguarded cast is out-of-range float-to-int
    // undefined behaviour and it is not benign on either x86-64 or AArch64.
    int q_i;
    if (!count_to_int(recycled.vec1[i], q_i)) {
      out[i] = NA_REAL;
      continue;
    }
    out[i] = fpBNB_scalar(q_i, recycled.vec2[i],
                          recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }

    return out;
  }


//' Beta Negative Binomial Quantile Function
//'
//' Quantile function for the Beta Negative Binomial (BNB) distribution
//' with parameters mu (mean), sigma (dispersion), and nu (shape).
//'
//' @param p vector of probabilities.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of positive shape parameters.
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The quantile is the smallest integer x with P(X <= x) >= p. It is found by
//' scanning the probability mass function upwards from 0 and adding the terms
//' until the sum reaches p: each term follows from the one before by its ratio
//' (recomputed from its logarithm every 1024 terms), and the sum is
//' error-compensated. There is no cap on the number of terms, so the cost is
//' O(x), about 4 ms per million terms: with sigma = 0.5 and nu = 1, mu = 2e7 (quantile 1.04e7)
//' takes 0.04 s, and mu = 3e9 (quantile 1.56e9) about 6 s.
//'
//' A quantile beyond 2147483646, the largest integer this function returns, is
//' \code{NA}, with a warning. When the head of the distribution underflows (a
//' large mu) a closed-form bound shows this at once. Otherwise the bound is
//' checked once the scan has added 65,536 terms; if it shows the quantile is
//' beyond the range the answer is \code{NA} within milliseconds, and if it does
//' not, the scan runs on until it finds the quantile or reaches the end of the
//' range (about 8 s at mu = 3e9, sigma = 0.5, nu = 1, p = 0.9, which is
//' \code{NA}).
//'
//' \code{p >= 1} gives \code{Inf}. A \code{p} so close to 1 that the CDF cannot
//' resolve it in double precision (a heavy tail) can give a result off by a few
//' units or more (+34 at \code{p = 1 - 1e-8}, mu = 90, sigma = 10, nu = 1), or
//' \code{NA} with a warning.
//'
//' @return A numeric vector of quantiles (whole numbers, \code{Inf} for
//'   \code{p >= 1}, \code{NA} where none was found).
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, 
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Single values
//' fqBNB(c(0.1, 0.5, 0.9), mu=2, sigma=1, nu=1)
//' 
//' # Vector inputs with recycling
//' fqBNB(c(0.25, 0.75), mu=c(1,2), sigma=0.5, nu=c(1,1.5,2))
//'
//' @export
// [[Rcpp::export]]
NumericVector fqBNB(const NumericVector& p,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu,
                      const bool& lower_tail = true,
                      const bool& log_p = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(p, mu, sigma, nu);
  const int n = recycled.n;
  
  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    // NaN/NA in any argument -> handled in the compute loop below (NA_REAL).
    // Skip validation here so a NaN does not slip past the range checks (NaN
    // comparisons are false) before we can map it to NA.
    if (ISNAN(recycled.vec1[i]) || ISNAN(recycled.vec2[i]) ||
        ISNAN(recycled.vec3[i]) || ISNAN(recycled.vec4[i])) continue;
    check_prob(recycled.vec1[i], log_p, 1.0001, "p must be >=0 and <=1");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0) stop("nu must be greater than 0");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // NaN/NA in any argument -> NA. Without this, a NaN p slips past the [0,1]
    // checks and reaches fqBNB_search, whose `cdf >= p` is always false for NaN,
    // so the search could never stop at p.
    if (ISNAN(recycled.vec1[i]) || ISNAN(recycled.vec2[i]) ||
        ISNAN(recycled.vec3[i]) || ISNAN(recycled.vec4[i])) {
      out[i] = NA_REAL;
      continue;
    }
    out[i] = fqBNB_scalar(recycled.vec1[i], recycled.vec2[i],
                          recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }

  // A non-NA input that gave NA: the search gave up (outside the loop above,
  // which SIMD_HINT declares free of dependencies between iterations)
  bool not_found = false;
  for (int i = 0; i < n && !not_found; i++)
    not_found = ISNAN(out[i]) && !(ISNAN(recycled.vec1[i]) ||
        ISNAN(recycled.vec2[i]) ||
        ISNAN(recycled.vec3[i]) ||
        ISNAN(recycled.vec4[i]));
  if (not_found)
    warning("NAs produced: a quantile was not found (the cumulative probability stops "
            "short of p)");
  return out;
}
