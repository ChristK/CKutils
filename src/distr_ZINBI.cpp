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
#include "distr_NBI.h"
#include "distr_ZINBI.h"   // canonical header-only scalar definitions
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// Zero-inflated NBI functions
// ZINBI *_scalar definitions now live (inline) in inst/include/distr_ZINBI.h


//' Zero-Inflated Negative Binomial Type I Distribution Density
//'
//' Probability density function for the Zero-Inflated Negative Binomial type I (ZINBI) 
//' distribution with parameters mu (mean), sigma (dispersion), and nu (zero-inflation probability).
//'
//' @param x vector of (non-negative integer) quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of zero-inflation probabilities (0 < nu < 1).
//' @param log logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The Zero-Inflated NBI distribution is a mixture of a degenerate distribution
//' at zero and a standard NBI distribution. The probability mass function is:
//' \deqn{P(Y = 0) = \nu + (1-\nu) f_{NBI}(0|\mu,\sigma)}
//' \deqn{P(Y = y) = (1-\nu) f_{NBI}(y|\mu,\sigma) \quad \text{for } y > 0}
//' where \eqn{f_{NBI}} is the NBI probability mass function.
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
//' fdZINBI(c(0,1,2,3), mu=2, sigma=1, nu=0.1)
//' 
//' # Vector inputs with recycling
//' fdZINBI(0:5, mu=c(1,2), sigma=0.5, nu=0.1)
//'
//' @export
// [[Rcpp::export]]
NumericVector fdZINBI(const NumericVector& x,
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
    // NaN/NA x is handled in the compute loop below (mapped to NA); skip the
    // `< 0` check here because NaN comparisons are always false anyway.
    if (ISNAN(recycled.vec1[i])) continue;
    if (recycled.vec1[i] < 0) stop("x must be >=0");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0 || recycled.vec4[i] >= 1.0) stop("nu must be between 0 and 1");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // NaN/NA x -> NA: static_cast<int>(NaN) below is out-of-range float-to-int
    // UB, and a NaN x slips past the `< 0` check (NaN comparisons are false).
    if (ISNAN(recycled.vec1[i])) {
      out[i] = NA_REAL;
      continue;
    }
    out[i] = fdZINBI_scalar(static_cast<int>(recycled.vec1[i]), recycled.vec2[i],
                            recycled.vec3[i], recycled.vec4[i], log);
  }

  return out;
}


//' Zero-Inflated Negative Binomial Type I Distribution CDF
//'
//' Cumulative distribution function for the Zero-Inflated Negative Binomial type I (ZINBI)
//' distribution with parameters mu (mean), sigma (dispersion), and nu (zero-inflation probability).
//'
//' @param q vector of quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of zero-inflation probabilities (0 < nu < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The cumulative distribution function for the Zero-Inflated NBI distribution is:
//' \deqn{F(q) = \nu + (1-\nu) F_{NBI}(q|\mu,\sigma)}
//' where \eqn{F_{NBI}} is the NBI cumulative distribution function.
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
//' fpZINBI(c(0,1,2,3), mu=2, sigma=1, nu=0.1)
//' 
//' # Vector inputs with recycling
//' fpZINBI(0:5, mu=c(1,2), sigma=0.5, nu=0.1)
//'
//' @export
// [[Rcpp::export]]
NumericVector fpZINBI(const NumericVector& q,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu,
                      const bool& lower_tail = true,
                      const bool& log_p = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(q, mu, sigma, nu);
  const int n = recycled.n;
  
  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    // NaN/NA q is handled in the compute loop below (mapped to NA); skip the
    // `< 0` check here because NaN comparisons are always false anyway.
    if (ISNAN(recycled.vec1[i])) continue;
    if (recycled.vec1[i] < 0) stop("q must be >=0");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0 || recycled.vec4[i] >= 1.0) stop("nu must be between 0 and 1");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // NaN/NA q -> NA: static_cast<int>(NaN) below is out-of-range float-to-int
    // UB, and a NaN q slips past the `< 0` check (NaN comparisons are false).
    if (ISNAN(recycled.vec1[i])) {
      out[i] = NA_REAL;
      continue;
    }
    out[i] = fpZINBI_scalar(static_cast<int>(recycled.vec1[i]), recycled.vec2[i],
                            recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }

  return out;
}


//' Zero-Inflated Negative Binomial Type I Distribution Quantile Function
//'
//' Quantile function for the Zero-Inflated Negative Binomial type I (ZINBI)
//' distribution with parameters mu (mean), sigma (dispersion), and nu (zero-inflation probability).
//'
//' @param p vector of probabilities.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of zero-inflation probabilities (0 < nu < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The quantile function returns the smallest integer \eqn{x} such that
//' \eqn{F(x) \geq p}, where \eqn{F} is the ZINBI cumulative distribution function.
//'
//' @return A numeric vector of quantiles.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, 
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Single values
//' fqZINBI(c(0.1, 0.5, 0.9), mu=2, sigma=1, nu=0.1)
//' 
//' # Vector inputs with recycling
//' fqZINBI(c(0.25, 0.5, 0.75), mu=c(1,2), sigma=0.5, nu=0.1)
//'
//' @export
// [[Rcpp::export]]
IntegerVector fqZINBI(const NumericVector& p,
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
    // NaN/NA inputs are handled in the compute loop below (mapped to NA); skip
    // the range checks here because every NaN comparison is false anyway.
    if (ISNAN(recycled.vec1[i]) || ISNAN(recycled.vec2[i]) ||
        ISNAN(recycled.vec3[i]) || ISNAN(recycled.vec4[i])) continue;
    if (recycled.vec1[i] < 0.0 || recycled.vec1[i] > 1.0) stop("p must be >=0 and <=1");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0 || recycled.vec4[i] >= 1.0) stop("nu must be between 0 and 1");
  }

  IntegerVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // NaN/NA in any argument -> NA quantile. Without this, a NaN p slips past
    // the [0,1] range checks above (every NaN comparison is false) and reaches
    // fqZINBI_scalar -> fqNBI_scalar, which casts/searches on a NaN-derived
    // value (out-of-range float-to-int UB / wrong result).
    if (ISNAN(recycled.vec1[i]) || ISNAN(recycled.vec2[i]) ||
        ISNAN(recycled.vec3[i]) || ISNAN(recycled.vec4[i])) {
      out[i] = NA_INTEGER;
      continue;
    }
    out[i] = fqZINBI_scalar(recycled.vec1[i], recycled.vec2[i],
                            recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }

  return out;
}


//' Zero-Inflated Negative Binomial Type I Distribution Random Generation
//'
//' Random generation for the Zero-Inflated Negative Binomial type I (ZINBI)
//' distribution with parameters mu (mean), sigma (dispersion), and nu (zero-inflation probability).
//'
//' @param n number of observations.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of zero-inflation probabilities (0 < nu < 1).
//'
//' @details
//' Random variates are generated using a mixture approach: with probability \eqn{\nu}
//' the value is 0, and with probability \eqn{1-\nu} the value is drawn from the
//' standard NBI distribution.
//'
//' @return A numeric vector of random variates.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, 
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Generate random variates
//' frZINBI(10, mu=2, sigma=1, nu=0.1)
//' 
//' # Vector inputs with recycling
//' frZINBI(5, mu=c(1,2), sigma=0.5, nu=0.1)
//'
//' @export
// [[Rcpp::export]]
IntegerVector frZINBI(const int& n,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu)
{
  if (n <= 0) stop("n must be a positive integer");
  
  // Recycle mu, sigma, and nu to length n
  NumericVector n_vec(n, 1.0);  // Create a vector of 1s for recycling
  auto recycled = recycle_vectors(n_vec, mu, sigma, nu);
  
  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0 || recycled.vec4[i] >= 1.0) stop("nu must be between 0 and 1");
  }

  IntegerVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = frZINBI_scalar(recycled.vec2[i], recycled.vec3[i], recycled.vec4[i]);
  }

  return out;
}
