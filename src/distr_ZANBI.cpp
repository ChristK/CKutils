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
#include "distr_ZANBI.h"   // canonical header-only scalar definitions
// [[Rcpp::plugins(cpp17)]]

// Enable vectorization hints for modern compilers
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif

using namespace Rcpp;

// Zero-altered NBI functions
// ZANBI *_scalar definitions now live (inline) in inst/include/distr_ZANBI.h so
// that downstream LinkingTo: CKutils consumers can call them directly.


//' Zero-Altered Negative Binomial Type I Distribution Density
//'
//' Probability density function for the Zero-Altered Negative Binomial type I (ZANBI) 
//' distribution with parameters mu (mean), sigma (dispersion), and nu (zero-alteration probability).
//'
//' @param x vector of (non-negative integer) quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of zero-alteration probabilities (0 < nu < 1).
//' @param log logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The Zero-Altered NBI distribution is a modification of the standard NBI distribution
//' where the probability at zero is altered. The probability mass function is:
//' \deqn{P(Y = 0) = \nu}
//' \deqn{P(Y = y) = (1-\nu) \frac{f_{NBI}(y|\mu,\sigma)}{1 - f_{NBI}(0|\mu,\sigma)} \quad \text{for } y > 0}
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
//' fdZANBI(c(0,1,2,3), mu=2, sigma=1, nu=0.1)
//' 
//' # Vector inputs with recycling
//' fdZANBI(0:5, mu=c(1,2), sigma=0.5, nu=0.1)
//'
//' @export
// [[Rcpp::export]]
NumericVector fdZANBI(const NumericVector& x,
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
    out[i] = fdZANBI_scalar(static_cast<int>(recycled.vec1[i]), recycled.vec2[i],
                            recycled.vec3[i], recycled.vec4[i], log);
  }

  return out;
}


//' Zero-Altered Negative Binomial Type I Distribution CDF
//'
//' Cumulative distribution function for the Zero-Altered Negative Binomial type I (ZANBI)
//' distribution with parameters mu (mean), sigma (dispersion), and nu (zero-alteration probability).
//'
//' @param q vector of quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of zero-alteration probabilities (0 < nu < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The cumulative distribution function for the Zero-Altered NBI distribution is:
//' \deqn{F(0) = \nu}
//' \deqn{F(q) = \nu + (1-\nu) \frac{F_{NBI}(q|\mu,\sigma) - F_{NBI}(0|\mu,\sigma)}{1 - F_{NBI}(0|\mu,\sigma)} \quad \text{for } q > 0}
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
//' fpZANBI(c(0,1,2,3), mu=2, sigma=1, nu=0.1)
//' 
//' # Vector inputs with recycling
//' fpZANBI(0:5, mu=c(1,2), sigma=0.5, nu=0.1)
//'
//' @export
// [[Rcpp::export]]
NumericVector fpZANBI(const NumericVector& q,
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
    out[i] = fpZANBI_scalar(static_cast<int>(recycled.vec1[i]), recycled.vec2[i],
                            recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }

  return out;
}


//' Zero-Altered Negative Binomial Type I Distribution Quantile Function
//'
//' Quantile function for the Zero-Altered Negative Binomial type I (ZANBI)
//' distribution with parameters mu (mean), sigma (dispersion), and nu (zero-alteration probability).
//'
//' @param p vector of probabilities.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of zero-alteration probabilities (0 < nu < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The quantile function returns the smallest integer \eqn{x} such that
//' \eqn{F(x) \geq p}, where \eqn{F} is the ZANBI cumulative distribution function.
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
//' fqZANBI(c(0.1, 0.5, 0.9), mu=2, sigma=1, nu=0.1)
//' 
//' # Vector inputs with recycling
//' fqZANBI(c(0.25, 0.5, 0.75), mu=c(1,2), sigma=0.5, nu=0.1)
//'
//' @export
// [[Rcpp::export]]
IntegerVector fqZANBI(const NumericVector& p,
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
    if (recycled.vec1[i] < 0.0 || recycled.vec1[i] > 1.0) stop("p must be >=0 and <=1");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0 || recycled.vec4[i] >= 1.0) stop("nu must be between 0 and 1");
  }

  IntegerVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // NaN/NA in any argument -> NA quantile. A NaN p (or NaN parameter) slips
    // past the [0,1]/positivity range checks (every NaN comparison is false)
    // and would reach fqZANBI_scalar -> static_cast<int>(...) UB or a wrong,
    // non-NA search result. Base R returns NA for a NaN probability.
    if (ISNAN(recycled.vec1[i]) || ISNAN(recycled.vec2[i]) ||
        ISNAN(recycled.vec3[i]) || ISNAN(recycled.vec4[i])) {
      out[i] = NA_INTEGER;
      continue;
    }
    out[i] = fqZANBI_scalar(recycled.vec1[i], recycled.vec2[i],
                            recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }

  return out;
}


//' Zero-Altered Negative Binomial Type I Distribution Random Generation
//'
//' Random generation for the Zero-Altered Negative Binomial type I (ZANBI)
//' distribution with parameters mu (mean), sigma (dispersion), and nu (zero-alteration probability).
//'
//' @param n number of observations.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of zero-alteration probabilities (0 < nu < 1).
//'
//' @details
//' Random variates are generated using a rejection approach: with probability \eqn{\nu}
//' the value is 0, and with probability \eqn{1-\nu} the value is drawn from the
//' standard NBI distribution truncated at zero (i.e., excluding zero).
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
//' frZANBI(10, mu=2, sigma=1, nu=0.1)
//' 
//' # Vector inputs with recycling
//' frZANBI(5, mu=c(1,2), sigma=0.5, nu=0.1)
//'
//' @export
// [[Rcpp::export]]
IntegerVector frZANBI(const int& n,
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
    out[i] = frZANBI_scalar(recycled.vec2[i], recycled.vec3[i], recycled.vec4[i]);
  }

  return out;
}
