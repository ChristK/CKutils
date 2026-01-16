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
// [[Rcpp::plugins(cpp17)]]

// Enable vectorization hints for modern compilers
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif

using namespace Rcpp;


// SIMD-optimised NBI density scalar function
double fdNBI_scalar(const int& x,
                    const double& mu = 1.0,
                    const double& sigma = 1.0,
                    const bool& log_p = false) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (x      < 0.0) stop("x must be >=0");
    
    // For very small sigma values, use Poisson approximation
    if (sigma < 1e-4) {
        return R::dpois(x, mu, log_p);
    }
    
    // Standard NBI calculation using negative binomial
    const double size = 1.0 / sigma;
    return R::dnbinom_mu(x, size, mu, log_p);
}


//' Negative Binomial Type I Distribution Density
//'
//' Probability density function for the Negative Binomial type I (NBI) distribution
//' with parameters mu (mean) and sigma (dispersion).
//'
//' @param x vector of (non-negative integer) quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The probability mass function of the NBI distribution is:
//' \deqn{f(y|\mu,\sigma) = \frac{\Gamma(y+1/\sigma)}{\Gamma(y+1)\Gamma(1/\sigma)} \left(\frac{1}{1+\mu\sigma}\right)^{1/\sigma} \left(\frac{\mu\sigma}{1+\mu\sigma}\right)^y}
//' for \eqn{y = 0, 1, 2, \ldots}, \eqn{\mu > 0}, and \eqn{\sigma > 0}.
//'
//' For \eqn{\sigma < 0.0001}, the distribution reduces to the Poisson distribution.
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
//' fdNBI(c(0,1,2,3), mu=2, sigma=1)
//' 
//' # Vector inputs with recycling
//' fdNBI(0:5, mu=c(1,2), sigma=0.5)
//'
//' @export
// [[Rcpp::export]]
NumericVector fdNBI(const NumericVector& x,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const bool& log_p = false)
  {
  // Recycle vectors to common length
  auto recycled = recycle_vectors(x, mu, sigma);
  const int n = recycled.n;
  
  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    if (recycled.vec1[i] < 0.0) stop("x must be >=0");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = fdNBI_scalar(static_cast<int>(recycled.vec1[i]), recycled.vec2[i], 
                          recycled.vec3[i], log_p);
  }

  return out;
}

// SIMD-optimised NBI CDF scalar function
double fpNBI_scalar(const int& q,
                    const double& mu = 1.0,
                    const double& sigma = 1.0,
                    const bool& lower_tail = true,
                    const bool& log_p = false) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (q      < 0) stop("q must be >=0");
    
    // For very small sigma values, use Poisson approximation
    if (sigma < 1e-4) {
        return R::ppois(q, mu, lower_tail, log_p);
    }
    
    // Standard NBI calculation using negative binomial
    const double size = 1.0 / sigma;
    return R::pnbinom_mu(q, size, mu, lower_tail, log_p);
}


//' Negative Binomial Type I Distribution Cumulative Distribution Function
//'
//' Cumulative distribution function for the Negative Binomial type I (NBI) distribution
//' with parameters mu (mean) and sigma (dispersion).
//'
//' @param q vector of quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The cumulative distribution function of the NBI distribution is:
//' \deqn{F(y|\mu,\sigma) = \sum_{i=0}^{y} f(i|\mu,\sigma)}
//' where \eqn{f(i|\mu,\sigma)} is the probability mass function.
//'
//' For \eqn{\sigma < 0.0001}, the distribution reduces to the Poisson distribution.
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
//' fpNBI(c(0,1,2,3), mu=2, sigma=1)
//' 
//' # Vector inputs with recycling
//' fpNBI(0:5, mu=c(1,2), sigma=0.5)
//'
//' @export
// [[Rcpp::export]]
NumericVector fpNBI(const NumericVector& q,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const bool& lower_tail = true,
                    const bool& log_p = false)
  {
  // Recycle vectors to common length
  auto recycled = recycle_vectors(q, mu, sigma);
  const int n = recycled.n;
  
  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    if (recycled.vec1[i] < 0) stop("q must be >=0");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = fpNBI_scalar(static_cast<int>(recycled.vec1[i]), recycled.vec2[i], 
                          recycled.vec3[i], lower_tail, log_p);
  }

  return out;
}

// SIMD-optimised NBI quantile scalar function
int fqNBI_scalar(const double& p,
                 const double& mu = 1.0,
                 const double& sigma = 1.0,
                 const bool& lower_tail = true,
                 const bool& log_p = false) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0) stop("mu must be greater than 0");
    // if (sigma <= 0) stop("sigma must be greater than 0");
    // if (p < 0.0 || p > 1.0) stop("p must be >=0 and <=1");
    
    // For very small sigma values, use Poisson approximation
    if (sigma < 1e-4) {
        return R::qpois(p, mu, lower_tail, log_p);
    }
    
    // Standard NBI calculation using negative binomial
    const double size = 1.0 / sigma;
    return R::qnbinom_mu(p, size, mu, lower_tail, log_p);
}


//' Negative Binomial Type I Distribution Quantile Function
//'
//' Quantile function for the Negative Binomial type I (NBI) distribution
//' with parameters mu (mean) and sigma (dispersion).
//'
//' @param p vector of probabilities.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The quantile function returns the smallest integer \eqn{x} such that
//' \eqn{F(x) \geq p}, where \eqn{F} is the cumulative distribution function.
//'
//' For \eqn{\sigma < 0.0001}, the distribution reduces to the Poisson distribution.
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
//' fqNBI(c(0.1, 0.5, 0.9), mu=2, sigma=1)
//' 
//' # Vector inputs with recycling
//' fqNBI(c(0.25, 0.5, 0.75), mu=c(1,2), sigma=0.5)
//'
//' @export
// [[Rcpp::export]]
IntegerVector fqNBI(const NumericVector& p,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const bool& lower_tail = true,
                    const bool& log_p = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(p, mu, sigma);
  const int n = recycled.n;
  
  // Validate parameters after recycling
  if (log_p) {
    for (int i = 0; i < n; i++)
    {
      if (recycled.vec2[i] <= 0.0)
        stop("mu must be greater than 0");
      if (recycled.vec3[i] <= 0.0)
        stop("sigma must be greater than 0");
    }
  } else {
  for (int i = 0; i < n; i++)
  {
    if (recycled.vec1[i] < 0.0 || recycled.vec1[i] > 1.0)
      stop("p must be >=0 and <=1");
    if (recycled.vec2[i] <= 0.0)
      stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0)
      stop("sigma must be greater than 0");
  }
}

  IntegerVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = fqNBI_scalar(recycled.vec1[i], recycled.vec2[i], 
                          recycled.vec3[i], lower_tail, log_p);
  }

  return out;
}

// SIMD-optimised NBI random generation scalar function
int frNBI_scalar(const double& mu = 1.0,
                 const double& sigma = 1.0) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0) stop("mu must be greater than 0");
    // if (sigma <= 0) stop("sigma must be greater than 0");
    
    // For very small sigma values, use Poisson approximation
    if (sigma < 1e-4) {
        return R::rpois(mu);
    }
    
    // Standard NBI generation using negative binomial
    const double size = 1.0 / sigma;
    return R::rnbinom(size, size / (size + mu));
}


//' Negative Binomial Type I Distribution Random Generation
//'
//' Random generation for the Negative Binomial type I (NBI) distribution
//' with parameters mu (mean) and sigma (dispersion).
//'
//' @param n number of observations.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//'
//' @details
//' Random variates are generated using the negative binomial distribution
//' with size parameter \eqn{1/\sigma} and mean parameter \eqn{\mu}.
//'
//' For \eqn{\sigma < 0.0001}, the distribution reduces to the Poisson distribution.
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
//' frNBI(10, mu=2, sigma=1)
//' 
//' # Vector inputs with recycling
//' frNBI(5, mu=c(1,2), sigma=0.5)
//'
//' @export
// [[Rcpp::export]]
IntegerVector frNBI(const int& n,
                    const NumericVector& mu,
                    const NumericVector& sigma)
{
  if (n <= 0) stop("n must be a positive integer");
  
  // Recycle mu and sigma to length n
  NumericVector n_vec(n, 1.0);  // Create a vector of 1s for recycling
  auto recycled = recycle_vectors(n_vec, mu, sigma);
  
  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
  }

  IntegerVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = frNBI_scalar(recycled.vec2[i], recycled.vec3[i]);
  }

  return out;
}
