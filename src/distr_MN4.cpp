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
#include <cmath>
#include <algorithm>
#include "recycling_helpers.h"
// [[Rcpp::plugins(cpp17)]]

// Enable vectorization hints for modern compilers
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif

using namespace Rcpp;

// Scalar helper for MN4 density
inline double fdMN4_scalar(const int& x,
                           const double& mu,
                           const double& sigma,
                           const double& nu,
                           const bool& log_) {
  if (mu <= 0.0) return R_NaN;
  if (sigma <= 0.0) return R_NaN;
  if (nu <= 0.0) return R_NaN;
  if (x < 1 || x > 4) return log_ ? R_NegInf : 0.0;
  
  double logfy = R_NegInf;
  const double normaliser = 1.0 + mu + sigma + nu;
  
  if (x == 1) logfy = std::log(mu);
  else if (x == 2) logfy = std::log(sigma);
  else if (x == 3) logfy = std::log(nu);
  else if (x == 4) logfy = 0.0;  // log(1) = 0
  
  logfy -= std::log(normaliser);
  
  return log_ ? logfy : std::exp(logfy);
}

// Scalar helper for MN4 distribution function
inline double fpMN4_scalar(const int& q,
                           const double& mu,
                           const double& sigma,
                           const double& nu,
                           const bool& lower_tail,
                           const bool& log_p) {
  if (mu <= 0.0) return R_NaN;
  if (sigma <= 0.0) return R_NaN;
  if (nu <= 0.0) return R_NaN;
  if (q < 1) return lower_tail ? (log_p ? R_NegInf : 0.0) : (log_p ? 0.0 : 1.0);
  if (q >= 4) return lower_tail ? (log_p ? 0.0 : 1.0) : (log_p ? R_NegInf : 0.0);
  
  const double normaliser = 1.0 + mu + sigma + nu;
  double cdf = 0.0;
  
  if (q >= 1) cdf = mu / normaliser;
  if (q >= 2) cdf = (mu + sigma) / normaliser;
  if (q >= 3) cdf = (mu + sigma + nu) / normaliser;
  if (q >= 4) cdf = 1.0;
  
  if (!lower_tail) cdf = 1.0 - cdf;
  
  return log_p ? std::log(cdf) : cdf;
}

// Scalar helper for MN4 quantile function
inline int fqMN4_scalar(const double& p,
                        const double& mu,
                        const double& sigma,
                        const double& nu,
                        const bool& lower_tail,
                        const bool& log_p) {
  if (mu <= 0.0) return NA_INTEGER;
  if (sigma <= 0.0) return NA_INTEGER;
  if (nu <= 0.0) return NA_INTEGER;
  
  double p_ = p;
  if (log_p) p_ = std::exp(p);
  if (!lower_tail) p_ = 1.0 - p_;
  
  if (p_ < 0.0 || p_ > 1.0) return NA_INTEGER;
  
  const double normaliser = 1.0 + mu + sigma + nu;
  int q = 1;
  
  if (p_ >= (mu / normaliser)) q = 2;
  if (p_ >= ((mu + sigma) / normaliser)) q = 3;
  if (p_ >= ((mu + sigma + nu) / normaliser)) q = 4;
  
  return q;
}

//' Multinomial Distribution with 4 Categories - Density Function
//'
//' Density function for the multinomial distribution with 4 categories,
//' optimised for performance with SIMD vectorisation and parameter recycling.
//'
//' @param x vector of (integer) quantiles. Must be 1, 2, 3, or 4.
//' @param mu vector of (positive) parameters for category 1.
//' @param sigma vector of (positive) parameters for category 2.
//' @param nu vector of (positive) parameters for category 3.
//' @param log_ logical; if TRUE, densities are returned on the log scale.
//'
//' @details
//' The multinomial distribution with 4 categories (MN4) is a discrete distribution
//' defined on the integers 1, 2, 3, 4. The probability mass function is:
//' \deqn{P(X = k) = \frac{\theta_k}{1 + \mu + \sigma + \nu}}
//' where \eqn{\theta_1 = \mu}, \eqn{\theta_2 = \sigma}, \eqn{\theta_3 = \nu},
//' and \eqn{\theta_4 = 1}.
//'
//' Parameters are recycled to the length of the longest vector following R's
//' standard recycling rules.
//'
//' @return A numeric vector of densities.
//'
//' @references
//' Rigby, R. A. and Stasinopoulos, D. M. (2005). Generalized additive models
//' for location, scale and shape. Applied Statistics, 54, 507-554.
//'
//' @note
//' This implementation is based on the gamlss.dist package dMN4 function
//' but optimised for performance with SIMD vectorisation and parameter recycling.
//'
//' @examples
//' # Basic usage
//' x <- c(1, 2, 3, 4)
//' mu <- c(1, 1, 1, 1)
//' sigma <- c(1, 1, 1, 1)
//' nu <- c(1, 1, 1, 1)
//' 
//' # Calculate densities
//' fdMN4(x, mu, sigma, nu)
//' 
//' # Log densities
//' fdMN4(x, mu, sigma, nu, log_ = TRUE)
//' 
//' # Parameter recycling
//' fdMN4(c(1, 2, 3, 4), mu = 2, sigma = 1, nu = 0.5)
//'
//' @seealso \code{\link{fpMN4}}, \code{\link{fqMN4}}
//' @export
// [[Rcpp::export]]
NumericVector fdMN4(const IntegerVector& x,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const NumericVector& nu,
                    const bool& log_ = false) {
  
  // Use existing recycling infrastructure
  auto recycled = recycle_vectors(x, mu, sigma, nu);
  const int n = recycled.n;
  
  // Convert recycled IntegerVector to NumericVector for x
  NumericVector x_num = recycled.vec1;
  IntegerVector x_int(n);
  for (int i = 0; i < n; i++) {
    x_int[i] = static_cast<int>(x_num[i]);
  }
  
  NumericVector out(n);
  
  // SIMD-optimised main computation loop
  SIMD_HINT
  for (int i = 0; i < n; i++) {
    out[i] = fdMN4_scalar(x_int[i], recycled.vec2[i], recycled.vec3[i], recycled.vec4[i], log_);
  }
  
  if (any(is_na(out))) warning("NaNs were produced");
  return out;
}

//' Multinomial Distribution with 4 Categories - Distribution Function
//'
//' Distribution function for the multinomial distribution with 4 categories,
//' optimised for performance with SIMD vectorisation and parameter recycling.
//'
//' @param q vector of (integer) quantiles. Must be 1, 2, 3, or 4.
//' @param mu vector of (positive) parameters for category 1.
//' @param sigma vector of (positive) parameters for category 2.
//' @param nu vector of (positive) parameters for category 3.
//' @param lower_tail logical; if TRUE (default), probabilities are P(X <= q),
//'        otherwise P(X > q).
//' @param log_p logical; if TRUE, probabilities are returned on the log scale.
//'
//' @details
//' The cumulative distribution function of the multinomial distribution with
//' 4 categories is:
//' \deqn{P(X \leq k) = \frac{\sum_{i=1}^{k} \theta_i}{1 + \mu + \sigma + \nu}}
//' where \eqn{\theta_1 = \mu}, \eqn{\theta_2 = \sigma}, \eqn{\theta_3 = \nu},
//' and \eqn{\theta_4 = 1}.
//'
//' Parameters are recycled to the length of the longest vector following R's
//' standard recycling rules.
//'
//' @return A numeric vector of probabilities.
//'
//' @references
//' Rigby, R. A. and Stasinopoulos, D. M. (2005). Generalized additive models
//' for location, scale and shape. Applied Statistics, 54, 507-554.
//'
//' @note
//' This implementation is based on the gamlss.dist package pMN4 function
//' but optimised for performance with SIMD vectorisation and parameter recycling.
//'
//' @examples
//' # Basic usage
//' q <- c(1, 2, 3, 4)
//' mu <- c(1, 1, 1, 1)
//' sigma <- c(1, 1, 1, 1)
//' nu <- c(1, 1, 1, 1)
//' 
//' # Calculate probabilities
//' fpMN4(q, mu, sigma, nu)
//' 
//' # Upper tail probabilities
//' fpMN4(q, mu, sigma, nu, lower_tail = FALSE)
//' 
//' # Log probabilities
//' fpMN4(q, mu, sigma, nu, log_p = TRUE)
//' 
//' # Parameter recycling
//' fpMN4(c(1, 2, 3, 4), mu = 2, sigma = 1, nu = 0.5)
//'
//' @seealso \code{\link{fdMN4}}, \code{\link{fqMN4}}
//' @export
// [[Rcpp::export]]
NumericVector fpMN4(const IntegerVector& q,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const NumericVector& nu,
                    const bool& lower_tail = true,
                    const bool& log_p = false) {
  
  // Use existing recycling infrastructure
  auto recycled = recycle_vectors(q, mu, sigma, nu);
  const int n = recycled.n;
  
  // Convert recycled IntegerVector to NumericVector for q
  NumericVector q_num = recycled.vec1;
  IntegerVector q_int(n);
  for (int i = 0; i < n; i++) {
    q_int[i] = static_cast<int>(q_num[i]);
  }
  
  NumericVector out(n);
  
  // SIMD-optimised main computation loop
  SIMD_HINT
  for (int i = 0; i < n; i++) {
    out[i] = fpMN4_scalar(q_int[i], recycled.vec2[i], recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }
  
  if (any(is_na(out))) warning("NaNs were produced");
  return out;
}

//' Multinomial Distribution with 4 Categories - Quantile Function
//'
//' Quantile function for the multinomial distribution with 4 categories,
//' optimised for performance with SIMD vectorisation and parameter recycling.
//'
//' @param p vector of probabilities (must be between 0 and 1).
//' @param mu vector of (positive) parameters for category 1.
//' @param sigma vector of (positive) parameters for category 2.
//' @param nu vector of (positive) parameters for category 3.
//' @param lower_tail logical; if TRUE (default), probabilities are P(X <= q),
//'        otherwise P(X > q).
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The quantile function returns the smallest integer k such that
//' \deqn{P(X \leq k) \geq p}
//' The quantiles are computed by comparing p with the cumulative probabilities
//' of the multinomial distribution.
//'
//' Parameters are recycled to the length of the longest vector following R's
//' standard recycling rules.
//'
//' @return An integer vector of quantiles.
//'
//' @references
//' Rigby, R. A. and Stasinopoulos, D. M. (2005). Generalized additive models
//' for location, scale and shape. Applied Statistics, 54, 507-554.
//'
//' @note
//' This implementation is based on the gamlss.dist package qMN4 function
//' but optimised for performance with SIMD vectorisation and parameter recycling.
//'
//' @examples
//' # Basic usage
//' p <- c(0.1, 0.3, 0.6, 0.9)
//' mu <- c(1, 1, 1, 1)
//' sigma <- c(1, 1, 1, 1)
//' nu <- c(1, 1, 1, 1)
//' 
//' # Calculate quantiles
//' fqMN4(p, mu, sigma, nu)
//' 
//' # Upper tail quantiles
//' fqMN4(p, mu, sigma, nu, lower_tail = FALSE)
//' 
//' # Log probabilities
//' fqMN4(log(p), mu, sigma, nu, log_p = TRUE)
//' 
//' # Parameter recycling
//' fqMN4(c(0.1, 0.3, 0.6, 0.9), mu = 2, sigma = 1, nu = 0.5)
//'
//' @seealso \code{\link{fdMN4}}, \code{\link{fpMN4}}
//' @export
// [[Rcpp::export]]
IntegerVector fqMN4(const NumericVector& p,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const NumericVector& nu,
                    const bool& lower_tail = true,
                    const bool& log_p = false) {
  
  // Use existing recycling infrastructure
  auto recycled = recycle_vectors(p, mu, sigma, nu);
  const int n = recycled.n;
  
  IntegerVector out(n);
  
  // SIMD-optimised main computation loop
  SIMD_HINT
  for (int i = 0; i < n; i++) {
    out[i] = fqMN4_scalar(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }
  
  if (any(is_na(out))) warning("NAs were produced");
  return out;
}

//' Multinomial Distribution with 4 Categories - Random Generation
//'
//' Random generation for the multinomial distribution with 4 categories,
//' optimised for performance with SIMD vectorisation and parameter recycling.
//'
//' @param n number of random values to generate.
//' @param mu vector of (positive) parameters for category 1.
//' @param sigma vector of (positive) parameters for category 2.
//' @param nu vector of (positive) parameters for category 3.
//'
//' @details
//' Random values are generated using the quantile function method:
//' generate uniform random variables and apply the quantile function.
//'
//' Parameters are recycled to the length n following R's standard recycling rules.
//'
//' @return An integer vector of random values.
//'
//' @references
//' Rigby, R. A. and Stasinopoulos, D. M. (2005). Generalized additive models
//' for location, scale and shape. Applied Statistics, 54, 507-554.
//'
//' @note
//' This implementation is based on the gamlss.dist package rMN4 function
//' but optimised for performance with SIMD vectorisation and parameter recycling.
//'
//' @examples
//' # Basic usage
//' frMN4(10, mu = 1, sigma = 1, nu = 1)
//' 
//' # With different parameters
//' frMN4(10, mu = 2, sigma = 1, nu = 0.5)
//' 
//' # Parameter recycling
//' frMN4(10, mu = c(1, 2), sigma = c(1, 0.5), nu = c(1, 2))
//'
//' @seealso \code{\link{fdMN4}}, \code{\link{fpMN4}}, \code{\link{fqMN4}}
//' @export
// [[Rcpp::export]]
IntegerVector frMN4(const int& n,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const NumericVector& nu) {
  
  if (n <= 0) stop("n must be a positive integer");
  
  // Use existing recycling infrastructure for parameters
  auto recycled = recycle_vectors(mu, sigma, nu);
  const int param_len = recycled.n;
  
  // Generate uniform random numbers
  NumericVector u = runif(n);
  
  IntegerVector out(n);
  
  // SIMD-optimised main computation loop
  SIMD_HINT
  for (int i = 0; i < n; i++) {
    int param_idx = i % param_len;  // Cycle through parameter values
    out[i] = fqMN4_scalar(u[i], recycled.vec1[param_idx], recycled.vec2[param_idx], recycled.vec3[param_idx], true, false);
  }
  
  if (any(is_na(out))) warning("NAs were produced");
  return out;
}

