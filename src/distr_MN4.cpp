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
#include "distr_MN4.h"   // canonical header-only scalar definitions
// [[Rcpp::plugins(cpp17)]]

// Enable vectorization hints for modern compilers
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif

using namespace Rcpp;

// MN4 *_scalar definitions now live (inline) in inst/include/distr_MN4.h


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
      // NA/NaN or a count too large to convert to int -> NA. See count_to_int()
      // in recycling_helpers.h: the unguarded cast is out-of-range float-to-int
      // undefined behaviour and it is not benign on either x86-64 or AArch64.
    // Mark an unusable value with NA_INTEGER so the main loop skips it.
    if (!count_to_int(x_num[i], x_int[i])) x_int[i] = NA_INTEGER;
  }

  NumericVector out(n);

  // SIMD-optimised main computation loop
  SIMD_HINT
  for (int i = 0; i < n; i++) {
    // NaN/NA x propagated from the conversion loop -> NA density.
    if (x_int[i] == NA_INTEGER) {
      out[i] = NA_REAL;
      continue;
    }
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
//' \code{lower_tail = FALSE} is computed as 1 - F, so a tiny upper-tail
//' probability has no relative accuracy (it is 0 once F rounds to 1).
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
      // NA/NaN or a count too large to convert to int -> NA. See count_to_int()
      // in recycling_helpers.h: the unguarded cast is out-of-range float-to-int
      // undefined behaviour and it is not benign on either x86-64 or AArch64.
    // Mark an unusable value with NA_INTEGER so the main loop skips it.
    if (!count_to_int(q_num[i], q_int[i])) q_int[i] = NA_INTEGER;
  }

  NumericVector out(n);

  // SIMD-optimised main computation loop
  SIMD_HINT
  for (int i = 0; i < n; i++) {
    // NaN/NA q propagated from the conversion loop -> NA probability.
    if (q_int[i] == NA_INTEGER) {
      out[i] = NA_REAL;
      continue;
    }
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
//' A \code{p} exactly equal to a cumulative probability gives the next category
//' (\code{fqMN4(0.25, 1, 1, 1)} is 2), so \code{fqMN4(fpMN4(k, ...), ...)} is
//' \code{k + 1}, not \code{k}, for \code{k = 1, 2, 3}; \code{p = 1} gives 4.
//' \code{lower_tail = FALSE} replaces \code{p} by 1 - \code{p}.
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
    // NaN/NA in p or any parameter -> NA quantile. Without this a NaN p slips
    // past the [0,1] range check in fqMN4_scalar (NaN comparisons are false).
    if (ISNAN(recycled.vec1[i]) || ISNAN(recycled.vec2[i]) ||
        ISNAN(recycled.vec3[i]) || ISNAN(recycled.vec4[i])) {
      out[i] = NA_INTEGER;
      continue;
    }
    out[i] = fqMN4_scalar(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }

  if (any(is_na(out))) warning("NAs were produced");
  return out;
}

// NOTE: there is deliberately no vectorised, Rcpp-exported frMN4() here.
// The exported frMN4() is the R implementation in R/rng_distr.R, which draws
// from dqrng::dqrunif() and inverts fqMN4(), like the other fr* functions.
// A C++ wrapper of the same name used to be exported from this file. It drew
// from R's own stream (Rcpp::runif), so it followed set.seed() and ignored
// dqrng::dqset.seed(): two calls under one dqset.seed() were not reproducible.
// It must not come back, for the reason given in the note in distr_NBI.cpp:
// R/ is collated alphabetically, so R/rng_distr.R is sourced after
// R/RcppExports.R and would silently overwrite a compiled frMN4(), leaving
// dead code and a man page with two identical \usage entries. There is no
// frMN4_scalar() in inst/include/distr_MN4.h; a LinkingTo: CKutils consumer
// that wants one draw takes fqMN4_scalar(u, mu, sigma, nu, true, false) at a
// uniform u of its own, which is what frMN4() does at dqrng::dqrunif().
