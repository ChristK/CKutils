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
#include "distr_BNB.h"
// [[Rcpp::plugins(cpp17)]]

// Enable vectorization hints for modern compilers
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif

using namespace Rcpp;


// SIMD-optimized Beta Negative Binomial density scalar function
double fdBNB_scalar(const int& x,
                      const double& mu = 1.0,
                      const double& sigma = 1.0,
                      const double& nu = 1.0,
                      const bool& log = false) {
    // Parameter validation (uncommented for performance)
    // if (mu    <= 0.0) stop("mu must be greater than 0");
    // if (sigma <= 0.0) stop("sigma must be greater than 0");
    // if (nu    <= 0.0) stop("nu must be greater than 0");
    // if (x      < 0.0) stop("x must be >=0");
    
    // Pre-compute commonly used values
    const double inv_sigma = 1.0 / sigma;
    const double inv_nu = 1.0 / nu;
    const double mu_nu_over_sigma = (mu * nu) * inv_sigma;
    
    const double m = inv_sigma + 1.0;
    const double n = mu_nu_over_sigma;
    const double k = inv_nu;
    
    // Use lgamma instead of lgammafn for better performance
    const double logL = R::lbeta(x + n, m + k) - R::lbeta(n, m) - 
                        R::lgammafn(x + 1) - R::lgammafn(k) + R::lgammafn(x + k);
    
    return log ? logL : std::exp(logL);
}


//' Beta Negative Binomial Distribution Density
//'
//' Probability density function for the Beta Negative Binomial (BNB) distribution
//' with parameters mu (mean), sigma (dispersion), and nu (shape).
//'
//' @param x vector of (non-negative integer) quantiles.
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
//' Distributions for modeling location, scale, and shape: Using GAMLSS in R, 
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
    if (recycled.vec1[i] < 0.0) stop("x must be >=0");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0) stop("nu must be greater than 0");
  }

 NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = fdBNB_scalar(static_cast<int>(recycled.vec1[i]), recycled.vec2[i], 
                          recycled.vec3[i], recycled.vec4[i], log);
  }

  return out;
}

// SIMD-optimized Beta Negative Binomial CDF scalar function
double fpBNB_scalar(const int& q,
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

    double cdf = 0.0;
    
    // Cache parameters for repeated use
    const double inv_sigma = 1.0 / sigma;
    const double inv_nu = 1.0 / nu;
    const double mu_nu_over_sigma = (mu * nu) * inv_sigma;
    const double m = inv_sigma + 1.0;
    const double n = mu_nu_over_sigma;
    const double k = inv_nu;
    
    // Pre-compute common terms
    const double log_beta_n_m = R::lbeta(n, m);
    const double log_gamma_k = R::lgammafn(k);
    
    for(int i = 0; i <= q; i++)
    {
      const double log_prob = R::lbeta(i + n, m + k) - log_beta_n_m - 
                             R::lgammafn(i + 1) - log_gamma_k + R::lgammafn(i + k);
      cdf += std::exp(log_prob);
    }
    
    if (!lower_tail) cdf = 1.0 - cdf;
    return log_p ? std::log(cdf) : cdf;
  }

//' Beta Negative Binomial Distribution Function
//'
//' Cumulative distribution function for the Beta Negative Binomial (BNB) distribution
//' with parameters mu (mean), sigma (dispersion), and nu (shape).
//'
//' @param q vector of (non-negative integer) quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of positive shape parameters.
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The cumulative distribution function is computed by summing the probability mass
//' function from 0 to q.
//'
//' @return A numeric vector of cumulative probabilities.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modeling location, scale, and shape: Using GAMLSS in R, 
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
    if (recycled.vec1[i] < 0) stop("q must be >=0");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0) stop("nu must be greater than 0");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = fpBNB_scalar(static_cast<int>(recycled.vec1[i]), recycled.vec2[i], 
                          recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }

    return out;
  }

// qBNB ----
// fast
double fqBNB_scalar(const double& p,
                    const double& mu = 1.0,
                    const double& sigma = 1.0,
                    const double& nu = 1.0,
                    const bool& lower_tail = true,
                    const bool& log_p = false)
{
  // if (mu    <= 0) stop("mu must be greater than 0");
  // if (sigma <= 0) stop("sigma must be greater than 0");
  // if (nu    <= 0) stop("nu must be greater than 0");
  // if (p < 0.0 || p > 1.0001) stop("p must be >=0 and <=1"); //I don't like this but it comes from original function

  double p_ = p;
  if (log_p) p_ = std::exp(p_);
  if (!lower_tail) p_ = 1.0 - p_;

  double QQQ = 0.0;
  double cumpro = 0.0;
  int j = 0;

  if (p_ + 1e-09 >= 1.0) {
    QQQ = R_PosInf;
  } else {
    // Optimized divide & conquer algorithm with adaptive step sizing
    
    // Better initial guess using quantile approximation
    j = std::max(1, static_cast<int>(mu * (2.0 * p_ + 0.3) + 0.5));
    cumpro = fpBNB_scalar(j, mu, sigma, nu, true, false);
    
    if (p_ <= cumpro) {
      // Search downward with exponential decay  
      int lower_bound = 0;
      int upper_bound = j;
      
      while (j > 0 && p_ <= cumpro) {
        j /= 2;
        cumpro = fpBNB_scalar(j, mu, sigma, nu, true, false);
      }
      
      // j is now the largest value where p_ > cumpro, or 0
      // But we need to check if j=0 satisfies p_ <= F(0)
      if (j == 0) {
        cumpro = fpBNB_scalar(0, mu, sigma, nu, true, false);
        if (p_ <= cumpro) {
          QQQ = 0;
        } else {
          lower_bound = 1;
          upper_bound = std::max(upper_bound, 1);
        }
      } else {
        lower_bound = j;
      }
      
      // Fine-tune search in narrow range [lower_bound, upper_bound] if not already found
      if (QQQ == 0 && lower_bound > 0) {
        for (int k = lower_bound; k <= upper_bound; k++) {
          cumpro = fpBNB_scalar(k, mu, sigma, nu, true, false);
          if (p_ <= cumpro) {
            QQQ = k;
            break;
          }
        }
      }
    } else {
      // Search upward with adaptive step sizing
      const int max_reasonable = std::min(INT_MAX, static_cast<int>(mu * 50 + 10000));
      
      // Exponential growth phase
      while (j < max_reasonable && p_ > cumpro) {
        j *= 2;
        if (j > max_reasonable) j = max_reasonable;
        cumpro = fpBNB_scalar(j, mu, sigma, nu, true, false);
      }
      
      // Binary search refinement for better precision
      int lower = j / 2;
      int upper = j;
      while (upper - lower > 5 && lower < max_reasonable) {  // Continue until gap <= 5
        const int mid = lower + (upper - lower) / 2;
        const double mid_cdf = fpBNB_scalar(mid, mu, sigma, nu, true, false);
        if (p_ <= mid_cdf) {
          upper = mid;
          cumpro = mid_cdf;
        } else {
          lower = mid;
        }
      }
      
      // Final linear search in small range for exact match
      for (int k = lower; k <= std::min(upper + 5, max_reasonable); k++) {
        cumpro = fpBNB_scalar(k, mu, sigma, nu, true, false);
        if (p_ <= cumpro) {
          QQQ = k;
          break;
        }
      }
    }
  }
  return QQQ;
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
//' The quantile function uses a fast divide-and-conquer algorithm to find
//' the quantiles efficiently.
//'
//' @return An integer vector of quantiles.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modeling location, scale, and shape: Using GAMLSS in R, 
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
    if (recycled.vec1[i] < 0.0 || recycled.vec1[i] > 1.0001) stop("p must be >=0 and <=1");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0) stop("nu must be greater than 0");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = fqBNB_scalar(recycled.vec1[i], recycled.vec2[i], 
                          recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }

  return out;
}
