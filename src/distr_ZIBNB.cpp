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
#include "distr_ZIBNB.h"   // canonical header-only scalar definitions
// [[Rcpp::plugins(cpp17)]]

// Enable vectorization hints for modern compilers
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif

using namespace Rcpp;

// qZIBNB ----
// ZIBNB *_scalar definitions now live (inline) in inst/include/distr_ZIBNB.h so
// that downstream LinkingTo: CKutils consumers can call them directly.


//' Zero Inflated Beta Negative Binomial Density
//'
//' Probability mass function for the Zero Inflated Beta Negative Binomial
//' (ZIBNB) distribution with parameters mu (mean), sigma (dispersion),
//' nu (shape), and tau (zero-inflation probability).
//'
//' @param x vector of (non-negative integer) quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of positive shape parameters.
//' @param tau vector of zero-inflation probabilities (0 < tau < 1).
//' @param log logical; if TRUE, densities are returned as log(density).
//'
//' @details
//' Zero inflation adds a point mass at zero on top of the BNB distribution:
//' \deqn{P(Y = 0) = \tau + (1-\tau) f_{BNB}(0|\mu,\sigma,\nu)}
//' \deqn{P(Y = y) = (1-\tau) f_{BNB}(y|\mu,\sigma,\nu) \quad \text{for } y > 0}
//' where \eqn{f_{BNB}} is the BNB probability mass function. Note the zero
//' probability exceeds \eqn{\tau}, unlike the zero adjusted (hurdle) ZABNB
//' case where it is exactly \eqn{\tau}.
//'
//' @return A numeric vector of densities.
//'
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Single values
//' fdZIBNB(c(0,1,2,3), mu=2, sigma=1, nu=1, tau=0.1)
//'
//' # Vector inputs with recycling
//' fdZIBNB(0:5, mu=c(1,2), sigma=0.5, nu=c(1,1.5), tau=0.1)
//'
//' @export
// [[Rcpp::export]]
NumericVector fdZIBNB(const NumericVector& x,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu,
                      const NumericVector& tau,
                      const bool& log = false)
{
  auto recycled = recycle_vectors(x, mu, sigma, nu, tau);
  const int n = recycled.n;

  for (int i = 0; i < n; i++)
  {
    if (recycled.vec1[i] < 0) stop("x must be >=0");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0) stop("nu must be greater than 0");
    if (recycled.vec5[i] <= 0.0 || recycled.vec5[i] >= 1.0) stop("tau must be >0 and <1");
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
    out[i] = fdZIBNB_scalar(x_i, recycled.vec2[i], recycled.vec3[i],
                            recycled.vec4[i], recycled.vec5[i], log);
  }

  return out;
}


//' Zero Inflated Beta Negative Binomial Distribution Function
//'
//' Cumulative distribution function for the Zero Inflated Beta Negative
//' Binomial (ZIBNB) distribution with parameters mu (mean), sigma (dispersion),
//' nu (shape), and tau (zero-inflation probability).
//'
//' @param q vector of quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of positive shape parameters.
//' @param tau vector of zero-inflation probabilities (0 < tau < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' Zero inflation shifts the whole distribution function:
//' \deqn{F(q) = \tau + (1-\tau) F_{BNB}(q|\mu,\sigma,\nu)}
//' where \eqn{F_{BNB}} is the BNB cumulative distribution function. Unlike the
//' zero adjusted (hurdle) ZABNB case there is no renormalisation, so no special
//' case is needed at zero.
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
//' fpZIBNB(c(0,1,2,3), mu=2, sigma=1, nu=1, tau=0.1)
//'
//' # Vector inputs with recycling
//' fpZIBNB(0:5, mu=c(1,2), sigma=0.5, nu=c(1,1.5), tau=0.1)
//'
//' @export
// [[Rcpp::export]]
NumericVector fpZIBNB(const NumericVector& q,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu,
                      const NumericVector& tau,
                      const bool& lower_tail = true,
                      const bool& log_p = false)
{
  auto recycled = recycle_vectors(q, mu, sigma, nu, tau);
  const int n = recycled.n;

  for (int i = 0; i < n; i++)
  {
    if (recycled.vec1[i] < 0) stop("q must be >=0");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0) stop("nu must be greater than 0");
    if (recycled.vec5[i] <= 0.0 || recycled.vec5[i] >= 1.0) stop("tau must be >0 and <1");
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
    out[i] = fpZIBNB_scalar(q_i, recycled.vec2[i], recycled.vec3[i],
                            recycled.vec4[i], recycled.vec5[i], lower_tail, log_p);
  }

  return out;
}


//' Zero Inflated Beta Negative Binomial Quantile Function
//'
//' Quantile function for the Zero Inflated Beta Negative Binomial (ZIBNB) distribution
//' with parameters mu (mean), sigma (dispersion), nu (shape), and tau (zero inflation).
//'
//' @param p vector of probabilities.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of positive shape parameters.
//' @param tau vector of zero inflation probabilities (0 < tau < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The zero inflated beta negative binomial distribution allows for excess zeros
//' beyond what the BNB distribution would predict.
//'
//' @return An integer vector of quantiles.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, 
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Single values
//' fqZIBNB(c(0.1, 0.5, 0.9), mu=2, sigma=1, nu=1, tau=0.1)
//' 
//' # Vector inputs with recycling
//' fqZIBNB(c(0.25, 0.75), mu=c(1,2), sigma=0.5, nu=c(1,1.5), tau=0.1)
//'
//' @export
// [[Rcpp::export]]
NumericVector fqZIBNB(const NumericVector& p,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu,
                      const NumericVector& tau,
                      const bool& lower_tail = true,
                      const bool& log_p = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(p, mu, sigma, nu, tau);
  const int n = recycled.n;
  
  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    check_prob(recycled.vec1[i], log_p, 1.0001, "p must be >=0 and <=1");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0) stop("nu must be greater than 0");
    if (recycled.vec5[i] <= 0.0 || recycled.vec5[i] >= 1.0) stop("tau must be >0 and <1");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // NaN/NA in any argument -> NA quantile. A NaN p (or NaN parameter) slips
    // past the [0,1]/positivity checks above (every NaN comparison is false) and
    // would otherwise reach the search inside fqZIBNB_scalar -> fqBNB_scalar,
    // returning a wrong, non-NA value instead of NA.
    if (ISNAN(recycled.vec1[i]) || ISNAN(recycled.vec2[i]) || ISNAN(recycled.vec3[i]) ||
        ISNAN(recycled.vec4[i]) || ISNAN(recycled.vec5[i])) {
      out[i] = NA_REAL;
      continue;
    }
    out[i] = fqZIBNB_scalar(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i],
                            recycled.vec4[i], recycled.vec5[i], lower_tail, log_p);
  }

  return out;
}
