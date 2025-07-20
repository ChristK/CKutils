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

// qZABNB ----
double fqZABNB_scalar(const double& p,
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
  // if (p < 0.0 || p > 1.0) stop("p must be >=0 and <=1"); //I don't like this but it comes from original function


  double p_ = p;
  if (log_p) p_ = exp(p_);
  if (!lower_tail) p_ = 1.0 - p_;

  p_ = (p_ - tau)/(1.0 - tau) - (1e-010);
  double cdf0 = fpBNB_scalar(0, mu, sigma, nu, true, false);
  p_ = cdf0 * (1.0 - p_) + p_;
  if (p_ < 0.0) p_ = 0.0;

  return fqBNB_scalar(p_, mu, sigma, nu, true, false);
}


//' Zero Adjusted Beta Negative Binomial Quantile Function
//'
//' Quantile function for the Zero Adjusted (Hurdle) Beta Negative Binomial (ZABNB) distribution
//' with parameters mu (mean), sigma (dispersion), nu (shape), and tau (hurdle probability).
//'
//' @param p vector of probabilities.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of positive shape parameters.
//' @param tau vector of hurdle probabilities (0 < tau < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The zero adjusted (hurdle) beta negative binomial distribution has two parts:
//' a point mass at zero and a truncated BNB distribution for positive values.
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
//' fqZABNB(c(0.1, 0.5, 0.9), mu=2, sigma=1, nu=1, tau=0.1)
//' 
//' # Vector inputs with recycling
//' fqZABNB(c(0.25, 0.75), mu=c(1,2), sigma=0.5, nu=c(1,1.5), tau=0.1)
//'
//' @export
// [[Rcpp::export]]
NumericVector fqZABNB(const NumericVector& p,
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
    if (recycled.vec1[i] < 0.0 || recycled.vec1[i] > 1.0) stop("p must be >=0 and <=1");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0) stop("nu must be greater than 0");
    if (recycled.vec5[i] <= 0.0 || recycled.vec5[i] >= 1.0) stop("tau must be >0 and <1");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = fqZABNB_scalar(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i], 
                            recycled.vec4[i], recycled.vec5[i], lower_tail, log_p);
  }

  return out;
}
