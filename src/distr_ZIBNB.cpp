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
    if (recycled.vec1[i] < 0.0 || recycled.vec1[i] > 1.0001) stop("p must be >=0 and <=1");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0) stop("nu must be greater than 0");
    if (recycled.vec5[i] <= 0.0 || recycled.vec5[i] >= 1.0) stop("tau must be >0 and <1");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = fqZIBNB_scalar(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i], 
                            recycled.vec4[i], recycled.vec5[i], lower_tail, log_p);
  }

  return out;
}
