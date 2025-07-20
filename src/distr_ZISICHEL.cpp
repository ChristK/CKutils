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
#include <algorithm>
#include "recycling_helpers.h"
#include "distr_SICHEL.h"
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

//' Zero-Inflated Sichel Distribution Quantile Function
//'
//' Quantile function for the zero-inflated Sichel distribution with parameters 
//' mu (mean), sigma (dispersion), nu (shape), and tau (zero-inflation).
//'
//' @param p vector of probabilities.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of shape parameters (real values).
//' @param tau vector of zero-inflation parameters (0 < tau < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The zero-inflated Sichel distribution is a mixture of a point mass at zero 
//' and a (truncated at zero) Sichel distribution.
//'
//' @return A numeric vector of quantiles.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modeling location, scale, and shape: Using GAMLSS in R, 
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Vector inputs with recycling
//' fqZISICHEL(c(0.25, 0.75), mu=c(1,2), sigma=1, nu=-0.5, tau=0.1)
//'
//' @export
// [[Rcpp::export]]
IntegerVector fqZISICHEL(NumericVector p,
                         const NumericVector& mu,
                         const NumericVector& sigma,
                         const NumericVector& nu,
                         const NumericVector& tau,
                         const bool& lower_tail = true,
                         const bool& log_p = false) {
    // Recycle vectors to common length
    auto recycled = recycle_vectors(p, mu, sigma, nu, tau);
    const int n = recycled.n;
    
    // Validate parameters after recycling
    if (log_p) {
      for (int i = 0; i < n; i++)
      {
        if (recycled.vec2[i] <= 0.0)
          stop("mu must be greater than 0");
        if (recycled.vec3[i] <= 0.0)
          stop("sigma must be greater than 0");
        if (recycled.vec5[i] <= 0.0 || recycled.vec5[i] >= 1.0)
          stop("tau must be between 0 and 1");
      }
    } else {
      for (int i = 0; i < n; i++)
      {
        if (recycled.vec1[i] < 0.0 || recycled.vec1[i] > 1.0001)
          stop("p must be between 0 and 1");
        if (recycled.vec2[i] <= 0.0)
          stop("mu must be greater than 0");
        if (recycled.vec3[i] <= 0.0)
          stop("sigma must be greater than 0");
        if (recycled.vec5[i] <= 0.0 || recycled.vec5[i] >= 1.0)
          stop("tau must be between 0 and 1");
      }
    }

    
    // Transform probabilities
    NumericVector p_transformed = clone(recycled.vec1);
    if (log_p) {
        for (int i = 0; i < n; i++) {
            p_transformed[i] = exp(p_transformed[i]);
        }
    }
    if (!lower_tail) {
        for (int i = 0; i < n; i++) {
            p_transformed[i] = 1.0 - p_transformed[i];
        }
    }
    
    NumericVector pnew(n);
    for (int i = 0; i < n; i++) {
        pnew[i] = (p_transformed[i] - recycled.vec5[i]) / (1.0 - recycled.vec5[i]) - 1e-7;
        if (pnew[i] < 0.0) pnew[i] = 0.0;
    }
    
    return fqSICHEL(pnew, recycled.vec2, recycled.vec3, recycled.vec4, true, false);
}

//' Zero-Inflated Sichel Distribution Cumulative Distribution Function
//'
//' Distribution function for the zero-inflated Sichel distribution with parameters 
//' mu (mean), sigma (dispersion), nu (shape), and tau (zero-inflation).
//'
//' @param q vector of quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of shape parameters (real values).
//' @param tau vector of zero-inflation parameters (0 < tau < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The zero-inflated Sichel distribution is a mixture of a point mass at zero 
//' and a (truncated at zero) Sichel distribution.
//'
//' @return A numeric vector of probabilities.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modeling location, scale, and shape: Using GAMLSS in R, 
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Vector inputs with recycling
//' fpZISICHEL(0:5, mu=c(1,2), sigma=1, nu=-0.5, tau=0.1)
//'
//' @export
// [[Rcpp::export]]
NumericVector fpZISICHEL(const NumericVector& q,
                         const NumericVector& mu,
                         const NumericVector& sigma,
                         const NumericVector& nu,
                         const NumericVector& tau,
                         const bool& lower_tail = true,
                         const bool& log_p = false) {
    // Recycle vectors to common length
    auto recycled = recycle_vectors(q, mu, sigma, nu, tau);
    const int n = recycled.n;
    
    // Validate parameters after recycling
    for (int i = 0; i < n; i++) {
        if (recycled.vec1[i] < 0.0) stop("q must be >=0");
        if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
        if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
        if (recycled.vec5[i] <= 0.0 || recycled.vec5[i] >= 1.0) 
            stop("tau must be between 0 and 1");
    }
    
    NumericVector cdf(n);
    
    for (int i = 0; i < n; i++) {
        const int qi = static_cast<int>(recycled.vec1[i]);
        const double mui = recycled.vec2[i];
        const double sigmai = recycled.vec3[i];
        const double nui = recycled.vec4[i];
        const double taui = recycled.vec5[i];
        
        const double sichel_cdf = fpSICHEL_scalar(qi, mui, sigmai, nui, true, false);
        cdf[i] = taui + (1.0 - taui) * sichel_cdf;
    }
    
    if (!lower_tail) cdf = 1.0 - cdf;
    if (log_p) cdf = log(cdf);
    
    return cdf;
}
