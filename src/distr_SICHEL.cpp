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
#include "distr_NBI.h"
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// Helper functions for SICHEL computations

// Compute cvec efficiently
inline double compute_cvec(const double& sigma, const double& nu) {
    return exp(log(R::bessel_k(1.0/sigma, nu + 1.0, 1)) - log(R::bessel_k(1.0/sigma, nu, 1)));
}

// Compute alpha efficiently
inline double compute_alpha(const double& sigma, const double& mu, const double& cvec) {
    return sqrt(1.0 + 2.0 * sigma * mu / cvec) / sigma;
}

// Compute lbes efficiently
inline double compute_lbes(const double& alpha, const double& nu) {
    return log(R::bessel_k(alpha, nu + 1.0, 1)) - log(R::bessel_k(alpha, nu, 1));
}

// Scalar helper function for tofySICHEL computation
double ftofySICHEL2_scalar(const int& y, const double& mu,
                          const double& sigma, const double& nu, 
                          const double& lbes, const double& cvec) {
    if (y <= 0) return 0.0;
    
    const int iy = y + 1;  // This is the key: iy = y + 1
    std::vector<double> tofY(iy);
    const double alpha = compute_alpha(sigma, mu, cvec);
    
    tofY[0] = (mu / cvec) * pow(1.0 + 2.0 * sigma * mu / cvec, -0.5) * exp(lbes);
    
    double sumT = 0.0;
    for (int j = 1; j < iy; j++) {  // j < iy, not j < y
        tofY[j] = (cvec * sigma * (2.0 * (j + nu) / mu) + (1.0 / tofY[j-1])) * 
                  pow(mu / (sigma * alpha * cvec), 2.0);
        sumT += log(tofY[j-1]);
    }
    
    return sumT;
}

//' Sichel Distribution Density
//'
//' Probability density function for the Sichel distribution with parameters 
//' mu (mean), sigma (dispersion), and nu (shape).
//'
//' @param x vector of (non-negative integer) quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of shape parameters (real values).
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The probability mass function of the Sichel distribution is:
//' \deqn{f(y|\mu,\sigma,\nu)= \frac{(\mu/c)^y K_{y+\nu}(\alpha)}{y!(\alpha \sigma)^{y+\nu} K_\nu(\frac{1}{\sigma})}}
//' for \eqn{y=0,1,2,...}, \eqn{\mu>0}, \eqn{\sigma>0} and \eqn{-\infty<\nu<\infty}
//' where \eqn{\alpha^2= 1/\sigma^2 +2*\mu/\sigma}, 
//' \eqn{c=K_{\nu+1}(1/\sigma)/K_{\nu}(1/\sigma)}, and 
//' \eqn{K_{\lambda}(t)} is the modified Bessel function of the third kind.
//'
//' When \eqn{\sigma > 10000} and \eqn{\nu > 0}, the function uses the NBI 
//' approximation for numerical stability.
//'
//' @return A numeric vector of density values.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modeling location, scale, and shape: Using GAMLSS in R, 
//' Chapman and Hall/CRC.
//' 
//' Stein, G. Z., Zucchini, W. and Juritz, J. M. (1987). Parameter
//' Estimation of the Sichel Distribution and its Multivariate Extension.
//' Journal of American Statistical Association, 82, 938-944.
//'
//' @examples
//' # Single values
//' fdSICHEL(c(0,1,2,3), mu=1, sigma=1, nu=-0.5)
//' 
//' # Vector inputs with recycling
//' fdSICHEL(0:5, mu=c(1,2), sigma=1, nu=-0.5)
//'
//' @export
// [[Rcpp::export]]
NumericVector fdSICHEL(const NumericVector& x,
                       const NumericVector& mu,
                       const NumericVector& sigma,
                       const NumericVector& nu,
                       const bool& log_p = false) {
    // Recycle vectors to common length
    auto recycled = recycle_vectors(x, mu, sigma, nu);
    const int n = recycled.n;
    
    // Validate parameters after recycling
    for (int i = 0; i < n; i++) {
        if (recycled.vec1[i] < 0.0) stop("x must be >=0");
        if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
        if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    }
    
    NumericVector logfy(n);
    
    for (int i = 0; i < n; i++) {
        const int xi = static_cast<int>(recycled.vec1[i]);
        const double mui = recycled.vec2[i];
        const double sigmai = recycled.vec3[i];
        const double nui = recycled.vec4[i];
        
        // Use NBI approximation for large sigma and positive nu
        if (sigmai > 10000.0 && nui > 0.0) {
            logfy[i] = fdNBI_scalar(xi, mui, 1.0/nui, log_p);
            continue;
        }
        
        const double cvec = compute_cvec(sigmai, nui);
        const double alpha = compute_alpha(sigmai, mui, cvec);
        const double lbes = compute_lbes(alpha, nui);
        const double sumlty = ftofySICHEL2_scalar(xi, mui, sigmai, nui, lbes, cvec);
        
        logfy[i] = -R::lgammafn(xi + 1.0) - nui * log(sigmai * alpha) + sumlty +
                   log(R::bessel_k(alpha, nui, 1)) - log(R::bessel_k(1.0/sigmai, nui, 1));
        
        if (!log_p) logfy[i] = exp(logfy[i]);
    }
    
    // Check for NaN/NA values
    if (any(is_na(logfy))) {
        warning("NaNs or NAs were produced");
    }
    
    return logfy;
}

// CDF helper function
double fcdfSICHEL_scalar(const int& y, const double& mu, const double& sigma, const double& nu) {
    if (y < 0) return 0.0;
    
    const int lyp1 = y + 1;
    const double cvec = compute_cvec(sigma, nu);
    const double alpha = compute_alpha(sigma, mu, cvec);
    const double lbes = compute_lbes(alpha, nu);
    
    std::vector<double> tynew(lyp1);
    std::vector<double> lpnew(lyp1);
    
    tynew[0] = (mu / cvec) * pow(1.0 + 2.0 * sigma * mu / cvec, -0.5) * exp(lbes);
    lpnew[0] = -nu * log(sigma * alpha) + log(R::bessel_k(alpha, nu, 1)) - 
               log(R::bessel_k(1.0/sigma, nu, 1));
    
    for (int j = 1; j < lyp1; j++) {
        tynew[j] = (cvec * sigma * (2.0 * (j + nu) / mu) + (1.0 / tynew[j-1])) * 
                   pow(mu / (sigma * alpha * cvec), 2.0);
        lpnew[j] = lpnew[j-1] + log(tynew[j-1]) - log(j);
    }
    
    double sumT = 0.0;
    for (int j = 0; j < lyp1; j++) {
        sumT += exp(lpnew[j]);
    }
    
    return sumT;
}

//' Sichel Distribution Cumulative Distribution Function
//'
//' Distribution function for the Sichel distribution with parameters 
//' mu (mean), sigma (dispersion), and nu (shape).
//'
//' @param q vector of quantiles.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of shape parameters (real values).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The cumulative distribution function computes the probability that a 
//' Sichel random variable is less than or equal to q.
//'
//' @return A numeric vector of probabilities.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modeling location, scale, and shape: Using GAMLSS in R, 
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Single values
//' fpSICHEL(c(0,1,2,3), mu=1, sigma=1, nu=-0.5)
//' 
//' # Vector inputs with recycling
//' fpSICHEL(0:5, mu=c(1,2), sigma=1, nu=-0.5)
//'
//' @export
// [[Rcpp::export]]
NumericVector fpSICHEL(const NumericVector& q,
                       const NumericVector& mu,
                       const NumericVector& sigma,
                       const NumericVector& nu,
                       const bool& lower_tail = true,
                       const bool& log_p = false) {
    // Recycle vectors to common length
    auto recycled = recycle_vectors(q, mu, sigma, nu);
    const int n = recycled.n;
    
    // Validate parameters after recycling
    for (int i = 0; i < n; i++) {
        if (recycled.vec1[i] < 0.0) stop("q must be >=0");
        if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
        if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    }
    
    NumericVector cdf(n);
    
    for (int i = 0; i < n; i++) {
        const int qi = static_cast<int>(recycled.vec1[i]);
        const double mui = recycled.vec2[i];
        const double sigmai = recycled.vec3[i];
        const double nui = recycled.vec4[i];
        
        cdf[i] = fcdfSICHEL_scalar(qi, mui, sigmai, nui);
    }
    
    if (!lower_tail) cdf = 1.0 - cdf;
    if (log_p) cdf = log(cdf);
    
    // Check for NaN/NA values
    if (any(is_na(cdf))) {
        warning("NaNs or NAs were produced");
    }
    
    return cdf;
}


// Scalar CDF function for internal use
double fpSICHEL_scalar(const int& q, const double& mu, const double& sigma, const double& nu,
                       const bool& lower_tail = true, const bool& log_p = false) {
    double cdf = fcdfSICHEL_scalar(q, mu, sigma, nu);
    
    if (!lower_tail) cdf = 1.0 - cdf;
    if (log_p) cdf = log(cdf);
    
    return cdf;
}

//' Sichel Distribution Quantile Function
//'
//' Quantile function for the Sichel distribution with parameters 
//' mu (mean), sigma (dispersion), and nu (shape).
//'
//' @param p vector of probabilities.
//' @param mu vector of positive means.
//' @param sigma vector of positive dispersion parameters.
//' @param nu vector of shape parameters (real values).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The quantile function uses a divide-and-conquer search algorithm to find
//' the smallest integer x such that P(X <= x) >= p.
//'
//' @return A numeric vector of quantiles.
//' 
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019) 
//' Distributions for modeling location, scale, and shape: Using GAMLSS in R, 
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Single values
//' fqSICHEL(c(0.1, 0.5, 0.9), mu=1, sigma=1, nu=-0.5)
//' 
//' # Vector inputs with recycling
//' fqSICHEL(c(0.25, 0.75), mu=c(1,2), sigma=1, nu=-0.5)
//'
//' @export
// [[Rcpp::export]]
IntegerVector fqSICHEL(NumericVector p,
                       const NumericVector& mu,
                       const NumericVector& sigma,
                       const NumericVector& nu,
                       const bool& lower_tail = true,
                       const bool& log_p = false) {
    // Recycle vectors to common length
    auto recycled = recycle_vectors(p, mu, sigma, nu);
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
        if (recycled.vec1[i] < 0.0 || recycled.vec1[i] > 1.0001)
          stop("p must be between 0 and 1");
        if (recycled.vec2[i] <= 0.0)
          stop("mu must be greater than 0");
        if (recycled.vec3[i] <= 0.0)
          stop("sigma must be greater than 0");
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
    
    IntegerVector QQQ(n);
    
    for (int i = 0; i < n; i++) {
        const double pi = p_transformed[i];
        const double mui = recycled.vec2[i];
        const double sigmai = recycled.vec3[i];
        const double nui = recycled.vec4[i];
        
        if (pi + 1e-09 >= 1.0) {
            QQQ[i] = R_PosInf;
            continue;
        }
        
        // Divide and conquer algorithm
        int j = static_cast<int>(mui * (pi + 0.5)) + 1; // Initial guess
        double cumpro = fpSICHEL_scalar(j, mui, sigmai, nui, true, false);
        
        if (pi <= cumpro) {
            // Search downward
            while (j > 0 && pi <= cumpro) {
                j /= 2;
                cumpro = fpSICHEL_scalar(j, mui, sigmai, nui, true, false);
            }
            // Linear search in the final range
            for (int k = j; k <= (j * 2 + 1); k++) {
                cumpro = fpSICHEL_scalar(k, mui, sigmai, nui, true, false);
                if (pi <= cumpro) {
                    QQQ[i] = k;
                    break;
                }
            }
        } else {
            // Search upward
            while (j < INT_MAX && pi > cumpro) {
                j *= 2;
                cumpro = fpSICHEL_scalar(j, mui, sigmai, nui, true, false);
            }
            
            j /= 2;
            cumpro = fpSICHEL_scalar(j, mui, sigmai, nui, true, false);
            
            // Coarse search with step 1000
            if ((j * 2) > (j + 1000)) {
                while (j < INT_MAX && pi > cumpro) {
                    j += 1000;
                    cumpro = fpSICHEL_scalar(j, mui, sigmai, nui, true, false);
                }
                if (j >= 1000) {
                    j -= 1000;
                    cumpro = fpSICHEL_scalar(j, mui, sigmai, nui, true, false);
                }
            }
            
            // Coarse search with step 100
            while (j < INT_MAX && pi > cumpro) {
                j += 100;
                cumpro = fpSICHEL_scalar(j, mui, sigmai, nui, true, false);
            }
            if (j >= 100) {
                j -= 100;
                cumpro = fpSICHEL_scalar(j, mui, sigmai, nui, true, false);
            }
            
            // Coarse search with step 10
            while (j < INT_MAX && pi > cumpro) {
                j += 10;
                cumpro = fpSICHEL_scalar(j, mui, sigmai, nui, true, false);
            }
            if (j >= 10) {
                j -= 10;
            }
            
            // Final linear search
            for (int k = j; k <= INT_MAX; k++) {
                cumpro = fpSICHEL_scalar(k, mui, sigmai, nui, true, false);
                if (pi <= cumpro) {
                    QQQ[i] = k;
                    break;
                }
            }
        }
    }
    
    return QQQ;
}
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
    
    return fqSICHEL(pnew, recycled.vec2, recycled.vec3, recycled.vec4);
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
        
        const double sichel_cdf = fpSICHEL_scalar(qi, mui, sigmai, nui);
        cdf[i] = taui + (1.0 - taui) * sichel_cdf;
    }
    
    if (!lower_tail) cdf = 1.0 - cdf;
    if (log_p) cdf = log(cdf);
    
    return cdf;
}


