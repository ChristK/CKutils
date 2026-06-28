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
#include "distr_SICHEL.h"   // canonical header-only scalar definitions
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// SICHEL helper and *_scalar definitions (compute_cvec, compute_alpha,
// compute_lbes, ftofySICHEL2_scalar, fcdfSICHEL_scalar, fpSICHEL_scalar) now
// live (inline) in inst/include/distr_SICHEL.h so that downstream
// LinkingTo: CKutils consumers can call them directly.

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
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, 
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
        // NaN/NA x -> NA: static_cast<int>(NaN) below is out-of-range float-to-int
        // UB, and a NaN x slips past the `< 0` check (NaN comparisons are false).
        if (ISNAN(recycled.vec1[i])) {
            logfy[i] = NA_REAL;
            continue;
        }
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

// fcdfSICHEL_scalar now lives (inline) in inst/include/distr_SICHEL.h.

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
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, 
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
        // NaN/NA q -> NA: static_cast<int>(NaN) below is out-of-range float-to-int
        // UB, and a NaN q slips past the `< 0` check (NaN comparisons are false).
        if (ISNAN(recycled.vec1[i])) {
            cdf[i] = NA_REAL;
            continue;
        }
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


// fpSICHEL_scalar now lives (inline) in inst/include/distr_SICHEL.h.

// Optimized quantile search using incremental CDF computation
// This directly computes densities incrementally without recomputing from scratch
int fqSICHEL_search(const double& p, const double& mu, const double& sigma, const double& nu) {
    // NaN/NA guard: the vector wrapper (fqSICHEL) already maps NaN args to NA, but
    // guard here too so the search/qpois path can never see NaN, which would lead
    // to out-of-range float-to-int undefined behaviour or a wrong non-NA result.
    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu)) {
        return NA_INTEGER;
    }
    // Use NBI approximation for large sigma and positive nu
    if (sigma > 10000.0 && nu > 0.0) {
        return fqNBI_scalar(p, mu, 1.0/nu, true, false);
    }
    
    // Precompute constants
    const double cvec = compute_cvec(sigma, nu);
    const double alpha = compute_alpha(sigma, mu, cvec);
    const double lbes = compute_lbes(alpha, nu);
    
    // Initial density and CDF at y=0
    double tynew_prev = (mu / cvec) * pow(1.0 + 2.0 * sigma * mu / cvec, -0.5) * exp(lbes);
    double lpnew_prev = -nu * log(sigma * alpha) + log(R::bessel_k(alpha, nu, 1)) - 
                        log(R::bessel_k(1.0/sigma, nu, 1));
    
    double cdf = exp(lpnew_prev);
    
    if (cdf >= p) {
        return 0;
    }
    
    // Incremental search
    const int max_iter = 1000000;
    const double sigma_alpha_cvec_sq = pow(mu / (sigma * alpha * cvec), 2.0);
    
    for (int j = 1; j < max_iter; j++) {
        double tynew_curr = (cvec * sigma * (2.0 * (j + nu) / mu) + (1.0 / tynew_prev)) * 
                           sigma_alpha_cvec_sq;
        double lpnew_curr = lpnew_prev + log(tynew_prev) - log(static_cast<double>(j));
        
        cdf += exp(lpnew_curr);
        
        if (cdf >= p) {
            return j;
        }
        
        tynew_prev = tynew_curr;
        lpnew_prev = lpnew_curr;
    }
    
    return max_iter;
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
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, 
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

        // NaN/NA in probability or any distribution parameter -> NA quantile.
        // Without this a NaN p slips past the [0,1] range checks above (every NaN
        // comparison is false) and reaches fqSICHEL_search, where it would feed an
        // out-of-range float-to-int cast / wrong non-NA search result. Base R
        // returns NA for a NaN probability.
        if (ISNAN(pi) || ISNAN(mui) || ISNAN(sigmai) || ISNAN(nui)) {
            QQQ[i] = NA_INTEGER;
            continue;
        }

        if (pi + 1e-09 >= 1.0) {
            QQQ[i] = R_PosInf;
            continue;
        }
        
        // Use optimized incremental search
        QQQ[i] = fqSICHEL_search(pi, mui, sigmai, nui);
    }
    
    return QQQ;
}
