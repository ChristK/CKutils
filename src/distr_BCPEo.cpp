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
// [[Rcpp::plugins(cpp17)]]

// Enable vectorization hints for modern compilers
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif

using namespace Rcpp;

// SIMD-optimized inline helper functions without caching
// These compute constants on-the-fly for optimal vectorization with unique tau values

// Precomputed constant to avoid repeated calls to log(2)
static constexpr double LOG_2 = 0.6931471805599453;

// Compute log_c for BCPEo distribution
inline double compute_log_c(const double inv_tau) {
  return 0.5 * (-2.0 * inv_tau * LOG_2 + R::lgammafn(inv_tau) - R::lgammafn(3.0 * inv_tau));
}

// Helper function for T distribution density
inline double fdBCPEo_hlp_f_T(const double t, const double tau, const bool log_ = false) {
  if (tau <= 0.0) return log_ ? R_NegInf : 0.0;
  
  const double inv_tau = 1.0 / tau;
  const double log_c = compute_log_c(inv_tau);
  const double c = exp(log_c);
  
  if (!R_finite(c) || c <= 0.0) return log_ ? R_NegInf : 0.0;
  
  const double abs_t_over_c = std::abs(t / c);
  const double term1 = log(tau) - log_c - (1.0 + inv_tau) * LOG_2 - R::lgammafn(inv_tau);
  const double log_lik = term1 - 0.5 * std::pow(abs_t_over_c, tau);  // Use tau, not inv_tau
  
  return log_ ? log_lik : exp(log_lik);
}

// Helper function for T distribution CDF
inline double fdBCPEo_hlp_F_T(const double t, const double tau) {
  if (tau <= 0.0) return 0.5;
  
  const double inv_tau = 1.0 / tau;
  const double log_c = compute_log_c(inv_tau);
  const double c = exp(log_c);
  
  if (!R_finite(c) || c <= 0.0) return 0.5;
  
  const double abs_t_over_c = std::abs(t / c);
  const double s = 0.5 * std::pow(abs_t_over_c, tau);  // Use tau, not inv_tau
  const double F_s = R::pgamma(s, inv_tau, 1.0, 1, 0);
  return 0.5 * (1.0 + F_s * R::sign(t));
}

// Helper function for T distribution quantile
inline double fqBCPEo_hlp_q_T(const double p, const double tau) {
  if (tau <= 0.0 || p < 0.0 || p > 1.0) return R_NaN;
  if (p == 0.5) return 0.0;
  
  const double inv_tau = 1.0 / tau;
  const double log_c = compute_log_c(inv_tau);
  const double c = exp(log_c);
  
  if (!R_finite(c) || c <= 0.0) return R_NaN;
  
  const double s = R::qgamma((2.0 * p - 1.0) * R::sign(p - 0.5), inv_tau, 1.0, true, false);
  if (!R_finite(s)) return R_NaN;
  
  return R::sign(p - 0.5) * std::pow(2.0 * s, inv_tau) * c;  // Use inv_tau here
}

//' Box-Cox Power Exponential Distribution (BCPEo) - Density Function
//'
//' Density function for the Box-Cox Power Exponential distribution with log link for mu,
//' optimized for SIMD vectorization and unique parameter values.
//'
//' @param x vector of (non-negative) quantiles.
//' @param mu vector of (positive) location parameters.
//' @param sigma vector of (positive) scale parameters.
//' @param nu vector of shape parameters.
//' @param tau vector of (positive) shape parameters.
//' @param log_ logical; if TRUE, densities are returned on the log scale.
//'
//' @details
//' The Box-Cox Power Exponential (BCPEo) distribution is a four-parameter continuous
//' distribution defined on the positive real line. The BCPEo variant uses a log link
//' for the location parameter mu.
//'
//' The probability density function is given by:
//' \deqn{f(x|\mu,\sigma,\nu,\tau) = \frac{1}{\sigma x^{\nu}} \frac{f_T(z)}{F_T(1/(\sigma|\nu|))}}
//' where \eqn{z = (x^{\nu} - 1)/(\nu\sigma)} when \eqn{\nu \neq 0} and 
//' \eqn{z = \log(x)/\sigma} when \eqn{\nu = 0}, and \eqn{f_T} and \eqn{F_T} are 
//' the density and distribution functions of a specific T distribution.
//'
//' This implementation is optimized for cases where tau values are rarely repeated,
//' using SIMD vectorization and per-element computation without caching.
//'
//' @return A numeric vector of densities.
//'
//' @references
//' Rigby, R. A. and Stasinopoulos, D. M. (2003) Flexible regression smoothing 
//' using GAMLSS. Applied Statistics, 52, 229-237.
//'
//' Stasinopoulos, D. M., Rigby, R. A., Heller, G. Z., Voudouris, V., and 
//' De Bastiani, F. (2017) Flexible Regression and Smoothing: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @note
//' This implementation is based on the gamlss.dist package dBCPEo function
//' but optimized for performance with unique parameter values and SIMD vectorization.
//'
//' @examples
//' # Basic usage
//' x <- c(1, 2, 3, 4, 5)
//' mu <- c(2, 2, 2, 2, 2)
//' sigma <- c(0.5, 0.5, 0.5, 0.5, 0.5)
//' nu <- c(1, 1, 1, 1, 1)
//' tau <- c(2, 2, 2, 2, 2)
//' 
//' # Calculate densities
//' fdBCPEo(x, mu, sigma, nu, tau)
//' 
//' # Log densities
//' fdBCPEo(x, mu, sigma, nu, tau, log_ = TRUE)
//'
//' @seealso \code{\link{fpBCPEo}}, \code{\link{fqBCPEo}}
//' @export
// [[Rcpp::export]]
NumericVector fdBCPEo(const NumericVector& x,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu,
                      const NumericVector& tau,
                      const bool& log_ = false)
{
  if (x.length() != mu.length() || mu.length() != sigma.length() ||
      sigma.length() != nu.length() || nu.length() != tau.length()
        ) stop("Distribution parameters must be of same length");

  const int n = x.length();
  NumericVector loglik(n);

  // Input validation
  for (int i = 0; i < n; i++) {
    if (x[i] < 0.0) stop("x must be >=0");
    if (mu[i] <= 0.0) stop("mu must be positive");
    if (sigma[i] <= 0.0) stop("sigma must be positive");  
    if (tau[i] <= 0.0) stop("tau must be positive");
  }

  // SIMD-optimized main computation loop without caching
  SIMD_HINT
  for (int i = 0; i < n; i++) {
    // Compute z transformation - equivalent to gamlss.dist logic
    double z;
    const double x_over_mu = x[i] / mu[i];
    if (nu[i] != 0.0) {
      z = (std::pow(x_over_mu, nu[i]) - 1.0) / (nu[i] * sigma[i]);
    } else {
      z = log(x_over_mu) / sigma[i];
    }

    // Compute log-likelihood using per-element helpers
    // Use abs(nu) directly to match gamlss.dist exactly
    const double logfZ = fdBCPEo_hlp_f_T(z, tau[i], true) -
      log(fdBCPEo_hlp_F_T(1.0 / (sigma[i] * std::abs(nu[i])), tau[i]));
    
    const double logder = (nu[i] - 1.0) * log(x[i]) - nu[i] * log(mu[i]) - log(sigma[i]);
    loglik[i] = logder + logfZ;
  }

  if (!log_) {
    SIMD_HINT
    for (int i = 0; i < n; i++) {
      loglik[i] = exp(loglik[i]);
    }
  }

  if (any(is_na(loglik))) warning("NaNs or NAs were produced");
  return loglik;
}

//' Box-Cox Power Exponential Distribution (BCPEo) - Distribution Function
//'
//' Distribution function for the Box-Cox Power Exponential distribution with log link for mu,
//' optimized for SIMD vectorization and unique parameter values.
//'
//' @param q vector of (non-negative) quantiles.
//' @param mu vector of (positive) location parameters.
//' @param sigma vector of (positive) scale parameters.
//' @param nu vector of shape parameters.
//' @param tau vector of (positive) shape parameters.
//' @param lower_tail logical; if TRUE (default), probabilities are P(X ≤ x), 
//'        otherwise P(X > x).
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The Box-Cox Power Exponential (BCPEo) distribution is a four-parameter continuous
//' distribution defined on the positive real line. This function computes the
//' cumulative distribution function (CDF).
//'
//' The cumulative distribution function is given by:
//' \deqn{F(x|\mu,\sigma,\nu,\tau) = \frac{F_T(z) - F_T(-1/(\sigma|\nu|))}{F_T(1/(\sigma|\nu|))}}
//' where \eqn{z = (x^{\nu} - 1)/(\nu\sigma)} when \eqn{\nu \neq 0} and 
//' \eqn{z = \log(x)/\sigma} when \eqn{\nu = 0}, and \eqn{F_T} is the 
//' distribution function of a specific T distribution.
//'
//' This implementation is optimized for cases where tau values are rarely repeated,
//' using SIMD vectorization and per-element computation without caching.
//'
//' @return A numeric vector of probabilities.
//'
//' @references
//' Rigby, R. A. and Stasinopoulos, D. M. (2003) Flexible regression smoothing 
//' using GAMLSS. Applied Statistics, 52, 229-237.
//'
//' Stasinopoulos, D. M., Rigby, R. A., Heller, G. Z., Voudouris, V., and 
//' De Bastiani, F. (2017) Flexible Regression and Smoothing: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @note
//' This implementation is based on the gamlss.dist package pBCPEo function
//' but optimized for performance with unique parameter values and SIMD vectorization.
//'
//' @examples
//' # Basic usage
//' q <- c(1, 2, 3, 4, 5)
//' mu <- c(2, 2, 2, 2, 2)
//' sigma <- c(0.5, 0.5, 0.5, 0.5, 0.5)
//' nu <- c(1, 1, 1, 1, 1)
//' tau <- c(2, 2, 2, 2, 2)
//' 
//' # Calculate probabilities
//' fpBCPEo(q, mu, sigma, nu, tau)
//' 
//' # Upper tail probabilities
//' fpBCPEo(q, mu, sigma, nu, tau, lower_tail = FALSE)
//' 
//' # Log probabilities
//' fpBCPEo(q, mu, sigma, nu, tau, log_p = TRUE)
//'
//' @seealso \code{\link{fdBCPEo}}, \code{\link{fqBCPEo}}
//' pBCPEo distribution function
//' @export
// [[Rcpp::export]]
NumericVector fpBCPEo(const NumericVector& q,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const NumericVector& tau,
                        const bool& lower_tail = true,
                        const bool& log_p = false)
{
  if (q.length() != mu.length() || mu.length() != sigma.length() ||
      sigma.length() != nu.length() || nu.length() != tau.length()
  ) stop("Distribution parameters must be of same length");

  const int n = q.length();
  NumericVector out(n);

  // Input validation
  for (int i = 0; i < n; i++) {
    if (q[i] < 0.0) stop("q must be positive");
    if (mu[i] <= 0.0) stop("mu must be positive");
    if (sigma[i] <= 0.0) stop("sigma must be positive");
    if (tau[i] <= 0.0) stop("tau must be positive");
  }

  // SIMD-optimized main computation loop without caching
  SIMD_HINT
  for (int i = 0; i < n; i++) {
    // Compute z transformation - equivalent to gamlss.dist logic
    double z;
    const double q_over_mu = q[i] / mu[i];
    if (nu[i] != 0.0) {
      z = (std::pow(q_over_mu, nu[i]) - 1.0) / (nu[i] * sigma[i]);
    } else {
      z = log(q_over_mu) / sigma[i];
    }

    const double FYy1 = fdBCPEo_hlp_F_T(z, tau[i]);

    double FYy2 = 0.0;
    if (nu[i] > 0.0) {
      FYy2 = fdBCPEo_hlp_F_T(-1.0 / (sigma[i] * std::abs(nu[i])), tau[i]);
    }
    const double FYy3 = fdBCPEo_hlp_F_T(1.0 / (sigma[i] * std::abs(nu[i])), tau[i]);
    out[i] = (FYy1 - FYy2) / FYy3;
  }

  if (!lower_tail) {
    SIMD_HINT  
    for (int i = 0; i < n; i++) {
      out[i] = 1.0 - out[i];
    }
  }
  
  if (log_p) {
    SIMD_HINT
    for (int i = 0; i < n; i++) {
      out[i] = log(out[i]);
    }
  }

  if (any(is_na(out))) warning("NaNs or NAs were produced");
  return out;
}

//' Box-Cox Power Exponential Distribution (BCPEo) - Quantile Function
//'
//' Quantile function for the Box-Cox Power Exponential distribution with log link for mu,
//' optimized for SIMD vectorization and unique parameter values.
//'
//' @param p vector of probabilities (must be between 0 and 1).
//' @param mu vector of (positive) location parameters.
//' @param sigma vector of (positive) scale parameters.
//' @param nu vector of shape parameters.
//' @param tau vector of (positive) shape parameters.
//' @param lower_tail logical; if TRUE (default), probabilities are P(X ≤ x), 
//'        otherwise P(X > x).
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The Box-Cox Power Exponential (BCPEo) distribution is a four-parameter continuous
//' distribution defined on the positive real line. This function computes the
//' quantile function (inverse CDF).
//'
//' The quantile function involves solving:
//' \deqn{F(x|\mu,\sigma,\nu,\tau) = p}
//' where F is the cumulative distribution function. The solution is obtained by
//' first computing the quantile z_a from the underlying T distribution, then
//' transforming back to the original scale.
//'
//' When \eqn{\nu = 0}: \eqn{x = \mu \exp(\sigma z_a)}
//' When \eqn{\nu \neq 0}: \eqn{x = \mu (\nu\sigma z_a + 1)^{1/\nu}}
//'
//' This implementation is optimized for cases where tau values are rarely repeated,
//' using SIMD vectorization and per-element computation without caching.
//'
//' @return A numeric vector of quantiles.
//'
//' @references
//' Rigby, R. A. and Stasinopoulos, D. M. (2003) Flexible regression smoothing 
//' using GAMLSS. Applied Statistics, 52, 229-237.
//'
//' Stasinopoulos, D. M., Rigby, R. A., Heller, G. Z., Voudouris, V., and 
//' De Bastiani, F. (2017) Flexible Regression and Smoothing: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @note
//' This implementation is based on the gamlss.dist package qBCPEo function
//' but optimized for performance with unique parameter values and SIMD vectorization.
//'
//' @examples
//' # Basic usage
//' p <- c(0.1, 0.25, 0.5, 0.75, 0.9)
//' mu <- c(2, 2, 2, 2, 2)
//' sigma <- c(0.5, 0.5, 0.5, 0.5, 0.5)
//' nu <- c(1, 1, 1, 1, 1)
//' tau <- c(2, 2, 2, 2, 2)
//' 
//' # Calculate quantiles
//' fqBCPEo(p, mu, sigma, nu, tau)
//' 
//' # Upper tail quantiles
//' fqBCPEo(p, mu, sigma, nu, tau, lower_tail = FALSE)
//' 
//' # Log probabilities
//' log_p <- log(p)
//' fqBCPEo(log_p, mu, sigma, nu, tau, log_p = TRUE)
//'
//' @seealso \code{\link{fdBCPEo}}, \code{\link{fpBCPEo}}
//' qBCPEo quantile function
//' @export
// [[Rcpp::export]]
NumericVector fqBCPEo(const NumericVector& p,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const NumericVector& tau,
                        const bool& lower_tail = true,
                        const bool& log_p = false)
{
  if (p.length() != mu.length() || mu.length() != sigma.length() ||
      sigma.length() != nu.length() || nu.length() != tau.length()
  ) stop("Distribution parameters must be of same length");

  const int n = p.length();
  NumericVector out(n);
  
  // Make a copy of p to avoid modifying the input
  NumericVector p_cloned = clone(p);
  
  // Input validation for parameters
  for (int i = 0; i < n; i++) {
    if (mu[i] <= 0.0) stop("mu must be positive");
    if (sigma[i] <= 0.0) stop("sigma must be positive");
    if (tau[i] <= 0.0) stop("tau must be positive");
  }
  
  // Transform p values first (matching R reference order)
  if (log_p) {
    for (int i = 0; i < n; i++) {
      p_cloned[i] = exp(p_cloned[i]);
    }
  }
  
  // Input validation for p values after log_p transformation (matching R reference)
  for (int i = 0; i < n; i++) {
    if (p_cloned[i] < 0.0 || p_cloned[i] > 1.0)
      stop("p must be between 0 and 1");
  }
  
  if (!lower_tail) {
    for (int i = 0; i < n; i++) {
      p_cloned[i] = 1.0 - p_cloned[i];
    }
  }

  // SIMD-optimized main computation loop without caching
  SIMD_HINT
  for (int i = 0; i < n; i++) {
    // Handle edge cases for p=0 and p=1
    if (p_cloned[i] == 0.0)
    {
      out[i] = 0.0;  // Lower bound of BCPEo distribution
      continue;
    }
    if (p_cloned[i] == 1.0)
    {
      out[i] = R_PosInf;  // Upper bound of BCPEo distribution
      continue;
    }

    double za;
    const double sigma_abs_nu = sigma[i] * std::abs(nu[i]);
    
    if (nu[i] < 0.0) {
      const double F_bound = fdBCPEo_hlp_F_T(1.0 / sigma_abs_nu, tau[i]);
      za = fqBCPEo_hlp_q_T(p_cloned[i] * F_bound, tau[i]);
    } else if (nu[i] == 0.0) {
      za = fqBCPEo_hlp_q_T(p_cloned[i], tau[i]);
    } else { // nu > 0
      const double F_bound = fdBCPEo_hlp_F_T(1.0 / sigma_abs_nu, tau[i]);
      za = fqBCPEo_hlp_q_T(1.0 - (1.0 - p_cloned[i]) * F_bound, tau[i]);
    }

    if (nu[i] == 0.0) {
      out[i] = mu[i] * exp(sigma[i] * za);
    } else {
      const double base = nu[i] * sigma[i] * za + 1.0;
      out[i] = mu[i] * std::pow(base, 1.0 / nu[i]);
    }
  }
  
  if (any(is_na(out))) warning("NaNs or NAs were produced");
  return out;
}