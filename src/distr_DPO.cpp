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
// SIMD headers are x86/x64 specific, only include on compatible architectures
#if defined(__x86_64__) || defined(__i386__) || defined(_M_X64) || defined(_M_IX86)
#include <immintrin.h>  // AVX/SSE
#include <xmmintrin.h>  // SSE
#include <emmintrin.h>  // SSE2
#endif
#include <vector>
#include <cstring>
#include "recycling_helpers.h"
#include "distr_DPO.h"   // canonical header-only scalar definitions
#include "distr_search.h"   // ck_search_stalled(), ck_search_settled(), CK_SEARCH_MAX
// [[Rcpp::plugins(cpp17)]]
using namespace Rcpp;

// Enable vectorization hints for modern compilers
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif

// SIMD utility functions
inline bool is_aligned(const void* ptr, size_t alignment = 32) {
    return reinterpret_cast<uintptr_t>(ptr) % alignment == 0;
}

// Check for SIMD support at runtime
inline bool has_avx2() {
    static int result = -1;
    if (result == -1) {
        #ifdef __AVX2__
            result = 1;
        #else
            result = 0;
        #endif
    }
    return result == 1;
}

// Vectorized math functions using SIMD (fallback to scalar if no SIMD)
inline void simd_exp_4(const double* input, double* output) {
    #ifdef __AVX2__
    if (has_avx2() && is_aligned(input) && is_aligned(output)) {
        __m256d x = _mm256_load_pd(input);
        (void)x; // Mark as intentionally unused to suppress warning
        // Fast approximation could be added here
        for (int i = 0; i < 4; i++) {
            output[i] = exp(input[i]);
        }
    } else
    #endif
    {
        for (int i = 0; i < 4; i++) {
            output[i] = exp(input[i]);
        }
    }
}

inline void simd_log_4(const double* input, double* output) {
    #ifdef __AVX2__
    if (has_avx2() && is_aligned(input) && is_aligned(output)) {
        for (int i = 0; i < 4; i++) {
            output[i] = log(input[i]);
        }
    } else
    #endif
    {
        for (int i = 0; i < 4; i++) {
            output[i] = log(input[i]);
        }
    }
}


// fdDPOgetC5_C_scalar, the DPOCache helper, and fdDPO_scalar now live (inline)
// in inst/include/distr_DPO.h so that downstream LinkingTo: CKutils consumers
// can call them directly.


//' Get Normalizing Constant for DPO Distribution
//'
//' Computes the logarithm of the normalizing constant for the DPO distribution.
//' This is an internal function used by the DPO distribution functions.
//'
//' @param x vector of (non-negative integer) quantiles
//' @param mu vector of positive means
//' @param sigma vector of positive dispersion parameters
//'
//' @details
//' This function computes the logarithm of the normalizing constant required
//' for the DPO (Double Poisson) distribution. The computation follows the
//' algorithm from gamlss.dist but with optimizations for performance.
//'
//' @return Vector of log normalizing constants
//'
//' @note
//' This function is based on the DPO distribution implementation from
//' the \pkg{gamlss.dist} package by Mikis Stasinopoulos, Robert Rigby,
//' and colleagues. The original gamlss.dist implementation is acknowledged
//' with gratitude.
//'
//' @references
//' Rigby, R. A. and Stasinopoulos D. M. (2005). Generalized additive models for 
//' location, scale and shape,(with discussion), Appl. Statist., 54, part 3, pp 507-554.
//' 
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, Chapman and Hall/CRC.
//'
//' @author Chris Kypridemos (optimised implementation), based on original work by 
//' Bob Rigby and Mikis Stasinopoulos from gamlss.dist package
//'
//' @seealso \code{\link{fdDPO}}, \code{\link{fpDPO}}, \code{\link{fqDPO}}
//'
//' @examples
//' # Log normalizing constants for given mu and sigma (x sets the search range)
//' fget_C(x = 0:5, mu = 2, sigma = 1.5)
//'
//' # mu and sigma are recycled to the length of the longest argument
//' fget_C(x = 0:3, mu = c(2, 4), sigma = c(1.2, 0.8))
//'
//' @export
// [[Rcpp::export]]
NumericVector fget_C(const IntegerVector& x,
                       const NumericVector& mu,
                       const NumericVector& sigma)
{
  // Any zero-length input -> zero-length result. This guards the i % length()
  // modulo-by-zero in the recycling loop below (UB / SIGFPE when mu or sigma is
  // empty while another arg is not).
  if (x.length() == 0 || mu.length() == 0 || sigma.length() == 0) {
    return NumericVector(0);
  }
  // The largest x, taken as a double (3 * an int can overflow) and skipping NA,
  // clamped so that maxV + 1 below still fits an int
  double mx = 0;
  for (int v : x) if (v != NA_INTEGER) mx = std::max(mx, static_cast<double>(v));
  const int maxV = static_cast<int>(std::min(std::max(3.0 * mx, 500.0), 2147483645.0));
  int lmu   = std::max(std::max(x.length(), mu.length()), sigma.length());

  // Per element, through the scalar: its window covers the mass around mu
  // (see fdDPO_C_window in distr_DPO.h). The old vector version summed every
  // element to max(3 * max(x), 500) only, in arrays that long.
  NumericVector out(lmu);
  for (int i = 0; i < lmu; i++) {
    out[i] = log(fdDPOgetC5_C_scalar(mu[i % mu.length()], sigma[i % sigma.length()],
                                     1, maxV + 1));
  }
  return out;
}

//' The DPO Distribution - Density Function
//'
//' Density function for the DPO (Double Poisson) distribution with parameters mu and sigma.
//' The DPO distribution is a discrete probability distribution that extends the
//' Poisson distribution by adding an additional dispersion parameter.
//'
//' @param x vector of (non-negative integer) quantiles. A non-integer is truncated to an integer; a count above 2147483646 gives \code{NA}.
//' @param mu vector of positive means
//' @param sigma vector of positive dispersion parameters
//' @param log_ logical; if TRUE, probabilities p are given as log(p)
//'
//' @details
//' The DPO distribution has probability mass function with mean mu and 
//' dispersion controlled by sigma. When sigma = 1, it reduces to the Poisson
//' distribution. Values of sigma > 1 indicate overdispersion, while sigma < 1
//' indicates underdispersion.
//' 
//' This implementation is based on the algorithms from the gamlss.dist package
//' by Rigby, R. A. and Stasinopoulos D. M.
//'
//' \emph{Limit.} The normalising constant of the density is a sum over the
//' counts around mu (its standard deviation is about
//' \code{sqrt(mu * max(sigma, 1))}), kept in a small per-thread cache keyed by
//' (mu, sigma). With a huge \code{mu} \emph{and} a huge \code{sigma} (for
//' example \code{mu = 3e9}, \code{sigma = 4e6}) the sum scans the whole integer
//' range before it gives up, so \code{fdDPO}, \code{fpDPO} and \code{fqDPO} each
//' take about 50 s (49.5 to 49.8 s measured) to answer \code{NaN} (\code{NA}
//' for \code{fqDPO}). An infinite \code{mu} or \code{sigma} gives \code{NaN}
//' at once.
//'
//' @return
//' \code{fdDPO} gives the density
//'
//' @references
//' Rigby, R. A. and Stasinopoulos D. M. (2005). Generalized additive models for 
//' location, scale and shape,(with discussion), Appl. Statist., 54, part 3, pp 507-554.
//' 
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, Chapman and Hall/CRC.
//' 
//' Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location 
//' scale and shape (GAMLSS) in R. Journal of Statistical Software, Vol. 23, Issue 7, Dec 2007.
//'
//' @author Chris Kypridemos (optimised implementation), based on original work by 
//' Bob Rigby and Mikis Stasinopoulos from gamlss.dist package
//'
//' @seealso \code{\link{fpDPO}}, \code{\link{fqDPO}}
//'
//' @examples
//' # Calculate density for single values
//' fdDPO(0:5, mu = 2, sigma = 1)
//' 
//' # Calculate log density
//' fdDPO(0:5, mu = 2, sigma = 1, log_ = TRUE)
//' 
//' # Parameter recycling
//' fdDPO(c(0, 1, 2), mu = c(1, 2, 3), sigma = c(0.5, 1, 1.5))
//'
//' @export
// [[Rcpp::export]]
NumericVector fdDPO(const IntegerVector &x,
                      const NumericVector &mu,
                      const NumericVector &sigma,
                      const bool &log_ = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(x, mu, sigma);
  const int n = recycled.n;
  
  NumericVector lh(n);
  
  // Process in chunks for better cache performance
  const int chunk_size = 32;
  
  for (int chunk_start = 0; chunk_start < n; chunk_start += chunk_size) {
    int chunk_end = std::min(chunk_start + chunk_size, n);
    
    // Prefetch data for this chunk
    #ifdef __builtin_prefetch
    if (chunk_end < n) {
      __builtin_prefetch(&recycled.vec1[chunk_end], 0, 3);
      __builtin_prefetch(&recycled.vec2[chunk_end], 0, 3);
      __builtin_prefetch(&recycled.vec3[chunk_end], 0, 3);
    }
    #endif
    
    for (int i = chunk_start; i < chunk_end; i++) {
      // NaN/NA x -> NA: static_cast<int>(NaN) below is out-of-range float-to-int
      // UB, and a NaN x slips past the `< 0` check (NaN comparisons are false).
      if (ISNAN(recycled.vec1[i])) {
        lh[i] = NA_REAL;
        continue;
      }
      if (recycled.vec1[i] < 0.0)
        stop("x must be >=0");
      if (recycled.vec2[i] <= 0.0)
        stop("mu must be greater than 0");
      if (recycled.vec3[i] <= 0.0)
        stop("sigma must be greater than 0");

      // NA/NaN or a count too large to convert to int -> NA. See count_to_int()
      // in recycling_helpers.h: the unguarded cast is out-of-range float-to-int
      // undefined behaviour and it is not benign on either x86-64 or AArch64.
      int x_i;
      if (!count_to_int(recycled.vec1[i], x_i)) {
        lh[i] = NA_REAL;
        continue;
      }
      lh[i] = fdDPO_scalar(x_i,
                          recycled.vec2[i], recycled.vec3[i], log_);
    }
  }

  // Check for NAs
  if (any(is_na(lh)))
    warning("NaNs or NAs were produced");
  return lh;
}

// fpDPO_scalar now lives (inline) in inst/include/distr_DPO.h so that
// downstream LinkingTo: CKutils consumers can call it directly.

// Optimized quantile search using incremental CDF computation
int fqDPO_search(const double& p,
                 const double& mu,
                 const double& sigma)
{
  // NaN/NA guard: the vector wrapper (fqDPO) already maps NaN args to NA, but
  // guard here too so R::qpois(NaN,...) and the static_cast<int> below can never
  // see NaN, which would be out-of-range float-to-int undefined behaviour.
  if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma)) {
    return NA_INTEGER;
  }
  // Fast path for near-Poisson case. R::qpois returns a double, and for a
  // large mu that double exceeds INT_MAX, so cast it through the same guard as
  // the count arguments: an unguarded cast is out-of-range UB and was returning
  // INT_MIN, i.e. a negative quantile (fqDPO(0.999, 3e9, 1) gave -2147483648
  // where the true quantile is 3000169260).
  if (std::abs(sigma - 1.0) < 1e-6) {
    int q_i;
    return count_to_int(R::qpois(p, mu, true, false), q_i) ? q_i : NA_INTEGER;
  }
  
  // Incremental search: compute CDF incrementally by adding densities. No
  // fixed cap: a density that is not finite gives NA_INTEGER, and so does a
  // stalled sum (ck_search_stalled, distr_search.h) that is not within
  // CK_P_FUZZ of p (ck_search_settled); a stalled sum within it returns the
  // index where it stopped. NA_INTEGER is returned rather than a number.
  // With a large mu, start at the first density that does not underflow
  // (ck_search_start, distr_search.h). For mu beyond the int range the
  // densities are NaN (their constant cannot be summed), and the search below
  // gives up at its first term.
  const long long start = (p > 0.0)
    ? ck_search_start([&](double q) { return fdDPO_scalar(static_cast<int>(q), mu, sigma, true); }, mu)
    : 0;
  double cdf = 0.0;
  double prev_density = -1.0;
  // Interruptible: Rcpp::checkUserInterrupt() every 2^20 terms (a scan can run for
  // tens of seconds). Only src/ loops do this: the inst/include kernels (fqBNB_search,
  // the DPO normalising-constant loop, fcdfSICHEL_scalar, CkDELCdf) are a LinkingTo API
  // that a consumer may call from threads where the R API must not be used, so they
  // stay uninterruptible. The first density at a new (mu, sigma) computes that constant
  // (cached): an interrupt during it takes effect once it is done (about 5 s at
  // mu = 2e9, sigma = 1e4).
  unsigned int steps = 0;
  for (int q = static_cast<int>(start); q <= CK_SEARCH_MAX; q++) {
    if ((++steps & 0xFFFFF) == 0) Rcpp::checkUserInterrupt();
    const double density = fdDPO_scalar(q, mu, sigma, false);
    if (!std::isfinite(density)) {
      return NA_INTEGER;
    }
    if (ck_search_stalled(density, prev_density, cdf)) {
      return ck_search_settled(cdf, p) ? q - 1 : NA_INTEGER;
    }
    cdf += density;
    if (cdf >= p) {
      return q;
    }
    prev_density = density;
  }
  return NA_INTEGER;
}

//' The DPO Distribution - Cumulative Distribution Function
//'
//' Distribution function for the DPO (Double Poisson) distribution with parameters mu and sigma.
//' Computes the cumulative distribution function (CDF) of the DPO distribution.
//'
//' @param q vector of (non-negative integer) quantiles. A non-integer is truncated to an integer; a count above 2147483646 gives \code{NA}.
//' @param mu vector of positive means
//' @param sigma vector of positive dispersion parameters
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x]
//' @param log_p logical; if TRUE, probabilities p are given as log(p)
//'
//' @details
//' The cumulative distribution function is computed using the normalizing constants
//' approach from gamlss.dist: the sum of the densities from the first one that
//' does not underflow up to q. The sum stops once it has settled (a falling term
//' past the largest no longer changes it), so a q far beyond the mass costs
//' only as far as the mass. The result never exceeds 1, and the upper tail is
//' summed from q + 1 where F > 0.5, so it is never negative.
//' 
//' This implementation is based on the algorithms from the gamlss.dist package
//' by Rigby, R. A. and Stasinopoulos D. M.
//'
//' \emph{Limit.} With a huge \code{mu} \emph{and} a huge \code{sigma} (for
//' example \code{mu = 3e9}, \code{sigma = 4e6}) the normalising constant takes
//' about 50 s to give up, and the answer is \code{NaN}: see \code{\link{fdDPO}}.
//'
//' @return
//' \code{fpDPO} gives the cumulative distribution function
//'
//' @references
//' Rigby, R. A. and Stasinopoulos D. M. (2005). Generalized additive models for 
//' location, scale and shape,(with discussion), Appl. Statist., 54, part 3, pp 507-554.
//' 
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R, Chapman and Hall/CRC.
//' 
//' Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location 
//' scale and shape (GAMLSS) in R. Journal of Statistical Software, Vol. 23, Issue 7, Dec 2007.
//'
//' @author Chris Kypridemos (optimised implementation), based on original work by 
//' Bob Rigby and Mikis Stasinopoulos from gamlss.dist package
//'
//' @seealso \code{\link{fdDPO}}, \code{\link{fqDPO}}
//'
//' @examples
//' # Calculate CDF for single values
//' fpDPO(0:5, mu = 2, sigma = 1)
//' 
//' # Calculate upper tail probabilities
//' fpDPO(0:5, mu = 2, sigma = 1, lower_tail = FALSE)
//' 
//' # Calculate log probabilities
//' fpDPO(0:5, mu = 2, sigma = 1, log_p = TRUE)
//' 
//' # Parameter recycling
//' fpDPO(c(0, 1, 2), mu = c(1, 2, 3), sigma = c(0.5, 1, 1.5))
//'
//' @export
// [[Rcpp::export]]
NumericVector fpDPO(const IntegerVector &q,
                      const NumericVector &mu,
                      const NumericVector &sigma,
                      const bool &lower_tail = true,
                      const bool &log_p = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(q, mu, sigma);
  const int n = recycled.n;
  
  NumericVector cdf(n);

  // Process with chunking for better cache performance
  const int chunk_size = 32;
  
  for (int chunk_start = 0; chunk_start < n; chunk_start += chunk_size) {
    // interrupt check every 2^10 elements (R-API call: wrapper only, never in the headers)
    if (chunk_start != 0 && (chunk_start & 0x3FF) == 0) Rcpp::checkUserInterrupt();
    int chunk_end = std::min(chunk_start + chunk_size, n);
    
    for (int i = chunk_start; i < chunk_end; i++) {
      // NaN/NA q -> NA: static_cast<int>(NaN) below is out-of-range float-to-int
      // UB, and a NaN q slips past the `< 0` check (NaN comparisons are false).
      if (ISNAN(recycled.vec1[i])) {
        cdf[i] = NA_REAL;
        continue;
      }
      if (recycled.vec1[i] < 0)
        stop("q must be >=0");
      if (recycled.vec2[i] <= 0.0)
        stop("mu must be greater than 0");
      if (recycled.vec3[i] <= 0.0)
        stop("sigma must be greater than 0");

      // NA/NaN or a count too large to convert to int -> NA. See count_to_int()
      // in recycling_helpers.h: the unguarded cast is out-of-range float-to-int
      // undefined behaviour and it is not benign on either x86-64 or AArch64.
      int q_i;
      if (!count_to_int(recycled.vec1[i], q_i)) {
        cdf[i] = NA_REAL;
        continue;
      }
      cdf[i] = fpDPO_scalar(q_i,
                           recycled.vec2[i], recycled.vec3[i], lower_tail, log_p);
    }
  }

  // Check for NAs
  if (any(is_na(cdf)))
    warning("NaNs or NAs were produced");
  return cdf;
}

//' Quantile Function for the DPO Distribution
//'
//' Computes quantiles of the DPO (Double Poisson) distribution, a discrete
//' distribution that extends the Poisson distribution with a dispersion parameter.
//'
//' @param p Vector of probabilities.
//' @param mu Vector of mu (location/mean) parameters (positive).
//' @param sigma Vector of sigma (dispersion) parameters (positive).
//' @param lower_tail Logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise P[X > x].
//' @param log_p Logical; if TRUE, probabilities p are given as log(p).
//' @param max_value Ignored; kept for compatibility. The search has no cap.
//'
//' @return Vector of quantiles corresponding to the given probabilities.
//'
//' @details
//' The DPO distribution is a two-parameter discrete distribution that reduces
//' to the Poisson distribution when sigma = 1. The quantile is the smallest
//' integer x with P(X <= x) >= p, found by adding the densities from the first
//' one that does not underflow, until the sum reaches p.
//'
//' \code{p = 1} (or above, up to 1.0001) gives \code{Inf}; any \code{p < 1}
//' has a finite quantile, including \code{p} within 1e-9 of 1
//' (\code{fqDPO(c(1 - 1e-9, 1 - 5e-10, 1 - 4e-11), 16.27, 7.53)} is 118 121 130).
//' A \code{p} so close to 1 that the summed CDF cannot reach it gives \code{NA}.
//'
//' \emph{Limit.} With a huge \code{mu} \emph{and} a huge \code{sigma} (for
//' example \code{mu = 3e9}, \code{sigma = 4e6}) the normalising constant takes
//' about 50 s to give up, and the answer is \code{NA}: see \code{\link{fdDPO}}.
//'
//' Parameter recycling is performed automatically - all parameter vectors
//' are recycled to the length of the longest vector.
//'
//' @section Parameter Validation:
//' - \code{p} must be a probability (a log probability if \code{log_p = TRUE});
//'   a value above 1 by up to 1e-4 is treated as 1, anything outside stops
//'   with an error
//' - \code{mu} and \code{sigma} must be positive, otherwise the function stops
//'   with an error
//' - an \code{NA} or \code{NaN} argument gives \code{NA}; any \code{NA} in the
//'   result gives a warning
//'
//' @note
//' This function is based on the DPO distribution implementation from
//' the \pkg{gamlss.dist} package by Mikis Stasinopoulos, Robert Rigby,
//' and colleagues. The original gamlss.dist implementation is acknowledged
//' with gratitude.
//'
//' @references
//' Rigby, R. A. and Stasinopoulos D. M. (2005). Generalized additive models
//' for location, scale and shape,(with discussion), \emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.
//'
//' Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
//' \emph{Flexible Regression and Smoothing: Using GAMLSS in R}, Chapman and Hall/CRC.
//'
//' Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location
//' scale and shape (GAMLSS) in R. \emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007.
//'
//' @author Chris Kypridemos [aut, cre], based on gamlss.dist by Mikis Stasinopoulos,
//' Robert Rigby, and colleagues
//'
//' @seealso \code{\link{fdDPO}}, \code{\link{fpDPO}}
//'
//' @examples
//' # Basic quantile computation
//' fqDPO(c(0.25, 0.5, 0.75), mu=5, sigma=1)
//'
//' # With parameter recycling
//' fqDPO(0.5, mu=c(1,5,10), sigma=c(0.5,1,2))
//'
//' # Using log probabilities
//' fqDPO(log(c(0.25, 0.5, 0.75)), mu=5, sigma=1, log_p=TRUE)
//'
//' # Upper tail probabilities
//' fqDPO(c(0.25, 0.5, 0.75), mu=5, sigma=1, lower_tail=FALSE)
//'
//' @export
// [[Rcpp::export]]
NumericVector fqDPO(NumericVector p,
                      const NumericVector &mu,
                      const NumericVector &sigma,
                      const bool &lower_tail = true,
                      const bool &log_p = false,
                      const int &max_value = 0)
{
  (void)max_value; // Unused parameter kept for API compatibility
  
  // Recycle vectors to common length
  auto recycled = recycle_vectors(p, mu, sigma);
  const int n = recycled.n;
  
  NumericVector QQQ(n);
  
  // Process in chunks for better performance
  const int chunk_size = 16;
  
  for (int chunk_start = 0; chunk_start < n; chunk_start += chunk_size) {
    int chunk_end = std::min(chunk_start + chunk_size, n);
    
    for (int i = chunk_start; i < chunk_end; i++) {
      double p_i = recycled.vec1[i];

      // NaN/NA in any argument -> NA quantile. Without this, a NaN p slips past
      // the [0,1] range checks below (every NaN comparison is false) and reaches
      // fqDPO_search -> static_cast<int>(R::qpois(NaN,...)), out-of-range
      // float-to-int UB.
      if (ISNAN(p_i) || ISNAN(recycled.vec2[i]) || ISNAN(recycled.vec3[i])) {
        QQQ[i] = NA_REAL;
        continue;
      }

      if (recycled.vec2[i] <= 0.0)
        stop("mu must be greater than 0");
      if (recycled.vec3[i] <= 0.0)
        stop("sigma must be greater than 0");

      // Apply transformations
      if (log_p)
        p_i = exp(p_i);
      if (p_i < 0.0 || p_i > 1.0001)
        stop("p must be between 0 and 1");
      if (!lower_tail)
        p_i = 1.0 - p_i;

      // p = 1 (or above it, within the 1.0001 tolerance) has an infinite
      // quantile; any p < 1 is searched, and its quantile is finite. The
      // `p + 1e-09 >= 1` cutoff of gamlss.dist::qDPO guards its R loop (max.value
      // iterations, a pDPO call each) and is deliberately not copied: the
      // search below needs no such guard, and with it every p in [1 - 1e-9, 1)
      // came out as Inf. A p so close to 1 that the summed CDF cannot reach it
      // is settled or NA in fqDPO_search (ck_search_stalled / ck_search_settled).
      if (p_i >= 1.0) {
        QQQ[i] = R_PosInf;
      } else {
        // Use optimized incremental search
        const double mu_val = recycled.vec2[i];
        const double sigma_val = recycled.vec3[i];
        
        // fqDPO_search reports "not representable" as NA_INTEGER; assigning
        // that int straight into a NumericVector would store INT_MIN as a
        // finite -2147483648 rather than NA.
        const int q_i = fqDPO_search(p_i, mu_val, sigma_val);
        QQQ[i] = (q_i == NA_INTEGER) ? NA_REAL : static_cast<double>(q_i);
      }
    }
  }
  
  if (any(is_na(QQQ)))
    warning("NaNs or NAs were produced");
  return QQQ;
}
