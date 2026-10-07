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
#include "distr_DEL.h"   // header-only scalar definitions (fdPO_scalar, ftofydel2_scalar, fdDEL_scalar, fpDEL_hlp_fn, fpDEL_scalar)
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

// fdPO_scalar and ftofydel2_scalar definitions now live (inline) in inst/include/distr_DEL.h



//' The Delaporte Distribution - Density Function
//'
//' Density function for the Delaporte distribution with parameters mu, sigma and nu.
//' The Delaporte distribution is a discrete probability distribution: the sum of
//' a Poisson variable and an independent negative binomial variable.
//'
//' @param x vector of (non-negative integer) quantiles. A non-integer is truncated to an integer; a count above 2147483646 gives \code{NA}.
//' @param mu vector of positive means
//' @param sigma vector of positive dispersion parameters
//' @param nu vector of parameters between 0 and 1
//' @param log_ logical; if TRUE, probabilities p are given as log(p)
//'
//' @details
//' The Delaporte distribution is the convolution of a Poisson distribution
//' with mean \eqn{\mu\nu} and a negative binomial distribution with size
//' \eqn{1/\sigma} and mean \eqn{\mu(1-\nu)}:
//' \deqn{P(X = x) = \sum_{k=0}^{x} \frac{e^{-\mu\nu}(\mu\nu)^k}{k!}
//'   \frac{\Gamma(x-k+1/\sigma)}{\Gamma(x-k+1)\Gamma(1/\sigma)}
//'   \left(\frac{1}{1+\mu\sigma(1-\nu)}\right)^{1/\sigma}
//'   \left(\frac{\mu\sigma(1-\nu)}{1+\mu\sigma(1-\nu)}\right)^{x-k}}
//' for x = 0, 1, 2, ..., mu > 0, sigma > 0, and 0 < nu < 1.
//' 
//' The mean is mu and the variance is mu + mu^2 * sigma * (1 - nu)^2.
//' 
//' When sigma is below 1e-04 the density is that of a Poisson distribution with
//' mean mu. The density is computed by a recurrence over the counts 0 to x in
//' constant memory, so the cost is O(x): about 10 ms per million
//' (\code{fdDEL(9.2e7, 1e8, 0.5, 0.5)} takes 0.9 s). In a vector call the
//' recurrence continues from one element to the next while mu, sigma and nu repeat
//' and x does not decrease.
//' 
//' This implementation is based on the algorithms from the gamlss.dist package
//' by Rigby, R. A. and Stasinopoulos D. M.
//'
//' @return
//' \code{fdDEL} gives the density
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
//' @seealso \code{\link{fpDEL}}, \code{\link{fqDEL}}
//'
//' @examples
//' # Calculate density for single values
//' fdDEL(0:5, mu = 2, sigma = 1, nu = 0.5)
//' 
//' # Calculate log density
//' fdDEL(0:5, mu = 2, sigma = 1, nu = 0.5, log = TRUE)
//' 
//' # Parameter recycling
//' fdDEL(c(0, 1, 2), mu = c(1, 2, 3), sigma = c(0.5, 1, 1.5), nu = c(0.3, 0.5, 0.7))
//'
//' @export
// [[Rcpp::export]]
NumericVector fdDEL(const IntegerVector &x,
                      const NumericVector &mu,
                      const NumericVector &sigma,
                      const NumericVector &nu,
                      const bool &log_ = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(x, mu, sigma, nu);
  const int n = recycled.n;
  
  NumericVector logfy(n);

  // The recurrence of the last DEL element. The next element continues it
  // when it has the same (mu, sigma, nu) and an x at or beyond its index, so
  // fdDEL(0:n, mu, sigma, nu) is O(n) rather than O(n^2); the values are the
  // same doubles either way. An x below CK_DEL_COMPENSATE_AFTER uses
  // CkDELRecurrence (as ever, bit for bit), an x at or above it
  // CkDELAccurate (distr_DEL.h, ACCURACY); each is kept with its own
  // parameters, so one kind of element does not discard the other's state.
  bool have_r = false, have_a = false;
  CkDELRecurrence r(1.0, 1.0, 0.5);
  CkDELAccurate a;
  double r_mu = 0.0, r_sigma = 0.0, r_nu = 0.0;
  double a_mu = 0.0, a_sigma = 0.0, a_nu = 0.0;

  // Process in chunks for better cache performance
  const int chunk_size = 16;

  for (int chunk_start = 0; chunk_start < n; chunk_start += chunk_size) {
    int chunk_end = std::min(chunk_start + chunk_size, n);
    
    // Prefetch data for this chunk
    #ifdef __builtin_prefetch
    if (chunk_end < n) {
      __builtin_prefetch(&recycled.vec1[chunk_end], 0, 3);
      __builtin_prefetch(&recycled.vec2[chunk_end], 0, 3);
      __builtin_prefetch(&recycled.vec3[chunk_end], 0, 3);
      __builtin_prefetch(&recycled.vec4[chunk_end], 0, 3);
    }
    #endif
    
    for (int i = chunk_start; i < chunk_end; i++) {
      // NaN/NA x -> NA: static_cast<int>(NaN) below is out-of-range float-to-int
      // UB, and a NaN x slips past the `< 0` check (NaN comparisons are false).
      if (ISNAN(recycled.vec1[i])) {
        logfy[i] = NA_REAL;
        continue;
      }
      if (recycled.vec1[i] < 0.0)
        stop("x must be >=0");
      if (recycled.vec2[i] <= 0.0)
        stop("mu must be greater than 0");
      if (recycled.vec3[i] <= 0.0)
        stop("sigma must be greater than 0");
      if (recycled.vec4[i] <= 0.0 || recycled.vec4[i] >= 1.0)
        stop("nu must be between 0 and 1");

      // NA/NaN or a count too large to convert to int -> NA. See count_to_int()
      // in recycling_helpers.h: the unguarded cast is out-of-range float-to-int
      // undefined behaviour and it is not benign on either x86-64 or AArch64.
      int x_i;
      if (!count_to_int(recycled.vec1[i], x_i)) {
        logfy[i] = NA_REAL;
        continue;
      }

      if (recycled.vec3[i] < 1e-04) {
        logfy[i] = R::dpois(x_i, recycled.vec2[i], (int)log_);
      } else {
        const double mu_val = recycled.vec2[i];
        const double sigma_val = recycled.vec3[i];
        const double nu_val = recycled.vec4[i];
        if (x_i < CK_DEL_COMPENSATE_AFTER) {
          if (!(have_r && mu_val == r_mu && sigma_val == r_sigma &&
                nu_val == r_nu && r.j <= x_i)) {
            r = CkDELRecurrence(mu_val, sigma_val, nu_val);
            r_mu = mu_val; r_sigma = sigma_val; r_nu = nu_val;
            have_r = true;
          }
          while (r.j < x_i) r.advance();   // r.j == x_i
          logfy[i] = r.log_density();
        } else {
          if (!(have_a && mu_val == a_mu && sigma_val == a_sigma &&
                nu_val == a_nu && a.j <= x_i)) {
            a = CkDELAccurate(mu_val, sigma_val, nu_val);
            a_mu = mu_val; a_sigma = sigma_val; a_nu = nu_val;
            have_a = true;
          }
          while (a.j < x_i) a.advance();   // a.j == x_i
          logfy[i] = a.log_density();
        }
        if (!log_)
          logfy[i] = exp(logfy[i]);
      }
    }
  }

  // Check for NAs
  if (any(is_na(logfy)))
    warning("NaNs or NAs were produced");
  return logfy;
}

// fdDEL_scalar and fpDEL_hlp_fn definitions now live (inline) in inst/include/distr_DEL.h

//' The Delaporte Distribution - Cumulative Distribution Function
//'
//' Distribution function for the Delaporte distribution with parameters mu, sigma and nu.
//' Computes the cumulative distribution function (CDF) of the Delaporte distribution.
//'
//' @param q vector of (non-negative integer) quantiles. A non-integer is truncated to an integer; a count above 2147483646 gives \code{NA}.
//' @param mu vector of positive means
//' @param sigma vector of positive dispersion parameters
//' @param nu vector of parameters between 0 and 1
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x], otherwise, P[X > x]
//' @param log_p logical; if TRUE, probabilities p are given as log(p)
//'
//' @details
//' The cumulative distribution function is computed as the sum of the probability
//' mass function from 0 to q, by the recurrence of \code{\link{fdDEL}}, so the
//' cost is O(q): about 12 ms per million counts (\code{fpDEL(9.2e7, 1e8, 0.5, 0.5)}
//' takes 1.1 s). In a vector call, \code{fpDEL(0:q, ...)} with repeated
//' parameters costs O(q), not O(q^2). The result never exceeds 1.
//' 
//' When sigma is very small (< 1e-04), the distribution is treated as a Poisson
//' distribution with mean mu.
//' 
//' \emph{Accuracy.} Up to count 4095 the plain recurrence is used. Against the
//' exact sum of the Poisson and negative binomial convolution
//' (\eqn{\sum_k f_{Pois}(k) F_{NB}(q-k)}) the largest error over five synthetic
//' parameter sets (mu about q, sigma 0.1 to 2, nu 0.2 to 0.9) was 1e-15 at q = 10,
//' 4e-14 at 50, 4e-13 at 200, 9e-12 at 1000 and 5e-11 at 4095. From count 4096
//' on a compensated recurrence takes over, and the same check gave errors below
//' 3e-14 at q = 4096, 5000, 20000, 65535 and 200000; over a wider set of
//' parameters the error is below 6e-13 for counts 4096 to 65535 and below 5e-12
//' up to 2e9.
//' 
//' This implementation is based on the algorithms from the gamlss.dist package
//' by Rigby, R. A. and Stasinopoulos D. M.
//'
//' @return
//' \code{fpDEL} gives the cumulative distribution function
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
//' @seealso \code{\link{fdDEL}}, \code{\link{fqDEL}}
//'
//' @examples
//' # Calculate CDF for single values
//' fpDEL(0:5, mu = 2, sigma = 1, nu = 0.5)
//' 
//' # Calculate upper tail probabilities
//' fpDEL(0:5, mu = 2, sigma = 1, nu = 0.5, lower_tail = FALSE)
//' 
//' # Calculate log probabilities
//' fpDEL(0:5, mu = 2, sigma = 1, nu = 0.5, log_p = TRUE)
//' 
//' # Parameter recycling
//' fpDEL(c(0, 1, 2), mu = c(1, 2, 3), sigma = c(0.5, 1, 1.5), nu = c(0.3, 0.5, 0.7))
//'
//' @export
// [[Rcpp::export]]
NumericVector fpDEL(const IntegerVector &q,
                      const NumericVector &mu,
                      const NumericVector &sigma,
                      const NumericVector &nu,
                      const bool &lower_tail = true,
                      const bool &log_p = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(q, mu, sigma, nu);
  const int n = recycled.n;
  
  NumericVector cdf(n);

  // The running CDF of the last element's parameter set. The next element
  // continues it when it has the same (mu, sigma, nu) and a q at or beyond
  // the last index added, so fpDEL(0:n, mu, sigma, nu) is O(n) rather than
  // O(n^2); the same terms are added in the same order, so the values are the
  // same doubles as fpDEL_hlp_fn's (it is the same code, CkDELCdf).
  bool have_c = false;
  double c_mu = 0.0, c_sigma = 0.0, c_nu = 0.0;
  CkDELCdf run(1.0, 1.0, 0.5);

  // Process with chunking for better performance
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
      if (recycled.vec4[i] <= 0.0 || recycled.vec4[i] >= 1)
        stop("nu must be between 0 and 1");

      // NA/NaN or a count too large to convert to int -> NA. See count_to_int()
      // in recycling_helpers.h: the unguarded cast is out-of-range float-to-int
      // undefined behaviour and it is not benign on either x86-64 or AArch64.
      int q_i;
      if (!count_to_int(recycled.vec1[i], q_i)) {
        cdf[i] = NA_REAL;
        continue;
      }
      const double mu_val = recycled.vec2[i];
      const double sigma_val = recycled.vec3[i];
      const double nu_val = recycled.vec4[i];
      if (!(have_c && mu_val == c_mu && sigma_val == c_sigma &&
            nu_val == c_nu && run.q <= q_i)) {
        run.reset(mu_val, sigma_val, nu_val);
        c_mu = mu_val; c_sigma = sigma_val; c_nu = nu_val; have_c = true;
      }
      // clamp the value returned, never the running sum (`run` is extended next)
      cdf[i] = std::min(run.advance_to(q_i), 1.0);
    }
  }

  // Vectorized transformations when possible
  if (!lower_tail || log_p) {
    #ifdef __AVX2__
    if (has_avx2() && n >= 4) {
      const int simd_chunks = n / 4;
      
      for (int chunk = 0; chunk < simd_chunks; chunk++) {
        int base_idx = chunk * 4;
        
        if (!lower_tail && log_p) {
          // Both transformations: log(1 - cdf)
          alignas(32) double cdf_vals[4];
          alignas(32) double result_vals[4];
          
          for (int k = 0; k < 4; k++) {
            cdf_vals[k] = cdf[base_idx + k];
          }
          
          __m256d cdf_vec = _mm256_load_pd(cdf_vals);
          __m256d one_vec = _mm256_set1_pd(1.0);
          __m256d complement = _mm256_sub_pd(one_vec, cdf_vec);
          
          _mm256_store_pd(result_vals, complement);
          for (int k = 0; k < 4; k++) {
            result_vals[k] = log(result_vals[k]);
          }
          
          for (int k = 0; k < 4; k++) {
            cdf[base_idx + k] = result_vals[k];
          }
        } else if (!lower_tail) {
          // Only complement: 1 - cdf
          alignas(32) double cdf_vals[4];
          alignas(32) double result_vals[4];
          
          for (int k = 0; k < 4; k++) {
            cdf_vals[k] = cdf[base_idx + k];
          }
          
          __m256d cdf_vec = _mm256_load_pd(cdf_vals);
          __m256d one_vec = _mm256_set1_pd(1.0);
          __m256d complement = _mm256_sub_pd(one_vec, cdf_vec);
          
          _mm256_store_pd(result_vals, complement);
          for (int k = 0; k < 4; k++) {
            cdf[base_idx + k] = result_vals[k];
          }
        } else if (log_p) {
          // Only log: log(cdf)
          for (int k = 0; k < 4; k++) {
            cdf[base_idx + k] = log(cdf[base_idx + k]);
          }
        }
      }
      
      // Handle remaining elements
      for (int i = simd_chunks * 4; i < n; i++) {
        if (!lower_tail && log_p) {
          cdf[i] = log(1.0 - cdf[i]);
        } else if (!lower_tail) {
          cdf[i] = 1.0 - cdf[i];
        } else if (log_p) {
          cdf[i] = log(cdf[i]);
        }
      }
    } else
    #endif
    {
      // Scalar fallback
      for (int i = 0; i < n; i++) {
        if (!lower_tail && log_p) {
          cdf[i] = log(1.0 - cdf[i]);
        } else if (!lower_tail) {
          cdf[i] = 1.0 - cdf[i];
        } else if (log_p) {
          cdf[i] = log(cdf[i]);
        }
      }
    }
  }

  if (any(is_na(cdf)))
    warning("NaNs or NAs were produced");
  return cdf;
}

// fpDEL_scalar definition now lives (inline) in inst/include/distr_DEL.h

// Optimized quantile search using incremental CDF computation
// This avoids recalculating CDF from scratch for each candidate
int fqDEL_search(const double &p,
                 const double &mu,
                 const double &sigma,
                 const double &nu)
{
  // NaN/NA guard: the vector wrapper (fqDEL) already maps NaN args to NA, but
  // guard here too so R::qpois(NaN,...) and the static_cast<int> below can never
  // see NaN, which would be out-of-range float-to-int undefined behaviour.
  if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma) || ISNAN(nu)) {
    return NA_INTEGER;
  }
  // For very small sigma, use Poisson distribution directly. R::qpois returns
  // a double that exceeds INT_MAX for a large mu, so route it through the same
  // guard as the count arguments rather than casting blind (an unguarded cast
  // is out-of-range UB and yields INT_MIN, i.e. a negative quantile).
  if (sigma < 1e-04) {
    int q_i;
    return count_to_int(R::qpois(p, mu, true, false), q_i) ? q_i : NA_INTEGER;
  }
  
  // Incremental search: sum densities until CDF >= p, one recurrence step per
  // index (O(q); the cumulative sums are fpDEL's, bit for bit). No fixed cap:
  // a non-finite density, or a stalled sum (ck_search_stalled, distr_search.h)
  // that is not within CK_P_FUZZ of p (ck_search_settled), ends a search that
  // cannot reach p, and NA_INTEGER is returned rather than a number; a stalled
  // sum within it returns the index where it stopped.
  // The first CK_DEL_COMPENSATE_AFTER indices run on CkDELRecurrence, as ever,
  // bit for bit. A search that has not reached p by then starts again from 0
  // on CkDELAccurate (distr_DEL.h, ACCURACY), whose terms are those of
  // CkDELCdf beyond that index: fqDEL(fpDEL(q)) == q there too.
  // Interruptible: Rcpp::checkUserInterrupt() every 2^20 terms (a scan can run for
  // tens of seconds). Only src/ loops do this: the inst/include kernels (fqBNB_search,
  // the DPO normalising-constant loop, fcdfSICHEL_scalar, CkDELCdf) are a LinkingTo API
  // that a consumer may call from threads where the R API must not be used, so they
  // stay uninterruptible.
  unsigned int steps = 0;
  {
    CkDELRecurrence r(mu, sigma, nu);
    double cdf = 0.0;
    double prev_density = -1.0;
    for (;;) {
      if ((++steps & 0xFFFFF) == 0) Rcpp::checkUserInterrupt();
      const double density = exp(r.log_density());
      if (!std::isfinite(density)) return NA_INTEGER;
      if (ck_search_stalled(density, prev_density, cdf)) {
        // the mass has settled before index 4096: no restart on phase 2
        return ck_search_settled(cdf, p) ? r.j - 1 : NA_INTEGER;
      }
      cdf += density;
      if (cdf >= p) {
        return r.j;
      }
      if (r.j + 1 == CK_DEL_COMPENSATE_AFTER) break;
      prev_density = density;
      r.advance();
    }
  }
  CkDELAccurate a(mu, sigma, nu);
  double cdf = 0.0;
  double prev_density = -1.0;
  for (;;) {
    if ((++steps & 0xFFFFF) == 0) Rcpp::checkUserInterrupt();
    const double density = a.density();
    if (!std::isfinite(density)) return NA_INTEGER;
    if (ck_search_stalled(density, prev_density, cdf)) {
      return ck_search_settled(cdf, p) ? a.j - 1 : NA_INTEGER;
    }
    cdf += density;
    if (cdf >= p) {
      return a.j;
    }
    if (a.j == CK_SEARCH_MAX) return NA_INTEGER;
    prev_density = density;
    a.advance();
  }
}

//' Quantile Function for the Delaporte Distribution
//'
//' Computes quantiles of the Delaporte distribution, a compound distribution of
//' Poisson and shifted negative binomial.
//'
//' @param p Vector of probabilities.
//' @param mu Vector of mu (location/mean) parameters (positive).
//' @param sigma Vector of sigma (scale) parameters (positive).
//' @param nu Vector of nu (shape) parameters, between 0 and 1.
//' @param lower_tail Logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise P[X > x].
//' @param log_p Logical; if TRUE, probabilities p are given as log(p).
//'
//' @return Vector of quantiles corresponding to the given probabilities.
//'
//' @details
//' The Delaporte distribution is a three-parameter discrete distribution
//' defined as the convolution of a Poisson distribution with mean
//' \code{mu * nu} and a negative binomial distribution with size
//' \code{1 / sigma} and mean \code{mu * (1 - nu)} (see \code{\link{fdDEL}}).
//'
//' The quantile is the smallest integer x with P(X <= x) >= p, found by adding
//' the probabilities from 0 upwards (the recurrence of \code{\link{fdDEL}}).
//' The cost is O(x): about 12 ms per million counts (a median near 9.2e7, at
//' mu = 1e8, takes 1.1 s). \code{p = 1} (or above, up to 1.0001) gives
//' \code{Inf}; any \code{p < 1} has a finite quantile, including \code{p}
//' within 1e-9 of 1 (\code{fqDEL(1 - 1e-9, 2.03065, 2.30919, 0.830551)} is 25).
//' A \code{p} so close to 1 that the summed CDF cannot reach it gives
//' \code{NA}. A quantile beyond 2147483646, the largest integer this function
//' returns, is \code{NA} too, but only after the scan has run to the end of the
//' range (there is no closed-form shortcut as for \code{\link{fqBNB}}). The
//' scan can be stopped with Ctrl-C. The accuracy of the CDF behind it is
//' described in \code{\link{fpDEL}}.
//'
//' Parameter recycling is performed automatically - all parameter vectors
//' are recycled to the length of the longest vector.
//'
//' @section Parameter Validation:
//' - \code{p} must be a probability (a log probability if \code{log_p = TRUE});
//'   a value above 1 by up to 1e-4 is treated as 1, anything outside stops
//'   with an error
//' - \code{mu} and \code{sigma} must be positive and \code{nu} between 0 and 1,
//'   otherwise the function stops with an error
//' - an \code{NA} or \code{NaN} argument gives \code{NA}; any \code{NA} in the
//'   result gives a warning
//'
//' @note
//' This function is based on the Delaporte distribution implementation from
//' the \pkg{gamlss.dist} package by Mikis Stasinopoulos, Robert Rigby,
//' Calliope Akantziliotou, Vlasios Voudouris, and Fernanda De Bastiani.
//' The original gamlss.dist implementation is acknowledged with gratitude.
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
//' @author Christos Kypraios [aut, cre], based on gamlss.dist by Mikis Stasinopoulos,
//' Robert Rigby, Calliope Akantziliotou, Vlasios Voudouris, Fernanda De Bastiani
//'
//' @seealso \code{\link{fdDEL}}, \code{\link{fpDEL}}
//'
//' @examples
//' # Basic quantile computation
//' fqDEL(c(0.25, 0.5, 0.75), mu=5, sigma=1, nu=0.2)
//'
//' # With parameter recycling
//' fqDEL(0.5, mu=c(1,5,10), sigma=c(0.5,1,2), nu=c(0.1,0.2,0.9))
//'
//' # Using log probabilities
//' fqDEL(log(c(0.25, 0.5, 0.75)), mu=5, sigma=1, nu=0.2, log_p=TRUE)
//'
//' # Upper tail probabilities
//' fqDEL(c(0.25, 0.5, 0.75), mu=5, sigma=1, nu=0.2, lower_tail=FALSE)
//'
//' @export
// [[Rcpp::export]]
NumericVector fqDEL(NumericVector p,
                      const NumericVector &mu,
                      const NumericVector &sigma,
                      const NumericVector &nu,
                      const bool &lower_tail = true,
                      const bool &log_p = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(p, mu, sigma, nu);
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
      // fqDEL_search -> static_cast<int>(R::qpois(NaN,...)), out-of-range
      // float-to-int UB.
      if (ISNAN(p_i) || ISNAN(recycled.vec2[i]) || ISNAN(recycled.vec3[i]) ||
          ISNAN(recycled.vec4[i])) {
        QQQ[i] = NA_REAL;
        continue;
      }

      if (recycled.vec2[i] <= 0.0)
        stop("mu must be greater than 0");
      if (recycled.vec3[i] <= 0.0)
        stop("sigma must be greater than 0");
      if (recycled.vec4[i] <= 0.0 || recycled.vec4[i] >= 1)
        stop("nu must be between 0 and 1");
      
      // Apply transformations in the same order as gamlss.dist
      if (log_p)
        p_i = exp(p_i);
      // NOTE gamlss.dist behavior has a bug with their bug with log.p validation
      if (p_i < 0.0 || p_i > 1.0001)
        stop("p must be between 0 and 1");

      if (!lower_tail)
        p_i = 1.0 - p_i;

      // p = 1 (or above it, within the 1.0001 tolerance) has an infinite
      // quantile; any p < 1 is searched, and its quantile is finite. The
      // `p + 1e-09 >= 1` cutoff of gamlss.dist::qDEL guards its R loop and is
      // deliberately not copied (as in fqDPO): a p so close to 1 that the summed
      // CDF cannot reach it is settled or NA in fqDEL_search.
      if (p_i >= 1.0) {
        QQQ[i] = R_PosInf;
      } else {
        // Use optimized incremental search
        const double mu_val = recycled.vec2[i];
        const double sigma_val = recycled.vec3[i];
        const double nu_val = recycled.vec4[i];
        
        // fqDEL_search reports "not representable" as NA_INTEGER; assigning
        // that int straight into a NumericVector would store INT_MIN as a
        // finite -2147483648 rather than NA.
        const int q_i = fqDEL_search(p_i, mu_val, sigma_val, nu_val);
        QQQ[i] = (q_i == NA_INTEGER) ? NA_REAL : static_cast<double>(q_i);
      }
    }
  }
  
  if (any(is_na(QQQ)))
    warning("NaNs or NAs were produced");
  return QQQ;
}
