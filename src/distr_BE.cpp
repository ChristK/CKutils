/* CKutils: an R package with some utility functions I use regularly
Copyright (C) 2026  Chris Kypridemos

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
#include "distr_BE.h"   // canonical header-only scalar definitions
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// Beta distribution (gamlss.dist's BE parametrisation)
// BE *_scalar definitions live (inline) in inst/include/distr_BE.h, with the
// order in which the shapes are evaluated and why. BE is continuous: x, q and the
// quantiles are doubles, so none of the count handling of the ZINBI wrappers is here.

// Base R's d/p/q functions warn "NaNs produced" once per call when a NaN comes
// out of inputs that are not NaN. So does this (as in distr_ZAGA.cpp). An NA/NaN
// input gives NA silently. Called after the compute loop, which carries a SIMD
// hint and so must not write a shared flag.
static inline void warn_if_nan_produced_BE(const NumericVector& out,
                                           const RecycledVectors3& r)
{
  for (int i = 0; i < r.n; i++)
  {
    if (ISNAN(out[i]) && !ISNAN(r.vec1[i]) && !ISNAN(r.vec2[i]) &&
        !ISNAN(r.vec3[i]))
    {
      warning("NaNs produced");
      return;
    }
  }
}

// The parameter checks, after recycling: 0 < mu < 1, 0 < sigma < 1. An element
// with an NA/NaN argument is not checked (every NaN comparison is false) and
// gives NA in the compute loop.
static inline void check_BE_params(const RecycledVectors3& r)
{
  for (int i = 0; i < r.n; i++)
  {
    if (ISNAN(r.vec1[i]) || ISNAN(r.vec2[i]) || ISNAN(r.vec3[i])) continue;
    if (r.vec2[i] <= 0.0 || r.vec2[i] >= 1.0) stop("mu must be between 0 and 1");
    if (r.vec3[i] <= 0.0 || r.vec3[i] >= 1.0) stop("sigma must be between 0 and 1");
  }
}


//' Beta Distribution Density (BE parametrisation)
//'
//' Probability density function of the beta distribution with mean \code{mu}
//' and dispersion \code{sigma}, as gamlss.dist's \code{BE()} parametrises it.
//'
//' @param x vector of quantiles.
//' @param mu vector of means (0 < mu < 1).
//' @param sigma vector of dispersions (0 < sigma < 1).
//' @param log logical; if TRUE, the density is given as log(f).
//'
//' @details
//' The beta distribution with shapes
//' \eqn{\alpha = \mu k}{shape1 = mu * k} and
//' \eqn{\beta = (1-\mu) k}{shape2 = (1 - mu) * k}, where
//' \eqn{k = (1-\sigma^2)/\sigma^2}{k = (1 - sigma^2) / sigma^2}, has mean
//' \eqn{\mu}{mu} and variance \eqn{\sigma^2\mu(1-\mu)}{sigma^2 * mu * (1 - mu)}.
//' The shapes are computed in that order, one operator at a time, and passed
//' to R's own \code{\link[stats]{dbeta}}, so \code{fdBE(x, mu, sigma)} is
//' identical to \code{dbeta(x, mu * k, (1 - mu) * k)} with
//' \code{k <- (1 - sigma^2) / sigma^2} in R.
//'
//' Where it differs from \code{\link[gamlss.dist]{dBE}}: below 0 and above 1
//' the density is 0 (\code{-Inf} on the log scale, where \code{dBE} gives 0);
//' at \code{x = 0} or \code{1} it is the beta density's value there (\code{Inf}
//' when the shape at that end is below 1; \code{dBE} gives 0); an \code{NA} or
//' \code{NaN} argument gives \code{NA} (\code{dBE} stops on an \code{NA}
//' parameter); and gamlss.dist evaluates the shapes in another order
//' (\code{a = mu * (1 - sigma^2) / (sigma^2)}, \code{b = a * (1 - mu) / mu}),
//' which differs by at most 2 ulp in a shape.
//'
//' Arguments are recycled to a common length (a zero-length argument gives a
//' zero-length result). \code{mu} and \code{sigma} are checked after recycling
//' and must lie strictly between 0 and 1; an element with an \code{NA} or
//' \code{NaN} argument is not checked.
//'
//' @return A numeric vector of densities.
//'
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @examples
//' fdBE(c(0.2, 0.5, 0.8), mu = 0.6, sigma = 0.3)
//' # the same as stats::dbeta with the BE shapes
//' k <- (1 - 0.3^2) / 0.3^2
//' dbeta(c(0.2, 0.5, 0.8), 0.6 * k, (1 - 0.6) * k)
//'
//' @seealso \code{\link{fpBE}}, \code{\link{fqBE}}, \code{\link{frBE}} for
//' the other BE functions.
//' @export
// [[Rcpp::export]]
NumericVector fdBE(const NumericVector& x,
                   const NumericVector& mu,
                   const NumericVector& sigma,
                   const bool& log = false)
{
  auto recycled = recycle_vectors(x, mu, sigma);
  const int n = recycled.n;
  check_BE_params(recycled);

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = fdBE_scalar(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i], log);
  }

  warn_if_nan_produced_BE(out, recycled);
  return out;
}


//' Beta Distribution CDF (BE parametrisation)
//'
//' Cumulative distribution function of the beta distribution with mean
//' \code{mu} and dispersion \code{sigma}, as gamlss.dist's \code{BE()}
//' parametrises it.
//'
//' @param q vector of quantiles.
//' @param mu vector of means (0 < mu < 1).
//' @param sigma vector of dispersions (0 < sigma < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' \code{fpBE(q, mu, sigma, lower_tail, log_p)} is identical to
//' \code{pbeta(q, mu * k, (1 - mu) * k, lower.tail = lower_tail, log.p = log_p)}
//' with \code{k <- (1 - sigma^2) / sigma^2} in R (see \code{\link{fdBE}} for the
//' shapes). So the upper tail is \code{pbeta}'s direct upper tail, never
//' \code{1 - F}, and a \code{q} below 0 or at or above 1 is on the requested tail
//' and scale (\code{\link[gamlss.dist]{pBE}} sets 0 below 0 and 1 from 1 on,
//' whatever the tail and scale).
//'
//' An \code{NA} or \code{NaN} argument gives \code{NA}; \code{mu} and
//' \code{sigma} must lie strictly between 0 and 1 (checked after recycling).
//'
//' @return A numeric vector of cumulative probabilities.
//'
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @examples
//' fpBE(c(0.2, 0.5, 0.8), mu = 0.6, sigma = 0.3)
//' fpBE(0.5, mu = NA, sigma = 0.3) # NA, where gamlss.dist::pBE() stops
//'
//' @seealso \code{\link{fdBE}}, \code{\link{fqBE}}, \code{\link{frBE}} for
//' the other BE functions.
//' @export
// [[Rcpp::export]]
NumericVector fpBE(const NumericVector& q,
                   const NumericVector& mu,
                   const NumericVector& sigma,
                   const bool& lower_tail = true,
                   const bool& log_p = false)
{
  auto recycled = recycle_vectors(q, mu, sigma);
  const int n = recycled.n;
  check_BE_params(recycled);

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = fpBE_scalar(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i],
                         lower_tail, log_p);
  }

  warn_if_nan_produced_BE(out, recycled);
  return out;
}


//' Beta Distribution Quantile Function (BE parametrisation)
//'
//' Quantile function of the beta distribution with mean \code{mu} and
//' dispersion \code{sigma}, as gamlss.dist's \code{BE()} parametrises it.
//'
//' @param p vector of probabilities.
//' @param mu vector of means (0 < mu < 1).
//' @param sigma vector of dispersions (0 < sigma < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' \code{fqBE(p, mu, sigma, lower_tail, log_p)} is identical to
//' \code{qbeta(p, mu * k, (1 - mu) * k, lower.tail = lower_tail, log.p = log_p)}
//' with \code{k <- (1 - sigma^2) / sigma^2} in R (see \code{\link{fdBE}} for the
//' shapes), the beta that IMPACTncdEngland2026 drew with from its own
//' \code{R/beta_dist.R} until it moved here: a model switching keeps its draws
//' bit for bit.
//'
//' Where it differs from \code{\link[gamlss.dist]{qBE}}: an \code{NA} or
//' \code{NaN} argument gives \code{NA} (\code{qBE} stops on an \code{NA}
//' parameter, so an exposure table can be read on rows outside its grid); the
//' upper tail is \code{qbeta}'s (\code{qBE(lower.tail = FALSE)} returns the
//' lower-tail quantile in gamlss.dist 6.1.11); \code{p} is not snapped
//' (\code{qBE} returns 0 below \code{p = 1e-10} and 1 above
//' \code{1 - 1e-10}); and the shapes are evaluated in another order (at most
//' 2 ulp apart, a quantile up to 2.3e-15 where \code{mu} and \code{sigma} lie
//' in (0.05, 0.95), more where a shape is small). A \code{p} outside [0, 1] (above 0
//' on the log scale) is an error.
//'
//' Limits, R's own (\code{qbeta} and \code{qBE} share them), all where the
//' smaller shape is below 1e-2 (\code{sigma} near 1 with \code{mu} near 0 or
//' 1): \code{qbeta} can warn that full precision may not have been achieved (not
//' on every such row), and a quantile on the natural scale can be exactly 0 or 1
//' (the other tail or the log scale reaches it) or a value slightly outside
//' [0, 1] (about 1e-13 for a smaller shape of 1e-3 to 1e-2, up to about 1e-11
//' below 1e-4), most often without the warning.
//'
//' @return A numeric vector of quantiles: in [0, 1], up to the rounding of
//' \code{qbeta} described under Limits.
//'
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @examples
//' fqBE(c(0.1, 0.5, 0.9), mu = 0.6, sigma = 0.3)
//' fpBE(fqBE(c(0.1, 0.5, 0.9), 0.6, 0.3), 0.6, 0.3)
//' fqBE(0.5, NA, 0.3) # NA, where gamlss.dist::qBE() stops
//'
//' @seealso \code{\link{fdBE}}, \code{\link{fpBE}}, \code{\link{frBE}} for
//' the other BE functions.
//' @export
// [[Rcpp::export]]
NumericVector fqBE(const NumericVector& p,
                   const NumericVector& mu,
                   const NumericVector& sigma,
                   const bool& lower_tail = true,
                   const bool& log_p = false)
{
  auto recycled = recycle_vectors(p, mu, sigma);
  const int n = recycled.n;
  for (int i = 0; i < n; i++)
  {
    if (ISNAN(recycled.vec1[i]) || ISNAN(recycled.vec2[i]) ||
        ISNAN(recycled.vec3[i])) continue;
    check_prob(recycled.vec1[i], log_p, 1.0, "p must be >=0 and <=1");
  }
  check_BE_params(recycled);

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    out[i] = fqBE_scalar(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i],
                         lower_tail, log_p);
  }

  warn_if_nan_produced_BE(out, recycled);
  return out;
}

// NOTE: there is deliberately no vectorised, Rcpp-exported frBE() here. The
// exported frBE() is the R implementation in R/rng_distr.R, which draws from
// dqrng::dqrunif() and inverts fqBE() (as frZAGA; see the note at the end of
// src/distr_ZINBI.cpp for why a same-named C++ export is avoided). The
// per-element frBE_scalar() is available (inline) in inst/include/distr_BE.h for
// a LinkingTo: CKutils consumer, takes a caller-supplied uniform, and agrees with
// frBE() draw for draw under one dqset.seed().
