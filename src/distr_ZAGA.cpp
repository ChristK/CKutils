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
#include "distr_ZAGA.h"   // canonical header-only scalar definitions
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// Zero-adjusted gamma functions
// ZAGA *_scalar definitions live (inline) in inst/include/distr_ZAGA.h.
// ZAGA is continuous: x, q and the quantiles are doubles, so none of the count
// handling (count_to_int, the integer quantiles) of the ZINBI wrappers is here.

// Base R's d/p/q functions warn "NaNs produced" once per call when a NaN comes
// out of inputs that are not NaN. So does this: an element whose four inputs are
// all non-NaN but whose result is NaN (sigma^2 underflowing to 0, a negative
// qgamma value mapped to NaN) gives one warning for the call. An NA/NaN input
// gives NA silently. Called after the compute loop, which carries a SIMD hint and
// so must not write a shared flag.
static inline void warn_if_nan_produced(const NumericVector& out,
                                        const RecycledVectors4& r)
{
  for (int i = 0; i < r.n; i++)
  {
    if (ISNAN(out[i]) && !ISNAN(r.vec1[i]) && !ISNAN(r.vec2[i]) &&
        !ISNAN(r.vec3[i]) && !ISNAN(r.vec4[i]))
    {
      warning("NaNs produced");
      return;
    }
  }
}


//' Zero-Adjusted Gamma Distribution Density
//'
//' Probability density function for the Zero-Adjusted Gamma (ZAGA) distribution
//' with parameters mu (mean of the gamma part), sigma (its coefficient of
//' variation) and nu (the probability of a zero).
//'
//' @param x vector of quantiles. A negative \code{x} has density 0.
//' @param mu vector of positive means of the gamma part.
//' @param sigma vector of positive coefficients of variation of the gamma part.
//' @param nu vector of zero probabilities (0 < nu < 1).
//' @param log logical; if TRUE, the density is given as log(f).
//'
//' @details
//' The ZAGA distribution is a mixture of a point mass at zero and a gamma
//' distribution on the positive reals. With probability \eqn{\nu}{nu} the value
//' is exactly zero; otherwise it is drawn from the gamma distribution with mean
//' \eqn{\mu}{mu} and coefficient of variation \eqn{\sigma}{sigma}, that is shape
//' \eqn{1/\sigma^2}{1/sigma^2} and scale \eqn{\mu\sigma^2}{mu * sigma^2}. Unlike
//' zero-inflation, the probability of a zero is exactly \eqn{\nu}{nu}. The
//' density (a probability at zero) is:
//' \deqn{f(0) = \nu}{f(0) = nu}
//' \deqn{f(y) = (1-\nu) f_{GA}(y|\mu,\sigma) \quad \text{for } y > 0}{f(y) = (1 - nu) f_GA(y | mu, sigma) for y > 0}
//' where \eqn{f_{GA}}{f_GA} is the gamma density. The mean is
//' \eqn{(1-\nu)\mu}{(1 - nu) * mu} and the variance
//' \eqn{(1-\nu)\mu^2(\sigma^2+\nu)}{(1 - nu) * mu^2 * (sigma^2 + nu)}.
//'
//' On the natural scale the density is \eqn{(1-\nu)}{(1 - nu)} times the gamma
//' density, except where that underflows or loses digits to subnormal
//' intermediates (a tiny scale \eqn{\mu\sigma^2}{mu * sigma^2}): below 1e-280 it
//' is the exponential of the log density, which keeps its digits and does not
//' underflow early. So \code{fdZAGA(1.004e-250, 1e-250, 1e-4, 0.3)} is 8.56e-94,
//' not 0.
//'
//' The parameters must satisfy \eqn{\mu > 0}{mu > 0}, \eqn{\sigma > 0}{sigma > 0}
//' and \eqn{0 < \nu < 1}{0 < nu < 1}, with \code{mu} and \code{sigma} finite (an
//' infinite one is an error). This is stricter than
//' \code{\link[gamlss.dist]{dZAGA}}, which accepts \code{nu} in \eqn{[0, 1]}
//' and lets \code{mu = 0} and \code{sigma = 0} through (giving \code{NaN}). A
//' \code{NaN} or \code{NA} in any argument gives \code{NA}. Where the four
//' arguments are all non-\code{NaN} but the result is \code{NaN} (for example
//' \code{sigma = 1e-200}, where \eqn{\sigma^2}{sigma^2} underflows to 0), there
//' is one warning \dQuote{NaNs produced} per call, as in base R. A
//' \eqn{\mu\sigma^2}{mu * sigma^2} below the normal range of doubles (subnormal)
//' loses accuracy, as it does in \code{dZAGA}.
//'
//' Where it differs from \code{\link[gamlss.dist]{dZAGA}}: the log density at a
//' negative \code{x} is \code{-Inf} (\code{dZAGA} gives 0), and the density at
//' \code{x = Inf} is 0 (\code{dZAGA} gives \code{NaN}). Unlike
//' \code{\link{fdZINBI}} and \code{\link{fdBCT}}, a negative \code{x} is not an
//' error.
//'
//' @return A numeric vector of density values.
//'
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Single values: the first is the probability of a zero
//' fdZAGA(c(0, 0.5, 1, 2), mu = 2, sigma = 0.5, nu = 0.1)
//'
//' # Vector inputs with recycling
//' fdZAGA(c(0, 1, 2, 3), mu = c(1, 2), sigma = 0.5, nu = 0.1)
//'
//' # Log density
//' fdZAGA(c(0, 1, 2), mu = 2, sigma = 0.5, nu = 0.1, log = TRUE)
//'
//' @seealso \code{\link{fpZAGA}}, \code{\link{fqZAGA}}, \code{\link{frZAGA}} for
//' the other ZAGA functions, and \code{\link[gamlss.dist]{ZAGA}} for the
//' original implementation.
//'
//' @export
// [[Rcpp::export]]
NumericVector fdZAGA(const NumericVector& x,
                     const NumericVector& mu,
                     const NumericVector& sigma,
                     const NumericVector& nu,
                     const bool& log = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(x, mu, sigma, nu);
  const int n = recycled.n;

  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    // NaN/NA in any argument is handled in the compute loop below (mapped to
    // NA); skip the range checks here because NaN comparisons are always false.
    // There is no check on x: every real x, negative ones included, is a valid
    // point of this distribution (density 0 below the support).
    if (ISNAN(recycled.vec1[i]) || ISNAN(recycled.vec2[i]) ||
        ISNAN(recycled.vec3[i]) || ISNAN(recycled.vec4[i])) continue;
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0 || recycled.vec4[i] >= 1.0) stop("nu must be between 0 and 1");
    // An infinite mu or sigma passes the range checks above but gives a
    // degenerate gamma (sigma = Inf returned a quantile of 0 for a p above nu),
    // so it is an error rather than a number.
    if (!R_FINITE(recycled.vec2[i])) stop("mu must be finite");
    if (!R_FINITE(recycled.vec3[i])) stop("sigma must be finite");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // fdZAGA_scalar maps NaN/NA in any argument to NA_REAL.
    out[i] = fdZAGA_scalar(recycled.vec1[i], recycled.vec2[i],
                           recycled.vec3[i], recycled.vec4[i], log);
  }

  warn_if_nan_produced(out, recycled);
  return out;
}


//' Zero-Adjusted Gamma Distribution CDF
//'
//' Cumulative distribution function for the Zero-Adjusted Gamma (ZAGA)
//' distribution with parameters mu (mean of the gamma part), sigma (its
//' coefficient of variation) and nu (the probability of a zero).
//'
//' @param q vector of quantiles. A negative \code{q} is below the support.
//' @param mu vector of positive means of the gamma part.
//' @param sigma vector of positive coefficients of variation of the gamma part.
//' @param nu vector of zero probabilities (0 < nu < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The cumulative distribution function of the ZAGA distribution is, for
//' \eqn{q \geq 0}{q >= 0}:
//' \deqn{F(q) = \nu + (1-\nu) F_{GA}(q|\mu,\sigma)}{F(q) = nu + (1 - nu) F_GA(q | mu, sigma)}
//' where \eqn{F_{GA}}{F_GA} is the gamma cumulative distribution function with
//' shape \eqn{1/\sigma^2}{1/sigma^2} and scale \eqn{\mu\sigma^2}{mu * sigma^2},
//' so \eqn{F(0) = \nu}{F(0) = nu} exactly (also where \eqn{\sigma^2}{sigma^2}
//' underflows to 0, which makes the gamma part undefined), and \eqn{F(q) = 0} for
//' \eqn{q < 0}.
//'
//' The upper tail, \eqn{P(Y > q) = (1-\nu)(1 - F_{GA}(q))}{P(Y > q) = (1 - nu) (1 - F_GA(q))},
//' is computed directly from the upper tail of the gamma distribution, in log
//' space for \code{log_p = TRUE}, and not as \eqn{1 - F}. It therefore keeps its
//' relative accuracy far in the tail, where \eqn{F} rounds to 1 and \eqn{1 - F}
//' is 0 (or wrong by orders of magnitude): at \code{q = 300} with \code{mu = 2},
//' \code{sigma = 1} and \code{nu = 0.1} the upper tail is 6.46e-66, where
//' \code{\link[gamlss.dist]{pZAGA}} gives 0 (and a log of \code{-Inf}).
//'
//' The lower tail on the log scale (\code{lower_tail = TRUE},
//' \code{log_p = TRUE}) is treated the same way where \eqn{F > 1/2}: it is
//' \eqn{\log(1 - U)}{log(1 - U)} computed as \code{log1p(-U)} from the direct
//' upper tail \eqn{U}, not as \eqn{\log F}{log F}, which is exactly 0 once
//' \eqn{F} rounds to 1 and loses relative accuracy before that. At
//' \code{q = 300} (same parameters) the log of the lower tail is
//' \eqn{-6.46 \times 10^{-66}}{-6.46e-66}, not 0. At
//' \eqn{F \leq 1/2}{F <= 1/2} it is \eqn{\log F}{log F}. The natural-scale lower
//' tail (\code{log_p = FALSE}) is
//' \eqn{\nu + (1-\nu) F_{GA}(q)}{nu + (1 - nu) F_GA(q)}, unchanged.
//'
//' The parameters must satisfy \eqn{\mu > 0}{mu > 0}, \eqn{\sigma > 0}{sigma > 0}
//' and \eqn{0 < \nu < 1}{0 < nu < 1}, with \code{mu} and \code{sigma} finite (an
//' infinite one is an error). This is stricter than
//' \code{\link[gamlss.dist]{pZAGA}}, which accepts \code{nu} in \eqn{[0, 1]}
//' and lets \code{mu = 0} and \code{sigma = 0} through (giving \code{NaN}). A
//' \code{NaN} or \code{NA} in any argument gives \code{NA}. Where the four
//' arguments are all non-\code{NaN} but the result is \code{NaN} (for example
//' \code{sigma = 1e-200} at \code{q > 0}, where \eqn{\sigma^2}{sigma^2}
//' underflows to 0), there is one warning \dQuote{NaNs produced} per call, as in
//' base R. A \eqn{\mu\sigma^2}{mu * sigma^2} below the normal range of doubles
//' (subnormal) loses accuracy, as it does in \code{pZAGA}.
//'
//' Where it differs from \code{\link[gamlss.dist]{pZAGA}}: a \code{q} below the
//' support (\eqn{q < 0}) is given on the requested tail and scale, so it is 1
//' for \code{lower_tail = FALSE}, and \code{-Inf} for \code{log_p = TRUE} on the
//' lower tail (\code{pZAGA} gives 0 in every case); and the upper tail, and the
//' log of the lower tail where \eqn{F > 1/2}, are direct, as described above.
//'
//' @return A numeric vector of cumulative probabilities.
//'
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Single values: F(0) is nu
//' fpZAGA(c(0, 0.5, 1, 2), mu = 2, sigma = 0.5, nu = 0.1)
//'
//' # Vector inputs with recycling
//' fpZAGA(c(0, 1, 2, 3), mu = c(1, 2), sigma = 0.5, nu = 0.1)
//'
//' # Upper tail, direct: still positive where 1 - F would be 0
//' fpZAGA(300, mu = 2, sigma = 1, nu = 0.1, lower_tail = FALSE)
//' fpZAGA(300, mu = 2, sigma = 1, nu = 0.1, lower_tail = FALSE, log_p = TRUE)
//'
//' # Lower tail on the log scale near F = 1: about -6.5e-66, not 0
//' fpZAGA(300, mu = 2, sigma = 1, nu = 0.1, log_p = TRUE)
//'
//' @seealso \code{\link{fdZAGA}}, \code{\link{fqZAGA}}, \code{\link{frZAGA}} for
//' the other ZAGA functions, and \code{\link[gamlss.dist]{ZAGA}} for the
//' original implementation.
//'
//' @export
// [[Rcpp::export]]
NumericVector fpZAGA(const NumericVector& q,
                     const NumericVector& mu,
                     const NumericVector& sigma,
                     const NumericVector& nu,
                     const bool& lower_tail = true,
                     const bool& log_p = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(q, mu, sigma, nu);
  const int n = recycled.n;

  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    // NaN/NA in any argument is handled in the compute loop below (mapped to
    // NA); skip the range checks here because NaN comparisons are always false.
    // There is no check on q: a negative q is below the support, not an error.
    if (ISNAN(recycled.vec1[i]) || ISNAN(recycled.vec2[i]) ||
        ISNAN(recycled.vec3[i]) || ISNAN(recycled.vec4[i])) continue;
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0 || recycled.vec4[i] >= 1.0) stop("nu must be between 0 and 1");
    // An infinite mu or sigma is an error, as in fdZAGA.
    if (!R_FINITE(recycled.vec2[i])) stop("mu must be finite");
    if (!R_FINITE(recycled.vec3[i])) stop("sigma must be finite");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // fpZAGA_scalar maps NaN/NA in any argument to NA_REAL.
    out[i] = fpZAGA_scalar(recycled.vec1[i], recycled.vec2[i],
                           recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }

  warn_if_nan_produced(out, recycled);
  return out;
}


//' Zero-Adjusted Gamma Distribution Quantile Function
//'
//' Quantile function for the Zero-Adjusted Gamma (ZAGA) distribution with
//' parameters mu (mean of the gamma part), sigma (its coefficient of variation)
//' and nu (the probability of a zero).
//'
//' @param p vector of probabilities.
//' @param mu vector of positive means of the gamma part.
//' @param sigma vector of positive coefficients of variation of the gamma part.
//' @param nu vector of zero probabilities (0 < nu < 1).
//' @param lower_tail logical; if TRUE (default), probabilities are P[X <= x],
//'   otherwise, P[X > x].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The quantile function returns the smallest \eqn{x \geq 0}{x >= 0} such that
//' \eqn{F(x) \geq p}{F(x) >= p}, where \eqn{F} is the ZAGA cumulative
//' distribution function. Every \eqn{p \leq \nu}{p <= nu} falls in the point
//' mass at zero and gives 0; above it the quantile is that of the gamma part at
//' \eqn{(p - \nu)/(1 - \nu)}{(p - nu)/(1 - nu)}. ZAGA is continuous, so the
//' quantile is a double, not an integer. A quantile is never negative: where
//' \code{qgamma} returns a negative value (shapes beyond about 1e49, that is
//' \code{sigma} below about 1e-25) the result is \code{NaN}.
//'
//' With \code{lower_tail = FALSE}, \code{p} is \eqn{P(Y > x)} and is inverted on
//' its own scale, from the upper tail of the gamma distribution (in log space
//' for \code{log_p = TRUE}), without forming \eqn{1 - p}. A far-upper-tail
//' quantile therefore keeps its relative accuracy: \code{p = 1e-300} gives a
//' finite quantile, where \eqn{1 - p} would round to 1. An upper-tail
//' \code{p} at or above \eqn{1 - \nu}{1 - nu} gives 0. On the log scale with a
//' tiny \code{nu}, a \code{p} just below \code{log1p(-nu)} puts the gamma part's
//' upper tail close to 1; where that tail exceeds 1/2 it is inverted from the
//' log of the gamma part's lower tail, \code{log(-expm1(r))} with
//' \code{r = p - log1p(-nu)}, which stays accurate where \code{qgamma} on
//' \code{r} itself does not (it is off by up to 18 percent, or \code{NaN}, for
//' \code{nu} below about 1e-184).
//'
//' The lower tail on the log scale (\code{lower_tail = TRUE},
//' \code{log_p = TRUE}) is inverted the same way where
//' \eqn{p > -\log 2}{p > -log(2)} (\eqn{F > 1/2}): from the upper tail
//' \eqn{-\mathrm{expm1}(p)}{-expm1(p)} rather than from
//' \eqn{\exp(p)}{exp(p)}, which rounds to 1 for \eqn{p} above about
//' \eqn{-5.6 \times 10^{-17}}{-5.6e-17} (\eqn{-2^{-54}}{-2^-54}, and would give
//' \code{Inf}) and loses the tail before that. A \code{p} of \code{-1e-300} thus
//' gives a finite quantile, and \code{p = 0} still gives \code{Inf}. For
//' \eqn{\nu > 1/2}{nu > 1/2}, \code{log(nu)} lies in this region, and a \code{p}
//' at or below it is the zero mass and gives 0 (\code{fqZAGA(log(nu), log_p =
//' TRUE)} is 0). For \eqn{p \leq -\log 2}{p <= -log(2)} the quantile is found
//' from \eqn{\exp(p)}{exp(p)} as before. The natural-scale lower tail
//' (\code{log_p = FALSE}) is unchanged.
//'
//' The parameters must satisfy \eqn{\mu > 0}{mu > 0}, \eqn{\sigma > 0}{sigma > 0}
//' and \eqn{0 < \nu < 1}{0 < nu < 1}, with \code{mu} and \code{sigma} finite (an
//' infinite one is an error). This is a stricter range than
//' \code{\link[gamlss.dist]{qZAGA}} accepts for \code{nu}
//' (\eqn{[0, 1]}). A probability outside \eqn{[0, 1]} (\eqn{p > 0} on the log
//' scale) is an error; a \code{NaN} or \code{NA} in any argument gives
//' \code{NA}. Where the four arguments are all non-\code{NaN} but the result is
//' \code{NaN} (for example \code{sigma = 1e-200}, where \eqn{\sigma^2}{sigma^2}
//' underflows to 0), there is one warning \dQuote{NaNs produced} per call, as in
//' base R. A \eqn{\mu\sigma^2}{mu * sigma^2} below the normal range of doubles
//' (subnormal) loses accuracy, as it does in \code{qZAGA}.
//'
//' Where it differs from \code{\link[gamlss.dist]{qZAGA}}: \code{p = 1} (on the
//' lower tail) gives \code{Inf}, where \code{qZAGA} stops with an error, as do
//' \code{\link{fqBCT}} and the other continuous quantile functions here; and an
//' upper-tail quantile, and a lower-tail quantile with \code{log_p = TRUE} where
//' \eqn{F > 1/2}, keep their relative accuracy, as described above.
//'
//' @return A numeric vector of quantiles (doubles; 0 inside the zero mass,
//'   \code{Inf} for a probability of 1).
//'
//' @references
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @examples
//' # Single values: the quantiles up to nu = 0.1 are zero
//' fqZAGA(c(0.05, 0.1, 0.5, 0.9), mu = 2, sigma = 0.5, nu = 0.1)
//'
//' # Vector inputs with recycling
//' fqZAGA(c(0.25, 0.5, 0.75), mu = c(1, 2), sigma = 0.5, nu = 0.1)
//'
//' # Round trip through the CDF
//' q <- fqZAGA(c(0.3, 0.6, 0.9), mu = 2, sigma = 0.5, nu = 0.1)
//' fpZAGA(q, mu = 2, sigma = 0.5, nu = 0.1)
//'
//' # Far upper tail, on its own scale
//' fqZAGA(1e-300, mu = 2, sigma = 1, nu = 0.1, lower_tail = FALSE)
//'
//' # The same quantile from the lower tail on the log scale: finite, not Inf
//' fqZAGA(-1e-300, mu = 2, sigma = 1, nu = 0.1, log_p = TRUE)
//'
//' @seealso \code{\link{fdZAGA}}, \code{\link{fpZAGA}}, \code{\link{frZAGA}} for
//' the other ZAGA functions, and \code{\link[gamlss.dist]{ZAGA}} for the
//' original implementation.
//'
//' @export
// [[Rcpp::export]]
NumericVector fqZAGA(const NumericVector& p,
                     const NumericVector& mu,
                     const NumericVector& sigma,
                     const NumericVector& nu,
                     const bool& lower_tail = true,
                     const bool& log_p = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(p, mu, sigma, nu);
  const int n = recycled.n;

  // Validate parameters after recycling
  for (int i = 0; i < n; i++)
  {
    // NaN/NA inputs are handled in the compute loop below (mapped to NA); skip
    // the range checks here because every NaN comparison is false anyway.
    if (ISNAN(recycled.vec1[i]) || ISNAN(recycled.vec2[i]) ||
        ISNAN(recycled.vec3[i]) || ISNAN(recycled.vec4[i])) continue;
    check_prob(recycled.vec1[i], log_p, 1.0, "p must be >=0 and <=1");
    if (recycled.vec2[i] <= 0.0) stop("mu must be greater than 0");
    if (recycled.vec3[i] <= 0.0) stop("sigma must be greater than 0");
    if (recycled.vec4[i] <= 0.0 || recycled.vec4[i] >= 1.0) stop("nu must be between 0 and 1");
    // An infinite mu or sigma is an error, as in fdZAGA.
    if (!R_FINITE(recycled.vec2[i])) stop("mu must be finite");
    if (!R_FINITE(recycled.vec3[i])) stop("sigma must be finite");
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // fqZAGA_scalar maps NaN/NA in any argument to NA_REAL, so a NaN p never
    // reaches R::qgamma.
    out[i] = fqZAGA_scalar(recycled.vec1[i], recycled.vec2[i],
                           recycled.vec3[i], recycled.vec4[i], lower_tail, log_p);
  }

  warn_if_nan_produced(out, recycled);
  return out;
}

// NOTE: there is deliberately no vectorised, Rcpp-exported frZAGA() here. The
// exported frZAGA() is the R implementation in R/rng_distr.R, which draws from
// dqrng::dqrunif() and inverts fqZAGA() (see the note at the end of
// src/distr_ZINBI.cpp for why a same-named C++ export is avoided). The
// per-element frZAGA_scalar() is available (inline) in inst/include/distr_ZAGA.h
// for a LinkingTo: CKutils consumer, takes a caller-supplied uniform, and agrees
// with frZAGA() draw for draw under one dqset.seed(). See the parity tests in
// inst/tinytest/test-rng_distr.R.
