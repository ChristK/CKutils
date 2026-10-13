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
#include "distr_BCPE.h"   // canonical header-only scalar definitions
#include "distr_BCPEo.h"  // the BCPEo aliases and declarations
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// Box-Cox Power Exponential functions
// BCPE *_scalar definitions live (inline) in inst/include/distr_BCPE.h. BCPE is
// continuous: x, q and the quantiles are doubles, so none of the count handling of
// the ZINBI wrappers is here. fdBCPEo/fpBCPEo/fqBCPEo (gamlss.dist's BCPEo, the
// same distribution) are thin wrappers over the same validation and kernels, so
// their results are identical() to the BCPE ones.

// The parameter checks of every BCPE/BCPEo wrapper, for one element whose
// arguments are all non-NaN: mu, sigma and tau positive, then all four finite (nu
// has no range). The messages are those of the earlier fdBCPEo/fpBCPEo/fqBCPEo.
static inline void check_bcpe_params(const double mu, const double sigma,
                                     const double nu, const double tau)
{
  if (mu <= 0.0) stop("mu must be positive");
  if (sigma <= 0.0) stop("sigma must be positive");
  if (tau <= 0.0) stop("tau must be positive");
  if (!R_FINITE(mu)) stop("mu must be finite");
  if (!R_FINITE(sigma)) stop("sigma must be finite");
  if (!R_FINITE(nu)) stop("nu must be finite");
  if (!R_FINITE(tau)) stop("tau must be finite");
}

static inline bool any_nan5(const RecycledVectors5& r, const int i)
{
  return ISNAN(r.vec1[i]) || ISNAN(r.vec2[i]) || ISNAN(r.vec3[i]) ||
         ISNAN(r.vec4[i]) || ISNAN(r.vec5[i]);
}

// Base R's d/p/q functions warn "NaNs produced" once per call when a NaN comes
// out of inputs that are not NaN. So does this: an element whose five inputs are
// all non-NaN but whose result is NaN gives one warning for the call (the kernels
// are designed never to do so for valid inputs; this is a safety net). An NA/NaN
// input gives NA silently. Called after the compute loop, which carries a SIMD
// hint and so must not write a shared flag.
static inline void warn_if_nan_produced(const NumericVector& out,
                                        const RecycledVectors5& r)
{
  for (int i = 0; i < r.n; i++)
  {
    if (ISNAN(out[i]) && !any_nan5(r, i))
    {
      warning("NaNs produced");
      return;
    }
  }
}


//' Box-Cox Power Exponential (BCPE) Distribution Density
//'
//' Probability density function of the Box-Cox Power Exponential (BCPE)
//' distribution of Rigby and Stasinopoulos (2004), with parameters mu (the
//' median, up to the effect of the truncation; see Details), sigma (a scale
//' parameter, approximately the coefficient of variation for
//' \eqn{\nu \geq 0}{nu >= 0}), nu (the Box-Cox power, skewness) and tau (the
//' power-exponential shape, kurtosis).
//' \code{fdBCPEo} is the same function (gamlss.dist's BCPEo, see Details).
//'
//' @param x vector of quantiles. An \code{x <= 0} has density 0.
//' @param mu vector of positive median parameters (see Details).
//' @param sigma vector of positive scale parameters.
//' @param nu vector of (real) Box-Cox powers.
//' @param tau vector of positive kurtosis parameters.
//' @param log_ logical; if TRUE, the density is given as log(f).
//'
//' @details
//' With \eqn{z} the Box-Cox transform of \eqn{y > 0},
//' \deqn{z = \frac{(y/\mu)^\nu - 1}{\nu\sigma} \; (\nu \neq 0), \qquad z = \frac{\log(y/\mu)}{\sigma} \; (\nu = 0),}{z = ((y/mu)^nu - 1)/(nu * sigma) (nu != 0), z = log(y/mu)/sigma (nu = 0),}
//' \eqn{Z} is a power exponential variable \eqn{T} with unit variance and shape
//' \eqn{\tau}{tau} (\eqn{\tau = 2}{tau = 2}: the standard normal; \eqn{\tau = 1}{tau = 1}:
//' the Laplace), truncated to the range of the transform (\eqn{z > -b} for
//' \eqn{\nu > 0}{nu > 0}, \eqn{z < b} for \eqn{\nu < 0}{nu < 0}, with
//' \eqn{b = 1/(\sigma|\nu|)}{b = 1/(sigma * |nu|)}). Its density is
//' \deqn{f(y) = \frac{y^{\nu-1}}{\mu^\nu \sigma} \frac{f_T(z)}{F_T(b)},}{f(y) = y^(nu - 1) / (mu^nu * sigma) * f_T(z) / F_T(b),}
//' where \eqn{f_T(t) = \tau / (c\, 2^{1+1/\tau} \Gamma(1/\tau)) \exp(-0.5 |t/c|^\tau)}{f_T(t) = tau / (c * 2^(1 + 1/tau) * Gamma(1/tau)) * exp(-0.5 * |t/c|^tau)},
//' \eqn{c^2 = 2^{-2/\tau} \Gamma(1/\tau) / \Gamma(3/\tau)}{c^2 = 2^(-2/tau) * Gamma(1/tau) / Gamma(3/tau)},
//' and \eqn{F_T(b)}{F_T(b)} (1 for \eqn{\nu = 0}{nu = 0}) is the mass the
//' truncation keeps. \eqn{\mu}{mu} is the \eqn{y} at which the transform is 0 (the
//' median of \eqn{T}): exactly the median of \eqn{Y} for \eqn{\nu = 0}{nu = 0}, and
//' for \eqn{\nu \neq 0}{nu != 0} the truncated mass moves the median of \eqn{Y} a
//' little (\eqn{F(\mu)}{F(mu)} is 0.4933 at \code{sigma = 0.3, nu = 1.5, tau = 2}
//' and 0.5067 at \code{nu = -1.5}). \eqn{\sigma}{sigma} is approximately the
//' coefficient of variation of \eqn{Y} for a small \eqn{\sigma}{sigma} and
//' \eqn{\nu \geq 0}{nu >= 0} only: for \eqn{\nu < 0}{nu < 0} the upper tail of
//' \eqn{Y} is a power law of index \eqn{|\nu|}{|nu|}, so the variance is infinite for
//' \eqn{-2 \leq \nu < 0}{-2 <= nu < 0} and the mean for
//' \eqn{-1 \leq \nu < 0}{-1 <= nu < 0}.
//'
//' \strong{BCPE and BCPEo.} gamlss.dist's \code{dBCPE} and \code{dBCPEo} (and
//' their p/q/r functions) are line for line the same function; the gamlss
//' \emph{families} \code{BCPE()} and \code{BCPEo()} differ only in the default link
//' for mu (identity and log), which matters when fitting, not here. So
//' \code{fdBCPEo} is \code{fdBCPE}: same arguments, same validation, same kernels,
//' and \code{identical()} results.
//'
//' \strong{Agreement with gamlss.dist.} Where \code{\link[gamlss.dist]{dBCPE}} is
//' accurate -- \eqn{\nu = 0}{nu = 0} or \eqn{|\nu| \min(1, \sigma) \geq 2^{-8}}{|nu| * min(1, sigma) >= 2^-8},
//' \eqn{\tau \geq 1}{tau >= 1}, and \eqn{y/\mu}{y/mu} and gamlss's intermediates in
//' the normal double range -- \code{fdBCPE} repeats its arithmetic operation for
//' operation, so the density and the log density are \code{identical()} to
//' \code{dBCPE} (called on a vector; see the note in \code{\link{fqBCPE}}) there.
//' That parity includes \code{dBCPE}'s own rounding error: within about 2e-13 at a
//' well-conditioned point, but growing as sigma falls, because gamlss's
//' \eqn{z = \log(y/\mu)/\sigma}{z = log(y/mu)/sigma} carries the rounding error of
//' \eqn{y/\mu}{y/mu} divided by \eqn{\sigma}{sigma} (at \code{sigma = 0.001},
//' \code{tau = 2}, \eqn{z = 10}{z = 10} the density can be off by up to about 1e-12,
//' relative). It is kept there.
//'
//' \strong{Elsewhere} the transform is computed as
//' \eqn{(\ell/\sigma)\,\mathrm{expm1}(\nu\ell)/(\nu\ell)}{(ell/sigma) * expm1(nu*ell)/(nu*ell)},
//' \eqn{\ell = \log(y/\mu)}{ell = log(y/mu)} (from \code{log1p((y - mu)/mu)} near
//' \eqn{y = \mu}{y = mu}), which keeps its digits down to \eqn{\nu = 0}{nu = 0}. That
//' is where \code{dBCPE} is not accurate: for a small \eqn{|\nu|}{|nu|}
//' \eqn{((y/\mu)^\nu - 1)/(\nu\sigma)}{((y/mu)^nu - 1)/(nu * sigma)} cancels (at
//' \eqn{\nu = 10^{-12}}{nu = 1e-12} \code{dBCPE} is typically off in the fourth to
//' fifth digit: the median relative error is 1e-4 over 1e5 draws with mu 15 to 40,
//' sigma 0.05 to 0.3, tau 1.2 to 4 and x at uniform quantiles, and the worst 0.8 \%;
//' at \eqn{\nu = 10^{-17}}{nu = 1e-17} it is the density of the wrong distribution:
//' \code{dBCPE(15, 25, 0.15, 1e-17, 2)} is 0.1773 for a true 5.4e-4); and for
//' \eqn{\tau < 1}{tau < 1}, where \eqn{f_T}{f_T} has a cusp at 0, the absolute error of
//' gamlss's \eqn{z} near the median is magnified (at \eqn{\tau = 0.05}{tau = 0.05},
//' \code{dBCPE(25 * (1 + 1e-15), 25, 0.05, 0.5, 0.05, log = TRUE)} is 21.602 for a
//' true 21.506).
//'
//' \strong{Domain and accuracy.} The computation was checked against 50-digit values
//' over mu 1e-6 to 1e6, sigma 1e-3 to 10, \eqn{\nu}{nu} from -10 to 10 (and
//' \eqn{|\nu|}{abs(nu)} down to 1e-300), tau 0.05 to 100 and x from 1e-298 to 1e299:
//' within 1e-13 relative (on the log scale, relative to \code{max(1, abs(log f))}), or
//' 16 ulp times the condition number of the point where that is larger; 5e-13 for the
//' elements of the region above, which carry gamlss.dist's error. Beyond these ranges
//' the function still returns numbers (no \code{NaN} came out of 2e5 random parameter
//' sets, with mu from 1e-250 to 1e250, sigma from 1e-8 to 1e3,
//' \eqn{|\nu|}{abs(nu)} up to 50 and tau from 1e-3 to 1e3), but their accuracy is not
//' verified.
//'
//' \strong{Edges.} The density is 0 (log: \code{-Inf}) for \code{x <= 0}
//' (including \code{-Inf}) and for \code{x = Inf}. At \code{x = 0} this is a
//' convention, not the limit: as \eqn{y \to 0}{y -> 0} the density tends to
//' \code{Inf} for \eqn{0 < \nu < 1}{0 < nu < 1} (\code{fdBCPE(1e-300, 25, 0.15, 0.5, 2)}
//' is 1.32e111), to \eqn{f_T(-b)/(\mu\sigma F_T(b))}{f_T(-b) / (mu * sigma * F_T(b))}
//' for \eqn{\nu = 1}{nu = 1}, and to 0 otherwise.
//'
//' \strong{Where it differs from \code{\link[gamlss.dist]{dBCPE}}}: the log density
//' at \code{x <= 0} is \code{-Inf} (\code{dBCPE} gives 0, with a warning for
//' \code{x < 0}); the
//' density at \code{x = Inf} is 0 (\code{dBCPE} gives \code{NaN} for
//' \eqn{\nu \geq 1}{nu >= 1}); a small \eqn{|\nu|}{|nu|} is accurate (above);
//' \code{NA} in any argument gives \code{NA} (\code{dBCPE} stops for an \code{NA}
//' in mu, sigma or tau); and the parameter range is strict (below).
//'
//' \strong{Validation.} After recycling, each element must have
//' \eqn{\mu > 0}{mu > 0}, \eqn{\sigma > 0}{sigma > 0} and \eqn{\tau > 0}{tau > 0}
//' (\dQuote{mu must be positive}, \dQuote{sigma must be positive}, \dQuote{tau must
//' be positive}), and all four parameters finite (\dQuote{mu must be finite}, ...,
//' \dQuote{nu must be finite}); \code{nu} has no range, and every real \code{x} is
//' valid. gamlss.dist accepts \code{mu = 0}, \code{sigma = 0}, \code{tau = 0} and
//' infinite parameters (giving \code{NaN} or meaningless numbers). An element
//' with an \code{NA} or \code{NaN} argument gives \code{NA} and is not checked.
//' A \code{NaN} result from non-\code{NaN} arguments (not expected inside the
//' supported domain above) gives one warning \dQuote{NaNs produced} per call. All
//' arguments recycle to the longest; a zero-length argument gives a zero-length
//' result.
//'
//' @return A numeric vector of densities.
//'
//' @references
//' Rigby, R. A. and Stasinopoulos, D. M. (2004) Smooth centile curves for skew and
//' kurtotic data modelled using the Box-Cox power exponential distribution.
//' Statistics in Medicine, 23, 3053-3076.
//'
//' Stasinopoulos, D. M., Rigby, R. A., Heller, G. Z., Voudouris, V., and
//' De Bastiani, F. (2017) Flexible Regression and Smoothing: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @examples
//' fdBCPE(c(10, 20, 25, 30, 40), mu = 25, sigma = 0.15, nu = 0.5, tau = 2)
//'
//' # Vector inputs with recycling, and the log density
//' fdBCPE(c(1, 2, 3), mu = c(2, 3), sigma = 0.5, nu = c(-1, 0, 1), tau = 1.5)
//' fdBCPE(c(1, 2, 3), mu = 2, sigma = 0.5, nu = 1, tau = 2, log_ = TRUE)
//'
//' # x <= 0 and x = Inf carry no density
//' fdBCPE(c(-1, 0, Inf), mu = 2, sigma = 0.5, nu = 1, tau = 2, log_ = TRUE)
//'
//' # BCPEo is the same distribution
//' identical(fdBCPEo(1:5, 2, 0.5, 1, 2), fdBCPE(1:5, 2, 0.5, 1, 2))
//'
//' @seealso \code{\link{fpBCPE}}, \code{\link{fqBCPE}}, \code{\link{frBCPE}} for
//' the other BCPE functions, and \code{\link[gamlss.dist]{BCPE}} for the original
//' implementation.
//'
//' @export
// [[Rcpp::export]]
NumericVector fdBCPE(const NumericVector& x,
                     const NumericVector& mu,
                     const NumericVector& sigma,
                     const NumericVector& nu,
                     const NumericVector& tau,
                     const bool& log_ = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(x, mu, sigma, nu, tau);
  const int n = recycled.n;

  // Validate parameters after recycling. NaN/NA in any argument is handled in the
  // compute loop below (mapped to NA); there is no check on x.
  for (int i = 0; i < n; i++)
  {
    if (any_nan5(recycled, i)) continue;
    check_bcpe_params(recycled.vec2[i], recycled.vec3[i], recycled.vec4[i], recycled.vec5[i]);
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // fdBCPE_scalar maps NaN/NA in any argument to NA_REAL.
    out[i] = fdBCPE_scalar(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i],
                           recycled.vec4[i], recycled.vec5[i], log_);
  }

  warn_if_nan_produced(out, recycled);
  return out;
}


//' Box-Cox Power Exponential (BCPE) Distribution CDF
//'
//' Cumulative distribution function of the Box-Cox Power Exponential (BCPE)
//' distribution, with parameters mu (the median, up to the effect of the
//' truncation; see \code{\link{fdBCPE}}), sigma, nu (the Box-Cox power) and tau
//' (the power-exponential shape). \code{fpBCPEo} is the same function
//' (gamlss.dist's BCPEo; see \code{\link{fdBCPE}}).
//'
//' @param q vector of quantiles. A \code{q <= 0} is below the support.
//' @param mu vector of positive median parameters (see \code{\link{fdBCPE}}).
//' @param sigma vector of positive scale parameters.
//' @param nu vector of (real) Box-Cox powers.
//' @param tau vector of positive kurtosis parameters.
//' @param lower_tail logical; if TRUE (default), probabilities are P[Y <= q],
//'   otherwise P[Y > q].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' With \eqn{z} the Box-Cox transform of \eqn{q} and \eqn{T} the power
//' exponential variable of \code{\link{fdBCPE}}, truncated at
//' \eqn{\mp b = \mp 1/(\sigma|\nu|)}{-+b = -+1/(sigma * |nu|)},
//' \deqn{F(q) = \frac{F_T(z) - F_T(-b)\,[\nu > 0]}{F_T(b)},}{F(q) = (F_T(z) - F_T(-b) * [nu > 0]) / F_T(b),}
//' and \eqn{F(q) = 0} for \eqn{q \leq 0}{q <= 0}. \code{fpBCPEo} is
//' \code{fpBCPE} (\code{identical()} results).
//'
//' \strong{Agreement with gamlss.dist.} On the natural scale of the lower tail
//' (\code{lower_tail = TRUE, log_p = FALSE}), where
//' \code{\link[gamlss.dist]{pBCPE}} is accurate, \code{fpBCPE} repeats its
//' arithmetic operation for operation and is \code{identical()} to it (\code{pBCPE}
//' called on a vector; see the note in \code{\link{fqBCPE}}) where all of these
//' hold: \eqn{\nu = 0}{nu = 0} or
//' \eqn{|\nu| \min(1, \sigma) \geq 2^{-8}}{|nu| * min(1, sigma) >= 2^-8};
//' \eqn{\tau \geq 1}{tau >= 1} (for \eqn{\tau < 1}{tau < 1} gamlss's \eqn{z} error
//' meets the cusp of \eqn{f_T}{f_T} at 0 near the median: at \code{sigma = 0.05},
//' \code{nu = 0.0789}, \code{tau = 0.05} \code{pBCPE} is up to 2.1e-4 off, relative);
//' for \eqn{z < 0} gamlss's \eqn{F_T(z) \geq 2^{-8}}{F_T(z) >= 2^-8}; for
//' \eqn{\nu > 0}{nu > 0} the truncation \eqn{F_T(z) - F_T(-b)}{F_T(z) - F_T(-b)}
//' keeps at least half of \eqn{F_T(z)}{F_T(z)}; its quotient is not above 1; and
//' \eqn{q/\mu}{q/mu} and gamlss's intermediates are in the normal double range. That
//' covers the bulk of a typical model (98 \% of the rows of 1e6 draws with mu 15 to 40,
//' sigma 0.05 to 0.3, nu -1.5 to 1.5, tau 1.2 to 4). There gamlss.dist, hence this
//' function, is within about \eqn{40 \epsilon \kappa}{40 * eps * kappa} of the truth
//' (\eqn{\kappa}{kappa} the condition number of the point), i.e. about 2e-13 at
//' well-conditioned points; at a small tau (below about 0.3) near
//' \eqn{q = \mu}{q = mu} the problem itself is ill-conditioned (\eqn{f_T}{f_T} has a
//' cusp at 0). That parity includes \code{pBCPE}'s own rounding error, which grows as
//' sigma falls (gamlss's \eqn{z} carries the rounding error of \eqn{q/\mu}{q/mu}
//' divided by \eqn{\sigma}{sigma}): for \eqn{\sigma \leq 0.01}{sigma <= 0.01} \eqn{F}
//' is off by up to about 5e-14 (absolute; \code{pBCPE(1.00056e-6, 1e-6, 0.001, 0, 1)} is
//' off by 3.4e-14), against about 1e-15 for the upper tail, which is computed
//' directly, so that \eqn{F} plus the upper tail is 1 to that, not to a few ulp. It is
//' kept there.
//'
//' \strong{Everything else is computed stably}: the upper tail, both log scales,
//' and the elements of the natural lower tail outside that region. The T masses
//' are direct tails, \eqn{0.5\, P(S > s)}{0.5 * P(S > s)} with
//' \eqn{S = 0.5|T/c|^\tau}{S = 0.5 * |T/c|^tau} a gamma variable, never
//' \eqn{0.5 (1 + G\, \mathrm{sign}(t))}{0.5 * (1 + G * sign(t))}; the masses of
//' \eqn{Z} are sums of them, never \eqn{1 - F}; the transform is
//' \eqn{(\ell/\sigma)\,\mathrm{expm1}(\nu\ell)/(\nu\ell)}{(ell/sigma) * expm1(nu*ell)/(nu*ell)}
//' with \eqn{\ell = \log(q/\mu)}{ell = log(q/mu)}; next to the truncation point
//' (\eqn{q \to 0}{q -> 0} for \eqn{\nu > 0}{nu > 0}, \eqn{q \to \infty}{q -> Inf} for
//' \eqn{\nu < 0}{nu < 0}, where the tail of \eqn{Y} is a power law) the mass between
//' \eqn{z} and \eqn{\mp b}{-+b} comes from the exactly known gap
//' \eqn{b (q/\mu)^\nu}{b * (q/mu)^nu}, by a difference that loses at most a bit or by
//' Gauss-Legendre quadrature. On the log scale \eqn{\log F}{log F} is
//' \code{log1p(-U)} from the direct upper tail \eqn{U} where \eqn{F > 1/2}, and the
//' log of a tiny side is computed in log space, so it stays finite far below the
//' double range. The relative error is a few ulp times the condition number of
//' the point.
//'
//' \strong{Domain and accuracy.} The computation was checked against 50-digit values
//' over mu 1e-6 to 1e6, sigma 1e-3 to 10, \eqn{\nu}{nu} from -10 to 10 (and
//' \eqn{|\nu|}{abs(nu)} down to 1e-300), tau 0.05 to 100 and q from 1e-298 to 1e299, on
//' both tails and scales (tails down to 1e-300, log tails down to -1e4): within 1e-13
//' relative (on a log scale, relative to the value), or 16 ulp times the condition
//' number of the point where that is larger; 5e-13 for the elements of the region
//' above, which carry gamlss.dist's error. Beyond these ranges the function still
//' returns numbers (no \code{NaN} came out of 2e5 random parameter sets, with mu from
//' 1e-250 to 1e250, sigma from 1e-8 to 1e3, \eqn{|\nu|}{abs(nu)} up to 50 and tau from
//' 1e-3 to 1e3), but their accuracy is not verified.
//'
//' \strong{Where it differs from \code{\link[gamlss.dist]{pBCPE}}}: a
//' \code{q <= 0} is given on the requested tail and scale (0, \code{-Inf}, 1, 0 for
//' (lower, natural), (lower, log), (upper, natural), (upper, log); \code{pBCPE}
//' gives 0 in every case), and \code{q = Inf} likewise (1, 0, 0, \code{-Inf}; \code{pBCPE}
//' is off by 2.2e-16, and gives \code{NaN} or -36.04 for the log upper tail, in up to
//' 0.4 \% of parameter sets); the upper tail is direct, not \eqn{1 - F}
//' (\code{pBCPE(100, 25, 0.15, 0, 2, lower.tail = FALSE)} is 0 for a true
//' 1.21e-20); the log scale is accurate at both ends
//' (\code{pBCPE(5, 25, 0.15, 0, 2, log.p = TRUE)} is \code{-Inf} for a true
//' -60.862); the far lower tail is accurate (\code{pBCPE(7, 25, 0.15, 0, 2)} is 0
//' for a true 1.0653e-17, since \eqn{0.5(1 + G\,\mathrm{sign}(z))}{0.5 * (1 + G * sign(z))}
//' rounds to 0 below about 1e-16); a \eqn{\nu}{nu} near 0 is accurate (at
//' \eqn{\nu = 10^{-17}}{nu = 1e-17} \code{pBCPE} is exactly 0.5 for \eqn{q/\mu}{q/mu}
//' from about \eqn{e^{-5.55}}{exp(-5.55)} = 0.004 to \eqn{e^{11.1}}{exp(11.1)} = 66,000,
//' 0 below that and 1 above); the truncation regime is accurate; at a large tau
//' gamlss's \eqn{0.5|z/c|^\tau}{0.5 * |z/c|^tau} underflows to 0 near the median, so
//' that \code{pBCPE(25.0025, 25, 0.15, 0.5, 100)} is 0.5 for a true 0.500192, and its
//' quotient \eqn{(F_1 - F_2)/F_3}{(F1 - F2) / F3} can round to \eqn{1 + 2.2 \times 10^{-16}}{1 + 2.2e-16},
//' a probability above 1 that \code{fpBCPE} never returns; \code{NA} in any argument
//' gives \code{NA} (\code{pBCPE} stops for an \code{NA} in mu, sigma or tau); and the
//' parameter range is strict.
//'
//' \strong{Validation} and \code{NA} handling are those of \code{\link{fdBCPE}}:
//' \eqn{\mu, \sigma, \tau > 0}{mu, sigma, tau > 0}, all four parameters finite, every
//' real \code{q} valid.
//'
//' @return A numeric vector of probabilities (on the log scale for
//'   \code{log_p = TRUE}).
//'
//' @references
//' Rigby, R. A. and Stasinopoulos, D. M. (2004) Smooth centile curves for skew and
//' kurtotic data modelled using the Box-Cox power exponential distribution.
//' Statistics in Medicine, 23, 3053-3076.
//'
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @examples
//' fpBCPE(c(10, 20, 25, 30, 40), mu = 25, sigma = 0.15, nu = 0.5, tau = 2)
//'
//' # The far lower tail, and the upper tail, keep their relative accuracy
//' fpBCPE(7, mu = 25, sigma = 0.15, nu = 0, tau = 2)
//' fpBCPE(100, mu = 25, sigma = 0.15, nu = 0, tau = 2, lower_tail = FALSE)
//' fpBCPE(5, mu = 25, sigma = 0.15, nu = 0, tau = 2, log_p = TRUE)
//'
//' # q <= 0 is below the support, on the requested tail and scale
//' fpBCPE(-1, 25, 0.15, 0.5, 2, lower_tail = FALSE)
//'
//' # The power-law tail next to the truncation point (nu > 0, q -> 0)
//' fpBCPE(1e-100, mu = 25, sigma = 0.15, nu = 0.5, tau = 2, log_p = TRUE)
//'
//' @seealso \code{\link{fdBCPE}}, \code{\link{fqBCPE}}, \code{\link{frBCPE}} for
//' the other BCPE functions, and \code{\link[gamlss.dist]{BCPE}} for the original
//' implementation.
//'
//' @export
// [[Rcpp::export]]
NumericVector fpBCPE(const NumericVector& q,
                     const NumericVector& mu,
                     const NumericVector& sigma,
                     const NumericVector& nu,
                     const NumericVector& tau,
                     const bool& lower_tail = true,
                     const bool& log_p = false)
{
  // Recycle vectors to common length
  auto recycled = recycle_vectors(q, mu, sigma, nu, tau);
  const int n = recycled.n;

  // Validate parameters after recycling. NaN/NA in any argument is handled in the
  // compute loop below (mapped to NA); a q <= 0 is below the support, not an error.
  for (int i = 0; i < n; i++)
  {
    if (any_nan5(recycled, i)) continue;
    check_bcpe_params(recycled.vec2[i], recycled.vec3[i], recycled.vec4[i], recycled.vec5[i]);
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // fpBCPE_scalar maps NaN/NA in any argument to NA_REAL.
    out[i] = fpBCPE_scalar(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i],
                           recycled.vec4[i], recycled.vec5[i], lower_tail, log_p);
  }

  warn_if_nan_produced(out, recycled);
  return out;
}


//' Box-Cox Power Exponential (BCPE) Distribution Quantile Function
//'
//' Quantile function of the Box-Cox Power Exponential (BCPE) distribution, with
//' parameters mu (the median, up to the effect of the truncation; see
//' \code{\link{fdBCPE}}), sigma, nu (the Box-Cox power) and tau (the
//' power-exponential shape). \code{fqBCPEo} is the same function (gamlss.dist's
//' BCPEo; see \code{\link{fdBCPE}}).
//'
//' @param p vector of probabilities.
//' @param mu vector of positive median parameters (see \code{\link{fdBCPE}}).
//' @param sigma vector of positive scale parameters.
//' @param nu vector of (real) Box-Cox powers.
//' @param tau vector of positive kurtosis parameters.
//' @param lower_tail logical; if TRUE (default), probabilities are P[Y <= y],
//'   otherwise P[Y > y].
//' @param log_p logical; if TRUE, probabilities p are given as log(p).
//'
//' @details
//' The quantile is the \eqn{y} with \eqn{F(y) = p} (\eqn{P(Y > y) = p} for
//' \code{lower_tail = FALSE}). With the T quantile \eqn{z_a} at the truncated
//' target (\eqn{p F_T(b)}{p * F_T(b)} for \eqn{\nu < 0}{nu < 0},
//' \eqn{1 - (1 - p) F_T(b)}{1 - (1 - p) * F_T(b)} for \eqn{\nu > 0}{nu > 0}),
//' \eqn{y = \mu (1 + \nu\sigma z_a)^{1/\nu}}{y = mu * (1 + nu * sigma * z_a)^(1/nu)}
//' (\eqn{\mu e^{\sigma z_a}}{mu * exp(sigma * z_a)} for \eqn{\nu = 0}{nu = 0}).
//' \code{p = 0} gives 0 and \code{p = 1} gives \code{Inf} on the lower tail (on
//' the log scale, \code{-Inf} and 0; the mirror image on the upper tail). A
//' quantile is never negative, and inside the supported domain (below) never
//' \code{NaN}: it underflows to 0 or overflows to \code{Inf} only where the true
//' value is outside the double range, except for an astronomically small tail
//' probability (below). \code{fqBCPEo} is \code{fqBCPE} (\code{identical()} results).
//'
//' \strong{Switching from gamlss.dist reproduces a model's draws.} On the natural
//' scale of the lower tail (\code{lower_tail = TRUE, log_p = FALSE}), where
//' \code{\link[gamlss.dist]{qBCPE}} is accurate, \code{fqBCPE} repeats its
//' arithmetic operation for operation and is \code{identical()} to it (to
//' \code{qBCPE} called on a vector; see Note): for \eqn{\nu \neq 0}{nu != 0} the base
//' \eqn{w = 1 + \nu\sigma z_a}{w = 1 + nu * sigma * z_a} of the power with
//' \eqn{|\nu| \min(1, w) \geq 2^{-8}}{|nu| * min(1, w) >= 2^-8} (\eqn{w = 1}{w = 1} for
//' \eqn{\nu = 0}{nu = 0}), gamlss's T target \eqn{p_t}{pt} with
//' \eqn{\min(p_t, 1 - p_t) \geq 2^{-8} \max(1, \sigma/w)}{min(pt, 1 - pt) >= 2^-8 * max(1, sigma/w)},
//' and gamlss's intermediates in the normal double range. There \code{qBCPE} is
//' within about 1e-13 of the truth, at any sigma. On 1e6 draws with mu 15 to 40,
//' sigma 0.05 to 0.3, nu -1.5 to 1.5, tau 1.2 to 4 and
//' \eqn{p \sim U(0, 1)}{p ~ U(0, 1)}, about 99 \% of \code{fqBCPE(p, ...)} are
//' \code{identical()} to \code{qBCPE(p, ...)}, and the rest differ by at most the
//' error of \code{qBCPE}.
//'
//' \strong{Everything else is computed stably}: the upper tail, both log scales,
//' and the elements of the natural lower tail outside that region. The smaller
//' tail mass is inverted, with R's \code{qgamma} on a probability of at most 1/2
//' on its own tail and then polished by Newton steps on \code{pgamma} (R's
//' \code{qgamma} is up to about 5e-10 off, relative, for upper tails between 1e-14
//' and 4e-12 at a shape of 0.25); the
//' truncated targets are sums (\eqn{L + pD}{L + p*D}), never \eqn{1 - (1-p)D}{1 - (1-p)*D};
//' the back-transform is \eqn{\mu \exp(\sigma z\, \mathrm{log1p}(x)/x)}{mu * exp(sigma * z * log1p(x)/x)},
//' \eqn{x = \nu\sigma z}{x = nu * sigma * z}, which stays accurate down to
//' \eqn{\nu = 0}{nu = 0}; and next to the truncation point, where
//' \eqn{1 + \nu\sigma z < 1/2}{1 + nu * sigma * z < 1/2} would have lost the digits of
//' \eqn{z}, the gap \eqn{h = b (y/\mu)^\nu}{h = b * (y/mu)^nu} to the truncation point is
//' solved for directly (safeguarded Newton), so the power-law tail of \eqn{Y} keeps
//' its relative accuracy. With \code{log_p = TRUE} and
//' \eqn{p > -\log 2}{p > -log(2)} the other tail \code{-expm1(p)} is inverted,
//' not \code{exp(p)}, so \code{p = -1e-300} gives a finite quantile.
//'
//' \strong{Domain and accuracy.} The computation was checked against 50-digit values
//' over mu 1e-6 to 1e6, sigma 1e-3 to 10, \eqn{\nu}{nu} from -10 to 10 (and
//' \eqn{|\nu|}{abs(nu)} down to 1e-300), tau 0.05 to 100 and p from 1e-300 (log p down
//' to -1e4), on both tails and scales: within 1e-13 relative, or 16 ulp times the
//' condition number of the point where that is larger; 5e-13 for the elements of the
//' region above, which carry gamlss.dist's error. Beyond these ranges the function
//' still returns numbers (no \code{NaN} and no negative quantile came out of 2e5
//' random parameter sets, with mu from 1e-250 to 1e250, sigma from 1e-8 to 1e3,
//' \eqn{|\nu|}{abs(nu)} up to 50 and tau from 1e-3 to 1e3), but their accuracy is not
//' verified. With \code{log_p = TRUE} and a \code{p} below about -1e17 at tau 0.05
//' (-1e32 at tau 0.1, -2e93 at tau 0.3; never for tau >= 1) the quantile of T
//' overflows, and the result is 0 (lower tail, \eqn{\nu < 0}{nu < 0}) or \code{Inf}
//' (upper tail, \eqn{\nu > 0}{nu > 0}) even where the true quantile is a finite
//' double (\code{fqBCPE(-1e100, 20, 1, -8, 0.1, log_p = TRUE)} is 0).
//'
//' \strong{Where it differs from \code{\link[gamlss.dist]{qBCPE}}}: the far tails
//' are accurate (\code{qBCPE(1e-20, 25, 0.15, 0, 2)} is 0 for a true 6.2309, and
//' \code{qBCPE} finds an upper-tail quantile from \eqn{1 - p}, so
//' \code{qBCPE(1e-20, 25, 0.15, 0, 2, lower.tail = FALSE)} is \code{Inf} for a true
//' 100.31); a \eqn{\nu}{nu} near 0 is accurate
//' (\code{qBCPE(0.9, 25, 0.15, 1e-12, 2)} is 5.8e-5 off; at
//' \eqn{\nu = 10^{-17}}{nu = 1e-17} \code{qBCPE} returns \code{mu} for moderate
//' \code{p} and \code{qBCPE(1e-300, 25, 0.15, 1e-17, 2)} is \code{Inf} for a true
//' 0.0965); a \code{p} outside \eqn{[0, 1]}{[0, 1]} (\eqn{p > 0}{p > 0} on the log
//' scale) is an error (\code{qBCPE} gives \code{NaN} with a warning); \code{NA} in
//' any argument gives \code{NA} (\code{qBCPE} stops for an \code{NA} in mu, sigma
//' or tau, and \code{qBCPE(0.5, nu = NA)} is 0); and the parameter range is strict.
//'
//' \strong{Validation} and \code{NA} handling are those of \code{\link{fdBCPE}},
//' plus \code{p} in \eqn{[0, 1]}{[0, 1]} (\dQuote{p must be between 0 and 1}) or
//' \eqn{p \leq 0}{p <= 0} on the log scale.
//'
//' @return A numeric vector of quantiles (doubles; 0 for a lower-tail probability
//'   of 0, \code{Inf} for 1).
//'
//' @note gamlss.dist's own \code{qBCPE}, \code{dBCPE} and \code{pBCPE} can give a
//' different last bit for an element computed in a call of length one than for the
//' same element inside a vector, in R 4.6.0 and later. Byte-compiled R evaluates
//' \code{exp()} of a length-one argument with the C library, while R's vectorised
//' \code{exp()} (R 4.6.0's own \code{Rexp}) returns \eqn{1 + x}{1 + x} for
//' \eqn{|x| \leq 2^{-26}}{|x| <= 2^-26}; R 4.5 and earlier use the C library
//' throughout. Which rule applies is a property of the R version, not of the
//' platform. \code{fqBCPE}, \code{fdBCPE} and \code{fpBCPE} follow the rule of the R
//' version they are compiled against, in the form of a vector call, so the
//' \code{identical()} claims on these pages hold against gamlss.dist called on a
//' vector under that R version. (In \code{qBCPE} at \eqn{\nu = 0}{nu = 0} the choice
//' also depends on how many elements of the call have \eqn{\nu = 0}{nu = 0}, as the
//' \code{exp()} is taken on that subset.) It matters only where gamlss takes
//' \code{exp()} of a tiny number: for \eqn{\nu = 0}{nu = 0} within about 1e-8 of the
//' median (more widely at a small tau), for tau within about 3.5e-8 of 2, and for a
//' density within about 1e-8 of 1.
//'
//' @references
//' Rigby, R. A. and Stasinopoulos, D. M. (2004) Smooth centile curves for skew and
//' kurtotic data modelled using the Box-Cox power exponential distribution.
//' Statistics in Medicine, 23, 3053-3076.
//'
//' Rigby, R. A., Stasinopoulos, D. M., Heller, G. Z., and De Bastiani, F. (2019)
//' Distributions for modelling location, scale, and shape: Using GAMLSS in R,
//' Chapman and Hall/CRC.
//'
//' @examples
//' fqBCPE(c(0.1, 0.25, 0.5, 0.75, 0.9), mu = 25, sigma = 0.15, nu = 0.5, tau = 2)
//'
//' # Round trip through the CDF
//' q <- fqBCPE(c(0.3, 0.6, 0.9), mu = 25, sigma = 0.15, nu = 0.5, tau = 2)
//' fpBCPE(q, mu = 25, sigma = 0.15, nu = 0.5, tau = 2)
//'
//' # Far tails, on their own scales
//' fqBCPE(1e-20, mu = 25, sigma = 0.15, nu = 0, tau = 2)
//' fqBCPE(1e-20, mu = 25, sigma = 0.15, nu = 0, tau = 2, lower_tail = FALSE)
//' fqBCPE(-1e-300, mu = 25, sigma = 0.15, nu = 0, tau = 2, log_p = TRUE)
//'
//' # nu near 0 is the log-power-exponential limit, without cancellation
//' q <- fqBCPE(0.9, mu = 25, sigma = 0.15, nu = c(1e-12, 0), tau = 2)
//' print(q, digits = 15)
//' if (requireNamespace("gamlss.dist", quietly = TRUE)) {
//'   # ((y/mu)^nu - 1)/(nu * sigma) cancels in gamlss.dist: 5.8e-5 off
//'   print(gamlss.dist::qBCPE(0.9, 25, 0.15, nu = 1e-12, tau = 2), digits = 15)
//' }
//'
//' @seealso \code{\link{fdBCPE}}, \code{\link{fpBCPE}}, \code{\link{frBCPE}} for
//' the other BCPE functions, and \code{\link[gamlss.dist]{BCPE}} for the original
//' implementation.
//'
//' @export
// [[Rcpp::export]]
NumericVector fqBCPE(const NumericVector& p,
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

  // Validate after recycling. NaN/NA inputs are handled in the compute loop below
  // (mapped to NA); skip the range checks for them.
  for (int i = 0; i < n; i++)
  {
    if (any_nan5(recycled, i)) continue;
    check_prob(recycled.vec1[i], log_p, 1.0, "p must be between 0 and 1");
    check_bcpe_params(recycled.vec2[i], recycled.vec3[i], recycled.vec4[i], recycled.vec5[i]);
  }

  NumericVector out(n);

  SIMD_HINT
  for (int i = 0; i < n; i++)
  {
    // fqBCPE_scalar maps NaN/NA in any argument to NA_REAL, so a NaN p never
    // reaches R::qgamma.
    out[i] = fqBCPE_scalar(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i],
                           recycled.vec4[i], recycled.vec5[i], lower_tail, log_p);
  }

  warn_if_nan_produced(out, recycled);
  return out;
}


// ---------------------------------------------------------------------------
// BCPEo: gamlss.dist's BCPEo is the same distribution as BCPE (its d/p/q/r
// functions are line for line BCPE's; the gamlss families differ only in the link
// for mu). These keep their signatures and call the BCPE wrappers, so the results
// are identical() and every fix of the BCPE kernels applies to them.
// ---------------------------------------------------------------------------

//' @rdname fdBCPE
//' @export
// [[Rcpp::export]]
NumericVector fdBCPEo(const NumericVector& x,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu,
                      const NumericVector& tau,
                      const bool& log_ = false)
{
  return fdBCPE(x, mu, sigma, nu, tau, log_);
}

//' @rdname fpBCPE
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
  return fpBCPE(q, mu, sigma, nu, tau, lower_tail, log_p);
}

//' @rdname fqBCPE
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
  return fqBCPE(p, mu, sigma, nu, tau, lower_tail, log_p);
}

// NOTE: there is deliberately no vectorised, Rcpp-exported frBCPE() here. The
// exported frBCPE() (and frBCPEo()) is the R implementation in R/rng_distr.R, which
// draws from dqrng::dqrunif() and inverts fqBCPE() (see the note at the end of
// src/distr_ZINBI.cpp for why a same-named C++ export is avoided). The per-element
// frBCPE_scalar() is available (inline) in inst/include/distr_BCPE.h for a
// LinkingTo: CKutils consumer, takes a caller-supplied uniform, and agrees with
// frBCPE() draw for draw under one dqset.seed(). See the parity tests in
// inst/tinytest/test-rng_distr.R.
