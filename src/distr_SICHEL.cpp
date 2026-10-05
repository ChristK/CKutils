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
#include <limits>
#include "recycling_helpers.h"
#include "distr_NBI.h"
#include "distr_SICHEL.h"   // canonical header-only scalar definitions
#include "distr_search.h"   // ck_search_gives_up(), CK_SEARCH_MAX
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
        // NA/NaN or a count too large to convert to int -> NA. See count_to_int()
        // in recycling_helpers.h: the unguarded cast is out-of-range float-to-int
        // undefined behaviour and it is not benign on either x86-64 or AArch64.
        int xi;
        if (!count_to_int(recycled.vec1[i], xi)) {
            logfy[i] = NA_REAL;
            continue;
        }
        logfy[i] = fdSICHEL_scalar(xi, recycled.vec2[i], recycled.vec3[i],
                                   recycled.vec4[i], log_p);
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
        // NA/NaN or a count too large to convert to int -> NA. See count_to_int()
        // in recycling_helpers.h: the unguarded cast is out-of-range float-to-int
        // undefined behaviour and it is not benign on either x86-64 or AArch64.
        int qi;
        if (!count_to_int(recycled.vec1[i], qi)) {
            cdf[i] = NA_REAL;
            continue;
        }
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

namespace {

// Is F(N) = P(Y <= N) < p, for Y ~ SICHEL(mu, sigma, nu)? A sufficient test
// that costs O(1) (5-55 pieces below), so that the quantile search reports a
// quantile beyond the int range at once instead of after 2^31 terms.
//
// Y is a Poisson mixture: Y | g ~ Poisson(mu g), and V = log(c g) has density
//     h(v) = exp(phi(v)),  phi(v) = nu v - cosh(v)/sigma - log(2 K_nu(1/sigma)),
// c = K_{nu+1}(1/sigma) / K_nu(1/sigma) (so E[g] = 1; DLMF 10.32.9 gives the
// normalisation). phi''(v) = -cosh(v)/sigma < 0 for every real nu and sigma > 0:
// h is log-concave, so tangent lines of phi lie above it and chords below it.
// For theta = N (1 + d) > N and x = log(c theta / mu), as P(Pois(l) <= N) falls
// in l:
//   F(N)     <= P(V <= x) + P(Pois(theta) <= N)                     (left)
//   1 - F(N) >= P(V > x) * (1 - P(Pois(theta) <= N))                 (right)
//   P(Pois(theta) <= N) <= exp(-N (d - log1p(d)))                   (Chernoff)
// When x is left of the mode of V, P(V <= x) is bounded above by integrating
// exp(tangent at each piece's midpoint) over pieces of (-inf, x], the last
// one (-inf, t] by exp(phi(t)) / phi'(t); otherwise P(V > x) is bounded below by
// integrating exp(chord) over pieces of [x, inf). Either march runs into a
// decaying tail and stops once the rest is negligible. Every partition gives a
// valid bound; the piece widths only set how tight it is.
// Returns true only when F(N) < p - m, m = 1e-3 min(p, 1 - p) + 1e-6 p: the
// scan's own rounding (relative error up to ~ N 2^-53 = 2.4e-7 after 2^31
// terms; ~7e-9 measured) can then never be what separates F(N) from p, so the
// answer is NA only where the scan would also have given NA.
struct SichelLogV {
    double nu, sigma, lnorm;   // phi(v) = nu v - 2 sinh(v/2)^2 / sigma - lnorm
    double phi(double v) const { const double s = std::sinh(0.5 * v); return nu * v - 2.0 * s * s / sigma - lnorm; }
    double dphi(double v) const { return nu - std::sinh(v) / sigma; }
    double curv(double v) const { return std::cosh(v) / sigma; }       // -phi''
};
inline double ck_lse(double a, double b) {                           // log(e^a + e^b)
    if (a == -INFINITY) return b;
    if (b == -INFINITY) return a;
    return std::max(a, b) + std::log1p(std::exp(-std::fabs(a - b)));
}
inline double ck_lsinhc(double z) {                                  // log(sinh(z) / z)
    z = std::fabs(z);
    return (z < 1e-4) ? z * z / 6.0 : z + std::log1p(-std::exp(-2.0 * z)) - std::log(2.0 * z);
}
inline double ck_lexpm1c(double d) {                                 // log((e^d - 1) / d)
    if (std::fabs(d) < 1e-8) return 0.5 * d;
    return (d > 0.0) ? d + std::log(-std::expm1(-d) / d) : std::log(std::expm1(d) / d);
}
constexpr double CK_SB_KAPPA = 0.25, CK_SB_HMAX = 0.5, CK_SB_TOL = 1e-7;
constexpr int CK_SB_KMAX = 4000;

// log of an upper bound on P(V <= x) (+Inf: none)
inline double sichel_log_cdf_upper(const SichelLogV& d, double x) {
    double lS = -INFINITY, t = x;
    for (int k = 0; k < CK_SB_KMAX; ++k) {
        const double b = d.dphi(t);
        if (b > 0.0) {
            const double lT = d.phi(t) - std::log(b);
            if (lT == -INFINITY) return lS;
            if (lT <= lS + std::log(CK_SB_TOL) || b * b >= 1e8 * d.curv(t)) return ck_lse(lS, lT);
        }
        const double h = std::min(CK_SB_HMAX, CK_SB_KAPPA /
            std::sqrt(d.curv(std::max(std::fabs(t), std::fabs(t - CK_SB_HMAX)))));
        if (!(h > 0.0)) break;
        const double a = t - 0.5 * h;
        const double lp = d.phi(a) + std::log(h) + ck_lsinhc(0.5 * h * d.dphi(a));
        if (!std::isnan(lp)) lS = ck_lse(lS, lp);
        t -= h;
    }
    const double b = d.dphi(t);
    return (b > 0.0) ? ck_lse(lS, d.phi(t) - std::log(b)) : INFINITY;
}

// log of a lower bound on P(V > x)
inline double sichel_log_sf_lower(const SichelLogV& d, double x) {
    double lS = -INFINITY, s = x, ps = d.phi(s);
    for (int k = 0; k < CK_SB_KMAX; ++k) {
        const double h = std::min(CK_SB_HMAX, CK_SB_KAPPA /
            std::sqrt(d.curv(std::max(std::fabs(s), std::fabs(s + CK_SB_HMAX)))));
        if (!(h > 0.0)) break;
        const double s2 = s + h, ps2 = d.phi(s2);
        if (ps == -INFINITY && ps2 == -INFINITY) break;
        if (ps != -INFINITY && ps2 != -INFINITY) {
            const double lp = ps + std::log(h) + ck_lexpm1c(ps2 - ps);
            if (!std::isnan(lp)) lS = ck_lse(lS, lp);
        }
        const double b2 = d.dphi(s2);
        if (b2 < 0.0 && ps2 - std::log(-b2) <= lS + std::log(CK_SB_TOL)) break;
        s = s2; ps = ps2;
    }
    return lS;
}

inline bool sichel_cdf_below(double N, double p, double mu, double sigma, double nu, double cvec) {
    if (!(p > 0.0 && p < 1.0) || !(N >= 1.0) || !std::isfinite(mu) || !(cvec > 0.0) || !std::isfinite(cvec))
        return false;
    const double ks = R::bessel_k(1.0 / sigma, nu, 2.0);          // K_nu(1/sigma) e^{1/sigma}
    if (!(ks > 0.0) || !std::isfinite(ks)) return false;
    const SichelLogV d{nu, sigma, std::log(2.0 * ks)};
    // theta = N (1 + delta) with the Chernoff term ~ 1e-3 p
    const double L = std::max(1.0, std::log(1e3) - std::log(p));
    double delta = std::sqrt(2.0 * L / N);
    delta *= 1.0 + delta / 3.0;
    const double lpois = -N * (delta - std::log1p(delta));
    const double x = std::log(cvec) + std::log(N) + std::log1p(delta) - std::log(mu);
    if (!std::isfinite(x)) return false;
    const double m = 1e-3 * std::min(p, 1.0 - p) + 1e-6 * p;
    if (x <= std::asinh(sigma * nu)) {                              // left of the mode of V
        const double lU = ck_lse(sichel_log_cdf_upper(d, x), lpois);
        return lU < std::log(p - m);
    }
    const double lSV = sichel_log_sf_lower(d, x);
    if (!(lSV < 0.0)) return false;                                 // numerical failure: no claim
    return lSV + std::log1p(-std::exp(lpois)) > std::log((1.0 - p) + m);
}

}  // namespace

// Internal test hook for sichel_cdf_below(), not part of the package API: no roxygen
// block and a dot-prefixed R name (CKutils:::.sichel_cdf_below), like
// .frNBI_scalar_vec in distr_rng_scalar_bridge.cpp. The arguments are recycled to the
// longest length. inst/tinytest/test-fSICHEL.R uses it at a small N to check that the
// bound never claims F(N) < p where fpSICHEL(N) >= p: the quantile search itself could
// only show that with a scan of 2^31 terms.
// [[Rcpp::export(name = ".sichel_cdf_below")]]
LogicalVector sichel_cdf_below_r(NumericVector N, NumericVector p, NumericVector mu,
                                 NumericVector sigma, NumericVector nu) {
    auto recycled = recycle_vectors(N, p, mu, sigma, nu);
    const int n = recycled.n;
    LogicalVector out(n);
    for (int i = 0; i < n; i++) {
        const double sigmai = recycled.vec4[i];
        const double nui = recycled.vec5[i];
        out[i] = sichel_cdf_below(recycled.vec1[i], recycled.vec2[i], recycled.vec3[i],
                                  sigmai, nui, compute_cvec(sigmai, nui));
    }
    return out;
}

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

    // A quantile beyond the int range is reported at once, instead of after
    // scanning CK_SEARCH_MAX terms (some 40 s, which cannot be interrupted). By
    // Markov's inequality P(Y > N) <= mu / (N + 1), so F(N) < p needs
    // mu > (1 - p)(N + 1); only then is the bound sichel_cdf_below() evaluated,
    // so a search with a small mu never pays for it.
    if (mu > (1.0 - p) * (CK_SEARCH_MAX + 1.0) &&
        sichel_cdf_below(CK_SEARCH_MAX, p, mu, sigma, nu, cvec)) {
        return NA_INTEGER;
    }

    const double alpha = compute_alpha(sigma, mu, cvec);
    const double lbes = compute_lbes(alpha, nu);
    
    // Initial density and CDF at y=0
    double tynew_prev = (mu / cvec) * pow(1.0 + 2.0 * sigma * mu / cvec, -0.5) * exp(lbes);
    double lpnew_prev = -nu * log(sigma * alpha) + log_bessel_k(alpha, nu) -
                        log_bessel_k(1.0/sigma, nu);

    double term_prev = exp(lpnew_prev);
    if (!std::isfinite(term_prev)) {
        return NA_INTEGER;
    }
    double cdf = term_prev;

    if (cdf >= p) {
        return 0;
    }

    // Incremental search. No fixed cap: ck_search_gives_up() (distr_search.h)
    // ends a search that cannot reach p, and NA_INTEGER is returned rather than
    // a number
    const double sigma_alpha_cvec_sq = pow(mu / (sigma * alpha * cvec), 2.0);

    for (int j = 1; j <= CK_SEARCH_MAX; j++) {
        double tynew_curr = (cvec * sigma * (2.0 * (j + nu) / mu) + (1.0 / tynew_prev)) *
                           sigma_alpha_cvec_sq;
        double lpnew_curr = lpnew_prev + log(tynew_prev) - log(static_cast<double>(j));
        const double term = exp(lpnew_curr);

        if (ck_search_gives_up(term, term_prev, cdf)) {
            return NA_INTEGER;
        }
        cdf += term;

        if (cdf >= p) {
            return j;
        }

        tynew_prev = tynew_curr;
        lpnew_prev = lpnew_curr;
        term_prev = term;
    }

    return NA_INTEGER;
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
    
    // Validate parameters after recycling. check_prob() ranges p on whichever
    // scale the caller supplied it, so the log_p case is validated rather than
    // skipped.
    for (int i = 0; i < n; i++)
    {
      check_prob(recycled.vec1[i], log_p, 1.0001, "p must be between 0 and 1");
      if (recycled.vec2[i] <= 0.0)
        stop("mu must be greater than 0");
      if (recycled.vec3[i] <= 0.0)
        stop("sigma must be greater than 0");
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
    bool not_found = false;  // a non-NA input gave NA
    
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

        // The quantile is Inf, which an int cannot hold. Assigning R_PosInf to
        // an IntegerVector element was an out-of-range float-to-int conversion:
        // undefined behaviour, that gives INT_MIN (= NA) on x86-64 and
        // saturates to INT_MAX on AArch64.
        if (pi + 1e-09 >= 1.0) {
            QQQ[i] = NA_INTEGER;
            not_found = true;
            continue;
        }

        // Use optimized incremental search (NA_INTEGER: not found)
        QQQ[i] = fqSICHEL_search(pi, mui, sigmai, nui);
        if (QQQ[i] == NA_INTEGER) not_found = true;
    }

    if (not_found)
        warning("NAs produced: a quantile is infinite (p = 1) or was not found "
                "(the cumulative probability stops short of p)");
    return QQQ;
}
