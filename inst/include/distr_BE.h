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

#ifndef DISTR_BE_H
#define DISTR_BE_H

// The beta distribution in gamlss.dist's BE parametrisation: mean mu in (0, 1)
// and dispersion sigma in (0, 1), variance sigma^2 * mu * (1 - mu). Header-only,
// inline scalar kernels, the single definition shared by src/distr_BE.cpp and by
// any package that declares LinkingTo: CKutils.
//
// The shapes are
//     k = (1 - sigma^2) / sigma^2,  shape1 = mu * k,  shape2 = (1 - mu) * k,
// evaluated in exactly that order, one operator at a time as R evaluates
//     k <- (1 - sigma^2) / sigma^2; a <- mu * k; b <- (1 - mu) * k
// (R's sigma^2 is sigma * sigma), and handed to R's own dbeta / pbeta / qbeta. So
// the kernels give, bit for bit, what that R code with stats::dbeta / pbeta /
// qbeta gives -- the NA-tolerant BE that IMPACTncdEngland2026 shipped in its own
// R/beta_dist.R (2026-11-17) and that a model's free-sugar fraction is drawn with:
// moving the functions to CKutils keeps the model's draws.
//
// gamlss.dist's dBE / pBE / qBE compute the same shapes in another order
//     a = mu * (1 - sigma^2) / (sigma^2),  b = a * (1 - mu) / mu,
// which differs by at most 2 ulp (relative 4.4e-16) in a shape: on 2e6 random
// (mu, sigma) a shape differed on 35 % (shape1) and 46 % (shape2) of the rows,
// and a quantile by up to 2.3e-15 where mu and sigma lie in (0.05, 0.95), more
// where a shape is small (up to ~1e-13 below 1e-2, ~1e-11 below 1e-4: qbeta's own
// precision there). Outside the shapes the gamlss.dist functions
// also differ by design (see the R documentation of fqBE): qBE snaps p below 1e-10
// to 0 and above 1 - 1e-10 to 1, its lower.tail = FALSE returns the lower-tail
// quantile (gamlss.dist 6.1.11), pBE / dBE clip at the support ignoring the tail
// and log scale, and every gamlss.dist function stops on an NA parameter.
//
// Limits, R's own (stats::qbeta and gamlss.dist::qBE share them), all where a shape is
// small (sigma near 1 with mu near 0 or 1); measured 2026-11-18 on 2e6 random rows
// (IMPACTncdEngland2026 claude_process/ckutils_be/03_doc_figures.R): qbeta warns "full
// precision may not have been achieved" only where the smaller shape is below 1e-2 (on
// 2 % of such rows at 1e-3 to 1e-2, 11 % at 1e-4 to 1e-3); there the mass crowds so
// close to 0 or 1 that a quantile on the natural scale can be exactly 0 or 1 (the other
// tail or the log scale reaches it), or a value slightly outside [0, 1] -- up to ~1e-13
// where the smaller shape is 1e-3 to 1e-2, ~1e-12 down to 1e-4, ~1e-11 below (2e7 rows)
// -- most often WITHOUT a warning. Nothing of this where the smaller shape is 1e-2 or more. On
// IMPACTncdEngland2026's free-sugar fraction table (1.77e6 rows, sigma 0.16-0.68, every
// shape >= 0.26): no warning, no quantile outside [0, 1], and |F(Q(p)) - p| at most
// 1.5e-14 for uniform p (99.9th percentile 1.3e-15), 8.6e-11 at p = 1 - 1e-6 (Q rounded
// to a double where the density is large).
//
// Contract (the vectorised wrappers check it; the kernels validate nothing):
// 0 < mu < 1 and 0 < sigma < 1. An NA or NaN in any argument gives NA_REAL.
// Outside the contract R's beta functions return NaN or a number the
// parametrisation does not describe; nothing hangs.

// R:: (the d/p/q wrappers) is Rcpp's, so the header includes Rcpp itself, as the
// other distr_*.h do: a LinkingTo consumer may include it first.
#include <Rcpp.h>
#include <Rmath.h>
#include <R_ext/Arith.h>

// Returns x rounded to double, so the product sigma * sigma cannot be fused with
// the subtraction 1 - sigma^2 into one FMA: R evaluates each operator separately,
// and a fused 1 - sigma * sigma is rounded once instead of twice. The same device
// as bcpe_rnd() in distr_BCPE.h: an empty asm statement through which the value
// passes in memory on every GNU-compatible compiler (no feature macro covers every
// flag that lets the compiler contract a * b + c), a volatile copy elsewhere.
inline double be_rnd(double x) {
#if defined(__GNUC__) || defined(__clang__)
    __asm__ volatile("" : "+m"(x));
    return x;
#else
    volatile double v = x;
    return v;
#endif
}

// The two shapes of BE(mu, sigma), in R's order of evaluation (see above).
inline void be_shapes(const double mu, const double sigma, double& shape1, double& shape2) {
    const double s2 = be_rnd(sigma * sigma);
    const double k  = (1.0 - s2) / s2;
    shape1 = mu * k;
    shape2 = (1.0 - mu) * k;
}

// Density. dbeta's own value everywhere: 0 below 0 and above 1 (-Inf on the log
// scale), and at x = 0 or 1 the beta density's value there (Inf when the shape at
// that end is below 1).
inline double fdBE_scalar(const double& x,
                          const double& mu,
                          const double& sigma,
                          const bool& log_p) {
    if (ISNAN(x) || ISNAN(mu) || ISNAN(sigma)) return NA_REAL;
    double a, b;
    be_shapes(mu, sigma, a, b);
    return R::dbeta(x, a, b, log_p);
}

// CDF, on the requested tail and scale (pbeta's: q < 0 and q >= 1 are on the
// requested tail and scale too).
inline double fpBE_scalar(const double& q,
                          const double& mu,
                          const double& sigma,
                          const bool& lower_tail,
                          const bool& log_p) {
    if (ISNAN(q) || ISNAN(mu) || ISNAN(sigma)) return NA_REAL;
    double a, b;
    be_shapes(mu, sigma, a, b);
    return R::pbeta(q, a, b, lower_tail, log_p);
}

// Quantile, on the requested tail and scale (qbeta's: the upper tail is
// inverted directly, never as 1 - p).
inline double fqBE_scalar(const double& p,
                          const double& mu,
                          const double& sigma,
                          const bool& lower_tail,
                          const bool& log_p) {
    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma)) return NA_REAL;
    double a, b;
    be_shapes(mu, sigma, a, b);
    return R::qbeta(p, a, b, lower_tail, log_p);
}

// One draw from a caller-supplied uniform u in (0, 1): the lower-tail quantile at
// u, as frBE() does with dqrng::dqrunif(), so the two agree draw for draw under
// one dqset.seed().
inline double frBE_scalar(const double& u,
                          const double& mu,
                          const double& sigma) {
    return fqBE_scalar(u, mu, sigma, true, false);
}

#endif // DISTR_BE_H
