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

#ifndef DISTR_BCPEO_H
#define DISTR_BCPEO_H

// gamlss.dist's BCPEo is the same distribution as its BCPE: dBCPEo/pBCPEo/qBCPEo/
// rBCPEo are line for line dBCPE/pBCPE/qBCPE/rBCPE (rBCPEo even calls qBCPE), and
// the gamlss families BCPE() and BCPEo() differ only in the default link for mu,
// which matters when fitting, not when evaluating d/p/q/r. So the BCPEo scalar API
// is a set of inline aliases of the BCPE kernels in distr_BCPE.h, which carries the
// contract (no validation; NaN -> NA; the edges of x, q and p; the gamlss region
// and the stable path; call from a thread that may call into R). A downstream
// package can `LinkingTo: CKutils` and `#include <distr_BCPEo.h>` (which includes
// distr_BCPE.h) and call either name. No `using namespace Rcpp;`, so a LinkingTo
// consumer's namespace is not polluted.
//
// The count caller contract documented in recycling_helpers.h does not apply
// here: BCPEo is continuous, takes its quantiles as doubles, and converts nothing
// to int.

#include "distr_BCPE.h"

// BCPEo density scalar: fdBCPE_scalar.
inline double fdBCPEo_scalar(const double& x,
                             const double& mu,
                             const double& sigma,
                             const double& nu,
                             const double& tau,
                             const bool& log_p) {
    return fdBCPE_scalar(x, mu, sigma, nu, tau, log_p);
}

// BCPEo CDF scalar: fpBCPE_scalar.
inline double fpBCPEo_scalar(const double& q,
                             const double& mu,
                             const double& sigma,
                             const double& nu,
                             const double& tau,
                             const bool& lower_tail,
                             const bool& log_p) {
    return fpBCPE_scalar(q, mu, sigma, nu, tau, lower_tail, log_p);
}

// BCPEo quantile scalar: fqBCPE_scalar.
inline double fqBCPEo_scalar(const double& p,
                             const double& mu,
                             const double& sigma,
                             const double& nu,
                             const double& tau,
                             const bool& lower_tail,
                             const bool& log_p) {
    return fqBCPE_scalar(p, mu, sigma, nu, tau, lower_tail, log_p);
}

// BCPEo random generation scalar: frBCPE_scalar (inverts the CDF at a
// caller-supplied uniform u in [0, 1)).
inline double frBCPEo_scalar(const double& u,
                             const double& mu,
                             const double& sigma,
                             const double& nu,
                             const double& tau) {
    return frBCPE_scalar(u, mu, sigma, nu, tau);
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_BCPE.cpp; identical()
// results to fdBCPE/fpBCPE/fqBCPE)
Rcpp::NumericVector fdBCPEo(const Rcpp::NumericVector& x,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& log_);

Rcpp::NumericVector fpBCPEo(const Rcpp::NumericVector& q,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& lower_tail,
                           const bool& log_p);

Rcpp::NumericVector fqBCPEo(const Rcpp::NumericVector& p,
                           const Rcpp::NumericVector& mu,
                           const Rcpp::NumericVector& sigma,
                           const Rcpp::NumericVector& nu,
                           const Rcpp::NumericVector& tau,
                           const bool& lower_tail,
                           const bool& log_p);

#endif // DISTR_BCPEO_H
