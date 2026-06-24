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

#ifndef DISTR_BCT_H
#define DISTR_BCT_H

// BCT is a continuous distribution implemented purely as vectorised, Rcpp-exported
// functions (defined in src/distr_BCT.cpp); it has no per-element *_scalar API to
// expose header-only. Only the vectorised declarations are provided here, for
// reference. No `using namespace Rcpp;` so a downstream LinkingTo consumer's
// namespace is not polluted.

#include <Rcpp.h>

Rcpp::NumericVector fdBCT(const Rcpp::NumericVector& x,
                         const Rcpp::NumericVector& mu,
                         const Rcpp::NumericVector& sigma,
                         const Rcpp::NumericVector& nu,
                         const Rcpp::NumericVector& tau,
                         const bool& log_);

Rcpp::NumericVector fpBCT(const Rcpp::NumericVector& q,
                         const Rcpp::NumericVector& mu,
                         const Rcpp::NumericVector& sigma,
                         const Rcpp::NumericVector& nu,
                         const Rcpp::NumericVector& tau,
                         const bool& lower_tail,
                         const bool& log_p);

Rcpp::NumericVector fqBCT(const Rcpp::NumericVector& p,
                         const Rcpp::NumericVector& mu,
                         const Rcpp::NumericVector& sigma,
                         const Rcpp::NumericVector& nu,
                         const Rcpp::NumericVector& tau,
                         const bool& lower_tail,
                         const bool& log_p);

#endif // DISTR_BCT_H
