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

#ifndef DISTR_ZISICHEL_H
#define DISTR_ZISICHEL_H

// Header-only API for the Zero-Inflated Sichel (ZISICHEL) distribution.
//
// ZISICHEL defines no scalar functions of its own; its vectorised,
// Rcpp-exported wrappers (declared at the bottom) live in
// src/distr_ZISICHEL.cpp and call the inline SICHEL scalars
// (fpSICHEL_scalar) defined in distr_SICHEL.h.

#include <Rcpp.h>
#include "distr_SICHEL.h"   // ZISICHEL wrappers call the inline SICHEL scalars

// Vectorised, Rcpp-exported wrappers (defined in src/distr_ZISICHEL.cpp)
Rcpp::IntegerVector fqZISICHEL(Rcpp::NumericVector p,
                              const Rcpp::NumericVector& mu,
                              const Rcpp::NumericVector& sigma,
                              const Rcpp::NumericVector& nu,
                              const Rcpp::NumericVector& tau,
                              const bool& lower_tail,
                              const bool& log_p);

Rcpp::NumericVector fpZISICHEL(const Rcpp::NumericVector& q,
                              const Rcpp::NumericVector& mu,
                              const Rcpp::NumericVector& sigma,
                              const Rcpp::NumericVector& nu,
                              const Rcpp::NumericVector& tau,
                              const bool& lower_tail,
                              const bool& log_p);

#endif // DISTR_ZISICHEL_H
