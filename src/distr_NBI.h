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

#ifndef DISTR_NBI_H
#define DISTR_NBI_H

#include <Rcpp.h>
#include <math.h>
#include <Rmath.h>
#include "recycling_helpers.h"

// Enable vectorization hints for modern compilers
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif

using namespace Rcpp;

// Basic NBI distribution functions (scalar versions)
double fdNBI_scalar(const int& x,
                    const double& mu,
                    const double& sigma,
                    const bool& log_p);

double fpNBI_scalar(const int& q,
                    const double& mu,
                    const double& sigma,
                    const bool& lower_tail,
                    const bool& log_p);

int fqNBI_scalar(const double& p,
                 const double& mu,
                 const double& sigma,
                 const bool& lower_tail,
                 const bool& log_p);

int frNBI_scalar(const double& mu,
                 const double& sigma);

// Basic NBI distribution functions (vectorized versions)
NumericVector fdNBI(const NumericVector& x,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const bool& log_p);

NumericVector fpNBI(const NumericVector& q,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const bool& lower_tail,
                    const bool& log_p);

IntegerVector fqNBI(const NumericVector& p,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const bool& lower_tail,
                    const bool& log_p);

IntegerVector frNBI(const int& n,
                    const NumericVector& mu,
                    const NumericVector& sigma);

#endif // DISTR_NBI_H
