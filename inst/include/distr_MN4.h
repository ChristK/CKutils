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

#ifndef DISTR_MN4_H
#define DISTR_MN4_H

#include <Rcpp.h>

using namespace Rcpp;

// MN4 scalar functions declarations
double fdMN4_scalar(const int& x,
                    const double& mu,
                    const double& sigma,
                    const double& nu,
                    const bool& log_);

double fpMN4_scalar(const int& q,
                    const double& mu,
                    const double& sigma,
                    const double& nu,
                    const bool& lower_tail,
                    const bool& log_p);

int fqMN4_scalar(const double& p,
                 const double& mu,
                 const double& sigma,
                 const double& nu,
                 const bool& lower_tail,
                 const bool& log_p);

// MN4 vectorized functions declarations
NumericVector fdMN4(const IntegerVector& x,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const NumericVector& nu,
                    const bool& log_);

NumericVector fpMN4(const IntegerVector& q,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const NumericVector& nu,
                    const bool& lower_tail,
                    const bool& log_p);

IntegerVector fqMN4(const NumericVector& p,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const NumericVector& nu,
                    const bool& lower_tail,
                    const bool& log_p);

IntegerVector frMN4(const int& n,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const NumericVector& nu);

#endif // DISTR_MN4_H
