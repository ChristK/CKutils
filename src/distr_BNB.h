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

#ifndef DISTR_BNB_H
#define DISTR_BNB_H

#include <Rcpp.h>

// BNB scalar functions declarations
double fdBNB_scalar(const int& x,
                    const double& mu,
                    const double& sigma,
                    const double& nu,
                    const bool& log);

double fpBNB_scalar(const int& q,
                    const double& mu,
                    const double& sigma,
                    const double& nu,
                    const bool& lower_tail,
                    const bool& log_p);

double fqBNB_scalar(const double& p,
                    const double& mu,
                    const double& sigma,
                    const double& nu,
                    const bool& lower_tail,
                    const bool& log_p);

#endif // DISTR_BNB_H
