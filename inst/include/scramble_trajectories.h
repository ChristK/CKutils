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

#ifndef SCRAMBLE_TRAJECTORIES_H
#define SCRAMBLE_TRAJECTORIES_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations
SEXP fscramble_trajectories(NumericVector& x, const LogicalVector& pid,
                           const double& rw_scale,
                           const bool& inplace);

#endif // SCRAMBLE_TRAJECTORIES_H
