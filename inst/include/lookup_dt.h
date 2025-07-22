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

#ifndef LOOKUP_DT_H
#define LOOKUP_DT_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations
IntegerVector fct_to_int_cpp(SEXP x, bool inplace);
IntegerVector starts_from_1_cpp(DataFrame tbl, CharacterVector on, int i, List min_lookup, List cardinality);
SEXP dtsubset(SEXP x, SEXP rows, SEXP cols);

#endif // LOOKUP_DT_H
