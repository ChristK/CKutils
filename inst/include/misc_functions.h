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

#ifndef MISC_FUNCTIONS_H
#define MISC_FUNCTIONS_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations for misc_functions.cpp

// Quantile computation functions
NumericVector fquantile(NumericVector x, NumericVector probs, bool na_rm = true);
List fquantile_byid(NumericVector x, NumericVector q, StringVector id, 
                   bool rounding = false, bool na_rm = true);

// Counting and proportion functions  
int count_if(LogicalVector x, bool na_rm = false);
double prop_if(LogicalVector x, bool na_rm = false);

// Clamping functions
NumericVector fclamp(NumericVector &x, 
                     NumericVector a = NumericVector::create(0.0), 
                     NumericVector b = NumericVector::create(1.0), 
                     const bool &inplace = false);
IntegerVector fclamp_int(IntegerVector &x, int a = 0, int b = 1, const bool &inplace = false);

// Equality and normalization
LogicalVector fequal(const NumericVector &x, const double &tol);
NumericVector fnormalise(const NumericVector &x);

// Interpolation
NumericVector lin_interpolation(const NumericVector &xp,
                               const NumericVector &x0,
                               const NumericVector &x1,
                               const NumericVector &y0,
                               const NumericVector &y1);

// Carry forward/backward functions
IntegerVector carry_forward(IntegerVector &x,
                           const LogicalVector &pid_mrk,
                           const int &y,
                           const bool &byref);

IntegerVector carry_forward_incr(IntegerVector &x, 
                                const LogicalVector &pid_mrk,
                                const bool &recur, 
                                const int &y,
                                const bool &byref);

IntegerVector carry_backward_decr(const IntegerVector &x, 
                                 const LogicalVector &pid_mrk,
                                 const int &y = 0);

// Marker and identification functions
LogicalVector mk_new_simulant_markers(const IntegerVector &pid);

LogicalVector identify_longdead(const IntegerVector &x, const LogicalVector &pid);

IntegerVector identify_invitees(const IntegerVector &elig,
                               const IntegerVector &prev_inv,
                               const NumericVector &prb,
                               const IntegerVector &freq,
                               const LogicalVector &pid);

// Health care and utility functions
IntegerVector hc_effect(const IntegerVector &x,
                       const double &prb_of_continuation,
                       const LogicalVector &pid);

double antilogit(const double &x);

#endif // MISC_FUNCTIONS_H