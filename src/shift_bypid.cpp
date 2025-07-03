/* IMPACTncdEngl is an implementation of the IMPACTncd framework, developed by Chris
 Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
 Guzman-Castillo, Amandine Robert, and Piotr Bandosz. This work has been
 funded by NIHR  HTA Project: 16/165/01 - IMPACTncdEngl: Health Outcomes
 Research Simulation Environment.  The views expressed are those of the
 authors and not necessarily those of the NHS, the NIHR or the Department of
 Health.

 Copyright (C) 2018-2020 University of Liverpool, Chris Kypridemos

 IMPACTncdEngl is free software; you can redistribute it and/or modify it under
 the terms of the GNU General Public License as published by the Free Software
 Foundation; either version 3 of the License, or (at your option) any later
 version. This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 details. You should have received a copy of the GNU General Public License
 along with this program; if not, see <http://www.gnu.org/licenses/> or write
 to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 Boston, MA 02110-1301 USA. */

#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
NumericVector shift_bypidNum(const NumericVector& x, const int& lag,
                            const double& replace, const IntegerVector& id) {
  // id should be sorted and same length as x
  int n = x.size();
  NumericVector out(n);
  
  if (lag >= 0) {
    // Positive lag (standard lag)
    int fill_end = std::min(lag, n);
    // Fill first positions with replace value
    std::fill(out.begin(), out.begin() + fill_end, replace);
    
    // Copy shifted values where IDs match
    for (int i = lag; i < n; i++) {
      out[i] = (id[i] == id[i-lag]) ? x[i-lag] : replace;
    }
  } else {
    // Negative lag (lead)
    int abs_lag = -lag;
    int copy_end = n - abs_lag;
    
    // Copy shifted values where IDs match
    for (int i = 0; i < copy_end; i++) {
      out[i] = (id[i] == id[i + abs_lag]) ? x[i + abs_lag] : replace;
    }
    
    // Fill last positions with replace value
    std::fill(out.begin() + copy_end, out.end(), replace);
  }
  return out;
  // } else {
  //   stop("This function does not work because number of ids is too small!");
  // }
}

//' @export
// [[Rcpp::export]]
IntegerVector shift_bypidInt(const IntegerVector& x, const int& lag,
                            const int& replace, const IntegerVector& id) {
  // id should be sorted and same length as x
  int n = x.size();
  IntegerVector out(n);
  
  if (lag >= 0) {
    // Positive lag (standard lag)
    int fill_end = std::min(lag, n);
    // Fill first positions with replace value
    std::fill(out.begin(), out.begin() + fill_end, replace);
    
    // Copy shifted values where IDs match
    for (int i = lag; i < n; i++) {
      out[i] = (id[i] == id[i-lag]) ? x[i-lag] : replace;
    }
  } else {
    // Negative lag (lead)
    int abs_lag = -lag;
    int copy_end = n - abs_lag;
    
    // Copy shifted values where IDs match
    for (int i = 0; i < copy_end; i++) {
      out[i] = (id[i] == id[i + abs_lag]) ? x[i + abs_lag] : replace;
    }
    
    // Fill last positions with replace value
    std::fill(out.begin() + copy_end, out.end(), replace);
  }
  
  // Handle factor attributes
  if (x.hasAttribute("class")) {
    CharacterVector tt = x.attr("class");
    if (tt[0] == "factor") { // only works if factor is the first class
      out.attr("levels") = x.attr("levels");
      out.attr("class") = "factor";
    }
  }

  return out;
}


//' @export
// [[Rcpp::export]]
LogicalVector shift_bypidBool(const LogicalVector& x, const int& lag,
                             const LogicalVector& replace, const IntegerVector& id) {
  // id should be sorted and same length as x
  int n = x.size();
  LogicalVector out(n);
  
  // Get the replacement value (first element of replace vector)
  int repl_val = replace[0];
  
  if (lag >= 0) {
    // Positive lag (standard lag)
    int fill_end = std::min(lag, n);
    // Fill first positions with replace value
    for (int i = 0; i < fill_end; i++) {
      out[i] = repl_val;
    }
    
    // Copy shifted values where IDs match
    for (int i = lag; i < n; i++) {
      out[i] = (id[i] == id[i-lag]) ? x[i-lag] : repl_val;
    }
  } else {
    // Negative lag (lead)
    int abs_lag = -lag;
    int copy_end = n - abs_lag;
    
    // Copy shifted values where IDs match
    for (int i = 0; i < copy_end; i++) {
      out[i] = (id[i] == id[i + abs_lag]) ? x[i + abs_lag] : repl_val;
    }
    
    // Fill last positions with replace value
    for (int i = copy_end; i < n; i++) {
      out[i] = repl_val;
    }
  }
  return out;
}

//' @export
// [[Rcpp::export]]
StringVector shift_bypidStr(const CharacterVector& x, const int& lag,
                             const std::string& replace, const IntegerVector& id) {
  // id should be sorted and same length as x
  int n = x.size();
  StringVector out(n);
  
  // Convert replace to Rcpp::String for compatibility with ternary operators
  Rcpp::String replace_str(replace);
  
  if (lag >= 0) {
    // Positive lag (standard lag)
    int fill_end = std::min(lag, n);
    // Fill first positions with replace value
    std::fill(out.begin(), out.begin() + fill_end, replace_str);
    
    // Copy shifted values where IDs match
    for (int i = lag; i < n; i++) {
      out[i] = (id[i] == id[i-lag]) ? x[i-lag] : replace_str;
    }
  } else {
    // Negative lag (lead)
    int abs_lag = -lag;
    int copy_end = n - abs_lag;
    
    // Copy shifted values where IDs match
    for (int i = 0; i < copy_end; i++) {
      out[i] = (id[i] == id[i + abs_lag]) ? x[i + abs_lag] : replace_str;
    }
    
    // Fill last positions with replace value
    std::fill(out.begin() + copy_end, out.end(), replace_str);
  }
  return out;
}



