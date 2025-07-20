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

#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

//' Shift Values by ID Groups
//' 
//' High-performance implementations for shifting/lagging values within groups 
//' defined by ID variables. These functions are optimised for panel data and 
//' time series analysis where values need to be shifted within groups while 
//' preserving group boundaries.
//' 
//' @param x Vector of values to be shifted. Can be numeric, integer, logical, or character.
//' @param lag Integer specifying the lag amount. Positive values create standard lags 
//'   (shift values forward in time), negative values create leads (shift values 
//'   backward in time).
//' @param replace Replacement value for positions that cannot be filled due to 
//'   shifting. For logical vectors, this should be a logical vector with one element.
//' @param id Integer vector of group identifiers. Must be the same length as x. 
//'   Should be sorted for optimal performance. Values are only shifted within 
//'   the same ID group.
//' 
//' @details
//' These functions implement efficient group-aware shifting operations commonly 
//' needed in panel data analysis. Key features include:
//' 
//' \itemize{
//'   \item \strong{Group preservation}: Values are never shifted across different ID groups
//'   \item \strong{Boundary handling}: Positions at group boundaries are filled with replacement values
//'   \item \strong{Bidirectional shifting}: Supports both lags (positive) and leads (negative)
//'   \item \strong{Type preservation}: Maintains original data types and attributes (e.g., factor levels)
//'   \item \strong{High performance}: Optimized C++ implementation with minimal memory allocation
//' }
//' 
//' For positive lag values:
//' \itemize{
//'   \item Values are shifted forward (standard lag operation)
//'   \item First `lag` positions in each group are filled with replacement value
//'   \item `x[i]` becomes `x[i-lag]` if `id[i] == id[i-lag]`, otherwise replacement value
//' }
//' 
//' For negative lag values (leads):
//' \itemize{
//'   \item Values are shifted backward (lead operation) 
//'   \item Last `abs(lag)` positions in each group are filled with replacement value
//'   \item `x[i]` becomes `x[i+abs(lag)]` if `id[i] == id[i+abs(lag)]`, otherwise replacement value
//' }
//' 
//' @return Vector of the same type and length as input `x`, with values shifted 
//'   according to the specified lag and group structure.
//' 
//' @examples
//' # Example data with two groups
//' id <- c(1, 1, 1, 2, 2, 2)
//' values <- c(10, 20, 30, 40, 50, 60)
//' 
//' # Lag by 1 (shift forward)
//' shift_bypidNum(values, lag = 1, replace = NA_real_, id = id)
//' # Result: [NA, 10, 20, NA, 40, 50]
//' 
//' # Lead by 1 (shift backward) 
//' shift_bypidNum(values, lag = -1, replace = NA_real_, id = id)
//' # Result: [20, 30, NA, 50, 60, NA]
//' 
//' # Integer data with factor preservation
//' factors <- factor(c("A", "B", "C", "A", "B", "C"))
//' factors_int <- as.integer(factors)
//' result <- shift_bypidInt(factors_int, lag = 1, replace = NA_integer_, id = id)
//' # Maintains factor attributes
//' 
//' # Logical data
//' logical_vals <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
//' shift_bypidBool(logical_vals, lag = 1, replace = FALSE, id = id)
//' 
//' # Character data
//' char_vals <- c("a", "b", "c", "d", "e", "f")
//' shift_bypidStr(char_vals, lag = 1, replace = "missing", id = id)
//' 
//' @seealso 
//' \code{\link[data.table]{shift}} for data.table's shift function,
//' \code{\link[dplyr]{lag}} and \code{\link[dplyr]{lead}} for dplyr alternatives.
//' 
//' @name shift_bypid
//' @rdname shift_bypid

//' Shift Numeric Values by ID Groups
//' 
//' @description
//' Efficiently shifts numeric values within groups defined by ID variables.
//' Optimized for panel data and time series analysis.
//' 
//' @param x Numeric vector of values to be shifted.
//' @param lag Integer specifying the lag amount. Positive values create standard lags 
//'   (shift values forward in time), negative values create leads (shift values 
//'   backward in time).
//' @param replace Numeric replacement value for positions that cannot be filled.
//' @param id Integer vector of group identifiers. Must be the same length as x. 
//'   Should be sorted for optimal performance. Values are only shifted within 
//'   the same ID group.
//' 
//' @return Numeric vector of the same length as input, with values shifted 
//'   according to the specified lag and group structure.
//' 
//' @export
//' @rdname shift_bypid
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

//' Shift Integer Values by ID Groups
//' 
//' @description
//' Efficiently shifts integer values within groups defined by ID variables.
//' Preserves factor attributes when present, making it suitable for categorical data.
//' 
//' @param x Integer vector of values to be shifted. Factor attributes are preserved.
//' @param lag Integer specifying the lag amount. Positive values create standard lags 
//'   (shift values forward in time), negative values create leads (shift values 
//'   backward in time).
//' @param replace Integer replacement value for positions that cannot be filled.
//' @param id Integer vector of group identifiers. Must be the same length as x. 
//'   Should be sorted for optimal performance. Values are only shifted within 
//'   the same ID group.
//' 
//' @details
//' This function automatically detects and preserves factor attributes including
//' levels and class information. This makes it particularly useful for shifting
//' categorical variables that have been converted to integers.
//' 
//' @return Integer vector of the same length as input, with values shifted 
//'   according to the specified lag and group structure. Factor attributes 
//'   are preserved if present in the input.
//' 
//' @export
//' @rdname shift_bypid
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


//' Shift Logical Values by ID Groups
//' 
//' @description
//' Efficiently shifts logical (boolean) values within groups defined by ID variables.
//' Designed for binary indicators and flag variables in panel data.
//' 
//' @param x Logical vector of values to be shifted.
//' @param lag Integer specifying the lag amount. Positive values create standard lags 
//'   (shift values forward in time), negative values create leads (shift values 
//'   backward in time).
//' @param replace Logical vector with one element specifying the replacement value 
//'   for positions that cannot be filled.
//' @param id Integer vector of group identifiers. Must be the same length as x. 
//'   Should be sorted for optimal performance. Values are only shifted within 
//'   the same ID group.
//' 
//' @details
//' The replacement parameter must be a logical vector (even with just one element)
//' to maintain type consistency. This is particularly useful for shifting binary
//' indicators, treatment flags, or event occurrence variables in longitudinal data.
//' 
//' @return Logical vector of the same length as input, with values shifted 
//'   according to the specified lag and group structure.
//' 
//' @export
//' @rdname shift_bypid
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

//' Shift Character Values by ID Groups
//' 
//' @description
//' Efficiently shifts character (string) values within groups defined by ID variables.
//' Ideal for text data, labels, and categorical variables in panel data analysis.
//' 
//' @param x Character vector of values to be shifted.
//' @param lag Integer specifying the lag amount. Positive values create standard lags 
//'   (shift values forward in time), negative values create leads (shift values 
//'   backward in time).
//' @param replace String replacement value for positions that cannot be filled.
//' @param id Integer vector of group identifiers. Must be the same length as x. 
//'   Should be sorted for optimal performance. Values are only shifted within 
//'   the same ID group.
//' 
//' @details
//' This function handles character data efficiently by converting the replacement
//' value to the appropriate Rcpp::String type internally. It's particularly useful
//' for shifting categorical variables, labels, or any text-based data in 
//' longitudinal datasets.
//' 
//' @return Character vector of the same length as input, with values shifted 
//'   according to the specified lag and group structure.
//' 
//' @export
//' @rdname shift_bypid
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



