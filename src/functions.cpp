/* CKutils: an R package with some utility functions I use regularly
Copyright (C) 2018  Chris Kypridemos

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
Fifth Floor, Boston, MA 02110-1301  USA.*/

#include <Rcpp.h>
//#include <string>
//#include <cmath>
using namespace Rcpp;

//quantile implementation for default R method (type 7)
//' @export
// [[Rcpp::export]]
NumericVector fquantile(NumericVector x, NumericVector probs, bool na_rm = true) {
  if (all(is_na(x))) {
    NumericVector out(probs.size(), NA_REAL);
    return(out);
  }
  if (na_rm) x = na_omit(x);
  const int n = x.size();
  NumericVector out(probs.size());
  IntegerVector ii(probs.size());
  NumericVector h(probs.size());
  NumericVector index = 1 + (n - 1) * probs;
  NumericVector lo = floor(index); //floor
  //ceiling
  NumericVector hi = ceiling(index);
  for(int i = 0; i < probs.size(); i++)
  {//catch corner case when index element is int and ceiling = floor
    h[i] = index[i] - lo[i];
  }
  std::sort(x.begin(), x.end());
  out = x[as<IntegerVector>(lo) - 1];
  if (all(is_na(ii)))
  {
    return(out);
  } else
  {
    x = x[as<IntegerVector>(hi) - 1];
    for(int i = 0; i < probs.size(); i++)
    {
      out[i] = (1 - h[i]) * out[i] + h[i] * x[i];
    }
    return(out);
  }
}

//' @export
// [[Rcpp::export]]
int count_if(LogicalVector x, bool na_rm = false) {
  if (na_rm) x = na_omit(x); // remove NA from denominator
  const int n = x.size();
  int counter = 0;
  for(int i = 0; i < n; i++) {
    if(x[i] == TRUE) {
      counter++;
    }
  }
  return counter;
}

//' @export
// [[Rcpp::export]]
double prop_if(LogicalVector x, bool na_rm = false) {
  if (na_rm) x = na_omit(x); // remove NA from denominator
  const int n = x.size();
  int counter = 0;
  for(int i = 0; i < n; i++) {
    if(x[i] == TRUE) {
      counter++;
    }
  }
  return counter/(double)n;
}
