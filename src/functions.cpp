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
List fquantile_byid(NumericVector x,
                    NumericVector q,
                    StringVector id,
                    bool rounding = false,
                    bool na_rm = true) {
  // Need to be sorted by id
  const int n = x.size();
  const int m = unique(id).size();
  NumericMatrix z(m, q.size());
  StringVector id_nam(m);
  int start = 0;
  int counter = 0;
  int end = 0;
  int counter_row = 0;

  for (int i = 1; i < n; i++) { // start from 2nd element
    if (id[i] == id[i-1]) counter++;
    else
    {
      start = i - counter - 1;
      end = i - 1;
      counter = 0;
      if (rounding) z.row(counter_row) = round(fquantile(x[seq(start, end)], q, na_rm), 0);
      else z.row(counter_row) = fquantile(x[seq(start, end)], q, na_rm);
      id_nam[counter_row] = id[end];
      counter_row++;
    }
  }
  // take care the last group. Independent of its number of rows
  if (rounding) z.row(counter_row) = round(fquantile(x[seq(n - 1 - counter, n - 1)], q, na_rm), 0);
  else z.row(counter_row) = fquantile(x[seq(n - 1 - counter, n - 1)], q, na_rm);
  id_nam[counter_row] = id[n - 1];

  // return(z);
  const int tt = 1 + q.size();
  List outputList(tt);
  outputList[0] = id_nam;
  for (int i = 1; i < tt; i++) {
    outputList[i] = z(_, i - 1);
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

//' @export
// [[Rcpp::export]]
NumericVector clamp(NumericVector& x, double a = 0.0, double b = 1.0, const bool& inplace = false) {
  if (a > b) {double c = a; a = b; b = c;}; // ensure a < b
  const int n = x.size();
  if (!inplace)
  {
    NumericVector out(n);
    for(int i = 0; i < n; i++) {
      if (Rcpp::NumericVector::is_na(x[i])) out[i] = NA_REAL;
      else
      {
        if (x[i] < a) out[i] = a;
        else if (x[i] > b) out[i] = b;
        else out[i] = x[i];
      }
    }
    return out;
  }
  else
  {
    for(int i = 0; i < n; i++) {
      if (x[i] < a) x[i] = a;
      else if (x[i] > b) x[i] = b;
    }
    return x; // transforms input vector inplace as long as an numeric is passed to it. If an integer vector is passed to it from R, an impricit copy is happening so it is not inplace anymore
  }
}

//' @export
// [[Rcpp::export]]
IntegerVector clamp_int(IntegerVector& x, int a = 0, int b = 1, const bool& inplace = false) {
  if (a > b) {int c = a; a = b; b = c;}; // ensure a < b
  const int n = x.size();
  if (!inplace)
  {
    IntegerVector out(n);
    for(int i = 0; i < n; i++) {
      if (Rcpp::IntegerVector::is_na(x[i])) out[i] = NA_INTEGER;
      else
      {
        if (x[i] < a) out[i] = a;
        else if (x[i] > b) out[i] = b;
        else out[i] = x[i];
      }
    }
    return out;
  }
  else
  {
    for(int i = 0; i < n; i++) {
      if (x[i] < a) x[i] = a;
      else if (x[i] > b) x[i] = b;
    }
    return x;
  }
}

//' @export
// [[Rcpp::export]]
LogicalVector fequal(const NumericVector& x, const double& tol) {
  NumericVector y = na_omit(x);
  const int n = y.size();
  for (int i = 0; i < n; ++i) {
    if (y[i] - y[0] > tol || y[0] - y[i] > tol)
      return wrap(false);
  }
  return wrap(true);
}

//' @export
// [[Rcpp::export]]
NumericVector fnormalise(const NumericVector& x) { // between 0, 1
  const int n = x.size();
  const double minx = min(na_omit(x));
  const double maxx = max(na_omit(x));
  NumericVector out(n);
  for (int i = 0; i < n; i++) {
    out[i] = (x[i] - minx) / (maxx - minx);
  }
  return out;
}

