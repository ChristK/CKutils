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
using namespace Rcpp;

//' Compute Quantiles Using Default R Type 7 Method
//'
//' This function computes quantiles for a numeric vector using the default R method (type 7).
//' It handles missing values and can remove them if requested.
//'
//' @param x A numeric vector from which quantiles are computed.
//' @param probs A numeric vector of probabilities for which quantiles are desired.
//' @param na_rm Logical flag indicating whether to remove NA values (default is true).
//'
//' @return A numeric vector containing the computed quantiles corresponding to the probabilities in \code{probs}.
//'
//' @export
// [[Rcpp::export]]
NumericVector fquantile(NumericVector x, NumericVector probs, bool na_rm = true)
{
  if (all(is_na(x)))
  {
    NumericVector out(probs.size(), NA_REAL);
    return (out);
  }
  if (na_rm)
    x = na_omit(x);
  const int n = x.size();
  NumericVector out(probs.size());
  IntegerVector ii(probs.size());
  NumericVector h(probs.size());
  NumericVector index = 1 + (n - 1) * probs;
  NumericVector lo = floor(index);   // floor
  NumericVector hi = ceiling(index); // ceiling
  for (int i = 0; i < probs.size(); i++)
  {
    // catch corner case when index element is integer (ceiling == floor)
    h[i] = index[i] - lo[i];
  }
  std::sort(x.begin(), x.end());
  out = x[as<IntegerVector>(lo) - 1];
  if (all(is_na(ii)))
  {
    return (out);
  }
  else
  {
    x = x[as<IntegerVector>(hi) - 1];
    for (int i = 0; i < probs.size(); i++)
    {
      out[i] = (1 - h[i]) * out[i] + h[i] * x[i];
    }
    return (out);
  }
}

//' Compute Quantiles by Group
//'
//' This function computes quantiles for subsets of a numeric vector defined by an ID vector.
//' The input vector must be sorted by the ID vector. Optionally, the results can be rounded.
//'
//' @param x A numeric vector from which quantiles are computed.
//' @param q A numeric vector of probabilities for which quantiles are desired.
//' @param id A string vector representing group identifiers. The vector \code{x} must be sorted by \code{id}.
//' @param rounding Logical flag indicating whether to round the quantiles (default is false).
//' @param na_rm Logical flag indicating whether to remove NA values (default is true).
//'
//' @return A list where the first element is a vector of group IDs and subsequent elements are numeric vectors of computed quantiles for each group.
//'
//' @export
// [[Rcpp::export]]
List fquantile_byid(NumericVector x,
                    NumericVector q,
                    StringVector id,
                    bool rounding = false,
                    bool na_rm = true)
{
  const int n = x.size();
  const int m = unique(id).size();
  NumericMatrix z(m, q.size());
  StringVector id_nam(m);
  int start = 0;
  int counter = 0;
  int end = 0;
  int counter_row = 0;

  for (int i = 1; i < n; i++)
  { // start from second element
    if (id[i] == id[i - 1])
      counter++;
    else
    {
      start = i - counter - 1;
      end = i - 1;
      counter = 0;
      if (rounding)
        z.row(counter_row) = round(fquantile(x[seq(start, end)], q, na_rm), 0);
      else
        z.row(counter_row) = fquantile(x[seq(start, end)], q, na_rm);
      id_nam[counter_row] = id[end];
      counter_row++;
    }
  }
  // Handle the last group regardless of its size
  if (rounding)
    z.row(counter_row) = round(fquantile(x[seq(n - 1 - counter, n - 1)], q, na_rm), 0);
  else
    z.row(counter_row) = fquantile(x[seq(n - 1 - counter, n - 1)], q, na_rm);
  id_nam[counter_row] = id[n - 1];

  const int tt = 1 + q.size();
  List outputList(tt);
  outputList[0] = id_nam;
  for (int i = 1; i < tt; i++)
  {
    outputList[i] = z(_, i - 1);
  }
  return (outputList);
}

//' Count TRUE Values in a Logical Vector
//'
//' This function counts the number of TRUE values in a logical vector, with an option to remove missing values.
//'
//' @param x A logical vector.
//' @param na_rm Logical flag indicating whether to remove NA values before counting (default is false).
//'
//' @return An integer representing the number of TRUE values in the vector.
//'
//' @export
// [[Rcpp::export]]
int count_if(LogicalVector x, bool na_rm = false)
{
  if (na_rm)
    x = na_omit(x); // remove NA values
  const int n = x.size();
  int counter = 0;
  for (int i = 0; i < n; i++)
  {
    if (x[i] == TRUE)
    {
      counter++;
    }
  }
  return counter;
}

//' Proportion of TRUE Values in a Logical Vector
//'
//' This function calculates the proportion of TRUE values in a logical vector, with an option to remove missing values.
//'
//' @param x A logical vector.
//' @param na_rm Logical flag indicating whether to remove NA values before calculating the proportion (default is false).
//'
//' @return A numeric value representing the proportion of TRUE values in the vector.
//'
//' @export
// [[Rcpp::export]]
double prop_if(LogicalVector x, bool na_rm = false)
{
  if (na_rm)
    x = na_omit(x); // remove NA values
  const int n = x.size();
  int counter = 0;
  for (int i = 0; i < n; i++)
  {
    if (x[i] == TRUE)
    {
      counter++;
    }
  }
  return counter / (double)n;
}

//' Clamp a Numeric Vector Within a Range
//'
//' This function clamps (limits) the values of a numeric vector to lie within a specified range [a, b].
//' Values below a are set to a and values above b are set to b. Optionally, the operation can be performed in-place.
//'
//' @param x A numeric vector to be clamped.
//' @param a The lower bound (default is 0.0).
//' @param b The upper bound (default is 1.0).
//' @param inplace Logical flag indicating whether to modify the input vector in place (default is false).
//'
//' @return A numeric vector with values clamped to the range [a, b].
//'
//' @export
// [[Rcpp::export]]
NumericVector fclamp(NumericVector &x, double a = 0.0, double b = 1.0, const bool &inplace = false)
{
  if (a > b)
  {
    double c = a;
    a = b;
    b = c;
  } // ensure a < b
  const int n = x.size();
  if (!inplace)
  {
    NumericVector out(n);
    for (int i = 0; i < n; i++)
    {
      if (Rcpp::NumericVector::is_na(x[i]))
        out[i] = NA_REAL;
      else
      {
        if (x[i] < a)
          out[i] = a;
        else if (x[i] > b)
          out[i] = b;
        else
          out[i] = x[i];
      }
    }
    return out;
  }
  else
  {
    for (int i = 0; i < n; i++)
    {
      if (x[i] < a)
        x[i] = a;
      else if (x[i] > b)
        x[i] = b;
    }
    return x;
  }
}

//' Clamp an Integer Vector Within a Range
//'
//' This function clamps the values of an integer vector to lie within a specified range [a, b].
//' Values below a are set to a and values above b are set to b. Optionally, the operation can be performed in-place.
//'
//' @param x An integer vector to be clamped.
//' @param a The lower bound (default is 0).
//' @param b The upper bound (default is 1).
//' @param inplace Logical flag indicating whether to modify the input vector in place (default is false).
//'
//' @return An integer vector with values clamped to the range [a, b].
//'
//' @export
// [[Rcpp::export]]
IntegerVector fclamp_int(IntegerVector &x, int a = 0, int b = 1, const bool &inplace = false)
{
  if (a > b)
  {
    int c = a;
    a = b;
    b = c;
  } // ensure a < b
  const int n = x.size();
  if (!inplace)
  {
    IntegerVector out(n);
    for (int i = 0; i < n; i++)
    {
      if (Rcpp::IntegerVector::is_na(x[i]))
        out[i] = NA_INTEGER;
      else
      {
        if (x[i] < a)
          out[i] = a;
        else if (x[i] > b)
          out[i] = b;
        else
          out[i] = x[i];
      }
    }
    return out;
  }
  else
  {
    for (int i = 0; i < n; i++)
    {
      if (x[i] < a)
        x[i] = a;
      else if (x[i] > b)
        x[i] = b;
    }
    return x;
  }
}

//' Check Numeric Vector Equality Within Tolerance
//'
//' This function checks whether all non-missing elements in a numeric vector are equal
//' within a specified tolerance. Differences between elements that are less than or equal
//' to the tolerance are considered negligible.
//'
//' @param x A numeric vector to test for equality.
//' @param tol A tolerance value. Differences below or equal to this value are considered equal.
//'
//' @return A logical value: TRUE if all non-missing elements are equal within the tolerance,
//'         and FALSE otherwise.
//'
//' @export
// [[Rcpp::export]]
LogicalVector fequal(const NumericVector &x, const double &tol)
{
  NumericVector y = na_omit(x);
  const int n = y.size();
  for (int i = 0; i < n; ++i)
  {
    if (y[i] - y[0] > tol || y[0] - y[i] > tol)
      return wrap(false);
  }
  return wrap(true);
}

//' Normalize a Numeric Vector to the 0-1 Range
//'
//' This function normalizes a numeric vector so that its values are scaled to lie within the 0 to 1 range.
//' If all elements in the vector are identical, the function returns a vector of ones.
//'
//' @param x A numeric vector to be normalized.
//'
//' @return A numeric vector with values scaled between 0 and 1.
//'
//' @export
// [[Rcpp::export]]
NumericVector fnormalise(const NumericVector &x)
{
  const int n = x.size();
  const double minx = min(na_omit(x));
  const double maxx = max(na_omit(x));
  NumericVector out(n);
  for (int i = 0; i < n; i++)
  {
    out[i] = (x[i] - minx) / (maxx - minx);
  }
  return out;
}

/* Linear Interpolation */
//' Perform Linear Interpolation
//'
//' This function performs linear interpolation for a set of points.
//'
//' @param xp A numeric vector of new x values at which to interpolate.
//' @param x0 A numeric vector of original x values (starting points for interpolation).
//' @param x1 A numeric vector of original x values (ending points for interpolation).
//' @param y0 A numeric vector of original y values corresponding to x0.
//' @param y1 A numeric vector of original y values corresponding to x1.
//'
//' @return A numeric vector of interpolated y values corresponding to \code{xp}.
//'
//' @export
// [[Rcpp::export]]
NumericVector lin_interpolation(
    const NumericVector &xp,
    const NumericVector &x0,
    const NumericVector &x1,
    const NumericVector &y0,
    const NumericVector &y1)
{
  return y0 + ((y1 - y0) / (x1 - x0)) * (xp - x0);
}
