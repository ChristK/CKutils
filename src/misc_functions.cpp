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
#include <Rmath.h>
#include "recycling_helpers.h"
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
  // Handle empty probs vector
  if (probs.size() == 0) {
    return NumericVector(0);
  }

  // Handle empty input
  if (x.size() == 0)
  {
    NumericVector out(probs.size(), NA_REAL);
    return (out);
  }

  // Handle all-NA input
  if (is_true(all(is_na(x))))
  {
    NumericVector out(probs.size(), NA_REAL);
    return (out);
  }

  if (na_rm)
    x = na_omit(x);

  // After na_omit, check if vector became empty
  const int n = x.size();
  if (n == 0)
  {
    NumericVector out(probs.size(), NA_REAL);
    return (out);
  }

  // Handle single element case - all quantiles are that single value
  if (n == 1)
  {
    NumericVector out(probs.size(), x[0]);
    return (out);
  }

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

  // Handle empty inputs
  if (n == 0 || q.size() == 0) {
    List outputList(1 + q.size());
    outputList[0] = StringVector(0);
    for (int i = 1; i <= q.size(); i++) {
      outputList[i] = NumericVector(0);
    }
    return outputList;
  }

  // Validate lengths match
  if (id.size() != n) {
    stop("Length of 'x' and 'id' must match");
  }

  const int m = unique(id).size();

  // Safety check: m must be > 0
  if (m == 0) {
    List outputList(1 + q.size());
    outputList[0] = StringVector(0);
    for (int i = 1; i <= q.size(); i++) {
      outputList[i] = NumericVector(0);
    }
    return outputList;
  }

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
      // Bounds check before accessing matrix
      if (counter_row >= m) {
        stop("Internal error: counter_row exceeded expected number of groups");
      }
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

  // Bounds check for last group
  if (counter_row >= m) {
    stop("Internal error: counter_row exceeded expected number of groups");
  }

  // Handle the last group regardless of its size
  // Ensure start index is valid
  int last_start = n - 1 - counter;
  if (last_start < 0) last_start = 0;

  if (rounding)
    z.row(counter_row) = round(fquantile(x[seq(last_start, n - 1)], q, na_rm), 0);
  else
    z.row(counter_row) = fquantile(x[seq(last_start, n - 1)], q, na_rm);
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
//' Values below a are set to a and values above b are set to b. Vector arguments a and b support recycling.
//' Optionally, the operation can be performed in-place.
//'
//' @param x A numeric vector to be clamped.
//' @param a A numeric vector of lower bounds (supports recycling, default is 0.0).
//' @param b A numeric vector of upper bounds (supports recycling, default is 1.0).
//' @param inplace Logical flag indicating whether to modify the input vector in place (default is false).
//'
//' @return A numeric vector with values clamped to the range [a, b].
//'
//' @export
// [[Rcpp::export]]
NumericVector fclamp(NumericVector &x, NumericVector a = NumericVector::create(0.0), 
                     NumericVector b = NumericVector::create(1.0), const bool &inplace = false)
{
  // Use recycling for x, a, and b vectors
  RecycledVectors3 recycled = recycle_vectors(x, a, b);
  const int n = recycled.n;
  NumericVector x_rec = recycled.vec1;
  NumericVector a_rec = recycled.vec2;
  NumericVector b_rec = recycled.vec3;
  
  if (!inplace)
  {
    NumericVector out(n);
    for (int i = 0; i < n; i++)
    {
      // Ensure a[i] <= b[i] for each element
      double ai = a_rec[i];
      double bi = b_rec[i];
      if (ai > bi)
      {
        double temp = ai;
        ai = bi;
        bi = temp;
      }
      
      if (Rcpp::NumericVector::is_na(x_rec[i]))
        out[i] = NA_REAL;
      else
      {
        if (x_rec[i] < ai)
          out[i] = ai;
        else if (x_rec[i] > bi)
          out[i] = bi;
        else
          out[i] = x_rec[i];
      }
    }
    return out;
  }
  else
  {
    // For in-place operation, we need to ensure x has the same length as recycled vectors
    if (x.size() != n)
    {
      stop("In-place operation requires consistent vector lengths");
    }
    
    for (int i = 0; i < n; i++)
    {
      // Ensure a[i] <= b[i] for each element
      double ai = a_rec[i];
      double bi = b_rec[i];
      if (ai > bi)
      {
        double temp = ai;
        ai = bi;
        bi = temp;
      }
      
      if (!Rcpp::NumericVector::is_na(x[i]))
      {
        if (x[i] < ai)
          x[i] = ai;
        else if (x[i] > bi)
          x[i] = bi;
      }
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

//' Normalise a Numeric Vector to the 0-1 Range
//'
//' This function normalises a numeric vector so that its values are scaled to lie within the 0 to 1 range.
//' If all elements in the vector are identical, the function returns a vector of ones.
//'
//' @param x A numeric vector to be normalised.
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

//' Carry Forward Values in an Integer Vector
//'
//' Propagates a specific value forward through an integer vector based on person ID markers.
//' When the previous element equals the target value and the current position is not a new person,
//' the target value is carried forward to the current position.
//'
//' **Important**: The input data must be grouped by person ID and sorted by year/time within each person.
//' This ensures proper temporal ordering for the carry-forward operation.
//'
//' **Missing Values**: Missing values (NA) in the input vectors are handled gracefully. Positions
//' with missing person ID markers or values are skipped during processing, preserving existing values.
//'
//' @param x An integer vector to process (must be sorted by year/time within person)
//' @param pid_mrk A logical vector marking new person IDs (TRUE for new person, FALSE otherwise).
//'   Data must be grouped by person and sorted by year/time.
//' @param y The integer value to carry forward
//' @param byref Logical; if TRUE, modifies `x` in place, if FALSE returns a new vector
//' @return An integer vector with values carried forward according to the rules
//' @export
// [[Rcpp::export]]
IntegerVector carry_forward(IntegerVector &x,
                            const LogicalVector &pid_mrk,
                            const int &y,
                            const bool &byref)
{
  const int n = x.size();

  // Handle empty input
  if (n == 0)
  {
    return IntegerVector(0);
  }

  if (byref) // Alters x by reference
  {
    for (int i = 1; i < n; i++) // Start from 1 to avoid out-of-bounds access
    {
      // Skip if current or previous position has missing values
      if (LogicalVector::is_na(pid_mrk(i)) || IntegerVector::is_na(x(i - 1)))
        continue;
      if (!pid_mrk(i) && x(i - 1) == y)
        x(i) = y;
    }
    return x;
  }
  else // Returns a new IntegerVector
  {
    IntegerVector out = clone(x);
    for (int i = 1; i < n; i++) // Start from 1 to avoid out-of-bounds access
    {
      // Skip if current or previous position has missing values
      if (LogicalVector::is_na(pid_mrk(i)) || IntegerVector::is_na(out(i - 1)))
        continue;
      if (!pid_mrk(i) && out(i - 1) == y)
        out(i) = y;
    }
    return out;
  }
}

//' Carry Forward with Incremental Values
//'
//' Propagates and increments values forward through an integer vector based on person ID markers.
//' The function operates in two distinct modes controlled by the `recur` parameter:
//'
//' **Non-Recursive Mode (recur = FALSE):**
//' - At each position i, if the position is not a new person AND the previous value >= y,
//'   then set current value = previous value + 1
//' - This creates continuously incrementing sequences (y, y+1, y+2, y+3, ...) within each person
//' - Incrementing stops only when a new person boundary is encountered
//'
//' **Recursive Mode (recur = TRUE):**
//' - At each position i, if the position is not a new person AND both current value >= y AND previous value >= y,
//'   then set current value = previous value + 1
//' - This requires both the current and previous positions to already be above threshold y
//' - If the current value drops below y, incrementing stops and must be "restarted" by having both values >= y again
//'
//' **Important**: The input data must be grouped by person ID and sorted by year/time within each person.
//' This ensures proper temporal ordering for the incremental carry-forward operation.
//'
//' **Missing Values**: Missing values (NA) in the input vectors are handled gracefully. Positions
//' with missing person ID markers or values are skipped during processing, preserving existing values.
//'
//' @param x An integer vector to process (must be sorted by year/time within person)
//' @param pid_mrk A logical vector marking new person IDs (TRUE for new person, FALSE otherwise).
//'   Data must be grouped by person and sorted by year/time.
//' @param recur Logical; if FALSE (non-recursive), continuously increment when previous >= y;
//'   if TRUE (recursive), increment only when both current >= y AND previous >= y
//' @param y The threshold value for triggering incremental carry-forward behaviour
//' @param byref Logical; if TRUE, modifies `x` in place, if FALSE returns a new vector
//' @return An integer vector with incremented values carried forward according to the specified mode
//' @export
// [[Rcpp::export]]
IntegerVector carry_forward_incr(IntegerVector &x, const LogicalVector &pid_mrk,
                                 const bool &recur, const int &y,
                                 const bool &byref)
{
  const int n = x.size();

  // Handle empty input
  if (n == 0)
  {
    return IntegerVector(0);
  }

  if (byref)
  {
    // Modify x in place
    if (recur)
    {
      for (int i = 1; i < n; i++)
      { // Start from 1 to avoid out-of-bounds access
        // Skip if missing values present
        if (LogicalVector::is_na(pid_mrk(i)) || IntegerVector::is_na(x(i)) || IntegerVector::is_na(x(i - 1)))
          continue;
        if (!pid_mrk(i) && x(i) >= y && x(i - 1) >= y)
        {
          x(i) = x(i - 1) + 1;
        }
      }
    }
    else
    {
      for (int i = 1; i < n; i++)
      { // Start from 1 to avoid out-of-bounds access
        // Skip if missing values present
        if (LogicalVector::is_na(pid_mrk(i)) || IntegerVector::is_na(x(i - 1)))
          continue;
        if (!pid_mrk(i) && x(i - 1) >= y)
        {
          x(i) = x(i - 1) + 1;
        }
      }
    }
    return x;
  }
  else
  {
    // Create new vector
    IntegerVector out = clone(x);
    if (recur)
    {
      for (int i = 1; i < n; i++)
      { // Start from 1 to avoid out-of-bounds access
        // Skip if missing values present
        if (LogicalVector::is_na(pid_mrk(i)) || IntegerVector::is_na(out(i)) || IntegerVector::is_na(out(i - 1)))
          continue;
        if (!pid_mrk(i) && out(i) >= y && out(i - 1) >= y)
        {
          out(i) = out(i - 1) + 1;
        }
      }
    }
    else
    {
      for (int i = 1; i < n; i++)
      { // Start from 1 to avoid out-of-bounds access
        // Skip if missing values present
        if (LogicalVector::is_na(pid_mrk(i)) || IntegerVector::is_na(out(i - 1)))
          continue;
        if (!pid_mrk(i) && out(i - 1) >= y)
        {
          out(i) = out(i - 1) + 1;
        }
      }
    }
    return out;
  }
}

//' Carry Values Backward with Decrementation
//'
//' Propagates values backward through an integer vector, decrementing values as it goes.
//' The function processes the vector from end to beginning, reducing values by 1 when
//' moving backward, respecting person ID boundaries.
//'
//' **Important**: The input data must be grouped by person ID and sorted by year/time within each person.
//' This ensures proper temporal ordering for the backward propagation operation.
//'
//' @param x An integer vector to process (must be sorted by year/time within person)
//' @param pid_mrk A logical vector marking new person IDs (TRUE for new person, FALSE otherwise).
//'   Data must be grouped by person and sorted by year/time.
//' @param y The threshold value for backward propagation
//' @return An integer vector with values carried backward and decremented
//' @export
// [[Rcpp::export]]
IntegerVector carry_backward_decr(const IntegerVector &x, const LogicalVector &pid_mrk,
                                  const int &y = 0)
{
  const int n = x.size();

  // Handle empty input
  if (n == 0)
  {
    return IntegerVector(0);
  }

  IntegerVector out = clone(x);
  for (int i = n - 1; i > 0; i--) // Go backwards but stop from one row before the last
  {
    // Skip if missing values present
    if (LogicalVector::is_na(pid_mrk(i)) || IntegerVector::is_na(out(i)))
      continue;

    if (!pid_mrk(i) && out(i) > y)
      out(i - 1) = out(i) - 1;
    if (i < (n - 1) && pid_mrk(i) && !pid_mrk(i + 1) && out(i + 1) > y && out(i) == y)
      out(i) = out(i + 1) - 1;
    if (out(i) < 0)
      out(i) = 0;
  }
  return out;
}

//' Create New Simulant Markers
//'
//' Creates a logical vector marking the positions where new simulants (persons) begin
//' based on changes in person ID values. The first position is always marked as TRUE.
//' This function is optimised for performance with minimal branching and memory access.
//'
//' **Important**: The input data must be grouped by person ID and sorted by year/time within each person.
//' This ensures correct identification of person boundaries in temporal data.
//'
//' **Missing Values**: Missing values (NA) in person IDs are treated as new simulants.
//' Each NA value will be marked as TRUE (start of new simulant).
//'
//' @param pid An integer vector of person IDs (must be grouped by person and sorted by year/time)
//' @return A logical vector where TRUE indicates the start of a new simulant
//' @export
// [[Rcpp::export]]
LogicalVector mk_new_simulant_markers(const IntegerVector &pid)
{
  // pid should be sorted and same length as x
  const int n = pid.size();

  // Handle empty input
  if (n == 0)
  {
    return LogicalVector(0);
  }

  LogicalVector new_simulant_markers(n);
  new_simulant_markers(0) = true;

  // Handle single element case
  if (n == 1)
  {
    return new_simulant_markers;
  }

  // Handle case where first element might be NA
  int previous_pid = IntegerVector::is_na(pid(0)) ? NA_INTEGER : pid(0);
  // Loop with no conditional branches in the body (therefore branch predictor should get it right almost every time) and minimal memory access by retaining previous_pid.
  for (int i = 1; i < n; i++)
  {
    // Handle missing values - if current pid is NA, mark as new simulant
    if (IntegerVector::is_na(pid(i)))
    {
      new_simulant_markers(i) = true;
      previous_pid = NA_INTEGER;
    }
    else if (IntegerVector::is_na(previous_pid))
    {
      new_simulant_markers(i) = true;
      previous_pid = pid(i);
    }
    else
    {
      new_simulant_markers(i) = pid(i) != previous_pid;
      previous_pid = pid(i);
    }
  }
  return new_simulant_markers;
}

//' Identify Long Dead Individuals
//'
//' Identifies positions where individuals have been "long dead" based on the previous
//' value being non-zero and the current position not being a new person ID.
//'
//' **Important**: The input data must be grouped by person ID and sorted by year/time within each person.
//' This ensures proper temporal sequencing for identifying long-dead status.
//'
//' @param x An integer vector representing death status or similar (must be sorted by year/time within person)
//' @param pid A logical vector marking new person IDs (TRUE for new person, FALSE otherwise).
//'   Data must be grouped by person and sorted by year/time.
//' @return A logical vector where TRUE indicates a "long dead" individual
//' @export
// [[Rcpp::export]]
LogicalVector identify_longdead(const IntegerVector &x, const LogicalVector &pid)
{
  const int n = x.size();

  // Handle empty input
  if (n == 0)
  {
    return LogicalVector(0);
  }

  LogicalVector out(n, false); // Initialize with false
  for (int i = 1; i < n; i++)  // Start from 1 to avoid out-of-bounds access
  {
    // Skip if missing values present
    if (LogicalVector::is_na(pid(i)) || IntegerVector::is_na(x(i - 1)))
      continue;
    out(i) = !pid(i) && (x(i - 1) != 0);
  }
  return out;
}

//' Identify Invitees for Health Screening Programs
//'
//' Determines which eligible individuals should be invited to screening programs
//' based on eligibility, invitation frequency, probabilities, and previous invitations.
//' The function respects minimum intervals between invitations and person ID boundaries.
//'
//' **Important**: The input data must be grouped by person ID and sorted by year/time within each person.
//' This ensures proper tracking of invitation intervals and temporal relationships.
//'
//' **Missing Values**: Missing values (NA) in any of the input vectors result in NA output
//' for that position, ensuring data integrity in the screening process.
//'
//' @param elig An integer vector indicating eligibility (1 = eligible, 0 = not eligible).
//'   Must be sorted by year/time within person.
//' @param prev_inv An integer vector indicating previous invitations (1 = previously invited, 0 = not).
//'   Must be sorted by year/time within person.
//' @param prb A numeric vector of invitation probabilities for each individual.
//'   Must be sorted by year/time within person.
//' @param freq An integer vector of minimum years between invitations for each individual.
//'   Must be sorted by year/time within person.
//' @param pid A logical vector marking new person IDs (TRUE for new person, FALSE otherwise).
//'   Data must be grouped by person and sorted by year/time.
//' @return An integer vector where 1 indicates an invitation should be sent, 0 otherwise, NA for missing inputs
//' @export
// [[Rcpp::export]]
IntegerVector identify_invitees(const IntegerVector &elig,
                                const IntegerVector &prev_inv,
                                const NumericVector &prb,
                                const IntegerVector &freq,
                                const LogicalVector &pid)
{
  const int n = elig.size();

  // Handle empty input
  if (n == 0)
  {
    return IntegerVector(0);
  }

  int counter = 1000; // to ensure > max(freq)
  IntegerVector out(n, 0);

  for (int i = 0; i < n; i++)
  {
    // Skip if any critical values are missing
    if (LogicalVector::is_na(pid(i)) || IntegerVector::is_na(elig(i)) ||
        IntegerVector::is_na(prev_inv(i)) || IntegerVector::is_na(freq(i)) ||
        NumericVector::is_na(prb(i)))
    {
      out(i) = NA_INTEGER;
      continue;
    }

    if (pid(i))
    {
      counter = 1000; // Reset counter for new person
    }
    else if (prev_inv(i) == 1)
    {
      counter = 0;
    }
    else if (counter < 1000)
    {
      counter++;
    }

    if (elig(i) == 1 && counter >= freq(i))
    {
      out(i) = R::rbinom(1, prb(i));
      if (out(i) == 1)
        counter = 0;
    }
  }
  return out;
}

//' Health Care Effect with Continuation Probability
//'
//' Models the continuation of a health care effect based on a probability of continuation.
//' When the previous time step had the effect (value 1) and it's not a new person,
//' a binomial trial determines if the effect continues.
//'
//' **Important**: The input data must be grouped by person ID and sorted by year/time within each person.
//' This ensures proper temporal sequencing for modelling effect continuation.
//'
//' @param x An integer vector representing health care effect status (1 = present, 0 = absent).
//'   Must be sorted by year/time within person.
//' @param prb_of_continuation A numeric value representing the probability of effect continuation
//' @param pid A logical vector marking new person IDs (TRUE for new person, FALSE otherwise).
//'   Data must be grouped by person and sorted by year/time.
//' @return An integer vector with updated health care effect status
//' @export
// [[Rcpp::export]]
IntegerVector hc_effect(const IntegerVector &x,
                        const double &prb_of_continuation,
                        const LogicalVector &pid)
{
  const int n = x.size();

  // Handle empty input
  if (n == 0)
  {
    return IntegerVector(0);
  }

  IntegerVector out = clone(x);
  for (int i = 1; i < n; i++) // Start from 1 to avoid out-of-bounds access
  {
    // Skip if missing values present
    if (LogicalVector::is_na(pid(i)) || IntegerVector::is_na(out(i - 1)))
      continue;
    // Only apply effect if current position is not new person (so effect doesn't cross person boundaries)
    if (!pid(i) && out(i - 1) == 1 && out(i) == 0)
      out(i) = R::rbinom(1, prb_of_continuation);
  }
  return out;
}

//' Anti-logit Transformation (Inverse Logit)
//'
//' Transforms a real number to a probability using the anti-logit (logistic) function.
//' This is the inverse of the logit transformation, converting log-odds back to probabilities.
//' The function computes exp(x) / (1 + exp(x)).
//'
//' @param x A numeric value to transform (can be any real number)
//' @return A numeric value between 0 and 1 representing a probability
//' @export
// [[Rcpp::export]]
double antilogit(const double &x)
{
  // Use numerically stable version to avoid overflow
  if (x > 0)
  {
    const double exp_neg_x = exp(-x);
    return 1.0 / (1.0 + exp_neg_x);
  }
  else
  {
    const double exp_x = exp(x);
    return exp_x / (1.0 + exp_x);
  }
}
