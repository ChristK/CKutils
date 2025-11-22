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

// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <dqrng.h>
#include <random>

using namespace Rcpp;


// Optimized helper function for random walk jumps
// Constrains values to stay within (0, 1) bounds
// Uses inline for better performance in tight loops
inline double fscramble_hlp(const double x, const double jump) {
  const double lower = x - jump;
  const double upper = x + jump;
  
  // Early return for boundary cases to avoid unnecessary RNG calls
  if (lower >= 1.0 || upper <= 0.0) return x;
  
  // Clamp bounds to valid range before RNG call
  const double safe_lower = (lower > 0.0) ? lower : 1e-10;
  const double safe_upper = (upper < 1.0) ? upper : (1.0 - 1e-10);
  
  // Ensure lower <= upper after clamping
  if (safe_lower >= safe_upper) return x;
  
  // Generate random value in the safe range using more efficient accessor
  dqrng::random_64bit_accessor rng;
  const double out = rng.variate<std::uniform_real_distribution<double>>(safe_lower, safe_upper);
  
  // Additional safety check (should be redundant with clamped bounds)
  return (out <= 0.0 || out >= 1.0) ? x : out;
}

//' Scramble trajectories with random walks
//'
//' @description
//' Scrambles rank trajectories of individuals using a continuous space random walk.
//' This function is useful for creating synthetic trajectories while ensuring the
//' distribution of exposure quintiles in the population remains uniform.
//'
//' @details
//' The function applies random perturbations to trajectories, where each point is
//' modified based on the previous point plus a random jump. The jumpiness parameter
//' controls the magnitude of these random walks. Values are constrained to remain
//' within the interval (0, 1).
//'
//' For each trajectory (defined by \code{pid == TRUE} markers), the first value remains
//' unchanged, and subsequent values are perturbed based on the previous value and
//' a random exponential jump with mean 0.02 * jumpiness.
//'
//' The random walks use the dqrng package for high-quality random number generation.
//' Seed and RNG type should be set using \code{dqset.seed()} and \code{dqRNGkind()} 
//' from the dqrng package.
//'
//' @param x A numeric vector of values to scramble, typically between 0 and 1.
//' @param pid A logical vector indicating the start of each trajectory 
//'   (\code{TRUE} = start of new trajectory, \code{FALSE} = continuation).
//'   Must be the same length as \code{x}.
//' @param jumpiness A positive numeric value controlling the magnitude of random jumps.
//'   Higher values create larger perturbations. Default is 1.0.
//' @param inplace A logical value indicating whether to modify \code{x} in place (\code{TRUE})
//'   or return a new vector (\code{FALSE}). Default is \code{TRUE}.
//'
//' @return 
//' If \code{inplace = TRUE}, returns \code{NULL} and modifies \code{x} directly.
//' If \code{inplace = FALSE}, returns a new numeric vector with scrambled values.
//'
//' @examples
//' \dontrun{
//' library(dqrng)
//' dqset.seed(42)
//' 
//' # Create sample trajectory data
//' x <- c(0.1, 0.2, 0.3, 0.5, 0.6, 0.7)  # Two trajectories
//' pid <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE)  # Trajectory starts
//' 
//' # Scramble without modifying original
//' scrambled <- fscramble_trajectories(x, pid, jumpiness = 0.5, inplace = FALSE)
//' print(scrambled)
//' 
//' # Scramble in place
//' original_x <- x
//' fscramble_trajectories(x, pid, jumpiness = 1.0, inplace = TRUE)
//' print(x)  # x is now modified
//' 
//' # Use with data.table for multiple trajectories
//' library(data.table)
//' dt <- data.table(
//'   id = rep(1:3, each = 4),
//'   time = rep(1:4, 3),
//'   value = runif(12)
//' )
//' setkey(dt, id, time)
//' dt[, pid := time == 1]  # Mark trajectory starts
//' dt[, scrambled := fscramble_trajectories(value, pid, 
//'                                         jumpiness = 0.8, 
//'                                         inplace = FALSE)]
//' }
//'
//' @seealso
//' \code{\link[dqrng]{dqset.seed}}, \code{\link[dqrng]{dqRNGkind}}
//'
//' @author Chris Kypridemos
//' @export
// [[Rcpp::export]]
SEXP fscramble_trajectories(NumericVector& x, const LogicalVector& pid,
                           const double& jumpiness = 1.0,
                           const bool& inplace = true)
{
  // Input validation
  if (jumpiness <= 0.0) stop("Jumpiness should be positive");
  
  const int n = x.size();
  if (n != pid.size()) stop("x and pid must have the same length");
  if (n == 0) return NumericVector(0);
  
  // Pre-generate all random jumps for efficiency
  dqrng::random_64bit_accessor rng;
  NumericVector jump(n);
  rng.generate<std::exponential_distribution<double>>(jump.begin(), jump.end(), 50.0 / jumpiness);
  
  if (inplace) {
    // In-place modification - more memory efficient
    for (int i = 1; i < n; ++i) {
      if (!pid[i]) {
        x[i] = fscramble_hlp(x[i-1], jump[i]);
      }
    }
    return R_NilValue;
  } else {
    // Create new vector - safer for functional programming
    NumericVector out = clone(x);  // More efficient than element-by-element copy
    
    for (int i = 1; i < n; ++i) {
      if (!pid[i]) {
        out[i] = fscramble_hlp(out[i-1], jump[i]);
      }
    }
    return out;
  }
}

