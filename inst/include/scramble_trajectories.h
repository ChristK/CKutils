#ifndef SCRAMBLE_TRAJECTORIES_H
#define SCRAMBLE_TRAJECTORIES_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations
SEXP fscramble_trajectories(NumericVector& x, const LogicalVector& pid,
                           const double& rw_scale,
                           const bool& inplace);

#endif // SCRAMBLE_TRAJECTORIES_H
