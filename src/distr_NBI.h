#ifndef DISTR_NBI_H
#define DISTR_NBI_H

#include <Rcpp.h>
#include <math.h>
#include <Rmath.h>
#include "recycling_helpers.h"

// Enable vectorization hints for modern compilers
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif

using namespace Rcpp;

// Basic NBI distribution functions (scalar versions)
double fdNBI_scalar(const int& x,
                    const double& mu,
                    const double& sigma,
                    const bool& log_p);

double fpNBI_scalar(const int& q,
                    const double& mu,
                    const double& sigma,
                    const bool& lower_tail,
                    const bool& log_p);

int fqNBI_scalar(const double& p,
                 const double& mu,
                 const double& sigma,
                 const bool& lower_tail,
                 const bool& log_p);

int frNBI_scalar(const double& mu,
                 const double& sigma);

// Basic NBI distribution functions (vectorized versions)
NumericVector fdNBI(const NumericVector& x,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const bool& log_p);

NumericVector fpNBI(const NumericVector& q,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const bool& lower_tail,
                    const bool& log_p);

IntegerVector fqNBI(const NumericVector& p,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const bool& lower_tail,
                    const bool& log_p);

IntegerVector frNBI(const int& n,
                    const NumericVector& mu,
                    const NumericVector& sigma);

#endif // DISTR_NBI_H
