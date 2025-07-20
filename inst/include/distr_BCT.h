#ifndef DISTR_BCT_H
#define DISTR_BCT_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations for BCT distribution
NumericVector fdBCT(const NumericVector& x,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const NumericVector& nu,
                    const NumericVector& tau,
                    const bool& log_p);

NumericVector fpBCT(const NumericVector& q,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const NumericVector& nu,
                    const NumericVector& tau,
                    const bool& lower_tail,
                    const bool& log_p);

NumericVector fqBCT(const NumericVector& p,
                    const NumericVector& mu,
                    const NumericVector& sigma,
                    const NumericVector& nu,
                    const NumericVector& tau,
                    const bool& lower_tail,
                    const bool& log_p);

#endif // DISTR_BCT_H
