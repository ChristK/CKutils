#ifndef DISTR_BCPEO_H
#define DISTR_BCPEO_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations for BCPEo distribution
NumericVector fdBCPEo(const NumericVector& x,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu,
                      const NumericVector& tau,
                      const bool& log_p);

NumericVector fpBCPEo(const NumericVector& q,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu,
                      const NumericVector& tau,
                      const bool& lower_tail,
                      const bool& log_p);

NumericVector fqBCPEo(const NumericVector& p,
                      const NumericVector& mu,
                      const NumericVector& sigma,
                      const NumericVector& nu,
                      const NumericVector& tau,
                      const bool& lower_tail,
                      const bool& log_p);

#endif // DISTR_BCPEO_H
