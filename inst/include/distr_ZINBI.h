#ifndef DISTR_ZINBI_H
#define DISTR_ZINBI_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations for ZINBI distribution
NumericVector fdZINBI(const NumericVector& x,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& log_p);

NumericVector fpZINBI(const NumericVector& q,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& lower_tail,
                        const bool& log_p);

NumericVector fqZINBI(const NumericVector& p,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& lower_tail,
                        const bool& log_p);

NumericVector frZINBI(const int& n,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu);

#endif // DISTR_ZINBI_H
