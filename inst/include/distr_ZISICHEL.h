#ifndef DISTR_ZISICHEL_H
#define DISTR_ZISICHEL_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations for ZISICHEL distribution
NumericVector fdZISICHEL(const NumericVector& x,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& log_p);

NumericVector fpZISICHEL(const NumericVector& q,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& lower_tail,
                        const bool& log_p);

NumericVector fqZISICHEL(const NumericVector& p,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& lower_tail,
                        const bool& log_p);

NumericVector frZISICHEL(const int& n,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu);

#endif // DISTR_ZISICHEL_H
