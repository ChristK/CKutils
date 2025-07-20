#ifndef DISTR_SICHEL_H
#define DISTR_SICHEL_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations for SICHEL distribution
NumericVector fdSICHEL(const NumericVector& x,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& log_p);

NumericVector fpSICHEL(const NumericVector& q,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& lower_tail,
                        const bool& log_p);

NumericVector fqSICHEL(const NumericVector& p,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& lower_tail,
                        const bool& log_p);

NumericVector frSICHEL(const int& n,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu);

#endif // DISTR_SICHEL_H
