#ifndef DISTR_ZIBNB_H
#define DISTR_ZIBNB_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations for ZIBNB distribution
NumericVector fdZIBNB(const NumericVector& x,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& log_p);

NumericVector fpZIBNB(const NumericVector& q,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& lower_tail,
                        const bool& log_p);

NumericVector fqZIBNB(const NumericVector& p,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& lower_tail,
                        const bool& log_p);

NumericVector frZIBNB(const int& n,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu);

#endif // DISTR_ZIBNB_H
