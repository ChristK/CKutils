#ifndef DISTR_ZABNB_H
#define DISTR_ZABNB_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations for ZABNB distribution
NumericVector fdZABNB(const NumericVector& x,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& log_p);

NumericVector fpZABNB(const NumericVector& q,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& lower_tail,
                        const bool& log_p);

NumericVector fqZABNB(const NumericVector& p,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu,
                        const bool& lower_tail,
                        const bool& log_p);

NumericVector frZABNB(const int& n,
                        const NumericVector& mu,
                        const NumericVector& sigma,
                        const NumericVector& nu);

#endif // DISTR_ZABNB_H
