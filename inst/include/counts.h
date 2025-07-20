#ifndef COUNTS_H
#define COUNTS_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations
IntegerVector tableRcpp(SEXP x);
IntegerVector counts(SEXP x);

#endif // COUNTS_H
