#ifndef LOOKUP_DT_H
#define LOOKUP_DT_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations
IntegerVector fct_to_int_cpp(SEXP x, bool inplace);
IntegerVector starts_from_1_cpp(DataFrame tbl, CharacterVector on, int i, List min_lookup, List cardinality);
SEXP dtsubset(SEXP x, SEXP rows, SEXP cols);

#endif // LOOKUP_DT_H
