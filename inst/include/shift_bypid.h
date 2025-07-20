#ifndef SHIFT_BYPID_H
#define SHIFT_BYPID_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations
NumericVector shift_bypidNum(const NumericVector& x, const int& lag,
                            const LogicalVector& pid_mrk,
                            const NumericVector& fillNumericVector::create(NA_REAL));

IntegerVector shift_bypidInt(const IntegerVector& x, const int& lag,
                            const LogicalVector& pid_mrk,
                            const IntegerVector& fillIntegerVector::create(NA_INTEGER));

LogicalVector shift_bypidBool(const LogicalVector& x, const int& lag,
                             const LogicalVector& pid_mrk,
                             const LogicalVector& fillLogicalVector::create(NA_LOGICAL));

StringVector shift_bypidStr(const CharacterVector& x, const int& lag,
                           const LogicalVector& pid_mrk,
                           const CharacterVector& fillCharacterVector::create(NA_STRING));

#endif // SHIFT_BYPID_H
