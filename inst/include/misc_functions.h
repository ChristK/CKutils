#ifndef MISC_FUNCTIONS_H
#define MISC_FUNCTIONS_H

#include <Rcpp.h>
using namespace Rcpp;

// Function declarations
NumericVector fquantile(NumericVector x, NumericVector probs, bool na_rm);
List fquantile_byid(NumericVector x, IntegerVector pid, NumericVector probs, bool na_rm);
int count_if(LogicalVector x, bool na_rm);
double prop_if(LogicalVector x, bool na_rm);
NumericVector fclamp(NumericVector &x, NumericVector a, NumericVector b, const bool &inplace);
IntegerVector fclamp_int(IntegerVector &x, int a, int b, const bool &inplace);
LogicalVector fequal(const NumericVector &x, const double &tol);
NumericVector fnormalise(const NumericVector &x);
NumericVector lin_interpolation(const NumericVector &x, const NumericVector &y, 
                                const NumericVector &xout);
IntegerVector carry_forward(IntegerVector &x, const LogicalVector &pid_mrk, const bool &inplace);
IntegerVector carry_forward_incr(IntegerVector &x, const LogicalVector &pid_mrk,
                                 const int &y, const bool &inplace);
IntegerVector carry_backward_decr(const IntegerVector &x, const LogicalVector &pid_mrk,
                                  const int &y, const bool &inplace);
LogicalVector mk_new_simulant_markers(const IntegerVector &pid);
LogicalVector identify_longdead(const IntegerVector &x, const LogicalVector &pid);
IntegerVector identify_invitees(const IntegerVector &elig, const IntegerVector &invited,
                                const LogicalVector &pid_mrk);
IntegerVector hc_effect(const IntegerVector &x, const LogicalVector &hc,
                        const double &hr, const LogicalVector &pid_mrk);
double antilogit(const double &x);
double antilogit(const double &x);

#endif // MISC_FUNCTIONS_H
