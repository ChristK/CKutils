#include <Rcpp.h>
#include <datatableAPI.h>
#include <algorithm>
using namespace Rcpp;

//------------------------------------------------------------------------------
//' Convert Factor to Integer (C++ Version)
//'
//' This function converts a factor (an integer vector with a "levels" attribute)
//' to its underlying integer representation by stripping away the factor's levels and class attributes.
//' By default, the function does not modify the input by reference (a copy is made), but if `inplace`
//' is set to `true`, the function will modify the input directly.
//'
//' @param x A factor object.
//' @param inplace Logical flag indicating whether to modify the input in place (default: false).
//'
//' @return An integer vector containing the underlying integer codes without factor attributes.
//'
//' @export
// [[Rcpp::export]]
IntegerVector fct_to_int_cpp(SEXP x, bool inplace = false)
{
    if (!Rf_isFactor(x))
    {
        stop("Input is not a factor");
    }
    IntegerVector result;
    if (inplace)
    {
        // Use the input directly (shallow copy; modifications may affect the original)
        result = IntegerVector(x);
    }
    else
    {
        // Make a deep copy to avoid modifying the original input
        result = IntegerVector(clone(x));
    }
    result.attr("levels") = R_NilValue;
    result.attr("class") = R_NilValue;
    return result;
}

//------------------------------------------------------------------------------
// Adjust Column Values to Start from 1
//
// For a given column in a data frame (specified by a character vector "on" and index i),
// this function subtracts (min_lookup - 1) from each element. For integers, it additionally
// replaces values outside the expected range (based on cardinality) with NA.
// For factor columns, it calls fct_to_int_cpp first.
// [[Rcpp::export]]
IntegerVector starts_from_1_cpp(DataFrame tbl, CharacterVector on, int i, List min_lookup, List cardinality)
{
    // Adjust for 1-indexing (R uses 1-indexing, C++ uses 0-indexing)
    int idx = i - 1;
    std::string colname = as<std::string>(on[idx]);
    SEXP coldata_sexp = tbl[colname];
    int minx = as<int>(min_lookup[idx]);
    int offset = minx - 1;

    if (TYPEOF(coldata_sexp) == INTSXP)
    {
        IntegerVector coldata(coldata_sexp);
        int n = coldata.size();
        IntegerVector out(n);
        int card = as<int>(cardinality[idx]);
        for (int j = 0; j < n; j++)
        {
            int val = coldata[j] - offset;
            if (val < 1 || val > card)
                out[j] = NA_INTEGER;
            else
                out[j] = val;
        }
        return out;
    }
    else if (Rf_isFactor(coldata_sexp))
    {
        IntegerVector coldata = fct_to_int_cpp(coldata_sexp, true);
        int n = coldata.size();
        IntegerVector out(n);
        for (int j = 0; j < n; j++)
        {
            out[j] = coldata[j] - offset;
        }
        return out;
    }
    else
    {
        stop("Column data must be either an integer or a factor.");
    }
}

// from https://github.com/Rdatatable/data.table/issues/4643

// [[Rcpp::export]]
SEXP dtsubset(SEXP x, SEXP rows, SEXP cols) { return DT_subsetDT(x, rows, cols); }
