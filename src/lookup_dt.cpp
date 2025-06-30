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
    if (Rf_isNull(x)) {
        stop("Input is NULL");
    }
    
    if (!Rf_isFactor(x))
    {
        stop("Input is not a factor");
    }
    
    // Check if the factor has valid length
    if (Rf_length(x) < 0) {
        stop("Invalid factor length");
    }
    
    IntegerVector result;
    if (inplace)
    {
        // Use the input directly (shallow copy; modifications may affect the original)
        // On Windows, this can be problematic with memory management
        result = IntegerVector(x);
    }
    else
    {
        // Make a deep copy to avoid modifying the original input
        // This is safer across platforms
        result = IntegerVector(clone(x));
    }
    
    // Safely remove attributes
    try {
        result.attr("levels") = R_NilValue;
        result.attr("class") = R_NilValue;
    } catch (const std::exception& e) {
        stop("Error removing factor attributes: " + std::string(e.what()));
    } catch (...) {
        stop("Unknown error removing factor attributes");
    }
    
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
    // Validate input parameters first
    if (i < 1 || i > on.size()) {
        stop("Index i is out of bounds for 'on' vector");
    }
    if (i - 1 >= min_lookup.size() || i - 1 >= cardinality.size()) {
        stop("Index i is out of bounds for min_lookup or cardinality lists");
    }
    
    // Adjust for 1-indexing (R uses 1-indexing, C++ uses 0-indexing)
    int idx = i - 1;
    std::string colname = as<std::string>(on[idx]);
    
    // Check if column exists in the data frame
    CharacterVector tbl_names = tbl.names();
    bool col_exists = false;
    for (int k = 0; k < tbl_names.size(); k++) {
        if (std::string(tbl_names[k]) == colname) {
            col_exists = true;
            break;
        }
    }
    if (!col_exists) {
        stop("Column '" + colname + "' not found in data frame");
    }
    
    SEXP coldata_sexp = tbl[colname];
    
    // Validate min_lookup and cardinality values
    if (Rf_isNull(min_lookup[idx]) || Rf_isNull(cardinality[idx])) {
        stop("min_lookup or cardinality contains NULL values");
    }
    
    int minx = as<int>(min_lookup[idx]);
    int card = as<int>(cardinality[idx]);
    
    // Validate cardinality is positive
    if (card <= 0) {
        stop("Cardinality must be positive");
    }
    
    int offset = minx - 1;

    if (TYPEOF(coldata_sexp) == INTSXP)
    {
        IntegerVector coldata(coldata_sexp);
        int n = coldata.size();
        IntegerVector out(n);
        
        for (int j = 0; j < n; j++)
        {
            // Check for NA values first
            if (IntegerVector::is_na(coldata[j])) {
                out[j] = NA_INTEGER;
                continue;
            }
            
            // Check for potential integer overflow/underflow
            if (coldata[j] == INT_MIN || (coldata[j] - offset) < INT_MIN || (coldata[j] - offset) > INT_MAX) {
                out[j] = NA_INTEGER;
                continue;
            }
            
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
        // Use inplace = false to avoid potential memory issues
        IntegerVector coldata = fct_to_int_cpp(coldata_sexp, false);
        int n = coldata.size();
        IntegerVector out(n);
        
        for (int j = 0; j < n; j++)
        {
            // Check for NA values first
            if (IntegerVector::is_na(coldata[j])) {
                out[j] = NA_INTEGER;
                continue;
            }
            
            // Check for potential integer overflow/underflow
            if (coldata[j] == INT_MIN || (coldata[j] - offset) < INT_MIN || (coldata[j] - offset) > INT_MAX) {
                out[j] = NA_INTEGER;
                continue;
            }
            
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
SEXP dtsubset(SEXP x, SEXP rows, SEXP cols) { 
    // Add safety checks for the data.table subset operation
    if (Rf_isNull(x)) {
        stop("Input data.table 'x' is NULL");
    }
    if (Rf_isNull(rows)) {
        stop("Row indices 'rows' is NULL");
    }
    if (Rf_isNull(cols)) {
        stop("Column indices 'cols' is NULL");
    }
    
    // Validate that x is actually a data.table
    if (!Rf_inherits(x, "data.table")) {
        stop("Input 'x' must be a data.table");
    }
    
    // Additional validation for rows - should be integer vector
    if (TYPEOF(rows) != INTSXP) {
        stop("Row indices must be integer");
    }
    
    // Additional validation for cols - should be integer vector  
    if (TYPEOF(cols) != INTSXP) {
        stop("Column indices must be integer");
    }
    
    // Check for empty vectors
    if (Rf_length(rows) == 0) {
        stop("Row indices vector is empty");
    }
    if (Rf_length(cols) == 0) {
        stop("Column indices vector is empty");
    }
    
    // Validate row indices are within bounds
    IntegerVector row_vec(rows);
    int nrows = Rf_length(VECTOR_ELT(x, 0)); // Number of rows in first column
    for (int i = 0; i < row_vec.size(); i++) {
        if (IntegerVector::is_na(row_vec[i])) {
            stop("Row indices cannot contain NA values");
        }
        if (row_vec[i] < 1 || row_vec[i] > nrows) {
            stop("Row index out of bounds: " + std::to_string(row_vec[i]) + 
                 " (valid range: 1 to " + std::to_string(nrows) + ")");
        }
    }
    
    // Validate column indices are within bounds
    IntegerVector col_vec(cols);
    int ncols = Rf_length(x); // Number of columns
    for (int i = 0; i < col_vec.size(); i++) {
        if (IntegerVector::is_na(col_vec[i])) {
            stop("Column indices cannot contain NA values");
        }
        if (col_vec[i] < 1 || col_vec[i] > ncols) {
            stop("Column index out of bounds: " + std::to_string(col_vec[i]) + 
                 " (valid range: 1 to " + std::to_string(ncols) + ")");
        }
    }
    
    // Call the data.table API function with validated inputs
    try {
        return DT_subsetDT(x, rows, cols);
    } catch (const std::exception& e) {
        stop("Error in data.table subset operation: " + std::string(e.what()));
    } catch (...) {
        stop("Unknown error in data.table subset operation");
    }
}
