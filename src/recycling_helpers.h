/* CKutils: an R package with some utility functions I use regularly
Copyright (C) 2025  Chris Kypridemos

CKutils is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see <http://www.gnu.org/licenses/>
or write to the Free Software Foundation, Inc., 51 Franklin Street,
Fifth Floor, Boston, MA 02110-1301  USA. */

#ifndef RECYCLING_HELPERS_H
#define RECYCLING_HELPERS_H

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <type_traits>

using namespace Rcpp;

// Helper structures to hold recycled vectors
struct RecycledVectors2 {
    NumericVector vec1, vec2;
    int n;
};

struct RecycledVectors3 {
    NumericVector vec1, vec2, vec3;
    int n;
};

struct RecycledVectors4 {
    NumericVector vec1, vec2, vec3, vec4;
    int n;
};

struct RecycledVectors5 {
    NumericVector vec1, vec2, vec3, vec4, vec5;
    int n;
};

// Type trait to check if a type is a valid Rcpp vector
template<typename T>
struct is_rcpp_vector : std::false_type {};

template<>
struct is_rcpp_vector<NumericVector> : std::true_type {};

template<>
struct is_rcpp_vector<IntegerVector> : std::true_type {};

// Helper function to convert any Rcpp vector to NumericVector efficiently
template<typename T>
inline NumericVector to_numeric_vector(const T& vec) {
    if constexpr (std::is_same_v<T, NumericVector>) {
        return vec;  // No conversion needed
    } else if constexpr (std::is_same_v<T, IntegerVector>) {
        return as<NumericVector>(vec);  // Convert IntegerVector to NumericVector
    } else {
        static_assert(is_rcpp_vector<T>::value, "Type must be a valid Rcpp vector type");
        return NumericVector();  // Should never reach here
    }
}

// Template function for efficient parameter recycling - 2 vectors
template<typename T1, typename T2>
inline RecycledVectors2 recycle_vectors(const T1& v1, const T2& v2) {
    static_assert(is_rcpp_vector<T1>::value && is_rcpp_vector<T2>::value, 
                  "Arguments must be Rcpp vector types");
    
    const int n1 = v1.length();
    const int n2 = v2.length();
    
    // Convert to NumericVector
    NumericVector nv1 = to_numeric_vector(v1);
    NumericVector nv2 = to_numeric_vector(v2);
    
    // Early escape if lengths are equal
    if (n1 == n2) {
        return {nv1, nv2, n1};
    }
    
    // Find maximum length
    const int max_n = std::max(n1, n2);
    
    // Recycle vectors to max length
    NumericVector rv1(max_n), rv2(max_n);
    
    for (int i = 0; i < max_n; i++) {
        rv1[i] = nv1[i % n1];
        rv2[i] = nv2[i % n2];
    }
    
    return {rv1, rv2, max_n};
}

// Template function for efficient parameter recycling - 3 vectors
template<typename T1, typename T2, typename T3>
inline RecycledVectors3 recycle_vectors(const T1& v1, const T2& v2, const T3& v3) {
    static_assert(is_rcpp_vector<T1>::value && is_rcpp_vector<T2>::value && 
                  is_rcpp_vector<T3>::value, "Arguments must be Rcpp vector types");
    
    const int n1 = v1.length();
    const int n2 = v2.length();
    const int n3 = v3.length();
    
    // Convert to NumericVector
    NumericVector nv1 = to_numeric_vector(v1);
    NumericVector nv2 = to_numeric_vector(v2);
    NumericVector nv3 = to_numeric_vector(v3);
    
    // Early escape if all lengths are equal
    if (n1 == n2 && n2 == n3) {
        return {nv1, nv2, nv3, n1};
    }
    
    // Find maximum length
    const int max_n = std::max({n1, n2, n3});
    
    // Recycle vectors to max length
    NumericVector rv1(max_n), rv2(max_n), rv3(max_n);
    
    for (int i = 0; i < max_n; i++) {
        rv1[i] = nv1[i % n1];
        rv2[i] = nv2[i % n2];
        rv3[i] = nv3[i % n3];
    }
    
    return {rv1, rv2, rv3, max_n};
}

// Template function for efficient parameter recycling - 4 vectors
template<typename T1, typename T2, typename T3, typename T4>
inline RecycledVectors4 recycle_vectors(const T1& v1, const T2& v2, const T3& v3, const T4& v4) {
    static_assert(is_rcpp_vector<T1>::value && is_rcpp_vector<T2>::value && 
                  is_rcpp_vector<T3>::value && is_rcpp_vector<T4>::value, 
                  "Arguments must be Rcpp vector types");
    
    const int n1 = v1.length();
    const int n2 = v2.length();
    const int n3 = v3.length();
    const int n4 = v4.length();
    
    // Convert to NumericVector
    NumericVector nv1 = to_numeric_vector(v1);
    NumericVector nv2 = to_numeric_vector(v2);
    NumericVector nv3 = to_numeric_vector(v3);
    NumericVector nv4 = to_numeric_vector(v4);
    
    // Early escape if all lengths are equal
    if (n1 == n2 && n2 == n3 && n3 == n4) {
        return {nv1, nv2, nv3, nv4, n1};
    }
    
    // Find maximum length
    const int max_n = std::max({n1, n2, n3, n4});
    
    // Recycle vectors to max length
    NumericVector rv1(max_n), rv2(max_n), rv3(max_n), rv4(max_n);
    
    for (int i = 0; i < max_n; i++) {
        rv1[i] = nv1[i % n1];
        rv2[i] = nv2[i % n2];
        rv3[i] = nv3[i % n3];
        rv4[i] = nv4[i % n4];
    }
    
    return {rv1, rv2, rv3, rv4, max_n};
}

// Template function for efficient parameter recycling - 5 vectors
template<typename T1, typename T2, typename T3, typename T4, typename T5>
inline RecycledVectors5 recycle_vectors(const T1& v1, const T2& v2, const T3& v3, const T4& v4, const T5& v5) {
    static_assert(is_rcpp_vector<T1>::value && is_rcpp_vector<T2>::value && 
                  is_rcpp_vector<T3>::value && is_rcpp_vector<T4>::value && 
                  is_rcpp_vector<T5>::value, "Arguments must be Rcpp vector types");
    
    const int n1 = v1.length();
    const int n2 = v2.length();
    const int n3 = v3.length();
    const int n4 = v4.length();
    const int n5 = v5.length();
    
    // Convert to NumericVector
    NumericVector nv1 = to_numeric_vector(v1);
    NumericVector nv2 = to_numeric_vector(v2);
    NumericVector nv3 = to_numeric_vector(v3);
    NumericVector nv4 = to_numeric_vector(v4);
    NumericVector nv5 = to_numeric_vector(v5);
    
    // Early escape if all lengths are equal
    if (n1 == n2 && n2 == n3 && n3 == n4 && n4 == n5) {
        return {nv1, nv2, nv3, nv4, nv5, n1};
    }
    
    // Find maximum length
    const int max_n = std::max({n1, n2, n3, n4, n5});
    
    // Recycle vectors to max length
    NumericVector rv1(max_n), rv2(max_n), rv3(max_n), rv4(max_n), rv5(max_n);
    
    for (int i = 0; i < max_n; i++) {
        rv1[i] = nv1[i % n1];
        rv2[i] = nv2[i % n2];
        rv3[i] = nv3[i % n3];
        rv4[i] = nv4[i % n4];
        rv5[i] = nv5[i % n5];
    }
    
    return {rv1, rv2, rv3, rv4, rv5, max_n};
}

#endif // RECYCLING_HELPERS_H
