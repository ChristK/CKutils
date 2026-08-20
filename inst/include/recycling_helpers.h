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

// Enable vectorization hints for modern compilers. Centralised here (every
// distr_*.cpp includes this header) so the vectorised wrappers have SIMD_HINT
// available regardless of header-include order. Guarded so the identical local
// re-definitions still present in some .cpp files remain harmless.
#ifndef SIMD_HINT
#if defined(__GNUC__) || defined(__clang__)
#define SIMD_HINT _Pragma("GCC ivdep")
#else
#define SIMD_HINT
#endif
#endif

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

    // Any zero-length input -> zero-length result (R recycling rule). This also
    // guards the i % nK below: with a zero length that modulo is i % 0, which is
    // undefined behaviour (SIGFPE on x86) once max_n > 0.
    if (n1 == 0 || n2 == 0) {
        return {NumericVector(0), NumericVector(0), 0};
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

    // Any zero-length input -> zero-length result (R recycling rule). This also
    // guards the i % nK below: with a zero length that modulo is i % 0, which is
    // undefined behaviour (SIGFPE on x86) once max_n > 0.
    if (n1 == 0 || n2 == 0 || n3 == 0) {
        return {NumericVector(0), NumericVector(0), NumericVector(0), 0};
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

    // Any zero-length input -> zero-length result (R recycling rule). This also
    // guards the i % nK below: with a zero length that modulo is i % 0, which is
    // undefined behaviour (SIGFPE on x86) once max_n > 0.
    if (n1 == 0 || n2 == 0 || n3 == 0 || n4 == 0) {
        return {NumericVector(0), NumericVector(0), NumericVector(0), NumericVector(0), 0};
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

    // Any zero-length input -> zero-length result (R recycling rule). This also
    // guards the i % nK below: with a zero length that modulo is i % 0, which is
    // undefined behaviour (SIGFPE on x86) once max_n > 0.
    if (n1 == 0 || n2 == 0 || n3 == 0 || n4 == 0 || n5 == 0) {
        return {NumericVector(0), NumericVector(0), NumericVector(0), NumericVector(0), NumericVector(0), 0};
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

// ---------------------------------------------------------------------------
// Shared argument guards for the distribution wrappers
// ---------------------------------------------------------------------------

// Largest count the header-only *_scalar kernels can be handed safely.
//
// The kernels take `int` and overflow their own int arithmetic at INT_MAX
// itself: the CDF kernels accumulate with `for (int i = 0; i <= q; i++)`, whose
// i++ is signed-integer overflow when i reaches INT_MAX (so the loop never
// terminates), and the density kernels evaluate lgammafn(x + 1), which wraps to
// a negative argument and silently returns a wrong density. INT_MAX - 1 is the
// largest value for which both are well defined.
constexpr double CK_MAX_COUNT = 2147483646.0;  // INT_MAX - 1

// Convert a count argument (x or q, always supplied as a double after
// recycling) to the int the *_scalar kernels expect.
//
// Returns false, leaving `value` untouched, when the double cannot be
// represented: NA/NaN, or a magnitude beyond CK_MAX_COUNT. Callers map a false
// return to NA. Doing the conversion unguarded is out-of-range float-to-int
// undefined behaviour, and it is not benign -- x86-64 saturates to INT_MIN (so
// a huge count silently reads as negative, and a CDF whose true value is ~1
// comes back as 0) while AArch64 saturates to INT_MAX (which walks the
// accumulation loops described above).
//
// This asks only "is the value representable as an int?", never "is it a valid
// support point?". Range validity stays with each wrapper, which is what lets
// the count distributions reject a negative x with stop() while fdMN4/fpMN4,
// whose support is the four categories 1:4, keep returning a density of 0 for
// an out-of-category value such as -1.
inline bool count_to_int(const double& v, int& value) {
    if (ISNAN(v) || v > CK_MAX_COUNT || v < -CK_MAX_COUNT) return false;
    value = static_cast<int>(v);
    return true;
}

// ===========================================================================
// CALLER CONTRACT FOR THE HEADER-ONLY *_scalar KERNELS  (read before using
// them from a package that declares LinkingTo: CKutils)
// ===========================================================================
//
// count_to_int() above guards the R-facing vectorised wrappers in src/. It does
// NOT guard the inline *_scalar kernels in inst/include/distr_*.h, and those
// kernels DELIBERATELY perform no bounds checking of their own -- they exist to
// be called from a hot per-row loop, so every branch they do not take is the
// point of them. A downstream C++ caller therefore owns the check.
//
// The contract is:
//
//     0 <= x, q <= CK_MAX_COUNT        (i.e. <= INT_MAX - 1)
//
// Call count_to_int() yourself, or otherwise establish the bound, before
// passing a count to any *_scalar kernel. Violating it is not a graceful
// failure -- these are the concrete consequences:
//
//   NON-TERMINATION at exactly q == INT_MAX. These accumulate with
//   `for (int i = 0; i <= q; i++)`, so when i reaches INT_MAX the i++ is
//   signed-integer overflow and the loop never exits (a call does not return,
//   and is not interruptible from R):
//       fpBNB_scalar     (inst/include/distr_BNB.h)
//       fpDPO_scalar     (inst/include/distr_DPO.h)
//       fpDEL_hlp_fn     (inst/include/distr_DEL.h), and fpDEL_scalar via it
//       fpZABNB_scalar   (inst/include/distr_ZABNB.h), via fpBNB_scalar
//
//   SILENTLY WRONG RESULTS at exactly x == INT_MAX. These evaluate
//   lgamma(x + 1), whose argument wraps to INT_MIN:
//       fdBNB_scalar     (inst/include/distr_BNB.h)
//       fdDPO_scalar     (inst/include/distr_DPO.h)
//       fdDEL_scalar     (inst/include/distr_DEL.h)
//       fdZABNB_scalar   (inst/include/distr_ZABNB.h), via fdBNB_scalar
//   e.g. fdBNB_scalar(INT_MAX, 2, 1, 1) returns 0 where the true density is
//   about 2.2e-27.
//
//   O(y) ALLOCATION, and a negative size_t at y == INT_MAX. These size a
//   std::vector from the count itself, so a large y asks for tens of gigabytes
//   and y + 1 / y + 2 overflows at the boundary:
//       ftofydel2_scalar (inst/include/distr_DEL.h)  -- vector(y + 2)
//       fdSICHEL_scalar, fpSICHEL_scalar (inst/include/distr_SICHEL.h)
//                                                    -- two vector(y + 1)
//       fpZISICHEL_scalar (inst/include/distr_ZISICHEL.h), via fpSICHEL_scalar
//
// Separately from correctness, note that the CDF kernels above are O(q) in
// TIME with three lgamma/lbeta calls per step, so a merely large-but-legal q
// is slow rather than wrong: q = 2^31 - 1024 takes roughly five minutes.
// Budget for that, or bound q well below CK_MAX_COUNT.
//
// The kernels that are safe for any int, because they are closed form or
// bounded by their support, are: fdNBI_scalar / fpNBI_scalar (and so the ZANBI
// and ZINBI scalars built on them), fdMN4_scalar / fpMN4_scalar, and the
// fq*_search quantile searches, which cap their own iteration count.

// Validate a probability argument of a quantile function on the scale the
// caller actually supplied it.
//
// With log_p = true the argument is log(p), so the admissible range is
// [-Inf, 0]; on the natural scale it is [0, upper] (upper is 1, or the 1.0001
// slack some of the gamlss.dist ports carry). Validating the natural-scale
// range against a log-scale argument rejects every legitimate log(p) < 0, which
// is what made log_p unusable across this family.
//
// NaN returns without erroring: the callers map NaN to NA in their compute
// loop, and every NaN comparison is false in any case. `msg` carries each
// function's established natural-scale wording so error-matching tests keep
// working.
inline void check_prob(const double& p, const bool& log_p, const double& upper,
                       const char* msg) {
    if (ISNAN(p)) return;
    if (log_p) {
        if (p > 0.0) stop("p must be <=0 when log_p = TRUE (p is on the log scale)");
    } else if (p < 0.0 || p > upper) {
        stop("%s", msg);
    }
}

#endif // RECYCLING_HELPERS_H
