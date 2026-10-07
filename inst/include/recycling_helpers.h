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
// The kernels take `int`, and the DPO ones still overflow their own int
// arithmetic at INT_MAX itself: fdDPO_scalar evaluates lgammafn(x + 1), whose
// argument wraps to INT_MIN, and an fpDPO_scalar sum that cannot settle counts
// with `for (int i = ...; i <= q; i++)`, whose i++ is signed-integer overflow
// when i reaches INT_MAX (so that loop never terminates). The BNB, DEL and
// SICHEL kernels no longer do either (see the contract below). INT_MAX - 1 is
// the largest value for which every kernel is well defined.
constexpr double CK_MAX_COUNT = 2147483646.0;  // INT_MAX - 1

// Convert a count argument (x or q, always supplied as a double after
// recycling) to the int the *_scalar kernels expect.
//
// Returns false, leaving `value` untouched, when the double cannot be
// represented: NA/NaN, or a magnitude beyond CK_MAX_COUNT. Callers map a false
// return to NA. Doing the conversion unguarded is out-of-range float-to-int
// undefined behaviour, and it is not benign -- x86-64 saturates to INT_MIN (so
// a huge count silently reads as negative, and a CDF whose true value is ~1
// comes back as 0) while AArch64 saturates to INT_MAX (which reaches the
// INT_MAX cases described in the contract below).
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
// passing a count to any *_scalar kernel. Where the contract is broken the
// consequences differ by family; these are the concrete ones at 0.1.34 (times
// are for one core of an -O2 build, at INT_MAX, for the parameters shown):
//
//   NON-TERMINATION at exactly q == INT_MAX, fpDPO_scalar only, and only for a
//   sum that cannot settle. It accumulates with `for (int i = ...; i <= q; i++)`,
//   so i++ at INT_MAX is signed-integer overflow and a loop that has not stopped
//   by then never exits (the call does not return, and is not interruptible
//   from R). The sum stops once it has settled, so for a finite mu > 0 and
//   sigma > 0 a q of INT_MAX returns at once, and a non-finite mu or sigma
//   returns NaN at once. What still runs to q is a sum of terms that are all 0:
//   mu <= 0 or sigma <= 0, which the wrappers reject. For those,
//   fpDPO_scalar(INT_MAX, 0, 1.5) did not return in 90 s, while q = INT_MAX - 1
//   took 5.8 s.
//       fpDPO_scalar     (inst/include/distr_DPO.h)
//
//   A WRONG LOG DENSITY at exactly x == INT_MAX. fdDPO_scalar evaluates
//   lgammafn(x + 1), whose argument wraps to INT_MIN (a pole): the log density
//   comes back as -Inf, e.g. fdDPO_scalar(INT_MAX, 2, 1.5, log = TRUE) where the
//   true value is about -2.8e10. On the natural scale that example is 0, which
//   is also the true value to double precision.
//       fdDPO_scalar     (inst/include/distr_DPO.h)
//
// The BNB, DEL and SICHEL kernels no longer fail at INT_MAX itself (they used to:
// the BNB and DEL CDF loops never returned, the BNB and DEL densities wrapped,
// and the DEL and SICHEL kernels allocated O(y) memory, with y + 1 or y + 2
// overflowing at the boundary). Now:
//
//   - fdBNB_scalar and fdZABNB_scalar work in double and are right at INT_MAX:
//     fdBNB_scalar(INT_MAX, 2, 1, 1) is 1.21169035e-27, the exact value
//     12 / ((x + 2)(x + 3)(x + 4)) to the 9 digits printed (it was 0).
//   - fpBNB_scalar and fpZABNB_scalar count with a 64-bit index and return at
//     INT_MAX: fpBNB_scalar(INT_MAX, 2, 1, 1) takes 4.8 s (about 2.3 ns per
//     term). They stop earlier when the terms underflow past the mode, as they
//     do for a small sigma (sigma = 0.01: 0.001 s).
//   - ftofydel2_scalar, fdDEL_scalar and fpDEL_hlp_fn (so fpDEL_scalar) carry
//     one recurrence state, O(1) memory, and return at INT_MAX:
//     fdDEL_scalar(INT_MAX, 2, 1, 0.5) takes 17.6 s and fpDEL_hlp_fn(INT_MAX,
//     2, 1, 0.5) 63.5 s (median of 3).
//   - ftofySICHEL2_scalar and fcdfSICHEL_scalar (so fdSICHEL_scalar,
//     fpSICHEL_scalar and the ZISICHEL scalars) carry only the previous step of
//     the recursion: O(1) memory, and no overflow at INT_MAX.
//     fdSICHEL_scalar(INT_MAX, 2, 1, -0.5) takes 13.9 s. The CDF stops once its
//     sum has settled, so fpSICHEL_scalar(INT_MAX, 2, 1, -0.5) is instant, and
//     fpSICHEL_scalar(INT_MAX, 1e9, 1, -0.5), which does not settle early,
//     takes 25.9 s.
//   Their cost is TIME: they are O(q) (O(x) for a density) and not interruptible
//   from R (the searches behind the R functions fqDEL, fqSICHEL and fqDPO, in
//   src/, check for an interrupt; these kernels do not, as a LinkingTo caller may
//   run them where R cannot be called), so a large-but-legal count is slow rather
//   than wrong. Bound q well below CK_MAX_COUNT in anything performance-sensitive.
//
// One more time cost, in DPO: the normalising constant of fdDPO_scalar and
// fpDPO_scalar sums over a window around mu. With a huge mu AND a huge sigma
// (mu = 3e9, sigma = 4e6, for which the window starts at 0) it meets no stopping
// rule, scans the whole int range and only then gives NaN: 49.6 s for
// fpDPO_scalar(10, 3e9, 4e6), and the same ~50 s on the first call for
// fdDPO_scalar(1, 3e9, 4e6). The wrappers inherit it: fqDPO, fpDPO and fdDPO take
// that long to answer NA / NaN for such a parameter set.
//
// The kernels that are safe for any int, because they are closed form or bounded
// by their support, are: fdNBI_scalar / fpNBI_scalar (and so the ZANBI and
// ZINBI scalars built on them), and fdMN4_scalar / fpMN4_scalar. The fq*_search
// quantile searches (distr_search.h) scan at most CK_SEARCH_MAX = INT_MAX - 1
// terms and return "not found" (NA) for a quantile beyond that, and give up
// earlier on a search that cannot reach p; they are O(quantile) in time.
//
// The frNBI_scalar / frZANBI_scalar / frZINBI_scalar samplers take a uniform
// and invert the corresponding fq*_scalar, so they inherit its bound and hold no
// RNG state of their own. They are safe only for a finite mu and sigma and a
// uniform u < 1, and return NA_INTEGER (not a count) when the quantile is not
// an int in [0, CK_MAX_COUNT]: u = 1 for frNBI_scalar (the ZANBI and ZINBI
// ones stay finite there), a non-finite mu, or a mu so large that the quantile
// exceeds INT_MAX - 1 (frNBI_scalar(0.5, 1e10, 1) is NA).

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
