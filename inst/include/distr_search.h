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

#ifndef CKUTILS_DISTR_SEARCH_H
#define CKUTILS_DISTR_SEARCH_H

// Shared by the discrete quantile searches (BNB, SICHEL, DPO, DEL; ZIBNB, ZABNB
// and ZISICHEL through them), which add one probability term at a time, from 0,
// until the CDF reaches p. A search must give up -- and report "not found"
// (NA_INTEGER) rather than a number -- when the CDF can no longer get there:
//   - a term that is not finite (the density computation broke down), or
//   - past the largest term (terms falling), a term too small to change the
//     sum in double precision while the sum is still below p (the rest of the
//     tail cannot add up to p either), or
//   - a quantile beyond CK_SEARCH_MAX, which an int cannot hold.
// Up to 0.1.34 the searches stopped after 1e6 terms instead, and returned 1e6
// as if it were the quantile.

#include <climits>
#include <cmath>

constexpr int CK_SEARCH_MAX = INT_MAX - 1;

// `cdf` is the sum before `term` is added; `prev_term` the previous term (pass
// a negative value for the first).
inline bool ck_search_gives_up(const double& term, const double& prev_term,
                               const double& cdf) {
    return !std::isfinite(term) ||
           (cdf > 0.0 && term < prev_term && cdf + term == cdf);
}

// Where to start a search whose term at any index can be computed directly
// (BNB, DPO), for p > 0. With a large mu the first terms underflow to exactly
// 0 and add nothing, for as long as it takes to reach the mass. The
// distributions are unimodal and right-skewed: the terms rise up to the mode,
// which lies at or below the mean. So the first positive term is found by
// bisection between 0 and the mean, and the search starts there with the same
// result, as the terms skipped were 0. Returns 0 when the term at 0 is
// positive, or when the term at the mean is 0 too (then the search scans from
// 0 as before). Only whether a term underflows is asked: neighbouring terms
// are not compared, as at an index near INT_MAX the log terms, differences of
// lgamma values ~4e10, carry rounding noise far larger than their steps.
template <class LogTerm>
inline long long ck_search_start(LogTerm log_term, const double& mean) {
    auto positive = [&](long long i) { return std::exp(log_term(static_cast<double>(i))) > 0.0; };
    if (positive(0)) return 0;
    const double top = std::min(std::floor(mean), static_cast<double>(CK_SEARCH_MAX));
    if (!(top >= 1.0)) return 0;
    long long hi = static_cast<long long>(top);
    if (!positive(hi)) return 0;
    long long lo = 0;  // term(lo) is 0, term(hi) positive
    while (hi - lo > 1) {
        const long long mid = lo + (hi - lo) / 2;
        if (positive(mid)) hi = mid; else lo = mid;
    }
    return hi;
}

#endif
