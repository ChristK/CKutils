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

#ifndef DISTR_DEL_H
#define DISTR_DEL_H

// Header-only scalar API for the Delaporte (DEL) distribution.
//
// The *_scalar functions (and the helpers they call) below are defined
// `inline` so that a downstream package can `LinkingTo: CKutils`,
// `#include <distr_DEL.h>` and call them directly from its own C++ (e.g. in a
// hot per-row loop) without linking against CKutils.so. The vectorised,
// Rcpp-exported wrappers (declared at the bottom) live in src/distr_DEL.cpp and
// call these same inline scalars.
//
// CALLER CONTRACT (unguarded on purpose -- see recycling_helpers.h for the full
// statement). These kernels do no bounds checking, so the caller must ensure
//     0 <= x, q <= CK_MAX_COUNT   (INT_MAX - 1)
// before calling them; count_to_int() in recycling_helpers.h does that test.
// The vectorised wrappers below already apply it, but a package using
// LinkingTo: CKutils to call the scalars directly does not get it. Here the
// kernels terminate and use O(1) memory for any int, INT_MAX included, but
// they are O(x) / O(q) in TIME (one log, one division and, for the CDF, one
// exp and one lgamma per index), so a large-but-legal count is slow rather
// than wrong: about a minute at q = 2^31 - 2 for fpDEL_hlp_fn.
// Before 0.1.34, ftofydel2_scalar rebuilt its recurrence in a std::vector of
// y + 2 doubles on every call (O(y) memory, and y + 2 overflowed at the
// boundary), so fpDEL_hlp_fn and the quantile search were O(q^2) in time,
// fpDEL_hlp_fn's `for (int i = 0; i <= q; i++)` never returned at
// q == INT_MAX, and fdDEL_scalar's lgamma(x + 1) wrapped at x == INT_MAX.
//
// ACCURACY. CkDELRecurrence evaluates log f(j) = logpy0 - lgamma(j + 1) + S_j:
// three numbers of size j log j that nearly cancel, summed in double precision,
// so its error grows quickly with j. Against the exact Poisson (x) negative
// binomial convolution (long double) the CDF is off, at worst over sigma 0.05-3,
// nu 0.1-0.9, by about 7e-11 at q = 4095, 1.3e-10 at 8e3, 3e-9 at 65535, 1e-5 to
// 3e-4 at 1e7 and, from 1e8 on, by 1e-2 up to 1e4 (no longer a probability);
// quantiles were wrong by up to 6.6e5 units, or came back as a false NA. Up to
// CK_DEL_COMPENSATE_AFTER (4096) indices that recurrence is still what runs, bit
// for bit as before, so nothing changes for the counts the models use (<= 10) or
// for any value computed in fewer steps than that. Below that index its error is
// inside 1e-10 for sigma 0.05-3, nu 0.1-0.9 (<= 7e-11) and for 2539 of 2540
// parameter sets (mu 0.3 to 16000, sigma 1e-4 to 100, nu to within 1e-6 of 0 or
// 1; 2500 of them random). The exception is nearly pure Poisson parameters
// (1 - nu <= 1e-5, a negative binomial part of mean < 0.04): every term ratio is
// then the same lam, S_j adds the same increment each step and its rounding
// drifts, to 1.1e-10 at q = 1500 and 1.4e-9 at 4095. From that index on the
// density, the CDF and the quantile search use CkDELAccurate (below), which has
// no such cancellation and measured |F - F_ref| < 6e-13 for 4096 <= q <= 65535
// on the same 2540 sets (3e-14 for the nearly Poisson ones), < 5e-12 for mu from
// 1e5 to 2e9 (q up to 2.0e9, near the largest legal count), sigma 0.05-3, nu
// 0.1-0.9, and < 2e-11 on a random fuzz of 297 parameter sets (sigma 1e-4 to 100,
// nu within 1e-6 of 0 or 1). It takes about 13 ns per index in the CDF and the
// quantile search, where CkDELRecurrence took 23, but a call costs a fixed extra
// from 4096 on: about 70 us for the Poisson window (R::dpois per index, about
// 39 sqrt(lam) of them) and, for a quantile search, the pass of the old recurrence
// to index 4095 before the restart (another 95 us). So counts of 4096 to about
// 2e4 (CDF) or 5e4 (quantile) take up to 0.2 ms longer than before; above that
// the call is faster, up to 2.3 times.

#include <Rcpp.h>   // brings in the R:: namespace math functions (dpois, dnbinom_mu, ...)
#include <cmath>

// dPO (Poisson) density scalar helper
inline double fdPO_scalar(const int &x, const double &mu = 1.0, const double &sigma = 1.0, const bool &log_ = false)
{
  // if (x < 0) stop("x must be >=0");
  // if (mu <= 0.0) stop("mu must be greater than 0");
  // if (sigma <= 0.0) stop("sigma must be greater than 0");
  double fy;
  if (sigma > 1e-04)
  {
    fy = R::dnbinom_mu(x, 1.0 / sigma, mu, log_);
  }
  else
  {
    fy = R::dpois(x, mu, log_);
  }
  return fy;
}

// The recurrence of gamlss.dist's tofydel2, carried forward one index at a
// time. With t_j = (j + 1) f(j + 1) / f(j) and S_j = sum_{k < j} log(t_k),
//     log f(j) = logpy0 - lgamma(j + 1) + S_j,
//     logpy0   = -mu nu - (1 / sigma) log(1 + mu sigma (1 - nu)).
// The state (j, t_j, S_j) advances with the same expressions, in the same
// order, as the earlier std::vector loop did, so every value is the same
// double; but it takes O(1) memory and one step per index, where rebuilding
// t_0..t_{y-1} for every y made the CDF and the quantile search O(q^2).
// advance() moves j to j + 1: the caller must not advance past INT_MAX.
// (The Poisson branch, sigma < 1e-4, does not use it.)
struct CkDELRecurrence {
  int j;          // the index
  double t;       // t_j
  double S;       // S_j
  double logpy0;  // log f(0)
  double mu_nu, inv_sigma_1_minus_nu, dum_const;
  CkDELRecurrence(const double &mu, const double &sigma, const double &nu) {
    mu_nu = mu * nu;
    const double one_minus_nu = 1.0 - nu;
    const double mu_sigma_1_minus_nu = mu * sigma * one_minus_nu;
    const double sigma_1_minus_nu = sigma * one_minus_nu;
    t = mu_nu + mu * one_minus_nu / (1.0 + mu_sigma_1_minus_nu);   // t_0
    inv_sigma_1_minus_nu = 1.0 / sigma_1_minus_nu;
    dum_const = 1.0 + 1.0 / mu_sigma_1_minus_nu;
    logpy0 = -mu * nu - (1.0 / sigma) * log(1.0 + mu * sigma * one_minus_nu);
    S = 0.0;
    j = 0;
  }
  inline void advance() {
    S += log(t);
    ++j;
    t = (j + mu_nu + inv_sigma_1_minus_nu - (mu_nu * j) / t) / dum_const;
  }
  // log f(j); j + 1.0, not j + 1, so that j == INT_MAX does not overflow
  inline double log_density() const { return logpy0 - lgamma(j + 1.0) + S; }
};

// The index from which the density, the CDF and the quantile search use
// CkDELAccurate rather than CkDELRecurrence (see ACCURACY above). Every value
// computed in fewer steps than this is unchanged. 4096 is the largest power of
// two for which CkDELRecurrence is still inside 1e-10 for sigma 0.05-3,
// nu 0.1-0.9 (7e-11 at 4095, 1.3e-10 at 8e3); 1024 would also cover the
// nearly Poisson corner (6e-11 at 1000) but would change the values at counts
// of 1024 to 4095.
constexpr int CK_DEL_COMPENSATE_AFTER = 4096;

// Densities below lam - CK_DEL_SKIP_SD sqrt(lam) - 10 are not evaluated (they
// underflow: see CkDELAccurate); a ratio f(j + 1) / f(j) below
// CK_DEL_RATIO_FLOOR is clamped to it (a spent tail, where the e-state below
// cannot resolve it).
constexpr double CK_DEL_SKIP_SD = 39.0;
constexpr double CK_DEL_RATIO_FLOOR = 1e-300;

// Neumaier-compensated addition: s + c is the running sum of the x added so far.
inline void ck_del_neumaier_add(double &s, double &c, const double &x) {
  const double t = s + x;
  c += (std::fabs(s) >= std::fabs(x)) ? ((s - t) + x) : ((x - t) + s);
  s = t;
}

// The same recurrence, without the cancellation. DEL(mu, sigma, nu) is
// Poisson(lam) + NB(size r, success probability p), with lam = mu nu, r = 1 / sigma,
// p = 1 / (1 + mu sigma (1 - nu)) and q = 1 - p; its pgf gives the three-term
// recurrence (j + 1) f(j + 1) = (q j + lam + r q) f(j) - lam q f(j - 1), i.e.
// for t_j = (j + 1) f(j + 1) / f(j) (what tofydel2 recurs on)
//     t_j = q j + lam + r q - lam q j / t_{j-1},   t_0 = lam + r q,  t_j >= lam + r q.
// Instead of t_j (size ~ j, so each step rounds at the 1e-16 relative level of
// a number up to 1e9, and the rounding of the constants lam, p, 1 + 1 / (mu sigma
// (1 - nu)) is amplified along the way) the state is a deviation that stays small:
//   left of the Poisson peak (mode 0, j < floor(lam)), s_j = t_j - lam, with
//       s_0 = r q,  s_{j+1} = q (r + (j + 1) s_j / (lam + s_j))
//     (positive terms only), and log f(j) = log Pois(j; lam) + C_j with
//       C_0 = r log p,  C_{j+1} = C_j + log1p(s_j / lam).
//     The large part, the Poisson log density, is one call of R::dpois(), which is
//     accurate at the peak (in the far tail it is only good to ~1e-9, where the
//     densities are below 1e-14); C_j lies between r log p = -(1 / sigma) log(1 + mu
//     sigma (1 - nu)), the size of the NB normalisation, and 0, not of the size of j. For j <= lam,
//     f(j) = sum_i NB(i) Pois(j - i) <= Pois(j; lam), so every density below
//     lam - 39 sqrt(lam) - 10 underflows to 0 (a Poisson log density below
//     -760): density() returns that 0 without evaluating anything.
//   from the peak on (mode 1), e_j = t_j - (j + 1), which is O(1) .. O(sd):
//       e_0 = lam + r q - 1 (only if lam < 1),
//       e_{j+1} = p (lam - (j + 1)) + (r q - 1) + lam q e_j / t_j,  t_j = (j + 1) + e_j,
//     and log f(j + 1) = log f(j) + log1p(e_j / (j + 1)), log f carried as a
//     Neumaier-compensated pair (no rounding accumulates in it). lam q e / t is
//     evaluated as u - p u, and p, not q = 1 - p ~ 1, is the NB parameter used
//     everywhere, so that one set of constants fixes both the normalisation
//     log f(0) = -lam + r log p and the steps.
//   mode 2 only for parameters that give no usable density (NaN, an infinite lam or
//     r, or p == 0 from an overflow in mu sigma): every density is NaN, as
//     CkDELRecurrence's are.
// advance() moves j to j + 1: the caller must not advance past INT_MAX. The
// Poisson branch (sigma < 1e-4) does not use it. O(1) memory, O(j) time: about
// 10 ns per index in mode 0 and 14 in mode 1 (CkDELRecurrence: 28 and 18).
struct CkDELAccurate {
  int j;           // the index
  int mode;        // 0: s-state, 1: e-state, 2: no recurrence
  double v;        // s_j (mode 0) or e_j (mode 1)
  double L, Lc;    // mode 0: C_j; mode 1: log f(j); mode 2: log f(j) for all j. The value is L + Lc
  double lam, p, q, r, K1;   // K1 = r q - 1
  double peak;     // floor(lam): the index where mode 0 hands over to mode 1
  double skip;     // mode 0: the densities below this index are 0
  CkDELAccurate() : j(0), mode(2), v(0.0), L(-INFINITY), Lc(0.0), lam(0.0), p(0.0), q(0.0), r(0.0),
                    K1(0.0), peak(0.0), skip(0.0) {}
  CkDELAccurate(const double &mu, const double &sigma, const double &nu)
      : j(0), mode(2), v(0.0), L(0.0), Lc(0.0) {
    lam = mu * nu;
    p = 1.0 / (1.0 + mu * sigma * (1.0 - nu));
    q = 1.0 - p;
    r = 1.0 / sigma;
    const double pr = p * r;
    K1 = (r - 1.0) - pr;
    peak = std::floor(lam);
    skip = 0.0;
    if (!std::isfinite(lam) || !(p > 0.0) || !std::isfinite(r)) {
      L = NAN;                                  // mode 2: NaN densities, as CkDELRecurrence gives
    } else if (peak < 1.0) {                    // lam < 1: nothing to gain from a Poisson part
      mode = 1;
      v = (lam + (r - pr)) - 1.0;               // e_0
      L = -lam;
      ck_del_neumaier_add(L, Lc, r * std::log(p));
    } else {
      mode = 0;
      v = q * r;                                // s_0
      L = r * std::log(p);                      // C_0
      skip = std::floor(lam - CK_DEL_SKIP_SD * std::sqrt(lam) - 10.0);
      if (skip < 0.0) skip = 0.0;
    }
  }
  inline void advance() {
    if (mode == 1) {
      const double jp1 = j + 1.0;
      double t = jp1 + v;                       // t_j
      if (!(t >= CK_DEL_RATIO_FLOOR * jp1)) {   // f(j + 1) / f(j) below the floor, or not a number: a spent tail
        t = CK_DEL_RATIO_FLOOR * jp1;
        v = t - jp1;
      }
      // log(t / (j + 1)): log1p of the small deviation unless the ratio is below 1/2, where
      // t = (j + 1) + e does not cancel and log1p(-1) = -Inf would not do
      ck_del_neumaier_add(L, Lc, (v > -0.5 * jp1) ? std::log1p(v / jp1) : std::log(t / jp1));
      const double u = lam * (v / t);
      v = p * (lam - jp1) + K1 + (u - p * u);
      ++j;
    } else if (mode == 0) {
      ck_del_neumaier_add(L, Lc, std::log1p(v / lam));
      v = q * (r + (j + 1.0) * (v / (lam + v)));
      ++j;
      if (static_cast<double>(j) >= peak) {     // the Poisson peak: hand over to the e-state
        double Lf = R::dpois(static_cast<double>(j), lam, true), Lfc = 0.0;
        ck_del_neumaier_add(Lf, Lfc, L);
        ck_del_neumaier_add(Lf, Lfc, Lc);
        v = (lam - (j + 1.0)) + v;              // e_j = lam + s_j - (j + 1)
        L = Lf;
        Lc = Lfc;
        mode = 1;
      }
    } else {
      ++j;
    }
  }
  // log f(j)
  inline double log_density() const {
    if (mode == 0) return R::dpois(static_cast<double>(j), lam, true) + (L + Lc);
    return L + Lc;
  }
  // f(j), without evaluating the densities that underflow
  inline double density() const {
    if (mode == 0 && static_cast<double>(j) < skip) return 0.0;
    return std::exp(log_density());
  }
};

// S_y = sum_{j < y} log(t_j) (helper used by fdDEL_scalar): O(y) time, O(1)
// memory. Only meaningful, as a way to the density, below
// CK_DEL_COMPENSATE_AFTER: fdDEL_scalar does not use it beyond.
inline double ftofydel2_scalar(const int &y, const double &mu,
                       const double &sigma, const double &nu) {
    if (y <= 0) return 0.0;
    CkDELRecurrence r(mu, sigma, nu);
    while (r.j < y) r.advance();   // ends at j == y, so y == INT_MAX is safe
    return r.S;
}

// Optimized scalar density function
inline double fdDEL_scalar(const int &x,
                      const double &mu,
                      const double &sigma,
                      const double &nu,
                      const bool &log_ = false)
{
  double logfy = 0.0;
  if (sigma < 1e-04) {
    logfy = R::dpois(x, mu, (int)log_);
  } else if (x >= CK_DEL_COMPENSATE_AFTER) {
    CkDELAccurate a(mu, sigma, nu);
    while (a.j < x) a.advance();   // ends at j == x, so x == INT_MAX is safe
    logfy = a.log_density();
    if (!log_)
      logfy = exp(logfy);
  } else {
    const double one_minus_nu = 1.0 - nu;
    double logpy0 = -mu * nu - (1.0 / sigma) *
                    log(1.0 + mu * sigma * one_minus_nu);
    double S = ftofydel2_scalar(x, mu, sigma, nu);
    logfy = logpy0 - lgamma(x + 1.0) + S;   // x + 1.0: no overflow at INT_MAX
    if (!log_)
      logfy = exp(logfy);
  }
  return logfy;
}

// The running CDF F(q) = the densities at 0..q added in that order in double
// precision (gamlss.dist's pDEL is sum(dDEL(0:q, ...)), which R accumulates in
// long double, so the two agree to rounding, not bit for bit) of ONE parameter
// set, extendable to a larger q without starting again. fpDEL_hlp_fn is one
// pass of it; the vector fpDEL keeps one across consecutive elements with the
// same parameters. The recurrence is built for every sigma (the Poisson branch
// does not use it), so a NaN sigma gives NaN terms rather than stale state.
// Up to index CK_DEL_COMPENSATE_AFTER - 1 the terms come from CkDELRecurrence;
// the first `to` at or beyond it restarts the sum from index 0 with
// CkDELAccurate (built then, not before: one more log and sqrt per parameter
// set would cost the small-q calls a few percent), and the sum goes on with it.
// The values already returned are not touched.
struct CkDELCdf {
  CkDELRecurrence r;
  CkDELAccurate a;   // built only when the sum has to pass CK_DEL_COMPENSATE_AFTER
  double mu, sigma, nu;
  bool poisson;      // sigma < 1e-04: the terms are Poisson(mu) densities
  bool accurate;     // the sum is being made with `a`
  double ans;        // the sum of the densities at 0..q
  int q;             // the last index added; -1 before the first
  CkDELCdf(const double &mu_, const double &sigma_, const double &nu_)
      : r(mu_, sigma_, nu_), a(), mu(mu_), sigma(sigma_), nu(nu_), poisson(sigma_ < 1e-04),
        accurate(false), ans(0.0), q(-1) {}
  // Start again on another parameter set, in place: what `*this = CkDELCdf(...)`
  // does, without building and copying `a` (a vector fpDEL with a parameter set
  // per element would pay for that on every element).
  inline void reset(const double &mu_, const double &sigma_, const double &nu_) {
    r = CkDELRecurrence(mu_, sigma_, nu_);
    mu = mu_; sigma = sigma_; nu = nu_;
    poisson = sigma_ < 1e-04;
    accurate = false;
    ans = 0.0;
    q = -1;
  }
  // F(to), for any 0 <= to <= INT_MAX. A `to` below q returns the sum at q.
  // The loop ends at index `to` before incrementing, so INT_MAX returns.
  inline double advance_to(const int &to) {
    if (poisson) {
      while (q < to) {
        ++q;
        ans += R::dpois(q, mu, false);
      }
      return ans;
    }
    if (!accurate && to >= CK_DEL_COMPENSATE_AFTER) {
      a = CkDELAccurate(mu, sigma, nu);
      accurate = true;
      ans = 0.0;
      q = -1;
    }
    if (accurate) {
      while (q < to) {
        if (q >= 0) a.advance();   // a.j == q + 1
        ++q;
        ans += a.density();
      }
      return ans;
    }
    while (q < to) {
      if (q >= 0) r.advance();   // r.j == q + 1
      ++q;
      ans += exp(r.log_density());
    }
    return ans;
  }
};

// CDF helper: the sum of the densities at 0..q. One recurrence step per
// index: O(q) time, O(1) memory.
inline double fpDEL_hlp_fn(const int &q,
                      const double &mu,
                      const double &sigma,
                      const double &nu)
{
  if (q < 0) return 0.0;
  CkDELCdf cdf(mu, sigma, nu);
  return cdf.advance_to(q);
}

// Optimized scalar CDF function
inline double fpDEL_scalar(const int &q,
                      const double &mu,
                      const double &sigma,
                      const double &nu,
                      const bool &lower_tail = true,
                      const bool &log_p = false)
{
  double cdf = fpDEL_hlp_fn(q, mu, sigma, nu);
  if (!lower_tail)
    cdf = 1.0 - cdf;
  if (log_p)
    cdf = log(cdf);

  return cdf;
}

// Vectorised, Rcpp-exported wrappers (defined in src/distr_DEL.cpp)
Rcpp::NumericVector fdDEL(const Rcpp::IntegerVector &x,
                         const Rcpp::NumericVector &mu,
                         const Rcpp::NumericVector &sigma,
                         const Rcpp::NumericVector &nu,
                         const bool &log_);

Rcpp::NumericVector fpDEL(const Rcpp::IntegerVector &q,
                         const Rcpp::NumericVector &mu,
                         const Rcpp::NumericVector &sigma,
                         const Rcpp::NumericVector &nu,
                         const bool &lower_tail,
                         const bool &log_p);

Rcpp::NumericVector fqDEL(Rcpp::NumericVector p,
                         const Rcpp::NumericVector &mu,
                         const Rcpp::NumericVector &sigma,
                         const Rcpp::NumericVector &nu,
                         const bool &lower_tail,
                         const bool &log_p);

#endif // DISTR_DEL_H
