# CKutils 0.1.31

## New features

* **`fdZIBNB()`, `fpZIBNB()` and `fdZISICHEL()`**: the last two partially
  implemented distributions now have the full d/p/q/r set, so every
  distribution CKutils covers is complete with respect to `gamlss.dist`.
  `ZIBNB` previously had only `fqZIBNB()`/`frZIBNB()` and `ZISICHEL` only
  `fpZISICHEL()`/`fqZISICHEL()`/`frZISICHEL()`.

  Both are zero *inflated*, so the zero mass is added to rather than replacing
  the base mass: \eqn{P(Y = 0) = \tau + (1-\tau) f(0)} and
  \eqn{P(Y = y) = (1-\tau) f(y)}. That is the opposite convention to the
  zero *adjusted* `ZABNB`/`ZANBI`, where \eqn{P(Y = 0)} is exactly \eqn{\tau};
  the test suite asserts the contrast directly.

  Supporting changes:

  - **`fdSICHEL_scalar()` is new** in `inst/include/distr_SICHEL.h`. The Sichel
    density had only ever existed inside the `fdSICHEL()` wrapper, so it was
    unavailable to `LinkingTo: CKutils` consumers and to `fdZISICHEL_scalar()`.
    It is now extracted, and `fdSICHEL()` delegates to it rather than
    duplicating the Bessel-function evaluation.
  - **`fdZISICHEL_scalar()` and `fpZISICHEL_scalar()` are new.** `ZISICHEL`
    previously defined no scalars at all; `fpZISICHEL()` inlined the formula.
    Both wrappers now delegate to the scalars, as the rest of the family does.

* **Documentation fix.** The caller contract added in 0.1.30
  named `fdSICHEL_scalar()` and `fpZISICHEL_scalar()` as O(y)-allocating
  kernels, but neither existed at the time. The contract now names the
  functions that actually allocate -- `ftofydel2_scalar()`,
  `ftofySICHEL2_scalar()` and `fcdfSICHEL_scalar()` -- and lists the density
  and CDF kernels that inherit the behaviour through them.

# CKutils 0.1.30

## New features

* **`fdZABNB()`** and **`fpZABNB()`**: the Zero Adjusted (hurdle) Beta Negative
  Binomial distribution now has a density and a distribution function to go with
  the existing `fqZABNB()` and `frZABNB()`, completing the d/p/q/r set. Both
  follow `gamlss.dist::dZABNB()`/`pZABNB()` (Rigby et al. 2019): the zero
  probability is exactly `tau` and the positive part is the BNB mass
  renormalised by `1 - f_BNB(0)`. As with the rest of the family, the scalar
  kernels are `inline` in `inst/include/distr_ZABNB.h`, so a package that
  declares `LinkingTo: CKutils` can call `fdZABNB_scalar()` / `fpZABNB_scalar()`
  directly from C++ without going through R.

  Two deliberate refinements over the reference implementation, both exact in
  real arithmetic:

  - `fdZABNB(0, ...)` returns `tau` unchanged instead of `exp(log(tau))`, which
    is off by up to 1 ulp. In a hurdle model `P(Y = 0) = tau` holds by
    construction, so the round trip buys nothing.
  - The positive branch uses `log1p(-tau)` and `log(-expm1(log f_BNB(0)))`
    rather than `log(1 - tau)` and `log(1 - f_BNB(0))`. The literal differences
    cancel catastrophically for a tiny `tau`, and for a small `mu` (where
    `f_BNB(0)` approaches 1) `1 - f_BNB(0)` can round to exactly 0 and send the
    density to `-Inf`. `fdZABNB()` is finite down to `mu = 1e-8`.

  Both also return `NA` for an `x`/`q` too large to convert to `int`, rather
  than performing the conversion. Out-of-range float-to-int conversion is
  undefined behaviour and it is not benign here: x86 saturates to `INT_MIN`, so
  the CDF would silently report 0 where the truth is ~1, while AArch64
  saturates to `INT_MAX`. `.Machine$integer.max` itself is excluded as well,
  because the BNB scalars overflow their own `int` arithmetic exactly there:
  the `0..q` accumulation in `fpBNB_scalar()` never terminates, and
  `fdBNB_scalar()` evaluates `lgammafn(x + 1)`, which collapses the density to
  0 when the true value is ~2.2e-27.
  (`gamlss.dist::pZABNB()` simply hangs on such an input.) The same guard is
  now shared with the rest of the family -- see the bug fixes below.

## Bug fixes

* **All `fd*()` / `fp*()` functions**: an `x`/`q` too large to convert to `int`
  now yields `NA` instead of undefined behaviour. Every wrapper cast its
  recycled `double` argument with a bare `static_cast<int>`; out-of-range
  float-to-int conversion is undefined behaviour, and it is not benign. On
  x86-64 the conversion saturates to `INT_MIN`, so the value silently read as a
  *negative* count -- `fpZABNB(2^31, mu = 2, sigma = 1, nu = 1, tau = 0.1)`
  returned `0` for a CDF whose true value is ~1, and `fpZANBI(4e9, ...)` did the
  same, in both cases also breaking monotonicity of the CDF. On AArch64 the
  conversion saturates to `INT_MAX` instead, which sends the `0..q` accumulation
  loops on a walk of about two billion iterations -- a hang rather than a wrong
  number, and one no value-comparison test would catch. Both architectures are
  now exercised in CI (`arch-int-conversion`, on arm64 macOS, arm64 Linux and
  x86-64 Linux) by `inst/tinytest/test-arch-int-conversion.R`, which asserts
  both the value and prompt return. The conversion is now performed by a
  single shared `count_to_int()` helper in `recycling_helpers.h`, used by all 15
  conversion sites across `NBI`, `BNB`, `ZANBI`, `ZINBI`, `ZABNB`, `SICHEL`,
  `ZISICHEL`, `DPO`, `DEL` and `MN4`. The helper answers only "is this
  representable as an `int`?"; range validity remains with each wrapper, so
  `fdMN4()` still returns a density of 0 for an out-of-category value.

* **`fqDPO()` and `fqDEL()`**: the Poisson fast path in their quantile searches
  cast `R::qpois()` -- a `double` -- with the same unguarded conversion. For a
  large `mu` that value exceeds `INT_MAX`, so `fqDPO(0.999, mu = 3e9, sigma = 1)`
  returned `-2147483648`, a negative quantile, where the true value is
  `3000169260` (`gamlss.dist::qDPO()` returns `NA` here). Both now report `NA`.

* **All `fq*()` functions**: `log_p = TRUE` is usable again. `fqBNB()`,
  `fqZABNB()`, `fqZIBNB()`, `fqZANBI()` and `fqZINBI()` range-checked `p`
  against `[0, 1]` *before* applying the `log_p` back-transform, so every
  legitimate `log(p) < 0` was rejected with `"p must be >=0 and <=1"` and the
  argument could not be used at all. `fqNBI()`, `fqSICHEL()` and `fqZISICHEL()`
  dodged that by skipping the check entirely when `log_p = TRUE`, leaving the
  argument unvalidated. A shared `check_prob()` helper now ranges `p` on
  whichever scale the caller supplied: `[-Inf, 0]` under `log_p = TRUE` and
  `[0, 1]` otherwise. A log-scale call now agrees with the natural-scale call
  for all thirteen quantile functions. Note that `gamlss.dist` has the same
  defect in `qBNB()`, `qZABNB()`, `qZIBNB()`, `qZANBI()`, `qZINBI()`, `qNBI()`,
  `qDPO()`, `qDEL()` and `qSICHEL()`, so CKutils no longer matches the
  reference here -- it is correct where the reference is not.

* **`frNBI()`, `frZANBI()`, `frZINBI()`: removed a shadowed duplicate
  definition.** Each of these three names was defined *twice* in the package --
  once as an Rcpp-exported C++ wrapper (surfacing through `R/RcppExports.R`) and
  once as an R function in `R/rng_distr.R` that draws from `dqrng::dqrunif()`.
  `DESCRIPTION` has no `Collate` field, so `R/` is collated alphabetically and
  `R/rng_distr.R` was always sourced after `R/RcppExports.R`, silently
  overwriting the compiled version. The R implementation was therefore already
  the one users got; the C++ wrapper was dead code whose only reachable entry
  point was `.Call("_CKutils_frNBI", ...)`, and each of the three `.Rd` pages
  carried two identical `\usage` entries as a result.

  The compiled wrappers have been deleted. **There is no user-visible change**:
  all three functions remain exported, keep their arguments, defaults and
  behaviour, and their man pages now have one `\usage` entry each. What goes
  away is the dead object code, the dependence on filename collation order for
  correct behaviour, and the duplicated documentation. `frMN4()` was left alone
  -- it has no R counterpart, so its C++ wrapper is the real export.

  The `frNBI_scalar()`, `frZANBI_scalar()` and `frZINBI_scalar()` inline kernels
  remain in `inst/include/` for `LinkingTo: CKutils` consumers, but nothing in
  the package calls them any more, so they are no longer reachable from R and
  the `.Call`-based tests that used to cover them have been removed.

* **`frNBI_scalar()`, `frZANBI_scalar()`, `frZINBI_scalar()` now match the
  exported R samplers exactly** (breaking change to the header-only C++ API).
  These inline kernels drew from R's own stream -- `R::runif()`, `R::rpois()`,
  `R::rnbinom()`, with rejection sampling for the zero-adjusted cases -- while
  the exported `frNBI()`/`frZANBI()`/`frZINBI()` invert the CDF at
  `dqrng::dqrunif()` uniforms. A `LinkingTo: CKutils` consumer calling the
  kernels therefore got a *different sample* from an R user calling the
  documented function of the same name, even under a fixed seed, and the two
  could not be reconciled.

  Each kernel now takes the uniform as its first argument and inverts the
  corresponding `fq*_scalar()`, which is precisely what the R function does:

  ```cpp
  // was: frZANBI_scalar(mu, sigma, nu)          -- drew from R's stream
  // now: frZANBI_scalar(u, mu, sigma, nu)       == fqZANBI_scalar(u, ...)
  ```

  Feeding them `dqrng` uniforms now reproduces the R samplers draw for draw
  under one `dqset.seed()`; `inst/tinytest/test-rng_distr.R` asserts that
  identity over several parameter settings. Keeping the RNG outside the kernel
  also lets a caller hoist the generator out of a hot loop, and avoids imposing
  a `dqrng` dependency on consumers of `inst/include/distr_*.h`.

  It also removes a latent hang: `frZANBI_scalar()` sampled the zero-truncated
  NBI with `do { x = frNBI_scalar(...); } while (x == 0);`, which spins as
  `mu -> 0` because almost every draw from the untruncated NBI is a zero. The
  inverse-CDF form is constant-work.

  Nothing in CKutils called these kernels, and the only known downstream
  consumer (IMPACTncd_Engl) uses `fqZANBI_scalar()`/`fpZANBI_scalar()` and is
  unaffected. They are reachable from R for testing through the internal
  `.frNBI_scalar_vec()` / `.frZANBI_scalar_vec()` / `.frZINBI_scalar_vec()`
  bridges, which are deliberately not exported.

* **`fdZANBI()`: accurate density for a small `mu`.** The positive branch now
  divides by `log(-expm1(log f_NBI(0)))` instead of `log(1 - f_NBI(0))` -- the
  construction `fdZABNB()` already used, and identical in exact arithmetic. The
  literal `1 - f_NBI(0)` cancels catastrophically as `mu` shrinks and
  `f_NBI(0)` approaches 1, eventually rounding to exactly 0 and sending the
  density to `Inf`. Measured against the exact value at `x = 1`, `sigma = 1`,
  `nu = 0.1` (whose limit as `mu -> 0` is `1 - nu = 0.9`):

  | `mu` | before | after |
  |------|--------|-------|
  | `1e-10` | rel. err `8.3e-08` | rel. err `1.0e-10` |
  | `1e-12` | rel. err `2.2e-05` | rel. err `1.0e-12` |
  | `1e-14` | rel. err `8.0e-04` | rel. err `1.7e-14` |
  | `1e-16` | rel. err **`9.9e-02`** (returns `0.810648`) | rel. err `2.2e-15` |
  | `1e-17` and below | `Inf` | correct, down to at least `mu = 1e-300` |

  `gamlss.dist::dZANBI()` matches the "before" column exactly, including the
  `Inf`, so CKutils is now materially more accurate than the reference here.

  Note this is *unlike* `fdZABNB()`, which retains a hard floor near
  `mu = 1e-14`: `fdBNB_scalar()` returns `log f_BNB(0) == 0` exactly below that
  point, so the information is destroyed upstream and no reformulation in the
  hurdle layer can recover it. `fdNBI_scalar()` has no such floor, because it
  delegates to `R::dpois()`/`R::dnbinom_mu()`, which give `log f(0)` accurately
  (as `-mu`, or `-log1p(mu*sigma)/sigma`) at any representable `mu`.

* **`fdZANBI()` and `fdZINBI()`: `log1p(-nu)` in place of `log(1 - nu)`.** A
  consistency and robustness change rather than a measurable one: `1 - nu`
  rounds to exactly 1 for a `nu` below the double epsilon, so the literal form
  loses the factor entirely, but the resulting change in the density is at most
  about `5e-17` in relative terms -- at or below double-precision rounding of
  the result itself. It is made so that all three zero-adjusted densities
  (`fdZANBI()`, `fdZINBI()`, `fdZABNB()`) share one construction. `fdZINBI()`'s
  positive branch carries no `1 - f(0)` factor, so this is the only change it
  needed.

## Documentation

* **Header-only `*_scalar` kernels: caller contract documented.** The
  `count_to_int()` guard added above protects the R-facing wrappers only. The
  `inline` kernels in `inst/include/distr_*.h` remain unguarded *by design* --
  they exist to be called from a hot per-element loop, so a package that
  declares `LinkingTo: CKutils` and calls them directly owns the bounds check
  itself. That contract, `0 <= x, q <= CK_MAX_COUNT` (`INT_MAX - 1`), was
  previously implicit and is now stated in three places: authoritatively in
  `inst/include/recycling_helpers.h` alongside `count_to_int()`, per-header in
  every `inst/include/distr_*.h`, and for downstream authors in `README.md`.

  The documentation is explicit about what breaking it costs, because none of
  it fails gracefully:

  - `fpBNB_scalar()`, `fpDPO_scalar()`, `fpDEL_hlp_fn()`/`fpDEL_scalar()` and
    `fpZABNB_scalar()` **never return** at `q == INT_MAX` -- they accumulate
    with `for (int i = 0; i <= q; i++)`, so `i++` overflows and the loop has no
    exit. The call is not interruptible from R.
  - `fdBNB_scalar()`, `fdDPO_scalar()`, `fdDEL_scalar()` and `fdZABNB_scalar()`
    return a **silently wrong** density at `x == INT_MAX`, because
    `lgamma(x + 1)` wraps its argument to `INT_MIN`.
  - `ftofydel2_scalar()`, `fdSICHEL_scalar()`, `fpSICHEL_scalar()` and
    `fpZISICHEL_scalar()` size a `std::vector<double>` from the count itself,
    so they allocate **O(y)** memory and overflow that size at the boundary.

  `fdNBI_scalar()`/`fpNBI_scalar()` (and the ZANBI and ZINBI kernels built on
  them), `fdMN4_scalar()`/`fpMN4_scalar()`, and the `fq*_search()` quantile
  searches are safe for any `int` and are documented as such. Separately, the
  CDF kernels are O(q) in *time*, so a large-but-legal `q` is slow rather than
  wrong -- `q = 2^31 - 1024` takes roughly five minutes.

# CKutils 0.1.29

## Bug fixes

* **`installLocalPackageIfChanged()`**: the "is it already installed?" check now
  uses the non-loading `find.package()` test (`.pkg_is_installed()`) instead of
  `requireNamespace()`. `requireNamespace()` *loads* the target package's
  namespace into the current session merely to check availability; if a
  reinstall then followed, `R CMD INSTALL` overwrote that package's on-disk
  files while its old namespace was still live in memory, and the next
  access/unload in the same session raised
  `lazy-load database '...' is corrupt` / `internal error ... in R_decompress1`
  (R cannot hot-swap a loaded compiled package). The check no longer loads
  anything, so a caller can reinstall and then load a clean copy in the same
  session.

* **`detach_package()`**: now also unloads a namespace that is *loaded but not
  attached* (e.g. one pulled in by `requireNamespace()` or as a dependency).
  Such a namespace is absent from `search()` yet still locks the shared library
  and maps the lazy-load database, so it must be unloaded before a reinstall
  overwrites those files. Previously it was reported as "not attached" and left
  loaded.

# CKutils 0.1.28

## Bug fixes

* **`dependencies()`**: the install-vs-skip decision is now re-evaluated fresh
  for each package instead of from a single `installed.packages()` snapshot
  taken before the loop. Previously, when an earlier package in the list pulled
  in a later list entry as a transitive dependency, that entry was left
  installed but absent from the stale snapshot, so it was needlessly scheduled
  for reinstallation -- on Windows producing the benign-but-noisy warning
  `package 'x' is in use and will not be installed`. The freshness check uses a
  non-loading `find.package()` test, so it never attaches the package's
  namespace (which would lock its DLL and re-create the problem). Return value
  and the `update = TRUE` behaviour are unchanged.

# CKutils 0.1.27

## Bug fixes

* **`fquantile()`**: fixed undefined behaviour when the input contained
  `NA`/`NaN`. `std::sort()` was applied to data containing `NaN` (not a valid
  strict-weak ordering), giving platform-dependent results that surfaced as a
  test failure on macOS. Missing values are now detected with `ISNAN()` (matching
  R's `is.na(NaN) == TRUE`): with `na_rm = FALSE` any `NA`/`NaN` propagates to
  `NA`, and the sort always runs on a fresh copy, which also fixes `fquantile()`
  mutating its caller's vector in place.

* **Distribution functions** (`fd*`/`fp*`/`fq*` for NBI, DPO, DEL, SICHEL, BNB,
  MN4, ZANBI, ZINBI, ZISICHEL, ZABNB, ZIBNB, BCPEo, BCT): `NA`/`NaN` quantiles or
  probabilities no longer trigger out-of-range float-to-int undefined behaviour
  or bypass range validation; they now propagate to `NA`, as in base R.

* **`shift_bypidNum()`/`shift_bypidInt()`/`shift_bypidBool()`/`shift_bypidStr()`**:
  a `NA_integer_` `lag` (stored as `INT_MIN`) caused signed-integer overflow and
  an out-of-bounds access (a segfault); it is now rejected with an informative
  error.

* **Distribution parameter recycling**: a zero-length argument alongside a
  non-empty one caused an integer modulo-by-zero (a crash); it now returns a
  zero-length result, following R's recycling rules.

* **`cklut`**: out-of-range row indices in the gather path and out-of-range or
  `NA` key indices in the build path are now bounds-checked, preventing
  out-of-bounds memory access; `i64` value columns now treat a plain `NaN` (not
  only `NA`) as missing.

# CKutils 0.1.25

## New features

* **`cklut`: a memory-mapped, on-disk lookup table and drop-in for `lookup_dt`.**
  Build a dense lookup table once and query it by reading value columns straight
  from a memory-mapped binary file, so the table can be larger than RAM and
  there is no per-call rebuild. Value columns may be of any type (double,
  integer, logical, factor/character), exactly like `lookup_dt`, and unmatched
  keys return `NA`.

  * `cklut_build()` — build from a `data.table`, CSV, or Parquet source.
  * `cklut_lookup()` — drop-in for `lookup_dt()`; returns a list of typed
    vectors, as a `data.table` (`as.data.table = TRUE`, the default) or merged
    into `tbl` (`merge = TRUE`).
  * `cklut_open()` — open a previously built table (optionally `warm`).
  * `cklut_to_dt()`, `cklut_to_csv()` (via `data.table::fwrite`), and
    `cklut_to_parquet()` — read a table back / export it.

  Correctness is validated against **both** `lookup_dt` and `absorb_dt` on
  mixed-type values and no-match (`NA`) rows. On a warm, in-RAM 1.26M-row table,
  `cklut_lookup()` is roughly 1.4–3x faster than `lookup_dt()` depending on query
  volume; the larger structural wins are larger-than-RAM tables and avoiding the
  per-call lookup-table setup.

  The C++ engine, benchmarks, and validation harness live in `cklut/`.

## Bug fixes

* `guess_gamlss()` referenced an undefined `dt` (which resolved to `stats::dt`)
  instead of its local `data.table`, so the function always errored before
  returning. It now completes and returns the predicted variable as documented.

* `gnrt_folder_structure()` built the list of folder paths but never created any
  directories (it returned `NULL` without side effects). It now actually creates
  the documented folder structure under `path`.

## Documentation

* Reviewed and completed the roxygen documentation of all exported functions:
  every exported function now documents its return value (`@return`) and carries
  a runnable `@examples` section (heavier model-fitting helpers use `\dontrun`).
  `R CMD check` reports no documentation problems.

## Testing and coverage

* Greatly expanded the `tinytest` suite to systematically cover the package's
  functions and C++ helpers: the `frXXX` random generators, parquet I/O
  round-trips, the gamlss/polr/reldist helpers, `cklut` (build/lookup/export
  plus the multi-shard and `warm` prefetch paths), and the compiled helpers
  (`shift_bypid*`, `counts`/`tableRcpp`, the distribution validation/`log_p`
  branches, parameter recycling, and clamping). Code that cannot be exercised
  by unit tests — package load/unload hooks, real package-install machinery,
  Windows-only and network-only paths, `requireNamespace` guards for Suggests
  packages, and defensive guards — is marked with `# nocov`. Overall test
  coverage rises to roughly 95%.

## Notes

* CSV export preserves factor *labels* but not factor level order; rebuilding
  from CSV yields factors with sorted levels. Use Parquet (or pass an explicit
  level order) when exact factor-level fidelity matters.
