# CKutils 0.1.34

## Bug fixes

* **`is_valid_lookup_tbl()` now checks every key column, not just the first.**
  Its `return(TRUE)` sat inside the loop over the key columns, so the two
  per-column checks -- integer (or factor) storage and consecutive integer
  values -- ran only on the first key in its sorted order, in which `year`
  always comes first. The row-count check after them cannot catch a gap: it
  counts the distinct values present, so a full grid over `age = c(30L, 35L)`
  has exactly the expected number of rows. Such a table, keyed on `year` and
  `age`, was accepted, and `lookup_dt()` -- which finds rows by arithmetic on
  `max - min + 1` values per integer key -- then returned the age-35 row for
  age 31, a value not in the table, and failed with "Calculated row indices
  are out of bounds" for age 35 itself. Whether a gap was caught depended on
  the column names: the same gap in a key that sorts first was. A non-integer
  key after the first was rejected, but by the row-count check, with the
  misleading "should have 0 rows". Each now gets its own check, in any
  position. This is the validation `lookup_dt()` runs when
  `check_lookup_tbl_validity = TRUE`, its default.

  Tables that 0.1.33 accepted can now be rejected: those with a gap in an
  integer key other than the first. `lookup_dt()` could not look such a table
  up correctly.
* The advice to key the table, and the keying under `fixkey = TRUE`, now come
  only after every check has passed, so `fixkey = TRUE` no longer keys a table
  it then rejects.
* **`is_valid_lookup_tbl()` rejects NA in a key column.** A factor label that
  is not one of the levels becomes NA. With one such row in each block the row
  count still matched, the table was accepted, and `lookup_dt()` -- which
  reads factor codes by position -- returned shifted values without a
  warning, often for the cells the table held as well: in a year x age x sex
  x qimd table in which the last qimd level, "5 least deprived", was written
  "5", all 60 lookups were wrong. The message names the column and, for a factor, the likely cause. NA
  in an integer key, which `lookup_dt()` already refused, is now reported by
  the validator too.
* It also rejects `keycols` that are duplicated, NA or not columns of the
  table, which gave a wrong row count, were silently dropped, or failed inside
  data.table; and an empty table, which it accepted when the keys were integer
  (`lookup_dt()` refused it itself).
* The gap error names the missing values, e.g. `(missing: 31, 32, 33, 34)`: the
  first ten, and how many in all, in the key's own class, so that an `IDate`
  key shows dates. A key whose values span more than the integer range no
  longer fails with "missing value where TRUE/FALSE needed". Likewise, a factor
  key in which a level never occurs is reported by name, rather than as a
  wrong row count.
* **Key order and duplicates are read from the rows, not from a key or index
  that `lookup_tbl` already carries.** Base `[[<-` and dplyr verbs keep
  data.table's `sorted` and `index` attributes after changing values or the
  row order. `setkeyv()` trusts them and skips the sort, and `duplicated()`
  trusts a key that the key columns prefix, comparing adjacent rows only. So a
  stale key or index made `lookup_dt()` read the wrong rows -- silently, or
  failing with "Cardinality must be positive" -- and a stale key could hide a
  duplicate from `is_valid_lookup_tbl()`. Now one pass over the rows, in C++
  (`key_order_cpp()`), tells whether they are in key order and whether two
  neighbours are equal; rows out of key order are checked for duplicates on a
  new table of the key columns, without key or index. Under validation,
  `lookup_dt()` then marks the key if the rows follow it, or else drops key and
  indices, so that `setkeyv()` really sorts. `fixkey = TRUE` likewise sorts by
  the rows, and repairs a stale key that already names the key columns; the
  key advice says when the key is stale.
* **`generate_corr_unifs()` applied its correction for uniform margins twice.**
  Its loop visited each pair of variables as (i, j) and again as (j, i), and
  adjusted both entries each time, so every off-diagonal correlation went
  through `2 * sin(pi * r / 6)` twice. The uniforms thus had correlation
  `2 * sin(pi * r / 6)` instead of `r`: too far from zero by up to 0.018
  (largest near |r| = 0.58; 0.518 for a target of 0.5). A near-singular target
  could also become indefinite, and `chol()` failed with "the leading minor of
  order 3 is not positive". Each entry is now adjusted once. IMPACTncd_Engl
  uses it for the exposure rank correlations of its synthetic population: in
  its 19-exposure matrix, 5 of the 171 pairs were off by more than 0.01, the
  largest by 0.017, so the model's outputs will shift slightly.
* **`detach_package()` no longer loops forever when `detach()` refuses.**
  `detach()` errors for a package that another attached package depends on,
  and for `base`. The loop swallowed the error and tried again, forever,
  printing "Detached package: <pkg>" each time. It now stops, says why, and
  returns `FALSE`.
* `detach_package()` unloads a package's shared library only once its
  namespace is gone. When another loaded namespace imports the package (MASS,
  imported by gamlss), the namespace stays loaded; its shared library was
  unloaded all the same, and every later call into its compiled code failed
  with "NULL value passed as symbol address" until R restarted. It now keeps
  the library, and for a namespace that was loaded but not attached returns
  `FALSE` with the reason ("imported by gamlss.dist, gamlss") rather than "was
  not attached". The library is looked for where the namespace was loaded
  from (`find.package()`), not only in `.libPaths()`.
* **`cklut_build()` no longer kills the R session on a key with no values.**
  When the last key dimension (in cklut order) came out with no values -- e.g.
  an empty table whose last key is a factor with no levels or a character
  column, or, with `check = FALSE`, an all-NA factor or character key, or an
  integer64 key -- the C++ computed shard sizes by an integer division by
  zero, and the process died with SIGFPE. `cklut_build()` now refuses, whatever
  `check` says, an empty table, NA or non-finite key values, integer64 keys
  (whose bit patterns it read as tiny doubles) and numeric keys outside the
  integer range; and the C++ throws an error for an empty innermost dimension,
  as the standalone `cklut/` writer now does for a table without value columns
  (another division by zero; not reachable from R).
* With `check = TRUE`, `cklut_build()` also refuses a numeric key that is not
  a whole number. `c(1, 1.5, 3)` passed the row-count and uniqueness checks,
  and truncation then mapped 1 and 1.5 to the same cell, so a lookup returned
  the wrong value for one of the cells.
* **`lookup_dt()` checks the row count of `lookup_tbl` even with
  `check_lookup_tbl_validity = FALSE`.** Its row arithmetic needs one row per
  combination of key values, and it computes the number of combinations
  anyway, so the check costs nothing. With validation off (IMPACTncd's
  `logs: no`), a table with a gap in an integer key, a missing row or an
  unused factor level was read wrong without a warning: the year x age
  {30, 35} table above returned the age-35 row for age 31. Such tables now
  stop with "lookup_tbl is not a full grid of its key values". It does not
  catch everything the full validation does: NA in a factor key, for one.
  The range check before it now works in double, so an integer key spanning
  more than the integer range gets "range too large" instead of "missing value
  where TRUE/FALSE needed", and a key whose last value lies below its first is
  reported as not a full grid.
* **`fdDPO()`, `fpDPO()` and `fqDPO()` no longer go wrong at a large `mu`**
  (with `sigma != 1`). The normalising constant is the inverse of a sum over
  y = 0, 1, ... whose mass lies around `mu`, and the sum stopped at
  `max(3 * x, 500)`. For a small x and `mu` beyond ~300 it missed the mass:
  densities in the left tail came out many orders of magnitude too large (some
  5e7 times at `mu = 1000`, `sigma = 10`, x = 0), and from `mu` ~5000 the terms
  in the window underflowed, so the constant was `Inf`, and with it every
  `fpDPO()`, while `fqDPO()` returned 0. A large `sigma` lost mass in its heavy
  tail (2.4% at `mu = 2`, `sigma = 1000`). The sum now runs from 40 standard
  deviations below `mu` until its terms are negligible, and matches a
  log-sum-exp normalisation to ~1e-12. It depends on `mu` and `sigma` only, so
  it is cached once for all x: the old x-dependent window made `fpDPO()` at a
  large `mu` recompute it for almost every term. gamlss.dist's `dDPO()` sums the
  same truncated window, so two tests now use an implementation-independent
  reference. `fget_C()` computes the constant the same way.
* **The discrete quantile functions no longer return their iteration cap as
  the quantile.** `fqBNB()`, `fqSICHEL()`, `fqDPO()` and `fqDEL()` -- and
  `fqZIBNB()`, `fqZABNB()` and `fqZISICHEL()` through them -- add one
  probability at a time until the CDF reaches p, and stopped after 1e6 terms,
  returning 1e6 whatever the CDF: at `mu = 2e6` `fqBNB()` returned 1,000,000
  where the median is 1,039,684 (the CDF at 1e6 being 0.488), and `fqSICHEL()`
  1,000,000 for 1,351,683. There is no fixed cap now. A search gives up, with
  NA and a warning, only when the CDF cannot reach p: a term that is not finite,
  or terms past the largest too small to change a sum that is still more than
  64 `.Machine$double.eps` (relative) short of p (within that, the index where
  the sum stopped is returned). A BNB quantile beyond the int range is reported
  as NA when the closed-form mode shows it: at once for a large `mu`, otherwise
  after at most 65,536 terms (about 0.3 ms); where the bound does not show it,
  after scanning the range (about 8 s). BNB and DPO searches at a large `mu`
  start at the first term that does not underflow. The time grows with the
  quantile: about 4 ms per million terms for BNB (`mu = 2e7`: 0.04 s; a
  quantile of 1.56e9 at `mu = 3e9`: about 6 s), about 12.6 ms per million terms
  for SICHEL (`mu = 2e7`: 0.17 s), whose terms come from a recursion. A SICHEL
  quantile beyond the int range is reported at once too, from a bound on the
  CDF (SICHEL is a Poisson mixture over a log-concave law):
  `fqSICHEL(0.5, mu = 1e300, sigma = 1, nu = -0.5)` and
  `fqZISICHEL()` there are NA in about a millisecond, not after some 40 s that
  could not be interrupted. Only within about 1% (in `mu`) of the limit, or
  further out for `p` within 1e-6 of 1 with a heavy tail, is such a quantile
  known to be NA only after scanning the range.
* `fqSICHEL()` and `fqZISICHEL()` return NA, with a warning, for p = 1: the
  quantile is `Inf`, which an integer cannot hold. They stored `Inf` into the
  integer result, an out-of-range conversion whose result is undefined: NA on
  x86-64, 2147483647 on AArch64.
* **`fdSICHEL()`, `fpSICHEL()` and `fqSICHEL()` work at a large `mu` and a
  small `sigma`.** They used the unscaled Bessel K, which underflows to 0 for
  an argument beyond ~700 (alpha is ~2000 at `mu = 2e6`, 1/sigma is 1000 at
  `sigma = 0.001`); the logs of such values gave NaN. They now use the
  exponentially scaled K; results elsewhere are unchanged.
* `set_lookup_tbl_key()` sets the key from the rows, as
  `is_valid_lookup_tbl(fixkey = TRUE)` now does: `setkeyv()` trusted a key or an
  index left stale by tools outside data.table, and kept or applied a wrong row
  order under the key. With integer or factor keys one pass over the rows
  marks a key they already follow; otherwise they are sorted from scratch. It
  also names absent key columns.
* **`fqNBI()`, `fqZINBI()` and `fqZANBI()` return `NA` with a warning for a
  quantile an integer cannot hold, instead of converting it with undefined
  behaviour.** `fqNBI_scalar()` passed the double from `R::qnbinom_mu()` /
  `R::qpois()` through an implicit `int` conversion. That is undefined for
  `p = 1` (also `log_p = TRUE` with `p = 0`, and `lower_tail = FALSE` with
  `p = 0`), for `mu = Inf`, and for a quantile above `INT_MAX`
  (`fqNBI(0.5, mu = 1e10, sigma = 1)`, whose true value is 6931471805): it gave
  `NA` only by accident on x86-64, silently, and probably `INT_MAX` on AArch64.
  The three functions now warn once per call ("NAs produced: a quantile is
  infinite (p = 1) or beyond the integer range"). The largest quantile returned
  is `.Machine$integer.max - 1`, the package's `CK_MAX_COUNT`, so a quantile of
  exactly `INT_MAX` is `NA` too. `fqZINBI()` and `fqZANBI()` still return a
  finite value at `p = 1`, and in-range quantiles are unchanged.
* **`fqBCT()` accepts `p = 0` and `p = 1`.** It stopped with "p must be between
  0 and 1" for either, which aborted a whole vector for one boundary
  probability, while `gamlss.dist::qBCT()` and `fqBCPEo()` return the ends of
  the support. It now returns 0 and `Inf`. A `p` outside [0, 1] is still an
  error.
* **`fdDPO()`, `fpDPO()` and `fqDPO()` return at once for an infinite `mu` or
  `sigma`.** There is no normalising constant then, and the loop that sums it
  ran all of its 2^31 iterations to find that out: `fdDPO(1, Inf, 2)` took 33 s
  and `fqDPO(0.5, Inf, 2)` 100 s to return the same `NaN` (`NA` for `fqDPO()`),
  with the usual warning. A sum that reaches the int cap without meeting a
  stopping rule (a finite `sigma` near 1e10) is `NaN` too, rather than the
  constant of the truncated sum. The out-of-range float-to-int conversions on
  this path, which are undefined behaviour, are gone: the cache slot of a
  non-finite or huge `(mu, sigma)`, the start of the sum for `mu / sigma`
  beyond the int range, the window of a NaN `mu` or `sigma`, and `max(x) * 3`
  in `fget_C()`. Results for finite arguments are unchanged bit for bit.
* **`frMN4()` returns `integer(0)` for a zero-length `mu`, `sigma` or `nu`,
  instead of killing the R session.** It took an integer modulo by the recycled
  parameter length, which is 0 then, and the floating point exception (SIGFPE)
  ended the process. The other twelve `fr*` functions already return a
  zero-length result. `n <= 0` still stops with "n must be a positive integer",
  and by this fix alone the draws for non-empty parameters are unchanged (the
  `frMN4()` bullet on dqrng, below, changes its stream).
* **`fqDPO()` returns the finite quantile for `p` in [1 - 1e-9, 1), not `Inf`.**
  It returned `Inf` for every `p` with `p + 1e-9 >= 1`, a cutoff copied from
  `gamlss.dist::qDPO()`, where it guards an R loop. The C++ search needs no such
  guard, and the quantile there is finite:
  `fqDPO(c(1 - 1e-9, 1 - 5e-10, 1 - 4e-11), 16.27, 7.53)` is `118 121 130`, not
  `Inf Inf Inf`. Stored as an integer, as in the IMPACTncd models
  (`smok_quit_yrs` and `smok_dur_ex`, about 4e-10 of the draws), the `Inf`
  became `NA`. `p = 1` is still `Inf`, and every `p` below `1 - 1e-9` gives the
  result it gave before. The search still stops at the first `q` whose CDF is at
  least `p`, so `fqDPO(fpDPO(x))` is `x`. When the summed CDF stops growing
  within 64 * `.Machine$double.eps` (relative) of `p`, the result is the index
  where it stopped; further below `p` it is `NA`, with a warning. For `p` within
  ~1e-13 of 1 the quantile is accurate only to a few units.
* **BNB densities, CDFs and quantiles were inaccurate at large counts.** The log
  of a BNB term was `lbeta(i + n, m + k) - lbeta(n, m) - lgamma(i + 1) -
  lgamma(k) + lgamma(i + k)`, whose last three parts are large and nearly
  cancel (`lgamma(i + 1)` is 2e10 at `i = 1e9`), and the CDF and the quantile
  search added the terms in plain double precision. `fqBNB(0.5, 2e8, 0.7, 1.3)`
  returned 83762994 where the median is 83762993, `fqBNB(1 - 1e-8, 90, 10, 1)`
  162955745 for 169562369 (-4%), `fpBNB(1e7, 2e6, 0.7, 1.3)` was off by 2.7e-10
  and `fdBNB(1.5e9, 3e9, 0.7, 1.3, log = TRUE)` by 1.2e-5. The log of a term is
  now `lbeta(i + n, m + k) - lbeta(n, m) - log(i + k) - lbeta(i + 1, k)`,
  successive terms follow each other by their ratio (recomputed from the log
  every 1024 terms), and the CDF is a compensated (Neumaier) sum. A term costs
  about 4 ns instead of 100: `fpBNB()` is about 35 times faster, and stops as
  soon as its terms underflow (`fpBNB(5e7, 1, 1e-3, 1)` took 3 s and takes
  0.04 ms); `fqZABNB()` on the smoking models' parameters is 1.9 times faster
  with identical quantiles (240,000 draws compared). Values move in the last
  bits (~1e-14). Left as they were: quantiles with `1 - p < ~1e-5` in a heavy
  tail, which can be off by a few units (the CDF steps fall below the spacing
  of `p`), and `sigma <~ 1e-4`, where `lbeta` cancels.
* In the header-only BNB scalars, `fdBNB_scalar(INT_MAX)` no longer wraps (it
  returned `-Inf`) and `fpBNB_scalar(INT_MAX)` returns (about 5 s) instead of
  never.
* **`fpZANBI()` and `fpZINBI()`: the upper tail is no longer `1 - F`, and
  `fpZANBI()` is no longer `NaN` at a tiny `mu`.** One minus the CDF is 0, or
  wrong by orders of magnitude, once the CDF rounds to 1:
  `fpZANBI(200, 5, .5, .3, lower_tail = FALSE)` returned 0 for a true 1.9e-28.
  The upper tail is now `(1 - nu) * S(q) / (1 - F(0))` (ZANBI) and
  `(1 - nu) * S(q)` (ZINBI), with `S` the NBI upper tail from R's
  `pnbinom()`/`ppois()` (in log space for `log_p = TRUE`); it agrees with a
  `pnbinom(lower.tail = FALSE)` reference to 1e-15. `fpZANBI(1, 1e-17, 1, 0.5)`
  was `NaN`, because F(0) rounds to 1 and the lower tail divided 0 by 0;
  `1 - F(0)` is now `-expm1(log f(0))`, and where it is below 1e-3 the lower
  tail is `1 - P(X > q)`. Elsewhere the lower tail is unchanged bit for bit,
  which covers the IMPACTncd models (`mu >= 0.46`, through
  `fpZANBI_scalar()`).
* **`frMN4()` draws from dqrng, like the other twelve `fr*` functions; its
  random stream changes.** It was a C++ function that called R's `runif()`, so
  it followed `set.seed()` and ignored `dqrng::dqset.seed()`: two calls under one
  `dqset.seed()` gave different draws. It is now the R function
  `fqMN4(dqrng::dqrunif(n), mu, sigma, nu)`, the construction of the others, so
  its draws are reproducible under `dqset.seed()` and leave base R's stream
  alone; a given seed gives different draws than before, from the same
  distribution. As for the other `fr*`, unequal parameter lengths follow R's
  recycling rules and the result is as long as the longest of `n` and the
  parameters; a non-numeric or vector `n` stops with "n must be a positive
  integer". The compiled `frMN4()` and its declaration in
  `inst/include/distr_MN4.h` are gone (a `LinkingTo` consumer takes
  `fqMN4_scalar()` at a uniform of its own). The models call `fqMN4()`, not
  `frMN4()`.
* **The SICHEL and ZISICHEL functions are accurate at a very small `sigma`.**
  The recursion starts from `c = K_{nu+1}(1/sigma) / K_nu(1/sigma)`, from
  `log(K_{nu+1}(alpha) / K_nu(alpha))` and from the log pmf at 0, and each was a
  difference of two `log K` values (`log(scaled K) - x`), which kept the
  rounding of `x = 1/sigma`: at `sigma = 1e-12`, where SICHEL is Poisson to
  1.1e-11, `fpSICHEL(60:120, 90, 1e-12, -0.5)` was 4.0e-9 from the true CDF and
  `fdSICHEL()` 6e-5 (relative). Each is now a ratio of scaled K's, and
  `alpha - 1/sigma` is written without the subtraction; where a ratio overflows
  or underflows, the old form is used, so no value that was finite becomes
  `NaN`. At the models' parameters the quantiles are unchanged and the CDF
  moves by < 2e-13 (relative); far in the tail of a very negative `nu` (near
  -14.7, counts 7 to 14) a density moves by up to 5e-8, towards the true value.
* **`fqBNB()` and `fqSICHEL()` return the finite quantile for `p` in
  [1 - 1e-9, 1), as `fqDPO()` does.** They returned `Inf` (`fqBNB()`) and `NA`
  with a warning (`fqSICHEL()`) for every `p` with `p + 1e-9 >= 1`, the cutoff of
  `gamlss.dist::qBNB()` and `qSICHEL()`: `fqBNB(c(1 - 1e-9, 1 - 5e-10, 1 - 2e-10),
  10.6139, 0.08621, 0.0174699)` is `342 365 398`, and `fqSICHEL(c(1 - 1e-9,
  1 - 5e-10, 1 - 1e-10), 2.34544, 0.212277, -6.19696)` is `33 34 37`. `p = 1` is
  still `Inf` (`NA` with a warning for the integer `fqSICHEL()`), and every `p`
  below the window gives the result it gave before. The stall rule is
  `fqDPO()`'s, so for `p` within ~1e-13 of 1 the quantile is accurate only to a
  few units (more in a heavy tail), and `NA` where the summed CDF cannot reach
  `p`. `fqZABNB()` follows `fqBNB()` (the models' `smok_cig_ex`: about 1e-13 of
  the draws, which were `Inf`).
* **`fqDPO()` no longer answers `NA` where `p(0)` is at the underflow limit.**
  For a `sigma` large against `mu` the double Poisson pmf has a second, shallow
  mode at 0. Where `mu / sigma` is about 743, `p(0)` is the smallest positive
  double and the densities after it are exactly 0 for a while, ahead of the
  main mass near `mu`; the quantile search took that zero for a sum that had
  stopped growing (an exact 0 changes no sum) and gave up:
  `fqDPO(c(0.1, 0.5, 0.9), 17934.73, 24.14)` was `NA NA NA` for `17094 17931
  18781`, and so were all quantiles of about a quarter of the pairs in that
  band. A sum still below `DBL_MIN` no longer counts as settled
  (`ck_search_stalled()`, shared by all the searches; the BNB, SICHEL and DEL
  results are unchanged).
* **`fpDEL()`, `fqDEL()` and `fdDEL()` are accurate at large counts.** The DEL
  recurrence formed `log f(j) = logpy0 - lgamma(j + 1) + S_j` in double
  precision, from numbers of size `j log j` that nearly cancel, so its error grew
  with `j`: against the exact Poisson + negative binomial convolution the CDF
  was off by 1e-10 at q = 8e3, -7.5e-8 at 9.4e5, -4.1e-6 at 9.4e6 and 8.8e-3 at
  9.4e7; quantiles were wrong by 1 to 663,857 units, two of them a false `NA`,
  and `fqDEL(0.5, 1e9, 0.3, 0.4)` returned 412402586 where the CDF is 1.4e-5.
  From index 4096 on, the density, the CDF and the quantile search use a
  re-formulated recurrence: it carries the deviation of the term ratio from its
  Poisson value (before the Poisson peak) or from 1 (after it), anchors the log
  density with one `dpois()` at the peak, and sums it with compensation.
  Measured `|F - F_ref| < 5e-12` for counts up to 2.0e9 (`mu` 1e5 to 2e9) and
  `< 2e-11` on a fuzz of 297 parameter sets; it is also faster per count (13 ns
  against 23). Values for fewer than 4096 steps, including every count the
  IMPACTncd models use (<= 10), are unchanged bit for bit.
* **`fqDEL()` returns the finite quantile for `p` in [1 - 1e-9, 1), as `fqDPO()`,
  `fqBNB()` and `fqSICHEL()` do.** It returned `Inf` for every `p` with
  `p + 1e-9 >= 1`, the cutoff of `gamlss.dist::qDEL()`: `fqDEL(c(1 - 1e-9,
  1 - 5e-10, 1 - 1e-10), 2.03065, 2.30919, 0.830551)` is `25 26 28`. Only
  `p >= 1` is `Inf`, and the search ends with the shared stall rule (a sum that
  has stopped growing within 64 eps of `p` returns the index where it stopped).
  The models' DEL draws stay below the window and are unchanged.
* **Summed CDFs never exceed 1, so upper tails are never negative.**
  `fpDPO()`, `fpDEL()`, `fpBNB()`, `fpSICHEL()`, `fpZANBI()` and `fpBCT()` (and
  the ZI/ZA variants through them) could return a CDF a few ulp above 1, from
  rounding in their sums, which made `lower_tail = FALSE` negative and NaN on
  the log scale (`fpBNB(1000, 90, 0.02, 0.02, lower_tail = FALSE)` was
  -5.9e-14). The value returned is now at most 1 (a running sum that continues
  to the next element is not clamped). In the models' ranges this moves stored
  CDF values such as `fpDPO(90, ...)` by at most 1.24e-14 and leaves every
  quantile draw unchanged. `fpDPO()` also returns `NaN` at the first
  non-finite term, so an infinite `mu` or `sigma` no longer sums `NaN` terms up
  to `q`.
* **Zero-inflated and zero-altered quantiles no longer take a fixed 1e-10 or
  1e-7 off `p`.** After removing the zero mass, `fqZINBI()`, `fqZANBI()` and
  `fqZABNB()` subtracted 1e-10, and `fqZIBNB()` and `fqZISICHEL()` 1e-7, offsets
  copied from gamlss.dist that are about a million times the rounding of `p`.
  Every `p` that close above a CDF value came out one quantile too low
  (`fqZIBNB(0.564835214835, 5, 0.5, 1, 0.1)` was 2 for 3), `q(p(x)) == x`
  failed wherever the CDF step was smaller, and the upper end was capped
  (`fqZIBNB(1 - 1e-8, 148.473, 0.108158, 1.866, 0.501399)` was 7953 for 9936).
  Only the rounding slack, 2 `DBL_EPSILON`, is now taken off. A zero-altered
  quantile above the mass at 0 is at least 1. At `p = 1`, `fqZIBNB()` and
  `fqZABNB()` return `Inf` and `fqZISICHEL()` `NA` with a warning, as their
  base distributions do; `fqZINBI()` and `fqZANBI()` stay finite, which
  callers of `fqZANBI_scalar(1 - u, ...)` rely on. In the models, the draws
  that move go up one step to the correct quantile: about 1.2e-6 of fruit
  (ZISICHEL) draws, 1.3e-7 of alcohol (ZINBI), 2e-8 of the ZANBI durations
  and 8e-9 of `smok_cig_ex` (ZABNB).
* **Long quantile searches can be stopped with Ctrl-C.** Nothing in CKutils
  checked for a user interrupt, so a scan such as `fqSICHEL(0.5, 3.19e9, 1,
  -0.5)` (about 27 s) or `fqDEL(0.5, 1e9, 0.3, 0.4)` ran to the end.
  `fqDEL()`, `fqSICHEL()` and `fqDPO()` now check every 2^20 terms, and the
  vector `fpDEL()`, `fpSICHEL()` and `fpDPO()` every 1024 elements; they stop
  within a second. The scalar kernels in `inst/include` (`fqBNB_search()`,
  `CkDELCdf`, `fcdfSICHEL_scalar()`, the DPO normalising constant) do not
  check, because a LinkingTo caller may run them where R cannot be called: a
  single element there, or the DPO constant at a new `mu` and `sigma` (about
  5 s at `mu = 2e9`, `sigma = 1e4`), runs to its end first. Results are
  unchanged.
* **Draws of the IMPACTncd models.** Only some fixes of this release move
  model draws, each for a small share. The zero-inflated / zero-altered
  offsets: about 1.2e-6 of the fruit (ZISICHEL) draws, 1.3e-7 of alcohol
  (ZINBI), 2e-8 of the ZANBI durations and 8e-9 of `smok_cig_ex` (ZABNB) go up
  one step, each to the correct quantile. `p` within 1e-9 of 1: about 4e-10 of
  the `smok_quit_yrs` and `smok_dur_ex` (DPO) draws and about 1e-13 of
  `smok_cig_ex` (ZABNB) were `Inf` (`NA_integer_` once the models stored them
  as integers) and are now finite. `frMN4()` draws from another stream (the
  models call `fqMN4()`, which is unchanged). The CDF clamp moves CDF values
  that summed above 1 by at most 1.24e-14 (synthetic parameters over the
  models' ranges) and no quantile draw; the SICHEL start values move the CDF by
  less than 2e-13 and the BNB terms by about 1e-14, and no quantile. All other
  fixes leave the models' draws unchanged: the DPO, BNB, SICHEL and DEL
  rewrites, the shared stall rule and the new upper tails, which the commits
  checked bit for bit or draw for draw on the models' parameters.

## Documentation

* `lookup_dt()`: `exclude_col` is documented as it behaves. A column named
  there that both tables have is not ignored, but looked up as a value column,
  so with `merge = TRUE` it overwrites that column of `tbl` (as IMPACTncd
  relies on, to refresh distribution parameters). Example 3 said such a column
  "should be ignored", and showed the caller's values; it now shows the
  overwritten ones.
* The help pages of the count distributions describe what the code does:
  the BNB, SICHEL, DEL and DPO searches are scans with no cap (cost per
  million terms, when a quantile beyond the int range is `NA` at once and when
  only after a scan), not "divide-and-conquer", binary search, SIMD or caching;
  the DEL pmf is the Poisson + negative binomial convolution, its variance
  `mu + mu^2 sigma (1 - nu)^2` (the page had `(1 - nu)`) and its measured
  accuracy; the zero-inflated Sichel is not truncated at zero; the DPO limit at
  a huge `mu` and `sigma`; `p = 1` for each quantile function; non-integer
  counts truncate; `fqMN4()` at a `p` equal to a CDF value; `lower_tail = FALSE`
  for BCT, BCPEo and MN4 is 1 - F. The `fr*()` functions say that `n` is a
  single number (a vector errors).
* The hazard contract for the `inst/include` kernels (`recycling_helpers.h`,
  the README and the `distr_*.h` headers) and the comments of the arch
  workflows match the code: only the DPO kernels still fail at `INT_MAX`
  itself (an `fpDPO_scalar()` sum that cannot settle does not return; the
  `fdDPO_scalar()` log density is `-Inf`); the BNB, DEL and SICHEL kernels
  return there in O(1) memory but O(q) time (`fpBNB_scalar()` about 5 s,
  `fpDEL_hlp_fn()` about a minute); the DPO limit at a huge `mu` and `sigma`;
  the samplers are safe only for finite parameters and `u < 1`; which loops
  can be interrupted. Comments about behaviour changed in this release say
  "before 0.1.34", not "up to 0.1.34".

## Performance

* Validation now takes less time than in 0.1.33, while checking every key.
  With `sort()` and `diff()` on every integer key, as first fixed,
  `is_valid_lookup_tbl()` took 1.58 times as long as 0.1.33. Now it tests a key
  for gaps without sorting -- an integer key is consecutive when its number of
  distinct values, which the row-count check needs anyway, equals
  `max - min + 1` -- and counts a factor's codes with `tabulate()`, as `anyNA()`
  on a factor allocates a logical vector as long as the column; and the one
  C++ pass over the rows (`key_order_cpp()`) replaces `duplicated()` for the
  duplicate check whenever the rows are in key order, as they are once
  `lookup_dt()` has keyed the table. On a 3.9M-row table (chd_incd, years
  13-43, ages 30-99) the validator takes 0.063 s (0.1.33: 0.092 s; first fix:
  0.145 s), on a 12.3M-row one (the bmi exposure table) 0.203 s (0.285 s;
  0.450 s); a validated lookup of 1e6 rows takes 0.122 s (0.153 s; 0.200 s)
  and 0.293 s (0.385 s; 0.546 s). The C++ pass itself takes 0.013 s and
  0.043 s. All figures single-threaded, the median of 3 fresh R processes, on
  the key columns of IMPACTncd_Engl's tables, already keyed.
* **`fpDEL()` and `fqDEL()` are O(q), not O(q^2).** `ftofydel2_scalar()`
  rebuilt gamlss.dist's `tofydel2` recurrence, in a `std::vector` of y + 2
  doubles, for every y, and `fpDEL()` and the quantile search called it for
  every count from 0 to q. The recurrence is now carried forward one step per
  count, in O(1) memory, and the vector `fdDEL()` and `fpDEL()` carry it from
  one element to the next while `mu`, `sigma` and `nu` repeat (`fpDEL(0:q)` was
  O(q^3)). The values are the same doubles (from count 4096 on, the accuracy
  bullet under Bug fixes then changes them). A quantile near 1e5
  (`fqDEL(0.5, 1e5, 0.01, 0.5)`) took 43 s and now takes 2 ms; on 1e6 rows of
  IMPACTncd's veg parameters `fqDEL()` is 1.4 times and `fpDEL(10, ...)` 2.3
  times faster.
* The DEL scalars in `inst/include/distr_DEL.h` (`LinkingTo`) are safe at
  `INT_MAX` and take O(1) memory: `ftofydel2_scalar()` allocated 8 (y + 2) bytes
  per call (`std::bad_alloc` at 2147483646), `fpDEL_hlp_fn()` never returned at
  `INT_MAX`, and `fdDEL_scalar()` wrapped there. They are still O(q) in time:
  about a minute for the CDF at `INT_MAX`.
* **`fpSICHEL()`, `fdSICHEL()`, `fpZISICHEL()` and `fdZISICHEL()` take O(1)
  memory, and the CDF stops once its sum has settled.** `fcdfSICHEL_scalar()`
  stored the whole Bessel-ratio recursion in two `std::vector`s of q + 1 doubles
  and `ftofySICHEL2_scalar()` in one, although each step needs only the previous
  one, and the CDF went on adding terms long after its sum had stopped changing.
  The recursion is now carried forward one step at a time, and the CDF returns
  once a term past the mode leaves the sum unchanged (the pmf is unimodal); the
  return is withheld where a very negative `nu` makes the recursion break down,
  so the values are the same doubles, NaN included (11.5 million results
  compared bit for bit). `fpSICHEL(5e7, 2, 1, -0.5)` took 0.89 s and 762 MB and
  takes 2 ms and no extra memory. The header scalars are safe at `INT_MAX`
  (where the CDF needed about 32 GB) but still O(q) in time where the sum does
  not settle (about 26 s at 2147483646).
* **`fqBNB()`, `fqZIBNB()` and `fqZABNB()` evaluate the term at 0 once, and
  check the int-range bound late.** The quantile search computed the log of the
  term at 0 up to three times (in the closed-form bound for a quantile beyond
  the int range, in the search for the first term that does not underflow, and
  as its own first term), and the bound added another log term. The term is now
  computed once, and the bound runs once the search has added 65,536 terms (at
  once, as before, when the term at 0 underflows or nearly: a large `mu`). A
  search that ends sooner cannot have been stopped by the bound, so every
  quantile is unchanged (738,805 results compared). On the smoking models'
  parameters `fqZABNB()` takes 0.66 us a draw, against 1.78 us in 0.1.33.
* **`fpDPO()` stops summing once the CDF has settled, and sums the upper tail
  itself.** The CDF added every density from 0 to `q`, however far past the
  mass: `fpDPO(1e7, 90, 2)` took 0.5 s for a sum complete by `q = 220`. It now
  starts at the first density that does not underflow (for `q > 64`) and stops
  once the terms, past the largest, no longer change a (normal) sum, which gives
  the same double as the full sum: the lower tail is unchanged bit for bit,
  including the models' calls at `q = 0` and `90`. (The DPO pmf can have a
  second mode at 0, and where it underflows a zero term is not taken as
  settled.) `lower_tail = FALSE` was `1 - F`, which is 0, negative or wrong by
  orders of magnitude once `F` rounds to 1: `fpDPO(80, 10, 2, lower_tail =
  FALSE)` was 4.4e-16 for a tail of 2.0e-23. Where `F > 0.5` the tail is now
  summed from `q + 1`: within 6e-12 (relative) of a log-sum-exp reference,
  never negative, and finite with `log_p = TRUE`.

## Tests

* `test-lookup_dt.R`: a gap in the second key, in a key after `year` and in the
  last of three keys; a double second key; consecutive integer keys after
  `year` still pass; one key message per call; `fixkey = TRUE` leaves a
  rejected table unkeyed; `lookup_dt()` refuses the `year`/`age` table above.
  Test 17, labelled "non-consecutive int key", had passed for the wrong reason
  -- its gapped key sorts second, so the row count rejected the table -- and
  now pins the message. These tests fail with 0.1.33's validator and pass with
  this one, apart from two that 0.1.33 passes too: consecutive integer keys
  after `year`, and one key message per call.
* `test-lookup_dt.R`, Tests 12i-12v: the missing values in the gap message
  (few, many, an `IDate` key, a range wider than an integer); NA in a factor
  key, in `is_valid_lookup_tbl()` and through `lookup_dt()`; an unused factor
  level, by name; NA in an integer key; NA reported as NA rather than as the
  duplicates it creates; an empty table; duplicated, NA and absent `keycols`; a
  stale key, a stale index, and a stale index on rows in key order; a
  duplicate hidden by a stale key; `fixkey = TRUE` past a stale index and with
  a stale key. All 21 fail with the loop fix as first committed, and pass with
  this one.
* `test-misc_functions.R`: at n = 1e6 the realised correlation is within 0.005
  of its target, which the double correction missed by more than 20 standard
  errors (the existing test allowed 0.1 at n = 1e4); a near-singular target no
  longer fails in `chol()`.
* `test-package_ops.R`: `detach_package("base")` returns `FALSE` after one
  attempt, with the reason. `search()` is wrapped to report the package
  attached at most 5 times, so a regression fails instead of hanging the test
  run. (A time limit cannot be relied on: when it fires inside the old loop's
  `try()`, the error is swallowed and R clears the limit.)
* `test-cklut_extra.R`: `cklut_build()` refuses an empty table (factor key with
  no levels; character key; integer key under `check = FALSE`), an all-NA
  factor key with no levels and an infinite key under `check = FALSE`, an NA
  integer key, a fractional numeric key, a numeric key beyond the integer
  range and an integer64 key; and the C++ refuses an innermost dimension with
  no values.
* `test-lookup_extra.R`: with validation off, the year x age {30, 35} table and
  a factor level that never occurs stop on the row count. `test-lookup_dt.R`
  Tests 32 and 40 now expect that message, instead of the later "Calculated
  row indices are out of bounds", which the row count makes unreachable.
* `test-fDPO.R`: Tests 21 and 23 compare with `dDPO_lse()`, a log-sum-exp
  normalisation, instead of gamlss.dist's truncated `dDPO()`; new: densities at
  `mu = 5000` sum to 1, `fpDPO(0, 5000, 2)` is 0 (was `Inf`), and the left tail
  at `mu = 1000`, `sigma = 10`.
* `test-distr_huge_mu.R`: medians beyond 1e6 for BNB, ZIBNB, ZABNB, SICHEL,
  ZISICHEL and DPO, checked by F(q) >= p > F(q - 1); SICHEL at `sigma = 0.001`;
  NA with a warning for p = 1 and for quantiles beyond the int range.
* `test-lookup_dt.R`, Tests 4b-4d: `set_lookup_tbl_key()` past a stale index,
  with a stale key, and on a double key column; and an absent key column.
* `test-fNBI.R`, `test-fZINBI.R`, `test-fZANBI.R`: `NA` with one warning per
  call for `p = 1`, `mu = Inf`, and a quantile above `INT_MAX` or exactly at
  it; ordinary quantiles unchanged and silent; `fqZINBI()` and `fqZANBI()` keep
  their finite value at `p = 1`. `test-fBCT.R`: `p = 0` and `p = 1` give 0 and
  `Inf` on both scales and tails, for `nu` of either sign; a `p` outside
  [0, 1] and invalid parameters still error.
* `test-fDEL_linear.R` (new): `fqDEL()` and `fpDEL()` at a median near 1e5
  against a direct convolution of `dnbinom()` and `ppois()`; `fpDEL(0:2000)` is
  the running sum of `fdDEL(0:2000)`, bit for bit; the vector forms equal
  element-by-element calls for ascending, descending and repeated counts;
  `fqDEL(fpDEL(0:10)) == 0:10` at IMPACTncd's veg parameters; a NaN or NA
  `sigma` after a valid element stays NA. Its three timing expectations (under
  5 s; the old code took 12-45 s per call) run only under `at_home()`.
* `test-fDPO.R`, Test 27: an infinite `mu` or `sigma` gives `NaN` (`NA` for
  `fqDPO()`) in under a second (before: 33 s, and 100 s for `fqDPO()`); Test 27b,
  a guard: `fget_C()` with an `NA` and `.Machine$integer.max` in `x`.
* `test-fMN4.R`, Tests 36-37: a zero-length `mu`, `sigma` or `nu` gives
  `integer(0)` (with the code before, each of these calls killed R).
* `test-distr_near_one.R` (new): finite DPO quantiles in [1 - 1e-9, 1) at
  pinned values (margins of at least 4.6e-12 from an independent long-double
  reference), the same answers on the upper tail and the log scale, round trips
  `fqDPO(fpDPO(x)) == x`, and `Inf` at `p = 1`. `test-fDPO.R` Tests 16 and 26
  compare with gamlss.dist only where its `qDPO()` is finite.
* `test-distr_huge_mu.R`: three BNB quantiles between 1e7 and 2e8 (margins of
  1e5 to 2e7 ulp from a quad-precision reference), a CDF at q = 1e7 and two log
  densities at 1e6 and 1.5e9, all of which the build before got wrong;
  `test-fBNB.R`: the CDF equals the sum of the densities, and `fpBNB()` past the
  point where its terms underflow (with a timing bound at home).
* `test-distr_huge_mu.R`: SICHEL and ZISICHEL quantiles beyond the int range
  are NA at once (timed at home), and finite quantiles with the bound's gate
  open stay finite; `test-fSICHEL.R`, Test 35: at N = 2000, over 24 parameter
  combinations, the bound behind this never claims F(N) < p where the CDF says
  otherwise, and fires well beyond the boundary.
* `test-fSICHEL.R` and `test-fZISICHEL.R`: the CDF at q = 5e7 is the CDF at
  q = 1e4, bit for bit (with a timing bound at home); and where a very negative
  `nu` breaks the recursion, `fpSICHEL()` is NaN exactly where the densities are.
* `test-fZANBI.R`, `test-fZINBI.R`: upper tails against `pnbinom()`-based and
  exact geometric references, as ratios or logs (a plain `expect_equal()` with a
  reference far below its tolerance cannot fail); `fpZANBI()` at `mu = 1e-17` is
  in [0, 1]; both tails sum to 1.
* `test-fMN4.R`, Tests 38-47: `frMN4()` is reproducible under `dqset.seed()`,
  equals `fqMN4(dqrunif(n))` draw for draw, ignores `set.seed()` and leaves base
  R's stream alone, matches the MN4 probabilities, and validates `n`; Tests 18,
  19 and 24 are seeded with `dqset.seed()`. A mutation check (14 deliberately
  broken variants) showed each is caught.
* `test-fSICHEL.R`: at `sigma = 1e-12` the CDF and the density against the
  Poisson plus the first-order effect of the mixing variance (a closed form with
  no Bessel function), the quantile search against the CDF on both sides of a
  step, and the models' box against `gamlss.dist`.
* `test-fDPO.R`, Test 28: `fpDPO(1e7, 90, 2)` equals `fpDPO(1000, 90, 2)` to
  the bit; upper tails against a log-sum-exp reference, as ratios and on the
  log scale; a (mu, sigma, q) grid including bimodal sets; guards at the models'
  parameters; and a pair where p(0) is at the underflow limit and the dip after
  it underflows to exactly 0.
* `test-distr_near_one.R` (BNB and SICHEL part): finite quantiles in
  [1 - 1e-9, 1) at pinned values (margins of at least 1.7e-12 from a
  quad-precision pmf sum and the Poisson-GIG mixture integrals), brackets on the
  package's own CDF deeper in the window, the same answers on the upper tail and
  the log scale, the settle step and the heavy-tail `NA`, `p = 1`, and round
  trips.
* `test-fDPO.R`, Test 29: at two pairs where `p(0)` is at the underflow limit
  and exact zeros follow, `fqDPO()` is finite, brackets `p` on `fpDPO()` and
  equals the quantile of the log-sum-exp density, also for `p` from 1e-320 to
  1e-10; a `p` above such a pair's largest sum still ends at once.
* `test-fDEL_linear.R`: the CDF, density and quantiles from count 4096 on
  against an R-level Poisson-NB convolution, including exact quantiles at `mu`
  1.2e4 to 1e9 (above 1e6 only under `at_home()`); vector == element-by-element
  across 4096; `fqDEL(fpDEL(q)) == q` there; odd parameters at 4095, 4096 and
  70000.
* `test-distr_near_one.R` (DEL part): finite quantiles in [1 - 1e-9, 1) pinned
  against a Poisson-NB convolution reference (margins of at least 1e-12),
  brackets on `fpDEL()` for quantiles beyond 4096, `Inf` at `p = 1`, the upper
  tail and log scale, the settle step, and round trips.
* `test-distr_cdf_le_one.R` (new): for DPO, DEL (scalar and vector), BNB,
  SICHEL, BCT and the ZI/ZA families, cases whose CDF exceeded 1 give a CDF of
  at most 1, a non-negative upper tail and a finite log upper tail; the vector
  `fpDEL()` still equals element-wise calls; `fpDPO(1e7, Inf, 2)` is `NaN` at
  once.
* `test-distr_near_one.R` (zero-inflated / zero-altered part): `q(F(x) + 1e-11)`
  and `q(F(x) - 1e-11)` at x = 0 and 1 for all five families, round trips
  (with zero masses up to 0.999), `p` at the zero mass of ZANBI and ZABNB,
  `p = 1`, the upper end pinned against a long-double reference, and the
  zero-altered transform at a small `mu`. `test-fZINBI.R`, `test-fZANBI.R`: the
  quantile at `p = 1` is finite and not below the old value, instead of pinned.

# CKutils 0.1.33

## Performance

* **`read_parquet_dt()` no longer deep-copies every table it reads.** arrow's
  `as.data.frame()` is `to_data_frame()` + its R-metadata step +
  `as.data.frame()`, and `write_parquet_dt()` stores class `data.table`, so the
  last step dispatched to `as.data.frame.data.table()`: a full copy, made only
  to be turned back into a data.table by `setDT()`. The data.table path now
  takes arrow's conversion without that last step (`.pq_arrow_to_df()`).
  Measured over 642 reads of 191 parquet datasets (IMPACTncd_Engl2026's inputs
  and model outputs): **-20% read time**, results `identical()`. If arrow ever
  drops its (unexported) metadata step, the old conversion is used.
  `as_data_table = FALSE` is unchanged.
* The copy had a second job, kept: it removed the write-time key (`sorted`) and
  `index` that arrow restores from the stored metadata. They are now removed by
  reference with `setattr()`. They must go: `setDT(key = )` trusts a `sorted`
  attribute that is already present and does not sort, while a multi-partition
  read returns rows in PATH order (`year=10` before `year=3`) -- keeping them
  gave a false key on 238 of those 642 reads.

## Tests

* `test-parquet_read_conversion.R`: the false-key variant (the metadata step
  without the removal) claims the key on an out-of-order multi-partition
  fixture; `read_parquet_dt()` on it is truly in key order by base `order()`
  (`forderv()` reuses a stored key, so it cannot check this), also across the
  `year=9` / `year=10` boundary; no stale index; a custom attribute restored;
  no deep copy; `as_data_table = FALSE` still a plain data.frame. The existing
  parquet tests did not catch a false key (737 passed on that variant but one).

# CKutils 0.1.32

## Tests

* The distribution tests no longer take as reference the gamlss.dist 6.1-11
  (CRAN 2026-09-10) functions that are wrong there: its d{DPO,DEL,ZINBI}() when
  the parameters vary along the vectors, pNBI() below sigma = 1e-4, pZABNB(),
  qZIBNB() and dZISICHEL(). The references are now implementation-independent
  (base R's dnbinom()/ppois(), the zero-inflation identity, CDF = cumsum(PMF),
  quantile = min{y : F(y) >= p}) or gamlss.dist's constant-parameter calls.
  CKutils' own results are unchanged: they meet every one of these to <= 1e-15.

## Performance

* **`read_parquet_dt()` no longer attempts an open that is certain to fail.**
  It opens with `open_dataset(path, partitioning = partitioning)` and, on
  error, with `open_dataset(path)`, which discovers Hive-style (`key=value`)
  partitions itself. arrow takes the default `partitioning = "hive"` as the
  *name* of a partition field, so on every Hive-partitioned and every flat
  directory the first attempt failed -- after listing everything below the
  directory, so it cost about 0.27 ms per file: ~26 ms for a 98-file table,
  ~5.7 s for a 20,000-file tree.

  With `partitioning = "hive"` and one local directory, `read_parquet_dt()`
  now first checks whether that attempt is certain to fail under arrow's own
  rules, from the normalised path and one non-recursive listing of the
  directory, and if so opens with the fallback directly. It is certain to fail
  when

  - a segment of the path, or an entry directly under the directory, is a
    `key=value` whose key is not `hive` (and contains no `%`, which arrow would
    unescape) -- e.g. every Hive-partitioned dataset, and any directory below
    a `key=value` directory; or
  - there is no `key=value` there at all, and no subdirectory or symbolic
    link directly under the directory (a flat or empty directory).

  Everything else takes the two-step open exactly as before. So a directory
  whose files sit in subdirectories that are not `key=value` still gets the
  extra `hive` column the first attempt gives it, and a partition key
  literally named `hive` -- or a directory below an ancestor named
  `hive=...` -- is read as before. A file, a vector of paths, a URI and any
  other `partitioning` value are opened as before.

  - **Results are unchanged**: data, types, factor levels, column order, keys,
    and errors (class, message and call). The one visible difference is in an
    error's backtrace: when the fallback itself fails after a skipped attempt
    (e.g. a directory of files that are not parquet), `rlang::last_trace()`
    no longer shows the four frames of the failed first attempt's `tryCatch()`
    handler.
  - No state, no cache, no new arguments or options: a dataset rewritten or
    re-laid-out on disk is read exactly as a first read would be.
  - The check costs a `normalizePath()`, one directory listing and, for a
    directory with no `key=value` entry, one `stat` per entry: 10-160 us for
    the layouts measured (3 ms for a 2,000-entry directory).
  - Measured in IMPACTncd_Engl2026 (one windowed chunk, n = 10,000, run side
    by side with 0.1.31): the baseline run 593 -> 516 s (-13%) and a scenario
    arm 174 -> 143 s, with bit-identical output. Every one of the model's 3,938
    directory reads skips the failing attempt; its 48 file reads are opened as
    before.
  - New tests (`test-parquet_first_open.R`) pin which layouts skip, that every
    skipped attempt really fails under the installed arrow (a tripwire for a
    future arrow that decides differently), and that every read is identical
    to the two-step open, errors included, on 53 layouts.

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
