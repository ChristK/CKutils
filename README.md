# CKutils

<!-- badges: start -->
[![R-CMD-check](https://github.com/ChristK/CKutils/workflows/R-CMD-check/badge.svg)](https://github.com/ChristK/CKutils/actions)
<!-- [![Tests](https://github.com/ChristK/CKutils/workflows/Tests/badge.svg)](https://github.com/ChristK/CKutils/actions) -->
[![rchk](https://github.com/ChristK/CKutils/workflows/rchk/badge.svg)](https://github.com/ChristK/CKutils/actions)
[![test-coverage](https://github.com/ChristK/CKutils/workflows/test-coverage/badge.svg)](https://github.com/ChristK/CKutils/actions)
[![Codecov test coverage](https://codecov.io/gh/ChristK/CKutils/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ChristK/CKutils?branch=main)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/CKutils)](https://cran.r-project.org/package=CKutils)
<!-- badges: end -->

A collection of utility functions for R data analysis and simulation modelling, featuring high-performance implementations for common data manipulation tasks, statistical distributions, and package management operations.

## Key Features

- **🚀 Fast data operations**: Optimised data.table operations with C++ backend
- **📊 Statistical distributions**: more efficient implementation of some the distributions available in the [gamlss.dist](https://cran.r-project.org/package=gamlss.dist) package
- **🔧 Data manipulation**: Efficient lookup tables, quantile calculations, and data transformations  
- **📦 Package utilities**: Dependency management and local package installation helpers
- **⚡ Performance optimised**: SIMD vectorisation for numerical computations

## Installation

```r
# Install from GitHub
if (!require(remotes)) install.packages("remotes")
remotes::install_github("ChristK/CKutils")
```

## Quick Start

```r
library(CKutils)
library(data.table)
# Fast data.table operations
lookup_table <- data.table(x = 1:1000, y = rnorm(1000), key = "x")
dtb <- data.table(x = c(1L, 5L, 10L))
lookup_dt(dtb, lookup_table)  # Fast lookup by key
dtb[]

# Statistical distributions
fdBCPEo(x = 1:5, mu = rep(2, 5), sigma = rep(0.5, 5), nu = rep(1, 5), tau = rep(2, 5))  # BCPEo density

# Utility functions
normalise(c(1, 2, 3, 4, 5))  # Normalise to [0,1]
```

## Larger-than-RAM lookups: `cklut`

`cklut_*` is a memory-mapped, on-disk drop-in for `lookup_dt`. Build a dense
lookup table once (from a `data.table`, CSV or Parquet); queries then read value
columns straight off the memory-mapped file, so the table can exceed RAM and
there is no per-call rebuild. Value columns may be double, integer, logical or
factor/character — exactly like `lookup_dt` — and unmatched keys return `NA`.

```r
ck  <- cklut_build(lookup_tbl, "dist", keys = c("year", "age", "sex"))
res <- cklut_lookup(tbl, ck)          # drop-in for lookup_dt(tbl, lookup_tbl)
cklut_to_csv(ck, "dist.csv")          # export back out (also cklut_to_parquet)
```

The C++ engine, benchmarks and validation (against both `lookup_dt` and
`absorb_dt`) live in [`cklut/`](cklut/).

## Using the C++ kernels from another package

The distribution functions ship their per-element kernels as `inline` functions in
`inst/include/distr_*.h`, so another package can call them straight from its own
C++ — no `.Call()` round trip, no linking against `CKutils.so`:

```r
# DESCRIPTION
LinkingTo: Rcpp, CKutils
```

```cpp
#include <distr_BNB.h>
#include <recycling_helpers.h>

double d = fdBNB_scalar(x, mu, sigma, nu, /* log = */ false);
```

### Caller contract: bound your counts

**The `*_scalar` kernels perform no bounds checking, by design** — they are meant
for hot per-element loops, so every branch they skip is the point of them. The
R-facing wrappers (`fdBNB()`, `fpBNB()`, …) validate on your behalf; calling the
kernels directly means you take that job on. The contract is:

```
0 <= x, q <= CK_MAX_COUNT      // CK_MAX_COUNT == INT_MAX - 1
```

Use `count_to_int()` from `recycling_helpers.h`, which is exactly the guard the
wrappers apply:

```cpp
int q_i;
if (!count_to_int(q_double, q_i)) return NA_REAL;   // NA, NaN, or out of range
double cdf = fpBNB_scalar(q_i, mu, sigma, nu, true, false);
```

Where the contract is broken the consequences differ by kernel. Only the DPO
kernels can still fail at `INT_MAX` itself:

| kernel | what happens when the contract is broken |
|---|---|
| `fpDPO_scalar` | **The call never returns** at `q == INT_MAX` when its sum cannot settle. It accumulates with `for (int i = ...; i <= q; i++)`, so `i++` overflows and the loop has no exit; not interruptible from R. The sum stops once settled, so a finite `mu > 0` and `sigma > 0` return at once, and a non-finite `mu` or `sigma` gives `NaN` at once; what still runs to `q` is `mu <= 0` or `sigma <= 0` (every term 0, which the wrappers reject): `fpDPO_scalar(INT_MAX, 0, 1.5)` did not return in 90 s. |
| `fdDPO_scalar` | **A wrong log density** at `x == INT_MAX`. It evaluates `lgamma(x + 1)`, whose argument wraps to `INT_MIN`; e.g. `fdDPO_scalar(INT_MAX, 2, 1.5, log = TRUE)` gives `-Inf` where the true value is about `-2.8e10` (on the natural scale that example is `0`, which is also the true value). |

The BNB, DEL and SICHEL kernels no longer fail at `INT_MAX` itself. They used to
(the BNB and DEL CDF loops never returned, the BNB and DEL densities wrapped, and
the DEL and SICHEL kernels allocated a `std::vector<double>` of `y + 1` or `y + 2`
elements, tens of gigabytes for a large `y`); now they take O(1) memory and
return, but are O(q) in **time**:

| kernel | at `INT_MAX` (one core, `-O2`) |
|---|---|
| `fdBNB_scalar`, `fdZABNB_scalar` | right: `fdBNB_scalar(INT_MAX, 2, 1, 1)` is `1.21169035e-27`, the exact value (it was `0`). |
| `fpBNB_scalar`, `fpZABNB_scalar` | return: `fpBNB_scalar(INT_MAX, 2, 1, 1)` takes about 4.8 s; less where the terms underflow (`sigma = 0.01`: 0.001 s). |
| `ftofydel2_scalar`, `fdDEL_scalar`, `fpDEL_hlp_fn`/`fpDEL_scalar` | return: `fdDEL_scalar(INT_MAX, 2, 1, 0.5)` takes about 18 s, `fpDEL_hlp_fn(INT_MAX, 2, 1, 0.5)` about 64 s. |
| `fdSICHEL_scalar`, `fpSICHEL_scalar`, `fdZISICHEL_scalar`, `fpZISICHEL_scalar` | return: `fdSICHEL_scalar(INT_MAX, 2, 1, -0.5)` takes about 14 s; the CDF stops once its sum has settled (`fpSICHEL_scalar(INT_MAX, 2, 1, -0.5)` is instant), and `fpSICHEL_scalar(INT_MAX, 1e9, 1, -0.5)` takes about 26 s. |

None of these loops can be interrupted from R (the R functions `fqDEL()`,
`fqSICHEL()` and `fqDPO()` check for Ctrl-C in their own searches; these kernels
do not, as a LinkingTo caller may run them where R cannot be called), so a
large-but-legal `q` is slow rather than wrong: bound `q` well below
`CK_MAX_COUNT` in anything performance-sensitive.

One more cost, in DPO: with a huge `mu` **and** a huge `sigma` (`mu = 3e9`,
`sigma = 4e6`) the normalising constant meets no stopping rule, scans the whole
`int` range and only then gives `NaN`: about 50 s for `fpDPO_scalar(10, 3e9, 4e6)`
(and for the first `fdDPO_scalar` call), and the same for `fqDPO()`, `fpDPO()` and
`fdDPO()` with such a parameter set.

Safe for any `int`, being closed form or bounded by their support:
`fdNBI_scalar`/`fpNBI_scalar` (and the ZANBI and ZINBI kernels built on them) and
`fdMN4_scalar`/`fpMN4_scalar`. The `fq*_search` quantile searches scan at most
`CK_SEARCH_MAX` (`INT_MAX - 1`) terms, return `NA` for a quantile beyond it, and
give up earlier on a search that cannot reach `p`; they are O(quantile) in time.
The `frNBI_scalar`/`frZANBI_scalar`/`frZINBI_scalar` samplers invert the
corresponding `fq*_scalar` at a uniform you supply, and are safe only for finite
parameters and `u < 1`: they return `NA_INTEGER` (not a count) when the quantile
is not an `int` in `[0, CK_MAX_COUNT]` (`u = 1` for `frNBI_scalar`, a non-finite
`mu`, or a `mu` so large that the quantile exceeds `INT_MAX - 1`).

`recycling_helpers.h` carries the authoritative statement of all of the above,
and each `distr_*.h` repeats the part that applies to it.

The two saturation directions are not symmetric in their consequences: on
x86-64 you get a wrong number, on AArch64 an `INT_MAX` that now returns for the
BNB, DEL and SICHEL kernels (after seconds to a minute) but can still be a
non-terminating loop in a DPO sum that cannot settle. Both are
checked on real hardware — `inst/tinytest/test-arch-int-conversion.R` runs on
arm64 macOS, arm64 Linux and x86-64 Linux in the `arch-int-conversion`
workflow, asserting that every guarded call returns `NA` *and* returns
promptly, so the hang mode is caught as well as the wrong-value mode. That
workflow also runs `.github/scripts/arch_cast_probe.R`, which compiles a
deliberately unguarded cast and prints which way the CPU actually saturates —
so the per-architecture claims above are measured rather than inferred.

## License

GPL-3 | See [LICENSE.md](LICENSE.md) for details
