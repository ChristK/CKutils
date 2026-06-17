# CKutils

<!-- badges: start -->
[![R-CMD-check](https://github.com/ChristK/CKutils/workflows/R-CMD-check/badge.svg)](https://github.com/ChristK/CKutils/actions)
<!-- [![Tests](https://github.com/ChristK/CKutils/workflows/Tests/badge.svg)](https://github.com/ChristK/CKutils/actions) -->
[![rchk](https://github.com/ChristK/CKutils/workflows/rchk/badge.svg)](https://github.com/ChristK/CKutils/actions)
[![test-coverage](https://github.com/ChristK/CKutils/workflows/test-coverage/badge.svg)](https://github.com/ChristK/CKutils/actions)
[![Codecov test coverage](https://codecov.io/gh/ChristK/CKutils/branch/master/graph/badge.svg)](https://app.codecov.io/gh/ChristK/CKutils?branch=master)
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

## License

GPL-3 | See [LICENSE.md](LICENSE.md) for details
