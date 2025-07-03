# CKutils

<!-- badges: start -->
[![R-CMD-check](https://github.com/ChristK/CKutils/workflows/R-CMD-check/badge.svg)](https://github.com/ChristK/CKutils/actions)
[![Tests](https://github.com/ChristK/CKutils/workflows/Tests/badge.svg)](https://github.com/ChristK/CKutils/actions)
[![test-coverage](https://github.com/ChristK/CKutils/workflows/test-coverage/badge.svg)](https://github.com/ChristK/CKutils/actions)
[![Codecov test coverage](https://codecov.io/gh/ChristK/CKutils/branch/main/graph/badge.svg)](https://codecov.io/gh/ChristK/CKutils?branch=main)
<!-- badges: end -->

A collection of utility functions for R data analysis and simulation modelling, featuring high-performance implementations for common data manipulation tasks, statistical distributions, and package management operations.

## Key Features

- **🚀 Fast data operations**: Optimized data.table operations with C++ backend
- **📊 Statistical distributions**: more efficient implementation of some the distributions available in the [gamlss.dist](https://cran.r-project.org/package=gamlss.dist) package
- **🔧 Data manipulation**: Efficient lookup tables, quantile calculations, and data transformations  
- **📦 Package utilities**: Dependency management and local package installation helpers
- **⚡ Performance optimized**: SIMD vectorization for numerical computations

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
normalise(c(1, 2, 3, 4, 5))  # Normalize to [0,1]
```

## License

GPL-3 | See [LICENSE.md](LICENSE.md) for details
