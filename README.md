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

# Fast data.table operations
dt <- data.table(x = 1:1000, y = rnorm(1000))
lookup_dt(dt, x = c(1, 5, 10))  # Fast lookup by key

# Statistical distributions
fdBCPEo(x = 1:5, mu = 2, sigma = 0.5, nu = 1, tau = 2)  # BCPEo density

# Utility functions
normalise(c(1, 2, 3, 4, 5))  # Normalize to [0,1]
```

## License

GPL-3 | See [LICENSE.md](LICENSE.md) for details
