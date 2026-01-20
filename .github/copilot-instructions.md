# CKutils Copilot Instructions

## Project Overview

CKutils is an R package providing high-performance utility functions for simulation modelling, with C++ backends via Rcpp. Key domains: data.table operations, statistical distributions (GAMLSS-compatible), and package management utilities.

## Architecture

- **R files** ([R/](R/)): Public API with roxygen2 documentation
- **C++ files** ([src/](src/)): Performance-critical implementations using Rcpp
- **Tests** ([inst/tinytest/](inst/tinytest/)): tinytest framework, validated against gamlss.dist

### Key Modules
| File | Purpose |
|------|---------|
| `R/rng_distr.R` | `fr*` random generation functions (BCPEo, BCT, NBI, SICHEL, etc.) |
| `R/lookup_dt.R` | Fast key-based table lookups with `lookup_dt()`, `absorb_dt()` |
| `R/dt_ops.R` | data.table utilities: `clone_dt()`, `del_dt_rows()` |
| `src/distr_*.cpp` | Vectorised distribution functions (`fd*`, `fp*`, `fq*` for PDF/CDF/quantile) |
| `src/lookup_dt.cpp` | C++ backend for `lookup_dt()` using data.table API |

## Development Workflow

### Pre-push validation (REQUIRED)
```bash
./check-before-push.sh
```
This runs: roxygen2 → R CMD build → R CMD INSTALL → R CMD check --as-cran → tinytest suite

### Quick iteration during development
```r
# Regenerate documentation and install
roxygen2::roxygenise()
# Run tests
tinytest::test_package("CKutils")
```

### Build requirements
- C++17 (set in `src/Makevars`)
- Dependencies: `data.table >= 1.18.0`, `Rcpp`, `dqrng`, `arrow >= 22.0.0`
- Tests require: `gamlss.dist` for distribution validation

## Coding Conventions

### R Functions
- Use **roxygen2** for all exports with `@export` tag
- Document all parameters with types and defaults in `@param`
- Include runnable `@examples` for every exported function
- Prefix fast C++ wrappers with `f` (e.g., `fdBCPEo` vs gamlss.dist's `dBCPEo`)

### Distribution Functions Pattern
Each distribution (e.g., BCPEo) follows naming convention:
- `fd*` - density (PDF), accepts `log_` parameter
- `fp*` - probability (CDF), accepts `lower_tail` parameter
- `fq*` - quantile (inverse CDF)
- `fr*` - random generation, uses `dqrng::dqrunif` for high-quality RNG

### C++ Code
- Use `recycling_helpers.h` for parameter recycling (all vectors same length)
- Include SIMD hints where applicable: `#pragma GCC ivdep`
- Validate inputs early with `Rcpp::stop()` for clear error messages
- For data.table integration, include `<datatableAPI.h>`

### data.table Operations
- Functions modify tables **in place** by default (data.table semantics)
- Use `copy()` explicitly when non-destructive behaviour is needed
- Factor columns in `lookup_dt()` must have consecutive integer levels starting from 1

## Testing Pattern

Tests in `inst/tinytest/test-*.R` follow this structure:
```r
# Skip if optional dependency unavailable
if (!requireNamespace("gamlss.dist", quietly = TRUE)) {
  exit_file("gamlss.dist not available")
}

# Generate test data with seed for reproducibility
data <- generate_test_data(n = 100, seed = 123)

# Compare against reference implementation
expect_equal(fdBCPEo(...), gamlss.dist::dBCPEo(...), info = "PDF matches reference")
```

## Common Gotchas

1. **Lookup tables**: Keys must be factors OR consecutive 1-based integers; use `is_valid_lookup_tbl()` to validate
2. **Random number generation**: Always use `dqrng::dqrunif` instead of base R's `runif` for reproducibility
3. **Windows compatibility**: C++ code must handle in-place modifications carefully; prefer `clone()` in Rcpp
4. **R CMD check**: Avoid non-portable compiler flags; `check-before-push.sh` sets safe defaults
