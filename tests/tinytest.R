# CKutils test runner using tinytest
# Optimized for GitHub Actions CI with parallel test support

if (require("tinytest", quietly = TRUE)) {
  # Detect number of CPUs for parallel testing
  # Use parallel::detectCores() with fallback
  ncpus <- getOption(
    "Ncpus",
    default = max(1L, parallel::detectCores(logical = FALSE) - 1L)
  )

  # In CI environments, limit parallelism to avoid resource contention
  if (nzchar(Sys.getenv("CI")) || nzchar(Sys.getenv("GITHUB_ACTIONS"))) {
    ncpus <- min(ncpus, 2L)
  }

  # Run tests with parallel support when ncpus > 1
  tinytest::test_package(
    "CKutils",
    ncpu = ncpus,
    side_effects = TRUE  # Allow tests with side effects
  )
}
