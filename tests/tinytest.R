# CKutils test runner using tinytest
# Optimized for GitHub Actions CI with parallel test support

if (require("tinytest", quietly = TRUE)) {
  # Detect number of CPUs for parallel testing
  # Use parallel::detectCores() with fallback, capped at sensible maximum
  detected_cores <- parallel::detectCores(logical = FALSE)
  if (is.na(detected_cores) || detected_cores < 1L) {
    detected_cores <- 1L
  }
  ncpus <- getOption(
    "Ncpus",
    default = max(1L, min(detected_cores - 1L, 4L))  # Cap at 4 cores max
  )

  # In CI environments or R CMD check, limit parallelism to avoid resource contention
  if (nzchar(Sys.getenv("CI")) || nzchar(Sys.getenv("GITHUB_ACTIONS")) ||
      nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
    ncpus <- min(ncpus, 2L)
  }

  # Run tests with parallel support when ncpus > 1
  tinytest::test_package(
    "CKutils",
    ncpu = ncpus,
    side_effects = TRUE  # Allow tests with side effects
  )
}
