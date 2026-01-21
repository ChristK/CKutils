# =============================================================================
# Tests for package_ops.R
# =============================================================================
# Tests for detach_package, dependencies functions
# Note: installLocalPackage and installLocalPackageIfChanged are skipped
# as they require side effects (package installation, roxygen2 processing)

# Note: This test file is designed to run via tinytest::test_package("CKutils")
# which automatically loads the package.

# Ensure CRAN mirror is set for tests that use available.packages()
if (is.null(getOption("repos")) || getOption("repos")["CRAN"] == "@CRAN@") {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

# =============================================================================
# Tests for detach_package function
# =============================================================================

# Test 1: Error on non-character input
expect_error(
  detach_package(123),
  pattern = "must be a character string",
  info = "detach_package: Error on numeric input"
)

expect_error(
  detach_package(NULL),
  pattern = "must be a character string",
  info = "detach_package: Error on NULL input"
)

# Test 2: detach_package returns FALSE for non-attached package
result <- detach_package("nonexistent_package_xyz")
expect_false(
  result,
  info = "detach_package: Returns FALSE for non-attached package"
)

# Test 3: Message shown for non-attached package
expect_message(
  detach_package("another_nonexistent_pkg"),
  pattern = "was not attached",
  info = "detach_package: Shows message for non-attached package"
)

# =============================================================================
# Tests for dependencies function
# =============================================================================

# Test 1: Error when no packages specified
expect_error(
  dependencies(),
  pattern = "No packages specified",
  info = "dependencies: Error when no packages specified"
)

# Test 2: Error when pkges is not character
expect_error(
  dependencies(pkges = 123),
  pattern = "must be character strings",
  info = "dependencies: Error when pkges is not character"
)

# Test 3: Basic functionality with already installed package
# Using 'stats' which is always available
result <- dependencies(
  "stats",
  install = FALSE,
  quiet = TRUE,
  verbose = TRUE
)
expect_true(is.data.frame(result), info = "dependencies: Returns data.frame")
expect_true("stats" %in% rownames(result), info = "dependencies: rownames OK")
expect_true(
  all(c("loaded", "installed", "loaded.version", "available.version") %in%
        names(result)),
  info = "dependencies: Correct column names"
)
expect_true(
  result["stats", "loaded"],
  info = "dependencies: stats package loads correctly"
)

# Test 4: Test with multiple base packages (quoted)
result_multi <- dependencies(
  c("stats", "utils"),
  install = FALSE,
  quiet = TRUE,
  verbose = TRUE
)
expect_equal(nrow(result_multi), 2, info = "dependencies: Multiple packages")
expect_true(all(result_multi$loaded), info = "dependencies: All loaded")

# Test 5: Test unquoted package names
result_unquoted <- dependencies(
  stats, utils,
  install = FALSE,
  quiet = TRUE,
  verbose = TRUE
)
expect_equal(nrow(result_unquoted), 2, info = "dependencies: Unquoted names")
expect_true(
  "stats" %in% rownames(result_unquoted),
  info = "dependencies: Unquoted stats in rownames"
)
expect_true(
  "utils" %in% rownames(result_unquoted),
  info = "dependencies: Unquoted utils in rownames"
)

# Test 6: Test quiet parameter
expect_silent(
  dependencies("stats", install = FALSE, quiet = TRUE, verbose = FALSE),
  info = "dependencies: quiet=TRUE suppresses messages"
)

# Test 7: Duplicate packages are handled
result_dups <- dependencies(
  c("stats", "stats", "utils"),
  install = FALSE,
  quiet = TRUE,
  verbose = TRUE
)
expect_equal(nrow(result_dups), 2, info = "dependencies: Duplicates removed")

# Test 8: Test invisible return when verbose = FALSE
result_invisible <- dependencies(
  "stats",
  install = FALSE,
  quiet = TRUE,
  verbose = FALSE
)
expect_true(
  is.data.frame(result_invisible),
  info = "dependencies: Returns df even with verbose=FALSE"
)

# =============================================================================
# Tests for installLocalPackage error conditions
# =============================================================================
# Note: We only test error conditions, not actual installation

# Test: Error when DESCRIPTION file not found
temp_empty_dir <- tempfile("empty_pkg")
dir.create(temp_empty_dir)
on.exit(unlink(temp_empty_dir, recursive = TRUE), add = TRUE)

expect_error(
  installLocalPackage(temp_empty_dir),
  pattern = "DESCRIPTION file not found",
  info = "installLocalPackage: Error when DESCRIPTION missing"
)

# Test: Error for installLocalPackageIfChanged when DESCRIPTION missing
expect_error(
  installLocalPackageIfChanged(temp_empty_dir, tempfile()),
  pattern = "DESCRIPTION file not found",
  info = "installLocalPackageIfChanged: Error when DESCRIPTION missing"
)
