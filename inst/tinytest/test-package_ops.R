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
# Regression tests for the install-vs-skip decision (stale-snapshot bug)
# =============================================================================
# Bug: dependencies() used to take a single installed.packages() snapshot before
# its per-package loop and decide install-vs-skip from that *stale* snapshot.
# When installing an earlier package pulled a later list entry in as a transitive
# dependency, that entry ended up installed on disk but absent from the snapshot,
# so it was needlessly scheduled for reinstall -> on Windows the benign-but-noisy
# warning "package 'x' is in use and will not be installed". The fix re-checks
# each package fresh, against live library state, via the non-loading helper
# .pkg_is_installed() (find.package(), not requireNamespace()).
#
# What is / isn't testable: the end-to-end failure needs a real mid-loop
# install.packages() (to make the on-disk state diverge from a pre-loop
# snapshot), which the test suite must not perform. So the discriminating
# regression guard lives at the helper level -- .pkg_is_installed() must reflect
# the *current* library trees, so a package appearing after any earlier snapshot
# is still recognised.

.pkg_is_installed <- getFromNamespace(".pkg_is_installed", "CKutils")

# Contract of the helper: TRUE for an installed package, FALSE for a missing one,
# without loading/attaching anything (find.package, not requireNamespace).
expect_true(
  .pkg_is_installed("tools"),
  info = ".pkg_is_installed: TRUE for an installed (base) package"
)
expect_false(
  .pkg_is_installed("this_package_does_not_exist_zzz_123"),
  info = ".pkg_is_installed: FALSE for a non-existent package"
)

# Discriminating regression guard for the stale-snapshot bug: a package that
# becomes installed *after* a snapshot was taken must still be seen by the fresh
# check. We reproduce the bug's blind spot by (1) taking a snapshot, then (2)
# materialising a minimal package in a temporary library prepended to .libPaths()
# afterwards. A snapshot-based check (the old code) would miss it; the live
# find.package() check the fix uses must not -- so this assertion would FAIL if
# .pkg_is_installed() ever reverted to consulting a pre-captured snapshot.
# Results are captured into booleans and .libPaths()/the temp dir are restored
# *before* asserting, so global state stays clean regardless of the outcome.
# NB: the fake package is intentionally NOT passed to dependencies() -- it is not
# loadable, so it would hit the `!myrequire(pkg)` branch and attempt a real
# install.packages() (a network call). It is only ever fed to the helper.
stale_snapshot <- rownames(installed.packages())
fake_pkg <- "ckutilsfakepkgzzz"
tmplib <- tempfile("ckutils_testlib")
dir.create(tmplib)
fake_dir <- file.path(tmplib, fake_pkg)
dir.create(fake_dir)
writeLines(
  c(
    paste0("Package: ", fake_pkg),
    "Version: 1.0",
    "Type: Package",
    "Title: Fake Package For Testing find.package Detection",
    "Description: Not a real package; exists only so find.package() sees it.",
    "License: GPL-3"
  ),
  file.path(fake_dir, "DESCRIPTION")
)
old_libs <- .libPaths()
.libPaths(c(tmplib, old_libs))
fake_absent_from_snapshot <- !(fake_pkg %in% stale_snapshot)
fake_seen_by_fresh_check <- .pkg_is_installed(fake_pkg)
.libPaths(old_libs)
unlink(tmplib, recursive = TRUE)

expect_true(
  fake_absent_from_snapshot,
  info = "regression setup: package is absent from the pre-existing snapshot"
)
expect_true(
  fake_seen_by_fresh_check,
  info = "dependencies: fresh find.package check sees a pkg installed post-snapshot"
)

# Behavioural smoke test (NOT a stale-snapshot regression guard -- it passes on
# the old code too): an already-installed, loadable, non-repository package flows
# through dependencies() with install = TRUE without being (re)installed and
# still loads, exercising the no-install / loaded-package path end-to-end through
# the fixed .pkg_is_installed() check.
result_no_reinstall <- dependencies(
  "tools",
  install = TRUE,
  update = FALSE,
  quiet = TRUE,
  verbose = TRUE
)
expect_false(
  result_no_reinstall["tools", "installed"],
  info = "dependencies: already-installed package is not reinstalled (install=TRUE)"
)
expect_true(
  result_no_reinstall["tools", "loaded"],
  info = "dependencies: already-installed package still loads"
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
