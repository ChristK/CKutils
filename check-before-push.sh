#!/bin/bash
# Local test script to simulate what GitHub Actions will run
# Run this before pushing to catch issues early

echo "=== CKutils Local Test Runner ==="
echo "This simulates what GitHub Actions will run"
echo ""

# Use ccache if available to speed up compilation
if command -v ccache >/dev/null 2>&1; then
    echo "⚡ Using ccache for compilation"
    export CC="ccache gcc"
    export CXX="ccache g++"
    export CXX11="ccache g++"
    export CXX14="ccache g++"
    export CXX17="ccache g++"
    export CXX20="ccache g++"
else
    echo "ℹ️  ccache not found; proceeding without it"
fi

# Use portable compiler flags to avoid non-portable NOTE in R CMD check
export CFLAGS="-O2 -pipe"
export CXXFLAGS="-O2 -pipe"
export CXX11FLAGS="-O2 -pipe"
export CXX14FLAGS="-O2 -pipe"
export CXX17FLAGS="-O2 -pipe"
export CXX20FLAGS="-O2 -pipe"
export _R_CHECK_COMPILATION_FLAGS_KNOWN_="-Werror=format-security -Wp,-D_FORTIFY_SOURCE=2 -Wp,-D_GLIBCXX_ASSERTIONS -march=x86-64-v2"

# Check if we're in the right directory
if [ ! -f "DESCRIPTION" ]; then
    echo "Error: Please run this script from the CKutils package root directory"
    exit 1
fi

# Clean up previous build artifacts
echo "🧹 Cleaning up previous build artifacts..."
rm -f CKutils_*.tar.gz
rm -rf CKutils.Rcheck # R CMD check output directory with dot
rm -rf ..Rcheck # R CMD check output directory if run from subdir or with .. prefix
rm -f tests/test.R # Remove auto-generated test file

# Get package name and version
PKG_NAME=$(grep -E "^Package:" DESCRIPTION | awk '{print $2}')
PKG_VERSION=$(grep -E "^Version:" DESCRIPTION | awk '{print $2}')
TARBALL="${PKG_NAME}_${PKG_VERSION}.tar.gz"

Rscript -e "roxygen2::roxygenise()"

# Build the package
echo "📦 Building package tarball..."
R CMD build .

if [ ! -f "$TARBALL" ]; then
    echo "❌ Package build failed: $TARBALL not found"
    exit 1
fi
echo "✅ Package built: $TARBALL"

# Install the package from the tarball (optional, but good for a full test)
echo ""
echo "📦 Installing package from tarball..."
R CMD INSTALL "$TARBALL" --no-multiarch --with-keep.source

if [ $? -ne 0 ]; then
    echo "❌ Package installation from tarball failed"
    exit 1
fi
echo "✅ Package installed from tarball."


# Run R CMD check on the tarball
echo ""
echo "🔍 Running R CMD check on $TARBALL..."
R CMD check "$TARBALL" --no-manual --as-cran

# R CMD check creates a directory named <packagename>.Rcheck
CHECK_DIR="${PKG_NAME}.Rcheck"

if [ ! -d "$CHECK_DIR" ] || ! grep -q "Status: OK" "${CHECK_DIR}/00check.log"; then
    if [ -d "$CHECK_DIR" ]; then
        WARNINGS=$(grep -c "WARNING" "${CHECK_DIR}/00check.log")
        NOTES=$(grep -c "NOTE" "${CHECK_DIR}/00check.log")
        echo "❌ R CMD check found ${WARNINGS} WARNING(s) and ${NOTES} NOTE(s)."
        # Print details from the log
        cat "${CHECK_DIR}/00check.log"
    else
        echo "❌ R CMD check failed to produce a check directory."
    fi
    exit 1
fi

# Run tinytest tests
echo ""
echo "🧪 Running tinytest tests..."
Rscript -e "
library(tinytest)
library(CKutils)

cat('Running all tests...\n')
results <- run_test_dir('tinytest/')

cat('\n=== TEST SUMMARY ===\n')
total_tests <- length(results)
passed_tests <- sum(is.na(results) | results == TRUE)
failed_tests <- sum(!is.na(results) & results == FALSE)

cat('Total tests:', total_tests, '\n')
cat('Passed:', passed_tests, '\n')
cat('Failed:', failed_tests, '\n')

if (failed_tests > 0) {
    cat('\n❌ Some tests failed!\n')
    print(results[!is.na(results) & results == FALSE])
    quit(status = 1)
} else {
    cat('\n✅ All tests passed!\n')
}
"

if [ $? -ne 0 ]; then
    echo "❌ Tests failed"
    exit 1
fi

echo ""
echo "🎉 All checks passed! Ready to push to GitHub."
echo ""

# Clean up auto-generated files
echo "🧹 Cleaning up auto-generated test files..."
rm -f tests/test.R

echo "GitHub Actions will run:"
echo "  - R CMD check on Ubuntu, Windows, and macOS"
echo "  - Full test suite with tinytest"
echo "  - Test coverage analysis"
