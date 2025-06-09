#!/bin/bash
# Local test script to simulate what GitHub Actions will run
# Run this before pushing to catch issues early

echo "=== CKutils Local Test Runner ==="
echo "This simulates what GitHub Actions will run"
echo ""

# Check if we're in the right directory
if [ ! -f "DESCRIPTION" ]; then
    echo "Error: Please run this script from the CKutils package root directory"
    exit 1
fi

# Install the package
echo "📦 Installing package..."
R CMD INSTALL . --no-multiarch --with-keep.source

if [ $? -ne 0 ]; then
    echo "❌ Package installation failed"
    exit 1
fi

# Run R CMD check
echo ""
echo "🔍 Running R CMD check..."
R CMD check . --no-manual --as-cran

if [ $? -ne 0 ]; then
    echo "❌ R CMD check failed"
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
echo "GitHub Actions will run:"
echo "  - R CMD check on Ubuntu, Windows, and macOS"
echo "  - Full test suite with tinytest"
echo "  - Test coverage analysis"
