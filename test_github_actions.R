#!/usr/bin/env Rscript
# Test script to simulate GitHub Actions workflows

cat("=== Testing GitHub Actions Workflows ===\n")

# Load libraries
library(tinytest)
library(CKutils)

cat("\n1. Testing main test_package workflow...\n")
results <- test_package('CKutils')

cat("\n=== TINYTEST RESULTS ===\n")
print(results)

# Check if any tests failed
if (length(results) == 0) {
  stop('No tests were found!')
}

failed_tests <- results[!is.na(results) & results == FALSE]
if (length(failed_tests) > 0) {
  cat('\n=== FAILED TESTS ===\n')
  print(failed_tests)
  stop('Some tests failed!')
} else {
  cat('\nAll tests passed successfully!\n')
  cat('Total tests run:', length(results), '\n')
}

cat("\n2. Testing specific dt_ops tests...\n")
test_dir <- system.file('tinytest', package='CKutils')
dt_ops_file <- file.path(test_dir, 'test-dt_ops.R')

cat('Test directory:', test_dir, '\n')
cat('dt_ops file path:', dt_ops_file, '\n')
cat('File exists:', file.exists(dt_ops_file), '\n')

if (!file.exists(dt_ops_file)) {
  stop('test-dt_ops.R file not found in installed package!')
}

dt_ops_results <- run_test_file(dt_ops_file)

cat('dt_ops.R tests:', length(dt_ops_results), 'tests run\n')

# Check for failures
if (any(!is.na(dt_ops_results) & dt_ops_results == FALSE)) {
  stop('dt_ops.R tests failed!')
} else {
  cat('All dt_ops.R tests passed!\n')
}

cat("\n=== ALL GITHUB ACTIONS TESTS SUCCESSFUL ===\n")
