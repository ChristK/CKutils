# GitHub Actions CI/CD Setup for CKutils

This document explains the automated testing setup for the CKutils R package.

## Overview

The package uses GitHub Actions to automatically run tests and checks whenever code is pushed to the repository. This ensures code quality and catches issues early.

## Workflows

### 1. R-CMD-check (`R-CMD-check.yml`)
- **Triggers**: Push/PR to main, master, or develop branches
- **Platforms**: Ubuntu, Windows, macOS
- **R versions**: release, devel (on Ubuntu)
- **What it does**: 
  - Runs `R CMD check` (the standard R package check)
  - Installs dependencies automatically
  - Checks package structure, documentation, and examples

### 2. Tests (`tests.yml`)
- **Triggers**: Push/PR to main, master, or develop branches  
- **Platforms**: Ubuntu, Windows, macOS
- **What it does**:
  - Installs the package
  - Runs all tinytest tests
  - Specifically validates dt_ops.R tests (41 tests)
  - Provides detailed test output

### 3. Test Coverage (`test-coverage.yml`)
- **Triggers**: Push/PR to main, master branches
- **Platform**: Ubuntu only
- **What it does**:
  - Measures code coverage using the `covr` package
  - Reports which lines of code are tested

## Status Badges

The README.md includes status badges that show:
- [![R-CMD-check](https://github.com/ChristK/CKutils/workflows/R-CMD-check/badge.svg)](https://github.com/ChristK/CKutils/actions)
- [![Tests](https://github.com/ChristK/CKutils/workflows/Tests/badge.svg)](https://github.com/ChristK/CKutils/actions)

## Local Testing

Before pushing to GitHub, you can run the local test script:

```bash
./check-before-push.sh
```

This script:
1. Installs the package locally
2. Runs R CMD check
3. Runs all tinytest tests
4. Reports any failures

## Current Test Coverage

The package includes **61 total tests** across multiple test files:
- `test-dt_ops.R`: 41 tests (comprehensive coverage of dt_ops.R)
- `test-estim_beta_params.R`: 4 tests
- `test-match_colnames_pattern.R`: 3 tests  
- `test-outersect.R`: 4 tests
- `test-pctl_rank.R`: 2 tests
- `test-resample.R`: 6 tests
- `test.R`: 1 test

## What Happens When You Push

1. **GitHub receives your push**
2. **Three workflows start automatically**:
   - R-CMD-check runs on 4 different OS/R combinations
   - Tests run on 3 different operating systems
   - Coverage analysis runs on Ubuntu
3. **Status badges update** showing pass/fail status
4. **Email notifications** (if configured) for failures
5. **Pull request checks** prevent merging if tests fail

## Benefits

- **Early error detection**: Catch issues before they reach users
- **Cross-platform validation**: Ensure code works on Windows, macOS, Linux
- **Automated quality control**: No manual testing needed
- **Confidence in releases**: Know that all tests pass before publishing
- **Collaboration**: Contributors can see test results immediately

## Troubleshooting

If tests fail on GitHub but pass locally:
1. Check the GitHub Actions logs for specific error messages
2. Ensure all dependencies are listed in DESCRIPTION
3. Consider platform-specific issues (file paths, line endings)
4. Run `./check-before-push.sh` to catch issues early

## Maintenance

The workflows are designed to be low-maintenance:
- Dependencies are automatically resolved
- R versions are kept current by GitHub Actions
- No manual updates needed for most changes
- Workflows will run on every push automatically
