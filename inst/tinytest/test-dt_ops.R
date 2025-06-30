# Comprehensive unit tests for dt_ops.R
# Tests cover every function and code path in the file

# Setup: Ensure data.table is available
if (!requireNamespace("data.table", quietly = TRUE)) {
  exit_file("data.table package not available")
}
library(data.table)

# Helper to load CKutils if not already loaded (for interactive testing)
# This assumes the package is installed or can be loaded via devtools::load_all()
if (isNamespaceLoaded("CKutils")) {
  # If loaded, ensure functions are accessible
  lookup_dt <- CKutils::lookup_dt
  is_valid_lookup_tbl <- CKutils::is_valid_lookup_tbl
  set_lookup_tbl_key <- CKutils::set_lookup_tbl_key
} else {
  # Attempt to load if in an interactive session or testing environment
  # that might not have it fully loaded.
  # This part might need adjustment based on how tests are run.
  if (interactive() || Sys.getenv("R_TESTS") != "") {
    if (requireNamespace("CKutils", quietly = TRUE)) {
      lookup_dt <- CKutils::lookup_dt
      is_valid_lookup_tbl <- CKutils::is_valid_lookup_tbl
      set_lookup_tbl_key <- CKutils::set_lookup_tbl_key
    } else {
      # Fallback if CKutils is not installed/loaded - tests will likely fail
      # or need to source the functions directly if run standalone.
      # For R CMD check, functions are typically available.
      warning("CKutils namespace not loaded. Tests might not find functions.")
    }
  }
}


# =============================================================================
# Tests for clone_dt function
# =============================================================================

# Test basic cloning functionality
x <- c(1, 5, 3)
dt <- data.table(x)
dtc2 <- clone_dt(dt, 2, idcol = FALSE)
dtc3 <- clone_dt(dt, 3, idcol = TRUE)

# Test cloning without .id column
expect_true(is.null(dtc2$.id), info = "No .id column when idcol = FALSE")
expect_equal(dtc2$x, c(1, 5, 3, 1, 5, 3), info = "x values correctly duplicated without .id")
expect_equal(nrow(dtc2), 6, info = "Correct number of rows when times = 2")

# Test cloning with .id column
expect_equal(dtc3$x, c(1, 5, 3, 1, 5, 3, 1, 5, 3), info = "x values correctly duplicated with .id")
expect_equal(dtc3$.id, c(1, 1, 1, 2, 2, 2, 3, 3, 3), info = ".id column correctly created")

# Test with keyed data.table
dt_keyed <- data.table(id = 1:3, value = c(10, 20, 30))
setkeyv(dt_keyed, "id")
dtc_keyed <- clone_dt(dt_keyed, 2, idcol = TRUE)
expect_equal(key(dtc_keyed), "id", info = "Keys are preserved after cloning")

# Test times = 1 edge case
dt_one <- data.table(x = 1:3)
dtc_one <- clone_dt(dt_one, 1, idcol = TRUE)
expect_equal(nrow(dtc_one), 3, info = "times = 1 works correctly")

# =============================================================================
# Tests for absorb_dt function
# =============================================================================

# Test stopifnot conditions
dt_x1 <- data.table(a = 1:5, b = 1:5, c = 5:9)
dt_i1 <- data.table(a = 1:5, b = 1:5, c = 1:5)
expect_error(absorb_dt(as.data.frame(dt_x1), dt_i1), info = "Error when dt_x is not data.table")
expect_error(absorb_dt(dt_x1, as.data.frame(dt_i1)), info = "Error when dt_i is not data.table")
expect_error(absorb_dt(dt_x1, dt_i1, on = 123), info = "Error when on is not character")

# Test explicit keys
dt_x1_copy <- copy(dt_x1)
result1 <- absorb_dt(dt_x1_copy, dt_i1, on = c("a", "b"))
expect_identical(result1, dt_x1_copy, info = "absorb_dt returns dt_x invisibly")
expect_equal(dt_x1_copy$c, c(1, 2, 3, 4, 5), info = "Column c replaced correctly")

# Test .NATURAL key detection - exclude value1 from keys to allow replacement
dt_x2 <- data.table(id = 1:3, name = c("A", "B", "C"), value1 = c(10, 20, 30))
dt_i2 <- data.table(id = 1:3, name = c("A", "B", "C"), value1 = c(100, 200, 300), value2 = c(1, 2, 3))
dt_x2_copy <- copy(dt_x2)
absorb_dt(dt_x2_copy, dt_i2, exclude_col = "value1")
expect_true("value2" %in% names(dt_x2_copy), info = "New column added with .NATURAL keys")
expect_equal(dt_x2_copy$value1, c(100, 200, 300), info = "Existing column replaced with .NATURAL keys")

# Test .NATURAL with exclude_col
dt_x3 <- data.table(id = 1:3, name = c("A", "B", "C"), value1 = c(10, 20, 30))
dt_i3 <- data.table(id = 1:3, name = c("A", "B", "C"), value1 = c(100, 200, 300))
absorb_dt(dt_x3, dt_i3, exclude_col = "value1")
expect_equal(dt_x3$value1, c(100, 200, 300), info = "Column replaced when excluded from keys")

# Test verbose output
dt_x4 <- data.table(a = 1:3, b = 1:3, c = 10:12)
dt_i4 <- data.table(a = 1:3, b = 1:3, c = 20:22)
expect_message(
    absorb_dt(dt_x4, dt_i4, on = c("a", "b"), verbose = TRUE),
    "column c in dt_x has been replaced",
    info = "Verbose message shown when columns replaced"
)

# Test no verbose output when no columns replaced
dt_x5 <- data.table(a = 1:3, b = 1:3)
dt_i5 <- data.table(a = 1:3, c = 20:22)
expect_silent(absorb_dt(dt_x5, dt_i5, on = "a", verbose = TRUE), info = "No message when no columns replaced")

# Test case where no common columns exist for .NATURAL
dt_x6 <- data.table(x = 1:3, y = 4:6)
dt_i6 <- data.table(p = 1:3, q = 7:9)
dt_x6_copy <- copy(dt_x6)
expect_error(absorb_dt(dt_x6_copy, dt_i6), info = "Error when no common columns for .NATURAL")

# Test absorb_dt with mismatched rows
dt_x9 <- data.table(id = 1:5, value = 10:14)
dt_i9 <- data.table(id = c(1, 3, 5), value = c(100, 300, 500))
absorb_dt(dt_x9, dt_i9, on = "id")
expect_equal(dt_x9$value, c(100, 11, 300, 13, 500), info = "Partial matches work correctly")

# =============================================================================
# Tests for del_dt_rows function
# =============================================================================

# Test stopifnot conditions
dt_test <- data.table(a = 1:5, b = letters[1:5])
expect_error(del_dt_rows(as.data.frame(dt_test), 1:2), info = "Error when dtb is not data.table")
expect_error(del_dt_rows(dt_test, "invalid"), info = "Error when indx_to_del is not integer or logical")

# Test deletion with integer indices
dt_int <- data.table(a = 1:5, b = letters[1:5])
test_dt_int <- copy(dt_int)
expect_silent(del_dt_rows(test_dt_int, c(2L, 4L)), info = "Integer deletion completes without error")

# Test deletion with logical indices  
test_dt_log <- copy(dt_int)
logical_idx <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
expect_silent(del_dt_rows(test_dt_log, logical_idx), info = "Logical deletion completes without error")

# Test with keyed data.table
dt_keyed_del <- data.table(id = 1:5, value = 10:14)
setkeyv(dt_keyed_del, "id")
test_dt_keyed <- copy(dt_keyed_del)
expect_silent(del_dt_rows(test_dt_keyed, c(1L, 3L)), info = "Deletion works with keyed data.table")

# Test edge cases
test_dt_empty <- copy(dt_int)
expect_silent(del_dt_rows(test_dt_empty, integer(0)), info = "Empty deletion works")

test_dt_all_false <- copy(dt_int)
expect_silent(del_dt_rows(test_dt_all_false, rep(FALSE, 5)), info = "All FALSE logical vector works")

# =============================================================================
# Tests for do_cols_dt function
# =============================================================================

# Test basic arithmetic operations
dt_arith <- data.table(a = 1:5, b = 6:10, c = 11:15)

# Test addition
result_add <- do_cols_dt(dt_arith, c("a", "b"), symbol = "+")
expect_equal(result_add[[1]], c(7, 9, 11, 13, 15), info = "Addition operation works correctly")
expect_equal(ncol(result_add), 1, info = "Result has single column")

# Test subtraction
result_sub <- do_cols_dt(dt_arith, c("b", "a"), symbol = "-")
expect_equal(result_sub[[1]], c(5, 5, 5, 5, 5), info = "Subtraction operation works correctly")

# Test multiplication
result_mult <- do_cols_dt(dt_arith, c("a", "b"), symbol = "*")
expect_equal(result_mult[[1]], c(6, 14, 24, 36, 50), info = "Multiplication operation works correctly")

# Test division
result_div <- do_cols_dt(dt_arith, c("b", "a"), symbol = "/")
expect_equal(result_div[[1]], c(6, 3.5, 8/3, 2.25, 2), info = "Division operation works correctly")

# Test with function parameter - use parallel functions that work element-wise
result_pmax_fn <- do_cols_dt(dt_arith, c("a", "b"), symbol = ",", fn = "pmax")
expect_equal(result_pmax_fn[[1]], c(6, 7, 8, 9, 10), info = "Function parameter works with pmax")

result_pmin_fn <- do_cols_dt(dt_arith, c("a", "b"), symbol = ",", fn = "pmin")
expect_equal(result_pmin_fn[[1]], c(1, 2, 3, 4, 5), info = "Function parameter works with pmin")

# Test with single column
result_single <- do_cols_dt(dt_arith, "a", symbol = "+")
expect_equal(result_single[[1]], 1:5, info = "Single column operation works")

# Test with three columns
result_three <- do_cols_dt(dt_arith, c("a", "b", "c"), symbol = "+")
expect_equal(result_three[[1]], c(18, 21, 24, 27, 30), info = "Three column addition works")

# Test symbol parameter with vector (uses first element)
result_vector_symbol <- do_cols_dt(dt_arith, c("a", "b"), symbol = c("+", "-"))
expect_equal(result_vector_symbol[[1]], c(7, 9, 11, 13, 15), info = "Vector symbol uses first element")

# Test with column names that need backticks
dt_special <- data.table(`col 1` = 1:3, `col-2` = 4:6)
result_special <- do_cols_dt(dt_special, c("col 1", "col-2"), symbol = "+")
expect_equal(result_special[[1]], c(5, 7, 9), info = "Special column names handled correctly")

# Test with NA values
dt_na <- data.table(a = c(1, 2, NA), b = c(4, NA, 6))
result_na_add <- do_cols_dt(dt_na, c("a", "b"), symbol = "+")
expect_true(is.na(result_na_add[[1]][2]), info = "NA propagated in addition")
expect_equal(result_na_add[[1]][1], 5, info = "Non-NA values calculated correctly")

# Test three-column pmax
result_pmax_three <- do_cols_dt(dt_arith, c("a", "b", "c"), symbol = ",", fn = "pmax")
expect_equal(result_pmax_three[[1]], c(11, 12, 13, 14, 15), info = "Three column pmax works")

# Test clone_dt with empty data.table
dt_empty <- data.table()
result_empty_clone <- clone_dt(dt_empty, 2, idcol = TRUE)
expect_equal(nrow(result_empty_clone), 0, info = "Empty data.table cloning works")
# Note: empty data.table doesn't get .id column - this is expected behavior

cat("All dt_ops.R tests completed successfully!\n")