# Tinytest script for functions in R/lookup_dt.R
# load_package(CKutils) # nolint

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


# --- Tests for set_lookup_tbl_key ---

# Test 1: Basic key setting
dt1 <- data.table(a = 1:3, b = factor(letters[1:3])) # Changed b to factor
set_lookup_tbl_key(dt1, c("a", "b"))
expect_identical(key(dt1), c("a", "b"), 
                 info = "set_lookup_tbl_key: Basic key setting")

# Test 2: Key setting with 'year' prioritization
dt2 <- data.table(year = 2020:2022, id = 1:3, val = runif(3))
set_lookup_tbl_key(dt2, c("id", "year"))
expect_identical(key(dt2), c("year", "id"), 
                 info = "set_lookup_tbl_key: 'year' prioritization in key setting")

# Test 3: Error on non-data.table input
expect_error(set_lookup_tbl_key(list(a = 1), "a"),
             info = "set_lookup_tbl_key: Error on non-data.table input")

# Test 4: Error on missing keycols
dt4 <- data.table(a = 1)
expect_error(set_lookup_tbl_key(dt4, NULL),
             info = "set_lookup_tbl_key: Error on missing keycols (NULL)")
expect_error(set_lookup_tbl_key(dt4, character(0)),
             info = "set_lookup_tbl_key: Error on missing keycols (empty character)")

# --- Tests for is_valid_lookup_tbl ---

# Test 5: Valid lookup table
valid_lt <- data.table(id = 1:3, cat = factor(rep(letters[1:2], length.out = 3)))
# To be truly valid for the function's internal checks on expected_rows, it needs all combinations
valid_lt_full <- CJ(id = 1L:2L, cat = factor(letters[1:2])) # Ensure id is integer
valid_lt_full[, val := runif(.N)]
setkeyv(valid_lt_full, c("cat", "id")) # sorted keys
expect_true(is_valid_lookup_tbl(valid_lt_full, keycols = c("id", "cat")),
            info = "is_valid_lookup_tbl: Valid table")

# Test 6: Error on non-data.table input for is_valid_lookup_tbl
expect_error(is_valid_lookup_tbl(list(a = 1), "a"),
             info = "is_valid_lookup_tbl: Error on non-data.table input")

# Test 7: Error on missing keycols for is_valid_lookup_tbl
dt7 <- data.table(a = 1)
expect_error(is_valid_lookup_tbl(dt7, NULL),
             info = "is_valid_lookup_tbl: Error on missing keycols (NULL)")
expect_error(is_valid_lookup_tbl(dt7, character(0)),
             info = "is_valid_lookup_tbl: Error on missing keycols (empty character)")

# Test 8: Error on duplicate keys
dup_keys_lt <- data.table(id = c(1L, 1L, 2L), val = 1:3) # Ensure id is integer
setkey(dup_keys_lt, id)
expect_error(is_valid_lookup_tbl(dup_keys_lt, "id"),
             info = "is_valid_lookup_tbl: Error on duplicate keys")

# Test 9: Error on non-integer/non-factor key column
char_key_lt <- data.table(id = c("a", "b"), val = 1:2)
expect_error(is_valid_lookup_tbl(char_key_lt, "id"),
             info = "is_valid_lookup_tbl: Error on character key column (should be integer/factor)")

# Test 10: Error on non-consecutive integer key
non_cons_lt <- data.table(id = c(1L, 3L), val = 1:2) # Ensure id is integer
setkey(non_cons_lt, id)
expect_error(is_valid_lookup_tbl(non_cons_lt, "id"),
             info = "is_valid_lookup_tbl: Error on non-consecutive integer key")

# Test 11: Error on incorrect number of rows
test11_lt <- CJ(id = 1L:2L, cat = factor(letters[1:2])) # 4 rows
test11_lt_missing_row <- test11_lt[-1,] # 3 rows
setkeyv(test11_lt_missing_row, c("cat", "id")) # Actual key is (cat, id)
expect_error(is_valid_lookup_tbl(test11_lt_missing_row, keycols = c("cat", "id")),
             info = "is_valid_lookup_tbl: Error on incorrect number of rows")


# Test 12: fixkey = TRUE functionality
fixkey_dt <- CJ(id = 1L:2L, cat = factor(letters[1:2]), sorted = FALSE) # Ensure id is integer
fixkey_dt[, val := runif(.N)]
expect_null(key(fixkey_dt), info = "is_valid_lookup_tbl (fixkey): Key is initially NULL")
expect_message(is_valid_lookup_tbl(fixkey_dt, c("id", "cat"), fixkey = TRUE),
               info = "is_valid_lookup_tbl (fixkey): Message about setting key")
expect_identical(key(fixkey_dt), c("cat", "id"),
                 info = "is_valid_lookup_tbl (fixkey): Key is set by fixkey=TRUE")


# --- Tests for lookup_dt ---

# Setup data for lookup_dt tests
main_tbl <- data.table(id = c(1, 2, 1, 3, 2),
                       cat = factor(c("A", "B", "A", "C", "B")),
                       year = c(2020, 2021, 2020, 2022, 2021))

lookup_tbl_vals <- data.table(id = c(1, 2, 3, 1, 2, 3),
                              cat = factor(rep(c("A", "B", "C"), each = 2)),
                              year = rep(c(2020, 2021), times = 3),
                              value1 = 10:15,
                              value2 = letters[1:6])
# Ensure id_key is integer for 'lt'
lt <- CJ(id_key = 1L:2L, factor_key = factor(c("X", "Y")))
lt[, lookup_val := paste0(id_key, factor_key)]
setkeyv(lt, c("factor_key", "id_key"))

tbl_to_lookup <- data.table(id_key = c(1L, 2L, 1L, 2L, 1L),
                            factor_key = factor(c("X", "Y", "X", "Y", "Y")),
                            other_data = runif(5))

# Test 13: Basic merge (merge = TRUE)
tbl_copy13 <- copy(tbl_to_lookup)
result13 <- lookup_dt(tbl_copy13, lt, merge = TRUE)
expect_true(is.data.table(result13), info = "lookup_dt (merge=T): Result is data.table")
expect_true("lookup_val" %in% names(result13), info = "lookup_dt (merge=T): Lookup column added")
expect_identical(result13$lookup_val, c("1X", "2Y", "1X", "2Y", "1Y"),
                 info = "lookup_dt (merge=T): Correct values merged")
expect_identical(result13, tbl_copy13,
                 info = "lookup_dt (merge=T): Modifies tbl by reference")


# Test 14: Return only lookup results (merge = FALSE)
tbl_copy14 <- copy(tbl_to_lookup)
result14 <- lookup_dt(tbl_copy14, lt, merge = FALSE)
expect_true(is.data.table(result14), info = "lookup_dt (merge=F): Result is data.table")
expect_identical(names(result14), "lookup_val",
                 info = "lookup_dt (merge=F): Contains only lookup column")
expect_identical(result14$lookup_val, c("1X", "2Y", "1X", "2Y", "1Y"),
                 info = "lookup_dt (merge=F): Correct values returned")
expect_false("lookup_val" %in% names(tbl_copy14))


# Test 15: exclude_col functionality
# Create a simple lookup table with integer keys and an extra column to exclude
lt_excl <- data.table(
  id_key = rep(1:2, each = 2L),
  factor_key = factor(c("X", "Y", "X", "Y")),
  common_but_exclude = c("ignore1", "ignore2", "ignore1", "ignore2"),
  lookup_val = c("1X", "1Y", "2X", "2Y")
)
setkeyv(lt_excl, c("factor_key", "id_key"))

# Create table to lookup with the same key columns plus the exclude column
tbl_to_lookup_excl <- data.table(id_key = c(1L, 2L, 1L),
                                 factor_key = factor(c("X", "Y", "X")),
                                 common_but_exclude = c("keep1", "keep2", "keep3"))
tbl_copy15 <- copy(tbl_to_lookup_excl)
result15 <- lookup_dt(tbl_copy15, lt_excl, merge = TRUE, exclude_col = "common_but_exclude", check_lookup_tbl_validity = TRUE)
expect_true("lookup_val" %in% names(result15),
            info = "lookup_dt (exclude_col): Lookup column added")
expect_identical(result15$lookup_val, c("1X", "2Y", "1X"),
                 info = "lookup_dt (exclude_col): Correct values merged with exclude_col")

# Test 16: check_lookup_tbl_validity = TRUE with a valid table
tbl_copy16 <- copy(tbl_to_lookup)
result16 <- lookup_dt(tbl_copy16, lt, merge = TRUE, check_lookup_tbl_validity = TRUE)
expect_identical(result16$lookup_val, c("1X", "2Y", "1X", "2Y", "1Y"),
                 info = "lookup_dt (check_valid=T, valid_lt): Correct merge with valid lookup")

# Test 17: check_lookup_tbl_validity = TRUE with an invalid table (non-consecutive int key)
# Ensure id_key is integer here too, so the type check passes and non-consecutive check is hit
invalid_lt_noncons <- data.table(id_key = c(1L, 3L), factor_key = factor(c("X", "Y")))
invalid_lt_noncons[, lookup_val := paste0(id_key, factor_key)]
setkeyv(invalid_lt_noncons, c("factor_key", "id_key"))
tbl_copy17 <- copy(tbl_to_lookup)
expect_error(lookup_dt(tbl_copy17, invalid_lt_noncons, merge = TRUE, check_lookup_tbl_validity = TRUE),
             info = "lookup_dt (check_valid=T, invalid_lt): Error with invalid lookup table (non-consecutive int)")

# Test 18: Error on tbl not a data.table
expect_error(lookup_dt(list(), lt),
             info = "lookup_dt: Error if tbl is not data.table")

# Test 19: Error on lookup_tbl not a data.table
expect_error(lookup_dt(tbl_to_lookup, list()),
             info = "lookup_dt: Error if lookup_tbl is not data.table")

# Test 20: Error on no common keys
no_common_lt <- data.table(z_key = 1:2, val = "a")
setkey(no_common_lt, z_key)
expect_error(lookup_dt(tbl_to_lookup, no_common_lt),
             info = "lookup_dt: Error on no common keys")

# Test 21: Error on no value columns in lookup_tbl
no_val_cols_lt <- data.table(id_key = 1:2, factor_key = factor(c("X", "Y")))
setkeyv(no_val_cols_lt, c("factor_key", "id_key"))
expect_error(lookup_dt(tbl_to_lookup, no_val_cols_lt),
             info = "lookup_dt: Error on no value columns in lookup_tbl")


# Test 22: Different factor levels with check_lookup_tbl_validity = TRUE
tbl_diff_levels <- data.table(id_key = 1L, factor_key = factor("Z", levels = c("X", "Y", "Z"))) # Ensure id_key is integer
lt_levels <- data.table(id_key = 1L, factor_key = factor("X", levels = c("X", "Y")), val = "v") # Ensure id_key is integer
setkeyv(lt_levels, c("factor_key", "id_key"))
expect_error(lookup_dt(tbl_diff_levels, lt_levels, check_lookup_tbl_validity = TRUE),
             info = "lookup_dt (check_valid=T): Error on different factor levels")

# Test 23: Values in tbl outside range of lookup_tbl for integer keys (check_lookup_tbl_validity = TRUE)
tbl_outside_range <- data.table(id_key = c(1L, 2L, 3L), factor_key = factor("X"))
# Ensure id_key is integer for 'lt_int_range'
lt_int_range <- CJ(id_key = 1:2, factor_key = factor("X"))
lt_int_range[, lookup_val := paste0(2 * id_key, factor_key)]
setkeyv(lt_int_range, c("factor_key", "id_key"))

expect_message(lookup_dt(tbl_outside_range, lt_int_range, check_lookup_tbl_validity = TRUE, merge = TRUE),
               info = "lookup_dt (check_valid=T): Message for tbl values outside lookup_tbl integer key range")

result23 <- lookup_dt(copy(tbl_outside_range), lt_int_range, check_lookup_tbl_validity = FALSE, merge = TRUE, exclude_col = "lookup_val")
expect_identical(result23$lookup_val, c("2X", "4X", NA_character_),
                 info = "lookup_dt: NA for values outside integer key range (no check)")


# Test 24: More complex lookup with multiple key columns (integer, factor, year-like)
main_complex <- data.table(
  student_id = c(101L, 102L, 101L, 103L, 102L),
  course_code = factor(c("CS101", "MA202", "CS101", "PH303", "MA202")),
  term_year = c(2023L, 2023L, 2023L, 2024L, 2024L)
)

lookup_complex <- CJ(
  student_id = c(101L, 102L, 103L),
  course_code = factor(c("CS101", "MA202", "PH303")),
  term_year = c(2023L, 2024L)
)
lookup_complex[, grade := sample(c("A", "B", "C", "D"), .N, replace = TRUE)]
setkeyv(lookup_complex, c("course_code", "student_id", "term_year"))

main_copy24 <- copy(main_complex)
result24 <- lookup_dt(main_copy24, lookup_complex, merge = TRUE)

# Simplified subsetting for val1, val2, val4, val5
val1 <- lookup_complex[student_id == 101L & course_code == "CS101" & term_year == 2023L]$grade
val2 <- lookup_complex[student_id == 102L & course_code == "MA202" & term_year == 2023L]$grade
if (length(val1) > 0) val3 <- val1 else val3 <- NA_character_
val4 <- lookup_complex[student_id == 103L & course_code == "PH303" & term_year == 2024L]$grade
val5 <- lookup_complex[student_id == 102L & course_code == "MA202" & term_year == 2024L]$grade

expected_grades <- c(val1, val2, val3, val4, val5)

if (length(result24$grade) == length(expected_grades)) {
    expect_identical(result24$grade, expected_grades,
                     info = "lookup_dt: Complex lookup with multiple keys, correct values")
} else {
    # Use expect_true for proper test failure that tinytest can catch
    expect_true(FALSE, 
        info = paste("lookup_dt: Complex lookup - length mismatch. Got", 
                     length(result24$grade), "Expected", length(expected_grades)))
}


# Test 25: Lookup with no matches in tbl for some rows (should produce NAs)
tbl_some_nomatch2 <- data.table(id_key = c(1L, 2L, 1L, 3L),
                               factor_key = factor(c("X", "Y", "Y", "X")))
expect_message(result25 <- lookup_dt(copy(tbl_some_nomatch2), lt, merge = TRUE))
expect_identical(result25$lookup_val, c("1X", "2Y", "1Y", NA_character_),
                 info = "lookup_dt: NA for rows in tbl with no match in lookup_tbl (integer key out of range)")

# Test 26: Empty tbl
empty_tbl <- tbl_to_lookup[0, ]
expect_warning(result26_merge_true <- lookup_dt(copy(empty_tbl), lt, merge = TRUE))
expect_identical(nrow(result26_merge_true), 0L, info = "lookup_dt (empty tbl, merge=T): 0 rows")
expect_true("lookup_val" %in% names(result26_merge_true), info = "lookup_dt (empty tbl, merge=T): lookup col present")

expect_warning(result26_merge_false <- lookup_dt(empty_tbl, lt, merge = FALSE))
expect_identical(nrow(result26_merge_false), 0L, info = "lookup_dt (empty tbl, merge=F): 0 rows")
expect_identical(names(result26_merge_false), "lookup_val", info = "lookup_dt (empty tbl, merge=F): lookup col present")


# Test 27: Empty lookup_tbl (integer key) - should fail validation
lt_int_empty <- data.table(id_key = integer(0), lookup_val = character(0))
setkey(lt_int_empty, id_key)
tbl_for_empty_lt <- data.table(id_key = c(1L, 2L))
expect_error(lookup_dt(copy(tbl_for_empty_lt), lt_int_empty, merge = TRUE),
             info = "lookup_dt: Empty lookup_tbl fails validation (integer key)")

# Test 27b: Empty lookup_tbl (factor key) - should fail validation
lt_factor_empty <- data.table(factor_key = factor(levels=c("X","Y")), lookup_val = character(0))
setkey(lt_factor_empty, factor_key)
tbl_for_empty_factor_lt <- data.table(factor_key = factor(c("X")))
expect_error(lookup_dt(copy(tbl_for_empty_factor_lt), lt_factor_empty, merge = TRUE),
             info = "lookup_dt: Empty lookup_tbl fails validation (factor key)")

# Test 29: Examples from documentation
main_dt_ex1 <- data.table(id = 1:5, category = factor(c("A", "B", "A", "C", "B")))
lookup_values_ex1 <- data.table(category = factor(c("A", "B", "C")), value = c(10, 20, 30))
expect_message(result_ex1 <- lookup_dt(copy(main_dt_ex1), lookup_values_ex1, merge = TRUE))
expected_ex1 <- data.table(id = 1:5, category = factor(c("A", "B", "A", "C", "B")), value = c(10, 20, 10, 30, 20))
expect_identical(result_ex1, expected_ex1, info = "lookup_dt: Example 1 from documentation")

sales_data_ex3 <- data.table(
  region = c("North", "South", "North"),
  item = factor(c("apple", "banana", "apple"), levels = c("apple", "banana")),
  sales_rep_id = c(1, 2, 1)
)
item_details_ex3 <- data.table(
  item = factor(c("apple", "banana"), levels = c("apple", "banana")), 
  category = c("fruit", "fruit"),
  supplier_id = c(10, 20),
  sales_rep_id = c(99, 99)
)
result_ex3 <- lookup_dt(
  copy(sales_data_ex3),
  item_details_ex3,
  exclude_col = "sales_rep_id",
  merge = TRUE,
  check_lookup_tbl_validity = FALSE
)
expected_ex3 <- data.table(region = c("North", "South", "North"),
                           item = factor(c("apple", "banana", "apple"), levels = c("apple", "banana")),
                           sales_rep_id = c(99, 99, 99),
                           category = c("fruit", "fruit", "fruit"),
                           supplier_id = c(10, 20, 10))
setcolorder(result_ex3, names(expected_ex3))
expect_identical(result_ex3, expected_ex3, info = "lookup_dt: Example 3 (exclude_col) from documentation")


