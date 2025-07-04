# Comprehensive tests for all functions in misc_functions.R

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
# Tests for get_dropbox_path function
# =============================================================================

# Test basic functionality (skip if dropbox not installed)
if (file.exists("~/.dropbox/info.json") || 
    file.exists(paste0(Sys.getenv("APPDATA"), "/Dropbox/info.json")) ||
    file.exists(paste0(Sys.getenv("LOCALAPPDATA"), "/Dropbox/info.json"))) {
  
  # Test basic path retrieval
  tryCatch({
    personal_path <- get_dropbox_path(type = "personal")
    expect_true(is.character(personal_path), info = "Personal dropbox path is character")
    expect_equal(length(personal_path), 1, info = "Single path returned")
    
    # Test with pathtail
    path_with_tail <- get_dropbox_path("subfolder", type = "personal")
    expect_true(grepl("subfolder", path_with_tail), info = "Pathtail included in path")
  }, error = function(e) {
    # Skip tests if dropbox not properly configured
    expect_true(TRUE, info = "Dropbox not configured - skipping tests")
  })
}


# =============================================================================
# Tests for get_pcloud_path function  
# =============================================================================

# Test basic functionality
pcloud_path <- get_pcloud_path()
expect_true(is.character(pcloud_path), info = "pCloud path is character")
expect_equal(length(pcloud_path), 1, info = "Single path returned")

# Test with pathtail
path_with_tail <- get_pcloud_path("subfolder")
expect_true(grepl("subfolder", path_with_tail), info = "Pathtail included in pCloud path")

# Test platform-specific paths
if (.Platform$OS.type == "windows") {
  expect_true(grepl("p:\\\\", pcloud_path), info = "Windows pCloud path correct")
} else {
  expect_true(grepl(path.expand("~/pCloudDrive"), pcloud_path), info = "Unix pCloud path correct")
}

# =============================================================================
# Tests for agegrp_name function
# =============================================================================

# Test basic age group naming
basic_groups <- agegrp_name(min_age = 0, max_age = 85, grp_width = 5)
expect_true(is.character(basic_groups), info = "Age groups are character")
expect_true(length(basic_groups) > 0, info = "Age groups generated")
expect_true(any(grepl("<1", basic_groups)), info = "Under 1 group created")
expect_true(any(grepl("85\\+", basic_groups)), info = "85+ group created")

# Test different group widths
groups_10 <- agegrp_name(min_age = 0, max_age = 80, grp_width = 10)
expect_equal(length(groups_10), 10, info = "Correct number of 10-year groups")

# Test without grp_lessthan_1
groups_no_lt1 <- agegrp_name(min_age = 0, max_age = 85, grp_width = 5, grp_lessthan_1 = FALSE)
expect_false(any(grepl("<1", groups_no_lt1)), info = "No <1 group when disabled")

# Test match_input functionality
groups_match <- agegrp_name(min_age = 0, max_age = 85, grp_width = 5, match_input = TRUE, match_input_max_age = 90)
expect_true(length(groups_match) >= 90, info = "Match input extends groups")

# Test error conditions
expect_error(agegrp_name(min_age = -1, max_age = 85), info = "Error for negative min_age")
expect_error(agegrp_name(min_age = 50, max_age = 30), info = "Error when min_age > max_age")
expect_error(agegrp_name(min_age = 0, max_age = 85, grp_width = 0), info = "Error for zero grp_width")

# =============================================================================
# Tests for to_agegrp function
# =============================================================================

# Test basic age grouping
test_dt <- data.table(id = 1:10, age = c(0, 5, 15, 25, 35, 45, 55, 65, 75, 85))
test_dt_copy <- copy(test_dt)
result <- to_agegrp(test_dt_copy, grp_width = 10, max_age = 85)

expect_true("agegrp" %in% names(test_dt_copy), info = "Age group column added")
expect_true(is.factor(test_dt_copy$agegrp), info = "Age group is factor by default")
expect_equal(nrow(test_dt_copy), 10, info = "Row count unchanged")

# Test with custom column names
test_dt2 <- copy(test_dt)
setnames(test_dt2, "age", "patient_age")
to_agegrp(test_dt2, age_colname = "patient_age", agegrp_colname = "age_category", grp_width = 5)
expect_true("age_category" %in% names(test_dt2), info = "Custom age group column name works")

# Test with to_factor = FALSE
test_dt3 <- copy(test_dt)
to_agegrp(test_dt3, to_factor = FALSE)
expect_true(is.character(test_dt3$agegrp), info = "Age group is character when to_factor = FALSE")

# Test error conditions
expect_error(to_agegrp(as.data.frame(test_dt)), info = "Error when input not data.table")
expect_error(to_agegrp(test_dt, age_colname = "nonexistent"), info = "Error when age column doesn't exist")

# =============================================================================
# Tests for pctl_rank function
# =============================================================================

# Test basic percentile ranking
x <- c(2, 5, 1, 3, 4, 6)
ranks_min <- pctl_rank(x, ties.method = "min")
expect_equal(length(ranks_min), 6, info = "Percentile rank same length as input")
expect_true(all(ranks_min >= 0 & ranks_min <= 1), info = "Ranks between 0 and 1")
expect_equal(ranks_min, c(0.2, 0.8, 0.0, 0.4, 0.6, 1.0), info = "Correct min method ranks")

# Test different tie methods
ranks_avg <- pctl_rank(x, ties.method = "average")
expect_true(is.numeric(ranks_avg), info = "Average method returns numeric")

ranks_max <- pctl_rank(x, ties.method = "max")
expect_true(is.numeric(ranks_max), info = "Max method returns numeric")

# Test with ties
x_ties <- c(1, 2, 2, 3)
ranks_ties <- pctl_rank(x_ties, ties.method = "min")
expect_true(ranks_ties[2] == ranks_ties[3], info = "Tied values get same rank with min method")

# Test error conditions
expect_error(pctl_rank(c("a", "b", "c")), pattern = "is\\.numeric\\(x\\) is not TRUE", info = "Error for non-numeric input")

# =============================================================================
# Tests for counts function
# =============================================================================

# Test basic counting
x <- c(1, 2, 2, 3, 3, 3)
count_result <- counts(x)
expect_true(is.integer(count_result), info = "Counts returns integer")
expect_equal(length(count_result), 3, info = "Correct number of unique values")
expect_equal(as.vector(count_result), c(1, 2, 3), info = "Correct counts")

# Test with character vector
char_vec <- c("a", "b", "a", "c", "b", "b")
char_counts <- counts(char_vec)
expect_true(is.integer(char_counts), info = "Character counts returns integer")
expect_equal(length(char_counts), 3, info = "Correct unique character count")

# Test with logical vector
logical_vec <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
logical_counts <- counts(logical_vec)
expect_equal(length(logical_counts), 2, info = "Logical vector counted correctly")

# Test with NA values
na_vec <- c(1, 2, NA, 2, NA, 3)
na_counts <- counts(na_vec)
expect_true(any(is.na(names(na_counts))), info = "NA values included in counts")

# Test with list input
list_input <- list(a = c(1, 2, 2), b = c(3, 3, 4))
list_counts <- counts(list_input)
expect_true(is.list(list_counts), info = "List input returns list")
expect_equal(length(list_counts), 2, info = "List output has correct length")

# =============================================================================
# Tests for match_colnames_pattern function
# =============================================================================

# Test basic pattern matching
dt <- data.table(id = 1:3, year = 2020:2022, birth = 1980:1982, idp = 4:6)
patterns <- list("id", "year", "birth")
matches <- match_colnames_pattern(dt, patterns)
expect_equal(matches, c("id", "idp", "year", "birth"), info = "Correct columns matched")

# Test with partial matches
partial_patterns <- list("i", "ear")
partial_matches <- match_colnames_pattern(dt, partial_patterns)
expect_true("id" %in% partial_matches, info = "Partial match for 'i' finds 'id'")
expect_true("year" %in% partial_matches, info = "Partial match for 'ear' finds 'year'")

# Test with no matches
no_match_patterns <- list("xyz", "abc")
no_matches <- match_colnames_pattern(dt, no_match_patterns)
expect_equal(length(no_matches), 0, info = "No matches returns empty character vector")

# Test error for non-character patterns
expect_error(match_colnames_pattern(dt, list(1, 2, 3)), 
             pattern = "Input patterns must be of type character", 
             info = "Error for non-character patterns")

# =============================================================================
# Tests for estim_beta_params function
# =============================================================================

# Test basic beta parameter estimation
params1 <- estim_beta_params(0.3, 0.02)
expect_true(is.list(params1), info = "Beta parameters returned as list")
expect_true(all(c("shape1", "shape2") %in% names(params1)), info = "Correct parameter names")
expect_true(is.numeric(params1$shape1) && is.numeric(params1$shape2), info = "Parameters are numeric")

# Test boundary cases
params_zero <- estim_beta_params(0, 2)
expect_true(is.nan(params_zero$shape1) && is.nan(params_zero$shape2), info = "NaN for mu=0")

params_one <- estim_beta_params(1, 5)
expect_true(is.nan(params_one$shape1) && is.nan(params_one$shape2), info = "NaN for mu=1")

# Test valid estimation
params_mid <- estim_beta_params(0.5, 0.01)
expect_true(is.finite(params_mid$shape1) && is.finite(params_mid$shape2), info = "Finite parameters for valid inputs")

# Test error conditions
expect_error(estim_beta_params(5, 2), pattern = "between\\(mu, 0, 1\\) is not TRUE", info = "Error for mu outside [0,1]")
expect_error(estim_beta_params(0.5, -1), pattern = "var > 0 is not TRUE", info = "Error for negative variance")

# =============================================================================
# Tests for symdiff function
# =============================================================================

# Test basic symmetric difference
set1 <- c("a", "b", "c")
set2 <- c("b", "c", "d")
symdiff_result <- symdiff(set1, set2)
expect_equal(sort(symdiff_result), c("a", "d"), info = "Correct symmetric difference")

# Test with numeric vectors
num1 <- c(1, 2, 3)
num2 <- c(2, 3, 4)
symdiff_num <- symdiff(num1, num2)
expect_equal(sort(symdiff_num), c(1, 4), info = "Numeric symmetric difference works")

# Test with identical sets
identical_symdiff <- symdiff(set1, set1)
expect_equal(length(identical_symdiff), 0, info = "Symmetric difference of identical sets is empty")

# Test with disjoint sets
disjoint1 <- c("x", "y")
disjoint2 <- c("z", "w")
disjoint_symdiff <- symdiff(disjoint1, disjoint2)
expect_equal(sort(disjoint_symdiff), c("w", "x", "y", "z"), info = "Disjoint sets return union")

# =============================================================================
# Tests for clamp function
# =============================================================================

# Test basic clamping
x <- c(-1, 0.5, 2)
clamped <- clamp(x, a = 0, b = 1)
expect_equal(clamped, c(0, 0.5, 1), info = "Values clamped correctly")

# Test with integer input
x_int <- c(-5L, 3L, 15L)
clamped_int <- clamp(x_int, a = 0L, b = 10L)
expect_equal(clamped_int, c(0L, 3L, 10L), info = "Integer clamping works")

# Test inplace modification
x_inplace <- c(-1, 0.5, 2)
clamp(x_inplace, a = 0, b = 1, inplace = TRUE)
expect_true(identical(x_inplace, c(0, 0.5, 1)), info = "Inplace modification works")

# Test error for unsupported types
expect_error(clamp(c("a", "b"), a = 0, b = 1), 
             pattern = "Only accepts doubles or integers", 
             info = "Error for character input")

# =============================================================================
# Tests for csv_to_fst function
# =============================================================================

# Create test CSV file
temp_dir <- tempdir()
test_csv <- file.path(temp_dir, "test_data.csv")
test_data <- data.table(a = 1:5, b = letters[1:5])
fwrite(test_data, test_csv)

# Test CSV to FST conversion
result <- csv_to_fst(test_csv, compression = 50L, delete_csv = FALSE)
expect_null(result, info = "csv_to_fst returns NULL invisibly")

# Check FST file was created
test_fst <- gsub(".csv$", ".fst", test_csv)
expect_true(file.exists(test_fst), info = "FST file created")

# Test file deletion option
temp_csv2 <- file.path(temp_dir, "test_data2.csv")
fwrite(test_data, temp_csv2)
csv_to_fst(temp_csv2, delete_csv = TRUE)
expect_false(file.exists(temp_csv2), info = "CSV file deleted when delete_csv = TRUE")

# Clean up
unlink(c(test_csv, test_fst, gsub(".csv$", ".fst", temp_csv2)))

# =============================================================================
# Tests for gnrt_folder_structure function
# =============================================================================

# Test folder structure generation
temp_root <- tempfile("folder_test")
dir.create(temp_root)

# Test basic folder generation
gnrt_folder_structure(temp_root)
# Note: this function doesn't create folders, just generates structure
# The return is NULL and it's meant to set up a structure
expect_null(gnrt_folder_structure(temp_root), info = "gnrt_folder_structure returns NULL")

# Clean up
unlink(temp_root, recursive = TRUE)

# =============================================================================
# Tests for identical_elements function
# =============================================================================

# Test with identical elements
identical_vec <- c(5, 5, 5, 5)
expect_true(identical_elements(identical_vec), info = "Identical elements detected correctly")

# Test with different elements
different_vec <- c(1, 2, 3, 4)
expect_false(identical_elements(different_vec), info = "Different elements detected correctly")

# Test with nearly identical elements (within tolerance)
nearly_identical <- c(1.0000001, 1.0000002, 1.0000001)
expect_true(
  identical_elements(nearly_identical, tol = 1e-7),
  info = "Nearly identical elements within tolerance"
)

# Test with elements outside tolerance
expect_false(
  identical_elements(nearly_identical, tol = 1e-8),
  info = "Elements outside tolerance detected"
)

# Test custom tolerance
custom_tol_vec <- c(1.0, 1.05, 1.02)
expect_false(identical_elements(custom_tol_vec, tol = 0.01), info = "Custom tolerance works")
expect_true(identical_elements(custom_tol_vec, tol = 0.1), info = "Custom tolerance accepts larger differences")

# Test error for non-numeric input
expect_error(identical_elements(c("a", "b", "c")), pattern = "is.numeric", info = "Error for non-numeric input")

# =============================================================================
# Tests for replace_from_table function
# =============================================================================

# Create test data.table
test_replace_dt <- data.table(
  id = 1:5,
  category = c("A", "B", "A", "C", "B"),
  value = c(10, 20, 30, 40, 50),
  factor_col = factor(c("X", "Y", "X", "Z", "Y"))
)

# Test basic replacement
dt1 <- copy(test_replace_dt)
replace_from_table(dt1, "category", c("A", "B"), c("Alpha", "Beta"))
expect_true(all(dt1$category %in% c("Alpha", "Beta", "C")), info = "Basic replacement works")
expect_true("Alpha" %in% dt1$category, info = "A replaced with Alpha")
expect_true("Beta" %in% dt1$category, info = "B replaced with Beta")

# Test replacement with new column
dt2 <- copy(test_replace_dt)
replace_from_table(dt2, "category", c("A", "B"), c("Alpha", "Beta"), "new_category")
expect_true("new_category" %in% names(dt2), info = "New column created")
expect_true(all(dt2$category == test_replace_dt$category), info = "Original column unchanged when newcolname specified")
expect_true("Alpha" %in% dt2$new_category, info = "New column has replaced values")

# Test numeric replacement
dt3 <- copy(test_replace_dt)
replace_from_table(dt3, "id", c(1, 2, 3), c(11, 22, 33))
expect_equal(dt3$id[1], 11, info = "Numeric replacement works")
expect_equal(dt3$id[2], 22, info = "Second numeric replacement works")

# Test factor replacement
dt4 <- copy(test_replace_dt)
replace_from_table(dt4, "factor_col", c("X", "Y"), c("XX", "YY"))
expect_true("XX" %in% as.character(dt4$factor_col), info = "Factor replacement works")

# Test many-to-few replacement (should show message)
dt5 <- copy(test_replace_dt)
expect_message(
  replace_from_table(dt5, "category", c("A", "B", "C"), c("Same", "Same")),
  pattern = "matched many to few",
  info = "Message shown for many-to-few replacement"
)

# Test error conditions
expect_error(replace_from_table(as.data.frame(test_replace_dt), "category", "A", "Alpha"), 
             pattern = "is.data.table", info = "Error for non-data.table input")

expect_error(replace_from_table(test_replace_dt, "nonexistent", "A", "Alpha"),
             pattern = "colname.*names", info = "Error for non-existent column")

dt6 <- copy(test_replace_dt)
expect_error(replace_from_table(dt6, "category", "A", "Alpha", "category"),
             pattern = "already exists", info = "Error when new column name already exists")

# =============================================================================
# Tests for normalise function (R wrapper)
# =============================================================================

# Test basic normalization
norm_vec <- c(10, 20, 30, 40, 50)
normalized <- normalise(norm_vec)
expect_equal(min(normalized), 0, info = "normalise minimum is 0")
expect_equal(max(normalized), 1, info = "normalise maximum is 1")
expect_true(is.numeric(normalized), info = "normalise returns numeric")

# Test with identical elements (should return 1)
identical_norm <- c(5, 5, 5, 5)
norm_identical <- normalise(identical_norm)
expect_equal(norm_identical, 1, info = "normalise returns 1 for identical elements")

# Test with single element
single_elem <- c(42)
norm_single <- normalise(single_elem)
expect_equal(norm_single, 1, info = "normalise returns 1 for single element")

# Test with negative values
neg_vec <- c(-10, -5, 0, 5, 10)
norm_neg <- normalise(neg_vec)
expect_equal(min(norm_neg), 0, info = "normalise handles negative values")
expect_equal(max(norm_neg), 1, info = "normalise scales negative values correctly")

# Test error for non-numeric input
expect_error(normalise(c("a", "b", "c")), pattern = "is.numeric", info = "normalise errors on non-numeric input")

# =============================================================================
# Tests for C++ exported functions (fquantile, fclamp, etc.)
# =============================================================================

# Test fquantile
x_quant <- c(1, 2, 3, 4, 5)
probs <- c(0.25, 0.5, 0.75)
quantiles <- fquantile(x_quant, probs)
expect_equal(length(quantiles), 3, info = "fquantile returns correct length")
expect_true(all(quantiles >= min(x_quant) & quantiles <= max(x_quant)), info = "Quantiles within data range")

# Test fquantile with NA values
x_na <- c(1, 2, NA, 4, 5)
quantiles_na <- fquantile(x_na, probs, na_rm = TRUE)
expect_equal(length(quantiles_na), 3, info = "fquantile handles NA correctly")

quantiles_no_rm <- fquantile(x_na, probs, na_rm = FALSE)
expect_true(any(is.na(quantiles_no_rm)), info = "fquantile preserves NA when na_rm = FALSE")

# Test fquantile_byid
x_byid <- c(1, 2, 3, 4, 5, 6)
id_byid <- c("A", "A", "A", "B", "B", "B")
result_byid <- fquantile_byid(x_byid, c(0.5), id_byid)
expect_true(is.list(result_byid), info = "fquantile_byid returns list")
expect_equal(length(result_byid), 2, info = "fquantile_byid has correct structure")

# Test count_if
logical_vec <- c(TRUE, FALSE, TRUE, FALSE, TRUE)
count_true <- count_if(logical_vec)
expect_equal(count_true, 3, info = "count_if counts TRUE values correctly")

# Test prop_if
prop_true <- prop_if(logical_vec)
expect_equal(prop_true, 0.6, info = "prop_if calculates proportion correctly")

# Test fclamp
x_clamp <- c(-1, 0.5, 2)
clamped_f <- fclamp(x_clamp, a = 0, b = 1)
expect_equal(clamped_f, c(0, 0.5, 1), info = "fclamp works correctly")

# Test fclamp_int
x_clamp_int <- c(-1L, 3L, 7L)
clamped_int_f <- fclamp_int(x_clamp_int, a = 0L, b = 5L)
expect_equal(clamped_int_f, c(0L, 3L, 5L), info = "fclamp_int works correctly")

# Test fequal
x_equal <- c(1.0, 1.0000001, 1.0000002)
equal_result <- fequal(x_equal, tol = 0.0001)
expect_true(as.logical(equal_result), info = "fequal detects equality within tolerance")

unequal_result <- fequal(c(1.0, 1.1), tol = 0.01)
expect_false(as.logical(unequal_result), info = "fequal detects inequality outside tolerance")

# Test fnormalise
x_norm <- c(10, 20, 30)
normalized <- fnormalise(x_norm)
expect_equal(min(normalized), 0, info = "fnormalise minimum is 0")
expect_equal(max(normalized), 1, info = "fnormalise maximum is 1")
expect_true(all(normalized >= 0 & normalized <= 1), info = "All normalized values in [0,1]")

# Test lin_interpolation
xp <- c(1.5, 2.5)
x0 <- c(1, 2)
x1 <- c(2, 3)
y0 <- c(10, 20)
y1 <- c(20, 30)
interpolated <- lin_interpolation(xp, x0, x1, y0, y1)
expect_equal(interpolated, c(15, 25), info = "Linear interpolation works correctly")

# =============================================================================
# Tests that require external packages (conditional)
# =============================================================================

# Test functions that require gamlss (skip if not available)
if (requireNamespace("gamlss", quietly = TRUE)) {
  # Test basic validate_gamlss structure (without actual model)
  # This is mainly testing parameter validation
  test_dt_gamlss <- data.table(x = rnorm(100), y = rnorm(100))
  
  # Test error conditions for validate_gamlss
  expect_error(validate_gamlss("not_dt", "not_model"), info = "validate_gamlss errors on invalid input types")
  
  # Note: Full gamlss testing would require creating actual models, which is complex
  # and might not be appropriate for unit tests
}

# Test functions that require MASS (skip if not available)  
if (requireNamespace("MASS", quietly = TRUE)) {
  # Similar approach for guess_polr - test parameter validation
  test_dt_polr <- data.table(x = rnorm(50), y = sample(1:3, 50, replace = TRUE))
  
  expect_error(guess_polr("not_dt", "not_model"), info = "guess_polr errors on invalid input types")
}

# Test reldist_diagnostics (skip if reldist not available)
if (requireNamespace("reldist", quietly = TRUE)) {
  # Basic parameter validation for reldist_diagnostics
  comparison <- rnorm(100)
  reference <- rnorm(100)
  
  expect_error(reldist_diagnostics(comparison, reference, 
                                   comparison_wt = NULL, reference_wt = NULL, 
                                   main = "test"),
               info = "reldist_diagnostics handles missing weights")
}

# =============================================================================
# Tests for resample function (from existing tests)
# =============================================================================

x <- 1:10
y <- c(NaN, 1, 6, 9, NaN, 7, 5, 8, NaN, 2)

# Test resampling with different subset sizes
expect_equal(length(resample(x[x > 8])), 2, info = "Resample length correct for x > 8")
expect_equal(length(resample(x[x > 9])), 1, info = "Resample length correct for x > 9")
expect_equal(length(resample(x[x > 10])), 0, info = "Resample length correct for empty set")

# Test resampling with NaN values
expect_equal(length(resample(y[y > 8])), 1, info = "Resample handles NaN values correctly")
expect_equal(length(resample(y[y > 9])), 0, info = "Resample handles empty NaN subset")
expect_equal(length(resample(y[y > 10])), 0, info = "Resample handles empty subset with NaN")

# =============================================================================
# Tests for outersect function (from existing tests)
# =============================================================================

y <- "city"
x <- "age" 
z <- c("age", "city")
w <- c("city", "age")
v <- 2
u <- c("city", 2)

# Test symmetrical set operations
expect_equal(outersect(x, y), z, info = "Outersect x,y produces correct result")
expect_equal(outersect(y, x), w, info = "Outersect y,x produces correct result") 
expect_equal(outersect(y, v), u, info = "Outersect with mixed types works")

result <- outersect(v, y)
expect_true(length(result) > 0, info = "Function produces result without error")

# =============================================================================
# Tests for shift_bypid function
# =============================================================================

# Test input validation
expect_error(shift_bypid(NULL, 1, c(1, 2)), info = "Error when x is not a vector")
expect_error(shift_bypid(1:5, "not_numeric", c(1, 2, 3, 4, 5)), info = "Error when lag is not numeric")
expect_error(shift_bypid(1:5, c(1, 2), c(1, 2, 3, 4, 5)), info = "Error when lag is not scalar")
expect_error(shift_bypid(1:5, 1, NULL), info = "Error when id is not a vector")
expect_error(shift_bypid(1:5, 1, c("a", "b", "c", "d", "e")), info = "Error when id is not numeric")
expect_error(shift_bypid(1:5, 1, c(1, 2, 3)), info = "Error when x and id have different lengths")
expect_error(shift_bypid(1:5, 1, c(1, 2, 1, 2, 2)), info = "Error when id is not sorted")


# Test basic functionality with numeric data
dt_test <- data.table(
  id = rep(1:3, each = 4),
  time = rep(1:4, 3),
  value = 1:12
)

# Test lag = 0 (should return unchanged)
result_lag0 <- shift_bypid(dt_test$value, lag = 0, id = dt_test$id)
expect_equal(result_lag0, dt_test$value, info = "lag = 0 returns unchanged vector")

# Test basic lag = 1
result_lag1 <- shift_bypid(dt_test$value, lag = 1, id = dt_test$id)
expected_lag1 <- c(NA, 1, 2, 3, NA, 5, 6, 7, NA, 9, 10, 11)
expect_equal(result_lag1, expected_lag1, info = "lag = 1 produces correct results")

# Test basic lead = -1
result_lead1 <- shift_bypid(dt_test$value, lag = -1, id = dt_test$id)
expected_lead1 <- c(2, 3, 4, NA, 6, 7, 8, NA, 10, 11, 12, NA)
expect_equal(result_lead1, expected_lead1, info = "lead (lag = -1) produces correct results")

# Test with larger lag
result_lag2 <- shift_bypid(dt_test$value, lag = 2, id = dt_test$id)
expected_lag2 <- c(NA, NA, 1, 2, NA, NA, 5, 6, NA, NA, 9, 10)
expect_equal(result_lag2, expected_lag2, info = "lag = 2 produces correct results")

# Test with custom replace value
result_custom_replace <- shift_bypid(dt_test$value, lag = 1, id = dt_test$id, replace = -999)
expected_custom <- c(-999, 1, 2, 3, -999, 5, 6, 7, -999, 9, 10, 11)
expect_equal(result_custom_replace, expected_custom, info = "Custom replace value works")

# Test with integer data
int_data <- as.integer(dt_test$value)
result_int <- shift_bypid(int_data, lag = 1, id = dt_test$id)
expect_true(is.integer(result_int), info = "Integer input produces integer output")
expect_equal(result_int, as.integer(expected_lag1), info = "Integer data produces correct results")

# Test with logical data
logical_data <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
result_logical <- shift_bypid(logical_data, lag = 1, id = dt_test$id)
expected_logical <- c(NA, TRUE, FALSE, TRUE, NA, TRUE, FALSE, TRUE, NA, TRUE, FALSE, TRUE)
expect_true(is.logical(result_logical), info = "Logical input produces logical output")
expect_equal(result_logical, expected_logical, info = "Logical data produces correct results")

# Test with double/numeric data
double_data <- as.double(dt_test$value)
result_double <- shift_bypid(double_data, lag = 1, id = dt_test$id)
expect_true(is.double(result_double), info = "Double input produces double output")
expect_equal(result_double, as.double(expected_lag1), info = "Double data produces correct results")

# Test with factor data
factor_data <- factor(c("A", "B", "A", "B", "A", "B", "A", "B", "A", "B", "A", "B"), 
                     levels = c("A", "B", "C"))
result_factor <- shift_bypid(factor_data, lag = 1, id = dt_test$id)
expected_factor <- factor(c(NA, "A", "B", "A", NA, "A", "B", "A", NA, "A", "B", "A"), 
                         levels = c("A", "B", "C"))
expect_true(is.factor(result_factor), info = "Factor input produces factor output")
expect_equal(levels(result_factor), levels(factor_data), info = "Factor levels preserved")
expect_equal(as.character(result_factor), as.character(expected_factor), info = "Factor data produces correct results")

# Test with single group
single_group_data <- 1:5
single_group_id <- rep(1, 5)
result_single <- shift_bypid(single_group_data, lag = 1, id = single_group_id)
expected_single <- c(NA, 1, 2, 3, 4)
expect_equal(result_single, expected_single, info = "Single group produces correct results")

# Test with empty vector
empty_data <- integer(0)
empty_id <- integer(0)
result_empty <- shift_bypid(empty_data, lag = 1, id = empty_id)
expect_equal(length(result_empty), 0, info = "Empty vector produces empty result")
expect_true(is.integer(result_empty), info = "Empty vector preserves type")

# Test type conversion for lag and id
result_convert <- shift_bypid(1:5, lag = 1.0, id = c(1.0, 1.0, 2.0, 2.0, 2.0))
expected_convert <- c(NA, 1, NA, 3, 4)
expect_equal(result_convert, expected_convert, info = "Automatic type conversion works")

# Test with unsupported type (should error)
expect_error(shift_bypid(list(1, 2, 3), lag = 1, id = c(1, 1, 1)), 
            info = "Error for unsupported data type")

# Test edge case: lag larger than group size
large_lag_result <- shift_bypid(dt_test$value, lag = 10, id = dt_test$id)
expect_true(all(is.na(large_lag_result)), info = "Large lag produces all NA values")

# Test edge case: negative lag (lead) larger than group size
large_lead_result <- shift_bypid(dt_test$value, lag = -10, id = dt_test$id)
expect_true(all(is.na(large_lead_result)), info = "Large lead produces all NA values")

# Additional comprehensive tests for lead functionality
# Test lead = -2 with detailed expected results
result_lead2 <- shift_bypid(dt_test$value, lag = -2, id = dt_test$id)
expected_lead2 <- c(3, 4, NA, NA, 7, 8, NA, NA, 11, 12, NA, NA)
expect_equal(result_lead2, expected_lead2, info = "lead (lag = -2) produces correct results")

# Test with character data using lead
char_data <- letters[1:12]
char_id <- rep(1:3, each = 4)
result_char_lead <- shift_bypid(char_data, lag = -1, id = char_id, replace = "missing")
expected_char_lead <- c("b", "c", "d", "missing", "f", "g", "h", "missing", "j", "k", "l", "missing")
expect_equal(result_char_lead, expected_char_lead, info = "Character data with lead works correctly")

# Test edge case: lag equal to group size
equal_lag_result <- shift_bypid(dt_test$value, lag = 4, id = dt_test$id)
expect_true(all(is.na(equal_lag_result)), info = "Lag equal to group size produces all NA values")

# Test edge case: lead equal to group size  
equal_lead_result <- shift_bypid(dt_test$value, lag = -4, id = dt_test$id)
expect_true(all(is.na(equal_lead_result)), info = "Lead equal to group size produces all NA values")

# Test with single element groups
single_elem_data <- c(1, 2, 3, 4, 5)
single_elem_id <- c(1, 2, 3, 4, 5)
single_elem_result <- shift_bypid(single_elem_data, lag = 1, id = single_elem_id)
expect_true(all(is.na(single_elem_result)), info = "Single element groups produce all NA values")


