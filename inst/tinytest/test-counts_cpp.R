# Tests for the C++ tabulation helpers in src/counts.cpp:
#   counts()   (exported) - frequency table preserving value-type dispatch
#   tableRcpp() (exported via Rcpp) - thin wrapper over Rcpp::table
# Existing tests cover the numeric/character/logical paths of counts(); here we
# add the integer path, the character-NA path, and the full tableRcpp dispatch.

tableRcpp <- getFromNamespace("tableRcpp", "CKutils")

# -----------------------------------------------------------------------------
# counts(): integer (INTSXP) dispatch branch
# -----------------------------------------------------------------------------
int_res <- counts(c(1L, 2L, 2L, 3L, 3L, 3L))
expect_true(is.integer(int_res), info = "counts(integer) returns integer")
expect_equal(as.vector(int_res), c(1L, 2L, 3L),
             info = "counts(integer) tallies correctly")
expect_equal(names(int_res), c("1", "2", "3"),
             info = "counts(integer) names are the values")

# integer NA handling
int_na <- counts(c(1L, NA_integer_, 1L, 2L, NA_integer_))
expect_true(any(is.na(names(int_na))) || "NA" %in% names(int_na),
            info = "counts(integer) keeps NA bucket")

# -----------------------------------------------------------------------------
# counts(): character path with NA (exercises the NA-name fix-up)
# -----------------------------------------------------------------------------
chr_na <- counts(c("a", "b", NA, "a", NA, "b"))
expect_equal(sum(chr_na), 6L, info = "counts(character w/ NA) totals all elements")
expect_true(any(is.na(names(chr_na))),
            info = "counts(character) restores NA as a real NA name")

# -----------------------------------------------------------------------------
# counts(): unsupported type errors
# -----------------------------------------------------------------------------
expect_error(counts(complex(real = 1, imaginary = 1)),
             pattern = "unrecognized SEXP type",
             info = "counts() errors on unsupported (complex) type")

# -----------------------------------------------------------------------------
# tableRcpp(): all four supported dispatch branches + error branch
# -----------------------------------------------------------------------------
t_int <- tableRcpp(c(1L, 1L, 2L, 3L, 3L, 3L))
expect_equal(as.vector(t_int), c(2L, 1L, 3L), info = "tableRcpp(integer)")

t_dbl <- tableRcpp(c(1.5, 1.5, 2.5))
expect_equal(as.vector(t_dbl), c(2L, 1L), info = "tableRcpp(double)")

t_chr <- tableRcpp(c("x", "y", "x", "x"))
expect_equal(as.vector(t_chr[order(names(t_chr))]), c(3L, 1L), info = "tableRcpp(character)")

t_lgl <- tableRcpp(c(TRUE, FALSE, TRUE))
expect_equal(sum(t_lgl), 3L, info = "tableRcpp(logical)")

expect_error(tableRcpp(complex(real = 1, imaginary = 1)),
             pattern = "unrecognized SEXP type",
             info = "tableRcpp() errors on unsupported type")
