# Tests for the validation / error branches of the C++ helpers in
# src/lookup_dt.cpp: fct_to_int_cpp (exported), and the internal
# starts_from_1_cpp and dtsubset (reached via the package namespace).

starts_from_1_cpp <- getFromNamespace("starts_from_1_cpp", "CKutils")
dtsubset          <- getFromNamespace("dtsubset", "CKutils")

# -----------------------------------------------------------------------------
# fct_to_int_cpp
# -----------------------------------------------------------------------------
expect_error(fct_to_int_cpp(NULL), pattern = "NULL",
             info = "fct_to_int_cpp errors on NULL input")
expect_error(fct_to_int_cpp(1L), pattern = "not a factor",
             info = "fct_to_int_cpp errors on non-factor input")
expect_error(fct_to_int_cpp("a"), pattern = "not a factor",
             info = "fct_to_int_cpp errors on character input")

f <- factor(c("b", "a", "c", "a"), levels = c("a", "b", "c"))
expect_equal(fct_to_int_cpp(f), c(2L, 1L, 3L, 1L),
             info = "fct_to_int_cpp returns underlying integer codes (copy)")
expect_null(attr(fct_to_int_cpp(f), "levels"),
            info = "fct_to_int_cpp strips levels attribute")
expect_true(is.integer(fct_to_int_cpp(f, inplace = TRUE)),
            info = "fct_to_int_cpp inplace=TRUE returns an integer vector")

# -----------------------------------------------------------------------------
# starts_from_1_cpp
# -----------------------------------------------------------------------------
tbl <- data.frame(
  a = c(3L, 5L, NA_integer_, 7L),
  f = factor(c("x", "y", "z", "x"), levels = c("x", "y", "z")),
  ch = c("p", "q", "r", "s"),
  stringsAsFactors = FALSE
)

# index out of bounds for 'on'
expect_error(starts_from_1_cpp(tbl, c("a"), 0L, list(1L), list(10L)),
             pattern = "out of bounds", info = "starts_from_1_cpp: i < 1")
expect_error(starts_from_1_cpp(tbl, c("a"), 2L, list(1L), list(10L)),
             pattern = "out of bounds", info = "starts_from_1_cpp: i > on size")
# index out of bounds for min_lookup / cardinality
expect_error(starts_from_1_cpp(tbl, c("a", "f"), 2L, list(1L), list(10L)),
             pattern = "out of bounds for min_lookup",
             info = "starts_from_1_cpp: i beyond min_lookup/cardinality length")
# column not present in tbl
expect_error(starts_from_1_cpp(tbl, c("missing"), 1L, list(1L), list(10L)),
             pattern = "not found", info = "starts_from_1_cpp: column not found")
# NULL min_lookup / cardinality entries
expect_error(starts_from_1_cpp(tbl, c("a"), 1L, list(NULL), list(10L)),
             pattern = "NULL", info = "starts_from_1_cpp: NULL min_lookup entry")
expect_error(starts_from_1_cpp(tbl, c("a"), 1L, list(1L), list(NULL)),
             pattern = "NULL", info = "starts_from_1_cpp: NULL cardinality entry")
# non-positive cardinality
expect_error(starts_from_1_cpp(tbl, c("a"), 1L, list(1L), list(0L)),
             pattern = "positive", info = "starts_from_1_cpp: cardinality <= 0")

# integer column happy path + NA + out-of-range -> NA
out_int <- starts_from_1_cpp(tbl, c("a"), 1L, list(2L), list(4L))
# offset = min_lookup - 1 = 1; values: 3->2, 5->4, NA->NA, 7->6 (>card=4 -> NA)
expect_equal(out_int, c(2L, 4L, NA_integer_, NA_integer_),
             info = "starts_from_1_cpp: integer adjust with NA and range clipping")

# NA integer stays NA
out_na <- starts_from_1_cpp(data.frame(a = NA_integer_), c("a"), 1L, list(1L), list(5L))
expect_true(is.na(out_na[1]), info = "starts_from_1_cpp: NA integer stays NA")

# factor column path (uses fct_to_int_cpp internally)
out_fac <- starts_from_1_cpp(tbl, c("f"), 1L, list(1L), list(3L))
expect_equal(out_fac, c(1L, 2L, 3L, 1L),
             info = "starts_from_1_cpp: factor column converted to 1-based codes")

# unsupported column type (character) -> error
expect_error(starts_from_1_cpp(tbl, c("ch"), 1L, list(1L), list(3L)),
             pattern = "integer or a factor",
             info = "starts_from_1_cpp: character column rejected")

# -----------------------------------------------------------------------------
# dtsubset
# -----------------------------------------------------------------------------
DT <- data.table::data.table(x = 1:5, y = letters[1:5], z = 11:15)

expect_error(dtsubset(NULL, 1L, 1L), pattern = "NULL",
             info = "dtsubset: NULL x")
expect_error(dtsubset(DT, NULL, 1L), pattern = "NULL",
             info = "dtsubset: NULL rows")
expect_error(dtsubset(DT, 1L, NULL), pattern = "NULL",
             info = "dtsubset: NULL cols")
expect_error(dtsubset(data.frame(x = 1:5), 1L, 1L), pattern = "must be a data.table",
             info = "dtsubset: x not a data.table")
expect_error(dtsubset(DT, 1.0, 1L), pattern = "Row indices must be integer",
             info = "dtsubset: rows not integer")
expect_error(dtsubset(DT, 1L, 1.0), pattern = "Column indices must be integer",
             info = "dtsubset: cols not integer")
expect_error(dtsubset(DT, integer(0), 1L), pattern = "Row indices vector is empty",
             info = "dtsubset: empty rows")
expect_error(dtsubset(DT, 1L, integer(0)), pattern = "Column indices vector is empty",
             info = "dtsubset: empty cols")
expect_error(dtsubset(DT, 1L, NA_integer_), pattern = "cannot contain NA",
             info = "dtsubset: NA column index")
expect_error(dtsubset(DT, 1L, 99L), pattern = "out of bounds",
             info = "dtsubset: column index out of range")
expect_error(dtsubset(DT, 1L, 0L), pattern = "out of bounds",
             info = "dtsubset: column index below range")

# happy path: select rows 1,3 and columns 1,3
res <- dtsubset(DT, c(1L, 3L), c(1L, 3L))
expect_equal(res[[1]], c(1L, 3L), info = "dtsubset: subset returns selected rows (col 1)")
expect_equal(res[[2]], c(11L, 13L), info = "dtsubset: subset returns selected rows (col 3)")
