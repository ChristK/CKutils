# Tinytest script: extra coverage for R/lookup_dt.R
# Targets previously-uncovered branches in lookup_dt() and its helpers.

if (!requireNamespace("data.table", quietly = TRUE)) {
  exit_file("data.table package not available")
}
suppressMessages(library(data.table))

# --- Line 198: non-integer/non-numeric (character) lookup key, validity OFF ---
# With check_lookup_tbl_validity = FALSE the type check inside the cardinality
# loop is reached: a key column that is neither factor, integer, nor numeric.
tbl_char <- data.table(k = c("a", "b"), x = 1:2)
lt_char  <- data.table(k = c("a", "b"), val = c("p", "q"))
setkey(lt_char, k)
expect_error(
  lookup_dt(copy(tbl_char), lt_char, check_lookup_tbl_validity = FALSE),
  pattern = "must be integer or numeric",
  info = "lookup_dt: character lookup key triggers 'must be integer or numeric'"
)

# --- Line 203: NA values in a numeric lookup key, validity OFF ---
tbl_na <- data.table(k = c(1, 2), x = 1:2)
lt_na  <- data.table(k = c(1, NA_real_), val = c("p", "q"))
setkey(lt_na, k)
expect_error(
  lookup_dt(copy(tbl_na), lt_na, check_lookup_tbl_validity = FALSE),
  pattern = "contains NA values",
  info = "lookup_dt: NA in numeric lookup key triggers 'contains NA values'"
)

# --- Line 211: integer-overflow / range-too-large guard, validity OFF ---
# Span between min and max exceeds .Machine$integer.max.
tbl_ovf <- data.table(k = c(1L, 2L), x = 1:2)
lt_ovf  <- data.table(k = c(-.Machine$integer.max, 1), val = c("p", "q"))
setkeyv(lt_ovf, "k")
expect_error(
  lookup_dt(copy(tbl_ovf), lt_ovf, check_lookup_tbl_validity = FALSE),
  pattern = "range too large",
  info = "lookup_dt: huge key range triggers 'range too large'"
)

# --- Line 190: factor key with different levels in tbl vs lookup_tbl, validity ON ---
# lookup_tbl is internally valid (2 levels, 2 rows) so is_valid_lookup_tbl passes,
# but tbl carries an extra level so the in-loop identical(levels) check fires.
lt_lvl  <- data.table(k = factor(c("X", "Y"), levels = c("X", "Y")),
                      val = c("v1", "v2"))
setkey(lt_lvl, k)
tbl_lvl <- data.table(k = factor("X", levels = c("X", "Y", "Z")), x = 1L)
expect_error(
  lookup_dt(tbl_lvl, lt_lvl, check_lookup_tbl_validity = TRUE),
  pattern = "different levels",
  info = "lookup_dt: differing factor levels triggers 'has different levels'"
)

# --- Lines 248-252: C++ index-calculation error handler ---
# tbl key column is character while lookup key is a factor; with validity OFF the
# levels check is skipped, so starts_from_1_cpp is called and throws, exercising
# the tryCatch handler that augments the message with column-type diagnostics.
lt_cpp  <- data.table(k = factor(c("X", "Y")), val = c("a", "b"))
setkey(lt_cpp, k)
tbl_cpp <- data.table(k = c("X", "Y"), x = 1:2)
expect_error(
  lookup_dt(copy(tbl_cpp), lt_cpp, check_lookup_tbl_validity = FALSE),
  pattern = "Error in C\\+\\+ index calculation",
  info = "lookup_dt: C++ failure routed through diagnostic tryCatch handler"
)

# --- Lines 256-258: warning when computed row indices contain NA, validity ON ---
# Integer lookup is internally valid; a tbl value outside the key range maps to an
# NA index, triggering both the message (line 219) and the NA warning (line 257).
lt_naidx  <- CJ(k = 1:2)
lt_naidx[, v := letters[1:2]]
setkey(lt_naidx, k)
tbl_naidx <- data.table(k = c(1L, 2L, 5L))
expect_warning(
  suppressMessages(
    lookup_dt(copy(tbl_naidx), lt_naidx, check_lookup_tbl_validity = TRUE, merge = TRUE)
  ),
  pattern = "row indices are NA",
  info = "lookup_dt: NA row index produces 'row indices are NA' warning"
)
# And the merged result keeps an NA for the unmatched row.
res_naidx <- suppressMessages(suppressWarnings(
  lookup_dt(copy(tbl_naidx), lt_naidx, check_lookup_tbl_validity = TRUE, merge = TRUE)
))
expect_identical(res_naidx$v, c("a", "b", NA_character_),
                 info = "lookup_dt: unmatched out-of-range row yields NA value")

# --- merge = FALSE path with the same NA-index scenario (return-only branch) ---
res_naidx_f <- suppressMessages(suppressWarnings(
  lookup_dt(copy(tbl_naidx), lt_naidx, check_lookup_tbl_validity = FALSE, merge = FALSE)
))
expect_identical(names(res_naidx_f), "v",
                 info = "lookup_dt (merge=FALSE): returns only the value column")
expect_identical(res_naidx_f$v, c("a", "b", NA_character_),
                 info = "lookup_dt (merge=FALSE): correct values incl NA for unmatched")

# --- is_valid_lookup_tbl: double (non-integer) key column triggers type error ---
dlt <- data.table(k = c(1, 2), v = c("a", "b"))
setkey(dlt, k)
expect_error(
  is_valid_lookup_tbl(dlt, "k"),
  pattern = "must be of type integer",
  info = "is_valid_lookup_tbl: double key column rejected as non-integer"
)

# --- is_valid_lookup_tbl: valid integer single-key table returns TRUE ---
ilt <- data.table(k = 1:3, v = letters[1:3])
setkey(ilt, k)
expect_true(is_valid_lookup_tbl(ilt, "k"),
            info = "is_valid_lookup_tbl: valid consecutive integer key returns TRUE")

# --- is_valid_lookup_tbl: fixkey = TRUE sets the key and emits a message ---
fk <- CJ(id = 1L:2L, cat = factor(letters[1:2]), sorted = FALSE)
fk[, v := runif(.N)]
expect_message(
  is_valid_lookup_tbl(fk, c("id", "cat"), fixkey = TRUE),
  info = "is_valid_lookup_tbl (fixkey=TRUE): emits key-setting message"
)
expect_identical(key(fk), c("cat", "id"),
                 info = "is_valid_lookup_tbl (fixkey=TRUE): key actually set")

# --- set_lookup_tbl_key: 'year' priority and basic operation ---
sk <- data.table(year = 2020:2022, id = 1:3, v = runif(3))
ret <- set_lookup_tbl_key(sk, c("id", "year"))
expect_identical(key(sk), c("year", "id"),
                 info = "set_lookup_tbl_key: 'year' prioritised in key order")
expect_true(is.data.table(ret),
            info = "set_lookup_tbl_key: returns the data.table (invisibly)")

# --- set_lookup_tbl_key: error paths ---
expect_error(set_lookup_tbl_key(list(a = 1), "a"),
             pattern = "should be a data.table",
             info = "set_lookup_tbl_key: non-data.table input errors")
expect_error(set_lookup_tbl_key(data.table(a = 1), character(0)),
             pattern = "keycols argument is missing",
             info = "set_lookup_tbl_key: empty keycols errors")
