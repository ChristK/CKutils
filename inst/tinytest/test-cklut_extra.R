# Extra coverage tests for R/cklut.R -- targets uncovered branches:
#   .cklut_value_type (all type branches incl. error)
#   cklut_build (file-type dispatch, input validation/error paths, value_types
#               override, double/int/logical/character/integer64 value cols)
#   print.cklut, cklut_lookup (error/validation paths), cklut_to_dt,
#   cklut_to_parquet/csv round-trips, cklut_open, is_valid_lookup_tbl,
#   set_lookup_tbl_key.

if (!requireNamespace("data.table", quietly = TRUE)) {
  exit_file("data.table package not available")
}
suppressMessages(library(data.table))

# ---------------------------------------------------------------------------
# A small dense grid with one of every supported value-column type.
# keys: a (integer, consecutive) and grp (factor)
# ---------------------------------------------------------------------------
make_tbl <- function() {
  dt <- CJ(a = 1:3, grp = factor(c("x", "y"), levels = c("x", "y")))
  n <- nrow(dt)
  dt[, dbl := as.double(a) + 0.5]                 # f64
  dt[, intc := as.integer(a) * 10L]               # i32
  dt[, lg  := (a %% 2L) == 0L]                    # lgl
  dt[, chr := paste0("v", a)]                     # character -> str
  dt[, fct := factor(c("lo", "hi")[1L + (a %% 2L)],
                     levels = c("lo", "hi"))]      # factor -> str
  dt[]
}

keys  <- c("a", "grp")
tbl   <- make_tbl()
base  <- tempfile("ckx")
ck    <- cklut_build(copy(tbl), base, keys = keys)

# --- build returns a cklut handle; schema is sane --------------------------
expect_inherits(ck, "cklut", info = "cklut_build returns cklut handle")
expect_inherits(cklut_open(paste0(base, ".ckmeta")), "cklut",
                info = "cklut_open returns cklut handle")
expect_equal(ck$schema$n_rows, as.double(nrow(tbl)), info = "row count")

# --- .cklut_value_type inference across all branches -----------------------
# (.cklut_value_type is internal; reach it through the inferred value_types)
vt <- ck$schema$value_types
names(vt) <- ck$schema$value_names
expect_equal(unname(vt["dbl"]),  "f64", info = "double -> f64")
expect_equal(unname(vt["intc"]), "i32", info = "integer -> i32")
expect_equal(unname(vt["lg"]),   "lgl", info = "logical -> lgl")
expect_equal(unname(vt["chr"]),  "str", info = "character -> str")
expect_equal(unname(vt["fct"]),  "str", info = "factor -> str")

# --- integer64 value column -> i64 (the inherits("integer64") branch) ------
if (requireNamespace("bit64", quietly = TRUE)) {
  suppressMessages(library(bit64))
  tbl64 <- copy(tbl)
  tbl64[, big := as.integer64(a) * 1000L]
  base64 <- tempfile("ckx64")
  ck64 <- cklut_build(copy(tbl64), base64, keys = keys)
  vt64 <- ck64$schema$value_types
  names(vt64) <- ck64$schema$value_names
  expect_equal(unname(vt64["big"]), "i64", info = "integer64 -> i64")
}

# --- .cklut_value_type error branch: unsupported column type ---------------
# A list/complex value column is unsupported.
bad <- copy(tbl)
bad[, cx := complex(real = a, imaginary = 0)]
expect_error(cklut_build(copy(bad), tempfile("ckbad"), keys = keys),
             pattern = "unsupported value column type",
             info = ".cklut_value_type stops on unsupported type")

# --- value_types override branch ------------------------------------------
# Force the integer column 'intc' to be stored as f64.
base_ov <- tempfile("ckov")
ck_ov <- cklut_build(copy(tbl), base_ov, keys = keys,
                     value_types = c(intc = "f64"))
vt_ov <- ck_ov$schema$value_types
names(vt_ov) <- ck_ov$schema$value_names
expect_equal(unname(vt_ov["intc"]), "f64", info = "value_types override -> f64")

# ---------------------------------------------------------------------------
# cklut_build input-validation / error paths
# ---------------------------------------------------------------------------
# unsupported file extension
expect_error(cklut_build("nope.txt", tempfile("ckbad2"), keys = keys),
             pattern = "unsupported file type",
             info = "build: bad file extension")

# missing keys argument
expect_error(cklut_build(copy(tbl), tempfile("ckbad3")),
             pattern = "`keys` is required",
             info = "build: missing keys")
# empty keys
expect_error(cklut_build(copy(tbl), tempfile("ckbad3b"), keys = character(0)),
             pattern = "`keys` is required",
             info = "build: empty keys")

# keys not present in data
expect_error(cklut_build(copy(tbl), tempfile("ckbad4"), keys = c("a", "nope")),
             pattern = "keys not in data",
             info = "build: key not in data")

# no value columns (all columns are keys)
onlykeys <- tbl[, ..keys]
expect_error(cklut_build(copy(onlykeys), tempfile("ckbad5"), keys = keys),
             pattern = "no value columns",
             info = "build: no value columns")

# explicit empty values vector
expect_error(cklut_build(copy(tbl), tempfile("ckbad5b"), keys = keys,
                         values = character(0)),
             pattern = "no value columns",
             info = "build: empty values vector")

# unsupported key type (logical key)
badkey <- copy(tbl)
badkey[, lk := rep(c(TRUE, FALSE), length.out = .N)]
expect_error(cklut_build(copy(badkey), tempfile("ckbad6"),
                         keys = c("a", "lk"),
                         values = c("dbl")),
             pattern = "must be integer, numeric or factor",
             info = "build: unsupported key type")

# check=TRUE: not a dense grid (drop a row)
notdense <- tbl[-1L]
expect_error(cklut_build(copy(notdense), tempfile("ckbad7"), keys = keys,
                         check = TRUE),
             pattern = "not a dense grid",
             info = "build: non-dense grid")

# check=TRUE: duplicated keys (still right row count but not unique)
dup <- copy(tbl)
dup[2L, a := tbl[[1L, "a"]]]
dup[2L, grp := tbl[[1L, "grp"]]]
expect_error(cklut_build(copy(dup), tempfile("ckbad8"), keys = keys,
                         check = TRUE),
             pattern = "not a dense grid|not unique",
             info = "build: duplicated keys / dense check")

# check=FALSE bypasses the grid validation (non-dense grid builds fine)
ck_nc <- cklut_build(copy(tbl), tempfile("cknc"), keys = keys, check = FALSE)
expect_inherits(ck_nc, "cklut", info = "build: check=FALSE skips grid check")

# character key path + data.frame (not data.table) input coercion ----------
dfin <- as.data.frame(make_tbl())
dfin$grp <- as.character(dfin$grp)               # character key -> sort/match path
ck_chr <- cklut_build(dfin, tempfile("ckchr"), keys = keys)
expect_inherits(ck_chr, "cklut", info = "build: char key + df coercion")

# ---------------------------------------------------------------------------
# print.cklut
# ---------------------------------------------------------------------------
out <- capture.output(print(ck))
expect_true(any(grepl("<cklut>", out, fixed = TRUE)),
            info = "print: header line")
expect_true(any(grepl("keys", out)),   info = "print: keys line")
expect_true(any(grepl("values", out)), info = "print: values line")
# print returns its argument invisibly
expect_identical(print(ck), ck, info = "print returns x invisibly")

# ---------------------------------------------------------------------------
# cklut_lookup: success + error/validation paths
# ---------------------------------------------------------------------------
q <- data.table(a = c(1L, 2L, 3L, 99L),                  # 99 -> no match -> NA
                grp = factor(c("x", "y", "x", "y"),
                             levels = c("x", "y")))

# accept a path (character) as `ck` -> cklut_open branch
got_path <- suppressMessages(cklut_lookup(copy(q), paste0(base, ".ckmeta"),
                                          merge = FALSE))
expect_inherits(got_path, "data.table", info = "lookup: path -> opens handle")
expect_equal(nrow(got_path), nrow(q), info = "lookup: one row per query")
expect_true(is.na(got_path$dbl[4L]), info = "lookup: no-match -> NA")

# values match the source table for the in-range rows
got <- suppressMessages(cklut_lookup(copy(q), ck, merge = FALSE))
expect_equal(got$dbl[1:3], c(1.5, 2.5, 3.5), info = "lookup: double values")
expect_equal(got$intc[1:3], c(10L, 20L, 30L), info = "lookup: integer values")

# merge = TRUE adds value columns to tbl by reference
m <- copy(q)
suppressMessages(cklut_lookup(m, ck, merge = TRUE))
expect_true(all(c("dbl", "intc", "lg", "chr", "fct") %in% names(m)),
            info = "lookup: merge=TRUE adds value cols")

# merge=TRUE on a data.frame triggers as.data.table coercion branch
mdf <- as.data.frame(q)
res_df <- suppressMessages(cklut_lookup(mdf, ck, merge = TRUE))
expect_inherits(res_df, "data.table", info = "lookup: merge coerces df -> dt")

# as.data.table = FALSE -> plain list
li <- suppressMessages(cklut_lookup(copy(q), ck, merge = FALSE,
                                    as.data.table = FALSE))
expect_true(is.list(li) && !is.data.table(li),
            info = "lookup: as.data.table=FALSE -> list")

# check=FALSE suppresses the no-match message (no error expected)
got_nc <- cklut_lookup(copy(q), ck, merge = FALSE, check = FALSE)
expect_equal(nrow(got_nc), nrow(q), info = "lookup: check=FALSE works")

# error: ck not a handle / path
expect_error(cklut_lookup(copy(q), 42L),
             pattern = "must be a cklut handle or .ckmeta path",
             info = "lookup: bad ck arg")

# error: excluding a key column
expect_error(cklut_lookup(copy(q), ck, exclude_col = "a"),
             pattern = "cannot exclude key column",
             info = "lookup: exclude key column")

# error: tbl missing a key column
expect_error(cklut_lookup(q[, .(a)], ck),
             pattern = "missing key column",
             info = "lookup: missing key in tbl")

# ---------------------------------------------------------------------------
# cklut_to_dt / cklut_to_csv / cklut_to_parquet round-trips
# ---------------------------------------------------------------------------
full <- cklut_to_dt(ck)
expect_inherits(full, "data.table", info = "to_dt: data.table")
expect_equal(nrow(full), nrow(tbl), info = "to_dt: full grid row count")
expect_true(all(c(keys, "dbl", "intc", "lg", "chr", "fct") %in% names(full)),
            info = "to_dt: has keys + values")

# to_dt accepts a path too
full_p <- cklut_to_dt(paste0(base, ".ckmeta"))
expect_equal(nrow(full_p), nrow(tbl), info = "to_dt: path arg")

# CSV round-trip
csv <- tempfile(fileext = ".csv")
expect_equal(cklut_to_csv(ck, csv), csv, info = "to_csv returns path")
expect_true(file.exists(csv), info = "to_csv wrote file")
ck_csv <- cklut_build(csv, tempfile("ckcsv"), keys = keys)
got_csv <- suppressMessages(cklut_lookup(copy(q), ck_csv, merge = FALSE))
expect_equal(got_csv$dbl, got$dbl, info = "csv round-trip: double values")

# Parquet round-trip (write to tempfile, rebuild, verify)
if (requireNamespace("arrow", quietly = TRUE)) {
  pq <- tempfile(fileext = ".parquet")
  expect_equal(cklut_to_parquet(ck, pq), pq, info = "to_parquet returns path")
  expect_true(file.exists(pq), info = "to_parquet wrote file")
  ck_pq <- cklut_build(pq, tempfile("ckpq"), keys = keys)
  got_pq <- suppressMessages(cklut_lookup(copy(q), ck_pq, merge = FALSE))
  expect_equal(got_pq$dbl, got$dbl, info = "parquet round-trip: double values")
  expect_equal(got_pq$intc, got$intc, info = "parquet round-trip: int values")
}

# ---------------------------------------------------------------------------
# is_valid_lookup_tbl / set_lookup_tbl_key round-trip
# ---------------------------------------------------------------------------
lt <- copy(tbl)
set_lookup_tbl_key(lt, keys)
expect_equal(key(lt), keys, info = "set_lookup_tbl_key sets the key")
expect_true(is_valid_lookup_tbl(lt, keys),
            info = "is_valid_lookup_tbl: dense unique grid is valid")
