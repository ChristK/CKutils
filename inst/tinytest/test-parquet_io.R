# Tests for parquet I/O helpers in misc_functions.R:
#   write_parquet_dt(), read_parquet_dt(), arrow_in()
#
# Covers single-file and partitioned-dataset paths, all optional arguments,
# key metadata preservation/restoration, and error/validation branches.

# ---------------------------------------------------------------------------
# Setup / dependency guards
# ---------------------------------------------------------------------------
if (!requireNamespace("data.table", quietly = TRUE)) {
  exit_file("data.table package not available")
}
if (!requireNamespace("arrow", quietly = TRUE)) {
  exit_file("arrow package not available")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  exit_file("jsonlite package not available")
}
suppressMessages(library(data.table))

set.seed(42)

# Helper: compare two data.tables irrespective of row order / key state
expect_dt_equal <- function(a, b, cols = NULL, info = "") {
  a <- data.table::as.data.table(a)
  b <- data.table::as.data.table(b)
  if (is.null(cols)) cols <- intersect(names(a), names(b))
  a <- a[, ..cols]
  b <- b[, ..cols]
  data.table::setcolorder(b, names(a))
  data.table::setorderv(a, names(a))
  data.table::setorderv(b, names(b))
  data.table::setkey(a, NULL)
  data.table::setkey(b, NULL)
  expect_equal(a, b, info = info)
}

# A reusable keyed data.table
make_dt <- function(n = 20L) {
  dt <- data.table::data.table(
    id    = seq_len(n),
    year  = rep(2020:2023, length.out = n),
    age   = 20L + seq_len(n),
    sex   = rep(c("F", "M"), length.out = n),
    value = rnorm(n)
  )
  data.table::setkey(dt, id, year)
  dt[]
}


# ===========================================================================
# arrow_in()
# ===========================================================================

# Returns an arrow Expression for a column name
expr1 <- arrow_in("year", c(2020, 2021))
expect_true(inherits(expr1, "Expression"), info = "arrow_in returns Expression")

# Accepts an existing arrow::Expression as 'field'
expr2 <- arrow_in(arrow::Expression$field_ref("year"), 2020)
expect_true(inherits(expr2, "Expression"), info = "arrow_in accepts Expression field")

# Single-value (scalar coerced to vector) works
expr3 <- arrow_in("sex", "F")
expect_true(inherits(expr3, "Expression"), info = "arrow_in single value")

# Error: empty values vector
expect_error(arrow_in("year", character(0)),
             info = "arrow_in errors on empty values")

# Error: field must be a single name or Expression
expect_error(arrow_in(c("a", "b"), 1),
             info = "arrow_in errors on multi-length field")
expect_error(arrow_in(42, 1),
             info = "arrow_in errors on non-character non-Expression field")


# ===========================================================================
# write_parquet_dt() / read_parquet_dt() -- single file round-trip
# ===========================================================================

dt <- make_dt()

# --- default: preserve existing keys ---------------------------------------
tf <- tempfile(fileext = ".parquet")
res <- write_parquet_dt(dt, tf)
expect_equal(res, tf, info = "write_parquet_dt returns path invisibly")
expect_true(file.exists(tf), info = "single parquet file written")

# Key metadata stored as JSON in r.data.table.keys
pf <- arrow::read_parquet(tf, as_data_frame = FALSE)
expect_equal(jsonlite::fromJSON(pf$metadata[["r.data.table.keys"]]),
             c("id", "year"), info = "keys stored in metadata")

got <- read_parquet_dt(tf)
expect_true(data.table::is.data.table(got), info = "read returns data.table")
expect_equal(data.table::key(got), c("id", "year"),
             info = "keys restored from metadata")
expect_dt_equal(got, dt, info = "single-file round-trip values equal")

# --- explicit character keys -----------------------------------------------
tf_k <- tempfile(fileext = ".parquet")
write_parquet_dt(dt, tf_k, keys = c("age"))
got_k <- read_parquet_dt(tf_k)
expect_equal(data.table::key(got_k), "age", info = "explicit key restored")

# --- impactncd key logic ----------------------------------------------------
# Excludes mu/sigma/nu/tau/maxq/minq and cols ending in a digit, sorts, year last
dti <- data.table::data.table(
  year = 2020:2024, age = 30:34, sex = "M",
  mu = rnorm(5), sigma = runif(5), tau = 1:5,
  col2 = 1:5            # ends in digit -> excluded from keys
)
tf_i <- tempfile(fileext = ".parquet")
write_parquet_dt(dti, tf_i, keys = "impactncd")
got_i <- read_parquet_dt(tf_i)
expect_equal(data.table::key(got_i), c("age", "sex", "year"),
             info = "impactncd keys computed correctly")
# column reordering puts keys first
expect_equal(head(names(got_i), 3), c("age", "sex", "year"),
             info = "impactncd reorders key columns first")

# --- no keys at all ---------------------------------------------------------
dt_nokey <- data.table::data.table(a = 1:3, b = 4:6)
tf_nk <- tempfile(fileext = ".parquet")
write_parquet_dt(dt_nokey, tf_nk)
pf_nk <- arrow::read_parquet(tf_nk, as_data_frame = FALSE)
expect_true(is.null(pf_nk$metadata[["r.data.table.keys"]]),
            info = "no key metadata when data.table unkeyed")
got_nk <- read_parquet_dt(tf_nk)
expect_equal(length(data.table::key(got_nk)), 0L,
             info = "no keys restored when none stored")
expect_dt_equal(got_nk, dt_nokey, info = "unkeyed round-trip values equal")

# --- compression options ----------------------------------------------------
for (cmp in c("gzip", "zstd", "uncompressed")) {
  tf_c <- tempfile(fileext = ".parquet")
  write_parquet_dt(dt, tf_c, compression = cmp)
  got_c <- read_parquet_dt(tf_c)
  expect_dt_equal(got_c, dt, info = paste("compression round-trip:", cmp))
}

# --- factor column round-trip ----------------------------------------------
dt_f <- data.table::data.table(g = factor(c("a", "b", "a", "b")), v = 1:4)
data.table::setkey(dt_f, g)
tf_f <- tempfile(fileext = ".parquet")
write_parquet_dt(dt_f, tf_f)
got_f <- read_parquet_dt(tf_f)
expect_true(is.factor(got_f$g), info = "factor column preserved as factor")
expect_equal(data.table::key(got_f), "g", info = "factor key restored")

# --- data.frame input (coerced to data.table) ------------------------------
df_in <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)
tf_df <- tempfile(fileext = ".parquet")
write_parquet_dt(df_in, tf_df)
got_df <- read_parquet_dt(tf_df)
expect_dt_equal(got_df, df_in, info = "data.frame input round-trip")


# ===========================================================================
# read_parquet_dt() -- optional arguments
# ===========================================================================

# --- column projection ------------------------------------------------------
got_proj <- read_parquet_dt(tf, cols = c("id", "value"))
expect_equal(sort(names(got_proj)), c("id", "value"),
             info = "cols projection keeps only requested columns")
# key not applied because not all key columns are present after projection
expect_equal(length(data.table::key(got_proj)), 0L,
             info = "keys dropped when key cols absent from projection")

# --- row filter via arrow::Expression --------------------------------------
got_filt <- read_parquet_dt(
  tf, filter = arrow::Expression$field_ref("age") >= 30L)
expect_true(all(got_filt$age >= 30L), info = "filter applied (age >= 30)")
expect_equal(nrow(got_filt), sum(dt$age >= 30L),
             info = "filter row count correct")

# --- filter via arrow_in() --------------------------------------------------
got_in <- read_parquet_dt(tf, filter = arrow_in("year", c(2020, 2021)))
expect_true(all(got_in$year %in% c(2020, 2021)),
            info = "arrow_in filter restricts years")

# --- as_data_table = FALSE returns data.frame ------------------------------
got_dfout <- read_parquet_dt(tf, as_data_table = FALSE)
expect_false(data.table::is.data.table(got_dfout),
             info = "as_data_table=FALSE returns plain data.frame")
expect_true(is.data.frame(got_dfout),
            info = "as_data_table=FALSE result is data.frame")

# --- keys_fallback applied when no metadata keys ---------------------------
got_fb <- read_parquet_dt(tf_nk, keys_fallback = c("a", "b"))
expect_equal(data.table::key(got_fb), c("a", "b"),
             info = "keys_fallback applied when no metadata keys")

# --- keys_fallback ignored when columns missing ----------------------------
got_fb2 <- read_parquet_dt(tf_nk, keys_fallback = c("nonexistent"))
expect_equal(length(data.table::key(got_fb2)), 0L,
             info = "keys_fallback ignored when columns absent")

# --- metadata keys take precedence over keys_fallback ----------------------
got_pref <- read_parquet_dt(tf, keys_fallback = c("age"))
expect_equal(data.table::key(got_pref), c("id", "year"),
             info = "metadata keys win over keys_fallback")

# --- vector of files --------------------------------------------------------
f1 <- tempfile(fileext = ".parquet")
f2 <- tempfile(fileext = ".parquet")
write_parquet_dt(dt[1:10], f1)
write_parquet_dt(dt[11:20], f2)
got_vec <- read_parquet_dt(c(f1, f2))
expect_equal(nrow(got_vec), 20L, info = "vector-of-files reads all rows")
expect_dt_equal(got_vec, dt, info = "vector-of-files round-trip values equal")


# ===========================================================================
# Partitioned-dataset code path
# ===========================================================================

# --- explicit partitioning column ------------------------------------------
pdir <- tempfile()
dir.create(pdir)
write_parquet_dt(dt, pdir, partitioning = "year")
# Hive-style directories created
parts <- list.files(pdir)
expect_true(all(grepl("^year=", parts)),
            info = "hive partition directories created")
expect_equal(length(parts), length(unique(dt$year)),
             info = "one directory per partition value")

got_part <- read_parquet_dt(pdir)
expect_true(data.table::is.data.table(got_part),
            info = "partitioned read returns data.table")
expect_equal(nrow(got_part), nrow(dt),
             info = "partitioned read returns all rows")
expect_equal(data.table::key(got_part), c("id", "year"),
             info = "keys restored from partitioned dataset metadata")
expect_dt_equal(got_part, dt, info = "partitioned round-trip values equal")

# --- partitioned read with projection + filter -----------------------------
got_pf <- read_parquet_dt(
  pdir,
  cols   = c("id", "year", "value"),
  filter = arrow_in("year", 2020))
expect_true(all(got_pf$year == 2020),
            info = "partitioned read with arrow_in filter")
expect_equal(sort(names(got_pf)), c("id", "value", "year"),
             info = "partitioned read with projection")

# --- partitioning = TRUE auto-partitions by year ---------------------------
pdir2 <- tempfile()
dir.create(pdir2)
write_parquet_dt(dt, pdir2, partitioning = TRUE)
expect_true(all(grepl("^year=", list.files(pdir2))),
            info = "partitioning=TRUE auto-partitions by year")

# --- partitioning = TRUE with no 'year' column -> single file --------------
pf_noyear <- tempfile(fileext = ".parquet")
write_parquet_dt(dt_nokey, pf_noyear, partitioning = TRUE)
expect_true(file.exists(pf_noyear) && !dir.exists(pf_noyear),
            info = "partitioning=TRUE without year writes single file")


# ===========================================================================
# Pre-built sample data files (inst/testdata)
# ===========================================================================

sample_file <- system.file("testdata", "test_read_parquet.parquet",
                            package = "CKutils")
expect_true(nzchar(sample_file) && file.exists(sample_file),
            info = "sample single parquet file exists")
samp <- read_parquet_dt(sample_file)
expect_true(data.table::is.data.table(samp),
            info = "sample file read as data.table")
expect_equal(sort(names(samp)), c("age", "id", "sex", "value"),
             info = "sample file has expected columns")
expect_equal(nrow(samp), 100L, info = "sample file has 100 rows")

# keys_fallback on a sample file that lacks key metadata
samp_fb <- read_parquet_dt(sample_file, keys_fallback = c("id", "age"))
expect_equal(data.table::key(samp_fb), c("id", "age"),
             info = "keys_fallback applied to sample file")

# Pre-built partitioned dataset (partitioned by sex=F/M).
# NOTE: open_dataset() with explicit partitioning="hive" errors in this arrow
# version when partitions are auto-detected; read_parquet_dt's internal
# tryCatch falls back to open_dataset() without partitioning, exercising that
# recovery branch.
sample_part <- system.file("testdata", "partitioned_test",
                           package = "CKutils")
expect_true(nzchar(sample_part) && dir.exists(sample_part),
            info = "sample partitioned dataset exists")
samp_p <- read_parquet_dt(sample_part)
expect_true(data.table::is.data.table(samp_p),
            info = "sample partitioned dataset read as data.table")
expect_true("sex" %in% names(samp_p),
            info = "partition column 'sex' present in result")
expect_equal(nrow(samp_p), 100L,
             info = "sample partitioned dataset has 100 rows")
expect_true(all(c("F", "M") %in% unique(samp_p$sex)),
            info = "both partition values present")

# filter the sample partitioned dataset by partition column
samp_pf <- read_parquet_dt(sample_part, filter = arrow_in("sex", "F"))
expect_true(all(samp_pf$sex == "F"),
            info = "filter on partitioned sample restricts to F")


# ===========================================================================
# Error / validation branches
# ===========================================================================

# write_parquet_dt: path must be a single string
expect_error(write_parquet_dt(dt, c("a", "b")),
             info = "write errors on non-scalar path")

# write_parquet_dt: x must be data.table/data.frame/fst path
expect_error(write_parquet_dt(42, tempfile()),
             info = "write errors on invalid x type")

# write_parquet_dt: missing key columns
expect_error(write_parquet_dt(dt, tempfile(), keys = "zzz"),
             info = "write errors on missing key columns")

# write_parquet_dt: invalid keys type (not NULL/char/'impactncd')
expect_error(write_parquet_dt(dt, tempfile(), keys = list(1)),
             info = "write errors on invalid keys type")

# write_parquet_dt: invalid partitioning type
expect_error(write_parquet_dt(dt, tempfile(), partitioning = 42),
             info = "write errors on invalid partitioning type")

# write_parquet_dt: missing partitioning columns
expect_error(write_parquet_dt(dt, tempfile(), partitioning = "zzz"),
             info = "write errors on missing partitioning columns")

# read_parquet_dt: filter must be an arrow::Expression
expect_error(read_parquet_dt(tf, filter = "age >= 1"),
             info = "read errors on non-Expression filter")

# ---------------------------------------------------------------------------
# fst-file input to write_parquet_dt (only if fst is available)
# ---------------------------------------------------------------------------
if (requireNamespace("fst", quietly = TRUE)) {
  fst_in <- tempfile(fileext = ".fst")
  fst::write_fst(as.data.frame(dt), fst_in)
  tf_fst <- tempfile(fileext = ".parquet")
  write_parquet_dt(fst_in, tf_fst, keys = c("id", "year"))
  got_fst <- read_parquet_dt(tf_fst)
  expect_equal(data.table::key(got_fst), c("id", "year"),
               info = "fst input -> parquet preserves specified keys")
  expect_equal(nrow(got_fst), nrow(dt),
               info = "fst input round-trip row count")
  expect_dt_equal(got_fst, dt, info = "fst input round-trip values equal")
}
