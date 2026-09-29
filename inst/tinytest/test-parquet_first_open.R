# Tests for read_parquet_dt()'s skip of the open that is certain to fail
# (.pq_first_open_fails(), 0.1.32).
#
# read_parquet_dt() used to open every path with
#   tryCatch(open_dataset(path, partitioning = partitioning),
#            error = function(e) open_dataset(path))
# and arrow reads the default partitioning = "hive" as a FIELD NAME, so on a
# Hive-partitioned or flat directory the first attempt always failed. It is now
# skipped when .pq_first_open_fails() says it is certain to fail. Pinned here:
#   1. which layouts take which branch (the guard's answer, per layout);
#   2. every skip is of an open that really FAILS under the installed arrow --
#      the tripwire for a future arrow that decides differently;
#   3. every read is identical() to the historical two-step open (a copy of
#      read_parquet_dt() whose guard always answers FALSE), for data, keys,
#      column order and errors (class and message), under several call patterns;
#   4. the skip really skips (one open_dataset() call, without `partitioning`),
#      the two-step open still falls back, and the nested non-Hive layout keeps
#      the spurious `hive` column the first attempt gives it.

if (!requireNamespace("data.table", quietly = TRUE)) exit_file("data.table not available")
if (!requireNamespace("arrow", quietly = TRUE)) exit_file("arrow not available")
if (!requireNamespace("jsonlite", quietly = TRUE)) exit_file("jsonlite not available")
suppressMessages(library(data.table))

ns <- asNamespace("CKutils")
guard <- get(".pq_first_open_fails", envir = ns)
unix <- .Platform$OS.type == "unix"

# ---------------------------------------------------------------------------
# Layouts
# ---------------------------------------------------------------------------
root <- file.path(tempfile("pq_first_open_"), "r")
dir.create(root, recursive = TRUE)
d <- data.table(year = rep(1:3, each = 4), age = rep(30:33, 3), v = as.numeric(1:12),
                sex = factor(rep(c("men", "women"), 6)))
setkeyv(d, c("year", "age"))
d1 <- d[year == 1]; d2 <- d[year == 2]
nv <- function(x) x[, !"year"]
W <- function(x, p) {
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  write_parquet_dt(x, p)
}
f <- function(...) file.path(root, ...)
K <- list()
K$hive <- f("hive"); write_parquet_dt(d, K$hive, partitioning = "year")
K$hive2 <- f("hive2"); write_parquet_dt(d, K$hive2, partitioning = c("year", "sex"))
K$flat <- f("flat"); W(d1, f("flat", "a.parquet")); W(d2, f("flat", "b.parquet"))
K$nest <- f("nest"); W(d1, f("nest", "x", "a.parquet")); W(d2, f("nest", "y", "b.parquet"))
K$nest_rootfile <- f("nest_rootfile")
W(d1, f("nest_rootfile", "a.parquet")); W(d2, f("nest_rootfile", "x", "b.parquet"))
K$mixed <- f("mixed")
W(nv(d1), f("mixed", "year=1", "p.parquet")); W(nv(d2), f("mixed", "other", "q.parquet"))
K$hivekey <- f("hivekey")   # the partition key IS named "hive"
W(nv(d1), f("hivekey", "hive=1", "p.parquet")); W(nv(d2), f("hivekey", "hive=2", "p.parquet"))
K$hive_and_year <- f("hive_and_year")
W(nv(d1), f("hive_and_year", "hive=1", "p.parquet")); W(nv(d2), f("hive_and_year", "year=2", "p.parquet"))
K$rootfile <- f("rootfile")
W(nv(d1), f("rootfile", "year=1", "p.parquet")); W(nv(d2), f("rootfile", "q.parquet"))
K$hidden_files <- f("hidden_files"); write_parquet_dt(d, K$hidden_files, partitioning = "year")
writeLines("x", f("hidden_files", ".tmp_writer")); writeLines("x", f("hidden_files", "_SUCCESS"))
K$hidden_dir <- f("hidden_dir")
W(d1, f("hidden_dir", "a.parquet")); W(d2, f("hidden_dir", ".hid", "b.parquet"))
K$hidden_kv_dir <- f("hidden_kv_dir")
W(d1, f("hidden_kv_dir", "a.parquet")); W(d2, f("hidden_kv_dir", ".tmp=1", "b.parquet"))
K$underscore_kv_dir <- f("underscore_kv_dir")
W(d1, f("underscore_kv_dir", "a.parquet")); W(d2, f("underscore_kv_dir", "_x=1", "b.parquet"))
K$single <- f("flat", "a.parquet")
K$vec <- c(f("flat", "b.parquet"), f("flat", "a.parquet"))
K$vec_dirs <- c(K$hive, K$flat)
K$trailing <- paste0(K$hive, "/")
K$dotseg <- f("flat", "..", "hive")
K$corrupt <- f("corrupt"); write_parquet_dt(d, K$corrupt, partitioning = "year")
writeLines("not parquet", list.files(f("corrupt", "year=2"), full.names = TRUE)[1L])
K$empty <- f("empty"); dir.create(K$empty)
K$absent <- f("absent")
K$empty_subdir <- f("empty_subdir"); W(d1, f("empty_subdir", "a.parquet")); dir.create(f("empty_subdir", "x"))
K$empty_kv_subdir <- f("empty_kv_subdir")
W(d1, f("empty_kv_subdir", "a.parquet")); dir.create(f("empty_kv_subdir", "year=9"))
K$nonparquet <- f("nonparquet"); dir.create(K$nonparquet)
writeLines("x", f("nonparquet", "a.qs")); writeLines("a,b", f("nonparquet", "b.csv"))
K$eq_x <- f("eq_x"); W(nv(d1), f("eq_x", "=x", "p.parquet")); W(nv(d2), f("eq_x", "=y", "q.parquet"))
K$x_eq <- f("x_eq"); W(nv(d1), f("x_eq", "x=", "p.parquet")); W(nv(d2), f("x_eq", "y=", "q.parquet"))
K$a_b_c <- f("a_b_c"); W(nv(d1), f("a_b_c", "a=b=c", "p.parquet")); W(nv(d2), f("a_b_c", "a=d=e", "q.parquet"))
K$kvfile <- f("kvfile"); W(d1, f("kvfile", "k=1.parquet")); W(d2, f("kvfile", "sub", "q.parquet"))
K$kvfile_only <- f("kvfile_only"); W(d1, f("kvfile_only", "k=1.parquet")); W(d2, f("kvfile_only", "b.parquet"))
K$hivefile_only <- f("hivefile_only")
W(d1, f("hivefile_only", "hive=1.parquet")); W(d2, f("hivefile_only", "b.parquet"))
K$pct_hive <- f("pct_hive")   # "h%69ve" unescapes to "hive"
W(nv(d1), f("pct_hive", "h%69ve=1", "p.parquet")); W(nv(d2), f("pct_hive", "h%69ve=2", "q.parquet"))
K$pct_year <- f("pct_year")
W(nv(d1), f("pct_year", "y%65ar=1", "p.parquet")); W(nv(d2), f("pct_year", "y%65ar=2", "q.parquet"))
K$deep_kv <- f("deep_kv")
W(d1, f("deep_kv", "x", "k=1", "a.parquet")); W(d2, f("deep_kv", "y", "k=2", "b.parquet"))
K$Hive_case <- f("Hive_case")
W(nv(d1), f("Hive_case", "Hive=1", "p.parquet")); W(nv(d2), f("Hive_case", "Hive=2", "q.parquet"))
# key=value ANCESTORS: arrow parses the full path, so they count too
K$anc_kv_flat <- f("anc", "k=5", "flat")
W(d1, f("anc", "k=5", "flat", "a.parquet")); W(d2, f("anc", "k=5", "flat", "b.parquet"))
K$anc_kv_nest <- f("anc", "k=5", "nest")
W(d1, f("anc", "k=5", "nest", "x", "a.parquet")); W(d2, f("anc", "k=5", "nest", "y", "b.parquet"))
K$anc_kv_hive <- f("anc", "k=5", "hive"); write_parquet_dt(d, K$anc_kv_hive, partitioning = "year")
K$anc_kv_self <- f("anc", "k=6")
W(d1, f("anc", "k=6", "x", "a.parquet")); W(d2, f("anc", "k=6", "y", "b.parquet"))
K$anc_hive_flat <- f("anc", "hive=5", "flat")
W(d1, f("anc", "hive=5", "flat", "a.parquet")); W(d2, f("anc", "hive=5", "flat", "b.parquet"))
K$anc_hive_nest <- f("anc", "hive=5", "nest")
W(d1, f("anc", "hive=5", "nest", "x", "a.parquet")); W(d2, f("anc", "hive=5", "nest", "y", "b.parquet"))
K$anc_hive_hive <- f("anc", "hive=5", "hive"); write_parquet_dt(d, K$anc_hive_hive, partitioning = "year")
K$anc_hive_empty <- f("anc", "hive=5", "empty"); dir.create(K$anc_hive_empty)
K$anc_pct_flat <- f("anc", "h%69ve=5", "flat")
W(d1, f("anc", "h%69ve=5", "flat", "a.parquet")); W(d2, f("anc", "h%69ve=5", "flat", "b.parquet"))
K$anc_hive_self_flat <- f("anc", "hive=7")
W(d1, f("anc", "hive=7", "a.parquet")); W(d2, f("anc", "hive=7", "b.parquet"))
if (unix) {
  K$symlink_ds <- f("link_to_hive"); file.symlink(K$hive, K$symlink_ds)
  K$symlink_part <- f("symlink_part")
  W(nv(d1), f("symlink_part", "year=1", "p.parquet")); W(nv(d2), f("elsewhere", "year=2", "p.parquet"))
  file.symlink(f("elsewhere", "year=2"), f("symlink_part", "year=2"))
  K$symlink_flat_file <- f("symlink_flat_file"); W(d1, f("symlink_flat_file", "a.parquet"))
  file.symlink(f("flat", "b.parquet"), f("symlink_flat_file", "b.parquet"))
  K$dangling <- f("dangling"); W(d1, f("dangling", "a.parquet"))
  file.symlink(f("nowhere"), f("dangling", "gone"))
  K$loop <- f("loop"); W(d1, f("loop", "a.parquet")); file.symlink(K$loop, f("loop", "self"))
  K$symlink_file <- f("link_to_file.parquet"); file.symlink(f("flat", "a.parquet"), K$symlink_file)
  # a link whose OWN name has no '=' but whose target sits under hive=5: arrow
  # normalises the path first, so the target's ancestors are what count
  K$link_to_anc_hive_flat <- f("link_to_anc_hive_flat"); file.symlink(K$anc_hive_flat, K$link_to_anc_hive_flat)
  K$link_into_kv_nest <- f("link_into_kv_nest"); file.symlink(K$anc_kv_nest, K$link_into_kv_nest)
  K$perm <- f("perm"); write_parquet_dt(d, K$perm, partitioning = "year")
  Sys.chmod(f("perm", "year=2"), "000")
  if (file.access(f("perm", "year=2"), 4L) == 0L) K$perm <- NULL   # e.g. running as root
}

# the guard's expected answer per layout (TRUE = the first attempt is skipped)
skip_expected <- c(
  hive = TRUE, hive2 = TRUE, flat = TRUE, nest = FALSE, nest_rootfile = FALSE, mixed = TRUE,
  hivekey = FALSE, hive_and_year = TRUE, rootfile = TRUE, hidden_files = TRUE, hidden_dir = FALSE,
  hidden_kv_dir = TRUE, underscore_kv_dir = TRUE, single = FALSE, vec = FALSE, vec_dirs = FALSE,
  trailing = TRUE, dotseg = TRUE, corrupt = TRUE, empty = TRUE, absent = FALSE, empty_subdir = FALSE,
  empty_kv_subdir = TRUE, nonparquet = TRUE, eq_x = TRUE, x_eq = TRUE, a_b_c = TRUE, kvfile = TRUE,
  kvfile_only = TRUE, hivefile_only = FALSE, pct_hive = FALSE, pct_year = FALSE, deep_kv = FALSE,
  Hive_case = TRUE, anc_kv_flat = TRUE, anc_kv_nest = TRUE, anc_kv_hive = TRUE, anc_kv_self = TRUE,
  anc_hive_flat = FALSE, anc_hive_nest = FALSE, anc_hive_hive = TRUE, anc_hive_empty = FALSE,
  anc_pct_flat = FALSE, anc_hive_self_flat = FALSE, symlink_ds = TRUE, symlink_part = TRUE,
  symlink_flat_file = FALSE, dangling = FALSE, loop = FALSE, symlink_file = FALSE,
  link_to_anc_hive_flat = FALSE, link_into_kv_nest = TRUE, perm = TRUE
)

# a key=value segment in the temporary directory's OWN path would make every
# first attempt fail (and the guard skip it); the per-layout answers above
# assume there is none
tmp_has_key <- any(grepl("=", strsplit(normalizePath(root, winslash = "/"), "/", fixed = TRUE)[[1L]],
                         fixed = TRUE))

first_fails <- function(p) {
  inherits(tryCatch(arrow::open_dataset(p, format = "parquet", partitioning = "hive"),
                    error = function(e) e), "error")
}

# ---------------------------------------------------------------------------
# 1 + 2. the branch per layout, and every skip is of a failing open
# ---------------------------------------------------------------------------
for (kn in names(K)) {
  p <- K[[kn]]
  g <- guard(p, "hive")
  expect_true(is.logical(g) && length(g) == 1L && !is.na(g), info = paste(kn, ": guard gives TRUE or FALSE"))
  if (!tmp_has_key) {
    expect_identical(g, unname(skip_expected[kn]), info = paste(kn, ": guard skips the first open:", skip_expected[kn]))
  }
  if (g) expect_true(first_fails(p), info = paste(kn, ": the skipped first open really fails under this arrow"))
}
expect_true(all(names(K) %in% names(skip_expected)), info = "every layout has an expected branch")

# the layouts where skipping would CHANGE the result (the first attempt
# succeeds with an extra `hive` column) are exactly the ones the guard keeps
for (kn in intersect(c("nest", "nest_rootfile", "loop", "anc_hive_flat", "anc_hive_nest", "anc_pct_flat",
                       "anc_hive_self_flat", "link_to_anc_hive_flat", "hivefile_only"), names(K))) {
  ok <- tryCatch(arrow::open_dataset(K[[kn]], format = "parquet", partitioning = "hive"), error = function(e) NULL)
  expect_true(!is.null(ok) && "hive" %in% names(ok),
              info = paste(kn, ": the first open succeeds with a 'hive' column (the case the guard must keep)"))
  expect_false(guard(K[[kn]], "hive"), info = paste(kn, ": guard keeps the two-step open"))
}

# only the default "hive" is ever skipped; and argument edge cases answer FALSE
other_part <- list(null = NULL, year = "year", Hive = "Hive", two = c("hive", "hive"), named = c(a = "hive"),
                   factory = arrow::hive_partition(), schema = arrow::schema(year = arrow::int32()),
                   object = arrow::HivePartitioning$create(arrow::schema(year = arrow::int32())))
for (pn in names(other_part)) {
  expect_false(guard(K$hive, other_part[[pn]]), info = paste("partitioning", pn, "never skips"))
}
expect_false(guard(NA_character_, "hive"), info = "NA path: no skip")
expect_false(guard("", "hive"), info = "empty path: no skip")
expect_false(guard(c(K$hive, K$hive), "hive"), info = "two copies of a directory: no skip")
expect_false(guard(paste0("file://", K$hive), "hive"), info = "a URI: no skip")
expect_true(guard(K$hive, c("hive")), info = "partitioning = c('hive') is the default")

# ---------------------------------------------------------------------------
# 3. identical() to the historical two-step open, errors included
# ---------------------------------------------------------------------------
# read_parquet_dt() exactly as it is, except that its guard always answers
# FALSE -- i.e. the 0.1.31 two-step open
old_read <- read_parquet_dt
environment(old_read) <- list2env(list(.pq_first_open_fails = function(path, partitioning) FALSE), parent = ns)

outcome <- function(fun, p, args) {
  tryCatch(do.call(fun, c(list(p), args)), error = function(e) e)
}
same <- function(a, b) {
  if (inherits(a, "error") || inherits(b, "error")) {
    return(inherits(a, "error") && inherits(b, "error") && identical(class(a), class(b)) &&
             identical(conditionMessage(a), conditionMessage(b)))
  }
  identical(a, b) && identical(names(a), names(b)) && identical(class(a), class(b)) &&
    identical(key(a), key(b)) &&
    all(vapply(names(a), function(cn) identical(a[[cn]], b[[cn]]) && identical(levels(a[[cn]]), levels(b[[cn]])), NA))
}
patterns <- list(
  plain = list(),
  cols = list(cols = c("age", "v")),
  filter_age = list(filter = arrow::Expression$field_ref("age") >= 32L),
  filter_year = list(filter = arrow_in("year", 2L)),
  cols_filter = list(cols = c("age", "v"), filter = arrow::Expression$field_ref("age") >= 32L),
  as_df = list(as_data_table = FALSE),
  keys_fallback = list(keys_fallback = "age"),
  part_null = list(partitioning = NULL),
  part_year = list(partitioning = "year")
)
for (kn in names(K)) for (pn in names(patterns)) {
  a <- outcome(read_parquet_dt, K[[kn]], patterns[[pn]])
  b <- outcome(old_read, K[[kn]], patterns[[pn]])
  expect_true(same(a, b), info = paste(kn, pn, ": identical to the two-step open"))
}

# the paths that must still error, exactly as before (class + message)
for (kn in intersect(c("absent", "corrupt", "nonparquet", "vec_dirs", "perm"), names(K))) {
  a <- outcome(read_parquet_dt, K[[kn]], list())
  expect_true(inherits(a, "error"), info = paste(kn, ": still an error"))
}

# a nested non-Hive directory keeps the spurious `hive` column (the historical
# first attempt succeeds there), a Hive one gets its partition column
expect_true("hive" %in% names(read_parquet_dt(K$nest)), info = "nested non-Hive: 'hive' column kept")
expect_identical(sort(unique(read_parquet_dt(K$nest)$hive)), c("x", "y"), info = "nested non-Hive: hive = subdir names")
expect_identical(names(read_parquet_dt(K$hive)), c("age", "v", "sex", "year"), info = "hive: partition column last")
expect_identical(key(read_parquet_dt(K$hive)), c("year", "age"), info = "hive: key restored")
expect_identical(dim(read_parquet_dt(K$flat)), c(8L, 4L), info = "flat: both files read")
expect_identical(key(read_parquet_dt(K$flat)), c("year", "age"), info = "flat: key restored")
if (unix) expect_identical(read_parquet_dt(K$symlink_ds), read_parquet_dt(K$hive), info = "a symlink reads as its target")

# ---------------------------------------------------------------------------
# 4. the branch actually taken: count open_dataset() calls and their arguments
# ---------------------------------------------------------------------------
calls <- new.env()
counted_read <- read_parquet_dt
environment(counted_read) <- list2env(list(open_dataset = function(sources, ...) {
  a <- list(...)
  calls$log <- c(calls$log, if ("partitioning" %in% names(a)) "with_partitioning" else "without")
  arrow::open_dataset(sources, ...)
}), parent = ns)
log_of <- function(p) { calls$log <- character(); invisible(tryCatch(counted_read(p), error = function(e) e)); calls$log }

if (!tmp_has_key) {
  expect_identical(log_of(K$hive), "without", info = "Hive dir: one open, the fallback only")
  expect_identical(log_of(K$flat), "without", info = "flat dir: one open, the fallback only")
  expect_identical(log_of(K$anc_kv_nest), "without", info = "under a k=v ancestor: one open, the fallback only")
  expect_identical(log_of(K$empty), "without", info = "empty dir: one open (the fallback's error)")
  expect_identical(log_of(K$nest), "with_partitioning", info = "nested non-Hive: the first attempt, which succeeds")
  expect_identical(log_of(K$hivekey), "with_partitioning", info = "key named hive: the first attempt, which succeeds")
  expect_identical(log_of(K$deep_kv), c("with_partitioning", "without"),
                   info = "deeper k=v only: first attempt fails, then the fallback (tryCatch branch)")
  expect_identical(log_of(K$empty_subdir), c("with_partitioning", "without"),
                   info = "empty subdirectory: two-step open, falls back")
}
expect_identical(log_of(K$single), "with_partitioning", info = "a file: one open, which succeeds")
expect_identical(log_of(K$absent), c("with_partitioning", "without"), info = "absent path: two-step open, both fail")
calls$log <- character(); invisible(counted_read(K$hive, partitioning = "year"))
expect_identical(calls$log, "with_partitioning", info = "partitioning = 'year' on a year= dataset: the first open, which succeeds")
calls$log <- character(); invisible(counted_read(K$hive, partitioning = "sex"))
expect_identical(calls$log, c("with_partitioning", "without"), info = "partitioning = 'sex': two-step open, falls back")
# an empty directory is not an error: both opens give a table with no columns
expect_identical(dim(read_parquet_dt(K$empty)), c(0L, 0L), info = "empty directory: a 0 x 0 table, as before")

if (!is.null(K$perm)) Sys.chmod(f("perm", "year=2"), "755")   # let the temporary directory be removed
