# read_parquet_dt()'s Arrow -> data.table conversion (CKutils 0.1.33).
#
# write_parquet_dt() stores the table's R attributes in the file -- class
# data.table, the key (`sorted`), any `index`, any custom attribute -- and
# arrow's conversion re-applies them. The two that describe ROW ORDER must not
# survive: setDT(key = ) trusts a `sorted` attribute that is already present and
# does not sort, while a multi-partition read comes back in PATH order (year=10
# before year=3). read_parquet_dt() removes them with setattr() after arrow's
# metadata step (it used to rely on as.data.frame.data.table(), whose deep copy
# removed them). Checked here:
#   0. THE FALSE-KEY VARIANT: arrow's metadata step without that removal, then
#      setDT(key = ), on this fixture -- it claims the key and is NOT in key
#      order, so the fixture really exercises the hazard (if arrow ever stops
#      restoring the write-time key, this fails and says so);
#   1. read_parquet_dt() on the same keyed multi-partition data comes back TRULY
#      in key order -- by base order(), because forderv() reuses a stored key
#      and cannot see this;
#   2. a filtered read across the year=9 / year=10 boundary, likewise;
#   3. no stale secondary index;
#   4. a custom table attribute is restored;
#   5. the values equal the original, in key order;
#   6. the data.table path makes no deep copy (as.data.frame.data.table is not
#      called), and as_data_table = FALSE still returns a plain data.frame.

library(data.table)

truly_keyed <- function(x) {
  k <- key(x)
  !is.null(k) && identical(do.call(order, c(unname(lapply(k, function(cn) x[[cn]])),
                                            list(na.last = FALSE, method = "radix"))), seq_len(nrow(x)))
}

dt <- data.table(year = rep(c(3L, 9L, 10L, 25L), each = 6L), age = rep(30:35, 4L),
                 sex = factor(rep(c("men", "women"), 12L)), v = seq_len(24L) / 7)
setkeyv(dt, c("year", "age"))
setindexv(dt, "v")
setattr(dt, "source", "test fixture, 2026")
td <- tempfile("pq_conv_"); dir.create(td)
p <- file.path(td, "ds")
write_parquet_dt(dt, p, partitioning = "year")

# 0. the false-key variant
tab0 <- arrow::open_dataset(p, format = "parquet")$NewScan()$Finish()$ToTable()
apply_md <- get0("apply_arrow_r_metadata", envir = asNamespace("arrow"), inherits = FALSE)
if (is.function(apply_md)) {
  fk <- apply_md(tab0$to_data_frame(), tab0$metadata[["r"]])
  expect_identical(attr(fk, "sorted"), c("year", "age"),
                   info = "0: arrow's metadata step restores the write-time key")
  setDT(fk, key = c("year", "age"))
  expect_identical(key(fk), c("year", "age"), info = "0: the false-key variant claims the key")
  expect_false(truly_keyed(fk), info = "0: ...and is NOT in key order: a false key (the hazard is real here)")
}

got <- read_parquet_dt(p)
expect_identical(key(got), c("year", "age"), info = "1: the stored key is restored")
expect_true(truly_keyed(got), info = "1: a multi-partition read is truly in key order (path order is 10, 25, 3, 9)")
expect_identical(unique(got$year), c(3L, 9L, 10L, 25L), info = "1: years in numeric, not path, order")

got2 <- read_parquet_dt(p, filter = arrow_in("year", c(9L, 10L)))
expect_true(truly_keyed(got2), info = "2: a filtered read across year=9 / year=10 is truly in key order")
expect_identical(unique(got2$year), c(9L, 10L), info = "2: and holds only those years")

expect_null(indices(got), info = "3: no stale secondary index")

expect_identical(attr(got, "source"), "test fixture, 2026", info = "4: a custom table attribute is restored")

exp <- copy(dt); setattr(exp, "index", NULL)
setcolorder(got, names(exp))
expect_equal(got, exp, check.attributes = FALSE, info = "5: the values equal the original")
expect_identical(levels(got$sex), levels(dt$sex), info = "5: factor levels")

# Counted by a wrapper registered as the S3 method itself -- what dispatch looks
# up. Not trace(): it splices the tracer into the traced body, so a `<<-` in it
# never reaches this test's environment (it lands in the global one).
hits <- 0L
orig_ <- getS3method("as.data.frame", "data.table")
registerS3method("as.data.frame", "data.table",
                 function(x, ...) { hits <<- hits + 1L; orig_(x, ...) }, envir = asNamespace("data.table"))
g3 <- read_parquet_dt(p)
registerS3method("as.data.frame", "data.table", orig_, envir = asNamespace("data.table"))
expect_identical(getS3method("as.data.frame", "data.table"), orig_, info = "6: (the method is restored)")
expect_identical(hits, 0L, info = "6: the data.table path makes no deep copy")
expect_true(data.table:::selfrefok(g3) > 0L && truelength(g3) > length(g3),
            info = "6: a valid, over-allocated data.table")
f_ <- function(d) d[, probe_ := 1L]
f_(g3)
expect_true("probe_" %in% names(g3), info = "6: := by reference reaches the caller")
gdf <- read_parquet_dt(p, as_data_table = FALSE)
expect_identical(class(gdf), "data.frame", info = "6: as_data_table = FALSE returns a plain data.frame")
expect_null(attr(gdf, "sorted"), info = "6: ...without a key")

unlink(td, recursive = TRUE)
