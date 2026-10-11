# write_parquet_dt(): `compression` reaches the files of a PARTITIONED write too.
# It used to be dropped there, so every partitioned dataset was snappy whatever
# codec was asked for (only the single-file branch passed it on).
#
# arrow (R) cannot report a file's codec, so the codec is shown two ways:
# zstd and gzip frames carry fixed magic bytes (28 b5 2f fd; 1f 8b 08), which
# must appear in every partition file written with that codec and not in an
# uncompressed or snappy one; and every codec must give a smaller dataset than
# "uncompressed", which a write that ignores the codec cannot. The data must
# also come back unchanged.

if (!requireNamespace("data.table", quietly = TRUE)) exit_file("data.table not available")
if (!requireNamespace("arrow", quietly = TRUE)) exit_file("arrow not available")
if (!requireNamespace("jsonlite", quietly = TRUE)) exit_file("jsonlite not available")
suppressMessages(library(data.table))

# A build of arrow may lack some codecs (zstd and lz4 are optional)
codecs <- c("uncompressed", "snappy", "gzip", "zstd", "lz4")
codecs <- codecs[vapply(codecs, arrow::codec_is_available, logical(1))]
if (!all(c("uncompressed", "snappy") %in% codecs)) exit_file("arrow lacks snappy")

magic <- list(zstd = as.raw(c(0x28, 0xb5, 0x2f, 0xfd)), gzip = as.raw(c(0x1f, 0x8b, 0x08)))
has_magic <- function(file, m) {
  length(grepRaw(m, readBin(file, "raw", file.size(file)), fixed = TRUE)) > 0L
}

set.seed(1)
d <- CJ(year = 1:2, id = 1:20000)
d[, v := round(runif(.N), 2)][, w := id %% 7L]
setkeyv(d, c("year", "id"))

td <- tempfile("pq_compression_")
dir.create(td)

files <- list()
for (cd in codecs) {
  p <- file.path(td, cd, "ds")
  write_parquet_dt(copy(d), p, partitioning = "year", compression = cd)
  files[[cd]] <- list.files(p, recursive = TRUE, full.names = TRUE)
  back <- read_parquet_dt(p)
  setkeyv(back, c("year", "id"))
  expect_identical(back[, .(year, id, v, w)], d[, .(year, id, v, w)],
                   info = paste("partitioned round trip,", cd))
}
expect_true(all(lengths(files) == 2L), info = "one file per year partition")

# The codec's own frames are in every partition file
for (cd in intersect(c("zstd", "gzip"), codecs)) {
  expect_true(all(vapply(files[[cd]], has_magic, logical(1), m = magic[[cd]])),
              info = paste("every partition file holds", cd, "frames"))
}
if ("zstd" %in% codecs) {
  for (cd in c("uncompressed", "snappy")) {
    expect_false(any(vapply(files[[cd]], has_magic, logical(1), m = magic$zstd)),
                 info = paste("no zstd frames in a", cd, "partition file"))
  }
}

# Every codec compresses the partitions
sizes <- vapply(files, function(f) sum(file.size(f)), numeric(1))
for (cd in setdiff(codecs, "uncompressed")) {
  expect_true(sizes[[cd]] < sizes[["uncompressed"]],
              info = paste(cd, "partitions are smaller than uncompressed ones"))
}

# The single-file branch, which always passed the codec on, is unchanged
for (cd in intersect(c("lz4", "zstd"), codecs)) {
  f <- file.path(td, paste0("single_", cd, ".parquet"))
  write_parquet_dt(copy(d), f, compression = cd)
  expect_identical(read_parquet_dt(f)[, .(year, id, v, w)], d[, .(year, id, v, w)],
                   info = paste("single file round trip,", cd))
  if (cd == "zstd") expect_true(has_magic(f, magic$zstd), info = "single file holds zstd frames")
}

# ---------------------------------------------------------------------------
# compression_level: the codec's level, on both branches
# ---------------------------------------------------------------------------
md5_of <- function(files) unname(tools::md5sum(sort(files)))
single_file <- function(name, ...) {
  f <- file.path(td, paste0(name, ".parquet"))
  write_parquet_dt(copy(d), f, ...)
  f
}
partitioned <- function(name, ...) {
  p <- file.path(td, name, "ds")
  write_parquet_dt(copy(d), p, partitioning = "year", ...)
  list.files(p, recursive = TRUE, full.names = TRUE)
}

# NULL (the default) writes what a call without the argument wrote
expect_identical(md5_of(single_file("lvl_none")), md5_of(single_file("lvl_null", compression_level = NULL)),
                 info = "compression_level = NULL: same bytes, single file")
expect_identical(md5_of(partitioned("plvl_none")), md5_of(partitioned("plvl_null", compression_level = NULL)),
                 info = "compression_level = NULL: same bytes, partitioned")

if ("zstd" %in% codecs) {
  # a higher level compresses more, on both branches
  s1 <- file.size(single_file("z1", compression = "zstd", compression_level = 1))
  s19 <- file.size(single_file("z19", compression = "zstd", compression_level = 19))
  expect_true(s19 < s1, info = "zstd level 19 is smaller than level 1, single file")
  p1 <- sum(file.size(partitioned("pz1", compression = "zstd", compression_level = 1)))
  p19 <- sum(file.size(partitioned("pz19", compression = "zstd", compression_level = 19)))
  expect_true(p19 < p1, info = "zstd level 19 is smaller than level 1, partitioned")
  # the data come back at level 9, and a level as a double is taken as is
  pz9 <- file.path(td, "pz9", "ds")
  write_parquet_dt(copy(d), pz9, partitioning = "year", compression = "zstd", compression_level = 9)
  back <- read_parquet_dt(pz9)
  setkeyv(back, c("year", "id"))
  expect_identical(back[, .(year, id, v, w)], d[, .(year, id, v, w)],
                   info = "partitioned round trip, zstd level 9")
  expect_identical(md5_of(single_file("z9d", compression = "zstd", compression_level = 9)),
                   md5_of(single_file("z9i", compression = "zstd", compression_level = 9L)),
                   info = "level 9 and 9L write the same bytes")
  # arrow caps a zstd level beyond 22 at 22
  expect_identical(md5_of(single_file("z100", compression = "zstd", compression_level = 100)),
                   md5_of(single_file("z22", compression = "zstd", compression_level = 22)),
                   info = "zstd level 100 is capped at 22")
}

if ("gzip" %in% codecs) {
  g1 <- single_file("g1", compression = "gzip", compression_level = 1)
  g9 <- single_file("g9", compression = "gzip", compression_level = 9)
  expect_false(identical(md5_of(g1), md5_of(g9)), info = "gzip levels 1 and 9 write different bytes")
  expect_identical(read_parquet_dt(g1)[, .(year, id, v, w)], d[, .(year, id, v, w)],
                   info = "single file round trip, gzip level 1")
  for (bad in c(-1, 10, 19)) {
    expect_error(single_file("gbad", compression = "gzip", compression_level = bad),
                 "between 0 and 9 for gzip", info = paste("gzip level", bad))
  }
}

# Not a single whole number
for (bad in list("9", c(1, 2), NA, NA_integer_, 1.5, Inf, numeric(0))) {
  expect_error(single_file("bad", compression = "zstd", compression_level = bad),
               "must be NULL or a single whole number",
               info = paste("compression_level =", deparse(bad)))
}
# A codec without levels: arrow's own error, on both branches
expect_error(single_file("snl", compression = "snappy", compression_level = 1),
             "doesn't support setting a compression level", info = "snappy has no levels, single file")
expect_error(partitioned("psnl", compression = "snappy", compression_level = 1),
             "doesn't support setting a compression level", info = "snappy has no levels, partitioned")

unlink(td, recursive = TRUE)
