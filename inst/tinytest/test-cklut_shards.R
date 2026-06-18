# Tests for the multi-shard / warm-up code paths of cklut.
# A small `max_bytes` forces the table to be split across several shards, which
# exercises the fast index->shard division (detail::FastDivU64::div) and the
# per-shard prefetch/populate warm-up path (cklut_open(warm = TRUE)).

if (!requireNamespace("data.table", quietly = TRUE)) {
  exit_file("data.table package not available")
}
suppressMessages(library(data.table))

set.seed(11)

# A dense grid with enough rows that a tiny shard cap creates many shards.
# id spans 1:64 (a power of two) to also exercise power-of-two strides.
grid <- CJ(id = 1:64, grp = 1:50)
grid[, val := id + grp / 100]          # double value column
grid[, cnt := as.integer(id * grp)]    # integer value column

keys <- c("id", "grp")

base <- tempfile("ckshard")
# 2 KB shard cap -> the payload is split into many shards.
ck <- cklut_build(copy(grid), base, keys = keys, max_bytes = 2048)
expect_inherits(ck, "cklut", info = "multi-shard build returns a handle")
expect_true(ck$schema$n_shards > 1,
            info = "small max_bytes produces more than one shard")

# Query the whole grid back; lookups must cross shard boundaries correctly,
# which forces div() to map each linear index to its shard.
q <- grid[, ..keys][sample(.N)]        # shuffled so lookups hit many shards
got <- suppressMessages(cklut_lookup(copy(q), ck, merge = FALSE))

oracle <- suppressMessages(lookup_dt(copy(q), copy(grid), merge = FALSE,
                                     check_lookup_tbl_validity = FALSE))
expect_equal(got$val, oracle$val, info = "multi-shard lookup: double column correct")
expect_equal(got$cnt, oracle$cnt, info = "multi-shard lookup: integer column correct")

# Reopen the same on-disk table with warm = TRUE: this walks every shard and
# calls willneed() + populate() to pull all pages resident before returning.
ck_warm <- cklut_open(paste0(base, ".ckmeta"), warm = TRUE)
expect_inherits(ck_warm, "cklut", info = "warm open returns a handle")
got_warm <- suppressMessages(cklut_lookup(copy(q), ck_warm, merge = FALSE))
expect_equal(got_warm$val, oracle$val, info = "warm multi-shard lookup correct")

# A full round-trip of a multi-shard table back to a data.table.
rt <- cklut_to_dt(ck)
expect_equal(nrow(rt), nrow(grid), info = "multi-shard cklut_to_dt full grid")
