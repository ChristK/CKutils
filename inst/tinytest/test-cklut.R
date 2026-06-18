# Unit tests for the cklut on-disk lookup table (drop-in for lookup_dt).
# Correctness is checked against BOTH lookup_dt and absorb_dt on a dense table
# with mixed-type value columns, plus NA / no-match handling and CSV round-trip.

if (!requireNamespace("data.table", quietly = TRUE)) {
  exit_file("data.table package not available")
}
suppressMessages(library(data.table))

set.seed(7)
region_levels <- c("north", "south", "east", "west")   # non-alphabetical on purpose
sex_levels    <- c("men", "women")
years <- 2018:2020; ages <- 40:59

lookup_tbl <- CJ(year = years, age = ages,
                 region = factor(region_levels, levels = region_levels),
                 sex    = factor(sex_levels,    levels = sex_levels))
rc <- as.integer(lookup_tbl$region); sc <- as.integer(lookup_tbl$sex)
lookup_tbl[, mu   := year + age / 100 + rc / 1000 + sc / 1e4]                       # double
lookup_tbl[, cnt  := as.integer(year * 10L + age + rc)]                             # integer
lookup_tbl[, grp  := factor(c("lo", "hi")[1L + (age %% 2L)], levels = c("lo", "hi"))] # factor
lookup_tbl[, flag := (rc + sc) %% 2L == 0L]                                         # logical
lookup_tbl[1, mu := NA_real_]; lookup_tbl[2, cnt := NA_integer_]                    # planted NAs

keys  <- c("year", "age", "region", "sex")
vcols <- c("mu", "cnt", "grp", "flag")

base <- tempfile("cktest")
ck <- cklut_build(copy(lookup_tbl), base, keys = keys)

expect_inherits(ck, "cklut", info = "cklut_build returns a handle")
expect_equal(ck$schema$n_rows, as.double(nrow(lookup_tbl)), info = "row count")
expect_equal(ck$schema$value_types, c("f64", "i32", "str", "lgl"), info = "inferred value types")

# query rows, some deliberately out of range -> NA
N <- 2000L
q <- data.table(
  year   = sample(c(years, 2021L), N, replace = TRUE),
  age    = sample(c(ages, 100L),  N, replace = TRUE),
  region = factor(sample(region_levels, N, replace = TRUE), levels = region_levels),
  sex    = factor(sample(sex_levels,    N, replace = TRUE), levels = sex_levels))

# oracle 1: lookup_dt
exp_ld <- suppressMessages(lookup_dt(copy(q), copy(lookup_tbl), merge = FALSE,
                                     check_lookup_tbl_validity = FALSE))
# oracle 2: absorb_dt (data.table join)
ab <- copy(q); suppressMessages(absorb_dt(ab, copy(lookup_tbl), on = keys))
exp_ab <- ab[, ..vcols]
# cklut
got <- suppressMessages(cklut_lookup(copy(q), ck, merge = FALSE))

for (c in vcols) {
  expect_equal(got[[c]], exp_ld[[c]], info = paste("cklut vs lookup_dt:", c))
  expect_equal(got[[c]], exp_ab[[c]], info = paste("cklut vs absorb_dt:", c))
}

# merge = TRUE adds the value columns to tbl
m <- copy(q); suppressMessages(cklut_lookup(m, ck, merge = TRUE))
expect_true(all(vcols %in% names(m)), info = "merge=TRUE adds value columns")
expect_equal(m$mu, exp_ld$mu, info = "merge=TRUE values correct")

# as.data.table = FALSE returns a plain list
li <- suppressMessages(cklut_lookup(copy(q), ck, merge = FALSE, as.data.table = FALSE))
expect_true(is.list(li) && !is.data.table(li), info = "as.data.table=FALSE -> list")

# round-trip the whole table
expect_equal(nrow(cklut_to_dt(ck)), nrow(lookup_tbl), info = "cklut_to_dt full grid")

# CSV export preserves values (labels); factor level order is not encoded in CSV
csv <- tempfile(fileext = ".csv")
cklut_to_csv(ck, csv)
ck2 <- cklut_build(csv, tempfile("cktest2"), keys = keys)
got2 <- suppressMessages(cklut_lookup(copy(q), ck2, merge = FALSE))
expect_equal(as.character(got2$grp), as.character(got$grp), info = "CSV round-trip labels")
expect_equal(got2$mu, got$mu, info = "CSV round-trip numeric")
