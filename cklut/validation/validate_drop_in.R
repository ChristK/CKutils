## validate_drop_in.R
## Validates the cklut R drop-in (cklut_build / cklut_lookup) against BOTH the
## real CKutils::lookup_dt and CKutils::absorb_dt on a dense lookup table with
## mixed-type value columns (double / integer / factor / logical), including
## NA-in-table, out-of-range (no-match -> NA), and a CSV round-trip.
##
## It compiles the package's own C++ sources with Rcpp::sourceCpp (so it needs
## only Rcpp + data.table installed, not a full package build) and sources the
## R layer directly. Run from the repository root:
##
##   Rscript cklut/validation/validate_drop_in.R
##
## Exit status 0 = all oracles agree on every column; 1 = mismatch.

suppressMessages({ library(Rcpp); library(data.table) })

# locate repo root from this script's path (fallback: current dir)
args <- commandArgs(FALSE)
here <- dirname(sub("--file=", "", grep("--file=", args, value = TRUE)[1]))
root <- if (length(here) && !is.na(here) && nzchar(here)) {
  normalizePath(file.path(here, "..", ".."))
} else normalizePath(".")

dt_inc <- system.file("include", package = "data.table")
Sys.setenv(PKG_CPPFLAGS = paste0("-I", file.path(root, "cklut"), " -I\"", dt_inc, "\""))
# Compile the package's C++ outside src/ so Rcpp doesn't pull in the package's
# LinkingTo deps (dqrng/sitmo) as Rcpp::depends.
.cc <- tempfile("ckcc"); dir.create(.cc)
for (f in c("cklut_r.cpp", "lookup_dt.cpp", "cklut.h", "cklut_typed.h", "cklut_build_typed.h"))
  file.copy(file.path(root, "src", f), file.path(.cc, f), overwrite = TRUE)
sourceCpp(file.path(.cc, "lookup_dt.cpp"))   # real starts_from_1_cpp + dtsubset
sourceCpp(file.path(.cc, "cklut_r.cpp"))     # cklut primitives
source(file.path(root, "R", "lookup_dt.R"))          # lookup_dt, is_valid_lookup_tbl
source(file.path(root, "R", "dt_ops.R"))             # absorb_dt
source(file.path(root, "R", "cklut.R"))              # cklut_build / cklut_lookup / ...

set.seed(7); options(digits = 17)
region_levels <- c("north", "south", "east", "west")   # non-alphabetical on purpose
sex_levels    <- c("men", "women")
years <- 2018:2020; ages <- 40:99

lookup_tbl <- CJ(year = years, age = ages,
                 region = factor(region_levels, levels = region_levels),
                 sex    = factor(sex_levels,    levels = sex_levels))
rc <- as.integer(lookup_tbl$region); sc <- as.integer(lookup_tbl$sex)
lookup_tbl[, mu   := year + age / 100 + rc / 1000 + sc / 1e4]                        # double
lookup_tbl[, cnt  := as.integer(year * 10L + age + rc)]                              # integer
lookup_tbl[, grp  := factor(c("lo", "hi")[1L + (age %% 2L)], levels = c("lo", "hi"))]  # factor
lookup_tbl[, flag := (rc + sc) %% 2L == 0L]                                          # logical
lookup_tbl[1, mu := NA_real_]; lookup_tbl[2, cnt := NA_integer_]                     # planted NAs

keys  <- c("year", "age", "region", "sex")
vcols <- c("mu", "cnt", "grp", "flag")

base <- tempfile("ckval")
ck <- cklut_build(copy(lookup_tbl), base, keys = keys)
print(ck)

N <- 4000L
q <- data.table(
  year   = sample(c(years, 2021L), N, replace = TRUE),
  age    = sample(c(ages, 100L),  N, replace = TRUE),
  region = factor(sample(region_levels, N, replace = TRUE), levels = region_levels),
  sex    = factor(sample(sex_levels,    N, replace = TRUE), levels = sex_levels))

exp_ld <- suppressMessages(lookup_dt(copy(q), copy(lookup_tbl), merge = FALSE,
                                     check_lookup_tbl_validity = FALSE))
ab <- copy(q); suppressMessages(absorb_dt(ab, copy(lookup_tbl), on = keys))
exp_ab <- ab[, ..vcols]
got <- suppressMessages(cklut_lookup(copy(q), ck, merge = FALSE))

cmp <- function(a, b, nm) {
  ok <- vapply(vcols, function(c) isTRUE(all.equal(a[[c]], b[[c]])), logical(1))
  cat(sprintf("  %-26s %s\n", nm, if (all(ok)) "MATCH" else
      paste("MISMATCH:", paste(vcols[!ok], collapse = ","))))
  all(ok)
}
cat("\n=== column-by-column equality ===\n")
r1 <- cmp(got, exp_ld, "cklut vs lookup_dt")
r2 <- cmp(got, exp_ab, "cklut vs absorb_dt")
r3 <- cmp(exp_ld, exp_ab, "lookup_dt vs absorb_dt")
cat("  no-match rows -> NA:", sum(is.na(got$mu)), "of", N, "\n")

# CSV stores labels but not factor level order; compare by content.
to_chr <- function(d) { d <- copy(d); for (c in vcols) if (is.factor(d[[c]])) set(d, j = c, value = as.character(d[[c]])); d }
csv <- tempfile(fileext = ".csv"); cklut_to_csv(ck, csv)
ck2 <- cklut_build(csv, tempfile("ckval2"), keys = keys)
got2 <- suppressMessages(cklut_lookup(copy(q), ck2, merge = FALSE))
r4 <- cmp(to_chr(got), to_chr(got2), "cklut vs CSV-roundtrip(labels)")

ok <- r1 && r2 && r3 && r4
cat("\nRESULT:", if (ok) "PASS (all oracles agree)" else "FAIL", "\n")
quit(status = if (ok) 0L else 1L)
