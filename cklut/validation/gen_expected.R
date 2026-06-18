## gen_expected.R
## Builds a representative dense lookup table + a set of query rows, runs the
## lookup_dt reference oracle, and writes:
##   lookup_long.csv     -- the full lookup table (keys + values), for cklut
##   queries_expected.csv-- query keys + the values lookup_dt returns for them
##
## To validate against the REAL function, set USE_REAL_CKUTILS=1 in the env
## (requires CKutils installed); otherwise the bundled reference oracle is used.

suppressWarnings(suppressMessages(library(data.table)))
here <- dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1]))
if (is.na(here) || here == "") here <- "."
source(file.path(here, "reference_lookup.R"))

set.seed(42)
options(digits = 17)

## ---- schema -------------------------------------------------------------
## Mixed key types on purpose:
##  - year, age : consecutive integer keys (test min-offset != 1 and != 0)
##  - region, sex : factors with NON-alphabetical level order (test that the
##                  cklut category order must follow factor levels, not sort)
region_levels <- c("north", "south", "east", "west")
sex_levels    <- c("men", "women")
years <- 2018:2020
ages  <- 40:99

## ---- dense lookup table (every key combination) -------------------------
lookup_tbl <- CJ(
  year   = years,
  age    = ages,
  region = factor(region_levels, levels = region_levels),
  sex    = factor(sex_levels,    levels = sex_levels)
)
# value columns are deterministic functions of ALL keys, so a wrong index
# (wrong row) is guaranteed to produce a wrong value.
rc <- as.integer(lookup_tbl$region)
sc <- as.integer(lookup_tbl$sex)
lookup_tbl[, mu    := year + age / 100 + rc / 1000 + sc / 10000]
lookup_tbl[, sigma := age + rc * 10 + sc]
lookup_tbl[, p     := (year * 1000 + age * 10 + rc + sc) / 7]

## ---- query rows (random combos, with repeats) ---------------------------
N <- 5000L
queries <- data.table(
  year   = sample(years, N, replace = TRUE),
  age    = sample(ages,  N, replace = TRUE),
  region = factor(sample(region_levels, N, replace = TRUE), levels = region_levels),
  sex    = factor(sample(sex_levels,    N, replace = TRUE), levels = sex_levels)
)

## ---- run the oracle -----------------------------------------------------
use_real <- nzchar(Sys.getenv("USE_REAL_CKUTILS"))
if (use_real) {
  message("Oracle: CKutils::lookup_dt (real package)")
  library(CKutils)
  exp_vals <- lookup_dt(copy(queries), copy(lookup_tbl), merge = FALSE,
                        check_lookup_tbl_validity = FALSE)
  on_order <- sort(intersect(names(queries), names(lookup_tbl)))
  on_order <- on_order[order(match(on_order, "year"))]
} else {
  message("Oracle: bundled lookup_dt reference port")
  ref <- lookup_dt_ref(copy(queries), copy(lookup_tbl))
  exp_vals <- ref$values
  on_order <- ref$on
}

## The cklut C++ harness hard-codes dim order [year, age, region, sex]; assert
## lookup_dt resolves the same order so the two can never silently drift.
stopifnot(identical(on_order, c("year", "age", "region", "sex")))
cat("resolved key order (on):", paste(on_order, collapse = ", "), "\n")

## ---- write outputs (labels for factors, full precision) -----------------
out_lookup <- copy(lookup_tbl)
out_lookup[, region := as.character(region)]
out_lookup[, sex    := as.character(sex)]
setcolorder(out_lookup, c("year", "age", "region", "sex", "mu", "sigma", "p"))
fwrite(out_lookup, file.path(here, "lookup_long.csv"))

out_q <- copy(queries)
out_q[, region := as.character(region)]
out_q[, sex    := as.character(sex)]
out_q <- cbind(out_q, exp_vals)
setcolorder(out_q, c("year", "age", "region", "sex", "mu", "sigma", "p"))
fwrite(out_q, file.path(here, "queries_expected.csv"))

cat("wrote", nrow(out_lookup), "lookup rows and", nrow(out_q), "query rows\n")
