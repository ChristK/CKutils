## bench_r.R
## Speed comparison of the cklut R drop-in (cklut_lookup) vs CKutils::lookup_dt
## on a large dense table, warm and in-RAM. Both compute the row index and gather
## values in C++. cklut additionally works for tables larger than RAM and needs
## no per-call lookup-table rebuild/validation; this micro-benchmark measures the
## warm in-RAM case (the least favourable for cklut).
##
## Run from the repository root:
##   Rscript cklut/bench/bench_r.R [n_query_millions]

suppressMessages({ library(Rcpp); library(data.table) })
args <- commandArgs(TRUE)
M <- as.integer((if (length(args)) as.numeric(args[1]) else 5) * 1e6)

cargs <- commandArgs(FALSE)
here <- dirname(sub("--file=", "", grep("--file=", cargs, value = TRUE)[1]))
root <- if (length(here) && !is.na(here) && nzchar(here)) {
  normalizePath(file.path(here, "..", ".."))
} else normalizePath(".")
dt_inc <- system.file("include", package = "data.table")
Sys.setenv(PKG_CPPFLAGS = paste0("-I", file.path(root, "cklut"), " -I\"", dt_inc, "\""))
.cc <- tempfile("ckcc"); dir.create(.cc)
for (f in c("cklut_r.cpp", "lookup_dt.cpp", "cklut.h", "cklut_typed.h", "cklut_build_typed.h"))
  file.copy(file.path(root, "src", f), file.path(.cc, f), overwrite = TRUE)
sourceCpp(file.path(.cc, "lookup_dt.cpp"))
sourceCpp(file.path(.cc, "cklut_r.cpp"))
source(file.path(root, "R", "lookup_dt.R"))
source(file.path(root, "R", "cklut.R"))
setDTthreads(1L)

set.seed(1)
years <- 2000:2020; ages <- 0:99
regions <- factor(sprintf("r%02d", 1:10)); sexes <- factor(c("m", "w"))
causes <- factor(sprintf("c%02d", 1:30))
lt <- CJ(year = years, age = ages, region = regions, sex = sexes, cause = causes)
lt[, mu := runif(.N)][, sd := runif(.N)][, a := runif(.N)][, b := runif(.N)]
keys <- c("year", "age", "region", "sex", "cause")
cat("lookup_tbl rows:", format(nrow(lt), big.mark = ","),
    " | query rows:", format(M, big.mark = ","), "\n")

ck <- cklut_build(copy(lt), tempfile("ckbench"), keys = keys)
q <- data.table(year = sample(years, M, TRUE), age = sample(ages, M, TRUE),
                region = sample(regions, M, TRUE), sex = sample(sexes, M, TRUE),
                cause = sample(causes, M, TRUE))

tm <- function(expr) { gc(); (system.time(for (i in 1:3) eval.parent(substitute(expr)))["elapsed"]) / 3 }
t_ld <- tm(lookup_dt(copy(q), lt, merge = FALSE, check_lookup_tbl_validity = FALSE))
t_ck <- tm(cklut_lookup(copy(q), ck, merge = FALSE, check = FALSE))
cat(sprintf("lookup_dt : %.3f s/call\ncklut     : %.3f s/call\nspeedup   : %.2fx\n",
            t_ld, t_ck, t_ld / t_ck))
