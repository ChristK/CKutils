#!/usr/bin/env Rscript
# Measure how THIS machine's CPU resolves an out-of-range double -> int
# conversion, and confirm that the count_to_int() guard makes the answer
# architecture-independent.
#
#   Rscript .github/scripts/arch_cast_probe.R
#
# Why this lives in .github/scripts and not in the package
# -------------------------------------------------------
# It deliberately performs undefined behaviour: an unguarded (int) cast of a
# double that does not fit. That is exactly what CKutils removed, and shipping
# it would (rightly) be flagged by CRAN's UBSAN builds. Here it is a diagnostic,
# compiled at CI time with Rcpp::cppFunction, never part of the package.
#
# What it is for
# --------------
# NEWS.md and README.md state that the conversion saturates to INT_MIN on
# x86-64 and to INT_MAX on AArch64, and that the AArch64 case is the dangerous
# one because INT_MAX then feeds the `for (int i = 0; i <= q; i++)`
# accumulation in the CDF kernels, which cannot terminate. The x86 half was
# measured; the AArch64 half was taken from the architecture reference manual.
# Running this on an arm64 runner turns that into an observation.
#
# It is informational: it prints what it finds and only FAILS if the guarded
# path misbehaves, because the guard is what the package actually promises. A
# surprising saturation direction is a documentation matter, not a defect.

if (!requireNamespace("Rcpp", quietly = TRUE)) {
  cat("Rcpp not available; skipping architecture probe\n"); quit(status = 0L)
}

machine <- Sys.info()[["machine"]]
is_arm  <- grepl("aarch64|arm64", machine, ignore.case = TRUE)
cat("== out-of-range double -> int conversion on this machine ==\n")
cat("   machine : ", machine, "\n", sep = "")
cat("   platform: ", R.version$platform, "\n", sep = "")
cat("   compiler: ", tryCatch(system("$(R CMD config CXX) --version", intern = TRUE)[1],
                              error = function(e) "unknown"), "\n\n", sep = "")

# Compile the unguarded cast. `volatile` stops the compiler folding it away and
# stops it assuming the UB cannot happen.
Rcpp::cppFunction('
double unguarded_cast(double d) {
  volatile double v = d;
  return (double) (int) v;
}')

INT_MIN <- -2147483648
INT_MAX <-  2147483647
probes  <- c(`2^31`     = 2^31,
             `4e9`      = 4e9,
             `1e10`     = 1e10,
             `1e300`    = 1e300,
             `Inf`      = Inf,
             `-2^31-10` = -(2^31) - 10,
             `-Inf`     = -Inf)

res <- vapply(probes, unguarded_cast, numeric(1))
label <- function(v) if (identical(v, INT_MIN)) "INT_MIN" else
                     if (identical(v, INT_MAX)) "INT_MAX" else format(v)
cat(sprintf("   %-10s -> %-14s (%s)\n", names(probes), format(res),
            vapply(res, label, character(1))), sep = "")

pos <- res[c("2^31", "4e9", "1e10", "1e300", "Inf")]
direction <- if (all(pos == INT_MIN)) "INT_MIN (x86-64 style)" else
             if (all(pos == INT_MAX)) "INT_MAX (AArch64 style)" else "mixed/other"
cat("\n   positive overflow saturates to: ", direction, "\n", sep = "")

expected <- if (is_arm) "INT_MAX (AArch64 style)" else "INT_MIN (x86-64 style)"
if (identical(direction, expected)) {
  cat("   this MATCHES what NEWS.md and README.md document for this architecture.\n")
} else {
  cat("   NOTE: NEWS.md and README.md document ", expected, " for this\n",
      "   architecture. The documentation should be corrected to say: ",
      direction, ".\n", sep = "")
}

if (identical(direction, "INT_MAX (AArch64 style)")) {
  cat("\n   Consequence had the guard not been added: this value would enter\n",
      "   `for (int i = 0; i <= q; i++)` in fpBNB_scalar/fpDPO_scalar/\n",
      "   fpDEL_hlp_fn, where i++ at INT_MAX is signed overflow and the loop\n",
      "   never exits -- a hang, not a wrong number.\n", sep = "")
}

# --- the part that actually gates CI -----------------------------------------
cat("\n== guarded path: CKutils must return NA regardless of the above ==\n")
if (!requireNamespace("CKutils", quietly = TRUE)) {
  cat("   CKutils not installed; skipping the guarded check\n"); quit(status = 0L)
}
suppressMessages(library(CKutils))

checks <- list(
  "fpZABNB(2^31)"        = function() fpZABNB(2^31, 2, 1, 1, 0.1),
  "fpZANBI(4e9)"         = function() fpZANBI(4e9, 2, 1, 0.1),
  "fpBNB(1e10)"          = function() fpBNB(1e10, 2, 1, 1),
  "fpDPO(Inf)"           = function() fpDPO(Inf, 2, 1),
  "fdZABNB(4e9)"         = function() fdZABNB(4e9, 2, 1, 1, 0.1),
  "fpZABNB(INT_MAX)"     = function() fpZABNB(.Machine$integer.max, 2, 1, 1, 0.1),
  "fqDPO(0.999, 3e9, 1)" = function() fqDPO(0.999, 3e9, 1)
)
bad <- character(0)
for (nm in names(checks)) {
  t0 <- system.time(v <- suppressWarnings(checks[[nm]]()))[["elapsed"]]
  ok <- is.na(v) && t0 < 30
  cat(sprintf("   %-22s -> %-6s in %5.2fs  %s\n", nm, format(v), t0,
              if (ok) "OK" else "FAIL"))
  if (!ok) bad <- c(bad, nm)
}

if (length(bad)) {
  cat("\n   FAILED on this architecture: ", paste(bad, collapse = ", "), "\n", sep = "")
  quit(status = 1L)
}
cat("\n   All guarded paths return NA promptly on ", machine, ".\n", sep = "")
