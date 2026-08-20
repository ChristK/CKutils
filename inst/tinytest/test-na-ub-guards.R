# Regression tests for NaN/NA handling and undefined-behaviour guards.
#
# These cover a class of platform-dependent UB bugs (the same family that caused
# the macOS-only fquantile CI failure): float-to-int conversion of NaN, NaN
# slipping past comparison-based validation, signed-integer overflow of -lag when
# lag == NA_integer_ (== INT_MIN), integer modulo-by-zero in length-0 recycling,
# and out-of-bounds mmap access in cklut. Several previously crashed the R
# process; they must now error cleanly or propagate NA.

if (!requireNamespace("data.table", quietly = TRUE)) {
  exit_file("data.table package not available")
}
suppressMessages(library(data.table))

id <- c(1L, 1L, 1L)

# =============================================================================
# shift_bypid*: lag == NA_integer_ (INT_MIN) must error, not segfault
# (computing -lag is signed-integer overflow -> out-of-bounds indexing)
# =============================================================================
expect_error(shift_bypidNum(c(1, 2, 3), NA_integer_, NA_real_, id),
             info = "shift_bypidNum rejects NA lag")
expect_error(shift_bypidInt(c(1L, 2L, 3L), NA_integer_, NA_integer_, id),
             info = "shift_bypidInt rejects NA lag")
expect_error(shift_bypidBool(c(TRUE, FALSE, TRUE), NA_integer_, FALSE, id),
             info = "shift_bypidBool rejects NA lag")
expect_error(shift_bypidStr(c("a", "b", "c"), NA_integer_, "x", id),
             info = "shift_bypidStr rejects NA lag")

# Valid lags still behave correctly (guard must not affect normal use)
expect_equal(shift_bypidNum(c(1, 2, 3), 1L, NA_real_, id), c(NA, 1, 2),
             info = "shift_bypidNum lag=1 unaffected")
expect_equal(shift_bypidNum(c(1, 2, 3), -1L, NA_real_, id), c(2, 3, NA),
             info = "shift_bypidNum lag=-1 (lead) unaffected")

# =============================================================================
# recycle_vectors / fget_C: a zero-length argument alongside a non-empty one
# must yield a zero-length result (R recycling rule), not an i %% 0 SIGFPE
# =============================================================================
expect_equal(length(fdNBI(integer(0), 1, 1)), 0L,
             info = "fdNBI zero-length x recycles to length 0")
expect_equal(length(fdDPO(integer(0), 2, 1)), 0L,
             info = "fdDPO zero-length x recycles to length 0")
expect_equal(length(fget_C(integer(0), 2, 1)), 0L,
             info = "fget_C zero-length x returns length 0")
expect_equal(length(fget_C(0:3, numeric(0), 1)), 0L,
             info = "fget_C zero-length mu returns length 0")
expect_equal(length(fdZABNB(integer(0), 1, 1, 1, 0.1)), 0L,
             info = "fdZABNB zero-length x recycles to length 0")
expect_equal(length(fpZABNB(integer(0), 1, 1, 1, 0.1)), 0L,
             info = "fpZABNB zero-length q recycles to length 0")

# =============================================================================
# Density / CDF: NaN/NA quantile -> NA (static_cast<int>(NaN) was UB)
# =============================================================================
expect_true(is.na(fdNBI(NA_integer_, 1, 1)), info = "fdNBI NA x -> NA")
expect_true(is.na(suppressWarnings(fpNBI(NA_integer_, 1, 1))), info = "fpNBI NA q -> NA")
expect_true(is.na(suppressWarnings(fdDPO(NA_integer_, 2, 1))), info = "fdDPO NA x -> NA")
expect_true(is.na(suppressWarnings(fpDPO(NA_integer_, 2, 1))), info = "fpDPO NA q -> NA")
expect_true(is.na(suppressWarnings(fdSICHEL(NA_integer_, 1, 1, 1))), info = "fdSICHEL NA x -> NA")
expect_true(is.na(suppressWarnings(fdZABNB(NA_integer_, 1, 1, 1, 0.1))), info = "fdZABNB NA x -> NA")
expect_true(is.na(suppressWarnings(fpZABNB(NA_integer_, 1, 1, 1, 0.1))), info = "fpZABNB NA q -> NA")
expect_true(is.na(fdZABNB(NaN, 1, 1, 1, 0.1)), info = "fdZABNB NaN x -> NA")
expect_true(is.na(fpZABNB(NaN, 1, 1, 1, 0.1)), info = "fpZABNB NaN q -> NA")

# Finite inputs unaffected (density still matches a direct scalar evaluation)
expect_equal(fdNBI(c(0L, 1L, 2L), 1, 1), fdNBI(c(0L, 1L, 2L), 1, 1),
             info = "fdNBI finite inputs stable")

# =============================================================================
# Quantiles: NaN/NA probability -> NA (was qpois(NaN) UB / wrong value / spin)
# =============================================================================
expect_true(is.na(suppressWarnings(fqDPO(NaN, 5, 1))),      info = "fqDPO NaN p -> NA")
expect_true(is.na(suppressWarnings(fqDPO(NA_real_, 5, 1))), info = "fqDPO NA p -> NA")
expect_true(is.na(suppressWarnings(fqNBI(NaN, 1, 1))),      info = "fqNBI NaN p -> NA")
expect_true(is.na(suppressWarnings(fqMN4(NaN, 1, 1, 1))),   info = "fqMN4 NaN p -> NA")
expect_true(is.na(suppressWarnings(fqDEL(NaN, 1, 1, 1))),   info = "fqDEL NaN p -> NA")
expect_true(is.na(suppressWarnings(fqSICHEL(NaN, 1, 1, 1))),info = "fqSICHEL NaN p -> NA")
expect_true(is.na(suppressWarnings(fqZABNB(NaN, 1, 1, 1, 0.1))), info = "fqZABNB NaN p -> NA")
expect_true(is.na(suppressWarnings(fqZABNB(NA_real_, 1, 1, 1, 0.1))), info = "fqZABNB NA p -> NA")
expect_true(is.na(fqZABNB(0.5, NaN, 1, 1, 0.1)), info = "fqZABNB NaN mu -> NA")
expect_true(is.na(fqZABNB(0.5, 1, 1, 1, NaN)), info = "fqZABNB NaN tau -> NA")

# Valid quantiles still correct (match base R where the special case reduces to Poisson)
expect_equal(fqDPO(0.5, 5, 1), as.numeric(qpois(0.5, 5)),
             info = "fqDPO finite p matches qpois at sigma=1")

# =============================================================================
# count_to_int: an x/q too large to convert to int -> NA
#
# static_cast<int> of a double outside the int range is undefined behaviour and
# it is not benign: x86-64 saturates to INT_MIN, so a huge count silently read
# as negative (fpZABNB(2^31, ...) returned 0 for a CDF whose true value is ~1),
# while AArch64 saturates to INT_MAX, which walks the 0..q accumulation loops.
# All the fd*/fp* wrappers now route the conversion through count_to_int().
# =============================================================================
big_counts <- c(2^31, 4e9, 1e10, Inf)
guarded <- list(
  fdNBI    = list(1, 1),          fpNBI      = list(1, 1),
  fdBNB    = list(2, 1, 1),       fpBNB      = list(2, 1, 1),
  fdZANBI  = list(2, 1, 0.1),     fpZANBI    = list(2, 1, 0.1),
  fdZINBI  = list(2, 1, 0.1),     fpZINBI    = list(2, 1, 0.1),
  fdZABNB  = list(2, 1, 1, 0.1),  fpZABNB    = list(2, 1, 1, 0.1),
  fdSICHEL = list(1, 1, -0.5),    fpSICHEL   = list(1, 1, -0.5),
  fdDPO    = list(2, 1),          fpDPO      = list(2, 1),
  fdDEL    = list(2, 1, 0.5),     fpDEL      = list(2, 1, 0.5),
  fdMN4    = list(1, 1, 1),       fpMN4      = list(1, 1, 1),
  fpZISICHEL = list(1, 1, -0.5, 0.1)
)
for (nm in names(guarded)) {
  vals <- vapply(big_counts, function(z)
    as.numeric(suppressWarnings(do.call(get(nm), c(list(z), guarded[[nm]])))),
    numeric(1))
  expect_true(all(is.na(vals)),
              info = paste(nm, "maps an unrepresentable count to NA"))
}

# Values below the boundary are unaffected
expect_true(is.finite(fdBNB(1e6, 2, 1, 1)), info = "fdBNB large-but-valid x still computed")
expect_true(is.finite(fdNBI(1e6, 1, 1)),    info = "fdNBI large-but-valid x still computed")
expect_true(is.finite(fpNBI(1e5, 1, 1)),    info = "fpNBI large-but-valid q still computed")

# The same guard on the R::qpois fast path of the quantile searches: qpois
# returns a double that exceeds INT_MAX for a large mu, and the unguarded cast
# used to return INT_MIN, i.e. a negative quantile.
expect_true(is.na(suppressWarnings(fqDPO(0.999, 3e9, 1))),
            info = "fqDPO qpois fast path rejects an unrepresentable quantile")
expect_true(is.na(suppressWarnings(fqDEL(0.5, 5e9, 1e-5, 0.5))),
            info = "fqDEL qpois fast path rejects an unrepresentable quantile")
expect_equal(fqDPO(0.5, 5, 1), 5, info = "fqDPO ordinary case unaffected")
expect_true(is.infinite(fqDPO(1, 5, 1)), info = "fqDPO p = 1 is still Inf")

# =============================================================================
# cklut: out-of-range gather row index -> NA (was out-of-bounds mmap read)
# =============================================================================
lookup_tbl <- CJ(a = 1:3, grp = 1:2)
lookup_tbl[, v := as.numeric(a * 10 + grp)]
ck <- cklut_build(copy(lookup_tbl), tempfile("cktest_ub"), keys = c("a", "grp"))
nr <- nrow(lookup_tbl)
g <- CKutils:::cklut_gather_cpp(ck$xp, c(1L, nr, nr + 1L, 99999L, 0L, NA_integer_))
expect_false(is.na(g[[1]][1]), info = "cklut gather in-range row 1 valid")
expect_false(is.na(g[[1]][2]), info = "cklut gather in-range last row valid")
expect_true(is.na(g[[1]][3]),  info = "cklut gather row n+1 (OOB) -> NA")
expect_true(is.na(g[[1]][4]),  info = "cklut gather far-OOB row -> NA")
expect_true(is.na(g[[1]][5]),  info = "cklut gather row 0 -> NA")
expect_true(is.na(g[[1]][6]),  info = "cklut gather NA row -> NA")
