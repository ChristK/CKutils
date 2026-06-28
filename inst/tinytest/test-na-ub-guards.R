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

# =============================================================================
# Density / CDF: NaN/NA quantile -> NA (static_cast<int>(NaN) was UB)
# =============================================================================
expect_true(is.na(fdNBI(NA_integer_, 1, 1)), info = "fdNBI NA x -> NA")
expect_true(is.na(suppressWarnings(fpNBI(NA_integer_, 1, 1))), info = "fpNBI NA q -> NA")
expect_true(is.na(suppressWarnings(fdDPO(NA_integer_, 2, 1))), info = "fdDPO NA x -> NA")
expect_true(is.na(suppressWarnings(fpDPO(NA_integer_, 2, 1))), info = "fpDPO NA q -> NA")
expect_true(is.na(suppressWarnings(fdSICHEL(NA_integer_, 1, 1, 1))), info = "fdSICHEL NA x -> NA")

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

# Valid quantiles still correct (match base R where the special case reduces to Poisson)
expect_equal(fqDPO(0.5, 5, 1), as.numeric(qpois(0.5, 5)),
             info = "fqDPO finite p matches qpois at sigma=1")

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
