# Architecture-sensitive tests for the double -> int conversion guard.
#
# Converting a double that lies outside the int range is undefined behaviour,
# and crucially it is undefined in a PLATFORM-DEPENDENT way:
#
#   x86-64 (cvttsd2si)  saturates to INT_MIN for any out-of-range value, so a
#                       huge count silently read as NEGATIVE. fpZABNB(2^31, ...)
#                       returned 0 for a CDF whose true value is ~1.
#   AArch64 (fcvtzs)    saturates toward the sign, so a large POSITIVE double
#                       becomes INT_MAX. That value then enters the
#                       `for (int i = 0; i <= q; i++)` accumulation in the CDF
#                       kernels, which cannot terminate (i++ overflows at
#                       INT_MAX) -- a hang rather than a wrong number.
#
# count_to_int() in recycling_helpers.h removes the conversion, so every
# platform must now return NA. These tests exist to be run on BOTH
# architectures in CI (macos-latest and ubuntu-24.04-arm are arm64; the
# ubuntu/windows jobs are x86-64) and they assert exact values, so a divergence
# between architectures shows up as a failure on one of them rather than as a
# silently different answer.
#
# The elapsed-time assertions are not performance tests. They separate the two
# failure modes: a wrong VALUE and a NON-TERMINATING loop both need catching,
# and a value check alone cannot distinguish "returned the wrong number
# quickly" from "never returned". The bounds are enormously generous (seconds,
# against microseconds for a correct answer and hours for the overflow loop),
# so they cannot flake on a slow or loaded runner.

arch <- paste0(Sys.info()[["machine"]], " / ", R.version$platform)
is_arm <- grepl("aarch64|arm64", arch, ignore.case = TRUE)
cat("test-arch-int-conversion: running on ", arch,
    if (is_arm) "  [AArch64]" else "  [not AArch64]", "\n", sep = "")

# Values that used to reach the unguarded cast. INT_MAX itself is included
# because it is representable yet still breaks the kernels: the CDF loops
# overflow their counter there, and the density kernels evaluate lgamma(x + 1).
unrepresentable <- c(2^31, 4e9, 1e10, 1e300, Inf, .Machine$integer.max)

# Every density and CDF, with a valid parameter set for each.
guarded <- list(
  fdNBI      = list(1, 1),           fpNBI      = list(1, 1),
  fdBNB      = list(2, 1, 1),        fpBNB      = list(2, 1, 1),
  fdZANBI    = list(2, 1, 0.1),      fpZANBI    = list(2, 1, 0.1),
  fdZINBI    = list(2, 1, 0.1),      fpZINBI    = list(2, 1, 0.1),
  fdZABNB    = list(2, 1, 1, 0.1),   fpZABNB    = list(2, 1, 1, 0.1),
  fdSICHEL   = list(1, 1, -0.5),     fpSICHEL   = list(1, 1, -0.5),
  fdDPO      = list(2, 1),           fpDPO      = list(2, 1),
  fdDEL      = list(2, 1, 0.5),      fpDEL      = list(2, 1, 0.5),
  fdMN4      = list(1, 1, 1),        fpMN4      = list(1, 1, 1),
  fpZISICHEL = list(1, 1, -0.5, 0.1)
)

for (nm in names(guarded)) {
  el <- system.time({
    vals <- vapply(unrepresentable, function(z)
      as.numeric(suppressWarnings(do.call(get(nm), c(list(z), guarded[[nm]])))),
      numeric(1))
  })[["elapsed"]]

  # Value: NA on every architecture. On x86 an unguarded build returned 0 (fp)
  # or NaN (fd) here; on AArch64 it would not have returned at all.
  expect_true(all(is.na(vals)),
              info = paste0(nm, ": unrepresentable count -> NA on ", arch,
                            " (got ", paste(format(vals), collapse = ", "), ")"))

  # Termination: an AArch64 saturation to INT_MAX would send the 0..q loop on a
  # two-billion-iteration walk. Anything past a few seconds means the guard is
  # not in force on this platform.
  expect_true(el < 30,
              info = paste0(nm, ": returns promptly on ", arch,
                            " (took ", format(el), "s; a saturated INT_MAX loop would not finish)"))
}

# The quantile searches take a Poisson fast path that casts R::qpois(), whose
# double exceeds INT_MAX for a large mu. Same guard, same expectation.
expect_true(is.na(suppressWarnings(fqDPO(0.999, 3e9, 1))),
            info = paste("fqDPO qpois fast path -> NA on", arch))
expect_true(is.na(suppressWarnings(fqDEL(0.5, 5e9, 1e-5, 0.5))),
            info = paste("fqDEL qpois fast path -> NA on", arch))

# Counts below the cap must still be computed, identically on every platform.
# These are exact values, so an architecture that diverged would fail here.
expect_equal(fdNBI(0L, 1, 1), 0.5,
             info = paste("fdNBI(0) is exact on", arch))
expect_true(is.finite(fdBNB(1e6, 2, 1, 1)),
            info = paste("fdBNB large-but-valid x still computed on", arch))
expect_true(is.finite(fpNBI(1e5, 1, 1)),
            info = paste("fpNBI large-but-valid q still computed on", arch))
expect_equal(fpZABNB(0, 2, 1, 1, 0.1), 0.1,
             info = paste("fpZABNB(0) == tau exactly on", arch))

# The boundary itself: INT_MAX - 1 is the largest count the kernels can take,
# and must still work; INT_MAX must not.
expect_true(is.finite(fdZABNB(.Machine$integer.max - 1, 2, 1, 1, 0.1)),
            info = paste("fdZABNB accepts INT_MAX - 1 on", arch))
expect_true(is.na(fdZABNB(.Machine$integer.max, 2, 1, 1, 0.1)),
            info = paste("fdZABNB rejects INT_MAX on", arch))
expect_true(is.na(fpZABNB(.Machine$integer.max, 2, 1, 1, 0.1)),
            info = paste("fpZABNB rejects INT_MAX on", arch))

# Negative and NaN counts are unaffected by the guard: the count distributions
# still error on x < 0, and MN4, whose support is the four categories 1:4,
# still returns a density of 0 for an out-of-category value rather than NA.
expect_error(fdBNB(-1, 2, 1, 1), "x must be >=0",
             info = paste("fdBNB still rejects a negative x on", arch))
expect_equal(suppressWarnings(fdMN4(-1, 1, 1, 1)), 0,
             info = paste("fdMN4 out-of-category x is still 0, not NA, on", arch))
expect_true(is.na(fdBNB(NaN, 2, 1, 1)),
            info = paste("fdBNB NaN x -> NA on", arch))
