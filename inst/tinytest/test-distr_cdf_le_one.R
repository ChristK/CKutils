# A CDF is never above 1, an upper tail never negative, and a log upper tail never NaN.
# Some CDFs add densities, and rounding left the sum above 1 by ~1e-15: then 1 - cdf was
# negative (and log(1 - cdf) NaN). The value that is returned is clamped to <= 1; the running
# sums (fqDEL's, fpDEL's across elements) are not. Every case below failed before.
# The expectations compare with 1 and 0 exactly (<=, >=): no tolerance can hide a violation.
ok3 <- function(f, ...) {
  v  <- f(..., lower_tail = TRUE)
  up <- f(..., lower_tail = FALSE)
  lu <- f(..., lower_tail = FALSE, log_p = TRUE)
  list(cdf = all(v <= 1), up = all(up >= 0), lup = !anyNA(lu))
}
chk <- function(r, info) {
  expect_true(r$cdf, info = paste(info, "cdf <= 1"))
  expect_true(r$up,  info = paste(info, "upper tail >= 0"))
  expect_true(r$lup, info = paste(info, "log upper tail not NaN"))
}

chk(ok3(fpDPO, 90L, 3.3475323192455302, 4.9360886266044446), "DPO")
chk(ok3(fpDEL, 1200L, 5, 1.001e-4, 0.5), "DEL scalar")
chk(ok3(fpDEL, 0:60, 2.03065, 2.30919, 0.830551), "DEL vector")
chk(ok3(fpBCT, 55, 4.3673604550318759, 0.21248852961936127, 0.4968533537998811, 22.91528201087851), "BCT")
chk(ok3(fpBNB, 1000L, 90, 0.02, 0.02), "BNB")
chk(ok3(fpSICHEL, 0:400, 10, 0.2, -3), "SICHEL")
# the zero-inflated and zero-altered members inherit the clamp
chk(ok3(fpZIBNB, 1000L, 90, 0.02, 0.02, 0.1), "ZIBNB")
chk(ok3(fpZABNB, 1000L, 90, 0.02, 0.02, 0.1), "ZABNB")
chk(ok3(fpZISICHEL, 0:400, 10, 0.2, -3, 0.1), "ZISICHEL")
chk(ok3(fpZANBI, 288L, 0.1577427181, 0.1893024861, 0.01238402824), "ZANBI")

# exact values of the reported cases
expect_identical(fpDEL(1200L, 5, 1.001e-4, 0.5), 1)
expect_identical(fpDEL(1200L, 5, 1.001e-4, 0.5, lower_tail = FALSE), 0)

# the clamp is on the returned value only: the vector fpDEL's running sum continues past
# an element that was clamped
x <- 0:60
v <- fpDEL(x, 2.03065, 2.30919, 0.830551)
expect_true(all(diff(v) >= 0))
expect_identical(v, vapply(x, function(i) fpDEL(i, 2.03065, 2.30919, 0.830551), 0))

# a non-finite parameter returns NaN at once: the lower loop used to add NaN terms up to q
tm <- system.time(r <- fpDPO(1e7L, Inf, 2))[["elapsed"]]
expect_true(is.nan(r))
expect_true(tm < 1, info = "fpDPO(1e7, Inf, 2) returns at once")
expect_true(is.nan(fpDPO(1e7L, 5, Inf)))
