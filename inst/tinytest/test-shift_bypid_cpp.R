# Direct tests for the exported C++ shift helpers in src/shift_bypid.cpp.
# The R wrapper shift_bypid() validates/short-circuits inputs (empty vectors,
# length mismatches, scalar replace), so several defensive branches in the C++
# functions are only reachable by calling them directly.

id  <- c(1L, 1L, 1L, 2L, 2L, 2L)
num <- c(10, 20, 30, 40, 50, 60)

# -----------------------------------------------------------------------------
# shift_bypidNum
# -----------------------------------------------------------------------------
expect_equal(shift_bypidNum(numeric(0), 1L, NA_real_, integer(0)), numeric(0),
             info = "shift_bypidNum: empty input returns empty")
expect_error(shift_bypidNum(num, 1L, NA_real_, c(1L, 1L)), pattern = "must match",
             info = "shift_bypidNum: length mismatch errors")
expect_equal(shift_bypidNum(num, 1L, NA_real_, id), c(NA, 10, 20, NA, 40, 50),
             info = "shift_bypidNum: positive lag within groups")
expect_equal(shift_bypidNum(num, -1L, NA_real_, id), c(20, 30, NA, 50, 60, NA),
             info = "shift_bypidNum: negative lag (lead) within groups")
expect_equal(shift_bypidNum(num, 10L, -1, id), rep(-1, 6),
             info = "shift_bypidNum: |lag| >= n fills with replace (positive)")
expect_equal(shift_bypidNum(num, -10L, -1, id), rep(-1, 6),
             info = "shift_bypidNum: |lag| >= n fills with replace (negative)")

# -----------------------------------------------------------------------------
# shift_bypidInt (preserves factor attributes)
# -----------------------------------------------------------------------------
ints <- c(10L, 20L, 30L, 40L, 50L, 60L)
expect_equal(shift_bypidInt(integer(0), 1L, NA_integer_, integer(0)), integer(0),
             info = "shift_bypidInt: empty input returns empty")
expect_error(shift_bypidInt(ints, 1L, NA_integer_, c(1L, 1L)), pattern = "must match",
             info = "shift_bypidInt: length mismatch errors")
expect_equal(shift_bypidInt(ints, 1L, NA_integer_, id), c(NA, 10L, 20L, NA, 40L, 50L),
             info = "shift_bypidInt: positive lag")
expect_equal(shift_bypidInt(ints, -1L, NA_integer_, id), c(20L, 30L, NA, 50L, 60L, NA),
             info = "shift_bypidInt: negative lag (lead)")

# factor input: normal-lag branch preserves levels + class
fac <- factor(c("a", "b", "c", "a", "b", "c"), levels = c("a", "b", "c"))
res_fac <- shift_bypidInt(fac, 1L, NA_integer_, id)
expect_equal(levels(res_fac), c("a", "b", "c"),
             info = "shift_bypidInt: factor levels preserved (normal lag)")
expect_true(is.factor(res_fac), info = "shift_bypidInt: result is a factor (normal lag)")

# factor input: |lag| >= n branch also preserves factor attributes
res_fac_big <- shift_bypidInt(fac, 10L, NA_integer_, id)
expect_true(is.factor(res_fac_big),
            info = "shift_bypidInt: factor preserved on |lag| >= n branch")
expect_true(all(is.na(res_fac_big)),
            info = "shift_bypidInt: |lag| >= n yields all replace")

# non-factor integer, |lag| >= n
expect_equal(shift_bypidInt(ints, -10L, -1L, id), rep(-1L, 6),
             info = "shift_bypidInt: |lag| >= n fills with replace (negative)")

# -----------------------------------------------------------------------------
# shift_bypidBool
# -----------------------------------------------------------------------------
bools <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
expect_equal(shift_bypidBool(logical(0), 1L, FALSE, integer(0)), logical(0),
             info = "shift_bypidBool: empty input returns empty")
expect_error(shift_bypidBool(bools, 1L, FALSE, c(1L, 1L)), pattern = "must match",
             info = "shift_bypidBool: length mismatch errors")
expect_error(shift_bypidBool(bools, 1L, logical(0), id), pattern = "at least one element",
             info = "shift_bypidBool: empty replace errors")
expect_equal(shift_bypidBool(bools, 1L, FALSE, id),
             c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE),
             info = "shift_bypidBool: positive lag")
expect_equal(shift_bypidBool(bools, -1L, FALSE, id),
             c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
             info = "shift_bypidBool: negative lag (lead)")
expect_equal(shift_bypidBool(bools, 9L, TRUE, id), rep(TRUE, 6),
             info = "shift_bypidBool: |lag| >= n fills with replace")

# -----------------------------------------------------------------------------
# shift_bypidStr
# -----------------------------------------------------------------------------
chars <- c("a", "b", "c", "d", "e", "f")
expect_equal(shift_bypidStr(character(0), 1L, "z", integer(0)), character(0),
             info = "shift_bypidStr: empty input returns empty")
expect_error(shift_bypidStr(chars, 1L, "z", c(1L, 1L)), pattern = "must match",
             info = "shift_bypidStr: length mismatch errors")
expect_equal(shift_bypidStr(chars, 1L, "miss", id),
             c("miss", "a", "b", "miss", "d", "e"),
             info = "shift_bypidStr: positive lag")
expect_equal(shift_bypidStr(chars, -1L, "miss", id),
             c("b", "c", "miss", "e", "f", "miss"),
             info = "shift_bypidStr: negative lag (lead)")
expect_equal(shift_bypidStr(chars, 8L, "miss", id), rep("miss", 6),
             info = "shift_bypidStr: |lag| >= n fills with replace")
