# Supplementary tests for branches of the carry_* / carry_backward_decr C++
# helpers in src/misc_functions.cpp that need specific byref/recur/NA inputs.

# carry_forward, byref = TRUE, with NA in pid_mrk and NA in the previous value
# -> exercises the in-place NA-skip `continue` branch.
x1 <- c(7L, 7L, 7L, 7L)
pm1 <- c(TRUE, NA, FALSE, FALSE)         # NA marker at position 2
r1  <- carry_forward(x1, pm1, y = 7L, byref = TRUE)
expect_equal(x1, r1, info = "carry_forward byref returns the (mutated) input")
expect_true(is.integer(r1), info = "carry_forward byref NA-skip path runs")

# NA at x(i-1) makes carry_forward skip (continue) and leave x(i) untouched.
x1b <- c(NA_integer_, 5L, 5L)            # NA in x(0); i=1 -> skip
pm1b <- c(TRUE, FALSE, FALSE)
carry_forward(x1b, pm1b, y = 5L, byref = TRUE)
expect_true(is.na(x1b[1]), info = "carry_forward byref NA at x(i-1) -> skip")

# carry_forward_incr, byref = TRUE, recur = TRUE, NA-skip branch (NA marker)
xr <- c(5L, 6L, NA_integer_, 7L)
pmr <- c(TRUE, FALSE, FALSE, NA)         # NA marker at last position -> skip
carry_forward_incr(xr, pmr, recur = TRUE, y = 5L, byref = TRUE)
expect_true(is.integer(xr), info = "carry_forward_incr byref recur NA-skip runs")

# carry_forward_incr, byref = TRUE, recur = FALSE: NA at x(i-1) -> skip,
# otherwise continuous increment.
xi <- c(NA_integer_, 5L, 5L)
pmi <- c(TRUE, FALSE, FALSE)
carry_forward_incr(xi, pmi, recur = FALSE, y = 5L, byref = TRUE)
# i=1: prev is NA -> skip; i=2: prev(5)>=5 -> x(2)=6
expect_true(is.na(xi[1]), info = "carry_forward_incr non-recur NA at x(i-1) -> skip")
expect_equal(xi[3], 6L, info = "carry_forward_incr non-recur increments")

# carry_backward_decr: NA-skip (continue), the new-person back-fill branch, and
# the negative-clamp branch (out(i) < 0 -> 0).
xb  <- c(3L, 0L, NA_integer_, 2L, 1L)
pmb <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
rb  <- carry_backward_decr(xb, pmb, y = 0L)
expect_true(is.integer(rb) && length(rb) == 5L,
            info = "carry_backward_decr returns same-length integer vector")

# negative-clamp branch: values that would go below zero are clamped up to 0.
# The clamp applies to processed positions (i = n-1 .. 1); index 0 is not visited.
xneg <- c(-5L, -5L, -5L)
pmneg <- c(TRUE, FALSE, FALSE)
rneg <- carry_backward_decr(xneg, pmneg, y = -10L)
expect_true(any(rneg == 0L), info = "carry_backward_decr clamps negatives to 0")

# fclamp with inplace = TRUE AND a > b: exercises the in-place bound-swap branch.
xc <- c(-1, 0.5, 2, 5)
r_inplace_swap <- fclamp(xc, a = 3, b = 1, inplace = TRUE)   # a > b -> swapped to [1, 3]
expect_equal(xc, c(1, 1, 2, 3),
             info = "fclamp inplace with a > b swaps bounds and clamps in place")
expect_equal(r_inplace_swap, c(1, 1, 2, 3),
             info = "fclamp inplace returns the clamped vector")
