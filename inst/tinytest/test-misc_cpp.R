# Branch / edge-case tests for the exported C++ functions in src/misc_functions.cpp
# Complements test-misc_functions.R (happy paths) by focusing on the
# currently-untested branches: empty / all-NA / single-element inputs,
# na_rm TRUE/FALSE, inplace/byref TRUE/FALSE, bound swaps, recycling,
# NA handling and stop() error branches.

if (!requireNamespace("data.table", quietly = TRUE)) {
  exit_file("data.table package not available")
}
suppressMessages(library(data.table))

# =============================================================================
# fquantile
# =============================================================================

# Empty probs -> length-0 numeric, regardless of x
expect_equal(fquantile(c(1, 2, 3), numeric(0)), numeric(0),
             info = "fquantile empty probs returns numeric(0)")
expect_equal(length(fquantile(c(1, 2, 3), numeric(0))), 0L,
             info = "fquantile empty probs length 0")

# Empty x -> NA for every prob
res_empty_x <- fquantile(numeric(0), c(0.25, 0.5, 0.75))
expect_equal(length(res_empty_x), 3L, info = "fquantile empty x length matches probs")
expect_true(all(is.na(res_empty_x)), info = "fquantile empty x all NA")

# All-NA x -> NA for every prob (this branch fires before na_rm)
res_allna <- fquantile(c(NA_real_, NA_real_), c(0.25, 0.5))
expect_equal(length(res_allna), 2L, info = "fquantile all-NA length matches probs")
expect_true(all(is.na(res_allna)), info = "fquantile all-NA returns all NA")

# na_rm = TRUE removing NAs and leaving a single element -> that value everywhere
res_single <- fquantile(c(NA, 7, NA), c(0.1, 0.9), na_rm = TRUE)
expect_equal(res_single, c(7, 7), info = "fquantile single non-NA element repeats value")

# na_rm = FALSE with NA present (but not all NA) keeps NA -> NA result
res_no_rm <- fquantile(c(1, 2, NA, 4, 5), c(0.5), na_rm = FALSE)
expect_true(is.na(res_no_rm), info = "fquantile na_rm=FALSE propagates NA")

# Normal interpolation (type 7) on integer-spaced data
expect_equal(fquantile(c(1, 2, 3, 4, 5), c(0.25, 0.5, 0.75)), c(2, 3, 4),
             info = "fquantile normal interpolation correct")
# Non-trivial interpolation (index falls between observations)
expect_equal(fquantile(c(0, 10), c(0.25)), 2.5,
             info = "fquantile interpolates between two points")

# =============================================================================
# fquantile_byid
# =============================================================================

# Empty x -> list of length 1 + length(q), with empty id slot
res_bid_emptyx <- fquantile_byid(numeric(0), c(0.5), character(0))
expect_true(is.list(res_bid_emptyx), info = "fquantile_byid empty x returns list")
expect_equal(length(res_bid_emptyx), 2L, info = "fquantile_byid empty x list length 1+q")
expect_equal(length(res_bid_emptyx[[1]]), 0L, info = "fquantile_byid empty x empty id slot")

# Empty q -> list of length 1, just the empty id slot
res_bid_emptyq <- fquantile_byid(c(1, 2, 3), numeric(0), c("A", "A", "A"))
expect_true(is.list(res_bid_emptyq), info = "fquantile_byid empty q returns list")
expect_equal(length(res_bid_emptyq), 1L, info = "fquantile_byid empty q list length 1")

# Length mismatch between x and id -> stop()
expect_error(fquantile_byid(c(1, 2, 3), c(0.5), c("A", "B")),
             pattern = "Length of 'x' and 'id' must match",
             info = "fquantile_byid length mismatch errors")

# Multiple groups, rounding = FALSE
res_bid <- fquantile_byid(c(1, 2, 3, 4, 5, 6), c(0.5),
                          c("A", "A", "A", "B", "B", "B"), rounding = FALSE)
expect_equal(res_bid[[1]], c("A", "B"), info = "fquantile_byid multi-group ids")
expect_equal(res_bid[[2]], c(2, 5), info = "fquantile_byid multi-group medians")

# Multiple groups, rounding = TRUE (medians are integers here, so round is no-op
# numerically but exercises the rounding branch)
res_bid_round <- fquantile_byid(c(1, 2, 4, 4, 5, 7), c(0.5),
                                c("A", "A", "A", "B", "B", "B"), rounding = TRUE)
expect_equal(res_bid_round[[1]], c("A", "B"), info = "fquantile_byid rounding ids")
expect_equal(res_bid_round[[2]], round(c(2, 5), 0),
             info = "fquantile_byid rounding=TRUE rounds quantiles")

# Single group
res_bid_single <- fquantile_byid(c(2, 4, 6), c(0.5), c("G", "G", "G"))
expect_equal(res_bid_single[[1]], "G", info = "fquantile_byid single group id")
expect_equal(res_bid_single[[2]], 4, info = "fquantile_byid single group median")

# =============================================================================
# count_if / prop_if
# =============================================================================

# Without NA
expect_equal(count_if(c(TRUE, FALSE, TRUE, TRUE)), 3L, info = "count_if no NA")
expect_equal(prop_if(c(TRUE, FALSE, TRUE, TRUE)), 0.75, info = "prop_if no NA")

# With NA, na_rm = FALSE: NA counts toward denominator for prop_if but is not TRUE
expect_equal(count_if(c(TRUE, NA, FALSE, TRUE), na_rm = FALSE), 2L,
             info = "count_if NA na_rm=FALSE counts only TRUE")
expect_equal(prop_if(c(TRUE, NA, FALSE, TRUE), na_rm = FALSE), 0.5,
             info = "prop_if NA na_rm=FALSE keeps NA in denominator")

# With NA, na_rm = TRUE: NA dropped before counting
expect_equal(count_if(c(TRUE, NA, FALSE, TRUE), na_rm = TRUE), 2L,
             info = "count_if NA na_rm=TRUE")
expect_equal(prop_if(c(TRUE, NA, FALSE, TRUE), na_rm = TRUE), 2 / 3,
             info = "prop_if NA na_rm=TRUE drops NA from denominator")

# count_if on empty vector -> 0
expect_equal(count_if(logical(0)), 0L, info = "count_if empty -> 0")
# prop_if on empty vector -> 0/0 = NaN
expect_true(is.nan(prop_if(logical(0))), info = "prop_if empty -> NaN")

# =============================================================================
# fclamp
# =============================================================================

# inplace = FALSE (default): returns clamped copy, leaves input untouched
x_fc <- c(-1, 0.5, 2)
expect_equal(fclamp(x_fc, a = 0, b = 1), c(0, 0.5, 1),
             info = "fclamp inplace=FALSE clamps")
expect_equal(x_fc, c(-1, 0.5, 2), info = "fclamp inplace=FALSE leaves input unchanged")

# a > b swap branch (bounds get swapped to [0, 1])
expect_equal(fclamp(c(-1, 0.5, 2), a = 1, b = 0), c(0, 0.5, 1),
             info = "fclamp swaps a>b bounds")

# NA element preserved as NA
expect_equal(fclamp(c(NA, 0.5, 2), a = 0, b = 1), c(NA, 0.5, 1),
             info = "fclamp keeps NA")

# Recycling of a / b against longer x
expect_equal(fclamp(c(-5, 5, 0.5), a = 0, b = c(1, 2, 3)), c(0, 2, 0.5),
             info = "fclamp recycles scalar a against vector b")

# inplace = TRUE: mutates x by reference
x_fc_ip <- c(-1, 0.5, 2)
ret_ip <- fclamp(x_fc_ip, a = 0, b = 1, inplace = TRUE)
expect_equal(x_fc_ip, c(0, 0.5, 1), info = "fclamp inplace=TRUE mutates input")
expect_equal(ret_ip, c(0, 0.5, 1), info = "fclamp inplace=TRUE returns clamped vector")

# inplace = TRUE with length mismatch after recycling -> stop()
x_fc_bad <- c(0.5)
expect_error(fclamp(x_fc_bad, a = c(0, 0, 0), b = c(1, 1, 1), inplace = TRUE),
             pattern = "In-place operation requires consistent vector lengths",
             info = "fclamp inplace length mismatch errors")

# =============================================================================
# fclamp_int
# =============================================================================

# inplace = FALSE, a > b swap, and NA element
expect_equal(fclamp_int(c(-1L, 3L, NA, 7L), a = 5L, b = 0L),
             c(0L, 3L, NA, 5L),
             info = "fclamp_int swaps bounds and keeps NA (inplace=FALSE)")

# inplace = TRUE mutates input
x_fci <- c(-1L, 3L, 7L)
fclamp_int(x_fci, a = 0L, b = 5L, inplace = TRUE)
expect_equal(x_fci, c(0L, 3L, 5L), info = "fclamp_int inplace=TRUE mutates input")

# inplace = FALSE leaves input unchanged
x_fci2 <- c(-1L, 3L, 7L)
out_fci2 <- fclamp_int(x_fci2, a = 0L, b = 5L, inplace = FALSE)
expect_equal(out_fci2, c(0L, 3L, 5L), info = "fclamp_int inplace=FALSE clamps")
expect_equal(x_fci2, c(-1L, 3L, 7L), info = "fclamp_int inplace=FALSE unchanged input")

# =============================================================================
# fequal
# =============================================================================

# Equal within tolerance, with NA dropped by na_omit
expect_true(as.logical(fequal(c(1.0, NA, 1.00001), 0.001)),
            info = "fequal equal-within-tol with NA")
# Not equal -> FALSE
expect_false(as.logical(fequal(c(1.0, NA, 2.0), 0.001)),
             info = "fequal unequal returns FALSE")
# All-NA -> na_omit empties -> loop skipped -> TRUE (vacuously equal)
expect_true(as.logical(fequal(c(NA_real_, NA_real_), 0.001)),
            info = "fequal all-NA returns TRUE (vacuous)")
# Exactly equal values
expect_true(as.logical(fequal(c(3.0, 3.0, 3.0), 0.0)),
            info = "fequal identical values, zero tol")

# =============================================================================
# fnormalise
# =============================================================================

# Normal case scales to [0, 1]
expect_equal(fnormalise(c(10, 20, 30)), c(0, 0.5, 1), info = "fnormalise scales 0-1")
# NA: min/max computed on na_omit; NA element stays NA, others scaled
res_fn_na <- fnormalise(c(2, NA, 6, 10))
expect_equal(res_fn_na, c(0, NA, 0.5, 1), info = "fnormalise keeps NA, scales rest")

# =============================================================================
# lin_interpolation
# =============================================================================

expect_equal(lin_interpolation(c(1.5), c(1), c(2), c(10), c(20)), 15,
             info = "lin_interpolation midpoint")
expect_equal(lin_interpolation(c(1.5, 2.5), c(1, 2), c(2, 3), c(10, 20), c(20, 30)),
             c(15, 25), info = "lin_interpolation vectorised")
# Extrapolation beyond x1 follows the same line
expect_equal(lin_interpolation(c(3), c(1), c(2), c(10), c(20)), 30,
             info = "lin_interpolation extrapolates linearly")

# =============================================================================
# carry_forward (byref TRUE/FALSE, empty, NA, person boundaries)
# =============================================================================

# Empty input
expect_equal(length(carry_forward(integer(0), logical(0), 1L, FALSE)), 0L,
             info = "carry_forward empty input")

# byref = FALSE returns new vector and does NOT mutate the original
x_cf <- c(1L, 0L, 2L, 0L, 1L)
pid_cf <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
out_cf <- carry_forward(x_cf, pid_cf, 1L, FALSE)
expect_equal(out_cf, c(1L, 1L, 1L, 0L, 1L), info = "carry_forward byref=FALSE result")
expect_equal(x_cf, c(1L, 0L, 2L, 0L, 1L), info = "carry_forward byref=FALSE no mutation")

# byref = TRUE mutates in place
x_cf_ip <- c(1L, 0L, 2L, 0L, 1L)
carry_forward(x_cf_ip, pid_cf, 1L, TRUE)
expect_equal(x_cf_ip, c(1L, 1L, 1L, 0L, 1L), info = "carry_forward byref=TRUE mutates")

# NA in x at a position: position with NA pid/prev-x is skipped, but carry
# continues once a valid value is set. Here x(0)=1==y propagates forward.
x_cf_na <- c(1L, NA, 1L, 0L)
expect_equal(carry_forward(x_cf_na, c(TRUE, FALSE, FALSE, FALSE), 1L, FALSE),
             c(1L, 1L, 1L, 1L), info = "carry_forward NA handling / propagation")

# NA in pid_mrk skips that position
x_cf_pidna <- c(1L, 0L, 0L)
expect_equal(carry_forward(x_cf_pidna, c(TRUE, NA, FALSE), 1L, FALSE),
             c(1L, 0L, 0L), info = "carry_forward NA pid_mrk skips position")

# Person boundary (pid TRUE) stops the carry
x_cf_b <- c(1L, 0L)
expect_equal(carry_forward(x_cf_b, c(TRUE, TRUE), 1L, FALSE), c(1L, 0L),
             info = "carry_forward person boundary blocks carry")

# =============================================================================
# carry_forward_incr (recur TRUE/FALSE, byref TRUE/FALSE, empty, NA)
# =============================================================================

# Empty input
expect_equal(length(carry_forward_incr(integer(0), logical(0), FALSE, 1L, FALSE)), 0L,
             info = "carry_forward_incr empty input")

pid_cfi <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
             TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
x_cfi <- c(0L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L)

# Non-recursive: increment continuously once previous >= y
out_nonrec <- carry_forward_incr(x_cfi, pid_cfi, FALSE, 1L, FALSE)
expect_equal(out_nonrec, c(0L, 0L, 1L, 2L, 3L, 4L, 0L, 1L, 2L, 3L, 4L, 5L),
             info = "carry_forward_incr non-recursive")

# Recursive: increment only when both current >= y AND previous >= y
out_rec <- carry_forward_incr(x_cfi, pid_cfi, TRUE, 1L, FALSE)
expect_equal(out_rec, c(0L, 0L, 1L, 0L, 1L, 2L, 0L, 1L, 2L, 0L, 1L, 2L),
             info = "carry_forward_incr recursive")

# byref = TRUE mutates in place (non-recursive)
x_cfi_ip <- c(x_cfi)
carry_forward_incr(x_cfi_ip, pid_cfi, FALSE, 1L, TRUE)
expect_equal(x_cfi_ip, c(0L, 0L, 1L, 2L, 3L, 4L, 0L, 1L, 2L, 3L, 4L, 5L),
             info = "carry_forward_incr byref=TRUE non-recursive mutates")

# byref = TRUE mutates in place (recursive)
x_cfi_ip2 <- c(x_cfi)
carry_forward_incr(x_cfi_ip2, pid_cfi, TRUE, 1L, TRUE)
expect_equal(x_cfi_ip2, c(0L, 0L, 1L, 0L, 1L, 2L, 0L, 1L, 2L, 0L, 1L, 2L),
             info = "carry_forward_incr byref=TRUE recursive mutates")

# NA handling, non-recursive: NA in previous element skips position i
x_cfi_na <- c(NA, 0L, 0L)
expect_equal(carry_forward_incr(x_cfi_na, c(TRUE, FALSE, FALSE), FALSE, 1L, FALSE),
             c(NA, 0L, 0L),
             info = "carry_forward_incr non-recursive NA in prev skips")

# NA handling, recursive: NA in current element skips
x_cfi_na2 <- c(1L, 2L, NA, 2L)
expect_equal(carry_forward_incr(x_cfi_na2, c(TRUE, FALSE, FALSE, FALSE), TRUE, 1L, FALSE),
             c(1L, 2L, NA, 2L),
             info = "carry_forward_incr recursive NA in current skips")

# =============================================================================
# carry_backward_decr (empty, NA, threshold, person boundary, no-negatives)
# =============================================================================

# Empty input
expect_equal(length(carry_backward_decr(integer(0), logical(0), 0L)), 0L,
             info = "carry_backward_decr empty input")

# Basic backward decrement (default threshold y = 0)
x_cb <- c(0L, 0L, 5L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
pid_cb <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
            TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
expect_equal(carry_backward_decr(x_cb, pid_cb),
             c(3L, 4L, 5L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
             info = "carry_backward_decr default threshold")

# Higher threshold suppresses propagation
expect_equal(carry_backward_decr(x_cb, pid_cb, 6L), x_cb,
             info = "carry_backward_decr threshold above values leaves unchanged")

# Never goes below 0
x_cb_neg <- c(0L, 1L, 1L, 1L, 0L)
pid_cb_neg <- c(TRUE, FALSE, FALSE, TRUE, FALSE)
expect_equal(carry_backward_decr(x_cb_neg, pid_cb_neg),
             c(0L, 0L, 1L, 1L, 0L),
             info = "carry_backward_decr clamps at 0")

# NA in x at position i is skipped
x_cb_na <- c(0L, NA, 3L, 0L)
expect_equal(carry_backward_decr(x_cb_na, c(TRUE, FALSE, FALSE, FALSE)),
             c(1L, 2L, 3L, 0L),
             info = "carry_backward_decr skips NA at position i")

# =============================================================================
# mk_new_simulant_markers (empty, single, NA, boundaries)
# =============================================================================

expect_equal(length(mk_new_simulant_markers(integer(0))), 0L,
             info = "mk_new_simulant_markers empty input")
expect_equal(mk_new_simulant_markers(1L), TRUE,
             info = "mk_new_simulant_markers single element TRUE")
expect_equal(mk_new_simulant_markers(c(1L, 1L, 2L, 3L, 3L)),
             c(TRUE, FALSE, TRUE, TRUE, FALSE),
             info = "mk_new_simulant_markers basic boundaries")
# NA treated as new simulant; the value following an NA is also a new simulant
expect_equal(mk_new_simulant_markers(c(1L, 1L, NA, NA, 2L)),
             c(TRUE, FALSE, TRUE, TRUE, TRUE),
             info = "mk_new_simulant_markers NA handling")
# First element NA -> previous_pid NA branch
expect_equal(mk_new_simulant_markers(c(NA, 5L, 5L)),
             c(TRUE, TRUE, FALSE),
             info = "mk_new_simulant_markers leading NA")

# =============================================================================
# identify_longdead (empty, single, NA, boundaries)
# =============================================================================

expect_equal(length(identify_longdead(integer(0), logical(0))), 0L,
             info = "identify_longdead empty input")
expect_equal(identify_longdead(5L, TRUE), FALSE,
             info = "identify_longdead single element")
expect_equal(identify_longdead(c(0L, 1L, 0L, 1L, 0L),
                               c(TRUE, FALSE, FALSE, TRUE, FALSE)),
             c(FALSE, FALSE, TRUE, FALSE, TRUE),
             info = "identify_longdead basic")
# NA in pid or prev-x skips position
expect_equal(identify_longdead(c(1L, 1L, 1L),
                               c(TRUE, NA, FALSE)),
             c(FALSE, FALSE, TRUE),
             info = "identify_longdead NA pid skips")
expect_equal(identify_longdead(c(NA, 1L, 1L),
                               c(TRUE, FALSE, FALSE)),
             c(FALSE, FALSE, TRUE),
             info = "identify_longdead NA prev-x skips")

# =============================================================================
# identify_invitees (empty, NA -> NA, eligibility / frequency / probability)
# =============================================================================

expect_equal(length(identify_invitees(integer(0), integer(0), numeric(0),
                                       integer(0), logical(0))), 0L,
             info = "identify_invitees empty input")

# Deterministic probability 1 with eligible + frequency met -> all invited
inv_all <- identify_invitees(c(1L, 1L, 1L),
                             c(0L, 0L, 0L),
                             c(1.0, 1.0, 1.0),
                             c(0L, 0L, 0L),
                             c(TRUE, FALSE, FALSE))
expect_equal(inv_all, c(1L, 1L, 1L),
             info = "identify_invitees prob=1, eligible, freq met -> invite")

# Probability 0 -> never invited
inv_none <- identify_invitees(c(1L, 1L, 1L),
                              c(0L, 0L, 0L),
                              c(0.0, 0.0, 0.0),
                              c(0L, 0L, 0L),
                              c(TRUE, FALSE, FALSE))
expect_equal(inv_none, c(0L, 0L, 0L),
             info = "identify_invitees prob=0 -> no invites")

# Ineligible -> never invited even at prob 1
inv_inelig <- identify_invitees(c(0L, 0L, 0L),
                                c(0L, 0L, 0L),
                                c(1.0, 1.0, 1.0),
                                c(0L, 0L, 0L),
                                c(TRUE, FALSE, FALSE))
expect_equal(inv_inelig, c(0L, 0L, 0L),
             info = "identify_invitees ineligible -> no invites")

# prev_inv = 1 resets counter to 0; with freq 1 the next year is blocked
inv_freq <- identify_invitees(c(1L, 1L),
                              c(0L, 1L),
                              c(1.0, 1.0),
                              c(5L, 5L),
                              c(TRUE, FALSE))
# Year 1: new person, counter=1000 >= freq -> invite (1)
# Year 2: prev_inv==1 -> counter=0 < freq(5) -> not invited
expect_equal(inv_freq, c(1L, 0L),
             info = "identify_invitees frequency interval respected")

# NA in any critical input -> NA output for that position
inv_na <- identify_invitees(c(1L, NA),
                            c(0L, 0L),
                            c(1.0, 1.0),
                            c(1L, 1L),
                            c(TRUE, FALSE))
expect_true(is.na(inv_na[2]), info = "identify_invitees NA input -> NA output")

# =============================================================================
# hc_effect (empty, NA, boundaries) -- deterministic prob endpoints
# =============================================================================

expect_equal(length(hc_effect(integer(0), 0.5, logical(0))), 0L,
             info = "hc_effect empty input")
# prob = 1: effect continues from a previous 1 (within person)
expect_equal(hc_effect(c(0L, 1L, 0L, 1L, 0L), 1.0,
                       c(TRUE, FALSE, FALSE, TRUE, FALSE)),
             c(0L, 1L, 1L, 1L, 1L),
             info = "hc_effect prob=1 continues effect")
# prob = 0: never continues
expect_equal(hc_effect(c(0L, 1L, 0L, 1L, 0L), 0.0,
                       c(TRUE, FALSE, FALSE, TRUE, FALSE)),
             c(0L, 1L, 0L, 1L, 0L),
             info = "hc_effect prob=0 leaves unchanged")
# NA in prev element skips
expect_equal(hc_effect(c(1L, NA, 0L), 1.0, c(TRUE, FALSE, FALSE)),
             c(1L, NA, 0L),
             info = "hc_effect NA prev skips")

# =============================================================================
# antilogit (both numerically-stable branches, symmetry, endpoints)
# =============================================================================

expect_equal(antilogit(0), 0.5, info = "antilogit(0) = 0.5")
# x > 0 branch
expect_equal(antilogit(2), exp(2) / (1 + exp(2)), tolerance = 1e-12,
             info = "antilogit positive branch")
# x <= 0 branch
expect_equal(antilogit(-2), exp(-2) / (1 + exp(-2)), tolerance = 1e-12,
             info = "antilogit non-positive branch")
# Symmetry
expect_equal(antilogit(-3), 1 - antilogit(3), tolerance = 1e-12,
             info = "antilogit symmetry")
# Numerical stability at extremes
expect_true(is.finite(antilogit(700)) && is.finite(antilogit(-700)),
            info = "antilogit finite at extremes")
expect_true(antilogit(700) <= 1 && antilogit(-700) >= 0,
            info = "antilogit bounded [0,1] at extremes")
