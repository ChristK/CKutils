# Extra tests targeting branch coverage of misc_functions.R:
#   agegrp_name, replace_from_table, to_agegrp, get_pcloud_path, arrow_in

if (!requireNamespace("data.table", quietly = TRUE)) {
  exit_file("data.table package not available")
}
suppressMessages(library(data.table))
set.seed(42)

# =============================================================================
# get_pcloud_path
# =============================================================================

pc <- get_pcloud_path()
expect_true(is.character(pc), info = "get_pcloud_path returns character")
expect_equal(length(pc), 1L, info = "get_pcloud_path returns scalar")
expect_true(nchar(pc) > 0L, info = "get_pcloud_path non-empty")

pc_tail <- get_pcloud_path("subdir")
expect_true(is.character(pc_tail), info = "get_pcloud_path(tail) is character")
expect_equal(length(pc_tail), 1L, info = "get_pcloud_path(tail) scalar")
expect_true(grepl("subdir", pc_tail), info = "pathtail appended to pcloud path")

# =============================================================================
# agegrp_name
# =============================================================================

# grp_width > 1, open band ((tail(x,1)-1) != max_age): NN+ naming, no <1
g_open <- agegrp_name(20, 79, 5, grp_lessthan_1 = FALSE)
expect_equal(
  g_open,
  c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
    "50-54", "55-59", "60-64", "65-69", "70-74", "75-79"),
  info = "open band w5 no open-ended needed (tail-1 == max here is 79? check)"
)

# Documented: 20-79 actually has 75-79 closing exactly; use 20-80 for the NN+ band
g_openplus <- agegrp_name(20, 80, 5, grp_lessthan_1 = FALSE)
expect_true("80+" %in% g_openplus, info = "open-ended 80+ band created")
expect_equal(g_openplus[length(g_openplus)], "80+", info = "last band is 80+")

# grp_width > 1, closed band (tail(x,1)-1 == max_age) -> head drop, no NN+
g_closed <- agegrp_name(20, 79, 5)
expect_false(any(grepl("\\+", g_closed)), info = "closed band has no open-ended")

# grp_lessthan_1 TRUE with min_age == 0, open band
g_lt1_open <- agegrp_name(0, 80, 10, TRUE)
expect_equal(g_lt1_open[1], "<1", info = "<1 first band when grp_lessthan_1 & min_age==0")
expect_equal(g_lt1_open[2], "01-09", info = "second band relabeled to 01-NN")
expect_equal(g_lt1_open[length(g_lt1_open)], "80+", info = "open-ended last band with <1")

# grp_lessthan_1 TRUE with min_age == 0, closed band (tail-1 == max)
g_lt1_closed <- agegrp_name(0, 9, 5, TRUE)
expect_equal(g_lt1_closed, c("<1", "01-04", "05-09"), info = "<1 closed band")

# grp_width == 1 branch (else branch)
g_w1 <- agegrp_name(20, 25, 1)
expect_equal(g_w1, c("20", "21", "22", "23", "24", "25"),
             info = "grp_width == 1 yields single-year labels")

# match_input TRUE, else path (grp_lessthan_1 FALSE), open band (tail-1 != max)
g_m_open_else <- agegrp_name(20, 30, 5, FALSE, TRUE)
expect_equal(length(g_m_open_else), 11L,
             info = "match_input open else: 5+5 repeats + 1 open band")
expect_equal(g_m_open_else[1], "20-24", info = "match open else first")
expect_equal(g_m_open_else[length(g_m_open_else)], "30+", info = "match open else last")

# match_input TRUE, else path, open band with match_input_max_age extension
g_m_open_else_mia <- agegrp_name(20, 30, 5, FALSE, TRUE, 32)
expect_equal(length(g_m_open_else_mia), 13L,
             info = "match_input_max_age extends open band repeats")
expect_equal(sum(g_m_open_else_mia == "30+"), 3L,
             info = "open band repeated to match_input_max_age")

# match_input TRUE, else path, closed band (tail-1 == max)
g_m_closed_else <- agegrp_name(20, 29, 5, FALSE, TRUE)
expect_equal(length(g_m_closed_else), 10L,
             info = "match_input closed else: each band repeated grp_width")
expect_false(any(grepl("\\+", g_m_closed_else)),
             info = "match closed else no open-ended")

# match_input TRUE, grp_lessthan_1 & min_age==0 path, open band (tail-1 != max)
g_m_open_lt1 <- agegrp_name(0, 30, 5, TRUE, TRUE)
expect_equal(g_m_open_lt1[1], "<1", info = "match <1 open: first is <1")
expect_equal(g_m_open_lt1[length(g_m_open_lt1)], "30+",
             info = "match <1 open: last is 30+")
expect_equal(sum(g_m_open_lt1 == "01-04"), 4L,
             info = "match <1 open: 01-04 band has 4 entries (first overwritten by <1)")
expect_equal(sum(g_m_open_lt1 == "05-09"), 5L,
             info = "match <1 open: full interior band repeated grp_width times")

# match_input TRUE, grp_lessthan_1 & min_age==0 path, closed band (tail-1 == max)
g_m_closed_lt1 <- agegrp_name(0, 29, 5, TRUE, TRUE)
expect_equal(g_m_closed_lt1[1], "<1", info = "match <1 closed: first is <1")
expect_false(any(grepl("\\+", g_m_closed_lt1)),
             info = "match <1 closed: no open-ended band")
expect_equal(length(g_m_closed_lt1), 30L,
             info = "match <1 closed: length covers each single year")

# stopifnot validation failures
expect_error(agegrp_name(min_age = -1, max_age = 80), info = "min_age < 0 errors")
expect_error(agegrp_name(min_age = 50, max_age = 30), info = "max_age <= min_age errors")
expect_error(agegrp_name(min_age = 0, max_age = 80, grp_width = 0),
             info = "grp_width < 1 errors")
expect_error(agegrp_name(min_age = 0, max_age = 80, match_input_max_age = 70),
             info = "match_input_max_age < max_age errors")
expect_error(agegrp_name(min_age = 0, max_age = 0), info = "max_age == 0 errors")

# =============================================================================
# replace_from_table
# =============================================================================

base_dt <- data.table(
  a = 1:5,
  b = seq(1, 2.2, 0.3),
  d = letters[1:5]
)
base_dt[, e := factor(a, labels = LETTERS[1:5])]

# same-class, by reference (newcolname NULL)
r1 <- replace_from_table(copy(base_dt), "a", 3L, -11L)
expect_equal(r1$a, c(1L, 2L, -11L, 4L, 5L), info = "same-class byref replace")
expect_equal(names(r1), names(base_dt), info = "column order preserved byref")

# same-class, into a new column
r2 <- replace_from_table(copy(base_dt), "a", 3L, -11L, "newcol")
expect_true("newcol" %in% names(r2), info = "new column created")
expect_equal(r2$newcol, c(1L, 2L, -11L, 4L, 5L), info = "new column holds replaced values")
expect_equal(r2$a, base_dt$a, info = "original column unchanged with newcolname")

# same-class many-to-one (length(from) > length(to)) -> message branch + byref
expect_message(
  replace_from_table(copy(base_dt), "a", 1:3, 3L),
  pattern = "matched many to few",
  info = "many-to-few message"
)
r3 <- suppressMessages(replace_from_table(copy(base_dt), "a", 1:3, 3L))
expect_equal(r3$a, c(3L, 3L, 3L, 4L, 5L), info = "many-to-one values correct")

# different-class coercion, by reference (emits coercion message)
expect_message(
  replace_from_table(copy(base_dt), "b", 1.3, "a"),
  pattern = "coerced to",
  info = "diff-class byref coercion message"
)
r4 <- suppressMessages(replace_from_table(copy(base_dt), "b", 1.3, "a"))
expect_true(is.character(r4$b), info = "diff-class byref column coerced to character")
expect_equal(r4$b, c("1", "a", "1.6", "1.9", "2.2"), info = "diff-class byref values")
expect_equal(names(r4), names(base_dt), info = "diff-class byref preserves col order")

# different-class coercion, into a new column (no coercion message in this branch)
r5 <- replace_from_table(copy(base_dt), "b", 1.3, "a", "nc")
expect_true("nc" %in% names(r5), info = "diff-class new column created")
expect_equal(r5$nc, c("1", "a", "1.6", "1.9", "2.2"), info = "diff-class newcol values")
expect_equal(r5$b, base_dt$b, info = "diff-class original column unchanged")

# factor column replacement
r6 <- replace_from_table(copy(base_dt), "e", "B", "J")
expect_true("J" %in% as.character(r6$e), info = "factor replacement works")
expect_equal(as.character(r6$e), c("A", "J", "C", "D", "E"), info = "factor values correct")

# error: newcolname already exists
expect_error(
  replace_from_table(copy(base_dt), "a", 3L, -11L, "b"),
  pattern = "already exists",
  info = "error when newcolname exists"
)

# error: not a data.table
expect_error(replace_from_table(as.data.frame(base_dt), "a", 3L, -11L),
             info = "error on non-data.table")
# error: colname not in dtb
expect_error(replace_from_table(copy(base_dt), "zzz", 1, 2),
             info = "error on missing colname")
# error: length(from) < length(to)
expect_error(replace_from_table(copy(base_dt), "a", 1L, c(1L, 2L)),
             info = "error when from shorter than to")

# =============================================================================
# to_agegrp
# =============================================================================

# default options, factor output
t1 <- to_agegrp(data.table(age = 0:99))
expect_true("agegrp" %in% names(t1), info = "agegrp column created")
expect_true(is.factor(t1$agegrp), info = "agegrp is factor by default")
expect_equal(nrow(t1), 100L, info = "row count unchanged")
expect_equal(as.character(t1$agegrp[1]), "<1", info = "age 0 -> <1")
expect_equal(as.character(t1$agegrp[100]), "85+", info = "age 99 -> 85+")

# custom max_age
t2 <- to_agegrp(data.table(age = 0:99), max_age = 80L)
expect_true("80+" %in% as.character(t2$agegrp), info = "custom max_age open band")

# custom grp_width, to_factor FALSE
t3 <- to_agegrp(data.table(age = 0:99), grp_width = 10, max_age = 85, to_factor = FALSE)
expect_true(is.character(t3$agegrp), info = "to_factor=FALSE yields character")

# custom column names + explicit min_age
t4_in <- data.table(patient_age = c(3L, 7L, 40L))
t4 <- to_agegrp(
  t4_in,
  grp_width = 5L,
  max_age = 20L,
  age_colname = "patient_age",
  agegrp_colname = "age_cat",
  to_factor = TRUE,
  min_age = 0L
)
expect_true("age_cat" %in% names(t4), info = "custom agegrp_colname")
expect_true(is.factor(t4$age_cat), info = "custom-named column is factor")
expect_equal(as.character(t4$age_cat), c("01-04", "05-09", "20+"),
             info = "to_agegrp values correct with explicit min_age/max_age")

# error conditions
expect_error(to_agegrp(as.data.frame(data.table(age = 0:9))),
             info = "to_agegrp error on non-data.table")
expect_error(to_agegrp(data.table(age = 0:9), age_colname = "nope"),
             info = "to_agegrp error on missing age column")

# =============================================================================
# arrow_in
# =============================================================================

if (requireNamespace("arrow", quietly = TRUE)) {
  expr <- arrow_in("sex", c("M", "F"))
  expect_true(inherits(expr, "Expression"), info = "arrow_in returns Expression")

  # Expression field input branch
  expr2 <- arrow_in(arrow::Expression$field_ref("sex"), c("M", "F"))
  expect_true(inherits(expr2, "Expression"), info = "arrow_in accepts Expression field")

  # single value
  expr3 <- arrow_in("age", 40L)
  expect_true(inherits(expr3, "Expression"), info = "arrow_in single value works")

  # Construct an in-memory arrow Table and filter with the expression to
  # confirm the produced is_in expression is functional.
  tbl <- arrow::arrow_table(
    data.table(sex = c("M", "F", "M", "F"), age = c(30L, 40L, 50L, 40L))
  )
  filtered <- as.data.frame(
    dplyr::filter(tbl, arrow_in("sex", "M"))
  )
  expect_equal(nrow(filtered), 2L, info = "arrow_in is_in filters table rows")
  expect_true(all(filtered$sex == "M"), info = "arrow_in filter keeps matching values")

  # error: empty values
  expect_error(arrow_in("sex", character(0)),
               info = "arrow_in errors on empty values")
  # error: bad field (vector of length > 1)
  expect_error(arrow_in(c("sex", "age"), "M"),
               info = "arrow_in errors on non-scalar field")
} else {
  expect_true(TRUE, info = "arrow not available - skipping arrow_in tests")
}
